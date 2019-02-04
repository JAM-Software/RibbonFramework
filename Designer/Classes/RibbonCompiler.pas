unit RibbonCompiler;

interface

uses
  Classes,
  SysUtils,
  RibbonMarkup;

type
  TRibbonCompiler = class;

  TMessageKind = (mkInfo, mkWarning, mkError, mkPipe);

  TRibbonCompilerMessageEvent = procedure(const Compiler: TRibbonCompiler;
    const MsgType: TMessageKind; const Msg: String) of object;

  TRibbonCompileResult = (crOk, crNoFilename, crNoTools, crRibbonCompilerError,
    crResourceCompilerError, crDelphiCompilerError, crHeaderConversionError,
    crNoOutput, crException);

  TRibbonCompiler = class
  {$REGION 'Internal Declarations'}
  strict private
    FOutputDllPath: String;
    FOnMessage: TRibbonCompilerMessageEvent;
  strict private
    procedure DoMessage(const MsgType: TMessageKind;
      const Msg: String); overload;
    procedure DoMessage(const MsgType: TMessageKind;
      const Msg: String; const Args: array of const); overload;
    function Execute(const Application, CurrentDir: String;
      const Parameters: array of String): Boolean;
    procedure CreateDelphiProject(const Filename: String);
  {$ENDREGION 'Internal Declarations'}
  public
    function Compile(const Document: TRibbonDocument; ResourceName: string = 'APPLICATION'): TRibbonCompileResult;

    property OutputDllPath: String read FOutputDllPath;
    property OnMessage: TRibbonCompilerMessageEvent read FOnMessage write FOnMessage;
    class procedure HandleCommandLine; static;
  end;

resourcestring
  RS_NO_FILENAME = 'Document needs to be saved before it can be compiled.';
  RS_NO_TOOLS = 'Compilation tools (RC.exe, UICC.exe and/or DCC32.exe) are not available. You need to specify the location of these tools in the Settings dialog box.';
  RS_STARTING_RIBBON_COMPILER = 'STARTING THE RIBBON COMPILER...';
  RS_STARTING_RESOURCE_COMPILER = 'STARTING THE RESOURCE COMPILER...';
  RS_STARTING_DELPHI_COMPILER = 'STARTING DELPHI COMPILER...';
  RS_PIPE_ERROR = 'Application output of "%s" not available';
  RS_ERROR_EXECUTING = 'Error executing "%s": %s';
  RS_COMPILE_SUCCESS = 'RIBBON COMPILED SUCCESSFULLY!';
  RS_NO_DLL = 'Delphi compiler did not output a resource DLL';

implementation

uses
  Windows,
  IOUtils,
  Settings,
  MarkupGenerator;

{ TRibbonCompiler }

function TRibbonCompiler.Compile(const Document: TRibbonDocument; ResourceName: string = 'APPLICATION'): TRibbonCompileResult;
var
  DocDir, DprFilename: String;
  lResouceCompilerPath: string;
  lMarkupGenerator: TMarkupGenerator;
  lBmlFileParam: string;
  lHeaderFileParam: string;
  lRcFilePath: string;
  lRcFileParam: string;
  lNameParam: string;
begin
  try
    if (Document.Filename = '') or (not TFile.Exists(Document.Filename)) then
    begin
      DoMessage(mkError, RS_NO_FILENAME);
      Exit(crNoFilename);
    end;

    if (not TSettings.Instance.ToolsAvailable) then
    begin
      DoMessage(mkError, RS_NO_TOOLS);
      Exit(crNoTools);
    end;

    DoMessage(mkInfo, RS_STARTING_RIBBON_COMPILER);
    DoMessage(mkInfo, '');

    DocDir := ExtractFilePath(Document.Filename);

    lBmlFileParam := ChangeFileExt(Document.Filename, '.bml').QuotedString('"');
    lHeaderFileParam := Format('"/header:%s"', [ChangeFileExt(Document.Filename, '.h')]);
    lRcFilePath := ChangeFileExt(Document.Filename, '.rc');
    lRcFileParam := Format('"/res:%s"', [lRcFilePath]);
    lNameParam := Format('"/name:%s"', [ResourceName]);

    // Run ribbon compiler UICC.exe to convert the markup XML to a header, a resource and a bml file.
    if not Execute(TSettings.Instance.RibbonCompilerPath, DocDir, ['"/W0"', Document.Filename.QuotedString('"'), lBmlFileParam, lHeaderFileParam, lRcFileParam, lNameParam]) then
      Exit(crRibbonCompilerError);

    // Run the resource compiler, so that we can include the file into a .pas file
    lResouceCompilerPath := ExtractFilePath(TSettings.Instance.RibbonCompilerPath) + 'rc.exe';
    if not Execute(lResouceCompilerPath, DocDir, [lRcFilePath.QuotedString('"')]) then
      Exit(crResourceCompilerError);

    // Generate the pas file, using the generated files from the previous steps.
    lMarkupGenerator := TMarkupGenerator.Create(Document.Filename, ResourceName);
    try
      lMarkupGenerator.GenerateMarkupFiles;
    finally
      lMarkupGenerator.Free;
    end;

    DoMessage(mkInfo, sLineBreak + RS_STARTING_DELPHI_COMPILER);
    DprFilename := ChangeFileExt(Document.Filename, '.dpr');
    CreateDelphiProject(DprFilename);
    if (not Execute(TSettings.Instance.DelphiCompilerPath, DocDir,
      ['"' + DprFilename + '"']))
    then
      Exit(crDelphiCompilerError);

    DoMessage(mkInfo, '');
    FOutputDllPath := ChangeFileExt(DprFilename, '.dll');
    if (TFile.Exists(FOutputDllPath)) then
    begin
      Result := crOk;
      DoMessage(mkInfo, RS_COMPILE_SUCCESS)
    end
    else
    begin
      Result := crNoOutput;
      DoMessage(mkError, RS_NO_DLL);
    end;
  except
    on E: Exception do
    begin
      Result := crException;
      DoMessage(mkError, E.Message);
    end;
  end;
end;

procedure TRibbonCompiler.DoMessage(const MsgType: TMessageKind;
  const Msg: String);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, MsgType, Msg);
end;

procedure TRibbonCompiler.CreateDelphiProject(const Filename: String);
const
  PROJECT_SOURCE =
    'library %s;' + sLineBreak +
    sLineBreak +
    '{$R *.res}' + sLineBreak +
    sLineBreak +
    'begin' + sLineBreak +
    'end.' + sLineBreak;
var
  Source: String;
  AnsiSource: AnsiString;
  Stream: TFileStream;
begin
  Source := Format(PROJECT_SOURCE, [ChangeFileExt(ExtractFilename(Filename), '')]);
  {$WARNINGS OFF}
  AnsiSource := AnsiString(Source);
  {$WARNINGS ON}
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    Stream.WriteBuffer(AnsiSource[1], Length(AnsiSource));
  finally
    Stream.Free;
  end;
end;

function AttachConsole(dwProcessID: Integer): Boolean; stdcall; external 'kernel32.dll';


procedure TRibbonCompiler.DoMessage(const MsgType: TMessageKind;
  const Msg: String; const Args: array of const);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, MsgType, Format(Msg, Args));
end;

function TRibbonCompiler.Execute(const Application, CurrentDir: String;
  const Parameters: array of String): Boolean;
var
  SecurityAttrs: TSecurityAttributes;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  ReadPipe, WritePipe: THandle;
  CmdLine, Param: String;
  AppRunning, BytesRead, BytesAvail, BytesLeft: Cardinal;
  Buffer: TBytes;
  ExitCode: Cardinal;

  procedure LogBuffer;
  var
    S: String;
  begin
    if BytesRead = 0 then
      exit;
    Buffer[BytesRead] := 0;
    With TEncoding.GetEncoding(CP_OEMCP) do begin
      S := GetString(Buffer);
      Free;
    end;
    DoMessage(mkPipe, Trim(S) + sLineBreak);
  end;

begin
  Result := False;
  SecurityAttrs.nLength := SizeOf(SecurityAttrs);
  SecurityAttrs.lpSecurityDescriptor := nil;
  SecurityAttrs.bInheritHandle := True;

  if not Assigned(FOnMessage) or (not CreatePipe(ReadPipe, WritePipe, @SecurityAttrs, 0)) then
  begin
    DoMessage(mkWarning, RS_PIPE_ERROR, [ExtractFilename(Application)]);
    ReadPipe := 0;
    WritePipe := 0;
    // Use parent console if available
    AttachConsole(-1);
  end;

  try
    FillChar(StartupInfo, SizeOf(StartupInfo), 0);
    StartupInfo.cb := SizeOf(StartupInfo);
    StartupInfo.hStdOutput := WritePipe;
    StartupInfo.hStdInput := 0;
    StartupInfo.hStdError := WritePipe;
    if (ReadPipe = 0) or (WritePipe = 0) then
      StartupInfo.dwFlags := 0
    else
      StartupInfo.dwFlags := STARTF_USESTDHANDLES;
    StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := SW_HIDE;

    CmdLine := Application;
    for Param in Parameters do
      CmdLine := CmdLine + ' ' + Param;
    {$ifdef DEBUG}
    DoMessage(TMessageKind.mkInfo, 'Executing: ' +  CmdLine);
    {$endif}
    if (not CreateProcess(nil, PChar(CmdLine), @SecurityAttrs, @SecurityAttrs,
      True, NORMAL_PRIORITY_CLASS, nil, PChar(CurrentDir), StartupInfo, ProcessInfo))
    then
    begin
      DoMessage(mkError, RS_ERROR_EXECUTING, [Application, SysErrorMessage(GetLastError)]);
      Exit;
    end;

    try
      SetLength(Buffer, 1024);
      repeat
        BytesRead := 0;
        AppRunning := WaitForSingleObject(ProcessInfo.hProcess, 10);
        if PeekNamedPipe(ReadPipe, @Buffer[0], Length(Buffer) -1, @BytesRead, @BytesAvail, @BytesLeft) and (BytesAvail >0) then
        begin
          if ReadFile(ReadPipe, Buffer[0], BytesAvail, BytesRead, nil) then
            LogBuffer();
        end;//if
      until (AppRunning <> WAIT_TIMEOUT);
      Result := GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);
      Result := Result and (ExitCode = 0);
    finally
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end;
  finally
    CloseHandle(ReadPipe);
    CloseHandle(WritePipe);
  end;
end;

class procedure TRibbonCompiler.HandleCommandLine;
var
  lRibbonFile: string;
  lRibbonDocument: TRibbonDocument;
  lRibbonCompiler: TRibbonCompiler;
  lSuccess: Boolean;
begin
  if (ParamCount = 0) or (ParamCount > 2) or FindCmdLineSwitch('HELP') or FindCmdLineSwitch('?') then
    writeln('Usage: RibbonCMDCompiler.exe [markupfile] [Resourcename (optional)].')
  else begin
    ExitCode := 1;
    lRibbonFile := ParamStr(1);

    if not FileExists(lRibbonFile) then
      writeln('File not found: ' + lRibbonFile)

    else if not TSettings.Instance.ToolsAvailable then
      writeln(RS_TOOLS_HEADER + sLineBreak + RS_TOOLS_MESSAGE)

    else begin
      lRibbonCompiler := TRibbonCompiler.Create;
      try
        lRibbonDocument := TRibbonDocument.Create;
        try
          try
            lRibbonDocument.LoadFromFile(lRibbonFile);
          except on E: ERibbonMarkupError do
            writeln(E.Message);
          end;

        if not ParamStr(2).IsEmpty then
          lSuccess := lRibbonCompiler.Compile(lRibbonDocument, ParamStr(2)) = crOk
        else
          lSuccess := lRibbonCompiler.Compile(lRibbonDocument) = crOk;

        if lSuccess then
          ExitCode := 0;

        finally
          lRibbonDocument.Free;
        end;
      finally
        lRibbonCompiler.Free;
      end;
    end;
  end;
end;

end.
