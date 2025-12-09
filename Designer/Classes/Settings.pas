unit Settings;

interface

uses
  Classes,
  SysUtils;

type
  TSettings = class
  {$REGION 'Internal Declarations'}
  strict private
    class var FInstance: TSettings;
  strict private
    FSettingsFilename: String;
    FRibbonCompilerPath: String;
    FResourceCompilerPath: String;
    FDelphiCompilerPath: String;
  private
    constructor Create(const Dummy: Integer); overload;
    procedure Load;
    function GetRibbonCompilerPath: String;
    /// Searches the %PATH% environment variable and different well-known locations of the Winodws toolkit for the given exe file.
    /// Returns the full path to the exe file, if it can be found. If no such path can be found, an empty string is returned-
    function FindTool(const pExeName: string): string;
  public
    class constructor Create;
    class destructor Destroy;
  {$ENDREGION}
  public
    constructor Create; overload;
    procedure Save;

    /// Tries to find the used tool on the current system if their paths are empty
    procedure DetectTools();
    function ToolsAvailable: Boolean;
    /// Returns a separated list of paths that can be used to search for the ribbon tools. We use the PATH variable as well as some well known locations.
    function GetSearchPaths: string;

    class property Instance: TSettings read FInstance;

    property RibbonCompilerPath: String read FRibbonCompilerPath write FRibbonCompilerPath;
    property ResourceCompilerPath: String read FResourceCompilerPath write FResourceCompilerPath;
    /// The path of the directory in which the ribbon compiler UICC.exe can be found
    property RibbonCompilerDir: String read GetRibbonCompilerPath;
    property DelphiCompilerPath: String read FDelphiCompilerPath write FDelphiCompilerPath;
  end;

resourcestring
  RS_CANNOT_SAVE_SETTINGS = 'Unable to save settings.';
  RS_TOOLS_HEADER = 'Cannot find compilation tools';
  RS_TOOLS_MESSAGE = 'One or more ribbon compilation tools (UICC.exe, DCC32.exe and/or RC.exe) could not be found.';
  RS_TOOLS_SETUP = 'Do you want to open the settings dialog box to specify the locations of these tools now?';

implementation

uses
  Windows,
  Registry,
  IOUtils,
  ShFolder,
  BasicXml;

const // Elemement Names
  EN_SETTINGS = 'Settings';
  EN_SETTING  = 'Setting';

const // Attribute Names
  AN_NAME     = 'name';
  AN_VALUE    = 'value';

const // Setting Names
  SN_RIBBON_COMPILER   = 'RibbonCompiler';
  SN_RESOURCE_COMPILER = 'ResourceCompiler';
  SN_DELPHI_COMPILER   = 'DelphiCompiler';

{ TSettings }

constructor TSettings.Create;
begin
  raise EInvalidOperation.Create('Don''t create a TSettings instance manually. Use TSettings.Instance.');
end;

constructor TSettings.Create(const Dummy: Integer);
var
  Path: array [0..MAX_PATH] of Char;
begin
  inherited Create;
  if Succeeded(SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, 0, Path)) then
  begin
    FSettingsFilename := Path;
    FSettingsFilename := TPath.Combine(FSettingsFilename, 'Ribbon Designer');
    ForceDirectories(FSettingsFilename);
    FSettingsFilename := TPath.Combine(FSettingsFilename, 'Settings.xml');
    Load;
  end;
end;

procedure TSettings.DetectTools();
var
  Reg: TRegistry;
  BdsVersion: Integer;
  BdsKey, BdsPath: String;
begin
  if not FileExists(FRibbonCompilerPath) then
    FRibbonCompilerPath := FindTool('UICC.exe');
  if not FileExists(FResourceCompilerPath) then
    FResourceCompilerPath := FindTool('rc.exe');
  if not FileExists(FResourceCompilerPath) then
    FResourceCompilerPath := FindTool('resinator.exe');

  // Find delphi compiler
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
      for BdsVersion := 50 downto 10 do
      begin
        BdsKey := 'Software\Embarcadero\BDS\' + IntToStr(BdsVersion) + '.0';
        if (Reg.OpenKeyReadOnly(BdsKey)) then
        begin
          BdsPath := Reg.ReadString('RootDir');
          if (BdsPath <> '') then
          begin
            BdsPath := TPath.Combine(BdsPath, 'bin');
            if (FDelphiCompilerPath = '') then
            begin
              FDelphiCompilerPath := TPath.Combine(BdsPath, 'DCC32.exe');
              if (not TFile.Exists(FDelphiCompilerPath)) then
                FDelphiCompilerPath := '';
            end;
          end;
        end;

        if (FDelphiCompilerPath <> '') then
          Break;
      end;
  finally
    Reg.Free;
  end;

end;

function TSettings.GetSearchPaths: string;
var
  lList : TStringList;
begin
  lList := TStringList.Create;
  try
    lList.Delimiter := PathSep;
    lList.Add(ExtractFileDir(ParamStr(0))); // Check current directory of the RibbonCMDCompiler.exe first to find ribbon compiler UICC.exe
    lList.Add(GetEnvironmentVariable('PATH'));
    lList.Add(GetEnvironmentVariable('ProgramFiles(x86)') + '\Windows Kits\10\bin\10.0.26100.0\x86\');
    lList.Add(GetEnvironmentVariable('ProgramFiles(x86)') + '\Windows Kits\10\bin\10.0.18362.0\x86\');
    lList.Add(GetEnvironmentVariable('ProgramFiles')      + '\Windows Kits\10\bin\x86\');
    lList.Add(GetEnvironmentVariable('ProgramFiles(x86)') + '\Windows Kits\10\bin\x86\');
    lList.Add(GetEnvironmentVariable('ProgramFiles')      + '\Windows Kits\8.1\bin\x86\');
    lList.Add(GetEnvironmentVariable('ProgramFiles(x86)') + '\Windows Kits\8.1\bin\x86\');
    lList.Add(GetEnvironmentVariable('ProgramFiles')      + '\Microsoft SDKs\Windows\v7.1\Bin\');
    lList.Add(GetEnvironmentVariable('ProgramFiles(x86)') + '\Microsoft SDKs\Windows\v7.1\Bin\');
    Result := lList.DelimitedText.Replace('"', '');
  finally
    lList.Free;
  end;
end;

function TSettings.FindTool(const pExeName: string): string;
begin
  Result := FileSearch(pExeName, GetSearchPaths);
end;

function TSettings.GetRibbonCompilerPath: String;
begin
  Exit(ExcludeTrailingPathDelimiter(ExtractFilePath(FRibbonCompilerPath)))
end;

class constructor TSettings.Create;
begin
  FInstance := TSettings.Create(0);
end;

class destructor TSettings.Destroy;
begin
  FInstance.Free;
end;

procedure TSettings.Load;
var
  Doc: TXmlDocument;
  E: TXmlElement;
  Name, Value: String;
begin
  if (not FileExists(FSettingsFilename)) then
    Exit;

  Doc := TXmlDocument.Create;
  try
    Doc.LoadFromFile(FSettingsFilename);
    if (Doc.Root = nil) or (Doc.Root.Name <> EN_SETTINGS) then
      Exit;

    for E in Doc.Root do
    begin
      if (E.Name = EN_SETTING) then
      begin
        Name := E.AttributeAsString[AN_NAME];
        Value := E.AttributeAsString[AN_VALUE];
        if (Value <> '') then
        begin
          if (Name = SN_RIBBON_COMPILER) and FileExists(Value) then
            FRibbonCompilerPath := Value
          else if (Name = SN_DELPHI_COMPILER) and FileExists(Value) then
            FDelphiCompilerPath := Value;
        end;
      end;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TSettings.Save;
var
  Writer: TXmlWriter;
  Stream: TFileStream;
  Xml: RawByteString;

  procedure SaveSetting(const Name, Value: String);
  begin
    Writer.WriteStartElement(EN_SETTING);
    Writer.WriteAttribute(AN_NAME, Name);
    Writer.WriteAttribute(AN_VALUE, Value);
    Writer.WriteEndElement;
  end;

begin
  if (FSettingsFilename = '') then
    raise Exception.Create(RS_CANNOT_SAVE_SETTINGS);

  Writer := TXmlWriter.Create;
  try
    Writer.Indent := True;
    Writer.WriteStartElement(EN_SETTINGS);
    SaveSetting(SN_RIBBON_COMPILER, FRibbonCompilerPath);
    SaveSetting(SN_RESOURCE_COMPILER, FResourceCompilerPath);
    SaveSetting(SN_DELPHI_COMPILER, FDelphiCompilerPath);
    Writer.WriteEndElement;
    Xml := Writer.AsXml;
  finally
    Writer.Free;
  end;

  Stream := TFileStream.Create(FSettingsFilename, fmCreate);
  try
    if (Xml <> '') then
      Stream.WriteBuffer(Xml[1], Length(Xml));
  finally
    Stream.Free;
  end;
end;

function TSettings.ToolsAvailable: Boolean;
begin
  DetectTools();
  Result := (FRibbonCompilerPath <> '') and (FDelphiCompilerPath <> '') and (FResourceCompilerPath <> '');
end;

end.
