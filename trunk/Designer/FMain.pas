unit FMain;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComCtrls,
  ToolWin,
  StdCtrls,
  ActnList,
  ExtCtrls,
  AppEvnts,
  ImgList,
  Menus,
  ShellApi,
  RibbonMarkup,
  RibbonCompiler,
  Settings,
  BasicZip,
  FCommands,
  FViews,
  FXmlSource,
  FPreview,
  FSettings,
  FNewFile;

type
  TFormMain = class(TForm)
    ToolBar: TToolBar;
    ButtonOpen: TToolButton;
    MemoMessages: TMemo;
    ButtonPreview: TToolButton;
    ActionList: TActionList;
    ActionPreview: TAction;
    SplitterLog: TSplitter;
    PageControl: TPageControl;
    TabSheetCommands: TTabSheet;
    ApplicationEvents: TApplicationEvents;
    StatusBar: TStatusBar;
    ActionOpen: TAction;
    ActionNew: TAction;
    ActionSave: TAction;
    ActionSaveAs: TAction;
    ButtonSave: TToolButton;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    TabSheetViews: TTabSheet;
    Images: TImageList;
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuNew: TMenuItem;
    MenuOpen: TMenuItem;
    N1: TMenuItem;
    MenuSave: TMenuItem;
    SaveAsMenu: TMenuItem;
    ActionExit: TAction;
    N2: TMenuItem;
    MenuExit: TMenuItem;
    MenuProject: TMenuItem;
    MenuPreview: TMenuItem;
    ToolButton1: TToolButton;
    ActionSettings: TAction;
    N3: TMenuItem;
    SettingsMenu: TMenuItem;
    TimerRestoreLog: TTimer;
    ActionNewBlank: TAction;
    TabSheetXmlSource: TTabSheet;
    ActionBuild: TAction;
    ButtonBuild: TToolButton;
    BuildMenu: TMenuItem;
    ActionTutorial: TAction;
    ActionWebSite: TAction;
    ActionMSDN: TAction;
    MenuHelp: TMenuItem;
    MenuTutorial: TMenuItem;
    MenuWebsite: TMenuItem;
    N4: TMenuItem;
    MenuMSDN: TMenuItem;
    procedure FormActivate(Sender: TObject);
    procedure ActionPreviewExecute(Sender: TObject);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure ApplicationEventsHint(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ActionSaveExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionSettingsExecute(Sender: TObject);
    procedure TimerRestoreLogTimer(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionBuildExecute(Sender: TObject);
    procedure ActionTutorialExecute(Sender: TObject);
    procedure ActionWebSiteExecute(Sender: TObject);
    procedure ActionMSDNExecute(Sender: TObject);
  private
    { Private declarations }
    FInitialized: Boolean;
    FDocument: TRibbonDocument;
    FCompiler: TRibbonCompiler;
    FFrameCommands: TFrameCommands;
    FFrameViews: TFrameViews;
    FFrameXmlSource: TFrameXmlSource;
    FModified: Boolean;
    FPreviewForm: TFormPreview;
  private
    procedure CMShowingChanged(var Msg: TMessage); message CM_SHOWINGCHANGED;
    procedure RibbonCompilerMessage(const Compiler: TRibbonCompiler;
      const MsgType: TMessageKind; const Msg: String);
    procedure ClearLog;
    procedure Log(const MsgType: TMessageKind; const Msg: String);
    procedure NewFile(const EmptyFile: Boolean);
    procedure OpenFile(const Filename: String);
    procedure ClearDocument;
    procedure ShowDocument;
    procedure ShowSettingsDialog;
    procedure UpdateCaption;
    procedure UpdateControls;
    procedure ClearModified;
    function CheckSave: Boolean;
    procedure BuildAndPreview(const Preview: Boolean);
    procedure OpenWebsite(const Url: String);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Modified;
  end;

var
  FormMain: TFormMain;

resourcestring
  RS_CANNOT_LOAD_DLL = 'Unable to load Ribbon Resource DLL';
  RS_MODIFIED = 'Modified';
  RS_TOOLS_HEADER = 'Cannot find compilation tools';
  RS_TOOLS_MESSAGE = 'One or more ribbon compilation tools (RC.exe, UICC.exe and/or DCC32.exe) could not be found.' + sLineBreak +
    'You need these tools to compile and preview ribbons.' + sLineBreak +
    'Do you want to open the settings dialog box to specify the locations of these tools now?';
  RS_RIBBON_DESIGNER = 'Ribbon Designer';
  RS_UNTITLED = '(untitled document)';
  RS_CHANGED_HEADER = 'Document has changed';
  RS_CHANGED_MESSAGE = 'The document has changed. Do you want to save the changes?';
  RS_DIFFERENT_DIR_HEADER = 'Directory changed';
  RS_DIFFERENT_DIR_MESSAGE = 'You are about to save to document to a different directory.' + sLineBreak +
    'Any images associated with this document will NOT be copied to this new directory.' + sLineBreak +
    'If you want to keep these images, you will need to copy them to the new directory yourself.' + sLineBreak +
    'Do you want to continue to save this document?';

implementation

{$R *.dfm}

const // Status Panel
  SP_MODIFIED = 0;
  SP_HINT     = 1;

procedure TFormMain.ActionBuildExecute(Sender: TObject);
begin
  BuildAndPreview(False);
end;

procedure TFormMain.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ActionMSDNExecute(Sender: TObject);
begin
  OpenWebsite('http://msdn.microsoft.com/en-us/library/dd371191%28v=VS.85%29.aspx');
end;

procedure TFormMain.ActionNewExecute(Sender: TObject);
begin
  NewFile(False);
end;

procedure TFormMain.ActionOpenExecute(Sender: TObject);
begin
  if (not CheckSave) then
    Exit;

  if (OpenDialog.Execute) then
    OpenFile(OpenDialog.FileName);
end;

procedure TFormMain.ActionPreviewExecute(Sender: TObject);
begin
  BuildAndPreview(True);
end;

procedure TFormMain.ActionSaveAsExecute(Sender: TObject);
var
  OrigDir, NewDir: String;
begin
  OrigDir := ExtractFilePath(FDocument.Filename);
  SaveDialog.FileName := ExtractFilename(FDocument.Filename);
  SaveDialog.InitialDir := ExtractFilePath(OrigDir);
  if (SaveDialog.Execute) then
  begin
    NewDir := ExtractFilePath(SaveDialog.FileName);
    if (OrigDir <> '') and (not SameText(OrigDir, NewDir)) then
    begin
      if (TaskMessageDlg(RS_DIFFERENT_DIR_HEADER, RS_DIFFERENT_DIR_MESSAGE,
        mtConfirmation, [mbYes, mbNo], 0, mbNo) = mrNo)
      then
        Exit;
    end;
    FDocument.SaveToFile(SaveDialog.FileName);
    UpdateCaption;
    UpdateControls;
    ClearModified;
  end;
end;

procedure TFormMain.ActionSaveExecute(Sender: TObject);
begin
  if (FDocument.Filename = '') then
    ActionSaveAs.Execute
  else
  begin
    FDocument.SaveToFile(FDocument.Filename);
    ClearModified;
  end;
end;

procedure TFormMain.ActionSettingsExecute(Sender: TObject);
begin
  ShowSettingsDialog;
end;

procedure TFormMain.ActionTutorialExecute(Sender: TObject);
begin
  OpenWebsite('http://www.bilsen.com/windowsribbon/tutorial.shtml');
end;

procedure TFormMain.ActionWebSiteExecute(Sender: TObject);
begin
  OpenWebsite('http://www.bilsen.com/windowsribbon/index.shtml');
end;

procedure TFormMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
  MemoMessages.Color := clRed;
  Log(mkError, E.Message);
  TimerRestoreLog.Enabled := True;
end;

procedure TFormMain.ApplicationEventsHint(Sender: TObject);
begin
  StatusBar.Panels[SP_HINT].Text := Application.Hint;
end;

procedure TFormMain.BuildAndPreview(const Preview: Boolean);
var
  DllInstance: THandle;
  Result: TRibbonCompileResult;
begin
  ClearLog;
  if (FModified) then
    ActionSave.Execute;
  FreeAndNil(FPreviewForm);
  Result := FCompiler.Compile(FDocument);
  if (Result = crOk) then
  begin
    if (Preview) then
    begin
      DllInstance := LoadLibraryEx(PChar(FCompiler.OutputDllPath), 0, LOAD_LIBRARY_AS_DATAFILE);
      if (DllInstance = 0) then
      begin
        Log(mkError, RS_CANNOT_LOAD_DLL);
        Exit;
      end;

      try
        FPreviewForm := TFormPreview.Create(DllInstance, FDocument);
        FPreviewForm.Show;
      except
        FreeLibrary(DllInstance);
      end;
    end;
  end
  else
  begin
    MemoMessages.Color := clRed;
//    MemoMessages.Update;
    TimerRestoreLog.Enabled := True;
//    if (Result = crRibbonCompilerError) then
//    begin
//      FFrameXmlSource.Activate;
//      PageControl.ActivePage := TabSheetXmlSource;
//    end;
  end
end;

function TFormMain.CheckSave: Boolean;
begin
  Result := True;
  if (FModified) then
  begin
    case TaskMessageDlg(RS_CHANGED_HEADER, RS_CHANGED_MESSAGE, mtConfirmation,
      mbYesNoCancel, 0, mbYes)
    of
      mrYes:
        begin
          if (ActionSave.Enabled) then
            ActionSave.Execute
          else
            ActionSaveAs.Execute;
        end;

      mrNo:
        ;

      mrCancel:
        Result := False;
    end;
  end;
end;

procedure TFormMain.ClearDocument;
begin
  FFrameCommands.ClearDocument;
  FFrameViews.ClearDocument;
  FFrameXmlSource.ClearDocument;
  FDocument.Clear;
end;

procedure TFormMain.ClearLog;
begin
  MemoMessages.Clear;
end;

procedure TFormMain.ClearModified;
begin
  FModified := False;
  StatusBar.Panels[SP_MODIFIED].Text := '';
end;

procedure TFormMain.CMShowingChanged(var Msg: TMessage);
begin
  inherited;
  if Showing and (not FInitialized) then
  begin
    FInitialized := True;
    if (not TSettings.Instance.ToolsAvailable) then
      if (TaskMessageDlg(RS_TOOLS_HEADER, RS_TOOLS_MESSAGE, mtWarning,
        [mbYes, mbNo], 0, mbYes) = mrYes)
      then
        ShowSettingsDialog;
  end;
end;

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  FDocument := TRibbonDocument.Create;
  FCompiler := TRibbonCompiler.Create;
  FCompiler.OnMessage := RibbonCompilerMessage;

  FFrameCommands := TFrameCommands.Create(Self);
  FFrameCommands.Parent := TabSheetCommands;

  FFrameViews := TFrameViews.Create(Self);
  FFrameViews.Parent := TabSheetViews;

  FFrameXmlSource := TFrameXmlSource.Create(Self);
  FFrameXmlSource.Parent := TabSheetXmlSource;

  if (ParamCount > 0) and FileExists(ParamStr(1)) then
    OpenFile(ParamStr(1))
  else
    NewFile(True);
end;

destructor TFormMain.Destroy;
begin
  FCompiler.Free;
  FDocument.Free;
  inherited;
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
  MemoMessages.SelLength := 0;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FPreviewForm) then
    FreeAndNil(FPreviewForm);
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (not CheckSave) then
    CanClose := False;
end;

procedure TFormMain.Log(const MsgType: TMessageKind;
  const Msg: String);
const
  MSG_TYPES: array [TMessageKind] of String = (
    '', 'WARNING: ', 'ERROR: ', '');
begin
  MemoMessages.Lines.Add(MSG_TYPES[MsgType] + Msg);
end;

procedure TFormMain.Modified;
begin
  if (not FModified) then
  begin
    FModified := True;
    StatusBar.Panels[SP_MODIFIED].Text := RS_MODIFIED;
  end;
end;

procedure TFormMain.NewFile(const EmptyFile: Boolean);
var
  Template: TRibbonTemplate;
  Filename, FilePath: String;
  ZipStream: TResourceStream;
  ZipReader: TZipReader;
begin
  if (not CheckSave) then
    Exit;

  if (EmptyFile) then
  begin
    Template := rtNone;
    Filename := '';
  end
  else if (not NewFileDialog(Template, Filename)) then
    Exit;

  ClearDocument;
  if (Template = rtNone) then
  begin
    if (Filename <> '') then
      FDocument.SaveToFile(Filename)
  end
  else
  begin
    Screen.Cursor := crHourGlass;
    try
      FilePath := ExtractFilePath(Filename);
      ZipReader := nil;
      ZipStream := TResourceStream.Create(HInstance, 'TEMPLATE_WORDPAD', 'ZIP');
      try
        ZipReader := TZipReader.Create(ZipStream);
        ZipReader.Extract(FilePath);
      finally
        ZipStream.Free;
        ZipReader.Free;
      end;
      RenameFile(FilePath + 'RibbonMarkup.xml', Filename);
      FDocument.LoadFromFile(Filename);
    finally
      Screen.Cursor := crDefault;
    end;
  end;

  PageControl.ActivePage := TabSheetCommands;
  ActiveControl := FFrameCommands.ListViewCommands;
  ShowDocument;
  UpdateCaption;
  UpdateControls;
  ClearModified;
end;

procedure TFormMain.OpenFile(const Filename: String);
begin
  ClearDocument;
  FDocument.LoadFromFile(Filename);
  PageControl.ActivePage := TabSheetCommands;
  ActiveControl := FFrameCommands.ListViewCommands;
  ShowDocument;
  UpdateCaption;
  UpdateControls;
  ClearModified;
end;

procedure TFormMain.OpenWebsite(const Url: String);
begin
  ShellExecute(Handle, 'open', PWideChar(Url), nil, nil, SW_SHOWNORMAL);
end;

procedure TFormMain.PageControlChange(Sender: TObject);
begin
  if (PageControl.ActivePage = TabSheetViews) then
  begin
    FFrameCommands.Deactivate;
    FFrameXmlSource.Deactivate;
    FFrameViews.Activate;
  end
  else if (PageControl.ActivePage = TabSheetCommands) then
  begin
    FFrameViews.Deactivate;
    FFrameXmlSource.Deactivate;
    FFrameCommands.Activate
  end
  else if (PageControl.ActivePage = TabSheetXmlSource) then
  begin
    if (FModified) then
      ActionSave.Execute;
    FFrameViews.Deactivate;
    FFrameCommands.Deactivate;
    FFrameXmlSource.Activate;
  end;
end;

procedure TFormMain.RibbonCompilerMessage(const Compiler: TRibbonCompiler;
  const MsgType: TMessageKind; const Msg: String);
begin
  if (MsgType = mkPipe) then
    MemoMessages.Text := MemoMessages.Text + Msg
  else
    Log(MsgType, Msg);
end;

procedure TFormMain.ShowDocument;
begin
  FFrameXmlSource.Deactivate;
  FFrameViews.Deactivate;
  FFrameCommands.Activate;
  FFrameCommands.ShowDocument(FDocument);
  FFrameViews.ShowDocument(FDocument);
  FFrameXmlSource.ShowDocument(FDocument);
end;

procedure TFormMain.ShowSettingsDialog;
var
  Form: TFormSettings;
begin
  Form := TFormSettings.Create(TSettings.Instance);
  try
    Form.ShowModal;
  finally
    Form.Release;
  end;
end;

procedure TFormMain.TimerRestoreLogTimer(Sender: TObject);
begin
  TimerRestoreLog.Enabled := False;
  MemoMessages.Color := clWindow;
end;

procedure TFormMain.UpdateCaption;
begin
  if (FDocument.Filename = '') then
    Caption := RS_UNTITLED + ' - ' + RS_RIBBON_DESIGNER
  else
    Caption := ExtractFilename(FDocument.Filename) + ' - ' + RS_RIBBON_DESIGNER;
end;

procedure TFormMain.UpdateControls;
begin
  ActionPreview.Enabled := FileExists(FDocument.Filename);
  ActionBuild.Enabled := ActionPreview.Enabled;
end;

end.
