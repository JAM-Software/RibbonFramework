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
  Actions,
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
  FNewFile, System.ImageList, JvComponentBase, JvAppStorage, JvFormPlacement, JvMRUList, JvMRUManager;

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
    ActionSetResourceName: TAction;
    Setresourcename1: TMenuItem;
    ActionGenerateResourceIDs: TAction;
    AutogenerateIDsforallcommands1: TMenuItem;
    ActionGenerateCommandIDs: TAction;
    AutogenerateIDsforallresources1: TMenuItem;
    JvFormStorage: TJvFormStorage;
    MenuRecents: TMenuItem;
    JvMRUManager: TJvMRUManager;
    PopupMenuButtonOpen: TPopupMenu;
    tesst1: TMenuItem;
    ActionOpenLastUsedFileAtStartup: TAction;
    N5: TMenuItem;
    Openlastusedfileatstartup1: TMenuItem;
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
    procedure ActionSetResourceNameExecute(Sender: TObject);
    procedure ActionGenerateResourceIDsExecute(Sender: TObject);
    procedure ActionGenerateCommandIDsExecute(Sender: TObject);
    procedure JvMRUManagerClick(Sender: TObject; const RecentName, Caption: string; UserData: Integer);
    procedure JvMRUManagerAfterUpdate(Sender: TObject);
    procedure JvFormStorageBeforeSavePlacement(Sender: TObject);
    procedure ActionOpenLastUsedFileAtStartupExecute(Sender: TObject);
    procedure JvFormStorageRestorePlacement(Sender: TObject);
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
    procedure PopupMenuButtonOpenClick(Sender: TObject);
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

uses
  UITypes, System.Win.Registry, UIRibbonUtils, System.Math, DMShared;

{$R *.dfm}

const // Status Panel
  SP_MODIFIED = 0;
  SP_HINT     = 1;

  LAST_USED_FILENAME = 'Last Used Filename';

procedure TFormMain.ActionBuildExecute(Sender: TObject);
begin
  BuildAndPreview(False);
end;

procedure TFormMain.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ActionGenerateCommandIDsExecute(Sender: TObject);
var
  lCommand: TRibbonCommand;
  I: Integer;
begin
  for I := 0 to FFrameCommands.ListViewCommands.Items.Count - 1 do
  begin
    lCommand := FFrameCommands.ListViewCommands.Items[i].Data;
    if lCommand.ID = 0 then
      // Try to mimic the auto ID generation of the ribbon compiler.
      lCommand.ID := FFrameCommands.FindSmallestUnusedID(I + 2)
  end;

  FFrameCommands.RefreshSelection;
end;

procedure TFormMain.ActionGenerateResourceIDsExecute(Sender: TObject);

var
  FAutoID: integer;

  Procedure SetID(rs:TRibbonString);
  begin
    if RS.Content <>'' then begin
      RS.Id := FAutoID;
      inc(FAutoID);
    end;
  end;

  Procedure setImageID(rl:TRibbonList<TRibbonImage>);
    var
    i: integer;
  begin
    for i := 0 to rl.count-1 do begin
      rl.Items[i].Id := FAutoId;
      inc(FAutoID);
    end;
  end;

  var
  command:TRibbonCommand;
  i, maxID :integer;
  s: string;

begin
  {First work out the maximum no of command ids that will be required}
  MaxID := FFrameCommands.ListViewCommands.items.count-1;
  for I := 0 to FFrameCommands.ListViewCommands.items.count-1 do begin
    Command := FFrameCommands.ListViewCommands.Items[i].Data;
    with command do begin
      if LabelTitle.Content <> '' then
        inc(MaxID);
      if LabelDescription.Content <> '' then
        inc(MaxID);
      if TooltipTitle.Content <> '' then
        inc(MaxID);
      if TooltipDescription.Content <> '' then
        inc(MaxID);
      if Keytip.Content <> '' then
        inc(MaxID);
      inc(MaxID, SmallImages.Count+LargeImages.Count
        +SmallHighContrastImages.Count+LargeHighContrastImages.Count);
    end;
  end;

  if inputQuery('ID Number', 'Enter the starting ID number between 2 & '+(59999-MaxID).ToString, s) then begin
    if not tryStrToInt(s, FAutoID) then begin
      raise Exception.Create('Invald integer value');
      exit;
    end;
  end
  else
    exit;

  if (FAutoID < 2) or (FAUtoID + MaxID > 59999) then begin
    raise Exception.Create(FAutoID.ToString + 'is an invlid starting ID. '
    +'Must be a number between 2 and < '+ (59999 - MaxID).ToString);
    exit;
  end;

  for I := 0 to FFrameCommands.ListViewCommands.items.count-1 do begin
    Command := FFrameCommands.ListViewCommands.Items[i].Data;

    with command do begin
      
      SetID(LabelTitle);
      SetID(LabelDescription);
      SetID(TooltipTitle);
      SetID(TooltipDescription);
      SetID(Keytip);

      setImageID(SmallImages);
      setImageID(LargeImages);
      setImageID(SmallHighContrastImages);
      setImageID(LargeHighContrastImages);
    end;
  end;

  FFrameCommands.RefreshSelection;
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

  if (OpenDialog.Execute) then begin
    OpenFile(OpenDialog.FileName);
    JvMRUManager.Add(OpenDialog.FileName, 0);
  end;
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

procedure TFormMain.ActionSetResourceNameExecute(Sender: TObject);
var
  lUserInput: string;
begin
  lUserInput := InputBox('Enter resource name', 'Please enter a resource name that is used for this ribbon markup', FDocument.Application.ResourceName);
  if lUserInput <> '' then
    FDocument.Application.ResourceName := lUserInput;
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
  // Create DLL only if a preview is requested
  Result := FCompiler.Compile(FDocument, False, FDocument.Application.ResourceName, Preview);

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
        FPreviewForm := TFormPreview.Create(DllInstance, FDocument, FDocument.Application.ResourceName);
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
      if (TaskMessageDlg(RS_TOOLS_HEADER, RS_TOOLS_MESSAGE + sLineBreak + RS_TOOLS_SETUP, mtWarning,
        [mbYes, mbNo], 0, mbYes) = mrYes)
      then
        ShowSettingsDialog;
  end;
end;

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  // Write the path to the Ribbon Designer to the registry, so that the designtime package knows where to find it. Issue #22
  With TRegistry.Create do
    try
      OpenKey(cRegistryPath, True);
      WriteString(cRegistryKeyDesigner, ParamStr(0));
    finally
      Free;
    end;

  FDocument := TRibbonDocument.Create;
  FCompiler := TRibbonCompiler.Create;
  FCompiler.OnMessage := RibbonCompilerMessage;

  FFrameCommands := TFrameCommands.Create(Self);
  FFrameCommands.Parent := TabSheetCommands;

  FFrameViews := TFrameViews.Create(Self);
  FFrameViews.Parent := TabSheetViews;

  FFrameXmlSource := TFrameXmlSource.Create(Self);
  FFrameXmlSource.Parent := TabSheetXmlSource;

  // Handle command line options
  if (ParamCount > 0) and FileExists(ParamStr(1)) then begin // File passed at the command line?
    OpenFile(ParamStr(1));
    if FindCmdLineSwitch('BUILD') then begin
      ActionBuild.Execute();
      Application.ShowMainForm := False;
      Application.Terminate();
    end// if /BUILD
    else begin
      NewFile(True);
    end;//else
  end; // if file passed
end;

destructor TFormMain.Destroy;
begin
  FCompiler.Free;
  FDocument.Free;
  inherited;
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
  JvMRUManager.Load;
  MemoMessages.SelLength := 0;
  if ParamStr(1) <> '' then
    OpenFile(ParamStr(1));
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

procedure TFormMain.JvMRUManagerClick(Sender: TObject; const RecentName, Caption: string; UserData: Integer);
begin
  OpenFile(RecentName);
end;

procedure TFormMain.JvMRUManagerAfterUpdate(Sender: TObject);
begin
  var Tag := 0;
  PopupMenuButtonOpen.Items.Clear;
  for var s in JvMRUManager.Strings do begin
    var MenuItem := TMenuItem.Create(PopupMenuButtonOpen);
    MenuItem.Caption := s;
    MenuItem.Tag := Tag;
    MenuItem.OnClick := PopupMenuButtonOpenClick;
    PopupMenuButtonOpen.Items.Add(MenuItem);
    Inc(Tag);
  end;
end;

procedure TFormMain.PopupMenuButtonOpenClick(Sender: TObject);
begin
  OpenFile(JvMRUManager.Strings[(Sender as TMenuItem).Tag]);
end;

procedure TFormMain.JvFormStorageBeforeSavePlacement(Sender: TObject);
begin
  if (FDocument <> nil) and FileExists(FDocument.Filename) then begin
    JvFormStorage.WriteString(LAST_USED_FILENAME, FDocument.Filename);
  end else begin
    JvFormStorage.DeleteValue(LAST_USED_FILENAME)
  end;
end;

procedure TFormMain.ActionOpenLastUsedFileAtStartupExecute(Sender: TObject);
begin
  ActionOpenLastUsedFileAtStartup.Checked := not ActionOpenLastUsedFileAtStartup.Checked;
end;

procedure TFormMain.JvFormStorageRestorePlacement(Sender: TObject);
begin
  if ActionOpenLastUsedFileAtStartup.Checked then begin
    var FileName := JvFormStorage.ReadString(LAST_USED_FILENAME, '');
    if FileExists(FileName) then begin
      OpenFile(FileName);
    end;
  end;
end;

end.
