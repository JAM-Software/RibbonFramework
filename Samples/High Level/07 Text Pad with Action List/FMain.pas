unit FMain;

{ NOTE: Make sure you use the Windows SDK Resource Compiler instead of the
  Borland Resource Compiler. The Borland Resource Compiler does not handle
  32-bitmaps.
  To select the Windows SDK Resource Compiler, open the Project Options and
  go to the Resource Compiler page. There set "Resource compiler to use" to
  the Windows SDK Resource Compiler. }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, GraphUtil, StdCtrls, RichEdit, RichEditEx, UIRibbon,
  UIRibbonCommands, UIRibbonActions, ActnList, StdActns, System.Actions;

type
  TFormMain = class(TForm)
    StatusBar: TStatusBar;
    RichEdit: TRichEdit;
    Actions: TActionList;
    CmdCut: TEditCut;
    CmdCopy: TEditCopy;
    CmdPaste: TEditPaste;
    ActionNotImplemented: TAction;
    CmdIndent: TAction;
    CmdOutdent: TAction;
    CmdList: TRibbonCollectionAction;
    CmdLineSpacing10: TAction;
    CmdLineSpacing115: TAction;
    CmdLineSpacing15: TAction;
    CmdLineSpacing20: TAction;
    CmdLineSpacingAfter: TAction;
    CmdAlignLeft: TAction;
    CmdAlignCenter: TAction;
    CmdAlignRight: TAction;
    CmdAlignJustify: TAction;
    CmdFind: TSearchFind;
    CmdSelectAll: TEditSelectAll;
    CmdUndo: TAction;
    CmdRedo: TAction;
    CmdPrint: TPrintDlg;
    CmdQuickPrint: TAction;
    CmdPrintPreview: TAction;
    CmdClosePrintPreview: TAction;
    CmdExit: TFileExit;
    Ribbon: TUIRibbon;
    CmdPasteSpecial: TAction;
    CmdFont: TRibbonFontAction;
    CmdOpen: TFileOpen;
    CmdRecentItems: TRecentItemAction;
    CmdSave: TAction;
    CmdSaveAs: TFileSaveAs;
    CmdNew: TAction;
    procedure RichEditSelectionChange(Sender: TObject);
    procedure RichEditChange(Sender: TObject);
    procedure RichEditContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ActionNotImplementedExecute(Sender: TObject);
    procedure ActionIndentOutdentExecute(Sender: TObject);
    procedure CmdListExecute(Sender: TObject);
    procedure ActionLineSpacingExecute(Sender: TObject);
    procedure CmdLineSpacingAfterExecute(Sender: TObject);
    procedure CmdAlignExecute(Sender: TObject);
    procedure CmdFindAccept(Sender: TObject);
    procedure CmdUndoExecute(Sender: TObject);
    procedure CmdRedoExecute(Sender: TObject);
    procedure CmdPrintAccept(Sender: TObject);
    procedure CmdQuickPrintExecute(Sender: TObject);
    procedure CmdPrintPreviewExecute(Sender: TObject);
    procedure CmdClosePrintPreviewExecute(Sender: TObject);
    procedure CmdFontChanged(const Args: TUICommandFontEventArgs);
    procedure RibbonLoaded(Sender: TObject);
    procedure CommandCreated(const Sender: TUIRibbon; const Command: TUICommand);
    procedure CmdOpenAccept(Sender: TObject);
    procedure CmdRecentItemsExecute(Sender: TObject);
    procedure CmdSaveAsAccept(Sender: TObject);
    procedure CmdNewExecute(Sender: TObject);
    procedure CmdSaveExecute(Sender: TObject);
  private
    { Private declarations }
    FRichEditEx: TRichEditEx;
    FPrintPreviewMode: Boolean;
    /// The path of the currently opened file
    FCurrentfilePath: String;
    procedure UpdateRibbonControls;
    procedure PopulateListGallery;
    /// Loads a file into the editor and adds the file path to the recent items.
    /// <param name="pFilePath">The path of the file that should be loaded.</param>
    procedure Load(const pFilePath: string);
    /// Loads the editor contents to a file.
    /// <param name="pFilePath">The path of the file to that the editor content should be saved.</param>
    procedure Save(const pFilePath: string);
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}


uses
  RibbonMarkup;

{ TFormMain }

procedure TFormMain.CmdAlignExecute(Sender: TObject);
var
  ParaFormat: TParaFormat2;
begin
  ParaFormat := FRichEditEx.ParaFormat;
  if (Sender = CmdAlignLeft) then
    ParaFormat.wAlignment := PFA_LEFT
  else if (Sender = CmdAlignCenter) then
    ParaFormat.wAlignment := PFA_CENTER
  else if (Sender = CmdAlignRight) then
    ParaFormat.wAlignment := PFA_RIGHT
  else if (Sender = CmdAlignJustify) then
    ParaFormat.wAlignment := PFA_JUSTIFY;
  ParaFormat.dwMask := PFM_ALIGNMENT;
  FRichEditEx.ParaFormat := ParaFormat;
  UpdateRibbonControls; { Update Checked state of alignment buttons }
end;

procedure TFormMain.CmdClosePrintPreviewExecute(Sender: TObject);
begin
  FPrintPreviewMode := False;
  Ribbon.SetApplicationModes([0]);
  RichEdit.Enabled := True;
end;

procedure TFormMain.CmdFindAccept(Sender: TObject);
var
  SearchTypes: TSearchTypes;
  Pos: Integer;
begin
  SearchTypes := [];
  if (frWholeWord in CmdFind.Dialog.Options) then
    Include(SearchTypes, stWholeWord);
  if (frMatchCase in CmdFind.Dialog.Options) then
    Include(SearchTypes, stMatchCase);
  Pos := RichEdit.FindText(CmdFind.Dialog.FindText, RichEdit.SelStart + 1,
    MaxInt - RichEdit.SelStart - 1, SearchTypes);
  if (Pos >= 0) then
  begin
    RichEdit.SelStart := Pos;
    RichEdit.SelLength := Length(CmdFind.Dialog.FindText);
  end;
end;

procedure TFormMain.ActionIndentOutdentExecute(Sender: TObject);
var
  ParaFormat: TParaFormat2;
begin
  ParaFormat := FRichEditEx.ParaFormat;
  if (Sender = CmdIndent) then
    ParaFormat.dxStartIndent := ParaFormat.dxStartIndent + 500
  else if (ParaFormat.dxStartIndent >= 500) then
    ParaFormat.dxStartIndent := ParaFormat.dxStartIndent - 500;
  ParaFormat.dwMask := PFM_STARTINDENT;
  FRichEditEx.ParaFormat := ParaFormat;
end;

procedure TFormMain.ActionLineSpacingExecute(Sender: TObject);
var
  ParaFormat: TParaFormat2;
begin
  ParaFormat := FRichEditEx.ParaFormat;
  if (Sender = CmdLineSpacing10) then
    ParaFormat.bLineSpacingRule := 0
  else if (Sender = CmdLineSpacing15) then
    ParaFormat.bLineSpacingRule := 1
  else if (Sender = CmdLineSpacing20) then
    ParaFormat.bLineSpacingRule := 2
  else
  begin
    ParaFormat.bLineSpacingRule := 5;
    ParaFormat.dyLineSpacing := 23;
  end;
  ParaFormat.dwMask := PFM_LINESPACING;
  FRichEditEx.ParaFormat := ParaFormat;
  UpdateRibbonControls; { Update the "checked" state of the Line Spacing buttons }
end;

procedure TFormMain.CmdListExecute(Sender: TObject);
var
  ParaFormat: TParaFormat2;
begin
  ParaFormat := FRichEditEx.ParaFormat;
   if (CmdList.UICommand.SelectedItem < 0) then begin // Has the "Button" part of split button been clicked?
    // toggle
    if ParaFormat.wNumbering > 0 then
      ParaFormat.wNumbering := 0
    else
      ParaFormat.wNumbering := PFN_BULLET
  end//if < 0
  else
    ParaFormat.wNumbering := CmdList.UICommand.SelectedItem;
  ParaFormat.dwMask := PFM_NUMBERING;
  FRichEditEx.ParaFormat := ParaFormat;
end;

procedure TFormMain.ActionNotImplementedExecute(Sender: TObject);
begin
   ShowMessage('Not implemented!');
end;

procedure TFormMain.CmdPrintAccept(Sender: TObject);
begin
  RichEdit.Print('TextPad');
end;

procedure TFormMain.CmdPrintPreviewExecute(Sender: TObject);
begin
  FPrintPreviewMode := not FPrintPreviewMode;

  { Switch application modes. Show or Hide "Print preview" tab. }
  if (FPrintPreviewMode) then
    Ribbon.SetApplicationModes([1])
  else
    Ribbon.SetApplicationModes([0]);

  RichEdit.Enabled := (not FPrintPreviewMode);
end;

procedure TFormMain.CmdQuickPrintExecute(Sender: TObject);
begin
  RichEdit.Print('TextPad');
end;

procedure TFormMain.CmdRedoExecute(Sender: TObject);
begin
  FRichEditEx.Redo;
end;

procedure TFormMain.CmdLineSpacingAfterExecute(Sender: TObject);
var
  ParaFormat: TParaFormat2;
begin
  ParaFormat := FRichEditEx.ParaFormat;
  if (CmdLineSpacingAfter.Checked) then
    ParaFormat.dySpaceAfter := 0
  else
    ParaFormat.dySpaceAfter := 200;
  ParaFormat.dwMask := PFM_SPACEAFTER;
  FRichEditEx.ParaFormat := ParaFormat;
end;

procedure TFormMain.CmdUndoExecute(Sender: TObject);
begin
  FRichEditEx.Undo;
end;

procedure TFormMain.CommandCreated(const Sender: TUIRibbon; const Command: TUICommand);
begin
  if Command = CmdList.UICommand then
    PopulateListGallery;

  case Command.CommandId of
    { These commands are not implemented in this demo }
    CmdReplace,
    CmdParagraph,
    CmdInsertPicture,
    CmdChangePicture,
    CmdResizePicture,
    CmdPaintDrawing,
    CmdDateAndTime,
    CmdInsertObject,
    CmdZoomIn,
    CmdZoomOut,
    CmdZoom100Percent,
    CmdWrapToWindow,
    CmdWrapToRuler,
    CmdNoWrap,
    CmdInches,
    CmdCentimeters,
    CmdPicas,
    CmdPoints,
    CmdHelp,
    CmdRichTextDocument,
    CmdOfficeOpenXMLDocument,
    CmdOpenDocumentText,
    CmdPlainTextDocument,
    CmdOtherFormats,
    CmdPageSetup,
    CmdEmail,
    CmdAbout,
    CmdNextPage,
    CmdPreviousPage,
    CmdViewOnePage,
    CmdViewTwoPages,
    CmdRuler,
    CmdStatusBar:
      Command.ActionLink.Action := ActionNotImplemented;
  end;
end;

constructor TFormMain.Create(Owner: TComponent);
begin
  inherited;
  FRichEditEx := TRichEditEx.Create(RichEdit);
end;

destructor TFormMain.Destroy;
begin
  FRichEditEx.Free;
  inherited;
end;

procedure TFormMain.CmdRecentItemsExecute(Sender: TObject);
begin
  Load((Sender as TRecentItemAction).UICommand.GetSelected.LabelText);
end;

procedure TFormMain.CmdFontChanged(const Args: TUICommandFontEventArgs);
var
  CharFormat: TCharFormat2;
begin
  Args.Font.AssignTo(CharFormat);
  FRichEditEx.CharFormat := CharFormat;
end;

procedure TFormMain.PopulateListGallery;
const
  RESOURCE_NAMES: array [0..6] of String = ('NONE', 'BULLETS', 'NUMBERED',
    'LOWERCASE', 'UPPERCASE', 'ROMANLOWER', 'ROMANUPPER');
  LABELS: array [0..6] of String = ('None', 'Bullet', 'Numbering',
    'Alphabet - Lower case', 'Alphabet - Upper case',
    'Roman Numeral - Lower case', 'Roman Numeral - Upper case');
var
  I, Dpi: Integer;
  ResourceName: String;
  Item: TUIGalleryCollectionItem;
begin
  Dpi := Screen.PixelsPerInch;
  if (Dpi> 120) then
    Dpi := 144
  else if (Dpi > 96) then
    Dpi := 120
  else
    Dpi := 96;

  for I := 0 to 6 do
  begin
    ResourceName := Format('LIST_%s_%.3d', [RESOURCE_NAMES[I], Dpi]);
    Item := TUIGalleryCollectionItem.Create;
    Item.LabelText := LABELS[I];
    Item.Image := TUIImage.Create(HInstance, ResourceName);
    CmdList.UICommand.Items.Add(Item);
  end;
end;

procedure TFormMain.RibbonLoaded;
begin
  inherited;
  Color := ColorAdjustLuma(Ribbon.BackgroundColor, -25, False);
//  CmdCut.Control := RichEdit;
//  CmdCopy.Control := RichEdit;
//  CmdPaste.Control := RichEdit;
//  CmdSelectAll.Control := RichEdit;
  UpdateRibbonControls;
end;

procedure TFormMain.RichEditChange(Sender: TObject);
begin
  UpdateRibbonControls;
end;

procedure TFormMain.RichEditContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  Ribbon.ShowContextPopup(CmdContextPopupEditText);
end;

procedure TFormMain.RichEditSelectionChange(Sender: TObject);
begin
  UpdateRibbonControls;
end;

procedure TFormMain.UpdateRibbonControls;
var
  ParaFormat: TParaFormat2;
begin
  ParaFormat := FRichEditEx.ParaFormat;

  CmdPaste.Enabled := FRichEditEx.CanPaste;

  CmdPasteSpecial.Enabled := FRichEditEx.CanPaste;

  CmdCopy.Enabled := (RichEdit.SelLength > 0);
  CmdCut.Enabled := (RichEdit.SelLength > 0);

  if Assigned(CmdFont.UICommand) then
    CmdFont.UICommand.Font.Assign(FRichEditEx.CharFormat);

  if Assigned(CmdList.UICommand) then
    CmdList.UICommand.SelectedItem := ParaFormat.wNumbering;

  CmdLineSpacing10.Checked := (ParaFormat.bLineSpacingRule = 0);
  CmdLineSpacing15.Checked := (ParaFormat.bLineSpacingRule = 1);
  CmdLineSpacing20.Checked := (ParaFormat.bLineSpacingRule = 2);
  CmdLineSpacing115.Checked := (ParaFormat.bLineSpacingRule > 2);

  { This is not accurate, but good enough for a demo }
  CmdLineSpacingAfter.Checked := (ParaFormat.dySpaceAfter > 0);

  CmdAlignLeft.Checked := (ParaFormat.wAlignment = PFA_LEFT);
  CmdAlignCenter.Checked := (ParaFormat.wAlignment = PFA_CENTER);
  CmdAlignRight.Checked := (ParaFormat.wAlignment = PFA_RIGHT);
  CmdAlignJustify.Checked := (ParaFormat.wAlignment = PFA_JUSTIFY);

  CmdUndo.Enabled := FRichEditEx.CanUndo;
  CmdRedo.Enabled := FRichEditEx.CanRedo;
end;

procedure TFormMain.CmdNewExecute(Sender: TObject);
begin
  RichEdit.Clear();
  fCurrentFilePath := '';
end;

procedure TFormMain.CmdOpenAccept(Sender: TObject);
begin
  Load(CmdOpen.Dialog.FileName);
end;

procedure TFormMain.CmdSaveAsAccept(Sender: TObject);
begin
  Save(CmdSaveAs.Dialog.FileName);
end;

procedure TFormMain.CmdSaveExecute(Sender: TObject);
begin
  if FCurrentfilePath.IsEmpty then
    CmdSaveAs.Execute()
  else
    Save(FCurrentfilePath);
end;

procedure TFormMain.Load(const pFilePath: string);
begin
  RichEdit.Lines.LoadFromFile(pFilePath);
  Ribbon.RecentItems.Add(pFilePath);
  FCurrentfilePath := pFilePath;
end;

procedure TFormMain.Save(const pFilePath: string);
begin
  RichEdit.Lines.SaveToFile(pFilePath);
  FCurrentfilePath := pFilePath;
end;




end.
