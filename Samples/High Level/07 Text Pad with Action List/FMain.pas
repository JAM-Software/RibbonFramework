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
    ActionFind: TSearchFind;
    CmdSelectAll: TEditSelectAll;
    ActionUndo: TAction;
    ActionRedo: TAction;
    ActionPrint: TPrintDlg;
    ActionQuickPrint: TAction;
    ActionPrintPreview: TAction;
    ActionClosePrintPreview: TAction;
    ActionExit: TFileExit;
    Ribbon: TUIRibbon;
    CmdPasteSpecial: TAction;
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
    procedure ActionFindAccept(Sender: TObject);
    procedure ActionUndoExecute(Sender: TObject);
    procedure ActionRedoExecute(Sender: TObject);
    procedure ActionPrintAccept(Sender: TObject);
    procedure ActionQuickPrintExecute(Sender: TObject);
    procedure ActionPrintPreviewExecute(Sender: TObject);
    procedure ActionClosePrintPreviewExecute(Sender: TObject);
    procedure RibbonLoaded(Sender: TObject);
    procedure CommandCreated(const Sender: TUIRibbon; const Command: TUICommand);
  private
    { Private declarations }
    FRichEditEx: TRichEditEx;
    FCmdPasteSpecial: TUICommandAction;
    FCmdFont: TUICommandFont;
    FCmdAlignLeft: TUICommandBoolean;
    FCmdAlignCenter: TUICommandBoolean;
    FCmdAlignRight: TUICommandBoolean;
    FCmdAlignJustify: TUICommandBoolean;
    FCmdFind: TUICommandAction;
    FCmdReplace: TUICommandAction;
    FCmdUndo: TUICommandAction;
    FCmdRedo: TUICommandAction;
    FCmdPrint: TUICommandAction;
    FCmdQuickPrint: TUICommandAction;
    FCmdPrintPreview: TUICommandAction;
    FCmdClosePrintPreview: TUICommandAction;
    FCmdExit: TUICommandAction;
    FPrintPreviewMode: Boolean;
    procedure UpdateRibbonControls;
    procedure PopulateListGallery;
    procedure FontChanged(const Args: TUICommandFontEventArgs);
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

procedure TFormMain.ActionClosePrintPreviewExecute(Sender: TObject);
begin
  FPrintPreviewMode := False;
  Ribbon.SetApplicationModes([0]);
  RichEdit.Enabled := True;
end;

procedure TFormMain.ActionFindAccept(Sender: TObject);
var
  SearchTypes: TSearchTypes;
  Pos: Integer;
begin
  SearchTypes := [];
  if (frWholeWord in ActionFind.Dialog.Options) then
    Include(SearchTypes, stWholeWord);
  if (frMatchCase in ActionFind.Dialog.Options) then
    Include(SearchTypes, stMatchCase);
  Pos := RichEdit.FindText(ActionFind.Dialog.FindText, RichEdit.SelStart + 1,
    MaxInt - RichEdit.SelStart - 1, SearchTypes);
  if (Pos >= 0) then
  begin
    RichEdit.SelStart := Pos;
    RichEdit.SelLength := Length(ActionFind.Dialog.FindText);
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

procedure TFormMain.ActionPrintAccept(Sender: TObject);
begin
  RichEdit.Print('TextPad');
end;

procedure TFormMain.ActionPrintPreviewExecute(Sender: TObject);
begin
  FPrintPreviewMode := not FPrintPreviewMode;

  { Switch application modes. Show or Hide "Print preview" tab. }
  if (FPrintPreviewMode) then
    Ribbon.SetApplicationModes([1])
  else
    Ribbon.SetApplicationModes([0]);

  RichEdit.Enabled := (not FPrintPreviewMode);
end;

procedure TFormMain.ActionQuickPrintExecute(Sender: TObject);
begin
  RichEdit.Print('TextPad');
end;

procedure TFormMain.ActionRedoExecute(Sender: TObject);
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

procedure TFormMain.ActionUndoExecute(Sender: TObject);
begin
  FRichEditEx.Undo;
end;

procedure TFormMain.CommandCreated(const Sender: TUIRibbon;
  const Command: TUICommand);
begin
  if Command = CmdList.UICommand then
    PopulateListGallery;

  case Command.CommandId of
    CmdFont:
      begin
        FCmdFont := Command as TUICommandFont;
        FCmdFont.OnChanged := FontChanged;
      end;

    CmdFind:
      begin
        FCmdFind := Command as TUICommandAction;
        FCmdFind.SetShortCut([ssCtrl], 'F');
        FCmdFind.ActionLink.Action := ActionFind;
      end;

    CmdReplace:
      begin
        FCmdReplace := Command as TUICommandAction;
        FCmdReplace.SetShortCut([ssCtrl], 'H');
        FCmdReplace.ActionLink.Action := ActionNotImplemented;
      end;

    CmdUndo:
      begin
        FCmdUndo := Command as TUICommandAction;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdUndo.ActionLink.Action := ActionUndo;
      end;

    CmdRedo:
      begin
        FCmdRedo := Command as TUICommandAction;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdRedo.ActionLink.Action := ActionRedo;
      end;

    CmdPrint:
      begin
        FCmdPrint := Command as TUICommandAction;
        FCmdPrint.ActionLink.Action := ActionPrint;
      end;

    CmdQuickPrint:
      begin
        FCmdQuickPrint := Command as TUICommandAction;
        FCmdQuickPrint.ActionLink.Action := ActionQuickPrint;
      end;

    CmdPrintPreview:
      begin
        FCmdPrintPreview := Command as TUICommandAction;
        FCmdPrintPreview.ActionLink.Action := ActionPrintPreview;
      end;

    CmdClosePrintPreview:
      begin
        FCmdClosePrintPreview := Command as TUICommandAction;
        FCmdClosePrintPreview.ActionLink.Action := ActionClosePrintPreview;
      end;

    CmdExit:
      begin
        FCmdExit := Command as TUICommandAction;
        FCmdExit.ActionLink.Action := ActionExit;
      end;

    { These commands are not implemented in this demo }
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
    CmdNew,
    CmdOpen,
    CmdSave,
    CmdSaveAs,
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

procedure TFormMain.FontChanged(const Args: TUICommandFontEventArgs);
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

  if Assigned(FCmdPasteSpecial) then
    FCmdPasteSpecial.Enabled := FRichEditEx.CanPaste;

  CmdCopy.Enabled := (RichEdit.SelLength > 0);
  CmdCut.Enabled := (RichEdit.SelLength > 0);

  if Assigned(FCmdFont) then
    FCmdFont.Font.Assign(FRichEditEx.CharFormat);

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

  ActionUndo.Enabled := FRichEditEx.CanUndo;
  ActionRedo.Enabled := FRichEditEx.CanRedo;
end;

end.
