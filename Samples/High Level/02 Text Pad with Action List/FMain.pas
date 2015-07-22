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
    ActionCut: TEditCut;
    ActionCopy: TEditCopy;
    ActionPaste: TEditPaste;
    ActionNotImplemented: TAction;
    ActionIndent: TAction;
    ActionOutdent: TAction;
    ActionList: TAction;
    ActionLineSpacing10: TAction;
    ActionLineSpacing115: TAction;
    ActionLineSpacing15: TAction;
    ActionLineSpacing20: TAction;
    ActionSpaceAfter: TAction;
    ActionAlignLeft: TAction;
    ActionAlignCenter: TAction;
    ActionAlignRight: TAction;
    ActionAlignJustify: TAction;
    ActionFind: TSearchFind;
    ActionSelectAll: TEditSelectAll;
    ActionUndo: TAction;
    ActionRedo: TAction;
    ActionPrint: TPrintDlg;
    ActionQuickPrint: TAction;
    ActionPrintPreview: TAction;
    ActionClosePrintPreview: TAction;
    ActionExit: TFileExit;
    Ribbon: TUIRibbon;
    ActionPasteSpecial: TAction;
    procedure RichEditSelectionChange(Sender: TObject);
    procedure RichEditChange(Sender: TObject);
    procedure RichEditContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ActionNotImplementedExecute(Sender: TObject);
    procedure ActionIndentOutdentExecute(Sender: TObject);
    procedure ActionListExecute(Sender: TObject);
    procedure ActionLineSpacingExecute(Sender: TObject);
    procedure ActionSpaceAfterExecute(Sender: TObject);
    procedure ActionAlignExecute(Sender: TObject);
    procedure ActionFindAccept(Sender: TObject);
    procedure ActionUndoExecute(Sender: TObject);
    procedure ActionRedoExecute(Sender: TObject);
    procedure ActionPrintAccept(Sender: TObject);
    procedure ActionQuickPrintExecute(Sender: TObject);
    procedure ActionPrintPreviewExecute(Sender: TObject);
    procedure ActionClosePrintPreviewExecute(Sender: TObject);
    procedure RibbonLoaded(Sender: TObject);
    procedure RibbonCommandCreate(const Sender: TUIRibbon;
      const Command: TUICommand);
    procedure ActionPasteSpecialUpdate(Sender: TObject);
    procedure ActionUndoUpdate(Sender: TObject);
    procedure ActionRedoUpdate(Sender: TObject);
  private
    { Private declarations }
    FRichEditEx: TRichEditEx;
    FCmdPaste: TUICommandAction;
    FCmdPasteSpecial: TUICommandAction;
    FCmdCut: TUICommandAction;
    FCmdCopy: TUICommandAction;
    FCmdFont: TUICommandFont;
    FCmdOutdent: TUICommandAction;
    FCmdIndent: TUICommandAction;
    FCmdList: TUICommandCollection;
    FCmdLineSpacing1_0: TUICommandBoolean;
    FCmdLineSpacing1_15: TUICommandBoolean;
    FCmdLineSpacing1_5: TUICommandBoolean;
    FCmdLineSpacing2: TUICommandBoolean;
    FCmdSpaceAfter: TUICommandBoolean;
    FCmdAlignLeft: TUICommandBoolean;
    FCmdAlignCenter: TUICommandBoolean;
    FCmdAlignRight: TUICommandBoolean;
    FCmdAlignJustify: TUICommandBoolean;
    FCmdFind: TUICommandAction;
    FCmdReplace: TUICommandAction;
    FCmdSelectAll: TUICommandAction;
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
{$R 'Ribbon\RibbonMarkup.res'}

uses
  RibbonMarkup;

{ TFormMain }

procedure TFormMain.ActionAlignExecute(Sender: TObject);
var
  ParaFormat: TParaFormat2;
begin
  ParaFormat := FRichEditEx.ParaFormat;
  if (Sender = ActionAlignLeft) then
    ParaFormat.wAlignment := PFA_LEFT
  else if (Sender = ActionAlignCenter) then
    ParaFormat.wAlignment := PFA_CENTER
  else if (Sender = ActionAlignRight) then
    ParaFormat.wAlignment := PFA_RIGHT
  else if (Sender = ActionAlignJustify) then
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
  if (Sender = ActionIndent) then
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
  if (Sender = ActionLineSpacing10) then
    ParaFormat.bLineSpacingRule := 0
  else if (Sender = ActionLineSpacing15) then
    ParaFormat.bLineSpacingRule := 1
  else if (Sender = ActionLineSpacing20) then
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

procedure TFormMain.ActionListExecute(Sender: TObject);
var
  ParaFormat: TParaFormat2;
begin
  ParaFormat := FRichEditEx.ParaFormat;
  if (FCmdList.SelectedItem < 0) then
    { "Button" part of split button has been clicked }
    ParaFormat.wNumbering := PFN_BULLET
  else
    ParaFormat.wNumbering := FCmdList.SelectedItem;
  ParaFormat.dwMask := PFM_NUMBERING;
  FRichEditEx.ParaFormat := ParaFormat;
end;

procedure TFormMain.ActionNotImplementedExecute(Sender: TObject);
begin
  ShowMessage('Not implemented!');
end;

procedure TFormMain.ActionPasteSpecialUpdate(Sender: TObject);
begin
  FCmdPasteSpecial.Enabled := FRichEditEx.CanPaste;
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

procedure TFormMain.ActionRedoUpdate(Sender: TObject);
begin
  ActionRedo.Enabled := FRichEditEx.CanRedo;
end;

procedure TFormMain.ActionSpaceAfterExecute(Sender: TObject);
var
  ParaFormat: TParaFormat2;
begin
  ParaFormat := FRichEditEx.ParaFormat;
  if (FCmdSpaceAfter.Checked) then
    ParaFormat.dySpaceAfter := 200
  else
    ParaFormat.dySpaceAfter := 0;
  ParaFormat.dwMask := PFM_SPACEAFTER;
  FRichEditEx.ParaFormat := ParaFormat;
end;

procedure TFormMain.ActionUndoExecute(Sender: TObject);
begin
  FRichEditEx.Undo;
end;

procedure TFormMain.ActionUndoUpdate(Sender: TObject);
begin
  ActionUndo.Enabled := FRichEditEx.CanUndo
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
    FCmdList.Items.Add(Item);
  end;
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

procedure TFormMain.RibbonCommandCreate(const Sender: TUIRibbon;
  const Command: TUICommand);
begin
  case Command.CommandId of
    CmdPaste:
      begin
        FCmdPaste := Command as TUICommandAction;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdPaste.ActionLink.Action := ActionPaste;
      end;

    CmdPasteSpecial:
      begin
        FCmdPasteSpecial := Command as TUICommandAction;
        FCmdPasteSpecial.SetShortCut([ssCtrl, ssAlt], 'V');
        FCmdPasteSpecial.ActionLink.Action := ActionNotImplemented;
      end;

    CmdCut:
      begin
        FCmdCut := Command as TUICommandAction;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdCut.ActionLink.Action := ActionCut;
      end;

    CmdCopy:
      begin
        FCmdCopy := Command as TUICommandAction;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdCopy.ActionLink.Action := ActionCopy;
      end;

    CmdFont:
      begin
        FCmdFont := Command as TUICommandFont;
        FCmdFont.OnChanged := FontChanged;
      end;

    CmdIndent:
      begin
        FCmdIndent := Command as TUICommandAction;
        FCmdIndent.ActionLink.Action := ActionIndent;
      end;

    CmdOutdent:
      begin
        FCmdOutdent := Command as TUICommandAction;
        FCmdOutdent.ActionLink.Action := ActionOutdent;
      end;

    CmdList:
      begin
        FCmdList := Command as TUICommandCollection;
        PopulateListGallery;
        FCmdList.ActionLink.Action := ActionList;
      end;

    CmdLineSpacing1_0:
      begin
        FCmdLineSpacing1_0 := Command as TUICommandBoolean;
        FCmdLineSpacing1_0.ActionLink.Action := ActionLineSpacing10;
      end;

    CmdLineSpacing1_15:
      begin
        FCmdLineSpacing1_15 := Command as TUICommandBoolean;
        FCmdLineSpacing1_15.ActionLink.Action := ActionLineSpacing115;
      end;

    CmdLineSpacing1_5:
      begin
        FCmdLineSpacing1_5 := Command as TUICommandBoolean;
        FCmdLineSpacing1_5.ActionLink.Action := ActionLineSpacing15;
      end;

    CmdLineSpacing2:
      begin
        FCmdLineSpacing2 := Command as TUICommandBoolean;
        FCmdLineSpacing2.ActionLink.Action := ActionLineSpacing20;
      end;

    CmdLineSpacingAfter:
      begin
        FCmdSpaceAfter := Command as TUICommandBoolean;
        FCmdSpaceAfter.ActionLink.Action := ActionSpaceAfter;
      end;

    CmdAlignLeft:
      begin
        FCmdAlignLeft := Command as TUICommandBoolean;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdAlignLeft.ActionLink.Action := ActionAlignLeft;
      end;

    CmdAlignCenter:
      begin
        FCmdAlignCenter := Command as TUICommandBoolean;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdAlignCenter.ActionLink.Action := ActionAlignCenter;
      end;

    CmdAlignRight:
      begin
        FCmdAlignRight := Command as TUICommandBoolean;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdAlignRight.ActionLink.Action := ActionAlignRight;
      end;

    CmdAlignJustify:
      begin
        FCmdAlignJustify := Command as TUICommandBoolean;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdAlignJustify.ActionLink.Action := ActionAlignJustify;
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

    CmdSelectAll:
      begin
        FCmdSelectAll := Command as TUICommandAction;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdSelectAll.ActionLink.Action := ActionSelectAll;
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

procedure TFormMain.RibbonLoaded(Sender: TObject);
begin
  Color := ColorAdjustLuma(Ribbon.BackgroundColor, -25, False);
  ActionCut.Control := RichEdit;
  ActionCopy.Control := RichEdit;
  ActionPaste.Control := RichEdit;
  ActionSelectAll.Control := RichEdit;
  UpdateRibbonControls;
end;

procedure TFormMain.UpdateRibbonControls;
var
  ParaFormat: TParaFormat2;
begin
  ParaFormat := FRichEditEx.ParaFormat;

  if Assigned(FCmdFont) then
    FCmdFont.Font.Assign(FRichEditEx.CharFormat);

  if Assigned(FCmdList) then
    FCmdList.SelectedItem := ParaFormat.wNumbering;

  ActionLineSpacing10.Checked := (ParaFormat.bLineSpacingRule = 0);
  ActionLineSpacing15.Checked := (ParaFormat.bLineSpacingRule = 1);
  ActionLineSpacing20.Checked := (ParaFormat.bLineSpacingRule = 2);
  ActionLineSpacing115.Checked := (ParaFormat.bLineSpacingRule > 2);

  { This is not accurate, but good enough for a demo }
  ActionSpaceAfter.Checked := (ParaFormat.dySpaceAfter > 0);

  ActionAlignLeft.Checked := (ParaFormat.wAlignment = PFA_LEFT);
  ActionAlignCenter.Checked := (ParaFormat.wAlignment = PFA_CENTER);
  ActionAlignRight.Checked := (ParaFormat.wAlignment = PFA_RIGHT);
  ActionAlignJustify.Checked := (ParaFormat.wAlignment = PFA_JUSTIFY);
end;

end.
