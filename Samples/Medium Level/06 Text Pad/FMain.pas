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
  UIRibbonForm, UIRibbonCommands;

type
  TFormMain = class(TUIRibbonForm)
    StatusBar: TStatusBar;
    RichEdit: TRichEdit;
    FindDialog: TFindDialog;
    PrintDialog: TPrintDialog;
    procedure RichEditSelectionChange(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure RichEditChange(Sender: TObject);
    procedure RichEditContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
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
    procedure NotImplemented(const Args: TUICommandActionEventArgs);
    procedure NotImplemented2(const Args: TUICommandBooleanEventArgs);
    procedure PasteExecute(const Args: TUICommandActionEventArgs);
    procedure CutExecute(const Args: TUICommandActionEventArgs);
    procedure CopyExecute(const Args: TUICommandActionEventArgs);
    procedure FontChanged(const Args: TUICommandFontEventArgs);
    procedure IndentOutdentExecute(const Args: TUICommandActionEventArgs);
    procedure ListSelect(const Args: TUICommandCollectionEventArgs);
    procedure LineSpacingToggle(const Args: TUICommandBooleanEventArgs);
    procedure SpaceAfterToggle(const Args: TUICommandBooleanEventArgs);
    procedure AlignToggle(const Args: TUICommandBooleanEventArgs);
    procedure FindExecute(const Args: TUICommandActionEventArgs);
    procedure SelectAllExecute(const Args: TUICommandActionEventArgs);
    procedure UndoExecute(const Args: TUICommandActionEventArgs);
    procedure RedoExecute(const Args: TUICommandActionEventArgs);
    procedure PrintExecute(const Args: TUICommandActionEventArgs);
    procedure QuickPrintExecute(const Args: TUICommandActionEventArgs);
    procedure PrintPreviewExecute(const Args: TUICommandActionEventArgs);
    procedure ClosePrintPreviewExecute(const Args: TUICommandActionEventArgs);
    procedure ExitExecute(const Args: TUICommandActionEventArgs);
  strict protected
    procedure RibbonLoaded; override;
    procedure CommandCreated(const Sender: TUIRibbon;
      const Command: TUICommand); override;
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

procedure TFormMain.AlignToggle(const Args: TUICommandBooleanEventArgs);
var
  ParaFormat: TParaFormat2;
begin
  if (Args.Checked) then
  begin
    ParaFormat := FRichEditEx.ParaFormat;
    case Args.Command.CommandId of
      CmdAlignLeft:
        ParaFormat.wAlignment := PFA_LEFT;
      CmdAlignCenter:
        ParaFormat.wAlignment := PFA_CENTER;
      CmdAlignRight:
        ParaFormat.wAlignment := PFA_RIGHT;
      CmdAlignJustify:
        ParaFormat.wAlignment := PFA_JUSTIFY;
    end;
    ParaFormat.dwMask := PFM_ALIGNMENT;
    FRichEditEx.ParaFormat := ParaFormat;
  end;
  UpdateRibbonControls; { Update Checked state of alignment buttons }
end;

procedure TFormMain.ClosePrintPreviewExecute(const Args: TUICommandActionEventArgs);
begin
  FPrintPreviewMode := False;
  Ribbon.SetApplicationModes([0]);
  RichEdit.Enabled := True;
end;

procedure TFormMain.CommandCreated(const Sender: TUIRibbon;
  const Command: TUICommand);
begin
  inherited;
  case Command.CommandId of
    CmdPaste:
      begin
        FCmdPaste := Command as TUICommandAction;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdPaste.OnExecute := PasteExecute;
      end;

    CmdPasteSpecial:
      begin
        FCmdPasteSpecial := Command as TUICommandAction;
        FCmdPasteSpecial.SetShortCut([ssCtrl, ssAlt], 'V');
        FCmdPasteSpecial.OnExecute := NotImplemented;
      end;

    CmdCut:
      begin
        FCmdCut := Command as TUICommandAction;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdCut.OnExecute := CutExecute;
      end;

    CmdCopy:
      begin
        FCmdCopy := Command as TUICommandAction;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdCopy.OnExecute := CopyExecute;
      end;

    CmdFont:
      begin
        FCmdFont := Command as TUICommandFont;
        FCmdFont.OnChanged := FontChanged;
      end;

    CmdIndent:
      begin
        FCmdIndent := Command as TUICommandAction;
        FCmdIndent.OnExecute := IndentOutdentExecute;
      end;

    CmdOutdent:
      begin
        FCmdOutdent := Command as TUICommandAction;
        FCmdOutdent.OnExecute := IndentOutdentExecute;
      end;

    CmdList:
      begin
        FCmdList := Command as TUICommandCollection;
        PopulateListGallery;
        FCmdList.OnSelect := ListSelect;
      end;

    CmdLineSpacing1_0:
      begin
        FCmdLineSpacing1_0 := Command as TUICommandBoolean;
        FCmdLineSpacing1_0.OnToggle := LineSpacingToggle;
      end;

    CmdLineSpacing1_15:
      begin
        FCmdLineSpacing1_15 := Command as TUICommandBoolean;
        FCmdLineSpacing1_15.OnToggle := LineSpacingToggle;
      end;

    CmdLineSpacing1_5:
      begin
        FCmdLineSpacing1_5 := Command as TUICommandBoolean;
        FCmdLineSpacing1_5.OnToggle := LineSpacingToggle;
      end;

    CmdLineSpacing2:
      begin
        FCmdLineSpacing2 := Command as TUICommandBoolean;
        FCmdLineSpacing2.OnToggle := LineSpacingToggle;
      end;

    CmdLineSpacingAfter:
      begin
        FCmdSpaceAfter := Command as TUICommandBoolean;
        FCmdSpaceAfter.OnToggle := SpaceAfterToggle;
      end;

    CmdAlignLeft:
      begin
        FCmdAlignLeft := Command as TUICommandBoolean;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdAlignLeft.OnToggle := AlignToggle;
      end;

    CmdAlignCenter:
      begin
        FCmdAlignCenter := Command as TUICommandBoolean;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdAlignCenter.OnToggle := AlignToggle;
      end;

    CmdAlignRight:
      begin
        FCmdAlignRight := Command as TUICommandBoolean;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdAlignRight.OnToggle := AlignToggle;
      end;

    CmdAlignJustify:
      begin
        FCmdAlignJustify := Command as TUICommandBoolean;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdAlignJustify.OnToggle := AlignToggle;
      end;

    CmdFind:
      begin
        FCmdFind := Command as TUICommandAction;
        FCmdFind.SetShortCut([ssCtrl], 'F');
        FCmdFind.OnExecute := FindExecute;
      end;

    CmdReplace:
      begin
        FCmdReplace := Command as TUICommandAction;
        FCmdReplace.SetShortCut([ssCtrl], 'H');
        FCmdReplace.OnExecute := NotImplemented;
      end;

    CmdSelectAll:
      begin
        FCmdSelectAll := Command as TUICommandAction;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdSelectAll.OnExecute := SelectAllExecute;
      end;

    CmdUndo:
      begin
        FCmdUndo := Command as TUICommandAction;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdUndo.OnExecute := UndoExecute;
      end;

    CmdRedo:
      begin
        FCmdRedo := Command as TUICommandAction;
        { No need to set keyboard shortcut.
          TRichEdit handles this command natively. }
        FCmdRedo.OnExecute := RedoExecute;
      end;

    CmdPrint:
      begin
        FCmdPrint := Command as TUICommandAction;
        FCmdPrint.OnExecute := PrintExecute;
      end;

    CmdQuickPrint:
      begin
        FCmdQuickPrint := Command as TUICommandAction;
        FCmdQuickPrint.OnExecute := QuickPrintExecute;
      end;

    CmdPrintPreview:
      begin
        FCmdPrintPreview := Command as TUICommandAction;
        FCmdPrintPreview.OnExecute := PrintPreviewExecute;
      end;

    CmdClosePrintPreview:
      begin
        FCmdClosePrintPreview := Command as TUICommandAction;
        FCmdClosePrintPreview.OnExecute := ClosePrintPreviewExecute;
      end;

    CmdExit:
      begin
        FCmdExit := Command as TUICommandAction;
        FCmdExit.OnExecute := ExitExecute;
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
    CmdPreviousPage:
      (Command as TUICommandAction).OnExecute := NotImplemented;

    CmdViewOnePage,
    CmdViewTwoPages,
    CmdRuler,
    CmdStatusBar:
      (Command as TUICommandBoolean).OnToggle := NotImplemented2;
  end;
end;

procedure TFormMain.CopyExecute(const Args: TUICommandActionEventArgs);
begin
  RichEdit.CopyToClipboard;
end;

constructor TFormMain.Create(Owner: TComponent);
begin
  inherited;
  FRichEditEx := TRichEditEx.Create(RichEdit);
end;

procedure TFormMain.CutExecute(const Args: TUICommandActionEventArgs);
begin
  RichEdit.CutToClipboard;
end;

destructor TFormMain.Destroy;
begin
  FRichEditEx.Free;
  inherited;
end;

procedure TFormMain.ExitExecute(const Args: TUICommandActionEventArgs);
begin
  Close;
end;

procedure TFormMain.FindDialogFind(Sender: TObject);
var
  SearchTypes: TSearchTypes;
  Pos: Integer;
begin
  SearchTypes := [];
  if (frWholeWord in FindDialog.Options) then
    Include(SearchTypes, stWholeWord);
  if (frMatchCase in FindDialog.Options) then
    Include(SearchTypes, stMatchCase);
  Pos := RichEdit.FindText(FindDialog.FindText, RichEdit.SelStart + 1,
    MaxInt - RichEdit.SelStart - 1, SearchTypes);
  if (Pos >= 0) then
  begin
    RichEdit.SelStart := Pos;
    RichEdit.SelLength := Length(FindDialog.FindText);
  end;
end;

procedure TFormMain.FindExecute(const Args: TUICommandActionEventArgs);
begin
  FindDialog.Execute;
end;

procedure TFormMain.FontChanged(const Args: TUICommandFontEventArgs);
var
  CharFormat: TCharFormat2;
begin
  Args.Font.AssignTo(CharFormat);
  FRichEditEx.CharFormat := CharFormat;
end;

procedure TFormMain.IndentOutdentExecute(const Args: TUICommandActionEventArgs);
var
  ParaFormat: TParaFormat2;
begin
  ParaFormat := FRichEditEx.ParaFormat;
  if (Args.Command = FCmdIndent) then
    ParaFormat.dxStartIndent := ParaFormat.dxStartIndent + 500
  else
  if (ParaFormat.dxStartIndent >= 500) then
    ParaFormat.dxStartIndent := ParaFormat.dxStartIndent - 500;
  ParaFormat.dwMask := PFM_STARTINDENT;
  FRichEditEx.ParaFormat := ParaFormat;
end;

procedure TFormMain.LineSpacingToggle(const Args: TUICommandBooleanEventArgs);
var
  ParaFormat: TParaFormat2;
begin
  if (Args.Checked) then
  begin
    ParaFormat := FRichEditEx.ParaFormat;
    case Args.Command.CommandId of
      CmdLineSpacing1_0:
        ParaFormat.bLineSpacingRule := 0;

      CmdLineSpacing1_5:
        ParaFormat.bLineSpacingRule := 1;

      CmdLineSpacing2:
        ParaFormat.bLineSpacingRule := 2;
    else
      begin
        ParaFormat.bLineSpacingRule := 5;
        ParaFormat.dyLineSpacing := 23;
      end;
    end;
    ParaFormat.dwMask := PFM_LINESPACING;
    FRichEditEx.ParaFormat := ParaFormat;
  end;
  UpdateRibbonControls; { Update the "checked" state of the Line Spacing buttons }
end;

procedure TFormMain.ListSelect(const Args: TUICommandCollectionEventArgs);
var
  ParaFormat: TParaFormat2;
begin
  ParaFormat := FRichEditEx.ParaFormat;
  if (Args.ItemIndex < 0) then
    { "Button" part of split button has been clicked }
    ParaFormat.wNumbering := PFN_BULLET
  else
    ParaFormat.wNumbering := Args.ItemIndex;
  ParaFormat.dwMask := PFM_NUMBERING;
  FRichEditEx.ParaFormat := ParaFormat;
end;

procedure TFormMain.NotImplemented2(const Args: TUICommandBooleanEventArgs);
begin
  ShowMessage('Not implemented!');
end;

procedure TFormMain.NotImplemented(const Args: TUICommandActionEventArgs);
begin
  ShowMessage('Not implemented!');
end;

procedure TFormMain.PasteExecute(const Args: TUICommandActionEventArgs);
begin
  RichEdit.PasteFromClipboard;
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

procedure TFormMain.PrintExecute(const Args: TUICommandActionEventArgs);
begin
  if PrintDialog.Execute then
    RichEdit.Print('TextPad');
end;

procedure TFormMain.PrintPreviewExecute(const Args: TUICommandActionEventArgs);
begin
  FPrintPreviewMode := not FPrintPreviewMode;

  { Switch application modes. Show or Hide "Print preview" tab. }
  if (FPrintPreviewMode) then
    Ribbon.SetApplicationModes([1])
  else
    Ribbon.SetApplicationModes([0]);

  RichEdit.Enabled := (not FPrintPreviewMode);
end;

procedure TFormMain.QuickPrintExecute(const Args: TUICommandActionEventArgs);
begin
  RichEdit.Print('TextPad');
end;

procedure TFormMain.RedoExecute(const Args: TUICommandActionEventArgs);
begin
  FRichEditEx.Redo;
end;

procedure TFormMain.RibbonLoaded;
begin
  inherited;
  Color := ColorAdjustLuma(Ribbon.BackgroundColor, -25, False);
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

procedure TFormMain.SelectAllExecute(const Args: TUICommandActionEventArgs);
begin
  RichEdit.SelectAll;
end;

procedure TFormMain.SpaceAfterToggle(const Args: TUICommandBooleanEventArgs);
var
  ParaFormat: TParaFormat2;
begin
  ParaFormat := FRichEditEx.ParaFormat;
  if (Args.Checked) then
    ParaFormat.dySpaceAfter := 200
  else
    ParaFormat.dySpaceAfter := 0;
  ParaFormat.dwMask := PFM_SPACEAFTER;
  FRichEditEx.ParaFormat := ParaFormat;
end;

procedure TFormMain.UndoExecute(const Args: TUICommandActionEventArgs);
begin
  FRichEditEx.Undo;
end;

procedure TFormMain.UpdateRibbonControls;
var
  ParaFormat: TParaFormat2;
begin
  ParaFormat := FRichEditEx.ParaFormat;

  if Assigned(FCmdPaste) then
    FCmdPaste.Enabled := FRichEditEx.CanPaste;

  if Assigned(FCmdPasteSpecial) then
    FCmdPasteSpecial.Enabled := FRichEditEx.CanPaste;

  if Assigned(FCmdCopy) then
    FCmdCopy.Enabled := (RichEdit.SelLength > 0);

  if Assigned(FCmdCut) then
    FCmdCut.Enabled := (RichEdit.SelLength > 0);

  if Assigned(FCmdFont) then
    FCmdFont.Font.Assign(FRichEditEx.CharFormat);

  if Assigned(FCmdList) then
    FCmdList.SelectedItem := ParaFormat.wNumbering;

  if Assigned(FCmdLineSpacing1_0) then
    FCmdLineSpacing1_0.Checked := (ParaFormat.bLineSpacingRule = 0);

  if Assigned(FCmdLineSpacing1_5) then
    FCmdLineSpacing1_5.Checked := (ParaFormat.bLineSpacingRule = 1);

  if Assigned(FCmdLineSpacing2) then
    FCmdLineSpacing2.Checked := (ParaFormat.bLineSpacingRule = 2);

  if Assigned(FCmdLineSpacing1_15) then
    { This is not accurate, but good enough for a demo }
    FCmdLineSpacing1_15.Checked := (ParaFormat.bLineSpacingRule > 2);

  if Assigned(FCmdSpaceAfter) then
    { This is not accurate, but good enough for a demo }
    FCmdSpaceAfter.Checked := (ParaFormat.dySpaceAfter > 0);

  if Assigned(FCmdAlignLeft) then
    FCmdAlignLeft.Checked := (ParaFormat.wAlignment = PFA_LEFT);

  if Assigned(FCmdAlignCenter) then
    FCmdAlignCenter.Checked := (ParaFormat.wAlignment = PFA_CENTER);

  if Assigned(FCmdAlignRight) then
    FCmdAlignRight.Checked := (ParaFormat.wAlignment = PFA_RIGHT);

  if Assigned(FCmdAlignJustify) then
    FCmdAlignJustify.Checked := (ParaFormat.wAlignment = PFA_JUSTIFY);

  if Assigned(FCmdUndo) then
    FCmdUndo.Enabled := FRichEditEx.CanUndo;

  if Assigned(FCmdRedo) then
    FCmdRedo.Enabled := FRichEditEx.CanRedo;
end;

end.
