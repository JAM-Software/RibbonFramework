unit FCommands;

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
  ExtCtrls,
  ComCtrls,
  ToolWin,
  StdCtrls,
  ImgList,
  ActnList,
  Actions,
  RibbonMarkup,
  UIRibbonCommands,
  FImageList, 
  Menus;

type
  TCommandSortType = (Name, Caption, None);

  TFrameCommands = class(TFrame)
    PanelCommands: TPanel;
    ToolBarCommands: TToolBar;
    ListViewCommands: TListView;
    SplitterCommands: TSplitter;
    PanelCommandProperties: TPanel;
    LabelHeader: TLabel;
    PanelProps: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    Label11: TLabel;
    Label14: TLabel;
    Label17: TLabel;
    EditKeytip: TEdit;
    EditTooltipDescription: TEdit;
    EditTooltipTitle: TEdit;
    EditDescription: TEdit;
    EditCaption: TEdit;
    EditComment: TEdit;
    EditId: TEdit;
    EditSymbol: TEdit;
    EditName: TEdit;
    UpDownId: TUpDown;
    EditCaptionId: TEdit;
    EditDescriptionId: TEdit;
    EditTooltipTitleId: TEdit;
    EditTooltipDescriptionId: TEdit;
    EditKeytipId: TEdit;
    Label18: TLabel;
    Label15: TLabel;
    Label13: TLabel;
    Label9: TLabel;
    Label6: TLabel;
    UpDownCaptionId: TUpDown;
    UpDownDescriptionId: TUpDown;
    UpDownTooltipTitleId: TUpDown;
    UpDownTooltipDescriptionId: TUpDown;
    UpDownKeyTipId: TUpDown;
    Label19: TLabel;
    Label16: TLabel;
    Label12: TLabel;
    Label10: TLabel;
    Label7: TLabel;
    EditCaptionSymbol: TEdit;
    EditDescriptionSymbol: TEdit;
    EditTooltipTitleSymbol: TEdit;
    EditTooltipDescriptionSymbol: TEdit;
    EditKeyTipSymbol: TEdit;
    PanelImages: TPanel;
    PanelHighContrastImages: TPanel;
    PanelSmallImages: TPanel;
    PanelLargeImages: TPanel;
    PanelSmallHCImages: TPanel;
    PanelLargeHCImages: TPanel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    ImageListToolbars: TImageList;
    ButtonAddCommand: TToolButton;
    ActionList: TActionList;
    ActionAddCommand: TAction;
    ActionDeleteCommand: TAction;
    ButtonDeleteCommand: TToolButton;
    PopupMenuList: TPopupMenu;
    MenuAddCommand: TMenuItem;
    MenuDeleteCommand: TMenuItem;
    ActionMoveUp: TAction;
    ActionMoveDown: TAction;
    ButtonMoveUp: TToolButton;
    ButtonMoveDown: TToolButton;
    N1: TMenuItem;
    MenuMoveDown: TMenuItem;
    MenuMoveUp: TMenuItem;
    BtnGenerateID: TButton;
    procedure ListViewCommandsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure EditNameChange(Sender: TObject);
    procedure EditSymbolChange(Sender: TObject);
    procedure EditIdChange(Sender: TObject);
    procedure EditCommentChange(Sender: TObject);
    procedure EditCaptionChange(Sender: TObject);
    procedure EditCaptionIdChange(Sender: TObject);
    procedure EditCaptionSymbolChange(Sender: TObject);
    procedure EditDescriptionChange(Sender: TObject);
    procedure EditDescriptionIdChange(Sender: TObject);
    procedure EditDescriptionSymbolChange(Sender: TObject);
    procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Integer; Direction: TUpDownDirection);
    procedure EditTooltipTitleChange(Sender: TObject);
    procedure EditTooltipTitleIdChange(Sender: TObject);
    procedure EditTooltipTitleSymbolChange(Sender: TObject);
    procedure EditTooltipDescriptionChange(Sender: TObject);
    procedure EditTooltipDescriptionIdChange(Sender: TObject);
    procedure EditTooltipDescriptionSymbolChange(Sender: TObject);
    procedure EditKeytipChange(Sender: TObject);
    procedure EditKeytipIdChange(Sender: TObject);
    procedure EditKeyTipSymbolChange(Sender: TObject);
    procedure PanelCommandPropertiesResize(Sender: TObject);
    procedure PanelImagesResize(Sender: TObject);
    procedure PanelHighContrastImagesResize(Sender: TObject);
    procedure ActionDeleteCommandExecute(Sender: TObject);
    procedure ActionAddCommandExecute(Sender: TObject);
    procedure EditNameKeyPress(Sender: TObject; var Key: Char);
    procedure ActionMoveUpExecute(Sender: TObject);
    procedure ActionMoveDownExecute(Sender: TObject);
    procedure BtnGenerateIDClick(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    procedure ActionAddCommandUpdate(Sender: TObject);
    procedure ListViewCommandsColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListViewCommandsCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
  private
    { Private declarations }
    FDocument: TRibbonDocument;
    FCommand: TRibbonCommand;
    FUpdating: Boolean;
    FFrameSmallImages: TFrameImageList;
    FFrameLargeImages: TFrameImageList;
    FFrameSmallHCImages: TFrameImageList;
    FFrameLargeHCImages: TFrameImageList;
    FNewCommandIndex: Integer;
    FSortBy: TCommandSortType;
    FSortDescending: Boolean;
    function AddCommand(const Command: TRibbonCommand): TListItem;
    procedure ShowSelection;
    procedure Modified;
    procedure EnableControls(const Enable: Boolean);
    procedure MoveCommand(const Direction: Integer);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Activate;
    procedure Deactivate;
    procedure ClearDocument;
    procedure RefreshSelection;
    procedure ShowDocument(const Document: TRibbonDocument);
  end;

const
  RS_DELETE_COMMAND_HEADER = 'Delete command?';
  RS_DELETE_COMMAND_MESSAGE = 'There are %d control(s) that reference this command.' + sLineBreak +
    'If you delete this command, those controls may become unusable. ' +
    'Do you want to delete this command (this cannot be undone)?';

implementation

{$R *.dfm}

uses
  UITypes,
  FMain, 
  System.Generics.Collections;

{ TFrameCommands }

procedure TFrameCommands.ActionAddCommandExecute(Sender: TObject);
var
  Command: TRibbonCommand;
begin
  Inc(FNewCommandIndex);
  Command := FDocument.Application.AddCommand('Command' + IntToStr(FNewCommandIndex));
  ListViewCommands.Selected := AddCommand(Command);
  ListViewCommands.Selected.Focused := True;
  ListViewCommands.Selected.MakeVisible(False);
  EditName.SetFocus;
  Modified;
end;

procedure TFrameCommands.ActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(ListViewCommands.Selected);
end;

procedure TFrameCommands.ActionAddCommandUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FDocument);
end;

procedure TFrameCommands.ActionDeleteCommandExecute(Sender: TObject);
begin
  if Assigned(FCommand) and ((FCommand.ReferenceCount = 0) or
    (TaskMessageDlg(RS_DELETE_COMMAND_HEADER,
    Format(RS_DELETE_COMMAND_MESSAGE, [FCommand.ReferenceCount]),
    mtConfirmation, [mbYes, mbNo], 0, mbYes) = mrYes))
  then
  begin
    FDocument.Application.DeleteCommand(FCommand);
    ListViewCommands.Selected.Free;
    ShowSelection;
    Modified;
  end;
end;

procedure TFrameCommands.ActionMoveDownExecute(Sender: TObject);
begin
  MoveCommand(1);
end;

procedure TFrameCommands.ActionMoveUpExecute(Sender: TObject);
begin
  MoveCommand(-1);
end;

procedure TFrameCommands.Activate;
begin
  ActionDeleteCommand.ShortCut := ShortCut(VK_DELETE, [ssCtrl]);
  ActionMoveUp.ShortCut := ShortCut(VK_UP, [ssCtrl]);
  ActionMoveDown.ShortCut := ShortCut(VK_DOWN, [ssCtrl]);;
end;

function TFrameCommands.AddCommand(const Command: TRibbonCommand): TListItem;
begin
  Result := ListViewCommands.Items.Add;
  Result.Caption := Command.Name;
  Result.SubItems.Add(Command.LabelTitle.Content);
  Result.Data := Command;
end;

procedure TFrameCommands.ClearDocument;
begin
  ListViewCommands.Clear;
end;

constructor TFrameCommands.Create(AOwner: TComponent);
begin
  inherited;

  FSortBy := TCommandSortType.None;

  FFrameSmallImages := TFrameImageList.Create(Self);
  FFrameSmallImages.Name := 'FrameSmallImages';
  FFrameSmallImages.Parent := PanelSmallImages;

  FFrameLargeImages := TFrameImageList.Create(Self);
  FFrameLargeImages.Name := 'FrameLargeImages';
  FFrameLargeImages.Parent := PanelLargeImages;

  FFrameSmallHCImages := TFrameImageList.Create(Self);
  FFrameSmallHCImages.Name := 'FrameSmallHCImages';
  FFrameSmallHCImages.Parent := PanelSmallHCImages;

  FFrameLargeHCImages := TFrameImageList.Create(Self);
  FFrameLargeHCImages.Name := 'FrameLargeHCImages';
  FFrameLargeHCImages.Parent := PanelLargeHCImages;
end;

procedure TFrameCommands.BtnGenerateIDClick(Sender: TObject);
var
  command: TRibbonCommand;
  highID : integer;
begin
  highID := 1;
  for command in FDocument.Application.Commands do
    if command.Id > highID then
      highID := command.Id;
  EditId.Text := IntToStr(highID + 1);
end;

procedure TFrameCommands.Deactivate;
begin
  ActionDeleteCommand.ShortCut := 0;
  ActionMoveUp.ShortCut := 0;
  ActionMoveDown.ShortCut := 0;
end;

procedure TFrameCommands.EditCaptionChange(Sender: TObject);
begin
  if Assigned(FCommand) and (FCommand.LabelTitle.Content <> EditCaption.Text) then
  begin
    FCommand.LabelTitle.Content := EditCaption.Text;
    ListViewCommands.Selected.SubItems[0] := EditCaption.Text;
    Modified;
  end;
end;

procedure TFrameCommands.EditCaptionIdChange(Sender: TObject);
begin
  if Assigned(FCommand) and (UpDownCaptionId.Position <> 1) and (FCommand.LabelTitle.Id <> UpDownCaptionId.Position) then
  begin
    FCommand.LabelTitle.Id := UpDownCaptionId.Position;
    Modified;
  end;
end;

procedure TFrameCommands.EditCaptionSymbolChange(Sender: TObject);
begin
  if Assigned(FCommand) and (FCommand.LabelTitle.Symbol <> EditCaptionSymbol.Text) then
  begin
    FCommand.LabelTitle.Symbol := EditCaptionSymbol.Text;
    Modified;
  end;
end;

procedure TFrameCommands.EditCommentChange(Sender: TObject);
begin
  if Assigned(FCommand) and (FCommand.Comment <> EditComment.Text) then
  begin
    FCommand.Comment := EditComment.Text;
    Modified;
  end;
end;

procedure TFrameCommands.EditDescriptionChange(Sender: TObject);
begin
  if Assigned(FCommand) and (FCommand.LabelDescription.Content <> EditDescription.Text) then
  begin
    FCommand.LabelDescription.Content := EditDescription.Text;
    Modified;
  end;
end;

procedure TFrameCommands.EditDescriptionIdChange(Sender: TObject);
begin
  if Assigned(FCommand) and (UpDownDescriptionId.Position <> 1) and (FCommand.LabelDescription.Id <> UpDownDescriptionId.Position) then
  begin
    FCommand.LabelDescription.Id := UpDownDescriptionId.Position;
    Modified;
  end;
end;

procedure TFrameCommands.EditDescriptionSymbolChange(Sender: TObject);
begin
  if Assigned(FCommand) and (FCommand.LabelDescription.Symbol <> EditDescriptionSymbol.Text) then
  begin
    FCommand.LabelDescription.Symbol := EditDescriptionSymbol.Text;
    Modified;
  end;
end;

procedure TFrameCommands.EditIdChange(Sender: TObject);
begin
  if Assigned(FCommand) and (UpDownId.Position <> 1) and (FCommand.Id <> UpDownId.Position) then
  begin
    FCommand.Id := UpDownId.Position;
    Modified;
  end;
end;

procedure TFrameCommands.EditKeytipChange(Sender: TObject);
begin
  if Assigned(FCommand) and (FCommand.Keytip.Content <> EditKeytip.Text) then
  begin
    FCommand.Keytip.Content := EditKeytip.Text;
    Modified;
  end;
end;

procedure TFrameCommands.EditKeytipIdChange(Sender: TObject);
begin
  if Assigned(FCommand) and (UpDownKeyTipId.Position <> 1) and (FCommand.Keytip.Id <> UpDownKeyTipId.Position) then
  begin
    FCommand.Keytip.Id := UpDownKeyTipId.Position;
    Modified;
  end;
end;

procedure TFrameCommands.EditKeyTipSymbolChange(Sender: TObject);
begin
  if Assigned(FCommand) and (FCommand.Keytip.Symbol <> EditKeyTipSymbol.Text) then
  begin
    FCommand.Keytip.Symbol := EditKeyTipSymbol.Text;
    Modified;
  end;
end;

procedure TFrameCommands.EditNameChange(Sender: TObject);
begin
  if Assigned(FCommand) and (FCommand.Name <> EditName.Text) then
  begin
    FCommand.Name := EditName.Text;
    ListViewCommands.Selected.Caption := EditName.Text;
    Modified;
  end;
end;

procedure TFrameCommands.EditNameKeyPress(Sender: TObject; var Key: Char);
var
  Edit: TEdit;
begin
  { Only allow valid Name/Symbol characters }
  Edit := Sender as TEdit;
  case Key of
    #3, // Ctrl-C
    #$16, // Ctrl-V
    #$18, // Ctrl-X
    #8, // backspace
    'A'..'Z',
    'a'..'z',
    '_':
      { OK };

    '0'..'9':
      if (Edit.SelStart = 0) then
        Key := #0;
  else
    Key := #0;
  end;
end;

procedure TFrameCommands.EditSymbolChange(Sender: TObject);
begin
  if Assigned(FCommand) and (FCommand.Symbol <> EditSymbol.Text) then
  begin
    FCommand.Symbol := EditSymbol.Text;
    Modified;
  end;
end;

procedure TFrameCommands.EditTooltipDescriptionChange(Sender: TObject);
begin
  if Assigned(FCommand) and (FCommand.TooltipDescription.Content <> EditTooltipDescription.Text) then
  begin
    FCommand.TooltipDescription.Content := EditTooltipDescription.Text;
    Modified;
  end;
end;

procedure TFrameCommands.EditTooltipDescriptionIdChange(Sender: TObject);
begin
  if Assigned(FCommand) and (UpDownTooltipDescriptionId.Position <> 1) and (FCommand.TooltipDescription.Id <> UpDownTooltipDescriptionId.Position) then
  begin
    FCommand.TooltipDescription.Id := UpDownTooltipDescriptionId.Position;
    Modified;
  end;
end;

procedure TFrameCommands.EditTooltipDescriptionSymbolChange(Sender: TObject);
begin
  if Assigned(FCommand) and (FCommand.TooltipDescription.Symbol <> EditTooltipDescriptionSymbol.Text) then
  begin
    FCommand.TooltipDescription.Symbol := EditTooltipDescriptionSymbol.Text;
    Modified;
  end;
end;

procedure TFrameCommands.EditTooltipTitleChange(Sender: TObject);
begin
  if Assigned(FCommand) and (FCommand.TooltipTitle.Content <> EditTooltipTitle.Text) then
  begin
    FCommand.TooltipTitle.Content := EditTooltipTitle.Text;
    Modified;
  end;
end;

procedure TFrameCommands.EditTooltipTitleIdChange(Sender: TObject);
begin
  if Assigned(FCommand) and (UpDownTooltipTitleId.Position <> 1) and (FCommand.TooltipTitle.Id <> UpDownTooltipTitleId.Position) then
  begin
    FCommand.TooltipTitle.Id := UpDownTooltipTitleId.Position;
    Modified;
  end;
end;

procedure TFrameCommands.EditTooltipTitleSymbolChange(Sender: TObject);
begin
  if Assigned(FCommand) and (FCommand.TooltipTitle.Symbol <> EditTooltipTitleSymbol.Text) then
  begin
    FCommand.TooltipTitle.Symbol := EditTooltipTitleSymbol.Text;
    Modified;
  end;
end;

procedure TFrameCommands.EnableControls(const Enable: Boolean);
var
  I: Integer;
begin
  for I := 0 to PanelCommandProperties.ControlCount - 1 do
    PanelCommandProperties.Controls[I].Enabled := Enable;
  ActionDeleteCommand.Enabled := Enable;
end;

procedure TFrameCommands.ListViewCommandsColumnClick(Sender: TObject; Column: TListColumn);
var
  lCommands: TList<TRibbonCommand>;
  lItem: TListItem;
begin
  TListView(Sender).SortType := stNone;

  if FSortBy = TCommandSortType(Column.Index) then
    FSortDescending := not FSortDescending
  else begin
    FSortBy := TCommandSortType(Column.Index);
    FSortDescending := False;
  end;

  ListViewCommands.SortType := TSortType.stText;

  lCommands := TList<TRibbonCommand>.Create;
  try
    for lItem in TListView(Sender).Items do
      lCommands.Add(lItem.Data);

    FDocument.Application.Commands.Assign(lCommands);
  finally
    lCommands.Free;
  end;
end;

procedure TFrameCommands.ListViewCommandsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  case FSortBy of
    TCommandSortType.None:
      Compare := 0;
    TCommandSortType.Name:
      Compare := CompareText(Item1.Caption, Item2.Caption)
    else
      Compare := CompareText(Item1.SubItems[0], Item2.SubItems[0]);
  end;

  if FSortDescending then
    Compare := -Compare;
end;

procedure TFrameCommands.ListViewCommandsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  ShowSelection;
end;

procedure TFrameCommands.Modified;
begin
  if (not FUpdating) then
    FormMain.Modified;
end;

procedure TFrameCommands.MoveCommand(const Direction: Integer);
var
  Item, NewItem: TListItem;
  Command: TRibbonCommand;
begin
  ListViewCommands.SortType := stNone;

  Item := ListViewCommands.Selected;
  if (Item = nil) or (Item.Data = nil) then
    Exit;
  Command := Item.Data;

  if (FDocument.Application.Reorder(Command, Direction)) then
  begin
    if (Direction < 0) then
      NewItem := ListViewCommands.Items.Insert(Item.Index - 1)
    else
      NewItem := ListViewCommands.Items.Insert(Item.Index + 2);
    NewItem.Assign(Item);
    Item.Free;
    ListViewCommands.Selected := NewItem;
    NewItem.Focused := True;
    NewItem.MakeVisible(False);
    Modified;
  end;
end;

procedure TFrameCommands.PanelCommandPropertiesResize(Sender: TObject);
begin
  PanelImages.Height := (PanelCommandProperties.Height - PanelProps.Height - LabelHeader.Height) div 2;
end;

procedure TFrameCommands.PanelHighContrastImagesResize(Sender: TObject);
begin
  PanelSmallHCImages.Width := PanelHighContrastImages.Width div 2;
end;

procedure TFrameCommands.PanelImagesResize(Sender: TObject);
begin
  PanelSmallImages.Width := PanelImages.Width div 2;
end;

procedure TFrameCommands.ShowDocument(const Document: TRibbonDocument);
var
  Command: TRibbonCommand;
  Index: Integer;
begin
  FDocument := Document;
  FNewCommandIndex := 0;
  ListViewCommands.Items.BeginUpdate;
  try
    ListViewCommands.Clear;
    for Command in FDocument.Application.Commands do
    begin
      AddCommand(Command);
      if (SameText(Copy(Command.Name, 1, 7), 'Command')) then
      begin
        Index := StrToIntDef(Copy(Command.Name, 8, MaxInt), -1);
        if (Index > FNewCommandIndex) then
          FNewCommandIndex := Index;
      end;
    end;
    if (ListViewCommands.Items.Count > 0) then
      ListViewCommands.ItemIndex := 0
    else
      ShowSelection;
  finally
    ListViewCommands.Items.EndUpdate;
  end;
end;

procedure TFrameCommands.RefreshSelection;
begin
  ShowSelection;
end;

procedure TFrameCommands.ShowSelection;
var
  Item: TListItem;
begin
  Item := ListViewCommands.Selected;
  if Assigned(Item) then
  begin
    FCommand := Item.Data;
    ActionMoveUp.Enabled := (Item.Index > 0);
    ActionMoveDown.Enabled := (Item.Index < (ListViewCommands.Items.Count - 1));
  end
  else
  begin
    FCommand := nil;
    ActionMoveUp.Enabled := False;
    ActionMoveDown.Enabled := False;
  end;

  FUpdating := True;
  try
    if Assigned(FCommand) then
    begin
      EnableControls(True);
      EditName.Text := FCommand.Name;
      EditSymbol.Text := FCommand.Symbol;
      UpDownId.Position := FCommand.Id;
      EditComment.Text := FCommand.Comment;

      EditCaption.Text := FCommand.LabelTitle.Content;
      UpDownCaptionId.Position := FCommand.LabelTitle.Id;
      EditCaptionSymbol.Text := FCommand.LabelTitle.Symbol;

      EditDescription.Text := FCommand.LabelDescription.Content;
      UpDownDescriptionId.Position := FCommand.LabelDescription.Id;
      EditDescriptionSymbol.Text := FCommand.LabelDescription.Symbol;

      EditTooltipTitle.Text := FCommand.TooltipTitle.Content;
      UpDownTooltipTitleId.Position := FCommand.TooltipTitle.Id;
      EditTooltipTitleSymbol.Text := FCommand.TooltipTitle.Symbol;

      EditTooltipDescription.Text := FCommand.TooltipDescription.Content;
      UpDownTooltipDescriptionId.Position := FCommand.TooltipDescription.Id;
      EditTooltipDescriptionSymbol.Text := FCommand.TooltipDescription.Symbol;

      EditKeytip.Text := FCommand.Keytip.Content;
      UpDownKeyTipId.Position := FCommand.Keytip.Id;
      EditKeyTipSymbol.Text := FCommand.Keytip.Symbol;
    end
    else
    begin
      EnableControls(False);
      EditName.Text := '';
      EditSymbol.Text := '';
      UpDownId.Position := 0;
      EditComment.Text := '';

      EditCaption.Text := '';
      UpDownCaptionId.Position := 0;
      EditCaptionSymbol.Text := '';

      EditDescription.Text := '';
      UpDownDescriptionId.Position := 0;
      EditDescriptionSymbol.Text := '';

      EditTooltipTitle.Text := '';
      UpDownTooltipTitleId.Position := 0;
      EditTooltipTitleSymbol.Text := '';

      EditTooltipDescription.Text := '';
      UpDownTooltipDescriptionId.Position := 0;
      EditTooltipDescriptionSymbol.Text := '';

      EditKeytip.Text := '';
      UpDownKeyTipId.Position := 0;
      EditKeyTipSymbol.Text := '';
    end;
    FFrameSmallImages.ShowImages(FCommand, []);
    FFrameLargeImages.ShowImages(FCommand, [ifLarge]);
    FFrameSmallHCImages.ShowImages(FCommand, [ifHighContrast]);
    FFrameLargeHCImages.ShowImages(FCommand, [ifLarge, ifHighContrast]);
  finally
    FUpdating := False;
  end;
end;

procedure TFrameCommands.UpDownChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Integer; Direction: TUpDownDirection);
var
  UpDown: TUpDown;
begin
  UpDown := Sender as TUpDown;
  { Skip value 1 }
  AllowChange := (NewValue <> 1);
  if (not AllowChange) then
  begin
    if (Direction = updUp) then
      UpDown.Position := 2
    else
      UpDown.Position := 0;
  end;
end;

end.
