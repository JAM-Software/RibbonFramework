unit FGallery;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FControl, StdCtrls, ExtCtrls, RibbonMarkup, ComCtrls;

type
  TFrameGallery = class(TFrameControl)
    Label2: TLabel;
    ComboBoxGalleryType: TComboBox;
    CheckBoxHasLargeItems: TCheckBox;
    Label3: TLabel;
    EditItemWidth: TEdit;
    UpDownItemWidth: TUpDown;
    Label4: TLabel;
    Label5: TLabel;
    EditItemHeight: TEdit;
    UpDownItemHeight: TUpDown;
    Label6: TLabel;
    Label7: TLabel;
    ComboBoxTextPosition: TComboBox;
    GroupBox1: TGroupBox;
    Label8: TLabel;
    ComboBoxLayoutType: TComboBox;
    LabelRowCount: TLabel;
    EditRows: TEdit;
    UpDownRows: TUpDown;
    LabelRowCountInfo: TLabel;
    LabelColumnCount: TLabel;
    EditColumns: TEdit;
    UpDownColumns: TUpDown;
    LabelGripper: TLabel;
    ComboBoxGripper: TComboBox;
    procedure ComboBoxGalleryTypeChange(Sender: TObject);
    procedure CheckBoxHasLargeItemsClick(Sender: TObject);
    procedure EditItemWidthChange(Sender: TObject);
    procedure EditItemHeightChange(Sender: TObject);
    procedure ComboBoxTextPositionChange(Sender: TObject);
    procedure ComboBoxLayoutTypeChange(Sender: TObject);
    procedure EditRowsChange(Sender: TObject);
    procedure EditColumnsChange(Sender: TObject);
    procedure ComboBoxGripperChange(Sender: TObject);
  private
    { Private declarations }
    FGallery: TRibbonGallery;
    procedure UpdateControls;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

resourcestring
  RS_NONE     = 'None';
  RS_VERTICAL = 'Vertical';
  RS_CORNER   = 'Corner';

implementation

{$R *.dfm}

const // Layout Type
  LT_DEFAULT  = 0;
  LT_VERTICAL = 1;
  LT_FLOW     = 2;

{ TFrameGallery }

procedure TFrameGallery.CheckBoxHasLargeItemsClick(Sender: TObject);
begin
  if (CheckBoxHasLargeItems.Checked <> FGallery.HasLargeItems) then
  begin
    FGallery.HasLargeItems := CheckBoxHasLargeItems.Checked;
    Modified;
  end;
end;

procedure TFrameGallery.ComboBoxGalleryTypeChange(Sender: TObject);
begin
  if (ComboBoxGalleryType.ItemIndex <> Ord(FGallery.GalleryType)) then
  begin
    FGallery.GalleryType := TRibbonGalleryType(ComboBoxGalleryType.ItemIndex);
    Modified;
  end;
end;

procedure TFrameGallery.ComboBoxGripperChange(Sender: TObject);
var
  GripperIndex: Integer;
  VertLayout: TRibbonVerticalMenuLayout;
  FlowLayout: TRibbonFlowMenuLayout;
begin
  VertLayout := nil;
  FlowLayout := nil;
  GripperIndex := -1;
  if Assigned(FGallery.MenuLayout) then
  begin
    if (FGallery.MenuLayout is TRibbonVerticalMenuLayout) then
    begin
      VertLayout := TRibbonVerticalMenuLayout(FGallery.MenuLayout);
      GripperIndex := Ord(VertLayout.Gripper);
    end
    else if (FGallery.MenuLayout is TRibbonFlowMenuLayout) then
    begin
      FlowLayout := TRibbonFlowMenuLayout(FGallery.MenuLayout);
      GripperIndex := Ord(FlowLayout.Gripper);
    end
    else
      Assert(False);

    if (GripperIndex <> ComboBoxGripper.ItemIndex) then
    begin
      if Assigned(VertLayout) then
        VertLayout.Gripper := TRibbonSingleColumnGripperType(ComboBoxGripper.ItemIndex)
      else if Assigned(FlowLayout) then
        FlowLayout.Gripper := TRibbonMultiColumnGripperType(ComboBoxGripper.ItemIndex);
      Modified;
    end;
  end;
end;

procedure TFrameGallery.ComboBoxLayoutTypeChange(Sender: TObject);
begin
  case ComboBoxLayoutType.ItemIndex of
    LT_DEFAULT:
      if Assigned(FGallery.MenuLayout) then
      begin
        FGallery.DeleteMenuLayout;
        Modified;
        UpdateControls;
      end;

    LT_VERTICAL:
      if (FGallery.MenuLayout = nil) or (not (FGallery.MenuLayout is TRibbonVerticalMenuLayout)) then
      begin
        FGallery.CreateVerticalMenuLayout;
        Modified;
        UpdateControls;
      end;

    LT_FLOW:
      if (FGallery.MenuLayout = nil) or (not (FGallery.MenuLayout is TRibbonFlowMenuLayout)) then
      begin
        FGallery.CreateFlowMenuLayout;
        Modified;
        UpdateControls;
      end;
  else
    Assert(False);
  end;
end;

procedure TFrameGallery.ComboBoxTextPositionChange(Sender: TObject);
begin
  if (ComboBoxTextPosition.ItemIndex <> Ord(FGallery.TextPosition)) then
  begin
    FGallery.TextPosition := TRibbonTextPosition(ComboBoxTextPosition.ItemIndex);
    Modified;
  end;
end;

procedure TFrameGallery.EditColumnsChange(Sender: TObject);
var
  Layout: TRibbonFlowMenuLayout;
begin
  if Assigned(FGallery.MenuLayout) and (FGallery.MenuLayout is TRibbonFlowMenuLayout) then
  begin
    Layout := TRibbonFlowMenuLayout(FGallery.MenuLayout);
    if (UpDownColumns.Position <> Layout.Columns) then
    begin
      Layout.Columns := UpDownColumns.Position;
      Modified;
    end;
  end;
end;

procedure TFrameGallery.EditItemHeightChange(Sender: TObject);
begin
  if (UpDownItemHeight.Position <> FGallery.ItemHeight) then
  begin
    FGallery.ItemHeight := UpDownItemHeight.Position;
    Modified;
  end;
end;

procedure TFrameGallery.EditItemWidthChange(Sender: TObject);
begin
  if (UpDownItemWidth.Position <> FGallery.ItemWidth) then
  begin
    FGallery.ItemWidth := UpDownItemWidth.Position;
    Modified;
  end;
end;

procedure TFrameGallery.EditRowsChange(Sender: TObject);
begin
  if Assigned(FGallery.MenuLayout) and (UpDownRows.Position <> FGallery.MenuLayout.Rows) then
  begin
    FGallery.MenuLayout.Rows := UpDownRows.Position;
    Modified;
  end;
end;

procedure TFrameGallery.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FGallery := Subject as TRibbonGallery;
  ComboBoxGalleryType.ItemIndex := Ord(FGallery.GalleryType);
  ComboBoxTextPosition.ItemIndex := Ord(FGallery.TextPosition);
  UpDownItemWidth.Position := FGallery.ItemWidth;
  UpDownItemHeight.Position := FGallery.ItemHeight;
  CheckBoxHasLargeItems.Checked := FGallery.HasLargeItems;
  if Assigned(FGallery.MenuLayout) then
  begin
    if (FGallery.MenuLayout is TRibbonVerticalMenuLayout) then
      ComboBoxLayoutType.ItemIndex := LT_VERTICAL
    else if (FGallery.MenuLayout is TRibbonFlowMenuLayout) then
      ComboBoxLayoutType.ItemIndex := LT_FLOW
    else
      Assert(False);
  end
  else
    ComboBoxLayoutType.ItemIndex := LT_DEFAULT;
  UpdateControls;
end;

procedure TFrameGallery.UpdateControls;
begin
  LabelRowCount.Enabled := (ComboBoxLayoutType.ItemIndex <> LT_DEFAULT);
  EditRows.Enabled := LabelRowCount.Enabled;
  UpDownRows.Enabled := LabelRowCount.Enabled;
  LabelRowCountInfo.Enabled := LabelRowCount.Enabled;
  if (UpDownRows.Enabled) and Assigned(FGallery.MenuLayout) then
    UpDownRows.Position := FGallery.MenuLayout.Rows
  else
    UpDownRows.Position := -1;

  LabelColumnCount.Enabled := (ComboBoxLayoutType.ItemIndex = LT_FLOW);
  EditColumns.Enabled := LabelColumnCount.Enabled;
  UpDownColumns.Enabled := LabelColumnCount.Enabled;
  if (UpDownColumns.Enabled) and Assigned(FGallery.MenuLayout) and (FGallery.MenuLayout is TRibbonFlowMenuLayout) then
  begin
    UpDownColumns.Position := TRibbonFlowMenuLayout(FGallery.MenuLayout).Columns;
    UpDownColumns.Min := 1;
  end
  else
  begin
    UpDownColumns.Min := -1;
    UpDownColumns.Position := -1;
  end;

  LabelGripper.Enabled := (ComboBoxLayoutType.ItemIndex <> LT_DEFAULT);
  ComboBoxGripper.Enabled := LabelGripper.Enabled;
  if (ComboBoxGripper.Enabled) then
  begin
    ComboBoxGripper.Items.Clear;
    ComboBoxGripper.Items.Add(RS_NONE);
    ComboBoxGripper.Items.Add(RS_VERTICAL);
    if (ComboBoxLayoutType.ItemIndex = LT_FLOW) then
      ComboBoxGripper.Items.Add(RS_CORNER);
    if Assigned(FGallery.MenuLayout) then
    begin
      if (FGallery.MenuLayout is TRibbonVerticalMenuLayout) then
        ComboBoxGripper.ItemIndex := Ord(TRibbonVerticalMenuLayout(FGallery.MenuLayout).Gripper)
      else if (FGallery.MenuLayout is TRibbonFlowMenuLayout) then
        ComboBoxGripper.ItemIndex := Ord(TRibbonFlowMenuLayout(FGallery.MenuLayout).Gripper)
      else
        Assert(False);
    end;
  end
  else
    ComboBoxGripper.ItemIndex := -1;
end;

end.
