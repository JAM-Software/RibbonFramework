unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UIRibbon, UIRibbonCommands, UIRibbonUtils, uRenderer,
  RibbonConst;

type
  TFormMain = class(TForm)
    Ribbon: TUIRibbon;
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FCmdShapes: TUICommandCollection;
    FCmdBorderStyles: TUICommandCollection;
    FCmdBorderSizes: TUICommandCollection;
    FCmdLayouts: TUICommandCollection;
    FCmdSizeAndColor: TUICommandCollection;
    FCmdColor: array [0..2] of TUICommandBoolean;
    FCmdSize: array [0..2] of TUICommandBoolean;

    procedure CmdShapesSelect(const Args: TUICommandCollectionEventArgs);
    procedure CmdBorderStylesSelect(const Args: TUICommandCollectionEventArgs);
    procedure CmdBorderSizesSelect(const Args: TUICommandCollectionEventArgs);
    procedure CmdLayoutsSelect(const Args: TUICommandCollectionEventArgs);

    procedure CmdColorToggle(const Args: TUICommandBooleanEventArgs);
    procedure CmdSizeToggle(const Args: TUICommandBooleanEventArgs);

    procedure PopulateShapeGallery;
    procedure PopulateBorderStylesGallery;
    procedure PopulateBorderSizesGallery;
    procedure PopulateLayoutsGallery;
    procedure PopulateSizeAndColorGallery;
  published
    procedure RibbonLoaded(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  Math;

{ TFormMain }

procedure TFormMain.CmdBorderSizesSelect(const Args: TUICommandCollectionEventArgs);
var
  Param: TRenderParam;
  S: String;
begin
  if (Args.Verb = cvExecute) then
  begin
    Param := Renderer.GetRenderParam;
    case Args.ItemIndex of
      -1: begin
            { Text was entered in the combobox. This text is available in the
              upLabel property. }
            if (Args.Properties.GetValue(upLabel, S)) then
              Param.BorderSize := EnsureRange(StrToIntDef(S, 1), 1, 15);
          end;
       0: Param.BorderSize := 1;
       1: Param.BorderSize := 3;
       2: Param.BorderSize := 5;
    end;
    Renderer.UpdateRenderParam(Param);
  end;
end;

procedure TFormMain.CmdBorderStylesSelect(const Args: TUICommandCollectionEventArgs);
var
  Param: TRenderParam;
begin
  Param := Renderer.GetRenderParam;
  if (Args.ItemIndex < 0) then
  begin
    { Top part of the split button was clicked. }
    if (Param.BorderStyle = bsNone) then
      Param.BorderStyle := bsSolid
    else
      Param.BorderStyle := bsNone;
  end
  else
    Param.BorderStyle := TBorderStyle(Args.ItemIndex);

  Renderer.UpdateRenderParam(Param);

  { The Border Sizes combo box must be re-enabled if a solid style is selected }
  FCmdBorderSizes.Enabled := (Param.BorderStyle = bsSolid);

  { The gallery button will appear checked (down) when there is a border. }
  FCmdBorderStyles.Checked := (Param.BorderStyle <> bsNone);
end;

procedure TFormMain.CmdColorToggle(const Args: TUICommandBooleanEventArgs);
var
  Param: TRenderParam;
  I: Integer;
begin
  if (Args.Verb = cvExecute) then
  begin
    Param := Renderer.GetRenderParam;
    Param.ShapeColor := TShapeColor(Args.Command.Tag);
    Renderer.UpdateRenderParam(Param);

    { Set the Checked property of the 3 color buttons }
    for I := 0 to 2 do
      FCmdColor[I].Checked := (FCmdColor[I] = Args.Command);
  end;
end;

procedure TFormMain.CmdLayoutsSelect(const Args: TUICommandCollectionEventArgs);
var
  Param: TRenderParam;
begin
  if (Args.Verb = cvExecute) and (Args.ItemIndex >= 0) then
  begin
    Param := Renderer.GetRenderParam;
    Param.ViewLayout := TViewLayout(Args.ItemIndex);
    Renderer.UpdateRenderParam(Param);
  end;
end;

procedure TFormMain.CmdShapesSelect(const Args: TUICommandCollectionEventArgs);
var
  Param: TRenderParam;
begin
  Param := Renderer.GetRenderParam;
  Param.ShapeType := TShapeType(Args.ItemIndex);
  Renderer.UpdateRenderParam(Param);
end;

procedure TFormMain.CmdSizeToggle(const Args: TUICommandBooleanEventArgs);
var
  Param: TRenderParam;
  I: Integer;
begin
  if (Args.Verb = cvExecute) then
  begin
    Param := Renderer.GetRenderParam;
    Param.ShapeSize := TShapeSize(Args.Command.Tag);
    Renderer.UpdateRenderParam(Param);

    { Set the Checked property of the 3 size buttons }
    for I := 0 to 2 do
      FCmdSize[I].Checked := (FCmdSize[I] = Args.Command);
  end;
end;

procedure TFormMain.FormPaint(Sender: TObject);
var
  R: TRect;
begin
  R := ClientRect;
  Inc(R.Top, Ribbon.Height);
  Renderer.DrawShapes(Canvas, R);
end;

procedure TFormMain.FormResize(Sender: TObject);
var
  R: TRect;
begin
  if Assigned(Ribbon) then
  begin
    R := ClientRect;
    R.Top := Ribbon.Height;
    InvalidateRect(Handle, R, True);
  end;
end;

procedure TFormMain.PopulateBorderSizesGallery;
const
  LabelIds: array [0..2] of Integer = (IDS_BORDERSIZE_1, IDS_BORDERSIZE_3,
    IDS_BORDERSIZE_5);
var
  I: Integer;
  Item: TUIGalleryCollectionItem;
begin
  FCmdBorderSizes.Items.BeginUpdate;
  try
    for I := 0 to Length(LabelIds) - 1 do
    begin
      Item := TUIGalleryCollectionItem.Create;
      Item.LabelText := CreateStringFromResource(LabelIds[I]);
      FCmdBorderSizes.Items.Add(Item);
    end;
  finally
    FCmdBorderSizes.Items.EndUpdate;
  end;

  FCmdBorderSizes.SelectedItem := 0;
  FCmdBorderSizes.Enabled := False;
end;

procedure TFormMain.PopulateBorderStylesGallery;
const
  LabelIds: array [0..2] of Integer = (IDS_BORDER_NONE, IDS_BORDER_SOLID,
    IDS_BORDER_DASH);
var
  ImageIds: array [0..2] of Integer;
  Item: TUIGalleryCollectionItem;
  Dpi, I: Integer;
begin
  Dpi := Screen.PixelsPerInch;
  if (Dpi > 144) then
  begin
    ImageIds[0] := IDB_NONE_192;
    ImageIds[1] := IDB_SOLID_192;
    ImageIds[2] := IDB_DASH_192;
  end
  else if (Dpi > 120) then
  begin
    ImageIds[0] := IDB_NONE_144;
    ImageIds[1] := IDB_SOLID_144;
    ImageIds[2] := IDB_DASH_144;
  end
  else if (Dpi > 96) then
  begin
    ImageIds[0] := IDB_NONE_120;
    ImageIds[1] := IDB_SOLID_120;
    ImageIds[2] := IDB_DASH_120;
  end
  else
  begin
    ImageIds[0] := IDB_NONE_96;
    ImageIds[1] := IDB_SOLID_96;
    ImageIds[2] := IDB_DASH_96;
  end;

  FCmdBorderStyles.Items.BeginUpdate;
  try
    for I := 0 to Length(LabelIds) - 1 do
    begin
      Item := TUIGalleryCollectionItem.Create;
      Item.LabelText := CreateStringFromResource(LabelIds[I]);
      Item.Image := TUIImage.Create(ImageIds[I]);
      FCmdBorderStyles.Items.Add(Item);
    end;
  finally
    FCmdBorderStyles.Items.EndUpdate;
  end;

  FCmdBorderStyles.SelectedItem := Ord(bsNone);
end;

procedure TFormMain.PopulateLayoutsGallery;
const
  LabelIds: array [0..2] of Integer = (IDS_LAYOUT_1, IDS_LAYOUT_2,
    IDS_LAYOUT_3);
var
  Item: TUIGalleryCollectionItem;
  I: Integer;
begin
  FCmdLayouts.Items.BeginUpdate;
  try
    for I := 0 to Length(LabelIds) - 1 do
    begin
      Item := TUIGalleryCollectionItem.Create;
      Item.LabelText := CreateStringFromResource(LabelIds[I]);
      FCmdLayouts.Items.Add(Item);
    end;
  finally
    FCmdLayouts.Items.EndUpdate;
  end;

  FCmdLayouts.SelectedItem := 0;
end;

procedure TFormMain.PopulateShapeGallery;
const
  LabelIds: array [0..3] of Integer = (IDS_RECTANGLE, IDS_ELLIPSE,
    IDS_ROUNDED_RECTANGLE, IDS_DIAMOND);
var
  ImageIds: array [0..3] of Integer;
  Item: TUIGalleryCollectionItem;
  Dpi, I: Integer;
begin
  Dpi := Screen.PixelsPerInch;
  if (Dpi > 144) then
  begin
    ImageIds[0] := IDB_RECTANGLE_192;
    ImageIds[1] := IDB_ELLIPSE_192;
    ImageIds[2] := IDB_ROUNDED_RECTANGLE_192;
    ImageIds[3] := IDB_DIAMOND_192;
  end
  else if (Dpi > 120) then
  begin
    ImageIds[0] := IDB_RECTANGLE_144;
    ImageIds[1] := IDB_ELLIPSE_144;
    ImageIds[2] := IDB_ROUNDED_RECTANGLE_144;
    ImageIds[3] := IDB_DIAMOND_144;
  end
  else if (Dpi > 96) then
  begin
    ImageIds[0] := IDB_RECTANGLE_120;
    ImageIds[1] := IDB_ELLIPSE_120;
    ImageIds[2] := IDB_ROUNDED_RECTANGLE_120;
    ImageIds[3] := IDB_DIAMOND_120;
  end
  else
  begin
    ImageIds[0] := IDB_RECTANGLE_96;
    ImageIds[1] := IDB_ELLIPSE_96;
    ImageIds[2] := IDB_ROUNDED_RECTANGLE_96;
    ImageIds[3] := IDB_DIAMOND_96;
  end;

  FCmdShapes.Items.BeginUpdate;
  try
    for I := 0 to Length(LabelIds) - 1 do
    begin
      Item := TUIGalleryCollectionItem.Create;
      Item.LabelText := CreateStringFromResource(LabelIds[I]);
      Item.Image := TUIImage.Create(ImageIds[I]);
      FCmdShapes.Items.Add(Item);
    end;
  finally
    FCmdShapes.Items.EndUpdate;
  end;

  FCmdShapes.SelectedItem := Ord(stRectangle);
end;

procedure TFormMain.PopulateSizeAndColorGallery;
var
  Item: TUIGalleryCollectionItem;
  CategoryId, I: Integer;
begin
  { Fill Categories }
  Item := TUIGalleryCollectionItem.Create;
  Item.LabelText := CreateStringFromResource(IDS_SIZE_CATEGORY);
  Item.CategoryId := 0;
  FCmdSizeAndColor.Categories.Add(Item);

  Item := TUIGalleryCollectionItem.Create;
  Item.LabelText := CreateStringFromResource(IDS_COLOR_CATEGORY);
  Item.CategoryId := 1;
  FCmdSizeAndColor.Categories.Add(Item);

  { Fill Items }
  for CategoryId := 0 to 1 do
  begin
    for I := 0 to 2 do
    begin
      Item := TUIGalleryCollectionItem.Create;
      Item.CategoryId := CategoryId;
      if (CategoryId = 0) then
        Item.Command := FCmdSize[I]
      else
        Item.Command := FCmdColor[I];
      FCmdSizeAndColor.Items.Add(Item);
    end;
  end;
end;

procedure TFormMain.RibbonLoaded;
var
  I: Integer;
begin
  inherited;
  FCmdShapes := Ribbon[IDR_CMD_SHAPES] as TUICommandCollection;
  FCmdShapes.OnSelect := CmdShapesSelect;

  FCmdBorderStyles := Ribbon[IDR_CMD_BORDERSTYLES] as TUICommandCollection;
  FCmdBorderStyles.OnSelect:= CmdBorderStylesSelect;

  FCmdBorderSizes := Ribbon[IDR_CMD_BORDERSIZES] as TUICommandCollection;
  FCmdBorderSizes.OnSelect:= CmdBorderSizesSelect;

  FCmdLayouts := Ribbon[IDR_CMD_LAYOUTS] as TUICommandCollection;
  FCmdLayouts.OnSelect:= CmdLayoutsSelect;

  FCmdSizeAndColor := Ribbon[IDR_CMD_SIZEANDCOLOR] as TUICommandCollection;
  for I := 0 to 2 do
  begin
    FCmdColor[I] := TUICommandBoolean.Create(Ribbon, IDR_CMD_RED + I);
    FCmdColor[I].Tag := I;
    FCmdColor[I].OnToggle := CmdColorToggle;
    FCmdColor[I].Checked := (I = 0);

    FCmdSize[I] := TUICommandBoolean.Create(Ribbon, IDR_CMD_SMALL + I);
    FCmdSize[I].Tag := I;
    FCmdSize[I].Checked := (I = 0);
    FCmdSize[I].OnToggle := CmdSizeToggle;
  end;

  { Populate the galleries }
  PopulateShapeGallery;
  PopulateBorderStylesGallery;
  PopulateBorderSizesGallery;
  PopulateLayoutsGallery;
  PopulateSizeAndColorGallery;

  Renderer.Initialize(Self);
end;

end.
