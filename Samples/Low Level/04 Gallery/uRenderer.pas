unit uRenderer;

interface

uses
  Windows,
  Graphics,
  Forms;

type
  // Enums used to define the parameters for the renderer.
  TShapeType = (stRectangle, stEllipse, stRoundedRectangle, stDiamond);
  TShapeColor = (scRed, scGreen, scBlue);
  TShapeSize = (ssSmall, ssMedium, ssLarge);
  TBorderStyle = (bsNone, bsSolid, bsDash);
  TViewLayout = (vl1, vl2, vl3);

type
  // Parameters to the renderer- updated by the controls in the ribbon.
  TRenderParam = record
    ShapeType: TShapeType;
    ShapeColor: TShapeColor;
    ShapeSize: TShapeSize;
    BorderStyle: TBorderStyle;
    BorderSize: Integer;
    ViewLayout: TViewLayout;
  end;

type
  TRenderer = record
  private
    const FColors: array [TShapeColor] of TColor = (clRed, clGreen, clBlue);
  private
    FForm: TCustomForm;
    FParam: TRenderParam;
  private
    procedure DrawRectangle(const Canvas: TCanvas; const Center: TPoint;
      const BoundingBoxLength: Integer; const IsRounded: Boolean);
    procedure DrawEllipse(const Canvas: TCanvas; const Center: TPoint;
      const BoundingBoxLength: Integer);
    procedure DrawDiamond(const Canvas: TCanvas; const Center: TPoint;
      const BoundingBoxLength: Integer);
  public
    procedure Initialize(const Form: TCustomForm);
    procedure DrawShapes(const Canvas: TCanvas; const Rect: TRect);
    function GetRenderParam: TRenderParam;
    procedure UpdateRenderParam(const Parameter: TRenderParam);
  end;

var
  Renderer: TRenderer;

implementation

uses
  Math;

const
  SHAPE_MARGIN = 5;
{ TRenderer }

procedure TRenderer.DrawDiamond(const Canvas: TCanvas; const Center: TPoint;
  const BoundingBoxLength: Integer);
var
  P: array [0..3] of TPoint;
  Length: Integer;
begin
  Length := BoundingBoxLength div 2;

  P[0].X := Center.X;
  P[0].Y := Center.Y - Length;

  P[1].X := Center.X - Length;
  P[1].Y := Center.Y;

  P[2].X := Center.X;
  P[2].Y := Center.Y + Length;

  P[3].X := Center.X + Length;
  P[3].Y := Center.Y;

  Canvas.Polygon(P);
end;

procedure TRenderer.DrawEllipse(const Canvas: TCanvas; const Center: TPoint;
  const BoundingBoxLength: Integer);
begin
  Canvas.Ellipse(
    Center.X - BoundingBoxLength div 2,
    Center.Y - BoundingBoxLength div 2,
    Center.X + BoundingBoxLength div 2,
    Center.Y + BoundingBoxLength div 2);
end;

procedure TRenderer.DrawRectangle(const Canvas: TCanvas; const Center: TPoint;
  const BoundingBoxLength: Integer; const IsRounded: Boolean);
var
  RoundLength: Integer;
begin
  if (IsRounded) then
  begin
    RoundLength := Max(BoundingBoxLength div 10, 2);
    Canvas.RoundRect(
      Center.X - BoundingBoxLength div 2,
      Center.Y - BoundingBoxLength div 2,
      Center.X + BoundingBoxLength div 2,
      Center.Y + BoundingBoxLength div 2,
      RoundLength, RoundLength);
  end
  else
    Canvas.Rectangle(
      Center.X - BoundingBoxLength div 2,
      Center.Y - BoundingBoxLength div 2,
      Center.X + BoundingBoxLength div 2,
      Center.Y + BoundingBoxLength div 2);
end;

procedure TRenderer.DrawShapes(const Canvas: TCanvas; const Rect: TRect);
var
  ShapeType: TShapeType;
  Count, Length, I, J: Integer;
  Size: TSize;
  Center: TPoint;
begin
  ShapeType := FParam.ShapeType;
  Count := Ord(FParam.ViewLayout) + 1;
  Size.cx := Rect.Right - Rect.Left;
  Size.cy := Rect.Bottom - Rect.Top;

  Canvas.Brush.Color := FColors[FParam.ShapeColor];
  Canvas.Pen.Color := clBlack;
  case FParam.BorderStyle of
    bsDash:
      begin
        Canvas.Pen.Style := psDash;
        // Dash pen only works with a width of 1
        Canvas.Pen.Width := 1;
      end;

    bsSolid:
      begin
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Width := FParam.BorderSize;
      end;
  else
    Canvas.Pen.Style := psClear;
  end;

  // Pick up the shorter length.
  Length := Min(Size.cx, Size.cy) div Count;

  // Take margin into count
  Dec(Length, SHAPE_MARGIN);

  if (Length > 0) then
    // Now scale the length based on the parameter
    Length := Length * (Ord(FParam.ShapeSize) + 1) div 3
  else
    Length := 0;

  for I := 0 to Count - 1 do
  begin
    for J := 0 to Count - 1 do
    begin
      Center.Y := Rect.Top + (2 * I + 1) * Size.cy div (2 * Count);
      Center.X := Rect.Left + (2 * J + 1) * Size.cx div (2 * Count);

      case ShapeType of
        stRectangle,
        stRoundedRectangle:
          DrawRectangle(Canvas, Center, Length, (ShapeType = stRoundedRectangle));

        stEllipse:
          DrawEllipse(Canvas, Center, Length);

        stDiamond:
          DrawDiamond(Canvas, Center, Length);
      end;
    end;
  end;
end;

function TRenderer.GetRenderParam: TRenderParam;
begin
  Result := FParam;
end;

procedure TRenderer.Initialize(const Form: TCustomForm);
begin
  FForm := Form;
  FParam.ShapeType := stRectangle;
  FParam.ShapeColor := scRed;
  FParam.ShapeSize := ssSmall;
  FParam.BorderStyle :=bsNone;
  FParam.BorderSize := 1;
  FParam.ViewLayout := vl1;
end;

procedure TRenderer.UpdateRenderParam(const Parameter: TRenderParam);
begin
  FParam := Parameter;
  FForm.Invalidate;
end;

end.
