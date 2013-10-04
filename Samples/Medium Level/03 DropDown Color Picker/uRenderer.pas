unit uRenderer;

interface

uses
  Forms,
  Windows,
  Graphics,
  UIRibbonApi,
  UIRibbonCommands;

const
  DDCP_WIDTH = 4;
  DDCP_HEIGHT = 4;

type
  TColorProperty = record
    ColorType: TUISwatchColorType;
    Color: TColor;
  end;

type
  TRenderer = record
  private
    FForm: TCustomForm;
    FHighlightIndex: Integer;
    FColors: array [0..(DDCP_WIDTH * DDCP_HEIGHT) - 1] of TColorProperty;
    FStoredColor: TColorProperty;
    function GetColor(const Index: Integer): TColor;
  private
    procedure InitializeBrushFromColorProp(const Brush: TBrush;
      const ColorProp: TColorProperty);
  public
    procedure Initialize(const Form: TCustomForm);
    procedure Draw(const Canvas: TCanvas; const RibbonHeight: Integer);
    procedure Execute(const Verb: TUICommandVerb; const Color: TColorProperty);
    procedure ClearColorGrid;

    property Colors[const Index: Integer]: TColor read GetColor;
  end;

var
  Renderer: TRenderer;

implementation

{ TRenderer }

procedure TRenderer.ClearColorGrid;
var
  I: Integer;
begin
  for I := 0 to (DDCP_WIDTH * DDCP_HEIGHT) - 1 do
    FColors[I].ColorType := ctNoColor;
  FHighlightIndex := 0;
  FForm.Invalidate;
end;

procedure TRenderer.Draw(const Canvas: TCanvas; const RibbonHeight: Integer);
var
  R: TRect;
  CellWidth, CellHeight, I, Row, Col: Integer;
begin
  Assert(Assigned(FForm));
  R := FForm.ClientRect;
  Inc(R.Top, RibbonHeight);

  CellHeight := (R.Bottom - R.Top) div DDCP_HEIGHT;
  CellWidth := (R.Right - R.Left) div DDCP_WIDTH;
  for I := 0 to (DDCP_WIDTH * DDCP_HEIGHT) - 1 do
  begin
    InitializeBrushFromColorProp(Canvas.Brush, FColors[I]);
    Col := I mod DDCP_WIDTH;
    Row := I div DDCP_WIDTH;

    if (I = FHighlightIndex) then
    begin
      Canvas.Pen.Style := psInsideFrame;
      Canvas.Pen.Width := 5;
      Canvas.Pen.Color := clYellow
    end
    else
    begin
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Width := 1;
      Canvas.Pen.Color := clBlack
    end;

    Canvas.Rectangle(
      R.Left + Col * CellWidth,
      R.Top + Row * CellHeight,
      R.Left + (Col + 1) * CellWidth,
      R.Top + (Row + 1) * CellHeight);
  end;
end;

procedure TRenderer.Execute(const Verb: TUICommandVerb;
  const Color: TColorProperty);
begin
  FColors[FHighlightIndex] := Color;

  if (Verb = cvCancelPreview) then
    FColors[FHighlightIndex] := FStoredColor;

  if (Verb = cvExecute) then
  begin
    Inc(FHighlightIndex);
    if (FHighlightIndex >= (DDCP_WIDTH * DDCP_HEIGHT)) then
      FHighlightIndex := 0;
    FStoredColor := FColors[FHighlightIndex];
  end;

  FForm.Invalidate;
end;

function TRenderer.GetColor(const Index: Integer): TColor;
begin
  case FColors[Index].ColorType of
    ctNoColor:
      Result := clWhite;

    ctRgb:
      Result := FColors[Index].Color;

    ctAutomatic:
      Result := clWindowText;
  else
    Result := clBlack;
  end;
end;

procedure TRenderer.Initialize(const Form: TCustomForm);
begin
  FForm := Form;
  FHighlightIndex := 0;
end;

procedure TRenderer.InitializeBrushFromColorProp(const Brush: TBrush;
  const ColorProp: TColorProperty);
begin
  case ColorProp.ColorType of
    ctNoColor:
      begin
        // Set brush to white (for no color).
        Brush.Style := bsSolid;
        Brush.Color := clWhite;
      end;

    ctRgb:
      begin
        // Set brush to stored RGB color.
        Brush.Style := bsSolid;
        Brush.Color := ColorProp.Color;
      end;

    ctAutomatic:
      begin
        // Set brush to system color.
        Brush.Style := bsSolid;
        Brush.Color := clWindowText;
      end;
  end;
end;

end.
