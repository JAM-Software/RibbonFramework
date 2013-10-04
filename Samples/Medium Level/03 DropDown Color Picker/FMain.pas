unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UIRibbonForm, UIRibbonCommands, RibbonConsts, uRenderer;

type
  TFormMain = class(TUIRibbonForm)
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FCmdUpdate: TUICommandAction;
    FCmdClear: TUICommandAction;
    FCmdThemedColorPicker: TUICommandColorAnchor;
    FCmdStandardColorPicker: TUICommandColorAnchor;
    FCmdHighlightColorPicker: TUICommandColorAnchor;

    procedure CmdUpdateExecute(const Args: TUICommandActionEventArgs);
    procedure CmdClearExecute(const Args: TUICommandActionEventArgs);
    procedure ColorPickerExecute(const Args: TUICommandColorEventArgs);
  strict protected
    procedure RibbonLoaded; override;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

{ TFormMain }

procedure TFormMain.CmdClearExecute(const Args: TUICommandActionEventArgs);
begin
  if (Args.Verb = cvExecute) then
    Renderer.ClearColorGrid;
end;

procedure TFormMain.CmdUpdateExecute(const Args: TUICommandActionEventArgs);
var
  Colors: TArray<TColor>;
  I: Integer;
begin
  if (Args.Verb = cvExecute) then
  begin
    { Set the standard colors to the colors currently in the grid. }
    SetLength(Colors, DDCP_WIDTH * DDCP_HEIGHT);
    for I := 0 to Length(Colors) - 1 do
      Colors[I] := Renderer.Colors[I];
    FCmdStandardColorPicker.StandardColors := Colors;

    { Update the tooltops for the colors. }
    Ribbon.InvalidateUICommand(FCmdStandardColorPicker, upStandardColorsTooltips);
  end;
end;

procedure TFormMain.ColorPickerExecute(const Args: TUICommandColorEventArgs);
var
  ColorProp: TColorProperty;
begin
  ColorProp.ColorType := Args.ColorType;
  ColorProp.Color := Args.CustomColor;
  Renderer.Execute(Args.Verb, ColorProp);
end;

procedure TFormMain.FormPaint(Sender: TObject);
begin
  Renderer.Draw(Canvas, Ribbon.Height);
end;

procedure TFormMain.FormResize(Sender: TObject);
var
  R: TRect;
begin
  { Ribbon may not exist yet at startup }
  if Assigned(Ribbon) then
  begin
    { Invalidate the client area below the ribbon }
    R := ClientRect;
    R.Top := Ribbon.Height;
    InvalidateRect(Handle, R, True);
  end;
end;

procedure TFormMain.RibbonLoaded;
const
  STANDARD_COLORS: array [0..9] of TColor = (
    $0000C0, // Dark red
    $0000FF, // Red
    $00C0FF, // Gold
    $00FFFF, // Yellow
    $50D092, // Lime
    $50B000, // Dark green
    $F0B000, // Turquoise
    $C06000, // Dark blue
    $602000, // Dark blue
    $A03060);// Purple
var
  I: Integer;
  Colors: TArray<TColor>;
begin
  inherited;
  FCmdUpdate := Ribbon[IDR_CMD_UPDATE] as TUICommandAction;
  FCmdClear := Ribbon[IDR_CMD_CLEAR] as TUICommandAction;
  FCmdUpdate.OnExecute := CmdUpdateExecute;
  FCmdClear.OnExecute := CmdClearExecute;

  FCmdThemedColorPicker := Ribbon[IDR_CMD_THEMEDDCP] as TUICommandColorAnchor;
  FCmdStandardColorPicker := Ribbon[IDR_CMD_STANDARDDDCP] as TUICommandColorAnchor;
  FCmdHighlightColorPicker := Ribbon[IDR_CMD_HIGHLIGHTDDCP] as TUICommandColorAnchor;

  FCmdThemedColorPicker.Color := clLime;
  FCmdThemedColorPicker.OnExecute := ColorPickerExecute;

  FCmdStandardColorPicker.Color := clRed;
  FCmdStandardColorPicker.OnExecute := ColorPickerExecute;

  FCmdHighlightColorPicker.Color := clBlue;
  FCmdHighlightColorPicker.OnExecute := ColorPickerExecute;

  { Set the standard colors }
  SetLength(Colors, DDCP_WIDTH * DDCP_HEIGHT);
  for I := 0 to Length(Colors) - 1 do
    Colors[I] := STANDARD_COLORS[I mod 10];
  FCmdStandardColorPicker.StandardColors := Colors;

  { Update the tooltips for the standard colors }
  Ribbon.InvalidateUICommand(FCmdStandardColorPicker, upStandardColorsTooltips);

  Renderer.Initialize(Self);
end;

end.
