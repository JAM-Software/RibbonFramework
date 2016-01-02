unit UIRibbonUtils;

interface

uses
  Graphics,
  GraphUtil,
  UIRibbonApi;

function HsbToColor(const Hsb: TUIHsbColor): TColor; overload;
function HsbToColor(const H, S, B: Integer): TColor; overload;
function HsbToHsbColor(const H, S, B: Integer): TUIHsbColor; inline;
procedure HsbColorToHsb(const Color: TUIHsbColor; out H, S, B: Integer); inline;
function ColorToHsb(const Color: TColor): TUIHsbColor;

function CreateStringFromResource(const ResourceId: Integer): String;


const
  cPackageTitle = 'Ribbon Framework';
  cRegistryPath = 'Software\Embarcadero\' + cPackageTitle;
  cRegistryKeyDesigner = 'RibbonDesigner';



implementation

uses
  Windows,
  ActiveX;

function HsbToColor(const Hsb: TUIHsbColor): TColor;
var
  H, S, B: Integer;
begin
  H := Hsb and $FF;
  S := (Hsb shr 8) and $FF;
  B := (Hsb shr 16) and $FF;
  Result := HsbToColor(H, S, B);
end;

function HsbToColor(const H, S, B: Integer): TColor; overload;
var
  HH, SS, LL: Integer;
  LD: Double;
begin
  { Convert B to L }
  LD := Exp((B - 257.7) / 149.9);

  { ColorHLSToRGB requires H, L and S to be in 0..240 range. }
  HH := (H * 240) div 255;
  SS := (S * 240) div 255;
  LL := Round(LD * 240);

  Result := ColorHLSToRGB(HH, LL, SS);
end;

function HsbToHsbColor(const H, S, B: Integer): TUIHsbColor; inline;
begin
  Result := H or (S shl 8) or (B shl 16);
end;

procedure HsbColorToHsb(const Color: TUIHsbColor; out H, S, B: Integer); inline;
begin
  H := (Color and $FF);
  S := (Color shr 8) and $FF;
  B := (Color shr 16) and $FF;
end;

function ColorToHsb(const Color: TColor): TUIHsbColor;
var
  H, L, S: Word;
  B: Integer;
  LD: Double;
begin
  ColorRGBToHLS(ColorToRGB(Color), H, L, S);
  H := (H * 255) div 240;
  S := (S * 255) div 240;
  LD := L / 240;
  if (LD > 0.9821) then
    B := 255
  else if (LD < 0.1793) then
    B := 0
  else
    B := Trunc(257.7 + 149.9 * Ln(LD) + 0.5);
  Result := UIHsb(H, S, B);
end;

function CreateStringFromResource(const ResourceId: Integer): String;
var
  Buffer: array [0..4095] of Char;
begin
  SetString(Result, Buffer,
    LoadString(HInstance, ResourceId, Buffer, Length(Buffer)));
end;

initialization

end.
