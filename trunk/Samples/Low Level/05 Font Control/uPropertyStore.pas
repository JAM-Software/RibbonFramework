unit uPropertyStore;

interface

uses
  PropSys,
  RichEdit;

// Convert from CHARFORMAT2 to IPropertyStore so it can be passed to the font control.
procedure GetIPropStoreFromCharFormat2(const CharFormat: TCharFormat2;
  const Props: IPropertyStore);

// Convert from IPropertyStore to CHARFORMAT2 so it can be used formatting text.
procedure GetCharFormat2FromIPropStore(const Props: IPropertyStore;
  out CharFormat: TCharFormat2);

implementation

uses
  SysUtils,
  ActiveX,
  WinApiEx,
  UIRibbonApi;

const
  TWIPS_PER_POINT = 20; // For setting font size in CHARFORMAT2.

procedure GetIPropStoreFromCharFormat2(const CharFormat: TCharFormat2;
  const Props: IPropertyStore);
var
  PropVar: TPropVariant;
  DecSize: TDecimal;

  procedure SetProp(const Prop: TUIPropertyKey; const Mask, Effect: Cardinal);
  var
    PropVar: TPropVariant;
  begin
    PropVariantInit(PropVar);
    if ((CharFormat.dwMask and Mask) <> 0) then
    begin
      // Set the effect value to UIFontPropertiesSet or UIFontPropertiesNotSet.
      if ((CharFormat.dwEffects and Effect) <> 0) then
        UIInitPropertyFromUInt32(Prop, Ord(UIFontPropertiesSet), PropVar)
      else
        UIInitPropertyFromUInt32(Prop, Ord(UIFontPropertiesNotSet), PropVar);
    end
    else
      // The effect value is not available so set it to UIFontPropertiesNotAvailable.
      UIInitPropertyFromUInt32(Prop, Ord(UIFontPropertiesNotAvailable), PropVar);
    Props.SetValue(TPropertyKey(Prop), PropVar);
    PropVariantClear(PropVar);
  end;

begin
  SetProp(UI_PKEY_FontProperties_Bold, CFM_BOLD, CFE_BOLD);
  SetProp(UI_PKEY_FontProperties_Italic, CFM_ITALIC, CFE_ITALIC);
  SetProp(UI_PKEY_FontProperties_Underline, CFM_UNDERLINE, CFE_UNDERLINE);
  SetProp(UI_PKEY_FontProperties_Strikethrough, CFM_STRIKEOUT, CFE_STRIKEOUT);

  PropVariantInit(PropVar);
  if ((CharFormat.dwMask and CFE_SUBSCRIPT) <> 0) then
  begin
    if ((CharFormat.dwMask and CFM_SUBSCRIPT) <> 0) and ((CharFormat.dwEffects and CFE_SUBSCRIPT) <> 0) then
      // Set the vertical positioning value to UIFontVerticalPositionSubscript.
      UIInitPropertyFromUInt32(UI_PKEY_FontProperties_VerticalPositioning, Ord(UIFontVerticalPositionSubscript), PropVar)
    else if ((CharFormat.dwEffects and CFE_SUPERSCRIPT) <> 0) then
      // Set the vertical positioning value to UIFontVerticalPositionSuperscript.
      UIInitPropertyFromUInt32(UI_PKEY_FontProperties_VerticalPositioning, Ord(UIFontVerticalPositionSuperscript), PropVar);
  end
  else if ((CharFormat.dwMask and CFM_OFFSET) <> 0) then
  begin
    if (CharFormat.yOffset > 0) then
      // Set the vertical positioning value to UIFontVerticalPositionSuperscript.
      UIInitPropertyFromUInt32(UI_PKEY_FontProperties_VerticalPositioning, Ord(UIFontVerticalPositionSuperscript), PropVar)
    else if (CharFormat.yOffset < 0) then
      // Set the vertical positioning value to UIFontVerticalPositionSubscript.
      UIInitPropertyFromUInt32(UI_PKEY_FontProperties_VerticalPositioning, Ord(UIFontVerticalPositionSubscript), PropVar)
    else
      // The value is not available so set the vertical positioning value to UIFontVerticalPositionNotAvailable.
      UIInitPropertyFromUInt32(UI_PKEY_FontProperties_VerticalPositioning, Ord(UIFontVerticalPositionNotAvailable), PropVar);
  end;
  // Set UI_PKEY_FontProperties_VerticalPositioning value in property store.
  Props.SetValue(TPropertyKey(UI_PKEY_FontProperties_VerticalPositioning), PropVar);
  PropVariantClear(PropVar);

  PropVariantInit(PropVar);
  if ((CharFormat.dwMask and CFM_FACE) <> 0) then
    // Set the font family value to the font name.
    UIInitPropertyFromString(UI_PKEY_FontProperties_Family, CharFormat.szFaceName, PropVar)
  else
    // Font family name is not available so set it to blank string.
    UIInitPropertyFromString(UI_PKEY_FontProperties_Family, '', PropVar);
  // Set UI_PKEY_FontProperties_Family value in property store.
  Props.SetValue(TPropertyKey(UI_PKEY_FontProperties_Family), PropVar);
  PropVariantClear(PropVar);

  PropVariantInit(PropVar);
  if ((CharFormat.dwMask and CFM_SIZE) <> 0) then
    // Font size value is available so get the font size.
    VarDecFromR8(CharFormat.yHeight / TWIPS_PER_POINT, DecSize)
  else
    // The font size is not available so set it to zero.
    VarDecFromI4(0, DecSize);
  // Set UI_PKEY_FontProperties_Size value in property store.
  UIInitPropertyFromDecimal(UI_PKEY_FontProperties_Size, DecSize, PropVar);
  Props.SetValue(TPropertyKey(UI_PKEY_FontProperties_Size), PropVar);
  PropVariantClear(PropVar);

  if ((CharFormat.dwMask and CFM_COLOR) <> 0) and ((CharFormat.dwEffects and CFE_AUTOCOLOR) = 0) then
  begin
    // There is a color value so set the type to UISwatchColorTypeRgb in property store.
    PropVariantInit(PropVar);
    UIInitPropertyFromUInt32(UI_PKEY_FontProperties_ForegroundColorType, Ord(UISwatchColorTypeRgb), PropVar);
    Props.SetValue(TPropertyKey(UI_PKEY_FontProperties_ForegroundColorType), PropVar);
    PropVariantClear(PropVar);

    // Set the color value in property store.
    PropVariantInit(PropVar);
    UIInitPropertyFromUInt32(UI_PKEY_FontProperties_ForegroundColor, CharFormat.crTextColor, PropVar);
    Props.SetValue(TPropertyKey(UI_PKEY_FontProperties_ForegroundColor), PropVar);
    PropVariantClear(PropVar);
  end
  else if ((CharFormat.dwMask and CFM_COLOR) <> 0) and ((CharFormat.dwEffects and CFE_AUTOCOLOR) <> 0) then
  begin
    // The color is automatic color so set the type to UISwatchColorTypeAutomatic in property store.
    PropVariantInit(PropVar);
    UIInitPropertyFromUInt32(UI_PKEY_FontProperties_ForegroundColorType, Ord(UISwatchColorTypeAutomatic), PropVar);
    Props.SetValue(TPropertyKey(UI_PKEY_FontProperties_ForegroundColorType), PropVar);
    PropVariantClear(PropVar);
  end;

  if ((CharFormat.dwMask and CFM_BACKCOLOR) <> 0) and ((CharFormat.dwEffects and CFE_AUTOBACKCOLOR) = 0) then
  begin
    // There is a color value so set the type to UISwatchColorTypeRgb in property store.
    PropVariantInit(PropVar);
    UIInitPropertyFromUInt32(UI_PKEY_FontProperties_BackgroundColorType, Ord(UISwatchColorTypeRgb), PropVar);
    Props.SetValue(TPropertyKey(UI_PKEY_FontProperties_BackgroundColorType), PropVar);
    PropVariantClear(PropVar);

    // Set the color value in property store.
    PropVariantInit(PropVar);
    UIInitPropertyFromUInt32(UI_PKEY_FontProperties_BackgroundColor, CharFormat.crBackColor, PropVar);
    Props.SetValue(TPropertyKey(UI_PKEY_FontProperties_BackgroundColor), PropVar);
    PropVariantClear(PropVar);
  end
  else
  begin
    // There is no color so set the type to UI_SWATCHCOLORTYPE_NOCOLOR in property store.
    PropVariantInit(PropVar);
    UIInitPropertyFromUInt32(UI_PKEY_FontProperties_BackgroundColorType, Ord(UISwatchColorTypeNoColor), PropVar);
    Props.SetValue(TPropertyKey(UI_PKEY_FontProperties_BackgroundColorType), PropVar);
    PropVariantClear(PropVar);
  end;
end;

procedure GetCharFormat2FromIPropStore(const Props: IPropertyStore;
  out CharFormat: TCharFormat2);
var
  PropVar: TPropVariant;
  Value: Integer;
  Family: String;
  DecSize: TDecimal;
  Size: Double;

  procedure GetProp(const Prop: TUIPropertyKey; const Mask, Effect: Cardinal);
  var
    PropVar: TPropVariant;
    Value: Integer;
  begin
    // Get the effect value from the property store.
    PropVariantInit(PropVar);
    if Succeeded(Props.GetValue(TPropertyKey(Prop), PropVar)) then
    begin
      UIPropertyToUInt32(Prop, PropVar, Cardinal(Value));
      if (Value <> Ord(UIFontPropertiesNotAvailable)) then
      begin
        // There is a value assigned, so set the corresponding members in CharFormat2 variable.
        CharFormat.dwMask := CharFormat.dwMask or Mask;
        if (Value = Ord(UIFontPropertiesSet)) then
          CharFormat.dwEffects := CharFormat.dwEffects or Effect;
      end;
    end;
    PropVariantClear(PropVar);
  end;

begin
  FillChar(CharFormat, SizeOf(CharFormat), 0);
  CharFormat.cbSize := SizeOf(CharFormat);

  GetProp(UI_PKEY_FontProperties_Bold, CFM_BOLD, CFE_BOLD);
  GetProp(UI_PKEY_FontProperties_Italic, CFM_ITALIC, CFE_ITALIC);
  GetProp(UI_PKEY_FontProperties_Underline, CFM_UNDERLINE, CFE_UNDERLINE);
  GetProp(UI_PKEY_FontProperties_Strikethrough, CFM_STRIKEOUT, CFE_STRIKEOUT);

  // Get the vertical positioning value from the property store.
  PropVariantInit(PropVar);
  if Succeeded(Props.GetValue(TPropertyKey(UI_PKEY_FontProperties_VerticalPositioning), PropVar)) then
  begin
    UIPropertyToUInt32(UI_PKEY_FontProperties_VerticalPositioning, PropVar, Cardinal(Value));
    if (Value <> Ord(UIFontVerticalPositionNotAvailable)) then
    begin
      // There is a value assigned, so set the corresponding members in CharFormat2 variable.
      CharFormat.dwMask := CharFormat.dwMask or (CFM_SUBSCRIPT or CFM_SUPERSCRIPT);
      if (Value <> Ord(UIFontVerticalPositionNotSet)) then
      begin
        if (Value = Ord(UIFontVerticalPositionSuperscript)) then
          CharFormat.dwEffects := CharFormat.dwEffects or CFE_SUPERSCRIPT
        else
          CharFormat.dwEffects := CharFormat.dwEffects or CFE_SUBSCRIPT;
      end;
    end;
  end;
  PropVariantClear(PropVar);

  // Get the font family value from the property store.
  PropVariantInit(PropVar);
  if Succeeded(Props.GetValue(TPropertyKey(UI_PKEY_FontProperties_Family), PropVar)) then
  begin
    UIPropertyToString(UI_PKEY_FontProperties_Family, PropVar, Family);
    // Blank string is used as "Not Available" value.
    if (Family <> '') then
    begin
      // There is a value assigned, so set the corresponding members in CharFormat2 variable.
      CharFormat.dwMask := CharFormat.dwMask or CFM_FACE;
      StrLCopy(CharFormat.szFaceName, PChar(Family), Length(CharFormat.szFaceName));
    end;
  end;
  PropVariantClear(PropVar);

  // Get the font size value from the property store.
  PropVariantInit(PropVar);
  if Succeeded(Props.GetValue(TPropertyKey(UI_PKEY_FontProperties_Size), PropVar)) then
  begin
    UIPropertyToDecimal(UI_PKEY_FontProperties_Size, PropVar, DecSize);
    VarR8FromDec(@DecSize, Size);
    // Zero is used as "Not Available" value.
    if (Size > 0) then
    begin
      // There is a value assigned, so set the corresponding members in CharFormat2 variable.
      CharFormat.dwMask := CharFormat.dwMask or CFM_SIZE;
      CharFormat.yHeight := Trunc(Size * TWIPS_PER_POINT);
    end;
  end;
  PropVariantClear(PropVar);

  // Get the foreground color type value from the property store.
  PropVariantInit(PropVar);
  if Succeeded(Props.GetValue(TPropertyKey(UI_PKEY_FontProperties_ForegroundColorType), PropVar)) then
  begin
    UIPropertyToUInt32(UI_PKEY_FontProperties_ForegroundColorType, PropVar, Cardinal(Value));
    if (Value = Ord(UISwatchColorTypeAutomatic)) then
    begin
      // The color type is automatic, so set the corresponding members in CharFormat2 variable.
      CharFormat.dwMask := CharFormat.dwMask or CFM_COLOR;
      CharFormat.dwEffects := CharFormat.dwEffects or CFE_AUTOCOLOR;
    end;
  end;
  PropVariantClear(PropVar);

  // Get the foreground color value from the property store.
  PropVariantInit(PropVar);
  if Succeeded(Props.GetValue(TPropertyKey(UI_PKEY_FontProperties_ForegroundColor), PropVar)) then
  begin
    UIPropertyToUInt32(UI_PKEY_FontProperties_ForegroundColor, PropVar, Cardinal(Value));
    CharFormat.dwMask := CharFormat.dwMask or CFM_COLOR;
    CharFormat.crTextColor := Value;
  end;
  PropVariantClear(PropVar);

  // Get the background color type value from the property store.
  PropVariantInit(PropVar);
  if Succeeded(Props.GetValue(TPropertyKey(UI_PKEY_FontProperties_BackgroundColorType), PropVar)) then
  begin
    UIPropertyToUInt32(UI_PKEY_FontProperties_BackgroundColorType, PropVar, Cardinal(Value));
    if (Value = Ord(UISwatchColorTypeNoColor)) then
    begin
      // The color type is no color, so set the corresponding members in CharFormat2 variable.
      CharFormat.dwMask := CharFormat.dwMask or CFM_COLOR;
      CharFormat.dwEffects := CharFormat.dwEffects or CFE_AUTOBACKCOLOR;
    end;
  end;
  PropVariantClear(PropVar);

  // Get the background color value from the property store.
  PropVariantInit(PropVar);
  if Succeeded(Props.GetValue(TPropertyKey(UI_PKEY_FontProperties_BackgroundColor), PropVar)) then
  begin
    UIPropertyToUInt32(UI_PKEY_FontProperties_BackgroundColor, PropVar, Cardinal(Value));
    CharFormat.dwMask := CharFormat.dwMask or CFM_BACKCOLOR;
    CharFormat.crBackColor := Value;
  end;
  PropVariantClear(PropVar);
end;

end.
