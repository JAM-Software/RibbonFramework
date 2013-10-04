unit uCommandHandler;

interface

uses
  Windows,
  Graphics,
  ActiveX,
  UIRibbonApi;

type
  { CLASS: TButtonHandler(IUICommandHandler)
    PURPOSE: Implements interface IUICommandHandler.
    COMMENTS:
      This is a command handler used by button commands in this sample.
      IUICommandHandler should be returned by the application during command
      creation. }
  TButtonHandler = class(TInterfacedObject, IUICommandHandler)
  private
    FFramework: IUIFramework;
  private
    { IUICommandHandler }
    function Execute(CommandId: UInt32; Verb: _UIExecutionVerb;
      Key: PUIPropertyKey; CurrentValue: PPropVariant;
      CommandExecutionProperties: IUISimplePropertySet): HRESULT; stdcall;
    function UpdateProperty(CommandId: UInt32; const Key: TUIPropertyKey;
      CurrentValue: PPropVariant; out NewValue: TPropVariant): HRESULT; stdcall;
  public
    constructor Create(const Framework: IUIFramework);
  end;

type
  { CLASS: TColorPickerHandler(IUICommandHandler)
    PURPOSE: Implements interface IUICommandHandler.
    COMMENTS:
      This is a command handler used by color picker in this sample.
      IUICommandHandler should be returned by the application during command
      creation. }
  TColorPickerHandler = class(TInterfacedObject, IUICommandHandler)
  private
    const COLORS: array [0..9] of TColor = (
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
  private
    FInitialized: Boolean;
  private
    { IUICommandHandler }
    function Execute(CommandId: UInt32; Verb: _UIExecutionVerb;
      Key: PUIPropertyKey; CurrentValue: PPropVariant;
      CommandExecutionProperties: IUISimplePropertySet): HRESULT; stdcall;
    function UpdateProperty(CommandId: UInt32; const Key: TUIPropertyKey;
      CurrentValue: PPropVariant; out NewValue: TPropVariant): HRESULT; stdcall;
  end;

implementation

uses
  ComObj,
  WinApiEx,
  RibbonConsts,
  uRenderer;

{ TButtonHandler }

constructor TButtonHandler.Create(const Framework: IUIFramework);
begin
  inherited Create;
  FFramework := Framework;
end;

function TButtonHandler.Execute(CommandId: UInt32; Verb: _UIExecutionVerb;
  Key: PUIPropertyKey; CurrentValue: PPropVariant;
  CommandExecutionProperties: IUISimplePropertySet): HRESULT;
begin
  Result := S_OK;
  try
    if (CommandId = IDR_CMD_UPDATE) and (Verb = UIExecutionVerbExecute) then
    begin
      // Invalidate customized standard DDCP to update the color chips.
      FFramework.InvalidateUICommand(IDR_CMD_STANDARDDDCP,
        [UIInvalidationsProperty], @UI_PKEY_StandardColors);

      // Invalidate customized standard DDCP to update color tooltips.
      FFramework.InvalidateUICommand(IDR_CMD_STANDARDDDCP,
        [UIInvalidationsProperty], @UI_PKEY_StandardColorsTooltips);
    end;

    if (CommandId = IDR_CMD_CLEAR) and (Verb = UIExecutionVerbExecute) then
      // Clear color grid.
      Renderer.ClearColorGrid;
  except
    on E: EOleSysError do
      Result := E.ErrorCode
    else
      Result := E_FAIL;
  end;
end;

function TButtonHandler.UpdateProperty(CommandId: UInt32;
  const Key: TUIPropertyKey; CurrentValue: PPropVariant;
  out NewValue: TPropVariant): HRESULT;
begin
  Result := S_OK;
end;

{ TColorPickerHandler }

function TColorPickerHandler.Execute(CommandId: UInt32; Verb: _UIExecutionVerb;
  Key: PUIPropertyKey; CurrentValue: PPropVariant;
  CommandExecutionProperties: IUISimplePropertySet): HRESULT;
var
  ColorProp: TColorProperty;
  Color: TColor;
  ColorType: _UISwatchColorType;
  Value: TPropVariant;
begin
  Result := S_OK;
  Color := clBlack;
  ColorType := UISwatchColorTypeNoColor;

  try
    case Verb of
      UIExecutionVerbExecute,
      UIExecutionVerbPreview,
      UIExecutionVerbCancelPreview:
        begin
          // The Ribbon framework passes color type as the primary property.
          Result := UIPropertyToUInt32(UI_PKEY_ColorType, CurrentValue^,
            Cardinal(ColorType));
          if Failed(Result) then
            Exit;

          // The Ribbon framework passes color as additional property if the
          // color type is RGB.
          if (ColorType = UISwatchColorTypeRgb) and Assigned(CommandExecutionProperties) then
          begin
            // Retrieve color
            CommandExecutionProperties.GetValue(UI_PKEY_Color, Value);
            UIPropertyToUInt32(UI_PKEY_Color, Value, Cardinal(Color));
          end;

          ColorProp.Color := Color;
          ColorProp.ColorType := ColorType;

          // Handle the execution event.
          Renderer.Execute(Verb, ColorProp);
        end;
    end;
  except
    on E: EOleSysError do
      Result := E.ErrorCode
    else
      Result := E_FAIL;
  end;
end;

function TColorPickerHandler.UpdateProperty(CommandId: UInt32;
  const Key: TUIPropertyKey; CurrentValue: PPropVariant;
  out NewValue: TPropVariant): HRESULT;
var
  StandardColors: array [0..(DDCP_WIDTH * DDCP_HEIGHT) - 1] of TColor;
  I: Integer;
begin
  Result := S_OK;
  if (Key = UI_PKEY_ColorType) then
  begin
    // Initialize the DDCP color type.
    Result := UIInitPropertyFromUInt32(Key, Ord(UISwatchColorTypeRgb), NewValue);
    if Failed(Result) then
      Exit;
  end
  else if (Key = UI_PKEY_Color) then
  begin
    // Initialize the default selected color for each DDCP.
    case CommandId of
      IDR_CMD_THEMEDDCP:
        Result := UIInitPropertyFromUInt32(Key, RGB(0,255,0), NewValue);

      IDR_CMD_STANDARDDDCP:
        Result := UIInitPropertyFromUInt32(Key, RGB(255,0,0), NewValue);

      IDR_CMD_HIGHLIGHTDDCP:
        Result := UIInitPropertyFromUInt32(Key, RGB(0,0,255), NewValue);
    end;
    if Failed(Result) then
      Exit;
  end;

  if (CommandId = IDR_CMD_STANDARDDDCP) then
  begin
    // Customize standard DDCP.
    if (Key = UI_PKEY_StandardColors) then
    begin
      // Customize color chips in standard DDCP.
      for I := 0 to Length(StandardColors) - 1 do
      begin
        if (not FInitialized) then
          StandardColors[I] := COLORS[I mod 10]
        else
          StandardColors[I] := Renderer.Colors[I];
      end;

      Result := InitPropVariantFromUInt32Vector(@StandardColors[0],
        DDCP_WIDTH * DDCP_HEIGHT, NewValue);
      if Failed(Result) then
        Exit;

      FInitialized := True;
    end
    else if (Key = UI_PKEY_StandardColorsTooltips) then
      // Customize color tooltips in standard DDCP.
      // Return S_OK to let the API determine tooltips automatically.
      Result := S_OK;
  end;
end;

end.
