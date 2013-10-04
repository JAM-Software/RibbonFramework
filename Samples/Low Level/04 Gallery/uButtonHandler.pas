unit uButtonHandler;

interface

uses
  Windows,
  ActiveX,
  UIRibbonApi;

type
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

implementation

uses
  RibbonConst,
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
var
  Param: TRenderParam;
begin
  Result := E_FAIL;
  if (Verb = UIExecutionVerbExecute) then
  begin
    if Assigned(Key) and (Key^ = UI_PKEY_BooleanValue) then
    begin
      // The user clicked one of the toggle buttons in the command gallery.
      Param := Renderer.GetRenderParam;
      case CommandId of
        IDR_CMD_RED,
        IDR_CMD_GREEN,
        IDR_CMD_BLUE:
          Param.ShapeColor := TShapeColor(CommandId - IDR_CMD_RED);

        IDR_CMD_SMALL,
        IDR_CMD_MEDIUM,
        IDR_CMD_LARGE:
          Param.ShapeSize := TShapeSize(CommandId - IDR_CMD_SMALL);
      end;

      Renderer.UpdateRenderParam(Param);
      // Update the BooleanValue property on all commands to deselect the
      // previous size or color.
      FFramework.InvalidateUICommand(UIAllCommands, [UIInvalidationsProperty],
        Key);
    end;
  end;
end;

function TButtonHandler.UpdateProperty(CommandId: UInt32;
  const Key: TUIPropertyKey; CurrentValue: PPropVariant;
  out NewValue: TPropVariant): HRESULT;
var
  Param: TRenderParam;
begin
  Result := E_FAIL;
  if (Key = UI_PKEY_BooleanValue) then
  begin
    Param := Renderer.GetRenderParam;

    // The currently active size and color will appear selected; the others
    // will appear deselected.
    if (CommandId = UInt32(Ord(Param.ShapeSize) + IDR_CMD_SMALL)) or
       (CommandId = UInt32(Ord(Param.ShapeColor) + IDR_CMD_RED))
    then
      Result := UIInitPropertyFromBoolean(Key, True, NewValue)
    else
      Result := UIInitPropertyFromBoolean(Key, False, NewValue);
  end;
end;

end.
