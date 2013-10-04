unit uBorderSizeHandler;

interface

uses
  Windows,
  ActiveX,
  ComObj,
  UIRibbonApi;

type
  TBorderSizeHandler = class(TInterfacedObject, IUICommandHandler)
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
  SysUtils,
  RibbonConst,
  Math,
  WinApiEx,
  uRenderer,
  uPropertySet;

{ TBorderSizeHandler }

function TBorderSizeHandler.Execute(CommandId: UInt32; Verb: _UIExecutionVerb;
  Key: PUIPropertyKey; CurrentValue: PPropVariant;
  CommandExecutionProperties: IUISimplePropertySet): HRESULT;
var
  Param: TRenderParam;
  Selected: Cardinal;
  Val: TPropVariant;
  S: String;
  NewSize: Integer;
begin
  Result := E_FAIL;
  if (Verb = UIExecutionVerbExecute) then
  begin
    if Assigned(Key) and (Key^ = UI_PKEY_SelectedItem) then
    begin
      Param := Renderer.GetRenderParam;
      Selected := CurrentValue.ulVal;
      case Selected of
        0: Param.BorderSize := 1;
        1: Param.BorderSize := 3;
        2: Param.BorderSize := 5;
        Cardinal(UICollectionInvalidIndex):
          // The new selection is a value that the user typed.
          if (CommandExecutionProperties <> nil) then
          begin
            // The text entered by the user is contained in the property set
            // with the pkey UI_PKEY_Label.
            CommandExecutionProperties.GetValue(UI_PKEY_Label, Val);
            S := Val.bstrVal;
            NewSize := EnsureRange(StrToIntDef(S, 1), 1, 15);
            Param.BorderSize := NewSize;
            PropVariantClear(Val);
          end;
      end;
      Renderer.UpdateRenderParam(Param);
    end;
  end;
end;

function TBorderSizeHandler.UpdateProperty(CommandId: UInt32;
  const Key: TUIPropertyKey; CurrentValue: PPropVariant;
  out NewValue: TPropVariant): HRESULT;
const
  LabelIds: array [0..2] of Integer = (IDS_BORDERSIZE_1, IDS_BORDERSIZE_3,
    IDS_BORDERSIZE_5);
var
  Unk: IUnknown;
  Collection: IUICollection;
  I: Integer;
  Item: TPropertySet;
  ItemIntf: IUISimplePropertySet;
  Lbl: array [0..MAX_RESOURCE_LENGTH - 1] of Char;
  Param: TRenderParam;
begin
  Result := E_FAIL;
  try
    if (Key = UI_PKEY_ItemsSource) then
    begin
      Unk := IUnknown(CurrentValue.pStorage {punkVal});
      if (not Supports(Unk, IUICollection, Collection)) then
        Exit;

      // Populate the combobox with the three default border sizes
      for I := 0 to Length(LabelIds) - 1 do
      begin
        // Create a new property set for each item.
        Item := TPropertySet.Create;
        ItemIntf := Item;

        // Load the label from the resource file.
        LoadString(HInstance, LabelIds[I], Lbl, MAX_RESOURCE_LENGTH);

        // Initialize the property set with the image and label that were just
        // loaded and no category.
        Item.InitializeItemProperties(nil, Lbl, UICollectionInvalidIndex);

        // Add the newly-created property set to the collection supplied by the
        // framework.
        Collection.Add(ItemIntf);
      end;
      Result := S_OK;
    end else if (Key = UI_PKEY_Categories) then
      { A return value of S_FALSE or E_NOTIMPL will result in a gallery with no
        categories. If you return any error other than E_NOTIMPL, the contents of
        the gallery will not display. }
      Result := S_FALSE
    else if (Key = UI_PKEY_SelectedItem) then
      // Use the first size as the default selection.
      Result := UIInitPropertyFromUInt32(Key, 0, NewValue)
    else if (Key = UI_PKEY_StringValue) then
    begin
      // Set the text of the size combobox to the current border size.
      Param := Renderer.GetRenderParam;
      Result := UIInitPropertyFromString(Key, IntToStr(Param.BorderSize), NewValue);
    end
    else if (Key = UI_PKEY_Enabled) then
    begin
      // The border size combobox gets disabled when Dash Line is selected for
      // the border style.
      Param := Renderer.GetRenderParam;
      Result := UIInitPropertyFromBoolean(Key, (Param.BorderStyle <> bsDash), NewValue)
    end;
  except
    on E: EOleSysError do
      Result := E.ErrorCode
    else
      Result := E_FAIL;
  end;
end;

end.
