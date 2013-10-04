unit uLayoutHandler;

interface

uses
  Windows,
  ActiveX,
  ComObj,
  UIRibbonApi;

type
  TLayoutHandler = class(TInterfacedObject, IUICommandHandler)
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
  uPropertySet,
  uRenderer;

{ TLayoutHandler }

function TLayoutHandler.Execute(CommandId: UInt32; Verb: _UIExecutionVerb;
  Key: PUIPropertyKey; CurrentValue: PPropVariant;
  CommandExecutionProperties: IUISimplePropertySet): HRESULT;
var
  Param: TRenderParam;
  Selected: Cardinal;
begin
  Result := E_FAIL;
  if (Verb = UIExecutionVerbExecute) then
  begin
    if Assigned(Key) and (Key^ = UI_PKEY_SelectedItem) then
    begin
      // Get the newly-selected layout and update the render parameters with it.
      Param := Renderer.GetRenderParam;
      Selected := CurrentValue.ulVal;
      Param.ViewLayout := TViewLayout(Selected);
      Renderer.UpdateRenderParam(Param);
      Result := S_OK;
    end;
  end;
end;

function TLayoutHandler.UpdateProperty(CommandId: UInt32;
  const Key: TUIPropertyKey; CurrentValue: PPropVariant;
  out NewValue: TPropVariant): HRESULT;
const
  LabelIds: array [0..2] of Integer = (IDS_LAYOUT_1, IDS_LAYOUT_2,
    IDS_LAYOUT_3);
var
  Unk: IUnknown;
  Collection: IUICollection;
  I: Integer;
  Item: TPropertySet;
  ItemIntf: IUISimplePropertySet;
  Lbl: array [0..MAX_RESOURCE_LENGTH - 1] of Char;
begin
  Result := E_FAIL;
  try
    if (Key = UI_PKEY_ItemsSource) then
    begin
      Unk := IUnknown(CurrentValue.pStorage {punkVal});
      if (not Supports(Unk, IUICollection, Collection)) then
        Exit;

      // Populate the combobox with the three layout options.
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
      Exit(S_FALSE);
  except
    on E: EOleSysError do
      Result := E.ErrorCode
    else
      Result := E_FAIL;
  end;
end;

end.
