unit uSizeAndColorHandler;

interface

uses
  Windows,
  ActiveX,
  ComObj,
  UIRibbonApi;

type
  TSizeAndColorHandler = class(TInterfacedObject, IUICommandHandler)
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
  uPropertySet;

{ TSizeAndColorHandler }

function TSizeAndColorHandler.Execute(CommandId: UInt32; Verb: _UIExecutionVerb;
  Key: PUIPropertyKey; CurrentValue: PPropVariant;
  CommandExecutionProperties: IUISimplePropertySet): HRESULT;
begin
  Result := E_FAIL;
end;

function TSizeAndColorHandler.UpdateProperty(CommandId: UInt32;
  const Key: TUIPropertyKey; CurrentValue: PPropVariant;
  out NewValue: TPropVariant): HRESULT;
const
  CommandIds: array [0..5] of Integer = (IDR_CMD_SMALL, IDR_CMD_MEDIUM,
    IDR_CMD_LARGE, IDR_CMD_RED, IDR_CMD_GREEN, IDR_CMD_BLUE);
  CategoryIds: array [0..5] of Integer = (0, 0, 0, 1, 1, 1);
var
  Unk: IUnknown;
  Collection: IUICollection;
  Size, Color, Command: TPropertySet;
  SizeIntf, ColorIntf, CommandIntf: IUISimplePropertySet;
  SizeLabel, ColorLabel: array [0..MAX_RESOURCE_LENGTH - 1] of Char;
  I: Integer;
begin
  Result := E_FAIL;
  try
    if (Key = UI_PKEY_Categories) then
    begin
      Unk := IUnknown(CurrentValue.pStorage {punkVal});
      if (not Supports(Unk, IUICollection, Collection)) then
        Exit;

      // Create a property set for the Size category.
      Size := TPropertySet.Create;
      SizeIntf := Size;

      // Load the label for the Size category from the resource file.
      LoadString(HInstance, IDS_SIZE_CATEGORY, SizeLabel, MAX_RESOURCE_LENGTH);

      // Initialize the property set with the label that was just loaded and a
      // category id of 0.
      Size.InitializeCategoryProperties(SizeLabel, 0);

      // Add the newly-created property set to the collection supplied by the
      // framework.
      Collection.Add(SizeIntf);

      // Create a property set for the Color category.
      Color := TPropertySet.Create;
      ColorIntf := Color;

      // Load the label for the Color category from the resource file.
      LoadString(HInstance, IDS_COLOR_CATEGORY, ColorLabel, MAX_RESOURCE_LENGTH);

      // Initialize the property set with the label that was just loaded and a
      // category id of 1.
      Color.InitializeCategoryProperties(ColorLabel, 1);

      // Add the newly-created property set to the collection supplied by the
      // framework.
      Collection.Add(ColorIntf);

      Result := S_OK;
    end
    else if (Key = UI_PKEY_ItemsSource) then
    begin
      Unk := IUnknown(CurrentValue.pStorage {punkVal});
      if (not Supports(Unk, IUICollection, Collection)) then
        Exit;

      // Populate the gallery with the three size and three colors in two
      // separate categories.
      for I := 0 to Length(CommandIds) - 1 do
      begin
        // Create a new property set for each item.
        Command := TPropertySet.Create;
        CommandIntf := Command;

        // Initialize the property set with the appropriate command id and
        // category id and type Boolean (which makes these appear as ToggleButtons).
        Command.InitializeCommandProperties(CategoryIds[I], CommandIds[I],
          UICommandTypeBoolean);

        // Add the newly-created property set to the collection supplied by the
        // framework.
        Collection.Add(CommandIntf);
      end;

      Result := S_OK;
    end;
  except
    on E: EOleSysError do
      Result := E.ErrorCode
    else
      Result := E_FAIL;
  end;
end;

end.
