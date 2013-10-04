unit uBorderStyleHandler;

interface

uses
  Windows,
  ActiveX,
  ComObj,
  UIRibbonApi;

type
  TBorderStyleHandler = class(TInterfacedObject, IUICommandHandler)
  private
    FFramework: IUIFramework;
    FImageFromBitmapFactory: IUIImageFromBitmap;
  private
    function CreateUIImageFromBitmapResource(const Resource: LPCTSTR): IUIImage;
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
  Forms,
  SysUtils,
  RibbonConst,
  uPropertySet,
  uRenderer;

{ TBorderStyleHandler }

constructor TBorderStyleHandler.Create(const Framework: IUIFramework);
begin
  inherited Create;
  FFramework := Framework;
end;

function TBorderStyleHandler.CreateUIImageFromBitmapResource(
  const Resource: LPCTSTR): IUIImage;
var
  Bitmap: HBITMAP;
begin
  Result := nil;
  if (FImageFromBitmapFactory = nil) then
    FImageFromBitmapFactory := CoUIRibbonImageFromBitmapFactory.Create;

  // Load the bitmap from the resource file.
  Bitmap := LoadImage(HInstance, Resource, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION);
  if (Bitmap = 0) then
    raise Exception.Create('Cannot load bitmap');

  // Use the factory implemented by the framework to produce an IUIImage.
  try
    Result := FImageFromBitmapFactory.CreateImage(Bitmap, UIOwnershipTransfer);
  except
    DeleteObject(Bitmap);
  end;
end;

function TBorderStyleHandler.Execute(CommandId: UInt32; Verb: _UIExecutionVerb;
  Key: PUIPropertyKey; CurrentValue: PPropVariant;
  CommandExecutionProperties: IUISimplePropertySet): HRESULT;
var
  Param: TRenderParam;
  Selected: Cardinal;
begin
  Result := E_FAIL;
  Param := Renderer.GetRenderParam;
  if (Verb = UIExecutionVerbExecute) then
  begin
    if (CurrentValue = nil) then
    begin
      // The Button part of the Border Style SplitButtonGallery was clicked.
      if (Param.BorderStyle = bsNone) then
        Param.BorderStyle := bsSolid
      else
        Param.BorderStyle := bsNone;
      Renderer.UpdateRenderParam(Param);

      // Update the visual state of the button- toggled on for border selected,
      // off for no border.
      FFramework.InvalidateUICommand(IDR_CMD_BORDERSTYLES,
        [UIInvalidationsProperty], @UI_PKEY_BooleanValue);

      // The Border Sizes combobox needs to be re-enabled if the dash border
      // was just de-selected.
      FFramework.InvalidateUICommand(IDR_CMD_BORDERSIZES,
        [UIInvalidationsProperty], @UI_PKEY_Enabled);

      Result := S_OK;
    end
    else if Assigned(Key) and (Key^ = UI_PKEY_SelectedItem) then
    begin
      UIPropertyToUInt32(Key^, CurrentValue^, Selected);
      Param.BorderStyle := TBorderStyle(Selected);
      Renderer.UpdateRenderParam(Param);

      // The Border Sizes combobox needs to be disabled if dash border was just
      // selected, or enabled otherwise.
      FFramework.InvalidateUICommand(IDR_CMD_BORDERSIZES,
        [UIInvalidationsProperty], @UI_PKEY_Enabled);
      FFramework.InvalidateUICommand(IDR_CMD_BORDERSTYLES,
        [UIInvalidationsProperty], @UI_PKEY_BooleanValue);

      Result := S_OK;
    end;
  end;
end;

function TBorderStyleHandler.UpdateProperty(CommandId: UInt32;
  const Key: TUIPropertyKey; CurrentValue: PPropVariant;
  out NewValue: TPropVariant): HRESULT;
const
  LabelIds: array [0..2] of Integer = (IDS_BORDER_NONE, IDS_BORDER_SOLID,
    IDS_BORDER_DASH);
var
  Unk: IUnknown;
  Collection: IUICollection;
  ImageIds: array [0..2] of Integer;
  Dpi, I: Integer;
  Item: TPropertySet;
  ItemIntf: IUISimplePropertySet;
  Img: IUIImage;
  Lbl: array [0..MAX_RESOURCE_LENGTH - 1] of Char;
  Param: TRenderParam;
begin
  Result := E_FAIL;
  try
    if (Key = UI_PKEY_Categories) then
      { A return value of S_FALSE or E_NOTIMPL will result in a gallery with no
        categories. If you return any error other than E_NOTIMPL, the contents of
        the gallery will not display. }
      Result := S_FALSE
    else if (Key = UI_PKEY_ItemsSource) then
    begin
      Unk := IUnknown(CurrentValue.pStorage {punkVal});
      if (not Supports(Unk, IUICollection, Collection)) then
        Exit;


      Dpi := Screen.PixelsPerInch;
      if (Dpi > 144) then
      begin
        ImageIds[0] := IDB_NONE_192;
        ImageIds[1] := IDB_SOLID_192;
        ImageIds[2] := IDB_DASH_192;
      end
      else if (Dpi > 120) then
      begin
        ImageIds[0] := IDB_NONE_144;
        ImageIds[1] := IDB_SOLID_144;
        ImageIds[2] := IDB_DASH_144;
      end
      else if (Dpi > 96) then
      begin
        ImageIds[0] := IDB_NONE_120;
        ImageIds[1] := IDB_SOLID_120;
        ImageIds[2] := IDB_DASH_120;
      end
      else
      begin
        ImageIds[0] := IDB_NONE_96;
        ImageIds[1] := IDB_SOLID_96;
        ImageIds[2] := IDB_DASH_96;
      end;

      // Populate the dropdown with the three border styles.
      for I := 0 to Length(LabelIds) - 1 do
      begin
        // Create a new property set for each item.
        Item := TPropertySet.Create;
        ItemIntf := Item;

        // Create an IUIImage from a resource id.
        Img := CreateUIImageFromBitmapResource(MAKEINTRESOURCE(ImageIds[I]));

        // Load the label from the resource file.
        LoadString(HInstance, LabelIds[I], Lbl, MAX_RESOURCE_LENGTH);

        // Initialize the property set with the image and label that were just
        // loaded and no category.
        Item.InitializeItemProperties(Img, Lbl, UICollectionInvalidIndex);

        // Add the newly-created property set to the collection supplied by the
        // framework.
        Collection.Add(ItemIntf);
      end;
      Result := S_OK;
    end
    else if (Key = UI_PKEY_SelectedItem) then
    begin
      // Use the current border style as the selection.
      Param := Renderer.GetRenderParam;
      Result := UIInitPropertyFromUInt32(Key, Ord(Param.BorderStyle), NewValue);
    end
    else if (Key = UI_PKEY_BooleanValue) then
    begin
      // The button will appear selected if there is a border, or deselected if
      // there is no border.
      Param := Renderer.GetRenderParam;
      FFramework.InvalidateUICommand(IDR_CMD_BORDERSTYLES, [UIInvalidationsProperty],
        @UI_PKEY_SelectedItem);
      Result := UIInitPropertyFromBoolean(Key, (Param.BorderStyle <> bsNone), NewValue);
    end;
  except
    on E: EOleSysError do
      Result := E.ErrorCode
    else
      Result := E_FAIL;
  end;
end;

end.
