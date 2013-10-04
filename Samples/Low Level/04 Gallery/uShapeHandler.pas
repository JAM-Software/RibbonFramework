unit uShapeHandler;

interface

uses
  Windows,
  ActiveX,
  ComObj,
  UIRibbonApi;

type
  TShapeHandler = class(TInterfacedObject, IUICommandHandler)
  private
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
  end;

implementation

uses
  SysUtils,
  RibbonConst,
  Forms,
  uRenderer,
  uPropertySet;

{ TShapeHandler }

function TShapeHandler.CreateUIImageFromBitmapResource(
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

function TShapeHandler.Execute(CommandId: UInt32; Verb: _UIExecutionVerb;
  Key: PUIPropertyKey; CurrentValue: PPropVariant;
  CommandExecutionProperties: IUISimplePropertySet): HRESULT;
var
  Param: TRenderParam;
  Selected: Cardinal;
begin
  Param := Renderer.GetRenderParam;
  Result := UIPropertyToUInt32(Key^, CurrentValue^, Selected);
  case Verb of
    UIExecutionVerbPreview:
      begin
        // Show a preview of a new shape.
        Param.ShapeType := TShapeType(Selected);
        Renderer.UpdateRenderParam(Param);
        Result := S_OK;
      end;

    UIExecutionVerbCancelPreview:
      begin
        // Show the shape that was selected before the preview- CurrentValue
        // contains the previous selected item.
        // Note that the renderer did not have to store the value from before
        // preview was called.
        Param.ShapeType := TShapeType(Selected);
        Renderer.UpdateRenderParam(Param);
        Result := S_OK;
      end;

    UIExecutionVerbExecute:
      begin
        if Assigned(Key) and (Key^ = UI_PKEY_SelectedItem) then
        begin
          // Update the renderer with the newly-selected shape.
          Param.ShapeType := TShapeType(Selected);
          Renderer.UpdateRenderParam(Param);
          Result := S_OK;
        end;
      end;
  end;
end;

function TShapeHandler.UpdateProperty(CommandId: UInt32;
  const Key: TUIPropertyKey; CurrentValue: PPropVariant;
  out NewValue: TPropVariant): HRESULT;
{ This function is used to initialize the contents and selection of the
  gallery. }
var
  Unk: IUnknown;
  Collection: IUICollection;
  ImageIds, LabelIds: array [0..3] of Integer;
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

      LabelIds[0] := IDS_RECTANGLE;
      LabelIds[1] := IDS_ELLIPSE;
      LabelIds[2] := IDS_ROUNDED_RECTANGLE;
      LabelIds[3] := IDS_DIAMOND;

      Dpi := Screen.PixelsPerInch;
      if (Dpi > 144) then
      begin
        ImageIds[0] := IDB_RECTANGLE_192;
        ImageIds[1] := IDB_ELLIPSE_192;
        ImageIds[2] := IDB_ROUNDED_RECTANGLE_192;
        ImageIds[3] := IDB_DIAMOND_192;
      end
      else if (Dpi > 120) then
      begin
        ImageIds[0] := IDB_RECTANGLE_144;
        ImageIds[1] := IDB_ELLIPSE_144;
        ImageIds[2] := IDB_ROUNDED_RECTANGLE_144;
        ImageIds[3] := IDB_DIAMOND_144;
      end
      else if (Dpi > 96) then
      begin
        ImageIds[0] := IDB_RECTANGLE_120;
        ImageIds[1] := IDB_ELLIPSE_120;
        ImageIds[2] := IDB_ROUNDED_RECTANGLE_120;
        ImageIds[3] := IDB_DIAMOND_120;
      end
      else
      begin
        ImageIds[0] := IDB_RECTANGLE_96;
        ImageIds[1] := IDB_ELLIPSE_96;
        ImageIds[2] := IDB_ROUNDED_RECTANGLE_96;
        ImageIds[3] := IDB_DIAMOND_96;
      end;

      // Populate the gallery with the four available shape types.
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
      // Use the current shape as the selection.
      Param := Renderer.GetRenderParam;
      Result := UIInitPropertyFromUInt32(Key, Ord(Param.ShapeType), NewValue);
    end;
  except
    on E: EOleSysError do
      Result := E.ErrorCode
    else
      Result := E_FAIL;
  end;
end;

end.
