unit uPropertySet;

interface

uses
  Windows,
  ActiveX,
  ComObj,
  UIRibbonApi;

const
  MAX_RESOURCE_LENGTH = 256;

type
  TPropertySet = class(TInterfacedObject, IUISimplePropertySet)
  private
    FLabel: String;                // Used for items and categories.
    FCategoryId: Integer;          // Used for items, categories, and commands.
    FImgItem: IUIImage;            // Used for items only.
    FCommandId: Integer;           // Used for commands only.
    FCommandType: _UICommandType;  // Used for commands only.
  private
    { IUISimplePropertySet }
    function GetValue(const Key: TUIPropertyKey; out Value: TPropVariant): HResult; stdcall;
  public
    constructor Create;
    procedure InitializeItemProperties(const Image: IUIImage;
      const Lbl: String; const CategoryId: Integer);
    procedure InitializeCategoryProperties(const Lbl: String;
      const CategoryId: Integer);
    procedure InitializeCommandProperties(const CategoryId, CommandId: Integer;
      const CommandType: _UICommandType);
  end;

implementation

{ TPropertySet }

constructor TPropertySet.Create;
begin
  inherited;
  FCategoryId := UICollectionInvalidIndex;
  FCommandId := -1;
  FCommandType := UICommandTypeUnknown;
end;

function TPropertySet.GetValue(const Key: TUIPropertyKey;
  out Value: TPropVariant): HResult;
begin
  if (Key = UI_PKEY_ItemImage) then
  begin
    if Assigned(FImgItem) then
      Result := UIInitPropertyFromImage(Key, FImgItem, Value)
    else
      Result := S_FALSE;
  end
  else if (Key = UI_PKEY_Label) then
    Result := UIInitPropertyFromString(Key, FLabel, Value)
  else if (Key = UI_PKEY_CategoryId) then
    Result := UIInitPropertyFromUInt32(Key, FCategoryId, Value)
  else if (Key = UI_PKEY_CommandId) then
  begin
    if (FCommandId <> -1) then
      Result := UIInitPropertyFromUInt32(Key, FCommandId, Value)
    else
      Result := S_FALSE;
  end else if (Key = UI_PKEY_CommandType) then
    Result := UIInitPropertyFromUInt32(Key, Ord(FCommandType), Value)
  else
    Result := E_FAIL;
end;

procedure TPropertySet.InitializeCategoryProperties(const Lbl: String;
  const CategoryId: Integer);
begin
  FLabel := Lbl;
  FCategoryId := CategoryId;
end;

procedure TPropertySet.InitializeCommandProperties(const CategoryId,
  CommandId: Integer; const CommandType: _UICommandType);
begin
  FCategoryId := CategoryId;
  FCommandId := CommandId;
  FCommandType := CommandType;
end;

procedure TPropertySet.InitializeItemProperties(const Image: IUIImage;
  const Lbl: String; const CategoryId: Integer);
begin
  FImgItem := Image;
  FLabel := Lbl;
  FCategoryId := CategoryId;
end;

end.
