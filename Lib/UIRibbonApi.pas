unit UIRibbonApi;

{ Translation of the UIRibbon.idl file and related header files.
  Made the following changes to make it more Delphi-like:

  -Types start with a "T" and use CamelCaps instead of underscores and uppercase
   characters. The original C(++) typenames are NOT preserved to avoid confusion.
   For example, the original type UI_SWATCHCOLORTYPE is renamed to
   TUISwatchColorType.
  -Elements of enumerations also use CamelCaps instead of underscores and
   uppercase characters. For example, the UI_SWATCHCOLORTYPE_NOCOLOR member
   of the enumeration above is renamed to UISwatchColorTypeNoColor.
  -Some methods use the "safecall" calling convention instead of returning an
   HResult. This way, error conditions are handled using exceptions instead of
   having to check the function result every time. Likewise, the final "out"
   parameter (if any) of each method has been turned into a function result. }

interface

uses
  Windows,
  ActiveX;

{$MINENUMSIZE 4}
{$ALIGN 8}

{$REGION 'PropIdl.h'}
type
  TPropVariantEx = record
    case Byte of
      0: (PropVar: TPropVariant);
      1: (DecVal: TDecimal);
  end;
{$ENDREGION 'PropIdl.h'}

{$REGION 'UIRibbonKeydef.h'}
type
  TUIPropertyKey = packed record
  public
    FmtId: TGuid;
    PId: DWord;
  public
    class operator Equal(const A, B: TUIPropertyKey): Boolean;
    class operator Equal(const A: TUIPropertyKey; const B: TPropertyKey): Boolean;
    class operator NotEqual(const A, B: TUIPropertyKey): Boolean;
    class operator NotEqual(const A: TUIPropertyKey; const B: TPropertyKey): Boolean;
  end;
  PUIPropertyKey = ^TUIPropertyKey;

(*#define DEFINE_UIPROPERTYKEY(name, type, index) EXTERN_C \\
    const PROPERTYKEY DECLSPEC_SELECTANY name = { { 0x00000000 + index, 0x7363, \\
    0x696e, { 0x84, 0x41, 0x79, 0x8a, 0xcf, 0x5a, 0xeb, 0xb7 } }, type } *)
{$ENDREGION 'UIRibbonKeydef.h'}

{$REGION 'UIRibbon.h/idl'}
const
  { Core command properties }

  // DEFINE_UIPROPERTYKEY(UI_PKEY_Enabled,                      VT_BOOL,                1);
  UI_PKEY_Enabled: TUIPropertyKey =
    (FmtId: '{00000001-7363-696E-8441-798ACF5AEBB7}'; PId: VT_BOOL);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_LabelDescription,             VT_LPWSTR,              2);
  UI_PKEY_LabelDescription: TUIPropertyKey =
    (FmtId: '{00000002-7363-696E-8441-798ACF5AEBB7}'; PId: VT_LPWSTR);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_Keytip,                       VT_LPWSTR,              3);
  UI_PKEY_Keytip: TUIPropertyKey =
    (FmtId: '{00000003-7363-696E-8441-798ACF5AEBB7}'; PId: VT_LPWSTR);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_Label,                        VT_LPWSTR,              4);
  UI_PKEY_Label: TUIPropertyKey =
    (FmtId: '{00000004-7363-696E-8441-798ACF5AEBB7}'; PId: VT_LPWSTR);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_TooltipDescription,           VT_LPWSTR,              5);
  UI_PKEY_TooltipDescription: TUIPropertyKey =
    (FmtId: '{00000005-7363-696E-8441-798ACF5AEBB7}'; PId: VT_LPWSTR);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_TooltipTitle,                 VT_LPWSTR,              6);
  UI_PKEY_TooltipTitle: TUIPropertyKey =
    (FmtId: '{00000006-7363-696E-8441-798ACF5AEBB7}'; PId: VT_LPWSTR);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_LargeImage,                   VT_UNKNOWN,             7); // IUIImage
  UI_PKEY_LargeImage: TUIPropertyKey =
    (FmtId: '{00000007-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UNKNOWN);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_LargeHighContrastImage,       VT_UNKNOWN,             8); // IUIImage
  UI_PKEY_LargeHighContrastImage: TUIPropertyKey =
    (FmtId: '{00000008-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UNKNOWN);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_SmallImage,                   VT_UNKNOWN,             9); // IUIImage
  UI_PKEY_SmallImage: TUIPropertyKey =
    (FmtId: '{00000009-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UNKNOWN);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_SmallHighContrastImage,       VT_UNKNOWN,             10); // IUIImage
  UI_PKEY_SmallHighContrastImage: TUIPropertyKey =
    (FmtId: '{0000000A-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UNKNOWN);

  { Collection properties }

  // DEFINE_UIPROPERTYKEY(UI_PKEY_CommandId,                    VT_UI4,                 100);
  UI_PKEY_CommandId: TUIPropertyKey =
    (FmtId: '{00000064-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_ItemsSource,                  VT_UNKNOWN,             101); // IEnumUnknown or IUICollection
  UI_PKEY_ItemsSource: TUIPropertyKey =
    (FmtId: '{00000065-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UNKNOWN);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_Categories,                   VT_UNKNOWN,             102); // IEnumUnknown or IUICollection
  UI_PKEY_Categories: TUIPropertyKey =
    (FmtId: '{00000066-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UNKNOWN);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_CategoryId,                   VT_UI4,                 103);
  UI_PKEY_CategoryId: TUIPropertyKey =
    (FmtId: '{00000067-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_SelectedItem,                 VT_UI4,                 104);
  UI_PKEY_SelectedItem: TUIPropertyKey =
    (FmtId: '{00000068-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_CommandType,                  VT_UI4,                 105);
  UI_PKEY_CommandType: TUIPropertyKey =
    (FmtId: '{00000069-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_ItemImage,                    VT_UNKNOWN,             106); // IUIImage
  UI_PKEY_ItemImage: TUIPropertyKey =
    (FmtId: '{0000006A-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UNKNOWN);

  { Control properties }

  // DEFINE_UIPROPERTYKEY(UI_PKEY_BooleanValue,                 VT_BOOL,                200);
  UI_PKEY_BooleanValue: TUIPropertyKey =
    (FmtId: '{000000C8-7363-696E-8441-798ACF5AEBB7}'; PId: VT_BOOL);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_DecimalValue,                 VT_DECIMAL,             201);
  UI_PKEY_DecimalValue: TUIPropertyKey =
    (FmtId: '{000000C9-7363-696E-8441-798ACF5AEBB7}'; PId: VT_DECIMAL);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_StringValue,                  VT_LPWSTR,              202);
  UI_PKEY_StringValue: TUIPropertyKey =
    (FmtId: '{000000CA-7363-696E-8441-798ACF5AEBB7}'; PId: VT_LPWSTR);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_MaxValue,                     VT_DECIMAL,             203);
  UI_PKEY_MaxValue: TUIPropertyKey =
    (FmtId: '{000000CB-7363-696E-8441-798ACF5AEBB7}'; PId: VT_DECIMAL);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_MinValue,                     VT_DECIMAL,             204);
  UI_PKEY_MinValue: TUIPropertyKey =
    (FmtId: '{000000CC-7363-696E-8441-798ACF5AEBB7}'; PId: VT_DECIMAL);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_Increment,                    VT_DECIMAL,             205);
  UI_PKEY_Increment: TUIPropertyKey =
    (FmtId: '{000000CD-7363-696E-8441-798ACF5AEBB7}'; PId: VT_DECIMAL);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_DecimalPlaces,                VT_UI4,                 206);
  UI_PKEY_DecimalPlaces: TUIPropertyKey =
    (FmtId: '{000000CE-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_FormatString,                 VT_LPWSTR,              207);
  UI_PKEY_FormatString: TUIPropertyKey =
    (FmtId: '{000000CF-7363-696E-8441-798ACF5AEBB7}'; PId: VT_LPWSTR);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_RepresentativeString,         VT_LPWSTR,              208);
  UI_PKEY_RepresentativeString: TUIPropertyKey =
    (FmtId: '{000000D0-7363-696E-8441-798ACF5AEBB7}'; PId: VT_LPWSTR);

  { Font control properties }

  // DEFINE_UIPROPERTYKEY(UI_PKEY_FontProperties,                     VT_UNKNOWN,             300); // IPropertyStore
  UI_PKEY_FontProperties: TUIPropertyKey =
    (FmtId: '{0000012C-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UNKNOWN);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_FontProperties_Family,              VT_LPWSTR,              301);
  UI_PKEY_FontProperties_Family: TUIPropertyKey =
    (FmtId: '{0000012D-7363-696E-8441-798ACF5AEBB7}'; PId: VT_LPWSTR);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_FontProperties_Size,                VT_DECIMAL,             302);
  UI_PKEY_FontProperties_Size: TUIPropertyKey =
    (FmtId: '{0000012E-7363-696E-8441-798ACF5AEBB7}'; PId: VT_DECIMAL);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_FontProperties_Bold,                VT_UI4,                 303); // UI_FONTPROPERTIES
  UI_PKEY_FontProperties_Bold: TUIPropertyKey =
    (FmtId: '{0000012F-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_FontProperties_Italic,              VT_UI4,                 304); // UI_FONTPROPERTIES
  UI_PKEY_FontProperties_Italic: TUIPropertyKey =
    (FmtId: '{00000130-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_FontProperties_Underline,           VT_UI4,                 305); // UI_FONTPROPERTIES
  UI_PKEY_FontProperties_Underline: TUIPropertyKey =
    (FmtId: '{00000131-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_FontProperties_Strikethrough,       VT_UI4,                 306); // UI_FONTPROPERTIES
  UI_PKEY_FontProperties_Strikethrough: TUIPropertyKey =
    (FmtId: '{00000132-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_FontProperties_VerticalPositioning, VT_UI4,                 307); // UI_FONTVERTICALPOSITION
  UI_PKEY_FontProperties_VerticalPositioning: TUIPropertyKey =
    (FmtId: '{00000133-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_FontProperties_ForegroundColor,     VT_UI4,                 308); // COLORREF
  UI_PKEY_FontProperties_ForegroundColor: TUIPropertyKey =
    (FmtId: '{00000134-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_FontProperties_BackgroundColor,     VT_UI4,                 309); // COLORREF
  UI_PKEY_FontProperties_BackgroundColor: TUIPropertyKey =
    (FmtId: '{00000135-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_FontProperties_ForegroundColorType, VT_UI4,                 310); // UI_SWATCHCOLORTYPE
  UI_PKEY_FontProperties_ForegroundColorType: TUIPropertyKey =
    (FmtId: '{00000136-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_FontProperties_BackgroundColorType, VT_UI4,                 311); // UI_SWATCHCOLORTYPE
  UI_PKEY_FontProperties_BackgroundColorType: TUIPropertyKey =
    (FmtId: '{00000137-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_FontProperties_ChangedProperties,   VT_UNKNOWN,             312); // IPropertyStore
  UI_PKEY_FontProperties_ChangedProperties: TUIPropertyKey =
    (FmtId: '{00000138-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UNKNOWN);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_FontProperties_DeltaSize,           VT_UI4,                 313); // UI_FONTDELTASIZE
  UI_PKEY_FontProperties_DeltaSize: TUIPropertyKey =
    (FmtId: '{00000139-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  { Application menu properties }

  // DEFINE_UIPROPERTYKEY(UI_PKEY_RecentItems,                  VT_ARRAY|VT_UNKNOWN,    350);
  UI_PKEY_RecentItems: TUIPropertyKey =
    (FmtId: '{0000015E-7363-696E-8441-798ACF5AEBB7}'; PId: VT_ARRAY or VT_UNKNOWN);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_Pinned,                       VT_BOOL,                351);
  UI_PKEY_Pinned: TUIPropertyKey =
    (FmtId: '{0000015F-7363-696E-8441-798ACF5AEBB7}'; PId: VT_BOOL);

  { Color picker properties }

  // DEFINE_UIPROPERTYKEY(UI_PKEY_Color,                        VT_UI4,                 400); // COLORREF
  UI_PKEY_Color: TUIPropertyKey =
    (FmtId: '{00000190-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_ColorType,                    VT_UI4,                 401); // UI_SWATCHCOLORTYPE
  UI_PKEY_ColorType: TUIPropertyKey =
    (FmtId: '{00000191-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_ColorMode,                    VT_UI4,                 402); // UI_SWATCHCOLORTYPE
  UI_PKEY_ColorMode: TUIPropertyKey =
    (FmtId: '{00000192-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_ThemeColorsCategoryLabel,     VT_LPWSTR,              403);
  UI_PKEY_ThemeColorsCategoryLabel: TUIPropertyKey =
    (FmtId: '{00000193-7363-696E-8441-798ACF5AEBB7}'; PId: VT_LPWSTR);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_StandardColorsCategoryLabel,  VT_LPWSTR,              404);
  UI_PKEY_StandardColorsCategoryLabel: TUIPropertyKey =
    (FmtId: '{00000194-7363-696E-8441-798ACF5AEBB7}'; PId: VT_LPWSTR);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_RecentColorsCategoryLabel,    VT_LPWSTR,              405);
  UI_PKEY_RecentColorsCategoryLabel: TUIPropertyKey =
    (FmtId: '{00000195-7363-696E-8441-798ACF5AEBB7}'; PId: VT_LPWSTR);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_AutomaticColorLabel,          VT_LPWSTR,              406);
  UI_PKEY_AutomaticColorLabel: TUIPropertyKey =
    (FmtId: '{00000196-7363-696E-8441-798ACF5AEBB7}'; PId: VT_LPWSTR);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_NoColorLabel,                 VT_LPWSTR,              407);
  UI_PKEY_NoColorLabel: TUIPropertyKey =
    (FmtId: '{00000197-7363-696E-8441-798ACF5AEBB7}'; PId: VT_LPWSTR);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_MoreColorsLabel,              VT_LPWSTR,              408);
  UI_PKEY_MoreColorsLabel: TUIPropertyKey =
    (FmtId: '{00000198-7363-696E-8441-798ACF5AEBB7}'; PId: VT_LPWSTR);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_ThemeColors,                  VT_VECTOR|VT_UI4,       409);
  UI_PKEY_ThemeColors: TUIPropertyKey =
    (FmtId: '{00000199-7363-696E-8441-798ACF5AEBB7}'; PId: VT_VECTOR or VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_StandardColors,               VT_VECTOR|VT_UI4,       410);
  UI_PKEY_StandardColors: TUIPropertyKey =
    (FmtId: '{0000019A-7363-696E-8441-798ACF5AEBB7}'; PId: VT_VECTOR or VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_ThemeColorsTooltips,          VT_VECTOR|VT_LPWSTR,    411);
  UI_PKEY_ThemeColorsTooltips: TUIPropertyKey =
    (FmtId: '{0000019B-7363-696E-8441-798ACF5AEBB7}'; PId: VT_VECTOR or VT_LPWSTR);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_StandardColorsTooltips,       VT_VECTOR|VT_LPWSTR,    412);
  UI_PKEY_StandardColorsTooltips: TUIPropertyKey =
    (FmtId: '{0000019C-7363-696E-8441-798ACF5AEBB7}'; PId: VT_VECTOR or VT_LPWSTR);

  { Ribbon properties }

  // DEFINE_UIPROPERTYKEY(UI_PKEY_Viewable,                     VT_BOOL,                1000);
  UI_PKEY_Viewable: TUIPropertyKey =
    (FmtId: '{000003E8-7363-696E-8441-798ACF5AEBB7}'; PId: VT_BOOL);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_Minimized,                    VT_BOOL,                1001);
  UI_PKEY_Minimized: TUIPropertyKey =
    (FmtId: '{000003E9-7363-696E-8441-798ACF5AEBB7}'; PId: VT_BOOL);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_QuickAccessToolbarDock,       VT_UI4,                 1002);
  UI_PKEY_QuickAccessToolbarDock: TUIPropertyKey =
    (FmtId: '{000003EA-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  { Contextual tabset properties }

  // DEFINE_UIPROPERTYKEY(UI_PKEY_ContextAvailable,             VT_UI4,                 1100);
  UI_PKEY_ContextAvailable: TUIPropertyKey =
    (FmtId: '{0000044C-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  { Color properties
    These are specified using hue, saturation and brightness. The background,
    highlight and text colors of all controls will be adjusted to the specified
    hues and saturations. The brightness does not represent a component of a
    specific color, but rather the overall brightness of the controls -
    $00 is darkest, $FF is lightest. }

  // DEFINE_UIPROPERTYKEY(UI_PKEY_GlobalBackgroundColor,        VT_UI4,                 2000); // UI_HSBCOLOR
  UI_PKEY_GlobalBackgroundColor: TUIPropertyKey =
    (FmtId: '{000007D0-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_GlobalHighlightColor,         VT_UI4,                 2001); // UI_HSBCOLOR
  UI_PKEY_GlobalHighlightColor: TUIPropertyKey =
    (FmtId: '{000007D1-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

  // DEFINE_UIPROPERTYKEY(UI_PKEY_GlobalTextColor,              VT_UI4,                 2002); // UI_HSBCOLOR
  UI_PKEY_GlobalTextColor: TUIPropertyKey =
    (FmtId: '{000007D2-7363-696E-8441-798ACF5AEBB7}'; PId: VT_UI4);

// Hue, Saturation, Brightness color specification

type
  TUIHsbColor = DWORD;

function UIHsb(const Hue, Saturation, Brightness: Byte): TUIHsbColor; inline;

// Windows Ribbon interfaces implemented by the framework

type
  _UIContextAvailability = (
    UIContextAvailabilityNotAvailable,
    UIContextAvailabilityAvailable,
    UIContextAvailabilityActive);

type
  _UIFontProperties = (
    UIFontPropertiesNotAvailable,
    UIFontPropertiesNotSet,
    UIFontPropertiesSet);

type
  _UIFontVerticalPosition = (
    UIFontVerticalPositionNotAvailable,
    UIFontVerticalPositionNotSet,
    UIFontVerticalPositionSuperscript,
    UIFontVerticalPositionSubscript);

type
  _UIFontUnderline = (
    UIFontUnderlineNotAvailable,
    UIFontUnderlineNotSet,
    UIFontUnderlineSet);

type
  _UIFontDeltaSize = (
    UIFontDeltaSizeGrow,
    UIFontDeltaSizeShrink);

type
  _UIControlDock = (
    UIControlDockTop = 1,
    UIControlDockBottom = 3);

// Types for the color picker

type // Determines whether a swatch has a color, is nocolor or automatic.
  _UISwatchColorType = (
    UISwatchColorTypeNoColor,    // Inactive swatch
    UISwatchColorTypeAutomatic,  // Automatic swatch
    UISwatchColorTypeRgb);       // Solid color swatch

// If the mode is set to MONOCHROME, the swatch's RGB color value will be
// interpreted as a 1 bit-per-pixel pattern.
type
  _UISwatchColorMode = (
    UISwatchColorModeNormal,
    UISwatchColorModeMonochrome);

type
  // Forward declarations
  IUIApplication = interface;

  // Simple property bag
  IUISimplePropertySet = interface(IUnknown)
  ['{c205bb48-5b1c-4219-a106-15bd0a5f24e2}']
    // Retrieves the stored value of a given property
    function GetValue(const Key: TUIPropertyKey; out Value: TPropVariant): HResult; stdcall;
  end;

  // Ribbon view interface
  IUIRibbon = interface(IUnknown)
  ['{803982ab-370a-4f7e-a9e7-8784036a6e26}']
    // Returns the Ribbon height
    function GetHeight: UInt32; safecall;

    // Load Ribbon parameters (e.g. QuickAccessToolbar) from a stream
    function LoadSettingsFromStream(const Stream: IStream): HResult; stdcall;

    // Save Ribbon parameters (e.g. QuickAccessToolbar) to a stream
    function SaveSettingsToStream(const Stream: IStream): HResult; stdcall;
  end;

  _UIInvalidation = (
    UIInvalidationsState,           // UI_PKEY_Enabled
    UIInvalidationsValue,           // Value property
    UIInvalidationsProperty,        // Any property
    UIInvalidationsAllProperties);  // All properties

  _UIInvalidations = set of _UIInvalidation;

  // Windows Ribbon Application interface
  IUIFramework = interface(IUnknown)
  ['{F4F0385D-6872-43a8-AD09-4C339CB3F5C5}']
    // Connects the framework and the application
    procedure Initialize(FrameWnd: HWND; const Application: IUIApplication); safecall;

    // Releases all framework objects
    function Destroy: HRESULT; stdcall;

    // Loads and instantiates the views and commands specified in markup
    procedure LoadUI(Instance: HINST; ResourceName: LPCWSTR); safecall;

    // Retrieves a pointer to a view object
    function GetView(ViewId: UInt32; const Riid: TGuid): IUnknown; safecall;

    // Retrieves the current value of a property
    function GetUICommandProperty(CommandId: UInt32;
      const Key: TUIPropertyKey; out Value: TPropVariant): HRESULT; stdcall;

    // Immediately sets the value of a property
    function SetUICommandProperty(CommandId: UInt32; const Key: TUIPropertyKey;
      const Value: TPropVariant): HRESULT; stdcall;

    // Asks the framework to retrieve the new value of a property at the next
    // update cycle
    procedure InvalidateUICommand(CommandId: UInt32; Flags: _UIInvalidations;
      Key: PUIPropertyKey); safecall;

    // Flush all the pending UI command updates
    function FlushPendingInvalidations: HResult; stdcall;

    // Asks the framework to switch to the list of modes specified and adjust
    // visibility of controls accordingly
    procedure SetModes(Modes: UInt32); safecall;
  end;

  // Windows Ribbon ContextualUI interface
  IUIContextualUI = interface(IUnknown)
  ['{EEA11F37-7C46-437c-8E55-B52122B29293}']
    // Sets the desired anchor point where ContextualUI should be displayed.
    // Typically this is the mouse location at the time of right click.
    // x and y are in virtual screen coordinates.
    procedure ShowAtLocation(X, Y: Int32); safecall;
  end;

  // Windows Ribbon Collection interface
  IUICollection = interface(IUnknown)
  ['{DF4F45BF-6F9D-4dd7-9D68-D8F9CD18C4DB}']
    // Retrieves the count of the collection
    function GetCount: UInt32; safecall;

    // Retrieves an item
    function GetItem(Index: UInt32): IUnknown; safecall;

    // Adds an item to the end
    procedure Add(const Item: IUnknown); safecall;

    // Inserts an item
    procedure Insert(Index: UInt32; const Item: IUnknown); safecall;

    // Removes an item at the specified position
    procedure RemoveAt(Index: UInt32); safecall;

    // Replaces an item at the specified position
    procedure Replace(IndexReplaces: UInt32; const ItemReplaceWith: IUnknown); safecall;

    // Clear the collection
    procedure Clear; safecall;
  end;

  _UICollectionChange = (
    UICollectionChangeInsert,
    UICollectionChangeRemove,
    UICollectionChangeReplace,
    UICollectionChangeReset);

  // Connection Sink for listening to collection changes
  IUICollectionChangedEvent = interface(IUnknown)
  ['{6502AE91-A14D-44b5-BBD0-62AACC581D52}']
    procedure OnChanged(Action: _UICollectionChange; OldIndex: UInt32;
      const OldItem: IUnknown; NewIndex: UInt32;
      const NewItem: IUnknown); safecall;
  end;

  // Windows Ribbon interfaces implemented by the application

  _UIExecutionVerb = (
    UIExecutionVerbExecute,
    UIExecutionVerbPreview,
    UIExecutionVerbCancelPreview);

  // Command handler interface
  IUICommandHandler = interface(IUnknown)
  ['{75ae0a2d-dc03-4c9f-8883-069660d0beb6}']
    // User action callback, with transient execution parameters
    function Execute(CommandId: UInt32; Verb: _UIExecutionVerb;
      Key: PUIPropertyKey; Value: PPropVariant;
      CommandExecutionProperties: IUISimplePropertySet): HRESULT; stdcall;

    // Informs of the current value of a property, and queries for the new one
    function UpdateProperty(CommandId: UInt32; const Key: TUIPropertyKey;
      CurrentValue: PPropVariant; var NewValue: TPropVariant): HRESULT; stdcall;
  end;

  // Types of UI commands
  _UICommandType = (
    UICommandTypeUnknown,
    UICommandTypeGroup,
    UICommandTypeAction,
    UICommandTypeAnchor,
    UICommandTypeContext,
    UICommandTypeCollection,
    UICommandTypeCommandCollection,
    UICommandTypeDecimal,
    UICommandTypeBoolean,
    UICommandTypeFont,
    UICommandTypeRecentItems,
    UICommandTypeColorAnchor,
    UICommandTypeColorCollection);

  // Types of UI Views
  _UIViewType = (
    UIViewTypeRibbon = 1);

  _UIViewVerb = (
    UIViewVerbCreate,
    UIViewVerbDestroy,
    UIViewVerbSize,
    UIViewVerbError);

  // Application callback interface
  IUIApplication = interface(IUnknown)
  ['{D428903C-729A-491d-910D-682A08FF2522}']
    // A view has changed
    function OnViewChanged(ViewId: UInt32; TypeId: _UIViewType;
      const View: IUnknown; Verb: _UIViewVerb; ReasonCode: Int32): HRESULT; stdcall;

    // Command creation callback
    function OnCreateUICommand(CommandId: UInt32; TypeId: _UICommandType;
      out CommandHandler: IUICommandHandler): HRESULT; stdcall;

    // Command destroy callback
    function OnDestroyUICommand(CommandId: UInt32; TypeId: _UICommandType;
      const CommandHandler: IUICommandHandler): HRESULT; stdcall;
  end;

  // Container for bitmap image
  IUIImage = interface(IUnknown)
  ['{23c8c838-4de6-436b-ab01-5554bb7c30dd}']
    function GetBitmap: HBITMAP; safecall;
  end;

  _UIOwnership = (
    UIOwnershipTransfer,  // IUIImage now owns HBITMAP.
    UIOwnershipCopy);     // IUIImage creates a copy of HBITMAP. Caller still owns HBITMAP.

  // Produces containers for bitmap images
  IUIImageFromBitmap = interface(IUnknown)
  ['{18aba7f3-4c1c-4ba2-bf6c-f5c3326fa816}']
    function CreateImage(Bitmap: HBITMAP; Options: _UIOwnership): IUIImage; safecall;
  end;

const
  UIAllCommands = 0;
  UICollectionInvalidIndex = -1;//$FFFFFFFF;

function UIMakeAppMode(const X: Integer): UInt32; inline;

const
  CLSID_UIRibbonFramework             : TGuid = '{926749fa-2615-4987-8845-c33e65f2b957}';
  CLSID_UIRibbonImageFromBitmapFactory: TGuid = '{0f7434b6-59b6-4250-999e-d168d6ae4293}';
  LIBID_UIRibbon                      : TGuid = '{942f35c2-e83b-45ef-b085-ac295dd63d5b}';

type
  CoUIRibbonFramework = class
    class function Create: IUIFramework; static;
  end;

type
  CoUIRibbonImageFromBitmapFactory = class
    class function Create: IUIImageFromBitmap; static;
  end;
{$ENDREGION 'UIRibbon.h/idl'}

{$REGION 'UIRibbonPropertyHelpers.h'}

// PROPVARIANT initializer helpers

function UIInitPropertyFromBoolean(const Key: TUIPropertyKey; const Value: Boolean;
  out PropVar: TPropVariant): HRESULT;

function UIInitPropertyFromUInt32(const Key: TUIPropertyKey; const Value: UInt32;
  out PropVar: TPropVariant): HRESULT;

function UIInitPropertyFromString(const Key: TUIPropertyKey; const Value: String;
  out PropVar: TPropVariant): HRESULT;

function UIInitPropertyFromDecimal(const Key: TUIPropertyKey; const Value: TDecimal;
  out PropVar: TPropVariant): HRESULT;

function UIInitPropertyFromInterface(const Key: TUIPropertyKey; const Value: IUnknown;
  out PropVar: TPropVariant): HRESULT;

function UIInitPropertyFromImage(const Key: TUIPropertyKey; const Value: IUIImage;
  out PropVar: TPropVariant): HRESULT;

function UIInitPropertyFromIUnknownArray(const Key: TUIPropertyKey; const Value: PSafeArray;
  out PropVar: TPropVariant): HRESULT;

// Extract value from PROPVARIANT

function UIPropertyToBoolean(const Key: TUIPropertyKey; const PropVar: TPropVariant;
  out Value: Boolean): HRESULT;

function UIPropertyToUInt32(const Key: TUIPropertyKey; const PropVar: TPropVariant;
  out Value: UInt32): HRESULT;

function UIPropertyToString(const Key: TUIPropertyKey; const PropVar: TPropVariant;
  out Value: String): HRESULT;
function UIPropertyToStringAlloc(const Key: TUIPropertyKey; const PropVar: TPropVariant;
  out Value: String): HRESULT;

function UIPropertyToDecimal(const Key: TUIPropertyKey; const PropVar: TPropVariant;
  out Value: TDecimal): HRESULT;

function UIPropertyToInterface(const Key: TUIPropertyKey; const PropVar: TPropVariant;
  out Value: IUnknown): HRESULT;

function UIPropertyToImage(const Key: TUIPropertyKey; const PropVar: TPropVariant;
  out Value: IUIImage): HRESULT;

function UIPropertyToIUnknownArrayAlloc(const Key: TUIPropertyKey; const PropVar: TPropVariant;
  out Value: PSafeArray): HRESULT;
{$ENDREGION 'UIRibbonPropertyHelpers.h'}

implementation

uses
  ComObj,
  ShLwApi,
  SysUtils,
  WinApiEx;

{$REGION 'UIRibbonKeydef.h'}

{ TUIPropertyKey }

class operator TUIPropertyKey.Equal(const A, B: TUIPropertyKey): Boolean;
begin
  Result := CompareMem(@A, @B, SizeOf(TUIPropertyKey));
end;

class operator TUIPropertyKey.Equal(const A: TUIPropertyKey;
  const B: TPropertyKey): Boolean;
begin
  Result := CompareMem(@A, @B, SizeOf(TUIPropertyKey));
end;

class operator TUIPropertyKey.NotEqual(const A, B: TUIPropertyKey): Boolean;
begin
  Result := not CompareMem(@A, @B, SizeOf(TUIPropertyKey));
end;

class operator TUIPropertyKey.NotEqual(const A: TUIPropertyKey;
  const B: TPropertyKey): Boolean;
begin
  Result := not CompareMem(@A, @B, SizeOf(TUIPropertyKey));
end;
{$ENDREGION 'UIRibbonKeydef.h'}

{$REGION 'UIRibbon.h/idl'}
function UIHsb(const Hue, Saturation, Brightness: Byte): TUIHsbColor; inline;
begin
  Result := Hue or (Saturation shl 8) or (Brightness shl 16);
end;

function UIMakeAppMode(const X: Integer): UInt32; inline;
begin
  Result := 1 shl X;
end;

{ CoUIRibbonFramework }

class function CoUIRibbonFramework.Create: IUIFramework;
begin
  Result := CreateComObject(CLSID_UIRibbonFramework) as IUIFramework;
end;

{ CoUIRibbonImageFromBitmapFactory }

class function CoUIRibbonImageFromBitmapFactory.Create: IUIImageFromBitmap;
begin
  Result := CreateComObject(CLSID_UIRibbonImageFromBitmapFactory) as IUIImageFromBitmap;
end;
{$ENDREGION 'UIRibbon.h/idl'}

{$REGION 'UIRibbonPropertyHelpers.h'}
function UIInitPropertyFromBoolean(const Key: TUIPropertyKey; const Value: Boolean;
  out PropVar: TPropVariant): HRESULT;
begin
  if (Key.PId = VT_BOOL) then
    Result := InitPropVariantFromBoolean(Value, PropVar)
  else
    Result := E_INVALIDARG;
end;

function UIInitPropertyFromUInt32(const Key: TUIPropertyKey; const Value: UInt32;
  out PropVar: TPropVariant): HRESULT;
begin
  if (Key.PId = VT_UI4) then
    Result := InitPropVariantFromUInt32(Value, PropVar)
  else
    Result := E_INVALIDARG;
end;

function UIInitPropertyFromString(const Key: TUIPropertyKey; const Value: String;
  out PropVar: TPropVariant): HRESULT;
begin
  if (Key.PId = VT_LPWSTR) then
    Result := InitPropVariantFromString(Value, PropVar)
  else
    Result := E_INVALIDARG;
end;

function UIInitPropertyFromDecimal(const Key: TUIPropertyKey; const Value: TDecimal;
  out PropVar: TPropVariant): HRESULT;
var
  PropVarEx: TPropVariantEx absolute PropVar;
begin
  if (Key.PId = VT_DECIMAL) then
  begin
    //  Must set decVal before vt because the two overlap.
    PropVarEx.DecVal := Value;
    PropVarEx.PropVar.vt := VT_DECIMAL;
    Result := S_OK;
  end
  else
    Result := E_INVALIDARG;
end;

function UIInitPropertyFromInterface(const Key: TUIPropertyKey; const Value: IUnknown;
  out PropVar: TPropVariant): HRESULT;
begin
  if (Key.PId = VT_UNKNOWN) then
  begin
    PropVar.vt := VT_UNKNOWN;
    PropVar.pStorage {punkVal} := Pointer(Value);
    if Assigned(Value) then
      { The PropVar gets its own reference }
      Value._AddRef;
    Result := S_OK;
  end
  else
    Result := E_INVALIDARG;
end;

function UIInitPropertyFromImage(const Key: TUIPropertyKey; const Value: IUIImage;
  out PropVar: TPropVariant): HRESULT;
begin
  Result := UIInitPropertyFromInterface(Key, Value, PropVar);
end;

function UIInitPropertyFromIUnknownArray(const Key: TUIPropertyKey; const Value: PSafeArray;
  out PropVar: TPropVariant): HRESULT;
begin
  if (Key.PId = (VT_UNKNOWN or VT_ARRAY)) and ((Value.fFeatures and FADF_UNKNOWN) <> 0) then
  begin
    Result := SafeArrayCopy(Value, PSafeArray(PropVar.pStorage {parray}));
    if Succeeded(Result) then
      PropVar.vt := VT_ARRAY or VT_UNKNOWN;
  end
  else
    Result := E_INVALIDARG;
end;

function UIPropertyToBoolean(const Key: TUIPropertyKey; const PropVar: TPropVariant;
  out Value: Boolean): HRESULT;
var
  Ret: BOOL;
begin
  if (Key.PId = VT_BOOL) then
  begin
    Result := PropVariantToBoolean(PropVar, Ret);
    Value := Ret;
  end
  else
    Result := E_INVALIDARG;
end;

function UIPropertyToUInt32(const Key: TUIPropertyKey; const PropVar: TPropVariant;
  out Value: UInt32): HRESULT;
begin
  if (Key.PId = VT_UI4) then
    Result := PropVariantToUInt32(PropVar, Value)
  else
    Result := E_INVALIDARG;
end;

function UIPropertyToString(const Key: TUIPropertyKey; const PropVar: TPropVariant;
  out Value: String): HRESULT;
var
  Ret: LPWSTR;
begin
  if (Key.PId = VT_LPWSTR) then
  begin
    Result := PropVariantToStringAlloc(PropVar, Ret);
    Value := Ret;
    CoTaskMemFree(Ret);
  end
  else
    Result := E_INVALIDARG;
end;

function UIPropertyToStringAlloc(const Key: TUIPropertyKey; const PropVar: TPropVariant;
  out Value: String): HRESULT;
begin
  Result := UIPropertyToString(Key, PropVar, Value);
end;

function UIPropertyToDecimal(const Key: TUIPropertyKey; const PropVar: TPropVariant;
  out Value: TDecimal): HRESULT;
var
  PropVarEx: TPropVariantEx absolute PropVar;
begin
  if (Key.PId = VT_DECIMAL) and (PropVar.vt = VT_DECIMAL) then
  begin
    Value := PropVarEx.DecVal;
    Result := S_OK;
  end
  else
    Result := E_INVALIDARG;
end;

function UIPropertyToInterface(const Key: TUIPropertyKey; const PropVar: TPropVariant;
  out Value: IUnknown): HRESULT;
begin
  Value := nil;
  if (Key.PId = VT_UNKNOWN) and (PropVar.vt = VT_UNKNOWN) then
  begin
    if Assigned(PropVar.pStorage {punkval}) then
      Value := IUnknown(PropVar.pStorage);
    Result := S_OK;
  end
  else
    Result := E_INVALIDARG;
end;

function UIPropertyToImage(const Key: TUIPropertyKey; const PropVar: TPropVariant;
  out Value: IUIImage): HRESULT;
var
  Ret: IUnknown;
begin
  Result := UIPropertyToInterface(Key, PropVar, Ret);
  if Succeeded(Result) and Assigned(Ret) then
    Result := Ret.QueryInterface(IUIImage, Value);
end;

function UIPropertyToIUnknownArrayAlloc(const Key: TUIPropertyKey; const PropVar: TPropVariant;
  out Value: PSafeArray): HRESULT;
begin
  if (Key.PId = (VT_UNKNOWN or VT_ARRAY)) and (PropVar.vt = (VT_UNKNOWN or VT_ARRAY)) then
    Result := SafeArrayCopy(PropVar.pStorage {parray}, Value)
  else
    Result := E_INVALIDARG;
end;
{$ENDREGION 'UIRibbonPropertyHelpers.h'}

initialization
  Assert(SizeOf(TUIPropertyKey) = SizeOf(TPropertyKey));

end.
