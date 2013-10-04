unit RibbonMarkup;

{ Classes for reading and writing Ribbon Markup XML files.
  We could use Delphi's XML Data Binding to convert the Ribbon schema UICC.xsd
  to a set of interfaces, but that's not desireable for various reasons:
  -That approach is not very fast and memory efficient.
  -You would still need additional support classes or routines to easily work
   with those object.
  -But most importantly, Ribbon Markup doesn't really lend itself for this
   because the same data can be represented in multiple ways. For example,
   the LabelType of a command can be represented as a LabelTitle attribute
   of the <Command> element, or as a sub-element of the <Command.LabelTitle>
   element (much like XAML). This class model unifies this: it can reads both
   ways as a single LabelTitle, and it writes it in a format that is most
   efficient. }

interface

uses
  Contnrs,
  Classes,
  SysUtils,
  Generics.Collections,
  BasicXml;

type
  ERibbonMarkupError = class(Exception);

type
  TRibbonCommandName = record
    Name: String; // '' when Id is used
    Id: Integer;  // -1 when Name is used
  end;

type
  TRibbonGroupSpacing = (gsSmall, gsMedium, gsLarge);
  TRibbonGroupLayout = (glPopup, glSmall, glMedium, glLarge);
  TRibbonMenuCategoryClass = (ccStandardItems, ccMajorItems);
  TRibbonBasicSizeDefinition = (sdCustom, sdAdvanced, sdOneButton, sdTwoButtons,
    sdThreeButtons, sdThreeButtonsOneBigAndTwoSmall, sdThreeButtonsAndOneCheckBox,
    sdFourButtons, sdFiveButtons, sdFiveOrSixButtons, sdSixButtons,
    sdSixButtonsTwoColumns, sdSevenButtons, sdEightButtons,
    sdEightButtonsLastThreeSmall, sdNineButtons, sdTenButtons, sdElevenButtons,
    sdOneFontControl, sdIntFontOnly, sdIntRichFont, sdIntFontWithColor,
    sdOneInRibbonGallery, sdBigButtonsAndSmallButtonsOrInputs,
    sdInRibbonGalleryAndBigButton, sdInRibbonGalleryAndButtonsGalleryScalesFirst,
    sdInRibbonGalleryAndThreeButtons, sdButtonGroupsAndInputs, sdButtonGroups);
  TRibbonFontType = (ftFontOnly, ftFontWithColor, ftRichFont);
  TRibbonColorTemplate = (ctThemeColors, ctStandardColors, ctHighlightColors);
  TRibbonChipSize = (csSmall, csMedium, csLarge);
  TRibbonGalleryType = (gtItems, gtCommands);
  TRibbonTextPosition = (tpBottom, tpHide, tpLeft, tpOverlap, tpRight, tpTop);
  TRibbonSingleColumnGripperType = (sgNone, sgVertical);
  TRibbonMultiColumnGripperType = (mgNone, mgVertical, mgCorner);
  TRibbonGroupSizeType = (stLarge, stMedium, stSmall);
  TRibbonImageSize = (isLarge, isSmall);
  TRibbonComboBoxResizeType = (rtNoResize, rtVerticalResize);
  TRibbonObjectType = (otButton, otToggleButton, otSplitButton, otDropDownButton,
    otDropDownColorPicker, otSpinner, otCheckBox, otComboBox, otQatButton,
    otQatToggleButton, otQatCheckBox, otHelpButton, otFloatieFontControl,
    otFontControl, otControlGroup, otDropDownGallery, otSplitButtonGallery,
    otInRibbonGallery, otApplicationMenu, otQuickAccessToolbar, otGroup, otTab,
    otTabGroup, otList, otDictionary, otString, otImage, otDocument, otCommand,
    otRecentItems, otAppMenuGroup, otMenuGroup, otMiniToolbarMenuGroup,
    otVerticalMenuLayout, otFlowMenuLayout, otScale, otScalingPolicy,
    otControlNameMap, otControlSizeDefinition, otRow, otControlSizeGroup,
    otColumnBreak, otGroupSizeDefinition, otSizeDefinition,
    otRibbonSizeDefinition, otViewRibbon, otMiniToolbar, otContextMenu,
    otContextMap, otViewContextPopup, otApplication, otSplitButton_Items,
    otRibbonSizeDefinitions, otScalingPolicy_IdealSizes, otContextualTabs,
    otMiniToolbars, otContextMenus, otContextMaps);

type
  TRibbonApplication = class;
  TRibbonDocument = class;
  TRibbonCommand = class;
  TRibbonContextMenu = class;
  TRibbonMiniToolbar = class;
  TRibbonSizeDefinition = class;
  TRibbonViewContextPopup = class;

  TRibbonObject = class abstract
  {$REGION 'Internal Declarations'}
  strict private
    FOwner: TRibbonDocument;
    FNotifyList: TList<TRibbonObject>;
    function GetReferenceCount: Integer;
  protected
    procedure Error(const Element: TXmlElement; const Msg: String); overload;
    procedure Error(const Element: TXmlElement; const Msg: String;
      const Args: array of const); overload;
    procedure FreeNotification(const Listener: TRibbonObject);
    procedure RemoveFreeNotification(const Listener: TRibbonObject);
    procedure FreeNotify(const Obj: TRibbonObject); virtual;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const Owner: TRibbonDocument);
    destructor Destroy; override;
    function DisplayName: String; virtual;
    class function ObjectType: TRibbonObjectType; virtual; abstract;

    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; virtual;
    function Delete(const Obj: TRibbonObject): Boolean; virtual;
    function CanReorder: Boolean; virtual;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; virtual;

    property Owner: TRibbonDocument read FOwner;
    property ReferenceCount: Integer read GetReferenceCount;
  end;

  TRibbonList<T> = class(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FItems: TList<T>;
    FOwnsItems: Boolean;
    function GetCount: Integer;
    function GetItem(const Index: Integer): T;
  private
    procedure Add(const Item: T);
    function Remove(const Item: T): Boolean;
    procedure Clear;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const Owner: TRibbonDocument; const OwnsItems: Boolean);
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    function GetEnumerator: TEnumerator<T>;

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: T read GetItem; default;
  end;

  TRibbonDictionary<TKey, TValue> = class(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FItems: TDictionary<TKey, TValue>;
    function GetCount: Integer;
    function GetItem(const Key: TKey): TValue;
    function GetKeys: TDictionary<TKey, TValue>.TKeyCollection;
    function GetValues: TDictionary<TKey, TValue>.TValueCollection;
  private
    procedure Add(const Key: TKey; const Value: TValue);
    procedure Remove(const Key: TKey);
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const Owner: TRibbonDocument);
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;

    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    function ContainsKey(const Key: TKey): Boolean;

    property Count: Integer read GetCount;
    property Items[const Key: TKey]: TValue read GetItem; default;
    function GetEnumerator: TDictionary<TKey,TValue>.TPairEnumerator;
    property Keys: TDictionary<TKey,TValue>.TKeyCollection read GetKeys;
    property Values: TDictionary<TKey,TValue>.TValueCollection read GetValues;
  end;

  TRibbonString = class(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FContent: String;
    FId: Integer;
    FSymbol: String;
    procedure SetId(const Value: Integer);
    procedure SetSymbol(const Value: String);
  private
    procedure Initialize(const E: TXmlElement);
    procedure Save(const Writer: TXmlWriter; const ElementName: String);
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;

    function IsSimpleString: Boolean;
    function HasSimpleString: Boolean;

    property Content: String read FContent write FContent;
    property Id: Integer read FId write SetId;
    property Symbol: String read FSymbol write SetSymbol;
  end;

  TRibbonImage = class(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FSource: String;
    FMinDpi: Integer;
    FId: Integer;
    FSymbol: String;
    procedure SetId(const Value: Integer);
    procedure SetMinDpi(const Value: Integer);
    procedure SetSymbol(const Value: String);
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
    procedure Save(const Writer: TXmlWriter);
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;

    property Source: String read FSource write FSource;
    property MinDpi: Integer read FMinDpi write SetMinDpi;
    property Id: Integer read FId write SetId;
    property Symbol: String read FSymbol write SetSymbol;
  end;

  TRibbonDocument = class(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FFilename: String;
    FDirectory: String;
    FApplication: TRibbonApplication;
  private
    function CreateObject(const ObjType: TRibbonObjectType): TRibbonObject;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;

    procedure Clear;

    { Load Ribbon Markup from a file, stream, XML string or XML document.
      Raises an exception if the markup doesn't conform to the Ribbon schema
      (UICC.xsd). }
    procedure LoadFromFile(const Filename: String);
    procedure LoadFromStream(const Stream: TStream);
    procedure LoadFromXml(const Xml: RawByteString);
    procedure LoadFromXmlDocument(const XmlDoc: TXmlDocument);

    { Saves the Ribbon Markup to a file, stream or XML string }
    procedure SaveToFile(const Filename: String);
    procedure SaveToStream(const Stream: TStream);
    procedure SaveToXml(out Xml: RawByteString);

    { Finds a command with the given name or Id.
      Returns nil when the command does not exist. }
    function FindCommand(const Name: String): TRibbonCommand; overload;
    function FindCommand(const Id: Integer): TRibbonCommand; overload;
    function FindCommand(const Name: TRibbonCommandName): TRibbonCommand; overload;

    { Finds a context menu or mini toolbar with the given name }
    function FindContextMenu(const Name: String): TRibbonContextMenu;
    function FindMiniToolbar(const Name: String): TRibbonMiniToolbar;

    { If Filename is an absolute filename, returns the filename.
      Otherwise, returns the fully qualified filename relative to the
      document directory. }
    function BuildAbsoluteFilename(const Filename: String): String;

    { Returns the relative part of the filename, relative to the
      document directory. }
    function BuildRelativeFilename(const Filename: String): String;

    { Root <Application> element. Never nil. }
    property Application: TRibbonApplication read FApplication;

    { Fully qualified filename }
    property Filename: String read FFilename;

    { Directory containing the ribbon file, including trailing backslash }
    property Directory: String read FDirectory;
  end;

  TRibbonCommand = class(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FName: String;
    FSymbol: String;
    FId: Integer;
    FComment: String;
    FLabelTitle: TRibbonString;
    FLabelDescription: TRibbonString;
    FTooltipTitle: TRibbonString;
    FTooltipDescription: TRibbonString;
    FKeytip: TRibbonString;
    FSmallImages: TRibbonList<TRibbonImage>;
    FLargeImages: TRibbonList<TRibbonImage>;
    FSmallHighContrastImages: TRibbonList<TRibbonImage>;
    FLargeHighContrastImages: TRibbonList<TRibbonImage>;
    FConstructing: Boolean;
    procedure SetName(const Value: String);
    procedure SetSymbol(const Value: String);
    procedure SetId(const Value: Integer);
    procedure LoadImages(const List: TRibbonList<TRibbonImage>;
      const E: TXmlElement);
    procedure SaveImages(const Writer: TXmlWriter;
      const List: TRibbonList<TRibbonImage>; const ElementName: String);
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
    procedure Save(const Writer: TXmlWriter);
    procedure SaveRef(const Writer: TXmlWriter; const AttrName: String);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;

    function DisplayName: String; override;
    function AddSmallImage: TRibbonImage;
    function AddLargeImage: TRibbonImage;
    function AddSmallHighContrastImage: TRibbonImage;
    function AddLargeHighContrastImage: TRibbonImage;
    procedure DeleteImage(const Image: TRibbonImage);

    property Name: String read FName write SetName;
    property Symbol: String read FSymbol write SetSymbol;
    property Id: Integer read FId write SetId;
    property Comment: String read FComment write FComment;
    property LabelTitle: TRibbonString read FLabelTitle;
    property LabelDescription: TRibbonString read FLabelDescription;
    property TooltipTitle: TRibbonString read FTooltipTitle;
    property TooltipDescription: TRibbonString read FTooltipDescription;
    property Keytip: TRibbonString read FKeytip;
    property SmallImages: TRibbonList<TRibbonImage> read FSmallImages;
    property LargeImages: TRibbonList<TRibbonImage> read FLargeImages;
    property SmallHighContrastImages: TRibbonList<TRibbonImage> read FSmallHighContrastImages;
    property LargeHighContrastImages: TRibbonList<TRibbonImage> read FLargeHighContrastImages;
  end;

  TRibbonCommandRefObject = class abstract(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FCommandRef: TRibbonCommand;
    procedure SetCommandRef(const Value: TRibbonCommand);
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); virtual;
    procedure FreeNotify(const Obj: TRibbonObject); override;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;

    function DisplayName: String; override;

    property CommandRef: TRibbonCommand read FCommandRef write SetCommandRef;
  end;

  TRibbonView = class abstract(TRibbonObject)
  {$REGION 'Internal Declarations'}
  protected
    procedure Save(const Writer: TXmlWriter); virtual; abstract;
  {$ENDREGION 'Internal Declarations'}
  end;

  TRibbonApplicationMenuRecentItems = class(TRibbonCommandRefObject)
  {$REGION 'Internal Declarations'}
  strict private
    FMaxCount: Integer;
    FEnablePinning: Boolean;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;

    property MaxCount: Integer read FMaxCount write FMaxCount;
    property EnablePinning: Boolean read FEnablePinning write FEnablePinning;
  end;

  TRibbonControl = class abstract(TRibbonCommandRefObject)
  {$REGION 'Internal Declarations'}
  strict private
    FApplicationModes: Cardinal;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function SupportApplicationModes: Boolean; virtual; abstract;
    function CanReorder: Boolean; override;

    property ApplicationModes: Cardinal read FApplicationModes write FApplicationModes;
  end;

  TRibbonAppMenuGroup = class(TRibbonCommandRefObject)
  {$REGION 'Internal Declarations'}
  strict private
    FCategoryClass: TRibbonMenuCategoryClass;
    FControls: TRibbonList<TRibbonControl>;
  strict protected
    function AddControl(const E: TXmlElement): Boolean; virtual;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function CanReorder: Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    { CategoryClass is ignored when menu group is part of the Application Menu }
    property CategoryClass: TRibbonMenuCategoryClass read FCategoryClass write FCategoryClass;

    property Controls: TRibbonList<TRibbonControl> read FControls;
  end;

  TRibbonMenuGroup = class(TRibbonAppMenuGroup)
  {$REGION 'Internal Declarations'}
  strict protected
    function AddControl(const E: TXmlElement): Boolean; override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
  end;

  TRibbonMiniToolbarMenuGroup = class(TRibbonMenuGroup)
  {$REGION 'Internal Declarations'}
  strict protected
    function AddControl(const E: TXmlElement): Boolean; override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
  end;

  TRibbonButton = class(TRibbonControl)
  {$REGION 'Internal Declarations'}
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;
  end;

  TRibbonToggleButton = class(TRibbonControl)
  {$REGION 'Internal Declarations'}
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;
  end;

  TRibbonSplitButton = class(TRibbonControl)
  {$REGION 'Internal Declarations'}
  strict private
    FButtonItem: TRibbonControl;
    FMenuGroups: TRibbonList<TRibbonMenuGroup>;
    FControls: TRibbonList<TRibbonControl>;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;

    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    procedure DeleteButtonItem;
    procedure CreateButtonItem;
    procedure CreateToggleButtonItem;

    { Either a TRibbonButton or TRibbonToggleButton }
    property ButtonItem: TRibbonControl read FButtonItem;

    property MenuGroups: TRibbonList<TRibbonMenuGroup> read FMenuGroups;
    property Controls: TRibbonList<TRibbonControl> read FControls;
  end;

  TRibbonDropDownButton = class(TRibbonControl)
  {$REGION 'Internal Declarations'}
  strict private
    FControls: TRibbonList<TRibbonControl>;
    FMenuGroups: TRibbonList<TRibbonMenuGroup>;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    property Controls: TRibbonList<TRibbonControl> read FControls;
    property MenuGroups: TRibbonList<TRibbonMenuGroup> read FMenuGroups;
  end;

  TRibbonDropDownColorPicker = class(TRibbonControl)
  {$REGION 'Internal Declarations'}
  strict private
    FColorTemplate: TRibbonColorTemplate;
    FChipSize: TRibbonChipSize;
    FColumns: Integer;
    FThemeColorGridRows: Integer;
    FStandardColorGridRows: Integer;
    FRecentColorGridRows: Integer;
    FIsAutomaticColorButtonVisible: Boolean;
    FIsNoColorButtonVisible: Boolean;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;

    property ColorTemplate: TRibbonColorTemplate read FColorTemplate write FColorTemplate;
    property ChipSize: TRibbonChipSize read FChipSize write FChipSize;
    property Columns: Integer read FColumns write FColumns;
    property ThemeColorGridRows: Integer read FThemeColorGridRows write FThemeColorGridRows;
    property StandardColorGridRows: Integer read FStandardColorGridRows write FStandardColorGridRows;
    property RecentColorGridRows: Integer read FRecentColorGridRows write FRecentColorGridRows;
    property IsAutomaticColorButtonVisible: Boolean read FIsAutomaticColorButtonVisible write FIsAutomaticColorButtonVisible;
    property IsNoColorButtonVisible: Boolean read FIsNoColorButtonVisible write FIsNoColorButtonVisible;
  end;

  TRibbonSpinner = class(TRibbonControl)
  {$REGION 'Internal Declarations'}
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;
  end;

  TRibbonCheckBox = class(TRibbonControl)
  {$REGION 'Internal Declarations'}
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;
  end;

  TRibbonComboBox = class(TRibbonControl)
  {$REGION 'Internal Declarations'}
  strict private
    FIsEditable: Boolean;
    FResizeType: TRibbonComboBoxResizeType;
    FIsAutoCompleteEnabled: Boolean;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;

    property IsEditable: Boolean read FIsEditable write FIsEditable;
    property ResizeType: TRibbonComboBoxResizeType read FResizeType write FResizeType;
    property IsAutoCompleteEnabled: Boolean read FIsAutoCompleteEnabled write FIsAutoCompleteEnabled;
  end;

  TRibbonQatControl = class abstract(TRibbonControl)
  {$REGION 'Internal Declarations'}
  strict private
    FIsChecked: Boolean;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function SupportApplicationModes: Boolean; override;
    property IsChecked: Boolean read FIsChecked write FIsChecked;
  end;

  TRibbonQatButton = class(TRibbonQatControl)
  {$REGION 'Internal Declarations'}
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
  end;

  TRibbonQatToggleButton = class(TRibbonQatControl)
  {$REGION 'Internal Declarations'}
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
  end;

  TRibbonQatCheckBox = class(TRibbonQatControl)
  {$REGION 'Internal Declarations'}
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
  end;

  TRibbonHelpButton = class(TRibbonControl)
  {$REGION 'Internal Declarations'}
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;
    function CanReorder: Boolean; override;
  end;

  TRibbonFloatieFontControl = class(TRibbonControl)
  {$REGION 'Internal Declarations'}
  strict private
    FShowTrueTypeOnly: Boolean;
    FShowVerticalFonts: Boolean;
    FMinimumFontSize: Integer;
    FMaximumFontSize: Integer;
    procedure SetMaximumFontSize(const Value: Integer);
    procedure SetMinimumFontSize(const Value: Integer);
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
    procedure SaveAttributes(const Writer: TXmlWriter); virtual;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;

    property ShowTrueTypeOnly: Boolean read FShowTrueTypeOnly write FShowTrueTypeOnly;
    property ShowVerticalFonts: Boolean read FShowVerticalFonts write FShowVerticalFonts;
    property MinimumFontSize: Integer read FMinimumFontSize write SetMinimumFontSize;
    property MaximumFontSize: Integer read FMaximumFontSize write SetMaximumFontSize;
  end;

  TRibbonFontControl = class(TRibbonFloatieFontControl)
  {$REGION 'Internal Declarations'}
  strict private
    FFontType: TRibbonFontType;
    FIsStrikethroughButtonVisible: Boolean;
    FIsUnderlineButtonVisible: Boolean;
    FIsHighlightButtonVisible: Boolean;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure SaveAttributes(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;

    property FontType: TRibbonFontType read FFontType write FFontType;
    property IsStrikethroughButtonVisible: Boolean read FIsStrikethroughButtonVisible write FIsStrikethroughButtonVisible;
    property IsUnderlineButtonVisible: Boolean read FIsUnderlineButtonVisible write FIsUnderlineButtonVisible;
    property IsHighlightButtonVisible: Boolean read FIsHighlightButtonVisible write FIsHighlightButtonVisible;
  end;

  { NOTE: This is not an actual control, since it doesn't have a CommandRef
    property. However, we treat it as a control so it can be places alongside
    other controls. }
  TRibbonControlGroup = class(TRibbonControl)
  {$REGION 'Internal Declarations'}
  strict private
    FSequenceNumber: Integer;
    FControls: TRibbonList<TRibbonControl>;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    function DisplayName: String; override;
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    destructor Destroy; override;
    property SequenceNumber: Integer read FSequenceNumber write FSequenceNumber;
    property Controls: TRibbonList<TRibbonControl> read FControls;
  end;

  TRibbonGalleryMenuLayout = class abstract(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FRows: Integer;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); virtual;
  {$ENDREGION 'Internal Declarations'}
  public
    property Rows: Integer read FRows write FRows;
  end;

  TRibbonVerticalMenuLayout = class(TRibbonGalleryMenuLayout)
  {$REGION 'Internal Declarations'}
  strict private
    FGripper: TRibbonSingleColumnGripperType;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;

    property Gripper: TRibbonSingleColumnGripperType read FGripper write FGripper;
  end;

  TRibbonFlowMenuLayout = class(TRibbonGalleryMenuLayout)
  {$REGION 'Internal Declarations'}
  strict private
    FColumns: Integer;
    FGripper: TRibbonMultiColumnGripperType;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;

    property Columns: Integer read FColumns write FColumns;
    property Gripper: TRibbonMultiColumnGripperType read FGripper write FGripper;
  end;

  TRibbonGallery = class abstract(TRibbonControl)
  {$REGION 'Internal Declarations'}
  strict private
    FGalleryType: TRibbonGalleryType;
    FHasLargeItems: Boolean;
    FItemHeight: Integer;
    FItemWidth: Integer;
    FTextPosition: TRibbonTextPosition;
    FMenuLayout: TRibbonGalleryMenuLayout;
    FMenuGroups: TRibbonList<TRibbonMenuGroup>;
    FControls: TRibbonList<TRibbonControl>;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
    procedure SaveAttributes(const Writer: TXmlWriter); virtual;
    procedure SaveMenuLayout(const Writer: TXmlWriter); virtual; abstract;
    procedure SaveMenuGroups(const Writer: TXmlWriter); virtual; abstract;
    function HandleElement(const E: TXmlElement): Boolean; virtual;
    procedure LoadMenuLayout(const E: TXmlElement);
    procedure LoadMenuGroups(const E: TXmlElement);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;

    procedure DeleteMenuLayout;
    procedure CreateVerticalMenuLayout;
    procedure CreateFlowMenuLayout;

    property GalleryType: TRibbonGalleryType read FGalleryType write FGalleryType;
    property HasLargeItems: Boolean read FHasLargeItems write FHasLargeItems;
    property ItemHeight: Integer read FItemHeight write FItemHeight;
    property ItemWidth: Integer read FItemWidth write FItemWidth;
    property TextPosition: TRibbonTextPosition read FTextPosition write FTextPosition;

    { Either a TRibbonVerticalMenuLayout or TRibbonFlowMenuLayout }
    property MenuLayout: TRibbonGalleryMenuLayout read FMenuLayout;
    property MenuGroups: TRibbonList<TRibbonMenuGroup> read FMenuGroups;
    property Controls: TRibbonList<TRibbonControl> read FControls;
  end;

  TRibbonDropDownGallery = class(TRibbonGallery)
  {$REGION 'Internal Declarations'}
  protected
    procedure Save(const Writer: TXmlWriter); override;
    procedure SaveMenuLayout(const Writer: TXmlWriter); override;
    procedure SaveMenuGroups(const Writer: TXmlWriter); override;
    function HandleElement(const E: TXmlElement): Boolean; override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;
  end;

  TRibbonSplitButtonGallery = class(TRibbonGallery)
  {$REGION 'Internal Declarations'}
  protected
    procedure Save(const Writer: TXmlWriter); override;
    procedure SaveMenuLayout(const Writer: TXmlWriter); override;
    procedure SaveMenuGroups(const Writer: TXmlWriter); override;
    function HandleElement(const E: TXmlElement): Boolean; override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;
  end;

  TRibbonInRibbonGallery = class(TRibbonGallery)
  {$REGION 'Internal Declarations'}
  strict private
    FMinColumnsLarge: Integer;
    FMaxColumnsMedium: Integer;
    FMinColumnsMedium: Integer;
    FMaxColumns: Integer;
    FMaxRows: Integer;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
    procedure SaveAttributes(const Writer: TXmlWriter); override;
    procedure SaveMenuLayout(const Writer: TXmlWriter); override;
    procedure SaveMenuGroups(const Writer: TXmlWriter); override;
    function HandleElement(const E: TXmlElement): Boolean; override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;

    property MinColumnsLarge: Integer read FMinColumnsLarge write FMinColumnsLarge;
    property MaxColumnsMedium: Integer read FMaxColumnsMedium write FMaxColumnsMedium;
    property MinColumnsMedium: Integer read FMinColumnsMedium write FMinColumnsMedium;
    property MaxColumns: Integer read FMaxColumns write FMaxColumns;
    property MaxRows: Integer read FMaxRows write FMaxRows;
  end;

  TRibbonApplicationMenu = class(TRibbonControl)
  {$REGION 'Internal Declarations'}
  strict private
    FRecentItems: TRibbonApplicationMenuRecentItems;
    FMenuGroups: TRibbonList<TRibbonAppMenuGroup>;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;
    procedure EnableRecentItems(const Enable: Boolean);

    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function CanReorder: Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    property RecentItems: TRibbonApplicationMenuRecentItems read FRecentItems;
    property MenuGroups: TRibbonList<TRibbonAppMenuGroup> read FMenuGroups;
  end;

  TRibbonQuickAccessToolbar = class(TRibbonControl)
  {$REGION 'Internal Declarations'}
  strict private
    FCustomizeCommandRef: TRibbonCommand;
    FControls: TRibbonList<TRibbonQatControl>;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
    procedure SetCustomizeCommandRef(const Value: TRibbonCommand);
  protected
    procedure Save(const Writer: TXmlWriter); override;
    procedure FreeNotify(const Obj: TRibbonObject); override;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function CanReorder: Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    property CustomizeCommandRef: TRibbonCommand read FCustomizeCommandRef write SetCustomizeCommandRef;
    property Controls: TRibbonList<TRibbonQatControl> read FControls;
  end;

  TRibbonScale = class(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FGroupRef: TRibbonCommand;
    FSize: TRibbonGroupLayout;
    procedure SetGroupRef(const Value: TRibbonCommand);
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
    procedure Save(const Writer: TXmlWriter);
  protected
    procedure FreeNotify(const Obj: TRibbonObject); override;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    function DisplayName: String; override;
    function CanReorder: Boolean; override;

    property GroupRef: TRibbonCommand read FGroupRef write SetGroupRef;
    property Size: TRibbonGroupLayout read FSize write FSize;
  end;

  TRibbonScalingPolicy = class(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FIdealSizes: TRibbonList<TRibbonScale>;
    FScales: TRibbonList<TRibbonScale>;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
    procedure Save(const Writer: TXmlWriter);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    function DisplayName: String; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function AddIdealSize: TRibbonScale;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    property IdealSizes: TRibbonList<TRibbonScale> read FIdealSizes;
    property Scales: TRibbonList<TRibbonScale> read FScales;
  end;

  TRibbonControlNameMap = class(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FControlNameDefinitions: TRibbonList<String>;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
    procedure Save(const Writer: TXmlWriter);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    procedure Clear;
    procedure Add(const Name: String);

    property ControlNameDefinitions: TRibbonList<String> read FControlNameDefinitions;
  end;

  TRibbonGroupSizeDefinitionElement = class abstract(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FOwnerDefinition: TRibbonSizeDefinition;
  private
    constructor Create(const Owner: TRibbonDocument;
      const OwnerDefinition: TRibbonSizeDefinition);
  protected
    procedure Save(const Writer: TXmlWriter); virtual; abstract;
  {$ENDREGION 'Internal Declarations'}
  public
    function CanReorder: Boolean; override;

    property OwnerDefinition: TRibbonSizeDefinition read FOwnerDefinition;
  end;

  TRibbonControlSizeDefinition = class(TRibbonGroupSizeDefinitionElement)
  {$REGION 'Internal Declarations'}
  strict private
    FImageSize: TRibbonImageSize;
    FIsLabelVisible: Boolean;
    FIsImageVisible: Boolean;
    FIsPopup: Boolean;
    FControlName: String;
  private
    constructor Create(const Owner: TRibbonDocument;
      const OwnerDefinition: TRibbonSizeDefinition); overload;
    constructor Create(const Owner: TRibbonDocument;
      const OwnerDefinition: TRibbonSizeDefinition; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
    function DisplayName: String; override;

    property ImageSize: TRibbonImageSize read FImageSize write FImageSize;
    property IsLabelVisible: Boolean read FIsLabelVisible write FIsLabelVisible;
    property IsImageVisible: Boolean read FIsImageVisible write FIsImageVisible;
    property IsPopup: Boolean read FIsPopup write FIsPopup;
    property ControlName: String read FControlName write FControlName;
  end;

  TRibbonRow = class(TRibbonGroupSizeDefinitionElement)
  {$REGION 'Internal Declarations'}
  strict private
    FElements: TRibbonList<TRibbonGroupSizeDefinitionElement>;
  private
    constructor Create(const Owner: TRibbonDocument;
      const OwnerDefinition: TRibbonSizeDefinition); overload;
    constructor Create(const Owner: TRibbonDocument;
      const OwnerDefinition: TRibbonSizeDefinition; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    function DisplayName: String; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    { List of elements of type:
      -TRibbonControlSizeDefinition
      -TRibbonControlSizeGroup }
    property Elements: TRibbonList<TRibbonGroupSizeDefinitionElement> read FElements;
  end;

  TRibbonControlSizeGroup = class(TRibbonGroupSizeDefinitionElement)
  {$REGION 'Internal Declarations'}
  strict private
    FControlSizeDefinitions: TRibbonList<TRibbonControlSizeDefinition>;
  private
    constructor Create(const Owner: TRibbonDocument;
      const OwnerDefinition: TRibbonSizeDefinition); overload;
    constructor Create(const Owner: TRibbonDocument;
      const OwnerDefinition: TRibbonSizeDefinition; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    function DisplayName: String; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    property ControlSizeDefinitions: TRibbonList<TRibbonControlSizeDefinition> read FControlSizeDefinitions;
  end;

  TRibbonColumnBreak = class(TRibbonGroupSizeDefinitionElement)
  {$REGION 'Internal Declarations'}
  strict private
    FShowSeparator: Boolean;
  private
    constructor Create(const Owner: TRibbonDocument;
      const OwnerDefinition: TRibbonSizeDefinition); overload;
    constructor Create(const Owner: TRibbonDocument;
      const OwnerDefinition: TRibbonSizeDefinition; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
    function DisplayName: String; override;

    property ShowSeparator: Boolean read FShowSeparator write FShowSeparator;
  end;

  TRibbonGroupSizeDefinition = class(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FOwnerDefinition: TRibbonSizeDefinition;
    FSize: TRibbonGroupSizeType;
    FElements: TRibbonList<TRibbonGroupSizeDefinitionElement>;
  private
    constructor Create(const Owner: TRibbonDocument;
      const OwnerDefinition: TRibbonSizeDefinition); overload;
    constructor Create(const Owner: TRibbonDocument;
      const OwnerDefinition: TRibbonSizeDefinition; const E: TXmlElement); overload;
    procedure Save(const Writer: TXmlWriter);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    function DisplayName: String; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function CanReorder: Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    property Size: TRibbonGroupSizeType read FSize write FSize;

    { List of elements of type:
      -TRibbonControlSizeDefinition
      -TRibbonControlSizeGroup
      -TRibbonColumnBreak
      -TRibbonRow }
    property Elements: TRibbonList<TRibbonGroupSizeDefinitionElement> read FElements;
  end;

  TRibbonSizeDefinition = class(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FControlNameMap: TRibbonControlNameMap;
    FGroupSizeDefinitions: TRibbonList<TRibbonGroupSizeDefinition>;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
    procedure Save(const Writer: TXmlWriter);
  strict protected
    procedure SaveAttributes(const Writer: TXmlWriter); virtual;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    function DisplayName: String; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function CanReorder: Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    property ControlNameMap: TRibbonControlNameMap read FControlNameMap;
    property GroupSizeDefinitions: TRibbonList<TRibbonGroupSizeDefinition> read FGroupSizeDefinitions;
  end;

  TRibbonRibbonSizeDefinition = class(TRibbonSizeDefinition)
  {$REGION 'Internal Declarations'}
  strict private
    FName: String;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  strict protected
    procedure SaveAttributes(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ObjectType: TRibbonObjectType; override;
    function DisplayName: String; override;

    property Name: String read FName write FName;
  end;

  TRibbonGroup = class(TRibbonControl)
  {$REGION 'Internal Declarations'}
  strict private
    FBasicSizeDefinition: TRibbonBasicSizeDefinition;
    FCustomSizeDefinition: String;
    FSizeDefinition: TRibbonSizeDefinition;
    FControls: TRibbonList<TRibbonControl>;
    procedure SetBasicSizeDefinition(const Value: TRibbonBasicSizeDefinition);
    procedure SetCustomSizeDefinition(const Value: String);
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    procedure CreateAdvancedSizeDefinition;
    procedure DeleteAdvancedSizeDefinition;

    property BasicSizeDefinition: TRibbonBasicSizeDefinition read FBasicSizeDefinition write SetBasicSizeDefinition;
    property CustomSizeDefinition: String read FCustomSizeDefinition write SetCustomSizeDefinition;
    property SizeDefinition: TRibbonSizeDefinition read FSizeDefinition;
    property Controls: TRibbonList<TRibbonControl> read FControls;
  end;

  TRibbonTab = class(TRibbonControl)
  {$REGION 'Internal Declarations'}
  strict private
    FScalingPolicy: TRibbonScalingPolicy;
    FGroups: TRibbonList<TRibbonGroup>;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    property ScalingPolicy: TRibbonScalingPolicy read FScalingPolicy;
    property Groups: TRibbonList<TRibbonGroup> read FGroups;
  end;

  TRibbonTabGroup = class(TRibbonControl)
  {$REGION 'Internal Declarations'}
  strict private
    FTabs: TRibbonList<TRibbonTab>;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    class function SupportApplicationModes: Boolean; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    property Tabs: TRibbonList<TRibbonTab> read FTabs;
  end;

  TRibbonViewRibbon = class(TRibbonView)
  {$REGION 'Internal Declarations'}
  strict private
    FName: String;
    FGroupSpacing: TRibbonGroupSpacing;
    FApplicationMenu: TRibbonApplicationMenu;
    FHelpButton: TRibbonHelpButton;
    FQuickAccessToolbar: TRibbonQuickAccessToolbar;
    FTabs: TRibbonList<TRibbonTab>;
    FContextualTabs: TRibbonList<TRibbonTabGroup>;
    FSizeDefinitions: TRibbonList<TRibbonRibbonSizeDefinition>;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    function DisplayName: String; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    property Name: String read FName write FName;
    property GroupSpacing: TRibbonGroupSpacing read FGroupSpacing write FGroupSpacing;
    property ApplicationMenu: TRibbonApplicationMenu read FApplicationMenu;
    property HelpButton: TRibbonHelpButton read FHelpButton;
    property QuickAccessToolbar: TRibbonQuickAccessToolbar read FQuickAccessToolbar;
    property Tabs: TRibbonList<TRibbonTab> read FTabs;
    property ContextualTabs: TRibbonList<TRibbonTabGroup> read FContextualTabs;
    property SizeDefinitions: TRibbonList<TRibbonRibbonSizeDefinition> read FSizeDefinitions write FSizeDefinitions;
  end;

  TRibbonMiniToolbar = class(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FName: String;
    FMenuGroups: TRibbonList<TRibbonMiniToolbarMenuGroup>;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
    procedure Save(const Writer: TXmlWriter);
    procedure SaveRef(const Writer: TXmlWriter);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    function DisplayName: String; override;
    class function ObjectType: TRibbonObjectType; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function CanReorder: Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    property Name: String read FName write FName;
    property MenuGroups: TRibbonList<TRibbonMiniToolbarMenuGroup> read FMenuGroups;
  end;

  TRibbonContextMenu = class(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FName: String;
    FMenuGroups: TRibbonList<TRibbonMenuGroup>;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
    procedure Save(const Writer: TXmlWriter);
    procedure SaveRef(const Writer: TXmlWriter);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    function DisplayName: String; override;
    class function ObjectType: TRibbonObjectType; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function CanReorder: Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    property Name: String read FName write FName;
    property MenuGroups: TRibbonList<TRibbonMenuGroup> read FMenuGroups;
  end;

  TRibbonContextMap = class(TRibbonCommandRefObject)
  {$REGION 'Internal Declarations'}
  strict private
    FContextPopup: TRibbonViewContextPopup;
    FContextMenuRef: TRibbonContextMenu;
    FMiniToolbarRef: TRibbonMiniToolbar;
    FContextMenuName: String;
    FMiniToolbarName: String;
    procedure SetContextMenuRef(const Value: TRibbonContextMenu);
    procedure SetMiniToolbarRef(const Value: TRibbonMiniToolbar);
  private
    constructor Create(const Owner: TRibbonDocument;
      const Popup: TRibbonViewContextPopup); overload;
    constructor Create(const Owner: TRibbonDocument;
      const Popup: TRibbonViewContextPopup; const E: TXmlElement); overload;
    procedure FixupReferences;
  protected
    procedure Save(const Writer: TXmlWriter); override;
    procedure FreeNotify(const Obj: TRibbonObject); override;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    function DisplayName: String; override;
    class function ObjectType: TRibbonObjectType; override;
    function CanReorder: Boolean; override;

    property ContextPopup: TRibbonViewContextPopup read FContextPopup;
    property ContextMenuRef: TRibbonContextMenu read FContextMenuRef write SetContextMenuRef;
    property MiniToolbarRef: TRibbonMiniToolbar read FMiniToolbarRef write SetMiniToolbarRef;
  end;

  TRibbonViewContextPopup = class(TRibbonView)
  {$REGION 'Internal Declarations'}
  strict private
    FMiniToolbars: TRibbonList<TRibbonMiniToolbar>;
    FContextMenus: TRibbonList<TRibbonContextMenu>;
    FContextMaps: TRibbonList<TRibbonContextMap>;
  protected
    procedure Save(const Writer: TXmlWriter); override;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor Create(const Owner: TRibbonDocument; const E: TXmlElement); overload;
    procedure FixupContextMaps;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    property MiniToolbars: TRibbonList<TRibbonMiniToolbar> read FMiniToolbars;
    property ContextMenus: TRibbonList<TRibbonContextMenu> read FContextMenus;
    property ContextMaps: TRibbonList<TRibbonContextMap> read FContextMaps;
  end;

  TRibbonApplication = class(TRibbonObject)
  {$REGION 'Internal Declarations'}
  strict private
    FCommands: TRibbonList<TRibbonCommand>;
    FCommandsByName: TRibbonDictionary<String, TRibbonCommand>;
    FCommandsById: TRibbonDictionary<Integer, TRibbonCommand>;
    FViews: TRibbonList<TRibbonView>;
    function GetRibbon: TRibbonViewRibbon;
  private
    constructor Create(const Owner: TRibbonDocument); overload;
    constructor CreateEmpty(const Owner: TRibbonDocument); overload;
    procedure Load(const E: TXmlElement);
    procedure Save(const Writer: TXmlWriter);
    procedure FixupContextMaps;
    procedure CommandNameChanged(const Command: TRibbonCommand;
      const OldName, NewName: String);
    procedure CommandIdChanged(const Command: TRibbonCommand;
      const OldId, NewId: Integer);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;
    class function ObjectType: TRibbonObjectType; override;
    function AddNew(const ObjType: TRibbonObjectType): TRibbonObject; override;
    function Delete(const Obj: TRibbonObject): Boolean; override;
    function Reorder(const Child: TRibbonObject; const Direction: Integer): Boolean; override;

    { <Application.Commands> element. }
    property Commands: TRibbonList<TRibbonCommand> read FCommands;

    function AddCommand(const Name: String): TRibbonCommand;
    procedure DeleteCommand(const Command: TRibbonCommand);

    { Finds a command with the given name or Id.
      Returns nil when the command does not exist. }
    function FindCommand(const Name: String): TRibbonCommand; overload;
    function FindCommand(const Id: Integer): TRibbonCommand; overload;
    function FindCommand(const Name: TRibbonCommandName): TRibbonCommand; overload;

    { Finds a context menu or mini toolbar with the given name }
    function FindContextMenu(const Name: String): TRibbonContextMenu;
    function FindMiniToolbar(const Name: String): TRibbonMiniToolbar;

    { <Application.Views> element. }
    property Views: TRibbonList<TRibbonView> read FViews;

    property Ribbon: TRibbonViewRibbon read GetRibbon;
  end;

resourcestring
  RS_NO_APPLICATION = 'Ribbon XML must have an <Application> root element.';
  RS_INVALID_COMMANDS = 'There can be at most 1 <Application.Commands> element.';
  RS_INVALID_VIEWS = 'There must be exactly 1 <Application.Views> element.';
  RS_INVALID_COMMAND_NAME = 'Invalid command name "%s". Name must start with a letter or underscore and contain only letters, digits or underscores after that.';
  RS_INVALID_SYMBOL = 'Invalid symbol "%s". Symbol must start with a letter or underscore and contain only letters, digits or underscores after that.';
  RS_ELEMENT_EXPECTED = 'Element <%s> expected, but found <%s>.';
  RS_INVALID_ID = 'Id (%d) must have a value between 2 and 59999.';
  RS_MULTIPLE_RIBBON_STRINGS = 'Element can have at most 1 <String> child element.';
  RS_UNSUPPORTED_CHILD_ELEMENT = 'Child element <%s> is not allowed within element <%s>.';
  RS_INVALID_DPI_VALUE = 'MinDPI value must be at least 96.';
  RS_MULTIPLE_ELEMENTS = 'Element <%s> can have at most 1 <%s> child element.';
  RS_SINGLE_ELEMENT = 'Element <%s> must contain exactly 1 <%s> child element.';
  RS_REQUIRED_ELEMENT = 'Element <%s> must have at least 1 <%s> child element.';
  RS_INVALID_GROUP_SPACING = 'Invalid GroupSpacing attribute value.';
  RS_INVALID_CATEGORY_CLASS = 'Invalid Class attribute value.';
  RS_INVALID_APPLICATION_MODE = 'Invalid ApplicationModes attribute value.';
  RS_INVALID_BUTTON_ITEM = '<SplitButton.ButtonItem> must reference a Button or ToggleButton.';
  RS_INVALID_SPLITBUTTON = 'A <SplitButton> must either have a <SplitButton.MenuGroups> element or one or more control elements.';
  RS_INVALID_SIZE = 'Invalid Size attribute value.';
  RS_INVALID_FONT_TYPE = 'Invalid FontType attribute value.';
  RS_INVALID_DROPDOWN_BUTTON = 'A <DropDownButton> must either have one or more <MenuGroup> elements or control elements.';
  RS_INVALID_COLOR_TEMPLATE = 'Invalid ColorTemplate attribute value.';
  RS_INVALID_CHIP_SIZE = 'Invalid ChipSize attribute value.';
  RS_INVALID_TYPE = 'Invalid Type attribute value.';
  RS_INVALID_TEXT_POSITION = 'Invalid TextPosition attribute value.';
  RS_INVALID_GRIPPER = 'Invalid Gripper attribute value.';
  RS_INVALID_GROUP_SIZE_DEFINITIONS = '<SizeDefinition> element must contain between 1 and 3 <GroupSizeDefinition> child elements.';
  RS_INVALID_IMAGE_SIZE = 'Invalid ImageSize attribute value.';
  RS_INVALID_RESIZE_TYPE = 'Invalid ResizeType attribute value.';
  RS_INVALID_GALLERY = 'A Gallery must either have a <MenuGroups> element or one or more control elements.';
  RS_CANNOT_ADD_MENU_GROUP_TO_SPLIT_BUTTON = 'Cannot add a menu group to a split button that already contains controls. A split button must either contain menu groups or controls.';
  RS_CANNOT_ADD_MENU_GROUP_TO_DROP_DOWN_BUTTON = 'Cannot add a menu group to a drop-down button that already contains controls. A drop-down button must either contain menu groups or controls.';
  RS_CANNOT_ADD_CONTROL_TO_SPLIT_BUTTON = 'Cannot add a control to a split button that already contains menu groups. A split button must either contain menu groups or controls.';
  RS_CANNOT_ADD_CONTROL_TO_DROP_DOWN_BUTTON = 'Cannot add a control to a drop-down button that already contains menu groups. A drop-down button must either contain menu groups or controls.';
  RS_MAX_GROUP_SIZE_DEF = 'You can add at most 3 groups to a size definition.';
  RS_MIN_GROUP_SIZE_DEF = 'There must be at least 1 group in a size definition.';
  RS_MIN_MINI_TOOLBAR = 'There must be at least 1 menu group in a mini toolbar.';
  RS_MIN_CONTEXT_MENU = 'There must be at least 1 menu group in a context menu.';
  RS_SIZE_DEF = 'Size Definition';
  RS_GROUP = 'Group';
  RS_CONTROL = 'Control';
  RS_ROW = 'Row';
  RS_COLUMN_BREAK = 'Column Break';
  RS_CONTROL_GROUP = 'Control Group';
  RS_CONTEXT_MENU = 'Context Menu';
  RS_MINI_TOOLBAR = 'Mini Toolbar';
  RS_CONTEXT_MAP = 'Context Map';
  RS_SCALING_POLICY = 'Scaling Policy';

implementation

uses
  Math,
  IOUtils,
  StrUtils,
  Windows;

const
  RIBBON_NAMESPACE = 'http://schemas.microsoft.com/windows/2009/Ribbon';

const // Element Names
  EN_APPLICATION                               = 'Application';
  EN_APPLICATION_COMMANDS                      = 'Application.Commands';
  EN_APPLICATION_VIEWS                         = 'Application.Views';
  EN_STRING                                    = 'String';
  EN_STRING_CONTENT                            = 'String.Content';
  EN_STRING_ID                                 = 'String.Id';
  EN_STRING_SYMBOL                             = 'String.Symbol';
  EN_COMMAND                                   = 'Command';
  EN_COMMAND_NAME                              = 'Command.Name';
  EN_COMMAND_SYMBOL                            = 'Command.Symbol';
  EN_COMMAND_ID                                = 'Command.Id';
  EN_COMMAND_LABEL_TITLE                       = 'Command.LabelTitle';
  EN_COMMAND_LABEL_DESCRIPTION                 = 'Command.LabelDescription';
  EN_COMMAND_KEYTIP                            = 'Command.Keytip';
  EN_COMMAND_TOOLTIP_TITLE                     = 'Command.TooltipTitle';
  EN_COMMAND_TOOLTIP_DESCRIPTION               = 'Command.TooltipDescription';
  EN_COMMAND_SMALL_IMAGES                      = 'Command.SmallImages';
  EN_COMMAND_LARGE_IMAGES                      = 'Command.LargeImages';
  EN_COMMAND_SMALL_HIGH_CONTRAST_IMAGES        = 'Command.SmallHighContrastImages';
  EN_COMMAND_LARGE_HIGH_CONTRAST_IMAGES        = 'Command.LargeHighContrastImages';
  EN_COMMAND_COMMENT                           = 'Command.Comment';
  EN_IMAGE                                     = 'Image';
  EN_IMAGE_SOURCE                              = 'Image.Source';
  EN_RIBBON                                    = 'Ribbon';
  EN_RIBBON_SIZE_DEFINITIONS                   = 'Ribbon.SizeDefinitions';
  EN_RIBBON_APPLICATION_MENU                   = 'Ribbon.ApplicationMenu';
  EN_RIBBON_HELP_BUTTON                        = 'Ribbon.HelpButton';
  EN_RIBBON_TABS                               = 'Ribbon.Tabs';
  EN_RIBBON_CONTEXTUAL_TABS                    = 'Ribbon.ContextualTabs';
  EN_RIBBON_QUICK_ACCESS_TOOLBAR               = 'Ribbon.QuickAccessToolbar';
  EN_CONTEXT_POPUP                             = 'ContextPopup';
  EN_CONTEXT_POPUP_MINI_TOOLBARS               = 'ContextPopup.MiniToolbars';
  EN_CONTEXT_POPUP_CONTEXT_MENUS               = 'ContextPopup.ContextMenus';
  EN_CONTEXT_POPUP_CONTEXT_MAPS                = 'ContextPopup.ContextMaps';
  EN_MINI_TOOLBAR                              = 'MiniToolbar';
  EN_CONTEXT_MENU                              = 'ContextMenu';
  EN_CONTEXT_MAP                               = 'ContextMap';
  EN_APPLICATION_MENU                          = 'ApplicationMenu';
  EN_APPLICATION_MENU_RECENT_ITEMS             = 'ApplicationMenu.RecentItems';
  EN_RECENT_ITEMS                              = 'RecentItems';
  EN_MENU_GROUP                                = 'MenuGroup';
  EN_BUTTON                                    = 'Button';
  EN_SPLIT_BUTTON                              = 'SplitButton';
  EN_SPLIT_BUTTON_BUTTON_ITEM                  = 'SplitButton.ButtonItem';
  EN_SPLIT_BUTTON_MENU_GROUPS                  = 'SplitButton.MenuGroups';
  EN_DROP_DOWN_BUTTON                          = 'DropDownButton';
  EN_DROP_DOWN_GALLERY                         = 'DropDownGallery';
  EN_DROP_DOWN_GALLERY_MENU_LAYOUT             = 'DropDownGallery.MenuLayout';
  EN_DROP_DOWN_GALLERY_MENU_GROUPS             = 'DropDownGallery.MenuGroups';
  EN_SPLIT_BUTTON_GALLERY                      = 'SplitButtonGallery';
  EN_SPLIT_BUTTON_GALLERY_MENU_LAYOUT          = 'SplitButtonGallery.MenuLayout';
  EN_SPLIT_BUTTON_GALLERY_MENU_GROUPS          = 'SplitButtonGallery.MenuGroups';
  EN_TOGGLE_BUTTON                             = 'ToggleButton';
  EN_CHECK_BOX                                 = 'CheckBox';
  EN_DROP_DOWN_COLOR_PICKER                    = 'DropDownColorPicker';
  EN_HELP_BUTTON                               = 'HelpButton';
  EN_QUICK_ACCESS_TOOLBAR                      = 'QuickAccessToolbar';
  EN_QUICK_ACCESS_TOOLBAR_APPLICATION_DEFAULTS = 'QuickAccessToolbar.ApplicationDefaults';
  EN_TAB                                       = 'Tab';
  EN_TAB_SCALING_POLICY                        = 'Tab.ScalingPolicy';
  EN_GROUP                                     = 'Group';
  EN_SCALING_POLICY                            = 'ScalingPolicy';
  EN_SCALING_POLICY_IDEAL_SIZES                = 'ScalingPolicy.IdealSizes';
  EN_SCALE                                     = 'Scale';
  EN_SIZE_DEFINITION                           = 'SizeDefinition';
  EN_CONTROL_GROUP                             = 'ControlGroup';
  EN_COMBO_BOX                                 = 'ComboBox';
  EN_SPINNER                                   = 'Spinner';
  EN_IN_RIBBON_GALLERY                         = 'InRibbonGallery';
  EN_IN_RIBBON_GALLERY_MENU_LAYOUT             = 'InRibbonGallery.MenuLayout';
  EN_IN_RIBBON_GALLERY_MENU_GROUPS             = 'InRibbonGallery.MenuGroups';
  EN_FONT_CONTROL                              = 'FontControl';
  EN_VERTICAL_MENU_LAYOUT                      = 'VerticalMenuLayout';
  EN_FLOW_MENU_LAYOUT                          = 'FlowMenuLayout';
  EN_CONTROL_NAME_MAP                          = 'ControlNameMap';
  EN_CONTROL_NAME_DEFINITION                   = 'ControlNameDefinition';
  EN_GROUP_SIZE_DEFINITION                     = 'GroupSizeDefinition';
  EN_CONTROL_SIZE_DEFINITION                   = 'ControlSizeDefinition';
  EN_COLUMN_BREAK                              = 'ColumnBreak';
  EN_ROW                                       = 'Row';
  EN_TAB_GROUP                                 = 'TabGroup';

const // Attribute Names
  AN_XMLNS                                     = 'xmlns';
  AN_NAME                                      = 'Name';
  AN_SYMBOL                                    = 'Symbol';
  AN_ID                                        = 'Id';
  AN_CONTENT                                   = 'Content';
  AN_COMMENT                                   = 'Comment';
  AN_LABEL_TITLE                               = 'LabelTitle';
  AN_LABEL_DESCRIPTION                         = 'LabelDescription';
  AN_TOOLTIP_TITLE                             = 'TooltipTitle';
  AN_TOOLTIP_DESCRIPTION                       = 'TooltipDescription';
  AN_KEYTIP                                    = 'Keytip';
  AN_SOURCE                                    = 'Source';
  AN_MIN_DPI                                   = 'MinDPI';
  AN_GROUP_SPACING                             = 'GroupSpacing';
  AN_COMMAND_NAME                              = 'CommandName';
  AN_MAX_COUNT                                 = 'MaxCount';
  AN_ENABLE_PINNING                            = 'EnablePinning';
  AN_CLASS                                     = 'Class';
  AN_APPLICATION_MODES                         = 'ApplicationModes';
  AN_CUSTOMIZE_COMMAND_NAME                    = 'CustomizeCommandName';
  AN_APPLICATION_DEFAULTS_IS_CHECKED           = 'ApplicationDefaults.IsChecked';
  AN_GROUP                                     = 'Group';
  AN_SIZE                                      = 'Size';
  AN_SIZE_DEFINITION                           = 'SizeDefinition';
  AN_FONT_TYPE                                 = 'FontType';
  AN_IS_STRIKETHROUGH_BUTTON_VISIBLE           = 'IsStrikethroughButtonVisible';
  AN_IS_UNDERLINE_BUTTON_VISIBLE               = 'IsUnderlineButtonVisible';
  AN_IS_HIGHLIGHT_BUTTON_VISIBLE               = 'IsHighlightButtonVisible';
  AN_SHOW_TRUE_TYPE_ONLY                       = 'ShowTrueTypeOnly';
  AN_SHOW_VERTICAL_FONTS                       = 'ShowVerticalFonts';
  AN_MINIMUM_FONT_SIZE                         = 'MinimumFontSize';
  AN_MAXIMUM_FONT_SIZE                         = 'MaximumFontSize';
  AN_SEQUENCE_NUMBER                           = 'SequenceNumber';
  AN_MINI_TOOLBAR                              = 'MiniToolbar';
  AN_CONTEXT_MENU                              = 'ContextMenu';
  AN_COLOR_TEMPLATE                            = 'ColorTemplate';
  AN_CHIP_SIZE                                 = 'ChipSize';
  AN_COLUMNS                                   = 'Columns';
  AN_THEME_COLOR_GRID_ROWS                     = 'ThemeColorGridRows';
  AN_STANDARD_COLOR_GRID_ROWS                  = 'StandardColorGridRows';
  AN_RECENT_COLOR_GRID_ROWS                    = 'RecentColorGridRows';
  AN_IS_AUTOMATIC_COLOR_BUTTON_VISIBLE         = 'IsAutomaticColorButtonVisible';
  AN_IS_NO_COLOR_BUTTON_VISIBLE                = 'IsNoColorButtonVisible';
  AN_TYPE                                      = 'Type';
  AN_HAS_LARGE_ITEMS                           = 'HasLargeItems';
  AN_ITEM_HEIGHT                               = 'ItemHeight';
  AN_ITEM_WIDTH                                = 'ItemWidth';
  AN_TEXT_POSITION                             = 'TextPosition';
  AN_MIN_COLUMNS_LARGE                         = 'MinColumnsLarge';
  AN_MAX_COLUMNS_MEDIUM                        = 'MaxColumnsMedium';
  AN_MIN_COLUMNS_MEDIUM                        = 'MinColumnsMedium';
  AN_MAX_COLUMNS                               = 'MaxColumns';
  AN_MAX_ROWS                                  = 'MaxRows';
  AN_ROWS                                      = 'Rows';
  AN_GRIPPER                                   = 'Gripper';
  AN_IMAGE_SIZE                                = 'ImageSize';
  AN_IS_LABEL_VISIBLE                          = 'IsLabelVisible';
  AN_IS_IMAGE_VISIBLE                          = 'IsImageVisible';
  AN_IS_POPUP                                  = 'IsPopup';
  AN_CONTROL_NAME                              = 'ControlName';
  AN_IS_EDITABLE                               = 'IsEditable';
  AN_RESIZE_TYPE                               = 'ResizeType';
  AN_IS_AUTO_COMPLETE_ENABLED                  = 'IsAutoCompleteEnabled';
  AN_SHOW_SEPARATOR                            = 'ShowSeparator';

const // Enum Strings
  // TRibbonGroupSpacing/TRibbonGroupLayout
  ES_SMALL                                     = 'Small';
  ES_MEDIUM                                    = 'Medium';
  ES_LARGE                                     = 'Large';

  // TRibbonMenuCategoryClass
  ES_STANDARD_ITEMS                            = 'StandardItems';
  ES_MAJOR_ITEMS                               = 'MajorItems';

  // TRibbonGroupLayout
  ES_POPUP                                     = 'Popup';

  // TRibbonBasicSizeDefinition
  ES_SIZE_DEFINITION: array [TRibbonBasicSizeDefinition] of String = ('', '',
    'OneButton', 'TwoButtons', 'ThreeButtons', 'ThreeButtons-OneBigAndTwoSmall',
    'ThreeButtonsAndOneCheckBox', 'FourButtons', 'FiveButtons',
    'FiveOrSixButtons', 'SixButtons', 'SixButtons-TwoColumns', 'SevenButtons',
    'EightButtons', 'EightButtons-LastThreeSmall', 'NineButtons', 'TenButtons',
    'ElevenButtons', 'OneFontControl', 'IntFontOnly', 'IntRichFont',
    'IntFontWithColor', 'OneInRibbonGallery', 'BigButtonsAndSmallButtonsOrInputs',
    'InRibbonGalleryAndBigButton', 'InRibbonGalleryAndButtons-GalleryScalesFirst',
    'InRibbonGalleryAndThreeButtons', 'ButtonGroupsAndInputs', 'ButtonGroups');

  // TRibbonFontType
  ES_FONT_ONLY                                 = 'FontOnly';
  ES_FONT_WITH_COLOR                           = 'FontWithColor';
  ES_RICH_FONT                                 = 'RichFont';

  // TRibbonColorTemplate
  ES_THEME_COLORS                              = 'ThemeColors';
  ES_STANDARD_COLORS                           = 'StandardColors';
  ES_HIGHLIGHT_COLORS                          = 'HighlightColors';

  // TRibbonGalleryType
  ES_ITEMS                                     = 'Items';
  ES_COMMANDS                                  = 'Commands';

  // TRibbonTextPosition
  ES_BOTTOM                                    = 'Bottom';
  ES_HIDE                                      = 'Hide';
  ES_LEFT                                      = 'Left';
  ES_OVERLAP                                   = 'Overlap';
  ES_RIGHT                                     = 'Right';
  ES_TOP                                       = 'Top';

  // TRibbonSingleColumnGripperType/TRibbonMultiColumnGripperType
  ES_NONE                                      = 'None';
  ES_VERTICAL                                  = 'Vertical';
  ES_CORNER                                    = 'Corner';

  // TRibbonComboBoxResizeType
  ES_NO_RESIZE                                 = 'NoResize';
  ES_VERTICAL_RESIZE                           = 'VerticalResize';

function IsValidCommandNameString(const Name: String): Boolean;
var
  C: Char;
  I: Integer;
begin
  Result := (Length(Name) <= 100);
  if (Result) and (Name <> '') then
  begin
    C := Name[1];
    Result := ((C >= 'A') and (C <= 'Z')) or ((C >= 'a') and (C <= 'z')) or (C = '_');
    if Result then
    begin
      for I := 2 to Length(Name) do
      begin
        C := Name[I];
        Result := ((C >= 'A') and (C <= 'Z')) or ((C >= 'a') and (C <= 'z'))
          or ((C >= '0') and (C <= '9')) or (C = '_');
        if (not Result) then
          Exit;
      end;
    end;
  end;
end;

function IsValidSymbolString(const Symbol: String): Boolean; inline;
begin
  Result := IsValidCommandNameString(Symbol);
end;

function IsValidCommandValue(const Value: Integer): Boolean;
begin
  Result := (Value = 0) or ((Value >= 2) and (Value < 59999));
end;

function StringToCommandValue(const S: String): Integer;
begin
  if (S = '') then
    Result := 0
  else if (not TryStrToInt(S, Result)) then
  begin
    if (UpperCase(Copy(S, 1, 2)) = '0X') then
      Result := StrToIntDef('$' + Copy(S, 3, MaxInt), 0)
    else
      Result := -1; // Invalid
  end;
end;

function StringToCommandName(const S: String): TRibbonCommandName;
begin
  Result.Id := StringToCommandValue(S);
  if (Result.Id <= 0) then
  begin
    Result.Id := -1;
    Result.Name := S;
  end
  else
    Result.Name := '';
end;

{ TRibbonObject }

function TRibbonObject.AddNew(const ObjType: TRibbonObjectType): TRibbonObject;
begin
  Result := nil;
end;

function TRibbonObject.CanReorder: Boolean;
begin
  Result := False;
end;

constructor TRibbonObject.Create(const Owner: TRibbonDocument);
begin
  inherited Create;
  FOwner := Owner;
end;

procedure TRibbonObject.Error(const Element: TXmlElement; const Msg: String);
begin
  if Assigned(Element) then
    raise ERibbonMarkupError.Create(Format('Line %d: %s', [Element.GetLineNumber, Msg]))
  else
    raise ERibbonMarkupError.Create(Msg);
end;

function TRibbonObject.Delete(const Obj: TRibbonObject): Boolean;
begin
  Result := False;
end;

destructor TRibbonObject.Destroy;
var
  Listener: TRibbonObject;
begin
  if Assigned(FNotifyList) then
  begin
    for Listener in FNotifyList do
      Listener.FreeNotify(Self);
    FNotifyList.Free;
  end;
  inherited;
end;

function TRibbonObject.DisplayName: String;
begin
  Result := ClassName;
  if StartsText('TRibbon', Result) then
    Result := Copy(Result, 8, MaxInt);
  Result := '(' + Result + ')';
end;

procedure TRibbonObject.Error(const Element: TXmlElement; const Msg: String;
  const Args: array of const);
begin
  Error(Element, Format(Msg, Args));
end;

procedure TRibbonObject.FreeNotification(const Listener: TRibbonObject);
begin
  if (FNotifyList = nil) then
    FNotifyList := TList<TRibbonObject>.Create;
  if (FNotifyList.IndexOf(Listener) < 0) then
    FNotifyList.Add(Listener);
end;

procedure TRibbonObject.FreeNotify(const Obj: TRibbonObject);
begin
  { No default implementation }
end;

function TRibbonObject.GetReferenceCount: Integer;
begin
  if Assigned(FNotifyList) then
    Result := FNotifyList.Count
  else
    Result := 0;
end;

procedure TRibbonObject.RemoveFreeNotification(const Listener: TRibbonObject);
begin
  if Assigned(FNotifyList) then
    FNotifyList.Remove(Listener);
end;

function TRibbonObject.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  Result := False;
end;

{ TRibbonList<T> }

procedure TRibbonList<T>.Add(const Item: T);
begin
  FItems.Add(Item);
end;

procedure TRibbonList<T>.Clear;
var
  Item: T;
begin
  if Assigned(FItems) then
  begin
    if (FOwnsItems) then
      for Item in FItems do
        PObject(@Item)^.Free;
    FItems.Clear;
  end;
end;

constructor TRibbonList<T>.Create(const Owner: TRibbonDocument;
  const OwnsItems: Boolean);
begin
  inherited Create(Owner);
  FItems := TList<T>.Create;
  FOwnsItems := OwnsItems;
end;

destructor TRibbonList<T>.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TRibbonList<T>.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TRibbonList<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := FItems.GetEnumerator;
end;

function TRibbonList<T>.GetItem(const Index: Integer): T;
begin
  Result := FItems[Index];
end;

class function TRibbonList<T>.ObjectType: TRibbonObjectType;
begin
  Result := otList;
end;

function TRibbonList<T>.Remove(const Item: T): Boolean;
var
  I: Integer;
begin
  I := FItems.IndexOf(Item);
  Result := (I >= 0);
  if (Result) then
  begin
    if (FOwnsItems) then
      PObject(@Item)^.Free;
    FItems.Delete(I);
  end;
end;

function TRibbonList<T>.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
var
  Item: T;
  Index: Integer;
begin
  Result := False;
  Item := T(Child);
  Index := FItems.IndexOf(Item);
  if (Direction < 0) then
    Result := (Index > 0)
  else
    Result := (Index >= 0) and (Index < (FItems.Count - 1));
  if (Result) then
    FItems.Exchange(Index, Index + Direction);
end;

{ TRibbonDictionary<TKey, TValue> }

procedure TRibbonDictionary<TKey, TValue>.Add(const Key: TKey;
  const Value: TValue);
begin
  FItems.AddOrSetValue(Key, Value);
end;

function TRibbonDictionary<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;
begin
  Result := FItems.ContainsKey(Key);
end;

constructor TRibbonDictionary<TKey, TValue>.Create(
  const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FItems := TDictionary<TKey, TValue>.Create;
end;

destructor TRibbonDictionary<TKey, TValue>.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TRibbonDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TRibbonDictionary<TKey, TValue>.GetEnumerator: TDictionary<TKey, TValue>.TPairEnumerator;
begin
  Result := FItems.GetEnumerator;
end;

function TRibbonDictionary<TKey, TValue>.GetItem(const Key: TKey): TValue;
begin
  Result := FItems[Key];
end;

function TRibbonDictionary<TKey, TValue>.GetKeys: TDictionary<TKey, TValue>.TKeyCollection;
begin
  Result := FItems.Keys;
end;

function TRibbonDictionary<TKey, TValue>.GetValues: TDictionary<TKey, TValue>.TValueCollection;
begin
  Result := FItems.Values;
end;

class function TRibbonDictionary<TKey, TValue>.ObjectType: TRibbonObjectType;
begin
  Result := otDictionary;
end;

procedure TRibbonDictionary<TKey, TValue>.Remove(const Key: TKey);
begin
  FItems.Remove(Key);
end;

function TRibbonDictionary<TKey, TValue>.TryGetValue(const Key: TKey;
  out Value: TValue): Boolean;
begin
  Result := FItems.TryGetValue(Key, Value);
end;

{ TRibbonString }

function TRibbonString.HasSimpleString: Boolean;
begin
  Result := (FId = 0) and (FSymbol = '') and (FContent <> '');
end;

procedure TRibbonString.Initialize(const E: TXmlElement);
var
  C, GC: TXmlElement;
begin
  FContent := E.ContentAsString;
  FId := 0;
  FSymbol := '';

  if (E.ChildCount = 0) then
    Exit;

  if (E.ChildCount > 1) then
    Error(E, RS_MULTIPLE_RIBBON_STRINGS)
  else
  begin
    C := E[0];
    if (C.Name <> EN_STRING) then
      Error(C, RS_ELEMENT_EXPECTED, [EN_STRING, C.Name]);

    FContent := C.AttributeAsString[AN_CONTENT];
    if (FContent = '') then
      FContent := C.ContentAsString;
    SetId(StringToCommandValue(C.AttributeAsString[AN_ID]));
    SetSymbol(C.AttributeAsString[AN_SYMBOL]);

    for GC in C do
    begin
      if (GC.Name = EN_STRING_CONTENT) then
        FContent := GC.ContentAsString
      else if (GC.Name = EN_STRING_ID) then
        SetId(StringToCommandValue(C.ContentAsString))
      else if (GC.Name = EN_STRING_SYMBOL) then
        SetSymbol(C.ContentAsString)
      else
        Error(GC, RS_UNSUPPORTED_CHILD_ELEMENT, [GC.Name, C.Name]);
    end;
  end;
end;

function TRibbonString.IsSimpleString: Boolean;
begin
  Result := (FId = 0) and (FSymbol = '');
end;

class function TRibbonString.ObjectType: TRibbonObjectType;
begin
  Result := otString;
end;

procedure TRibbonString.Save(const Writer: TXmlWriter; const ElementName: String);
begin
  if (FId <> 0) or (FSymbol <> '') then
  begin
    Writer.WriteStartElement(ElementName);

      Writer.WriteStartElement(EN_STRING);
      if (FId <> 0) then
        Writer.WriteAttribute(AN_ID, FId);
      if (FSymbol <> '') then
        Writer.WriteAttribute(AN_SYMBOL, FSymbol);
      if (FContent <> '') then
        Writer.WriteContent(FContent);
      Writer.WriteEndElement;

    Writer.WriteEndElement;
  end;
end;

procedure TRibbonString.SetId(const Value: Integer);
begin
  if (Value <> FId) then
  begin
    if (not IsValidCommandValue(Value)) then
      Error(nil, RS_INVALID_ID, [Value]);
    FId := Value;
  end;
end;

procedure TRibbonString.SetSymbol(const Value: String);
begin
  if (Value <> FSymbol) then
  begin
    if (not IsValidSymbolString(Value)) then
      Error(nil, RS_INVALID_SYMBOL, [Value]);
    FSymbol := Value;
  end;
end;

{ TRibbonImage }

constructor TRibbonImage.Create(const Owner: TRibbonDocument);
begin
  inherited;
end;

constructor TRibbonImage.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  C: TXmlElement;
begin
  Create(Owner);
  if (E.Name <> EN_IMAGE) then
    Error(E, RS_ELEMENT_EXPECTED, [EN_IMAGE, E.Name]);

  FSource := E.AttributeAsString[AN_SOURCE];
  if (FSource = '') then
    FSource := E.ContentAsString;
  SetId(StringToCommandValue(E.AttributeAsString[AN_ID]));
  SetSymbol(E.AttributeAsString[AN_SYMBOL]);
  SetMinDpi(E.AttributeAsInteger[AN_MIN_DPI]);

  for C in E do
  begin
    if (C.Name = EN_IMAGE_SOURCE) then
      FSource := C.ContentAsString
    else
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;
end;

class function TRibbonImage.ObjectType: TRibbonObjectType;
begin
  Result := otImage;
end;

procedure TRibbonImage.Save(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_IMAGE);
  if (FId <> 0) then
    Writer.WriteAttribute(AN_ID, FId);
  if (FSymbol <> '') then
    Writer.WriteAttribute(AN_SYMBOL, FSymbol);
  if (FMinDpi <> 0) then
    Writer.WriteAttribute(AN_MIN_DPI, FMinDpi);
  if (FSource <> '') then
    Writer.WriteContent(FSource);
  Writer.WriteEndElement;
end;

procedure TRibbonImage.SetId(const Value: Integer);
begin
  if (Value <> FId) then
  begin
    if (not IsValidCommandValue(Value)) then
      Error(nil, RS_INVALID_ID, [Value]);
    FId := Value;
  end;
end;

procedure TRibbonImage.SetMinDpi(const Value: Integer);
begin
  if (Value <> FMinDpi) then
  begin
    if (Value <> 0) and (Value < 96) then
      Error(nil, RS_INVALID_DPI_VALUE);
    FMinDpi := Value;
  end;
end;

procedure TRibbonImage.SetSymbol(const Value: String);
begin
  if (Value <> FSymbol) then
  begin
    if (not IsValidSymbolString(Value)) then
      Error(nil, RS_INVALID_SYMBOL, [Value]);
    FSymbol := Value;
  end;
end;

{ TRibbonDocument }

{ This function is available since Delphi XE }
function IsRelativePath(const Path: String): Boolean;
var
  L: Integer;
begin
  L := Length(Path);
  Result := (L > 0) and (Path[1] <> PathDelim) and (L > 1) and (Path[2] <> ':');
end;

function TRibbonDocument.BuildAbsoluteFilename(const Filename: String): String;
var
  I: Integer;
begin
  if (IsRelativePath(Filename)) then
    Result := FDirectory + Filename
  else
    Result := Filename;
  for I := 1 to Length(Result) do
    if (Result[I] = '/') then
      Result[I] := '\';
end;

function TRibbonDocument.BuildRelativeFilename(const Filename: String): String;
var
  I: Integer;
begin
  if StartsText(FDirectory, Filename) then
  begin
    Result := Copy(Filename, Length(FDirectory) + 1, MaxInt);
    for I := 1 to Length(Result) do
      if (Result[I] = '\') then
        Result[I] := '/';
  end
  else
    Result := Filename;
end;

procedure TRibbonDocument.Clear;
begin
  FFilename := '';
  FDirectory := '';
  FreeAndNil(FApplication);
  FApplication := TRibbonApplication.Create(Self);
end;

constructor TRibbonDocument.Create;
begin
  inherited Create(nil);
  FApplication := TRibbonApplication.Create(Self);
end;

function TRibbonDocument.CreateObject(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  case ObjType of
    otButton:
      Result := TRibbonButton.Create(Self);

    otSplitButton:
      Result := TRibbonSplitButton.Create(Self);

    otToggleButton:
      Result := TRibbonToggleButton.Create(Self);

    otDropDownButton:
      Result := TRibbonDropDownButton.Create(Self);

    otCheckBox:
      Result := TRibbonCheckBox.Create(Self);

    otComboBox:
      Result := TRibbonComboBox.Create(Self);

    otSpinner:
      Result := TRibbonSpinner.Create(Self);

    otDropDownGallery:
      Result := TRibbonDropDownGallery.Create(Self);

    otSplitButtonGallery:
      Result := TRibbonSplitButtonGallery.Create(Self);

    otInRibbonGallery:
      Result := TRibbonInRibbonGallery.Create(Self);

    otDropDownColorPicker:
      Result := TRibbonDropDownColorPicker.Create(Self);

    otFontControl:
      Result := TRibbonFontControl.Create(Self);

    otFloatieFontControl:
      Result := TRibbonFloatieFontControl.Create(Self);

    otControlGroup:
      Result := TRibbonControlGroup.Create(Self);

    otQatButton:
      Result := TRibbonQatButton.Create(Self);

    otQatToggleButton:
      Result := TRibbonQatToggleButton.Create(Self);

    otQatCheckBox:
      Result := TRibbonQatCheckBox.Create(Self);

    otRibbonSizeDefinition:
      Result := TRibbonRibbonSizeDefinition.Create(Self);
  else
    begin
      Result := nil;
      Assert(False);
    end;
  end;
end;

destructor TRibbonDocument.Destroy;
begin
  FApplication.Free;
  inherited;
end;

function TRibbonDocument.FindCommand(const Id: Integer): TRibbonCommand;
begin
  Result := FApplication.FindCommand(Id);
end;

function TRibbonDocument.FindCommand(const Name: String): TRibbonCommand;
begin
  Result := FApplication.FindCommand(Name);
end;

function TRibbonDocument.FindCommand(
  const Name: TRibbonCommandName): TRibbonCommand;
begin
  Result := FApplication.FindCommand(Name);
end;

function TRibbonDocument.FindContextMenu(
  const Name: String): TRibbonContextMenu;
begin
  Result := FApplication.FindContextMenu(Name);
end;

function TRibbonDocument.FindMiniToolbar(
  const Name: String): TRibbonMiniToolbar;
begin
  Result := FApplication.FindMiniToolbar(Name);
end;

procedure TRibbonDocument.LoadFromFile(const Filename: String);
var
  Doc: TXmlDocument;
begin
  FFilename := ExpandFileName(Filename);
  FDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(FFilename));
  Doc := TXmlDocument.Create;
  try
    Doc.LoadFromFile(FFilename);
    LoadFromXmlDocument(Doc);
  finally
    Doc.Free;
  end;
end;

procedure TRibbonDocument.LoadFromStream(const Stream: TStream);
var
  Doc: TXmlDocument;
begin
  FFilename := '';
  Doc := TXmlDocument.Create;
  try
    Doc.LoadFromStream(Stream);
    LoadFromXmlDocument(Doc);
  finally
    Doc.Free;
  end;
end;

procedure TRibbonDocument.LoadFromXml(const Xml: RawByteString);
var
  Doc: TXmlDocument;
begin
  FFilename := '';
  Doc := TXmlDocument.Create;
  try
    Doc.LoadFromXml(Xml);
    LoadFromXmlDocument(Doc);
  finally
    Doc.Free;
  end;
end;

procedure TRibbonDocument.LoadFromXmlDocument(const XmlDoc: TXmlDocument);
begin
  FreeAndNil(FApplication);
  FApplication := TRibbonApplication.CreateEmpty(Self);
  FApplication.Load(XmlDoc.Root);
end;

class function TRibbonDocument.ObjectType: TRibbonObjectType;
begin
  Result := otDocument;
end;

procedure TRibbonDocument.SaveToFile(const Filename: String);
var
  Xml: RawByteString;
  Stream: TFileStream;
begin
  SaveToXml(Xml);
  if (Xml <> '') then
  begin
    if FileExists(Filename) then
      CopyFile(PChar(Filename), PChar(Filename + '.bak'), False);
    Stream := TFileStream.Create(Filename, fmCreate);
    try
      Stream.WriteBuffer(Xml[1], Length(Xml));
    finally
      Stream.Free;
    end;
    FFilename := ExpandFileName(Filename);
    FDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(FFilename));
  end;
end;

procedure TRibbonDocument.SaveToStream(const Stream: TStream);
var
  Xml: RawByteString;
begin
  SaveToXml(Xml);
  if (Xml <> '') then
    Stream.WriteBuffer(Xml[1], Length(Xml));
end;

procedure TRibbonDocument.SaveToXml(out Xml: RawByteString);
var
  Writer: TXmlWriter;
begin
  Writer := TXmlWriter.Create;
  try
    Writer.Indent := True;
    FApplication.Save(Writer);
    Xml := Writer.AsXml;
  finally
    Writer.Free;
  end;
end;

{ TRibbonApplication }

constructor TRibbonApplication.CreateEmpty(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
end;

function TRibbonApplication.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj is TRibbonViewContextPopup) then
    Result := FViews.Remove(TRibbonViewContextPopup(Obj))
  else
    Result := inherited Delete(Obj);
end;

procedure TRibbonApplication.DeleteCommand(const Command: TRibbonCommand);
begin
  FCommands.Remove(Command);
end;

destructor TRibbonApplication.Destroy;
begin
  FCommands.Free;
  FCommandsByName.Free;
  FCommandsById.Free;
  FViews.Free;
  inherited;
end;

function TRibbonApplication.FindCommand(
  const Name: TRibbonCommandName): TRibbonCommand;
begin
  if (Name.Id > 0) then
    FCommandsById.TryGetValue(Name.Id, Result)
  else
    FCommandsByName.TryGetValue(Name.Name, Result);
end;

function TRibbonApplication.FindContextMenu(
  const Name: String): TRibbonContextMenu;
var
  View: TRibbonView;
  Popup: TRibbonViewContextPopup absolute View;
begin
  if (Name <> '') then
  begin
    for View in FViews do
    begin
      if (View is TRibbonViewContextPopup) then
      begin
        for Result in Popup.ContextMenus do
          if (Result.Name = Name) then
            Exit;
      end;
    end;
  end;
  Result := nil;
end;

function TRibbonApplication.FindMiniToolbar(
  const Name: String): TRibbonMiniToolbar;
var
  View: TRibbonView;
  Popup: TRibbonViewContextPopup absolute View;
begin
  if (Name <> '') then
  begin
    for View in FViews do
    begin
      if (View is TRibbonViewContextPopup) then
      begin
        for Result in Popup.MiniToolbars do
          if (Result.Name = Name) then
            Exit;
      end;
    end;
  end;
  Result := nil;
end;

procedure TRibbonApplication.FixupContextMaps;
var
  View: TRibbonView;
  Popup: TRibbonViewContextPopup absolute View;
begin
  for View in FViews do
  begin
    if (View is TRibbonViewContextPopup) then
      Popup.FixupContextMaps;
  end;
end;

function TRibbonApplication.GetRibbon: TRibbonViewRibbon;
var
  View: TRibbonView;
begin
  for View in FViews do
    if (View is TRibbonViewRibbon) then
      Exit(TRibbonViewRibbon(View));
  Result := nil;
end;

procedure TRibbonApplication.Load(const E: TXmlElement);
var
  C, GC: TXmlElement;
  HasRibbon: Boolean;
  Command: TRibbonCommand;
begin
  if (E = nil) or (E.Name <> EN_APPLICATION) then
    Error(E, RS_NO_APPLICATION);

  HasRibbon := False;

  { Load commands first }
  for C in E do
  begin
    if (C.Name = EN_APPLICATION_COMMANDS) then
    begin
      if Assigned(FCommands) then
        Error(C, RS_INVALID_COMMANDS);
      FCommands := TRibbonList<TRibbonCommand>.Create(Owner, True);
      FCommandsByName := TRibbonDictionary<String, TRibbonCommand>.Create(Owner);
      FCommandsById := TRibbonDictionary<Integer, TRibbonCommand>.Create(Owner);
      for GC in C do
      begin
        Command := TRibbonCommand.Create(Owner, GC);
        FCommands.Add(Command);
        if (Command.Name <> '') then
          FCommandsByName.Add(Command.Name, Command);
        if (Command.Id <> 0) then
          FCommandsById.Add(Command.Id, Command);
      end;
    end
    else if (C.Name <> EN_APPLICATION_VIEWS) then
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;

  if (FCommands = nil) then
  begin
    FCommands := TRibbonList<TRibbonCommand>.Create(Owner, True);
    FCommandsByName := TRibbonDictionary<String, TRibbonCommand>.Create(Owner);
    FCommandsById := TRibbonDictionary<Integer, TRibbonCommand>.Create(Owner);
  end;

  { Load views next. These depend on the commands. }
  for C in E do
  begin
    if (C.Name = EN_APPLICATION_VIEWS) then
    begin
      if Assigned(FViews) then
        Error(C, RS_INVALID_VIEWS);
      FViews := TRibbonList<TRibbonView>.Create(Owner, True);
      for GC in C do
      begin
        if (GC.Name = EN_RIBBON) then
        begin
          if (HasRibbon) then
            Error(GC, RS_MULTIPLE_ELEMENTS, [C.Name, GC.Name]);
          HasRibbon := True;
          FViews.Add(TRibbonViewRibbon.Create(Owner, GC));
        end
        else if (GC.Name = EN_CONTEXT_POPUP) then
          FViews.Add(TRibbonViewContextPopup.Create(Owner, GC))
        else
          Error(GC, RS_UNSUPPORTED_CHILD_ELEMENT, [GC.Name, C.Name]);
      end;
    end
    else if (C.Name <> EN_APPLICATION_COMMANDS) then
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;

  if (not HasRibbon) then
    Error(E, RS_SINGLE_ELEMENT, [EN_APPLICATION_VIEWS, EN_RIBBON]);

  if (FViews = nil) then
    Error(E, RS_INVALID_VIEWS);

  { Fixup the <ContextPopup.ContextMaps> references }
  FixupContextMaps;
end;

class function TRibbonApplication.ObjectType: TRibbonObjectType;
begin
  Result := otApplication;
end;

function TRibbonApplication.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonCommand) then
    Result := FCommands.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonApplication.Save(const Writer: TXmlWriter);
var
  Command: TRibbonCommand;
  View: TRibbonView;
begin
  Writer.WriteStartElement(EN_APPLICATION);
  Writer.WriteAttribute(AN_XMLNS, RIBBON_NAMESPACE);

  if (FCommands.Count > 0) then
  begin
    Writer.WriteStartElement(EN_APPLICATION_COMMANDS);

    for Command in FCommands do
      Command.Save(Writer);

    Writer.WriteEndElement;
  end;

  Writer.WriteStartElement(EN_APPLICATION_VIEWS);
  for View in FViews do
    View.Save(Writer);
  Writer.WriteEndElement;

  Writer.WriteEndElement;
end;

function TRibbonApplication.FindCommand(const Id: Integer): TRibbonCommand;
begin
  FCommandsById.TryGetValue(Id, Result);
end;

function TRibbonApplication.FindCommand(const Name: String): TRibbonCommand;
begin
  FCommandsByName.TryGetValue(Name, Result);
end;

function TRibbonApplication.AddCommand(const Name: String): TRibbonCommand;
begin
  Result := TRibbonCommand.Create(Owner);
  Result.Name := Name;
  FCommands.Add(Result);
end;

function TRibbonApplication.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType = otViewContextPopup) then
  begin
    Result := TRibbonViewContextPopup.Create(Owner);
    FViews.Add(TRibbonViewContextPopup(Result));
  end
  else
    Result := inherited AddNew(ObjType);
end;

procedure TRibbonApplication.CommandIdChanged(const Command: TRibbonCommand;
  const OldId, NewId: Integer);
begin
  if (NewId <> OldId) then
  begin
    FCommandsById.Remove(OldId);
    FCommandsById.Add(NewId, Command);
  end;
end;

procedure TRibbonApplication.CommandNameChanged(const Command: TRibbonCommand;
  const OldName, NewName: String);
begin
  if (NewName <> OldName) then
  begin
    FCommandsByName.Remove(OldName);
    FCommandsByName.Add(NewName, Command);
  end;
end;

constructor TRibbonApplication.Create(const Owner: TRibbonDocument);
begin
  inherited;
  FCommands := TRibbonList<TRibbonCommand>.Create(Owner, True);
  FCommandsByName := TRibbonDictionary<String, TRibbonCommand>.Create(Owner);
  FCommandsById := TRibbonDictionary<Integer, TRibbonCommand>.Create(Owner);
  FViews := TRibbonList<TRibbonView>.Create(Owner, True);
  FViews.Add(TRibbonViewRibbon.Create(Owner));
  FViews.Add(TRibbonViewContextPopup.Create(Owner));
end;

{ TRibbonCommand }

constructor TRibbonCommand.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FLabelTitle := TRibbonString.Create(Owner);
  FLabelDescription := TRibbonString.Create(Owner);
  FTooltipTitle := TRibbonString.Create(Owner);
  FTooltipDescription := TRibbonString.Create(Owner);
  FKeytip := TRibbonString.Create(Owner);
  FSmallImages := TRibbonList<TRibbonImage>.Create(Owner, True);
  FLargeImages := TRibbonList<TRibbonImage>.Create(Owner, True);
  FSmallHighContrastImages := TRibbonList<TRibbonImage>.Create(Owner, True);
  FLargeHighContrastImages := TRibbonList<TRibbonImage>.Create(Owner, True);
end;

function TRibbonCommand.AddLargeHighContrastImage: TRibbonImage;
begin
  Result := TRibbonImage.Create(Owner);
  FLargeHighContrastImages.Add(Result);

end;

function TRibbonCommand.AddLargeImage: TRibbonImage;
begin
  Result := TRibbonImage.Create(Owner);
  FLargeImages.Add(Result);
end;

function TRibbonCommand.AddSmallHighContrastImage: TRibbonImage;
begin
  Result := TRibbonImage.Create(Owner);
  FSmallHighContrastImages.Add(Result);
end;

function TRibbonCommand.AddSmallImage: TRibbonImage;
begin
  Result := TRibbonImage.Create(Owner);
  FSmallImages.Add(Result);
end;

constructor TRibbonCommand.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  C: TXmlElement;
begin
  Create(Owner);
  if (E.Name <> EN_COMMAND) then
    Error(E, RS_ELEMENT_EXPECTED, [EN_COMMAND, E.Name]);

  FConstructing := True;
  SetName(E.AttributeAsString[AN_NAME]);
  SetSymbol(E.AttributeAsString[AN_SYMBOL]);
  SetId(StringToCommandValue(E.AttributeAsString[AN_ID]));
  FConstructing := False;
  FComment := E.AttributeAsString[AN_COMMENT];
  FLabelTitle.Content := E.AttributeAsString[AN_LABEL_TITLE];
  FLabelDescription.Content := E.AttributeAsString[AN_LABEL_DESCRIPTION];
  FTooltipTitle.Content := E.AttributeAsString[AN_TOOLTIP_TITLE];
  FTooltipDescription.Content := E.AttributeAsString[AN_TOOLTIP_DESCRIPTION];
  FKeytip.Content := E.AttributeAsString[AN_KEYTIP];

  for C in E do
  begin
    if (C.Name = EN_COMMAND_NAME) then
      SetName(C.ContentAsString)
    else if (C.Name = EN_COMMAND_SYMBOL) then
      SetSymbol(C.ContentAsString)
    else if (C.Name = EN_COMMAND_ID) then
      SetId(StringToCommandValue(C.ContentAsString))
    else if (C.Name = EN_COMMAND_LABEL_TITLE) then
      FLabelTitle.Initialize(C)
    else if (C.Name = EN_COMMAND_LABEL_DESCRIPTION) then
      FLabelDescription.Initialize(C)
    else if (C.Name = EN_COMMAND_KEYTIP) then
      FKeytip.Initialize(C)
    else if (C.Name = EN_COMMAND_TOOLTIP_TITLE) then
      FTooltipTitle.Initialize(C)
    else if (C.Name = EN_COMMAND_TOOLTIP_DESCRIPTION) then
      FTooltipDescription.Initialize(C)
    else if (C.Name = EN_COMMAND_SMALL_IMAGES) then
      LoadImages(FSmallImages, C)
    else if (C.Name = EN_COMMAND_LARGE_IMAGES) then
      LoadImages(FLargeImages, C)
    else if (C.Name = EN_COMMAND_SMALL_HIGH_CONTRAST_IMAGES) then
      LoadImages(FSmallHighContrastImages, C)
    else if (C.Name = EN_COMMAND_LARGE_HIGH_CONTRAST_IMAGES) then
      LoadImages(FLargeHighContrastImages, C)
    else if (C.Name = EN_COMMAND_COMMENT) then
      FComment := C.ContentAsString
    else
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;
end;

procedure TRibbonCommand.DeleteImage(const Image: TRibbonImage);
begin
  FSmallImages.Remove(Image);
  FLargeImages.Remove(Image);
  FSmallHighContrastImages.Remove(Image);
  FLargeHighContrastImages.Remove(Image);
end;

destructor TRibbonCommand.Destroy;
begin
  FLabelTitle.Free;
  FLabelDescription.Free;
  FTooltipTitle.Free;
  FTooltipDescription.Free;
  FKeytip.Free;
  FSmallImages.Free;
  FLargeImages.Free;
  FSmallHighContrastImages.Free;
  FLargeHighContrastImages.Free;
  inherited;
end;

function TRibbonCommand.DisplayName: String;
begin
  if (FName = '') then
  begin
    if (FSymbol = '') then
    begin
      if (FId = 0) then
        Result := ''
      else
        Result := IntToStr(FId);
    end
    else
      Result := FSymbol;
  end
  else
    Result := FName;

  if (FLabelTitle.Content <> '') then
    Result := Result + ' (' + FLabelTitle.Content + ')';
end;

procedure TRibbonCommand.LoadImages(const List: TRibbonList<TRibbonImage>;
  const E: TXmlElement);
var
  C: TXmlElement;
begin
  for C in E do
    List.Add(TRibbonImage.Create(Owner, C));
end;

class function TRibbonCommand.ObjectType: TRibbonObjectType;
begin
  Result := otCommand;
end;

procedure TRibbonCommand.Save(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_COMMAND);

  if (FName <> '') then
    Writer.WriteAttribute(AN_NAME, FName);
  if (FComment <> '') then
    Writer.WriteAttribute(AN_COMMENT, FComment);
  if (FSymbol <> '') then
    Writer.WriteAttribute(AN_SYMBOL, FSymbol);
  if (FId <> 0) then
    Writer.WriteAttribute(AN_ID, FId);
  if (FLabelTitle.HasSimpleString) then
    Writer.WriteAttribute(AN_LABEL_TITLE, FLabelTitle.Content);
  if (FLabelDescription.HasSimpleString) then
    Writer.WriteAttribute(AN_LABEL_DESCRIPTION, FLabelDescription.Content);
  if (FTooltipTitle.HasSimpleString) then
    Writer.WriteAttribute(AN_TOOLTIP_TITLE, FTooltipTitle.Content);
  if (FTooltipDescription.HasSimpleString) then
    Writer.WriteAttribute(AN_TOOLTIP_DESCRIPTION, FTooltipDescription.Content);
  if (FKeytip.HasSimpleString) then
    Writer.WriteAttribute(AN_KEYTIP, FKeytip.Content);

  FLabelTitle.Save(Writer, EN_COMMAND_LABEL_TITLE);
  FLabelDescription.Save(Writer, EN_COMMAND_LABEL_DESCRIPTION);
  FTooltipTitle.Save(Writer, EN_COMMAND_TOOLTIP_TITLE);
  FTooltipDescription.Save(Writer, EN_COMMAND_TOOLTIP_DESCRIPTION);
  FKeytip.Save(Writer, EN_COMMAND_KEYTIP);
  SaveImages(Writer, FSmallImages, EN_COMMAND_SMALL_IMAGES);
  SaveImages(Writer, FLargeImages, EN_COMMAND_LARGE_IMAGES);
  SaveImages(Writer, FSmallHighContrastImages, EN_COMMAND_SMALL_HIGH_CONTRAST_IMAGES);
  SaveImages(Writer, FLargeHighContrastImages, EN_COMMAND_LARGE_HIGH_CONTRAST_IMAGES);

  Writer.WriteEndElement;
end;

procedure TRibbonCommand.SaveImages(const Writer: TXmlWriter;
  const List: TRibbonList<TRibbonImage>; const ElementName: String);
var
  Image: TRibbonImage;
begin
  if (List.Count > 0) then
  begin
    Writer.WriteStartElement(ElementName);
    for Image in List do
      Image.Save(Writer);
    Writer.WriteEndElement;
  end;
end;

procedure TRibbonCommand.SaveRef(const Writer: TXmlWriter;
  const AttrName: String);
begin
  if (FName <> '') then
    Writer.WriteAttribute(AttrName, FName)
  else if (FId <> 0) then
    Writer.WriteAttribute(AttrName, FId);
end;

procedure TRibbonCommand.SetId(const Value: Integer);
begin
  if (Value <> FId) then
  begin
    if (not IsValidCommandValue(Value)) then
      Error(nil, RS_INVALID_ID, [Value]);
    if (not FConstructing) then
      Owner.Application.CommandIdChanged(Self, FId, Value);
    FId := Value;
  end;
end;

procedure TRibbonCommand.SetName(const Value: String);
begin
  if (Value <> FName) then
  begin
    if (not IsValidCommandNameString(Value)) then
      Error(nil, RS_INVALID_COMMAND_NAME, [Value]);
    if (not FConstructing) then
      Owner.Application.CommandNameChanged(Self, FName, Value);
    FName := Value;
  end;
end;

procedure TRibbonCommand.SetSymbol(const Value: String);
begin
  if (Value <> FSymbol) then
  begin
    if (not IsValidSymbolString(Value)) then
      Error(nil, RS_INVALID_SYMBOL, [Value]);
    FSymbol := Value;
  end;
end;

{ TRibbonCommandRefObject }

constructor TRibbonCommandRefObject.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
end;

constructor TRibbonCommandRefObject.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  Name: TRibbonCommandName;
begin
  inherited Create(Owner);
  Name := StringToCommandName(E.AttributeAsString[AN_COMMAND_NAME]);
  FCommandRef := Owner.FindCommand(Name);
  if Assigned(FCommandRef) then
    FCommandRef.FreeNotification(Self);
end;

destructor TRibbonCommandRefObject.Destroy;
begin
  if Assigned(FCommandRef) then
    FCommandRef.RemoveFreeNotification(Self);
  inherited;
end;

function TRibbonCommandRefObject.DisplayName: String;
begin
  if Assigned(FCommandRef) then
  begin
    Result := FCommandRef.LabelTitle.Content;
    if (Result = '') and (FCommandRef.Name <> '') then
      Result := '(' + FCommandRef.Name + ')';
  end
  else
    Result := '';

  if (Result = '') then
    Result := inherited DisplayName;
end;

procedure TRibbonCommandRefObject.FreeNotify(const Obj: TRibbonObject);
begin
  inherited;
  if (Obj = FCommandRef) then
    FCommandRef := nil;
end;

procedure TRibbonCommandRefObject.Save(const Writer: TXmlWriter);
begin
  if Assigned(FCommandRef) then
    FCommandRef.SaveRef(Writer, AN_COMMAND_NAME);
end;

procedure TRibbonCommandRefObject.SetCommandRef(const Value: TRibbonCommand);
begin
  if (Value <> FCommandRef) then
  begin
    if Assigned(FCommandRef) then
      FCommandRef.RemoveFreeNotification(Self);

    FCommandRef := Value;

    if Assigned(FCommandRef) then
      FCommandRef.FreeNotification(Self);
  end;
end;

{ TRibbonApplicationMenuRecentItems }

constructor TRibbonApplicationMenuRecentItems.Create(
  const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FMaxCount := 10;
  FEnablePinning := True;
end;

constructor TRibbonApplicationMenuRecentItems.Create(
  const Owner: TRibbonDocument; const E: TXmlElement);
var
  C: TXmlElement;
begin
  if (E.ChildCount = 0) or (E[0].Name <> EN_RECENT_ITEMS) then
    Error(E, RS_SINGLE_ELEMENT, [E.Name, EN_RECENT_ITEMS]);
  inherited Create(Owner, E[0]);
  C := E[0];
  FMaxCount := C.AttributeAsIntegerDef(AN_MAX_COUNT, 10);
  FEnablePinning := C.AttributeAsBooleanDef(AN_ENABLE_PINNING, True);
end;

class function TRibbonApplicationMenuRecentItems.ObjectType: TRibbonObjectType;
begin
  Result := otRecentItems;
end;

procedure TRibbonApplicationMenuRecentItems.Save(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_APPLICATION_MENU_RECENT_ITEMS);
    Writer.WriteStartElement(EN_RECENT_ITEMS);

    inherited;

    if (FMaxCount <> 10) then
      Writer.WriteAttribute(AN_MAX_COUNT, FMaxCount);

    if (not FEnablePinning) then
      Writer.WriteAttribute(AN_ENABLE_PINNING, FEnablePinning);

    Writer.WriteEndElement;
  Writer.WriteEndElement;
end;

{ TRibbonControl }

constructor TRibbonControl.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FApplicationModes := $FFFFFFFF;
end;

function TRibbonControl.CanReorder: Boolean;
begin
  Result := True;
end;

constructor TRibbonControl.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  S: String;
  Modes: TStringList;
  I: Integer;
begin
  inherited Create(Owner, E);
  FApplicationModes := $FFFFFFFF;
  if (SupportApplicationModes) then
  begin
    S := E.AttributeAsString[AN_APPLICATION_MODES];
    if (S <> '') then
    begin
      Modes := TStringList.Create;
      try
        Modes.Delimiter := ',';
        Modes.DelimitedText := S;
        if (Modes.Count > 0) then
        begin
          FApplicationModes := 0;
          for S in Modes do
          begin
            I := StrToIntDef(S, -1);
            if (I < 0) or (I > 31) then
              Error(E, RS_INVALID_APPLICATION_MODE);
            FApplicationModes := FApplicationModes or (1 shl I);
          end;
        end;
      finally
        Modes.Free;
      end;
    end;
  end;
end;

procedure TRibbonControl.Save(const Writer: TXmlWriter);
var
  S: String;
  I: Integer;
begin
  inherited;
  if (SupportApplicationModes) and (FApplicationModes <> $FFFFFFFF) then
  begin
    S := '';
    for I := 0 to 31 do
    begin
      if ((FApplicationModes and (1 shl I)) <> 0) then
      begin
        if (S <> '') then
          S := S + ',';
        S := S + IntToStr(I);
      end;
    end;
    Writer.WriteAttribute(AN_APPLICATION_MODES, S);
  end;
end;

{ TRibbonButton }

class function TRibbonButton.ObjectType: TRibbonObjectType;
begin
  Result := otButton;
end;

procedure TRibbonButton.Save(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_BUTTON);
  inherited;
  Writer.WriteEndElement;
end;

class function TRibbonButton.SupportApplicationModes: Boolean;
begin
  Result := True;
end;

{ TRibbonToggleButton }

class function TRibbonToggleButton.ObjectType: TRibbonObjectType;
begin
  Result := otToggleButton;
end;

procedure TRibbonToggleButton.Save(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_TOGGLE_BUTTON);
  inherited;
  Writer.WriteEndElement;
end;

class function TRibbonToggleButton.SupportApplicationModes: Boolean;
begin
  Result := False;
end;

{ TRibbonSplitButton }

constructor TRibbonSplitButton.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FMenuGroups := TRibbonList<TRibbonMenuGroup>.Create(Owner, True);
  FControls := TRibbonList<TRibbonControl>.Create(Owner, True);
end;

function TRibbonSplitButton.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType = otMenuGroup) then
  begin
    if (FControls.Count > 0) then
      Error(nil, RS_CANNOT_ADD_MENU_GROUP_TO_SPLIT_BUTTON);
    Result := TRibbonMenuGroup.Create(Owner);
    FMenuGroups.Add(TRibbonMenuGroup(Result));
  end
  else if (ObjType in [otToggleButton, otButton, otCheckBox, otSplitButton,
    otDropDownButton, otDropDownGallery, otSplitButtonGallery,
    otDropDownColorPicker]) then
  begin
    if (FMenuGroups.Count > 0) then
      Error(nil, RS_CANNOT_ADD_CONTROL_TO_SPLIT_BUTTON);
    Result := Owner.CreateObject(ObjType);
    FControls.Add(Result as TRibbonControl);
  end
  else
    Result := inherited AddNew(ObjType);
end;

constructor TRibbonSplitButton.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  C, GC: TXmlElement;
  HasMenuGroups: Boolean;
begin
  inherited Create(Owner, E);
  FMenuGroups := TRibbonList<TRibbonMenuGroup>.Create(Owner, True);
  FControls := TRibbonList<TRibbonControl>.Create(Owner, True);
  HasMenuGroups := False;
  for C in E do
  begin
    if (C.Name = EN_SPLIT_BUTTON_BUTTON_ITEM) then
    begin
      if (C.ChildCount > 1) then
        Error(C, RS_MULTIPLE_ELEMENTS, [C.Name, C[0].Name]);
      if (C.ChildCount = 1) then
      begin
        GC := C[0];
        if (GC.Name = EN_BUTTON) then
          FButtonItem := TRibbonButton.Create(Owner, GC)
        else if (GC.Name = EN_TOGGLE_BUTTON) then
          FButtonItem := TRibbonToggleButton.Create(Owner, GC)
        else
          Error(GC, RS_INVALID_BUTTON_ITEM);
      end;
    end
    else if (C.Name = EN_SPLIT_BUTTON_MENU_GROUPS) then
    begin
      if (HasMenuGroups) then
        Error(C, RS_SINGLE_ELEMENT, [C.Name, EN_MENU_GROUP]);
      HasMenuGroups := True;
      for GC in C do
        FMenuGroups.Add(TRibbonMenuGroup.Create(Owner, GC));
    end
    else if (C.Name = EN_TOGGLE_BUTTON) then
      FControls.Add(TRibbonToggleButton.Create(Owner, C))
    else if (C.Name = EN_CHECK_BOX) then
      FControls.Add(TRibbonCheckBox.Create(Owner, C))
    else if (C.Name = EN_BUTTON) then
      FControls.Add(TRibbonButton.Create(Owner, C))
    else if (C.Name = EN_SPLIT_BUTTON) then
      FControls.Add(TRibbonSplitButton.Create(Owner, C))
    else if (C.Name = EN_DROP_DOWN_BUTTON) then
      FControls.Add(TRibbonDropDownButton.Create(Owner, C))
    else if (C.Name = EN_DROP_DOWN_GALLERY) then
      FControls.Add(TRibbonDropDownGallery.Create(Owner, C))
    else if (C.Name = EN_SPLIT_BUTTON_GALLERY) then
      FControls.Add(TRibbonSplitButtonGallery.Create(Owner, C))
    else if (C.Name = EN_DROP_DOWN_COLOR_PICKER) then
      FControls.Add(TRibbonDropDownColorPicker.Create(Owner, C))
    else
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;

  { There must be either 1 or more controls, or a menu group }
  if (FControls.Count > 0) and (FMenuGroups.Count > 0) then
    Error(E, RS_INVALID_SPLITBUTTON)
//  else if (FControls.Count = 0) and (FMenuGroups.Count = 0) then
//    Error(E, RS_INVALID_SPLITBUTTON);
end;

procedure TRibbonSplitButton.CreateButtonItem;
begin
  FreeAndNil(FButtonItem);
  FButtonItem := TRibbonButton.Create(Owner);
end;

procedure TRibbonSplitButton.CreateToggleButtonItem;
begin
  FreeAndNil(FButtonItem);
  FButtonItem := TRibbonToggleButton.Create(Owner);
end;

function TRibbonSplitButton.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj.ObjectType = otMenuGroup) then
    Result := FMenuGroups.Remove(Obj as TRibbonMenuGroup)
  else if (Obj is TRibbonControl) then
    Result := FControls.Remove(TRibbonControl(Obj))
  else
    Result := inherited Delete(Obj);
end;

procedure TRibbonSplitButton.DeleteButtonItem;
begin
  FreeAndNil(FButtonItem);
end;

destructor TRibbonSplitButton.Destroy;
begin
  FMenuGroups.Free;
  FControls.Free;
  FButtonItem.Free;
  inherited;
end;

class function TRibbonSplitButton.ObjectType: TRibbonObjectType;
begin
  Result := otSplitButton;
end;

function TRibbonSplitButton.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonMenuGroup) then
    Result := FMenuGroups.Reorder(Child, Direction)
  else if (Child is TRibbonControl) then
    Result := FControls.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonSplitButton.Save(const Writer: TXmlWriter);
var
  Group: TRibbonMenuGroup;
  Control: TRibbonControl;
begin
  Writer.WriteStartElement(EN_SPLIT_BUTTON);
  inherited;

  if Assigned(FButtonItem) then
  begin
    Writer.WriteStartElement(EN_SPLIT_BUTTON_BUTTON_ITEM);
    FButtonItem.Save(Writer);
    Writer.WriteEndElement;
  end;

  if (FMenuGroups.Count > 0) then
  begin
    Writer.WriteStartElement(EN_SPLIT_BUTTON_MENU_GROUPS);
    for Group in FMenuGroups do
      Group.Save(Writer);
    Writer.WriteEndElement;
  end;

  for Control in FControls do
    Control.Save(Writer);

  Writer.WriteEndElement;
end;

class function TRibbonSplitButton.SupportApplicationModes: Boolean;
begin
  Result := True;
end;

{ TRibbonDropDownButton }

constructor TRibbonDropDownButton.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FControls := TRibbonList<TRibbonControl>.Create(Owner, True);
  FMenuGroups := TRibbonList<TRibbonMenuGroup>.Create(Owner, True);
end;

function TRibbonDropDownButton.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType = otMenuGroup) then
  begin
    if (FControls.Count > 0) then
      Error(nil, RS_CANNOT_ADD_MENU_GROUP_TO_DROP_DOWN_BUTTON);
    Result := TRibbonMenuGroup.Create(Owner);
    FMenuGroups.Add(TRibbonMenuGroup(Result));
  end
  else if (ObjType in [otToggleButton, otCheckBox, otButton, otSplitButton,
    otDropDownButton, otDropDownGallery, otSplitButtonGallery,
    otDropDownColorPicker]) then
  begin
    if (FMenuGroups.Count > 0) then
      Error(nil, RS_CANNOT_ADD_CONTROL_TO_DROP_DOWN_BUTTON);
    Result := Owner.CreateObject(ObjType);
    FControls.Add(Result as TRibbonControl);
  end
  else
    Result := inherited AddNew(ObjType);
end;

constructor TRibbonDropDownButton.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  C: TXmlElement;
begin
  inherited Create(Owner, E);
  FControls := TRibbonList<TRibbonControl>.Create(Owner, True);
  FMenuGroups := TRibbonList<TRibbonMenuGroup>.Create(Owner, True);
  for C in E do
  begin
    if (C.Name = EN_MENU_GROUP) then
      FMenuGroups.Add(TRibbonMenuGroup.Create(Owner, C))
    else if (C.Name = EN_TOGGLE_BUTTON) then
      FControls.Add(TRibbonToggleButton.Create(Owner, C))
    else if (C.Name = EN_CHECK_BOX) then
      FControls.Add(TRibbonCheckBox.Create(Owner, C))
    else if (C.Name = EN_BUTTON) then
      FControls.Add(TRibbonButton.Create(Owner, C))
    else if (C.Name = EN_SPLIT_BUTTON) then
      FControls.Add(TRibbonSplitButton.Create(Owner, C))
    else if (C.Name = EN_DROP_DOWN_BUTTON) then
      FControls.Add(TRibbonDropDownButton.Create(Owner, C))
    else if (C.Name = EN_DROP_DOWN_GALLERY) then
      FControls.Add(TRibbonDropDownGallery.Create(Owner, C))
    else if (C.Name = EN_SPLIT_BUTTON_GALLERY) then
      FControls.Add(TRibbonSplitButtonGallery.Create(Owner, C))
    else if (C.Name = EN_DROP_DOWN_COLOR_PICKER) then
      FControls.Add(TRibbonDropDownColorPicker.Create(Owner, C))
    else
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;

  { There must be either 0 or more controls or 1 menu group or more }
  if (FControls.Count > 0) and (FMenuGroups.Count > 0) then
    Error(E, RS_INVALID_DROPDOWN_BUTTON)
end;

function TRibbonDropDownButton.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj.ObjectType = otMenuGroup) then
    Result := FMenuGroups.Remove(Obj as TRibbonMenuGroup)
  else if (Obj is TRibbonControl) then
    Result := FControls.Remove(TRibbonControl(Obj))
  else
    Result := inherited Delete(Obj);
end;

destructor TRibbonDropDownButton.Destroy;
begin
  FControls.Free;
  FMenuGroups.Free;
  inherited;
end;

class function TRibbonDropDownButton.ObjectType: TRibbonObjectType;
begin
  Result := otDropDownButton;
end;

function TRibbonDropDownButton.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonMenuGroup) then
    Result := FMenuGroups.Reorder(Child, Direction)
  else if (Child is TRibbonControl) then
    Result := FControls.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonDropDownButton.Save(const Writer: TXmlWriter);
var
  Group: TRibbonMenuGroup;
  Control: TRibbonControl;
begin
  Writer.WriteStartElement(EN_DROP_DOWN_BUTTON);
  inherited;

  for Group in FMenuGroups do
    Group.Save(Writer);

  for Control in FControls do
    Control.Save(Writer);

  Writer.WriteEndElement;
end;

class function TRibbonDropDownButton.SupportApplicationModes: Boolean;
begin
  Result := True;
end;

{ TRibbonDropDownColorPicker }

constructor TRibbonDropDownColorPicker.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FColorTemplate := ctThemeColors;
  FChipSize := csSmall;
  FIsAutomaticColorButtonVisible := True;
  FIsNoColorButtonVisible := True;
end;

constructor TRibbonDropDownColorPicker.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  S: String;
begin
  inherited Create(Owner, E);
  S := E.AttributeAsString[AN_COLOR_TEMPLATE];
  if (S = '') or (S = ES_THEME_COLORS) then
    FColorTemplate := ctThemeColors
  else if (S = ES_STANDARD_COLORS) then
    FColorTemplate := ctStandardColors
  else if (S = ES_HIGHLIGHT_COLORS) then
    FColorTemplate := ctHighlightColors
  else
    Error(E, RS_INVALID_COLOR_TEMPLATE);

  S := E.AttributeAsString[AN_CHIP_SIZE];
  if (S = '') or (S = ES_SMALL) then
    FChipSize := csSmall
  else if (S = ES_MEDIUM) then
    FChipSize := csMedium
  else if (S = ES_LARGE) then
    FChipSize := csLarge
  else
    Error(E, RS_INVALID_CHIP_SIZE);

  FColumns := E.AttributeAsInteger[AN_COLUMNS];
  FThemeColorGridRows := E.AttributeAsInteger[AN_THEME_COLOR_GRID_ROWS];
  FStandardColorGridRows := E.AttributeAsInteger[AN_STANDARD_COLOR_GRID_ROWS];
  FRecentColorGridRows := E.AttributeAsInteger[AN_RECENT_COLOR_GRID_ROWS];
  FIsAutomaticColorButtonVisible := E.AttributeAsBooleanDef(AN_IS_AUTOMATIC_COLOR_BUTTON_VISIBLE, True);
  FIsNoColorButtonVisible := E.AttributeAsBooleanDef(AN_IS_NO_COLOR_BUTTON_VISIBLE, True);
end;

class function TRibbonDropDownColorPicker.ObjectType: TRibbonObjectType;
begin
  Result := otDropDownColorPicker;
end;

procedure TRibbonDropDownColorPicker.Save(const Writer: TXmlWriter);
const
  COLOR_TEMPLATES: array [TRibbonColorTemplate] of String = (
    ES_THEME_COLORS, ES_STANDARD_COLORS, ES_HIGHLIGHT_COLORS);
  CHIP_SIZES: array [TRibbonChipSize] of String = (
    ES_SMALL, ES_MEDIUM, ES_LARGE);
begin
  Writer.WriteStartElement(EN_DROP_DOWN_COLOR_PICKER);
  inherited;
  if (FColorTemplate <> ctThemeColors) then
    Writer.WriteAttribute(AN_COLOR_TEMPLATE, COLOR_TEMPLATES[FColorTemplate]);
  if (FChipSize <> csSmall) then
    Writer.WriteAttribute(AN_CHIP_SIZE, CHIP_SIZES[FChipSize]);
  if (FColumns <> 0) then
    Writer.WriteAttribute(AN_COLUMNS, FColumns);
  if (FThemeColorGridRows <> 0) then
    Writer.WriteAttribute(AN_THEME_COLOR_GRID_ROWS, FThemeColorGridRows);
  if (FStandardColorGridRows <> 0) then
    Writer.WriteAttribute(AN_STANDARD_COLOR_GRID_ROWS, FStandardColorGridRows);
  if (FRecentColorGridRows <> 0) then
    Writer.WriteAttribute(AN_RECENT_COLOR_GRID_ROWS, FRecentColorGridRows);
  if (not FIsAutomaticColorButtonVisible) then
    Writer.WriteAttribute(AN_IS_AUTOMATIC_COLOR_BUTTON_VISIBLE, FIsAutomaticColorButtonVisible);
  if (not FIsNoColorButtonVisible) then
    Writer.WriteAttribute(AN_IS_NO_COLOR_BUTTON_VISIBLE, FIsNoColorButtonVisible);
  Writer.WriteEndElement;
end;

class function TRibbonDropDownColorPicker.SupportApplicationModes: Boolean;
begin
  Result := False;
end;

{ TRibbonSpinner }

class function TRibbonSpinner.ObjectType: TRibbonObjectType;
begin
  Result := otSpinner;
end;

procedure TRibbonSpinner.Save(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_SPINNER);
  inherited;
  Writer.WriteEndElement;
end;

class function TRibbonSpinner.SupportApplicationModes: Boolean;
begin
  Result := False;
end;

{ TRibbonCheckBox }

class function TRibbonCheckBox.ObjectType: TRibbonObjectType;
begin
  Result := otCheckBox;
end;

procedure TRibbonCheckBox.Save(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_CHECK_BOX);
  inherited;
  Writer.WriteEndElement;
end;

class function TRibbonCheckBox.SupportApplicationModes: Boolean;
begin
  Result := False;
end;

{ TRibbonComboBox }

constructor TRibbonComboBox.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FResizeType := rtNoResize;
  FIsEditable := True;
  FIsAutoCompleteEnabled := True;
end;

constructor TRibbonComboBox.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  S: String;
begin
  inherited Create(Owner, E);
  S := E.AttributeAsString[AN_RESIZE_TYPE];
  if (S = '') or (S = ES_NO_RESIZE) then
    FResizeType := rtNoResize
  else if (S = ES_VERTICAL_RESIZE) then
    FResizeType := rtVerticalResize
  else
    Error(E, RS_INVALID_RESIZE_TYPE);
  FIsEditable := E.AttributeAsBooleanDef(AN_IS_EDITABLE, True);
  FIsAutoCompleteEnabled := E.AttributeAsBooleanDef(AN_IS_AUTO_COMPLETE_ENABLED, True);
end;

class function TRibbonComboBox.ObjectType: TRibbonObjectType;
begin
  Result := otComboBox;
end;

procedure TRibbonComboBox.Save(const Writer: TXmlWriter);
const
  RESIZE_TYPES: array [TRibbonComboBoxResizeType] of String = (
    ES_NO_RESIZE, ES_VERTICAL_RESIZE);
begin
  Writer.WriteStartElement(EN_COMBO_BOX);
  inherited;

  if (FResizeType <> rtNoResize) then
    Writer.WriteAttribute(AN_RESIZE_TYPE, RESIZE_TYPES[FResizeType]);

  if (not FIsEditable) then
    Writer.WriteAttribute(AN_IS_EDITABLE, FIsEditable);

  if (not FIsAutoCompleteEnabled) then
    Writer.WriteAttribute(AN_IS_AUTO_COMPLETE_ENABLED, FIsAutoCompleteEnabled);

  Writer.WriteEndElement;
end;

class function TRibbonComboBox.SupportApplicationModes: Boolean;
begin
  Result := False;
end;

{ TRibbonQatControl }

constructor TRibbonQatControl.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FIsChecked := True;
end;

constructor TRibbonQatControl.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
begin
  inherited Create(Owner, E);
  FIsChecked := E.AttributeAsBooleanDef(AN_APPLICATION_DEFAULTS_IS_CHECKED, True);
end;

procedure TRibbonQatControl.Save(const Writer: TXmlWriter);
begin
  inherited;
  Writer.WriteAttribute(AN_APPLICATION_DEFAULTS_IS_CHECKED, FIsChecked);
end;

class function TRibbonQatControl.SupportApplicationModes: Boolean;
begin
  Result := False;
end;

{ TRibbonQatButton }

class function TRibbonQatButton.ObjectType: TRibbonObjectType;
begin
  Result := otQatButton;
end;

procedure TRibbonQatButton.Save(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_BUTTON);
  inherited;
  Writer.WriteEndElement;
end;

{ TRibbonQatToggleButton }

class function TRibbonQatToggleButton.ObjectType: TRibbonObjectType;
begin
  Result := otQatToggleButton;
end;

procedure TRibbonQatToggleButton.Save(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_TOGGLE_BUTTON);
  inherited;
  Writer.WriteEndElement;
end;

{ TRibbonQatCheckBox }

class function TRibbonQatCheckBox.ObjectType: TRibbonObjectType;
begin
  Result := otQatCheckBox;
end;

procedure TRibbonQatCheckBox.Save(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_CHECK_BOX);
  inherited;
  Writer.WriteEndElement;
end;

{ TRibbonHelpButton }

function TRibbonHelpButton.CanReorder: Boolean;
begin
  Result := False;
end;

class function TRibbonHelpButton.ObjectType: TRibbonObjectType;
begin
  Result := otHelpButton;
end;

procedure TRibbonHelpButton.Save(const Writer: TXmlWriter);
begin
  if (CommandRef = nil) then
    Exit;

  Writer.WriteStartElement(EN_RIBBON_HELP_BUTTON);
    Writer.WriteStartElement(EN_HELP_BUTTON);
    inherited;
    Writer.WriteEndElement;
  Writer.WriteEndElement;
end;

class function TRibbonHelpButton.SupportApplicationModes: Boolean;
begin
  Result := False;
end;

{ TRibbonFloatieFontControl }

constructor TRibbonFloatieFontControl.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FShowVerticalFonts := True;
  FMinimumFontSize := 1;
  FMaximumFontSize := 9999;
end;

constructor TRibbonFloatieFontControl.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
begin
  inherited Create(Owner, E);
  FShowTrueTypeOnly := E.AttributeAsBooleanDef(AN_SHOW_TRUE_TYPE_ONLY, False);
  FShowVerticalFonts := E.AttributeAsBooleanDef(AN_SHOW_VERTICAL_FONTS, True);
  FMinimumFontSize := E.AttributeAsIntegerDef(AN_MINIMUM_FONT_SIZE, 1);
  FMaximumFontSize := E.AttributeAsIntegerDef(AN_MAXIMUM_FONT_SIZE, 9999);
  FMinimumFontSize := EnsureRange(FMinimumFontSize, 1, 9999);
  FMaximumFontSize := EnsureRange(FMaximumFontSize, 1, 9999);
end;

class function TRibbonFloatieFontControl.ObjectType: TRibbonObjectType;
begin
  Result := otFloatieFontControl;
end;

procedure TRibbonFloatieFontControl.Save(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_FONT_CONTROL);
  inherited;
  SaveAttributes(Writer);
  Writer.WriteEndElement;
end;

procedure TRibbonFloatieFontControl.SaveAttributes(const Writer: TXmlWriter);
begin
  if (FShowTrueTypeOnly) then
    Writer.WriteAttribute(AN_SHOW_TRUE_TYPE_ONLY, FShowTrueTypeOnly);
  if (not FShowVerticalFonts) then
    Writer.WriteAttribute(AN_SHOW_VERTICAL_FONTS, FShowVerticalFonts);
  if (FMinimumFontSize <> 1) then
    Writer.WriteAttribute(AN_MINIMUM_FONT_SIZE, FMinimumFontSize);
  if (FMaximumFontSize <> 9999) then
    Writer.WriteAttribute(AN_MAXIMUM_FONT_SIZE, FMaximumFontSize);
end;

procedure TRibbonFloatieFontControl.SetMaximumFontSize(const Value: Integer);
begin
  FMaximumFontSize := EnsureRange(Value, 1, 9999);
end;

procedure TRibbonFloatieFontControl.SetMinimumFontSize(const Value: Integer);
begin
  FMinimumFontSize := EnsureRange(Value, 1, 9999);
end;

class function TRibbonFloatieFontControl.SupportApplicationModes: Boolean;
begin
  Result := False;
end;

{ TRibbonFontControl }

constructor TRibbonFontControl.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FFontType := ftFontOnly;
  FIsStrikethroughButtonVisible := True;
  FIsUnderlineButtonVisible := True;
end;

constructor TRibbonFontControl.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  S: String;
begin
  inherited Create(Owner, E);
  S := E.AttributeAsString[AN_FONT_TYPE];
  if (S = ES_FONT_ONLY) or (S = '') then
    FFontType := ftFontOnly
  else if (S = ES_FONT_WITH_COLOR) then
    FFontType := ftFontWithColor
  else if (S = ES_RICH_FONT) then
    FFontType := ftRichFont
  else
    Error(E, RS_INVALID_FONT_TYPE);

  FIsStrikethroughButtonVisible := E.AttributeAsBooleanDef(AN_IS_STRIKETHROUGH_BUTTON_VISIBLE, True);
  FIsUnderlineButtonVisible := E.AttributeAsBooleanDef(AN_IS_UNDERLINE_BUTTON_VISIBLE, True);
  FIsHighlightButtonVisible := E.AttributeAsBooleanDef(AN_IS_HIGHLIGHT_BUTTON_VISIBLE, (FFontType <> ftFontOnly));
end;

class function TRibbonFontControl.ObjectType: TRibbonObjectType;
begin
  Result := otFontControl;
end;

procedure TRibbonFontControl.SaveAttributes(const Writer: TXmlWriter);
const
  FONT_TYPES: array [TRibbonFontType] of String = (
    ES_FONT_ONLY, ES_FONT_WITH_COLOR, ES_RICH_FONT);
begin
  if (FFontType <> ftFontOnly) then
    Writer.WriteAttribute(AN_FONT_TYPE, FONT_TYPES[FFontType]);
  if (FFontType <> ftRichFont) then
  begin
    if (not FIsStrikethroughButtonVisible) then
      Writer.WriteAttribute(AN_IS_STRIKETHROUGH_BUTTON_VISIBLE, FIsStrikethroughButtonVisible);
    if (not FIsUnderlineButtonVisible) then
      Writer.WriteAttribute(AN_IS_UNDERLINE_BUTTON_VISIBLE, FIsUnderlineButtonVisible);
    if (FFontType <> ftFontOnly) and (FIsHighlightButtonVisible) then
      Writer.WriteAttribute(AN_IS_HIGHLIGHT_BUTTON_VISIBLE, FIsHighlightButtonVisible);
  end;
  inherited;
end;

{ TRibbonControlGroup }

constructor TRibbonControlGroup.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FControls := TRibbonList<TRibbonControl>.Create(Owner, True);
end;

function TRibbonControlGroup.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType in [otControlGroup, otToggleButton, otCheckBox, otButton,
    otSplitButton, otDropDownButton, otDropDownGallery, otSplitButtonGallery,
    otDropDownColorPicker, otComboBox, otSpinner, otInRibbonGallery,
    otFontControl]) then
  begin
    Result := Owner.CreateObject(ObjType);
    FControls.Add(Result as TRibbonControl);
  end
  else
    Result := inherited AddNew(ObjType);
end;

constructor TRibbonControlGroup.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  C: TXmlElement;
begin
  inherited Create(Owner, E);
  FControls := TRibbonList<TRibbonControl>.Create(Owner, True);
  FSequenceNumber := E.AttributeAsInteger[AN_SEQUENCE_NUMBER];
  for C in E do
  begin
    if (C.Name = EN_CONTROL_GROUP) then
      FControls.Add(TRibbonControlGroup.Create(Owner, C))
    else if (C.Name = EN_TOGGLE_BUTTON) then
      FControls.Add(TRibbonToggleButton.Create(Owner, C))
    else if (C.Name = EN_CHECK_BOX) then
      FControls.Add(TRibbonCheckBox.Create(Owner, C))
    else if (C.Name = EN_BUTTON) then
      FControls.Add(TRibbonButton.Create(Owner, C))
    else if (C.Name = EN_SPLIT_BUTTON) then
      FControls.Add(TRibbonSplitButton.Create(Owner, C))
    else if (C.Name = EN_DROP_DOWN_BUTTON) then
      FControls.Add(TRibbonDropDownButton.Create(Owner, C))
    else if (C.Name = EN_DROP_DOWN_GALLERY) then
      FControls.Add(TRibbonDropDownGallery.Create(Owner, C))
    else if (C.Name = EN_SPLIT_BUTTON_GALLERY) then
      FControls.Add(TRibbonSplitButtonGallery.Create(Owner, C))
    else if (C.Name = EN_DROP_DOWN_COLOR_PICKER) then
      FControls.Add(TRibbonDropDownColorPicker.Create(Owner, C))
    else if (C.Name = EN_COMBO_BOX) then
      FControls.Add(TRibbonComboBox.Create(Owner, C))
    else if (C.Name = EN_SPINNER) then
      FControls.Add(TRibbonSpinner.Create(Owner, C))
    else if (C.Name = EN_IN_RIBBON_GALLERY) then
      FControls.Add(TRibbonInRibbonGallery.Create(Owner, C))
    else if (C.Name = EN_FONT_CONTROL) then
      FControls.Add(TRibbonFontControl.Create(Owner, C))
    else
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;
end;

function TRibbonControlGroup.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj is TRibbonControl) then
    Result := FControls.Remove(TRibbonControl(Obj))
  else
    Result := inherited Delete(Obj);
end;

destructor TRibbonControlGroup.Destroy;
begin
  FControls.Free;
  inherited;
end;

function TRibbonControlGroup.DisplayName: String;
begin
  Result := RS_CONTROL_GROUP;
end;

class function TRibbonControlGroup.ObjectType: TRibbonObjectType;
begin
  Result := otControlGroup;
end;

function TRibbonControlGroup.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonControl) then
    Result := FControls.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonControlGroup.Save(const Writer: TXmlWriter);
var
  Control: TRibbonControl;
begin
  Writer.WriteStartElement(EN_CONTROL_GROUP);
  { NO inherited }

  if (FSequenceNumber <> 0) then
    Writer.WriteAttribute(AN_SEQUENCE_NUMBER, FSequenceNumber);

  for Control in FControls do
    Control.Save(Writer);

  Writer.WriteEndElement;
end;

class function TRibbonControlGroup.SupportApplicationModes: Boolean;
begin
  Result := False;
end;

{ TRibbonGalleryMenuLayout }

constructor TRibbonGalleryMenuLayout.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FRows := -1;
end;

constructor TRibbonGalleryMenuLayout.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
begin
  inherited Create(Owner);
  FRows := E.AttributeAsIntegerDef(AN_ROWS, -1);
end;

procedure TRibbonGalleryMenuLayout.Save(const Writer: TXmlWriter);
begin
  if (FRows <> -1) then
    Writer.WriteAttribute(AN_ROWS, FRows);
end;

{ TRibbonVerticalMenuLayout }

constructor TRibbonVerticalMenuLayout.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FGripper := sgVertical;
end;

constructor TRibbonVerticalMenuLayout.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  S: String;
begin
  inherited Create(Owner, E);
  S := E.AttributeAsString[AN_GRIPPER];
  if (S = '') or (S = ES_VERTICAL) then
    FGripper := sgVertical
  else if (S = ES_NONE) then
    FGripper := sgNone
  else
    Error(E, RS_INVALID_GRIPPER);
end;

class function TRibbonVerticalMenuLayout.ObjectType: TRibbonObjectType;
begin
  Result := otVerticalMenuLayout;
end;

procedure TRibbonVerticalMenuLayout.Save(const Writer: TXmlWriter);
const
  GRIPPERS: array [TRibbonSingleColumnGripperType] of String = (
    ES_NONE, ES_VERTICAL);
begin
  Writer.WriteStartElement(EN_VERTICAL_MENU_LAYOUT);
  inherited;

  if (FGripper <> sgVertical) then
    Writer.WriteAttribute(AN_GRIPPER, GRIPPERS[FGripper]);

  Writer.WriteEndElement;
end;

{ TRibbonFlowMenuLayout }

constructor TRibbonFlowMenuLayout.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FGripper := mgCorner;
  FColumns := 2;
end;

constructor TRibbonFlowMenuLayout.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  S: String;
begin
  inherited Create(Owner, E);
  S := E.AttributeAsString[AN_GRIPPER];
  if (S = '') or (S = ES_CORNER) then
    FGripper := mgCorner
  else if (S = ES_VERTICAL) then
    FGripper := mgVertical
  else if (S = ES_NONE) then
    FGripper := mgNone
  else
    Error(E, RS_INVALID_GRIPPER);

  FColumns := E.AttributeAsIntegerDef(AN_COLUMNS, 2);
end;

class function TRibbonFlowMenuLayout.ObjectType: TRibbonObjectType;
begin
  Result := otFlowMenuLayout;
end;

procedure TRibbonFlowMenuLayout.Save(const Writer: TXmlWriter);
const
  GRIPPERS: array [TRibbonMultiColumnGripperType] of String = (
    ES_NONE, ES_VERTICAL, ES_CORNER);
begin
  Writer.WriteStartElement(EN_FLOW_MENU_LAYOUT);
  inherited;

  if (FGripper <> mgCorner) then
    Writer.WriteAttribute(AN_GRIPPER, GRIPPERS[FGripper]);

  if (FColumns <> 2) then
    Writer.WriteAttribute(AN_COLUMNS, FColumns);

  Writer.WriteEndElement;
end;

{ TRibbonGallery }

constructor TRibbonGallery.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FMenuGroups := TRibbonList<TRibbonMenuGroup>.Create(Owner, True);
  FControls := TRibbonList<TRibbonControl>.Create(Owner, True);
  FGalleryType := gtItems;
  FHasLargeItems := True;
  FItemHeight := -1;
  FItemWidth := -1;
  FTextPosition := tpBottom;
end;

constructor TRibbonGallery.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  S: String;
  C: TXmlElement;
begin
  inherited Create(Owner, E);
  FMenuGroups := TRibbonList<TRibbonMenuGroup>.Create(Owner, True);
  FControls := TRibbonList<TRibbonControl>.Create(Owner, True);
  S := E.AttributeAsString[AN_TYPE];
  if (S = '') or (S = ES_ITEMS) then
    FGalleryType := gtItems
  else if (S = ES_COMMANDS) then
    FGalleryType := gtCommands
  else
    Error(E, RS_INVALID_TYPE);

  FHasLargeItems := E.AttributeAsBooleanDef(AN_HAS_LARGE_ITEMS, True);
  FItemHeight := E.AttributeAsIntegerDef(AN_ITEM_HEIGHT, -1);
  FItemWidth := E.AttributeAsIntegerDef(AN_ITEM_WIDTH, -1);

  S := E.AttributeAsString[AN_TEXT_POSITION];
  if (S = '') or (S = ES_BOTTOM) then
    FTextPosition := tpBottom
  else if (S = ES_HIDE) then
    FTextPosition := tpHide
  else if (S = ES_LEFT) then
    FTextPosition := tpLeft
  else if (S = ES_OVERLAP) then
    FTextPosition := tpOverlap
  else if (S = ES_RIGHT) then
    FTextPosition := tpRight
  else if (S = ES_TOP) then
    FTextPosition := tpTop
  else
    Error(E, RS_INVALID_TEXT_POSITION);

  for C in E do
    if (not HandleElement(C)) then
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);

  { There must be either 1 or more controls, or a menu group }
  if (FControls.Count > 0) and (FMenuGroups.Count > 0) then
    Error(E, RS_INVALID_GALLERY)
//  else if (FControls.Count = 0) and (FMenuGroups.Count = 0) then
//    Error(E, RS_INVALID_GALLERY);
end;

procedure TRibbonGallery.CreateFlowMenuLayout;
begin
  FreeAndNil(FMenuLayout);
  FMenuLayout := TRibbonFlowMenuLayout.Create(Owner);
end;

procedure TRibbonGallery.CreateVerticalMenuLayout;
begin
  FreeAndNil(FMenuLayout);
  FMenuLayout := TRibbonVerticalMenuLayout.Create(Owner);
end;

procedure TRibbonGallery.DeleteMenuLayout;
begin
  FreeAndNil(FMenuLayout);
end;

destructor TRibbonGallery.Destroy;
begin
  FMenuGroups.Free;
  FControls.Free;
  FMenuLayout.Free;
  inherited;
end;

function TRibbonGallery.HandleElement(const E: TXmlElement): Boolean;
begin
  Result := True;
  if (E.Name = EN_TOGGLE_BUTTON) then
    FControls.Add(TRibbonToggleButton.Create(Owner, E))
  else if (E.Name = EN_CHECK_BOX) then
    FControls.Add(TRibbonCheckBox.Create(Owner, E))
  else if (E.Name = EN_BUTTON) then
    FControls.Add(TRibbonButton.Create(Owner, E))
  else if (E.Name = EN_SPLIT_BUTTON) then
    FControls.Add(TRibbonSplitButton.Create(Owner, E))
  else if (E.Name = EN_DROP_DOWN_BUTTON) then
    FControls.Add(TRibbonDropDownButton.Create(Owner, E))
  else if (E.Name = EN_DROP_DOWN_GALLERY) then
    FControls.Add(TRibbonDropDownGallery.Create(Owner, E))
  else if (E.Name = EN_SPLIT_BUTTON_GALLERY) then
    FControls.Add(TRibbonSplitButtonGallery.Create(Owner, E))
  else if (E.Name = EN_DROP_DOWN_COLOR_PICKER) then
    FControls.Add(TRibbonDropDownColorPicker.Create(Owner, E))
  else
    Result := False;
end;

procedure TRibbonGallery.LoadMenuGroups(const E: TXmlElement);
var
  C: TXmlElement;
begin
  if (E.ChildCount = 0) then
    Error(E, RS_REQUIRED_ELEMENT, [E.Name, EN_MENU_GROUP]);

  for C in E do
    FMenuGroups.Add(TRibbonMenuGroup.Create(Owner, C));
end;

procedure TRibbonGallery.LoadMenuLayout(const E: TXmlElement);
var
  C: TXmlElement;
begin
  if (E.ChildCount <> 1) then
    Error(E, RS_SINGLE_ELEMENT, [E.Name, EN_VERTICAL_MENU_LAYOUT + '/' + EN_FLOW_MENU_LAYOUT]);

  C := E[0];
  if (C.Name = EN_VERTICAL_MENU_LAYOUT) then
    FMenuLayout := TRibbonVerticalMenuLayout.Create(Owner, C)
  else if (C.Name = EN_FLOW_MENU_LAYOUT) then
    FMenuLayout := TRibbonFlowMenuLayout.Create(Owner, C)
  else
    Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
end;

procedure TRibbonGallery.Save(const Writer: TXmlWriter);
var
  Control: TRibbonControl;
begin
  inherited;
  SaveAttributes(Writer);
  if Assigned(FMenuLayout) then
    SaveMenuLayout(Writer);
  if (FMenuGroups.Count > 0) then
    SaveMenuGroups(Writer);
  for Control in FControls do
    Control.Save(Writer);
end;

procedure TRibbonGallery.SaveAttributes(const Writer: TXmlWriter);
const
  GALLERY_TYPES: array [TRibbonGalleryType] of String = (
    ES_ITEMS, ES_COMMANDS);
  TEXT_POSITIONS: array [TRibbonTextPosition] of String = (
    ES_BOTTOM, ES_HIDE, ES_LEFT, ES_OVERLAP, ES_RIGHT, ES_TOP);
begin
  if (FGalleryType <> gtItems) then
    Writer.WriteAttribute(AN_TYPE, GALLERY_TYPES[FGalleryType]);

  if (not FHasLargeItems) then
    Writer.WriteAttribute(AN_HAS_LARGE_ITEMS, FHasLargeItems);

  if (FItemHeight <> -1) then
    Writer.WriteAttribute(AN_ITEM_HEIGHT, FItemHeight);

  if (FItemWidth <> -1) then
    Writer.WriteAttribute(AN_ITEM_WIDTH, FItemWidth);

  if (FTextPosition <> tpBottom) then
    Writer.WriteAttribute(AN_TEXT_POSITION, TEXT_POSITIONS[FTextPosition]);
end;

{ TRibbonDropDownGallery }

function TRibbonDropDownGallery.HandleElement(const E: TXmlElement): Boolean;
begin
  Result := True;
  if (E.Name = EN_DROP_DOWN_GALLERY_MENU_LAYOUT) then
    LoadMenuLayout(E)
  else if (E.Name = EN_DROP_DOWN_GALLERY_MENU_GROUPS) then
    LoadMenuGroups(E)
  else
    Result := inherited HandleElement(E);
end;

class function TRibbonDropDownGallery.ObjectType: TRibbonObjectType;
begin
  Result := otDropDownGallery;
end;

procedure TRibbonDropDownGallery.Save(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_DROP_DOWN_GALLERY);
  inherited;
  Writer.WriteEndElement;
end;

procedure TRibbonDropDownGallery.SaveMenuGroups(const Writer: TXmlWriter);
var
  Group: TRibbonMenuGroup;
begin
  Writer.WriteStartElement(EN_DROP_DOWN_GALLERY_MENU_GROUPS);
  for Group in MenuGroups do
    Group.Save(Writer);
  Writer.WriteEndElement;
end;

procedure TRibbonDropDownGallery.SaveMenuLayout(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_DROP_DOWN_GALLERY_MENU_LAYOUT);
  MenuLayout.Save(Writer);
  Writer.WriteEndElement;
end;

class function TRibbonDropDownGallery.SupportApplicationModes: Boolean;
begin
  Result := True;
end;

{ TRibbonSplitButtonGallery }

function TRibbonSplitButtonGallery.HandleElement(const E: TXmlElement): Boolean;
begin
  Result := True;
  if (E.Name = EN_SPLIT_BUTTON_GALLERY_MENU_LAYOUT) then
    LoadMenuLayout(E)
  else if (E.Name = EN_SPLIT_BUTTON_GALLERY_MENU_GROUPS) then
    LoadMenuGroups(E)
  else
    Result := inherited HandleElement(E);
end;

class function TRibbonSplitButtonGallery.ObjectType: TRibbonObjectType;
begin
  Result := otSplitButtonGallery;
end;

procedure TRibbonSplitButtonGallery.Save(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_SPLIT_BUTTON_GALLERY);
  inherited;
  Writer.WriteEndElement;
end;

procedure TRibbonSplitButtonGallery.SaveMenuGroups(const Writer: TXmlWriter);
var
  Group: TRibbonMenuGroup;
begin
  Writer.WriteStartElement(EN_SPLIT_BUTTON_GALLERY_MENU_GROUPS);
  for Group in MenuGroups do
    Group.Save(Writer);
  Writer.WriteEndElement;
end;

procedure TRibbonSplitButtonGallery.SaveMenuLayout(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_SPLIT_BUTTON_GALLERY_MENU_LAYOUT);
  MenuLayout.Save(Writer);
  Writer.WriteEndElement;
end;

class function TRibbonSplitButtonGallery.SupportApplicationModes: Boolean;
begin
  Result := True;
end;

{ TRibbonInRibbonGallery }

constructor TRibbonInRibbonGallery.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FMinColumnsLarge := -1;
  FMaxColumnsMedium := -1;
  FMinColumnsMedium := -1;
  FMaxColumns := -1;
  FMaxRows := -1;
end;

constructor TRibbonInRibbonGallery.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
begin
  inherited Create(Owner, E);
  FMinColumnsLarge := E.AttributeAsIntegerDef(AN_MIN_COLUMNS_LARGE, -1);
  FMaxColumnsMedium := E.AttributeAsIntegerDef(AN_MAX_COLUMNS_MEDIUM, -1);
  FMinColumnsMedium := E.AttributeAsIntegerDef(AN_MIN_COLUMNS_MEDIUM, -1);
  FMaxColumns := E.AttributeAsIntegerDef(AN_MAX_COLUMNS, -1);
  FMaxRows := E.AttributeAsIntegerDef(AN_MAX_ROWS, -1);
end;

function TRibbonInRibbonGallery.HandleElement(const E: TXmlElement): Boolean;
begin
  Result := True;
  if (E.Name = EN_IN_RIBBON_GALLERY_MENU_LAYOUT) then
    LoadMenuLayout(E)
  else if (E.Name = EN_IN_RIBBON_GALLERY_MENU_GROUPS) then
    LoadMenuGroups(E)
  else
    Result := inherited HandleElement(E);
end;

class function TRibbonInRibbonGallery.ObjectType: TRibbonObjectType;
begin
  Result := otInRibbonGallery;
end;

procedure TRibbonInRibbonGallery.Save(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_IN_RIBBON_GALLERY);
  inherited;
  Writer.WriteEndElement;
end;

procedure TRibbonInRibbonGallery.SaveAttributes(const Writer: TXmlWriter);
begin
  inherited;
  if (FMinColumnsLarge <> -1) then
    Writer.WriteAttribute(AN_MIN_COLUMNS_LARGE, FMinColumnsLarge);
  if (FMaxColumnsMedium <> -1) then
    Writer.WriteAttribute(AN_MAX_COLUMNS_MEDIUM, FMaxColumnsMedium);
  if (FMinColumnsMedium <> -1) then
    Writer.WriteAttribute(AN_MIN_COLUMNS_MEDIUM, FMinColumnsMedium);
  if (FMaxColumns <> -1) then
    Writer.WriteAttribute(AN_MAX_COLUMNS, FMaxColumns);
  if (FMaxRows <> -1) then
    Writer.WriteAttribute(AN_MAX_ROWS, FMaxRows);
end;

procedure TRibbonInRibbonGallery.SaveMenuGroups(const Writer: TXmlWriter);
var
  Group: TRibbonMenuGroup;
begin
  Writer.WriteStartElement(EN_IN_RIBBON_GALLERY_MENU_GROUPS);
  for Group in MenuGroups do
    Group.Save(Writer);
  Writer.WriteEndElement;
end;

procedure TRibbonInRibbonGallery.SaveMenuLayout(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_IN_RIBBON_GALLERY_MENU_LAYOUT);
  MenuLayout.Save(Writer);
  Writer.WriteEndElement;
end;

class function TRibbonInRibbonGallery.SupportApplicationModes: Boolean;
begin
  Result := False;
end;

{ TRibbonAppMenuGroup }

function TRibbonAppMenuGroup.AddControl(const E: TXmlElement): Boolean;
begin
  Result := True;
  if (E.Name = EN_BUTTON) then
    FControls.Add(TRibbonButton.Create(Owner, E))
  else if (E.Name = EN_SPLIT_BUTTON) then
    FControls.Add(TRibbonSplitButton.Create(Owner, E))
  else if (E.Name = EN_DROP_DOWN_BUTTON) then
    FControls.Add(TRibbonDropDownButton.Create(Owner, E))
  else if (E.Name = EN_DROP_DOWN_GALLERY) then
    FControls.Add(TRibbonDropDownGallery.Create(Owner, E))
  else if (E.Name = EN_SPLIT_BUTTON_GALLERY) then
    FControls.Add(TRibbonSplitButtonGallery.Create(Owner, E))
  else
    Result := False;
end;

constructor TRibbonAppMenuGroup.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FControls := TRibbonList<TRibbonControl>.Create(Owner, True);
  FCategoryClass := ccStandardItems;
end;

function TRibbonAppMenuGroup.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType in [otButton, otSplitButton, otDropDownButton, otDropDownGallery,
    otSplitButtonGallery]) then
  begin
    Result := Owner.CreateObject(ObjType);
    FControls.Add(Result as TRibbonControl);
  end
  else
    Result := inherited AddNew(ObjType);
end;

function TRibbonAppMenuGroup.CanReorder: Boolean;
begin
  Result := True;
end;

constructor TRibbonAppMenuGroup.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  S: String;
  C: TXmlElement;
begin
  inherited Create(Owner, E);
  FControls := TRibbonList<TRibbonControl>.Create(Owner, True);
  S := E.AttributeAsString[AN_CLASS];
  if (S = '') or (S = ES_STANDARD_ITEMS) then
    FCategoryClass := ccStandardItems
  else if (S = ES_MAJOR_ITEMS) then
    FCategoryClass := ccMajorItems
  else
    Error(E, RS_INVALID_CATEGORY_CLASS);

  for C in E do
  begin
    if (not AddControl(C)) then
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;
end;

function TRibbonAppMenuGroup.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj is TRibbonControl) then
    Result := FControls.Remove(TRibbonControl(Obj))
  else
    Result := inherited Delete(Obj);
end;

destructor TRibbonAppMenuGroup.Destroy;
begin
  FControls.Free;
  inherited;
end;

class function TRibbonAppMenuGroup.ObjectType: TRibbonObjectType;
begin
  Result := otAppMenuGroup;
end;

function TRibbonAppMenuGroup.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonControl) then
    Result := FControls.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonAppMenuGroup.Save(const Writer: TXmlWriter);
const
  CLASSES: array [TRibbonMenuCategoryClass] of String = (
    ES_STANDARD_ITEMS, ES_MAJOR_ITEMS);
var
  Control: TRibbonControl;
begin
  Writer.WriteStartElement(EN_MENU_GROUP);

  inherited;

  if (FCategoryClass <> ccStandardItems) then
    Writer.WriteAttribute(AN_CLASS, CLASSES[FCategoryClass]);

  for Control in FControls do
    Control.Save(Writer);

  Writer.WriteEndElement;
end;

{ TRibbonMenuGroup }

function TRibbonMenuGroup.AddControl(const E: TXmlElement): Boolean;
begin
  Result := True;
  if (E.Name = EN_TOGGLE_BUTTON) then
    Controls.Add(TRibbonToggleButton.Create(Owner, E))
  else if (E.Name = EN_CHECK_BOX) then
    Controls.Add(TRibbonCheckBox.Create(Owner, E))
  else if (E.Name = EN_DROP_DOWN_COLOR_PICKER) then
    Controls.Add(TRibbonDropDownColorPicker.Create(Owner, E))
  else
    Result := inherited AddControl(E);
end;

function TRibbonMenuGroup.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType in [otToggleButton, otCheckBox, otDropDownColorPicker]) then
  begin
    Result := Owner.CreateObject(ObjType);
    Controls.Add(Result as TRibbonControl);
  end
  else
    Result := inherited AddNew(ObjType);
end;

class function TRibbonMenuGroup.ObjectType: TRibbonObjectType;
begin
  Result := otMenuGroup;
end;

{ TRibbonMiniToolbarMenuGroup }

function TRibbonMiniToolbarMenuGroup.AddControl(const E: TXmlElement): Boolean;
begin
  Result := True;
  if (E.Name = EN_COMBO_BOX) then
    Controls.Add(TRibbonComboBox.Create(Owner, E))
  else if (E.Name = EN_SPINNER) then
    Controls.Add(TRibbonSpinner.Create(Owner, E))
  else if (E.Name = EN_FONT_CONTROL) then
    Controls.Add(TRibbonFloatieFontControl.Create(Owner, E))
  else
    Result := inherited AddControl(E);
end;

function TRibbonMiniToolbarMenuGroup.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType in [otComboBox, otSpinner, otFloatieFontControl]) then
  begin
    Result := Owner.CreateObject(ObjType);
    Controls.Add(Result as TRibbonControl);
  end
  else
    Result := inherited AddNew(ObjType);
end;

class function TRibbonMiniToolbarMenuGroup.ObjectType: TRibbonObjectType;
begin
  Result := otMiniToolbarMenuGroup;
end;

{ TRibbonApplicationMenu }

constructor TRibbonApplicationMenu.Create(const Owner: TRibbonDocument);
begin
  inherited;
  FMenuGroups := TRibbonList<TRibbonAppMenuGroup>.Create(Owner, True);
end;

function TRibbonApplicationMenu.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType = otAppMenuGroup) then
  begin
    Result := TRibbonAppMenuGroup.Create(Owner);
    FMenuGroups.Add(TRibbonAppMenuGroup(Result));
  end
  else
    Result := inherited AddNew(ObjType);
end;

function TRibbonApplicationMenu.CanReorder: Boolean;
begin
  Result := False;
end;

constructor TRibbonApplicationMenu.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  C, GC: TXmlElement;
begin
  if (E.ChildCount > 0) then
    inherited Create(Owner, E[0])
  else
    inherited Create(Owner, E);

  FMenuGroups := TRibbonList<TRibbonAppMenuGroup>.Create(Owner, True);
  if (E.ChildCount > 0) then
  begin
    C := E[0];
    if (C.Name <> EN_APPLICATION_MENU) then
      Error(C, RS_ELEMENT_EXPECTED, [EN_APPLICATION_MENU, C.Name]);

    if (E.ChildCount > 1) then
      Error(E, RS_MULTIPLE_ELEMENTS, [E.Name, C.Name]);

    for GC in C do
    begin
      if (GC.Name = EN_APPLICATION_MENU_RECENT_ITEMS) then
      begin
        if Assigned(FRecentItems) then
          Error(GC, RS_MULTIPLE_ELEMENTS, [C.Name, GC.Name]);
        FRecentItems := TRibbonApplicationMenuRecentItems.Create(Owner, GC);
      end
      else if (GC.Name = EN_MENU_GROUP) then
        FMenuGroups.Add(TRibbonAppMenuGroup.Create(Owner, GC))
      else
        Error(GC, RS_UNSUPPORTED_CHILD_ELEMENT, [GC.Name, C.Name]);
    end;
  end;
end;

function TRibbonApplicationMenu.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj.ObjectType = otAppMenuGroup) then
    Result := FMenuGroups.Remove(TRibbonAppMenuGroup(Obj))
  else
    Result := inherited Delete(Obj);
end;

destructor TRibbonApplicationMenu.Destroy;
begin
  FMenuGroups.Free;
  FRecentItems.Free;
  inherited;
end;

procedure TRibbonApplicationMenu.EnableRecentItems(const Enable: Boolean);
begin
  if (Enable) then
  begin
    if (FRecentItems = nil) then
      FRecentItems := TRibbonApplicationMenuRecentItems.Create(Owner);
  end
  else
    FreeAndNil(FRecentItems);
end;

class function TRibbonApplicationMenu.ObjectType: TRibbonObjectType;
begin
  Result := otApplicationMenu;
end;

function TRibbonApplicationMenu.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonAppMenuGroup) then
    Result := FMenuGroups.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonApplicationMenu.Save(const Writer: TXmlWriter);
var
  Group: TRibbonAppMenuGroup;
begin
  Writer.WriteStartElement(EN_RIBBON_APPLICATION_MENU);
    Writer.WriteStartElement(EN_APPLICATION_MENU);
      inherited;

      if Assigned(FRecentItems) then
        FRecentItems.Save(Writer);

      for Group in FMenuGroups do
        Group.Save(Writer);

    Writer.WriteEndElement;
  Writer.WriteEndElement;
end;

class function TRibbonApplicationMenu.SupportApplicationModes: Boolean;
begin
  Result := False;
end;

{ TRibbonQuickAccessToolbar }

constructor TRibbonQuickAccessToolbar.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FControls := TRibbonList<TRibbonQatControl>.Create(Owner, True);
end;

function TRibbonQuickAccessToolbar.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType in [otQatButton, otQatCheckBox, otQatToggleButton]) then
  begin
    Result := Owner.CreateObject(ObjType);
    FControls.Add(Result as TRibbonQatControl);
  end
  else
    Result := inherited AddNew(ObjType);
end;

function TRibbonQuickAccessToolbar.CanReorder: Boolean;
begin
  Result := False;
end;

constructor TRibbonQuickAccessToolbar.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  Name: TRibbonCommandName;
  C, GC: TXmlElement;
begin
  inherited Create(Owner, E);
  FControls := TRibbonList<TRibbonQatControl>.Create(Owner, True);
  Name := StringToCommandName(E.AttributeAsString[AN_CUSTOMIZE_COMMAND_NAME]);
  FCustomizeCommandRef := Owner.FindCommand(Name);
  if Assigned(FCustomizeCommandRef) then
    FCustomizeCommandRef.FreeNotification(Self);
  if (E.ChildCount > 1) then
    Error(E, RS_MULTIPLE_ELEMENTS, [E.Name, E[0].Name]);
  if (E.ChildCount = 1) then
  begin
    C := E[0];
    if (C.Name <> EN_QUICK_ACCESS_TOOLBAR_APPLICATION_DEFAULTS) then
      Error(C, RS_ELEMENT_EXPECTED, [EN_QUICK_ACCESS_TOOLBAR_APPLICATION_DEFAULTS, C.Name]);

    for GC in C do
    begin
      if (GC.Name = EN_BUTTON) then
        FControls.Add(TRibbonQatButton.Create(Owner, GC))
      else if (GC.Name = EN_TOGGLE_BUTTON) then
        FControls.Add(TRibbonQatToggleButton.Create(Owner, GC))
      else if (GC.Name = EN_CHECK_BOX) then
        FControls.Add(TRibbonQatCheckBox.Create(Owner, GC))
      else
        Error(GC, RS_UNSUPPORTED_CHILD_ELEMENT, [GC.Name, C.Name]);
    end;
  end;
end;

function TRibbonQuickAccessToolbar.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj is TRibbonQatControl) then
    Result := FControls.Remove(TRibbonQatControl(Obj))
  else
    Result := inherited Delete(Obj);
end;

destructor TRibbonQuickAccessToolbar.Destroy;
begin
  if Assigned(FCustomizeCommandRef) then
    FCustomizeCommandRef.RemoveFreeNotification(Self);
  FControls.Free;
  inherited;
end;

procedure TRibbonQuickAccessToolbar.FreeNotify(const Obj: TRibbonObject);
begin
  inherited;
  if (Obj = FCustomizeCommandRef) then
    FCustomizeCommandRef := nil;
end;

class function TRibbonQuickAccessToolbar.ObjectType: TRibbonObjectType;
begin
  Result := otQuickAccessToolbar;
end;

function TRibbonQuickAccessToolbar.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonQatControl) then
    Result := FControls.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonQuickAccessToolbar.Save(const Writer: TXmlWriter);
var
  Control: TRibbonQatControl;
begin
  Writer.WriteStartElement(EN_RIBBON_QUICK_ACCESS_TOOLBAR);
    Writer.WriteStartElement(EN_QUICK_ACCESS_TOOLBAR);
    inherited;
    if Assigned(FCustomizeCommandRef) then
      FCustomizeCommandRef.SaveRef(Writer, AN_CUSTOMIZE_COMMAND_NAME);

    if (FControls.Count > 0) then
    begin
      Writer.WriteStartElement(EN_QUICK_ACCESS_TOOLBAR_APPLICATION_DEFAULTS);

      for Control in FControls do
        Control.Save(Writer);

      Writer.WriteEndElement;
    end;

    Writer.WriteEndElement;
  Writer.WriteEndElement;
end;

procedure TRibbonQuickAccessToolbar.SetCustomizeCommandRef(
  const Value: TRibbonCommand);
begin
  if (Value <> FCustomizeCommandRef) then
  begin
    if Assigned(FCustomizeCommandRef) then
      FCustomizeCommandRef.RemoveFreeNotification(Self);

    FCustomizeCommandRef := Value;

    if Assigned(FCustomizeCommandRef) then
      FCustomizeCommandRef.FreeNotification(Self);
  end;
end;

class function TRibbonQuickAccessToolbar.SupportApplicationModes: Boolean;
begin
  Result := False;
end;

{ TRibbonScale }

constructor TRibbonScale.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FSize := glSmall;
end;

function TRibbonScale.CanReorder: Boolean;
begin
  Result := True;
end;

constructor TRibbonScale.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  Name: TRibbonCommandName;
  S: String;
begin
  inherited Create(Owner);
  Name := StringToCommandName(E.AttributeAsString[AN_GROUP]);
  FGroupRef := Owner.FindCommand(Name);
  if Assigned(FGroupRef) then
    FGroupRef.FreeNotification(Self);
  S := E.AttributeAsString[AN_SIZE];
  if (S = ES_POPUP) then
    FSize := glPopup
  else if (S = ES_SMALL) or (S = '') then
    FSize := glSmall
  else if (S = ES_MEDIUM) then
    FSize := glMedium
  else if (S = ES_LARGE) then
    FSize := glLarge
  else
    Error(E, RS_INVALID_SIZE);
end;

destructor TRibbonScale.Destroy;
begin
  if Assigned(FGroupRef) then
    FGroupRef.RemoveFreeNotification(Self);
  inherited;
end;

function TRibbonScale.DisplayName: String;
const
  SIZES: array [TRibbonGroupLayout] of String = (
    ES_POPUP, ES_SMALL, ES_MEDIUM, ES_LARGE);
begin
  if Assigned(FGroupRef) then
  begin
    Result := FGroupRef.LabelTitle.Content;
    if (Result = '') then
    begin
      Result := FGroupRef.Name;
      if (Result = '') then
      begin
        Result := FGroupRef.Symbol;
        if (Result = '') then
          Result := IntToStr(FGroupRef.Id);
      end;
    end;
  end
  else
    Result := '(scale)';

  Result := Result + ' (' + SIZES[FSize] + ')';
end;

procedure TRibbonScale.FreeNotify(const Obj: TRibbonObject);
begin
  inherited;
  if (Obj = FGroupRef) then
    FGroupRef := nil;
end;

class function TRibbonScale.ObjectType: TRibbonObjectType;
begin
  Result := otScale;
end;

procedure TRibbonScale.Save(const Writer: TXmlWriter);
const
  SIZES: array [TRibbonGroupLayout] of String = (
    ES_POPUP, ES_SMALL, ES_MEDIUM, ES_LARGE);
begin
  Writer.WriteStartElement(EN_SCALE);

  if Assigned(FGroupRef) then
    FGroupRef.SaveRef(Writer, AN_GROUP);

  Writer.WriteAttribute(AN_SIZE, SIZES[FSize]);

  Writer.WriteEndElement;
end;

procedure TRibbonScale.SetGroupRef(const Value: TRibbonCommand);
begin
  if (Value <> FGroupRef) then
  begin
    if Assigned(FGroupRef) then
      FGroupRef.RemoveFreeNotification(Self);

    FGroupRef := Value;

    if Assigned(FGroupRef) then
      FGroupRef.FreeNotification(Self);
  end;
end;

{ TRibbonScalingPolicy }

constructor TRibbonScalingPolicy.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FIdealSizes := TRibbonList<TRibbonScale>.Create(Owner, True);
  FScales := TRibbonList<TRibbonScale>.Create(Owner, True);
end;

function TRibbonScalingPolicy.AddIdealSize: TRibbonScale;
begin
  Result := TRibbonScale.Create(Owner);
  FIdealSizes.Add(Result);
end;

function TRibbonScalingPolicy.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType = otScale) then
  begin
    Result := TRibbonScale.Create(Owner);
    FScales.Add(Result as TRibbonScale);
  end
  else
    Result := inherited AddNew(ObjType);
end;

constructor TRibbonScalingPolicy.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  C, GC: TXmlElement;
  HasIdealSizes: Boolean;
begin
  inherited Create(Owner);
  FIdealSizes := TRibbonList<TRibbonScale>.Create(Owner, True);
  FScales := TRibbonList<TRibbonScale>.Create(Owner, True);
  HasIdealSizes := False;
  for C in E do
  begin
    if (C.Name = EN_SCALING_POLICY_IDEAL_SIZES) then
    begin
      if (HasIdealSizes) then
        Error(C, RS_MULTIPLE_ELEMENTS, [E.Name, C.Name]);
      HasIdealSizes := True;
      for GC in C do
      begin
        if (GC.Name <> EN_SCALE) then
          Error(GC, RS_UNSUPPORTED_CHILD_ELEMENT, [GC.Name, C.Name]);
        FIdealSizes.Add(TRibbonScale.Create(Owner, GC));
      end;
    end
    else if (C.Name = EN_SCALE) then
      FScales.Add(TRibbonScale.Create(Owner, C))
    else
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;
end;

function TRibbonScalingPolicy.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj is TRibbonScale) then
    Result := FScales.Remove(TRibbonScale(Obj)) or FIdealSizes.Remove(TRibbonScale(Obj))
  else
    Result := inherited Delete(Obj);
end;

destructor TRibbonScalingPolicy.Destroy;
begin
  FIdealSizes.Free;
  FScales.Free;
  inherited;
end;

function TRibbonScalingPolicy.DisplayName: String;
begin
  Result := RS_SCALING_POLICY;
end;

class function TRibbonScalingPolicy.ObjectType: TRibbonObjectType;
begin
  Result := otScalingPolicy;
end;

function TRibbonScalingPolicy.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonScale) then
    Result := FIdealSizes.Reorder(Child, Direction) or FScales.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonScalingPolicy.Save(const Writer: TXmlWriter);
var
  Scale: TRibbonScale;
begin
  if (FIdealSizes.Count = 0) and (FScales.Count = 0) then
    Exit;

  Writer.WriteStartElement(EN_TAB_SCALING_POLICY);
    Writer.WriteStartElement(EN_SCALING_POLICY);

    if (FIdealSizes.Count > 0) then
    begin
      Writer.WriteStartElement(EN_SCALING_POLICY_IDEAL_SIZES);
      for Scale in FIdealSizes do
        Scale.Save(Writer);
      Writer.WriteEndElement;
    end;

    for Scale in FScales do
      Scale.Save(Writer);

    Writer.WriteEndElement;
  Writer.WriteEndElement;
end;

{ TRibbonControlNameMap }

constructor TRibbonControlNameMap.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FControlNameDefinitions := TRibbonList<String>.Create(Owner, False);
end;

procedure TRibbonControlNameMap.Add(const Name: String);
begin
  FControlNameDefinitions.Add(Name);
end;

procedure TRibbonControlNameMap.Clear;
begin
  FControlNameDefinitions.Clear;
end;

constructor TRibbonControlNameMap.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  C: TXmlElement;
  S: String;
begin
  inherited Create(Owner);
  FControlNameDefinitions := TRibbonList<String>.Create(Owner, False);
  for C in E do
  begin
    if (C.Name = EN_CONTROL_NAME_DEFINITION) then
    begin
      S := C.AttributeAsString[AN_NAME];
      if IsValidCommandNameString(S) then
        FControlNameDefinitions.Add(S)
      else
        Error(C, RS_INVALID_COMMAND_NAME, [S]);
    end
    else
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;
end;

destructor TRibbonControlNameMap.Destroy;
begin
  FControlNameDefinitions.Free;
  inherited;
end;

class function TRibbonControlNameMap.ObjectType: TRibbonObjectType;
begin
  Result := otControlNameMap;
end;

procedure TRibbonControlNameMap.Save(const Writer: TXmlWriter);
var
  S: String;
begin
  Writer.WriteStartElement(EN_CONTROL_NAME_MAP);

  for S in FControlNameDefinitions do
  begin
    Writer.WriteStartElement(EN_CONTROL_NAME_DEFINITION);
    Writer.WriteAttribute(AN_NAME, S);
    Writer.WriteEndElement;
  end;

  Writer.WriteEndElement;
end;

{ TRibbonGroupSizeDefinitionElement }

function TRibbonGroupSizeDefinitionElement.CanReorder: Boolean;
begin
  Result := True;
end;

constructor TRibbonGroupSizeDefinitionElement.Create(
  const Owner: TRibbonDocument; const OwnerDefinition: TRibbonSizeDefinition);
begin
  inherited Create(Owner);
  FOwnerDefinition := OwnerDefinition;
end;

{ TRibbonControlSizeDefinition }

constructor TRibbonControlSizeDefinition.Create(const Owner: TRibbonDocument;
  const OwnerDefinition: TRibbonSizeDefinition);
begin
  inherited Create(Owner, OwnerDefinition);
  FImageSize := isSmall;
  FIsLabelVisible := True;
  FIsImageVisible := True;
end;

constructor TRibbonControlSizeDefinition.Create(const Owner: TRibbonDocument;
  const OwnerDefinition: TRibbonSizeDefinition; const E: TXmlElement);
var
  S: String;
begin
  inherited Create(Owner, OwnerDefinition);
  S := E.AttributeAsString[AN_IMAGE_SIZE];
  if (S = '') or (S = ES_SMALL) then
    FImageSize := isSmall
  else if (S = ES_LARGE) then
    FImageSize := isLarge
  else
    Error(E, RS_INVALID_IMAGE_SIZE);

  FIsLabelVisible := E.AttributeAsBooleanDef(AN_IS_LABEL_VISIBLE, True);
  FIsImageVisible := E.AttributeAsBooleanDef(AN_IS_IMAGE_VISIBLE, True);
  FIsPopup := E.AttributeAsBooleanDef(AN_IS_POPUP, False);
  FControlName := E.AttributeAsString[AN_CONTROL_NAME];
  if (not IsValidCommandNameString(FControlName)) then
    Error(E, RS_INVALID_COMMAND_NAME, [FControlName]);
end;

function TRibbonControlSizeDefinition.DisplayName: String;
begin
  Result := RS_CONTROL;
  if (FControlName <> '') then
    Result := Result + ' (' + FControlName + ')';
end;

class function TRibbonControlSizeDefinition.ObjectType: TRibbonObjectType;
begin
  Result := otControlSizeDefinition;
end;

procedure TRibbonControlSizeDefinition.Save(const Writer: TXmlWriter);
const
  IMAGE_SIZES: array [TRibbonImageSize] of String = (
    ES_LARGE, ES_SMALL);
begin
  Writer.WriteStartElement(EN_CONTROL_SIZE_DEFINITION);

  if (FImageSize <> isSmall) then
    Writer.WriteAttribute(AN_IMAGE_SIZE, IMAGE_SIZES[FImageSize]);

  if (not FIsLabelVisible) then
    Writer.WriteAttribute(AN_IS_LABEL_VISIBLE, FIsLabelVisible);

  if (not FIsImageVisible) then
    Writer.WriteAttribute(AN_IS_IMAGE_VISIBLE, FIsImageVisible);

  if (FIsPopup) then
    Writer.WriteAttribute(AN_IS_POPUP, FIsPopup);

  if (FControlName <> '') then
    Writer.WriteAttribute(AN_CONTROL_NAME, FControlName);

  Writer.WriteEndElement;
end;

{ TRibbonRow }

constructor TRibbonRow.Create(const Owner: TRibbonDocument;
  const OwnerDefinition: TRibbonSizeDefinition);
begin
  inherited Create(Owner, OwnerDefinition);
  FElements := TRibbonList<TRibbonGroupSizeDefinitionElement>.Create(Owner, True);
end;

function TRibbonRow.AddNew(const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType in [otControlSizeDefinition, otControlSizeGroup]) then
  begin
    case ObjType of
      otControlSizeDefinition:
        Result := TRibbonControlSizeDefinition.Create(Owner, OwnerDefinition);

      otControlSizeGroup:
        Result := TRibbonControlSizeGroup.Create(Owner, OwnerDefinition);
    else
      begin
        Assert(False);
        Result := nil;
        Exit;
      end;
    end;
    FElements.Add(TRibbonGroupSizeDefinitionElement(Result));
  end
  else
    Result := inherited AddNew(ObjType);
end;

constructor TRibbonRow.Create(const Owner: TRibbonDocument;
  const OwnerDefinition: TRibbonSizeDefinition; const E: TXmlElement);
var
  C: TXmlElement;
begin
  inherited Create(Owner, OwnerDefinition);
  FElements := TRibbonList<TRibbonGroupSizeDefinitionElement>.Create(Owner, True);

  for C in E do
  begin
    if (C.Name = EN_CONTROL_SIZE_DEFINITION) then
      FElements.Add(TRibbonControlSizeDefinition.Create(Owner, OwnerDefinition, C))
    else if (C.Name = EN_CONTROL_GROUP) then
      FElements.Add(TRibbonControlSizeGroup.Create(Owner, OwnerDefinition, C))
    else
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;
end;

function TRibbonRow.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj is TRibbonGroupSizeDefinitionElement) then
    Result := FElements.Remove(TRibbonGroupSizeDefinitionElement(Obj))
  else
    Result := inherited Delete(Obj);
end;

destructor TRibbonRow.Destroy;
begin
  FElements.Free;
  inherited;
end;

function TRibbonRow.DisplayName: String;
begin
  Result := RS_ROW;
end;

class function TRibbonRow.ObjectType: TRibbonObjectType;
begin
  Result := otRow;
end;

function TRibbonRow.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonGroupSizeDefinitionElement) then
    Result := FElements.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonRow.Save(const Writer: TXmlWriter);
var
  Element: TRibbonGroupSizeDefinitionElement;
begin
  Writer.WriteStartElement(EN_ROW);

  for Element in FElements do
    Element.Save(Writer);

  Writer.WriteEndElement;
end;

{ TRibbonControlSizeGroup }

constructor TRibbonControlSizeGroup.Create(const Owner: TRibbonDocument;
  const OwnerDefinition: TRibbonSizeDefinition);
begin
  inherited Create(Owner, OwnerDefinition);
  FControlSizeDefinitions := TRibbonList<TRibbonControlSizeDefinition>.Create(Owner, True);
end;

function TRibbonControlSizeGroup.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType = otControlSizeDefinition) then
  begin
    Result := TRibbonControlSizeDefinition.Create(Owner, OwnerDefinition);
    FControlSizeDefinitions.Add(TRibbonControlSizeDefinition(Result));
  end
  else
    Result := inherited AddNew(ObjType);
end;

constructor TRibbonControlSizeGroup.Create(const Owner: TRibbonDocument;
  const OwnerDefinition: TRibbonSizeDefinition; const E: TXmlElement);
var
  C: TXmlElement;
begin
  inherited Create(Owner, OwnerDefinition);
  FControlSizeDefinitions := TRibbonList<TRibbonControlSizeDefinition>.Create(Owner, True);

  for C in E do
  begin
    if (C.Name = EN_CONTROL_SIZE_DEFINITION) then
      FControlSizeDefinitions.Add(TRibbonControlSizeDefinition.Create(Owner, OwnerDefinition, C))
    else
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;
end;

function TRibbonControlSizeGroup.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj is TRibbonControlSizeDefinition) then
    Result := FControlSizeDefinitions.Remove(TRibbonControlSizeDefinition(Obj))
  else
    Result := inherited Delete(Obj);
end;

destructor TRibbonControlSizeGroup.Destroy;
begin
  FControlSizeDefinitions.Free;
  inherited;
end;

function TRibbonControlSizeGroup.DisplayName: String;
begin
  Result := RS_GROUP;
end;

class function TRibbonControlSizeGroup.ObjectType: TRibbonObjectType;
begin
  Result := otControlSizeGroup;
end;

function TRibbonControlSizeGroup.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonControlSizeDefinition) then
    Result := FControlSizeDefinitions.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonControlSizeGroup.Save(const Writer: TXmlWriter);
var
  SizeDef: TRibbonControlSizeDefinition;
begin
  Writer.WriteStartElement(EN_CONTROL_GROUP);

  for SizeDef in FControlSizeDefinitions do
    SizeDef.Save(Writer);

  Writer.WriteEndElement;
end;

{ TRibbonColumnBreak }

constructor TRibbonColumnBreak.Create(const Owner: TRibbonDocument;
  const OwnerDefinition: TRibbonSizeDefinition);
begin
  inherited Create(Owner, OwnerDefinition);
  FShowSeparator := True;
end;

constructor TRibbonColumnBreak.Create(const Owner: TRibbonDocument;
  const OwnerDefinition: TRibbonSizeDefinition; const E: TXmlElement);
begin
  inherited Create(Owner, OwnerDefinition);
  FShowSeparator := E.AttributeAsBooleanDef(AN_SHOW_SEPARATOR, True);
end;

function TRibbonColumnBreak.DisplayName: String;
begin
  Result := RS_COLUMN_BREAK;
end;

class function TRibbonColumnBreak.ObjectType: TRibbonObjectType;
begin
  Result := otColumnBreak;
end;

procedure TRibbonColumnBreak.Save(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_COLUMN_BREAK);
  if (not FShowSeparator) then
    Writer.WriteAttribute(AN_SHOW_SEPARATOR, FShowSeparator);
  Writer.WriteEndElement;
end;

{ TRibbonGroupSizeDefinition }

constructor TRibbonGroupSizeDefinition.Create(const Owner: TRibbonDocument;
  const OwnerDefinition: TRibbonSizeDefinition);
begin
  inherited Create(Owner);
  FOwnerDefinition := OwnerDefinition;
  FElements := TRibbonList<TRibbonGroupSizeDefinitionElement>.Create(Owner, True);
  FSize := stLarge;
end;

function TRibbonGroupSizeDefinition.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType in [otControlSizeDefinition, otControlSizeGroup, otColumnBreak, otRow]) then
  begin
    case ObjType of
      otControlSizeDefinition:
        Result := TRibbonControlSizeDefinition.Create(Owner, FOwnerDefinition);

      otControlSizeGroup:
        Result := TRibbonControlSizeGroup.Create(Owner, FOwnerDefinition);

      otColumnBreak:
        Result := TRibbonColumnBreak.Create(Owner, FOwnerDefinition);

      otRow:
        Result := TRibbonRow.Create(Owner, FOwnerDefinition);
    else
      begin
        Assert(False);
        Result := nil;
        Exit;
      end;
    end;
    FElements.Add(TRibbonGroupSizeDefinitionElement(Result));
  end
  else
    Result := inherited AddNew(ObjType);
end;

function TRibbonGroupSizeDefinition.CanReorder: Boolean;
begin
  Result := True;
end;

constructor TRibbonGroupSizeDefinition.Create(const Owner: TRibbonDocument;
  const OwnerDefinition: TRibbonSizeDefinition; const E: TXmlElement);
var
  S: String;
  C: TXmlElement;
begin
  inherited Create(Owner);
  FOwnerDefinition := OwnerDefinition;
  FElements := TRibbonList<TRibbonGroupSizeDefinitionElement>.Create(Owner, True);
  S := E.AttributeAsString[AN_SIZE];
  if (S = '') or (S = ES_LARGE) then
    FSize := stLarge
  else if (S = ES_MEDIUM) then
    FSize := stMedium
  else if (S = ES_SMALL) then
    FSize := stSmall
  else
    Error(E, RS_INVALID_SIZE);

  for C in E do
  begin
    if (C.Name = EN_CONTROL_SIZE_DEFINITION) then
      FElements.Add(TRibbonControlSizeDefinition.Create(Owner, OwnerDefinition, C))
    else if (C.Name = EN_CONTROL_GROUP) then
      FElements.Add(TRibbonControlSizeGroup.Create(Owner, OwnerDefinition, C))
    else if (C.Name = EN_COLUMN_BREAK) then
      FElements.Add(TRibbonColumnBreak.Create(Owner, OwnerDefinition, C))
    else if (C.Name = EN_ROW) then
      FElements.Add(TRibbonRow.Create(Owner, OwnerDefinition, C))
    else
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;
end;

function TRibbonGroupSizeDefinition.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj is TRibbonGroupSizeDefinitionElement) then
    Result := FElements.Remove(TRibbonGroupSizeDefinitionElement(Obj))
  else
    Result := inherited Delete(Obj);
end;

destructor TRibbonGroupSizeDefinition.Destroy;
begin
  FElements.Free;
  inherited;
end;

function TRibbonGroupSizeDefinition.DisplayName: String;
const
  SIZES: array [TRibbonGroupSizeType] of String = (
    ES_LARGE, ES_MEDIUM, ES_SMALL);
begin
  Result := RS_GROUP + ' (' + SIZES[FSize] + ')';
end;

class function TRibbonGroupSizeDefinition.ObjectType: TRibbonObjectType;
begin
  Result := otGroupSizeDefinition;
end;

function TRibbonGroupSizeDefinition.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonGroupSizeDefinitionElement) then
    Result := FElements.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonGroupSizeDefinition.Save(const Writer: TXmlWriter);
const
  SIZES: array [TRibbonGroupSizeType] of String = (
    ES_LARGE, ES_MEDIUM, ES_SMALL);
var
  Element: TRibbonGroupSizeDefinitionElement;
begin
  Writer.WriteStartElement(EN_GROUP_SIZE_DEFINITION);

  Writer.WriteAttribute(AN_SIZE, SIZES[FSize]);

  for Element in FElements do
    Element.Save(Writer);

  Writer.WriteEndElement;
end;

{ TRibbonSizeDefinition }

constructor TRibbonSizeDefinition.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FGroupSizeDefinitions := TRibbonList<TRibbonGroupSizeDefinition>.Create(Owner, True);
  FGroupSizeDefinitions.Add(TRibbonGroupSizeDefinition.Create(Owner, Self));
  FControlNameMap := TRibbonControlNameMap.Create(Owner);
end;

function TRibbonSizeDefinition.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType = otGroupSizeDefinition) then
  begin
    if (FGroupSizeDefinitions.Count >= 3) then
      Error(nil, RS_MAX_GROUP_SIZE_DEF);
    Result := TRibbonGroupSizeDefinition.Create(Owner, Self);
    FGroupSizeDefinitions.Add(TRibbonGroupSizeDefinition(Result));
  end
  else
    Result := inherited AddNew(ObjType);
end;

function TRibbonSizeDefinition.CanReorder: Boolean;
begin
  Result := True;
end;

constructor TRibbonSizeDefinition.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  C: TXmlElement;
begin
  inherited Create(Owner);
  FGroupSizeDefinitions := TRibbonList<TRibbonGroupSizeDefinition>.Create(Owner, True);
  for C in E do
  begin
    if (C.Name = EN_CONTROL_NAME_MAP) then
    begin
      if Assigned(FControlNameMap) then
        Error(C, RS_MULTIPLE_ELEMENTS, [E.Name, C.Name]);
      FControlNameMap := TRibbonControlNameMap.Create(Owner, C);
    end
    else if (C.Name = EN_GROUP_SIZE_DEFINITION) then
      FGroupSizeDefinitions.Add(TRibbonGroupSizeDefinition.Create(Owner, Self, C))
    else
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;

  if (FGroupSizeDefinitions.Count < 1) or (FGroupSizeDefinitions.Count > 3) then
    Error(E, RS_INVALID_GROUP_SIZE_DEFINITIONS);

  if (FControlNameMap = nil) then
    FControlNameMap := TRibbonControlNameMap.Create(Owner);
end;

function TRibbonSizeDefinition.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj is TRibbonGroupSizeDefinition) then
  begin
    if (FGroupSizeDefinitions.Count <= 1) then
      Error(nil, RS_MIN_GROUP_SIZE_DEF);
    Result := FGroupSizeDefinitions.Remove(TRibbonGroupSizeDefinition(Obj))
  end
  else
    Result := inherited Delete(Obj);
end;

destructor TRibbonSizeDefinition.Destroy;
begin
  FGroupSizeDefinitions.Free;
  FControlNameMap.Free;
  inherited;
end;

function TRibbonSizeDefinition.DisplayName: String;
begin
  Result := RS_SIZE_DEF;
end;

class function TRibbonSizeDefinition.ObjectType: TRibbonObjectType;
begin
  Result := otSizeDefinition;
end;

function TRibbonSizeDefinition.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonGroupSizeDefinition) then
    Result := FGroupSizeDefinitions.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonSizeDefinition.Save(const Writer: TXmlWriter);
var
  Group: TRibbonGroupSizeDefinition;
begin
  Writer.WriteStartElement(AN_SIZE_DEFINITION);

  SaveAttributes(Writer);

  if Assigned(FControlNameMap) then
    FControlNameMap.Save(Writer);

  for Group in FGroupSizeDefinitions do
    Group.Save(Writer);

  Writer.WriteEndElement;
end;

procedure TRibbonSizeDefinition.SaveAttributes(const Writer: TXmlWriter);
begin
  { No default implementation }
end;

{ TRibbonRibbonSizeDefinition }

constructor TRibbonRibbonSizeDefinition.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
end;

constructor TRibbonRibbonSizeDefinition.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
begin
  inherited Create(Owner, E);
  FName := E.AttributeAsString[AN_NAME];
  if (not IsValidCommandNameString(FName)) then
    Error(E, RS_INVALID_COMMAND_NAME, [FName]);
end;

function TRibbonRibbonSizeDefinition.DisplayName: String;
begin
  Result := FName;
  if (Result = '') then
    Result := inherited DisplayName;
end;

class function TRibbonRibbonSizeDefinition.ObjectType: TRibbonObjectType;
begin
  Result := otRibbonSizeDefinition;
end;

procedure TRibbonRibbonSizeDefinition.SaveAttributes(const Writer: TXmlWriter);
begin
  inherited;
  Writer.WriteAttribute(AN_NAME, FName);
end;

{ TRibbonGroup }

constructor TRibbonGroup.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FControls := TRibbonList<TRibbonControl>.Create(Owner, True);
end;

function TRibbonGroup.AddNew(const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType in [otControlGroup, otToggleButton, otCheckBox, otButton,
    otSplitButton, otDropDownButton, otDropDownGallery, otSplitButtonGallery,
    otDropDownColorPicker, otComboBox, otSpinner, otInRibbonGallery, otFontControl]) then
  begin
    Result := Owner.CreateObject(ObjType);
    FControls.Add(Result as TRibbonControl);
  end
  else
    Result := inherited AddNew(ObjType);
end;

constructor TRibbonGroup.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  S: String;
  SD: TRibbonBasicSizeDefinition;
  C: TXmlElement;
begin
  inherited Create(Owner, E);
  FControls := TRibbonList<TRibbonControl>.Create(Owner, True);
  S := E.AttributeAsString[AN_SIZE_DEFINITION];
  for SD := Low(TRibbonBasicSizeDefinition) to High(TRibbonBasicSizeDefinition) do
    if (S = ES_SIZE_DEFINITION[SD]) then
    begin
      FBasicSizeDefinition := SD;
      Break;
    end;
  if (FBasicSizeDefinition = sdCustom) then
    FCustomSizeDefinition := S;

  for C in E do
  begin
    if (C.Name = EN_SIZE_DEFINITION) then
    begin
      if Assigned(FSizeDefinition) then
        Error(C, RS_MULTIPLE_ELEMENTS, [E.Name, C.Name]);
      FSizeDefinition := TRibbonSizeDefinition.Create(Owner, C);
      FBasicSizeDefinition := sdAdvanced;
    end
    else if (C.Name = EN_CONTROL_GROUP) then
      FControls.Add(TRibbonControlGroup.Create(Owner, C))
    else if (C.Name = EN_TOGGLE_BUTTON) then
      FControls.Add(TRibbonToggleButton.Create(Owner, C))
    else if (C.Name = EN_CHECK_BOX) then
      FControls.Add(TRibbonCheckBox.Create(Owner, C))
    else if (C.Name = EN_BUTTON) then
      FControls.Add(TRibbonButton.Create(Owner, C))
    else if (C.Name = EN_SPLIT_BUTTON) then
      FControls.Add(TRibbonSplitButton.Create(Owner, C))
    else if (C.Name = EN_DROP_DOWN_BUTTON) then
      FControls.Add(TRibbonDropDownButton.Create(Owner, C))
    else if (C.Name = EN_DROP_DOWN_GALLERY) then
      FControls.Add(TRibbonDropDownGallery.Create(Owner, C))
    else if (C.Name = EN_SPLIT_BUTTON_GALLERY) then
      FControls.Add(TRibbonSplitButtonGallery.Create(Owner, C))
    else if (C.Name = EN_DROP_DOWN_COLOR_PICKER) then
      FControls.Add(TRibbonDropDownColorPicker.Create(Owner, C))
    else if (C.Name = EN_COMBO_BOX) then
      FControls.Add(TRibbonComboBox.Create(Owner, C))
    else if (C.Name = EN_SPINNER) then
      FControls.Add(TRibbonSpinner.Create(Owner, C))
    else if (C.Name = EN_IN_RIBBON_GALLERY) then
      FControls.Add(TRibbonInRibbonGallery.Create(Owner, C))
    else if (C.Name = EN_FONT_CONTROL) then
      FControls.Add(TRibbonFontControl.Create(Owner, C))
    else
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;
end;

procedure TRibbonGroup.CreateAdvancedSizeDefinition;
begin
  FreeAndNil(FSizeDefinition);
  FSizeDefinition := TRibbonSizeDefinition.Create(Owner);
end;

function TRibbonGroup.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj is TRibbonControl) then
    Result := FControls.Remove(TRibbonControl(Obj))
  else
    Result := inherited Delete(Obj);
end;

procedure TRibbonGroup.DeleteAdvancedSizeDefinition;
begin
  FreeAndNil(FSizeDefinition);
end;

destructor TRibbonGroup.Destroy;
begin
  FControls.Free;
  FSizeDefinition.Free;
  inherited;
end;

class function TRibbonGroup.ObjectType: TRibbonObjectType;
begin
  Result := otGroup;
end;

function TRibbonGroup.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonControl) then
    Result := FControls.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonGroup.Save(const Writer: TXmlWriter);
var
  Control: TRibbonControl;
begin
  Writer.WriteStartElement(EN_GROUP);
  inherited;

  if (FBasicSizeDefinition > sdAdvanced) then
    Writer.WriteAttribute(AN_SIZE_DEFINITION, ES_SIZE_DEFINITION[FBasicSizeDefinition])
  else if (FBasicSizeDefinition = sdCustom) and (FCustomSizeDefinition <> '') then
    Writer.WriteAttribute(AN_SIZE_DEFINITION, FCustomSizeDefinition);

  if Assigned(FSizeDefinition) then
    FSizeDefinition.Save(Writer);

  for Control in FControls do
    Control.Save(Writer);

  Writer.WriteEndElement;
end;

procedure TRibbonGroup.SetBasicSizeDefinition(
  const Value: TRibbonBasicSizeDefinition);
begin
  if (Value <> FBasicSizeDefinition) then
  begin
    FBasicSizeDefinition := Value;
    if (Value <> sdCustom) then
      FCustomSizeDefinition := '';
  end;
end;

procedure TRibbonGroup.SetCustomSizeDefinition(const Value: String);
begin
  if (Value <> FCustomSizeDefinition) then
  begin
    FCustomSizeDefinition := Value;
    FBasicSizeDefinition := sdCustom;
  end;
end;

class function TRibbonGroup.SupportApplicationModes: Boolean;
begin
  Result := True;
end;

{ TRibbonTab }

constructor TRibbonTab.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FGroups := TRibbonList<TRibbonGroup>.Create(Owner, True);
  FScalingPolicy := TRibbonScalingPolicy.Create(Owner);
end;

function TRibbonTab.AddNew(const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType = otGroup) then
  begin
    Result := TRibbonGroup.Create(Owner);
    FGroups.Add(Result as TRibbonGroup);
  end
  else
    Result := inherited AddNew(ObjType);
end;

constructor TRibbonTab.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  C, GC: TXmlElement;
begin
  inherited Create(Owner, E);
  FGroups := TRibbonList<TRibbonGroup>.Create(Owner, True);
  if (E.Name <> EN_TAB) then
    Error(E, RS_ELEMENT_EXPECTED, [EN_TAB, E.Name]);
  for C in E do
  begin
    if (C.Name = EN_TAB_SCALING_POLICY) then
    begin
      if (C.ChildCount <> 1) then
        Error(C, RS_SINGLE_ELEMENT, [C.Name, EN_SCALING_POLICY]);
      GC := C[0];
      if (GC.Name <> EN_SCALING_POLICY) then
        Error(GC, RS_ELEMENT_EXPECTED, [EN_SCALING_POLICY, GC.Name]);
      FScalingPolicy := TRibbonScalingPolicy.Create(Owner, GC);
    end
    else if (C.Name = EN_GROUP) then
      FGroups.Add(TRibbonGroup.Create(Owner, C))
    else
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;
  if (FScalingPolicy = nil) then
    FScalingPolicy := TRibbonScalingPolicy.Create(Owner);
end;

function TRibbonTab.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj is TRibbonGroup) then
    Result := FGroups.Remove(TRibbonGroup(Obj))
  else
    Result := inherited Delete(Obj);
end;

destructor TRibbonTab.Destroy;
begin
  FGroups.Free;
  FScalingPolicy.Free;
  inherited;
end;

class function TRibbonTab.ObjectType: TRibbonObjectType;
begin
  Result := otTab;
end;

function TRibbonTab.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonGroup) then
    Result := FGroups.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonTab.Save(const Writer: TXmlWriter);
var
  Group: TRibbonGroup;
begin
  Writer.WriteStartElement(EN_TAB);
  inherited;

  if Assigned(FScalingPolicy) then
    FScalingPolicy.Save(Writer);

  for Group in FGroups do
    Group.Save(Writer);

  Writer.WriteEndElement;
end;

class function TRibbonTab.SupportApplicationModes: Boolean;
begin
  Result := True;
end;

{ TRibbonTabGroup }

constructor TRibbonTabGroup.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FTabs := TRibbonList<TRibbonTab>.Create(Owner, True);
end;

function TRibbonTabGroup.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType = otTab) then
  begin
    Result := TRibbonTab.Create(Owner);
    FTabs.Add(TRibbonTab(Result));
  end
  else
    Result := inherited AddNew(ObjType);
end;

constructor TRibbonTabGroup.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  C: TXmlElement;
begin
  inherited Create(Owner, E);
  FTabs := TRibbonList<TRibbonTab>.Create(Owner, True);
  for C in E do
  begin
    if (C.Name <> EN_TAB) then
      Error(C, RS_ELEMENT_EXPECTED, [EN_TAB, C.Name]);
    FTabs.Add(TRibbonTab.Create(Owner, C));
  end;
end;

function TRibbonTabGroup.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj is TRibbonTab) then
    Result := FTabs.Remove(TRibbonTab(Obj))
  else
    Result := inherited Delete(Obj);
end;

destructor TRibbonTabGroup.Destroy;
begin
  FTabs.Free;
  inherited;
end;

class function TRibbonTabGroup.ObjectType: TRibbonObjectType;
begin
  Result := otTabGroup;
end;

function TRibbonTabGroup.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonTab) then
    Result := FTabs.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonTabGroup.Save(const Writer: TXmlWriter);
var
  Tab: TRibbonTab;
begin
  Writer.WriteStartElement(EN_TAB_GROUP);
  inherited;
  for Tab in FTabs do
    Tab.Save(Writer);
  Writer.WriteEndElement;
end;

class function TRibbonTabGroup.SupportApplicationModes: Boolean;
begin
  Result := False;
end;

{ TRibbonViewRibbon }

constructor TRibbonViewRibbon.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FTabs := TRibbonList<TRibbonTab>.Create(Owner, True);
  FContextualTabs := TRibbonList<TRibbonTabGroup>.Create(Owner, True);
  FSizeDefinitions := TRibbonList<TRibbonRibbonSizeDefinition>.Create(Owner, True);
  FApplicationMenu := TRibbonApplicationMenu.Create(Owner);
  FQuickAccessToolbar := TRibbonQuickAccessToolbar.Create(Owner);
  FHelpButton := TRibbonHelpButton.Create(Owner);
  FGroupSpacing := gsSmall;
end;

function TRibbonViewRibbon.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType = otRibbonSizeDefinition) then
  begin
    Result := Owner.CreateObject(ObjType);
    FSizeDefinitions.Add(Result as TRibbonRibbonSizeDefinition);
  end
  else if (ObjType = otTab) then
  begin
    Result := TRibbonTab.Create(Owner);
    FTabs.Add(TRibbonTab(Result));
  end
  else if (ObjType = otTabGroup) then
  begin
    Result := TRibbonTabGroup.Create(Owner);
    FContextualTabs.Add(TRibbonTabGroup(Result));
  end
  else
    Result := inherited AddNew(ObjType);
end;

constructor TRibbonViewRibbon.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  S: String;
  C, GC: TXmlElement;
begin
  inherited Create(Owner);
  FTabs := TRibbonList<TRibbonTab>.Create(Owner, True);
  FContextualTabs := TRibbonList<TRibbonTabGroup>.Create(Owner, True);
  FSizeDefinitions := TRibbonList<TRibbonRibbonSizeDefinition>.Create(Owner, True);
  FName := E.AttributeAsString[AN_NAME];
  S := E.AttributeAsString[AN_GROUP_SPACING];
  if (S = ES_SMALL) or (S = '') then
    FGroupSpacing := gsSmall
  else if (S = ES_MEDIUM) then
    FGroupSpacing := gsMedium
  else if (S = ES_LARGE) then
    FGroupSpacing := gsLarge
  else
    Error(E, RS_INVALID_GROUP_SPACING);

  for C in E do
  begin
    if (C.Name = EN_RIBBON_SIZE_DEFINITIONS) then
    begin
      for GC in C do
      begin
        if (GC.Name <> EN_SIZE_DEFINITION) then
          Error(GC, RS_ELEMENT_EXPECTED, [EN_SIZE_DEFINITION, GC.Name]);
        FSizeDefinitions.Add(TRibbonRibbonSizeDefinition.Create(Owner, GC));
      end;
    end
    else if (C.Name = EN_RIBBON_APPLICATION_MENU) then
    begin
      if (Assigned(FApplicationMenu)) then
        Error(C, RS_MULTIPLE_ELEMENTS, [E.Name, C.Name]);
      FApplicationMenu := TRibbonApplicationMenu.Create(Owner, C);
    end
    else if (C.Name = EN_RIBBON_HELP_BUTTON) then
    begin
      if (C.ChildCount > 1) then
        Error(C, RS_MULTIPLE_ELEMENTS, [E.Name, C.Name]);
      if (C.ChildCount = 1) then
      begin
        GC := C[0];
        if (GC.Name <> EN_HELP_BUTTON) then
          Error(C, RS_ELEMENT_EXPECTED, [EN_HELP_BUTTON, GC.Name]);
        FHelpButton := TRibbonHelpButton.Create(Owner, GC)
      end;
    end
    else if (C.Name = EN_RIBBON_TABS) then
    begin
      if (FTabs.Count > 0) then
        Error(C, RS_MULTIPLE_ELEMENTS, [E.Name, C.Name]);
      for GC in C do
        FTabs.Add(TRibbonTab.Create(Owner, GC));
    end
    else if (C.Name = EN_RIBBON_CONTEXTUAL_TABS) then
    begin
      if (C.ChildCount = 0) then
        Error(C, RS_REQUIRED_ELEMENT, [C.Name, EN_TAB_GROUP]);
      for GC in C do
      begin
        if (GC.Name <> EN_TAB_GROUP) then
          Error(GC, RS_ELEMENT_EXPECTED, [EN_TAB_GROUP, GC.Name]);
        FContextualTabs.Add(TRibbonTabGroup.Create(Owner, GC));
      end;
    end
    else if (C.Name = EN_RIBBON_QUICK_ACCESS_TOOLBAR) then
    begin
      if Assigned(FQuickAccessToolbar) then
        Error(C, RS_MULTIPLE_ELEMENTS, [E.Name, C.Name]);
      if (C.ChildCount <> 1) then
        Error(C, RS_SINGLE_ELEMENT, [C.Name, EN_QUICK_ACCESS_TOOLBAR]);
      GC := C[0];
      if (GC.Name <> EN_QUICK_ACCESS_TOOLBAR) then
        Error(GC, RS_ELEMENT_EXPECTED, [EN_QUICK_ACCESS_TOOLBAR, GC.Name]);
      FQuickAccessToolbar := TRibbonQuickAccessToolbar.Create(Owner, GC);
    end
    else
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;

  if (FApplicationMenu = nil) then
    FApplicationMenu := TRibbonApplicationMenu.Create(Owner);
  if (FQuickAccessToolbar = nil) then
    FQuickAccessToolbar := TRibbonQuickAccessToolbar.Create(Owner);
  if (FHelpButton = nil) then
    FHelpButton := TRibbonHelpButton.Create(Owner);
end;

function TRibbonViewRibbon.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj is TRibbonRibbonSizeDefinition) then
    Result := FSizeDefinitions.Remove(TRibbonRibbonSizeDefinition(Obj))
  else if (Obj is TRibbonTab) then
    Result := FTabs.Remove(TRibbonTab(Obj))
  else if (Obj is TRibbonTabGroup) then
    Result := FContextualTabs.Remove(TRibbonTabGroup(Obj))
  else
    Result := inherited Delete(Obj);
end;

destructor TRibbonViewRibbon.Destroy;
begin
  FTabs.Free;
  FContextualTabs.Free;
  FSizeDefinitions.Free;
  FApplicationMenu.Free;
  FHelpButton.Free;
  FQuickAccessToolbar.Free;
  inherited;
end;

function TRibbonViewRibbon.DisplayName: String;
begin
  if (FName <> '') then
    Result := FName
  else
    Result := inherited DisplayName;
end;

class function TRibbonViewRibbon.ObjectType: TRibbonObjectType;
begin
  Result := otViewRibbon;
end;

function TRibbonViewRibbon.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonRibbonSizeDefinition) then
    Result := FSizeDefinitions.Reorder(Child, Direction)
  else if (Child is TRibbonTabGroup) then
    Result := FContextualTabs.Reorder(Child, Direction)
  else if (Child is TRibbonTab) then
    Result := FTabs.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonViewRibbon.Save(const Writer: TXmlWriter);
const
  GROUP_SPACINGS: array [TRibbonGroupSpacing] of String = (
    ES_SMALL, ES_MEDIUM, ES_LARGE);
var
  Tab: TRibbonTab;
  TabGroup: TRibbonTabGroup;
  SizeDef: TRibbonRibbonSizeDefinition;
begin
  Writer.WriteStartElement(EN_RIBBON);

  if (FName <> '') then
    Writer.WriteAttribute(AN_NAME, FName);
  if (FGroupSpacing <> gsSmall) then
    Writer.WriteAttribute(AN_GROUP_SPACING, GROUP_SPACINGS[FGroupSpacing]);

  if (FSizeDefinitions.Count > 0) then
  begin
    Writer.WriteStartElement(EN_RIBBON_SIZE_DEFINITIONS);
    for SizeDef in FSizeDefinitions do
      SizeDef.Save(Writer);
    Writer.WriteEndElement;
  end;

  if Assigned(FApplicationMenu) then
    FApplicationMenu.Save(Writer);

  if Assigned(FHelpButton) then
    FHelpButton.Save(Writer);

  if Assigned(FQuickAccessToolbar) then
    FQuickAccessToolbar.Save(Writer);

  if (FContextualTabs.Count > 0) then
  begin
    Writer.WriteStartElement(EN_RIBBON_CONTEXTUAL_TABS);
    for TabGroup in FContextualTabs do
      TabGroup.Save(Writer);
    Writer.WriteEndElement;
  end;

  if (FTabs.Count > 0) then
  begin
    Writer.WriteStartElement(EN_RIBBON_TABS);
    for Tab in FTabs do
      Tab.Save(Writer);
    Writer.WriteEndElement;
  end;

  Writer.WriteEndElement;
end;

{ TRibbonMiniToolbar }

constructor TRibbonMiniToolbar.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FMenuGroups := TRibbonList<TRibbonMiniToolbarMenuGroup>.Create(Owner, True);
  FMenuGroups.Add(TRibbonMiniToolbarMenuGroup.Create(Owner));
end;

function TRibbonMiniToolbar.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType = otMiniToolbarMenuGroup) then
  begin
    Result := TRibbonMiniToolbarMenuGroup.Create(Owner);
    FMenuGroups.Add(TRibbonMiniToolbarMenuGroup(Result));
  end
  else
    Result := inherited AddNew(ObjType);
end;

function TRibbonMiniToolbar.CanReorder: Boolean;
begin
  Result := True;
end;

constructor TRibbonMiniToolbar.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  C: TXmlElement;
begin
  inherited Create(Owner);
  FMenuGroups := TRibbonList<TRibbonMiniToolbarMenuGroup>.Create(Owner, True);
  FName := E.AttributeAsString[AN_NAME];
  if (E.ChildCount = 0) then
    Error(E, RS_REQUIRED_ELEMENT, [E.Name, EN_MENU_GROUP]);
  for C in E do
  begin
    if (C.Name <> EN_MENU_GROUP) then
      Error(C, RS_ELEMENT_EXPECTED, [EN_MENU_GROUP, C.Name]);
    FMenuGroups.Add(TRibbonMiniToolbarMenuGroup.Create(Owner, C));
  end;
end;

function TRibbonMiniToolbar.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj is TRibbonMiniToolbarMenuGroup) then
  begin
    if (FMenuGroups.Count <= 1) then
      Error(nil, RS_MIN_MINI_TOOLBAR);
    Result := FMenuGroups.Remove(TRibbonMiniToolbarMenuGroup(Obj))
  end
  else
    Result := inherited Delete(Obj);
end;

destructor TRibbonMiniToolbar.Destroy;
begin
  FMenuGroups.Free;
  inherited;
end;

function TRibbonMiniToolbar.DisplayName: String;
begin
  Result := FName;
  if (Result = '') then
    Result := RS_MINI_TOOLBAR;
end;

class function TRibbonMiniToolbar.ObjectType: TRibbonObjectType;
begin
  Result := otMiniToolbar;
end;

function TRibbonMiniToolbar.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonMiniToolbarMenuGroup) then
    Result := FMenuGroups.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonMiniToolbar.Save(const Writer: TXmlWriter);
var
  Group: TRibbonMiniToolbarMenuGroup;
begin
  Writer.WriteStartElement(EN_MINI_TOOLBAR);

  if (FName <> '') then
    Writer.WriteAttribute(AN_NAME, FName);

  for Group in FMenuGroups do
    Group.Save(Writer);

  Writer.WriteEndElement;
end;

procedure TRibbonMiniToolbar.SaveRef(const Writer: TXmlWriter);
begin
  if (FName <> '') then
    Writer.WriteAttribute(AN_MINI_TOOLBAR, FName);
end;

{ TRibbonContextMenu }

constructor TRibbonContextMenu.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FMenuGroups := TRibbonList<TRibbonMenuGroup>.Create(Owner, True);
  FMenuGroups.Add(TRibbonMenuGroup.Create(Owner));
end;

function TRibbonContextMenu.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType = otMenuGroup) then
  begin
    Result := TRibbonMenuGroup.Create(Owner);
    FMenuGroups.Add(TRibbonMenuGroup(Result));
  end
  else
    Result := inherited AddNew(ObjType);
end;

function TRibbonContextMenu.CanReorder: Boolean;
begin
  Result := True;
end;

constructor TRibbonContextMenu.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  C: TXmlElement;
begin
  inherited Create(Owner);
  FMenuGroups := TRibbonList<TRibbonMenuGroup>.Create(Owner, True);
  FName := E.AttributeAsString[AN_NAME];
  if (E.ChildCount = 0) then
    Error(E, RS_REQUIRED_ELEMENT, [E.Name, EN_MENU_GROUP]);
  for C in E do
  begin
    if (C.Name <> EN_MENU_GROUP) then
      Error(C, RS_ELEMENT_EXPECTED, [EN_MENU_GROUP, C.Name]);
    FMenuGroups.Add(TRibbonMenuGroup.Create(Owner, C));
  end;
end;

function TRibbonContextMenu.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj is TRibbonMenuGroup) then
  begin
    if (FMenuGroups.Count <= 1) then
      Error(nil, RS_MIN_CONTEXT_MENU);
    Result := FMenuGroups.Remove(TRibbonMenuGroup(Obj))
  end
  else
    Result := inherited Delete(Obj);
end;

destructor TRibbonContextMenu.Destroy;
begin
  FMenuGroups.Free;
  inherited;
end;

function TRibbonContextMenu.DisplayName: String;
begin
  Result := FName;
  if (Result = '') then
    Result := RS_CONTEXT_MENU;
end;

class function TRibbonContextMenu.ObjectType: TRibbonObjectType;
begin
  Result := otContextMenu;
end;

function TRibbonContextMenu.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonMenuGroup) then
    Result := FMenuGroups.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonContextMenu.Save(const Writer: TXmlWriter);
var
  Group: TRibbonMenuGroup;
begin
  Writer.WriteStartElement(EN_CONTEXT_MENU);

  if (FName <> '') then
    Writer.WriteAttribute(AN_NAME, FName);

  for Group in FMenuGroups do
    Group.Save(Writer);

  Writer.WriteEndElement;
end;

procedure TRibbonContextMenu.SaveRef(const Writer: TXmlWriter);
begin
  if (FName <> '') then
    Writer.WriteAttribute(AN_CONTEXT_MENU, FName);
end;

{ TRibbonContextMap }

constructor TRibbonContextMap.Create(const Owner: TRibbonDocument;
  const Popup: TRibbonViewContextPopup);
begin
  inherited Create(Owner);
  FContextPopup := Popup;
end;

function TRibbonContextMap.CanReorder: Boolean;
begin
  Result := True;
end;

constructor TRibbonContextMap.Create(const Owner: TRibbonDocument;
  const Popup: TRibbonViewContextPopup; const E: TXmlElement);
begin
  inherited Create(Owner, E);
  FContextPopup := Popup;
  FContextMenuName := E.AttributeAsString[AN_CONTEXT_MENU];
  FMiniToolbarName := E.AttributeAsString[AN_MINI_TOOLBAR];
end;

destructor TRibbonContextMap.Destroy;
begin
  if Assigned(FContextMenuRef) then
    FContextMenuRef.RemoveFreeNotification(Self);
  if Assigned(FMiniToolbarRef) then
    FMiniToolbarRef.RemoveFreeNotification(Self);
  inherited;
end;

function TRibbonContextMap.DisplayName: String;
var
  Menu, Toolbar: String;
begin
  if Assigned(FContextMenuRef) then
    Menu := FContextMenuRef.DisplayName
  else
    Menu := FContextMenuName;

  if Assigned(FMiniToolbarRef) then
    Toolbar := FMiniToolbarRef.DisplayName
  else
    Toolbar := FMiniToolbarName;

  if (Menu <> '') then
  begin
    if (Toolbar <> '') then
      Result := Menu + '<->' + Toolbar
    else
      Result := Menu;
  end
  else
  begin
    if (Toolbar <> '') then
      Result := Toolbar
    else
      Result := RS_CONTEXT_MAP;
  end;
end;

procedure TRibbonContextMap.FixupReferences;
begin
  FContextMenuRef := Owner.FindContextMenu(FContextMenuName);
  if Assigned(FContextMenuRef) then
    FContextMenuRef.FreeNotification(Self);
  FMiniToolbarRef := Owner.FindMiniToolbar(FMiniToolbarName);
  if Assigned(FMiniToolbarRef) then
    FMiniToolbarRef.FreeNotification(Self);
end;

procedure TRibbonContextMap.FreeNotify(const Obj: TRibbonObject);
begin
  inherited;
  if (Obj = FContextMenuRef) then
    FContextMenuRef := nil
  else if (Obj = FMiniToolbarRef) then
    FMiniToolbarRef := nil;
end;

class function TRibbonContextMap.ObjectType: TRibbonObjectType;
begin
  Result := otContextMap;
end;

procedure TRibbonContextMap.Save(const Writer: TXmlWriter);
begin
  Writer.WriteStartElement(EN_CONTEXT_MAP);
  inherited;
  if Assigned(FContextMenuRef) then
    FContextMenuRef.SaveRef(Writer);
  if Assigned(FMiniToolbarRef) then
    FMiniToolbarRef.SaveRef(Writer);
  Writer.WriteEndElement;
end;

procedure TRibbonContextMap.SetContextMenuRef(const Value: TRibbonContextMenu);
begin
  if (Value <> FContextMenuRef) then
  begin
    if Assigned(FContextMenuRef) then
      FContextMenuRef.RemoveFreeNotification(Self);

    FContextMenuRef := Value;

    if Assigned(FContextMenuRef) then
    begin
      FContextMenuRef.FreeNotification(Self);
      FContextMenuName := FContextMenuRef.Name;
    end
    else
      FContextMenuName := '';
  end;
end;

procedure TRibbonContextMap.SetMiniToolbarRef(const Value: TRibbonMiniToolbar);
begin
  if (Value <> FMiniToolbarRef) then
  begin
    if Assigned(FMiniToolbarRef) then
      FMiniToolbarRef.RemoveFreeNotification(Self);

    FMiniToolbarRef := Value;

    if Assigned(FMiniToolbarRef) then
    begin
      FMiniToolbarRef.FreeNotification(Self);
      FMiniToolbarName := FMiniToolbarRef.Name;
    end
    else
      FMiniToolbarName := '';
  end;
end;

{ TRibbonViewContextPopup }

constructor TRibbonViewContextPopup.Create(const Owner: TRibbonDocument);
begin
  inherited Create(Owner);
  FMiniToolbars := TRibbonList<TRibbonMiniToolbar>.Create(Owner, True);
  FContextMenus := TRibbonList<TRibbonContextMenu>.Create(Owner, True);
  FContextMaps := TRibbonList<TRibbonContextMap>.Create(Owner, True);
end;

function TRibbonViewContextPopup.AddNew(
  const ObjType: TRibbonObjectType): TRibbonObject;
begin
  if (ObjType = otMiniToolbar) then
  begin
    Result := TRibbonMiniToolbar.Create(Owner);
    FMiniToolbars.Add(TRibbonMiniToolbar(Result));
  end
  else if (ObjType = otContextMenu) then
  begin
    Result := TRibbonContextMenu.Create(Owner);
    FContextMenus.Add(TRibbonContextMenu(Result));
  end
  else if (ObjType = otContextMap) then
  begin
    Result := TRibbonContextMap.Create(Owner, Self);
    FContextMaps.Add(TRibbonContextMap(Result));
  end
  else
    Result := inherited AddNew(ObjType);
end;

constructor TRibbonViewContextPopup.Create(const Owner: TRibbonDocument;
  const E: TXmlElement);
var
  C, GC: TXmlElement;
begin
  inherited Create(Owner);
  FMiniToolbars := TRibbonList<TRibbonMiniToolbar>.Create(Owner, True);
  FContextMenus := TRibbonList<TRibbonContextMenu>.Create(Owner, True);
  FContextMaps := TRibbonList<TRibbonContextMap>.Create(Owner, True);
  for C in E do
  begin
    if (C.Name = EN_CONTEXT_POPUP_MINI_TOOLBARS) then
    begin
      if (FMiniToolbars.Count > 0) then
        Error(C, RS_MULTIPLE_ELEMENTS, [E.Name, C.Name]);
      for GC in C do
      begin
        if (GC.Name <> EN_MINI_TOOLBAR) then
          Error(GC, RS_ELEMENT_EXPECTED, [EN_MINI_TOOLBAR, GC.Name]);
        FMiniToolbars.Add(TRibbonMiniToolbar.Create(Owner, GC));
      end;
    end
    else if (C.Name = EN_CONTEXT_POPUP_CONTEXT_MENUS) then
    begin
      if (FContextMenus.Count > 0) then
        Error(C, RS_MULTIPLE_ELEMENTS, [E.Name, C.Name]);
      for GC in C do
      begin
        if (GC.Name <> EN_CONTEXT_MENU) then
          Error(GC, RS_ELEMENT_EXPECTED, [EN_CONTEXT_MENU, GC.Name]);
        FContextMenus.Add(TRibbonContextMenu.Create(Owner, GC));
      end;
    end
    else if (C.Name = EN_CONTEXT_POPUP_CONTEXT_MAPS) then
    begin
      if (FContextMaps.Count > 0) then
        Error(C, RS_MULTIPLE_ELEMENTS, [E.Name, C.Name]);
      for GC in C do
      begin
        if (GC.Name <> EN_CONTEXT_MAP) then
          Error(GC, RS_ELEMENT_EXPECTED, [EN_CONTEXT_MAP, GC.Name]);
        FContextMaps.Add(TRibbonContextMap.Create(Owner, Self, GC));
      end;
    end
    else
      Error(C, RS_UNSUPPORTED_CHILD_ELEMENT, [C.Name, E.Name]);
  end;
end;

function TRibbonViewContextPopup.Delete(const Obj: TRibbonObject): Boolean;
begin
  if (Obj is TRibbonMiniToolbar) then
    Result := FMiniToolbars.Remove(TRibbonMiniToolbar(Obj))
  else if (Obj is TRibbonContextMenu) then
    Result := FContextMenus.Remove(TRibbonContextMenu(Obj))
  else if (Obj is TRibbonContextMap) then
    Result := FContextMaps.Remove(TRibbonContextMap(Obj))
  else
    Result := inherited Delete(Obj);
end;

destructor TRibbonViewContextPopup.Destroy;
begin
  FMiniToolbars.Free;
  FContextMenus.Free;
  FContextMaps.Free;
  inherited;
end;

procedure TRibbonViewContextPopup.FixupContextMaps;
var
  Map: TRibbonContextMap;
begin
  for Map in FContextMaps do
    Map.FixupReferences;
end;

class function TRibbonViewContextPopup.ObjectType: TRibbonObjectType;
begin
  Result := otViewContextPopup;
end;

function TRibbonViewContextPopup.Reorder(const Child: TRibbonObject;
  const Direction: Integer): Boolean;
begin
  if (Child is TRibbonMiniToolbar) then
    Result := FMiniToolbars.Reorder(Child, Direction)
  else if (Child is TRibbonContextMenu) then
    Result := FContextMenus.Reorder(Child, Direction)
  else if (Child is TRibbonContextMap) then
    Result := FContextMaps.Reorder(Child, Direction)
  else
    Result := inherited Reorder(Child, Direction)
end;

procedure TRibbonViewContextPopup.Save(const Writer: TXmlWriter);
var
  MiniToolbar: TRibbonMiniToolbar;
  ContextMenu: TRibbonContextMenu;
  ContextMap: TRibbonContextMap;
begin
  Writer.WriteStartElement(EN_CONTEXT_POPUP);

  if (FMiniToolbars.Count > 0) then
  begin
    Writer.WriteStartElement(EN_CONTEXT_POPUP_MINI_TOOLBARS);
    for MiniToolbar in FMiniToolbars do
      MiniToolbar.Save(Writer);
    Writer.WriteEndElement;
  end;

  if (FContextMenus.Count > 0) then
  begin
    Writer.WriteStartElement(EN_CONTEXT_POPUP_CONTEXT_MENUS);
    for ContextMenu in FContextMenus do
      ContextMenu.Save(Writer);
    Writer.WriteEndElement;
  end;

  if (FContextMaps.Count > 0) then
  begin
    Writer.WriteStartElement(EN_CONTEXT_POPUP_CONTEXT_MAPS);
    for ContextMap in FContextMaps do
      ContextMap.Save(Writer);
    Writer.WriteEndElement;
  end;

  Writer.WriteEndElement;
end;

end.
