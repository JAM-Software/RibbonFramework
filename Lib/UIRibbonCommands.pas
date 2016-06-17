unit UIRibbonCommands;

interface

uses
  Generics.Collections,
  Windows,
  Messages,
  Graphics,
  Classes,
  ActiveX,
  Rtti,
  ComCtrls,
  RichEdit,
  PropSys,
  ActnList,
  UIRibbonApi,
  PNGImage,
  Vcl.ImgList,
  System.UITypes;

const
  WM_RIBBONCMD_NOTIFY = WM_USER + 241;

type
  TUICommandType = (ctUnknown, ctGroup, ctAction, ctAnchor, ctContext,
    ctCollection, ctCommandCollection, ctDecimal, ctBoolean, ctFont,
    ctRecentItems, ctColorAnchor, ctColorCollection);

  TUICommandVerb = (cvExecute, cvPreview, cvCancelPreview);

  TUICommandInvalidation = (ciState, ciValue, ciProperty, ciAllProperties);
  TUICommandInvalidations = set of TUICommandInvalidation;

  TUISwatchColorType = (ctNoColor, ctAutomatic, ctRgb);

  TUIContextAvailability = (caNotAvailable, caAvailable, caActive);

  TUIProperty = (
    upNone = -1,
    upUnknown,

    { Common Properties }
    upEnabled = $0001,
    upLabelDescription,
    upKeytip,
    upLabel,
    upTooltipDescription,
    upTooltipTitle,
    upLargeImage,
    upLargeHighContrastImage,
    upSmallImage,
    upSmallHighContrastImage,

    { Collection Properties }
    upCommandId = $0064,
    upItemsSource,
    upCategories,
    upCategoryId,
    upSelectedItem,
    upCommandType,
    upItemImage,

    { Control Properties }
    upBooleanValue = $00C8,
    upDecimalValue,
    upStringValue,
    upMaxValue,
    upMinValue,
    upIncrement,
    upDecimalPlaces,
    upFormatString,
    upRepresentativeString,

    { Font Control Properties }
    upFontProperties = $012C,
    upFontPropertiesFamily,
    upFontPropertiesSize,
    upFontPropertiesBold,
    upFontPropertiesItalic,
    upFontPropertiesUnderline,
    upFontPropertiesStrikethrough,
    upFontPropertiesVerticalPositioning,
    upFontPropertiesForegroundColor,
    upFontPropertiesBackgroundColor,
    upFontPropertiesForegroundColorType,
    upFontPropertiesBackgroundColorType,
    upFontPropertiesChangedProperties,
    upFontPropertiesDeltaSize,

    { Application Menu Properties }
    upRecentItems = $015E,
    upPinned,

    { Color Picker Properties }
    upColor = $0190,
    upColorType,
    upColorMode,
    upThemeColorsCategoryLabel,
    upStandardColorsCategoryLabel,
    upRecentColorsCategoryLabel,
    upAutomaticColorLabel,
    upNoColorLabel,
    upMoreColorsLabel,
    upThemeColors,
    upStandardColors,
    upThemeColorsTooltips,
    upStandardColorsTooltips,

    { Ribbon Properties }
    upViewable = $03E8,
    upMinimized,
    upQuickAccessToolbarDock,

    { Contextual Tabset Properties }
    upContextAvailable = $044C,

    { Color Properties }
    upGlobalBackgroundColor = $07D0,
    upGlobalHighlightColor,
    upGlobalTextColor);

{$REGION 'Forward Declarations'}
type
  TUICollection = class;
{$ENDREGION 'Forward Declarations'}

  TUICommandExecutionProperties = class
  {$REGION 'Internal Declarations'}
  private
    FPropertySet: IUISimplePropertySet;
  {$ENDREGION 'Internal Declarations'}
  public
    function GetValue(const Prop: TUIProperty; out Value: Cardinal): Boolean; overload;
    function GetValue(const Prop: TUIProperty; out Value: String): Boolean; overload;
  end;

{$REGION 'Abstract base command'}
  TUIImage = class;

  TUICommandUpdateImageEvent = procedure (Sender: TObject; const PropKey: TUIPropertyKey;
    out NewValue: TPropVariant; var Handled: boolean) of object;

  TUICommandUpdateHintEvent = procedure (Sender: TObject; const Value: string) of object;

  { Abstract base class for Ribbon Commands. }
  TUICommand = class abstract(TComponent, IUICommandHandler)
  {$REGION 'Internal Declarations'}
  strict private
    const NOTIFY_CACHED_PROPERTIES = 31;
  protected
    class var FProperties: TUICommandExecutionProperties;
  strict protected
    FFramework: IUIFramework;
    FCommandId: Cardinal;
    FTag: Integer;
    FAlive: Boolean;
    FShortCut: TShortCut;
    FCaption: String;
    FKeytip: String;
    FTooltipTitle: String;
    FTooltipDescription: String;
    FLargeImage: TUIImage;
    FSmallImage: TUIImage;
    FLargeHighContrastImage: TUIImage;
    FSmallHighContrastImage: TUIImage;
    FCachedProperties: TDictionary<TUIPropertyKey, TValue>;
    FValidProperties: set of (vpCaption, vpKeytip, vpTooltipTitle,
      vpTooltipDescription, vpLabelDescription, vpMinValue, vpMaxValue,
      vpIncrement, vpDecimalPlaces, vpRepresentativeString, vpFormatString);
    FActionLink: TActionLink;
    FOnUpdateHint: TUICommandUpdateHintEvent;
    FOnUpdateImage: TUICommandUpdateImageEvent;
    procedure SetAlive(const Value: Boolean);
  strict private
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetCaption(const Value: String);
    procedure SetKeytip(const Value: String);
    procedure SetTooltipTitle(const Value: String);
    procedure SetTooltipDescription(const Value: String);
    procedure SetSmallImage(const Value: TUIImage);
    function GetActionLink: TActionLink;
  private
    { IInterface }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  private
    { IUICommandHandler }
    function Execute(CommandId: UInt32; Verb: _UIExecutionVerb;
      Key: PUIPropertyKey; CurrentValue: PPropVariant;
      CommandExecutionProperties: IUISimplePropertySet): HRESULT; stdcall;

    function UpdateProperty(CommandId: UInt32; const Key: TUIPropertyKey;
      CurrentValue: PPropVariant; out NewValue: TPropVariant): HRESULT; stdcall;
    procedure SetLargeImage(const Value: TUIImage);
  strict private
    procedure ImageChanged(Sender: TObject);
  strict protected
    procedure GetPropertyValue(const Key: TUIPropertyKey; out Value: Boolean); overload;
    procedure GetPropertyValue(const Key: TUIPropertyKey; out Value: Cardinal); overload;
    procedure GetPropertyValue(const Key: TUIPropertyKey; out Value: Integer); overload;
    procedure GetPropertyValue(const Key: TUIPropertyKey; out Value: String); overload;
    procedure GetPropertyValue(const Key: TUIPropertyKey; out Value: Double); overload;
    procedure SetPropertyValue(const Key: TUIPropertyKey; const Value: Boolean); overload;
    procedure SetPropertyValue(const Key: TUIPropertyKey; const Value: Cardinal); overload;
    procedure SetPropertyValue(const Key: TUIPropertyKey; const Value: Integer); overload;
    procedure SetPropertyValue(const Key: TUIPropertyKey; const Value: String); overload;
    procedure SetPropertyValue(const Key: TUIPropertyKey; const Value: Double); overload;
    procedure CachePropertyValue(const Key: TUIPropertyKey; const Value: TValue);
  strict protected
    procedure DoExecute(const Prop: TUIProperty; const Verb: TUICommandVerb;
      const CurrentValue: PPropVariant; var Result: HRESULT); virtual; abstract;
    procedure DoUpdate(const Prop: TUIProperty; const CurrentValue: PPropVariant;
      out NewValue: TPropVariant; var Result: HRESULT); virtual;
    function CreateActionLink: TActionLink; virtual; abstract;
  protected
    procedure Notify(const Flag: Integer); virtual;

    property Alive: Boolean read FAlive write SetAlive;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const Ribbon: TObject{TUIRibbon}; const CommandId: Cardinal); reintroduce; virtual;
    destructor Destroy; override;

    /// <summary>
    /// Allows to assign the values of a VCL action to a ribbon command
    /// </summary>
    /// <param name="pAction">The action from whcih teh values should be taken.</param>
    /// <remarks></remarks>
    procedure Assign(const pAction: TCustomAction); reintroduce;

    { The command type for this class. You can inspect this before casting a
      command object to its actual type. For example. if CommandType returns
      ctBoolean, you can safely cast this object to a TUICommandBoolean. }
    class function CommandType: TUICommandType; virtual; abstract;

    { Sets a keyboard shortcut for this command. Key is a VK_xxx constant or
      a character. See also the ShortCut property.
      NOTE: Not all command types support keyboard shortcuts! }
    procedure SetShortCut(const Shift: TShiftState; const Key: Word); overload;
    procedure SetShortCut(const Shift: TShiftState; const Key: Char); overload;

    { Checks if this command handles the given ShortCut. If so, it executes
      the command and returns True. Otherwise, it returns False.
      You usually don't need to call this method yourself. If your form
      descends from TUIRibbonForm, then this is taken care of automatically. }
    function HandleShortCut(const ShortCut: TShortCut): Boolean; virtual;

    { A user-defineable tag }
    property Tag: Integer read FTag write FTag;

    { The Id of the command as specified in the Ribbon markup }
    property CommandId: Cardinal read FCommandId;

    (************************************************************************
     * The properties below are available for most (but not all) command
     * types.
     * NOTE: These properties are NOT initialized from the Ribbon Markup (the
     * framework doesn't provide this information). So the initial value of
     * these properties is always 0/nil/empty. }
     ************************************************************************)

    { Whether the command is enabled }
    property Enabled: Boolean read GetEnabled write SetEnabled;

    { Caption of the command. }
    property Caption: String read FCaption write SetCaption;

    { Keytip of the command. The keytip is displayed when the user presses the
      Alt key to navigate through the ribbon. }
    property Keytip: String read FKeyTip write SetKeytip;

    { A keyboard shortcut that can be used to execute this command.
      You can use the ShortCut function in the Menus unit to create a ShortCut.
      Alternatively, you can use the SetShortCut method.
      NOTE: Not all command types support keyboard shortcuts! }
    property ShortCut: TShortCut read FShortCut write FShortCut;

    { Title of the tooltip for this command (appears at the top of the tooltip
      popup in bold). }
    property TooltipTitle: String read FTooltipTitle write SetTooltipTitle;

    { Description of the tooltip for this command (appears below the
      TooltipTitle of the tooltip popup). }
    property TooltipDescription: String read FTooltipDescription write SetTooltipDescription;

    { The large image to display when the button/control is in large mode. }
    property LargeImage: TUIImage read FLargeImage write SetLargeImage;

    { The small image to display when the button/control is in small mode. }
    property SmallImage: TUIImage read FSmallImage write SetSmallImage;

    { The large image to display when the button/control is in large mode and
      the system is in High Contast Mode (Alt+Left Shift+PrtScn).
      See MSDN documentation on guidelines for the image format. }
    property LargeHighContrastImage: TUIImage read FLargeHighContrastImage;

    { The small image to display when the button/control is in small mode and
      the system is in High Contast Mode (Alt+Left Shift+PrtScn).
      See MSDN documentation on guidelines for the image format. }
    property SmallHighContrastImage: TUIImage read FSmallHighContrastImage;

    { Returns an actionlink that you can use to link this command to a
      delphi action. To link the command, set the Action property of this action
      link to the action that needs to be executed when to command is
      executed. For example, say you have a UICommand for pasting text from
      the clipboard (called FCmdPaste) and you also have a TAction component for
      pasting text from the clipboard (called ActionPaste). To link the two
      together, you would use:
        FCmdPaste.ActionLink.Action := ActionPaste. }
    property ActionLink: TActionLink read GetActionLink;

    { Can be used to dynamically generate an image for a command.}
    property OnUpdateImage: TUICommandUpdateImageEvent read FOnUpdateImage write
      FOnUpdateImage;

    { Allows you to override automation action.Hint changes.
      If assigned, event handler should process hint and set up command properties
      accordingly. If not assigned , internal processing is done in
      UIRibbonActions.pas/TUICommandActionLink.SetHint. }
    property OnUpdateHint: TUICommandUpdateHintEvent read FOnUpdateHint write
      FOnUpdateHint;

    (************************************************************************
     * A note about command events
     * ---------------------------
     * All events have a first parameter Command that identified the TUICommand
     * object that fired the event. Usually, there is also a Verb parameter
     * which specifies how the action should be performed:
     *   cvExecute: Execute the command.
     *   cvPreview: Show a preview of the command.
     *   cvCancelPreview: Cancel the preview of the command.
     * There may also be a Properties parameter, which can contain additional
     * properties.
     * Finally, there may be other parameters specific to the type of event.
     ************************************************************************)
  end;
  TUICommandClass = class of TUICommand;
{$ENDREGION 'Abstract base command'}

{$REGION 'Group command'}
  { Group command. Used by Groups on a ribbon tab. }
  TUICommandGroup = class(TUICommand)
  {$REGION 'Internal Declarations'}
  strict protected
    procedure DoExecute(const Prop: TUIProperty; const Verb: TUICommandVerb;
      const CurrentValue: PPropVariant; var Result: HRESULT); override;
    function CreateActionLink: TActionLink; override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function CommandType: TUICommandType; override;

    { No additional declarations }
  end;
{$ENDREGION 'Group command'}

{$REGION 'Action command'}
  TUICommandAction = class;

  TUICommandActionEventArgs = record
    Command: TUICommandAction;
    Verb: TUICommandVerb;
    Properties: TUICommandExecutionProperties;
  end;

  TUICommandActionExecuteEvent = procedure(const Args: TUICommandActionEventArgs) of object;

  { Action command. Uses by Buttons and Help Buttons. }
  TUICommandAction = class(TUICommand)
  {$REGION 'Internal Declarations'}
  strict private
    FLabelDescription: String;
    FOnExecute: TUICommandActionExecuteEvent;
  strict private
    procedure SetLabelDescription(const Value: String);
  strict protected
    procedure DoExecute(const Prop: TUIProperty; const Verb: TUICommandVerb;
      const CurrentValue: PPropVariant; var Result: HRESULT); override;
    procedure DoUpdate(const Prop: TUIProperty; const CurrentValue: PPropVariant;
      out NewValue: TPropVariant; var Result: HRESULT); override;
    function CreateActionLink: TActionLink; override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function CommandType: TUICommandType; override;
    function HandleShortCut(const ShortCut: TShortCut): Boolean; override;

    { Description of the label (caption). This description is only used for
      regular Buttons that appear on the right side in the Application Menu.
      The description appears below the button caption.
      NOTE: This property is NOT initialized from the Ribbon Markup (the
      framework doesn't provide this information). So the initial value is
      always empty. }
    property LabelDescription: String read FLabelDescription write SetLabelDescription;

    { Is fired when the action is executed. }
    property OnExecute: TUICommandActionExecuteEvent read FOnExecute write FOnExecute;
  end;
{$ENDREGION 'Action command'}

{$REGION 'Collection command'}
  TUICommandCollection = class;

  TUICommandCollectionEventArgs = record
    Command: TUICommandCollection;
    Verb: TUICommandVerb;
    ItemIndex: Integer;
    Properties: TUICommandExecutionProperties;
  end;

  TUICommandCollectionSelectEvent = procedure(const Args: TUICommandCollectionEventArgs) of object;

  { Collection Command. Used for Item Collections and Command Collections.
    Items Collections are used by Drop-Down Galleries, In-Ribbon Galleries,
      Split Button Galleries and Combo Boxes.
    Command Collections are used by Drop-Down Galleries, In-Ribbon Galleries,
      Split Button Galleries and the Quick Access Toolbar. }
  TUICommandCollection = class(TUICommand)
  {$REGION 'Internal Declarations'}
  strict private
    const NOTIFY_ITEMS = 1;
    const NOTIFY_CATEGORIES = 2;
  strict private
    FItems: TUICollection;
    FCategories: TUICollection;
    FRepresentativeString: String;
    FOnSelect: TUICommandCollectionSelectEvent;
    function GetSelectedItem: Integer;
    procedure SetSelectedItem(const Value: Integer);
    function GetText: String;
    procedure SetText(const Value: String);
    function GetChecked: Boolean;
    procedure SetChecked(const Value: Boolean);
    procedure SetRepresentativeString(const Value: String);
  strict private
    procedure ItemsChange(const Collection: TUICollection; const Immediate: Boolean);
    procedure CategoriesChange(const Collection: TUICollection; const Immediate: Boolean);
  strict protected
    procedure DoExecute(const Prop: TUIProperty; const Verb: TUICommandVerb;
      const CurrentValue: PPropVariant; var Result: HRESULT); override;
    procedure DoUpdate(const Prop: TUIProperty; const CurrentValue: PPropVariant;
      out NewValue: TPropVariant; var Result: HRESULT); override;
    function CreateActionLink: TActionLink; override;
  protected
    procedure Notify(const Flag: Integer); override;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const Ribbon: TObject; const CommandId: Cardinal); override;
    destructor Destroy; override;

    class function CommandType: TUICommandType; override;
    function HandleShortCut(const ShortCut: TShortCut): Boolean; override;

    { The items in the gallery. You can fill this collection with
      TUIGalleryCollectionItem objects, or your own objects. The items should
      have the following properties:
      -LabelText: the text of the item. This property is only used for Item
         Galleries (not for Command Galleries).
      -Category: the ID of the category to which the item belongs (see
         Categories). Or -1 if there are no categories.
      -Image: the image to use for the item (or nil for none).
      -Command: the command to use for the item. This property is only used for
         Command Galleries (not for Item Galleries).  }
    property Items: TUICollection read FItems;

    { The categories in the gallery. Items can optionally be organized by
      categories. You can fill this collection with TUIGalleryCollectionItem
      objects, or your own objects. The categories should have the following
      properties:
      -LabelText: the caption of the category.
      -CategoryId: the ID of the category. Items with this CategoryId will be
         placed in this category. }
    property Categories: TUICollection read FCategories;

    { The index of the currently selected item.
      For Split Button Galleries, the index will be -1 if the button (top) part
      of the split button was clicked.
      For Combo Boxes, the index will be -1 if no item is selected, or text
      is entered in the combo box. }
    property SelectedItem: Integer read GetSelectedItem write SetSelectedItem;

    { For Combo Boxes, the text entered into the combo box }
    property Text: String read GetText write SetText;

    { For Combo Boxes, this string is used by the Ribbon Framework to query the
      width of the Combo Box. Set this to the longest string you forecast. The
      string is never displayed, so it could contain any characters you want.
      It is just used to calculate the width of the control: the longer the
      string, the wider the control.
      NOTE: This property is NOT initialized from the Ribbon Markup (the
      framework doesn't provide this information). So the initial value is
      always 0.0. }
    property RepresentativeString: String read FRepresentativeString write SetRepresentativeString;

    { For Split Button Galleries, whether the button is checked or not. }
    property Checked: Boolean read GetChecked write SetChecked;

    { Fired when an item in the gallery is selected. ItemIndex is the index
      of the selected item. }
    property OnSelect: TUICommandCollectionSelectEvent read FOnSelect write FOnSelect;
  end;
{$ENDREGION 'Collection command'}

{$REGION 'Decimal command'}
  TUICommandDecimal = class;

  TUICommandDecimalChangeEvent = procedure(const Command: TUICommandDecimal;
    const Verb: TUICommandVerb; const Value: Double;
    const Properties: TUICommandExecutionProperties) of object;

  { Decimal command. Uses by the Spinner control. }
  TUICommandDecimal = class(TUICommand)
  {$REGION 'Internal Declarations'}
  strict private
    FMinValue: Double;
    FMaxValue: Double;
    FIncrement: Double;
    FDecimalPlaces: Integer;
    FRepresentativeString: String;
    FFormatString: String;
    FOnChange: TUICommandDecimalChangeEvent;
  strict private
    function GetValue: Double;
    procedure SetValue(const Value: Double);
    procedure SetMaxValue(const Value: Double);
    procedure SetMinValue(const Value: Double);
    procedure SetIncrement(const Value: Double);
    procedure SetDecimalPlaces(const Value: Integer);
    procedure SetRepresentativeString(const Value: String);
    procedure SetFormatString(const Value: String);
  strict protected
    procedure DoExecute(const Prop: TUIProperty; const Verb: TUICommandVerb;
      const CurrentValue: PPropVariant; var Result: HRESULT); override;
    procedure DoUpdate(const Prop: TUIProperty; const CurrentValue: PPropVariant;
      out NewValue: TPropVariant; var Result: HRESULT); override;
    function CreateActionLink: TActionLink; override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function CommandType: TUICommandType; override;
    function HandleShortCut(const ShortCut: TShortCut): Boolean; override;

    { The currently selected value }
    property Value: Double read GetValue write SetValue;

    { Mimimum allowable value
      NOTE: This property is NOT initialized from the Ribbon Markup (the
      framework doesn't provide this information). So the initial value is
      always 0.0. }
    property MinValue: Double read FMinValue write SetMinValue;

    { Maximum allowable value
      NOTE: This property is NOT initialized from the Ribbon Markup (the
      framework doesn't provide this information). So the initial value is
      always 0.0. }
    property MaxValue: Double read FMaxValue write SetMaxValue;

    { By how much the value in the spinner control increases or decreases
      when a spinner button is clicked.
      NOTE: This property is NOT initialized from the Ribbon Markup (the
      framework doesn't provide this information). So the initial value is
      always 0.0. }
    property Increment: Double read FIncrement write SetIncrement;

    { Number of decimal places to show in the spinner control.
      NOTE: This property is NOT initialized from the Ribbon Markup (the
      framework doesn't provide this information). So the initial value is
      always 0.0. }
    property DecimalPlaces: Integer read FDecimalPlaces write SetDecimalPlaces;

    { This string is used by the Ribbon Framework to query the width of the
      Spinner control. Set this to the longest string you forecast. The string
      is never displayed, so it could contain any characters you want. It is
      just used to calculate the width of the control: the longer the string,
      the wider the control.
      NOTE: This property is NOT initialized from the Ribbon Markup (the
      framework doesn't provide this information). So the initial value is
      always 0.0. }
    property RepresentativeString: String read FRepresentativeString write SetRepresentativeString;

    { The string (eg. unit of measurement) that is displayed to the right
      of the numer in the spinner control.
      NOTE: This property is NOT initialized from the Ribbon Markup (the
      framework doesn't provide this information). So the initial value is
      always 0.0. }
    property FormatString: String read FFormatString write SetFormatString;

    { Fired when the used changes the value of the spinner }
    property OnChange: TUICommandDecimalChangeEvent read FOnChange write FOnChange;
  end;
{$ENDREGION 'Decimal command'}

{$REGION 'Boolean command'}
  TUICommandBoolean = class;

  TUICommandBooleanEventArgs = record
    Command: TUICommandBoolean;
    Verb: TUICommandVerb;
    Checked: Boolean;
    Properties: TUICommandExecutionProperties;
  end;

  TUICommandBooleanToggleEvent = procedure(const Args: TUICommandBooleanEventArgs) of object;

  { Boolean command. Used by Toggle Buttons and Check Boxes. }
  TUICommandBoolean = class(TUICommand)
  {$REGION 'Internal Declarations'}
  strict private
    FLabelDescription: String;
    FOnToggle: TUICommandBooleanToggleEvent;
    function GetChecked: Boolean;
    procedure SetChecked(const Value: Boolean);
  strict private
    procedure SetLabelDescription(const Value: String);
  private
    procedure DoToggle(const Verb: TUICommandVerb;
      const CurrentValue: PPropVariant);
  strict protected
    procedure DoExecute(const Prop: TUIProperty; const Verb: TUICommandVerb;
      const CurrentValue: PPropVariant; var Result: HRESULT); override;
    procedure DoUpdate(const Prop: TUIProperty; const CurrentValue: PPropVariant;
      out NewValue: TPropVariant; var Result: HRESULT); override;
    function CreateActionLink: TActionLink; override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function CommandType: TUICommandType; override;
    function HandleShortCut(const ShortCut: TShortCut): Boolean; override;

    { Description of the label (caption). This description is only used for
      Toggle Buttons that appear on the right side in the Application Menu.
      The description appears below the button caption.
      NOTE: This property is NOT initialized from the Ribbon Markup (the
      framework doesn't provide this information). So the initial value is
      always empty. }
    property LabelDescription: String read FLabelDescription write SetLabelDescription;

    { Whether the button/checkbox is currently checked. }
    property Checked: Boolean read GetChecked write SetChecked;

    { Fired when a button/checkbox etc. is toggled. }
    property OnToggle: TUICommandBooleanToggleEvent read FOnToggle write FOnToggle;
  end;
{$ENDREGION 'Boolean command'}

{$REGION 'Anchor command'}
  { Anchor command. Used by the Application Menu, Drop-Down Buttons, Split
    Buttons and Tabs. }
  TUICommandAnchor = class(TUICommand)
  {$REGION 'Internal Declarations'}
  strict protected
    procedure DoExecute(const Prop: TUIProperty; const Verb: TUICommandVerb;
      const CurrentValue: PPropVariant; var Result: HRESULT); override;
    function CreateActionLink: TActionLink; override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function CommandType: TUICommandType; override;

    { No additional declarations }
  end;
{$ENDREGION 'Anchor command'}

{$REGION 'Context command'}
  { Context command. Used by (contextual) Tab Groups. }
  TUICommandContext = class(TUICommand)
  {$REGION 'Internal Declarations'}
  strict private
    function GetAvailability: TUIContextAvailability;
    procedure SetAvailability(const Value: TUIContextAvailability);
  strict protected
    procedure DoExecute(const Prop: TUIProperty; const Verb: TUICommandVerb;
      const CurrentValue: PPropVariant; var Result: HRESULT); override;
    function CreateActionLink: TActionLink; override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function CommandType: TUICommandType; override;

    { Whether the contextual tab is available:
        caNotAvailable: The tab is not available for the selected object.
        caAvailable: The tab is available for the selected object, but the tab
          is not the active tab.
        caActive: The tab is available for the selected object, and the tab
          is the active tab. }
    property Availability: TUIContextAvailability read GetAvailability write SetAvailability;
  end;
{$ENDREGION 'Context command'}

{$REGION 'Font command'}
  TUIFontVerticalPositioning = (vpNotAvailable, vpDefault, vpSuperscript, vpSubscript);
  TUIFontProperty = (fpNotAvailable, fpOff, fpOn);

  TUICommandFont = class;

  { Font description as used by the TUICommandFont command }
  TUIFont = class
  {$REGION 'Internal Declarations'}
  strict private
    FOwner: TUICommandFont;
    FFamily: String;
    FSize: Single;
    FBold: TUIFontProperty;
    FItalic: TUIFontProperty;
    FUnderline: TUIFontProperty;
    FStrikethrough: TUIFontProperty;
    FVerticalPositioning: TUIFontVerticalPositioning;
    FForegroundColor: TColor;
    FBackgroundColor: TColor;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetBold(const Value: TUIFontProperty);
    procedure SetFamily(const Value: String);
    procedure SetForegroundColor(const Value: TColor);
    procedure SetItalic(const Value: TUIFontProperty);
    procedure SetSize(const Value: Single);
    procedure SetStrikethrough(const Value: TUIFontProperty);
    procedure SetUnderline(const Value: TUIFontProperty);
    procedure SetVerticalPositioning(const Value: TUIFontVerticalPositioning);
  strict private
    procedure Changed;
  private
    procedure Assign(const Source: IPropertyStore); overload;
    procedure AssignTo(const Dest: IPropertyStore); overload;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const Owner: TUICommandFont);

    { Sets the properties of this font based on a regular TFont object. }
    procedure Assign(const Source: TFont); overload;

    { Sets the properties of this font based on a TTextAttributes object (as
      used by the TRichEdit control). }
    procedure Assign(const Source: TTextAttributes); overload;

    { Sets the properties of this font based on TCharFormat2 record (also used
      by the TRichEdit control for lower level font access; contains a few
      more settings than TTextAttributes). }
    procedure Assign(const Source: TCharFormat2); overload;

    { Sets the properties of a TFont object based on this font. }
    procedure AssignTo(const Dest: TFont); overload;

    { Sets the properties of a TTextAttributes object based on this font. }
    procedure AssignTo(const Dest: TTextAttributes); overload;

    { Sets the properties of a TCharFormat2 record based on this font. }
    procedure AssignTo(out Dest: TCharFormat2); overload;

    { Name of the font. Set to an empty string to clear the font selection. }
    property Family: String read FFamily write SetFamily;

    { Size of the font in Points. You can use fractional font sizes. However,
      the actual font size will be rounded to 1/20th of a point.
      (For example, size 12.13 will be rounded to 12.15).
      This value will be 0 if no single size is selected (either no text, or
      a run of heterogeneously sized text, is selected). }
    property Size: Single read FSize write SetSize;

    { Bold, Italic, Underline and Strikethrough properties. Each can have one
      of these values:
        fpNotAvailable: The property is not available (disabled).
        fpOn: The property is set.
        fpOff: The property is not set. }
    property Bold: TUIFontProperty read FBold write SetBold;
    property Italic: TUIFontProperty read FItalic write SetItalic;
    property Underline: TUIFontProperty read FUnderline write SetUnderline;
    property Strikethrough: TUIFontProperty read FStrikethrough write SetStrikethrough;

    { Vertical positioning:
        vpNotAvailable: Vertical positioning is not available (disabled).
        vpDefault: Default (no) vertical positioning.
        vpSuperscript: Use superscript.
        vpSubscript: Use subscript. }
    property VerticalPositioning: TUIFontVerticalPositioning read FVerticalPositioning write SetVerticalPositioning;

    { The foreground (text) color. Can be a regular color, or clDefault to use
      an automatic color (usually based on the current Windows theme). }
    property ForegroundColor: TColor read FForegroundColor write SetForegroundColor;

    { The background color. Can be a regular color, or clNone to use no
      background color. }
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
  end;

  TUICommandFontEventArgs = record
    Command: TUICommandFont;
    Verb: TUICommandVerb;
    Font: TUIFont;
    Properties: TUICommandExecutionProperties;
  end;

  TUICommandFontChangedEvent = procedure(const Args: TUICommandFontEventArgs) of object;

  { Font command. Used by the Font Control. }
  TUICommandFont = class(TUICommand)
  {$REGION 'Internal Declarations'}
  strict private
    const NOTIFY_FONT = 0;
  strict private
    FFont: TUIFont;
    FOnChanged: TUICommandFontChangedEvent;
  strict protected
    procedure DoExecute(const Prop: TUIProperty; const Verb: TUICommandVerb;
      const CurrentValue: PPropVariant; var Result: HRESULT); override;

    procedure DoUpdate(const Prop: TUIProperty; const CurrentValue: PPropVariant;
      out NewValue: TPropVariant; var Result: HRESULT); override;
    function CreateActionLink: TActionLink; override;
  protected
    procedure Notify(const Flag: Integer); override;
  private
    procedure FontChanged;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const Ribbon: TObject; const CommandId: Cardinal); override;
    destructor Destroy; override;

    class function CommandType: TUICommandType; override;

    { The properties of the font in the fint control. }
    property Font: TUIFont read FFont;

    { Fired when one or more of the font properties has changed.
      When the Verb is cvExecute or cvPreview, then the Font parameter of the
      event contains the new font settings. Otherwise, the Font parameter
      contains the current font settings. }
    property OnChanged: TUICommandFontChangedEvent read FOnChanged write FOnChanged;
  end;
{$ENDREGION 'Font command'}

{$REGION 'Color Anchor command'}
  TUICommandColorAnchor = class;

  TUICommandColorEventArgs = record
    Command: TUICommandColorAnchor;
    Verb: TUICommandVerb;
    ColorType: TUISwatchColorType;
    CustomColor: TColor;
    Properties: TUICommandExecutionProperties;
  end;

  TUICommandColorAnchorExecuteEvent = procedure(const Args: TUICommandColorEventArgs) of object;

  { Color Anchor command. Used by the Drop-Down Color Picker. }
  TUICommandColorAnchor = class(TUICommand)
  {$REGION 'Internal Declarations'}
  strict private
    FOnExecute: TUICommandColorAnchorExecuteEvent;
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetColorType: TUISwatchColorType;
    procedure SetColorType(const Value: TUISwatchColorType);
    function GetStandardColors: TArray<TColor>;
    procedure SetStandardColors(const Value: TArray<TColor>);
    function GetStandardColorTooltips: TArray<String>;
    procedure SetStandardColorTooltips(const Value: TArray<String>);
    function GetThemeColors: TArray<TColor>;
    procedure SetThemeColors(const Value: TArray<TColor>);
    function GetThemeColorTooltips: TArray<String>;
    procedure SetThemeColorTooltips(const Value: TArray<String>);
    function GetThemeColorsCategoryLabel: String;
    procedure SetThemeColorsCategoryLabel(const Value: String);
    function GetStandardColorsCategoryLabel: String;
    procedure SetStandardColorsCategoryLabel(const Value: String);
    function GetRecentColorsCategoryLabel: String;
    procedure SetRecentColorsCategoryLabel(const Value: String);
    function GetAutomaticColorLabel: String;
    procedure SetAutomaticColorLabel(const Value: String);
    function GetNoColorLabel: String;
    procedure SetNoColorLabel(const Value: String);
    function GetMoreColorsLabel: String;
    procedure SetMoreColorsLabel(const Value: String);
  strict protected
    procedure DoExecute(const Prop: TUIProperty; const Verb: TUICommandVerb;
      const CurrentValue: PPropVariant; var Result: HRESULT); override;
    function CreateActionLink: TActionLink; override;
  {$ENDREGION 'Internal Declarations'}
  public
    class function CommandType: TUICommandType; override;
    function HandleShortCut(const ShortCut: TShortCut): Boolean; override;

    { The selected color type (ctNoColor, ctAutomatic or ctRgb). When this
      property equals ctRgb, you can use the Color property to get the actual
      color. }
    property ColorType: TUISwatchColorType read GetColorType write SetColorType;

    { The currently selected color when ColorType=ctRgb. Or clNone when
      ColorType=ctNoColor, or clDefault when ColorType=ctAutomatic.
      When setting the color, the ColorType property will be adjusted
      accordingly. }
    property Color: TColor read GetColor write SetColor;

    { The standard colors to use when the ColorTemplate for the color picker
      contains a StandardColors grid. }
    property StandardColors: TArray<TColor> read GetStandardColors write SetStandardColors;

    { The tooltips of the standard colors to use when the ColorTemplate for the
      color picker contains a StandardColors grid. }
    property StandardColorTooltips: TArray<String> read GetStandardColorTooltips write SetStandardColorTooltips;

    { The category label to display for the "Standard colors" header }
    property StandardColorsCategoryLabel: String read GetStandardColorsCategoryLabel write SetStandardColorsCategoryLabel;

    { The theme colors to use when the ColorTemplate for the color picker
      contains a ThemeColors grid. }
    property ThemeColors: TArray<TColor> read GetThemeColors write SetThemeColors;

    { The tooltips of the theme colors to use when the ColorTemplate for the
      color picker contains a ThemeColors grid. }
    property ThemeColorTooltips: TArray<String> read GetThemeColorTooltips write SetThemeColorTooltips;

    { The category label to display for the "Theme colors" header }
    property ThemeColorsCategoryLabel: String read GetThemeColorsCategoryLabel write SetThemeColorsCategoryLabel;

    { The category label to display for the "Recent colors" header }
    property RecentColorsCategoryLabel: String read GetRecentColorsCategoryLabel write SetRecentColorsCategoryLabel;

    { The caption of the "Automatic color" button }
    property AutomaticColorLabel: String read GetAutomaticColorLabel write SetAutomaticColorLabel;

    { The caption of the "No color" button }
    property NoColorLabel: String read GetNoColorLabel write SetNoColorLabel;

    { The caption of the "More Colors..." button }
    property MoreColorsLabel: String read GetMoreColorsLabel write SetMoreColorsLabel;

    { Fired when a color is selected. }
    property OnExecute: TUICommandColorAnchorExecuteEvent read FOnExecute write FOnExecute;
  end;
{$ENDREGION 'Color Anchor command'}

{$REGION 'Recent Items command'}
  TUICommandRecentItems = class;

  TUICommandRecentItemsSelectEvent = procedure(const Command: TUICommandRecentItems;
    const Verb: TUICommandVerb; const ItemIndex: Integer;
    const Properties: TUICommandExecutionProperties) of object;

  { Recent Items command. Used for the Recent Items list in the Application
    Menu. }
  TUICommandRecentItems = class(TUICommand)
  {$REGION 'Internal Declarations'}
  strict private
    const NOTIFY_ITEMS = 1;
  strict private
    FItems: TUICollection;
    FOnSelect: TUICommandRecentItemsSelectEvent;
  strict private
    procedure ItemsChange(const Collection: TUICollection; const Immediate: Boolean);
  strict protected
    procedure DoUpdate(const Prop: TUIProperty; const CurrentValue: PPropVariant;
      out NewValue: TPropVariant; var Result: HRESULT); override;

    procedure DoExecute(const Prop: TUIProperty; const Verb: TUICommandVerb;
      const CurrentValue: PPropVariant; var Result: HRESULT); override;
    function CreateActionLink: TActionLink; override;
  protected
    procedure Notify(const Flag: Integer); override;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const Ribbon: TObject; const CommandId: Cardinal); override;
    destructor Destroy; override;

    class function CommandType: TUICommandType; override;

    { The items in the Recent Items List. You can fill this collection with
      TUIRecentItem objects, or your own objects. The items should have the
      following properties:
      -LabelText: the text of the item (eg. a filename)
      -Description: longer description of the item as it appears in the
        tooltip for the item (eg. the full path)
      -Pinned (boolean): whether the item is pinned to the application menu. }
    property Items: TUICollection read FItems;

    { Fired when a Recent Item is selected. ItemIndex is the index of the
      selected item (into the Items collection). }
    property OnSelect: TUICommandRecentItemsSelectEvent read FOnSelect write FOnSelect;
  end;
{$ENDREGION 'Recent Items command'}

{$REGION 'Collections'}
  { Implemented by TUICollectionItem }
  IUICollectionItem = interface
  ['{C50153F6-8951-4316-BBFF-3B639E6DA20A}']
  end;

  { Abstract base class for items you can add to a TUICollection.
    You need to override the GetPropertyValue method to return the value
    of a specific property.
    For a general-purpose implementation of this class, see
    TUIGalleryCollectionItem below. }
  TUICollectionItem = class abstract(TInterfacedObject, IUICollectionItem, IUISimplePropertySet)
  {$REGION 'Internal Declarations'}
  private
    { IUISimplePropertySet }
    function GetValue(const Key: TUIPropertyKey; out Value: TPropVariant): HResult; stdcall;
  strict protected
    function GetPropertyValue(const Prop: TUIProperty): TValue; virtual; abstract;
  {$ENDREGION 'Internal Declarations'}
  end;

  TUICollectionChangeEvent = procedure(const Collection: TUICollection;
    const Immediate: Boolean) of object;

  { Wrapper around the IUICollection interface.
    NOTE: There are two ways to modify the collection:
    1. Modify the collection with delayed updates by regular calls to Add,
       Delete, Clear etc. When you do this, modifications will be batched until
       the next Windows message pump. This batching improves performance, but
       has the effect that the control that hosts the collection will no be
       updated immediately. For example, when you fill a gallery collection,
       you start with an empty collection. Then, you add 3 items. When you
       set the SelectedItem property of the gallery to the first item, you
       will get an exception, because the actual gallery is still empty at this
       point until the next message loop is processed.
    2. Place the modifications between BeginUpdate and EndUpdate (similar to how
       other Delphi VCL controls work). As soon as you call EndUpdate, the
       changes will take effect. So, setting SelectedItem in this example will
       be OK. This is the preferred method of working with collections. }
  TUICollection = class
  {$REGION 'Internal Declarations'}
  strict private
    type
      TUICollectionImpl = class(TInterfacedObject, IEnumUnknown, IUICollection)
      strict private
        FItems: TList<IUnknown>;
        FOwnsItems: Boolean;
        FEnumIndex: Integer;
      private
        { IEnumUnknown. Apparently, when the Ribbon framework also requires
          that you implement the IEnumUnknown interface. }
        function Next(celt: Longint; out elt;
          pceltFetched: PLongint): HResult; stdcall;
        function Skip(celt: Longint): HResult; stdcall;
        function Reset: HResult; stdcall;
        function Clone(out enm: IEnumUnknown): HResult; stdcall;
      private
        { IUICollection}
        function GetCount: UInt32; safecall;
        function GetItem(Index: UInt32): IUnknown; safecall;
        procedure Add(const Item: IUnknown); safecall;
        procedure Insert(Index: UInt32; const Item: IUnknown); safecall;
        procedure RemoveAt(Index: UInt32); safecall;
        procedure Replace(IndexReplaces: UInt32; const ItemReplaceWith: IUnknown); safecall;
        procedure Clear; safecall;
      public
        constructor Create; overload;
        constructor Create(const Items: TList<IUnknown>; const EnumIndex: Integer); overload;
        destructor Destroy; override;
      end;
  strict private
    type
      TEnumerator = class
      strict private
        FCollection: TUICollection;
        FIndex: Integer;
        function GetCurrent: IUICollectionItem;
      public
        constructor Create(const Collection: TUICollection);
        property Current: IUICollectionItem read GetCurrent;
        function MoveNext: Boolean;
      end;
  strict private
    FHandle: IUICollection;
    FUpdateCount: Integer;
    FOnChange: TUICollectionChangeEvent;
    function GetCount: Integer;
    function GetItem(const Index: Integer): IUICollectionItem;
    procedure Changed(const Immediate: Boolean = False);
  private
    property OnChange: TUICollectionChangeEvent read FOnChange write FOnChange;
  {$ENDREGION 'Internal Declarations'}
  public
    { Creates a new empty collection }
    constructor Create; overload;
    destructor Destroy; override;

    { Creates a collection based on an exiting IUICollection object }
    constructor Create(const Handle: IUICollection); overload;

    { Starts a batch of updates to the collection. The control that uses this
      collection (like a gallery) will not be updated until EndUpdate is
      called. }
    procedure BeginUpdate;

    { See BeginUpdate }
    procedure EndUpdate;

    { Adds Item to the collection }
    procedure Add(const Item: IUICollectionItem);

    { Inserts Item in the collection at position Index }
    procedure Insert(const Index: Integer; const Item: IUICollectionItem);

    { Replace the element at position Index with Item }
    procedure Replace(const Index: Integer; const Item: IUICollectionItem);

    { Deletes the Index'th item }
    procedure Delete(const Index: Integer);

    { Clears the collection }
    procedure Clear;

    { To support for..in enumeration over the collection }
    function GetEnumerator: TEnumerator;

    { Number of items in the collection }
    property Count: Integer read GetCount;

    { The items in the collection }
    property Items[const Index: Integer]: IUICollectionItem read GetItem; default;

    { Low-level handle to the collection }
    property Handle: IUICollection read FHandle;
  end;

  { Default implementation for items that can be added to a gallery. You can
    use this class as-is, or you can derive a class from this class and add your
    own properties. Also, you don't need to use this class at all to populate
    galleries; you can create you own gallery item provider as long as you
    derive from TUICollectionItem. }
  TUIGalleryCollectionItem = class(TUICollectionItem)
  {$REGION 'Internal Declarations'}
  strict private
    FLabel: String;
    FCategoryId: Integer;
    FImage: IUIImage;
    FCommand: TUICommand; // Reference
  strict protected
    function GetPropertyValue(const Prop: TUIProperty): TValue; override;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const pLabel: String = ''; const pImage: IUIImage = nil);

    { The label text for the item. Used for Items and Categories. }
    property LabelText: String read FLabel write FLabel;

    { The ID of the category for this item. Equals -1 (default) if this item
      does not belong to a category, or if there are no categories.
      Used for Items, Categories and Commands. }
    property CategoryId: Integer read FCategoryId write FCategoryId;

    { The image to display for the item. Used for Items only. }
    property Image: IUIImage read FImage write FImage;

    { For command galleries, the command to use for this item. }
    property Command: TUICommand read FCommand write FCommand;
  end;

  { Default implementation for items that can be added to the Recent Items list
    of the Application Menu. You can use this class as-is, or you can derive a
    class from this class and add your own properties. Also, you don't need to
    use this class at all to populate the Recent Items list; you can create you
    own gallery item provider as long as you derive from TUICollectionItem. }
  TUIRecentItem = class(TUICollectionItem)
  {$REGION 'Internal Declarations'}
  strict private
    FLabel: String;
    FDescription: String;
    FPinned: Boolean;
  strict protected
    function GetPropertyValue(const Prop: TUIProperty): TValue; override;
  {$ENDREGION 'Internal Declarations'}
  public
    { The label text of the item (eg. a filename) }
    property LabelText: String read FLabel write FLabel;

    { Longer description of the item as it appears in the tooltip for the item
      (eg. the full path) }
    property Description: String read FDescription write FDescription;

    { Whether the item is pinned to the application menu. }
    property Pinned: Boolean read FPinned write FPinned;
  end;
{$ENDREGION 'Collections'}

{$REGION 'Images'}
  TUIImage = class(TInterfacedObject, IUIImage)
  {$REGION 'Internal Declarations'}
  private
    class var FImageFactory: IUIImageFromBitmap;
  private
    FHandle: IUIImage;
    FBitmap: HBitmap;
    FWidth: Integer;
    FHeight: Integer;
    FBitsPerPixel: Integer;
    FOnChanged: TNotifyEvent;
  private
    procedure GetBitmapProperties;
    procedure ConvertToAlphaBitmap(const Bitmap: TBitmap);
    procedure Load(const Instance: HINST; const ResourceName: PChar); overload;
    procedure LoadBmp(const Filename: String; const HighContrast: Boolean);
    procedure LoadPng(const Filename: String; const HighContrast: Boolean);
    procedure DoChanged;
    function CreatePreMultipliedBitmap(const Bitmap: HBitmap): HBitmap;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  private
    { IUIImage }
    function GetBitmap: HBITMAP; safecall;
  {$ENDREGION 'Internal Declarations'}
  public
    { Creates an image from a resource. This must be a regular BITMAP resource. }
    constructor Create(const ResourceId: Integer); overload;
    constructor Create(const Instance: HINST; const ResourceId: Integer); overload;
    constructor Create(const Instance: HINST; const ResourceName: String); overload;

    { Creates an image from an existing IUIImage object }
    constructor Create(const Handle: IUIImage); overload;

    { Creates an image from a BMP or PNG file. }
    constructor Create(const Filename: String;
      const HighContrast: Boolean = False); overload;

    constructor Create(pImageList: TCustomImageList; pImageIndex: TImageIndex); overload;

    { Loads the image from a resource (replaces the current image),
      This must be a regular BITMAP resource. }
    procedure Load(const ResourceId: Integer); overload;
    procedure Load(const Instance: HINST; const ResourceId: Integer); overload;
    procedure Load(const Instance: HINST; const ResourceName: String); overload;

    { Loads the image from a BMP or PNG file }
    procedure Load(const Filename: String; const HighContrast: Boolean = False); overload;

    { Draws the image (unstretched) to the given canvas at the given position.
      When the image contains transparency or an alpha channel, the image
      is blended with the background, so you may want to fill the background
      first before calling this method. }
    procedure Draw(const Target: TCanvas; const XTarget, YTarget: Integer); overload;
    procedure Draw(const Target: TCanvas; const XTarget, YTarget, WTarget,
      HTarget: Integer); overload;

    { Width of the image }
    property Width: Integer read FWidth;

    { Height of the image }
    property Height: Integer read FHeight;

    { Number of bits per pixel for the bitmap image. When this value equals 32,
      this means that the bitmap has an alpha channel. }
    property BitsPerPixel: Integer read FBitsPerPixel;

    { Low-level handle to the image }
    property Handle: IUIImage read FHandle;

    { Bitmap handle }
    property Bitmap: HBitmap read GetBitmap;
  end;
{$ENDREGION 'Images'}

{$REGION 'Internal'}
const
  UI_COMMAND_CLASSES: array [TUICommandType] of TUICommandClass = (
    nil,                   // ctUnknown,
    TUICommandGroup,       // ctGroup,
    TUICommandAction,      // ctAction,
    TUICommandAnchor,      // ctAnchor,
    TUICommandContext,     // ctContext,
    TUICommandCollection,  // ctCollection,
    TUICommandCollection,  // ctCommandCollection,
    TUICommandDecimal,     // ctDecimal,
    TUICommandBoolean,     // ctBoolean,
    TUICommandFont,        // ctFont,
    TUICommandRecentItems, // ctRecentItems,
    TUICommandColorAnchor, // ctColorAnchor,
    nil);                  // ctColorCollection,

function GetPropertyKey(const Prop: TUIProperty): PUIPropertyKey;
function GetProperty(const Key: TUIPropertyKey): TUIProperty;

{$ENDREGION 'Internal'}

implementation

uses
  Menus,
  SysUtils,
  CommCtrl,
  WinApiEx,
  Controls,
  UIRibbon,
  UIRibbonActions;

const
  UI_PROPERTIES: array [TUIProperty] of PUIPropertyKey = (
    nil,
    nil,

    @UI_PKEY_Enabled,
    @UI_PKEY_LabelDescription,
    @UI_PKEY_Keytip,
    @UI_PKEY_Label,
    @UI_PKEY_TooltipDescription,
    @UI_PKEY_TooltipTitle,
    @UI_PKEY_LargeImage,
    @UI_PKEY_LargeHighContrastImage,
    @UI_PKEY_SmallImage,
    @UI_PKEY_SmallHighContrastImage,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil,

    @UI_PKEY_CommandId,
    @UI_PKEY_ItemsSource,
    @UI_PKEY_Categories,
    @UI_PKEY_CategoryId,
    @UI_PKEY_SelectedItem,
    @UI_PKEY_CommandType,
    @UI_PKEY_ItemImage,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil,

    @UI_PKEY_BooleanValue,
    @UI_PKEY_DecimalValue,
    @UI_PKEY_StringValue,
    @UI_PKEY_MaxValue,
    @UI_PKEY_MinValue,
    @UI_PKEY_Increment,
    @UI_PKEY_DecimalPlaces,
    @UI_PKEY_FormatString,
    @UI_PKEY_RepresentativeString,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil,

    @UI_PKEY_FontProperties,
    @UI_PKEY_FontProperties_Family,
    @UI_PKEY_FontProperties_Size,
    @UI_PKEY_FontProperties_Bold,
    @UI_PKEY_FontProperties_Italic,
    @UI_PKEY_FontProperties_Underline,
    @UI_PKEY_FontProperties_Strikethrough,
    @UI_PKEY_FontProperties_VerticalPositioning,
    @UI_PKEY_FontProperties_ForegroundColor,
    @UI_PKEY_FontProperties_BackgroundColor,
    @UI_PKEY_FontProperties_ForegroundColorType,
    @UI_PKEY_FontProperties_BackgroundColorType,
    @UI_PKEY_FontProperties_ChangedProperties,
    @UI_PKEY_FontProperties_DeltaSize,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil,

    @UI_PKEY_RecentItems,
    @UI_PKEY_Pinned,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil,

    @UI_PKEY_Color,
    @UI_PKEY_ColorType,
    @UI_PKEY_ColorMode,
    @UI_PKEY_ThemeColorsCategoryLabel,
    @UI_PKEY_StandardColorsCategoryLabel,
    @UI_PKEY_RecentColorsCategoryLabel,
    @UI_PKEY_AutomaticColorLabel,
    @UI_PKEY_NoColorLabel,
    @UI_PKEY_MoreColorsLabel,
    @UI_PKEY_ThemeColors,
    @UI_PKEY_StandardColors,
    @UI_PKEY_ThemeColorsTooltips,
    @UI_PKEY_StandardColorsTooltips,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil,

    @UI_PKEY_Viewable,
    @UI_PKEY_Minimized,
    @UI_PKEY_QuickAccessToolbarDock,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil,

    @UI_PKEY_ContextAvailable,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,

    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil,

    @UI_PKEY_GlobalBackgroundColor,
    @UI_PKEY_GlobalHighlightColor,
    @UI_PKEY_GlobalTextColor);

{$IFDEF DEBUG}
procedure Initialize;
var
  Prop: TUIProperty;
  Key: PUIPropertyKey;
begin
  { The UI_PKEY_xxx property keys are GUIDs where the first 8 digits are
    numbered sequentially. We take advantage of this by making sure that the
    ordinal values of TUIProperty match the first 8 digits of the corresponding
    UI_PKEY_xxx value. That way, we can use TUIProperty instead of
    TUIPropertyKey, which is easier to use in comparisons, and also a bit
    faster. Here, we check wheter these oridinal values really match. }
  for Prop := Low(TUIProperty) to High(TUIProperty) do
  begin
    Key := UI_PROPERTIES[Prop];
    if Assigned(Key) then
    begin
       if (Key.FmtId.D1 <> Cardinal(Ord(Prop))) then
         Assert(False, Format('Property map error. Prop=%.4x, Key=%.4x',
           [Ord(Prop), Key.FmtId.D1]));
    end;
  end;
end;
{$ENDIF}

function GetPropertyKey(const Prop: TUIProperty): PUIPropertyKey;
begin
  Result := UI_PROPERTIES[Prop];
end;

function GetProperty(const Key: TUIPropertyKey): TUIProperty;
begin
  if (Key.FmtId.D1 <= Cardinal(Ord(High(TUIProperty)))) then
    Result := TUIProperty(Key.FmtId.D1)
  else
    Result := upUnknown;
end;

type

  { This notifier is used for delayed notification of TUICommand's of specific
    events. The notification will occur in the next message pump.

    For example, this notifier is used by collections whenever the collection
    changes. For example, a gallery as an Items collection. When you add
    multiple items to the collection, you don't want the gallery to update each
    time. You only want to update it when you are finished adding items. With
    a lot of standard VCL classes, it is the users responsibility to take care
    of this (usually by placing the changes between BeginUpdate and EndUpdate
    calls). We use a different model that does not require change in user
    behavior. Instead, each time the collection is changed, a notification
    is sent (using TNotifier.Notification). You can call this method multiple
    times, but it will only post the notification once. Then, on the next
    message pump, the notification is handled by calling TUICommand.Notify.
    There, the gallery is actually updated.

    For an TUICommand to use this model, it must do the following:
    -When a notification must be send, call Notifier.Noticication(Self, Flag).
     Flag is a value between 0 and 30 that defines the type of notification.
     This value can be anything. It's meaning is private to the command.
    -Override the TUICommand.Notify method. In this method, you can check this
     Flag again an take the appropriate action.

    See TUICommandCollection (Gallery) for details. }
  TNotifierFlag = 0..31;

  TNotifier = class
  strict private
    // Type for storing the TUICommands
    // We use Pointer here as generic type, because for those the hash value is being calculated without using the object instance.
    // Since the messages are processed delayed, it may happen that the object instance has already been destroyed.
    type TClients = TDictionary<Pointer, Cardinal>;
  strict private
    FNotificationWindow: HWND;
    FClients: TClients;
  strict private
    procedure WndProc(var Msg: TMessage);
    procedure Notify(const Command: TUICommand; const Flag: TNotifierFlag);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const Command: TUICommand);
    procedure Remove(const Command: TUICommand);
    procedure Notification(const Command: TUICommand; const Flag: TNotifierFlag);
  end;

var
  Notifier: TNotifier = nil;

type
  TUIRibbonAccess = class(TUIRibbon);

{ TNotifier }

procedure TNotifier.Add(const Command: TUICommand);
begin
  FClients.AddOrSetValue(Command, 0);
end;

constructor TNotifier.Create;
begin
  inherited;
  FNotificationWindow := AllocateHWnd(WndProc);
  FClients := TClients.Create();
end;

destructor TNotifier.Destroy;
begin
  if (FNotificationWindow <> 0) then
  begin
    DeallocateHWnd(FNotificationWindow);
    FNotificationWindow := 0;
  end;
  FClients.Free;
  inherited;
end;

procedure TNotifier.Notification(const Command: TUICommand;
  const Flag: TNotifierFlag);
var
  Flags: Cardinal;
begin
  { Don't send this notification if it has already been sent. }
  if (not (FClients.TryGetValue(Command, Flags))) then
    Flags := 0;

  if ((Flags and (1 shl Flag)) = 0) then
  begin
    PostMessage(FNotificationWindow, WM_RIBBONCMD_NOTIFY, Flag, LPARAM(Command));
    Flags := Flags or (1 shl Flag);
    FClients.AddOrSetValue(Command, Flags);
  end;
end;

procedure TNotifier.Notify(const Command: TUICommand; const Flag: TNotifierFlag);
var
  Flags: Cardinal;
begin
  if (FClients.TryGetValue(Command, Flags)) then
  begin
    Command.Notify(Flag);
    Flags := Flags and (not (1 shl Flag));
    FClients[Command] := Flags;
  end;
end;

procedure TNotifier.Remove(const Command: TUICommand);
begin
  FClients.Remove(Command);
end;

procedure TNotifier.WndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_RIBBONCMD_NOTIFY) then
    Notify(TUICommand(Msg.LParam), Msg.WParam)
  else
    Msg.Result := DefWindowProc(FNotificationWindow, Msg.Msg, Msg.WParam, Msg.LParam);
end;

{ TUICommandExecutionProperties }

function TUICommandExecutionProperties.GetValue(const Prop: TUIProperty;
  out Value: Cardinal): Boolean;
var
  Key: PUIPropertyKey;
  PropVar: TPropVariant;
begin
  Key := UI_PROPERTIES[Prop];
  Result := Assigned(FPropertySet) and Assigned(Key)
    and Succeeded(FPropertySet.GetValue(Key^, PropVar));
  if Result then
    UIPropertyToUInt32(Key^, PropVar, Value)
end;

function TUICommandExecutionProperties.GetValue(const Prop: TUIProperty;
  out Value: String): Boolean;
var
  Key: PUIPropertyKey;
  PropVar: TPropVariant;
begin
  Key := UI_PROPERTIES[Prop];
  Result := Assigned(FPropertySet) and Assigned(Key);
  if Result then
  begin
    Result := Succeeded(FPropertySet.GetValue(Key^, PropVar));
    if Result then
      Value := PropVar.bstrVal;
    PropVariantClear(PropVar);
  end;
end;

{ TUICommand }

function TUICommand._AddRef: Integer;
begin
  Result := -1;
end;

function TUICommand._Release: Integer;
begin
  Result := -1;
end;

procedure TUICommand.Assign(const pAction: TCustomAction);
const
  cAmpersand = '&';
begin
  // Assign general properties of the TAction to the Ribbon command
  Self.ActionLink.Action := pAction;
end;

procedure TUICommand.CachePropertyValue(const Key: TUIPropertyKey;
  const Value: TValue);
begin
  if (FCachedProperties = nil) then
    FCachedProperties := TDictionary<TUIPropertyKey, TValue>.Create;
  FCachedProperties.AddOrSetValue(Key, Value);
end;

constructor TUICommand.Create(const Ribbon: TObject;
  const CommandId: Cardinal);
begin
  if (not (Ribbon is TUIRibbon)) then
    raise EArgumentException.Create('Invalid Ribbon parameter');
  inherited Create(Ribbon as TUIRibbon);
  FFramework := TUIRibbon(Ribbon).Framework;
  FCommandId := CommandId;
  TUIRibbonAccess(Ribbon).AddCommand(Self);
//  FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_LargeImage);
//  FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_SmallImage);
end;

destructor TUICommand.Destroy;
begin
  Notifier.Remove(Self);
  FCachedProperties.Free;
  FLargeImage.Free;
  FSmallImage.Free;
  FLargeHighContrastImage.Free;
  FSmallHighContrastImage.Free;
  FActionLink.Free;
  inherited;
end;

procedure TUICommand.DoUpdate(const Prop: TUIProperty;
  const CurrentValue: PPropVariant; out NewValue: TPropVariant;
  var Result: HRESULT);

  procedure UpdateImage(var Image: TUIImage; const PropKey: TUIPropertyKey);
  var
    Handle: IUIImage;
    Handled: Boolean;
  begin
    if assigned(FOnUpdateImage) then begin
      Handled := False;
      FOnUpdateImage(Self, PropKey, NewValue, Handled);
      if Handled then
        Exit;
    end;

    if ((Image = nil) or (Image.Handle = nil)) and Assigned(CurrentValue) then
    begin
      UIPropertyToImage(PropKey, CurrentValue^, Handle);
      if (Image = nil) then
      begin
        Image := TUIImage.Create(Handle);
        Image.OnChanged := ImageChanged;
      end
      else
        Image.FHandle := Handle;
    end
    else if Assigned(Image) then
      UIInitPropertyFromImage(PropKey, Image.Handle, NewValue);
  end;

begin
  case Prop of
    upKeytip:
      begin
        if (vpKeytip in FValidProperties) then
          UIInitPropertyFromString(UI_PKEY_Keytip, FKeytip, NewValue);
      end;
    upLabel:
      begin
        if (vpCaption in FValidProperties) then
          UIInitPropertyFromString(UI_PKEY_Label, FCaption, NewValue);
      end;

    upTooltipTitle:
      begin
        if (vpTooltipTitle in FValidProperties) then
          UIInitPropertyFromString(UI_PKEY_TooltipTitle, FTooltipTitle, NewValue);
      end;

    upTooltipDescription:
      begin
        if (vpTooltipDescription in FValidProperties) then
          UIInitPropertyFromString(UI_PKEY_TooltipDescription, FTooltipDescription, NewValue);
      end;

    upLargeImage:
      UpdateImage(FLargeImage, UI_PKEY_LargeImage);

    upSmallImage:
      UpdateImage(FSmallImage, UI_PKEY_SmallImage);

    upLargeHighContrastImage:
      UpdateImage(FLargeHighContrastImage, UI_PKEY_LargeHighContrastImage);

    upSmallHighContrastImage:
      UpdateImage(FSmallHighContrastImage, UI_PKEY_SmallHighContrastImage);
  end;
end;

function TUICommand.Execute(CommandId: UInt32; Verb: _UIExecutionVerb;
  Key: PUIPropertyKey; CurrentValue: PPropVariant;
  CommandExecutionProperties: IUISimplePropertySet): HRESULT;
var
  Prop: TUIProperty;
begin
  if (Key = nil) then
    Prop := upNone
  else
    Prop := GetProperty(Key^);

  FProperties.FPropertySet := CommandExecutionProperties;
  Result := S_OK;
  DoExecute(Prop, TUICommandVerb(Verb), CurrentValue, Result);
end;

function TUICommand.GetActionLink: TActionLink;
begin
  if (FActionLink = nil) then
    FActionLink := CreateActionLink;
  Result := FActionLink;
end;

function TUICommand.GetEnabled: Boolean;
begin
  GetPropertyValue(UI_PKEY_Enabled, Result);
end;

procedure TUICommand.GetPropertyValue(const Key: TUIPropertyKey;
  out Value: Cardinal);
var
  Val: TPropVariant;
begin
  if Succeeded(FFramework.GetUICommandProperty(FCommandId,  Key, Val)) then
    UIPropertyToUInt32(Key, Val, Value)
  else
    Value := 0;
end;

procedure TUICommand.GetPropertyValue(const Key: TUIPropertyKey;
  out Value: Boolean);
var
  Val: TPropVariant;
begin
  if Succeeded(FFramework.GetUICommandProperty(FCommandId,  Key, Val)) then
    UIPropertyToBoolean(Key, Val, Value)
  else
    Value := False;
end;

procedure TUICommand.GetPropertyValue(const Key: TUIPropertyKey;
  out Value: Integer);
var
  Val: TPropVariant;
begin
  if Succeeded(FFramework.GetUICommandProperty(FCommandId,  Key, Val)) then
    UIPropertyToUInt32(Key, Val, Cardinal(Value))
  else
    Value := -1;
end;

procedure TUICommand.GetPropertyValue(const Key: TUIPropertyKey;
  out Value: String);
var
  Val: TPropVariant;
begin
  if Succeeded(FFramework.GetUICommandProperty(FCommandId,  Key, Val)) then
    UIPropertyToString(Key, Val, Value)
  else
    Value := '';
end;

procedure TUICommand.GetPropertyValue(const Key: TUIPropertyKey;
  out Value: Double);
var
  Val: TPropVariant;
  Dec: TDecimal;
begin
  if Succeeded(FFramework.GetUICommandProperty(FCommandId,  Key, Val)) then
  begin
    UIPropertyToDecimal(Key, Val, Dec);
    VarR8FromDec(@Dec, Value);
  end
  else
    Value := 0;
end;

function TUICommand.HandleShortCut(const ShortCut: TShortCut): Boolean;
begin
  Result := False;
end;

procedure TUICommand.ImageChanged(Sender: TObject);
begin
  if (Sender = FLargeImage) then
    FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_LargeImage)
  else if (Sender = FSmallImage) then
    FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_SmallImage)
  else if (Sender = FLargeHighContrastImage) then
    FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_LargeHighContrastImage)
  else if (Sender = FSmallHighContrastImage) then
    FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_SmallHighContrastImage)
end;

procedure TUICommand.Notify(const Flag: Integer);
var
  Val: TPropVariant;
  Prop: TPair<TUIPropertyKey, TValue>;
  Dec: TDecimal;
begin
  if (Flag = NOTIFY_CACHED_PROPERTIES) and Assigned(FCachedProperties) then
  begin
    for Prop in FCachedProperties do
    begin
      case Prop.Key.PId of
        VT_BOOL:
          UIInitPropertyFromBoolean(Prop.Key, Prop.Value.AsBoolean, Val);

        VT_I4,
        VT_UI4:
          UIInitPropertyFromUInt32(Prop.Key, UInt32(Prop.Value.AsInteger), Val);

        VT_LPWSTR:
          UIInitPropertyFromString(Prop.Key, Prop.Value.AsString, Val);

        VT_DECIMAL:
          begin
            VarDecFromR8(Prop.Value.AsExtended, Dec);
            UIInitPropertyFromDecimal(Prop.Key, Dec, Val);
          end;
      end;
      FFramework.SetUICommandProperty(FCommandId, Prop.Key, Val);
    end;
    FreeAndNil(FCachedProperties);
  end;
end;

procedure TUICommand.SetAlive(const Value: Boolean);
begin
  if (Value <> FAlive) then
  begin
    FAlive := Value;
    if (FAlive) and Assigned(FCachedProperties) then
      Notifier.Notification(Self, NOTIFY_CACHED_PROPERTIES);
  end;
end;

procedure TUICommand.SetCaption(const Value: String);
begin
  FCaption := Value;
  Include(FValidProperties, vpCaption);
  FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_Label);
end;

procedure TUICommand.SetEnabled(const Value: Boolean);
begin
  SetPropertyValue(UI_PKEY_Enabled, Value);
end;

procedure TUICommand.SetKeytip(const Value: String);
begin
  FKeytip := Value;
  Include(FValidProperties, vpKeytip);
  FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_Keytip);
end;

procedure TUICommand.SetPropertyValue(const Key: TUIPropertyKey;
  const Value: Cardinal);
var
  Val: TPropVariant;
begin
  if (FAlive) then
  begin
    UIInitPropertyFromUInt32(Key, Value, Val);
    FFramework.SetUICommandProperty(FCommandId, Key, Val);
  end
  else
    CachePropertyValue(Key, Value);
end;

procedure TUICommand.SetPropertyValue(const Key: TUIPropertyKey;
  const Value: Boolean);
var
  Val: TPropVariant;
begin
  if (FAlive) then
  begin
    UIInitPropertyFromBoolean(Key, Value, Val);
    FFramework.SetUICommandProperty(FCommandId, Key, Val);
  end
  else
    CachePropertyValue(Key, Value);
end;

procedure TUICommand.SetPropertyValue(const Key: TUIPropertyKey;
  const Value: Integer);
var
  Val: TPropVariant;
begin
  if (FAlive) then
  begin
    UIInitPropertyFromUInt32(Key, Cardinal(Value), Val);
    FFramework.SetUICommandProperty(FCommandId, Key, Val);
  end
  else
    CachePropertyValue(Key, Value);
end;

procedure TUICommand.SetPropertyValue(const Key: TUIPropertyKey;
  const Value: String);
var
  Val: TPropVariant;
begin
  if (FAlive) then
  begin
    UIInitPropertyFromString(Key, Value, Val);
    FFramework.SetUICommandProperty(FCommandId, Key, Val);
  end
  else
    CachePropertyValue(Key, Value);
end;

procedure TUICommand.SetPropertyValue(const Key: TUIPropertyKey;
  const Value: Double);
var
  Val: TPropVariant;
  Dec: TDecimal;
begin
  if (FAlive) then
  begin
    VarDecFromR8(Value, Dec);
    UIInitPropertyFromDecimal(Key, Dec, Val);
    FFramework.SetUICommandProperty(FCommandId, Key, Val);
  end
  else
    CachePropertyValue(Key, Value);
end;

procedure TUICommand.SetShortCut(const Shift: TShiftState; const Key: Word);
begin
  FShortCut := Menus.ShortCut(Key, Shift);
end;

procedure TUICommand.SetShortCut(const Shift: TShiftState; const Key: Char);
begin
  FShortCut := Menus.ShortCut(Ord(Key), Shift);
end;

procedure TUICommand.SetSmallImage(const Value: TUIImage);
begin
  if (FSmallImage <> Value) then begin
    FSmallImage.Free;
    FSmallImage := Value;
    FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_SmallImage);
  end;
end;

procedure TUICommand.SetLargeImage(const Value: TUIImage);
begin
  if (FLargeImage <> Value) then begin
    FLargeImage.Free;
    FLargeImage := Value;
    FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_LargeImage);
  end;
end;

procedure TUICommand.SetTooltipDescription(const Value: String);
begin
  FTooltipDescription := Value;
  Include(FValidProperties, vpTooltipDescription);
  FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_TooltipDescription);
end;

procedure TUICommand.SetTooltipTitle(const Value: String);
begin
  FTooltipTitle := Value;
  Include(FValidProperties, vpTooltipTitle);
  FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_TooltipTitle);
end;

function TUICommand.UpdateProperty(CommandId: UInt32; const Key: TUIPropertyKey;
  CurrentValue: PPropVariant; out NewValue: TPropVariant): HRESULT;
var
  Prop: TUIProperty;
begin
  Prop := GetProperty(Key);
  Result := S_FALSE;
  DoUpdate(Prop, CurrentValue, NewValue, Result);
end;

{ TUICommandGroup }

class function TUICommandGroup.CommandType: TUICommandType;
begin
  Result := ctGroup;
end;

function TUICommandGroup.CreateActionLink: TActionLink;
begin
  Result := TUICommandEmptyActionLink.Create(Self);
end;

procedure TUICommandGroup.DoExecute(const Prop: TUIProperty;
  const Verb: TUICommandVerb; const CurrentValue: PPropVariant;
  var Result: HRESULT);
begin
  { Nothing }
end;

{ TUICommandAction }

class function TUICommandAction.CommandType: TUICommandType;
begin
  Result := ctAction;
end;

function TUICommandAction.CreateActionLink: TActionLink;
begin
  Result := TUICommandActionActionLink.Create(Self);
end;

procedure TUICommandAction.DoExecute(const Prop: TUIProperty;
  const Verb: TUICommandVerb; const CurrentValue: PPropVariant;
  var Result: HRESULT);
var
  Args: TUICommandActionEventArgs;
begin
  if Assigned(FOnExecute) then
  begin
    Args.Command := Self;
    Args.Verb := Verb;
    Args.Properties := FProperties;
    FOnExecute(Args);
  end;
end;

procedure TUICommandAction.DoUpdate(const Prop: TUIProperty;
  const CurrentValue: PPropVariant; out NewValue: TPropVariant;
  var Result: HRESULT);
begin
  inherited;
  if (Prop = upLabelDescription) and (vpLabelDescription in FValidProperties) then
    UIInitPropertyFromString(UI_PKEY_LabelDescription, FLabelDescription, NewValue);
end;

function TUICommandAction.HandleShortCut(const ShortCut: TShortCut): Boolean;
var
  Args: TUICommandActionEventArgs;
begin
  Result := (ShortCut = FShortCut) and Enabled;
  if Result and Assigned(FOnExecute) then
  begin
    Args.Command := Self;
    Args.Verb := cvExecute;
    Args.Properties := FProperties;
    FOnExecute(Args);
  end;
end;

procedure TUICommandAction.SetLabelDescription(const Value: String);
begin
  FLabelDescription := Value;
  Include(FValidProperties, vpLabelDescription);
  FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_LabelDescription);
end;

{ TUICommandCollection }

procedure TUICommandCollection.CategoriesChange(const Collection: TUICollection;
  const Immediate: Boolean);
begin
  if (Immediate and Alive) then
    Notify(NOTIFY_CATEGORIES)
  else
    { Don't change collection immediately. Postpone until next message pump in
      case multiple items in the collection are changed at once. }
    Notifier.Notification(Self, NOTIFY_CATEGORIES);
end;

class function TUICommandCollection.CommandType: TUICommandType;
begin
  Result := ctCollection;
end;

constructor TUICommandCollection.Create(const Ribbon: TObject;
  const CommandId: Cardinal);
begin
  inherited;
  FItems := TUICollection.Create;
  FItems.OnChange := ItemsChange;
  FCategories := TUICollection.Create;
  FCategories.OnChange := CategoriesChange;
end;

function TUICommandCollection.CreateActionLink: TActionLink;
begin
  Result := TUICommandCollectionActionLink.Create(Self);
end;

destructor TUICommandCollection.Destroy;
begin
  FItems.Free;
  FCategories.Free;
  inherited;
end;

procedure TUICommandCollection.DoExecute(const Prop: TUIProperty;
  const Verb: TUICommandVerb; const CurrentValue: PPropVariant;
  var Result: HRESULT);
var
  Args: TUICommandCollectionEventArgs;
begin
  if Assigned(FOnSelect) then
  begin
    Args.Command := Self;
    Args.Verb := Verb;
    Args.Properties := FProperties;
    if (CurrentValue = nil) or Failed(UIPropertyToUInt32(UI_PKEY_SelectedItem, CurrentValue^, Cardinal(Args.ItemIndex))) then begin
      Args.ItemIndex := -1;
      Self.SelectedItem := Args.ItemIndex;
    end;//if
    FOnSelect(Args);
  end;
end;

procedure TUICommandCollection.DoUpdate(const Prop: TUIProperty;
  const CurrentValue: PPropVariant; out NewValue: TPropVariant;
  var Result: HRESULT);
begin
  inherited;
  if (Prop = upRepresentativeString) then
  begin
    if (vpRepresentativeString in FValidProperties) then
      UIInitPropertyFromString(UI_PKEY_RepresentativeString, FRepresentativeString, NewValue)
    else
      { To make sure combo boxes don't appear too small }
      Result := E_FAIL;
  end;
end;

function TUICommandCollection.GetChecked: Boolean;
begin
  GetPropertyValue(UI_PKEY_BooleanValue, Result);
end;

function TUICommandCollection.GetSelectedItem: Integer;
begin
  GetPropertyValue(UI_PKEY_SelectedItem, Result);
end;

function TUICommandCollection.GetText: String;
begin
  GetPropertyValue(UI_PKEY_StringValue, Result);
end;

function TUICommandCollection.HandleShortCut(
  const ShortCut: TShortCut): Boolean;
var
  Args: TUICommandCollectionEventArgs;
begin
  Result := (ShortCut = FShortCut);
  if Result and Enabled and Assigned(FOnSelect) then
  begin
    Args.Command := Self;
    Args.Verb := cvExecute;
    Args.ItemIndex := GetSelectedItem;
    Args.Properties := FProperties;
    FOnSelect(Args);
  end;
end;

procedure TUICommandCollection.ItemsChange(const Collection: TUICollection; const Immediate: Boolean);
begin
  if (Immediate and Alive) then
    Notify(NOTIFY_ITEMS)
  else
    { Don't change collection immediately. Postpone until next message pump in
      case multiple items in the collection are changed at once. }
    Notifier.Notification(Self, NOTIFY_ITEMS);
end;

procedure TUICommandCollection.Notify(const Flag: Integer);
var
  Val: TPropVariant;
begin
  inherited;
  case Flag of
    NOTIFY_ITEMS:
      begin
        UIInitPropertyFromInterface(UI_PKEY_ItemsSource, FItems.Handle, Val);
        FFramework.SetUICommandProperty(FCommandId, UI_PKEY_ItemsSource, Val);
      end;

    NOTIFY_CATEGORIES:
      begin
        UIInitPropertyFromInterface(UI_PKEY_ItemsSource, FCategories.Handle, Val);
        FFramework.SetUICommandProperty(FCommandId, UI_PKEY_Categories, Val);
        { The ribbon takes additional ownership of the collection, so we must
          release it to avoid a memory leak. }
        FCategories.Handle._Release;
      end;
  end;
end;

procedure TUICommandCollection.SetChecked(const Value: Boolean);
begin
  SetPropertyValue(UI_PKEY_BooleanValue, Value);
end;

procedure TUICommandCollection.SetRepresentativeString(const Value: String);
begin
  FRepresentativeString := Value;
  Include(FValidProperties, vpRepresentativeString);
  FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_RepresentativeString);
end;

procedure TUICommandCollection.SetSelectedItem(const Value: Integer);
begin
  SetPropertyValue(UI_PKEY_SelectedItem, Value);
end;

procedure TUICommandCollection.SetText(const Value: String);
begin
  SetPropertyValue(UI_PKEY_StringValue, Value);
end;

{ TUICommandDecimal }

class function TUICommandDecimal.CommandType: TUICommandType;
begin
  Result := ctDecimal;
end;

function TUICommandDecimal.CreateActionLink: TActionLink;
begin
  Result := TUICommandDecimalActionLink.Create(Self);
end;

procedure TUICommandDecimal.DoExecute(const Prop: TUIProperty;
  const Verb: TUICommandVerb; const CurrentValue: PPropVariant;
  var Result: HRESULT);
var
  Dec: TDecimal;
  Value: Double;
begin
  if Assigned(FOnChange) then
  begin
    if (CurrentValue = nil) or Failed(UIPropertyToDecimal(UI_PKEY_DecimalValue, CurrentValue^, Dec)) then
      Value := 0
    else
      VarR8FromDec(@Dec, Value);
    FOnChange(Self, Verb, Value, FProperties);
  end;
end;

procedure TUICommandDecimal.DoUpdate(const Prop: TUIProperty;
  const CurrentValue: PPropVariant; out NewValue: TPropVariant;
  var Result: HRESULT);
var
  Dec: TDecimal;
begin
  inherited;
  case Prop of
    upMinValue:
      begin
        if (vpMinValue in FValidProperties) then
        begin
          VarDecFromR8(FMinValue, Dec);
          UIInitPropertyFromDecimal(UI_PKEY_MinValue, Dec, NewValue);
        end;
      end;

    upMaxValue:
      begin
        if (vpMaxValue in FValidProperties) then
        begin
          VarDecFromR8(FMaxValue, Dec);
          UIInitPropertyFromDecimal(UI_PKEY_MaxValue, Dec, NewValue);
        end;
      end;

    upIncrement:
      begin
        if (vpIncrement in FValidProperties) then
        begin
          VarDecFromR8(FIncrement, Dec);
          UIInitPropertyFromDecimal(UI_PKEY_Increment, Dec, NewValue);
        end;
      end;

    upDecimalPlaces:
      begin
        if (vpDecimalPlaces in FValidProperties) then
          UIInitPropertyFromUInt32(UI_PKEY_DecimalPlaces, FDecimalPlaces, NewValue);
      end;

    upRepresentativeString:
      begin
        if (vpRepresentativeString in FValidProperties) then
          UIInitPropertyFromString(UI_PKEY_RepresentativeString, FRepresentativeString, NewValue);
      end;

    upFormatString:
      begin
        if (vpFormatString in FValidProperties) then
          UIInitPropertyFromString(UI_PKEY_FormatString, FFormatString, NewValue);
      end;
  end;
end;

function TUICommandDecimal.GetValue: Double;
begin
  GetPropertyValue(UI_PKEY_DecimalValue, Result);
end;

function TUICommandDecimal.HandleShortCut(const ShortCut: TShortCut): Boolean;
begin
  Result := (ShortCut = FShortCut);
  if (Result) and Enabled and Assigned(FOnChange) then
    FOnChange(Self, cvExecute, GetValue, FProperties);
end;

procedure TUICommandDecimal.SetDecimalPlaces(const Value: Integer);
begin
  FDecimalPlaces := Value;
  Include(FValidProperties, vpDecimalPlaces);
  FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_DecimalPlaces);
end;

procedure TUICommandDecimal.SetFormatString(const Value: String);
begin
  FFormatString := Value;
  Include(FValidProperties, vpFormatString);
  FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_FormatString);
end;

procedure TUICommandDecimal.SetIncrement(const Value: Double);
begin
  FIncrement := Value;
  Include(FValidProperties, vpIncrement);
  FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_Increment);
end;

procedure TUICommandDecimal.SetMaxValue(const Value: Double);
begin
  FMaxValue := Value;
  Include(FValidProperties, vpMaxValue);
  FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_MaxValue);
end;

procedure TUICommandDecimal.SetMinValue(const Value: Double);
begin
  FMinValue := Value;
  Include(FValidProperties, vpMinValue);
  FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_MinValue);
end;

procedure TUICommandDecimal.SetRepresentativeString(const Value: String);
begin
  FRepresentativeString := Value;
  Include(FValidProperties, vpRepresentativeString);
  FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_RepresentativeString);
end;

procedure TUICommandDecimal.SetValue(const Value: Double);
begin
  SetPropertyValue(UI_PKEY_DecimalValue, Value);
end;

{ TUICommandBoolean }

class function TUICommandBoolean.CommandType: TUICommandType;
begin
  Result := ctBoolean;
end;

function TUICommandBoolean.CreateActionLink: TActionLink;
begin
  Result := TUICommandBooleanActionLink.Create(Self);
end;

procedure TUICommandBoolean.DoExecute(const Prop: TUIProperty;
  const Verb: TUICommandVerb; const CurrentValue: PPropVariant;
  var Result: HRESULT);
begin
  case Prop of
    upBooleanValue:
      DoToggle(Verb, CurrentValue);
  end;
end;

procedure TUICommandBoolean.DoToggle(const Verb: TUICommandVerb;
  const CurrentValue: PPropVariant);
var
  Args: TUICommandBooleanEventArgs;
begin
  if Assigned(FOnToggle) then
  begin
    Args.Command := Self;
    Args.Verb := Verb;
    Args.Properties := FProperties;
    if (CurrentValue = nil) or Failed(UIPropertyToBoolean(UI_PKEY_BooleanValue, CurrentValue^, Args.Checked)) then
      Args.Checked := False;
    FOnToggle(Args);
  end;
end;

procedure TUICommandBoolean.DoUpdate(const Prop: TUIProperty;
  const CurrentValue: PPropVariant; out NewValue: TPropVariant;
  var Result: HRESULT);
begin
  inherited;
  if (Prop = upLabelDescription) and (vpLabelDescription in FValidProperties) then
    UIInitPropertyFromString(UI_PKEY_LabelDescription, FLabelDescription, NewValue);
end;

function TUICommandBoolean.GetChecked: Boolean;
begin
  GetPropertyValue(UI_PKEY_BooleanValue, Result);
end;

function TUICommandBoolean.HandleShortCut(const ShortCut: TShortCut): Boolean;
var
  Args: TUICommandBooleanEventArgs;
begin
  Result := (ShortCut = FShortCut);
  if (Result) and Enabled and Assigned(FOnToggle) then
  begin
    Args.Command := Self;
    Args.Verb := cvExecute;
    Args.Checked := GetChecked;
    Args.Properties := FProperties;
    FOnToggle(Args);
  end;
end;

procedure TUICommandBoolean.SetChecked(const Value: Boolean);
begin
  SetPropertyValue(UI_PKEY_BooleanValue, Value);
end;

procedure TUICommandBoolean.SetLabelDescription(const Value: String);
begin
  FLabelDescription := Value;
  Include(FValidProperties, vpLabelDescription);
  FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_LabelDescription);
end;

{ TUICommandAnchor }

class function TUICommandAnchor.CommandType: TUICommandType;
begin
  Result := ctAnchor;
end;

function TUICommandAnchor.CreateActionLink: TActionLink;
begin
  Result := TUICommandEmptyActionLink.Create(Self);
end;

procedure TUICommandAnchor.DoExecute(const Prop: TUIProperty;
  const Verb: TUICommandVerb; const CurrentValue: PPropVariant;
  var Result: HRESULT);
begin
  { Nothing }
end;

{ TUICommandContext }

class function TUICommandContext.CommandType: TUICommandType;
begin
  Result := ctContext;
end;

function TUICommandContext.CreateActionLink: TActionLink;
begin
  Result := TUICommandEmptyActionLink.Create(Self);
end;

procedure TUICommandContext.DoExecute(const Prop: TUIProperty;
  const Verb: TUICommandVerb; const CurrentValue: PPropVariant;
  var Result: HRESULT);
begin
  { Nothing }
end;

function TUICommandContext.GetAvailability: TUIContextAvailability;
var
  C: Cardinal;
begin
  GetPropertyValue(UI_PKEY_ContextAvailable, C);
  Result := TUIContextAvailability(C);
end;

procedure TUICommandContext.SetAvailability(const Value: TUIContextAvailability);
begin
  SetPropertyValue(UI_PKEY_ContextAvailable, Ord(Value));
end;

{ TUIFont }

const
  TWIPS_PER_POINT = 20;

procedure TUIFont.Assign(const Source: TFont);
const
  FLAGS: array [Boolean] of TUIFontProperty = (fpOff, fpOn);
begin
  if (Source = nil) then
    Exit;

  SetFamily(Source.Name);
  SetSize(Source.Size);
  SetBold(FLAGS[fsBold in Source.Style]);
  SetItalic(FLAGS[fsItalic in Source.Style]);
  SetUnderline(FLAGS[fsUnderline in Source.Style]);
  SetStrikethrough(FLAGS[fsStrikeOut in Source.Style]);
  SetForegroundColor(Source.Color);
end;

procedure TUIFont.Assign(const Source: TTextAttributes);
const
  FLAGS: array [Boolean, Boolean] of TUIFontProperty =
    ((fpNotAvailable, fpNotAvailable), (fpOff, fpOn));
var
  Attrs: TConsistentAttributes;
begin
  if (Source = nil) then
    Exit;

  Attrs := Source.ConsistentAttributes;

  if (caFace in Attrs) then
    SetFamily(Source.Name)
  else
    SetFamily('');

  if (caSize in Attrs) then
    SetSize(Source.Size / TWIPS_PER_POINT)
  else
    SetSize(0);

  SetBold(FLAGS[caBold in Attrs, fsBold in Source.Style]);
  SetItalic(FLAGS[caItalic in Attrs, fsItalic in Source.Style]);
  SetUnderline(FLAGS[caUnderline in Attrs, fsUnderline in Source.Style]);
  SetStrikethrough(FLAGS[caStrikeOut in Attrs, fsStrikeOut in Source.Style]);

  if (caColor in Attrs) then
    SetForegroundColor(Source.Color)
  else
    SetForegroundColor(clDefault);
end;

procedure TUIFont.Assign(const Source: TCharFormat2);
const
  FLAGS: array [Boolean, Boolean] of TUIFontProperty =
    ((fpNotAvailable, fpNotAvailable), (fpOff, fpOn));
begin
  SetBold(FLAGS[(Source.dwMask and CFM_BOLD) <> 0, (Source.dwEffects and CFE_BOLD) <> 0]);
  SetItalic(FLAGS[(Source.dwMask and CFM_ITALIC) <> 0, (Source.dwEffects and CFE_ITALIC) <> 0]);
  SetUnderline(FLAGS[(Source.dwMask and CFM_UNDERLINE) <> 0, (Source.dwEffects and CFE_UNDERLINE) <> 0]);
  SetStrikethrough(FLAGS[(Source.dwMask and CFM_STRIKEOUT) <> 0, (Source.dwEffects and CFE_STRIKEOUT) <> 0]);

  if ((Source.dwMask and CFE_SUBSCRIPT) <> 0) then
  begin
    if ((Source.dwMask and CFM_SUBSCRIPT) <> 0) and ((Source.dwEffects and CFE_SUBSCRIPT) <> 0) then
      SetVerticalPositioning(vpSubscript)
    else if ((Source.dwEffects and CFE_SUPERSCRIPT) <> 0) then
      SetVerticalPositioning(vpSuperscript)
    else
      SetVerticalPositioning(vpDefault);
  end
  else if ((Source.dwMask and CFM_OFFSET) <> 0) then
  begin
    if (Source.yOffset > 0) then
      SetVerticalPositioning(vpSuperscript)
    else if (Source.yOffset < 0) then
      SetVerticalPositioning(vpSubscript)
    else
      SetVerticalPositioning(vpNotAvailable)
  end
  else
    SetVerticalPositioning(vpDefault);

  if ((Source.dwMask and CFM_FACE) <> 0) then
    SetFamily(Source.szFaceName)
  else
    SetFamily('');

  if ((Source.dwMask and CFM_SIZE) <> 0) then
    SetSize(Source.yHeight / TWIPS_PER_POINT)
  else
    SetSize(0);

  if ((Source.dwMask and CFM_COLOR) <> 0) and ((Source.dwEffects and CFE_AUTOCOLOR) = 0) then
    SetForegroundColor(Source.crTextColor)
  else if ((Source.dwMask and CFM_COLOR) <> 0) and ((Source.dwEffects and CFE_AUTOCOLOR) <> 0) then
    SetForegroundColor(clDefault);

  if ((Source.dwMask and CFM_BACKCOLOR) <> 0) and ((Source.dwEffects and CFE_AUTOBACKCOLOR) = 0) then
    SetBackgroundColor(Source.crBackColor)
  else
    SetBackgroundColor(clNone);
end;

procedure TUIFont.AssignTo(const Dest: IPropertyStore);
var
  PropVar: TPropVariant;
  DecSize: TDecimal;

  procedure SetProp(const PropKey: TUIPropertyKey; const Prop: TUIFontProperty);
  var
    PropVar: TPropVariant;
  begin
    PropVariantInit(PropVar);
    UIInitPropertyFromUInt32(PropKey, Ord(Prop), PropVar);
    Dest.SetValue(TPropertyKey(PropKey), PropVar);
    PropVariantClear(PropVar);
  end;

begin
  SetProp(UI_PKEY_FontProperties_Bold, FBold);
  SetProp(UI_PKEY_FontProperties_Italic, FItalic);
  SetProp(UI_PKEY_FontProperties_Underline, FUnderline);
  SetProp(UI_PKEY_FontProperties_Strikethrough, FStrikethrough);

  PropVariantInit(PropVar);
  UIInitPropertyFromUInt32(UI_PKEY_FontProperties_VerticalPositioning, Ord(FVerticalPositioning), PropVar);
  Dest.SetValue(TPropertyKey(UI_PKEY_FontProperties_VerticalPositioning), PropVar);
  PropVariantClear(PropVar);

  PropVariantInit(PropVar);
  UIInitPropertyFromString(UI_PKEY_FontProperties_Family, FFamily, PropVar);
  Dest.SetValue(TPropertyKey(UI_PKEY_FontProperties_Family), PropVar);
  PropVariantClear(PropVar);

  PropVariantInit(PropVar);
  if (FSize <> 0) then
    VarDecFromR8(FSize, DecSize)
  else
    // The font size is not available so set it to zero.
    VarDecFromI4(0, DecSize);
  UIInitPropertyFromDecimal(UI_PKEY_FontProperties_Size, DecSize, PropVar);
  Dest.SetValue(TPropertyKey(UI_PKEY_FontProperties_Size), PropVar);
  PropVariantClear(PropVar);

  if (FForegroundColor = clDefault) then
  begin
    PropVariantInit(PropVar);
    UIInitPropertyFromUInt32(UI_PKEY_FontProperties_ForegroundColorType, Ord(UISwatchColorTypeAutomatic), PropVar);
    Dest.SetValue(TPropertyKey(UI_PKEY_FontProperties_ForegroundColorType), PropVar);
    PropVariantClear(PropVar);
  end
  else if (FForegroundColor <> clNone) then
  begin
    PropVariantInit(PropVar);
    UIInitPropertyFromUInt32(UI_PKEY_FontProperties_ForegroundColorType, Ord(UISwatchColorTypeRgb), PropVar);
    Dest.SetValue(TPropertyKey(UI_PKEY_FontProperties_ForegroundColorType), PropVar);
    PropVariantClear(PropVar);

    PropVariantInit(PropVar);
    UIInitPropertyFromUInt32(UI_PKEY_FontProperties_ForegroundColor, FForegroundColor, PropVar);
    Dest.SetValue(TPropertyKey(UI_PKEY_FontProperties_ForegroundColor), PropVar);
    PropVariantClear(PropVar);
  end;

  if (FBackgroundColor = clNone) then
  begin
    PropVariantInit(PropVar);
    UIInitPropertyFromUInt32(UI_PKEY_FontProperties_BackgroundColorType, Ord(UISwatchColorTypeNoColor), PropVar);
    Dest.SetValue(TPropertyKey(UI_PKEY_FontProperties_BackgroundColorType), PropVar);
    PropVariantClear(PropVar);
  end
  else if (FBackgroundColor <> clDefault) then
  begin
    PropVariantInit(PropVar);
    UIInitPropertyFromUInt32(UI_PKEY_FontProperties_BackgroundColorType, Ord(UISwatchColorTypeRgb), PropVar);
    Dest.SetValue(TPropertyKey(UI_PKEY_FontProperties_BackgroundColorType), PropVar);
    PropVariantClear(PropVar);

    PropVariantInit(PropVar);
    UIInitPropertyFromUInt32(UI_PKEY_FontProperties_BackgroundColor, FBackgroundColor, PropVar);
    Dest.SetValue(TPropertyKey(UI_PKEY_FontProperties_BackgroundColor), PropVar);
    PropVariantClear(PropVar);
  end;
end;

procedure TUIFont.Assign(const Source: IPropertyStore);
var
  PropVar: TPropVariant;
  Value: Cardinal;
  DecSize: TDecimal;

  procedure GetProp(const PropKey: TUIPropertyKey; out Prop: TUIFontProperty);
  var
    PropVar: TPropVariant;
    Value: Cardinal;
  begin
    // Get the effect value from the property store.
    PropVariantInit(PropVar);
    if Succeeded(Source.GetValue(TPropertyKey(PropKey), PropVar)) then
    begin
      UIPropertyToUInt32(PropKey, PropVar, Value);
      Prop := TUIFontProperty(Value);
    end;
    PropVariantClear(PropVar);
  end;

begin
  GetProp(UI_PKEY_FontProperties_Bold, FBold);
  GetProp(UI_PKEY_FontProperties_Italic, FItalic);
  GetProp(UI_PKEY_FontProperties_Underline, FUnderline);
  GetProp(UI_PKEY_FontProperties_Strikethrough, FStrikethrough);

  PropVariantInit(PropVar);
  if Succeeded(Source.GetValue(TPropertyKey(UI_PKEY_FontProperties_VerticalPositioning), PropVar)) then
  begin
    UIPropertyToUInt32(UI_PKEY_FontProperties_VerticalPositioning, PropVar, Value);
    FVerticalPositioning := TUIFontVerticalPositioning(Value);
  end;
  PropVariantClear(PropVar);

  PropVariantInit(PropVar);
  if Succeeded(Source.GetValue(TPropertyKey(UI_PKEY_FontProperties_Family), PropVar)) then
    UIPropertyToString(UI_PKEY_FontProperties_Family, PropVar, FFamily);
  PropVariantClear(PropVar);

  PropVariantInit(PropVar);
  if Succeeded(Source.GetValue(TPropertyKey(UI_PKEY_FontProperties_Size), PropVar)) then
  begin
    UIPropertyToDecimal(UI_PKEY_FontProperties_Size, PropVar, DecSize);
    VarR4FromDec(@DecSize, FSize);
  end;
  PropVariantClear(PropVar);

  PropVariantInit(PropVar);
  if Succeeded(Source.GetValue(TPropertyKey(UI_PKEY_FontProperties_ForegroundColorType), PropVar)) then
  begin
    UIPropertyToUInt32(UI_PKEY_FontProperties_ForegroundColorType, PropVar, Value);
    if (Integer(Value) = Ord(UISwatchColorTypeAutomatic)) then
      FForegroundColor := clDefault;
  end;
  PropVariantClear(PropVar);

  PropVariantInit(PropVar);
  if Succeeded(Source.GetValue(TPropertyKey(UI_PKEY_FontProperties_ForegroundColor), PropVar)) then
    UIPropertyToUInt32(UI_PKEY_FontProperties_ForegroundColor, PropVar, Cardinal(FForegroundColor));
  PropVariantClear(PropVar);

  PropVariantInit(PropVar);
  if Succeeded(Source.GetValue(TPropertyKey(UI_PKEY_FontProperties_BackgroundColorType), PropVar)) then
  begin
    UIPropertyToUInt32(UI_PKEY_FontProperties_BackgroundColorType, PropVar, Cardinal(Value));
    if (Integer(Value) = Ord(UISwatchColorTypeNoColor)) then
      FBackgroundColor := clNone;
  end;
  PropVariantClear(PropVar);

  PropVariantInit(PropVar);
  if Succeeded(Source.GetValue(TPropertyKey(UI_PKEY_FontProperties_BackgroundColor), PropVar)) then
    UIPropertyToUInt32(UI_PKEY_FontProperties_BackgroundColor, PropVar, Cardinal(FBackgroundColor));
  PropVariantClear(PropVar);
end;

procedure TUIFont.AssignTo(out Dest: TCharFormat2);

  procedure SetStyle(const Prop: TUIFontProperty; const Mask, Effect: Cardinal);
  begin
    if (Prop <> fpNotAvailable) then
    begin
      Dest.dwMask := Dest.dwMask or Mask;
      if (Prop = fpOn) then
        Dest.dwEffects := Dest.dwEffects or Effect;
    end;
  end;

begin
  FillChar(Dest, SizeOf(Dest), 0);
  Dest.cbSize := SizeOf(Dest);

  SetStyle(FBold, CFM_BOLD, CFE_BOLD);
  SetStyle(FItalic, CFM_ITALIC, CFE_ITALIC);
  SetStyle(FUnderline, CFM_UNDERLINE, CFE_UNDERLINE);
  SetStyle(FStrikethrough, CFM_STRIKEOUT, CFE_STRIKEOUT);

  if (FVerticalPositioning <> vpNotAvailable) then
  begin
    Dest.dwMask := Dest.dwMask or (CFM_SUBSCRIPT or CFM_SUPERSCRIPT);
    if (FVerticalPositioning = vpSuperscript) then
      Dest.dwEffects := Dest.dwEffects or CFE_SUPERSCRIPT
    else if (FVerticalPositioning = vpSubscript) then
      Dest.dwEffects := Dest.dwEffects or CFE_SUBSCRIPT;
  end;

  if (FFamily <> '') then
  begin
    Dest.dwMask := Dest.dwMask or CFM_FACE;
    StrLCopy(Dest.szFaceName, PChar(FFamily), Length(Dest.szFaceName));
  end;

  if (FSize <> 0) then
  begin
    Dest.dwMask := Dest.dwMask or CFM_SIZE;
    Dest.yHeight := Round(FSize * TWIPS_PER_POINT);
  end;

  if (FForegroundColor = clDefault) then
  begin
    Dest.dwMask := Dest.dwMask or CFM_COLOR;
    Dest.dwEffects := Dest.dwEffects or CFE_AUTOCOLOR;
  end
  else if (FForegroundColor <> clNone) then
  begin
    Dest.dwMask := Dest.dwMask or CFM_COLOR;
    Dest.crTextColor := FForegroundColor;
  end;

  if (FBackgroundColor = clNone) then
  begin
    Dest.dwMask := Dest.dwMask or CFM_BACKCOLOR;
    Dest.dwEffects := Dest.dwEffects or CFE_AUTOBACKCOLOR;
  end
  else if (FBackgroundColor <> clDefault) then
  begin
    Dest.dwMask := Dest.dwMask or CFM_BACKCOLOR;
    Dest.crBackColor := FBackgroundColor;
  end;
end;

procedure TUIFont.AssignTo(const Dest: TTextAttributes);
var
  Styles: TFontStyles;

  procedure SetStyle(const Style: TFontStyle; const Prop: TUIFontProperty);
  begin
    case Prop of
      fpOff: Exclude(Styles, Style);
      fpOn : Include(Styles, Style);
    end;
  end;

begin
  if (Dest = nil) then
    Exit;

  if (FFamily <> '') then
    Dest.Name := FFamily;

  if (FSize <> 0) then
    Dest.Size := Trunc(FSize);

  Styles := Dest.Style;
  SetStyle(fsBold, FBold);
  SetStyle(fsItalic, FItalic);
  SetStyle(fsUnderline, FUnderline);
  SetStyle(fsStrikeOut, FStrikethrough);
  Dest.Style := Styles;

  if (FForegroundColor <> clDefault) and (FForegroundColor <> clNone) then
    Dest.Color := FForegroundColor;
end;

procedure TUIFont.AssignTo(const Dest: TFont);
var
  Styles: TFontStyles;

  procedure SetStyle(const Style: TFontStyle; const Prop: TUIFontProperty);
  begin
    case Prop of
      fpOff: Exclude(Styles, Style);
      fpOn : Include(Styles, Style);
    end;
  end;

begin
  if (Dest = nil) then
    Exit;

  Dest.Name := FFamily;
  Dest.Size := Round(FSize);
  Styles := Dest.Style;
  SetStyle(fsBold, FBold);
  SetStyle(fsItalic, FItalic);
  SetStyle(fsUnderline, FUnderline);
  SetStyle(fsStrikeOut, FStrikethrough);
  Dest.Style := Styles;
  if (FForegroundColor <> clDefault) and (FForegroundColor <> clNone) then
    Dest.Color := FForegroundColor;
end;

procedure TUIFont.Changed;
begin
  FOwner.FontChanged;
end;

constructor TUIFont.Create(const Owner: TUICommandFont);
begin
  Assert(Assigned(Owner));
  inherited Create;
  FOwner := Owner;
  FForegroundColor := clDefault;
  FBackgroundColor := clNone;
end;

procedure TUIFont.SetBackgroundColor(const Value: TColor);
begin
  if (Value <> FBackgroundColor) then
  begin
    FBackgroundColor := Value;
    Changed;
  end;
end;

procedure TUIFont.SetBold(const Value: TUIFontProperty);
begin
  if (Value <> FBold) then
  begin
    FBold := Value;
    Changed;
  end;
end;

procedure TUIFont.SetFamily(const Value: String);
begin
  if (Value <> FFamily) then
  begin
    FFamily := Value;
    Changed;
  end;
end;

procedure TUIFont.SetForegroundColor(const Value: TColor);
begin
  if (Value <> FForegroundColor) then
  begin
    FForegroundColor := Value;
    Changed;
  end;
end;

procedure TUIFont.SetItalic(const Value: TUIFontProperty);
begin
  if (Value <> FItalic) then
  begin
    FItalic := Value;
    Changed;
  end;
end;

procedure TUIFont.SetSize(const Value: Single);
var
  CurTwips, NewTwips: Integer;
begin
  CurTwips := Round(FSize * TWIPS_PER_POINT);
  NewTwips := Round(Value * TWIPS_PER_POINT);
  if (NewTwips <> CurTwips) then
  begin
    FSize := Value;
    Changed;
  end;
end;

procedure TUIFont.SetStrikethrough(const Value: TUIFontProperty);
begin
  if (Value <> FStrikethrough) then
  begin
    FStrikethrough := Value;
    Changed;
  end;
end;

procedure TUIFont.SetUnderline(const Value: TUIFontProperty);
begin
  if (Value <> FUnderline) then
  begin
    FUnderline := Value;
    Changed;
  end;
end;

procedure TUIFont.SetVerticalPositioning(
  const Value: TUIFontVerticalPositioning);
begin
  if (Value <> FVerticalPositioning) then
  begin
    FVerticalPositioning := Value;
    Changed;
  end;
end;

{ TUICommandFont }

class function TUICommandFont.CommandType: TUICommandType;
begin
  Result := ctFont;
end;

constructor TUICommandFont.Create(const Ribbon: TObject;
  const CommandId: Cardinal);
begin
  inherited;
  FFont := TUIFont.Create(Self);
end;

function TUICommandFont.CreateActionLink: TActionLink;
begin
  Result := TUICommandFontActionLink.Create(Self);
end;

destructor TUICommandFont.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TUICommandFont.DoExecute(const Prop: TUIProperty;
  const Verb: TUICommandVerb; const CurrentValue: PPropVariant;
  var Result: HRESULT);
var
  NewFont, TempFont: TUIFont;
  VarChanges: TPropVariant;
  Changes: IPropertyStore;
  Unk: IUnknown;
  Args: TUICommandFontEventArgs;
begin
  if (Prop = upFontProperties) and Assigned(FOnChanged) then
  begin
    Args.Command := Self;
    Args.Verb := Verb;
    Args.Font := FFont;
    Args.Properties := FProperties;
    if (Verb = cvCancelPreview) then
      FOnChanged(Args)
    else
    begin
      NewFont := TUIFont.Create(Self);
      try
        Result := FProperties.FPropertySet.GetValue(UI_PKEY_FontProperties_ChangedProperties, VarChanges);
        if Succeeded(Result) then
        begin
          Result := UIPropertyToInterface(UI_PKEY_FontProperties, VarChanges, Unk);
          if Succeeded(Result) and Supports(Unk, IPropertyStore, Changes) then
          begin
            NewFont.Assign(Changes);
            Args.Font := NewFont;
            FOnChanged(Args);
            if (Verb = cvExecute) then
            begin
              TempFont := NewFont;
              NewFont := FFont;
              FFont := TempFont;
            end;
          end;
          PropVariantClear(VarChanges);
        end;
      finally
        NewFont.Free;
      end;
    end;
  end;
end;

procedure TUICommandFont.DoUpdate(const Prop: TUIProperty;
  const CurrentValue: PPropVariant; out NewValue: TPropVariant;
  var Result: HRESULT);
var
  Unk: IUnknown;
  PropertyStore: IPropertyStore;
begin
  inherited;
  if (Prop = upFontProperties) and Assigned(CurrentValue) then
  begin
    Result := UIPropertyToInterface(UI_PKEY_FontProperties, CurrentValue^, Unk);
    if Supports(Unk, IPropertyStore, PropertyStore) then
    begin
      FFont.AssignTo(PropertyStore);
      Result := UIInitPropertyFromInterface(UI_PKEY_FontProperties, PropertyStore, NewValue);
    end;
  end;
end;

procedure TUICommandFont.FontChanged;
begin
  Notifier.Notification(Self, NOTIFY_FONT);
end;

procedure TUICommandFont.Notify(const Flag: Integer);
begin
  inherited;
  if (Flag = NOTIFY_FONT) then
    FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsAllProperties], nil);
end;

{ TUICommandColorAnchor }

class function TUICommandColorAnchor.CommandType: TUICommandType;
begin
  Result := ctColorAnchor;
end;

function TUICommandColorAnchor.CreateActionLink: TActionLink;
begin
  Result := TUICommandColorAnchorActionLink.Create(Self);
end;

procedure TUICommandColorAnchor.DoExecute(const Prop: TUIProperty;
  const Verb: TUICommandVerb; const CurrentValue: PPropVariant;
  var Result: HRESULT);
var
  Args: TUICommandColorEventArgs;
  ColorType: _UISwatchColorType;
begin
  if Assigned(FOnExecute) then
  begin
    if (CurrentValue = nil) or Failed(UIPropertyToUInt32(UI_PKEY_ColorType, CurrentValue^, Cardinal(ColorType))) then
      Exit;

    Args.Command := Self;
    Args.Verb := Verb;
    Args.ColorType := TUISwatchColorType(ColorType);
    Args.Properties := FProperties;

    if (ColorType <> UISwatchColorTypeRgb) or (not FProperties.GetValue(upColor, Cardinal(Args.CustomColor))) then
      Args.CustomColor := clBlack;

    FOnExecute(Args);
  end;
end;

function TUICommandColorAnchor.GetAutomaticColorLabel: String;
begin
  GetPropertyValue(UI_PKEY_AutomaticColorLabel, Result);
end;

function TUICommandColorAnchor.GetColor: TColor;
begin
  case GetColorType of
    ctNoColor:
      Result := clNone;

    ctAutomatic:
      Result := clDefault;
  else
    GetPropertyValue(UI_PKEY_Color, Cardinal(Result));
  end;
end;

function TUICommandColorAnchor.GetColorType: TUISwatchColorType;
var
  C: Cardinal;
begin
  GetPropertyValue(UI_PKEY_ColorType, C);
  Result := TUISwatchColorType(C);
end;

function TUICommandColorAnchor.GetMoreColorsLabel: String;
begin
  GetPropertyValue(UI_PKEY_MoreColorsLabel, Result);
end;

function TUICommandColorAnchor.GetNoColorLabel: String;
begin
  GetPropertyValue(UI_PKEY_NoColorLabel, Result);
end;

function TUICommandColorAnchor.GetRecentColorsCategoryLabel: String;
begin
  GetPropertyValue(UI_PKEY_RecentColorsCategoryLabel, Result);
end;

function TUICommandColorAnchor.GetStandardColors: TArray<TColor>;
var
  Val: TPropVariant;
  Count: Cardinal;
begin
  if Succeeded(FFramework.GetUICommandProperty(FCommandId,  UI_PKEY_StandardColors, Val)) then
  begin
    Count := PropVariantGetElementCount(Val);
    SetLength(Result, Count);
    if (Count > 0) then
    begin
      PropVariantToUInt32Vector(Val, @Result[0], Count, Count);
      SetLength(Result, Count);
    end;
  end
  else
    SetLength(Result, 0);
end;

function TUICommandColorAnchor.GetStandardColorsCategoryLabel: String;
begin
  GetPropertyValue(UI_PKEY_StandardColorsCategoryLabel, Result);
end;

function TUICommandColorAnchor.GetStandardColorTooltips: TArray<String>;
var
  Val: TPropVariant;
  Count: Cardinal;
  Strings: array of PWideChar;
  I: Integer;
begin
  if Succeeded(FFramework.GetUICommandProperty(FCommandId,  UI_PKEY_StandardColorsTooltips, Val)) then
  begin
    Count := PropVariantGetElementCount(Val);
    SetLength(Strings, Count);
    if (Count > 0) then
    begin
      PropVariantToStringVector(Val, @Strings[0], Count, Count);
      SetLength(Result, Count);
      for I := 0 to Count - 1 do
      begin
        Result[I] := Strings[I];
        CoTaskMemFree(Strings[I]);
      end;
    end;
  end
  else
    SetLength(Result, 0);
end;

function TUICommandColorAnchor.GetThemeColors: TArray<TColor>;
var
  Val: TPropVariant;
  Count: Cardinal;
begin
  if Succeeded(FFramework.GetUICommandProperty(FCommandId,  UI_PKEY_ThemeColors, Val)) then
  begin
    Count := PropVariantGetElementCount(Val);
    SetLength(Result, Count);
    if (Count > 0) then
    begin
      PropVariantToUInt32Vector(Val, @Result[0], Count, Count);
      SetLength(Result, Count);
    end;
  end
  else
    SetLength(Result, 0);
end;

function TUICommandColorAnchor.GetThemeColorsCategoryLabel: String;
begin
  GetPropertyValue(UI_PKEY_ThemeColorsCategoryLabel, Result);
end;

function TUICommandColorAnchor.GetThemeColorTooltips: TArray<String>;
var
  Val: TPropVariant;
  Count: Cardinal;
  Strings: array of PWideChar;
  I: Integer;
begin
  if Succeeded(FFramework.GetUICommandProperty(FCommandId,  UI_PKEY_ThemeColorsTooltips, Val)) then
  begin
    Count := PropVariantGetElementCount(Val);
    SetLength(Strings, Count);
    if (Count > 0) then
    begin
      PropVariantToStringVector(Val, @Strings[0], Count, Count);
      SetLength(Result, Count);
      for I := 0 to Count - 1 do
      begin
        Result[I] := Strings[I];
        CoTaskMemFree(Strings[I]);
      end;
    end;
  end
  else
    SetLength(Result, 0);
end;

function TUICommandColorAnchor.HandleShortCut(
  const ShortCut: TShortCut): Boolean;
var
  Args: TUICommandColorEventArgs;
begin
  Result := (FShortCut = ShortCut);
  if Result and Enabled and Assigned(FOnExecute) then
  begin
    Args.Command := Self;
    Args.Verb := cvExecute;
    Args.ColorType := GetColorType;
    Args.CustomColor := GetColor;
    Args.Properties := FProperties;
    FOnExecute(Args);
  end;
end;

procedure TUICommandColorAnchor.SetAutomaticColorLabel(const Value: String);
begin
  SetPropertyValue(UI_PKEY_AutomaticColorLabel, Value);
end;

procedure TUICommandColorAnchor.SetColor(const Value: TColor);
begin
  if (Value = clNone) then
    SetColorType(ctNoColor)
  else if (Value = clDefault) then
    SetColorType(ctAutomatic)
  else
  begin
    SetColorType(ctRgb);
    SetPropertyValue(UI_PKEY_Color, Cardinal(Value));
  end;
end;

procedure TUICommandColorAnchor.SetColorType(const Value: TUISwatchColorType);
begin
  SetPropertyValue(UI_PKEY_ColorType, Ord(Value));
end;

procedure TUICommandColorAnchor.SetMoreColorsLabel(const Value: String);
begin
  SetPropertyValue(UI_PKEY_MoreColorsLabel, Value);
end;

procedure TUICommandColorAnchor.SetNoColorLabel(const Value: String);
begin
  SetPropertyValue(UI_PKEY_NoColorLabel, Value);
end;

procedure TUICommandColorAnchor.SetRecentColorsCategoryLabel(
  const Value: String);
begin
  SetPropertyValue(UI_PKEY_RecentColorsCategoryLabel, Value);
end;

procedure TUICommandColorAnchor.SetStandardColors(const Value: TArray<TColor>);
var
  Val: TPropVariant;
begin
  if (Length(Value) = 0) then
    InitPropVariantFromUInt32Vector(nil, 0, Val)
  else
    InitPropVariantFromUInt32Vector(@Value[0], Length(Value), Val);
  FFramework.SetUICommandProperty(FCommandId, UI_PKEY_StandardColors, Val);
end;

procedure TUICommandColorAnchor.SetStandardColorsCategoryLabel(
  const Value: String);
begin
  SetPropertyValue(UI_PKEY_StandardColorsCategoryLabel, Value);
end;

procedure TUICommandColorAnchor.SetStandardColorTooltips(
  const Value: TArray<String>);
var
  Val: TPropVariant;
  Strings: array of PWideChar;
  I: Integer;
begin
  if (Length(Value) = 0) then
    InitPropVariantFromStringVector(nil, 0, Val)
  else
  begin
    SetLength(Strings, Length(Value));
    for I := 0 to Length(Value) - 1 do
      Strings[I] := PWideChar(Value[I]);
    InitPropVariantFromStringVector(@Strings[0], Length(Value), Val);
  end;
  FFramework.SetUICommandProperty(FCommandId, UI_PKEY_StandardColorsTooltips, Val);
end;

procedure TUICommandColorAnchor.SetThemeColors(const Value: TArray<TColor>);
var
  Val: TPropVariant;
begin
  if (Length(Value) = 0) then
    InitPropVariantFromUInt32Vector(nil, 0, Val)
  else
    InitPropVariantFromUInt32Vector(@Value[0], Length(Value), Val);
  FFramework.SetUICommandProperty(FCommandId, UI_PKEY_ThemeColors, Val);
end;

procedure TUICommandColorAnchor.SetThemeColorsCategoryLabel(
  const Value: String);
begin
  SetPropertyValue(UI_PKEY_ThemeColorsCategoryLabel, Value);
end;

procedure TUICommandColorAnchor.SetThemeColorTooltips(
  const Value: TArray<String>);
var
  Val: TPropVariant;
  Strings: array of PWideChar;
  I: Integer;
begin
  if (Length(Value) = 0) then
    InitPropVariantFromStringVector(nil, 0, Val)
  else
  begin
    SetLength(Strings, Length(Value));
    for I := 0 to Length(Value) - 1 do
      Strings[I] := PWideChar(Value[I]);
    InitPropVariantFromStringVector(@Strings[0], Length(Value), Val);
  end;
  FFramework.SetUICommandProperty(FCommandId, UI_PKEY_ThemeColorsTooltips, Val);
end;

{ TUICommandRecentItems }

class function TUICommandRecentItems.CommandType: TUICommandType;
begin
  Result := ctRecentItems;
end;

constructor TUICommandRecentItems.Create(const Ribbon: TObject;
  const CommandId: Cardinal);
begin
  inherited;
  FItems := TUICollection.Create;
  FItems.OnChange := ItemsChange;
end;

function TUICommandRecentItems.CreateActionLink: TActionLink;
begin
  Result := TUICommandRecentItemsActionLink.Create(Self);
end;

destructor TUICommandRecentItems.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TUICommandRecentItems.DoExecute(const Prop: TUIProperty;
  const Verb: TUICommandVerb; const CurrentValue: PPropVariant;
  var Result: HRESULT);
var
  Index: Integer;
begin
  if Assigned(FOnSelect) and (Prop = upSelectedItem) then
  begin
    if (CurrentValue = nil) or Failed(UIPropertyToUInt32(UI_PKEY_SelectedItem, CurrentValue^, Cardinal(Index))) then
      Index := -1;
    FOnSelect(Self, Verb, Index, FProperties);
  end;
end;

procedure TUICommandRecentItems.DoUpdate(const Prop: TUIProperty;
  const CurrentValue: PPropVariant; out NewValue: TPropVariant;
  var Result: HRESULT);
var
  I, Count: Integer;
  UnkPtr: Pointer;
  List: PSafeArray;
  Unk: IUnknown;
begin
  inherited;
  if Assigned(ActionLink) then
    ActionLink.Update;
  if (Prop = upRecentItems) then
  begin
    Count := FItems.Count;
    List := SafeArrayCreateVector(VT_UNKNOWN, 0, Count);
    if Assigned(List) then
    begin
      for I := 0 to Count - 1 do
      begin
        Unk := FItems[I];
        UnkPtr := Pointer(Unk);
        SafeArrayPutElement(List, I, UnkPtr^);
      end;
      UIInitPropertyFromIUnknownArray(UI_PKEY_RecentItems, List, NewValue);
      SafeArrayDestroy(List);
    end;
  end;
end;

procedure TUICommandRecentItems.ItemsChange(const Collection: TUICollection;
  const Immediate: Boolean);
begin
  if (Immediate and Alive) then
    Notify(NOTIFY_ITEMS)
  else
    { Don't change collection immediately. Postpone until next message pump in
      case multiple items in the collection are changed at once. }
    Notifier.Notification(Self, NOTIFY_ITEMS);
end;

procedure TUICommandRecentItems.Notify(const Flag: Integer);
begin
  inherited;
  if (Flag = NOTIFY_ITEMS) then
    FFramework.InvalidateUICommand(FCommandId, [UIInvalidationsProperty], @UI_PKEY_RecentItems);
end;

{ TUICollectionItem }

function TUICollectionItem.GetValue(const Key: TUIPropertyKey;
  out Value: TPropVariant): HResult;
var
  Prop: TUIProperty;
  Val: TValue;
  Intf: IInterface;
begin
  Result := E_FAIL;
  Prop := GetProperty(Key);
  if (Prop > upUnknown) then
  begin
    Val := GetPropertyValue(Prop);
    case Key.PId of
      VT_LPWSTR:
        Result := UIInitPropertyFromString(Key, Val.AsString, Value);

      VT_UI4:
        Result := UIInitPropertyFromUInt32(Key, UInt32(Val.AsInteger), Value);

      VT_UNKNOWN:
        begin
          Intf := Val.AsInterface;
          Result := UIInitPropertyFromInterface(Key, Intf, Value);
        end;

      VT_BOOL:
        Result := UIInitPropertyFromBoolean(Key, Val.AsBoolean, Value);
    else
      Assert(False, 'Unsupported property type');
    end;
  end;
end;

{ TUICollection }

procedure TUICollection.Add(const Item: IUICollectionItem);
begin
  FHandle.Add(Item);
  Changed;
end;

procedure TUICollection.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TUICollection.Changed(const Immediate: Boolean);
begin
  if Assigned(FOnChange) and (FUpdateCount = 0) then
    FOnChange(Self, Immediate);
end;

procedure TUICollection.Clear;
begin
  FHandle.Clear;
  Changed;
end;

constructor TUICollection.Create;
begin
  inherited Create;
  FHandle := TUICollectionImpl.Create;
end;

constructor TUICollection.Create(const Handle: IUICollection);
begin
  Assert(Assigned(Handle));
  inherited Create;
  FHandle := Handle;
end;

procedure TUICollection.Delete(const Index: Integer);
begin
  FHandle.RemoveAt(Index);
  Changed;
end;

destructor TUICollection.Destroy;
begin
  { The ribbon takes additional ownership of the collection, so we must
    release it to avoid a memory leak. See issue #34 }
  while FHandle._Release() > 0 do;
  Pointer(FHandle) := nil; // Set to nil without decreasing ref count, this was done the line before. See issue #39
  Inherited;
end;

procedure TUICollection.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) then
    Changed(True);
end;

function TUICollection.GetCount: Integer;
begin
  Result := FHandle.GetCount;
end;

function TUICollection.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TUICollection.GetItem(const Index: Integer): IUICollectionItem;
var
  Unk: IUnknown;
begin
  Unk := FHandle.GetItem(Index);
  if (Unk = nil) or (not Supports(Unk, IUICollectionItem, Result)) then
    Result := nil;
end;

procedure TUICollection.Insert(const Index: Integer;
  const Item: IUICollectionItem);
begin
  FHandle.Insert(Index, Item);
  Changed;
end;

procedure TUICollection.Replace(const Index: Integer;
  const Item: IUICollectionItem);
begin
  FHandle.Replace(Index, Item);
  Changed;
end;

{ TUICollection.TUICollectionImpl }

procedure TUICollection.TUICollectionImpl.Add(const Item: IInterface);
begin
  FItems.Add(Item);
end;

procedure TUICollection.TUICollectionImpl.Clear;
begin
  FItems.Clear;
end;

function TUICollection.TUICollectionImpl.Clone(out enm: IEnumUnknown): HResult;
begin
  enm := TUICollectionImpl.Create(FItems, FEnumIndex);
  Result := S_OK;
end;

constructor TUICollection.TUICollectionImpl.Create(
  const Items: TList<IUnknown>; const EnumIndex: Integer);
begin
  inherited Create;
  FItems := Items;
  FOwnsItems := False;
  FEnumIndex := EnumIndex;
end;

constructor TUICollection.TUICollectionImpl.Create;
begin
  inherited Create;
  FItems := TList<IUnknown>.Create;
  FOwnsItems := True;
  FEnumIndex := 0;
end;

destructor TUICollection.TUICollectionImpl.Destroy;
begin
  if (FOwnsItems) then
    FItems.Free;
  inherited;
end;

function TUICollection.TUICollectionImpl.GetCount: UInt32;
begin
  Result := FItems.Count;
end;

function TUICollection.TUICollectionImpl.GetItem(Index: UInt32): IUnknown;
begin
  Result := FItems[Index];
end;

procedure TUICollection.TUICollectionImpl.Insert(Index: UInt32;
  const Item: IInterface);
begin
  FItems.Insert(Index, Item);
end;

function TUICollection.TUICollectionImpl.Next(celt: Integer; out elt;
  pceltFetched: PLongint): HResult;
var
  I: Integer;
  Items: array of IUnknown;
begin
  I := FItems.Count - FEnumIndex;
  if (celt <= I) then
    Result := S_OK
  else
  begin
    celt := I;
    Result := S_FALSE;
  end;

  if Assigned(pceltFetched) then
    pceltFetched^ := celt;

  if (celt > 0) then
  begin
    SetLength(Items, celt);
    for I := 0 to celt - 1 do
    begin
      Items[I] := FItems[FEnumIndex];
      Inc(FEnumIndex);
    end;
    IUnknown(elt) := Items[0];
  end;
end;

procedure TUICollection.TUICollectionImpl.RemoveAt(Index: UInt32);
begin
  FItems.Delete(Index);
end;

procedure TUICollection.TUICollectionImpl.Replace(IndexReplaces: UInt32;
  const ItemReplaceWith: IInterface);
begin
  FItems[IndexReplaces] := ItemReplaceWith;
end;

function TUICollection.TUICollectionImpl.Reset: HResult;
begin
  FEnumIndex := 0;
  Result := S_OK;
end;

function TUICollection.TUICollectionImpl.Skip(celt: Integer): HResult;
begin
  if ((FEnumIndex + celt) <= FItems.Count) then
  begin
    Inc(FEnumIndex, celt);
    // Sometimes, `celt` can be -1
    if FEnumIndex < 0 then
      FEnumIndex := 0;
    Result := S_OK;
  end
  else
    Result := S_FALSE;
end;

{ TUICollection.TEnumerator }

constructor TUICollection.TEnumerator.Create(const Collection: TUICollection);
begin
  inherited Create;
  FCollection := Collection;
  FIndex := -1;
end;

function TUICollection.TEnumerator.GetCurrent: IUICollectionItem;
begin
  Result := FCollection[FIndex];
end;

function TUICollection.TEnumerator.MoveNext: Boolean;
begin
  if (FIndex >= FCollection.Count) then
    Result := False
  else
  begin
    Inc(FIndex);
    Result := (FIndex < FCollection.Count);
  end;
end;

{ TUIGalleryCollectionItem }

constructor TUIGalleryCollectionItem.Create(const pLabel: String = ''; const pImage: IUIImage = nil);
begin
  inherited Create;
  FLabel := pLabel;
  FImage := pImage;
  FCategoryId := UICollectionInvalidIndex;
end;

function TUIGalleryCollectionItem.GetPropertyValue(
  const Prop: TUIProperty): TValue;
begin
  case Prop of
    upLabel:
      Result := FLabel;

    upCategoryId:
      Result := FCategoryId;

    upCommandId:
      if Assigned(FCommand) then
        Result := FCommand.CommandId
      else
        Result := TValue.Empty;

    upCommandType:
      if Assigned(FCommand) then
        Result := Ord(FCommand.CommandType)
      else
        Result := TValue.Empty;

    upItemImage:
      if Assigned(FImage) then
        Result := TValue.From<IUIImage>(FImage)
      else
        Result := TValue.Empty;
  else
    Result := TValue.Empty;
  end;
end;

{ TUIRecentItem }

function TUIRecentItem.GetPropertyValue(const Prop: TUIProperty): TValue;
begin
  case Prop of
    upLabel:
      Result := FLabel;

    upLabelDescription:
      Result := FDescription;

    upPinned:
      Result := FPinned;
  end;
end;

{ TUIImage }

constructor TUIImage.Create(const ResourceId: Integer);
begin
  inherited Create;
  Load(ResourceId);
end;

constructor TUIImage.Create(const Handle: IUIImage);
begin
  inherited Create;
  FHandle := Handle;
  if Assigned(FHandle) then
  begin
    FBitmap := FHandle.GetBitmap;
    GetBitmapProperties;
  end;
end;

procedure TUIImage.ConvertToAlphaBitmap(const Bitmap: TBitmap);
var
  TransparentColor: Cardinal;
  X, Y: Integer;
  P: PCardinal;
begin
  if (Bitmap.PixelFormat = pf32Bit) or (Bitmap.Width = 0) or (Bitmap.Height = 0) then
    Exit;

  Bitmap.PixelFormat := pf32Bit;
  TransparentColor := PCardinal(Bitmap.ScanLine[Bitmap.Height - 1])^;
  for Y := 0 to Bitmap.Height - 1 do
  begin
    P := Bitmap.ScanLine[Y];
    for X := 0 to Bitmap.Width - 1 do
    begin
      if (P^ = TransparentColor) then
        P^ := 0
      else
        P^ := P^ or $FF000000;
      Inc(P);
    end;
  end;
end;

constructor TUIImage.Create(const Filename: String; const HighContrast: Boolean);
begin
  inherited Create;
  Load(Filename, HighContrast);
end;

constructor TUIImage.Create(const Instance: HINST; const ResourceName: String);
begin
  inherited Create;
  Load(Instance, ResourceName);
end;

constructor TUIImage.Create(const Instance: HINST; const ResourceId: Integer);
begin
  inherited Create;
  Load(Instance, ResourceId);
end;

function TUIImage.CreatePreMultipliedBitmap(const Bitmap: HBitmap): HBitmap;
var
  DC: HDC;
  Info: TBitmapInfo;
  Data: Pointer;
  I, W, H, A, R, G, B: Integer;
  P: PCardinal;
begin
  Result := 0;
  Data := nil;
  DC := CreateCompatibleDC(0);
  try
    FillChar(Info, SizeOf(Info), 0);
    Info.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
    if (GetDIBits(DC, Bitmap, 0, 0, nil, Info, DIB_RGB_COLORS) <> 0) then
    begin
      W := Info.bmiHeader.biWidth;
      H := Abs(Info.bmiHeader.biHeight);
      Info.bmiHeader.biHeight := -H;
      GetMem(Data, W * H * 4);
      if (GetDIBits(DC, Bitmap, 0, H, Data, Info, DIB_RGB_COLORS) <> 0) then
      begin
        P := Data;
        for I := 0 to (W * H) - 1 do
        begin
          A := (P^ shr 24);
          R := (P^ shr 16) and $FF;
          G := (P^ shr 8) and $FF;
          B := P^ and $FF;

          { This should be: R := (R * A) div 255, but this is much faster and
            good enough for display purposes. }
          R := (A * R + 255) shr 8;
          G := (A * G + 255) shr 8;
          B := (A * B + 255) shr 8;

          P^ := (A shl 24) or (R shl 16) or (G shl 8) or B;
          Inc(P);
        end;

        Result := CreateBitmap(W, H, 1, 32, Data);
      end;
    end;
  finally
    DeleteDC(DC);
    FreeMem(Data);
  end;
end;

procedure TUIImage.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TUIImage.Draw(const Target: TCanvas; const XTarget, YTarget, WTarget,
  HTarget: Integer);
var
  SrcDC: HDC;
  Bitmap, OldBitmap: HBitmap;
  BF: TBlendFunction;
begin
  Bitmap := GetBitmap;
  if (Bitmap = 0) then
    Exit;

  SrcDC := CreateCompatibleDC(0);
  try
    if (FBitsPerPixel = 32) then
    begin
      { AlphaBlend requires that the bitmap is pre-multiplied with the Alpha
        values. }
      Bitmap := CreatePreMultipliedBitmap(Bitmap);
      if (Bitmap = 0) then
        Exit;
      try
        OldBitmap := SelectObject(SrcDC, Bitmap);
        BF.BlendOp := AC_SRC_OVER;
        BF.BlendFlags := 0;
        BF.SourceConstantAlpha := 255;
        BF.AlphaFormat := AC_SRC_ALPHA;
        AlphaBlend(Target.Handle, XTarget, YTarget, WTarget, HTarget,
          SrcDC, 0, 0, FWidth, FHeight, BF);
        SelectObject(SrcDC, OldBitmap);
      finally
        DeleteObject(Bitmap);
      end;
    end
    else
    begin
      OldBitmap := SelectObject(SrcDC, Bitmap);
      BitBlt(Target.Handle, XTarget, YTarget, FWidth, FHeight, SrcDC, 0, 0, SRCCOPY);
      SelectObject(SrcDC, OldBitmap);
    end;
  finally
    DeleteDC(SrcDC);
  end;
end;

procedure TUIImage.Draw(const Target: TCanvas; const XTarget, YTarget: Integer);
begin
  Draw(Target, XTarget, YTarget, FWidth, FHeight);
end;

function TUIImage.GetBitmap: HBITMAP;
begin
  if (FBitmap <> 0) then
    Result := FBitmap
  else if Assigned(FHandle) then
  begin
    FBitmap := FHandle.GetBitmap;
    Result := FBitmap
  end
  else
    Result := 0;
end;

procedure TUIImage.GetBitmapProperties;
var
  Props: Windows.TBitmap;
begin
  if (GetObject(FBitmap, SizeOf(Props), @Props) <> 0) then
  begin
    FWidth := Props.bmWidth;
    FHeight := Props.bmHeight;
    FBitsPerPixel := Props.bmBitsPixel;
  end;
end;

procedure TUIImage.Load(const Filename: String; const HighContrast: Boolean);
var
  Ext: String;
begin
  FHandle := nil;
  FBitmap := 0;
  Ext := UpperCase(ExtractFileExt(Filename));
  if (Ext = '.BMP') then
    LoadBmp(Filename, HighContrast)
  else if (Ext = '.PNG') then
    LoadPng(Filename, HighContrast)
  else
    raise EInvalidGraphicOperation.CreateFmt('Unsupported image file extensions (%s)', [Ext]);

  GetBitmapProperties;
  DoChanged;
end;

procedure TUIImage.Load(const Instance: HINST; const ResourceId: Integer);
begin
  Load(Instance, MAKEINTRESOURCE(ResourceId));
end;

procedure TUIImage.Load(const ResourceId: Integer);
begin
  Load(HInstance, ResourceId);
end;

procedure TUIImage.Load(const Instance: HINST; const ResourceName: String);
begin
  Load(Instance, PChar(ResourceName));
end;

procedure TUIImage.Load(const Instance: HINST; const ResourceName: PChar);
begin
  FHandle := nil;
  if Assigned(FImageFactory) then
  begin
    FBitmap := LoadImage(Instance, ResourceName, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION);
    if (FBitmap = 0) then
      raise EInvalidGraphic.Create('Cannot load bitmap');

    try
      FHandle := FImageFactory.CreateImage(FBitmap, UIOwnershipTransfer);
      GetBitmapProperties;
    except
      DeleteObject(FBitmap);
      FBitmap := 0;
    end;
  end;
end;

procedure TUIImage.LoadBmp(const Filename: String; const HighContrast: Boolean);
var
  Bmp: TBitmap;
begin
  if Assigned(FImageFactory) then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.LoadFromFile(Filename);
      if (not HighContrast) then
        ConvertToAlphaBitmap(Bmp);
      FHandle := FImageFactory.CreateImage(Bmp.Handle, UIOwnershipCopy);
      FBitmap := FHandle.GetBitmap;
    finally
      Bmp.Free;
    end;
  end;
end;

procedure TUIImage.LoadPng(const Filename: String; const HighContrast: Boolean);
var
  Png: TPngImage;
  Bmp: TBitmap;
begin
  if Assigned(FImageFactory) then
  begin
    Bmp := nil;
    Png := TPngImage.Create;
    try
      Png.LoadFromFile(Filename);
      Bmp := TBitmap.Create;
      Png.AssignTo(Bmp);
      if (not HighContrast) then
        ConvertToAlphaBitmap(Bmp);
      FHandle := FImageFactory.CreateImage(Bmp.Handle, UIOwnershipCopy);
      FBitmap := FHandle.GetBitmap;
    finally
      Bmp.Free;
      Png.Free;
    end;
  end;
end;

constructor TUIImage.Create(pImageList: TCustomImageList; pImageIndex: TImageIndex);
var
  lBitmap: TBitmap;
begin
  inherited Create;
  if Assigned(FImageFactory) and Assigned(pImageList) then
  begin
    lBitmap := TBitmap.Create;
    try
      lBitmap.Width  := pImageList.Width;
      lBitmap.Height := pImageList.Height;
      lBitmap.PixelFormat := pf32bit;
      lBitmap.Transparent := True;
      if not ImageList_DrawEx(pImageList.Handle, pImageIndex, lBitmap.Canvas.Handle, (lBitmap.Width - pImageList.Width) div 2, (lBitmap.Height - pImageList.Height) div 2, 0, 0, CLR_NONE, CLR_NONE, ILD_TRANSPARENT) then
        exit;
      FHandle := FImageFactory.CreateImage(lBitmap.Handle, UIOwnershipCopy);
      FBitmap := FHandle.GetBitmap;
    finally
      lBitmap.Free;
    end;
  end;
end;

initialization
  {$IFDEF DEBUG}
  Initialize;
  {$ENDIF}
  Notifier := TNotifier.Create;
  TUICommand.FProperties := TUICommandExecutionProperties.Create;
  if not Succeeded(CoCreateInstance(CLSID_UIRibbonImageFromBitmapFactory, nil,
    CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IUnknown, TUIImage.FImageFactory)) then
    TUIImage.FImageFactory := nil;

finalization
  TUICommand.FProperties.Free;
  Notifier.Free;

end.
