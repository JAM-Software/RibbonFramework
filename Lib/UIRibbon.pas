unit UIRibbon;

{ Windows Ribbon Framework for Delphi
  ===================================
  Please see http://www.bilsen.com/windowsribbon for detailed information
  about this library.

  Version history:
  5/27/2011: Version 1.0: Initial release by Erik van Bilsen.
  7/30/2011: Version 1.1: Added support for linking UI Commands to Delphi
             actions. See the "Text Pad with Action List" sample application
             and the web site for information on how to do this.
             Also, applications using this framework will also run on
             earlier Windows version now, but without ribbon functionality.
             The TUIRibbon.Available property can be used to check if the
             Ribbon Framework is available on the system. If not, you could
             enable a more traditional user interface.
             These changes were inspired by contributions from
             Patrick Kolla-ten Venne. Many thanks for this! }

interface

uses
  Messages,
  Windows,
  Graphics,
  Generics.Collections,
  Controls,
  Classes,
  ActnList,
  UIRibbonApi,
  UIRibbonCommands;

type
  /// This record stores information about a single ellemt in a ribbon UI
  TRibbonMarkupElement = record
  public
    Name: string;
    ID: integer;
    LabelTitleResourceID: integer;
    LabelDescriptionResourceID: integer;
    TooltipTitleResourceID: integer;
    TooltipDescriptionResourceID: integer;
    /// Returns an initializes record of this type
    constructor Create(pActionName: string; pActionID: integer; pLabelTitleResourceID: integer = -1; pLabelDescriptionResourceID: integer = -1; pTooltipTitleResourceID: integer = -1; pTooltipDescriptionResourceID: integer = -1);
  end;

  /// This class stores elements of the type <see cref="TRibbonMarkupElement>
  TRibbonMarkupElementList = class(TList<TRibbonMarkupElement>)
  strict private
    fResourceName: string;
    class var fContainer: TObjectList<TRibbonMarkupElementList>;
    class constructor Create;
    class destructor Destroy;
  public
    class function LookupListByResourceName(const pResourceName: string): TRibbonMarkupElementList;
    function TryGetItem(pID: integer; out pItem: TRibbonMarkupElement): boolean;
    /// Creates an instance of this class
    /// <param name="pResourceName">The name of the resource of the ribbon to which this list of ribbon elements belongs</param>
    constructor Create(pResourceName: string);
    /// The name of the resource of the ribbon to which this list of ribbon elements belongs
    property ResourceName: string read fResourceName write fResourceName;
  end;


  /// <summary>
  ///  Defines the available user-defined Ribbon application modes. The Windows
  ///  Ribbon Framework supports a set of 32 different application modes. The
  ///  meaning of each mode isn't specified by the framework but can be defined
  ///  individually by the project. Depending on the current application mode,
  ///  Ribbon elements are shown or hidden. Application modes may be combined,
  ///  e.g. if your application defines an application mode for a scenario where
  ///  your app is started with admin privileges and another application mode
  ///  for the trial version, you may combine both modes to express the state
  ///  "Trial version, started with admin privileges".
  /// </summary>
  TRibbonApplicationMode = 0..31;

  /// <summary>
  ///  A set of Application modes (see above).
  /// </summary>
  TRibbonApplicationModes = set of TRibbonApplicationMode;

type
  TUIRibbon = class;

  TUIQuickAccessToolbarPosition = (qpTop, qpBottom);

  TUIRibbomCommandEvent = procedure(const Sender: TUIRibbon; const Command: TUICommand) of object;

  /// <summary>
  /// Signature for an event which is fired when a resource string was loaded.
  /// </summary>
  /// <param name="Sender">The control which fired the event.</param>
  /// <param name="pCommand">the command for which the string was loaded.</param>
  /// <param name="pResourceID">The ID of the resource string that was loaded.</param>
  /// <param name="pString">The default string as it was loaded from the instance, empty if no resource string could be found.</param>
  /// <seealso cref="OnLoadResourceString" />
  /// <author>marder@jam-software.com</author>
  TUILoadResourceStringEvent = procedure(const Sender: TUIRibbon; const pCommand: TUICommand; pResourceID: Integer; var pString: string) of object;

  TUIRibbonOption = (roAutoPreserveState);
  TUIRibbonOptions = set of TUIRibbonOption;

  TUIRibbon = class(TWinControl, IUIApplication)
  {$REGION 'Internal Declarations'}
  strict private
    type
      TCommandEnumerator = TEnumerator<TUICommand>;
  strict private
    FFramework: IUIFramework;
    FRibbon: IUIRibbon;
    FResourceName: String;
    FResourceInstance: THandle;
    FCommands: TObjectDictionary<Cardinal, TUICommand>;
    FAvailable: Boolean;
    FOnCommandCreate: TUIRibbomCommandEvent;
    FOnLoaded: TNotifyEvent;
    FLoaded: Boolean;
    /// member variable for the property RibbonSettingsFilePath
    fRibbonSettingsFilePath: string;
    /// member variable for the property RibbonMapper
    fRibbonMapper: TRibbonMarkupElementList;
    /// <summary>
    ///  The set of currently active Ribbon application modes.
    /// </summary>
    /// <remarks>
    ///  The Ribbon Framework doesn't provide a property or getter method to
    ///  retrieve the current active application modes so we store that
    ///  information using an additional field.
    /// </remarks>
    fApplicationModes: TRibbonApplicationModes;
    /// member variable for the property ActionManager
    fActionManager: TCustomActionList;
    /// Handles the recent items in the backstage menu of the ribbon bar
    fRecentItems: TUICommandRecentItems;
    /// Ribbon configuration options.
    fOptions: TUIRibbonOptions;

    /// <summary>
    ///  Sets the application modes for this Ribbon form.
    /// </summary>
    /// <param name="pAppModes"></param>
    /// <returns>None</returns>
    /// <remarks>
    ///  This method simplifies setting of Ribbon application modes. One can
    ///  define it's own application modes, e.g. like this
    ///    MyRibbonAppMode = (foo = 0, bar = 1);
    ///  and then call SetApplicationModes which converts the set type into
    ///  an array which is required by the Ribbon Framework API.
    /// </remarks>
    procedure Set_ApplicationModes(const pAppModes: TRibbonApplicationModes);
    procedure SetApplicationModes(const Modes: Cardinal); overload;

    function Get_Command(const CommandId: Cardinal): TUICommand;
    function GetBackgroundHsbColor: TUIHsbColor;
    function GetHighlightHsbColor: TUIHsbColor;
    function GetTextHsbColor: TUIHsbColor;
    procedure SetBackgroundHsbColor(const Value: TUIHsbColor);
    procedure SetHighlightHsbColor(const Value: TUIHsbColor);
    procedure SetTextHsbColor(const Value: TUIHsbColor);
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    function GetMinimized: Boolean;
    procedure SetMinimized(const Value: Boolean);
    function GetQuickAccessToolbarPosition: TUIQuickAccessToolbarPosition;
    procedure SetQuickAccessToolbarPosition(
      const Value: TUIQuickAccessToolbarPosition);
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    /// <summary>
    ///  Sets the Availability property of a ribbon context tab command.
    /// </summary>
    /// <param name="pCommandId">The id of the ribbon command.</param>
    /// <param name="pContextAvailability">The availability state.</param>
    procedure SetContextTabAvailability(const pCommandId: Integer; const pContextAvailability: TUIContextAvailability);
  private
    { IInterface }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  private
    fOnLoadResourceString: TUILoadResourceStringEvent;
    { IUIApplication }
    function OnViewChanged(ViewId: UInt32; TypeId: _UIViewType;
      const View: IUnknown; Verb: _UIViewVerb; ReasonCode: Int32): HRESULT; stdcall;

    function OnCreateUICommand(CommandId: UInt32; TypeId: _UICommandType;
      out CommandHandler: IUICommandHandler): HRESULT; stdcall;

    function OnDestroyUICommand(CommandId: UInt32; TypeId: _UICommandType;
      const CommandHandler: IUICommandHandler): HRESULT; stdcall;
    function GetBackgroundColor: TColor;
    function GetHighlightColor: TColor;
    function GetTextColor: TColor;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetHighlightColor(const Value: TColor);
    procedure SetTextColor(const Value: TColor);
  protected
    procedure AddCommand(const Command: TUICommand);
    function GetColor(const PropKey: TUIPropertyKey): TUIHsbColor;
    procedure SetColor(const PropKey: TUIPropertyKey; const Value: TUIHsbColor);
    /// <summary>
    ///  Gets the path of the Ribbon settings file.
    /// </summary>
    /// <returns>Full file path to the Ribbon settings file.</returns>
    function GetRibbonSettingsFilePath(): string;
    /// <summary>
    ///  Load the Ribbon settings.
    /// </summary>
    function LoadRibbonSettings(): boolean;
    /// <summary>
    ///  Save the Ribbon settings.
    /// </summary>
    procedure SaveRibbonSettings();
    /// <summary>
    /// Loads a resource string an fires the <see cref="OnLoadResourceString"> event.
    /// </summary>
    /// <param name="pCommand">The command for which the resource string is loaded.</param>
    /// <param name="pResourceID">The ID of the resource string.</param>
    /// <returns>string</returns>
    /// <remarks></remarks>
    /// <exception>TODO</exception>
    /// <seealso cref="TODO" />
    /// <author>marder@jam-software.com</author>
    function DoLoadResourceString(const pCommand: TUICommand; pResourceID: Integer): string; virtual;
    /// This function is called for every <see "TUICommand"> which is created.
    procedure DoCommandCreated(const pCommand: TUICommand);
    /// <summary>
    ///  Localize the given Ribbon command using the resource identifiers of the given markup item.
    /// </summary>
    procedure LocalizeRibbonElement(const pCommand: TUICommand; const pMarkupItem: TRibbonMarkupElement);

    /// <summary>
    ///  Gets or sets the mapping dictionary, which is automatically created by
    ///  the "Ribbon Designer" of the "Windows Ribbon Framework".
    /// </summary>
    /// <remarks>
    ///  The mapping dictionary is contained in the pascal file, created by the
    ///  the "Ribbon Designer" of the "Windows Ribbon Framework". It contains
    ///  the required mapping between Ribbon command identifier and the
    ///  corresponding Action of the assigned ActionManager.
    /// </remarks>
    property RibbonMapper: TRibbonMarkupElementList read fRibbonMapper write fRibbonMapper;
  {$ENDREGION 'Internal Declarations'}
  public

    { Creates a new ribbon and places it on the Parent control (usually a form).}
    constructor Create(AOwner: TComponent); overload; override;

    { Creates a new ribbon and places it on the Parent control (usually a form).
      The OnCommandCreate is fired each time a command is loaded/created
      for the ribbon. You can use this event to initialize a command right
      after construction.
      NOTE: Ribbon functionality will be disabled when the Available-property
      returns False. }
    constructor Create(const pParent: TWinControl; const pOnCommandCreate: TUIRibbomCommandEvent); reintroduce; overload;

    destructor Destroy; override;

    { Loads the ribbon. }
    procedure Load(); overload;

    { Invalidates on or more aspects from a UI command. This will cause a
      repaint of the specified command. The command will be queried for new
      values based on the invalidation aspects.
        Command: The command to invalidate. If you specify nil, all commands
          will be invalidated.
        Aspects: The aspect(s) of the command(s) you want to invalidate.
          You usually use ciState and/or ciValue. If you want to invalidate
          a specific property, then use the overloaded version that accepts
          the property to invalidate. }
    procedure InvalidateUICommand(const Command: TUICommand;
      const Aspects: TUICommandInvalidations); overload;

    /// <summary>
    ///  Looks up the corresponding TAction for a given Ribbon Command ID.
    /// </summary>
    /// <param name="pIdentifier">The Ribbon command identifier.</param>
    /// <param name="pMatchedAction">Out parameter for the resulting TAction.</param>
    /// <returns>True if a corresponding TAction was found, false otherwise.</returns>
    /// <author>marder@jam-software.com</author>
    function GetActionForCommand(const pCommand: TUICommand): TCustomAction;

    { Invalidates a property from a UI command. This will cause a repaint of
      the specified command.
        Command: The command to invalidate. If you specify nil, all commands
          will be invalidated.
        Prop: The property that needs to be invalidated. }
    procedure InvalidateUICommand(const Command: TUICommand;
      const Prop: TUIProperty); overload;

    { Shows the context popup with the given Id at the given screen location.
      If you don't specify a location, the current mouse position is used.}
    procedure ShowContextPopup(const PopupId: Cardinal; const ScreenPos: TPoint); overload;
    procedure ShowContextPopup(const PopupId: Cardinal); overload;

    { Activates 1 or more application modes. You can either pass an array of
      integer values between 0 and 31, or you can pass a single Cardinal value
      where each bit represents an application mode. }
    procedure SetApplicationModes(const Modes: array of Integer); overload;

    { Saves to ribbon settings to a file or stream. The settings that are saved
      include the visibility and collapsed state of the ribbon, the location
      of the Quick Access Toolbar and the items pinned to the Quick Access
      Toolbar.
      Returns True on success and False on failure. }
    function SaveSettings(const Filename: String): Boolean; overload;
    function SaveSettings(const Stream: TStream): Boolean; overload;

    { Loads the ribbon settings previously saved with SaveSettings.
      Returns True on success and False on failure. }
    function LoadSettings(const Filename: String): Boolean; overload;
    function LoadSettings(const Stream: TStream): Boolean; overload;

    { Allows for..in enumerator over all commands. }
    function GetEnumerator: TCommandEnumerator;

    { Handles a keyboard shortcut by checking if any command handles the
      given shortcut. Returns True if the shortcut is handled.
      You usually don't need to call this method yourself. If your form
      descends from TUIRibbonForm or maps action to ribbon commands
      then this is taken care of automatically. }
    function HandleShortCut(const ShortCut: TShortCut): Boolean; overload;
    function HandleShortCut(const pMessage: TWMKey): Boolean; overload;

    { Tries to retrieve a command with the specified ID from the list of Commands. }
    function TryGetCommand(const CommandId: Cardinal; out Command: TUICommand): boolean;

    { Calls the Update method for those commands that have a TBasicAction
      descendant assigned. This way the command and its assigned action
      synchronize their properties states. }
    procedure InitiateAction; override;

    /// <summary>
    ///  Activates a ribbon context tab.
    /// </summary>
    /// <param name="pCommandId">The ID of the context tab.</param>
    /// <author>marder@jam-software.com</author>
    procedure ActivateContextTab(const pCommandId: Integer);

    /// <summary>
    ///  Enables a ribbon context tab.
    /// </summary>
    /// <param name="pCommandId">The ID of the context tab.</param>
    procedure EnableContextTab(const pCommandId: Integer);

    /// <summary>
    ///  Hides a ribbon context tab.
    /// </summary>
    /// <param name="pCommandId">The ID of the context tab.</param>
    procedure HideContextTab(const pCommandId: Integer);

    /// <summary>
    ///  Hides all ribbon context tabs.
    /// </summary>
    /// <param name="pExceptCommandId">Optional. The ID of the context tab that should be excluded from hiding.</param>
    procedure HideAllContextTabs(pExceptCommandId: Cardinal = High(Cardinal));

    /// <summary>
    ///  Looks up the corresponding Ribbon Command for a given TAction.
    /// </summary>
    /// <param name="pAction"></param>
    /// <returns>TUICommand</returns>
    function GetCommand(pAction: TCustomAction): TUICommand;

    /// <summary>
    ///  Sets the "Recent items" list in the application menu.
    /// </summary>
    /// <param name="pAction">The related action for the recent items.</param>
    /// <param name="pPaths">The list of paths that shall be added to the list.</param>
    procedure SetRecentItems(pAction: TAction; pPaths: TStrings); overload;

    /// <summary>
    ///  Get the currently selected "recent item".
    /// </summary>
    /// <returns>TUIRecentItem</returns>
    function GetSelectedRecentItem(): TUIRecentItem;

    { Whether the UI Ribbon Framework is available on the system.
      Returns False when the application is not running on Windows 7 or
      Windows Vista with the Platform update. In that case, all ribbon
      functionality will be disabled.
      You can use this property to display a "traditional" user interface (such
      as menus and toolbars) when the ribbon framework is not available. }
    property Available: Boolean read FAvailable;

    { Whether the ribbon is currently visible }
    property Visible: Boolean read GetVisible write SetVisible;

    { Whether the ribbon is currently minimized (collapsed) }
    property Minimized: Boolean read GetMinimized write SetMinimized;

    { Where the Quick Access Toolbar is docked relative to the ribbon (above
      or below }
    property QuickAccessToolbarPosition: TUIQuickAccessToolbarPosition read GetQuickAccessToolbarPosition write SetQuickAccessToolbarPosition;

    { The commands that are part of the ribbon, indexed by command ID.
      Accessing a non-existing command will raise an exception.
      NOTE: On startup, this list will only contain the commands that are
      already in use (that is, visible on the ribbon). Commands that are
      unique to the application menu, popup menus or the items of drop-down
      buttons, will only be added once they are needed. }
    property Commands[const CommandId: Cardinal]: TUICommand read Get_Command; default;

    { Background, Highlight and Text Color of the ribbon in HSB (Hue,
      Saturation, Brightness) format. }
    property BackgroundHsbColor: TUIHsbColor read GetBackgroundHsbColor write SetBackgroundHsbColor;
    property HighlightHsbColor: TUIHsbColor read GetHighlightHsbColor write SetHighlightHsbColor;
    property TextHsbColor: TUIHsbColor read GetTextHsbColor write SetTextHsbColor;

    { Background, Highlight and Text Color of the ribbon in regular TColor
      format. }
    property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor;
    property HighlightColor: TColor read GetHighlightColor write SetHighlightColor;
    property TextColor: TColor read GetTextColor write SetTextColor;

    { Low-level access to the Ribbon Framework. }
    property Framework: IUIFramework read FFramework;

    /// <summary>
    ///  Gets or sets the currently active Ribbon application modes.
    /// </summary>
    /// <seealso>TRibbonApplicationModes</seealso>
    property ApplicationModes: TRibbonApplicationModes read fApplicationModes write Set_ApplicationModes;

  published

    { The name of the Ribbon resource as it is stored in the resource file. }
    property ResourceName: String read FResourceName write FResourceName;
    { The module instance from which to load the Ribbon resource. }
    property ResourceInstance: THandle read FResourceInstance write FResourceInstance stored False;

    /// <summary>
    ///  Filename of the XML settings file that is used to store ribbon specific
    ///  settings such as the QuickAccessToolBar items. The default file name is
    ///  "RibbonSettings.xml".
    /// </summary>
    property RibbonSettingsFilePath: string read fRibbonSettingsFilePath write fRibbonSettingsFilePath;

    /// <summary>
    ///  Gets or sets the TActionManager component that is assigned to this Ribbon form.
    /// </summary>
    /// <remarks>
    ///  The Actions defined in the TActionManager component are used to connect
    ///  the Ribbon commands to the Actions of the application automatically.
    ///  You will have to assign the regarding ActionManager in the Main unit of
    ///  your application, e.g. like this:
    ///   Self.RibbonActionManager := MyActionManager;
    /// </remarks>
    property ActionManager: TCustomActionList read fActionManager write fActionManager;

    /// <summary>
    ///   Configuration options affecting the component behaviour.
    /// </summary>
    /// <remarks>
    ///   roAutoPreserveState - If set (default), ribbon settings are automatically
    ///                         saved to/loaded from RibbonSettingsFilePath.
    ///                         <seealso cref="RibbonSettingsFilePath" />
    /// </remarks>
    property Options: TUIRibbonOptions read fOptions write fOptions default [roAutoPreserveState];

    { The event that is fired when the Ribbon Framework creates a command. }
    property OnCommandCreate: TUIRibbomCommandEvent read FOnCommandCreate write FOnCommandCreate;
    { The event that is fired when the Ribbon Framework has been loaded. }
    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;
    /// This event is fired when a resource string was loaded, it allows the resource string to be changed.
    /// <seealso cref="OnLoadResourceString" />
    property OnLoadResourceString: TUILoadResourceStringEvent read fOnLoadResourceString write fOnLoadResourceString;
  end;

  procedure Register;

implementation

uses
  SysUtils,
  Actions,
  ActiveX,
  ComObj,
  Dialogs,
  PropSys,
  Forms,
  Menus,
  GraphUtil,
  UITypes,
  UIRibbonActions,
  UIRibbonUtils;

type
  TUICommandAccess = class(TUICommand);


{ TRibbonMarkupElement }

constructor TRibbonMarkupElement.Create(pActionName: string; pActionID: integer;
      pLabelTitleResourceID: integer = -1;
      pLabelDescriptionResourceID: integer = -1;
      pTooltipTitleResourceID: integer = -1;
      pTooltipDescriptionResourceID: integer = -1);
begin
  Self.Name := pActionName;
  Self.ID := pActionID;
  Self.LabelTitleResourceID := pLabelTitleResourceID;
  Self.LabelDescriptionResourceID := pLabelDescriptionResourceID;
  Self.TooltipTitleResourceID := pTooltipTitleResourceID;
  Self.TooltipDescriptionResourceID := pTooltipDescriptionResourceID;
end;

{ TRibbonMarkupElementList }

constructor TRibbonMarkupElementList.Create(pResourceName: string);
begin
  inherited Create();
  fResourceName := pResourceName;
  if not Assigned(fContainer) then
    fContainer := TObjectList<TRibbonMarkupElementList>.Create(True);
  fContainer.Add(Self);
end;

class constructor TRibbonMarkupElementList.Create;
begin
  fContainer := nil;
end;

class destructor TRibbonMarkupElementList.Destroy;
begin
  FreeAndNil(fContainer);
end;

class function TRibbonMarkupElementList.LookupListByResourceName(const pResourceName: string): TRibbonMarkupElementList;
var
  lElement: TRibbonMarkupElementList;
begin
  if not Assigned(fContainer) then
    Exit(nil);
  for lElement in fContainer do begin
    if SameText(pResourceName, lElement.ResourceName) then
      Exit(lElement);
  end;
  Exit(nil); // No match found
end;

function TRibbonMarkupElementList.TryGetItem(pID: integer; out pItem: TRibbonMarkupElement): boolean;
var
  lElement: TRibbonMarkupElement;
begin
  Assert(Assigned(Self), 'No ribbon mapper assigned!');
  for lElement in Self do
    if lElement.ID = pID then begin
      pItem := lElement;
      Exit(true);
    end;
  pItem := TRibbonMarkupElement.Create('', 0);
  Exit(false);
end;


{ TUIRibbon }

procedure TUIRibbon.ActivateContextTab(const pCommandId: Integer);
begin
  SetContextTabAvailability(pCommandId, TUIContextAvailability.caActive);
end;

procedure TUIRibbon.AddCommand(const Command: TUICommand);
begin
  FCommands.Add(Command.CommandId, Command);
end;

constructor TUIRibbon.Create(AOwner: TComponent);
var
  Intf: IUnknown;
  ParentForm: TCustomForm;
const
  cControlErrorMsg = 'TUIRibbon control can be used on TCustomForm or descendants only.';
begin
  inherited Create(AOwner);

  Color := clWhite;
  Height := 117;
  Align := TAlign.alTop;
  Top := 0;
  ControlStyle := ControlStyle + [csActionClient];

  Options := [roAutoPreserveState];

  FResourceInstance := 0;
  FResourceName := 'APPLICATION';
  FCommands := TObjectDictionary<Cardinal, TUICommand>.Create([doOwnsValues]);

  // Ensure the control is used on a TCustomForm.
  ParentForm := GetParentForm(AOwner as TWinControl);
  if not Assigned(ParentForm) then begin
    if csDesigning in ComponentState then
      MessageDlg(cControlErrorMsg,  mtError, [mbOK], 0)
    else
      raise Exception.Create(cControlErrorMsg);

    exit;
  end;

  Parent := ParentForm; // Make the form the parent of the ribbon control.

  if csDesigning in ComponentState then
    exit; // Initializing the ribbon doesn't work in design time, so exit here.

  FAvailable := Succeeded(CoCreateInstance(CLSID_UIRibbonFramework, nil,
    CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IUnknown, Intf));
  if (not FAvailable) then
    Height := 0
  else
  begin
    FFramework := Intf as IUIFramework;
    FFramework.Initialize(ParentForm.Handle, Self);
  end;
end;

procedure TUIRibbon.WMPaint(var Message: TMessage);
begin
  inherited;
  if Visible then begin
    Load();
	{ Redraw frame to prevent black caption bar on Aero }
    SetWindowPos(Parent.Handle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or
      SWP_NOZORDER or SWP_NOACTIVATE or SWP_DRAWFRAME);
  end;
end;

constructor TUIRibbon.Create(const pParent: TWinControl; const pOnCommandCreate: TUIRibbomCommandEvent);
begin
  Create(pParent);
  FOnCommandCreate := pOnCommandCreate;
end;

destructor TUIRibbon.Destroy;
begin
  if roAutoPreserveState in Options then
    SaveRibbonSettings(); // Save quick toolbar, etc.
  FFramework := nil;
  FreeAndNil(FCommands);
  inherited;
end;

procedure TUIRibbon.DoCommandCreated(const pCommand: TUICommand);
const
  sNoMappingFound = 'Action "%s" that maps to Ribbon command with ID %d was not found! Please ensure to update your Ribbon.Markup.xml file if you have renamed an Action.';
var
  lMarkupItem : TRibbonMarkupElement;
  lAction     : TCustomAction;
begin
  // When a Ribbon command is created, we check if there is a corresponding
  // TAction element available. If so, we assign the properties of that action
  // (Caption, Hint, etc.) to that ribbon element.
  if Assigned(Self.RibbonMapper) and Self.RibbonMapper.TryGetItem(pCommand.CommandId, lMarkupItem) then begin// Get the corresponding TAction for the given Ribbon command
    case pCommand.CommandType of
      TUICommandType.ctAction,
      TUICommandType.ctDecimal,
      TUICommandType.ctBoolean,
      TUICommandType.ctColorAnchor,
      TUICommandType.ctRecentItems:
      begin
        lAction := Self.GetActionForCommand(pCommand);
        if not Assigned(lAction) then
          raise Exception.Create(Format(sNoMappingFound, [lMarkupItem.Name, pCommand.CommandId]))
        else
          pCommand.Assign(lAction);
      end;
    // Try mapping ctAnchor (Tabs) to an action. If found, assign properties.
    // If not found, at least try to localize it.
      TUICommandType.ctAnchor: begin
        lAction := Self.GetActionForCommand(pCommand);
        if Assigned(lAction) then
          pCommand.Assign(lAction)
        else
          Self.LocalizeRibbonElement(pCommand, lMarkupItem);
      end
    else
      // If none of the types above, at least try to localize that command by
      // extracting the corresponding resource strings from the resource file.
      Self.LocalizeRibbonElement(pCommand, lMarkupItem);
    end;
  end;//if RibbonMapper
  if Assigned(FOnCommandCreate) then
    FOnCommandCreate(Self, pCommand);
end;

function TUIRibbon.DoLoadResourceString(const pCommand: TUICommand; pResourceID: Integer): string;
var
  lBuffer : array[0..255] of char;
  lResultCharCount : integer;
begin
  lResultCharCount := LoadString(HInstance, pResourceID,  lBuffer, sizeof(lBuffer));
  if lResultCharCount <> 0 then
    Result := lBuffer;
  if Assigned(Self.OnLoadResourceString) then
    OnLoadResourceString(Self, pCommand, pResourceID, Result);
end;

procedure TUIRibbon.EnableContextTab(const pCommandId: Integer);
begin
  SetContextTabAvailability(pCommandId, TUIContextAvailability.caAvailable);
end;

function TUIRibbon.GetActionForCommand(const pCommand: TUICommand): TCustomAction;
var
  lIndex: integer;
  lElement: TRibbonMarkupElement;
begin
  if not Assigned(fActionManager) then Exit(nil);

  if not RibbonMapper.TryGetItem(pCommand.CommandId, lElement) or lElement.Name.IsEmpty() then Exit(nil);
  for lIndex := 0 to fActionManager.ActionCount - 1 do begin
    // Compare Markup Symbol name against action name.
    if SameText(fActionManager.Actions[lIndex].Name, lElement.Name) then begin
      // Prevent invalid type cast exception
      if not (fActionManager.Actions[lIndex] is TCustomAction) then Exit(nil);
      // Cast and return the corresponding action
      Exit(fActionManager.Actions[lIndex] as TCustomAction);
    end;
  end;
  Exit(nil); // if no corresponding action found, exit with nil
end;

function TUIRibbon.GetBackgroundColor: TColor;
begin
  Result := HsbToColor(GetBackgroundHsbColor);
end;

function TUIRibbon.GetBackgroundHsbColor: TUIHsbColor;
begin
  Result := GetColor(UI_PKEY_GlobalBackgroundColor);
end;

function TUIRibbon.GetColor(const PropKey: TUIPropertyKey): TUIHsbColor;
var
  PropertyStore: IPropertyStore;
  PropValue: TPropVariant;
begin
  Result := 0;
  if Assigned(FFramework) and Supports(FFramework, IPropertyStore, PropertyStore) then
  begin
    if Succeeded(PropertyStore.GetValue(TPropertyKey(PropKey), PropValue)) then
      UIPropertyToUInt32(PropKey, PropValue, Result);
  end;
end;

function TUIRibbon.GetCommand(pAction: TCustomAction): TUICommand;
var
  lElement: TRibbonMarkupElement;
begin
  if (not Self.Visible) then
    exit(nil);
  for lElement in Self.RibbonMapper do begin
    if (lElement.Name = pAction.Name) then begin
      Self.TryGetCommand(lElement.ID, Result);
      Exit(Result);
    end;
  end;
  Exit(nil);
end;

function TUIRibbon.TryGetCommand(const CommandId: Cardinal; out Command: TUICommand): boolean;
begin
  Result := FCommands.TryGetValue(CommandId, Command);
end;

function TUIRibbon.Get_Command(const CommandId: Cardinal): TUICommand;
begin
  if (not TryGetCommand(CommandId, Result)) then
    raise EInvalidOperation.CreateFmt('Command %d does not exist', [CommandId]);
end;

function TUIRibbon.GetEnumerator: TCommandEnumerator;
begin
  Result := FCommands.Values.GetEnumerator;
end;

function TUIRibbon.GetHighlightColor: TColor;
begin
  Result := HsbToColor(GetHighlightHsbColor);
end;

function TUIRibbon.GetHighlightHsbColor: TUIHsbColor;
begin
  Result := GetColor(UI_PKEY_GlobalHighlightColor);
end;

function TUIRibbon.GetMinimized: Boolean;
var
  PropertyStore: IPropertyStore;
  PropValue: TPropVariant;
begin
  Result := True;
  if Assigned(FRibbon) and Supports(FRibbon, IPropertyStore, PropertyStore) then
  begin
    if Succeeded(PropertyStore.GetValue(TPropertyKey(UI_PKEY_Minimized), PropValue)) then
      UIPropertyToBoolean(UI_PKEY_Minimized, PropValue, Result);
  end;
end;

function TUIRibbon.GetQuickAccessToolbarPosition: TUIQuickAccessToolbarPosition;
var
  PropertyStore: IPropertyStore;
  PropValue: TPropVariant;
  Dock: Cardinal;
begin
  Result := qpTop;
  if Assigned(FRibbon) and Supports(FRibbon, IPropertyStore, PropertyStore) then
  begin
    if Succeeded(PropertyStore.GetValue(TPropertyKey(UI_PKEY_QuickAccessToolbarDock), PropValue)) then
    begin
      UIPropertyToUInt32(UI_PKEY_QuickAccessToolbarDock, PropValue, Dock);
      if (Integer(Dock) = Ord(UIControlDockBottom)) then
        Result := qpBottom;
    end;
  end;
end;

function TUIRibbon.GetTextColor: TColor;
begin
  Result := HsbToColor(GetTextHsbColor);
end;

function TUIRibbon.GetTextHsbColor: TUIHsbColor;
begin
  Result := GetColor(UI_PKEY_GlobalTextColor);
end;

function TUIRibbon.GetVisible: Boolean;
var
  PropertyStore: IPropertyStore;
  PropValue: TPropVariant;
begin
  if not Available then
    Exit(False);
  Result := inherited Visible;
  if Assigned(FRibbon) and Supports(FRibbon, IPropertyStore, PropertyStore) then
  begin
    if Succeeded(PropertyStore.GetValue(TPropertyKey(UI_PKEY_Viewable), PropValue)) then
      UIPropertyToBoolean(UI_PKEY_Viewable, PropValue, Result);
  end;
end;

function TUIRibbon.HandleShortCut(const ShortCut: TShortCut): Boolean;
var
  Command: TUICommand;
begin
  for Command in FCommands.Values do
    if (Command.HandleShortCut(ShortCut)) then
      Exit(True);
  Result := False;
end;

function TUIRibbon.HandleShortCut(const pMessage: TWMKey): Boolean;
var
  lShiftState: TShiftState;
  lShortCut: TShortCut;
begin
  lShiftState := KeyDataToShiftState(pMessage.KeyData);
  lShortCut := Menus.ShortCut(pMessage.CharCode, lShiftState);
  Result := (lShortCut <> scNone) and Self.HandleShortCut(lShortCut);
end;

procedure TUIRibbon.HideAllContextTabs(pExceptCommandId: Cardinal);
var
  lCommand: TUICommand;
begin
  //If Ribbons are disabled, exit here.
  if (not Self.Visible) then
    exit;
  // Iterate over the available ribbon elements and hide them
  for lCommand in Self do begin
    if (lCommand.CommandType = TUICommandType.ctContext) and (lCommand.CommandId <> pExceptCommandId) then
      HideContextTab(lCommand.CommandId);
  end;//for
end;

procedure TUIRibbon.HideContextTab(const pCommandId: Integer);
begin
  SetContextTabAvailability(pCommandId, TUIContextAvailability.caNotAvailable);
end;

procedure TUIRibbon.InitiateAction;
var
  Command: TUICommand;
begin
  inherited;
  for Command in FCommands.Values do begin
    if Assigned(Command.ActionLink.Action) then begin
      if not (Command.CommandType in [TUICommandType.ctRecentItems]) then // Recently used items will be updated when the File menu is opened, done in TUICommandRecentItems.DoUpdate()
        Command.ActionLink.Action.Update();
      // Ensure that ribbon command has enabled same state as assigned action.
      if Command.ActionLink.Action is TContainedAction then
        Command.Enabled := (Command.ActionLink.Action as TContainedAction).Enabled and (Command.ActionLink.Action as TContainedAction).Visible;
    end;
  end;
end;

procedure TUIRibbon.InvalidateUICommand(const Command: TUICommand;
  const Prop: TUIProperty);
var
  CommandId: Cardinal;
  Key: PUIPropertyKey;
begin
  if (FAvailable) then
  begin
    Key := GetPropertyKey(Prop);
    if Assigned(Key) then
    begin
      if Assigned(Command) then
        CommandId := Command.CommandId
      else
        CommandId := UIAllCommands;
      FFramework.InvalidateUICommand(CommandId, [UIInvalidationsProperty], Key);
    end;
  end;
end;

function TUIRibbon.GetRibbonSettingsFilePath(): string;
const
  cRibbonSettingsDefaultFileName = 'RibbonOptions.xml';
var
  lRibbonFilename: string;
begin
  if fRibbonSettingsFilePath.IsEmpty() and not (csDesigning in ComponentState) then begin
    lRibbonFilename := GetParentForm(Self).Name + '_' + cRibbonSettingsDefaultFileName;
    fRibbonSettingsFilePath := ExtractFilePath(ParamStr(0)) + lRibbonFilename;
    // Portable editions have their settings stored beside the executable.
    if not FileExists(fRibbonSettingsFilePath) then begin
      // If no settings file found beside the executable, we use appdata path, e.g.
      // "C:\Users\foo\AppData\Roaming\My Application"
      fRibbonSettingsFilePath := IncludeTrailingPathDelimiter(GetHomePath) + Application.Title + PathDelim + lRibbonFilename;
    end;//if
  end;//if
  Exit(fRibbonSettingsFilePath);
end;

function TUIRibbon.GetSelectedRecentItem: TUIRecentItem;
begin
  Result := (fRecentItems.ActionLink as TUICommandRecentItemsActionLink).Selected;
end;

function TUIRibbon.LoadSettings(const Filename: String): Boolean;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadSettings(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TUIRibbon.Load();
resourcestring
  sErrorLoadingRibbonRessource = 'An error occurred while trying to load the Ribbon resource "%s": %s';
var
  Inst: THandle;
  lForm: TCustomForm;
begin
  if (Available) and (inherited Visible) and not (FLoaded) then
  begin
    // Load mapper for mapping between commands and VCL actions
    fRibbonMapper := TRibbonMarkupElementList.LookupListByResourceName(FResourceName);
    if (FResourceInstance = 0) then
      Inst := HInstance
    else
      Inst := FResourceInstance;
    try
      FFramework.LoadUI(Inst, PChar(FResourceName + '_RIBBON'));
      FLoaded := True;
      if Assigned(FOnLoaded) then
        FOnLoaded(Self);
      if roAutoPreserveState in Options then
        LoadRibbonSettings();
    except
      on E: EOleException do begin
        E.Message := Format(sErrorLoadingRibbonRessource, [Self.ResourceName, e.Message]);
        raise;
      end;
    end;//try..except

    lForm := GetParentForm(Self);
    // Set the background color for the form if not yet defined. Use the same
    // color as the ribbon bar, just a bit more brightened, if themes are enabled.
    if (lForm.Color = clBtnFace) or (lForm.Color = clWindow) then begin
      if TOSVersion.Check(6, 2) then
        // For Windows 8 and later, we simply set a nearly white background. This is
        // because some changes to the ribbon color attributes have been introduced
        // with Windows 8, due to that we get bogus color values when querying the
        // BackgroundColor property. See the following thread:
        // http://social.msdn.microsoft.com/Forums/windowsdesktop/en-US/b38da1e9-34a0-440a-bdf0-bb293940dd0c/win8-ribbon-colors
        lForm.Color := RGB(254,254,254)
      else
        lForm.Color := GraphUtil.GetHighLightColor(Self.BackgroundColor);//  TColorHelper.IncreaseRgbValues(FRibbon.BackgroundColor, 17, 12, 10)
    end;//if clBtnFace
  end;
end;

function TUIRibbon.LoadRibbonSettings(): boolean;
var
  lSettingsFileFullPath: string;
begin
  // If Ribbons are not available, do not continue.
  if not Self.Visible then
    exit(false);
  // Get the path for the settings file (beside executable or %appdata%)
  lSettingsFileFullPath := GetRibbonSettingsFilePath();
  // If file not exists or is just a dummy file (portable editions), exit here.
  if not FileExists(RibbonSettingsFilePath) { or (FileSize(lSettingsFileFullPath) = 0)} then
    exit(false);
  // Otherwise, try to load the file.
  Result := Self.LoadSettings(lSettingsFileFullPath);
  Assert(Result, 'Loading ribbon settings failed with unknown error. File: ' + lSettingsFileFullPath);
end;

function TUIRibbon.LoadSettings(const Stream: TStream): Boolean;
var
  ComStream: IStream;
begin
  Result := Assigned(FRibbon);
  if (Result) then
  begin
    ComStream := TStreamAdapter.Create(Stream, soReference);
    Result := Succeeded(FRibbon.LoadSettingsFromStream(ComStream));
  end;
end;

procedure TUIRibbon.LocalizeRibbonElement(const pCommand: TUICommand; const pMarkupItem: TRibbonMarkupElement);
begin
  // Caption
  if (pMarkupItem.LabelTitleResourceID <> -1) then
    pCommand.Caption := DoLoadResourceString(pCommand, pMarkupItem.LabelTitleResourceID);
  // Description
  if (pMarkupItem.LabelDescriptionResourceID <> -1) and (pCommand.CommandType = TUICommandType.ctAction) then
    (pCommand as TUICommandAction).LabelDescription := DoLoadResourceString(pCommand, pMarkupItem.LabelDescriptionResourceID);
  // ToolTip title
  if (pMarkupItem.TooltipTitleResourceID <> -1) then
    pCommand.TooltipTitle := DoLoadResourceString(pCommand, pMarkupItem.TooltipTitleResourceID);
  // ToolTip description
  if (pMarkupItem.TooltipDescriptionResourceID <> -1) then
    pCommand.TooltipDescription := DoLoadResourceString(pCommand, pMarkupItem.TooltipDescriptionResourceID);

  if (pCommand.CommandType = TUICommandType.ctAnchor) and (not pCommand.Caption.IsEmpty()) then
    pCommand.Keytip := Trim(pCommand.Caption)[1];
end;

procedure TUIRibbon.InvalidateUICommand(const Command: TUICommand;
  const Aspects: TUICommandInvalidations);
var
  CommandId: Cardinal;
begin
  if (FAvailable) then
  begin
    if Assigned(Command) then
      CommandId := Command.CommandId
    else
      CommandId := UIAllCommands;
    FFramework.InvalidateUICommand(CommandId, _UIInvalidations(Aspects), nil);
  end;
end;

function TUIRibbon.OnCreateUICommand(CommandId: UInt32; TypeId: _UICommandType;
  out CommandHandler: IUICommandHandler): HRESULT;
var
  CommandClass: TUICommandClass;
  Command: TUICommand;
begin
  CommandClass := UI_COMMAND_CLASSES[TUICommandType(TypeId)];
  if (CommandClass = nil) then
    Result := E_FAIL
  else
  begin
    if (not FCommands.TryGetValue(CommandId, Command)) then
    begin
      Command := CommandClass.Create(Self, CommandId);
      DoCommandCreated(Command);
    end;

    CommandHandler := Command;
    TUICommandAccess(Command).Alive := True;
    Result := S_OK;
  end;
end;

function TUIRibbon.OnDestroyUICommand(CommandId: UInt32; TypeId: _UICommandType;
  const CommandHandler: IUICommandHandler): HRESULT;
begin
  FCommands.Remove(CommandId);
  Result := S_OK;
end;

function TUIRibbon.OnViewChanged(ViewId: UInt32; TypeId: _UIViewType;
  const View: IInterface; Verb: _UIViewVerb; ReasonCode: Int32): HRESULT;
var
  NewHeight: Integer;
begin
  Result := E_NOTIMPL;
  try
    { Checks to see if the view that was changed was a Ribbon view. }
    if (TypeId = UIViewTypeRibbon) then
    begin
      case Verb of
        { The view was newly created. }
        UIViewVerbCreate:
          begin
            FRibbon := View as IUIRibbon;
            Result := S_OK;
          end;
        { The view has been resized. For the Ribbon view, the application should
          call GetHeight to determine the height of the ribbon. }
        UIViewVerbSize:
          begin
            FRibbon := View as IUIRibbon;
            { Call to the framework to determine the desired height of the Ribbon. }
            NewHeight := FRibbon.GetHeight;
            if (NewHeight <> Height) then
            begin
              Height := NewHeight;
              { Realign controls to fit into the new client area }
              Parent.Realign;
              Parent.Invalidate;
            end;
            Result := S_OK;
          end;

        { The view was destroyed. }
        UIViewVerbDestroy:
          Result := S_OK;
      end;
    end;
  except
    on E: EOleSysError do
      Result := E.ErrorCode
    else
      Result := E_FAIL;
  end;
end;

procedure TUIRibbon.SetApplicationModes(const Modes: array of Integer);
var
  AppModes: Cardinal;
  Mode: Integer;
begin
  AppModes := 0;
  for Mode in Modes do
    if (Mode >= 0) and (Mode <= 31) then
      AppModes := AppModes or (1 shl Mode);
  SetApplicationModes(AppModes);
end;

function TUIRibbon.SaveSettings(const Filename: String): Boolean;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    Result := SaveSettings(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TUIRibbon.SaveRibbonSettings;
begin
  // If Ribbons are not available, do not continue.
  if not fLoaded then
    exit;
  // Save ribbon user settings
  try
    ForceDirectories(ExtractfilePath(RibbonSettingsFilePath));
    Self.SaveSettings(RibbonSettingsFilePath);
  except
    on E: EFileStreamError do
    begin
      OutputDebugString(PChar('An EFileStreamError error occured while trying to save the ribbon settings: ' + E.Message));
      {$ifdef DEBUG}
      raise e;
      {$endif}
    end;
  end;
end;

function TUIRibbon.SaveSettings(const Stream: TStream): Boolean;
var
  ComStream: IStream;
begin
  Result := Assigned(FRibbon);
  if (Result) and fLoaded then
  begin
    ComStream := TStreamAdapter.Create(Stream, soReference);
    Result := Succeeded(FRibbon.SaveSettingsToStream(ComStream));
  end;
end;

procedure TUIRibbon.SetApplicationModes(const Modes: Cardinal);
begin
  if (FAvailable) then
  begin
    Load();
    if (Modes = 0) then
      FFramework.SetModes(1)
    else
      FFramework.SetModes(Modes);
  end;
end;

procedure TUIRibbon.Set_ApplicationModes(const pAppModes: TRibbonApplicationModes);
var
  lIndex: integer;
  lElement: TRibbonApplicationMode;
  lArray: array of Integer;
begin
  if (not Self.Visible) then
    exit;
  lIndex := 0;
  for lElement in pAppModes do begin
    SetLength(lArray, lIndex+1);
    lArray[lIndex] := lElement;
    Inc(lIndex);
  end;
  Self.SetApplicationModes(lArray);
  fApplicationModes := pAppModes;
end;

procedure TUIRibbon.SetBackgroundColor(const Value: TColor);
begin
  SetBackgroundHsbColor(ColorToHsb(Value));
end;

procedure TUIRibbon.SetBackgroundHsbColor(const Value: TUIHsbColor);
begin
  SetColor(UI_PKEY_GlobalBackgroundColor, Value);
end;

procedure TUIRibbon.SetColor(const PropKey: TUIPropertyKey;
  const Value: TUIHsbColor);
var
  PropertyStore: IPropertyStore;
  PropValue: TPropVariant;
begin
  if Assigned(FFramework) and Supports(FFramework, IPropertyStore, PropertyStore) then
  begin
    UIInitPropertyFromUInt32(PropKey, Value, PropValue);
    PropertyStore.SetValue(TPropertyKey(PropKey), PropValue);
    PropertyStore.Commit;
  end;
end;

procedure TUIRibbon.SetContextTabAvailability(const pCommandId: Integer; const pContextAvailability: TUIContextAvailability);
var
  lCommand: TUICommand;
begin
  //If Ribbons are disabled, exit here.
  if (not Self.Visible) then
    exit;
  // Get the command with that ID
  if not Self.TryGetCommand(pCommandId, lCommand) then exit;
  // If found, check type, cast and set Availability property.
  if (lCommand.CommandType = TUICommandType.ctContext) then
    (lCommand as TUICommandContext).Availability := pContextAvailability;
end;

procedure TUIRibbon.SetHighlightColor(const Value: TColor);
begin
  SetHighlightHsbColor(ColorToHsb(Value));
end;

procedure TUIRibbon.SetHighlightHsbColor(const Value: TUIHsbColor);
begin
  SetColor(UI_PKEY_GlobalHighlightColor, Value);
end;

procedure TUIRibbon.SetMinimized(const Value: Boolean);
var
  PropertyStore: IPropertyStore;
  PropValue: TPropVariant;
begin
  if Assigned(FRibbon) and Supports(FRibbon, IPropertyStore, PropertyStore) then
  begin
    UIInitPropertyFromBoolean(UI_PKEY_Minimized, Value, PropValue);
    PropertyStore.SetValue(TPropertyKey(UI_PKEY_Minimized), PropValue);
    PropertyStore.Commit;
  end;
end;

procedure TUIRibbon.SetQuickAccessToolbarPosition(
  const Value: TUIQuickAccessToolbarPosition);
var
  PropertyStore: IPropertyStore;
  PropValue: TPropVariant;
  Dock: Cardinal;
begin
  if Assigned(FRibbon) and Supports(FRibbon, IPropertyStore, PropertyStore) then
  begin
    if (Value = qpBottom) then
      Dock := Ord(UIControlDockBottom)
    else
      Dock := Ord(UIControlDockTop);
    UIInitPropertyFromUInt32(UI_PKEY_QuickAccessToolbarDock, Dock, PropValue);
    PropertyStore.SetValue(TPropertyKey(UI_PKEY_QuickAccessToolbarDock), PropValue);
    PropertyStore.Commit;
  end;
end;

procedure TUIRibbon.SetRecentItems(pAction: TAction; pPaths: TStrings);
var
  lItem: TUIRecentItem;
  lPath: string;
begin
  if (not Self.Visible) then
    exit;

  if not Assigned(fRecentItems) then begin
    fRecentItems := Self.GetCommand(pAction) as TUICommandRecentItems;
  end;

  fRecentItems.Items.Clear();

  for lPath in pPaths do begin
    lItem := TUIRecentItem.Create;
    lItem.LabelText := lPath;
    fRecentItems.Items.Add(lItem);
  end;
end;

procedure TUIRibbon.SetTextColor(const Value: TColor);
begin
  SetTextHsbColor(ColorToHsb(Value));
end;

procedure TUIRibbon.SetTextHsbColor(const Value: TUIHsbColor);
begin
  SetColor(UI_PKEY_GlobalTextColor, Value);
end;

procedure TUIRibbon.SetVisible(const Value: Boolean);
var
  PropertyStore: IPropertyStore;
  PropValue: TPropVariant;
begin
  if Value then
    Self.Load();
  inherited Visible := Value;
  if Assigned(FRibbon) and Supports(FRibbon, IPropertyStore, PropertyStore) and FLoaded then
  begin
    UIInitPropertyFromBoolean(UI_PKEY_Viewable, Value, PropValue);
    PropertyStore.SetValue(TPropertyKey(UI_PKEY_Viewable), PropValue);
    PropertyStore.Commit;
  end;
end;

procedure TUIRibbon.ShowContextPopup(const PopupId: Cardinal);
begin
  ShowContextPopup(PopupId, Mouse.CursorPos);
end;

procedure TUIRibbon.ShowContextPopup(const PopupId: Cardinal;
  const ScreenPos: TPoint);
var
  Intf: IInterface;
  ContextualUI: IUIContextualUI;
begin
  if (FAvailable) then
  begin
    Intf := Framework.GetView(PopupId, IUIContextualUI);
    if Assigned(Intf) and Supports(Intf, IUIContextualUI, ContextualUI) then
      ContextualUI.ShowAtLocation(ScreenPos.X, ScreenPos.Y);
  end;
end;

function TUIRibbon._AddRef: Integer;
begin
  Result := -1;
end;

function TUIRibbon._Release: Integer;
begin
  Result := -1;
end;

procedure Register;
begin
  RegisterComponents('Windows Ribbon Framework for Delphi', [TUIRibbon]);
end;

end.
