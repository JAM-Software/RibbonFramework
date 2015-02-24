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
  UIRibbonApi,
  UIRibbonCommands;

type
  TUIRibbon = class;

  TUIQuickAccessToolbarPosition = (qpTop, qpBottom);

  TUIRibbomCommandEvent = procedure(const Sender: TUIRibbon; const Command: TUICommand) of object;

  TUIRibbon = class(TCustomControl, IUIApplication)
  {$REGION 'Internal Declarations'}
  strict private
    type
      TCommandEnumerator = TEnumerator<TUICommand>;
  strict private
    FFramework: IUIFramework;
    FRibbon: IUIRibbon;
    FResourceName: String;
    FResourceInstance: Integer;
    FCommands: TObjectDictionary<Cardinal, TUICommand>;
    FAvailable: Boolean;
    FOnCommandCreate: TUIRibbomCommandEvent;
    FOnLoaded: TNotifyEvent;
    FLoaded: Boolean;
    function GetCommand(const CommandId: Cardinal): TUICommand;
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
    constructor Create(const Parent: TWinControl;
      const OnCommandCreate: TUIRibbomCommandEvent); reintroduce; overload;

    destructor Destroy; override;

    { Loads the ribbon. }
    procedure Load(); overload;

    { Loads the ribbon with the given resource name from the given resource. }
    procedure Load(const ResourceName: String;
      const ResourceInstance: THandle = 0); overload;

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
    procedure SetApplicationModes(const Modes: Cardinal); overload;

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
      descends from TUIRibbonForm, then this is taken care of automatically. }
    function HandleShortCut(const ShortCut: TShortCut): Boolean;

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
    property Commands[const CommandId: Cardinal]: TUICommand read GetCommand; default;

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

  published

    { The name of the Ribbon resource as it is stored in the resource file. }
    property ResourceName: String read FResourceName write FResourceName;
    { The module instance from which to load the Ribbon resource. }
    property ResourceInstance: Integer read FResourceInstance write FResourceInstance;

    { The event that is fired when the Ribbon Framework creates a command. }
    property OnCommandCreate: TUIRibbomCommandEvent read FOnCommandCreate write FOnCommandCreate;
    { The event that is fired when the Ribbon Framework has been loaded. }
    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;
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
  UITypes,
  UIRibbonUtils;

type
  TUICommandAccess = class(TUICommand);

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

constructor TUIRibbon.Create(const Parent: TWinControl;
  const OnCommandCreate: TUIRibbomCommandEvent);
begin
  Create(Parent);
  FOnCommandCreate := OnCommandCreate;
end;

destructor TUIRibbon.Destroy;
begin
  FFramework := nil;
  FreeAndNil(FCommands);
  inherited;
end;

procedure TUIRibbon.EnableContextTab(const pCommandId: Integer);
begin
  SetContextTabAvailability(pCommandId, TUIContextAvailability.caAvailable);
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

function TUIRibbon.TryGetCommand(const CommandId: Cardinal; out Command: TUICommand): boolean;
begin
  Result := FCommands.TryGetValue(CommandId, Command);
end;

function TUIRibbon.GetCommand(const CommandId: Cardinal): TUICommand;
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

procedure TUIRibbon.Load();
var
  Inst: THandle;
begin
  if (Available) and (inherited Visible) and not (FLoaded) then
  begin
    if (FResourceInstance = 0) then
      Inst := HInstance
    else
      Inst := FResourceInstance;
    FFramework.LoadUI(Inst, PChar(FResourceName + '_RIBBON'));
    FLoaded := True;
    if Assigned(FOnLoaded) then
      FOnLoaded(Self);
  end;
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

procedure TUIRibbon.Load(const ResourceName: String;
  const ResourceInstance: THandle);
begin
  FResourceName := ResourceName;
  FResourceInstance := ResourceInstance;
  Load();
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
      if Assigned(FOnCommandCreate) then
        FOnCommandCreate(Self, Command);
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
          Result := S_OK;

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

function TUIRibbon.SaveSettings(const Stream: TStream): Boolean;
var
  ComStream: IStream;
begin
  Result := Assigned(FRibbon);
  if (Result) then
  begin
    ComStream := TStreamAdapter.Create(Stream, soReference);
    Result := Succeeded(FRibbon.SaveSettingsToStream(ComStream));
  end;
end;

procedure TUIRibbon.SetApplicationModes(const Modes: Cardinal);
begin
  if (FAvailable) then
  begin
    if (Modes = 0) then
      FFramework.SetModes(1)
    else
      FFramework.SetModes(Modes);
  end;
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
  inherited Visible := Value;
  if Assigned(FRibbon) and Supports(FRibbon, IPropertyStore, PropertyStore) then
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
