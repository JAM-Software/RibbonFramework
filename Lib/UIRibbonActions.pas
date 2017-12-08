unit UIRibbonActions;

interface

uses
  System.Classes,
  Vcl.Menus,
  ActnList,
  ActnMan,
  UIRibbonCommands,
  System.Generics.Collections,
  Winapi.ActiveX, 
  UIRibbonApi;

type
  TUICommandActionLink = class abstract (TActionLink)
  {$REGION 'Internal Declarations'}
  strict private
    FClient: TUICommand;
  {$ENDREGION 'Internal Declarations'}
  protected
    procedure SetAction(Value: TBasicAction); override;
    procedure AssignClient(AClient: TObject); override;
    function IsEnabledLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    procedure SetCaption(const Value: String); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetHint(const Value: String); override;
    procedure SetShortCut(Value: System.Classes.TShortCut); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;

    property Client: TUICommand read FClient;
  public
    constructor Create(const AClient: TUICommand); reintroduce;
    function Update: Boolean; override;
  end;

  TUICommandEmptyActionLink = class(TUICommandActionLink)
    { No additional declarations }
  end;

  TUICommandActionActionLink = class(TUICommandActionLink)
  {$REGION 'Internal Declarations'}
  strict private
    procedure CommandExecute(const Args: TUICommandActionEventArgs);
  {$ENDREGION 'Internal Declarations'}
  protected
    procedure SetAction(Value: TBasicAction); override;
  end;

  TUICommandCollectionActionLink = class(TUICommandActionLink)
  {$REGION 'Internal Declarations'}
  strict private
    procedure CommandSelect(const Args: TUICommandCollectionEventArgs);
  {$ENDREGION 'Internal Declarations'}
  protected
    procedure SetAction(Value: TBasicAction); override;
  end;

  TUICommandDecimalActionLink = class(TUICommandActionLink)
  {$REGION 'Internal Declarations'}
  strict private
    procedure CommandChange(const Command: TUICommandDecimal;
      const Verb: TUICommandVerb; const Value: Double;
      const Properties: TUICommandExecutionProperties);
  {$ENDREGION 'Internal Declarations'}
  protected
    procedure SetAction(Value: TBasicAction); override;
  end;

  TUICommandBooleanActionLink = class(TUICommandActionLink)
  {$REGION 'Internal Declarations'}
  strict private
    procedure CommandToggle(const Args: TUICommandBooleanEventArgs);
  {$ENDREGION 'Internal Declarations'}
  protected
    procedure SetAction(Value: TBasicAction); override;
    procedure SetChecked(Value: Boolean); override;
  end;

  TUICommandFontActionLink = class(TUICommandActionLink)
  {$REGION 'Internal Declarations'}
  strict private
    procedure CommandChanged(const Args: TUICommandFontEventArgs);
  {$ENDREGION 'Internal Declarations'}
  protected
    procedure SetAction(Value: TBasicAction); override;
  end;

  TUICommandColorAnchorActionLink = class(TUICommandActionLink)
  {$REGION 'Internal Declarations'}
  strict private
    procedure CommandExecute(const Args: TUICommandColorEventArgs);
  {$ENDREGION 'Internal Declarations'}
  protected
    procedure SetAction(Value: TBasicAction); override;
  end;

  TUICommandRecentItemsActionLink = class(TUICommandActionLink)
  {$REGION 'Internal Declarations'}
  strict private
    fSelected: TUIRecentItem;
    procedure CommandSelect(const Command: TUICommandRecentItems;
      const Verb: TUICommandVerb; const ItemIndex: Integer;
      const Properties: TUICommandExecutionProperties);
  {$ENDREGION 'Internal Declarations'}
  protected
    procedure SetAction(Value: TBasicAction); override;
  public
    //Added property "Selected" to allow access to selected item.
    //TODO: Create a custom action that provides the required properties.
    property Selected: TUIRecentItem read fSelected write fSelected;
  end;

  TRibbonAction<T:TUICommand> = class(TCustomAction)
  private
    fUICommand: T;
  public
    property UICommand: T read fUICommand write fUICommand;
  published
    property Caption;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property SecondaryShortCuts;
    property ShortCut default 0;
    property OnExecute;
    property OnHint;
    property OnUpdate;
  end;

  TRibbonCollectionAction = class(TRibbonAction<TUICommandCollection>)
    strict private
      fActionList: TList<TCustomAction>;
      fSelectedItem: Integer;
      fSelectionInitialized: Boolean;
      fOriginalOnSelect: TUICommandCollectionSelectEvent;
      /// This flag indicates that a refresh should be performed as soon as the collection is not being displayed anymore.
      fRefreshWhenNotDisplayed: Boolean;
      function GetItem(pIndex: Integer): TCustomAction;
      procedure SetSelectedItem(const pValue: Integer);
      /// Triggered on user induced selection changes. This handler will apply them to the action.
      procedure UICommandItemSelected(const Args: TUICommandCollectionEventArgs);
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      /// <summary>
      /// Adds an action to the internal list and populates it to the command
      /// </summary>
      /// <param name="pAction">The action that will be added to the collection.</param>
      /// The action's category will be used for the ribbon command as well.</param>
      /// <returns>None</returns>collection
      procedure Add(pAction: TCustomAction); overload;
      procedure Add(const pLabel: string); overload;
      /// <summary>
      /// Removes an action from the internal list and populates it to the command.
      /// </summary>
      /// <param name="pAction">The action that will be added to the collection.</param>
      procedure Remove(pAction: TCustomAction);
      /// Clears the internal list and the command collection.
      procedure Clear;
      /// Allows to add multiple actions add once.
      procedure AddRange(pSource: TList<TCustomAction>);
      /// <summary>
      /// Update handler for TUICommand.UpdateProperty. We assign this handler to the commands' OnUpdateProperty event, so that we can remove
      /// them from the collection as soon as they become invisible.
      /// </summary>
      procedure PropertyUpdated(Sender: TObject; const PropKey: TUIPropertyKey; var NewValue: TPropVariant; var Handled: boolean);
      /// <summary>
      /// This method uses the action items that are stored in the internal list fActionList,
      /// and dynamically creates commands that will be added to the collection.
      /// </summary>
      procedure RefreshCommandCollection();
      function Update(): Boolean; override;
      /// Returns the amount of actions that have been added.
      function ItemCount: Integer;
      function GetEnumerator: TEnumerator<TCustomAction>;
      /// <summary>
      /// This function indicates whether the collection is currently being displayed. The default implementation uses the visible state
      /// of the action. You might want to override this function in a subclass if you want to perform a more suffisticated check.
      /// </summary>
      function IsCurrentlyDisplayed: Boolean; virtual;
      property Items[Index: Integer]: TCustomAction read GetItem; default;
      /// <summary> This property correlates to the ribbon collection's SelecteItem property. We additionally store the value in member variable fSelectedItem and apply
      /// the index during the first call to the action's Update() method.
      /// </summary>
      property SelectedItem: Integer read fSelectedItem write SetSelectedItem;
    published
      property ImageIndex;
  end;

  /// <summary>
  /// This type of action can be used to link a TPopupMenu to a ribbon collection. The menu items, or rather their actions,
  /// will automatically be populated to the collection.
  /// </summary>
  TRibbonPopupMenuAction = class(TRibbonCollectionAction)
    strict private
      fPopupMenu: TPopupMenu;
      fOriginalOnMenuChange: TMenuChangeEvent;
    strict protected
      /// <summary> Setter for property Menu </summary>
      procedure SetPopupMenu(pValue: TPopupMenu);
      /// <summary> Event handler for the menu's OnChange-event. We use this to update the collection. </summary>
      procedure MenuChange(Sender: TObject; Source: TMenuItem; Rebuild: Boolean);
    published
      /// <summary>
      /// Property that can be used to dynamically fill the collection items:
      /// If this type of action is assigned to a collection type ribbon, such as a Drop Down Gallery,
      /// the collection is automatically filled using the menu items of this property.
      /// </summary>
      property Menu: TPopupMenu read fPopupMenu write SetPopupMenu;
  end;

  TRibbonColorAction = class(TRibbonAction<TUICommandColorAnchor>)
  end;

  TRibbonFontAction = class(TRibbonAction<TUICommandFont>)
  strict private
    fOnChanged: TUICommandFontChangedEvent;
  published
    { Fired when one or more of the font properties has changed.
      When the Verb is cvExecute or cvPreview, then the Font parameter of the
      event contains the new font settings. Otherwise, the Font parameter
      contains the current font settings. }
    property OnChanged: TUICommandFontChangedEvent read fOnChanged write fOnChanged;
  end;


implementation

uses
  Controls,
  UIRibbon,
  {$if CompilerVersion >= 24}
  System.Actions,
  {$endif}
  System.SysUtils,
  System.Math;

const
  cNoSelection = -1;

{ TUICommandActionLink }

procedure TUICommandActionLink.AssignClient(AClient: TObject);
begin
  inherited;
  FClient := AClient as TUICommand;
end;

constructor TUICommandActionLink.Create(const AClient: TUICommand);
begin
  inherited Create(AClient);
end;

function TUICommandActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

function TUICommandActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited and (FClient.UseImageFromAction or (TUIRibbonOption.roAssignImagesFromActionManager in TUIRibbon(FClient.Owner).Options));
end;

function TUICommandActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := False;
end;

procedure TUICommandActionLink.SetAction(Value: TBasicAction);
begin
  inherited;
  if Value is TCustomAction then with TCustomAction(Value) do
  begin
    if FClient.CommandType <> TUICommandType.ctAction then
      DisableIfNoHandler := False;
    // Trigger assigned OnUpdate method to determine whether the Ribbon command
    // shall be enabled or disabled (greyed out).
    Value.Update();
    Self.SetEnabled(Enabled and Visible);
    // Caption of the Ribbon Command
    Self.SetCaption(Caption);
    Self.SetHint(Hint);
    Self.SetChecked(Checked);
    Self.SetGroupIndex(GroupIndex);
    Self.SetShortCut(ShortCut);
    Self.SetImageIndex(ImageIndex);
  end;// if/with
end;

procedure TUICommandActionLink.SetCaption(const Value: String);
const
  cAmpersand = '&';
begin
  if IsCaptionLinked and (Value <> '') then begin
    FClient.Caption := Trim(Value.Replace('...', ''));// Remove trailing dots, they are uncommon in ribbon bars
    // Tooltip Title (bold string above the actual Tooltip)
    // Using the value of caption here because common Microsoft products do this as well.
    FClient.TooltipTitle := Value;
    // If action caption contains an ampersand (&), use the char following that
    // ampersand as Keytip for the ribbon element so that ALT+Char can be used the
    // same way as on regular VCL controls.
    if Value.Contains(cAmpersand) then
    begin
      FClient.Keytip := UpperCase(Value[Value.IndexOf(cAmpersand) + 2]);
    end;
  end;
  // For some reasons, the Windows Ribbon Framework makes the ToolTipTitle
  // invisible, if it equals the Commands Caption property. To aovid this, we
  // assign an additional space to the end of the string here.
  FClient.TooltipTitle := FClient.TooltipTitle + ' ';
end;

procedure TUICommandActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then
    FClient.Enabled := Value;
end;

procedure TUICommandActionLink.SetVisible(Value: Boolean);
var
  lCurrentValue, lNewValue: TPropVariant;
begin
  inherited;
  // The Windows ribbon framework does not off to make a button invisible at runtime, so we at least disable the button
  if not Value then
    FClient.Enabled := False;
  if Succeeded(UIInitPropertyFromBoolean(UI_PKEY_Viewable, TCustomAction(Action).Visible, lCurrentValue))
  and Succeeded(UIInitPropertyFromBoolean(UI_PKEY_Viewable, Value, lNewValue)) then
    (FClient as IUICommandHandler).UpdateProperty(FClient.CommandId, UI_PKEY_Viewable, @lCurrentValue, lNewValue);
end;

procedure TUICommandActionLink.SetShortCut(Value: System.Classes.TShortCut);
begin
  // If corresponding Action has a shortcut, we append it in text form to the TooltipTitle.
  if Value <> 0 then
  begin
    FClient.ShortCut := Value;
    FClient.TooltipTitle := Format('%s (%s)', [FClient.Caption, ShortCutToText(Value)]);
  end;
end;

function TUICommandActionLink.Update: Boolean;
begin
  if Assigned(Self.Action) then
    Result := inherited Update()
  else
    Result := False;
end;

procedure TUICommandActionLink.SetHint(const Value: String);
begin
  if IsHintLinked and not Value.IsEmpty then
  begin
    // Use the long hint of the action as TooltipDescription. If no separate
    // strings for Long and Short hint are provided, the regular string is used.
    FClient.TooltipDescription := GetLongHint(Value);

    //    I := Pos('|', Value);
//    if (I = 0) then
//      FClient.TooltipTitle := Value
//    else
//    begin
//      FClient.TooltipTitle := Copy(Value, 1, I - 1);
//      FClient.TooltipDescription := Copy(Value, I + 1, MaxInt);
//    end;

    // Some extra handling for the regular ribbon buttons (ctAction).
    if (FClient.CommandType = TUICommandType.ctAction) then
    begin
      // Regular ribbon buttons may also have a "Description" (this is not the
      // tooltip that any ribbon element has), which is displayed right beneath
      // the caption of large buttons in sub menus such as the application menu.
      // Use the short hint of the action as TooltipDescription. If no separate
      // strings for Long and Short hint are provided, the regular string is used.
      (FClient as TUICommandAction).LabelDescription := GetShortHint(Value);
    end;

    if assigned(FClient.OnUpdateHint) then
      FClient.OnUpdateHint(FClient, Value);
  end;
end;

procedure TUICommandActionLink.SetImageIndex(Value: Integer);
var
  lActionManager: TActionManager;
begin
  inherited;
  if (Value >= 0) and IsImageIndexLinked and (TContainedAction(Self.Action).ActionList is TActionManager) then begin
    lActionManager := TActionManager(TContainedAction(Self.Action).ActionList);
    if Assigned(lActionManager.Images) then
      FClient.SmallImage := TUIImage.Create(lActionManager.Images, Value);
    if Assigned(lActionManager.LargeImages) then
      FClient.LargeImage := TUIImage.Create(lActionManager.LargeImages, Value);
  end;
end;

procedure TUICommandActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  { No implementation }
end;

{ TUICommandActionActionLink }

procedure TUICommandActionActionLink.CommandExecute(
  const Args: TUICommandActionEventArgs);
begin
  if Assigned(Action) then
    Action.Execute;
end;

procedure TUICommandActionActionLink.SetAction(Value: TBasicAction);
begin
  inherited;
  if Assigned(Value) then
    (Client as TUICommandAction).OnExecute := CommandExecute;
end;

{ TUICommandCollectionActionLink }

procedure TUICommandCollectionActionLink.CommandSelect(
  const Args: TUICommandCollectionEventArgs);
begin
  if Assigned(Action) then
    Action.Execute;
end;

procedure TUICommandCollectionActionLink.SetAction(Value: TBasicAction);
begin
  inherited;
  (Client as TUICommandCollection).OnSelect := CommandSelect;
  if (Action is TRibbonCollectionAction) then begin
    TRibbonCollectionAction(Action).UICommand := (Client as TUICommandCollection);
    TRibbonCollectionAction(Action).RefreshCommandCollection;
  end;
end;

{ TUICommandDecimalActionLink }

procedure TUICommandDecimalActionLink.CommandChange(
  const Command: TUICommandDecimal; const Verb: TUICommandVerb;
  const Value: Double; const Properties: TUICommandExecutionProperties);
begin
  if Assigned(Action) then
    Action.Execute;
end;

procedure TUICommandDecimalActionLink.SetAction(Value: TBasicAction);
begin
  inherited;
  if Assigned(Value) then
    (Client as TUICommandDecimal).OnChange := CommandChange;
end;

{ TUICommandBooleanActionLink }

procedure TUICommandBooleanActionLink.CommandToggle(const Args: TUICommandBooleanEventArgs);
begin
  if Assigned(Action) then
    Action.Execute;
  // sync the Toogle state of the ribbon buton with the action. This is important as the ToggleButton toggles automatically.
  if IsCheckedLinked and Args.Command.Checked <> TContainedAction(Action).Checked then
  begin
    // TBasicAction does not have a Checked property
    SetChecked(TContainedAction(Action).Checked);
  end;//if
end;

procedure TUICommandBooleanActionLink.SetAction(Value: TBasicAction);
begin
  inherited;
  if Assigned(Value) then
    (Client as TUICommandBoolean).OnToggle := CommandToggle;
end;

procedure TUICommandBooleanActionLink.SetChecked(Value: Boolean);
begin
  inherited;
  // Toggle buttons have a "Checked" property, set it to the same state as
  // its corresponding TAction element has.
  (Client as TUICommandBoolean).Checked := Value;
end;

{ TUICommandFontActionLink }

procedure TUICommandFontActionLink.CommandChanged(
  const Args: TUICommandFontEventArgs);
begin
  if not Assigned(Action) then exit;
  if (Action is TRibbonFontAction) and Assigned(TRibbonFontAction(Action).OnChanged) then
    TRibbonFontAction(Action).OnChanged(Args)
  else
    Action.Execute;
end;

procedure TUICommandFontActionLink.SetAction(Value: TBasicAction);
begin
  inherited;
  if Assigned(Value) then
    (Client as TUICommandFont).OnChanged := CommandChanged;
  if (Action is TRibbonFontAction) then
    TRibbonFontAction(Action).UICommand := (Client as TUICommandFont);
end;

{ TUICommandColorAnchorActionLink }

procedure TUICommandColorAnchorActionLink.CommandExecute(
  const Args: TUICommandColorEventArgs);
begin
  if Assigned(Action) then
    Action.Execute;
end;

procedure TUICommandColorAnchorActionLink.SetAction(Value: TBasicAction);
begin
  inherited;
  if Assigned(Value) then
    (Client as TUICommandColorAnchor).OnExecute := CommandExecute;
  if (Action is TRibbonColorAction) then
    TRibbonColorAction(Action).UICommand := (Client as TUICommandColorAnchor);
end;

{ TUICommandRecentItemsActionLink }

procedure TUICommandRecentItemsActionLink.CommandSelect(
  const Command: TUICommandRecentItems; const Verb: TUICommandVerb;
  const ItemIndex: Integer; const Properties: TUICommandExecutionProperties);
var
  lItem: IUICollectionItem;
begin
  //[JAM:Lemke] Filling property "Selected" with required information

  lItem := Command.Items.Items[ItemIndex];

  Self.Selected := TUIRecentItem.Create;
  try
    Self.Selected.LabelText := (lItem as TUIRecentItem).LabelText;
    Self.Selected.Description := (lItem as TUIRecentItem).Description;
    Self.Selected.Pinned := (lItem as TUIRecentItem).Pinned;

    if Assigned(Action) then
      Action.Execute;
  finally
    FreeAndNil(fSelected);
  end;
end;

procedure TUICommandRecentItemsActionLink.SetAction(Value: TBasicAction);
begin
  inherited;
  if Assigned(Value) then
    (Client as TUICommandRecentItems).OnSelect := CommandSelect;
end;

{ TRibbonCollectionAction }

procedure TRibbonCollectionAction.Add(pAction: TCustomAction);
begin
  fActionList.Add(pAction);
  RefreshCommandCollection;
end;

procedure TRibbonCollectionAction.RefreshCommandCollection();
var
  lAction: TCustomAction;
  lCategory: string;
  lItem: TUIGalleryCollectionItem;
  lCommandAction: TUICommandAction;
  lCommandCollection: TUICommandCollection;
  I, lTargetCategoryId: Integer;

  function FindOrCreateCategory(pCategoryCaption: string): Integer;
  var
    lCurrentCollectionItem: IUICollectionItem;
    lGalleryCollectionItem: TUIGalleryCollectionItem;
  begin
    Result := -1;

    for lCurrentCollectionItem in lCommandCollection.Categories do begin
      lGalleryCollectionItem := lCurrentCollectionItem as TUIGalleryCollectionItem;
      if SameText(lGalleryCollectionItem.LabelText, pCategoryCaption) then
        // Category with given caption found -> return the correct id.
        Exit(lGalleryCollectionItem.CategoryId)
      else
        Result := Max(Result, lGalleryCollectionItem.CategoryId);  // Keep track of the highest used id. We may need it later to create a new category.
    end;
    // No category with given caption found -> create it
    lGalleryCollectionItem := TUIGalleryCollectionItem.Create;
    // Use highest seen category id, increased by one, for this new category
    Inc(Result);
    lGalleryCollectionItem.CategoryId := Result;
    lGalleryCollectionItem.LabelText := pCategoryCaption;
    lCommandCollection.Categories.Add(lGalleryCollectionItem);
  end;

begin
  // Command link is not (yet) created -> exit.
  if not Assigned(UICommand) then exit;

  // Check if a refresh was postponed while the collection was displayed and if we can now perform the refresh.
  if IsCurrentlyDisplayed then
  begin
    fRefreshWhenNotDisplayed := True;
    exit;
  end;

  fRefreshWhenNotDisplayed := False;
  lCommandCollection := UICommand as TUICommandCollection;

  lCommandCollection.Categories.BeginUpdate;
  lCommandCollection.Items.BeginUpdate;
  try
    // Clear the ribbon collection
    lCommandCollection.Items.Clear;
    // Iterate the internal list of actions and fill the ribbon collection
    for I := 0 to fActionList.Count - 1 do begin
      lAction := fActionList[I];
      if not lAction.Visible then
        continue;

      lCategory := fActionList[I].Category;
      if lCategory.IsEmpty then
        lTargetCategoryId := -1
      else
        lTargetCategoryId := FindOrCreateCategory(lCategory);

      // Create a new command item and assign the target action
      lCommandAction := TUICommandAction.Create((lCommandCollection.Owner as TUIRibbon), lAction);
      lCommandAction.OnUpdateProperty := PropertyUpdated;

      // Create a collection item, that holds the action and can be added to the collection.
      lItem := TUIGalleryCollectionItem.Create;
      lItem.Command := lCommandAction;
      lItem.CategoryId := lTargetCategoryId;
      lCommandCollection.Items.Add(lItem);
    end;
  finally
    lCommandCollection.Items.EndUpdate;
    lCommandCollection.Categories.EndUpdate;
  end;//try
end;

procedure TRibbonCollectionAction.Remove(pAction: TCustomAction);
begin
  fActionList.Remove(pAction);
  RefreshCommandCollection;
end;

procedure TRibbonCollectionAction.UICommandItemSelected(const Args: TUICommandCollectionEventArgs);
begin
  // Ignore events where the user only hovers over a selection. We only want to react to actual click events, i.e. when the selection changes.
  if Args.Verb <> TUICommandVerb.cvExecute then
    exit;

  fSelectedItem := UICommand.SelectedItem;
  if Assigned(fOriginalOnSelect) then
    fOriginalOnSelect(Args);
end;

procedure TRibbonCollectionAction.SetSelectedItem(const pValue: Integer);
begin
  fSelectedItem := pValue;
  if Assigned(UICommand) then
    UICommand.SelectedItem := fSelectedItem
end;

function TRibbonCollectionAction.Update(): Boolean;
begin
  Result := inherited;
  // If "SelectedItem" is set during program start, it will not be applied correctly by the framework (value will stay unchanged). Therefore, we store the index in member variable fSelectedItem
  // and apply it as soon as Update() is called for this action. After this point in time, we can assign it normally.
  if Assigned(UICommand) and not (fSelectionInitialized) then
  begin
    UICommand.SelectedItem := fSelectedItem;
    fOriginalOnSelect := UICommand.OnSelect;
    UICommand.OnSelect := UICommandItemSelected;
    fSelectionInitialized := True;
  end;
  if fRefreshWhenNotDisplayed and not IsCurrentlyDisplayed then
    RefreshCommandCollection;
end;

procedure TRibbonCollectionAction.PropertyUpdated(Sender: TObject; const PropKey: TUIPropertyKey; var NewValue: TPropVariant; var Handled: boolean);
var
  lActionVisible: Boolean;
begin
  if PropKey = UI_PKEY_Viewable then begin
    // Update is triggered before visibilty changes -> negate value
    if not Succeeded(UIPropertyToBoolean(PropKey, NewValue, lActionVisible)) then
      Exit;

//    This code is called whenever an action within the gallery becomes invisible.
//    Modifying the gallery while it is open will cause it to be emptied completely and new changes are only visble the next time it is opened.
//    To counter that effect, we check if the gallery is currently displayed and postpone the refresh until the gallery is not visibile anymore.
//    PLEASE NOTE: The default implementation of IsCurrentlyDisplayed does not check this. You have to override this and perform a more sophisticated check
//    if you want to prevent this effect.
//    See issue #60 (https://github.com/TurboPack/RibbonFramework/issues/60)

    if not lActionVisible then begin
      fActionList.Remove((Sender as TUICommand).ActionLink.Action as TCustomAction);
      fRefreshWhenNotDisplayed := True; // "Schedule" a refresh, during the next update.
    end
  end;
end;

procedure TRibbonCollectionAction.Add(const pLabel: string);
var
  lItem: TUIGalleryCollectionItem;
begin
  lItem := TUIGalleryCollectionItem.Create;
  lItem.LabelText := pLabel;
  UICommand.Items.Add(lItem);
  // Setting the RepresentativeString property will reserve enough space for the given text.
  // Though it's not perfectly accurate to just compare the string length, it should suffice in most cases.
  if Length(pLabel) > Length(UICommand.RepresentativeString) then
    UICommand.RepresentativeString := pLabel;
  if (UICommand.Items.Count = 1) and (fSelectedItem = -1) then  // Only one item exists and nothing has been selected yet -> Select it, so that we have a default selection.
    SelectedItem := 0;
end;

procedure TRibbonCollectionAction.AddRange(pSource: TList<TCustomAction>);
begin
  fActionList.AddRange(pSource);
  RefreshCommandCollection;
end;

procedure TRibbonCollectionAction.Clear;
begin
  fActionList.Clear;
  RefreshCommandCollection;
end;

constructor TRibbonCollectionAction.Create(AOwner: TComponent);
begin
  inherited;
  fActionList := TList<TCustomAction>.Create;
  fSelectionInitialized := False;
  fRefreshWhenNotDisplayed := False;
  fSelectedItem := cNoSelection;
end;

destructor TRibbonCollectionAction.Destroy;
begin
  FreeAndNil(fActionList);
  inherited;
end;

function TRibbonCollectionAction.GetEnumerator: TEnumerator<TCustomAction>;
begin
  Result := fActionList.GetEnumerator;
end;

function TRibbonCollectionAction.GetItem(pIndex: Integer): TCustomAction;
begin
  Exit(fActionList[pIndex]);
end;

function TRibbonCollectionAction.IsCurrentlyDisplayed: Boolean;
begin
  Result := Self.Visible;
end;

function TRibbonCollectionAction.ItemCount: Integer;
begin
  Result := fActionList.Count;
end;

{ TRibbonPopupMenuAction }

procedure TRibbonPopupMenuAction.MenuChange(Sender: TObject; Source: TMenuItem; Rebuild: Boolean);
var
  I, J: Integer;
  lCategory: string;
  lActionList: TList<TCustomAction>;
begin
  if Assigned(fOriginalOnMenuChange) then
    fOriginalOnMenuChange(Sender, Source, Rebuild);

  Clear; // Clear the collection and refill it.
  lCategory := ' ';
  // We use this list to collect actions category wise. We map Menu separators to ribbon categories.
  lActionList := TList<TCustomAction>.Create;
  try
    for I := 0 to Menu.Items.Count - 1 do begin
      if not Assigned(Menu.Items[I].Action) and SameText(Menu.Items[I].Caption, cLineCaption) then begin
        // This menu item is a separator -> use a category with empty caption
        lCategory := lCategory + ' ';
        // The first separator was found -> update the existing items
        if lCategory = ' ' then begin
          for J := 0 to lActionList.Count - 1 do
            lActionList[J].Category := lCategory;
        end;
        // Submit this list and clear it for the next category
        AddRange(lActionList);
        lActionList.Clear;
        // Change the category key, so that following items will use a different category.
        lCategory := lCategory + ' ';
      end
      else begin
        (Menu.Items[I].Action as TCustomAction).Category := lCategory;
        lActionList.Add(Menu.Items[I].Action as TCustomAction);
      end
    end;
    // Submit the last category
    AddRange(lActionList);
  finally
    FreeAndNil(lActionList);
  end;
end;

procedure TRibbonPopupMenuAction.SetPopupMenu(pValue: TPopupMenu);
begin
  fPopupMenu := pValue;
  // Keep track of pre-existing event handlers. We call them together with our custom event handler.
  fOriginalOnMenuChange := fPopupMenu.OnChange;
  fPopupMenu.OnChange := MenuChange;
end;

end.
