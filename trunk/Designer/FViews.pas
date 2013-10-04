unit FViews;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  ComCtrls,
  ToolWin,
  ImgList,
  RibbonMarkup,
  FBaseFrame,
  FViewRibbon,
  FApplicationMenu,
  FMenuGroup,
  FAppMenuGroup,
  FButton,
  FToggleButton,
  FQuickAccessToolbar,
  FQatControl,
  FHelpButton,
  FSplitButton,
  FDropDownButton,
  FTab,
  FTabGroup,
  FScale,
  FDropDownGallery,
  FSplitButtonGallery,
  FInRibbonGallery,
  FGroup,
  FSizeDefinition,
  FGroupSizeDefinition,
  FControlSizeDefinition,
  FColumnBreak,
  FFloatieFontControl,
  FFontControl,
  FControlGroup,
  FComboBox,
  FCheckBox,
  FDropDownColorPicker,
  FSpinner,
  FMiniToolbar,
  FContextMenu,
  FContextMap, ActnList, Menus, PlatformDefaultStyleActnCtrls, ActnPopup,
  XPStyleActnCtrls;

type
  TFrameViews = class(TFrame)
    PanelRibbon: TPanel;
    ToolBarViews: TToolBar;
    TreeViewRibbon: TTreeView;
    SplitterRibbon: TSplitter;
    ImageListTreeView: TImageList;
    PanelProps: TPanel;
    ActionList: TActionList;
    ActionAdd: TAction;
    ActionDelete: TAction;
    ButtonAdd: TToolButton;
    ButtonDelete: TToolButton;
    ActionAddMenuGroup: TAction;
    DropDownMenuAdd: TPopupMenu;
    MenuAddMenuGroup: TMenuItem;
    PopupMenuTree: TPopupMenu;
    PopupDelete: TMenuItem;
    N1: TMenuItem;
    PopupAddMenuGroup: TMenuItem;
    ActionAddButton: TAction;
    MenuAddButton: TMenuItem;
    PopupAddButton: TMenuItem;
    ActionAddSplitButton: TAction;
    MenuAddSplitButton: TMenuItem;
    PopupAddSplitButton: TMenuItem;
    ActionAddDropDownButton: TAction;
    MenuAddDropDownButton: TMenuItem;
    PopupAddDropDownButton: TMenuItem;
    ActionAddDropDownGallery: TAction;
    MenuAddDropDownGallery: TMenuItem;
    PopupAddDropDownGallery: TMenuItem;
    ActionAddSplitButtonGallery: TAction;
    AddSplitButtonGalleryMenu: TMenuItem;
    PopupAddSplitButtonGallery: TMenuItem;
    ActionAddCheckBox: TAction;
    MenuAddCheckBox: TMenuItem;
    PopupAddCheckBox: TMenuItem;
    ActionAddToggleButton: TAction;
    MenuAddToggleButton: TMenuItem;
    PopupAddToggleButton: TMenuItem;
    ActionAddDropDownColorPicker: TAction;
    MenuAddDropDownColorPicker: TMenuItem;
    PopupAddDropDownColorPicker: TMenuItem;
    ActionAddQatButton: TAction;
    ActionAddQatToggleButton: TAction;
    ActionAddQatCheckBox: TAction;
    MenuAddQatButton: TMenuItem;
    MenuAddQatToggleButton: TMenuItem;
    MenuAddQatCheckBox: TMenuItem;
    PopupAddQatButton: TMenuItem;
    PopupAddQatToggleButton: TMenuItem;
    PopupAddQatCheckBox: TMenuItem;
    ActionAddRibbonSizeDefinition: TAction;
    MenuAddRibbonSizeDefinition: TMenuItem;
    PopupAddRibbonSizeDefinition: TMenuItem;
    ActionAddGroupSizeDefinition: TAction;
    MenuAddGroupSizeDefinition: TMenuItem;
    PopupAddGroupSizeDefinition: TMenuItem;
    ActionAddControlSizeDefinition: TAction;
    ActionAddControlSizeGroup: TAction;
    ActionAddColumnBreak: TAction;
    ActionAddRow: TAction;
    MenuAddControlSizeDefinition: TMenuItem;
    MenuAddControlSizeGroup: TMenuItem;
    MenuAddColumnBreak: TMenuItem;
    AddRow1: TMenuItem;
    PopupAddControlSizeDefinition: TMenuItem;
    PopupAddControlSizeGroup: TMenuItem;
    PopupAddColumnBreak: TMenuItem;
    PopupAddRow: TMenuItem;
    ActionAddGroup: TAction;
    MenuAddGroup: TMenuItem;
    PopupAddGroup: TMenuItem;
    ActionAddScale: TAction;
    MenuAddScale: TMenuItem;
    PopupAddScale: TMenuItem;
    ActionAddControlGroup: TAction;
    MenuAddControlGroup: TMenuItem;
    PopupAddControlGroup: TMenuItem;
    ActionAddComboBox: TAction;
    MenuAddComboBox: TMenuItem;
    PopupAddComboBox: TMenuItem;
    ActionAddSpinner: TAction;
    MenuAddSpinner: TMenuItem;
    PopupAddSpinner: TMenuItem;
    ActionAddInRibbonGallery: TAction;
    MenuAddInRibbonGallery: TMenuItem;
    PopupAddInRibbonGallery: TMenuItem;
    ActionAddFontControl: TAction;
    MenuAddFontControl: TMenuItem;
    PopupAddFontControl: TMenuItem;
    ActionAddTab: TAction;
    MenuAddTab: TMenuItem;
    AddTabPopup: TMenuItem;
    ActionAddTabGroup: TAction;
    MenuAddTabGroup: TMenuItem;
    PopupAddTabGroup: TMenuItem;
    ActionAddContextPopup: TAction;
    MenuAddContextPopup: TMenuItem;
    PopupAddContextPopup: TMenuItem;
    ActionAddMiniToolbar: TAction;
    MenuAddMiniToolbar: TMenuItem;
    PopupAddMiniToolbar: TMenuItem;
    ActionAddContextMenu: TAction;
    MenuAddContextMenu: TMenuItem;
    PopupAddContextMenu: TMenuItem;
    ActionAddContextMap: TAction;
    MenuAddContextMap: TMenuItem;
    PopupAddContextMap: TMenuItem;
    ActionAddMiniToolbarMenuGroup: TAction;
    MenuAddMiniToolbarMenuGroup: TMenuItem;
    PopupAddMiniToolbarMenuGroup: TMenuItem;
    ActionAddFloatieFontControl: TAction;
    MenuAddFloatieFontControl: TMenuItem;
    PopupAddFloatieFontControl: TMenuItem;
    ActionMoveUp: TAction;
    ActionMoveDown: TAction;
    ButtonMoveUp: TToolButton;
    ButtonMoveDown: TToolButton;
    PopupMoveUp: TMenuItem;
    PopupMoveDown: TMenuItem;
    procedure TreeViewRibbonChange(Sender: TObject; Node: TTreeNode);
    procedure ActionAddMenuGroupExecute(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionAddButtonExecute(Sender: TObject);
    procedure ActionAddSplitButtonExecute(Sender: TObject);
    procedure ActionAddDropDownButtonExecute(Sender: TObject);
    procedure ActionAddDropDownGalleryExecute(Sender: TObject);
    procedure ActionAddSplitButtonGalleryExecute(Sender: TObject);
    procedure ActionAddCheckBoxExecute(Sender: TObject);
    procedure ActionAddToggleButtonExecute(Sender: TObject);
    procedure ActionAddDropDownColorPickerExecute(Sender: TObject);
    procedure ActionAddQatButtonExecute(Sender: TObject);
    procedure ActionAddQatToggleButtonExecute(Sender: TObject);
    procedure ActionAddQatCheckBoxExecute(Sender: TObject);
    procedure ActionAddRibbonSizeDefinitionExecute(Sender: TObject);
    procedure ActionAddGroupSizeDefinitionExecute(Sender: TObject);
    procedure ActionAddControlSizeDefinitionExecute(Sender: TObject);
    procedure ActionAddControlSizeGroupExecute(Sender: TObject);
    procedure ActionAddColumnBreakExecute(Sender: TObject);
    procedure ActionAddRowExecute(Sender: TObject);
    procedure ActionAddGroupExecute(Sender: TObject);
    procedure ActionAddScaleExecute(Sender: TObject);
    procedure ActionAddControlGroupExecute(Sender: TObject);
    procedure ActionAddComboBoxExecute(Sender: TObject);
    procedure ActionAddSpinnerExecute(Sender: TObject);
    procedure ActionAddInRibbonGalleryExecute(Sender: TObject);
    procedure ActionAddFontControlExecute(Sender: TObject);
    procedure ActionAddTabExecute(Sender: TObject);
    procedure ActionAddTabGroupExecute(Sender: TObject);
    procedure ActionAddContextPopupExecute(Sender: TObject);
    procedure ActionAddMiniToolbarExecute(Sender: TObject);
    procedure ActionAddContextMenuExecute(Sender: TObject);
    procedure ActionAddContextMapExecute(Sender: TObject);
    procedure ActionAddMiniToolbarMenuGroupExecute(Sender: TObject);
    procedure ActionAddFloatieFontControlExecute(Sender: TObject);
    procedure ActionMoveUpExecute(Sender: TObject);
    procedure ActionMoveDownExecute(Sender: TObject);
  private
    { Private declarations }
    FDocument: TRibbonDocument;
    FCurrentNode: TTreeNode;
    FCurrentFrame: TBaseFrame;
    FCurrentObject: TRibbonObject;
    FCommands: TStringList;
    FFrameViewRibbon: TFrameViewRibbon;
    FFrameApplicationMenu: TFrameApplicationMenu;
    FFrameMenuGroup: TFrameMenuGroup;
    FFrameAppMenuGroup: TFrameAppMenuGroup;
    FFrameButton: TFrameButton;
    FFrameToggleButton: TFrameToggleButton;
    FFrameSplitButton: TFrameSplitButton;
    FFrameDropDownButton: TFrameDropDownButton;
    FFrameQuickAccessToolbar: TFrameQuickAccessToolbar;
    FFrameQatControl: TFrameQatControl;
    FFrameHelpButton: TFrameHelpButton;
    FFrameTab: TFrameTab;
    FFrameTabGroup: TFrameTabGroup;
    FFrameGroup: TFrameGroup;
    FFrameScale: TFrameScale;
    FFrameDropDownGallery: TFrameDropDownGallery;
    FFrameSplitButtonGallery: TFrameSplitButtonGallery;
    FFrameInRibbonGallery: TFrameInRibbonGallery;
    FFrameSizeDefinition: TFrameSizeDefinition;
    FFrameGroupSizeDefinition: TFrameGroupSizeDefinition;
    FFrameControlSizeDefinition: TFrameControlSizeDefintion;
    FFrameColumnBreak: TFrameColumnBreak;
    FFrameFloatieFontControl: TFrameFloatieFontControl;
    FFrameFontControl: TFrameFontControl;
    FFrameControlGroup: TFrameControlGroup;
    FFrameComboBox: TFrameComboBox;
    FFrameCheckBox: TFrameCheckBox;
    FFrameDropDownColorPicker: TFrameDropDownColorPicker;
    FFrameSpinner: TFrameSpinner;
    FFrameMiniToolbar: TFrameMiniToolbar;
    FFrameContextMenu: TFrameContextMenu;
    FFrameContextMap: TFrameContextMap;
    FSelectAddedNode: Boolean;
    function AddNode(const Parent: TTreeNode; const Caption: String;
      const ImageIndex: TImageIndex; const Data: Pointer;
      const First: Boolean = False): TTreeNode;
    procedure AddApplicationMenu(const Parent: TTreeNode; const AppMenu: TRibbonApplicationMenu);
    procedure AddQuickAccessToolbar(const Parent: TTreeNode;
      const Qat: TRibbonQuickAccessToolbar);
    procedure AddAppMenuGroup(const Parent: TTreeNode; const Group: TRibbonAppMenuGroup);
    procedure AddMenuGroup(const Parent: TTreeNode; const Group: TRibbonMenuGroup);
    procedure AddTabs(const Parent: TTreeNode; const Tabs: TRibbonList<TRibbonTab>);
    procedure AddTab(const Parent: TTreeNode; const Tab: TRibbonTab);
    procedure AddScalingPolicy(const Parent: TTreeNode; const Scaling: TRibbonScalingPolicy);
    procedure AddScale(const Parent: TTreeNode; const Scale: TRibbonScale);
    procedure AddGroup(const Parent: TTreeNode; const Group: TRibbonGroup);
    procedure AddContextualTabs(const Parent: TTreeNode;
      const Tabs: TRibbonList<TRibbonTabGroup>);
    procedure AddTabGroup(const Parent: TTreeNode; const TabGroup: TRibbonTabGroup);
    procedure AddSizeDefinitions(const Parent: TTreeNode;
      const SizeDefs: TRibbonList<TRibbonRibbonSizeDefinition>);
    procedure AddRibbonSizeDefinition(const Parent: TTreeNode; const SizeDef: TRibbonRibbonSizeDefinition);
    procedure AddGroupSizeDefinition(const Parent: TTreeNode; const SizeDef: TRibbonGroupSizeDefinition);
    procedure AddSizeDefElement(const Parent: TTreeNode; const Element: TRibbonGroupSizeDefinitionElement);
    procedure AddMiniToolbars(const Parent: TTreeNode; const Toolbars: TRibbonList<TRibbonMiniToolbar>);
    procedure AddMiniToolbar(const Parent: TTreeNode; const Toolbar: TRibbonMiniToolbar);
    procedure AddContextMenus(const Parent: TTreeNode; const Menus: TRibbonList<TRibbonContextMenu>);
    procedure AddContextMenu(const Parent: TTreeNode; const Menu: TRibbonContextMenu);
    procedure AddContextMaps(const Parent: TTreeNode; const Maps: TRibbonList<TRibbonContextMap>);
    procedure AddContextMap(const Parent: TTreeNode; const Map: TRibbonContextMap);
    procedure AddContextPopup(const Popup: TRibbonViewContextPopup);
    procedure UpdateTreeNodeCaption(const Node: TTreeNode; const Recursive: Boolean);
    function GetObject(const Node: TTreeNode; out Obj: TRibbonObject): Boolean;
    procedure Modified;
    function AddNewObject(const ObjType: TRibbonObjectType): TRibbonObject;
    procedure MoveNode(const Direction: Integer);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ClearDocument;
    procedure ShowDocument(const Document: TRibbonDocument);
    procedure Activate;
    procedure Deactivate;
    procedure UpdateCurrentNode;

    procedure AddSizeDefinition(const Parent: TTreeNode; const SizeDef: TRibbonSizeDefinition);
    procedure AddControl(const Parent: TTreeNode; const Control: TRibbonControl);

    property Commands: TStringList read FCommands;
  end;

resourcestring
  RS_RIBBON = 'Ribbon';
  RS_APPLICATION_MENU = 'Application Menu';
  RS_MENU_GROUP = 'Menu Group';
  RS_HELP_BUTTON = 'Help Button';
  RS_QAT = 'Quick Access Toolbar';
  RS_TABS = 'Tabs';
  RS_CONTEXTUAL_TABS = 'Contextual Tabs';
  RS_SIZE_DEFS = 'Size Definitions';
  RS_NONE = '(none)';
  RS_BUTTON_ITEM = 'Button (Top) Item';
  RS_ITEMS = 'Items';
  RS_SCALING_POLICY = 'Scaling Policy';
  RS_IDEAL_SIZES = 'Ideal Sizes';
  RS_CONTROL_NAME_MAP = 'Control Name Map';
  RS_CONTEXT_POPUP = 'Context Popup';
  RS_MINI_TOOLBARS = 'Mini Toolbars';
  RS_CONTEXT_MENUS = 'Context Menus';
  RS_CONTEXT_MAPS = 'Context Maps';
  RS_DELETE_ITEM_HEADER = 'Delete Item?';
  RS_DELETE_ITEM_MESSAGE = 'Do you want to delete the selected item AND all subitems (this cannot be undone)?';

const // Image Index
  II_HELP_BUTTON      = 11;
  II_APPLICATION_MENU = 18;
  II_QAT              = 19;
  II_GROUP            = 20;
  II_TAB              = 21;
  II_CONTEXTUAL_TAB   = 22;
  II_RIBBON           = 23;
  II_MENU_GROUP       = 24;
  II_SIZE_DEF         = 25;
  II_BUTTON_ITEM      = 26;
  II_LIST             = 27;
  II_SCALING          = 28;
  II_SCALE            = 29;
  II_GROUP_SIZE_DEF   = 30;
  II_CONTROL_SIZE_DEF = 31;
  II_CONTEXT_POPUP    = 32;
  II_MINI_TOOLBAR     = 33;
  II_CONTEXT_MENU     = 34;
  II_CONTEXT_MAP      = 35;

implementation

{$R *.dfm}

uses
  FMain;

{ TFrameRibbon }

procedure TFrameViews.AddApplicationMenu(const Parent: TTreeNode;
  const AppMenu: TRibbonApplicationMenu);
var
  Node: TTreeNode;
  Group: TRibbonAppMenuGroup;
begin
  Node := AddNode(Parent, RS_APPLICATION_MENU, II_APPLICATION_MENU, AppMenu);
  for Group in AppMenu.MenuGroups do
    AddAppMenuGroup(Node, Group);
end;

procedure TFrameViews.AddAppMenuGroup(const Parent: TTreeNode;
  const Group: TRibbonAppMenuGroup);
var
  Node: TTreeNode;
  Control: TRibbonControl;
begin
  Node := AddNode(Parent, Group.DisplayName, II_MENU_GROUP, Group);
  for Control in Group.Controls do
    AddControl(Node, Control);
end;

procedure TFrameViews.AddContextMap(const Parent: TTreeNode;
  const Map: TRibbonContextMap);
begin
  AddNode(Parent, Map.DisplayName, II_CONTEXT_MAP, Map);
end;

procedure TFrameViews.AddContextMaps(const Parent: TTreeNode;
  const Maps: TRibbonList<TRibbonContextMap>);
var
  Node: TTreeNode;
  Map: TRibbonContextMap;
begin
  Node := AddNode(Parent, RS_CONTEXT_MAPS, II_CONTEXT_MAP, Maps);
  for Map in Maps do
    AddContextMap(Node, Map);
end;

procedure TFrameViews.AddContextMenu(const Parent: TTreeNode;
  const Menu: TRibbonContextMenu);
var
  Node: TTreeNode;
  Group: TRibbonMenuGroup;
  OrigSelectAddedNode: Boolean;
begin
  Node := AddNode(Parent, Menu.DisplayName, II_CONTEXT_MENU, Menu);
  OrigSelectAddedNode := FSelectAddedNode;
  FSelectAddedNode := False;
  try
    for Group in Menu.MenuGroups do
      AddMenuGroup(Node, Group);
  finally
    FSelectAddedNode := OrigSelectAddedNode;
  end;
end;

procedure TFrameViews.AddContextMenus(const Parent: TTreeNode;
  const Menus: TRibbonList<TRibbonContextMenu>);
var
  Node: TTreeNode;
  Menu: TRibbonContextMenu;
begin
  Node := AddNode(Parent, RS_CONTEXT_MENUS, II_CONTEXT_MENU, Menus);
  for Menu in Menus do
    AddContextMenu(Node, Menu);
end;

procedure TFrameViews.AddContextPopup(const Popup: TRibbonViewContextPopup);
var
  Root: TTreeNode;
  OrigSelectAddedNode: Boolean;
begin
  Root := AddNode(nil, RS_CONTEXT_POPUP, II_CONTEXT_POPUP, Popup);
  OrigSelectAddedNode := FSelectAddedNode;
  FSelectAddedNode := False;
  try
    AddMiniToolbars(Root, Popup.MiniToolbars);
    AddContextMenus(Root, Popup.ContextMenus);
    AddContextMaps(Root, Popup.ContextMaps);
    Root.Expand(False);
  finally
    FSelectAddedNode := OrigSelectAddedNode;
  end;
end;

procedure TFrameViews.AddContextualTabs(const Parent: TTreeNode;
  const Tabs: TRibbonList<TRibbonTabGroup>);
var
  Node: TTreeNode;
  TabGroup: TRibbonTabGroup;
begin
  Node := AddNode(Parent, RS_CONTEXTUAL_TABS, II_CONTEXTUAL_TAB, Tabs);
  for TabGroup in Tabs do
    AddTabGroup(Node, TabGroup);
end;

procedure TFrameViews.AddControl(const Parent: TTreeNode;
  const Control: TRibbonControl);
var
  Node, Child: TTreeNode;
  SplitButton: TRibbonSplitButton absolute Control;
  DropDownButton: TRibbonDropDownButton absolute Control;
  Gallery: TRibbonGallery absolute Control;
  ControlGroup: TRibbonControlGroup absolute Control;
  SubControl: TRibbonControl;
  MenuGroup: TRibbonMenuGroup;
  OrigSelectAddedNode: Boolean;
begin
  Node := AddNode(Parent, Control.DisplayName, Ord(Control.ObjectType), Control);
  OrigSelectAddedNode := FSelectAddedNode;
  FSelectAddedNode := False;
  try
    case Control.ObjectType of
      otSplitButton:
        begin
          Child := AddNode(Node, RS_BUTTON_ITEM, II_BUTTON_ITEM, nil);
          if Assigned(SplitButton.ButtonItem) then
            AddControl(Child, SplitButton.ButtonItem);

          Child := AddNode(Node, RS_ITEMS, II_LIST, SplitButton.Controls);
          for SubControl in SplitButton.Controls do
            AddControl(Child, SubControl);
          for MenuGroup in SplitButton.MenuGroups do
            AddMenuGroup(Child, MenuGroup);
        end;

      otDropDownButton:
        begin
          for SubControl in DropDownButton.Controls do
            AddControl(Node, SubControl);
          for MenuGroup in DropDownButton.MenuGroups do
            AddMenuGroup(Node, MenuGroup);
        end;

      otDropDownGallery,
      otSplitButtonGallery,
      otInRibbonGallery:
        begin
          for SubControl in Gallery.Controls do
            AddControl(Node, SubControl);
          for MenuGroup in Gallery.MenuGroups do
            AddMenuGroup(Node, MenuGroup);
        end;

      otControlGroup:
        for SubControl in ControlGroup.Controls do
          AddControl(Node, SubControl);
    end;
  finally
    FSelectAddedNode := OrigSelectAddedNode;
  end;
end;

procedure TFrameViews.AddGroup(const Parent: TTreeNode;
  const Group: TRibbonGroup);
var
  Node: TTreeNode;
  Control: TRibbonControl;
begin
  Node := AddNode(Parent, Group.DisplayName, II_GROUP, Group);
  if (Group.BasicSizeDefinition = sdAdvanced) and Assigned(Group.SizeDefinition) then
    AddSizeDefinition(Node, Group.SizeDefinition);
  for Control in Group.Controls do
    AddControl(Node, Control);
end;

procedure TFrameViews.AddGroupSizeDefinition(const Parent: TTreeNode;
  const SizeDef: TRibbonGroupSizeDefinition);
var
  Node: TTreeNode;
  Element: TRibbonGroupSizeDefinitionElement;
begin
  Node := AddNode(Parent, SizeDef.DisplayName, II_GROUP_SIZE_DEF, SizeDef);
  for Element in SizeDef.Elements do
    AddSizeDefElement(Node, Element);
end;

procedure TFrameViews.AddMenuGroup(const Parent: TTreeNode;
  const Group: TRibbonMenuGroup);
var
  Node: TTreeNode;
  Control: TRibbonControl;
begin
  Node := AddNode(Parent, Group.DisplayName, II_MENU_GROUP, Group);
  for Control in Group.Controls do
    AddControl(Node, Control);
end;

procedure TFrameViews.AddMiniToolbar(const Parent: TTreeNode;
  const Toolbar: TRibbonMiniToolbar);
var
  Node: TTreeNode;
  Group: TRibbonMiniToolbarMenuGroup;
  OrigSelectAddedNode: Boolean;
begin
  Node := AddNode(Parent, Toolbar.DisplayName, II_MINI_TOOLBAR, Toolbar);
  OrigSelectAddedNode := FSelectAddedNode;
  FSelectAddedNode := False;
  try
    for Group in Toolbar.MenuGroups do
      AddMenuGroup(Node, Group);
  finally
    FSelectAddedNode := OrigSelectAddedNode;
  end;
end;

procedure TFrameViews.AddMiniToolbars(const Parent: TTreeNode;
  const Toolbars: TRibbonList<TRibbonMiniToolbar>);
var
  Node: TTreeNode;
  Toolbar: TRibbonMiniToolbar;
begin
  Node := AddNode(Parent, RS_MINI_TOOLBARS, II_MINI_TOOLBAR, Toolbars);
  for Toolbar in Toolbars do
    AddMiniToolbar(Node, Toolbar);
end;

function TFrameViews.AddNewObject(const ObjType: TRibbonObjectType): TRibbonObject;
var
  Node: TTreeNode;
  Obj: TRibbonObject;
begin
  Result := nil;
  Node := TreeViewRibbon.Selected;
  if (Node = nil) or (Node.Data = nil) then
    Exit;

  Obj := Node.Data;
  if (Obj.ObjectType = otList) then
  begin
    if (Node.Parent = nil) or (Node.Parent.Data = nil) then
      Exit;
    Obj := Node.Parent.Data;
  end;

  Result := Obj.AddNew(ObjType);
  if (Result = nil) then
    Exit;

  if (Result.ObjectType = otTab) then
    AddTab(Node, TRibbonTab(Result))
  else if (Result is TRibbonControl) then
    AddControl(Node, TRibbonControl(Result))
  else
  begin
    case Result.ObjectType of
      otAppMenuGroup:
        AddAppMenuGroup(Node, Result as TRibbonAppMenuGroup);

      otMenuGroup:
        AddMenuGroup(Node, Result as TRibbonMenuGroup);

      otMiniToolbarMenuGroup:
        AddMenuGroup(Node, Result as TRibbonMiniToolbarMenuGroup);

      otRibbonSizeDefinition:
        AddRibbonSizeDefinition(Node, Result as TRibbonRibbonSizeDefinition);

      otGroupSizeDefinition:
        AddGroupSizeDefinition(Node, Result as TRibbonGroupSizeDefinition);

      otControlSizeDefinition,
      otControlSizeGroup,
      otColumnBreak,
      otRow:
        AddSizeDefElement(Node, Result as TRibbonGroupSizeDefinitionElement);

      otScale:
        AddScale(Node, Result as TRibbonScale);

      otMiniToolbar:
        AddMiniToolbar(Node, Result as TRibbonMiniToolbar);

      otContextMenu:
        AddContextMenu(Node, Result as TRibbonContextMenu);

      otContextMap:
        AddContextMap(Node, Result as TRibbonContextMap);
    else
      Assert(False);
    end;
  end;

  Modified;
end;

function TFrameViews.AddNode(const Parent: TTreeNode; const Caption: String;
  const ImageIndex: TImageIndex; const Data: Pointer; const First: Boolean): TTreeNode;
begin
  if (First) then
    Result := TreeViewRibbon.Items.AddChildFirst(Parent, Caption)
  else
    Result := TreeViewRibbon.Items.AddChild(Parent, Caption);
  Result.ImageIndex := ImageIndex;
  Result.SelectedIndex := ImageIndex;
  Result.Data := Data;

  if (FSelectAddedNode) then
  begin
    TreeViewRibbon.Selected := Result;
    Result.MakeVisible;
  end;
end;

procedure TFrameViews.AddQuickAccessToolbar(const Parent: TTreeNode;
  const Qat: TRibbonQuickAccessToolbar);
var
  Node: TTreeNode;
  Control: TRibbonControl;
begin
  Node := AddNode(Parent, RS_QAT, II_QAT, Qat);
  for Control in Qat.Controls do
    AddControl(Node, Control);
end;

procedure TFrameViews.AddRibbonSizeDefinition(const Parent: TTreeNode;
  const SizeDef: TRibbonRibbonSizeDefinition);
var
  Node: TTreeNode;
  Def: TRibbonGroupSizeDefinition;
  OrigSelectAddedNode: Boolean;
begin
  Node := AddNode(Parent, SizeDef.DisplayName, II_SIZE_DEF, SizeDef);
  OrigSelectAddedNode := FSelectAddedNode;
  FSelectAddedNode := False;
  try
    for Def in SizeDef.GroupSizeDefinitions do
      AddGroupSizeDefinition(Node, Def);
  finally
    FSelectAddedNode := OrigSelectAddedNode;
  end;
end;

procedure TFrameViews.AddScale(const Parent: TTreeNode;
  const Scale: TRibbonScale);
begin
  AddNode(Parent, Scale.DisplayName, II_SCALE, Scale);
end;

procedure TFrameViews.AddScalingPolicy(const Parent: TTreeNode;
  const Scaling: TRibbonScalingPolicy);
var
  Node, Child: TTreeNode;
  Scale: TRibbonScale;
begin
  Node := AddNode(Parent, RS_SCALING_POLICY, II_SCALING, Scaling);

  Child := AddNode(Node, RS_IDEAL_SIZES, II_SCALING, Scaling.IdealSizes);
  for Scale in Scaling.IdealSizes do
    AddNode(Child, Scale.DisplayName, II_SCALE, Scale);

  for Scale in Scaling.Scales do
    AddScale(Node, Scale);
end;

procedure TFrameViews.AddSizeDefElement(const Parent: TTreeNode;
  const Element: TRibbonGroupSizeDefinitionElement);
var
  Node: TTreeNode;
  Row: TRibbonRow absolute Element;
  Group: TRibbonControlSizeGroup absolute Element;
  SubElement: TRibbonGroupSizeDefinitionElement;
  Def: TRibbonControlSizeDefinition;
begin
  Node := AddNode(Parent, Element.DisplayName, II_CONTROL_SIZE_DEF, Element);

  if (Element is TRibbonRow) then
    for SubElement in Row.Elements do
      AddSizeDefElement(Node, SubElement);

  if (Element is TRibbonControlSizeGroup) then
    for Def in Group.ControlSizeDefinitions do
      AddSizeDefElement(Node, Def);
end;

procedure TFrameViews.AddSizeDefinition(const Parent: TTreeNode;
  const SizeDef: TRibbonSizeDefinition);
var
  Node: TTreeNode;
  Def: TRibbonGroupSizeDefinition;
begin
  Node := AddNode(Parent, RS_SIZE_DEF, II_SIZE_DEF, SizeDef, True);
  for Def in SizeDef.GroupSizeDefinitions do
   AddGroupSizeDefinition(Node, Def);
end;

procedure TFrameViews.AddSizeDefinitions(const Parent: TTreeNode;
  const SizeDefs: TRibbonList<TRibbonRibbonSizeDefinition>);
var
  Node: TTreeNode;
  SizeDef: TRibbonRibbonSizeDefinition;
begin
  Node := AddNode(Parent, RS_SIZE_DEFS, II_SIZE_DEF, SizeDefs);
  for SizeDef in SizeDefs do
    AddRibbonSizeDefinition(Node, SizeDef);
end;

procedure TFrameViews.AddTab(const Parent: TTreeNode; const Tab: TRibbonTab);
var
  Node: TTreeNode;
  Group: TRibbonGroup;
  OrigSelectAddedNode: Boolean;
begin
  Node := AddNode(Parent, Tab.DisplayName, II_TAB, Tab);
  OrigSelectAddedNode := FSelectAddedNode;
  FSelectAddedNode := False;
  try
    AddScalingPolicy(Node, Tab.ScalingPolicy);
    for Group in Tab.Groups do
      AddGroup(Node, Group);
  finally
    FSelectAddedNode := OrigSelectAddedNode;
  end;
end;

procedure TFrameViews.AddTabGroup(const Parent: TTreeNode;
  const TabGroup: TRibbonTabGroup);
var
  Node: TTreeNode;
  Tab: TRibbonTab;
begin
  Node := AddNode(Parent, TabGroup.DisplayName, II_CONTEXTUAL_TAB, TabGroup);
  for Tab in TabGroup.Tabs do
    AddTab(Node, Tab);
end;

procedure TFrameViews.AddTabs(const Parent: TTreeNode;
  const Tabs: TRibbonList<TRibbonTab>);
var
//  Node: TTreeNode;
  Tab: TRibbonTab;
begin
//  Node := AddNode(Parent, RS_TABS, II_TAB, FRibbon.Tabs);
  for Tab in Tabs do
    AddTab(Parent, Tab);
end;

procedure TFrameViews.ClearDocument;
begin
  TreeViewRibbon.Items.Clear;
end;

constructor TFrameViews.Create(AOwner: TComponent);
begin
  inherited;
  FCommands := TStringList.Create;

  FFrameViewRibbon := TFrameViewRibbon.Create(Self);
  FFrameViewRibbon.Parent := PanelProps;

  FFrameApplicationMenu := TFrameApplicationMenu.Create(Self);
  FFrameApplicationMenu.Parent := PanelProps;

  FFrameMenuGroup := TFrameMenuGroup.Create(Self);
  FFrameMenuGroup.Parent := PanelProps;

  FFrameAppMenuGroup := TFrameAppMenuGroup.Create(Self);
  FFrameAppMenuGroup.Parent := PanelProps;

  FFrameButton := TFrameButton.Create(Self);
  FFrameButton.Parent := PanelProps;

  FFrameToggleButton := TFrameToggleButton.Create(Self);
  FFrameToggleButton.Parent := PanelProps;

  FFrameSplitButton := TFrameSplitButton.Create(Self);
  FFrameSplitButton.Parent := PanelProps;

  FFrameDropDownButton := TFrameDropDownButton.Create(Self);
  FFrameDropDownButton.Parent := PanelProps;

  FFrameQuickAccessToolbar := TFrameQuickAccessToolbar.Create(Self);
  FFrameQuickAccessToolbar.Parent := PanelProps;

  FFrameQatControl := TFrameQatControl.Create(Self);
  FFrameQatControl.Parent := PanelProps;

  FFrameHelpButton := TFrameHelpButton.Create(Self);
  FFrameHelpButton.Parent := PanelProps;

  FFrameTab := TFrameTab.Create(Self);
  FFrameTab.Parent := PanelProps;

  FFrameTabGroup := TFrameTabGroup.Create(Self);
  FFrameTabGroup.Parent := PanelProps;

  FFrameGroup := TFrameGroup.Create(Self);
  FFrameGroup.Parent := PanelProps;

  FFrameScale := TFrameScale.Create(Self);
  FFrameScale.Parent := PanelProps;

  FFrameDropDownGallery := TFrameDropDownGallery.Create(Self);
  FFrameDropDownGallery.Parent := PanelProps;

  FFrameSplitButtonGallery := TFrameSplitButtonGallery.Create(Self);
  FFrameSplitButtonGallery.Parent := PanelProps;

  FFrameInRibbonGallery := TFrameInRibbonGallery.Create(Self);
  FFrameInRibbonGallery.Parent := PanelProps;

  FFrameSizeDefinition := TFrameSizeDefinition.Create(Self);
  FFrameSizeDefinition.Parent := PanelProps;

  FFrameGroupSizeDefinition := TFrameGroupSizeDefinition.Create(Self);
  FFrameGroupSizeDefinition.Parent := PanelProps;

  FFrameControlSizeDefinition := TFrameControlSizeDefintion.Create(Self);
  FFrameControlSizeDefinition.Parent := PanelProps;

  FFrameColumnBreak := TFrameColumnBreak.Create(Self);
  FFrameColumnBreak.Parent := PanelProps;

  FFrameFloatieFontControl := TFrameFloatieFontControl.Create(Self);
  FFrameFloatieFontControl.Parent := PanelProps;
  FFrameFloatieFontControl.Name := 'FrameFloatieFontControl1';

  FFrameFontControl := TFrameFontControl.Create(Self);
  FFrameFontControl.Parent := PanelProps;

  FFrameControlGroup := TFrameControlGroup.Create(Self);
  FFrameControlGroup.Parent := PanelProps;

  FFrameComboBox := TFrameComboBox.Create(Self);
  FFrameComboBox.Parent := PanelProps;

  FFrameCheckBox := TFrameCheckBox.Create(Self);
  FFrameCheckBox.Parent := PanelProps;

  FFrameDropDownColorPicker := TFrameDropDownColorPicker.Create(Self);
  FFrameDropDownColorPicker.Parent := PanelProps;

  FFrameSpinner := TFrameSpinner.Create(Self);
  FFrameSpinner.Parent := PanelProps;

  FFrameMiniToolbar := TFrameMiniToolbar.Create(Self);
  FFrameMiniToolbar.Parent := PanelProps;

  FFrameContextMenu := TFrameContextMenu.Create(Self);
  FFrameContextMenu.Parent := PanelProps;

  FFrameContextMap := TFrameContextMap.Create(Self);
  FFrameContextMap.Parent := PanelProps;
end;

procedure TFrameViews.Deactivate;
begin
  ActionDelete.ShortCut := 0;
  ActionMoveUp.ShortCut := 0;
  ActionMoveDown.ShortCut := 0;
end;

destructor TFrameViews.Destroy;
begin
  FCommands.Free;
  inherited;
end;

function TFrameViews.GetObject(const Node: TTreeNode; out Obj: TRibbonObject): Boolean;
var
  O: TObject;
begin
  Obj := nil;
  Result := Assigned(Node) and Assigned(Node.Data);
  if (Result) then
  begin
    O := Node.Data;
    Result := (O is TRibbonObject);
    if (Result) then
      Obj := TRibbonObject(O);
  end;
end;

procedure TFrameViews.Modified;
begin
  FormMain.Modified;
end;

procedure TFrameViews.MoveNode(const Direction: Integer);
var
  Node, ParentNode, Sibling: TTreeNode;
  Obj, ParentObj: TRibbonObject;
begin
  Node := TreeViewRibbon.Selected;
  if (Node = nil) or (Node.Data = nil) then
    Exit;
  Obj := Node.Data;

  ParentNode := Node.Parent;
  if (ParentNode = nil) or (ParentNode.Data = nil) then
    Exit;
  ParentObj := ParentNode.Data;

  if (ParentObj.ObjectType = otList) then
  begin
    ParentNode := ParentNode.Parent;
    if (ParentNode = nil) or (ParentNode.Data = nil) then
      Exit;
    ParentObj := ParentNode.Data;
  end;

  if (ParentObj.Reorder(Obj, Direction)) then
  begin
    if (Direction < 0) then
      Sibling := Node.getPrevSibling
    else
    begin
      Sibling := Node.getNextSibling;
      if Assigned(Sibling) then
        Sibling := Sibling.getNextSibling;
    end;

    if Assigned(Sibling) then
      Node.MoveTo(Sibling, naInsert)
    else
      Node.MoveTo(Node.getNextSibling, naAdd);

    ActionMoveUp.Enabled := (Node.getPrevSibling <> nil);
    ActionMoveDown.Enabled := (Node.getNextSibling <> nil);
    Modified;
  end;
end;

procedure TFrameViews.PopupMenuPopup(Sender: TObject);
var
  Node: TTreeNode;
  Obj: TRibbonObject;
  ObjType: TRibbonObjectType;
begin
  Node := TreeViewRibbon.Selected;
  if (Node = nil) or (Node.Data = nil) then
    ObjType := otApplication
  else
  begin
    Obj := Node.Data;
    ObjType := Obj.ObjectType;
  end;

  if (ObjType = otList) then
  begin
    if (Node = nil) or (Node.Parent = nil) or (Node.Parent.Data = nil) then
      ObjType := otApplication
    else
    begin
      Obj := Node.Parent.Data;
      ObjType := Obj.ObjectType;
    end;

    case ObjType of
      otSplitButton:
        ObjType := otSplitButton_Items;

      otViewRibbon:
        if (Node.ImageIndex = II_SIZE_DEF) then
          ObjType := otRibbonSizeDefinitions
        else if (Node.ImageIndex = II_CONTEXTUAL_TAB) then
          ObjType := otContextualTabs
        else
          ObjType := otList;

      otViewContextPopup:
        if (Node.ImageIndex = II_MINI_TOOLBAR) then
          ObjType := otMiniToolbars
        else if (Node.ImageIndex = II_CONTEXT_MENU) then
          ObjType := otContextMenus
        else if (Node.ImageIndex = II_CONTEXT_MAP) then
          ObjType := otContextMaps
        else
          ObjType := otList;

      otScalingPolicy:
        ObjType := otScalingPolicy_IdealSizes;
    else
      begin
        Abort;
        Exit;
      end;
    end;
  end;

  { Make sure Selected item stays selected when popup menu pops up }
  TreeViewRibbon.Selected := TreeViewRibbon.Selected;

  ActionAddButton.Visible := (ObjType in [otMenuGroup, otAppMenuGroup, otMiniToolbarMenuGroup,
    otSplitButton_Items, otGroup, otControlGroup, otDropDownButton]);
  ActionAddToggleButton.Visible := (ObjType in [otMenuGroup, otMiniToolbarMenuGroup,
    otSplitButton_Items, otGroup, otControlGroup, otDropDownButton]);
  ActionAddDropDownButton.Visible := (ObjType in [otMenuGroup, otAppMenuGroup,
    otMiniToolbarMenuGroup, otSplitButton_Items, otGroup, otControlGroup, otDropDownButton]);
  ActionAddSplitButton.Visible := (ObjType in [otMenuGroup, otAppMenuGroup,
    otMiniToolbarMenuGroup, otSplitButton_Items, otGroup, otControlGroup, otDropDownButton]);
  ActionAddCheckBox.Visible := (ObjType in [otMenuGroup, otMiniToolbarMenuGroup,
    otSplitButton_Items, otGroup, otControlGroup, otDropDownButton]);
  ActionAddDropDownGallery.Visible := (ObjType in [otMenuGroup, otAppMenuGroup,
    otMiniToolbarMenuGroup, otSplitButton_Items, otGroup, otControlGroup, otDropDownButton]);
  ActionAddSplitButtonGallery.Visible := (ObjType in [otMenuGroup, otAppMenuGroup,
    otMiniToolbarMenuGroup, otSplitButton_Items, otGroup, otControlGroup, otDropDownButton]);
  ActionAddDropDownColorPicker.Visible := (ObjType in [otMenuGroup, otMiniToolbarMenuGroup,
  otSplitButton_Items, otGroup, otControlGroup, otDropDownButton]);
  ActionAddMenuGroup.Visible := (ObjType in [otApplicationMenu, otSplitButton_Items, otContextMenu, otDropDownButton]);
  ActionAddQatButton.Visible := (ObjType in [otQuickAccessToolbar]);
  ActionAddQatToggleButton.Visible := (ObjType in [otQuickAccessToolbar]);
  ActionAddQatCheckBox.Visible := (ObjType in [otQuickAccessToolbar]);
  ActionAddRibbonSizeDefinition.Visible := (ObjType in [otRibbonSizeDefinitions]);
  ActionAddGroupSizeDefinition.Visible := (ObjType in [otRibbonSizeDefinition, otSizeDefinition]);
  ActionAddControlSizeDefinition.Visible := (ObjType in [otGroupSizeDefinition, otRow, otControlSizeGroup]);
  ActionAddControlSizeGroup.Visible := (ObjType in [otGroupSizeDefinition, otRow]);
  ActionAddColumnBreak.Visible := (ObjType in [otGroupSizeDefinition]);
  ActionAddRow.Visible := (ObjType in [otGroupSizeDefinition]);
  ActionAddGroup.Visible := (ObjType in [otTab]);
  ActionAddScale.Visible := (ObjType in [otScalingPolicy, otScalingPolicy_IdealSizes]);
  ActionAddControlGroup.Visible := (ObjType in [otGroup, otControlGroup]);
  ActionAddComboBox.Visible := (ObjType in [otMiniToolbarMenuGroup, otGroup, otControlGroup]);
  ActionAddSpinner.Visible := (ObjType in [otMiniToolbarMenuGroup, otGroup, otControlGroup]);
  ActionAddInRibbonGallery.Visible := (ObjType in [otGroup, otControlGroup]);
  ActionAddFontControl.Visible := (ObjType in [otGroup, otControlGroup]);
  ActionAddFloatieFontControl.Visible := (ObjType in [otMiniToolbarMenuGroup]);
  ActionAddTab.Visible := (ObjType in [otViewRibbon, otTabGroup]);
  ActionAddTabGroup.Visible := (ObjType in [otContextualTabs]);
//  ActionAddContextPopup.Visible := (Node.Level = 0);
  ActionAddMiniToolbar.Visible := (ObjType in [otMiniToolbars]);
  ActionAddContextMenu.Visible := (ObjType in [otContextMenus]);
  ActionAddContextMap.Visible := (ObjType in [otContextMaps]);
  ActionAddMiniToolbarMenuGroup.Visible := (ObjType in [otMiniToolbar]);
end;

procedure TFrameViews.ShowDocument(const Document: TRibbonDocument);
var
  Root: TTreeNode;
  View: TRibbonView;
  Ribbon: TRibbonViewRibbon absolute View;
begin
  FDocument := Document;
  if Assigned(FCurrentFrame) then
    FCurrentFrame.Visible := False;
  FCurrentFrame := nil;
  FCurrentNode := nil;
  FCurrentObject := nil;
  FSelectAddedNode := False;
  TreeViewRibbon.Items.BeginUpdate;
  try
    TreeViewRibbon.Items.Clear;

    for View in FDocument.Application.Views do
    begin
      if (View is TRibbonViewRibbon) then
      begin
        Root := AddNode(nil, RS_RIBBON, II_RIBBON, Ribbon);
        AddApplicationMenu(Root, Ribbon.ApplicationMenu);
        AddQuickAccessToolbar(Root, Ribbon.QuickAccessToolbar);
        AddSizeDefinitions(Root, Ribbon.SizeDefinitions);
        AddNode(Root, RS_HELP_BUTTON, II_HELP_BUTTON, Ribbon.HelpButton);
        AddContextualTabs(Root, Ribbon.ContextualTabs);
        AddTabs(Root, Ribbon.Tabs);
        Root.Expand(False);
      end
      else if (View is TRibbonViewContextPopup) then
        AddContextPopup(TRibbonViewContextPopup(View));
    end;
  finally
    TreeViewRibbon.Items.EndUpdate;
    FSelectAddedNode := True;
  end;
end;

procedure TFrameViews.TreeViewRibbonChange(Sender: TObject; Node: TTreeNode);
begin
  ActionDelete.Enabled := False;
  ActionMoveUp.Enabled := False;
  ActionMoveDown.Enabled := False;
  if (Node = nil) or (Node = FCurrentNode) or (not Node.Selected) then
    Exit;

  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.Visible := False;
    FCurrentFrame := nil;
  end;

  FCurrentNode := Node;
  FCurrentObject := Node.Data;
  if (FCurrentObject = nil) then
    Exit;

  ActionDelete.Enabled := True;

  ActionMoveUp.Enabled := FCurrentObject.CanReorder and (Node.getPrevSibling <> nil);
  ActionMoveDown.Enabled := FCurrentObject.CanReorder and (Node.getNextSibling <> nil);

  case FCurrentObject.ObjectType of
    otApplicationMenu:
      begin
        FCurrentFrame := FFrameApplicationMenu;
        ActionDelete.Enabled := False;
      end;

    otButton:
      FCurrentFrame := FFrameButton;

    otToggleButton:
      FCurrentFrame := FFrameToggleButton;

    otSplitButton:
      FCurrentFrame := FFrameSplitButton;

    otDropDownButton:
      FCurrentFrame := FFrameDropDownButton;

    otQuickAccessToolbar:
      begin
        FCurrentFrame := FFrameQuickAccessToolbar;
        ActionDelete.Enabled := False;
      end;

    otHelpButton:
      begin
        FCurrentFrame := FFrameHelpButton;
        ActionDelete.Enabled := False;
      end;

    otTab:
      FCurrentFrame := FFrameTab;

    otTabGroup:
      FCurrentFrame := FFrameTabGroup;

    otGroup:
      FCurrentFrame := FFrameGroup;

    otQatButton,
    otQatToggleButton,
    otQatCheckBox:
      FCurrentFrame := FFrameQatControl;

    otDropDownGallery:
      FCurrentFrame := FFrameDropDownGallery;

    otSplitButtonGallery:
      FCurrentFrame := FFrameSplitButtonGallery;

    otInRibbonGallery:
      FCurrentFrame := FFrameInRibbonGallery;

    otMenuGroup,
    otMiniToolbarMenuGroup:
      FCurrentFrame := FFrameMenuGroup;

    otAppMenuGroup:
      FCurrentFrame := FFrameAppMenuGroup;

    otScale:
      FCurrentFrame := FFrameScale;

    otViewRibbon:
      begin
        FCurrentFrame := FFrameViewRibbon;
        ActionDelete.Enabled := False;
      end;

    otSizeDefinition,
    otRibbonSizeDefinition:
      FCurrentFrame := FFrameSizeDefinition;

    otGroupSizeDefinition:
      FCurrentFrame := FFrameGroupSizeDefinition;

    otControlSizeDefinition:
      FCurrentFrame := FFrameControlSizeDefinition;

    otColumnBreak:
      FCurrentFrame := FFrameColumnBreak;

    otFloatieFontControl:
      FCurrentFrame := FFrameFloatieFontControl;

    otFontControl:
      FCurrentFrame := FFrameFontControl;

    otControlGroup:
      FCurrentFrame := FFrameControlGroup;

    otComboBox:
      FCurrentFrame := FFrameComboBox;

    otCheckBox:
      FCurrentFrame := FFrameCheckBox;

    otDropDownColorPicker:
      FCurrentFrame := FFrameDropDownColorPicker;

    otSpinner:
      FCurrentFrame := FFrameSpinner;

    otMiniToolbar:
      FCurrentFrame := FFrameMiniToolbar;

    otContextMenu:
      FCurrentFrame := FFrameContextMenu;

    otContextMap:
      FCurrentFrame := FFrameContextMap;

    otList,
    otViewContextPopup:
      begin
        ActionDelete.Enabled := False;
        { No properties }
        Exit;
      end;

    otRow,
    otControlSizeGroup,
    otScalingPolicy:
      { No properties }
      Exit;
  end;

  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.ShowProperties(FCurrentObject, FCurrentNode);
    FCurrentFrame.Visible := True;
  end
  else
    Assert(False);
end;

procedure TFrameViews.UpdateCurrentNode;
begin
  if Assigned(FCurrentNode) then
    UpdateTreeNodeCaption(FCurrentNode, False);
end;

procedure TFrameViews.UpdateTreeNodeCaption(const Node: TTreeNode;
  const Recursive: Boolean);
var
  Obj: TRibbonObject;
  Control: TRibbonControl absolute Obj;
  I: Integer;
begin
  Obj := Node.Data;
  if Assigned(Obj) then
  begin
    if (Obj is TRibbonControl) then
    begin
      if (not (Control.ObjectType in [otQuickAccessToolbar, otHelpButton, otApplicationMenu])) then
        Node.Text := Control.DisplayName;
    end
    else if (Obj.ObjectType <> otList) then
      Node.Text := Obj.DisplayName;
  end;

  if (Recursive) then
    for I := 0 to Node.Count - 1 do
      UpdateTreeNodeCaption(Node[I], True);
end;

procedure TFrameViews.ActionAddButtonExecute(Sender: TObject);
begin
  AddNewObject(otButton);
end;

procedure TFrameViews.ActionAddCheckBoxExecute(Sender: TObject);
begin
  AddNewObject(otCheckBox);
end;

procedure TFrameViews.ActionAddColumnBreakExecute(Sender: TObject);
begin
  AddNewObject(otColumnBreak);
end;

procedure TFrameViews.ActionAddComboBoxExecute(Sender: TObject);
begin
  AddNewObject(otComboBox);
end;

procedure TFrameViews.ActionAddContextMapExecute(Sender: TObject);
begin
  AddNewObject(otContextMap);
end;

procedure TFrameViews.ActionAddContextMenuExecute(Sender: TObject);
begin
  AddNewObject(otContextMenu);
end;

procedure TFrameViews.ActionAddContextPopupExecute(Sender: TObject);
//var
//  Popup: TRibbonViewContextPopup;
begin
//  Popup := FDocument.Application.AddNew(otViewContextPopup) as TRibbonViewContextPopup;
//  AddContextPopup(Popup);
//  Modified;
end;

procedure TFrameViews.ActionAddControlGroupExecute(Sender: TObject);
begin
  AddNewObject(otControlGroup);
end;

procedure TFrameViews.ActionAddControlSizeDefinitionExecute(Sender: TObject);
begin
  AddNewObject(otControlSizeDefinition);
end;

procedure TFrameViews.ActionAddControlSizeGroupExecute(Sender: TObject);
begin
  AddNewObject(otControlSizeGroup);
end;

procedure TFrameViews.ActionAddDropDownButtonExecute(Sender: TObject);
begin
  AddNewObject(otDropDownButton);
end;

procedure TFrameViews.ActionAddDropDownColorPickerExecute(Sender: TObject);
begin
  AddNewObject(otDropDownColorPicker);
end;

procedure TFrameViews.ActionAddDropDownGalleryExecute(Sender: TObject);
begin
  AddNewObject(otDropDownGallery);
end;

procedure TFrameViews.ActionAddFloatieFontControlExecute(Sender: TObject);
begin
  AddNewObject(otFloatieFontControl);
end;

procedure TFrameViews.ActionAddFontControlExecute(Sender: TObject);
begin
  AddNewObject(otFontControl);
end;

procedure TFrameViews.ActionAddGroupExecute(Sender: TObject);
begin
  AddNewObject(otGroup);
end;

procedure TFrameViews.ActionAddGroupSizeDefinitionExecute(Sender: TObject);
begin
  AddNewObject(otGroupSizeDefinition);
end;

procedure TFrameViews.ActionAddInRibbonGalleryExecute(Sender: TObject);
begin
  AddNewObject(otInRibbonGallery);
end;

procedure TFrameViews.ActionAddMenuGroupExecute(Sender: TObject);
var
  Node: TTreeNode;
  Obj: TRibbonObject;
begin
  Node := TreeViewRibbon.Selected;
  if (Node = nil) or (Node.Data = nil) then
    Exit;

  Obj := Node.Data;
  case Obj.ObjectType of
    otApplicationMenu:
      AddNewObject(otAppMenuGroup);
  else
    AddNewObject(otMenuGroup);
  end;
end;

procedure TFrameViews.ActionAddMiniToolbarExecute(Sender: TObject);
begin
  AddNewObject(otMiniToolbar);
end;

procedure TFrameViews.ActionAddMiniToolbarMenuGroupExecute(Sender: TObject);
begin
  AddNewObject(otMiniToolbarMenuGroup);
end;

procedure TFrameViews.ActionAddQatButtonExecute(Sender: TObject);
begin
  AddNewObject(otQatButton);
end;

procedure TFrameViews.ActionAddQatCheckBoxExecute(Sender: TObject);
begin
  AddNewObject(otQatCheckBox);
end;

procedure TFrameViews.ActionAddQatToggleButtonExecute(Sender: TObject);
begin
  AddNewObject(otQatToggleButton);
end;

procedure TFrameViews.ActionAddRibbonSizeDefinitionExecute(Sender: TObject);
begin
  AddNewObject(otRibbonSizeDefinition);
end;

procedure TFrameViews.ActionAddRowExecute(Sender: TObject);
begin
  AddNewObject(otRow);
end;

procedure TFrameViews.ActionAddScaleExecute(Sender: TObject);
var
  Node: TTreeNode;
  Obj: TRibbonObject;
  ScalingPolicy: TRibbonScalingPolicy absolute Obj;
  Scale: TRibbonScale;
begin
  Node := TreeViewRibbon.Selected;
  if GetObject(Node, Obj) then
  begin
    if (Obj.ObjectType = otList) then
    begin
      if GetObject(Node.Parent, Obj) and (Obj is TRibbonScalingPolicy) then
      begin
        Scale := ScalingPolicy.AddIdealSize;
        AddScale(Node, Scale);
        Modified;
        Exit;
      end;
    end;
  end;
  AddNewObject(otScale);
end;

procedure TFrameViews.ActionAddSpinnerExecute(Sender: TObject);
begin
  AddNewObject(otSpinner);
end;

procedure TFrameViews.ActionAddSplitButtonExecute(Sender: TObject);
begin
  AddNewObject(otSplitButton);
end;

procedure TFrameViews.ActionAddSplitButtonGalleryExecute(Sender: TObject);
begin
  AddNewObject(otSplitButtonGallery);
end;

procedure TFrameViews.ActionAddTabExecute(Sender: TObject);
begin
  AddNewObject(otTab);
end;

procedure TFrameViews.ActionAddTabGroupExecute(Sender: TObject);
begin
  AddNewObject(otTabGroup);
end;

procedure TFrameViews.ActionAddToggleButtonExecute(Sender: TObject);
begin
  AddNewObject(otToggleButton);
end;

procedure TFrameViews.ActionDeleteExecute(Sender: TObject);
var
  Node: TTreeNode;
  Obj, ParentObj: TRibbonObject;
begin
  Node := TreeViewRibbon.Selected;
  if (not GetObject(Node, Obj)) then
    Exit;

  if (Node.Count > 0) then
  begin
    if (TaskMessageDlg(RS_DELETE_ITEM_HEADER, RS_DELETE_ITEM_MESSAGE,
      mtConfirmation, [mbYes, mbNo], 0, mbYes) <> mrYes)
    then
      Exit;
  end;

  if (not GetObject(Node.Parent, ParentObj)) then
    Exit;

  if (ParentObj.ObjectType = otList) then
  begin
    if (not GetObject(Node.Parent.Parent, ParentObj)) then
      Exit;
  end;

  if ParentObj.Delete(Obj) then
  begin
    Node.Free;
    Modified;
  end;
end;

procedure TFrameViews.ActionMoveDownExecute(Sender: TObject);
begin
  MoveNode(1);
end;

procedure TFrameViews.ActionMoveUpExecute(Sender: TObject);
begin
  MoveNode(-1);
end;

procedure TFrameViews.Activate;
var
  Command: TRibbonCommand;
begin
  ActionDelete.ShortCut := ShortCut(VK_DELETE, [ssCtrl]);
  ActionMoveUp.ShortCut := ShortCut(VK_UP, [ssCtrl]);
  ActionMoveDown.ShortCut := ShortCut(VK_DOWN, [ssCtrl]);;

  UpdateTreeNodeCaption(TreeViewRibbon.Items[0], True);
  FCommands.Clear;
  FCommands.AddObject(RS_NONE, nil);
  for Command in FDocument.Application.Commands do
    FCommands.AddObject(Command.DisplayName, Command);
  FCommands.Sort;

  FFrameApplicationMenu.Activate;
  FFrameMenuGroup.Activate;
  FFrameAppMenuGroup.Activate;
  FFrameButton.Activate;
  FFrameToggleButton.Activate;
  FFrameSplitButton.Activate;
  FFrameDropDownButton.Activate;
  FFrameQuickAccessToolbar.Activate;
  FFrameQatControl.Activate;
  FFrameHelpButton.Activate;
  FFrameTab.Activate;
  FFrameTabGroup.Activate;
  FFrameGroup.Activate;
  FFrameScale.Activate;
  FFrameDropDownGallery.Activate;
  FFrameSplitButtonGallery.Activate;
  FFrameInRibbonGallery.Activate;
  FFrameFloatieFontControl.Activate;
  FFrameFontControl.Activate;
  FFrameComboBox.Activate;
  FFrameCheckBox.Activate;
  FFrameDropDownColorPicker.Activate;
  FFrameSpinner.Activate;
  FFrameContextMap.Activate;
end;

end.
