unit FPreview;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UIRibbon, UIRibbonApi, UIRibbonForm, UIRibbonCommands, ComCtrls,
  StdCtrls, CheckLst, RibbonMarkup, BasicXml, Generics.Collections, ExtCtrls;

type
  TFormPreview = class(TUIRibbonForm)
    PageControl: TPageControl;
    TabSheetAppModes: TTabSheet;
    TabSheetContextTabs: TTabSheet;
    TabSheetContextPopups: TTabSheet;
    TabSheetColorize: TTabSheet;
    CheckListBoxAppModes: TCheckListBox;
    LabelAppModes: TLabel;
    LabelContextTabs: TLabel;
    CheckListBoxContextTabs: TCheckListBox;
    LabelContextPopups: TLabel;
    ListBoxContextPopups: TListBox;
    GroupBoxSamples: TGroupBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel23: TPanel;
    Panel24: TPanel;
    GroupBoxBackgroundColor: TGroupBox;
    EditBkH: TEdit;
    UpDownBkH: TUpDown;
    EditBkS: TEdit;
    UpDownBkS: TUpDown;
    EditBkB: TEdit;
    UpDownBkB: TUpDown;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    GroupBoxHighlightColor: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    EditHiH: TEdit;
    UpDownHiH: TUpDown;
    EditHiS: TEdit;
    UpDownHiS: TUpDown;
    EditHiB: TEdit;
    UpDownHiB: TUpDown;
    GroupBoxTextColor: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    EditTextH: TEdit;
    UpDownTextH: TUpDown;
    EditTextS: TEdit;
    UpDownTextS: TUpDown;
    EditTextB: TEdit;
    UpDownTextB: TUpDown;
    procedure CheckListBoxAppModesClickCheck(Sender: TObject);
    procedure CheckListBoxContextTabsClickCheck(Sender: TObject);
    procedure ListBoxContextPopupsClick(Sender: TObject);
    procedure PanelSampleClick(Sender: TObject);
    procedure EditBackgroundColorChange(Sender: TObject);
    procedure EditHighlightColorChange(Sender: TObject);
    procedure EditTextColorChange(Sender: TObject);
  private
    { Private declarations }
    FInstance: THandle;
    FDocument: TRibbonDocument;
    FXmlDoc: TXmlDocument;
    FAllApplicationModes: Cardinal;
    FCommandMap: TDictionary<String, Cardinal>;
    FGalleryCommand: TUICommand;
    FGalleryImage: IUIImage;
    procedure ParseHeaderFile;
    function FindCommandId(const Command: TRibbonCommand; out Id: Cardinal): Boolean;
    procedure InitializeApplicationModes;
    procedure GetApplicationModes(const Element: TXmlElement);
    procedure InitializeContextualTabs;
    procedure InitializeContextPopups;
    procedure InitializeColorization;
  strict protected
    procedure CommandCreated(const Sender: TUIRibbon;
      const Command: TUICommand); override;
  public
    { Public declarations }
    constructor Create(const Instance: THandle; const Document: TRibbonDocument); reintroduce;
    destructor Destroy; override;

    function RibbonInstance: THandle; override;
  end;

resourcestring
  RS_CONTEXT_TAB_GROUP = 'Contextual Tab Group';
  RS_ITEM = 'Item';
  RS_ITEM_DESC = 'This is Item';

implementation

{$R *.dfm}
{$R Resources.res}

uses
  Math,
  UIRibbonUtils;

const
  SAMPLE_COLORS: array [0..11, 0..2] of Byte = (
    (143, 156, 242), (168, 158, 245), (128, 122, 242), ( 84, 148, 245),
    ( 43, 145, 245), ( 20, 156, 242), (  3, 161, 242), (235, 153, 245),
    (214, 143, 242), (194, 148, 242), ( 66, 156, 242), (  0,   0, 245));

{ TFormPreview }

procedure TFormPreview.CheckListBoxAppModesClickCheck(Sender: TObject);
var
  I, J: Integer;
  AppModes: Cardinal;
begin
  AppModes := 0;
  for I := 0 to CheckListBoxAppModes.Count - 1 do
    if (CheckListBoxAppModes.Checked[I]) then
    begin
      J := Integer(CheckListBoxAppModes.Items.Objects[I]);
      AppModes := AppModes or (1 shl J);
    end;

  if (AppModes = 0) then
  begin
    AppModes := 1;
    CheckListBoxAppModes.Checked[0] := True;
  end;

  Ribbon.SetApplicationModes(AppModes);
end;

procedure TFormPreview.CheckListBoxContextTabsClickCheck(Sender: TObject);
var
  I: Integer;
  CommandId: Cardinal;
  Command: TUICommandContext;
begin
  for I := 0 to CheckListBoxContextTabs.Count - 1 do
  begin
    CommandId := Cardinal(CheckListBoxContextTabs.Items.Objects[I]);
    Command := Ribbon.Commands[CommandId] as TUICommandContext;
    if Assigned(Command) then
    begin
      if (CheckListBoxContextTabs.Checked[I]) then
        Command.Availability := caAvailable
      else
        Command.Availability := caNotAvailable;
    end;
  end;
end;

constructor TFormPreview.Create(const Instance: THandle;
  const Document: TRibbonDocument);
begin
  inherited Create(nil);
  FInstance := Instance;
  FDocument := Document;
  FCommandMap := TDictionary<String, Cardinal>.Create;
  FGalleryCommand := TUICommandAnchor.Create(Ribbon, 50001);
  FGalleryImage := TUIImage.Create(HInstance, 'DEFAULT_IMAGE');
  FXmlDoc := TXmlDocument.Create;
  FXmlDoc.LoadFromFile(FDocument.Filename);
  PageControl.ActivePage := TabSheetAppModes;

  ParseHeaderFile;
  InitializeApplicationModes;
  InitializeContextualTabs;
  InitializeContextPopups;
  InitializeColorization;
end;

destructor TFormPreview.Destroy;
begin
  FreeLibrary(FInstance);
  FXmlDoc.Free;
  FCommandMap.Free;
  inherited;
end;

procedure TFormPreview.EditBackgroundColorChange(Sender: TObject);
begin
  Ribbon.BackgroundHsbColor := HsbToHsbColor(
    UpDownBkH.Position, UpDownBkS.Position, UpDownBkB.Position);
end;

procedure TFormPreview.EditHighlightColorChange(Sender: TObject);
begin
  Ribbon.HighlightHsbColor := HsbToHsbColor(
    UpDownHiH.Position, UpDownHiS.Position, UpDownHiB.Position);
end;

procedure TFormPreview.EditTextColorChange(Sender: TObject);
begin
  Ribbon.TextHsbColor := HsbToHsbColor(
    UpDownTextH.Position, UpDownTextS.Position, UpDownTextB.Position);
end;

function TFormPreview.FindCommandId(const Command: TRibbonCommand;
  out Id: Cardinal): Boolean;
begin
  if Assigned(Command) then
  begin
    Result := FCommandMap.TryGetValue(Command.Name, Id);
    if (not Result) then
      Result := FCommandMap.TryGetValue(Command.Symbol, Id);
  end
  else
    Result := False;
end;

procedure TFormPreview.GetApplicationModes(const Element: TXmlElement);
var
  S: String;
  Child: TXmlElement;
  Modes: TStringList;
  I: Integer;
begin
  S := Element.AttributeAsString['ApplicationModes'];
  if (S <> '') then
  begin
    Modes := TStringList.Create;
    try
      Modes.Delimiter := ',';
      Modes.DelimitedText := S;
      if (Modes.Count > 0) then
      begin
        for S in Modes do
        begin
          I := StrToIntDef(S, -1);
          if (I >= 0) and (I <= 31) then
            FAllApplicationModes := FAllApplicationModes or (1 shl I);
        end;
      end;
    finally
      Modes.Free;
    end;
  end;
  for Child in Element do
    GetApplicationModes(Child);
end;

procedure TFormPreview.InitializeApplicationModes;
var
  I: Integer;
begin
  GetApplicationModes(FXmlDoc.Root);
  CheckListBoxAppModes.Visible := (FAllApplicationModes <> 0);
  LabelAppModes.Visible := (FAllApplicationModes = 0);
  if (FAllApplicationModes <> 0) then
  begin
    CheckListBoxAppModes.Items.AddObject('0', Pointer(0));
    for I := 1 to 31 do
      if ((FAllApplicationModes and (1 shl I)) <> 0) then
        CheckListBoxAppModes.Items.AddObject(IntToStr(I), Pointer(I));
    CheckListBoxAppModes.Checked[0] := True;
  end;
end;

procedure TFormPreview.InitializeColorization;
var
  I, H, S, B: Integer;
  OuterPanel, InnerPanel: TPanel;
begin
  for I := 0 to Min(Length(SAMPLE_COLORS), GroupBoxSamples.ControlCount) - 1 do
  begin
    H := SAMPLE_COLORS[I, 0];
    S := SAMPLE_COLORS[I, 1];
    B := SAMPLE_COLORS[I, 2];

    OuterPanel:= GroupBoxSamples.Controls[I] as TPanel;
    InnerPanel:= OuterPanel.Controls[0] as TPanel;
    OuterPanel.Color := HsbToColor(H, S, 230);
    InnerPanel.Color := HsbToColor(H, S, B);
    InnerPanel.Font.Color := HsbToColor(H, S div 2, 158);
  end;

  HsbColorToHsb(Ribbon.BackgroundHsbColor, H, S, B);
  UpDownBkH.Position := H;
  UpDownBkS.Position := S;
  UpDownBkB.Position := B;

  HsbColorToHsb(Ribbon.HighlightHsbColor, H, S, B);
  UpDownHiH.Position := H;
  UpDownHiS.Position := S;
  UpDownHiB.Position := B;

  HsbColorToHsb(Ribbon.TextHsbColor, H, S, B);
  UpDownTextH.Position := H;
  UpDownTextS.Position := S;
  UpDownTextB.Position := B;
end;

procedure TFormPreview.InitializeContextPopups;
var
  View: TRibbonView;
  ContextPopup: TRibbonViewContextPopup absolute View;
  ContextMap: TRibbonContextMap;
  CommandId: Cardinal;
begin
  for View in FDocument.Application.Views do
  begin
    if (View.ObjectType = otViewContextPopup) then
    begin
      for ContextMap in ContextPopup.ContextMaps do
      begin
        if FindCommandId(ContextMap.CommandRef, CommandId) then
          ListBoxContextPopups.Items.AddObject(ContextMap.DisplayName, Pointer(CommandId));
      end;
    end;
  end;

  ListBoxContextPopups.Visible := (ListBoxContextPopups.Items.Count > 0);
  LabelContextPopups.Visible := (ListBoxContextPopups.Items.Count = 0);
end;

procedure TFormPreview.InitializeContextualTabs;
var
  Ribbon: TRibbonViewRibbon;
  TabGroup: TRibbonTabGroup;
  CommandId: Cardinal;
begin
  Ribbon := FDocument.Application.Ribbon;
  if Assigned(Ribbon) then
  begin
    for TabGroup in Ribbon.ContextualTabs do
    begin
      if FindCommandId(TabGroup.CommandRef, CommandId) then
        CheckListBoxContextTabs.Items.AddObject(TabGroup.DisplayName, Pointer(CommandId));
    end;
  end;

  CheckListBoxContextTabs.Visible := (CheckListBoxContextTabs.Items.Count > 0);
  LabelContextTabs.Visible := (CheckListBoxContextTabs.Items.Count = 0);
end;

procedure TFormPreview.ListBoxContextPopupsClick(Sender: TObject);
var
  CommandId: Cardinal;
begin
  if (ListBoxContextPopups.ItemIndex < 0) then
    Exit;

  CommandId := Cardinal(ListBoxContextPopups.Items.Objects[ListBoxContextPopups.ItemIndex]);
  Ribbon.ShowContextPopup(CommandId, Mouse.CursorPos);
end;

procedure TFormPreview.PanelSampleClick(Sender: TObject);
var
  SampleIndex, H, S, B: Integer;
begin
  SampleIndex := (Sender as TPanel).Tag;
  H := SAMPLE_COLORS[SampleIndex, 0];
  S := SAMPLE_COLORS[SampleIndex, 1];
  B := SAMPLE_COLORS[SampleIndex, 2];

  Ribbon.BackgroundHsbColor := HsbToHsbColor(H, S, B);
  UpDownBkH.Position := H;
  UpDownBkS.Position := S;
  UpDownBkB.Position := B;

  B := 230;
  Ribbon.HighlightHsbColor := HsbToHsbColor(H, S, B);
  UpDownHiH.Position := H;
  UpDownHiS.Position := S;
  UpDownHiB.Position := B;

  S := S div 2;
  B := 158;
  Ribbon.TextHsbColor := HsbToHsbColor(H, S, B);
  UpDownTextH.Position := H;
  UpDownTextS.Position := S;
  UpDownTextB.Position := B;
end;

procedure TFormPreview.ParseHeaderFile;
var
  HeaderFilename: String;
  HeaderFile: TextFile;
  S, CommandName: String;
  I, CommandId: Integer;
begin
  HeaderFilename := ChangeFileExt(FDocument.Filename, '.h');
  if (not FileExists(HeaderFilename)) then
    Exit;

  AssignFile(HeaderFile, HeaderFilename);
  Reset(HeaderFile);
  try
    while (not Eof(HeaderFile)) do
    begin
      ReadLn(HeaderFile, S);
      S := Trim(S);
      if (Copy(S, 1, 8) = '#define ') then
      begin
        Delete(S, 1, 8);
        I := Pos(' ', S);
        if (I > 0) then
        begin
          CommandName := Copy(S, 1, I - 1);
          Delete(S, 1, I);
          I := Pos(' ', S);
          if (I <> 0) then
            SetLength(S, I - 1);
          CommandId := StrToIntDef(S, -1);
          if (CommandId >= 0) then
            FCommandMap.AddOrSetValue(CommandName, CommandId);
        end;
      end;
    end;
  finally
    CloseFile(HeaderFile);
  end;
end;

procedure TFormPreview.CommandCreated(const Sender: TUIRibbon;
  const Command: TUICommand);
var
  CmdRecentItems: TUICommandRecentItems absolute Command;
  CmdCollection: TUICommandCollection absolute Command;
  RecentItem: TUIRecentItem;
  CollItem: TUIGalleryCollectionItem;
  I: Integer;
begin
  case Command.CommandType of
    ctRecentItems:
      begin
        CmdRecentItems.Items.BeginUpdate;
        try
          for I := 0 to 19 do
          begin
            RecentItem := TUIRecentItem.Create;
            RecentItem.LabelText := RS_ITEM + ' ' + Char(Ord('A') + I);
            RecentItem.Description := RS_ITEM_DESC + ' ' + Char(Ord('A') + I);
            RecentItem.Pinned := False;
            CmdRecentItems.Items.Add(RecentItem);
          end;
        finally
          CmdRecentItems.Items.EndUpdate;
        end;
      end;

    ctCollection,
    ctCommandCollection:
      begin
        CmdCollection.Items.BeginUpdate;
        try
          for I := 0 to 11 do
          begin
            CollItem := TUIGalleryCollectionItem.Create;

            { For Item galleries }
            CollItem.LabelText := RS_ITEM + ' ' + Char(Ord('A') + I);
            CollItem.Image := FGalleryImage;

            { For Command galleries }
            CollItem.Command := FGalleryCommand;

            CmdCollection.Items.Add(CollItem);
          end;
        finally
          CmdCollection.Items.EndUpdate;
        end;
      end;
  end;
end;

function TFormPreview.RibbonInstance: THandle;
begin
  Result := FInstance;
end;

end.
