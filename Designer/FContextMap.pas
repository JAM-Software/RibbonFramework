unit FContextMap;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FCommandRefObject, StdCtrls, ExtCtrls, pngimage, RibbonMarkup;

type
  TFrameContextMap = class(TFrameCommandRefObject)
    Label2: TLabel;
    ComboBoxMiniToolbar: TComboBox;
    Label3: TLabel;
    ComboBoxContextMenu: TComboBox;
    procedure ComboBoxMiniToolbarChange(Sender: TObject);
    procedure ComboBoxContextMenuChange(Sender: TObject);
  private
    { Private declarations }
    FContextMap: TRibbonContextMap;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

resourcestring
  RS_NONE = '(none)';

implementation

{$R *.dfm}

{ TFrameContextMap }

procedure TFrameContextMap.ComboBoxContextMenuChange(Sender: TObject);
var
  NewRef: TRibbonContextMenu;
begin
  if (ComboBoxContextMenu.ItemIndex <= 0) then
    NewRef := nil
  else
    NewRef := FContextMap.Owner.FindContextMenu(ComboBoxContextMenu.Text);
  if (NewRef <> FContextMap.ContextMenuRef) then
  begin
    FContextMap.ContextMenuRef := NewRef;
    Modified;
    UpdateCurrentNode;
  end;
end;

procedure TFrameContextMap.ComboBoxMiniToolbarChange(Sender: TObject);
var
  NewRef: TRibbonMiniToolbar;
begin
  if (ComboBoxMiniToolbar.ItemIndex <= 0) then
    NewRef := nil
  else
    NewRef := FContextMap.Owner.FindMiniToolbar(ComboBoxMiniToolbar.Text);
  if (NewRef <> FContextMap.MiniToolbarRef) then
  begin
    FContextMap.MiniToolbarRef := NewRef;
    Modified;
    UpdateCurrentNode;
  end;
end;

procedure TFrameContextMap.Initialize(const Subject: TRibbonObject);
var
  Toolbar: TRibbonMiniToolbar;
  Menu: TRibbonContextMenu;
begin
  inherited;
  FContextMap := Subject as TRibbonContextMap;

  ComboBoxMiniToolbar.Items.BeginUpdate;
  try
    ComboBoxMiniToolbar.Clear;
    ComboBoxMiniToolbar.Items.Add(RS_NONE);
    for Toolbar in FContextMap.ContextPopup.MiniToolbars do
      if (Toolbar.Name <> '') then
        ComboBoxMiniToolbar.Items.Add(Toolbar.Name);
  finally
    ComboBoxMiniToolbar.Items.EndUpdate;
  end;
  if Assigned(FContextMap.MiniToolbarRef) then
    ComboBoxMiniToolbar.ItemIndex := ComboBoxMiniToolbar.Items.IndexOf(FContextMap.MiniToolbarRef.Name);
  if (ComboBoxMiniToolbar.ItemIndex < 0) then
    ComboBoxMiniToolbar.ItemIndex := 0;

  ComboBoxContextMenu.Items.BeginUpdate;
  try
    ComboBoxContextMenu.Clear;
    ComboBoxContextMenu.Items.Add(RS_NONE);
    for Menu in FContextMap.ContextPopup.ContextMenus do
      if (Menu.Name <> '') then
        ComboBoxContextMenu.Items.Add(Menu.Name);
  finally
    ComboBoxContextMenu.Items.EndUpdate;
  end;
  if Assigned(FContextMap.ContextMenuRef) then
    ComboBoxContextMenu.ItemIndex := ComboBoxContextMenu.Items.IndexOf(FContextMap.ContextMenuRef.Name);
  if (ComboBoxContextMenu.ItemIndex < 0) then
    ComboBoxContextMenu.ItemIndex := 0;
end;

end.
