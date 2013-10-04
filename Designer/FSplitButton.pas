unit FSplitButton;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FControl, StdCtrls, ExtCtrls, pngimage, RibbonMarkup, ComCtrls;

type
  TFrameSplitButton = class(TFrameControl)
    Label2: TLabel;
    ComboBoxButtonItemType: TComboBox;
    procedure ComboBoxButtonItemTypeChange(Sender: TObject);
  private
    { Private declarations }
    FButton: TRibbonSplitButton;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  FViews;

const // Button Item type
  BI_NONE          = 0;
  BI_BUTTON        = 1;
  BI_TOGGLE_BUTTON = 2;

{ TFrameSplitButton }

procedure TFrameSplitButton.ComboBoxButtonItemTypeChange(Sender: TObject);
var
  ItemNode: TTreeNode;
  FrameViews: TFrameViews;
begin
  Assert(SubjectNode.Count > 0);
  ItemNode := SubjectNode.Item[0];
  FrameViews := Owner as TFrameViews;
  case ComboBoxButtonItemType.ItemIndex of
    BI_NONE:
      begin
        if (FButton.ButtonItem <> nil) then
        begin
          FButton.DeleteButtonItem;
          ItemNode.DeleteChildren;
          Modified;
        end;
      end;

    BI_BUTTON:
      begin
        if (FButton.ButtonItem = nil) or (FButton.ButtonItem.ObjectType = otToggleButton) then
        begin
          FButton.CreateButtonItem;
          ItemNode.DeleteChildren;
          FrameViews.AddControl(ItemNode, FButton.ButtonItem);
          SubjectNode.Selected := True;
          Modified;
        end;
      end;

    BI_TOGGLE_BUTTON:
      begin
        if (FButton.ButtonItem = nil) or (FButton.ButtonItem.ObjectType = otButton) then
        begin
          FButton.CreateToggleButtonItem;
          ItemNode.DeleteChildren;
          FrameViews.AddControl(ItemNode, FButton.ButtonItem);
          SubjectNode.Selected := True;
          Modified;
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TFrameSplitButton.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FButton := Subject as TRibbonSplitButton;
  if (FButton.ButtonItem = nil) then
    ComboBoxButtonItemType.ItemIndex := BI_NONE
  else if (FButton.ButtonItem.ObjectType = otButton) then
    ComboBoxButtonItemType.ItemIndex := BI_BUTTON
  else
    ComboBoxButtonItemType.ItemIndex := BI_TOGGLE_BUTTON;
end;

end.
