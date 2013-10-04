unit FGroup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FControl, StdCtrls, ExtCtrls, pngimage, RibbonMarkup;

type
  TFrameGroup = class(TFrameControl)
    Label2: TLabel;
    ComboBoxSizeDefinition: TComboBox;
    LabelCustomSizeDefinition: TLabel;
    ComboBoxCustomSizeDefinition: TComboBox;
    procedure ComboBoxSizeDefinitionChange(Sender: TObject);
    procedure ComboBoxCustomSizeDefinitionChange(Sender: TObject);
  private
    { Private declarations }
    FGroup: TRibbonGroup;
    procedure UpdateControls;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  FViews;

{ TFrameGroup }

procedure TFrameGroup.ComboBoxCustomSizeDefinitionChange(Sender: TObject);
begin
  inherited;
  if (ComboBoxCustomSizeDefinition.ItemIndex >= 0) then
  begin
    FGroup.CustomSizeDefinition := ComboBoxCustomSizeDefinition.Text;
    Modified;
  end;
end;

procedure TFrameGroup.ComboBoxSizeDefinitionChange(Sender: TObject);
begin
  if (ComboBoxSizeDefinition.ItemIndex <> Ord(FGroup.BasicSizeDefinition)) then
  begin
    FGroup.BasicSizeDefinition := TRibbonBasicSizeDefinition(ComboBoxSizeDefinition.ItemIndex);
    UpdateControls;
    Modified;
  end;
end;

procedure TFrameGroup.Initialize(const Subject: TRibbonObject);
var
  Ribbon: TRibbonViewRibbon;
  SizeDef: TRibbonRibbonSizeDefinition;
begin
  inherited;
  FGroup := Subject as TRibbonGroup;
  ComboBoxSizeDefinition.ItemIndex := Ord(FGroup.BasicSizeDefinition);

  ComboBoxCustomSizeDefinition.Items.Clear;
  Ribbon := FGroup.Owner.Application.Ribbon;
  if Assigned(Ribbon) then
    for SizeDef in Ribbon.SizeDefinitions do
      ComboBoxCustomSizeDefinition.Items.AddObject(SizeDef.DisplayName, SizeDef);
  ComboBoxCustomSizeDefinition.ItemIndex :=
    ComboBoxCustomSizeDefinition.Items.IndexOf(FGroup.CustomSizeDefinition);

  UpdateControls;
end;

procedure TFrameGroup.UpdateControls;
var
  FrameViews: TFrameViews;

  procedure DeleteSizeDefNode;
  var
    I: Integer;
  begin
    for I := SubjectNode.Count - 1 downto 0 do
      if (SubjectNode.Item[I].ImageIndex = II_SIZE_DEF) then
      begin
        SubjectNode.Item[I].Delete;
        Break;
      end;
  end;

begin
  LabelCustomSizeDefinition.Enabled := (FGroup.BasicSizeDefinition = sdCustom)
    and (ComboBoxCustomSizeDefinition.Items.Count > 0);
  ComboBoxCustomSizeDefinition.Enabled := LabelCustomSizeDefinition.Enabled;
  if (not ComboBoxCustomSizeDefinition.Enabled) then
    ComboBoxCustomSizeDefinition.ItemIndex := -1;

  FrameViews := Owner as TFrameViews;
  if (FGroup.BasicSizeDefinition = sdAdvanced) then
  begin
    if (FGroup.SizeDefinition = nil) then
    begin
      FGroup.CreateAdvancedSizeDefinition;
      DeleteSizeDefNode;
      FrameViews.AddSizeDefinition(SubjectNode, FGroup.SizeDefinition);
    end;
  end
  else
  begin
    if (FGroup.SizeDefinition <> nil) then
    begin
      FGroup.DeleteAdvancedSizeDefinition;
      DeleteSizeDefNode;
    end;
  end;
end;

end.
