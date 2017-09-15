unit FViewRibbon;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, FBaseFrame, RibbonMarkup, pngimage, ExtCtrls;

type
  TFrameViewRibbon = class(TBaseFrame)
    Label1: TLabel;
    EditName: TEdit;
    Label2: TLabel;
    LabelHeader: TLabel;
    ComboBoxGroupSpacing: TComboBox;
    procedure EditNameChange(Sender: TObject);
    procedure ComboBoxGroupSpacingChange(Sender: TObject);
  private
    { Private declarations }
    FRibbon: TRibbonViewRibbon;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFrameViewRibbon }

procedure TFrameViewRibbon.ComboBoxGroupSpacingChange(Sender: TObject);
begin
  if (ComboBoxGroupSpacing.ItemIndex <> Ord(FRibbon.GroupSpacing)) then
  begin
    FRibbon.GroupSpacing := TRibbonGroupSpacing(ComboBoxGroupSpacing.ItemIndex);
    Modified;
  end;
end;

procedure TFrameViewRibbon.EditNameChange(Sender: TObject);
begin
  if (EditName.Text <> FRibbon.Name) then
  begin
    FRibbon.Name := EditName.Text;
    Modified;
  end;
end;

procedure TFrameViewRibbon.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FRibbon := Subject as TRibbonViewRibbon;
  EditName.Text := FRibbon.Name;
  ComboBoxGroupSpacing.ItemIndex := Ord(FRibbon.GroupSpacing);
end;

end.
