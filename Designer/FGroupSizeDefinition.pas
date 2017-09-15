unit FGroupSizeDefinition;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FBaseFrame, ExtCtrls, StdCtrls, RibbonMarkup;

type
  TFrameGroupSizeDefinition = class(TBaseFrame)
    LabelHeader: TLabel;
    Label1: TLabel;
    ComboBoxSize: TComboBox;
    procedure ComboBoxSizeChange(Sender: TObject);
  private
    { Private declarations }
    FSizeDef: TRibbonGroupSizeDefinition;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TBaseFrame1 }

procedure TFrameGroupSizeDefinition.ComboBoxSizeChange(Sender: TObject);
begin
  if (ComboBoxSize.ItemIndex <> Ord(FSizeDef.Size)) then
  begin
    FSizeDef.Size := TRibbonGroupSizeType(ComboBoxSize.ItemIndex);
    Modified;
  end;
end;

procedure TFrameGroupSizeDefinition.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FSizeDef := Subject as TRibbonGroupSizeDefinition;
  ComboBoxSize.ItemIndex := Ord(FSizeDef.Size);
end;

end.
