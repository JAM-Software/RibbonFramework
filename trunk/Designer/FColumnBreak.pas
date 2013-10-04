unit FColumnBreak;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FBaseFrame, ExtCtrls, StdCtrls, RibbonMarkup;

type
  TFrameColumnBreak = class(TBaseFrame)
    LabelHeader: TLabel;
    CheckBoxShowSeparator: TCheckBox;
    procedure CheckBoxShowSeparatorClick(Sender: TObject);
  private
    { Private declarations }
    FColumnBreak: TRibbonColumnBreak;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFrameColumnBreak }

procedure TFrameColumnBreak.CheckBoxShowSeparatorClick(Sender: TObject);
begin
  if (FColumnBreak.ShowSeparator <> CheckBoxShowSeparator.Checked) then
  begin
    FColumnBreak.ShowSeparator := CheckBoxShowSeparator.Checked;
    Modified;
  end;
end;

procedure TFrameColumnBreak.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FColumnBreak := Subject as TRibbonColumnBreak;
  CheckBoxShowSeparator.Checked := FColumnBreak.ShowSeparator;
end;

end.
