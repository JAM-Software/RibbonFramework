unit FMiniToolbar;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FBaseFrame, ExtCtrls, RibbonMarkup, StdCtrls, pngimage;

type
  TFrameMiniToolbar = class(TBaseFrame)
    LabelHeader: TLabel;
    Label1: TLabel;
    EditName: TEdit;
    procedure EditNameChange(Sender: TObject);
  private
    { Private declarations }
    FToolbar: TRibbonMiniToolbar;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFrameMiniToolbar }

procedure TFrameMiniToolbar.EditNameChange(Sender: TObject);
begin
  if (EditName.Text <> FToolbar.Name) then
  begin
    FToolbar.Name := EditName.Text;
    Modified;
  end;
end;

procedure TFrameMiniToolbar.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FToolbar := Subject as TRibbonMiniToolbar;
  EditName.Text := FToolbar.Name;
end;

end.
