unit FContextMenu;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FBaseFrame, ExtCtrls, RibbonMarkup, StdCtrls, pngimage;

type
  TFrameContextMenu = class(TBaseFrame)
    LabelHeader: TLabel;
    Label1: TLabel;
    EditName: TEdit;
    procedure EditNameChange(Sender: TObject);
  private
    { Private declarations }
    FContextMenu: TRibbonContextMenu;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFrameContextMenu }

procedure TFrameContextMenu.EditNameChange(Sender: TObject);
begin
  if (EditName.Text <> FContextMenu.Name) then
  begin
    FContextMenu.Name := EditName.Text;
    Modified;
  end;
end;

procedure TFrameContextMenu.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FContextMenu := Subject as TRibbonContextMenu;
  EditName.Text := FContextMenu.Name;
end;

end.
