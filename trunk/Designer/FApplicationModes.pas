unit FApplicationModes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst;

type
  TFormApplicationModes = class(TForm)
    Label1: TLabel;
    CheckListBoxModes: TCheckListBox;
    Label2: TLabel;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    ButtonCheckAll: TButton;
    ButtonClearAll: TButton;
    procedure ButtonCheckAllClick(Sender: TObject);
    procedure ButtonClearAllClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FAppModes: Cardinal;
  public
    { Public declarations }
    constructor Create(const AppModes: Cardinal); reintroduce;

    property AppModes: Cardinal read FAppModes write FAppModes;
  end;

implementation

{$R *.dfm}

{ TFormApplicationModes }

procedure TFormApplicationModes.ButtonCheckAllClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to 31 do
    CheckListBoxModes.Checked[I] := True;
end;

procedure TFormApplicationModes.ButtonClearAllClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to 31 do
    CheckListBoxModes.Checked[I] := False;
end;

constructor TFormApplicationModes.Create(const AppModes: Cardinal);
var
  I: Integer;
begin
  inherited Create(nil);
  FAppModes := AppModes;
  for I := 0 to 31 do
    CheckListBoxModes.Checked[I] := ((FAppModes and (1 shl I)) <> 0);
end;

procedure TFormApplicationModes.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  I: Integer;
begin
  if (ModalResult = mrOk) then
  begin
    FAppModes := 0;
    for I := 0 to 31 do
      if CheckListBoxModes.Checked[I] then
        FAppModes := FAppModes or (1 shl I);
    if (FAppModes = 0) then
      FAppModes := $FFFFFFFF;
  end;
end;

end.
