unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UIRibbonForm, UIRibbonCommands;

type
  TFormMain = class(TUIRibbonForm)
    procedure FormContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    { Private declarations }
    FCurrentContext: Integer;
    FCommandContexts: array [0..3] of TUICommandBoolean;
  private
    procedure ContextToggle(const Args: TUICommandBooleanEventArgs);
  strict protected
    procedure RibbonLoaded; override;
  public
    { Public declarations }
    class function RibbonResourceName: String; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  ContextPopupConst;

{ TFormMain }

procedure TFormMain.ContextToggle(const Args: TUICommandBooleanEventArgs);
var
  I: Integer;
begin
  FCurrentContext := Args.Command.Tag;

  { Make the Command button appear checked, and the other ones not. }
  for I := 0 to 3 do
    FCommandContexts[I].Checked := (FCommandContexts[I] = Args.Command);
end;

procedure TFormMain.FormContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  P: TPoint;
begin
  P := ClientToScreen(MousePos);
  Ribbon.ShowContextPopup(FCurrentContext, P);
end;

procedure TFormMain.RibbonLoaded;
const
  CONTEXTS: array [0..3] of Integer = (
    IDC_CMD_CONTEXT1, IDC_CMD_CONTEXT2, IDC_CMD_CONTEXT3, IDC_CMD_CONTEXT4);
  TAGS: array [0..3] of Integer = (
    IDC_CMD_CONTEXTMAP1, IDC_CMD_CONTEXTMAP2, IDC_CMD_CONTEXTMAP3, IDC_CMD_CONTEXTMAP4);
var
  I: Integer;
begin
  inherited;
  FCurrentContext := IDC_CMD_CONTEXTMAP1;
  for I := 0 to 3 do
  begin
    FCommandContexts[I] := Ribbon[CONTEXTS[I]] as TUICommandBoolean;
    FCommandContexts[I].Tag := TAGS[I];
    FCommandContexts[I].OnToggle := ContextToggle;
  end;

  { Check the first button }
  FCommandContexts[0].Checked := True;
end;

class function TFormMain.RibbonResourceName: String;
begin
  Result := 'CONTEXTPOPUP';
end;

end.
