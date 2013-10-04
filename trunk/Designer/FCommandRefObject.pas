unit FCommandRefObject;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, FBaseFrame, StdCtrls, RibbonMarkup, ExtCtrls;

type
  TFrameCommandRefObject = class(TBaseFrame)
    Label1: TLabel;
    LabelHeader: TLabel;
    ComboBoxCommand: TComboBox;
    procedure ComboBoxCommandChange(Sender: TObject);
  strict private
    FCommandRefSubject: TRibbonCommandRefObject;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
    property CommandRefSubject: TRibbonCommandRefObject read FCommandRefSubject;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Activate; virtual;
  end;

implementation

{$R *.dfm}

uses
  FViews;

{ TFrameCommandRefObject }

procedure TFrameCommandRefObject.Activate;
var
  FrameViews: TFrameViews;
  CurrentCmd: String;
begin
  FrameViews := Owner as TFrameViews;
  CurrentCmd := ComboBoxCommand.Text;
  ComboBoxCommand.Items := FrameViews.Commands;
  if (CurrentCmd = '') then
    ComboBoxCommand.ItemIndex := 0
  else
  begin
    ComboBoxCommand.ItemIndex := ComboBoxCommand.Items.IndexOf(CurrentCmd);
    if (ComboBoxCommand.ItemIndex < 0) then
      ComboBoxCommand.ItemIndex := 0;
  end;
end;

procedure TFrameCommandRefObject.ComboBoxCommandChange(Sender: TObject);
var
  NewRef: TRibbonCommand;
begin
  if Assigned(FCommandRefSubject) then
  begin
    if (ComboBoxCommand.ItemIndex < 0) then
      NewRef := nil
    else
      NewRef := ComboBoxCommand.Items.Objects[ComboBoxCommand.ItemIndex] as TRibbonCommand;
    if (NewRef <> FCommandRefSubject.CommandRef) then
    begin
      FCommandRefSubject.CommandRef := NewRef;
      UpdateCurrentNode;
      Modified;
    end;
  end;
end;

constructor TFrameCommandRefObject.Create(AOwner: TComponent);
begin
  inherited;
  ComboBoxCommand.OnChange := ComboBoxCommandChange;
end;

procedure TFrameCommandRefObject.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  if (Subject is TRibbonCommandRefObject) then
  begin
    FCommandRefSubject := Subject as TRibbonCommandRefObject;
    if (FCommandRefSubject.CommandRef = nil) then
      ComboBoxCommand.ItemIndex := 0
    else
      ComboBoxCommand.ItemIndex := ComboBoxCommand.Items.IndexOfObject(FCommandRefSubject.CommandRef);
  end;
end;

end.
