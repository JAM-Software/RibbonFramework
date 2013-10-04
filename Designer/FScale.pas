unit FScale;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FCommandRefObject, StdCtrls, ExtCtrls, RibbonMarkup;

type
  TFrameScale = class(TFrameCommandRefObject)
    Label2: TLabel;
    ComboBoxSize: TComboBox;
    procedure ComboBoxCommandChange(Sender: TObject);
    procedure ComboBoxSizeChange(Sender: TObject);
  private
    { Private declarations }
    FScale: TRibbonScale;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFrameScale }

procedure TFrameScale.ComboBoxCommandChange(Sender: TObject);
var
  NewRef: TRibbonCommand;
begin
  if Assigned(FScale) then
  begin
    if (ComboBoxCommand.ItemIndex < 0) then
      NewRef := nil
    else
      NewRef := ComboBoxCommand.Items.Objects[ComboBoxCommand.ItemIndex] as TRibbonCommand;
    if (NewRef <> FScale.GroupRef) then
    begin
      FScale.GroupRef := NewRef;
      UpdateCurrentNode;
      Modified;
    end;
  end;
end;

procedure TFrameScale.ComboBoxSizeChange(Sender: TObject);
begin
  if (ComboBoxSize.ItemIndex <> Ord(FScale.Size)) then
  begin
    FScale.Size := TRibbonGroupLayout(ComboBoxSize.ItemIndex);
    Modified;
  end;
end;

procedure TFrameScale.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  ComboBoxCommand.OnChange := ComboBoxCommandChange;
  FScale := Subject as TRibbonScale;
  if (FScale.GroupRef = nil) then
    ComboBoxCommand.ItemIndex := 0
  else
    ComboBoxCommand.ItemIndex := ComboBoxCommand.Items.IndexOfObject(FScale.GroupRef);
  ComboBoxSize.ItemIndex := Ord(FScale.Size);
end;

end.
