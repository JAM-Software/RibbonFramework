unit FQuickAccessToolbar;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FCommandRefObject, StdCtrls, pngimage, ExtCtrls, RibbonMarkup;

type
  TFrameQuickAccessToolbar = class(TFrameCommandRefObject)
    Label2: TLabel;
    ComboBoxCustomizeCommand: TComboBox;
    procedure ComboBoxCustomizeCommandChange(Sender: TObject);
  private
    { Private declarations }
    FQuickAccessToolbar: TRibbonQuickAccessToolbar;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
    procedure Activate; override;
  end;

implementation

{$R *.dfm}

uses
  FViews;

{ TFrameQuickAccessToolbar }

procedure TFrameQuickAccessToolbar.Activate;
var
  FrameViews: TFrameViews;
  Cmd: TRibbonCommand;
begin
  inherited;
  FrameViews := Owner as TFrameViews;
  if (ComboBoxCustomizeCommand.ItemIndex >= 0) then
    Cmd := ComboBoxCustomizeCommand.Items.Objects[ComboBoxCustomizeCommand.ItemIndex] as TRibbonCommand
  else
    Cmd := nil;
  ComboBoxCustomizeCommand.Items := FrameViews.Commands;
  if (Cmd = nil) then
    ComboBoxCustomizeCommand.ItemIndex := 0
  else
  begin
    ComboBoxCustomizeCommand.ItemIndex := ComboBoxCustomizeCommand.Items.IndexOfObject(Cmd);
    if (ComboBoxCustomizeCommand.ItemIndex < 0) then
      ComboBoxCustomizeCommand.ItemIndex := 0;
  end;
end;

procedure TFrameQuickAccessToolbar.ComboBoxCustomizeCommandChange(
  Sender: TObject);
var
  NewRef: TRibbonCommand;
begin
  if (ComboBoxCustomizeCommand.ItemIndex < 0) then
    NewRef := nil
  else
    NewRef := ComboBoxCustomizeCommand.Items.Objects[ComboBoxCustomizeCommand.ItemIndex] as TRibbonCommand;
  if (NewRef <> FQuickAccessToolbar.CustomizeCommandRef) then
  begin
    FQuickAccessToolbar.CustomizeCommandRef := NewRef;
    Modified;
  end;
end;

procedure TFrameQuickAccessToolbar.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FQuickAccessToolbar := Subject as TRibbonQuickAccessToolbar;
  if (FQuickAccessToolbar.CustomizeCommandRef = nil) then
    ComboBoxCustomizeCommand.ItemIndex := 0
  else
    ComboBoxCustomizeCommand.ItemIndex := ComboBoxCustomizeCommand.Items.IndexOfObject(FQuickAccessToolbar.CustomizeCommandRef);
end;

end.
