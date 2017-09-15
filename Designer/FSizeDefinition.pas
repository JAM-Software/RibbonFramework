unit FSizeDefinition;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FBaseFrame, ExtCtrls, StdCtrls, RibbonMarkup;

type
  TFrameSizeDefinition = class(TBaseFrame)
    LabelHeader: TLabel;
    LabelName: TLabel;
    EditName: TEdit;
    Label1: TLabel;
    MemoControlNameMap: TMemo;
    procedure EditNameChange(Sender: TObject);
    procedure MemoControlNameMapChange(Sender: TObject);
    procedure MemoControlNameMapExit(Sender: TObject);
    procedure EditNameKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    FSizeDef: TRibbonSizeDefinition;
    FNamesChanged: Boolean;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFrameSizeDefinition }

procedure TFrameSizeDefinition.EditNameChange(Sender: TObject);
var
  Def: TRibbonRibbonSizeDefinition;
begin
  inherited;
  if (FSizeDef is TRibbonRibbonSizeDefinition) then
  begin
    Def := TRibbonRibbonSizeDefinition(FSizeDef);
    if (EditName.Text <> Def.Name) then
    begin
      Def.Name := EditName.Text;
      Modified;
    end;
  end;
end;

procedure TFrameSizeDefinition.EditNameKeyPress(Sender: TObject; var Key: Char);
var
  Edit: TEdit;
begin
  { Only allow valid Name/Symbol characters }
  Edit := Sender as TEdit;
  case Key of
    #8, // backspace
    'A'..'Z',
    'a'..'z',
    '_':
      { OK };

    '0'..'9':
      if (Edit.SelStart = 0) then
        Key := #0;
  else
    Key := #0;
  end;
end;

procedure TFrameSizeDefinition.Initialize(const Subject: TRibbonObject);
var
  S: String;
begin
  inherited;
  FSizeDef := Subject as TRibbonSizeDefinition;
  LabelName.Enabled := (FSizeDef is TRibbonRibbonSizeDefinition);
  EditName.Enabled := LabelName.Enabled;
  if (EditName.Enabled) then
    EditName.Text := TRibbonRibbonSizeDefinition(FSizeDef).Name
  else
    EditName.Text := '';

  MemoControlNameMap.Clear;
  for S in FSizeDef.ControlNameMap.ControlNameDefinitions do
    MemoControlNameMap.Lines.Add(S);

  FNamesChanged := False;
end;

procedure TFrameSizeDefinition.MemoControlNameMapChange(Sender: TObject);
begin
  inherited;
  FNamesChanged := True;
  Modified;
end;

procedure TFrameSizeDefinition.MemoControlNameMapExit(Sender: TObject);
var
  S, Name: String;
begin
  inherited;
  if FNamesChanged then
  begin
    FSizeDef.ControlNameMap.Clear;
    for S in MemoControlNameMap.Lines do
    begin
      Name := Trim(S);
      if (Name <> '') then
        FSizeDef.ControlNameMap.Add(Name);
    end;
  end;
end;

end.
