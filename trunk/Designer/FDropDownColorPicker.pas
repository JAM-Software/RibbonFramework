unit FDropDownColorPicker;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FCommandRefObject, StdCtrls, ExtCtrls, RibbonMarkup, pngimage,
  ComCtrls;

type
  TFrameDropDownColorPicker = class(TFrameCommandRefObject)
    Label2: TLabel;
    ComboBoxTemplate: TComboBox;
    ComboBoxChipSize: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    EditColumns: TEdit;
    UpDownColumns: TUpDown;
    Label5: TLabel;
    EditRecentRows: TEdit;
    UpDownRecentRows: TUpDown;
    Label6: TLabel;
    EditStandardRows: TEdit;
    UpDownStandardRows: TUpDown;
    Label7: TLabel;
    EditThemeRows: TEdit;
    UpDownThemeRows: TUpDown;
    CheckBoxAutoColor: TCheckBox;
    CheckBoxNoColor: TCheckBox;
    procedure ComboBoxTemplateChange(Sender: TObject);
    procedure ComboBoxChipSizeChange(Sender: TObject);
    procedure EditColumnsChange(Sender: TObject);
    procedure EditRecentRowsChange(Sender: TObject);
    procedure EditStandardRowsChange(Sender: TObject);
    procedure EditThemeRowsChange(Sender: TObject);
    procedure CheckBoxAutoColorClick(Sender: TObject);
    procedure CheckBoxNoColorClick(Sender: TObject);
  private
    { Private declarations }
    FColorPicker: TRibbonDropDownColorPicker;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFrameDropDownColorPicker }

procedure TFrameDropDownColorPicker.CheckBoxAutoColorClick(Sender: TObject);
begin
  if (CheckBoxAutoColor.Checked <> FColorPicker.IsAutomaticColorButtonVisible) then
  begin
    FColorPicker.IsAutomaticColorButtonVisible := CheckBoxAutoColor.Checked;
    Modified;
  end;
end;

procedure TFrameDropDownColorPicker.CheckBoxNoColorClick(Sender: TObject);
begin
  if (CheckBoxNoColor.Checked <> FColorPicker.IsNoColorButtonVisible) then
  begin
    FColorPicker.IsNoColorButtonVisible := CheckBoxNoColor.Checked;
    Modified;
  end;
end;

procedure TFrameDropDownColorPicker.ComboBoxChipSizeChange(Sender: TObject);
begin
  if (ComboBoxChipSize.ItemIndex <> Ord(FColorPicker.ChipSize)) then
  begin
    FColorPicker.ChipSize := TRibbonChipSize(ComboBoxChipSize.ItemIndex);
    Modified;
  end;
end;

procedure TFrameDropDownColorPicker.ComboBoxTemplateChange(Sender: TObject);
begin
  if (ComboBoxTemplate.ItemIndex <> Ord(FColorPicker.ColorTemplate)) then
  begin
    FColorPicker.ColorTemplate := TRibbonColorTemplate(ComboBoxTemplate.ItemIndex);
    Modified;
  end;
end;

procedure TFrameDropDownColorPicker.EditColumnsChange(Sender: TObject);
begin
  if (UpDownColumns.Position <> FColorPicker.Columns) then
  begin
    FColorPicker.Columns := UpDownColumns.Position;
    Modified;
  end;
end;

procedure TFrameDropDownColorPicker.EditRecentRowsChange(Sender: TObject);
begin
  if (UpDownRecentRows.Position <> FColorPicker.RecentColorGridRows) then
  begin
    FColorPicker.RecentColorGridRows := UpDownRecentRows.Position;
    Modified;
  end;
end;

procedure TFrameDropDownColorPicker.EditStandardRowsChange(Sender: TObject);
begin
  if (UpDownStandardRows.Position <> FColorPicker.StandardColorGridRows) then
  begin
    FColorPicker.StandardColorGridRows := UpDownStandardRows.Position;
    Modified;
  end;
end;

procedure TFrameDropDownColorPicker.EditThemeRowsChange(Sender: TObject);
begin
  if (UpDownThemeRows.Position <> FColorPicker.ThemeColorGridRows) then
  begin
    FColorPicker.ThemeColorGridRows := UpDownThemeRows.Position;
    Modified;
  end;
end;

procedure TFrameDropDownColorPicker.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FColorPicker := Subject as TRibbonDropDownColorPicker;
  ComboBoxTemplate.ItemIndex := Ord(FColorPicker.ColorTemplate);
  ComboBoxChipSize.ItemIndex := Ord(FColorPicker.ChipSize);
  UpDownColumns.Position := FColorPicker.Columns;
  UpDownRecentRows.Position := FColorPicker.RecentColorGridRows;
  UpDownStandardRows.Position := FColorPicker.StandardColorGridRows;
  UpDownThemeRows.Position := FColorPicker.ThemeColorGridRows;
  CheckBoxAutoColor.Checked := FColorPicker.IsAutomaticColorButtonVisible;
  CheckBoxNoColor.Checked := FColorPicker.IsNoColorButtonVisible;
end;

end.
