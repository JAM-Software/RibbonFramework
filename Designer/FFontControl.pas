unit FFontControl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FFloatieFontControl, StdCtrls, ComCtrls, pngimage, ExtCtrls,
  RibbonMarkup;

type
  TFrameFontControl = class(TFrameFloatieFontControl)
    Label2: TLabel;
    ComboBoxFontType: TComboBox;
    CheckBoxStrikethrough: TCheckBox;
    CheckBoxUnderline: TCheckBox;
    CheckBoxHighlight: TCheckBox;
    procedure ComboBoxFontTypeChange(Sender: TObject);
    procedure CheckBoxStrikethroughClick(Sender: TObject);
    procedure CheckBoxUnderlineClick(Sender: TObject);
    procedure CheckBoxHighlightClick(Sender: TObject);
  private
    { Private declarations }
    FFontControl: TRibbonFontControl;
    procedure UpdateControls;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

{ TFrameFontControl }

procedure TFrameFontControl.CheckBoxHighlightClick(Sender: TObject);
begin
  if (CheckBoxHighlight.Checked <> FFontControl.IsHighlightButtonVisible) then
  begin
    FFontControl.IsHighlightButtonVisible := CheckBoxHighlight.Checked;
    Modified;
  end;
end;

procedure TFrameFontControl.CheckBoxStrikethroughClick(Sender: TObject);
begin
  if (CheckBoxStrikethrough.Checked <> FFontControl.IsStrikethroughButtonVisible) then
  begin
    FFontControl.IsStrikethroughButtonVisible := CheckBoxStrikethrough.Checked;
    Modified;
  end;
end;

procedure TFrameFontControl.CheckBoxUnderlineClick(Sender: TObject);
begin
  if (CheckBoxUnderline.Checked <> FFontControl.IsUnderlineButtonVisible) then
  begin
    FFontControl.IsUnderlineButtonVisible := CheckBoxUnderline.Checked;
    Modified;
  end;
end;

procedure TFrameFontControl.ComboBoxFontTypeChange(Sender: TObject);
begin
  if (ComboBoxFontType.ItemIndex <> Ord(FFontControl.FontType)) then
  begin
    FFontControl.FontType := TRibbonFontType(ComboBoxFontType.ItemIndex);
    UpdateControls;
    Modified;
  end;
end;

constructor TFrameFontControl.Create(AOwner: TComponent);
begin
  inherited;
  EditMinFontSize.OnChange := EditMinFontSizeChange;
  EditMaxFontSize.OnChange := EditMaxFontSizeChange;
  CheckBoxTrueTypeOnly.OnClick := CheckBoxTrueTypeOnlyClick;
  CheckBoxVerticalFonts.OnClick := CheckBoxVerticalFontsClick;
end;

procedure TFrameFontControl.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FFontControl := Subject as TRibbonFontControl;
  ComboBoxFontType.ItemIndex := Ord(FFontControl.FontType);
  UpdateControls;
end;

procedure TFrameFontControl.UpdateControls;
begin
  if (FFontControl.FontType = ftRichFont) then
  begin
    CheckBoxStrikethrough.Checked := True;
    CheckBoxUnderline.Checked := True;
    CheckBoxHighlight.Checked := True;
    CheckBoxStrikethrough.Enabled := False;
    CheckBoxUnderline.Enabled := False;
    CheckBoxHighlight.Enabled := False;
  end
  else
  begin
    CheckBoxStrikethrough.Enabled := True;
    CheckBoxUnderline.Enabled := True;
    CheckBoxHighlight.Enabled := True;
    CheckBoxStrikethrough.Checked := FFontControl.IsStrikethroughButtonVisible;
    CheckBoxUnderline.Checked := FFontControl.IsUnderlineButtonVisible;
    CheckBoxHighlight.Checked := FFontControl.IsHighlightButtonVisible;
  end;
end;

end.
