unit FFloatieFontControl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FCommandRefObject, StdCtrls, ExtCtrls, pngimage, RibbonMarkup,
  ComCtrls;

type
  TFrameFloatieFontControl = class(TFrameCommandRefObject)
    Label3: TLabel;
    Label5: TLabel;
    EditMinFontSize: TEdit;
    EditMaxFontSize: TEdit;
    UpDownMinFontSize: TUpDown;
    UpDownMaxFontSize: TUpDown;
    CheckBoxTrueTypeOnly: TCheckBox;
    CheckBoxVerticalFonts: TCheckBox;
    procedure EditMinFontSizeChange(Sender: TObject);
    procedure EditMaxFontSizeChange(Sender: TObject);
    procedure CheckBoxTrueTypeOnlyClick(Sender: TObject);
    procedure CheckBoxVerticalFontsClick(Sender: TObject);
  private
    { Private declarations }
    FFloatControl: TRibbonFloatieFontControl;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFrameFloatieFontControl }

procedure TFrameFloatieFontControl.CheckBoxTrueTypeOnlyClick(Sender: TObject);
begin
  if (CheckBoxTrueTypeOnly.Checked <> FFloatControl.ShowTrueTypeOnly) then
  begin
    FFloatControl.ShowTrueTypeOnly := CheckBoxTrueTypeOnly.Checked;
    Modified;
  end;
end;

procedure TFrameFloatieFontControl.CheckBoxVerticalFontsClick(Sender: TObject);
begin
  if (CheckBoxVerticalFonts.Checked <> FFloatControl.ShowVerticalFonts) then
  begin
    FFloatControl.ShowVerticalFonts := CheckBoxVerticalFonts.Checked;
    Modified;
  end;
end;

procedure TFrameFloatieFontControl.EditMaxFontSizeChange(Sender: TObject);
begin
  if (UpDownMaxFontSize.Position <> FFloatControl.MaximumFontSize) then
  begin
    FFloatControl.MaximumFontSize := UpDownMaxFontSize.Position;
    Modified;
  end;
end;

procedure TFrameFloatieFontControl.EditMinFontSizeChange(Sender: TObject);
begin
  if (UpDownMinFontSize.Position <> FFloatControl.MinimumFontSize) then
  begin
    FFloatControl.MinimumFontSize := UpDownMinFontSize.Position;
    Modified;
  end;
end;

procedure TFrameFloatieFontControl.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FFloatControl := Subject as TRibbonFloatieFontControl;
  UpDownMinFontSize.Position := FFloatControl.MinimumFontSize;
  UpDownMaxFontSize.Position := FFloatControl.MaximumFontSize;
  CheckBoxTrueTypeOnly.Checked := FFloatControl.ShowTrueTypeOnly;
  CheckBoxVerticalFonts.Checked := FFloatControl.ShowVerticalFonts;
end;

end.
