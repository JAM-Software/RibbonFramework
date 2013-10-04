unit FControlGroup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FBaseFrame, ExtCtrls, StdCtrls, pngimage, ComCtrls, RibbonMarkup;

type
  TFrameControlGroup = class(TBaseFrame)
    LabelHeader: TLabel;
    Label3: TLabel;
    EditSequence: TEdit;
    UpDownSequence: TUpDown;
    Label1: TLabel;
    procedure EditSequenceChange(Sender: TObject);
  private
    { Private declarations }
    FGroup: TRibbonControlGroup;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFrameControlGroup }

procedure TFrameControlGroup.EditSequenceChange(Sender: TObject);
begin
  if (UpDownSequence.Position <> FGroup.SequenceNumber) then
  begin
    FGroup.SequenceNumber := UpDownSequence.Position;
    Modified;
  end;
end;

procedure TFrameControlGroup.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FGroup := Subject as TRibbonControlGroup;
  UpDownSequence.Position := FGroup.SequenceNumber;
end;

end.
