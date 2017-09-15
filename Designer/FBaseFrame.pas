unit FBaseFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, RibbonMarkup, ExtCtrls, ComCtrls;

type
  TBaseFrame = class(TFrame)
    ImageSample: TImage;
    procedure FrameResize(Sender: TObject);
  strict private
    FSubject: TRibbonObject;
    FSubjectNode: TTreeNode;
    FUpdating: Boolean;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); virtual;
    procedure Modified;
    procedure UpdateCurrentNode;

    property SubjectNode: TTreeNode read FSubjectNode;
  public
    constructor Create(AOwner: TComponent); override;

    procedure ShowProperties(const Subject: TRibbonObject; const Node: TTreeNode);

    property Subject: TRibbonObject read FSubject;
  end;

implementation

{$R *.dfm}

uses
  FMain,
  FViews;

{ TFrameBase1 }

constructor TBaseFrame.Create(AOwner: TComponent);
begin
  inherited;
  Align := alClient;
  Visible := False;
end;

procedure TBaseFrame.FrameResize(Sender: TObject);
begin
  ImageSample.Left := Width - ImageSample.Width - 4;
end;

procedure TBaseFrame.Initialize(const Subject: TRibbonObject);
begin
  FSubject := Subject;
end;

procedure TBaseFrame.Modified;
begin
  if (not FUpdating) then begin
    FormMain.Modified;
    UpdateCurrentNode;
  end;
end;

procedure TBaseFrame.ShowProperties(const Subject: TRibbonObject;
  const Node: TTreeNode);
begin
  FUpdating := True;
  try
    FSubjectNode := Node;
    Initialize(Subject);
  finally
    FUpdating := False;
  end;
end;

procedure TBaseFrame.UpdateCurrentNode;
var
  FrameViews: TFrameViews;
begin
  FrameViews := Owner as TFrameViews;
  FrameViews.UpdateCurrentNode;
end;

end.
