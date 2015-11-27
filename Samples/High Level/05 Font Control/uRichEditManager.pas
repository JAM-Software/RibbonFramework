unit uRichEditManager;

interface

uses
  ComCtrls,
  RichEdit;

type
  TRichEditManager = class
  strict private
    FRichEdit: TRichEdit;
  public
    constructor Create(const RichEdit: TRichEdit);
    function GetValues: TCharFormat2;
    procedure SetValues(const CharFormat: TCharFormat2);
    procedure ShowSelection;
  end;

implementation

uses
  Messages;

{ TRichEditManager }

constructor TRichEditManager.Create(const RichEdit: TRichEdit);
begin
  inherited Create;
  FRichEdit := RichEdit;
  FRichEdit.Font.Name := 'Arial';
  FRichEdit.Font.Size := 24;
end;

function TRichEditManager.GetValues: TCharFormat2;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.cbSize := SizeOf(Result);
  SendStructMessage(FRichEdit.Handle, EM_GETCHARFORMAT, SCF_SELECTION, Result);
end;

procedure TRichEditManager.SetValues(const CharFormat: TCharFormat2);
begin
  SendStructMessage(FRichEdit.Handle, EM_SETCHARFORMAT, SCF_SELECTION, CharFormat);
end;

procedure TRichEditManager.ShowSelection;
begin
  FRichEdit.HideSelection := False;
end;

end.
