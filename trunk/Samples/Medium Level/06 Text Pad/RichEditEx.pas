unit RichEditEx;

interface

uses
  Windows,
  Messages,
  ComCtrls,
  CommDlg,
  RichEdit;

type
  TRichEditEx = class
  {$REGION 'Internal Declarations'}
  strict private
    FRichEdit: TRichEdit;
    function GetCharFormat: TCharFormat2;
    procedure SetCharFormat(const Value: TCharFormat2);
    function GetParaFormat: TParaFormat2;
    procedure SetParaFormat(const Value: TParaFormat2);
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const RichEdit: TRichEdit);

    function CanPaste: Boolean;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    procedure Undo;
    procedure Redo;

    { Similar to TRichEdit.SelAttributes, but provides more properties, better
      suited for use with the Ribbon Font Control. }
    property CharFormat: TCharFormat2 read GetCharFormat write SetCharFormat;

    { Similar to TRichEdit.Paragraph, but provides more properties. }
    property ParaFormat: TParaFormat2 read GetParaFormat write SetParaFormat;
  end;

implementation

{ TRichEditEx }

function TRichEditEx.CanPaste: Boolean;
begin
  Result := (SendMessage(FRichEdit.Handle, EM_CANPASTE, 0, 0) <> 0);
end;

function TRichEditEx.CanRedo: Boolean;
begin
  Result := (SendMessage(FRichEdit.Handle, EM_CANREDO, 0, 0) <> 0);
end;

function TRichEditEx.CanUndo: Boolean;
begin
  Result := (SendMessage(FRichEdit.Handle, EM_CANUNDO, 0, 0) <> 0);
end;

constructor TRichEditEx.Create(const RichEdit: TRichEdit);
begin
  inherited Create;
  FRichEdit := RichEdit;
end;

function TRichEditEx.GetCharFormat: TCharFormat2;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.cbSize := SizeOf(Result);
  SendStructMessage(FRichEdit.Handle, EM_GETCHARFORMAT, SCF_SELECTION, Result);
end;

function TRichEditEx.GetParaFormat: TParaFormat2;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.cbSize := SizeOf(Result);
  SendStructMessage(FRichEdit.Handle, EM_GETPARAFORMAT, 0, Result);
end;

procedure TRichEditEx.Redo;
begin
  SendMessage(FRichEdit.Handle, EM_REDO, 0, 0)
end;

procedure TRichEditEx.SetCharFormat(const Value: TCharFormat2);
begin
  SendStructMessage(FRichEdit.Handle, EM_SETCHARFORMAT, SCF_SELECTION, Value);
end;

procedure TRichEditEx.SetParaFormat(const Value: TParaFormat2);
begin
  SendStructMessage(FRichEdit.Handle, EM_SETPARAFORMAT, 0, Value);
end;

procedure TRichEditEx.Undo;
begin
  SendMessage(FRichEdit.Handle, EM_UNDO, 0, 0)
end;

end.
