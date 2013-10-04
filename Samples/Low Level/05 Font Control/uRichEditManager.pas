unit uRichEditManager;

interface

uses
  ComCtrls,
  PropSys;

type
  TRichEditManager = class
  strict private
    FRichEdit: TRichEdit;
  public
    constructor Create(const RichEdit: TRichEdit);
    procedure GetValues(const Props: IPropertyStore);
    procedure SetValues(const Props: IPropertyStore);
    procedure SetPreviewValues(const Props: IPropertyStore);
    procedure CancelPreview(const Props: IPropertyStore);
    procedure ShowSelection;
  end;

implementation

uses
  RichEdit,
  Messages,
  uPropertyStore;

{ TRichEditManager }

procedure TRichEditManager.CancelPreview(const Props: IPropertyStore);
begin
  SetPreviewValues(Props);
end;

constructor TRichEditManager.Create(const RichEdit: TRichEdit);
begin
  inherited Create;
  FRichEdit := RichEdit;
  FRichEdit.Font.Name := 'Arial';
  FRichEdit.Font.Size := 24;
end;

procedure TRichEditManager.GetValues(const Props: IPropertyStore);
var
  CharFormat: TCharFormat2;
begin
  FillChar(CharFormat, SizeOf(CharFormat), 0);
  CharFormat.cbSize := SizeOf(CharFormat);
  SendStructMessage(FRichEdit.Handle, EM_GETCHARFORMAT, SCF_SELECTION, CharFormat);
  GetIPropStoreFromCharFormat2(CharFormat, Props);
end;

procedure TRichEditManager.SetPreviewValues(const Props: IPropertyStore);
var
  CharFormat: TCharFormat2;
begin
  GetCharFormat2FromIPropStore(Props, CharFormat);
  SendStructMessage(FRichEdit.Handle, EM_SETCHARFORMAT, SCF_SELECTION, CharFormat);
end;

procedure TRichEditManager.SetValues(const Props: IPropertyStore);
var
  CharFormat: TCharFormat2;
begin
  GetCharFormat2FromIPropStore(Props, CharFormat);
  SendStructMessage(FRichEdit.Handle, EM_SETCHARFORMAT, SCF_SELECTION, CharFormat);
end;

procedure TRichEditManager.ShowSelection;
begin
  FRichEdit.HideSelection := False;
end;

end.
