unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, RichEdit, UIRibbonForm, UIRibbonCommands,
  uRichEditManager, RibbonConst, WinApiEx;

type
  TFormMain = class(TUIRibbonForm)
    RichEdit: TRichEdit;
    procedure RichEditSelectionChange(Sender: TObject);
    procedure RichEditContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    { Private declarations }
    FRichEditManager: TRichEditManager;
    FCmdFont: TUICommandFont;
  private
    procedure CmdFontChanged(const Args: TUICommandFontEventArgs);
    procedure ShowMiniToolbar(const Pos: TPoint);
  strict protected
    procedure RibbonLoaded; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

{ TFormMain }

procedure TFormMain.CmdFontChanged(const Args: TUICommandFontEventArgs);
var
  CharFormat: TCharFormat2;
begin
  Args.Font.AssignTo(CharFormat);
  FRichEditManager.SetValues(CharFormat);
end;

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  FRichEditManager := TRichEditManager.Create(RichEdit);
end;

destructor TFormMain.Destroy;
begin
  FRichEditManager.Free;
  inherited;
end;

procedure TFormMain.RibbonLoaded;
begin
  inherited;
  FCmdFont := Ribbon[IDC_CMD_FONTCONTROL] as TUICommandFont;
  FCmdFont.Font.Assign(FRichEditManager.GetValues);
  FCmdFont.OnChanged := CmdFontChanged;
end;

procedure TFormMain.RichEditContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  ShowMiniToolbar(MousePos);
end;

procedure TFormMain.RichEditSelectionChange(Sender: TObject);
begin
  FCmdFont.Font.Assign(FRichEditManager.GetValues);
end;

procedure TFormMain.ShowMiniToolbar(const Pos: TPoint);
var
  P: TPoint;
  R: TRect;
begin
  P := Pos;
  if (P.X = -1) and (P.Y = -1) then
  begin
    // Keyboard initiated the context menu.
    R := RichEdit.ClientRect;
    R.TopLeft := RichEdit.ClientToScreen(R.TopLeft);
    R.BottomRight := RichEdit.ClientToScreen(R.BottomRight);

    // Show the mini toolbar where the cursor is.
    GetCursorPos(P);
    if (not PtInRect(R, P)) then
      // The cursor is not in the RichEdit window so use top left corner of the RichEdit control.
      P := R.TopLeft;
  end
  else
    P := RichEdit.ClientToScreen(P);

  // If there's a selection in the RichEdit control then the selection will be lost
  // because of mouse click, so show selection again before showing the context menu.
  FRichEditManager.ShowSelection;

  Ribbon.ShowContextPopup(IDC_CMD_CONTEXTMAP, P);
end;

end.
