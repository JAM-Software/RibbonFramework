unit UIRibbonForm;

interface

uses
  Windows,
  Messages,
  Classes,
  Forms,
  Controls,
  UIRibbon,
  UIRibbonCommands;

const
  WM_INVALIDATE_FRAME = WM_USER + 231;

type
  TUIRibbonForm = class(TForm)
  {$REGION 'Internal Declarations'}
  strict private
    FRibbon: TUIRibbon;
  private
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure WMInvalidateFrame(var Message: TMessage); message WM_INVALIDATE_FRAME;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
  {$ENDREGION 'Internal Declarations'}
  strict protected
    { This method is called just after a command for the ribbon is created.
      You can override this method to initialize the command right after
      construction. Note that commands are created at different times. On
      startup, all commands that are visible on the ribbon are created. However,
      commands that are part of the application menu or a drop-down are only
      created when they are needed (eg. when the application menu is opened.) }
    procedure CommandCreated(const Sender: TUIRibbon;
      const Command: TUICommand); virtual;

    { Is called after the ribbon has been loaded. At this point, you can safely
      access the Ribbon property and its properties. For example, you can
      override this method to access the commands that are loaded by the ribbon. }
    procedure RibbonLoaded; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function IsShortCut(var Message: TWMKey): Boolean; override;

    { Returns the name of the ribbon resource. By default, this is 'APPLICATION',
      which is the name the Ribbon Compiler uses when no name is specified.
      If you specified a name to the Ribbon Compiler (using then /name switch),
      then you need to override this function and return that name.
      Specifying an incorrect name will cause an exception on startup. }
    class function RibbonResourceName: String; virtual;

    { Returns the instance of the module to load the ribbon form. You only need
      to override this if you compiled the ribbon into a separate DLL. In that
      case, you need to return the DLL instance, and you MUST make sure that
      this handle is valid as long as the form is used (so do not call
      FreeLibrary until you are done with the form). }
    function RibbonInstance: THandle; virtual;

    { The ribbon hosted on this form }
    property Ribbon: TUIRibbon read FRibbon;
  end;

implementation

uses
  Menus;

{ TUIRibbonForm }

procedure TUIRibbonForm.AdjustClientRect(var Rect: TRect);
begin
  inherited;
  if Assigned(FRibbon) then
    Inc(Rect.Top, FRibbon.Height);
end;

procedure TUIRibbonForm.AfterConstruction;
begin
  inherited;
  FRibbon.Load(RibbonResourceName, RibbonInstance);
  RibbonLoaded;
end;

procedure TUIRibbonForm.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if Visible then
    PostMessage(Handle, WM_INVALIDATE_FRAME, 0, 0);
end;

procedure TUIRibbonForm.CommandCreated(const Sender: TUIRibbon;
  const Command: TUICommand);
begin
  { No default implementation }
end;

constructor TUIRibbonForm.Create(AOwner: TComponent);
begin
  inherited;
  FRibbon := TUIRibbon.Create(Self, CommandCreated);
end;

destructor TUIRibbonForm.Destroy;
begin
  FRibbon.Free;
  inherited;
end;

function TUIRibbonForm.IsShortCut(var Message: TWMKey): Boolean;
var
  ShiftState: TShiftState;
  ShortCut: TShortCut;
begin
  Result := inherited IsShortCut(Message);
  if (not Result) then
  begin
    ShiftState := KeyDataToShiftState(Message.KeyData);
    ShortCut := Menus.ShortCut(Message.CharCode, ShiftState);
    if (ShortCut <> scNone) then
      Result := Ribbon.HandleShortCut(ShortCut);
  end;
end;

function TUIRibbonForm.RibbonInstance: THandle;
begin
  Result := 0;
end;

procedure TUIRibbonForm.RibbonLoaded;
begin
  { No default implementation }
end;

class function TUIRibbonForm.RibbonResourceName: String;
begin
  Result := 'APPLICATION';
end;

procedure TUIRibbonForm.WMInvalidateFrame(var Message: TMessage);
begin
  { Redraw frame to prevent black caption bar on Aero }
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or
    SWP_NOZORDER or SWP_NOACTIVATE or SWP_DRAWFRAME);
end;

end.
