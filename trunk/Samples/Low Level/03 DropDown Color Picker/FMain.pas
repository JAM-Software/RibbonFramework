unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActiveX, ComObj, UIRibbonApi, uRenderer, uCommandHandler, RibbonConsts;

const
  WM_INVALIDATE_FRAME = WM_USER + 1;

type
  TFormMain = class(TForm, IUIApplication)
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FFramework: IUIFramework;
    FRibbonHeight: Integer;
    FButtonHandler: TButtonHandler;
    FColorPickerHandler: TColorPickerHandler;
  private
    { IUIApplication }
    function OnViewChanged(ViewId: UInt32; TypeId: _UIViewType;
      const View: IUnknown; Verb: _UIViewVerb; ReasonCode: Int32): HRESULT; stdcall;

    function OnCreateUICommand(CommandId: UInt32; TypeId: _UICommandType;
      out CommandHandler: IUICommandHandler): HRESULT; stdcall;

    function OnDestroyUICommand(CommandId: UInt32; TypeId: _UICommandType;
      const CommandHandler: IUICommandHandler): HRESULT; stdcall;
  protected
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure WMInvalidateFrame(var Message: TMessage); message WM_INVALIDATE_FRAME;
    procedure AdjustClientRect(var Rect: TRect); override;
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

procedure TFormMain.AdjustClientRect(var Rect: TRect);
begin
  inherited;
  Inc(Rect.Top, FRibbonHeight);
end;

procedure TFormMain.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if Visible then
    PostMessage(Handle, WM_INVALIDATE_FRAME, 0, 0);
end;

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  Renderer.Initialize(Self);
  FFramework := CoUIRibbonFramework.Create;
  FFramework.Initialize(Handle, Self);
  FFramework.LoadUI(HInstance, 'APPLICATION_RIBBON');
end;

destructor TFormMain.Destroy;
begin
  if Assigned(FFramework) then
    FFramework.Destroy;
  inherited;
end;

procedure TFormMain.FormPaint(Sender: TObject);
begin
  Renderer.Draw(Canvas, FRibbonHeight);
end;

procedure TFormMain.FormResize(Sender: TObject);
var
  R: TRect;
begin
  { Invalidate the client area below the ribbon }
  R := ClientRect;
  R.Top := FRibbonHeight;
  InvalidateRect(Handle, R, True);
end;

function TFormMain.OnCreateUICommand(CommandId: UInt32; TypeId: _UICommandType;
  out CommandHandler: IUICommandHandler): HRESULT;
{ PURPOSE: Called by the Ribbon framework for each command specified in markup,
  to allow the host application to bind a command handler to that command. }
begin
  Result := S_OK;
  case CommandId of
    IDR_CMD_UPDATE,
    IDR_CMD_CLEAR:
      begin
        if (FButtonHandler = nil) then
          FButtonHandler := TButtonHandler.Create(FFramework);
        CommandHandler := FButtonHandler;
      end;

    IDR_CMD_THEMEDDCP,
    IDR_CMD_STANDARDDDCP,
    IDR_CMD_HIGHLIGHTDDCP:
      begin
        if (FColorPickerHandler = nil) then
          FColorPickerHandler := TColorPickerHandler.Create;
        CommandHandler := FColorPickerHandler;
      end;
  end;
end;

function TFormMain.OnDestroyUICommand(CommandId: UInt32; TypeId: _UICommandType;
  const CommandHandler: IUICommandHandler): HRESULT;
{ PURPOSE: Called by the Ribbon framework for each command at the time of
  ribbon destruction. }
begin
  Result := E_NOTIMPL;
end;

function TFormMain.OnViewChanged(ViewId: UInt32; TypeId: _UIViewType;
  const View: IInterface; Verb: _UIViewVerb; ReasonCode: Int32): HRESULT;
{ PURPOSE: Called when the state of a View (Ribbon is a view) changes, for
  example, created, destroyed, or resized. }
var
  Ribbon: IUIRibbon;
begin
  Result := E_NOTIMPL;

  try
    // Checks to see if the view that was changed was a Ribbon view.
    if (TypeId = UIViewTypeRibbon) then
    begin
      case Verb of
        // The view was newly created.
        UIViewVerbCreate:
          Result := S_OK;

        // The view has been resized. For the Ribbon view, the application should
        // call GetHeight to determine the height of the ribbon.
        UIViewVerbSize:
          begin
            Ribbon := View as IUIRibbon;
            // Call to the framework to determine the desired height of the Ribbon.
            FRibbonHeight := Ribbon.GetHeight;
            // Realign controls to fit into the new client area
            Realign;
            Result := S_OK;
          end;

        // The view was destroyed.
        UIViewVerbDestroy:
          Result := S_OK;
      end;
    end;
  except
    on E: EOleSysError do
      Result := E.ErrorCode
    else
      Result := E_FAIL;
  end;
end;

procedure TFormMain.WMInvalidateFrame(var Message: TMessage);
begin
  { Redraw frame to prevent black caption bar on Aero }
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or
    SWP_NOZORDER or SWP_NOACTIVATE or SWP_DRAWFRAME);
end;

end.
