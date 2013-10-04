unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActiveX, ComObj, UIRibbonApi;

const
  WM_INVALIDATE_FRAME = WM_USER + 1;

type
  TFormMain = class(TForm, IUIApplication, IUICommandHandler)
    procedure FormContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    { Private declarations }
    FFramework: IUIFramework;
    FRibbonHeight: Integer;
    FCurrentContext: Integer;
  private
    { IUIApplication }
    function OnViewChanged(ViewId: UInt32; TypeId: _UIViewType;
      const View: IUnknown; Verb: _UIViewVerb; ReasonCode: Int32): HRESULT; stdcall;

    function OnCreateUICommand(CommandId: UInt32; TypeId: _UICommandType;
      out CommandHandler: IUICommandHandler): HRESULT; stdcall;

    function OnDestroyUICommand(CommandId: UInt32; TypeId: _UICommandType;
      const CommandHandler: IUICommandHandler): HRESULT; stdcall;
  private
    { IUICommandHandler }
    function Execute(CommandId: UInt32; Verb: _UIExecutionVerb;
      Key: PUIPropertyKey; CurrentValue: PPropVariant;
      CommandExecutionProperties: IUISimplePropertySet): HRESULT; stdcall;

    function UpdateProperty(CommandId: UInt32; const Key: TUIPropertyKey;
      CurrentValue: PPropVariant; out NewValue: TPropVariant): HRESULT; stdcall;
  protected
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure WMInvalidateFrame(var Message: TMessage); message WM_INVALIDATE_FRAME;
    procedure AdjustClientRect(var Rect: TRect); override;
  private
    procedure ShowContextualUI(const Location: TPoint);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  ContextPopupConst;

function GetContextMapIDFromCommandID(const CommandId: Integer): Integer;
begin
  case CommandId of
    IDC_CMD_CONTEXT1:
      Result := IDC_CMD_CONTEXTMAP1;

    IDC_CMD_CONTEXT2:
      Result := IDC_CMD_CONTEXTMAP2;

    IDC_CMD_CONTEXT3:
      Result := IDC_CMD_CONTEXTMAP3;

    IDC_CMD_CONTEXT4:
      Result := IDC_CMD_CONTEXTMAP4;
  else
    Result := 0;
  end;
end;

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
  FCurrentContext := IDC_CMD_CONTEXTMAP1;
  FFramework := CoUIRibbonFramework.Create;
  FFramework.Initialize(Handle, Self);
  FFramework.LoadUI(HInstance, 'CONTEXTPOPUP_RIBBON');
end;

destructor TFormMain.Destroy;
begin
  if Assigned(FFramework) then
    FFramework.Destroy;
  inherited;
end;

function TFormMain.Execute(CommandId: UInt32; Verb: _UIExecutionVerb;
  Key: PUIPropertyKey; CurrentValue: PPropVariant;
  CommandExecutionProperties: IUISimplePropertySet): HRESULT;
{ PURPOSE: Called by the Ribbon framework when a command is executed by the
  user. For example, when a button is pressed. }
begin
  Result := S_OK;
  if Assigned(Key) and (Key^ = UI_PKEY_BooleanValue) then
  begin
    if (CommandId <> cmdToggleButton) then
    begin
      FCurrentContext := GetContextMapIDFromCommandID(CommandId);

      { We need to update the toggle state (boolean value) of the toggle buttons,
        But we just invalidate all things to keep it simple. }
      FFramework.InvalidateUICommand(UIAllCommands, [UIInvalidationsValue], nil);
    end;
  end;
end;

procedure TFormMain.FormContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  ShowContextualUI(MousePos);
end;

function TFormMain.OnCreateUICommand(CommandId: UInt32; TypeId: _UICommandType;
  out CommandHandler: IUICommandHandler): HRESULT;
{ PURPOSE: Called by the Ribbon framework for each command specified in markup,
  to allow the host application to bind a command handler to that command.

  COMMENTS:
  In this SimpleRibbon sample, the same command handler is returned for all
  commands specified in the SimpleRibbon.xml file. }
begin
  CommandHandler := Self;
  Result := S_OK;
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

procedure TFormMain.ShowContextualUI(const Location: TPoint);
var
  ContextualUI: IUIContextualUI;
  P: TPoint;
begin
  { The basic pattern of how to show Contextual UI in a specified location. }
  ContextualUI := FFramework.GetView(FCurrentContext, IUIContextualUI) as IUIContextualUI;
  if Assigned(ContextualUI) then
  begin
    P := ClientToScreen(Location);
    ContextualUI.ShowAtLocation(P.X, P.Y);
  end;
end;

function TFormMain.UpdateProperty(CommandId: UInt32; const Key: TUIPropertyKey;
  CurrentValue: PPropVariant; out NewValue: TPropVariant): HRESULT;
{ PURPOSE: Called by the Ribbon framework when a command property (PKEY) needs
  to be updated.

  COMMENTS:
  This function is used to provide new command property values, such as labels,
  icons, or tooltip information, when requested by the Ribbon framework.
  In this sample, this updates the toggle state of the toggle buttons in the
  ribbon. }
begin
  Result := E_NOTIMPL;
  if (Key = UI_PKEY_BooleanValue) then
  begin
    if (CommandId <> cmdToggleButton) then
      Result := UIInitPropertyFromBoolean(UI_PKEY_BooleanValue,
        (FCurrentContext = GetContextMapIDFromCommandID(CommandId)),
        NewValue)
  end;
end;

procedure TFormMain.WMInvalidateFrame(var Message: TMessage);
begin
  { Redraw frame to prevent black caption bar on Aero }
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or
    SWP_NOZORDER or SWP_NOACTIVATE or SWP_DRAWFRAME);
end;

end.
