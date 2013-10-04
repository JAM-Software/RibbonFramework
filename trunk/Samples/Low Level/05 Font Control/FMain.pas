unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActiveX, ComObj, UIRibbonApi, StdCtrls, ComCtrls, PropSys,
  uRichEditManager, WinApiEx, RibbonConst;

const
  WM_INVALIDATE_FRAME = WM_USER + 1;

type
  TFormMain = class(TForm, IUIApplication, IUICommandHandler)
    RichEdit: TRichEdit;
    procedure RichEditSelectionChange(Sender: TObject);
    procedure RichEditContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    { Private declarations }
    FFramework: IUIFramework;
    FRibbonHeight: Integer;
    FRichEditManager: TRichEditManager;
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
    procedure ShowMiniToolbar(const Pos: TPoint);
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
  FRichEditManager := TRichEditManager.Create(RichEdit);
  FFramework := CoUIRibbonFramework.Create;
  FFramework.Initialize(Handle, Self);
  FFramework.LoadUI(HInstance, 'APPLICATION_RIBBON');
end;

destructor TFormMain.Destroy;
begin
  if Assigned(FFramework) then
    FFramework.Destroy;
  FRichEditManager.Free;
  inherited;
end;

function TFormMain.Execute(CommandId: UInt32; Verb: _UIExecutionVerb;
  Key: PUIPropertyKey; CurrentValue: PPropVariant;
  CommandExecutionProperties: IUISimplePropertySet): HRESULT;
var
  VarChanges: TPropVariant;
  Unk: IUnknown;
  Changes, Values: IPropertyStore;
begin
  Result := E_NOTIMPL;
  if Assigned(Key) and (Key^ = UI_PKEY_FontProperties) then
  begin
    case Verb of
      UIExecutionVerbExecute:
        begin
          Result := E_POINTER;
          if Assigned(CommandExecutionProperties) then
          begin
            // Get the changed properties.
            Result := CommandExecutionProperties.GetValue(UI_PKEY_FontProperties_ChangedProperties, VarChanges);
            if Succeeded(Result) then
            begin
              Result := UIPropertyToInterface(UI_PKEY_FontProperties, VarChanges, Unk);
              if Succeeded(Result) and Supports(Unk, IPropertyStore, Changes) then
                // Using the changed properties, set the new font on the selection on RichEdit control.
                FRichEditManager.SetValues(Changes);
              PropVariantClear(VarChanges);
            end;
          end;
        end;

      UIExecutionVerbPreview:
        begin
          Result := E_POINTER;
          if Assigned(CommandExecutionProperties) then
          begin
            // Get the changed properties.
            Result := CommandExecutionProperties.GetValue(UI_PKEY_FontProperties_ChangedProperties, VarChanges);
            if Succeeded(Result) then
            begin
              Result := UIPropertyToInterface(UI_PKEY_FontProperties, VarChanges, Unk);
              if Succeeded(Result) and Supports(Unk, IPropertyStore, Changes) then
                // Using the changed properties, set the new font on the selection on RichEdit control.
                FRichEditManager.SetPreviewValues(Changes);
              PropVariantClear(VarChanges);
            end;
          end;
        end;

      UIExecutionVerbCancelPreview:
        begin
          Result := E_POINTER;
          if Assigned(CurrentValue) then
          begin
            Result := UIPropertyToInterface(UI_PKEY_FontProperties, CurrentValue^, Unk);
            if Succeeded(Result) and Supports(Unk, IPropertyStore, Values) then
              FRichEditManager.CancelPreview(Values);
          end;
        end;
    end;
  end;
end;

function TFormMain.OnCreateUICommand(CommandId: UInt32; TypeId: _UICommandType;
  out CommandHandler: IUICommandHandler): HRESULT;
begin
  CommandHandler := Self;
  Result := S_OK;
end;

function TFormMain.OnDestroyUICommand(CommandId: UInt32; TypeId: _UICommandType;
  const CommandHandler: IUICommandHandler): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TFormMain.OnViewChanged(ViewId: UInt32; TypeId: _UIViewType;
  const View: IInterface; Verb: _UIViewVerb; ReasonCode: Int32): HRESULT;
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

procedure TFormMain.RichEditContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  ShowMiniToolbar(MousePos);
end;

procedure TFormMain.RichEditSelectionChange(Sender: TObject);
begin
  if Assigned(FFramework) then
    FFramework.InvalidateUICommand(IDC_CMD_FONTCONTROL, [UIInvalidationsAllProperties], nil);
end;

procedure TFormMain.ShowMiniToolbar(const Pos: TPoint);
var
  P: TPoint;
  R: TRect;
  ContextualUI: IUIContextualUI;
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

  if Assigned(FFramework) then
  begin
    // Get the view for contextual menu, which only has a mini toolbar.
    ContextualUI := FFramework.GetView(IDC_CMD_CONTEXTMAP, IUIContextualUI) as IUIContextualUI;
    ContextualUI.ShowAtLocation(P.X, P.Y);
  end;
end;

function TFormMain.UpdateProperty(CommandId: UInt32; const Key: TUIPropertyKey;
  CurrentValue: PPropVariant; out NewValue: TPropVariant): HRESULT;
var
  Unk: IUnknown;
  PropertyStore: IPropertyStore;
begin
  Result := E_NOTIMPL;
  if (Key = UI_PKEY_FontProperties) and Assigned(CurrentValue) then
  begin
    Result := UIPropertyToInterface(UI_PKEY_FontProperties, CurrentValue^, Unk);
    if Supports(Unk, IPropertyStore, PropertyStore) then
    begin
      FRichEditManager.GetValues(PropertyStore);
      Result := UIInitPropertyFromInterface(UI_PKEY_FontProperties, PropertyStore, NewValue);
    end;
  end;
end;

procedure TFormMain.WMInvalidateFrame(var Message: TMessage);
begin
  { Redraw frame to prevent black caption bar on Aero }
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or
    SWP_NOZORDER or SWP_NOACTIVATE or SWP_DRAWFRAME);
end;

end.
