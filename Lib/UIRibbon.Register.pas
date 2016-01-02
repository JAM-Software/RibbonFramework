unit UIRibbon.Register;

interface

uses
  DesignIntf, DesignEditors;

type
  // Adds ability to start RibbonDesigner to right click menu of TUIRibbon control in IDE desginer
  TMyRibbonFrameworkEditor=class(TComponentEditor)
  public
    class function GetDesignerPath(): string;
    function GetVerbCount: Integer; override;
    function GetVerb(pIndex: Integer): string; override;
    procedure ExecuteVerb(pIndex: Integer); override;
  end;

procedure Register;

implementation

uses
  Classes, UIRibbon, SysUtils, UIRibbonActions, System.Actions, System.Win.Registry,
  Winapi.ShellAPI, Winapi.Windows, UIRibbonUtils, ToolsApi;

procedure Register;
var
  lDesignerPath: string;
begin
  RegisterComponents('Windows Ribbon Framework for Delphi', [TUIRibbon]);
  RegisterActions(cPackageTitle, [TRibbonCollectionAction, TRibbonFontAction, TRibbonColorAction], nil);

  lDesignerPath := TMyRibbonFrameworkEditor.GetDesignerPath();
  if lDesignerPath.IsEmpty then begin
    // Search designer based on project path
    lDesignerPath := ExtractFilePath(ExcludeTrailingPathDelimiter(ExtractFilePath(getActiveProject.fileName)));
    lDesignerPath := IncludeTrailingPathDelimiter(lDesignerPath) + 'Designer\Bin\RibbonDesigner.exe';
    if not FileExists(lDesignerPath) then
      lDesignerPath := ''
    else begin
      // Write the path to the Ribbon Designer to the registry, so that the designtime package knows where to find it
      With TRegistry.Create do
        try
          OpenKey(cRegistryPath, True);
          WriteString(cRegistryKeyDesigner, lDesignerPath);
        finally
          Free;
        end;
    end;
  end;

  if not lDesignerPath.IsEmpty then begin
    RegisterComponentEditor(TUIRibbon, TMyRibbonFrameworkEditor);
  end;//if lDesignerPath
end;

{ TMyRibbonFrameworkEditor }

class function TMyRibbonFrameworkEditor.GetDesignerPath: string;
begin
  With TRegistry.Create do
  try
    OpenKeyReadOnly(cRegistryPath);
    Result := ReadString(cRegistryKeyDesigner);
  finally
    Free;
  end;
end;

procedure TMyRibbonFrameworkEditor.ExecuteVerb(pIndex: Integer);
//var
//  lRibbon: TUIRibbon;
begin
  inherited;
//  lRibbon := (Self.Component as TUiRibbon);
  case pIndex of
    0: ShellExecute(0, 'open', PChar(GetDesignerPath()), nil, nil, SW_SHOWNORMAL);
  end;
end;

function TMyRibbonFrameworkEditor.GetVerb(pIndex: Integer): string;
begin
  case pIndex of
    0: Result := '&Ribbon Designer';
  end;
end;

function TMyRibbonFrameworkEditor.GetVerbCount: Integer;
begin
  Exit(1);
end;

end.
