unit UIRibbon.Register;

interface

uses
  DesignIntf,
  DesignEditors;

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
  Winapi.ShellAPI, Winapi.Windows, UIRibbonUtils, ToolsApi, Vcl.Dialogs,
  System.IOUtils;

procedure Register;
var
  lDesignerPath: string;
begin
  RegisterComponents('Windows Ribbon Framework for Delphi', [TUIRibbon]);
  RegisterActions(cPackageTitle, [TRibbonCollectionAction, TRibbonPopupMenuAction, TRibbonFontAction, TRibbonColorAction, TRecentItemAction], nil);

  lDesignerPath := TMyRibbonFrameworkEditor.GetDesignerPath();
  if lDesignerPath.IsEmpty and (getActiveProject <> nil) then begin
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


/// Shows a file open dialog that allows to browse for an XML file.
function BrowseForXmlFile(): string;
begin
  With TOpenDialog.Create(nil) do
    try
      Filter := 'XML files|*.xml';
      InitialDir := ExtractFilePath( getActiveProject.FileName );
      if not Execute then
        Exit;
      Result := FileName;
    finally
      Free;
    end;
  // Make the path relative tothe project, so the the settings is portable
  Result :=  ExtractRelativePath(getActiveProject.FileName, Result);
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
var
  lRibbon: TUIRibbon;
begin
  inherited;
  lRibbon := (Self.Component as TUiRibbon);
  if lRibbon.RibbonSourceFile.IsEmpty or
     not FileExists(TPath.Combine(ExtractFilePath(getActiveProject.FileName), lRibbon.RibbonSourceFile))
  then
    lRibbon.RibbonSourceFile := BrowseForXmlFile();
  case pIndex of
    0: ShellExecute(0, 'open', PChar(GetDesignerPath()), PChar('"'+ TPath.Combine(ExtractFilePath(getActiveProject.FileName), lRibbon.RibbonSourceFile) +'"'), nil, SW_SHOWNORMAL);
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
