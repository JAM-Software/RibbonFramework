unit DMShared;

interface

uses
  SysUtils, Classes, ImgList, Controls, JvComputerInfoEx, JvComponentBase, JvAppStorage, JvAppIniStorage, System.ImageList;

type
  TDataModuleShared = class(TDataModule)
    ImageListAppModes: TImageList;
    JvAppIniFileStorage: TJvAppIniFileStorage;
    JvComputerInfoEx: TJvComputerInfoEx;
  private
    { Private declarations }
  public
    constructor Create(AOwner : TComponent); override;
  end;

var
  DataModuleShared: TDataModuleShared;

implementation
uses
  Vcl.Forms;

{$R *.dfm}

constructor TDataModuleShared.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  var IniFilePath := IncludeTrailingPathDelimiter(JvComputerInfoEx.Folders.AppData) + ChangeFileExt(ExtractFileName(Application.ExeName), '');
  ForceDirectories(IniFilePath);
  JvAppIniFileStorage.FileName := IncludeTrailingPathDelimiter(IniFilePath) + ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
end;

end.
