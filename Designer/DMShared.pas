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
  JvAppIniFileStorage.FileName := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(JvComputerInfoEx.Folders.AppData)
                                + ExtractFileName(Application.ExeName)) + ChangeFileExt(Application.ExeName, '.ini');
end;

end.
