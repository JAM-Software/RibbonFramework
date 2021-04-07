unit DMShared;

interface

uses
  SysUtils, Classes, ImgList, Controls;

type
  TDataModuleShared = class(TDataModule)
    ImageListAppModes: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModuleShared: TDataModuleShared;

implementation

{$R *.dfm}

end.
