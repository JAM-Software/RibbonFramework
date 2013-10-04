unit FMenuGroup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FCommandRefObject, StdCtrls, ExtCtrls, RibbonMarkup, pngimage;

type
  TFrameMenuGroup = class(TFrameCommandRefObject)
    Label2: TLabel;
    ComboBoxCategory: TComboBox;
    procedure ComboBoxCategoryChange(Sender: TObject);
  private
    { Private declarations }
    FMenuGroup: TRibbonMenuGroup;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

var
  FrameMenuGroup: TFrameMenuGroup;

implementation

{$R *.dfm}

procedure TFrameMenuGroup.ComboBoxCategoryChange(Sender: TObject);
begin
  if (ComboBoxCategory.ItemIndex <> Ord(FMenuGroup.CategoryClass)) then
  begin
    FMenuGroup.CategoryClass := TRibbonMenuCategoryClass(ComboBoxCategory.ItemIndex);
    Modified;
  end;
end;

procedure TFrameMenuGroup.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FMenuGroup := Subject as TRibbonMenuGroup;
  ComboBoxCategory.ItemIndex := Ord(FMenuGroup.CategoryClass);
end;

end.
