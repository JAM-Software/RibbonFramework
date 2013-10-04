unit FNewFile;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, ExtCtrls;

type
  TRibbonTemplate = (rtNone, rtWordPad);

type
  TFormNewFile = class(TForm)
    RadioGroupTemplate: TRadioGroup;
    GroupBoxPath: TGroupBox;
    Label1: TLabel;
    EditDirectory: TButtonedEdit;
    ImageList: TImageList;
    Label2: TLabel;
    EditFilename: TEdit;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    procedure EditDirectoryChange(Sender: TObject);
    procedure EditFilenameChange(Sender: TObject);
    procedure EditDirectoryRightButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateControls;
  public
    { Public declarations }
  end;

resourcestring
  RS_SELECT_DIR_CAPTION = 'Select or create directory for Ribbon Document';

function NewFileDialog(out Template: TRibbonTemplate; out Filename: String): Boolean;

implementation

{$R *.dfm}

{$WARN UNIT_PLATFORM OFF}

uses
  IOUtils,
  FileCtrl;

function NewFileDialog(out Template: TRibbonTemplate; out Filename: String): Boolean;
var
  Form: TFormNewFile;
begin
  Template := rtNone;
  Filename := '';
  Form := TFormNewFile.Create(nil);
  try
    Result := (Form.ShowModal = mrOk);
    if (Result) then
    begin
      Template := TRibbonTemplate(Form.RadioGroupTemplate.ItemIndex);
      Filename := TPath.Combine(Form.EditDirectory.Text, Form.EditFilename.Text);
    end;
  finally
    Form.Release;
  end;
end;

{ TFormNewFile }

procedure TFormNewFile.EditDirectoryChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TFormNewFile.EditDirectoryRightButtonClick(Sender: TObject);
var
  Directory: String;
begin
  Directory := EditDirectory.Text;
  if (SelectDirectory(RS_SELECT_DIR_CAPTION, ExtractFilePath(Directory), Directory, [sdNewFolder, sdNewUI])) then
  begin
    EditDirectory.Text := Directory;
    UpdateControls;
  end;
end;

procedure TFormNewFile.EditFilenameChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TFormNewFile.UpdateControls;
begin
  ButtonOk.Enabled := TDirectory.Exists(EditDirectory.Text) and (EditFilename.Text <> '');
end;

end.
