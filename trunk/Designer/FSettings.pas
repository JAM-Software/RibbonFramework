unit FSettings;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ImgList,
  ExtCtrls,
  Settings;

type
  TFormSettings = class(TForm)
    GroupBoxPaths: TGroupBox;
    Label1: TLabel;
    EditRibbonCompiler: TButtonedEdit;
    Label2: TLabel;
    EditResourceCompiler: TButtonedEdit;
    EditDelphiCompiler: TButtonedEdit;
    Label3: TLabel;
    ImageList: TImageList;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    OpenDialog: TOpenDialog;
    procedure EditPathChange(Sender: TObject);
    procedure EditRibbonCompilerRightButtonClick(Sender: TObject);
    procedure EditResourceCompilerRightButtonClick(Sender: TObject);
    procedure EditDelphiCompilerRightButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FSettings: TSettings;
    procedure UpdateControls;
  public
    { Public declarations }
    constructor Create(const Settings: TSettings); reintroduce;
  end;

implementation

{$R *.dfm}

uses
  IOUtils;

{ TFormSettings }

constructor TFormSettings.Create(const Settings: TSettings);
begin
  inherited Create(nil);
  FSettings := Settings;
  EditRibbonCompiler.Text := FSettings.RibbonCompilerPath;
  EditResourceCompiler.Text := FSettings.ResourceCompilerPath;
  EditDelphiCompiler.Text := FSettings.DelphiCompilerPath;
  UpdateControls;
end;

procedure TFormSettings.EditDelphiCompilerRightButtonClick(Sender: TObject);
begin
  OpenDialog.Filter := 'DCC32.exe|DCC32.exe';
  OpenDialog.InitialDir := ExtractFilePath(EditDelphiCompiler.Text);
  OpenDialog.FileName := ExtractFileName(EditDelphiCompiler.Text);
  if (OpenDialog.Execute) then
    EditDelphiCompiler.Text := OpenDialog.FileName;
end;

procedure TFormSettings.EditPathChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TFormSettings.EditResourceCompilerRightButtonClick(Sender: TObject);
begin
  OpenDialog.Filter := 'RC.exe|RC.exe';
  OpenDialog.InitialDir := ExtractFilePath(EditResourceCompiler.Text);
  OpenDialog.FileName := ExtractFileName(EditResourceCompiler.Text);
  if (OpenDialog.Execute) then
    EditResourceCompiler.Text := OpenDialog.FileName;
end;

procedure TFormSettings.EditRibbonCompilerRightButtonClick(Sender: TObject);
begin
  OpenDialog.Filter := 'UICC.exe|UICC.exe';
  OpenDialog.InitialDir := ExtractFilePath(EditRibbonCompiler.Text);
  OpenDialog.FileName := ExtractFileName(EditRibbonCompiler.Text);
  if (OpenDialog.Execute) then
    EditRibbonCompiler.Text := OpenDialog.FileName;
end;

procedure TFormSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (ModalResult = mrOk) then
  begin
    FSettings.RibbonCompilerPath := EditRibbonCompiler.Text;
    FSettings.ResourceCompilerPath := EditResourceCompiler.Text;
    FSettings.DelphiCompilerPath := EditDelphiCompiler.Text;
    FSettings.Save;
  end;
end;

procedure TFormSettings.UpdateControls;
begin
  ButtonOk.Enabled :=
    TFile.Exists(EditRibbonCompiler.Text) and
    TFile.Exists(EditResourceCompiler.Text) and
    TFile.Exists(EditDelphiCompiler.Text);
end;

end.
