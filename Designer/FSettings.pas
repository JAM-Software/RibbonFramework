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
    EditDelphiCompiler: TButtonedEdit;
    Label3: TLabel;
    ImageList: TImageList;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    OpenDialog: TOpenDialog;
    EditRibbonCompiler: TButtonedEdit;
    DownloadButton: TButton;
    procedure EditPathChange(Sender: TObject);
    procedure EditRibbonCompilerRightButtonClick(Sender: TObject);
    procedure EditDelphiCompilerRightButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DownloadButtonClick(Sender: TObject);
  private
    { Private declarations }
    FSettings: TSettings;
    procedure UpdateControls;
  public
    { Public declarations }
    constructor Create(const Settings: TSettings); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  ShellApi,
  IOUtils;

const
 cUiccDownloadUrl = 'http://www.microsoft.com/en-us/download/confirmation.aspx?id=8279';

{ TFormSettings }

procedure TFormSettings.DownloadButtonClick(Sender: TObject);
begin
  ShellExecute(0, nil, PChar(cUiccDownloadUrl), '', '', SW_SHOWNORMAL);
end;

constructor TFormSettings.Create(const Settings: TSettings);
begin
  inherited Create(nil);
  FSettings := Settings;
  Application.OnActivate := Self.EditPathChange; // Update controls when the form is activated
  UpdateControls;
end;

destructor TFormSettings.Destroy;
begin
  Application.OnActivate := nil;
  inherited;
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
    FSettings.DelphiCompilerPath := EditDelphiCompiler.Text;
    FSettings.Save;
  end;
end;

procedure TFormSettings.UpdateControls;
begin
  FSettings.DetectTools();
  if EditRibbonCompiler.Text = '' then
    EditRibbonCompiler.Text := FSettings.RibbonCompilerPath;
  if EditDelphiCompiler.Text = '' then
    EditDelphiCompiler.Text := FSettings.DelphiCompilerPath;
  ButtonOk.Enabled :=
    TFile.Exists(EditRibbonCompiler.Text) and
    TFile.Exists(EditDelphiCompiler.Text);
end;

end.
