unit FEditImage;

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
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  ImgList,
  RibbonMarkup,
  UIRibbonCommands,
  FImageList;

type
  TFormEditImage = class(TForm)
    PanelImage: TPanel;
    PaintBox: TPaintBox;
    MemoHelp: TMemo;
    PanelButtons: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    EditImageFile: TButtonedEdit;
    ImageList: TImageList;
    Label3: TLabel;
    ComboBoxMinDpi: TComboBox;
    Label6: TLabel;
    EditResourceId: TEdit;
    UpDownResourceId: TUpDown;
    Label7: TLabel;
    EditSymbol: TEdit;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    OpenDialog: TOpenDialog;
    procedure PaintBoxPaint(Sender: TObject);
    procedure EditImageFileChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EditImageFileRightButtonClick(Sender: TObject);
  private
    { Private declarations }
    FImage: TRibbonImage;
    FBitmap: TBitmap;
    FFilename: String;
    FFlags: TImageFlags;
    FInitialized: Boolean;
    procedure UpdateControls;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure ClearBitmap(const Bitmap: TBitmap);
  public
    { Public declarations }
    constructor Create(const Image: TRibbonImage; const Flags: TImageFlags); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  StrUtils,
  IOUtils,
  Math,
  FMain;

procedure TFormEditImage.ClearBitmap(const Bitmap: TBitmap);
begin
  Bitmap.Canvas.Brush.Color := clWhite;
  Bitmap.Canvas.FillRect(Rect(0,0,Bitmap.Width,Bitmap.Height));
end;

procedure TFormEditImage.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Showing and (not FInitialized) then
  begin
    FInitialized := True;
    if (FImage.Source = '') then
      EditImageFileRightButtonClick(nil);
  end;
end;

constructor TFormEditImage.Create(const Image: TRibbonImage;
  const Flags: TImageFlags);
var
  UIImage: TUIImage;
begin
  inherited Create(nil);
  FImage := Image;
  FFlags := Flags;
  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pf32Bit;
  FBitmap.SetSize(64, 64);
  ClearBitmap(FBitmap);
  FFilename := Image.Owner.BuildAbsoluteFilename(Image.Source);
  if FileExists(FFilename) then
  begin
    UIImage := TUIImage.Create(FFilename);
    try
      UIImage.Draw(FBitmap.Canvas, (64 - UIImage.Width) div 2, (64 - UIImage.Height) div 2);
    finally
      UIImage.Free;
    end;
  end;
  PaintBox.ControlStyle := PaintBox.ControlStyle + [csOpaque];

  EditImageFile.Text := FImage.Source;
  if (FImage.MinDpi = 0) then
    ComboBoxMinDpi.ItemIndex := 0
  else if (FImage.MinDpi < 108) then
    ComboBoxMinDpi.ItemIndex := 1
  else if (FImage.MinDpi < 132) then
    ComboBoxMinDpi.ItemIndex := 2
  else if (FImage.MinDpi < 156) then
    ComboBoxMinDpi.ItemIndex := 3
  else
    ComboBoxMinDpi.ItemIndex := 4;

  UpDownResourceId.Position := FImage.Id;
  EditSymbol.Text := FImage.Symbol;
  UpdateControls;
end;

destructor TFormEditImage.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TFormEditImage.EditImageFileChange(Sender: TObject);
begin
  FFilename := FImage.Owner.BuildAbsoluteFilename(EditImageFile.Text);
  UpdateControls;
end;

procedure TFormEditImage.EditImageFileRightButtonClick(Sender: TObject);
var
  NewFilename: String;
  UIImage: TUIImage;
  Bitmap: TBitmap;
  Size: Integer;
begin
  OpenDialog.InitialDir := ExtractFilePath(FFilename);
  OpenDialog.FileName := ExtractFileName(FFilename);
  if (OpenDialog.Execute) then
  begin
    NewFilename := OpenDialog.FileName;

    { TUIImage will automatically convert to 32-bit alpha image }
    Bitmap := nil;
    UIImage := TUIImage.Create(NewFilename, ifHighContrast in FFlags);
    try
      Bitmap := TBitmap.Create;
      Bitmap.Handle := UIImage.Bitmap;
      if (not StartsText(FImage.Owner.Directory, NewFilename)) then
      begin
        NewFilename := TPath.Combine(FImage.Owner.Directory, 'Res');
        ForceDirectories(NewFilename);
        NewFilename := TPath.Combine(NewFilename, ExtractFilename(OpenDialog.FileName));
      end;
      NewFilename := ChangeFileExt(NewFilename, '.bmp');
      Bitmap.SaveToFile(NewFilename);

      EditImageFile.Text := FImage.Owner.BuildRelativeFilename(NewFilename);
      FImage.Source := EditImageFile.Text;

      ClearBitmap(FBitmap);
      UIImage.Draw(FBitmap.Canvas, (64 - UIImage.Width) div 2, (64 - UIImage.Height) div 2);
      PaintBox.Repaint;

      Size := Max(Bitmap.Width, Bitmap.Height);
      if (ifLarge in FFlags) then
        Size := Size div 2;
      if (Size <= 16) then
        ComboBoxMinDpi.ItemIndex := 0
      else if (Size <= 20) then
        ComboBoxMinDpi.ItemIndex := 2
      else if (Size <= 24) then
        ComboBoxMinDpi.ItemIndex := 3
      else
        ComboBoxMinDpi.ItemIndex := 4;
    finally
      UIImage.Free;
      Bitmap.Free;
    end;
  end;
end;

procedure TFormEditImage.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (ModalResult = mrOk) then
  begin
    FImage.Source := EditImageFile.Text;
    case ComboBoxMinDpi.ItemIndex of
      0: FImage.MinDpi := 0;
      1: FImage.MinDpi := 96;
      2: FImage.MinDpi := 120;
      3: FImage.MinDpi := 144;
      4: FImage.MinDpi := 192;
    end;
    FImage.Id := UpDownResourceId.Position;
    FImage.Symbol := EditSymbol.Text;
    FormMain.Modified;
  end;
end;

procedure TFormEditImage.PaintBoxPaint(Sender: TObject);
begin
  PaintBox.Canvas.Draw(0, 0, FBitmap);
end;

procedure TFormEditImage.UpdateControls;
begin
  ButtonOk.Enabled := FileExists(FFilename);
end;

end.
