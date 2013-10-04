unit FImageList;

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
  ActnList,
  ImgList,
  ComCtrls,
  CommCtrl,
  ToolWin,
  RibbonMarkup,
  UIRibbonCommands, Menus;

type
  TImageFlag = (ifLarge, ifHighContrast);
  TImageFlags = set of TImageFlag;

type
  TFrameImageList = class(TFrame)
    ToolBarSmallImages: TToolBar;
    ButtonAddSmallImage: TToolButton;
    ButtonDeleteSmallImage: TToolButton;
    ButtonEditSmallImage: TToolButton;
    ListView: TListView;
    ImageListToolbars: TImageList;
    ImageList: TImageList;
    ActionList: TActionList;
    ActionAddImage: TAction;
    ActionDeleteImage: TAction;
    ActionEditImage: TAction;
    ActionAddMultiple: TAction;
    PopupMenu: TPopupMenu;
    PopupAdd: TMenuItem;
    PopupAddMultiple: TMenuItem;
    OpenDialog: TOpenDialog;
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListViewDblClick(Sender: TObject);
    procedure ActionEditImageExecute(Sender: TObject);
    procedure ActionAddImageExecute(Sender: TObject);
    procedure ActionDeleteImageExecute(Sender: TObject);
    procedure ActionAddMultipleExecute(Sender: TObject);
  private
    { Private declarations }
    FCommand: TRibbonCommand;
    FImages: TRibbonList<TRibbonImage>;
    FFlags: TImageFlags;
    procedure SetImageItem(const Item: TListItem; const Image: TRibbonImage);
  public
    { Public declarations }
    procedure ShowImages(const Command: TRibbonCommand; const Flags: TImageFlags);
  end;

resourcestring
  RS_NEED_SAVE_HEADER = 'Document must be saved';
  RS_NEED_SAVE_MESSAGE = 'The document needs to be saved before you can add images.' + sLineBreak +
    'Do you want to save the document now?';
//  RS_DELETE_IMAGE_HEADER = 'Delete Image?';
//  RS_DELETE_IMAGE_MESSAGE = 'Do you want to delete this image (this cannot be undone)?';

implementation

{$R *.dfm}

uses
  StrUtils,
  IOUtils,
  Math,
  FMain,
  FEditImage;

{ TFrameImageList }

procedure TFrameImageList.ActionAddImageExecute(Sender: TObject);
var
  Image: TRibbonImage;
  Form: TFormEditImage;
  Item: TListItem;
begin
  if (FCommand.Owner.Filename = '') then
  begin
    if (TaskMessageDlg(RS_NEED_SAVE_HEADER, RS_NEED_SAVE_MESSAGE, mtConfirmation,
      [mbYes, mbNo], 0, mbYes) = mrNo)
    then
      Exit;
    FormMain.ActionSaveAs.Execute;
    if (FCommand.Owner.Filename = '') then
      Exit;
  end;

  if (ifLarge in FFlags) then
    if (ifHighContrast in FFlags) then
      Image := FCommand.AddLargeHighContrastImage
    else
      Image := FCommand.AddLargeImage
  else
    if (ifHighContrast in FFlags) then
      Image := FCommand.AddSmallHighContrastImage
    else
      Image := FCommand.AddSmallImage;

  Form := TFormEditImage.Create(Image, FFlags);
  try
    if (Form.ShowModal = mrOk) then
    begin
      Item := ListView.Items.Add;
      SetImageItem(Item, Image);
      ListView.Selected := Item;
      Item.Focused := True;
    end
    else
      FCommand.DeleteImage(Image);
  finally
    Form.Release;
  end;
end;

procedure TFrameImageList.ActionAddMultipleExecute(Sender: TObject);
var
  S, Filename: String;
  Bitmap: TBitmap;
  UIImage: TUIImage;
  Image: TRibbonImage;
  Size: Integer;
  Item: TListItem;
begin
  if (OpenDialog.Execute) then
  begin
    for S in OpenDialog.Files do
    begin
      Filename := S;

      if (ifLarge in FFlags) then
        if (ifHighContrast in FFlags) then
          Image := FCommand.AddLargeHighContrastImage
        else
          Image := FCommand.AddLargeImage
      else
        if (ifHighContrast in FFlags) then
          Image := FCommand.AddSmallHighContrastImage
        else
          Image := FCommand.AddSmallImage;

      { TUIImage will automatically convert to 32-bit alpha image }
      Bitmap := nil;
      UIImage := TUIImage.Create(Filename, ifHighContrast in FFlags);
      try
        Bitmap := TBitmap.Create;
        Bitmap.Handle := UIImage.Bitmap;
        if (not StartsText(Image.Owner.Directory, Filename)) then
        begin
          Filename := TPath.Combine(Image.Owner.Directory, 'Res');
          ForceDirectories(Filename);
          Filename := TPath.Combine(Filename, ExtractFilename(S));
        end;
        Filename := ChangeFileExt(Filename, '.bmp');
        Bitmap.SaveToFile(Filename);
        Image.Source := Image.Owner.BuildRelativeFilename(Filename);

        Size := Max(Bitmap.Width, Bitmap.Height);
        if (ifLarge in FFlags) then
          Size := Size div 2;
        if (Size <= 16) then
          Image.MinDpi := 0
        else if (Size <= 20) then
          Image.MinDpi := 120
        else if (Size <= 24) then
          Image.MinDpi := 144
        else
          Image.MinDpi := 192
      finally
        UIImage.Free;
        Bitmap.Free;
      end;

      Item := ListView.Items.Add;
      SetImageItem(Item, Image);
      ListView.Selected := Item;
      Item.Focused := True;
      FormMain.Modified;
    end;
  end;
end;

procedure TFrameImageList.ActionDeleteImageExecute(Sender: TObject);
var
  Image: TRibbonImage;
begin
//  if Assigned(ListView.Selected) and (TaskMessageDlg(RS_DELETE_IMAGE_HEADER,
//    RS_DELETE_IMAGE_MESSAGE, mtConfirmation, [mbYes, mbNo], 0, mbYes) = mrYes)
//  then
  begin
    Image := ListView.Selected.Data;
    FCommand.DeleteImage(Image);
    ListView.Selected.Free;
    FormMain.Modified;
  end;
end;

procedure TFrameImageList.ActionEditImageExecute(Sender: TObject);
var
  Image: TRibbonImage;
  Form: TFormEditImage;
begin
  if Assigned(ListView.Selected) then
    Image := ListView.Selected.Data
  else
    Exit;

  if (Image = nil) then
    Exit;

  Form := TFormEditImage.Create(Image, FFlags);
  try
    if (Form.ShowModal = mrOk) then
      SetImageItem(ListView.Selected, Image);
  finally
    Form.Release;
  end;
end;

procedure TFrameImageList.ListViewDblClick(Sender: TObject);
begin
  if ActionEditImage.Enabled then
    ActionEditImage.Execute;
end;

procedure TFrameImageList.ListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  ActionDeleteImage.Enabled := Assigned(Item) and Selected;
  ActionEditImage.Enabled := Assigned(Item) and Selected;
end;

procedure TFrameImageList.SetImageItem(const Item: TListItem;
  const Image: TRibbonImage);
var
  Filename: String;
  UIImage: TUIImage;
  ImageIndex: TImageIndex;
  Bitmap: TBitmap;
  Y: Integer;
begin
  Filename := Image.Owner.BuildAbsoluteFilename(Image.Source);
  if FileExists(Filename) then
  begin
    UIImage := TUIImage.Create(Filename);
    try
      if (UIImage.Width = ImageList.Width) and (UIImage.Height = ImageList.Height) then
        ImageIndex := ImageList_Add(ImageList.Handle, UIImage.Bitmap, 0)
      else
      begin
        Bitmap := TBitmap.Create;
        try
          Bitmap.PixelFormat := pf32Bit;
          Bitmap.SetSize(ImageList.Width, ImageList.Height);
          for Y := 0 to Bitmap.Height - 1 do
            FillChar(Bitmap.ScanLine[Y]^, Bitmap.Width * 4, 0);

          if (UIImage.Width <= ImageList.Width) and (UIImage.Height <= ImageList.Height) then
            UIImage.Draw(Bitmap.Canvas,
              (ImageList.Width - UIImage.Width) div 2,
              (ImageList.Height - UIImage.Height) div 2)
          else
            UIImage.Draw(Bitmap.Canvas, 0, 0, ImageList.Width, ImageList.Height);
          ImageIndex := ImageList_Add(ImageList.Handle, Bitmap.Handle, 0)
        finally
          Bitmap.Free;
        end;
      end;
    finally
      UIImage.Free;
    end;
  end
  else
    ImageIndex := -1;

  Item.ImageIndex := ImageIndex;
  Item.SubItems.Clear;
  Item.SubItems.Add(IntToStr(Image.MinDpi));
  Item.SubItems.Add(IntToStr(Image.Id));
  Item.SubItems.Add(Image.Symbol);
  Item.SubItems.Add(Image.Source);
  Item.Data := Image;
end;

procedure TFrameImageList.ShowImages(const Command: TRibbonCommand;
  const Flags: TImageFlags);
var
  Image: TRibbonImage;
  Item: TListItem;
begin
  FImages := nil;
  FCommand := Command;
  FFlags := Flags;
  if Assigned(FCommand) then
  begin
    if (ifLarge in Flags) then
      if (ifHighContrast in Flags) then
        FImages := FCommand.LargeHighContrastImages
      else
        FImages := FCommand.LargeImages
    else
      if (ifHighContrast in Flags) then
        FImages := FCommand.SmallHighContrastImages
      else
        FImages := FCommand.SmallImages;
  end;
  ListView.Items.BeginUpdate;
  ImageList.BeginUpdate;
  try
    ListView.Clear;
    ImageList.Clear;

    ActionAddImage.Enabled := Assigned(FImages);
    ActionDeleteImage.Enabled := False;
    ActionEditImage.Enabled := False;

    if (FImages = nil) then
      Exit;

    for Image in FImages do
    begin
      Item := ListView.Items.Add;
      SetImageItem(Item, Image);
    end;

    if (ListView.Items.Count > 0) then
      ListView.ItemIndex := 0;
  finally
    ListView.Items.EndUpdate;
    ImageList.EndUpdate;
  end;
end;

end.
