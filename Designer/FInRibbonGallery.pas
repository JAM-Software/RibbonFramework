unit FInRibbonGallery;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FGallery, StdCtrls, ComCtrls, ExtCtrls, pngimage, RibbonMarkup;

type
  TFrameInRibbonGallery = class(TFrameGallery)
    Label9: TLabel;
    EditMinColumnsLarge: TEdit;
    UpDownMinColumnsLarge: TUpDown;
    Label10: TLabel;
    Label11: TLabel;
    EditMinColumnsMedium: TEdit;
    UpDownMinColumnsMedium: TUpDown;
    Label12: TLabel;
    Label13: TLabel;
    EditMaxColumnsMedium: TEdit;
    UpDownMaxColumnsMedium: TUpDown;
    Label14: TLabel;
    Label15: TLabel;
    EditMaxColumns: TEdit;
    UpDownMaxColumns: TUpDown;
    Label16: TLabel;
    Label17: TLabel;
    EditMaxRows: TEdit;
    UpDownMaxRows: TUpDown;
    Label18: TLabel;
    procedure EditMinColumnsLargeChange(Sender: TObject);
    procedure EditMinColumnsMediumChange(Sender: TObject);
    procedure EditMaxColumnsMediumChange(Sender: TObject);
    procedure EditMaxColumnsChange(Sender: TObject);
    procedure EditMaxRowsChange(Sender: TObject);
  private
    { Private declarations }
    FInRibbonGallery: TRibbonInRibbonGallery;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFrameInRibbonGallery }

procedure TFrameInRibbonGallery.EditMaxColumnsChange(Sender: TObject);
begin
  if (UpDownMaxColumns.Position <> FInRibbonGallery.MaxColumns) then
  begin
    FInRibbonGallery.MaxColumns := UpDownMaxColumns.Position;
    Modified;
  end;
end;

procedure TFrameInRibbonGallery.EditMaxColumnsMediumChange(Sender: TObject);
begin
  if (UpDownMaxColumnsMedium.Position <> FInRibbonGallery.MaxColumnsMedium) then
  begin
    FInRibbonGallery.MaxColumnsMedium := UpDownMaxColumnsMedium.Position;
    Modified;
  end;
end;

procedure TFrameInRibbonGallery.EditMaxRowsChange(Sender: TObject);
begin
  if (UpDownMaxRows.Position <> FInRibbonGallery.MaxRows) then
  begin
    FInRibbonGallery.MaxRows := UpDownMaxRows.Position;
    Modified;
  end;
end;

procedure TFrameInRibbonGallery.EditMinColumnsLargeChange(Sender: TObject);
begin
  if (UpDownMinColumnsLarge.Position <> FInRibbonGallery.MinColumnsLarge) then
  begin
    FInRibbonGallery.MinColumnsLarge := UpDownMinColumnsLarge.Position;
    Modified;
  end;
end;

procedure TFrameInRibbonGallery.EditMinColumnsMediumChange(Sender: TObject);
begin
  if (UpDownMinColumnsMedium.Position <> FInRibbonGallery.MinColumnsMedium) then
  begin
    FInRibbonGallery.MinColumnsMedium := UpDownMinColumnsMedium.Position;
    Modified;
  end;
end;

procedure TFrameInRibbonGallery.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FInRibbonGallery := Subject as TRibbonInRibbonGallery;
  UpDownMinColumnsLarge.Position := FInRibbonGallery.MinColumnsLarge;
  UpDownMinColumnsMedium.Position := FInRibbonGallery.MinColumnsMedium;
  UpDownMaxColumnsMedium.Position := FInRibbonGallery.MaxColumnsMedium;
  UpDownMaxColumns.Position := FInRibbonGallery.MaxColumns;
  UpDownMaxRows.Position := FInRibbonGallery.MaxRows;
end;

end.
