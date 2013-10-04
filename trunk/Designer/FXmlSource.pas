unit FXmlSource;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Types, RibbonMarkup, BasicXml;

type
  TFrameXmlSource = class(TFrame)
    TreeViewXmlSource: TTreeView;
    procedure TreeViewXmlSourceCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeViewXmlSourceClick(Sender: TObject);
    procedure TreeViewXmlSourceCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure TreeViewXmlSourceExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeViewXmlSourceDblClick(Sender: TObject);
  private
    { Private declarations }
    FDocument: TRibbonDocument;
    FXmlDoc: TXmlDocument;
    FMarginWidth: Integer;
    FSpaceWidth: Integer;
    FEqualWidth: Integer;
    FQuoteWidth: Integer;
    FLineCount: Integer;
    FAllowExpandCollapse: Boolean;
    function AddNode(const Parent: TTreeNode; const Element: TXmlElement;
      var LineNum: Integer): TTreeNode;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ClearDocument;
    procedure ShowDocument(const Document: TRibbonDocument);
    procedure Activate;
    procedure Deactivate;
  end;

implementation

{$R *.dfm}

{ TFrameXmlSource }

procedure TFrameXmlSource.Activate;
var
  Root: TTreeNode;
  LineNum: Integer;
begin
  FMarginWidth := -1;
  TreeViewXmlSource.Items.BeginUpdate;
  try
    TreeViewXmlSource.Items.Clear;
    if FileExists(FDocument.Filename) then
    begin
      FXmlDoc.LoadFromFile(FDocument.Filename);
      LineNum := 2;
      Root := AddNode(nil, FXmlDoc.Root, LineNum);
      FLineCount := LineNum - 1;
      FAllowExpandCollapse := True;
      TreeViewXmlSource.FullExpand;
      TreeViewXmlSource.Selected := Root;
      Root.MakeVisible;
    end;
  finally
    TreeViewXmlSource.Items.EndUpdate;
    FAllowExpandCollapse := False;
  end;
end;

function TFrameXmlSource.AddNode(const Parent: TTreeNode;
  const Element: TXmlElement; var LineNum: Integer): TTreeNode;
var
  Child: TXmlElement;
  EndNode: TTreeNode;
begin
  Result := TreeViewXmlSource.Items.AddChild(Parent, '');
  Result.Data := Element;
  Result.ImageIndex := LineNum;
  Inc(LineNum);
  if (Element.ChildCount > 0) then
  begin
    for Child in Element do
      AddNode(Result, Child, LineNum);
    EndNode := TreeViewXmlSource.Items.AddChild(Parent, Element.Name);
    EndNode.ImageIndex := LineNum;
    Inc(LineNum);
  end;
end;

procedure TFrameXmlSource.ClearDocument;
begin
  TreeViewXmlSource.Items.Clear;
end;

constructor TFrameXmlSource.Create(AOwner: TComponent);
begin
  inherited;
  FXmlDoc := TXmlDocument.Create;
end;

procedure TFrameXmlSource.Deactivate;
begin
  { Nothing yet }
end;

destructor TFrameXmlSource.Destroy;
begin
  FXmlDoc.Free;
  inherited;
end;

procedure TFrameXmlSource.ShowDocument(const Document: TRibbonDocument);
begin
  FDocument := Document;
end;

procedure TFrameXmlSource.TreeViewXmlSourceClick(Sender: TObject);
var
  P: TPoint;
  Node: TTreeNode;
begin
  P := Mouse.CursorPos;
  P := TreeViewXmlSource.ScreenToClient(P);
  Node := TreeViewXmlSource.GetNodeAt(P.X, P.Y);
  TreeViewXmlSource.Selected := Node;
  if Assigned(Node) and (Node.Count > 0) and (P.X < FMarginWidth) and (P.X > (FMarginWidth - 10)) then
  begin
    FAllowExpandCollapse := True;
    try
      Node.Expanded := not Node.Expanded;
    finally
      FAllowExpandCollapse := False;
    end;
  end;
end;

procedure TFrameXmlSource.TreeViewXmlSourceCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  AllowCollapse := FAllowExpandCollapse;
end;

procedure TFrameXmlSource.TreeViewXmlSourceCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  var DefaultDraw: Boolean);
const
  TEXT_FLAGS        = DT_NOPREFIX;
  COLOR_FOCUS       = $C0FFFF;
  COLOR_MARGIN      = $F4F4F4;
  COLOR_MARGIN_TEXT = $CC9999;
  COLOR_SYMBOL      = $FF0000;
  COLOR_ELEMENT     = $1515A3;
  COLOR_ATTRIBUTE   = $0000FF;
  COLOR_CONTENT     = $000000;
var
  RN, RT: TRect;
  Canvas: TCanvas;
  DC: HDC;
  Element: TXmlElement;
  S: String;
  I: Integer;
begin
  Element := Node.Data;
  Canvas := Sender.Canvas;
  DC := Canvas.Handle;
  SetBkMode(DC, TRANSPARENT);

  RN := Node.DisplayRect(False);
  RT := Node.DisplayRect(True);
  RT.Right := RN.Right;

  if (FMarginWidth < 0) then
  begin
    S := IntToStr(FLineCount);
    for I := 1 to Length(S) do
      S[I] := '0';
    FMarginWidth := Canvas.TextWidth(S) + 20;
    FSpaceWidth := Canvas.TextWidth(' ');
    FEqualWidth := Canvas.TextWidth('=');
    FQuoteWidth := Canvas.TextWidth('"');
  end;

  { Focus rect }
  if (cdsFocused in State) then
  begin
    Canvas.Brush.Color := COLOR_FOCUS;
    Canvas.FillRect(RN);
  end;

  { Margin with line numbers }
  Canvas.Brush.Color := COLOR_MARGIN;
  RN.Right := RN.Left + FMarginWidth;
  Canvas.FillRect(RN);
  I := Node.ImageIndex;
  if ((I mod 10) = 0) or (cdsFocused in State) then
    S := IntToStr(Node.ImageIndex)
  else
    S := '-';
  SetTextColor(DC, COLOR_MARGIN_TEXT);
  Dec(RN.Right, 18);
  DrawText(DC, S, Length(S), RN, TEXT_FLAGS or DT_RIGHT);

  { Expand/Collapse markers }
  if (Node.Count > 0) then
  begin
    RN.Left := RN.Right + 6;
    RN.Top := RN.Top + (RN.Bottom - RN.Top - 9) div 2;
    RN.Right := RN.Left + 9;
    RN.Bottom := RN.Top + 9;
    Canvas.Brush.Color := COLOR_MARGIN_TEXT;
    Canvas.FrameRect(RN);
    Canvas.Pen.Color := COLOR_MARGIN_TEXT;
    Canvas.MoveTo(RN.Left + 2, RN.Top + 4);
    Canvas.LineTo(RN.Left + 7, RN.Top + 4);
    if (not Node.Expanded) then
    begin
      Canvas.MoveTo(RN.Left + 4, RN.Top + 2);
      Canvas.LineTo(RN.Left + 4, RN.Top + 7);
    end;
  end
  else
  begin
    RN.Left := RN.Right + 10;
    Canvas.Pen.Color := COLOR_MARGIN_TEXT;
    Canvas.MoveTo(RN.Left, RN.Top);
    Canvas.LineTo(RN.Left, RN.Bottom);
  end;

  { Draw element }
  SetTextColor(DC, COLOR_SYMBOL);
  Inc(RT.Left, FMarginWidth);
  if Assigned(Element) then
    S := '<'
  else
    S := '</';
  DrawText(DC, S, Length(S), RT, TEXT_FLAGS);
  Inc(RT.Left, Canvas.TextWidth(S));

  SetTextColor(DC, COLOR_ELEMENT);
  if Assigned(Element) then
    S := Element.Name
  else
    S := Node.Text;
  DrawText(DC, S, Length(S), RT, TEXT_FLAGS);
  Inc(RT.Left, Canvas.TextWidth(S));

  { Draw attributes }
  if Assigned(Element) then
  begin
    for I := 0 to Element.AttributeCount - 1 do
    begin
      Inc(RT.Left, FSpaceWidth);
      SetTextColor(DC, COLOR_ATTRIBUTE);
      S := Element.AttributeNames[I];
      DrawText(DC, S, Length(S), RT, TEXT_FLAGS);
      Inc(RT.Left, Canvas.TextWidth(S));

      SetTextColor(DC, COLOR_SYMBOL);
      DrawText(DC, '=', 1, RT, TEXT_FLAGS);
      Inc(RT.Left, FEqualWidth);

      SetTextColor(DC, COLOR_CONTENT);
      DrawText(DC, '"', 1, RT, TEXT_FLAGS);
      Inc(RT.Left, FQuoteWidth);

      SetTextColor(DC, COLOR_SYMBOL);
      S := Element.AttributeValues[I];
      DrawText(DC, S, Length(S), RT, TEXT_FLAGS);
      Inc(RT.Left, Canvas.TextWidth(S));

      SetTextColor(DC, COLOR_CONTENT);
      DrawText(DC, '"', 1, RT, TEXT_FLAGS);
      Inc(RT.Left, FQuoteWidth);
    end;
  end;

  if Assigned(Element) then
    S := Element.ContentAsString
  else
    S := '';

  if (S <> '') then
  begin
    SetTextColor(DC, COLOR_SYMBOL);
    DrawText(DC, '>', 1, RT, TEXT_FLAGS);
    Inc(RT.Left, Canvas.TextWidth('>'));

    SetTextColor(DC, COLOR_CONTENT);
    DrawText(DC, S, Length(S), RT, TEXT_FLAGS);
    Inc(RT.Left, Canvas.TextWidth(S));

    SetTextColor(DC, COLOR_SYMBOL);
    DrawText(DC, '</', 2, RT, TEXT_FLAGS);
    Inc(RT.Left, Canvas.TextWidth('</'));

    S := Element.Name;
    SetTextColor(DC, COLOR_ELEMENT);
    DrawText(DC, S, Length(S), RT, TEXT_FLAGS);
    Inc(RT.Left, Canvas.TextWidth(S));

    SetTextColor(DC, COLOR_SYMBOL);
    DrawText(DC, '>', 1, RT, TEXT_FLAGS);
  end
  else
  begin
    SetTextColor(DC, COLOR_SYMBOL);
    if Assigned(Element) and (Node.Count = 0) then
      S := '/>'
    else
      S := '>';
    DrawText(DC, S, Length(S), RT, TEXT_FLAGS);
  end;

  DefaultDraw := False;
end;

procedure TFrameXmlSource.TreeViewXmlSourceDblClick(Sender: TObject);
var
  P: TPoint;
  Node: TTreeNode;
begin
  P := Mouse.CursorPos;
  P := TreeViewXmlSource.ScreenToClient(P);
  Node := TreeViewXmlSource.GetNodeAt(P.X, P.Y);
  if Assigned(Node) and (Node.Count > 0) then
  begin
    FAllowExpandCollapse := True;
    try
      Node.Expanded := not Node.Expanded;
    finally
      FAllowExpandCollapse := False;
    end;
  end;
end;

procedure TFrameXmlSource.TreeViewXmlSourceExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  AllowExpansion := FAllowExpandCollapse;
end;

end.
