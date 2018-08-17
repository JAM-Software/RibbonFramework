unit FCommandsSearch;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  RibbonMarkup, System.Generics.Collections;

type
  /// <summary>
  /// This form shows the list of commands and allows to search for specific names. The view is filtered, so that only mathing commands remain and can thus be selected in the list.
  /// </summary>
  TCommandSearchForm = class(TForm)
    BottomPanel: TPanel;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    TopPanel: TPanel;
    LabeledEditSearchInput: TLabeledEdit;
    ListViewCommands: TListView;
    /// <summary> Okay button event handler</summary>
    procedure ButtonOKClick(Sender: TObject);
    /// <summary> Cancel button event handler</summary>
    procedure ButtonCancelClick(Sender: TObject);
    /// <summary> OnChange event handler of the input field</summary>
    procedure LabeledEditSearchInputChange(Sender: TObject);
    /// <summary> OnDoubleClick event handler of the list view. Closes the list.</summary>
    procedure ListViewCommandsDblClick(Sender: TObject);
    /// <summary> OnShow event handler of the form.</summary>
    procedure FormShow(Sender: TObject);
    /// <summary> OnKeyDown handler of the input field. Forwards certain navigation keys to the list view, to allow easier keyboard navigation.</summary>
    procedure LabeledEditSearchInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    fSource: TListView;
    /// <summary> Will update the list view by filtering all items that do not match the current search.</summary>
    procedure UpdateListView;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; const pSource: TListView); reintroduce;
  end;

implementation

uses
  System.Math;



{$R *.dfm}

{ TCommandSearchForm }

procedure TCommandSearchForm.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TCommandSearchForm.ButtonOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

constructor TCommandSearchForm.Create(AOwner: TComponent; const pSource: TListView);
begin
  inherited Create(AOwner);
  fSource := pSource;
  UpdateListView;
end;

procedure TCommandSearchForm.FormShow(Sender: TObject);
begin
  LabeledEditSearchInput.SetFocus;
end;

procedure TCommandSearchForm.LabeledEditSearchInputChange(Sender: TObject);
begin
  UpdateListView;
end;

procedure TCommandSearchForm.LabeledEditSearchInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //Forward some keys to the list view to allow easier navigation
  if Key in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT, VK_END, VK_HOME] then
    ListViewCommands.Perform(WM_KEYDOWN, Key, 0);
end;

procedure TCommandSearchForm.ListViewCommandsDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TCommandSearchForm.UpdateListView;
var
  lItem: TListItem;
  lNewItem: TListItem;
  lSearchText: string;
begin
  Self.ListViewCommands.Clear;
  lSearchText := LabeledEditSearchInput.Text;

  for lItem in fSource.Items do
  begin
    if lSearchText.IsEmpty or (lItem.Caption.ToUpper.Contains(lSearchText.ToUpper)) then
    begin
      lNewItem := ListViewCommands.Items.Insert(ListViewCommands.Items.Count - 1);
      lNewItem.Assign(lItem);
    end;
  end;

  if ListViewCommands.Items.Count > 0 then
    ListViewCommands.Items[0].Selected := True;
end;

end.
