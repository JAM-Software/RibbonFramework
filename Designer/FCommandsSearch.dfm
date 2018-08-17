object CommandSearchForm: TCommandSearchForm
  Left = 0
  Top = 0
  Caption = 'Search for command'
  ClientHeight = 336
  ClientWidth = 391
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BottomPanel: TPanel
    Left = 0
    Top = 298
    Width = 391
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object ButtonCancel: TButton
      AlignWithMargins = True
      Left = 310
      Top = 6
      Width = 75
      Height = 26
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = ButtonCancelClick
    end
    object ButtonOK: TButton
      AlignWithMargins = True
      Left = 229
      Top = 6
      Width = 75
      Height = 26
      Margins.Top = 6
      Margins.Bottom = 6
      Align = alRight
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = ButtonOKClick
    end
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 391
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      391
      41)
    object LabeledEditSearchInput: TLabeledEdit
      Left = 100
      Top = 12
      Width = 285
      Height = 21
      Anchors = [akLeft, akTop, akRight, akBottom]
      EditLabel.Width = 83
      EditLabel.Height = 13
      EditLabel.Caption = 'Command name: '
      LabelPosition = lpLeft
      TabOrder = 0
      OnChange = LabeledEditSearchInputChange
      OnKeyDown = LabeledEditSearchInputKeyDown
    end
  end
  object ListViewCommands: TListView
    Left = 0
    Top = 41
    Width = 391
    Height = 257
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    Columns = <
      item
        Caption = 'Name'
        Width = 170
      end
      item
        AutoSize = True
        Caption = 'Caption'
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    SortType = stText
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = ListViewCommandsDblClick
  end
end
