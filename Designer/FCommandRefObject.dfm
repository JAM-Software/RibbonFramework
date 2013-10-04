inherited FrameCommandRefObject: TFrameCommandRefObject
  ParentFont = False
  object Label1: TLabel [0]
    Left = 6
    Top = 31
    Width = 47
    Height = 13
    Caption = 'Command'
  end
  object LabelHeader: TLabel [1]
    Left = 0
    Top = 0
    Width = 320
    Height = 22
    Align = alTop
    AutoSize = False
    Caption = '  Properties'
    Color = clActiveCaption
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
    Layout = tlCenter
    ExplicitLeft = -407
    ExplicitWidth = 727
  end
  object ComboBoxCommand: TComboBox
    Left = 120
    Top = 27
    Width = 250
    Height = 21
    Hint = 'The command associated with this control.'
    Style = csDropDownList
    DropDownCount = 50
    TabOrder = 0
    OnChange = ComboBoxCommandChange
  end
end
