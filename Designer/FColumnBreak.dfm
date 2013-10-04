inherited FrameColumnBreak: TFrameColumnBreak
  Width = 434
  ExplicitWidth = 434
  inherited ImageSample: TImage
    Left = 326
  end
  object LabelHeader: TLabel
    Left = 0
    Top = 0
    Width = 434
    Height = 22
    Align = alTop
    AutoSize = False
    Caption = '  Column Break Properties'
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
    ExplicitLeft = -267
    ExplicitWidth = 587
  end
  object CheckBoxShowSeparator: TCheckBox
    Left = 6
    Top = 33
    Width = 111
    Height = 17
    Caption = 'Show separator'
    TabOrder = 0
    OnClick = CheckBoxShowSeparatorClick
  end
end
