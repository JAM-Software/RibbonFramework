inherited FrameGroupSizeDefinition: TFrameGroupSizeDefinition
  Width = 544
  ExplicitWidth = 544
  inherited ImageSample: TImage
    Left = 436
    ExplicitLeft = 436
  end
  object LabelHeader: TLabel
    Left = 0
    Top = 0
    Width = 544
    Height = 22
    Align = alTop
    AutoSize = False
    Caption = '  Group Size Definition Properties'
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
    ExplicitLeft = -322
    ExplicitWidth = 642
  end
  object Label1: TLabel
    Left = 6
    Top = 31
    Width = 19
    Height = 13
    Caption = 'Size'
  end
  object ComboBoxSize: TComboBox
    Left = 120
    Top = 27
    Width = 250
    Height = 21
    Hint = 'The command associated with this control.'
    Style = csDropDownList
    DropDownCount = 30
    TabOrder = 0
    OnChange = ComboBoxSizeChange
    Items.Strings = (
      'Large'
      'Medium'
      'Small')
  end
end
