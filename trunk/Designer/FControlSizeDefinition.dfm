inherited FrameControlSizeDefintion: TFrameControlSizeDefintion
  Width = 587
  ExplicitWidth = 587
  inherited ImageSample: TImage
    Left = 479
  end
  object LabelHeader: TLabel
    Left = 0
    Top = 0
    Width = 587
    Height = 22
    Align = alTop
    AutoSize = False
    Caption = '  Control Size Definition Properties'
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
    ExplicitLeft = -224
    ExplicitWidth = 544
  end
  object Label1: TLabel
    Left = 6
    Top = 31
    Width = 65
    Height = 13
    Caption = 'Control Name'
  end
  object Label2: TLabel
    Left = 6
    Top = 58
    Width = 52
    Height = 13
    Caption = 'Image Size'
  end
  object ComboBoxControlName: TComboBox
    Left = 120
    Top = 27
    Width = 250
    Height = 21
    Hint = 'The name of the control to which this size definition applies.'
    Style = csDropDownList
    DropDownCount = 30
    TabOrder = 0
    OnChange = ComboBoxControlNameChange
  end
  object ComboBoxImageSize: TComboBox
    Left = 120
    Top = 54
    Width = 250
    Height = 21
    Hint = 'Size of the images for this definition'
    Style = csDropDownList
    DropDownCount = 30
    TabOrder = 1
    OnChange = ComboBoxImageSizeChange
    Items.Strings = (
      'Large'
      'Small')
  end
  object CheckBoxIsLabelVisible: TCheckBox
    Left = 6
    Top = 84
    Width = 111
    Height = 17
    Hint = 'Whether the control label is visible in this definition'
    Caption = 'Label visible'
    TabOrder = 2
    OnClick = CheckBoxIsLabelVisibleClick
  end
  object CheckBoxIsImageVisible: TCheckBox
    Left = 6
    Top = 107
    Width = 111
    Height = 17
    Hint = 'Whether the control image is visible in this definition'
    Caption = 'Image visible'
    TabOrder = 3
    OnClick = CheckBoxIsImageVisibleClick
  end
  object CheckBoxIsPopup: TCheckBox
    Left = 6
    Top = 130
    Width = 111
    Height = 17
    Caption = 'Is Popup'
    TabOrder = 4
    OnClick = CheckBoxIsPopupClick
  end
end
