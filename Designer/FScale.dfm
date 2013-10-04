inherited FrameScale: TFrameScale
  Width = 604
  ExplicitWidth = 604
  inherited Label1: TLabel
    Width = 29
    Caption = 'Group'
    ExplicitWidth = 29
  end
  inherited LabelHeader: TLabel
    Width = 604
    Caption = '  Scale Properties'
    ExplicitWidth = 604
  end
  inherited ImageSample: TImage
    Left = 496
    ExplicitLeft = 496
  end
  object Label2: TLabel [3]
    Left = 6
    Top = 58
    Width = 19
    Height = 13
    Caption = 'Size'
  end
  inherited ComboBoxCommand: TComboBox
    Hint = 
      'The command associated with the group of controls to which this ' +
      'scale applies.'
  end
  object ComboBoxSize: TComboBox
    Left = 120
    Top = 54
    Width = 250
    Height = 21
    Style = csDropDownList
    DropDownCount = 20
    TabOrder = 1
    OnChange = ComboBoxSizeChange
    Items.Strings = (
      'Popup'
      'Small'
      'Medium'
      'Large')
  end
end
