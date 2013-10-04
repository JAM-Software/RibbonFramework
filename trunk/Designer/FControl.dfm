inherited FrameControl: TFrameControl
  inherited LabelHeader: TLabel
    Caption = '  Control Properties'
  end
  object LabelApplicationModes: TLabel [2]
    Left = 6
    Top = 58
    Width = 86
    Height = 13
    Caption = 'Application Modes'
  end
  inherited ComboBoxCommand: TComboBox
    OnChange = nil
  end
  object EditApplicationModes: TButtonedEdit
    Left = 120
    Top = 55
    Width = 250
    Height = 21
    Hint = 'The application modes in which this control is available.'
    Images = DataModuleShared.ImageListAppModes
    ReadOnly = True
    RightButton.DisabledImageIndex = 2
    RightButton.HotImageIndex = 1
    RightButton.ImageIndex = 0
    RightButton.PressedImageIndex = 1
    RightButton.Visible = True
    TabOrder = 1
    OnRightButtonClick = EditApplicationModesRightButtonClick
  end
end
