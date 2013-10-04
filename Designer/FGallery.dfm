inherited FrameGallery: TFrameGallery
  Width = 562
  Height = 405
  ExplicitWidth = 562
  ExplicitHeight = 405
  DesignSize = (
    562
    405)
  inherited LabelHeader: TLabel
    Width = 562
    ExplicitWidth = 562
  end
  inherited ImageSample: TImage
    Left = 454
    ExplicitLeft = 454
  end
  object Label2: TLabel [4]
    Left = 6
    Top = 86
    Width = 60
    Height = 13
    Caption = 'Gallery Type'
  end
  object Label3: TLabel [5]
    Left = 6
    Top = 139
    Width = 53
    Height = 13
    Caption = 'Item Width'
  end
  object Label4: TLabel [6]
    Left = 207
    Top = 139
    Width = 140
    Height = 13
    Caption = 'Use -1 for auto/default width'
  end
  object Label5: TLabel [7]
    Left = 6
    Top = 166
    Width = 56
    Height = 13
    Caption = 'Item Height'
  end
  object Label6: TLabel [8]
    Left = 207
    Top = 166
    Width = 144
    Height = 13
    Caption = 'Use -1 for auto/default height'
  end
  object Label7: TLabel [9]
    Left = 6
    Top = 113
    Width = 62
    Height = 13
    Caption = 'Text Position'
  end
  object ComboBoxGalleryType: TComboBox
    Left = 120
    Top = 82
    Width = 250
    Height = 21
    Hint = 'Whether the gallery shows items or commands'
    Style = csDropDownList
    DropDownCount = 30
    TabOrder = 2
    OnChange = ComboBoxGalleryTypeChange
    Items.Strings = (
      'Items'
      'Commands')
  end
  object CheckBoxHasLargeItems: TCheckBox
    Left = 6
    Top = 192
    Width = 99
    Height = 17
    Hint = 'Whether the gallery shows large or small images'
    Caption = 'Has large items'
    TabOrder = 8
    OnClick = CheckBoxHasLargeItemsClick
  end
  object EditItemWidth: TEdit
    Left = 120
    Top = 136
    Width = 65
    Height = 21
    Hint = 'Width of the items in the gallery'
    TabOrder = 4
    Text = '-1'
    OnChange = EditItemWidthChange
  end
  object UpDownItemWidth: TUpDown
    Left = 185
    Top = 136
    Width = 16
    Height = 21
    Associate = EditItemWidth
    Min = -1
    Max = 999
    Position = -1
    TabOrder = 5
  end
  object EditItemHeight: TEdit
    Left = 120
    Top = 163
    Width = 65
    Height = 21
    Hint = 'Height of the items in the gallery'
    TabOrder = 6
    Text = '-1'
    OnChange = EditItemHeightChange
  end
  object UpDownItemHeight: TUpDown
    Left = 185
    Top = 163
    Width = 16
    Height = 21
    Associate = EditItemHeight
    Min = -1
    Max = 999
    Position = -1
    TabOrder = 7
  end
  object ComboBoxTextPosition: TComboBox
    Left = 120
    Top = 109
    Width = 250
    Height = 21
    Hint = 'Position of the text for the items'
    Style = csDropDownList
    DropDownCount = 30
    TabOrder = 3
    OnChange = ComboBoxTextPositionChange
    Items.Strings = (
      'Bottom'
      'Hide'
      'Left'
      'Overlap'
      'Right'
      'Top')
  end
  object GroupBox1: TGroupBox
    Left = 6
    Top = 215
    Width = 364
    Height = 130
    Caption = 'Menu Layout'
    TabOrder = 9
    object Label8: TLabel
      Left = 10
      Top = 21
      Width = 60
      Height = 13
      Caption = 'Layout Type'
    end
    object LabelRowCount: TLabel
      Left = 10
      Top = 47
      Width = 53
      Height = 13
      Caption = 'Row Count'
    end
    object LabelRowCountInfo: TLabel
      Left = 205
      Top = 47
      Width = 85
      Height = 13
      Caption = 'Use -1 for default'
    end
    object LabelColumnCount: TLabel
      Left = 10
      Top = 74
      Width = 67
      Height = 13
      Caption = 'Column Count'
    end
    object LabelGripper: TLabel
      Left = 10
      Top = 102
      Width = 35
      Height = 13
      Caption = 'Gripper'
    end
    object ComboBoxLayoutType: TComboBox
      Left = 118
      Top = 17
      Width = 235
      Height = 21
      Hint = 'How the items are layed out in the gallery'
      Style = csDropDownList
      DropDownCount = 30
      TabOrder = 0
      OnChange = ComboBoxLayoutTypeChange
      Items.Strings = (
        'Default Layout'
        'Vertical Layout'
        'Flow Layout')
    end
    object EditRows: TEdit
      Left = 118
      Top = 44
      Width = 65
      Height = 21
      Hint = 'Number of rows in the layout'
      TabOrder = 1
      Text = '-1'
      OnChange = EditRowsChange
    end
    object UpDownRows: TUpDown
      Left = 183
      Top = 44
      Width = 16
      Height = 21
      Associate = EditRows
      Min = -1
      Max = 999
      Position = -1
      TabOrder = 2
    end
    object EditColumns: TEdit
      Left = 118
      Top = 71
      Width = 65
      Height = 21
      Hint = 'Number of columns in the flow layout'
      TabOrder = 3
      Text = '2'
      OnChange = EditColumnsChange
    end
    object UpDownColumns: TUpDown
      Left = 183
      Top = 71
      Width = 16
      Height = 21
      Associate = EditColumns
      Min = 1
      Max = 999
      Position = 2
      TabOrder = 4
    end
    object ComboBoxGripper: TComboBox
      Left = 118
      Top = 98
      Width = 235
      Height = 21
      Hint = 'How the items are layed out in the gallery'
      Style = csDropDownList
      DropDownCount = 30
      TabOrder = 5
      OnChange = ComboBoxGripperChange
      Items.Strings = (
        'Default Layout'
        'Vertical Layout'
        'Flow Layout')
    end
  end
end
