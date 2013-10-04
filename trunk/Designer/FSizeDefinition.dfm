inherited FrameSizeDefinition: TFrameSizeDefinition
  Width = 642
  ExplicitWidth = 642
  inherited ImageSample: TImage
    Left = 534
    ExplicitLeft = 534
  end
  object LabelHeader: TLabel
    Left = 0
    Top = 0
    Width = 642
    Height = 22
    Align = alTop
    AutoSize = False
    Caption = '  Size Definition Properties'
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
    ExplicitLeft = -360
    ExplicitWidth = 680
  end
  object LabelName: TLabel
    Left = 6
    Top = 31
    Width = 27
    Height = 13
    Caption = 'Name'
  end
  object Label1: TLabel
    Left = 6
    Top = 58
    Width = 88
    Height = 13
    Caption = 'Control Name Map'
  end
  object EditName: TEdit
    Left = 120
    Top = 28
    Width = 250
    Height = 21
    Hint = 
      'Name of the size definition. This name is later used when you se' +
      't custom size definitions for a group of controls.'
    TabOrder = 0
    OnChange = EditNameChange
    OnKeyPress = EditNameKeyPress
  end
  object MemoControlNameMap: TMemo
    Left = 120
    Top = 55
    Width = 250
    Height = 78
    Hint = 
      'Specify some (arbitrary) control names that you use later in the' +
      ' control size definitions. Use one control name per line.'
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
    OnChange = MemoControlNameMapChange
    OnExit = MemoControlNameMapExit
  end
end
