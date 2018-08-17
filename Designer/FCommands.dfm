object FrameCommands: TFrameCommands
  Left = 0
  Top = 0
  Width = 451
  Height = 305
  Align = alClient
  TabOrder = 0
  object SplitterCommands: TSplitter
    Left = 281
    Top = 0
    Height = 305
    ResizeStyle = rsUpdate
  end
  object PanelCommands: TPanel
    Left = 0
    Top = 0
    Width = 281
    Height = 305
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object ToolBarCommands: TToolBar
      Left = 0
      Top = 0
      Width = 281
      Height = 22
      AutoSize = True
      ButtonWidth = 60
      Caption = 'ToolBarCommands'
      Color = clBtnFace
      Images = ImageListToolbars
      List = True
      ParentColor = False
      ShowCaptions = True
      TabOrder = 0
      Transparent = False
      object ButtonAddCommand: TToolButton
        Left = 0
        Top = 0
        Action = ActionAddCommand
        AutoSize = True
      end
      object ButtonDeleteCommand: TToolButton
        Left = 50
        Top = 0
        Action = ActionDeleteCommand
        AutoSize = True
      end
      object ButtonMoveUp: TToolButton
        Left = 112
        Top = 0
        Action = ActionMoveUp
        AutoSize = True
      end
      object ButtonMoveDown: TToolButton
        Left = 156
        Top = 0
        Action = ActionMoveDown
        AutoSize = True
      end
      object ButtonSearchCommand: TToolButton
        Left = 214
        Top = 0
        Action = ActionSearchCommand
      end
    end
    object ListViewCommands: TListView
      Left = 0
      Top = 22
      Width = 281
      Height = 283
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 100
        end
        item
          AutoSize = True
          Caption = 'Caption'
        end>
      GridLines = True
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      PopupMenu = PopupMenuList
      TabOrder = 1
      ViewStyle = vsReport
      OnColumnClick = ListViewCommandsColumnClick
      OnCompare = ListViewCommandsCompare
      OnSelectItem = ListViewCommandsSelectItem
    end
  end
  object PanelCommandProperties: TPanel
    Left = 284
    Top = 0
    Width = 167
    Height = 305
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    OnResize = PanelCommandPropertiesResize
    object LabelHeader: TLabel
      Left = 0
      Top = 0
      Width = 167
      Height = 22
      Align = alTop
      AutoSize = False
      Caption = '  Command Properties'
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
    end
    object PanelProps: TPanel
      Left = 0
      Top = 22
      Width = 167
      Height = 256
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 6
        Top = 14
        Width = 27
        Height = 13
        Caption = 'Name'
      end
      object Label2: TLabel
        Left = 6
        Top = 176
        Width = 34
        Height = 13
        Caption = 'Symbol'
      end
      object Label3: TLabel
        Left = 6
        Top = 203
        Width = 11
        Height = 13
        Caption = 'ID'
      end
      object Label4: TLabel
        Left = 6
        Top = 230
        Width = 45
        Height = 13
        Caption = 'Comment'
      end
      object Label5: TLabel
        Left = 6
        Top = 41
        Width = 37
        Height = 13
        Caption = 'Caption'
      end
      object Label8: TLabel
        Left = 6
        Top = 68
        Width = 53
        Height = 13
        Caption = 'Description'
      end
      object Label11: TLabel
        Left = 6
        Top = 95
        Width = 55
        Height = 13
        Caption = 'Tooltip Title'
      end
      object Label14: TLabel
        Left = 6
        Top = 122
        Width = 88
        Height = 13
        Caption = 'Tooltip Description'
      end
      object Label17: TLabel
        Left = 6
        Top = 149
        Width = 30
        Height = 13
        Caption = 'Keytip'
      end
      object Label18: TLabel
        Left = 271
        Top = 149
        Width = 11
        Height = 13
        Caption = 'ID'
      end
      object Label15: TLabel
        Left = 271
        Top = 122
        Width = 11
        Height = 13
        Caption = 'ID'
      end
      object Label13: TLabel
        Left = 271
        Top = 95
        Width = 11
        Height = 13
        Caption = 'ID'
      end
      object Label9: TLabel
        Left = 271
        Top = 68
        Width = 11
        Height = 13
        Caption = 'ID'
      end
      object Label6: TLabel
        Left = 271
        Top = 41
        Width = 11
        Height = 13
        Caption = 'ID'
      end
      object Label19: TLabel
        Left = 377
        Top = 149
        Width = 34
        Height = 13
        Caption = 'Symbol'
      end
      object Label16: TLabel
        Left = 377
        Top = 122
        Width = 34
        Height = 13
        Caption = 'Symbol'
      end
      object Label12: TLabel
        Left = 377
        Top = 95
        Width = 34
        Height = 13
        Caption = 'Symbol'
      end
      object Label10: TLabel
        Left = 377
        Top = 68
        Width = 34
        Height = 13
        Caption = 'Symbol'
      end
      object Label7: TLabel
        Left = 377
        Top = 41
        Width = 34
        Height = 13
        Caption = 'Symbol'
      end
      object EditKeytip: TEdit
        Left = 99
        Top = 146
        Width = 166
        Height = 21
        Hint = 
          'The keytip for the command. This is key sequence that is shown w' +
          'hen the user pressed the Alt kep to access ribbon controls.'
        TabOrder = 17
        OnChange = EditKeytipChange
      end
      object EditTooltipDescription: TEdit
        Left = 99
        Top = 119
        Width = 166
        Height = 21
        Hint = 
          'The tooltip description for the command. (Is displayed below the' +
          ' tooltip title in the tooltip popup)'
        TabOrder = 13
        OnChange = EditTooltipDescriptionChange
      end
      object EditTooltipTitle: TEdit
        Left = 99
        Top = 92
        Width = 166
        Height = 21
        Hint = 
          'The tooltip title for the command. (This is the bold caption of ' +
          'the tooltip)'
        TabOrder = 9
        OnChange = EditTooltipTitleChange
      end
      object EditDescription: TEdit
        Left = 99
        Top = 65
        Width = 166
        Height = 21
        Hint = 
          'The label description for the command. Is used when the command ' +
          'is displayed in the application menu.'
        TabOrder = 5
        OnChange = EditDescriptionChange
      end
      object EditCaption: TEdit
        Left = 99
        Top = 38
        Width = 166
        Height = 21
        Hint = 'The caption/label title for the command.'
        TabOrder = 1
        OnChange = EditCaptionChange
      end
      object EditComment: TEdit
        Left = 99
        Top = 227
        Width = 484
        Height = 21
        Hint = 
          'This text is placed as a comment in the Delphi unit containing t' +
          'he constant for this command.'
        TabOrder = 24
        OnChange = EditCommentChange
      end
      object EditId: TEdit
        Left = 99
        Top = 200
        Width = 166
        Height = 21
        Hint = 
          'A unique numeric identifier for the command (the value of the Sy' +
          'mbol constant). Use 0 for auto-generated identifiers.'
        TabOrder = 22
        Text = '0'
        OnChange = EditIdChange
      end
      object EditSymbol: TEdit
        Left = 99
        Top = 173
        Width = 166
        Height = 21
        Hint = 
          'This is the name of the constant that will be generated to acces' +
          's this command. If not specified, the command Name is used.'
        TabOrder = 21
        OnChange = EditSymbolChange
        OnKeyPress = EditNameKeyPress
      end
      object EditName: TEdit
        Left = 99
        Top = 11
        Width = 166
        Height = 21
        Hint = 
          'The command name is used to connect commands with controls. Unle' +
          'ss you specify a Symbol name, this name is also used as the name' +
          ' of the constant for this command.'
        TabOrder = 0
        OnChange = EditNameChange
        OnKeyPress = EditNameKeyPress
      end
      object UpDownId: TUpDown
        Left = 265
        Top = 200
        Width = 16
        Height = 21
        Associate = EditId
        Max = 59999
        TabOrder = 23
        Thousands = False
        OnChangingEx = UpDownChangingEx
      end
      object EditCaptionId: TEdit
        Left = 295
        Top = 38
        Width = 57
        Height = 21
        Hint = 
          'Numeric resource string identifier for the caption. Use 0 for au' +
          'to-generated identifiers.'
        TabOrder = 2
        Text = '0'
        OnChange = EditCaptionIdChange
      end
      object EditDescriptionId: TEdit
        Left = 295
        Top = 65
        Width = 57
        Height = 21
        Hint = 
          'Numeric resource string identifier for the description. Use 0 fo' +
          'r auto-generated identifiers.'
        TabOrder = 6
        Text = '0'
        OnChange = EditDescriptionIdChange
      end
      object EditTooltipTitleId: TEdit
        Left = 295
        Top = 92
        Width = 57
        Height = 21
        Hint = 
          'Numeric resource string identifier for the tooltip title. Use 0 ' +
          'for auto-generated identifiers.'
        TabOrder = 10
        Text = '0'
        OnChange = EditTooltipTitleIdChange
      end
      object EditTooltipDescriptionId: TEdit
        Left = 295
        Top = 119
        Width = 57
        Height = 21
        Hint = 
          'Numeric resource string identifier for the tooltip description. ' +
          'Use 0 for auto-generated identifiers.'
        TabOrder = 14
        Text = '0'
        OnChange = EditTooltipDescriptionIdChange
      end
      object EditKeytipId: TEdit
        Left = 295
        Top = 146
        Width = 57
        Height = 21
        Hint = 
          'Numeric resource string identifier for the keytip. Use 0 for aut' +
          'o-generated identifiers.'
        TabOrder = 18
        Text = '0'
        OnChange = EditKeytipIdChange
      end
      object UpDownCaptionId: TUpDown
        Left = 352
        Top = 38
        Width = 16
        Height = 21
        Associate = EditCaptionId
        Max = 59999
        TabOrder = 3
        Thousands = False
        OnChangingEx = UpDownChangingEx
      end
      object UpDownDescriptionId: TUpDown
        Left = 352
        Top = 65
        Width = 16
        Height = 21
        Associate = EditDescriptionId
        Max = 59999
        TabOrder = 7
        Thousands = False
        OnChangingEx = UpDownChangingEx
      end
      object UpDownTooltipTitleId: TUpDown
        Left = 352
        Top = 92
        Width = 16
        Height = 21
        Associate = EditTooltipTitleId
        Max = 59999
        TabOrder = 11
        Thousands = False
        OnChangingEx = UpDownChangingEx
      end
      object UpDownTooltipDescriptionId: TUpDown
        Left = 352
        Top = 119
        Width = 16
        Height = 21
        Associate = EditTooltipDescriptionId
        Max = 59999
        TabOrder = 15
        Thousands = False
        OnChangingEx = UpDownChangingEx
      end
      object UpDownKeyTipId: TUpDown
        Left = 352
        Top = 146
        Width = 16
        Height = 21
        Associate = EditKeytipId
        Max = 59999
        TabOrder = 19
        Thousands = False
        OnChangingEx = UpDownChangingEx
      end
      object EditCaptionSymbol: TEdit
        Left = 417
        Top = 38
        Width = 166
        Height = 21
        Hint = 
          'Constant name for the resource identifier. If not specified, it ' +
          'is automatically generated.'
        TabOrder = 4
        OnChange = EditCaptionSymbolChange
      end
      object EditDescriptionSymbol: TEdit
        Left = 417
        Top = 65
        Width = 166
        Height = 21
        Hint = 
          'Constant name for the resource identifier. If not specified, it ' +
          'is automatically generated.'
        TabOrder = 8
        OnChange = EditDescriptionSymbolChange
      end
      object EditTooltipTitleSymbol: TEdit
        Left = 417
        Top = 92
        Width = 166
        Height = 21
        Hint = 
          'Constant name for the resource identifier. If not specified, it ' +
          'is automatically generated.'
        TabOrder = 12
        OnChange = EditTooltipTitleSymbolChange
      end
      object EditTooltipDescriptionSymbol: TEdit
        Left = 417
        Top = 119
        Width = 166
        Height = 21
        Hint = 
          'Constant name for the resource identifier. If not specified, it ' +
          'is automatically generated.'
        TabOrder = 16
        OnChange = EditTooltipDescriptionSymbolChange
      end
      object EditKeyTipSymbol: TEdit
        Left = 417
        Top = 146
        Width = 166
        Height = 21
        Hint = 
          'Constant name for the resource identifier. If not specified, it ' +
          'is automatically generated.'
        TabOrder = 20
        OnChange = EditKeyTipSymbolChange
      end
      object BtnGenerateID: TButton
        Left = 295
        Top = 200
        Width = 116
        Height = 21
        Caption = 'Generate unused ID'
        TabOrder = 25
        OnClick = BtnGenerateIDClick
      end
    end
    object PanelImages: TPanel
      Left = 0
      Top = 278
      Width = 167
      Height = 47
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      OnResize = PanelImagesResize
      object PanelSmallImages: TPanel
        Left = 0
        Top = 0
        Width = 288
        Height = 47
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object Label20: TLabel
          Left = 0
          Top = 0
          Width = 288
          Height = 20
          Align = alTop
          AutoSize = False
          Caption = '  Small Images'
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
        end
      end
      object PanelLargeImages: TPanel
        Left = 288
        Top = 0
        Width = 583
        Height = 47
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object Label21: TLabel
          Left = 0
          Top = 0
          Width = 583
          Height = 20
          Align = alTop
          AutoSize = False
          Caption = '  Large Images'
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
        end
      end
    end
    object PanelHighContrastImages: TPanel
      Left = 0
      Top = 325
      Width = 167
      Height = 236
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      OnResize = PanelHighContrastImagesResize
      object PanelSmallHCImages: TPanel
        Left = 0
        Top = 0
        Width = 288
        Height = 236
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object Label22: TLabel
          Left = 0
          Top = 0
          Width = 288
          Height = 20
          Align = alTop
          AutoSize = False
          Caption = '  Small High-Contrast Images'
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
        end
      end
      object PanelLargeHCImages: TPanel
        Left = 288
        Top = 0
        Width = 583
        Height = 236
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object Label23: TLabel
          Left = 0
          Top = 0
          Width = 583
          Height = 20
          Align = alTop
          AutoSize = False
          Caption = '  Large High-Contrast Images'
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
        end
      end
    end
  end
  object ImageListToolbars: TImageList
    ColorDepth = cd32Bit
    Left = 32
    Top = 60
    Bitmap = {
      494C010105000900040010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      00000000000000000000000000000000000000000000D6D5D300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D7D5D3006E685E00ACA59B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B6B0A7008E847400ACA5
      9B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B9B2A9008E84
      7400ACA59B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B9B2
      A9008E847400A8A2980000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B2ADA400756F6600C2C0BF00DFDDD800B0ACA000B5AEA000B4AEA000B0AC
      A000DFDDD8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E2E0DC00A09A8F00B3A99400F0E1CA00FFF0DB00FFF2E000F0E7
      D800B3AC9C00C6C3BB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E1DFDA00AFA48C00FFE9CA00FFEBCF00FFEDD400FFEFDA00FFF1
      DF00FFF4E500B2AC9C00DFDDD800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B3AFA300EFD7B600FFE6C400FFE8C800FFEACE00FFECD300FFEF
      D800FFF1DD00F0E6D500B0ACA000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B6AD9C00FFE5C100FFE5C100FFE6C300FFE7C700FFEACC00FFEC
      D100FFEED700FFF0DC00B4AD9F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B8AF9F00FFE6C500FFE5C100FFE5C000FFE5C200FFE7C600FFE9
      CB00FFEBD000FFEDD500B4AD9F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B9B3A700ECD9BD00FFE7C500FFE5C100FFE5C000FFE5C000FFE6
      C400FFE8C900F0DFC400B0ACA000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E6E4E000AEA69000FEE9CD00FFE7C500FFE5C100FFE4BF00FFE4
      BF00FFE6C300B2A89100DFDDD800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000D0CDC500AEA69000EDDABE00FFE6C500FFE5C000EFD7
      B300AFA38B00C7C3BB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000E6E4E000B9B3A700B9AF9F00B7AE9C00B3AF
      A300E1DFDA000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000004590ABC07A612FF07A612FF07A612FF07A612FF04590ABC0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000281B0A77B77C2DFFB77C2DFFB77C2DFFB77C2DFFB77C2DFF291D0C770000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000170E045DB37728FF1A12075F00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000007A612FF52D56CFF47CB5CFF47C14EFF47BD4BFF07A612FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B37728FFE9C385FFE2B877FFDFB576FFDCB273FFD5AB70FDB77C2DFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000170E045DB37728FFE9C997FFB27525FF1A12075F000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000007A612FF4ED76CFF11C534FF00B71CFF47C857FF07A612FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B37728FFE0A133FFD38D16FFCE8511FFC97E0FFFD4AB71FAB77C2DFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000170E045DB37728FFE4B25BFFD58D16FFE8C790FFB27525FF1A12075F0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000007A612FF60DF81FF28D04EFF11C534FF4DD065FF07A612FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B37728FFE8AC3FFFDC9821FFD58E14FFD08813FFD8B073FAB77C2DFF0000
      000000000000000000000000000000000000000000000000000000000000170F
      045DB37728FFEABA60FFDE9A1DFFDA951AFFD58E17FFE8C791FFB87D2EFF1911
      065D000000000000000000000000000000000000000000000000000000000000
      00000000000007A612FF74E592FF44DA6AFF28D04EFF5DDA79FF07A612FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B37728FFECB345FFE2A22DFFDB9519FFD69017FFDBB476FAB77C2DFF0000
      0000000000000000000000000000000000000000000000000000180F045FB377
      28FFEFC164FFE8A522FFE3A121FFE09B1FFFDB951BFFD58F18FFE8C791FFB87D
      2EFF1911065D000000000000000000000000025306BC059E0DFF059E0DFF059E
      0DFF059E0DFF07A612FF83E9A0FF5AE17EFF40D967FF6CE188FF07A612FF07A6
      12FF07A612FF07A612FF07A612FF04590ABC0A2B52A8216CC8FF216CC8FF216C
      C8FF216CC8FF216CC8FF216CC8FF216CC8FF216CC8FF216CC8FF216CC8FF216C
      C8FF216CC8FF216CC8FF216CC8FF0E2F57A80000000000000000000000000000
      0000B37728FFF1B947FFE9AC39FFE29D1FFFDD971BFFDEB979FAB77C2DFF0000
      00000000000000000000000000000000000000000000170F045EB37728FFF4CD
      7AFFEEAE2BFFEBAA25FFE9A624FFE6A222FFE09C1EFFDC961BFFD79018FFE9C7
      91FFB87D2EFF1911065D0000000000000000049A0BFF70E38EFF75E695FF89EA
      A5FF91EDADFF95EDAEFF8CEBA7FF67E48CFF57E07CFF76E493FF6CE188FF5FDB
      79FF51D36AFF47CB5BFF47C14EFF07A612FF1763BEFF6EABFFFF74B1FFFF78B7
      FFFF6DB3FFFF6BB1FFFF6CB1FFFF6CAEFFFF6BA9FFFF68A3FFFF649CFFFF5F92
      FDFF5988F7FF537BF1FF4C70F1FF216CC8FF0000000000000000000000000000
      0000B37728FFF4BD49FFEEB544FFE8A729FFE39E1EFFE2BE7AFAB77C2DFF0000
      000000000000000000000000000000000000170F045EB37728FFF6D080FFF2BC
      49FFF0B331FFEEAE26FFECAB27FFE9A725FFE6A222FFE4A637FFE9BF72FFE6BB
      6FFFE9C891FFB87D2EFF1911065D00000000049A0BFF54DA73FF32D558FF48DC
      6EFF60E385FF72E694FF75E898FF72E694FF67E48CFF57E07CFF44DA6AFF30D3
      57FF1AC93FFF08BF2AFF47C857FF07A612FF1763BEFF68A3FFFF4B95FFFF529E
      FFFF56A5FFFF55A5FFFF50A2FFFF4A9EFFFF489AFFFF4593FFFF428BFFFF3D81
      FFFF3774F9FF2F67F2FF537BF5FF216CC8FF0000000000000000000000000000
      0000B37728FFF4BE4AFFF1BA4AFFEDB136FFE8A522FFE3C07DF9B77C2DFF0000
      000000000000000000000000000000000000492D09AAA56515FFA56515FFA565
      15FFA56515FFF0AF27FFEFAE28FFECAB27FFEAA724FFF0C575FFB87D2EFFB87D
      2EFFB87D2EFFB87D2EFF4E3310AA00000000049A0BFF49D567FF1ECC44FF30D3
      57FF46DB6CFF5EE283FF6EE691FF7DEA9DFF7DEA9DFF75E898FF63E388FF4EDE
      74FF36D65EFF20CD45FF4ED267FF07A612FF1763BEFF629BFEFF4188FDFF4892
      FFFF519CFFFF58A5FFFF5CA9FFFF5BA9FFFF58A6FFFF52A0FFFF4A97FFFF438E
      FFFF3E83FEFF3776F9FF5989FCFF216CC8FF492D09AA825115E0AF7121FFAF71
      21FFAF7121FFF2BC49FFF2BC4BFFF0B743FFEDAB29FFEEC981FDB77C2DFFB77C
      2DFFB77C2DFF9A6723EC4E3310AA000000000000000000000000000000000000
      0000A56515FFF0B02AFFF0B028FFEFAF29FFEDAB26FFF2C978FFB87D2EFF0000
      000000000000000000000000000000000000049A0BFF41D15EFF42D15FFF4BD6
      69FF59DC78FF69E189FF78E697FF6BE58EFF79E99AFF9AEEB4FF9EEFB6FF95ED
      AEFF83E9A0FF71E591FF5CDD7CFF07A612FF1763BEFF5D95FBFF6199FAFF649F
      FDFF6CA7FFFF72ADFFFF77B4FFFF7CB9FFFF81BCFFFF82BCFFFF82BEFFFF7CB6
      FFFF78AEFFFF73A6FFFF70A1FFFF216CC8FF170E045CA56515FFEFC879FFEDB7
      46FFEFB947FFF2BB49FFF2BD4AFFF2BC4BFFF0B335FFF4CF83FFF2CA7CFFF0C7
      7AFFEDC579FFB87D2EFF1810065C000000000000000000000000000000000000
      0000A56515FFEFB12DFFF0AF28FFF0B029FFF0AF29FFF4CC79FFB87D2EFF0000
      000000000000000000000000000000000000025306BC049A0BFF049A0BFF049A
      0BFF049A0BFF049A0BFF60DF81FF57E07CFF6BE58EFFA1F0B9FF059E0DFF059E
      0DFF059E0DFF059E0DFF07A612FF04590ABC0A2B52A81763BEFF1763BEFF1763
      BEFF1763BEFF1763BEFF1763BEFF1763BEFF1763BEFF1763BEFF216CC8FF216C
      C8FF216CC8FF216CC8FF216CC8FF0E2F57A800000000170E045CA56515FFEDC6
      79FFEBB444FFEEB846FFF1BB48FFF2BD4BFFF2BA43FFF0AF2AFFEDAB26FFF2CB
      7DFFB87D2EFF1810065C00000000000000000000000000000000000000000000
      0000A56515FFEDAF30FFEEAE26FFF0B029FFF0B029FFF6CD7AFFB87D2EFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000049A0BFF4FD86DFF3ED964FF53E07AFF95EDAEFF049A0BFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000170E045CA565
      15FFEDC678FFEAB344FFEDB746FFEFBA48FFF2BC4BFFF2B436FFF6CF7DFFB87D
      2EFF1810065C0000000000000000000000000000000000000000000000000000
      0000A56515FFEAB037FFEBAB25FFEEAE28FFEFAF29FFF6CE7AFFB87D2EFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000049A0BFF44D261FF25CF4CFF39D760FF81E99DFF049A0BFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000170E
      045CA56515FFECC478FFE9B244FFECB645FFF0BA49FFF7D690FFB87D2EFF1810
      065C000000000000000000000000000000000000000000000000000000000000
      0000A56515FFE6AF3DFFE7A726FFEBAA25FFEEAE27FFF6CF79FFB87D2EFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000049A0BFF3CCD57FF17C83CFF23CE49FF69E189FF049A0BFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000170E045CA56515FFEBC377FFE8B143FFF4D391FFB27525FF1810065C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A56515FFE2AB40FFE3A62BFFE6A622FFEBAB26FFF4CD79FFB87D2EFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000049A0BFF37CA50FF3FD05CFF47D464FF59DC78FF049A0BFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000170E045CA56515FFEAC376FFB27525FF1810065C000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A56515FFE4B867FFE7B75EFFE8B44EFFECB951FFF3CC79FFB87D2EFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000025306BC049A0BFF049A0BFF049A0BFF049A0BFF025306BC0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000170E045CA56515FF1810065C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000281B0A77A56515FFA56515FFA56515FFA56515FFA56515FF291D0C770000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF00BFFF0000000000001FFF000000000000
      8FFF000000000000C7FF000000000000E3FF000000000000F007000000000000
      F803000000000000F801000000000000F801000000000000F801000000000000
      F801000000000000F801000000000000F801000000000000FC03000000000000
      FE07000000000000FFFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object ActionList: TActionList
    Images = ImageListToolbars
    Left = 32
    Top = 116
    object ActionAddCommand: TAction
      Category = 'Commands'
      Caption = 'Add'
      Hint = 'Adds a new command'
      ImageIndex = 0
      ShortCut = 24621
      OnExecute = ActionAddCommandExecute
      OnUpdate = ActionAddCommandUpdate
    end
    object ActionDeleteCommand: TAction
      Category = 'Commands'
      Caption = 'Delete'
      Hint = 'Deletes the selected command'
      ImageIndex = 1
      OnExecute = ActionDeleteCommandExecute
      OnUpdate = ActionUpdate
    end
    object ActionMoveUp: TAction
      Category = 'Commands'
      Caption = 'Up'
      Hint = 'Moves the selected command up in the list'
      ImageIndex = 2
      OnExecute = ActionMoveUpExecute
      OnUpdate = ActionUpdate
    end
    object ActionMoveDown: TAction
      Category = 'Commands'
      Caption = 'Down'
      Hint = 'Moves the selected command down in the list'
      ImageIndex = 3
      OnExecute = ActionMoveDownExecute
      OnUpdate = ActionUpdate
    end
    object ActionSearchCommand: TAction
      Category = 'Commands'
      Caption = 'Search'
      ImageIndex = 4
      ShortCut = 16454
      OnExecute = ActionSearchCommandExecute
      OnUpdate = ActionUpdate
    end
  end
  object PopupMenuList: TPopupMenu
    Images = ImageListToolbars
    Left = 36
    Top = 176
    object MenuAddCommand: TMenuItem
      Action = ActionAddCommand
      Caption = 'Add Command'
    end
    object MenuDeleteCommand: TMenuItem
      Action = ActionDeleteCommand
      Caption = 'Delete Command'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MenuMoveUp: TMenuItem
      Action = ActionMoveUp
    end
    object MenuMoveDown: TMenuItem
      Action = ActionMoveDown
    end
  end
end
