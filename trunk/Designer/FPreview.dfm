object FormPreview: TFormPreview
  Left = 0
  Top = 0
  Caption = 'Ribbon Preview'
  ClientHeight = 462
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 635
    Height = 462
    ActivePage = TabSheetColorize
    Align = alTop
    TabOrder = 0
    object TabSheetAppModes: TTabSheet
      Caption = 'Application Modes'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LabelAppModes: TLabel
        Left = 0
        Top = 0
        Width = 198
        Height = 13
        Align = alTop
        Caption = '* There are no application modes defined'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object CheckListBoxAppModes: TCheckListBox
        Left = 0
        Top = 13
        Width = 627
        Height = 421
        OnClickCheck = CheckListBoxAppModesClickCheck
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object TabSheetContextTabs: TTabSheet
      Caption = 'Contextual Tabs'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LabelContextTabs: TLabel
        Left = 0
        Top = 0
        Width = 188
        Height = 13
        Align = alTop
        Caption = '* There are no contextual tabs defined'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object CheckListBoxContextTabs: TCheckListBox
        Left = 0
        Top = 13
        Width = 627
        Height = 421
        OnClickCheck = CheckListBoxContextTabsClickCheck
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object TabSheetContextPopups: TTabSheet
      Caption = 'Context Popups'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LabelContextPopups: TLabel
        Left = 0
        Top = 0
        Width = 182
        Height = 13
        Align = alTop
        Caption = '* There are no context popus defined'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object ListBoxContextPopups: TListBox
        Left = 0
        Top = 13
        Width = 627
        Height = 421
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = ListBoxContextPopupsClick
      end
    end
    object TabSheetColorize: TTabSheet
      Caption = 'Colorize'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBoxSamples: TGroupBox
        Left = 3
        Top = 3
        Width = 242
        Height = 222
        Caption = 'Samples'
        TabOrder = 0
        object Panel1: TPanel
          Left = 16
          Top = 24
          Width = 40
          Height = 40
          Cursor = crHandPoint
          BevelOuter = bvNone
          BorderWidth = 3
          Color = clGreen
          ParentBackground = False
          TabOrder = 0
          OnClick = PanelSampleClick
          object Panel2: TPanel
            Left = 3
            Top = 3
            Width = 34
            Height = 34
            Cursor = crHandPoint
            Align = alClient
            BevelOuter = bvNone
            Caption = 'Xy'
            Color = clLime
            ParentBackground = False
            TabOrder = 0
            OnClick = PanelSampleClick
          end
        end
        object Panel3: TPanel
          Tag = 1
          Left = 72
          Top = 24
          Width = 40
          Height = 40
          Cursor = crHandPoint
          BevelOuter = bvNone
          BorderWidth = 3
          Color = clGreen
          ParentBackground = False
          TabOrder = 1
          OnClick = PanelSampleClick
          object Panel4: TPanel
            Tag = 1
            Left = 3
            Top = 3
            Width = 34
            Height = 34
            Cursor = crHandPoint
            Align = alClient
            BevelOuter = bvNone
            Caption = 'Xy'
            Color = clLime
            ParentBackground = False
            TabOrder = 0
            OnClick = PanelSampleClick
          end
        end
        object Panel5: TPanel
          Tag = 2
          Left = 128
          Top = 24
          Width = 40
          Height = 40
          Cursor = crHandPoint
          BevelOuter = bvNone
          BorderWidth = 3
          Color = clGreen
          ParentBackground = False
          TabOrder = 2
          OnClick = PanelSampleClick
          object Panel6: TPanel
            Tag = 2
            Left = 3
            Top = 3
            Width = 34
            Height = 34
            Cursor = crHandPoint
            Align = alClient
            BevelOuter = bvNone
            Caption = 'Xy'
            Color = clLime
            ParentBackground = False
            TabOrder = 0
            OnClick = PanelSampleClick
          end
        end
        object Panel7: TPanel
          Tag = 3
          Left = 184
          Top = 24
          Width = 40
          Height = 40
          Cursor = crHandPoint
          BevelOuter = bvNone
          BorderWidth = 3
          Color = clGreen
          ParentBackground = False
          TabOrder = 3
          OnClick = PanelSampleClick
          object Panel8: TPanel
            Tag = 3
            Left = 3
            Top = 3
            Width = 34
            Height = 34
            Cursor = crHandPoint
            Align = alClient
            BevelOuter = bvNone
            Caption = 'Xy'
            Color = clLime
            ParentBackground = False
            TabOrder = 0
            OnClick = PanelSampleClick
          end
        end
        object Panel9: TPanel
          Tag = 4
          Left = 16
          Top = 80
          Width = 40
          Height = 40
          Cursor = crHandPoint
          BevelOuter = bvNone
          BorderWidth = 3
          Color = clGreen
          ParentBackground = False
          TabOrder = 4
          OnClick = PanelSampleClick
          object Panel10: TPanel
            Tag = 4
            Left = 3
            Top = 3
            Width = 34
            Height = 34
            Cursor = crHandPoint
            Align = alClient
            BevelOuter = bvNone
            Caption = 'Xy'
            Color = clLime
            ParentBackground = False
            TabOrder = 0
            OnClick = PanelSampleClick
          end
        end
        object Panel11: TPanel
          Tag = 5
          Left = 72
          Top = 80
          Width = 40
          Height = 40
          Cursor = crHandPoint
          BevelOuter = bvNone
          BorderWidth = 3
          Color = clGreen
          ParentBackground = False
          TabOrder = 5
          OnClick = PanelSampleClick
          object Panel12: TPanel
            Tag = 5
            Left = 3
            Top = 3
            Width = 34
            Height = 34
            Cursor = crHandPoint
            Align = alClient
            BevelOuter = bvNone
            Caption = 'Xy'
            Color = clLime
            ParentBackground = False
            TabOrder = 0
            OnClick = PanelSampleClick
          end
        end
        object Panel13: TPanel
          Tag = 6
          Left = 128
          Top = 80
          Width = 40
          Height = 40
          Cursor = crHandPoint
          BevelOuter = bvNone
          BorderWidth = 3
          Color = clGreen
          ParentBackground = False
          TabOrder = 6
          OnClick = PanelSampleClick
          object Panel14: TPanel
            Tag = 6
            Left = 3
            Top = 3
            Width = 34
            Height = 34
            Cursor = crHandPoint
            Align = alClient
            BevelOuter = bvNone
            Caption = 'Xy'
            Color = clLime
            ParentBackground = False
            TabOrder = 0
            OnClick = PanelSampleClick
          end
        end
        object Panel15: TPanel
          Tag = 7
          Left = 184
          Top = 80
          Width = 40
          Height = 40
          Cursor = crHandPoint
          BevelOuter = bvNone
          BorderWidth = 3
          Color = clGreen
          ParentBackground = False
          TabOrder = 7
          OnClick = PanelSampleClick
          object Panel16: TPanel
            Tag = 7
            Left = 3
            Top = 3
            Width = 34
            Height = 34
            Cursor = crHandPoint
            Align = alClient
            BevelOuter = bvNone
            Caption = 'Xy'
            Color = clLime
            ParentBackground = False
            TabOrder = 0
            OnClick = PanelSampleClick
          end
        end
        object Panel17: TPanel
          Tag = 8
          Left = 16
          Top = 136
          Width = 40
          Height = 40
          Cursor = crHandPoint
          BevelOuter = bvNone
          BorderWidth = 3
          Color = clGreen
          ParentBackground = False
          TabOrder = 8
          OnClick = PanelSampleClick
          object Panel18: TPanel
            Tag = 8
            Left = 3
            Top = 3
            Width = 34
            Height = 34
            Cursor = crHandPoint
            Align = alClient
            BevelOuter = bvNone
            Caption = 'Xy'
            Color = clLime
            ParentBackground = False
            TabOrder = 0
            OnClick = PanelSampleClick
          end
        end
        object Panel19: TPanel
          Tag = 9
          Left = 72
          Top = 136
          Width = 40
          Height = 40
          Cursor = crHandPoint
          BevelOuter = bvNone
          BorderWidth = 3
          Color = clGreen
          ParentBackground = False
          TabOrder = 9
          OnClick = PanelSampleClick
          object Panel20: TPanel
            Tag = 9
            Left = 3
            Top = 3
            Width = 34
            Height = 34
            Cursor = crHandPoint
            Align = alClient
            BevelOuter = bvNone
            Caption = 'Xy'
            Color = clLime
            ParentBackground = False
            TabOrder = 0
            OnClick = PanelSampleClick
          end
        end
        object Panel21: TPanel
          Tag = 10
          Left = 128
          Top = 136
          Width = 40
          Height = 40
          Cursor = crHandPoint
          BevelOuter = bvNone
          BorderWidth = 3
          Color = clGreen
          ParentBackground = False
          TabOrder = 10
          OnClick = PanelSampleClick
          object Panel22: TPanel
            Tag = 10
            Left = 3
            Top = 3
            Width = 34
            Height = 34
            Cursor = crHandPoint
            Align = alClient
            BevelOuter = bvNone
            Caption = 'Xy'
            Color = clLime
            ParentBackground = False
            TabOrder = 0
            OnClick = PanelSampleClick
          end
        end
        object Panel23: TPanel
          Tag = 11
          Left = 184
          Top = 136
          Width = 40
          Height = 40
          Cursor = crHandPoint
          BevelOuter = bvNone
          BorderWidth = 3
          Color = clGreen
          ParentBackground = False
          TabOrder = 11
          OnClick = PanelSampleClick
          object Panel24: TPanel
            Tag = 11
            Left = 3
            Top = 3
            Width = 34
            Height = 34
            Cursor = crHandPoint
            Align = alClient
            BevelOuter = bvNone
            Caption = 'Xy'
            Color = clLime
            ParentBackground = False
            TabOrder = 0
            OnClick = PanelSampleClick
          end
        end
      end
      object GroupBoxBackgroundColor: TGroupBox
        Left = 251
        Top = 3
        Width = 206
        Height = 70
        Caption = 'Ribbon Background Color'
        TabOrder = 1
        object Label1: TLabel
          Left = 8
          Top = 48
          Width = 19
          Height = 13
          Caption = 'Hue'
        end
        object Label2: TLabel
          Left = 76
          Top = 48
          Width = 50
          Height = 13
          Caption = 'Saturation'
        end
        object Label3: TLabel
          Left = 144
          Top = 48
          Width = 50
          Height = 13
          Caption = 'Brightness'
        end
        object EditBkH: TEdit
          Left = 8
          Top = 24
          Width = 37
          Height = 21
          TabOrder = 0
          Text = '0'
          OnChange = EditBackgroundColorChange
        end
        object UpDownBkH: TUpDown
          Left = 45
          Top = 24
          Width = 16
          Height = 21
          Associate = EditBkH
          Max = 255
          TabOrder = 1
        end
        object EditBkS: TEdit
          Left = 76
          Top = 24
          Width = 37
          Height = 21
          TabOrder = 2
          Text = '0'
          OnChange = EditBackgroundColorChange
        end
        object UpDownBkS: TUpDown
          Left = 113
          Top = 24
          Width = 16
          Height = 21
          Associate = EditBkS
          Max = 255
          TabOrder = 3
        end
        object EditBkB: TEdit
          Left = 144
          Top = 24
          Width = 37
          Height = 21
          TabOrder = 4
          Text = '0'
          OnChange = EditBackgroundColorChange
        end
        object UpDownBkB: TUpDown
          Left = 181
          Top = 24
          Width = 16
          Height = 21
          Associate = EditBkB
          Max = 255
          TabOrder = 5
        end
      end
      object GroupBoxHighlightColor: TGroupBox
        Left = 251
        Top = 79
        Width = 206
        Height = 70
        Caption = 'Ribbon Highlight Color'
        TabOrder = 2
        object Label4: TLabel
          Left = 8
          Top = 48
          Width = 19
          Height = 13
          Caption = 'Hue'
        end
        object Label5: TLabel
          Left = 76
          Top = 48
          Width = 50
          Height = 13
          Caption = 'Saturation'
        end
        object Label6: TLabel
          Left = 144
          Top = 48
          Width = 50
          Height = 13
          Caption = 'Brightness'
        end
        object EditHiH: TEdit
          Left = 8
          Top = 24
          Width = 37
          Height = 21
          TabOrder = 0
          Text = '0'
          OnChange = EditHighlightColorChange
        end
        object UpDownHiH: TUpDown
          Left = 45
          Top = 24
          Width = 16
          Height = 21
          Associate = EditHiH
          Max = 255
          TabOrder = 1
        end
        object EditHiS: TEdit
          Left = 76
          Top = 24
          Width = 37
          Height = 21
          TabOrder = 2
          Text = '0'
          OnChange = EditHighlightColorChange
        end
        object UpDownHiS: TUpDown
          Left = 113
          Top = 24
          Width = 16
          Height = 21
          Associate = EditHiS
          Max = 255
          TabOrder = 3
        end
        object EditHiB: TEdit
          Left = 144
          Top = 24
          Width = 37
          Height = 21
          TabOrder = 4
          Text = '0'
          OnChange = EditHighlightColorChange
        end
        object UpDownHiB: TUpDown
          Left = 181
          Top = 24
          Width = 16
          Height = 21
          Associate = EditHiB
          Max = 255
          TabOrder = 5
        end
      end
      object GroupBoxTextColor: TGroupBox
        Left = 251
        Top = 155
        Width = 206
        Height = 70
        Caption = 'Ribbon Text Color'
        TabOrder = 3
        object Label7: TLabel
          Left = 8
          Top = 48
          Width = 19
          Height = 13
          Caption = 'Hue'
        end
        object Label8: TLabel
          Left = 76
          Top = 48
          Width = 50
          Height = 13
          Caption = 'Saturation'
        end
        object Label9: TLabel
          Left = 144
          Top = 48
          Width = 50
          Height = 13
          Caption = 'Brightness'
        end
        object EditTextH: TEdit
          Left = 8
          Top = 24
          Width = 37
          Height = 21
          TabOrder = 0
          Text = '0'
          OnChange = EditTextColorChange
        end
        object UpDownTextH: TUpDown
          Left = 45
          Top = 24
          Width = 16
          Height = 21
          Associate = EditTextH
          Max = 255
          TabOrder = 1
        end
        object EditTextS: TEdit
          Left = 76
          Top = 24
          Width = 37
          Height = 21
          TabOrder = 2
          Text = '0'
          OnChange = EditTextColorChange
        end
        object UpDownTextS: TUpDown
          Left = 113
          Top = 24
          Width = 16
          Height = 21
          Associate = EditTextS
          Max = 255
          TabOrder = 3
        end
        object EditTextB: TEdit
          Left = 144
          Top = 24
          Width = 37
          Height = 21
          TabOrder = 4
          Text = '0'
          OnChange = EditTextColorChange
        end
        object UpDownTextB: TUpDown
          Left = 181
          Top = 24
          Width = 16
          Height = 21
          Associate = EditTextB
          Max = 255
          TabOrder = 5
        end
      end
    end
  end
end
