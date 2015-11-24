object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Text Pad'
  ClientHeight = 535
  ClientWidth = 783
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 516
    Width = 783
    Height = 19
    Panels = <>
  end
  object RichEdit: TRichEdit
    AlignWithMargins = True
    Left = 40
    Top = 137
    Width = 703
    Height = 379
    Margins.Left = 40
    Margins.Top = 20
    Margins.Right = 40
    Margins.Bottom = 0
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Calibri'
    Font.Style = []
    HideSelection = False
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
    OnChange = RichEditChange
    OnContextPopup = RichEditContextPopup
    OnSelectionChange = RichEditSelectionChange
  end
  object Ribbon: TUIRibbon
    Left = 0
    Top = 0
    Width = 783
    Height = 117
    ResourceName = 'APPLICATION'
    OnCommandCreate = CommandCreated
    OnLoaded = RibbonLoaded
    ExplicitLeft = 8
    ExplicitTop = 232
  end
  object FindDialog: TFindDialog
    Options = [frDown, frHideUpDown, frDisableUpDown]
    OnFind = FindDialogFind
    Left = 56
    Top = 32
  end
  object PrintDialog: TPrintDialog
    Left = 120
    Top = 32
  end
end
