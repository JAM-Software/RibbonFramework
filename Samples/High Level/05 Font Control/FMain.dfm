object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Font Control'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object RichEdit: TRichEdit
    Left = 0
    Top = 117
    Width = 635
    Height = 220
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Zoom = 100
    OnContextPopup = RichEditContextPopup
    OnSelectionChange = RichEditSelectionChange
    ExplicitTop = 0
    ExplicitHeight = 337
  end
  object Ribbon: TUIRibbon
    Left = 0
    Top = 0
    Width = 635
    Height = 117
    ResourceName = 'APPLICATION'
    OnLoaded = RibbonLoaded
    ExplicitLeft = 8
    ExplicitTop = 128
  end
end
