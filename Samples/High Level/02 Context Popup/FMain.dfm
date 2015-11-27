object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Click one button in ribbon, then right click in form'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnContextPopup = FormContextPopup
  PixelsPerInch = 96
  TextHeight = 13
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
