object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Gallery'
  ClientHeight = 532
  ClientWidth = 861
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Ribbon: TUIRibbon
    Left = 0
    Top = 0
    Width = 861
    Height = 117
    ResourceName = 'APPLICATION'
    OnLoaded = RibbonLoaded
    ExplicitLeft = 8
    ExplicitTop = 224
  end
end
