object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'DropDown Color Picker'
  ClientHeight = 562
  ClientWidth = 551
  Color = clBtnFace
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
    Width = 551
    Height = 117
    ResourceName = 'APPLICATION'
    Options = []
    OnLoaded = RibbonLoaded
  end
end
