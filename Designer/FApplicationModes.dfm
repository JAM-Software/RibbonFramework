object FormApplicationModes: TFormApplicationModes
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Application Modes'
  ClientHeight = 198
  ClientWidth = 387
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 322
    Height = 13
    Caption = 
      'Select 1 or more application modes in which this control is avai' +
      'lable.'
  end
  object Label2: TLabel
    Left = 8
    Top = 27
    Width = 226
    Height = 13
    Caption = 'There are 32 user-definable application modes.'
  end
  object CheckListBoxModes: TCheckListBox
    Left = 8
    Top = 46
    Width = 369
    Height = 113
    Columns = 4
    ItemHeight = 13
    Items.Strings = (
      'Mode 0'
      'Mode 1'
      'Mode 2'
      'Mode 3'
      'Mode 4'
      'Mode 5'
      'Mode 6'
      'Mode 7'
      'Mode 8'
      'Mode 9'
      'Mode 10'
      'Mode 11'
      'Mode 12'
      'Mode 13'
      'Mode 14'
      'Mode 15'
      'Mode 16'
      'Mode 17'
      'Mode 18'
      'Mode 19'
      'Mode 20'
      'Mode 21'
      'Mode 22'
      'Mode 23'
      'Mode 24'
      'Mode 25'
      'Mode 26'
      'Mode 27'
      'Mode 28'
      'Mode 29'
      'Mode 30'
      'Mode 31')
    TabOrder = 0
  end
  object ButtonOk: TButton
    Left = 223
    Top = 165
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object ButtonCancel: TButton
    Left = 304
    Top = 165
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ButtonCheckAll: TButton
    Left = 8
    Top = 165
    Width = 75
    Height = 25
    Caption = 'Check All'
    TabOrder = 3
    OnClick = ButtonCheckAllClick
  end
  object ButtonClearAll: TButton
    Left = 89
    Top = 165
    Width = 75
    Height = 25
    Caption = 'Clear All'
    TabOrder = 4
    OnClick = ButtonClearAllClick
  end
end
