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
    Top = 20
    Width = 703
    Height = 496
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
  object Actions: TActionList
    Left = 64
    Top = 28
    object ActionCut: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 0
      ShortCut = 16472
    end
    object ActionCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 1
      ShortCut = 16451
    end
    object ActionPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 2
      ShortCut = 16470
    end
    object ActionNotImplemented: TAction
      OnExecute = ActionNotImplementedExecute
    end
    object ActionIndent: TAction
      OnExecute = ActionIndentOutdentExecute
    end
    object ActionOutdent: TAction
      OnExecute = ActionIndentOutdentExecute
    end
    object ActionList: TAction
      OnExecute = ActionListExecute
    end
    object ActionLineSpacing10: TAction
      OnExecute = ActionLineSpacingExecute
    end
    object ActionLineSpacing115: TAction
      OnExecute = ActionLineSpacingExecute
    end
    object ActionLineSpacing15: TAction
      OnExecute = ActionLineSpacingExecute
    end
    object ActionLineSpacing20: TAction
      OnExecute = ActionLineSpacingExecute
    end
    object ActionSpaceAfter: TAction
      OnExecute = ActionSpaceAfterExecute
    end
    object ActionAlignLeft: TAction
      OnExecute = ActionAlignExecute
    end
    object ActionAlignCenter: TAction
      OnExecute = ActionAlignExecute
    end
    object ActionAlignRight: TAction
      OnExecute = ActionAlignExecute
    end
    object ActionAlignJustify: TAction
      Caption = 'ActionAlignJustify'
      OnExecute = ActionAlignExecute
    end
    object ActionFind: TSearchFind
      Category = 'Search'
      Caption = '&Find...'
      Hint = 'Find|Finds the specified text'
      ImageIndex = 34
      ShortCut = 16454
      OnAccept = ActionFindAccept
    end
    object ActionSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object ActionUndo: TAction
      OnExecute = ActionUndoExecute
    end
    object ActionRedo: TAction
      OnExecute = ActionRedoExecute
    end
    object ActionPrint: TPrintDlg
      Category = 'Dialog'
      Caption = '&Print...'
      ImageIndex = 14
      ShortCut = 16464
      OnAccept = ActionPrintAccept
    end
    object ActionQuickPrint: TAction
      OnExecute = ActionQuickPrintExecute
    end
    object ActionPrintPreview: TAction
      OnExecute = ActionPrintPreviewExecute
    end
    object ActionClosePrintPreview: TAction
      OnExecute = ActionClosePrintPreviewExecute
    end
    object ActionExit: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 43
    end
  end
end
