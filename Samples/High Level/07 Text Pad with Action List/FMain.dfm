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
    Zoom = 100
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
    ActionManager = Actions
    OnCommandCreate = CommandCreated
    OnLoaded = RibbonLoaded
  end
  object Actions: TActionList
    Left = 64
    Top = 28
    object CmdCut: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 0
      ShortCut = 16472
    end
    object CmdCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 1
      ShortCut = 16451
    end
    object CmdPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 2
      ShortCut = 16470
    end
    object ActionNotImplemented: TAction
      OnExecute = ActionNotImplementedExecute
    end
    object CmdIndent: TAction
      OnExecute = ActionIndentOutdentExecute
    end
    object CmdOutdent: TAction
      OnExecute = ActionIndentOutdentExecute
    end
    object CmdList: TRibbonCollectionAction
      Category = 'Ribbon Framework'
      OnExecute = CmdListExecute
    end
    object CmdLineSpacing10: TAction
      Caption = '1.0'
      OnExecute = ActionLineSpacingExecute
    end
    object CmdLineSpacing115: TAction
      Caption = '1.15'
      OnExecute = ActionLineSpacingExecute
    end
    object CmdLineSpacing15: TAction
      Caption = '1.5'
      OnExecute = ActionLineSpacingExecute
    end
    object CmdLineSpacing20: TAction
      Caption = '2.0'
      OnExecute = ActionLineSpacingExecute
    end
    object CmdLineSpacingAfter: TAction
      OnExecute = CmdLineSpacingAfterExecute
    end
    object CmdAlignLeft: TAction
      OnExecute = CmdAlignExecute
    end
    object CmdAlignCenter: TAction
      OnExecute = CmdAlignExecute
    end
    object CmdAlignRight: TAction
      OnExecute = CmdAlignExecute
    end
    object CmdAlignJustify: TAction
      Caption = 'CmdAlignJustify'
      OnExecute = CmdAlignExecute
    end
    object CmdFind: TSearchFind
      Category = 'Search'
      Caption = '&Find...'
      Hint = 'Find|Finds the specified text'
      ImageIndex = 34
      ShortCut = 16454
      OnAccept = CmdFindAccept
    end
    object CmdSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object CmdUndo: TAction
      Category = 'Edit'
      OnExecute = CmdUndoExecute
    end
    object CmdRedo: TAction
      Category = 'Edit'
      OnExecute = CmdRedoExecute
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
    object CmdPasteSpecial: TAction
      Category = 'Edit'
      Caption = 'Paste special'
      ShortCut = 24662
      OnExecute = ActionNotImplementedExecute
    end
  end
end
