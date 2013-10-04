program RibbonDesigner;

{$R 'UIRibbon.res' '..\Lib\UIRibbon.rc'}

uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  BasicXml in 'Classes\BasicXml.pas',
  RibbonMarkup in 'Classes\RibbonMarkup.pas',
  Settings in 'Classes\Settings.pas',
  RibbonCompiler in 'Classes\RibbonCompiler.pas',
  UIRibbonForm in '..\Lib\UIRibbonForm.pas',
  UIRibbon in '..\Lib\UIRibbon.pas',
  UIRibbonApi in '..\Lib\UIRibbonApi.pas',
  UIRibbonCommands in '..\Lib\UIRibbonCommands.pas',
  WinApiEx in '..\Lib\WinApiEx.pas',
  FPreview in 'FPreview.pas' {FormPreview},
  FCommands in 'FCommands.pas' {FrameCommands: TFrame},
  FEditImage in 'FEditImage.pas' {FormEditImage},
  FImageList in 'FImageList.pas' {FrameImageList: TFrame},
  FSettings in 'FSettings.pas' {FormSettings},
  FViews in 'FViews.pas' {FrameViews: TFrame},
  FControl in 'FControl.pas' {FrameControl: TFrame},
  FApplicationMenu in 'FApplicationMenu.pas' {FrameApplicationMenu: TFrame},
  DMShared in 'DMShared.pas' {DataModuleShared: TDataModule},
  FApplicationModes in 'FApplicationModes.pas' {FormApplicationModes},
  FViewRibbon in 'FViewRibbon.pas' {FrameViewRibbon},
  FCommandRefObject in 'FCommandRefObject.pas' {FrameCommandRefObject},
  FBaseFrame in 'FBaseFrame.pas' {BaseFrame: TFrame},
  FAppMenuGroup in 'FAppMenuGroup.pas' {FrameAppMenuGroup: TFrame},
  FButton in 'FButton.pas' {FrameButton: TFrame},
  FQuickAccessToolbar in 'FQuickAccessToolbar.pas' {FrameQuickAccessToolbar: TFrame},
  FHelpButton in 'FHelpButton.pas' {FrameHelpButton: TFrame},
  FTab in 'FTab.pas' {FrameTab: TFrame},
  FSplitButton in 'FSplitButton.pas' {FrameSplitButton: TFrame},
  FMenuGroup in 'FMenuGroup.pas' {FrameMenuGroup: TFrame},
  FQatControl in 'FQatControl.pas' {FrameQatControl: TFrame},
  FScale in 'FScale.pas' {FrameScale: TFrame},
  FDropDownButton in 'FDropDownButton.pas' {FrameDropDownButton: TFrame},
  FGallery in 'FGallery.pas' {FrameGallery: TFrame},
  FDropDownGallery in 'FDropDownGallery.pas' {FrameDropDownGallery: TFrame},
  FSplitButtonGallery in 'FSplitButtonGallery.pas' {FrameSplitButtonGallery: TFrame},
  FGroup in 'FGroup.pas' {FrameGroup: TFrame},
  FSizeDefinition in 'FSizeDefinition.pas' {FrameSizeDefinition: TFrame},
  FGroupSizeDefinition in 'FGroupSizeDefinition.pas' {FrameGroupSizeDefinition: TFrame},
  FControlSizeDefinition in 'FControlSizeDefinition.pas' {FrameControlSizeDefintion: TFrame},
  FColumnBreak in 'FColumnBreak.pas' {FrameColumnBreak: TFrame},
  FFloatieFontControl in 'FFloatieFontControl.pas' {FrameFloatieFontControl: TFrame},
  FFontControl in 'FFontControl.pas' {FrameFontControl: TFrame},
  FControlGroup in 'FControlGroup.pas' {FrameControlGroup: TFrame},
  FToggleButton in 'FToggleButton.pas' {FrameToggleButton: TFrame},
  FComboBox in 'FComboBox.pas' {FrameComboBox: TFrame},
  FCheckBox in 'FCheckBox.pas' {FrameCheckBox: TFrame},
  FDropDownColorPicker in 'FDropDownColorPicker.pas' {FrameDropDownColorPicker: TFrame},
  FSpinner in 'FSpinner.pas' {FrameSpinner: TFrame},
  FInRibbonGallery in 'FInRibbonGallery.pas' {FrameInRibbonGallery: TFrame},
  FTabGroup in 'FTabGroup.pas' {FrameTabGroup: TFrame},
  FMiniToolbar in 'FMiniToolbar.pas' {FrameMiniToolbar: TFrame},
  FContextMenu in 'FContextMenu.pas' {FrameContextMenu: TFrame},
  FContextMap in 'FContextMap.pas' {FrameContextMap: TFrame},
  FXmlSource in 'FXmlSource.pas' {FrameXmlSource: TFrame},
  UIRibbonUtils in '..\Lib\UIRibbonUtils.pas',
  FNewFile in 'FNewFile.pas' {FormNewFile},
  BasicZip in 'Classes\BasicZip.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Ribbon Designer';
  Application.CreateForm(TDataModuleShared, DataModuleShared);
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
