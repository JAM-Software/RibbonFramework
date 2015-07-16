unit ContextPopupUI;

// *****************************************************************************
// * This is an automatically generated source file for UI Element definition  *
// * resource symbols and values. Please do not modify manually.               *
// *****************************************************************************

interface

{$R 'ContextPopupUI.res'}

uses
	Generics.Collections, SysUtils, UIRibbon;

const
  cmdTab1 = 2;
  cmdTab1_LabelTitle_RESID = 200;
  cmdGroup1 = 3;
  cmdGroup1_SmallImages_RESID = 250;
  IDC_CMD_CONTEXT1 = 30001;
  IDC_CMD_CONTEXT1_LabelTitle_RESID = 201;
  IDC_CMD_CONTEXT1_LargeImages_RESID = 251;
  IDC_CMD_CONTEXT2 = 30002;
  IDC_CMD_CONTEXT2_LabelTitle_RESID = 202;
  IDC_CMD_CONTEXT2_LargeImages_RESID = 252;
  IDC_CMD_CONTEXT3 = 30003;
  IDC_CMD_CONTEXT3_LabelTitle_RESID = 203;
  IDC_CMD_CONTEXT3_LargeImages_RESID = 253;
  IDC_CMD_CONTEXT4 = 30004;
  IDC_CMD_CONTEXT4_LabelTitle_RESID = 204;
  IDC_CMD_CONTEXT4_LargeImages_RESID = 254;
  cmdCut = 4;
  cmdCut_LabelTitle_RESID = 205;
  cmdCut_SmallImages_RESID = 255;
  cmdCopy = 5;
  cmdCopy_LabelTitle_RESID = 206;
  cmdCopy_SmallImages_RESID = 256;
  cmdPaste = 6;
  cmdPaste_LabelTitle_RESID = 207;
  cmdPaste_LargeImages_RESID = 257;
  cmdToggleButton = 7;
  cmdToggleButton_LabelTitle_RESID = 208;
  cmdToggleButton_SmallImages_RESID = 258;
  cmdButton1 = 8;
  cmdButton1_LabelTitle_RESID = 209;
  cmdButton1_LargeImages_RESID = 259;
  cmdButton2 = 9;
  cmdButton2_LabelTitle_RESID = 210;
  cmdButton2_LargeImages_RESID = 260;
  cmdButton3 = 10;
  cmdButton3_LabelTitle_RESID = 211;
  cmdButton3_LargeImages_RESID = 261;
  cmdButtons = 11;
  cmdButtons_LabelTitle_RESID = 212;
  cmdButtons_LargeImages_RESID = 262;
  cmdQat = 12;
  cmdFileMenu = 13;
  cmdMRUList = 14;
  cmdMRUList_LabelTitle_RESID = 213;
  cmdExit = 15;
  cmdExit_LabelTitle_RESID = 60001;
  cmdExit_LargeImages_RESID = 60002;
  IDC_CMD_CONTEXTMAP1 = 16;
  IDC_CMD_CONTEXTMAP2 = 17;
  IDC_CMD_CONTEXTMAP3 = 18;
  IDC_CMD_CONTEXTMAP4 = 19;

implementation

function RegisterRibbonElements(): TRibbonMarkupElementList;
begin
  Result := TRibbonMarkupElementList.Create('');
  Result.Add(TRibbonMarkupElement.Create('cmdTab1', 2, 200, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdGroup1', 3, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDC_CMD_CONTEXT1', 30001, 201, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDC_CMD_CONTEXT2', 30002, 202, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDC_CMD_CONTEXT3', 30003, 203, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDC_CMD_CONTEXT4', 30004, 204, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdCut', 4, 205, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdCopy', 5, 206, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdPaste', 6, 207, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdToggleButton', 7, 208, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdButton1', 8, 209, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdButton2', 9, 210, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdButton3', 10, 211, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdButtons', 11, 212, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdQat', 12, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdFileMenu', 13, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdMRUList', 14, 213, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdExit', 15, 60001, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDC_CMD_CONTEXTMAP1', 16, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDC_CMD_CONTEXTMAP2', 17, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDC_CMD_CONTEXTMAP3', 18, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDC_CMD_CONTEXTMAP4', 19, -1, -1));
end;
initialization

  RegisterRibbonElements();
  
end.
