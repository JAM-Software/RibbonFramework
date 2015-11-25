unit SimpleRibbonUI;

// *****************************************************************************
// * This is an automatically generated source file for UI Element definition  *
// * resource symbols and values. Please do not modify manually.               *
// *****************************************************************************

interface

{$R 'SimpleRibbonUI.res'}

uses
	Generics.Collections, SysUtils, UIRibbon;

const
  IDC_CMD_TAB1 = 10000;
  IDC_CMD_TAB1_LabelTitle_RESID = 200;
  cmdGroup1 = 2;
  cmdGroup1_SmallImages_RESID = 201;
  cmdGroup2 = 3;
  cmdGroup3 = 4;
  cmdGroup4 = 5;
  cmdButton1 = 6;
  cmdButton1_LabelTitle_RESID = 210;
  cmdButton1_SmallImages_RESID = 212;
  cmdButton1_LargeImages_RESID = 211;
  cmdButton2 = 7;
  cmdButton2_LabelTitle_RESID = 220;
  cmdButton2_SmallImages_RESID = 222;
  cmdButton2_LargeImages_RESID = 221;
  cmdButton3 = 8;
  cmdButton3_LabelTitle_RESID = 230;
  cmdButton3_SmallImages_RESID = 232;
  cmdButton3_LargeImages_RESID = 231;
  cmdButton4 = 9;
  cmdButton4_LabelTitle_RESID = 240;
  cmdButton4_SmallImages_RESID = 242;
  cmdButton4_LargeImages_RESID = 241;
  cmdButton5 = 10;
  cmdButton5_LabelTitle_RESID = 250;
  cmdButton5_SmallImages_RESID = 251;
  cmdButton6 = 11;
  cmdToggleButton1 = 12;
  cmdToggleButton1_LabelTitle_RESID = 260;
  cmdToggleButton1_SmallImages_RESID = 262;
  cmdToggleButton1_LargeImages_RESID = 261;
  cmdToggleButton2 = 13;
  cmdToggleButton2_LabelTitle_RESID = 270;
  cmdToggleButton2_SmallImages_RESID = 271;
  cmdQat = 14;
  cmdFileMenu = 15;
  cmdMRUList = 16;
  cmdMRUList_LabelTitle_RESID = 280;
  IDC_CMD_EXIT = 17;
  IDC_CMD_EXIT_LabelTitle_RESID = 290;
  IDC_CMD_EXIT_LargeImages_RESID = 291;

implementation

function RegisterRibbonElements(): TRibbonMarkupElementList;
begin
  Result := TRibbonMarkupElementList.Create('');
  Result.Add(TRibbonMarkupElement.Create('IDC_CMD_TAB1', 10000, 200, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdGroup1', 2, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdGroup2', 3, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdGroup3', 4, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdGroup4', 5, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdButton1', 6, 210, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdButton2', 7, 220, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdButton3', 8, 230, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdButton4', 9, 240, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdButton5', 10, 250, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdButton6', 11, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdToggleButton1', 12, 260, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdToggleButton2', 13, 270, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdQat', 14, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdFileMenu', 15, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdMRUList', 16, 280, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDC_CMD_EXIT', 17, 290, -1));
end;
initialization

  RegisterRibbonElements();
  
end.
