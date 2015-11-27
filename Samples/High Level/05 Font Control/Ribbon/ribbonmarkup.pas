unit ribbonmarkup;

// *****************************************************************************
// * This is an automatically generated source file for UI Element definition  *
// * resource symbols and values. Please do not modify manually.               *
// *****************************************************************************

interface

{$R 'ribbonmarkup.res'}

uses
	Generics.Collections, SysUtils, UIRibbon;

const
  IDR_CMD_TAB1 = 10000;
  IDR_CMD_TAB1_LabelTitle_RESID = 200;
  cmdGroup1 = 2;
  cmdGroup1_SmallImages_RESID = 201;
  cmdQat = 3;
  cmdFileMenu = 4;
  cmdMRUList = 5;
  cmdMRUList_LabelTitle_RESID = 202;
  cmdExit = 6;
  cmdExit_LabelTitle_RESID = 203;
  cmdExit_LargeImages_RESID = 204;
  IDC_CMD_FONTCONTROL = 7;
  IDC_CMD_FONTCONTROL_Keytip_RESID = 205;
  IDC_CMD_CONTEXTMAP = 8;

implementation

function RegisterRibbonElements(): TRibbonMarkupElementList;
begin
  Result := TRibbonMarkupElementList.Create('');
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_TAB1', 10000, 200, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdGroup1', 2, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdQat', 3, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdFileMenu', 4, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdMRUList', 5, 202, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdExit', 6, 203, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDC_CMD_FONTCONTROL', 7, -1, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDC_CMD_CONTEXTMAP', 8, -1, -1));
end;
initialization

  RegisterRibbonElements();
  
end.
