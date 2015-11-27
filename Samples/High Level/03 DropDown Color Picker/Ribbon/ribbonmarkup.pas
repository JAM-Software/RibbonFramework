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
  cmdTabHome = 2;
  cmdTabHome_LabelTitle_RESID = 200;
  GroupDefaultDDCP = 3;
  GroupDefaultDDCP_LabelTitle_RESID = 210;
  GroupCustomDDCP = 4;
  GroupCustomDDCP_LabelTitle_RESID = 220;
  GroupColorGrid = 5;
  GroupColorGrid_LabelTitle_RESID = 230;
  IDR_CMD_THEMEDDCP = 6;
  IDR_CMD_THEMEDDCP_LabelTitle_RESID = 240;
  IDR_CMD_THEMEDDCP_LargeImages_RESID = 241;
  IDR_CMD_HIGHLIGHTDDCP = 7;
  IDR_CMD_HIGHLIGHTDDCP_LabelTitle_RESID = 250;
  IDR_CMD_HIGHLIGHTDDCP_LargeImages_RESID = 251;
  IDR_CMD_STANDARDDDCP = 8;
  IDR_CMD_STANDARDDDCP_LabelTitle_RESID = 260;
  IDR_CMD_STANDARDDDCP_LargeImages_RESID = 261;
  IDR_CMD_UPDATE = 9;
  IDR_CMD_UPDATE_LabelTitle_RESID = 270;
  IDR_CMD_UPDATE_TooltipTitle_RESID = 271;
  IDR_CMD_UPDATE_TooltipDescription_RESID = 272;
  IDR_CMD_UPDATE_LargeImages_RESID = 273;
  IDR_CMD_CLEAR = 10;
  IDR_CMD_CLEAR_LabelTitle_RESID = 280;
  IDR_CMD_CLEAR_TooltipTitle_RESID = 281;
  IDR_CMD_CLEAR_TooltipDescription_RESID = 282;
  IDR_CMD_CLEAR_LargeImages_RESID = 283;
  InternalCmd2_LabelTitle_RESID = 60001;

implementation

function RegisterRibbonElements(): TRibbonMarkupElementList;
begin
  Result := TRibbonMarkupElementList.Create('');
  Result.Add(TRibbonMarkupElement.Create('cmdTabHome', 2, 200, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('GroupDefaultDDCP', 3, 210, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('GroupCustomDDCP', 4, 220, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('GroupColorGrid', 5, 230, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_THEMEDDCP', 6, 240, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_HIGHLIGHTDDCP', 7, 250, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_STANDARDDDCP', 8, 260, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_UPDATE', 9, 270, -1, 271, 272));
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_CLEAR', 10, 280, 281));
end;
initialization

  RegisterRibbonElements();
  
end.
