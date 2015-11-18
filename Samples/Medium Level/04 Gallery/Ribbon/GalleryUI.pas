unit GalleryUI;

// *****************************************************************************
// * This is an automatically generated source file for UI Element definition  *
// * resource symbols and values. Please do not modify manually.               *
// *****************************************************************************

interface

{$R 'GalleryUI.res'}

uses
	Generics.Collections, SysUtils, UIRibbon;

const
  cmdTabHome = 2;
  cmdTabHome_LabelTitle_RESID = 200;
  cmdShapesGroup = 3;
  cmdShapesGroup_LabelTitle_RESID = 210;
  cmdBorderGroup = 4;
  cmdBorderGroup_LabelTitle_RESID = 220;
  cmdLayoutGroup = 5;
  cmdLayoutGroup_LabelTitle_RESID = 230;
  IDR_CMD_SHAPES = 6;
  IDR_CMD_SHAPES_LabelTitle_RESID = 240;
  IDR_CMD_SIZEANDCOLOR = 7;
  IDR_CMD_SIZEANDCOLOR_LabelTitle_RESID = 250;
  IDR_CMD_SIZEANDCOLOR_LargeImages_96__RESID = 251;
  IDR_CMD_SIZEANDCOLOR_LargeImages_120__RESID = 252;
  IDR_CMD_SIZEANDCOLOR_LargeImages_144__RESID = 253;
  IDR_CMD_SIZEANDCOLOR_LargeImages_192__RESID = 254;
  IDR_CMD_BORDERSTYLES = 8;
  IDR_CMD_BORDERSTYLES_LabelTitle_RESID = 260;
  IDR_CMD_BORDERSTYLES_LargeImages_96__RESID = 261;
  IDR_CMD_BORDERSTYLES_LargeImages_120__RESID = 262;
  IDR_CMD_BORDERSTYLES_LargeImages_144__RESID = 263;
  IDR_CMD_BORDERSTYLES_LargeImages_192__RESID = 264;
  IDR_CMD_BORDERSIZES = 9;
  IDR_CMD_BORDERSIZES_LabelTitle_RESID = 270;
  IDR_CMD_LAYOUTS = 10;
  IDR_CMD_LAYOUTS_LabelTitle_RESID = 280;
  IDR_CMD_SMALL = 100;
  IDR_CMD_SMALL_LabelTitle_RESID = 290;
  IDR_CMD_SMALL_LargeImages_96__RESID = 291;
  IDR_CMD_SMALL_LargeImages_120__RESID = 292;
  IDR_CMD_SMALL_LargeImages_144__RESID = 293;
  IDR_CMD_SMALL_LargeImages_192__RESID = 294;
  IDR_CMD_MEDIUM = 101;
  IDR_CMD_MEDIUM_LabelTitle_RESID = 300;
  IDR_CMD_MEDIUM_LargeImages_96__RESID = 301;
  IDR_CMD_MEDIUM_LargeImages_120__RESID = 302;
  IDR_CMD_MEDIUM_LargeImages_144__RESID = 303;
  IDR_CMD_MEDIUM_LargeImages_192__RESID = 304;
  IDR_CMD_LARGE = 102;
  IDR_CMD_LARGE_LabelTitle_RESID = 310;
  IDR_CMD_LARGE_LargeImages_96__RESID = 311;
  IDR_CMD_LARGE_LargeImages_120__RESID = 312;
  IDR_CMD_LARGE_LargeImages_144__RESID = 313;
  IDR_CMD_LARGE_LargeImages_192__RESID = 314;
  IDR_CMD_RED = 103;
  IDR_CMD_RED_LabelTitle_RESID = 320;
  IDR_CMD_RED_LargeImages_96__RESID = 321;
  IDR_CMD_RED_LargeImages_120__RESID = 322;
  IDR_CMD_RED_LargeImages_144__RESID = 323;
  IDR_CMD_RED_LargeImages_192__RESID = 324;
  IDR_CMD_GREEN = 104;
  IDR_CMD_GREEN_LabelTitle_RESID = 330;
  IDR_CMD_GREEN_LargeImages_96__RESID = 331;
  IDR_CMD_GREEN_LargeImages_120__RESID = 332;
  IDR_CMD_GREEN_LargeImages_144__RESID = 333;
  IDR_CMD_GREEN_LargeImages_192__RESID = 334;
  IDR_CMD_BLUE = 105;
  IDR_CMD_BLUE_LabelTitle_RESID = 340;
  IDR_CMD_BLUE_LargeImages_96__RESID = 341;
  IDR_CMD_BLUE_LargeImages_120__RESID = 342;
  IDR_CMD_BLUE_LargeImages_144__RESID = 343;
  IDR_CMD_BLUE_LargeImages_192__RESID = 344;
  InternalCmd2_LabelTitle_RESID = 60001;

implementation

function RegisterRibbonElements(): TRibbonMarkupElementList;
begin
  Result := TRibbonMarkupElementList.Create('');
  Result.Add(TRibbonMarkupElement.Create('cmdTabHome', 2, 200, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdShapesGroup', 3, 210, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdBorderGroup', 4, 220, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('cmdLayoutGroup', 5, 230, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_SHAPES', 6, 240, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_SIZEANDCOLOR', 7, 250, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_BORDERSTYLES', 8, 260, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_BORDERSIZES', 9, 270, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_LAYOUTS', 10, 280, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_SMALL', 100, 290, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_MEDIUM', 101, 300, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_LARGE', 102, 310, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_RED', 103, 320, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_GREEN', 104, 330, -1, -1, -1));
  Result.Add(TRibbonMarkupElement.Create('IDR_CMD_BLUE', 105, 340, -1));
end;
initialization

  RegisterRibbonElements();
  
end.
