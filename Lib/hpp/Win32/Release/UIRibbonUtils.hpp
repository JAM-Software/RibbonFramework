// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UIRibbonUtils.pas' rev: 30.00 (Windows)

#ifndef UiribbonutilsHPP
#define UiribbonutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.GraphUtil.hpp>
#include <UIRibbonApi.hpp>

//-- user supplied -----------------------------------------------------------

namespace Uiribbonutils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall HsbToColor(const unsigned Hsb)/* overload */;
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall HsbToColor(const int H, const int S, const int B)/* overload */;
extern DELPHI_PACKAGE unsigned __fastcall HsbToHsbColor(const int H, const int S, const int B);
extern DELPHI_PACKAGE void __fastcall HsbColorToHsb(const unsigned Color, /* out */ int &H, /* out */ int &S, /* out */ int &B);
extern DELPHI_PACKAGE unsigned __fastcall ColorToHsb(const System::Uitypes::TColor Color);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CreateStringFromResource(const int ResourceId);
}	/* namespace Uiribbonutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UIRIBBONUTILS)
using namespace Uiribbonutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UiribbonutilsHPP
