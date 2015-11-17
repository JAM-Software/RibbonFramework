// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'WinApiEx.pas' rev: 30.00 (Windows)

#ifndef WinapiexHPP
#define WinapiexHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.ActiveX.hpp>
#include <Winapi.ShlObj.hpp>

//-- user supplied -----------------------------------------------------------

namespace Winapiex
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef short *PSHORT;

typedef System::Word *PUSHORT;

typedef int LONG;

typedef int *PLONG;

typedef unsigned *PULONG;

typedef __int64 *PLONGLONG;

typedef unsigned __int64 *PULONGLONG;

typedef System::WideChar * PCWSTR;

typedef System::WideChar * *PPCWSTR;

typedef System::WideChar * PWSTR;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall PropVariantInit(/* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall PropVariantClear(tagPROPVARIANT &PropVar);
extern "C" unsigned __stdcall PropVariantGetElementCount(const tagPROPVARIANT &PropVarIn);
extern "C" HRESULT __stdcall InitPropVariantFromResource(NativeUInt Inst, unsigned Id, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantFromBuffer(const void * Buf, unsigned Count, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantFromCLSID(const GUID &Value, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantFromGUIDAsString(const GUID &Value, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantFromFileTime(const _FILETIME &Value, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantFromPropVariantVectorElem(Winapi::Activex::PPropVariant PropVarIn, unsigned Elem, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantVectorFromPropVariant(const tagPROPVARIANT &Value, /* out */ tagPROPVARIANT &PropVarVector);
extern "C" HRESULT __stdcall InitPropVariantFromStrRet(_STRRET &StrRet, PCUITEMID_CHILD Pidl, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantFromBooleanVector(const PBOOL Values, unsigned Count, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantFromInt16Vector(const PSHORT Values, unsigned Count, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantFromUInt16Vector(const PUSHORT Values, unsigned Count, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantFromInt32Vector(const PLONG Values, unsigned Count, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantFromUInt32Vector(const PULONG Values, unsigned Count, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantFromInt64Vector(const PLONGLONG Values, unsigned Count, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantFromUInt64Vector(const PULONGLONG Values, unsigned Count, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantFromDoubleVector(const System::PDouble Values, unsigned Count, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantFromFileTimeVector(const Winapi::Windows::PFileTime Values, unsigned Count, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantFromStringVector(PPCWSTR Values, unsigned Count, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall InitPropVariantFromStringAsVector(System::WideChar * Values, unsigned Count, /* out */ tagPROPVARIANT &PropVar);
extern "C" System::LongBool __stdcall PropVariantToBooleanWithDefault(const tagPROPVARIANT &PropVarIn, const System::LongBool Default);
extern "C" short __stdcall PropVariantToInt16WithDefault(const tagPROPVARIANT &PropVarIn, const short Default);
extern "C" System::Word __stdcall PropVariantToUInt16WithDefault(const tagPROPVARIANT &PropVarIn, const System::Word Default);
extern "C" int __stdcall PropVariantToInt32WithDefault(const tagPROPVARIANT &PropVarIn, const int Default);
extern "C" unsigned __stdcall PropVariantToUInt32WithDefault(const tagPROPVARIANT &PropVarIn, const unsigned Default);
extern "C" __int64 __stdcall PropVariantToInt64WithDefault(const tagPROPVARIANT &PropVarIn, const __int64 Default);
extern "C" unsigned __int64 __stdcall PropVariantToUInt64WithDefault(const tagPROPVARIANT &PropVarIn, const unsigned __int64 Default);
extern "C" double __stdcall PropVariantToDoubleWithDefault(const tagPROPVARIANT &PropVarIn, const double Default);
extern "C" System::WideChar * __stdcall PropVariantToStringWithDefault(const tagPROPVARIANT &PropVarIn, const System::WideChar * Default);
extern "C" HRESULT __stdcall PropVariantToBoolean(const tagPROPVARIANT &PropVarIn, /* out */ System::LongBool &Ret);
extern "C" HRESULT __stdcall PropVariantToInt16(const tagPROPVARIANT &PropVarIn, /* out */ short &Ret);
extern "C" HRESULT __stdcall PropVariantToUInt16(const tagPROPVARIANT &PropVarIn, /* out */ System::Word &Ret);
extern "C" HRESULT __stdcall PropVariantToInt32(const tagPROPVARIANT &PropVarIn, /* out */ int &Ret);
extern "C" HRESULT __stdcall PropVariantToUInt32(const tagPROPVARIANT &PropVarIn, /* out */ unsigned &Ret);
extern "C" HRESULT __stdcall PropVariantToInt64(const tagPROPVARIANT &PropVarIn, /* out */ __int64 &Ret);
extern "C" HRESULT __stdcall PropVariantToUInt64(const tagPROPVARIANT &PropVarIn, /* out */ unsigned __int64 &Ret);
extern "C" HRESULT __stdcall PropVariantToDouble(const tagPROPVARIANT &PropVarIn, /* out */ double &Ret);
extern "C" HRESULT __stdcall PropVariantToBuffer(const tagPROPVARIANT &PropVarIn, void * Buf, unsigned Count);
extern "C" HRESULT __stdcall PropVariantToString(const tagPROPVARIANT &PropVarIn, System::WideChar * Str, unsigned Count);
extern "C" HRESULT __stdcall PropVariantToGUID(const tagPROPVARIANT &PropVarIn, /* out */ GUID &Ret);
extern "C" HRESULT __stdcall PropVariantToStringAlloc(const tagPROPVARIANT &PropVarIn, /* out */ System::WideChar * &Ret);
extern "C" HRESULT __stdcall PropVariantToBSTR(const tagPROPVARIANT &PropVarIn, /* out */ System::WideChar * &Ret);
extern "C" HRESULT __stdcall PropVariantToStrRet(const tagPROPVARIANT &PropVarIn, /* out */ Winapi::Shlobj::PSTRRet &Ret);
extern "C" HRESULT __stdcall PropVariantToBooleanVector(const tagPROPVARIANT &PropVarIn, PBOOL PRgn, unsigned CRgn, /* out */ unsigned &pcElem);
extern "C" HRESULT __stdcall PropVariantToInt16Vector(const tagPROPVARIANT &PropVarIn, PSHORT PRgn, unsigned CRgn, /* out */ unsigned &pcElem);
extern "C" HRESULT __stdcall PropVariantToUInt16Vector(const tagPROPVARIANT &PropVarIn, PUSHORT PRgn, unsigned CRgn, /* out */ unsigned &pcElem);
extern "C" HRESULT __stdcall PropVariantToInt32Vector(const tagPROPVARIANT &PropVarIn, PLONG PRgn, unsigned CRgn, /* out */ unsigned &pcElem);
extern "C" HRESULT __stdcall PropVariantToUInt32Vector(const tagPROPVARIANT &PropVarIn, PULONG PRgn, unsigned CRgn, /* out */ unsigned &pcElem);
extern "C" HRESULT __stdcall PropVariantToInt64Vector(const tagPROPVARIANT &PropVarIn, PLONGLONG PRgn, unsigned CRgn, /* out */ unsigned &pcElem);
extern "C" HRESULT __stdcall PropVariantToUInt64Vector(const tagPROPVARIANT &PropVarIn, PULONGLONG PRgn, unsigned CRgn, /* out */ unsigned &pcElem);
extern "C" HRESULT __stdcall PropVariantToDoubleVector(const tagPROPVARIANT &PropVarIn, System::PDouble PRgn, unsigned CRgn, /* out */ unsigned &pcElem);
extern "C" HRESULT __stdcall PropVariantToFileTimeVector(const tagPROPVARIANT &PropVarIn, Winapi::Windows::PFileTime PRgn, unsigned CRgn, /* out */ unsigned &pcElem);
extern "C" HRESULT __stdcall PropVariantToStringVector(const tagPROPVARIANT &PropVarIn, System::PPWideChar PRgn, unsigned CRgn, /* out */ unsigned &pcElem);
extern DELPHI_PACKAGE HRESULT __fastcall InitPropVariantFromBoolean(const bool Value, /* out */ tagPROPVARIANT &PropVar);
extern DELPHI_PACKAGE HRESULT __fastcall InitPropVariantFromUInt32(const unsigned Value, /* out */ tagPROPVARIANT &PropVar);
extern DELPHI_PACKAGE HRESULT __fastcall InitPropVariantFromString(const System::UnicodeString Value, /* out */ tagPROPVARIANT &PropVar);
extern "C" HRESULT __stdcall VarDecFromI2(short Input, /* out */ tagDEC &Output);
extern "C" HRESULT __stdcall VarDecFromI4(int Input, /* out */ tagDEC &Output);
extern "C" HRESULT __stdcall VarDecFromR8(double Input, /* out */ tagDEC &Output);
extern "C" HRESULT __stdcall VarUI2FromDec(const tagDEC &Input, /* out */ System::Word &Output);
}	/* namespace Winapiex */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_WINAPIEX)
using namespace Winapiex;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// WinapiexHPP
