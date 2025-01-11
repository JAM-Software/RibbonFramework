unit WinApiEx;

{ Additional Windows API translations that are not part of the Delphi RTL (yet) }

interface

{$WARN SYMBOL_PLATFORM OFF}

uses
  WinApi.Windows,
  WinApi.ActiveX,
  WinApi.ShlObj;

type
  PSHORT = ^SHORT;
  PUSHORT = ^USHORT;
  LONG = Int32;
  PLONG = ^LONG;
  PULONG = ^ULONG;
  PLONGLONG = ^LONGLONG;
  PULONGLONG = ^ULONGLONG;
  PCWSTR = LPCWSTR;
  PPCWSTR = ^PCWSTR;
  PWSTR = PCWSTR;

{$REGION 'PropIdl.h'}
procedure PropVariantInit(out PropVar: TPropVariant); inline;
function PropVariantClear(var PropVar: TPropVariant): HRESULT; stdcall;
{$ENDREGION 'PropIdl.h'}

{$REGION 'propvarutil.h'}

// PropVariant Helpers

// Get propvariant infp
function PropVariantGetElementCount(const PropVarIn: TPropVariant): ULONG; stdcall;

// Initialize a propvariant

function InitPropVariantFromResource(Inst: HINST; Id: UINT;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromBuffer(const Buf: Pointer; Count: UINT;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromCLSID(const Value: TGuid;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromGUIDAsString(const Value: TGuid;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromFileTime(const Value: TFileTime;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromPropVariantVectorElem(PropVarIn: PPropVariant;
  Elem: ULONG; out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantVectorFromPropVariant(const Value: TPropVariant;
  out PropVarVector: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromStrRet(var StrRet: TStrRet; Pidl: PCUITEMID_CHILD;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromBooleanVector(const Values: PBOOL; Count: ULONG;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromInt16Vector(const Values: PSHORT; Count: ULONG;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromUInt16Vector(const Values: PUSHORT; Count: ULONG;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromInt32Vector(const Values: PLONG; Count: ULONG;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromUInt32Vector(const Values: PULONG; Count: ULONG;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromInt64Vector(const Values: PLONGLONG; Count: ULONG;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromUInt64Vector(const Values: PULONGLONG; Count: ULONG;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromDoubleVector(const Values: PDOUBLE; Count: ULONG;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromFileTimeVector(const Values: PFILETIME; Count: ULONG;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromStringVector(Values: PPCWSTR; Count: ULONG;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromStringAsVector(Values: PCWSTR; Count: ULONG;
  out PropVar: TPropVariant): HRESULT; stdcall;

function InitPropVariantFromBoolean(const Value: Boolean;
  out PropVar: TPropVariant): HRESULT; inline;

function InitPropVariantFromUInt32(const Value: UInt32;
  out PropVar: TPropVariant): HRESULT; inline;

function InitPropVariantFromString(const Value: String;
  out PropVar: TPropVariant): HRESULT; inline;

// Extract data from a propvariant

function PropVariantToBooleanWithDefault(const PropVarIn: TPropVariant;
  const Default: BOOL): BOOL; stdcall;

function PropVariantToInt16WithDefault(const PropVarIn: TPropVariant;
  const Default: SHORT): SHORT; stdcall;

function PropVariantToUInt16WithDefault(const PropVarIn: TPropVariant;
  const Default: USHORT): USHORT; stdcall;

function PropVariantToInt32WithDefault(const PropVarIn: TPropVariant;
  const Default: LONG): LONG; stdcall;

function PropVariantToUInt32WithDefault(const PropVarIn: TPropVariant;
  const Default: ULONG): ULONG; stdcall;

function PropVariantToInt64WithDefault(const PropVarIn: TPropVariant;
  const Default: LONGLONG): LONGLONG; stdcall;

function PropVariantToUInt64WithDefault(const PropVarIn: TPropVariant;
  const Default: ULONGLONG): ULONGLONG; stdcall;

function PropVariantToDoubleWithDefault(const PropVarIn: TPropVariant;
  const Default: DOUBLE): DOUBLE; stdcall;

function PropVariantToStringWithDefault(const PropVarIn: TPropVariant;
  const Default: PCWSTR): PCWSTR; stdcall;

function PropVariantToBoolean(const PropVarIn: TPropVariant;
  out Ret: BOOL): HRESULT; stdcall;

function PropVariantToInt16(const PropVarIn: TPropVariant;
  out Ret: SHORT): HRESULT; stdcall;

function PropVariantToUInt16(const PropVarIn: TPropVariant;
  out Ret: USHORT): HRESULT; stdcall;

function PropVariantToInt32(const PropVarIn: TPropVariant;
  out Ret: LONG): HRESULT; stdcall;

function PropVariantToUInt32(const PropVarIn: TPropVariant;
  out Ret: ULONG): HRESULT; stdcall;

function PropVariantToInt64(const PropVarIn: TPropVariant;
  out Ret: LONGLONG): HRESULT; stdcall;

function PropVariantToUInt64(const PropVarIn: TPropVariant;
  out Ret: ULONGLONG): HRESULT; stdcall;

function PropVariantToDouble(const PropVarIn: TPropVariant;
  out Ret: DOUBLE): HRESULT; stdcall;

function PropVariantToBuffer(const PropVarIn: TPropVariant; Buf: Pointer;
  Count: UINT): HRESULT; stdcall;

function PropVariantToString(const PropVarIn: TPropVariant; Str: PWSTR;
  Count: UINT): HRESULT; stdcall;

function PropVariantToGUID(const PropVarIn: TPropVariant;
  out Ret: TGUID): HRESULT; stdcall;

function PropVariantToStringAlloc(const PropVarIn: TPropVariant;
  out Ret: PWSTR): HRESULT; stdcall;

function PropVariantToBSTR(const PropVarIn: TPropVariant;
  out Ret: TBSTR): HRESULT; stdcall;

function PropVariantToStrRet(const PropVarIn: TPropVariant;
  out Ret: PStrRet): HRESULT; stdcall;

function PropVariantToBooleanVector(const PropVarIn: TPropVariant;
  PRgn: PBOOL; CRgn: ULONG; out pcElem: ULONG): HRESULT; stdcall;

function PropVariantToInt16Vector(const PropVarIn: TPropVariant;
  PRgn: PSHORT; CRgn: ULONG; out pcElem: ULONG): HRESULT; stdcall;

function PropVariantToUInt16Vector(const PropVarIn: TPropVariant;
  PRgn: PUSHORT; CRgn: ULONG; out pcElem: ULONG): HRESULT; stdcall;

function PropVariantToInt32Vector(const PropVarIn: TPropVariant;
  PRgn: PLONG; CRgn: ULONG; out pcElem: ULONG): HRESULT; stdcall;

function PropVariantToUInt32Vector(const PropVarIn: TPropVariant;
  PRgn: PULONG; CRgn: ULONG; out pcElem: ULONG): HRESULT; stdcall;

function PropVariantToInt64Vector(const PropVarIn: TPropVariant;
  PRgn: PLONGLONG; CRgn: ULONG; out pcElem: ULONG): HRESULT; stdcall;

function PropVariantToUInt64Vector(const PropVarIn: TPropVariant;
  PRgn: PULONGLONG; CRgn: ULONG; out pcElem: ULONG): HRESULT; stdcall;

function PropVariantToDoubleVector(const PropVarIn: TPropVariant;
  PRgn: PDouble; CRgn: ULONG; out pcElem: ULONG): HRESULT; stdcall;

function PropVariantToFileTimeVector(const PropVarIn: TPropVariant;
  PRgn: PFileTime; CRgn: ULONG; out pcElem: ULONG): HRESULT; stdcall;

function PropVariantToStringVector(const PropVarIn: TPropVariant;
  PRgn: PPWideChar; CRgn: ULONG; out pcElem: ULONG): HRESULT; stdcall;

{$ENDREGION 'propvarutil.h'}

{$REGION 'OleAuto.h'}
function VarDecFromI2(Input: Int16; out Output: TDecimal): HRESULT; stdcall;
function VarDecFromI4(Input: Int32; out Output: TDecimal): HRESULT; stdcall;
function VarDecFromR8(Input: Double; out Output: TDecimal): HRESULT; stdcall;
function VarUI2FromDec(const Input: TDecimal; out Output: UInt16): HRESULT; stdcall;

{$ENDREGION 'OleAuto.h'}

implementation

uses
  WinApi.ShLwApi;

{$REGION 'PropIdl.h'}
const
  OLE32_DLL = 'Ole32.dll';

procedure PropVariantInit(out PropVar: TPropVariant); inline;
begin
  FillChar(PropVar, SizeOf(PropVar), 0);
end;

function PropVariantClear; external OLE32_DLL delayed;
{$ENDREGION 'PropIdl.h'}

{$REGION 'propvarutil.h'}
const
  PROPSYS_DLL = 'Propsys.dll';

function PropVariantGetElementCount; external PROPSYS_DLL delayed;
function InitPropVariantFromResource; external PROPSYS_DLL delayed;
function InitPropVariantFromBuffer; external PROPSYS_DLL delayed;
function InitPropVariantFromCLSID; external PROPSYS_DLL delayed;
function InitPropVariantFromGUIDAsString; external PROPSYS_DLL delayed;
function InitPropVariantFromFileTime; external PROPSYS_DLL delayed;
function InitPropVariantFromPropVariantVectorElem; external PROPSYS_DLL delayed;
function InitPropVariantVectorFromPropVariant; external PROPSYS_DLL delayed;
function InitPropVariantFromStrRet; external PROPSYS_DLL delayed;
function InitPropVariantFromBooleanVector; external PROPSYS_DLL delayed;
function InitPropVariantFromInt16Vector; external PROPSYS_DLL delayed;
function InitPropVariantFromUInt16Vector; external PROPSYS_DLL delayed;
function InitPropVariantFromInt32Vector; external PROPSYS_DLL delayed;
function InitPropVariantFromUInt32Vector; external PROPSYS_DLL delayed;
function InitPropVariantFromInt64Vector; external PROPSYS_DLL delayed;
function InitPropVariantFromUInt64Vector; external PROPSYS_DLL delayed;
function InitPropVariantFromDoubleVector; external PROPSYS_DLL delayed;
function InitPropVariantFromFileTimeVector; external PROPSYS_DLL delayed;
function InitPropVariantFromStringVector; external PROPSYS_DLL delayed;
function InitPropVariantFromStringAsVector; external PROPSYS_DLL delayed;

function PropVariantToBooleanWithDefault; external PROPSYS_DLL delayed;
function PropVariantToInt16WithDefault; external PROPSYS_DLL delayed;
function PropVariantToUInt16WithDefault; external PROPSYS_DLL delayed;
function PropVariantToInt32WithDefault; external PROPSYS_DLL delayed;
function PropVariantToUInt32WithDefault; external PROPSYS_DLL delayed;
function PropVariantToInt64WithDefault; external PROPSYS_DLL delayed;
function PropVariantToUInt64WithDefault; external PROPSYS_DLL delayed;
function PropVariantToDoubleWithDefault; external PROPSYS_DLL delayed;
function PropVariantToStringWithDefault; external PROPSYS_DLL delayed;
function PropVariantToBoolean; external PROPSYS_DLL delayed;
function PropVariantToInt16; external PROPSYS_DLL delayed;
function PropVariantToUInt16; external PROPSYS_DLL delayed;
function PropVariantToInt32; external PROPSYS_DLL delayed;
function PropVariantToUInt32; external PROPSYS_DLL delayed;
function PropVariantToInt64; external PROPSYS_DLL delayed;
function PropVariantToUInt64; external PROPSYS_DLL delayed;
function PropVariantToDouble; external PROPSYS_DLL delayed;
function PropVariantToBuffer; external PROPSYS_DLL delayed;
function PropVariantToString; external PROPSYS_DLL delayed;
function PropVariantToGUID; external PROPSYS_DLL delayed;
function PropVariantToStringAlloc; external PROPSYS_DLL delayed;
function PropVariantToBSTR; external PROPSYS_DLL delayed;
function PropVariantToStrRet; external PROPSYS_DLL delayed;
function PropVariantToBooleanVector; external PROPSYS_DLL delayed;
function PropVariantToInt16Vector; external PROPSYS_DLL delayed;
function PropVariantToUInt16Vector; external PROPSYS_DLL delayed;
function PropVariantToInt32Vector; external PROPSYS_DLL delayed;
function PropVariantToUInt32Vector; external PROPSYS_DLL delayed;
function PropVariantToInt64Vector; external PROPSYS_DLL delayed;
function PropVariantToUInt64Vector; external PROPSYS_DLL delayed;
function PropVariantToDoubleVector; external PROPSYS_DLL delayed;
function PropVariantToFileTimeVector; external PROPSYS_DLL delayed;
function PropVariantToStringVector; external PROPSYS_DLL delayed;

function InitPropVariantFromBoolean(const Value: Boolean;
  out PropVar: TPropVariant): HRESULT; inline;
begin
  PropVar.vt := VT_BOOL;
  PropVar.boolVal := Value;
  Result := S_OK;
end;

function InitPropVariantFromUInt32(const Value: UInt32;
  out PropVar: TPropVariant): HRESULT; inline;
begin
  PropVar.vt := VT_UI4;
  PropVar.ulVal := Value;
  Result := S_OK;
end;

function InitPropVariantFromString(const Value: String;
  out PropVar: TPropVariant): HRESULT; inline;
begin
  PropVar.vt := VT_LPWSTR;
  Result := SHStrDupW(PChar(Value), PropVar.pwszVal);
  if Failed(Result) then
    PropVariantInit(PropVar);
end;
{$ENDREGION 'propvarutil.h'}

{$REGION 'OleAuto.h'}
const
  OLEAUT32_DLL = 'OleAut32.dll';
//
function VarDecFromI2; external OLEAUT32_DLL delayed;
function VarDecFromI4; external OLEAUT32_DLL delayed;
function VarDecFromR8; external OLEAUT32_DLL delayed;
function VarUI2FromDec; external OLEAUT32_DLL delayed;
{$ENDREGION 'OleAuto.h'}

end.
