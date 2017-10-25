// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UIRibbonApi.pas' rev: 32.00 (Windows)

#ifndef UiribbonapiHPP
#define UiribbonapiHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.ActiveX.hpp>

//-- user supplied -----------------------------------------------------------

namespace Uiribbonapi
{
//-- forward type declarations -----------------------------------------------
struct TPropVariantEx;
__interface IUISimplePropertySet;
typedef System::DelphiInterface<IUISimplePropertySet> _di_IUISimplePropertySet;
__interface IUIRibbon;
typedef System::DelphiInterface<IUIRibbon> _di_IUIRibbon;
__interface IUIFramework;
typedef System::DelphiInterface<IUIFramework> _di_IUIFramework;
__interface IUIContextualUI;
typedef System::DelphiInterface<IUIContextualUI> _di_IUIContextualUI;
__interface IUICollection;
typedef System::DelphiInterface<IUICollection> _di_IUICollection;
__interface IUICollectionChangedEvent;
typedef System::DelphiInterface<IUICollectionChangedEvent> _di_IUICollectionChangedEvent;
__interface IUICommandHandler;
typedef System::DelphiInterface<IUICommandHandler> _di_IUICommandHandler;
__interface IUIApplication;
typedef System::DelphiInterface<IUIApplication> _di_IUIApplication;
__interface IUIImage;
typedef System::DelphiInterface<IUIImage> _di_IUIImage;
__interface IUIImageFromBitmap;
typedef System::DelphiInterface<IUIImageFromBitmap> _di_IUIImageFromBitmap;
class DELPHICLASS CoUIRibbonFramework;
class DELPHICLASS CoUIRibbonImageFromBitmapFactory;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TPropVariantEx
{
	
public:
	union
	{
		struct 
		{
			tagDEC DecVal;
		};
		struct 
		{
			tagPROPVARIANT PropVar;
		};
		
	};
};


typedef _tagpropertykey TUIPropertyKey;

typedef Winapi::Activex::PPropertyKey PUIPropertyKey;

typedef unsigned TUIHsbColor;

enum DECLSPEC_DENUM _UIContextAvailability : unsigned int { UIContextAvailabilityNotAvailable, UIContextAvailabilityAvailable, UIContextAvailabilityActive };

enum DECLSPEC_DENUM _UIFontProperties : unsigned int { UIFontPropertiesNotAvailable, UIFontPropertiesNotSet, UIFontPropertiesSet };

enum DECLSPEC_DENUM _UIFontVerticalPosition : unsigned int { UIFontVerticalPositionNotAvailable, UIFontVerticalPositionNotSet, UIFontVerticalPositionSuperscript, UIFontVerticalPositionSubscript };

enum DECLSPEC_DENUM _UIFontUnderline : unsigned int { UIFontUnderlineNotAvailable, UIFontUnderlineNotSet, UIFontUnderlineSet };

enum DECLSPEC_DENUM _UIFontDeltaSize : unsigned int { UIFontDeltaSizeGrow, UIFontDeltaSizeShrink };

enum DECLSPEC_DENUM _UIControlDock : unsigned int { UIControlDockTop = 1, UIControlDockBottom = 3 };

enum DECLSPEC_DENUM _UISwatchColorType : unsigned int { UISwatchColorTypeNoColor, UISwatchColorTypeAutomatic, UISwatchColorTypeRgb };

enum DECLSPEC_DENUM _UISwatchColorMode : unsigned int { UISwatchColorModeNormal, UISwatchColorModeMonochrome };

__interface  INTERFACE_UUID("{C205BB48-5B1C-4219-A106-15BD0A5F24E2}") IUISimplePropertySet  : public System::IInterface 
{
	virtual HRESULT __stdcall GetValue(const _tagpropertykey &Key, /* out */ tagPROPVARIANT &Value) = 0 ;
};

__interface  INTERFACE_UUID("{803982AB-370A-4F7E-A9E7-8784036A6E26}") IUIRibbon  : public System::IInterface 
{
	virtual HRESULT __safecall GetHeight(unsigned &__GetHeight_result) = 0 ;
	virtual HRESULT __stdcall LoadSettingsFromStream(const _di_IStream Stream) = 0 ;
	virtual HRESULT __stdcall SaveSettingsToStream(const _di_IStream Stream) = 0 ;
};

enum DECLSPEC_DENUM _UIInvalidation : unsigned int { UIInvalidationsState, UIInvalidationsValue, UIInvalidationsProperty, UIInvalidationsAllProperties };

typedef System::Set<_UIInvalidation, _UIInvalidation::UIInvalidationsState, _UIInvalidation::UIInvalidationsAllProperties> _UIInvalidations;

__interface  INTERFACE_UUID("{F4F0385D-6872-43A8-AD09-4C339CB3F5C5}") IUIFramework  : public System::IInterface 
{
	virtual HRESULT __safecall Initialize(HWND FrameWnd, const _di_IUIApplication Application) = 0 ;
	virtual HRESULT __stdcall Destroy(void) = 0 ;
	virtual HRESULT __safecall LoadUI(NativeUInt Instance, System::WideChar * ResourceName) = 0 ;
	virtual HRESULT __safecall GetView(unsigned ViewId, const GUID &Riid, System::_di_IInterface &__GetView_result) = 0 ;
	virtual HRESULT __stdcall GetUICommandProperty(unsigned CommandId, const _tagpropertykey &Key, /* out */ tagPROPVARIANT &Value) = 0 ;
	virtual HRESULT __stdcall SetUICommandProperty(unsigned CommandId, const _tagpropertykey &Key, const tagPROPVARIANT &Value) = 0 ;
	virtual HRESULT __safecall InvalidateUICommand(unsigned CommandId, _UIInvalidations Flags, Winapi::Activex::PPropertyKey Key) = 0 ;
	virtual HRESULT __stdcall FlushPendingInvalidations(void) = 0 ;
	virtual HRESULT __safecall SetModes(unsigned Modes) = 0 ;
};

__interface  INTERFACE_UUID("{EEA11F37-7C46-437C-8E55-B52122B29293}") IUIContextualUI  : public System::IInterface 
{
	virtual HRESULT __safecall ShowAtLocation(int X, int Y) = 0 ;
};

__interface  INTERFACE_UUID("{DF4F45BF-6F9D-4DD7-9D68-D8F9CD18C4DB}") IUICollection  : public System::IInterface 
{
	virtual HRESULT __safecall GetCount(unsigned &__GetCount_result) = 0 ;
	virtual HRESULT __safecall GetItem(unsigned Index, System::_di_IInterface &__GetItem_result) = 0 ;
	virtual HRESULT __safecall Add(const System::_di_IInterface Item) = 0 ;
	virtual HRESULT __safecall Insert(unsigned Index, const System::_di_IInterface Item) = 0 ;
	virtual HRESULT __safecall RemoveAt(unsigned Index) = 0 ;
	virtual HRESULT __safecall Replace(unsigned IndexReplaces, const System::_di_IInterface ItemReplaceWith) = 0 ;
	virtual HRESULT __safecall Clear(void) = 0 ;
};

enum DECLSPEC_DENUM _UICollectionChange : unsigned int { UICollectionChangeInsert, UICollectionChangeRemove, UICollectionChangeReplace, UICollectionChangeReset };

__interface  INTERFACE_UUID("{6502AE91-A14D-44B5-BBD0-62AACC581D52}") IUICollectionChangedEvent  : public System::IInterface 
{
	virtual HRESULT __safecall OnChanged(_UICollectionChange Action, unsigned OldIndex, const System::_di_IInterface OldItem, unsigned NewIndex, const System::_di_IInterface NewItem) = 0 ;
};

enum DECLSPEC_DENUM _UIExecutionVerb : unsigned int { UIExecutionVerbExecute, UIExecutionVerbPreview, UIExecutionVerbCancelPreview };

__interface  INTERFACE_UUID("{75AE0A2D-DC03-4C9F-8883-069660D0BEB6}") IUICommandHandler  : public System::IInterface 
{
	virtual HRESULT __stdcall Execute(unsigned CommandId, _UIExecutionVerb Verb, Winapi::Activex::PPropertyKey Key, Winapi::Activex::PPropVariant Value, _di_IUISimplePropertySet CommandExecutionProperties) = 0 ;
	virtual HRESULT __stdcall UpdateProperty(unsigned CommandId, const _tagpropertykey &Key, Winapi::Activex::PPropVariant CurrentValue, tagPROPVARIANT &NewValue) = 0 ;
};

enum DECLSPEC_DENUM _UICommandType : unsigned int { UICommandTypeUnknown, UICommandTypeGroup, UICommandTypeAction, UICommandTypeAnchor, UICommandTypeContext, UICommandTypeCollection, UICommandTypeCommandCollection, UICommandTypeDecimal, UICommandTypeBoolean, UICommandTypeFont, UICommandTypeRecentItems, UICommandTypeColorAnchor, UICommandTypeColorCollection };

enum DECLSPEC_DENUM _UIViewType : unsigned int { UIViewTypeRibbon = 0x1 };

enum DECLSPEC_DENUM _UIViewVerb : unsigned int { UIViewVerbCreate, UIViewVerbDestroy, UIViewVerbSize, UIViewVerbError };

__interface  INTERFACE_UUID("{D428903C-729A-491D-910D-682A08FF2522}") IUIApplication  : public System::IInterface 
{
	virtual HRESULT __stdcall OnViewChanged(unsigned ViewId, _UIViewType TypeId, const System::_di_IInterface View, _UIViewVerb Verb, int ReasonCode) = 0 ;
	virtual HRESULT __stdcall OnCreateUICommand(unsigned CommandId, _UICommandType TypeId, /* out */ _di_IUICommandHandler &CommandHandler) = 0 ;
	virtual HRESULT __stdcall OnDestroyUICommand(unsigned CommandId, _UICommandType TypeId, const _di_IUICommandHandler CommandHandler) = 0 ;
};

__interface  INTERFACE_UUID("{23C8C838-4DE6-436B-AB01-5554BB7C30DD}") IUIImage  : public System::IInterface 
{
	virtual HRESULT __safecall GetBitmap(HBITMAP &__GetBitmap_result) = 0 ;
};

enum DECLSPEC_DENUM _UIOwnership : unsigned int { UIOwnershipTransfer, UIOwnershipCopy };

__interface  INTERFACE_UUID("{18ABA7F3-4C1C-4BA2-BF6C-F5C3326FA816}") IUIImageFromBitmap  : public System::IInterface 
{
	virtual HRESULT __safecall CreateImage(HBITMAP Bitmap, _UIOwnership Options, _di_IUIImage &__CreateImage_result) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION CoUIRibbonFramework : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	static _di_IUIFramework __fastcall Create();
public:
	/* TObject.Create */ inline __fastcall CoUIRibbonFramework(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~CoUIRibbonFramework(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION CoUIRibbonImageFromBitmapFactory : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	static _di_IUIImageFromBitmap __fastcall Create();
public:
	/* TObject.Create */ inline __fastcall CoUIRibbonImageFromBitmapFactory(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~CoUIRibbonImageFromBitmapFactory(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_Enabled;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_LabelDescription;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_Keytip;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_Label;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_TooltipDescription;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_TooltipTitle;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_LargeImage;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_LargeHighContrastImage;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_SmallImage;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_SmallHighContrastImage;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_CommandId;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_ItemsSource;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_Categories;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_CategoryId;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_SelectedItem;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_CommandType;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_ItemImage;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_BooleanValue;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_DecimalValue;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_StringValue;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_MaxValue;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_MinValue;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_Increment;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_DecimalPlaces;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_FormatString;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_RepresentativeString;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_FontProperties;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_FontProperties_Family;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_FontProperties_Size;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_FontProperties_Bold;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_FontProperties_Italic;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_FontProperties_Underline;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_FontProperties_Strikethrough;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_FontProperties_VerticalPositioning;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_FontProperties_ForegroundColor;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_FontProperties_BackgroundColor;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_FontProperties_ForegroundColorType;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_FontProperties_BackgroundColorType;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_FontProperties_ChangedProperties;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_FontProperties_DeltaSize;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_RecentItems;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_Pinned;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_Color;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_ColorType;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_ColorMode;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_ThemeColorsCategoryLabel;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_StandardColorsCategoryLabel;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_RecentColorsCategoryLabel;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_AutomaticColorLabel;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_NoColorLabel;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_MoreColorsLabel;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_ThemeColors;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_StandardColors;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_ThemeColorsTooltips;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_StandardColorsTooltips;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_Viewable;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_Minimized;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_QuickAccessToolbarDock;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_ContextAvailable;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_GlobalBackgroundColor;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_GlobalHighlightColor;
extern DELPHI_PACKAGE _tagpropertykey UI_PKEY_GlobalTextColor;
static const System::Int8 UIAllCommands = System::Int8(0x0);
static const System::Int8 UICollectionInvalidIndex = System::Int8(-1);
extern DELPHI_PACKAGE GUID CLSID_UIRibbonFramework;
extern DELPHI_PACKAGE GUID CLSID_UIRibbonImageFromBitmapFactory;
extern DELPHI_PACKAGE GUID LIBID_UIRibbon;
extern DELPHI_PACKAGE unsigned __fastcall UIHsb(const System::Byte Hue, const System::Byte Saturation, const System::Byte Brightness);
extern DELPHI_PACKAGE unsigned __fastcall UIMakeAppMode(const int X);
extern DELPHI_PACKAGE HRESULT __fastcall UIInitPropertyFromBoolean(const _tagpropertykey &Key, const bool Value, /* out */ tagPROPVARIANT &PropVar);
extern DELPHI_PACKAGE HRESULT __fastcall UIInitPropertyFromUInt32(const _tagpropertykey &Key, const unsigned Value, /* out */ tagPROPVARIANT &PropVar);
extern DELPHI_PACKAGE HRESULT __fastcall UIInitPropertyFromString(const _tagpropertykey &Key, const System::UnicodeString Value, /* out */ tagPROPVARIANT &PropVar);
extern DELPHI_PACKAGE HRESULT __fastcall UIInitPropertyFromDecimal(const _tagpropertykey &Key, const tagDEC &Value, /* out */ tagPROPVARIANT &PropVar);
extern DELPHI_PACKAGE HRESULT __fastcall UIInitPropertyFromInterface(const _tagpropertykey &Key, const System::_di_IInterface Value, /* out */ tagPROPVARIANT &PropVar);
extern DELPHI_PACKAGE HRESULT __fastcall UIInitPropertyFromImage(const _tagpropertykey &Key, const _di_IUIImage Value, /* out */ tagPROPVARIANT &PropVar);
extern DELPHI_PACKAGE HRESULT __fastcall UIInitPropertyFromIUnknownArray(const _tagpropertykey &Key, const Winapi::Activex::PSafeArray Value, /* out */ tagPROPVARIANT &PropVar);
extern DELPHI_PACKAGE HRESULT __fastcall UIPropertyToBoolean(const _tagpropertykey &Key, const tagPROPVARIANT &PropVar, /* out */ bool &Value);
extern DELPHI_PACKAGE HRESULT __fastcall UIPropertyToUInt32(const _tagpropertykey &Key, const tagPROPVARIANT &PropVar, /* out */ unsigned &Value);
extern DELPHI_PACKAGE HRESULT __fastcall UIPropertyToString(const _tagpropertykey &Key, const tagPROPVARIANT &PropVar, /* out */ System::UnicodeString &Value);
extern DELPHI_PACKAGE HRESULT __fastcall UIPropertyToStringAlloc(const _tagpropertykey &Key, const tagPROPVARIANT &PropVar, /* out */ System::UnicodeString &Value);
extern DELPHI_PACKAGE HRESULT __fastcall UIPropertyToDecimal(const _tagpropertykey &Key, const tagPROPVARIANT &PropVar, /* out */ tagDEC &Value);
extern DELPHI_PACKAGE HRESULT __fastcall UIPropertyToInterface(const _tagpropertykey &Key, const tagPROPVARIANT &PropVar, /* out */ System::_di_IInterface &Value);
extern DELPHI_PACKAGE HRESULT __fastcall UIPropertyToImage(const _tagpropertykey &Key, const tagPROPVARIANT &PropVar, /* out */ _di_IUIImage &Value);
extern DELPHI_PACKAGE HRESULT __fastcall UIPropertyToIUnknownArrayAlloc(const _tagpropertykey &Key, const tagPROPVARIANT &PropVar, /* out */ Winapi::Activex::PSafeArray &Value);
}	/* namespace Uiribbonapi */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UIRIBBONAPI)
using namespace Uiribbonapi;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UiribbonapiHPP
