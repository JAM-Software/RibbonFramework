// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UIRibbonCommands.pas' rev: 30.00 (Windows)

#ifndef UiribboncommandsHPP
#define UiribboncommandsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Generics.Collections.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Classes.hpp>
#include <Winapi.ActiveX.hpp>
#include <System.Rtti.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Winapi.RichEdit.hpp>
#include <Winapi.PropSys.hpp>
#include <Vcl.ActnList.hpp>
#include <UIRibbonApi.hpp>
#include <Vcl.Imaging.pngimage.hpp>
#include <System.UITypes.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Uiribboncommands
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUICommandExecutionProperties;
class DELPHICLASS TUICommand;
class DELPHICLASS TUICommandGroup;
struct TUICommandActionEventArgs;
class DELPHICLASS TUICommandAction;
struct TUICommandCollectionEventArgs;
class DELPHICLASS TUICommandCollection;
class DELPHICLASS TUICommandDecimal;
struct TUICommandBooleanEventArgs;
class DELPHICLASS TUICommandBoolean;
class DELPHICLASS TUICommandAnchor;
class DELPHICLASS TUICommandContext;
class DELPHICLASS TUIFont;
struct TUICommandFontEventArgs;
class DELPHICLASS TUICommandFont;
struct TUICommandColorEventArgs;
class DELPHICLASS TUICommandColorAnchor;
class DELPHICLASS TUICommandRecentItems;
__interface IUICollectionItem;
typedef System::DelphiInterface<IUICollectionItem> _di_IUICollectionItem;
class DELPHICLASS TUICollectionItem;
class DELPHICLASS TUICollection;
class DELPHICLASS TUIGalleryCollectionItem;
class DELPHICLASS TUIRecentItem;
class DELPHICLASS TUIImage;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TUICommandType : unsigned char { ctUnknown, ctGroup, ctAction, ctAnchor, ctContext, ctCollection, ctCommandCollection, ctDecimal, ctBoolean, ctFont, ctRecentItems, ctColorAnchor, ctColorCollection };

enum DECLSPEC_DENUM TUICommandVerb : unsigned char { cvExecute, cvPreview, cvCancelPreview };

enum DECLSPEC_DENUM TUICommandInvalidation : unsigned char { ciState, ciValue, ciProperty, ciAllProperties };

typedef System::Set<TUICommandInvalidation, TUICommandInvalidation::ciState, TUICommandInvalidation::ciAllProperties> TUICommandInvalidations;

enum DECLSPEC_DENUM TUISwatchColorType : unsigned char { ctNoColor, ctAutomatic, ctRgb };

enum DECLSPEC_DENUM TUIContextAvailability : unsigned char { caNotAvailable, caAvailable, caActive };

enum DECLSPEC_DENUM TUIProperty: short { upNone = -1, upUnknown, upEnabled, upLabelDescription, upKeytip, upLabel, upTooltipDescription, upTooltipTitle, upLargeImage, upLargeHighContrastImage, upSmallImage, upSmallHighContrastImage, upCommandId = 100, upItemsSource, upCategories, upCategoryId, upSelectedItem, upCommandType, upItemImage, upBooleanValue = 200, upDecimalValue, upStringValue, upMaxValue, upMinValue, upIncrement, upDecimalPlaces, upFormatString, upRepresentativeString, upFontProperties = 300, upFontPropertiesFamily, upFontPropertiesSize, upFontPropertiesBold, upFontPropertiesItalic, upFontPropertiesUnderline, upFontPropertiesStrikethrough, upFontPropertiesVerticalPositioning, upFontPropertiesForegroundColor, upFontPropertiesBackgroundColor, 
	upFontPropertiesForegroundColorType, upFontPropertiesBackgroundColorType, upFontPropertiesChangedProperties, upFontPropertiesDeltaSize, upRecentItems = 350, upPinned, upColor = 400, upColorType, upColorMode, upThemeColorsCategoryLabel, upStandardColorsCategoryLabel, upRecentColorsCategoryLabel, upAutomaticColorLabel, upNoColorLabel, upMoreColorsLabel, upThemeColors, upStandardColors, upThemeColorsTooltips, upStandardColorsTooltips, upViewable = 1000, upMinimized, upQuickAccessToolbarDock, upContextAvailable = 1100, upGlobalBackgroundColor = 2000, upGlobalHighlightColor, upGlobalTextColor };

class PASCALIMPLEMENTATION TUICommandExecutionProperties : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Uiribbonapi::_di_IUISimplePropertySet FPropertySet;
	
public:
	bool __fastcall GetValue(const TUIProperty Prop, /* out */ unsigned &Value)/* overload */;
	bool __fastcall GetValue(const TUIProperty Prop, /* out */ System::UnicodeString &Value)/* overload */;
public:
	/* TObject.Create */ inline __fastcall TUICommandExecutionProperties(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TUICommandExecutionProperties(void) { }
	
};


typedef void __fastcall (__closure *TUICommandUpdateImageEvent)(System::TObject* Sender, const Uiribbonapi::TUIPropertyKey &PropKey, /* out */ tagPROPVARIANT &NewValue, bool &Handled);

typedef void __fastcall (__closure *TUICommandUpdateHintEvent)(System::TObject* Sender, const System::UnicodeString Value);

class PASCALIMPLEMENTATION TUICommand : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
	
private:
	enum DECLSPEC_DENUM _TUICommand__1 : unsigned char { vpCaption, vpKeytip, vpTooltipTitle, vpTooltipDescription, vpLabelDescription, vpMinValue, vpMaxValue, vpIncrement, vpDecimalPlaces, vpRepresentativeString, vpFormatString };
	
	
private:
	static const System::Int8 NOTIFY_CACHED_PROPERTIES = System::Int8(0x1f);
	
	
protected:
	static TUICommandExecutionProperties* FProperties;
	
protected:
	Uiribbonapi::_di_IUIFramework FFramework;
	unsigned FCommandId;
	int FTag;
	bool FAlive;
	System::Classes::TShortCut FShortCut;
	System::UnicodeString FCaption;
	System::UnicodeString FKeytip;
	System::UnicodeString FTooltipTitle;
	System::UnicodeString FTooltipDescription;
	TUIImage* FLargeImage;
	TUIImage* FSmallImage;
	TUIImage* FLargeHighContrastImage;
	TUIImage* FSmallHighContrastImage;
	System::Generics::Collections::TDictionary__2<Uiribbonapi::TUIPropertyKey,System::Rtti::TValue>* FCachedProperties;
	System::Set<_TUICommand__1, _TUICommand__1::vpCaption, _TUICommand__1::vpFormatString> FValidProperties;
	Vcl::Actnlist::TActionLink* FActionLink;
	TUICommandUpdateHintEvent FOnUpdateHint;
	TUICommandUpdateImageEvent FOnUpdateImage;
	void __fastcall SetAlive(const bool Value);
	
private:
	bool __fastcall GetEnabled(void);
	void __fastcall SetEnabled(const bool Value);
	void __fastcall SetCaption(const System::UnicodeString Value);
	void __fastcall SetKeytip(const System::UnicodeString Value);
	void __fastcall SetTooltipTitle(const System::UnicodeString Value);
	void __fastcall SetTooltipDescription(const System::UnicodeString Value);
	Vcl::Actnlist::TActionLink* __fastcall GetActionLink(void);
	
private:
	HIDESBASE int __stdcall _AddRef(void);
	HIDESBASE int __stdcall _Release(void);
	HRESULT __stdcall Execute(unsigned CommandId, Uiribbonapi::_UIExecutionVerb Verb, Uiribbonapi::PUIPropertyKey Key, Winapi::Activex::PPropVariant CurrentValue, Uiribbonapi::_di_IUISimplePropertySet CommandExecutionProperties);
	HRESULT __stdcall UpdateProperty(unsigned CommandId, const Uiribbonapi::TUIPropertyKey &Key, Winapi::Activex::PPropVariant CurrentValue, /* out */ tagPROPVARIANT &NewValue);
	
private:
	void __fastcall ImageChanged(System::TObject* Sender);
	
protected:
	void __fastcall GetPropertyValue(const Uiribbonapi::TUIPropertyKey &Key, /* out */ bool &Value)/* overload */;
	void __fastcall GetPropertyValue(const Uiribbonapi::TUIPropertyKey &Key, /* out */ unsigned &Value)/* overload */;
	void __fastcall GetPropertyValue(const Uiribbonapi::TUIPropertyKey &Key, /* out */ int &Value)/* overload */;
	void __fastcall GetPropertyValue(const Uiribbonapi::TUIPropertyKey &Key, /* out */ System::UnicodeString &Value)/* overload */;
	void __fastcall GetPropertyValue(const Uiribbonapi::TUIPropertyKey &Key, /* out */ double &Value)/* overload */;
	void __fastcall SetPropertyValue(const Uiribbonapi::TUIPropertyKey &Key, const bool Value)/* overload */;
	void __fastcall SetPropertyValue(const Uiribbonapi::TUIPropertyKey &Key, const unsigned Value)/* overload */;
	void __fastcall SetPropertyValue(const Uiribbonapi::TUIPropertyKey &Key, const int Value)/* overload */;
	void __fastcall SetPropertyValue(const Uiribbonapi::TUIPropertyKey &Key, const System::UnicodeString Value)/* overload */;
	void __fastcall SetPropertyValue(const Uiribbonapi::TUIPropertyKey &Key, const double Value)/* overload */;
	void __fastcall CachePropertyValue(const Uiribbonapi::TUIPropertyKey &Key, const System::Rtti::TValue &Value);
	virtual void __fastcall DoExecute(const TUIProperty Prop, const TUICommandVerb Verb, const Winapi::Activex::PPropVariant CurrentValue, HRESULT &Result) = 0 ;
	virtual void __fastcall DoUpdate(const TUIProperty Prop, const Winapi::Activex::PPropVariant CurrentValue, /* out */ tagPROPVARIANT &NewValue, HRESULT &Result);
	virtual Vcl::Actnlist::TActionLink* __fastcall CreateActionLink(void) = 0 ;
	
protected:
	virtual void __fastcall Notify(const int Flag);
	__property bool Alive = {read=FAlive, write=SetAlive, nodefault};
	
public:
	__fastcall virtual TUICommand(System::TObject* const Ribbon, const unsigned CommandId);
	__fastcall virtual ~TUICommand(void);
	HIDESBASE void __fastcall Assign(Vcl::Actnlist::TCustomAction* const pAction);
	virtual __classmethod TUICommandType __fastcall CommandType() = 0 ;
	void __fastcall SetShortCut(const System::Classes::TShiftState Shift, const System::Word Key)/* overload */;
	void __fastcall SetShortCut(const System::Classes::TShiftState Shift, const System::WideChar Key)/* overload */;
	virtual bool __fastcall HandleShortCut(const System::Classes::TShortCut ShortCut);
	__property int Tag = {read=FTag, write=FTag, nodefault};
	__property unsigned CommandId = {read=FCommandId, nodefault};
	__property bool Enabled = {read=GetEnabled, write=SetEnabled, nodefault};
	__property System::UnicodeString Caption = {read=FCaption, write=SetCaption};
	__property System::UnicodeString Keytip = {read=FKeytip, write=SetKeytip};
	__property System::Classes::TShortCut ShortCut = {read=FShortCut, write=FShortCut, nodefault};
	__property System::UnicodeString TooltipTitle = {read=FTooltipTitle, write=SetTooltipTitle};
	__property System::UnicodeString TooltipDescription = {read=FTooltipDescription, write=SetTooltipDescription};
	__property TUIImage* LargeImage = {read=FLargeImage};
	__property TUIImage* SmallImage = {read=FSmallImage};
	__property TUIImage* LargeHighContrastImage = {read=FLargeHighContrastImage};
	__property TUIImage* SmallHighContrastImage = {read=FSmallHighContrastImage};
	__property Vcl::Actnlist::TActionLink* ActionLink = {read=GetActionLink};
	__property TUICommandUpdateImageEvent OnUpdateImage = {read=FOnUpdateImage, write=FOnUpdateImage};
	__property TUICommandUpdateHintEvent OnUpdateHint = {read=FOnUpdateHint, write=FOnUpdateHint};
private:
	void *__IUICommandHandler;	// Uiribbonapi::IUICommandHandler 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {75AE0A2D-DC03-4C9F-8883-069660D0BEB6}
	operator Uiribbonapi::_di_IUICommandHandler()
	{
		Uiribbonapi::_di_IUICommandHandler intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Uiribbonapi::IUICommandHandler*(void) { return (Uiribbonapi::IUICommandHandler*)&__IUICommandHandler; }
	#endif
	
};


_DECLARE_METACLASS(System::TMetaClass, TUICommandClass);

class PASCALIMPLEMENTATION TUICommandGroup : public TUICommand
{
	typedef TUICommand inherited;
	
protected:
	virtual void __fastcall DoExecute(const TUIProperty Prop, const TUICommandVerb Verb, const Winapi::Activex::PPropVariant CurrentValue, HRESULT &Result);
	virtual Vcl::Actnlist::TActionLink* __fastcall CreateActionLink(void);
	
public:
	__classmethod virtual TUICommandType __fastcall CommandType();
public:
	/* TUICommand.Create */ inline __fastcall virtual TUICommandGroup(System::TObject* const Ribbon, const unsigned CommandId) : TUICommand(Ribbon, CommandId) { }
	/* TUICommand.Destroy */ inline __fastcall virtual ~TUICommandGroup(void) { }
	
};


struct DECLSPEC_DRECORD TUICommandActionEventArgs
{
public:
	TUICommandAction* Command;
	TUICommandVerb Verb;
	TUICommandExecutionProperties* Properties;
};


typedef void __fastcall (__closure *TUICommandActionExecuteEvent)(const TUICommandActionEventArgs &Args);

class PASCALIMPLEMENTATION TUICommandAction : public TUICommand
{
	typedef TUICommand inherited;
	
private:
	System::UnicodeString FLabelDescription;
	TUICommandActionExecuteEvent FOnExecute;
	void __fastcall SetLabelDescription(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall DoExecute(const TUIProperty Prop, const TUICommandVerb Verb, const Winapi::Activex::PPropVariant CurrentValue, HRESULT &Result);
	virtual void __fastcall DoUpdate(const TUIProperty Prop, const Winapi::Activex::PPropVariant CurrentValue, /* out */ tagPROPVARIANT &NewValue, HRESULT &Result);
	virtual Vcl::Actnlist::TActionLink* __fastcall CreateActionLink(void);
	
public:
	__classmethod virtual TUICommandType __fastcall CommandType();
	virtual bool __fastcall HandleShortCut(const System::Classes::TShortCut ShortCut);
	__property System::UnicodeString LabelDescription = {read=FLabelDescription, write=SetLabelDescription};
	__property TUICommandActionExecuteEvent OnExecute = {read=FOnExecute, write=FOnExecute};
public:
	/* TUICommand.Create */ inline __fastcall virtual TUICommandAction(System::TObject* const Ribbon, const unsigned CommandId) : TUICommand(Ribbon, CommandId) { }
	/* TUICommand.Destroy */ inline __fastcall virtual ~TUICommandAction(void) { }
	
};


struct DECLSPEC_DRECORD TUICommandCollectionEventArgs
{
public:
	TUICommandCollection* Command;
	TUICommandVerb Verb;
	int ItemIndex;
	TUICommandExecutionProperties* Properties;
};


typedef void __fastcall (__closure *TUICommandCollectionSelectEvent)(const TUICommandCollectionEventArgs &Args);

class PASCALIMPLEMENTATION TUICommandCollection : public TUICommand
{
	typedef TUICommand inherited;
	
private:
	static const System::Int8 NOTIFY_ITEMS = System::Int8(0x1);
	
	static const System::Int8 NOTIFY_CATEGORIES = System::Int8(0x2);
	
	TUICollection* FItems;
	TUICollection* FCategories;
	System::UnicodeString FRepresentativeString;
	TUICommandCollectionSelectEvent FOnSelect;
	int __fastcall GetSelectedItem(void);
	void __fastcall SetSelectedItem(const int Value);
	System::UnicodeString __fastcall GetText(void);
	void __fastcall SetText(const System::UnicodeString Value);
	bool __fastcall GetChecked(void);
	void __fastcall SetChecked(const bool Value);
	void __fastcall SetRepresentativeString(const System::UnicodeString Value);
	void __fastcall ItemsChange(TUICollection* const Collection, const bool Immediate);
	void __fastcall CategoriesChange(TUICollection* const Collection, const bool Immediate);
	
protected:
	virtual void __fastcall DoExecute(const TUIProperty Prop, const TUICommandVerb Verb, const Winapi::Activex::PPropVariant CurrentValue, HRESULT &Result);
	virtual void __fastcall DoUpdate(const TUIProperty Prop, const Winapi::Activex::PPropVariant CurrentValue, /* out */ tagPROPVARIANT &NewValue, HRESULT &Result);
	virtual Vcl::Actnlist::TActionLink* __fastcall CreateActionLink(void);
	
protected:
	virtual void __fastcall Notify(const int Flag);
	
public:
	__fastcall virtual TUICommandCollection(System::TObject* const Ribbon, const unsigned CommandId);
	__fastcall virtual ~TUICommandCollection(void);
	__classmethod virtual TUICommandType __fastcall CommandType();
	virtual bool __fastcall HandleShortCut(const System::Classes::TShortCut ShortCut);
	__property TUICollection* Items = {read=FItems};
	__property TUICollection* Categories = {read=FCategories};
	__property int SelectedItem = {read=GetSelectedItem, write=SetSelectedItem, nodefault};
	__property System::UnicodeString Text = {read=GetText, write=SetText};
	__property System::UnicodeString RepresentativeString = {read=FRepresentativeString, write=SetRepresentativeString};
	__property bool Checked = {read=GetChecked, write=SetChecked, nodefault};
	__property TUICommandCollectionSelectEvent OnSelect = {read=FOnSelect, write=FOnSelect};
};


typedef void __fastcall (__closure *TUICommandDecimalChangeEvent)(TUICommandDecimal* const Command, const TUICommandVerb Verb, const double Value, TUICommandExecutionProperties* const Properties);

class PASCALIMPLEMENTATION TUICommandDecimal : public TUICommand
{
	typedef TUICommand inherited;
	
private:
	double FMinValue;
	double FMaxValue;
	double FIncrement;
	int FDecimalPlaces;
	System::UnicodeString FRepresentativeString;
	System::UnicodeString FFormatString;
	TUICommandDecimalChangeEvent FOnChange;
	double __fastcall GetValue(void);
	void __fastcall SetValue(const double Value);
	void __fastcall SetMaxValue(const double Value);
	void __fastcall SetMinValue(const double Value);
	void __fastcall SetIncrement(const double Value);
	void __fastcall SetDecimalPlaces(const int Value);
	void __fastcall SetRepresentativeString(const System::UnicodeString Value);
	void __fastcall SetFormatString(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall DoExecute(const TUIProperty Prop, const TUICommandVerb Verb, const Winapi::Activex::PPropVariant CurrentValue, HRESULT &Result);
	virtual void __fastcall DoUpdate(const TUIProperty Prop, const Winapi::Activex::PPropVariant CurrentValue, /* out */ tagPROPVARIANT &NewValue, HRESULT &Result);
	virtual Vcl::Actnlist::TActionLink* __fastcall CreateActionLink(void);
	
public:
	__classmethod virtual TUICommandType __fastcall CommandType();
	virtual bool __fastcall HandleShortCut(const System::Classes::TShortCut ShortCut);
	__property double Value = {read=GetValue, write=SetValue};
	__property double MinValue = {read=FMinValue, write=SetMinValue};
	__property double MaxValue = {read=FMaxValue, write=SetMaxValue};
	__property double Increment = {read=FIncrement, write=SetIncrement};
	__property int DecimalPlaces = {read=FDecimalPlaces, write=SetDecimalPlaces, nodefault};
	__property System::UnicodeString RepresentativeString = {read=FRepresentativeString, write=SetRepresentativeString};
	__property System::UnicodeString FormatString = {read=FFormatString, write=SetFormatString};
	__property TUICommandDecimalChangeEvent OnChange = {read=FOnChange, write=FOnChange};
public:
	/* TUICommand.Create */ inline __fastcall virtual TUICommandDecimal(System::TObject* const Ribbon, const unsigned CommandId) : TUICommand(Ribbon, CommandId) { }
	/* TUICommand.Destroy */ inline __fastcall virtual ~TUICommandDecimal(void) { }
	
};


struct DECLSPEC_DRECORD TUICommandBooleanEventArgs
{
public:
	TUICommandBoolean* Command;
	TUICommandVerb Verb;
	bool Checked;
	TUICommandExecutionProperties* Properties;
};


typedef void __fastcall (__closure *TUICommandBooleanToggleEvent)(const TUICommandBooleanEventArgs &Args);

class PASCALIMPLEMENTATION TUICommandBoolean : public TUICommand
{
	typedef TUICommand inherited;
	
private:
	System::UnicodeString FLabelDescription;
	TUICommandBooleanToggleEvent FOnToggle;
	bool __fastcall GetChecked(void);
	void __fastcall SetChecked(const bool Value);
	void __fastcall SetLabelDescription(const System::UnicodeString Value);
	
private:
	void __fastcall DoToggle(const TUICommandVerb Verb, const Winapi::Activex::PPropVariant CurrentValue);
	
protected:
	virtual void __fastcall DoExecute(const TUIProperty Prop, const TUICommandVerb Verb, const Winapi::Activex::PPropVariant CurrentValue, HRESULT &Result);
	virtual void __fastcall DoUpdate(const TUIProperty Prop, const Winapi::Activex::PPropVariant CurrentValue, /* out */ tagPROPVARIANT &NewValue, HRESULT &Result);
	virtual Vcl::Actnlist::TActionLink* __fastcall CreateActionLink(void);
	
public:
	__classmethod virtual TUICommandType __fastcall CommandType();
	virtual bool __fastcall HandleShortCut(const System::Classes::TShortCut ShortCut);
	__property System::UnicodeString LabelDescription = {read=FLabelDescription, write=SetLabelDescription};
	__property bool Checked = {read=GetChecked, write=SetChecked, nodefault};
	__property TUICommandBooleanToggleEvent OnToggle = {read=FOnToggle, write=FOnToggle};
public:
	/* TUICommand.Create */ inline __fastcall virtual TUICommandBoolean(System::TObject* const Ribbon, const unsigned CommandId) : TUICommand(Ribbon, CommandId) { }
	/* TUICommand.Destroy */ inline __fastcall virtual ~TUICommandBoolean(void) { }
	
};


class PASCALIMPLEMENTATION TUICommandAnchor : public TUICommand
{
	typedef TUICommand inherited;
	
protected:
	virtual void __fastcall DoExecute(const TUIProperty Prop, const TUICommandVerb Verb, const Winapi::Activex::PPropVariant CurrentValue, HRESULT &Result);
	virtual Vcl::Actnlist::TActionLink* __fastcall CreateActionLink(void);
	
public:
	__classmethod virtual TUICommandType __fastcall CommandType();
public:
	/* TUICommand.Create */ inline __fastcall virtual TUICommandAnchor(System::TObject* const Ribbon, const unsigned CommandId) : TUICommand(Ribbon, CommandId) { }
	/* TUICommand.Destroy */ inline __fastcall virtual ~TUICommandAnchor(void) { }
	
};


class PASCALIMPLEMENTATION TUICommandContext : public TUICommand
{
	typedef TUICommand inherited;
	
private:
	TUIContextAvailability __fastcall GetAvailability(void);
	void __fastcall SetAvailability(const TUIContextAvailability Value);
	
protected:
	virtual void __fastcall DoExecute(const TUIProperty Prop, const TUICommandVerb Verb, const Winapi::Activex::PPropVariant CurrentValue, HRESULT &Result);
	virtual Vcl::Actnlist::TActionLink* __fastcall CreateActionLink(void);
	
public:
	__classmethod virtual TUICommandType __fastcall CommandType();
	__property TUIContextAvailability Availability = {read=GetAvailability, write=SetAvailability, nodefault};
public:
	/* TUICommand.Create */ inline __fastcall virtual TUICommandContext(System::TObject* const Ribbon, const unsigned CommandId) : TUICommand(Ribbon, CommandId) { }
	/* TUICommand.Destroy */ inline __fastcall virtual ~TUICommandContext(void) { }
	
};


enum DECLSPEC_DENUM TUIFontVerticalPositioning : unsigned char { vpNotAvailable, vpDefault, vpSuperscript, vpSubscript };

enum DECLSPEC_DENUM TUIFontProperty : unsigned char { fpNotAvailable, fpOff, fpOn };

class PASCALIMPLEMENTATION TUIFont : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TUICommandFont* FOwner;
	System::UnicodeString FFamily;
	float FSize;
	TUIFontProperty FBold;
	TUIFontProperty FItalic;
	TUIFontProperty FUnderline;
	TUIFontProperty FStrikethrough;
	TUIFontVerticalPositioning FVerticalPositioning;
	System::Uitypes::TColor FForegroundColor;
	System::Uitypes::TColor FBackgroundColor;
	void __fastcall SetBackgroundColor(const System::Uitypes::TColor Value);
	void __fastcall SetBold(const TUIFontProperty Value);
	void __fastcall SetFamily(const System::UnicodeString Value);
	void __fastcall SetForegroundColor(const System::Uitypes::TColor Value);
	void __fastcall SetItalic(const TUIFontProperty Value);
	void __fastcall SetSize(const float Value);
	void __fastcall SetStrikethrough(const TUIFontProperty Value);
	void __fastcall SetUnderline(const TUIFontProperty Value);
	void __fastcall SetVerticalPositioning(const TUIFontVerticalPositioning Value);
	void __fastcall Changed(void);
	
private:
	void __fastcall Assign(const _di_IPropertyStore Source)/* overload */;
	void __fastcall AssignTo(const _di_IPropertyStore Dest)/* overload */;
	
public:
	__fastcall TUIFont(TUICommandFont* const Owner);
	void __fastcall Assign(Vcl::Graphics::TFont* const Source)/* overload */;
	void __fastcall Assign(Vcl::Comctrls::TTextAttributes* const Source)/* overload */;
	void __fastcall Assign(const Winapi::Richedit::_CHARFORMAT2W &Source)/* overload */;
	void __fastcall AssignTo(Vcl::Graphics::TFont* const Dest)/* overload */;
	void __fastcall AssignTo(Vcl::Comctrls::TTextAttributes* const Dest)/* overload */;
	void __fastcall AssignTo(/* out */ Winapi::Richedit::_CHARFORMAT2W &Dest)/* overload */;
	__property System::UnicodeString Family = {read=FFamily, write=SetFamily};
	__property float Size = {read=FSize, write=SetSize};
	__property TUIFontProperty Bold = {read=FBold, write=SetBold, nodefault};
	__property TUIFontProperty Italic = {read=FItalic, write=SetItalic, nodefault};
	__property TUIFontProperty Underline = {read=FUnderline, write=SetUnderline, nodefault};
	__property TUIFontProperty Strikethrough = {read=FStrikethrough, write=SetStrikethrough, nodefault};
	__property TUIFontVerticalPositioning VerticalPositioning = {read=FVerticalPositioning, write=SetVerticalPositioning, nodefault};
	__property System::Uitypes::TColor ForegroundColor = {read=FForegroundColor, write=SetForegroundColor, nodefault};
	__property System::Uitypes::TColor BackgroundColor = {read=FBackgroundColor, write=SetBackgroundColor, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TUIFont(void) { }
	
};


struct DECLSPEC_DRECORD TUICommandFontEventArgs
{
public:
	TUICommandFont* Command;
	TUICommandVerb Verb;
	TUIFont* Font;
	TUICommandExecutionProperties* Properties;
};


typedef void __fastcall (__closure *TUICommandFontChangedEvent)(const TUICommandFontEventArgs &Args);

class PASCALIMPLEMENTATION TUICommandFont : public TUICommand
{
	typedef TUICommand inherited;
	
private:
	static const System::Int8 NOTIFY_FONT = System::Int8(0x0);
	
	TUIFont* FFont;
	TUICommandFontChangedEvent FOnChanged;
	
protected:
	virtual void __fastcall DoExecute(const TUIProperty Prop, const TUICommandVerb Verb, const Winapi::Activex::PPropVariant CurrentValue, HRESULT &Result);
	virtual void __fastcall DoUpdate(const TUIProperty Prop, const Winapi::Activex::PPropVariant CurrentValue, /* out */ tagPROPVARIANT &NewValue, HRESULT &Result);
	virtual Vcl::Actnlist::TActionLink* __fastcall CreateActionLink(void);
	
protected:
	virtual void __fastcall Notify(const int Flag);
	
private:
	void __fastcall FontChanged(void);
	
public:
	__fastcall virtual TUICommandFont(System::TObject* const Ribbon, const unsigned CommandId);
	__fastcall virtual ~TUICommandFont(void);
	__classmethod virtual TUICommandType __fastcall CommandType();
	__property TUIFont* Font = {read=FFont};
	__property TUICommandFontChangedEvent OnChanged = {read=FOnChanged, write=FOnChanged};
};


struct DECLSPEC_DRECORD TUICommandColorEventArgs
{
public:
	TUICommandColorAnchor* Command;
	TUICommandVerb Verb;
	TUISwatchColorType ColorType;
	System::Uitypes::TColor CustomColor;
	TUICommandExecutionProperties* Properties;
};


typedef void __fastcall (__closure *TUICommandColorAnchorExecuteEvent)(const TUICommandColorEventArgs &Args);

class PASCALIMPLEMENTATION TUICommandColorAnchor : public TUICommand
{
	typedef TUICommand inherited;
	
private:
	TUICommandColorAnchorExecuteEvent FOnExecute;
	System::Uitypes::TColor __fastcall GetColor(void);
	void __fastcall SetColor(const System::Uitypes::TColor Value);
	TUISwatchColorType __fastcall GetColorType(void);
	void __fastcall SetColorType(const TUISwatchColorType Value);
	System::TArray__1<System::Uitypes::TColor> __fastcall GetStandardColors(void);
	void __fastcall SetStandardColors(const System::TArray__1<System::Uitypes::TColor> Value);
	System::TArray__1<System::UnicodeString> __fastcall GetStandardColorTooltips(void);
	void __fastcall SetStandardColorTooltips(const System::TArray__1<System::UnicodeString> Value);
	System::TArray__1<System::Uitypes::TColor> __fastcall GetThemeColors(void);
	void __fastcall SetThemeColors(const System::TArray__1<System::Uitypes::TColor> Value);
	System::TArray__1<System::UnicodeString> __fastcall GetThemeColorTooltips(void);
	void __fastcall SetThemeColorTooltips(const System::TArray__1<System::UnicodeString> Value);
	System::UnicodeString __fastcall GetThemeColorsCategoryLabel(void);
	void __fastcall SetThemeColorsCategoryLabel(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetStandardColorsCategoryLabel(void);
	void __fastcall SetStandardColorsCategoryLabel(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetRecentColorsCategoryLabel(void);
	void __fastcall SetRecentColorsCategoryLabel(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetAutomaticColorLabel(void);
	void __fastcall SetAutomaticColorLabel(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetNoColorLabel(void);
	void __fastcall SetNoColorLabel(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetMoreColorsLabel(void);
	void __fastcall SetMoreColorsLabel(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall DoExecute(const TUIProperty Prop, const TUICommandVerb Verb, const Winapi::Activex::PPropVariant CurrentValue, HRESULT &Result);
	virtual Vcl::Actnlist::TActionLink* __fastcall CreateActionLink(void);
	
public:
	__classmethod virtual TUICommandType __fastcall CommandType();
	virtual bool __fastcall HandleShortCut(const System::Classes::TShortCut ShortCut);
	__property TUISwatchColorType ColorType = {read=GetColorType, write=SetColorType, nodefault};
	__property System::Uitypes::TColor Color = {read=GetColor, write=SetColor, nodefault};
	__property System::TArray__1<System::Uitypes::TColor> StandardColors = {read=GetStandardColors, write=SetStandardColors};
	__property System::TArray__1<System::UnicodeString> StandardColorTooltips = {read=GetStandardColorTooltips, write=SetStandardColorTooltips};
	__property System::UnicodeString StandardColorsCategoryLabel = {read=GetStandardColorsCategoryLabel, write=SetStandardColorsCategoryLabel};
	__property System::TArray__1<System::Uitypes::TColor> ThemeColors = {read=GetThemeColors, write=SetThemeColors};
	__property System::TArray__1<System::UnicodeString> ThemeColorTooltips = {read=GetThemeColorTooltips, write=SetThemeColorTooltips};
	__property System::UnicodeString ThemeColorsCategoryLabel = {read=GetThemeColorsCategoryLabel, write=SetThemeColorsCategoryLabel};
	__property System::UnicodeString RecentColorsCategoryLabel = {read=GetRecentColorsCategoryLabel, write=SetRecentColorsCategoryLabel};
	__property System::UnicodeString AutomaticColorLabel = {read=GetAutomaticColorLabel, write=SetAutomaticColorLabel};
	__property System::UnicodeString NoColorLabel = {read=GetNoColorLabel, write=SetNoColorLabel};
	__property System::UnicodeString MoreColorsLabel = {read=GetMoreColorsLabel, write=SetMoreColorsLabel};
	__property TUICommandColorAnchorExecuteEvent OnExecute = {read=FOnExecute, write=FOnExecute};
public:
	/* TUICommand.Create */ inline __fastcall virtual TUICommandColorAnchor(System::TObject* const Ribbon, const unsigned CommandId) : TUICommand(Ribbon, CommandId) { }
	/* TUICommand.Destroy */ inline __fastcall virtual ~TUICommandColorAnchor(void) { }
	
};


typedef void __fastcall (__closure *TUICommandRecentItemsSelectEvent)(TUICommandRecentItems* const Command, const TUICommandVerb Verb, const int ItemIndex, TUICommandExecutionProperties* const Properties);

class PASCALIMPLEMENTATION TUICommandRecentItems : public TUICommand
{
	typedef TUICommand inherited;
	
private:
	static const System::Int8 NOTIFY_ITEMS = System::Int8(0x1);
	
	TUICollection* FItems;
	TUICommandRecentItemsSelectEvent FOnSelect;
	void __fastcall ItemsChange(TUICollection* const Collection, const bool Immediate);
	
protected:
	virtual void __fastcall DoUpdate(const TUIProperty Prop, const Winapi::Activex::PPropVariant CurrentValue, /* out */ tagPROPVARIANT &NewValue, HRESULT &Result);
	virtual void __fastcall DoExecute(const TUIProperty Prop, const TUICommandVerb Verb, const Winapi::Activex::PPropVariant CurrentValue, HRESULT &Result);
	virtual Vcl::Actnlist::TActionLink* __fastcall CreateActionLink(void);
	
protected:
	virtual void __fastcall Notify(const int Flag);
	
public:
	__fastcall virtual TUICommandRecentItems(System::TObject* const Ribbon, const unsigned CommandId);
	__fastcall virtual ~TUICommandRecentItems(void);
	__classmethod virtual TUICommandType __fastcall CommandType();
	__property TUICollection* Items = {read=FItems};
	__property TUICommandRecentItemsSelectEvent OnSelect = {read=FOnSelect, write=FOnSelect};
};


__interface  INTERFACE_UUID("{C50153F6-8951-4316-BBFF-3B639E6DA20A}") IUICollectionItem  : public System::IInterface 
{
	
};

class PASCALIMPLEMENTATION TUICollectionItem : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	HRESULT __stdcall GetValue(const Uiribbonapi::TUIPropertyKey &Key, /* out */ tagPROPVARIANT &Value);
	
protected:
	virtual System::Rtti::TValue __fastcall GetPropertyValue(const TUIProperty Prop) = 0 ;
	
public:
	__fastcall virtual ~TUICollectionItem(void);
public:
	/* TObject.Create */ inline __fastcall TUICollectionItem(void) : System::TInterfacedObject() { }
	
private:
	void *__IUISimplePropertySet;	// Uiribbonapi::IUISimplePropertySet 
	void *__IUICollectionItem;	// IUICollectionItem 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {C205BB48-5B1C-4219-A106-15BD0A5F24E2}
	operator Uiribbonapi::_di_IUISimplePropertySet()
	{
		Uiribbonapi::_di_IUISimplePropertySet intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Uiribbonapi::IUISimplePropertySet*(void) { return (Uiribbonapi::IUISimplePropertySet*)&__IUISimplePropertySet; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {C50153F6-8951-4316-BBFF-3B639E6DA20A}
	operator _di_IUICollectionItem()
	{
		_di_IUICollectionItem intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IUICollectionItem*(void) { return (IUICollectionItem*)&__IUICollectionItem; }
	#endif
	
};


typedef void __fastcall (__closure *TUICollectionChangeEvent)(TUICollection* const Collection, const bool Immediate);

class PASCALIMPLEMENTATION TUICollection : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	class DELPHICLASS TUICollectionImpl;
	class PASCALIMPLEMENTATION TUICollectionImpl : public System::TInterfacedObject
	{
		typedef System::TInterfacedObject inherited;
		
	private:
		System::Generics::Collections::TList__1<System::_di_IInterface>* FItems;
		bool FOwnsItems;
		int FEnumIndex;
		
	private:
		HRESULT __stdcall Next(int celt, /* out */ void *elt, System::PLongInt pceltFetched);
		HRESULT __stdcall Skip(int celt);
		HRESULT __stdcall Reset(void);
		HRESULT __stdcall Clone(/* out */ _di_IEnumUnknown &enm);
		HRESULT __safecall GetCount(unsigned &__GetCount_result);
		HRESULT __safecall GetItem(unsigned Index, System::_di_IInterface &__GetItem_result);
		HRESULT __safecall Add(const System::_di_IInterface Item);
		HRESULT __safecall Insert(unsigned Index, const System::_di_IInterface Item);
		HRESULT __safecall RemoveAt(unsigned Index);
		HRESULT __safecall Replace(unsigned IndexReplaces, const System::_di_IInterface ItemReplaceWith);
		HRESULT __safecall Clear(void);
		
	public:
		__fastcall TUICollectionImpl(void)/* overload */;
		__fastcall TUICollectionImpl(System::Generics::Collections::TList__1<System::_di_IInterface>* const Items, const int EnumIndex)/* overload */;
		__fastcall virtual ~TUICollectionImpl(void);
private:
		void *__IUICollection;	// Uiribbonapi::IUICollection 
		void *__IEnumUnknown;	// IEnumUnknown 
		
public:
		#if defined(MANAGED_INTERFACE_OPERATORS)
		// {DF4F45BF-6F9D-4DD7-9D68-D8F9CD18C4DB}
		operator Uiribbonapi::_di_IUICollection()
		{
			Uiribbonapi::_di_IUICollection intf;
			this->GetInterface(intf);
			return intf;
		}
		#else
		operator Uiribbonapi::IUICollection*(void) { return (Uiribbonapi::IUICollection*)&__IUICollection; }
		#endif
		#if defined(MANAGED_INTERFACE_OPERATORS)
		// {00000100-0000-0000-C000-000000000046}
		operator _di_IEnumUnknown()
		{
			_di_IEnumUnknown intf;
			this->GetInterface(intf);
			return intf;
		}
		#else
		operator IEnumUnknown*(void) { return (IEnumUnknown*)&__IEnumUnknown; }
		#endif
		
	};
	
	
	class DELPHICLASS TEnumerator;
	class PASCALIMPLEMENTATION TEnumerator : public System::TObject
	{
		typedef System::TObject inherited;
		
	private:
		TUICollection* FCollection;
		int FIndex;
		_di_IUICollectionItem __fastcall GetCurrent(void);
		
	public:
		__fastcall TEnumerator(TUICollection* const Collection);
		__property _di_IUICollectionItem Current = {read=GetCurrent};
		bool __fastcall MoveNext(void);
	public:
		/* TObject.Destroy */ inline __fastcall virtual ~TEnumerator(void) { }
		
	};
	
	
	
public:
	_di_IUICollectionItem operator[](const int Index) { return Items[Index]; }
	
private:
	Uiribbonapi::_di_IUICollection FHandle;
	int FUpdateCount;
	TUICollectionChangeEvent FOnChange;
	int __fastcall GetCount(void);
	_di_IUICollectionItem __fastcall GetItem(const int Index);
	void __fastcall Changed(const bool Immediate = false);
	
private:
	__property TUICollectionChangeEvent OnChange = {read=FOnChange, write=FOnChange};
	
public:
	__fastcall TUICollection(void)/* overload */;
	__fastcall TUICollection(const Uiribbonapi::_di_IUICollection Handle)/* overload */;
	void __fastcall BeginUpdate(void);
	void __fastcall EndUpdate(void);
	void __fastcall Add(const _di_IUICollectionItem Item);
	void __fastcall Insert(const int Index, const _di_IUICollectionItem Item);
	void __fastcall Replace(const int Index, const _di_IUICollectionItem Item);
	void __fastcall Delete(const int Index);
	void __fastcall Clear(void);
	TEnumerator* __fastcall GetEnumerator(void);
	__property int Count = {read=GetCount, nodefault};
	__property _di_IUICollectionItem Items[const int Index] = {read=GetItem/*, default*/};
	__property Uiribbonapi::_di_IUICollection Handle = {read=FHandle};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TUICollection(void) { }
	
};


class PASCALIMPLEMENTATION TUIGalleryCollectionItem : public TUICollectionItem
{
	typedef TUICollectionItem inherited;
	
private:
	System::UnicodeString FLabel;
	int FCategoryId;
	Uiribbonapi::_di_IUIImage FImage;
	TUICommand* FCommand;
	
protected:
	virtual System::Rtti::TValue __fastcall GetPropertyValue(const TUIProperty Prop);
	
public:
	__fastcall TUIGalleryCollectionItem(void);
	__property System::UnicodeString LabelText = {read=FLabel, write=FLabel};
	__property int CategoryId = {read=FCategoryId, write=FCategoryId, nodefault};
	__property Uiribbonapi::_di_IUIImage Image = {read=FImage, write=FImage};
	__property TUICommand* Command = {read=FCommand, write=FCommand};
public:
	/* TUICollectionItem.Destroy */ inline __fastcall virtual ~TUIGalleryCollectionItem(void) { }
	
};


class PASCALIMPLEMENTATION TUIRecentItem : public TUICollectionItem
{
	typedef TUICollectionItem inherited;
	
private:
	System::UnicodeString FLabel;
	System::UnicodeString FDescription;
	bool FPinned;
	
protected:
	virtual System::Rtti::TValue __fastcall GetPropertyValue(const TUIProperty Prop);
	
public:
	__property System::UnicodeString LabelText = {read=FLabel, write=FLabel};
	__property System::UnicodeString Description = {read=FDescription, write=FDescription};
	__property bool Pinned = {read=FPinned, write=FPinned, nodefault};
public:
	/* TUICollectionItem.Destroy */ inline __fastcall virtual ~TUIRecentItem(void) { }
	
public:
	/* TObject.Create */ inline __fastcall TUIRecentItem(void) : TUICollectionItem() { }
	
};


class PASCALIMPLEMENTATION TUIImage : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	static Uiribbonapi::_di_IUIImageFromBitmap FImageFactory;
	Uiribbonapi::_di_IUIImage FHandle;
	HBITMAP FBitmap;
	int FWidth;
	int FHeight;
	int FBitsPerPixel;
	System::Classes::TNotifyEvent FOnChanged;
	void __fastcall GetBitmapProperties(void);
	void __fastcall ConvertToAlphaBitmap(Vcl::Graphics::TBitmap* const Bitmap);
	void __fastcall Load(const NativeUInt Instance, const System::WideChar * ResourceName)/* overload */;
	void __fastcall LoadBmp(const System::UnicodeString Filename, const bool HighContrast);
	void __fastcall LoadPng(const System::UnicodeString Filename, const bool HighContrast);
	void __fastcall DoChanged(void);
	HBITMAP __fastcall CreatePreMultipliedBitmap(const HBITMAP Bitmap);
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
	HRESULT __safecall GetBitmap(HBITMAP &__GetBitmap_result);
	
public:
	__fastcall TUIImage(const int ResourceId)/* overload */;
	__fastcall TUIImage(const NativeUInt Instance, const int ResourceId)/* overload */;
	__fastcall TUIImage(const NativeUInt Instance, const System::UnicodeString ResourceName)/* overload */;
	__fastcall TUIImage(const Uiribbonapi::_di_IUIImage Handle)/* overload */;
	__fastcall TUIImage(const System::UnicodeString Filename, const bool HighContrast)/* overload */;
	void __fastcall Load(const int ResourceId)/* overload */;
	void __fastcall Load(const NativeUInt Instance, const int ResourceId)/* overload */;
	void __fastcall Load(const NativeUInt Instance, const System::UnicodeString ResourceName)/* overload */;
	void __fastcall Load(const System::UnicodeString Filename, const bool HighContrast = false)/* overload */;
	void __fastcall Draw(Vcl::Graphics::TCanvas* const Target, const int XTarget, const int YTarget)/* overload */;
	void __fastcall Draw(Vcl::Graphics::TCanvas* const Target, const int XTarget, const int YTarget, const int WTarget, const int HTarget)/* overload */;
	__property int Width = {read=FWidth, nodefault};
	__property int Height = {read=FHeight, nodefault};
	__property int BitsPerPixel = {read=FBitsPerPixel, nodefault};
	__property Uiribbonapi::_di_IUIImage Handle = {read=FHandle};
	#pragma option push -w-inl
	/* safecall wrapper */ inline HBITMAP _scw_GetBitmap() { HBITMAP __r; HRESULT __hr = GetBitmap(__r); System::CheckSafecallResult(__hr); return __r; }
	#pragma option pop
	__property HBITMAP Bitmap = {read=_scw_GetBitmap};
	
private:
	// __classmethod void __fastcall Create@();
	// __classmethod void __fastcall Destroy@();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TUIImage(void) { }
	
private:
	void *__IUIImage;	// Uiribbonapi::IUIImage 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {23C8C838-4DE6-436B-AB01-5554BB7C30DD}
	operator Uiribbonapi::_di_IUIImage()
	{
		Uiribbonapi::_di_IUIImage intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Uiribbonapi::IUIImage*(void) { return (Uiribbonapi::IUIImage*)&__IUIImage; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word WM_RIBBONCMD_NOTIFY = System::Word(0x4f1);
extern DELPHI_PACKAGE System::StaticArray<TUICommandClass, 13> UI_COMMAND_CLASSES;
extern DELPHI_PACKAGE Uiribbonapi::PUIPropertyKey __fastcall GetPropertyKey(const TUIProperty Prop);
extern DELPHI_PACKAGE TUIProperty __fastcall GetProperty(const Uiribbonapi::TUIPropertyKey &Key);
}	/* namespace Uiribboncommands */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UIRIBBONCOMMANDS)
using namespace Uiribboncommands;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UiribboncommandsHPP
