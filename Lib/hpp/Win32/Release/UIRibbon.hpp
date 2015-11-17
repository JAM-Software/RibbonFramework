// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UIRibbon.pas' rev: 30.00 (Windows)

#ifndef UiribbonHPP
#define UiribbonHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Messages.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Generics.Collections.hpp>
#include <Vcl.Controls.hpp>
#include <System.Classes.hpp>
#include <Vcl.ActnList.hpp>
#include <UIRibbonApi.hpp>
#include <UIRibbonCommands.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>
#include <System.SysUtils.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Uiribbon
{
//-- forward type declarations -----------------------------------------------
struct TRibbonMarkupElement;
class DELPHICLASS TRibbonMarkupElementList;
class DELPHICLASS TUIRibbon;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TRibbonMarkupElement
{
public:
	System::UnicodeString Name;
	int ID;
	int LabelTitleResourceID;
	int LabelDescriptionResourceID;
	int TooltipTitleResourceID;
	int TooltipDescriptionResourceID;
	__fastcall TRibbonMarkupElement(System::UnicodeString pActionName, int pActionID, int pLabelTitleResourceID, int pLabelDescriptionResourceID, int pTooltipTitleResourceID, int pTooltipDescriptionResourceID);
	TRibbonMarkupElement() {}
};


class PASCALIMPLEMENTATION TRibbonMarkupElementList : public System::Generics::Collections::TList__1<TRibbonMarkupElement>
{
	typedef System::Generics::Collections::TList__1<TRibbonMarkupElement> inherited;
	
private:
	System::UnicodeString fResourceName;
	static System::Generics::Collections::TObjectList__1<TRibbonMarkupElementList*>* fContainer;
	// __classmethod void __fastcall Create@();
	// __classmethod void __fastcall Destroy@();
	
public:
	__classmethod TRibbonMarkupElementList* __fastcall LookupListByResourceName(const System::UnicodeString pResourceName);
	bool __fastcall TryGetItem(int pID, /* out */ TRibbonMarkupElement &pItem);
	__fastcall TRibbonMarkupElementList(System::UnicodeString pResourceName);
	__property System::UnicodeString ResourceName = {read=fResourceName, write=fResourceName};
public:
	/* {System_Generics_Collections}TList<UIRibbon_TRibbonMarkupElement>.Destroy */ inline __fastcall virtual ~TRibbonMarkupElementList(void) { }
	
};


typedef System::Int8 TRibbonApplicationMode;

typedef System::Set<TRibbonApplicationMode, 0, 31> TRibbonApplicationModes;

enum DECLSPEC_DENUM TUIQuickAccessToolbarPosition : unsigned char { qpTop, qpBottom };

typedef void __fastcall (__closure *TUIRibbomCommandEvent)(TUIRibbon* const Sender, Uiribboncommands::TUICommand* const Command);

typedef void __fastcall (__closure *TUILoadResourceStringEvent)(TUIRibbon* const Sender, Uiribboncommands::TUICommand* const pCommand, int pResourceID, System::UnicodeString &pString);

enum DECLSPEC_DENUM TUIRibbonOption : unsigned char { roAutoPreserveState };

typedef System::Set<TUIRibbonOption, TUIRibbonOption::roAutoPreserveState, TUIRibbonOption::roAutoPreserveState> TUIRibbonOptions;

class PASCALIMPLEMENTATION TUIRibbon : public Vcl::Controls::TWinControl
{
	typedef Vcl::Controls::TWinControl inherited;
	
	
private:
	typedef System::Generics::Collections::TEnumerator__1<Uiribboncommands::TUICommand*>* TCommandEnumerator;
	
	
public:
	Uiribboncommands::TUICommand* operator[](const unsigned CommandId) { return Commands[CommandId]; }
	
private:
	Uiribbonapi::_di_IUIFramework FFramework;
	Uiribbonapi::_di_IUIRibbon FRibbon;
	System::UnicodeString FResourceName;
	NativeUInt FResourceInstance;
	System::Generics::Collections::TObjectDictionary__2<unsigned,Uiribboncommands::TUICommand*>* FCommands;
	bool FAvailable;
	TUIRibbomCommandEvent FOnCommandCreate;
	System::Classes::TNotifyEvent FOnLoaded;
	bool FLoaded;
	System::UnicodeString fRibbonSettingsFilePath;
	TRibbonMarkupElementList* fRibbonMapper;
	TRibbonApplicationModes fApplicationModes;
	Vcl::Actnlist::TCustomActionList* fActionManager;
	Uiribboncommands::TUICommandRecentItems* fRecentItems;
	TUIRibbonOptions fOptions;
	void __fastcall Set_ApplicationModes(const TRibbonApplicationModes pAppModes);
	void __fastcall SetApplicationModes(const unsigned Modes)/* overload */;
	Uiribboncommands::TUICommand* __fastcall Get_Command(const unsigned CommandId);
	unsigned __fastcall GetBackgroundHsbColor(void);
	unsigned __fastcall GetHighlightHsbColor(void);
	unsigned __fastcall GetTextHsbColor(void);
	void __fastcall SetBackgroundHsbColor(const unsigned Value);
	void __fastcall SetHighlightHsbColor(const unsigned Value);
	void __fastcall SetTextHsbColor(const unsigned Value);
	bool __fastcall GetVisible(void);
	HIDESBASE void __fastcall SetVisible(const bool Value);
	bool __fastcall GetMinimized(void);
	void __fastcall SetMinimized(const bool Value);
	TUIQuickAccessToolbarPosition __fastcall GetQuickAccessToolbarPosition(void);
	void __fastcall SetQuickAccessToolbarPosition(const TUIQuickAccessToolbarPosition Value);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TMessage &Message);
	void __fastcall SetContextTabAvailability(const int pCommandId, const Uiribboncommands::TUIContextAvailability pContextAvailability);
	
private:
	HIDESBASE int __stdcall _AddRef(void);
	HIDESBASE int __stdcall _Release(void);
	TUILoadResourceStringEvent fOnLoadResourceString;
	HRESULT __stdcall OnViewChanged(unsigned ViewId, Uiribbonapi::_UIViewType TypeId, const System::_di_IInterface View, Uiribbonapi::_UIViewVerb Verb, int ReasonCode);
	HRESULT __stdcall OnCreateUICommand(unsigned CommandId, Uiribbonapi::_UICommandType TypeId, /* out */ Uiribbonapi::_di_IUICommandHandler &CommandHandler);
	HRESULT __stdcall OnDestroyUICommand(unsigned CommandId, Uiribbonapi::_UICommandType TypeId, const Uiribbonapi::_di_IUICommandHandler CommandHandler);
	System::Uitypes::TColor __fastcall GetBackgroundColor(void);
	System::Uitypes::TColor __fastcall GetHighlightColor(void);
	System::Uitypes::TColor __fastcall GetTextColor(void);
	void __fastcall SetBackgroundColor(const System::Uitypes::TColor Value);
	void __fastcall SetHighlightColor(const System::Uitypes::TColor Value);
	void __fastcall SetTextColor(const System::Uitypes::TColor Value);
	
protected:
	void __fastcall AddCommand(Uiribboncommands::TUICommand* const Command);
	unsigned __fastcall GetColor(const Uiribbonapi::TUIPropertyKey &PropKey);
	HIDESBASE void __fastcall SetColor(const Uiribbonapi::TUIPropertyKey &PropKey, const unsigned Value);
	System::UnicodeString __fastcall GetRibbonSettingsFilePath(void);
	bool __fastcall LoadRibbonSettings(void);
	void __fastcall SaveRibbonSettings(void);
	virtual System::UnicodeString __fastcall DoLoadResourceString(Uiribboncommands::TUICommand* const pCommand, int pResourceID);
	void __fastcall DoCommandCreated(Uiribboncommands::TUICommand* const pCommand);
	void __fastcall LocalizeRibbonElement(Uiribboncommands::TUICommand* const pCommand, const TRibbonMarkupElement &pMarkupItem);
	__property TRibbonMarkupElementList* RibbonMapper = {read=fRibbonMapper, write=fRibbonMapper};
	
public:
	__fastcall virtual TUIRibbon(System::Classes::TComponent* AOwner)/* overload */;
	__fastcall TUIRibbon(Vcl::Controls::TWinControl* const pParent, const TUIRibbomCommandEvent pOnCommandCreate)/* overload */;
	__fastcall virtual ~TUIRibbon(void);
	void __fastcall Load(void)/* overload */;
	void __fastcall InvalidateUICommand(Uiribboncommands::TUICommand* const Command, const Uiribboncommands::TUICommandInvalidations Aspects)/* overload */;
	Vcl::Actnlist::TCustomAction* __fastcall GetActionForCommand(Uiribboncommands::TUICommand* const pCommand);
	void __fastcall InvalidateUICommand(Uiribboncommands::TUICommand* const Command, const Uiribboncommands::TUIProperty Prop)/* overload */;
	void __fastcall ShowContextPopup(const unsigned PopupId, const System::Types::TPoint &ScreenPos)/* overload */;
	void __fastcall ShowContextPopup(const unsigned PopupId)/* overload */;
	void __fastcall SetApplicationModes(int const *Modes, const int Modes_High)/* overload */;
	bool __fastcall SaveSettings(const System::UnicodeString Filename)/* overload */;
	bool __fastcall SaveSettings(System::Classes::TStream* const Stream)/* overload */;
	bool __fastcall LoadSettings(const System::UnicodeString Filename)/* overload */;
	bool __fastcall LoadSettings(System::Classes::TStream* const Stream)/* overload */;
	HIDESBASE System::Generics::Collections::TEnumerator__1<Uiribboncommands::TUICommand*>* __fastcall GetEnumerator(void);
	bool __fastcall HandleShortCut(const System::Classes::TShortCut ShortCut)/* overload */;
	bool __fastcall HandleShortCut(const Winapi::Messages::TWMKey &pMessage)/* overload */;
	bool __fastcall TryGetCommand(const unsigned CommandId, /* out */ Uiribboncommands::TUICommand* &Command);
	virtual void __fastcall InitiateAction(void);
	void __fastcall ActivateContextTab(const int pCommandId);
	void __fastcall EnableContextTab(const int pCommandId);
	void __fastcall HideContextTab(const int pCommandId);
	void __fastcall HideAllContextTabs(unsigned pExceptCommandId = (unsigned)(0xffffffff));
	Uiribboncommands::TUICommand* __fastcall GetCommand(Vcl::Actnlist::TCustomAction* pAction);
	void __fastcall SetRecentItems(Vcl::Actnlist::TAction* pAction, System::Classes::TStrings* pPaths)/* overload */;
	Uiribboncommands::TUIRecentItem* __fastcall GetSelectedRecentItem(void);
	__property bool Available = {read=FAvailable, nodefault};
	__property bool Visible = {read=GetVisible, write=SetVisible, nodefault};
	__property bool Minimized = {read=GetMinimized, write=SetMinimized, nodefault};
	__property TUIQuickAccessToolbarPosition QuickAccessToolbarPosition = {read=GetQuickAccessToolbarPosition, write=SetQuickAccessToolbarPosition, nodefault};
	__property Uiribboncommands::TUICommand* Commands[const unsigned CommandId] = {read=Get_Command/*, default*/};
	__property unsigned BackgroundHsbColor = {read=GetBackgroundHsbColor, write=SetBackgroundHsbColor, nodefault};
	__property unsigned HighlightHsbColor = {read=GetHighlightHsbColor, write=SetHighlightHsbColor, nodefault};
	__property unsigned TextHsbColor = {read=GetTextHsbColor, write=SetTextHsbColor, nodefault};
	__property System::Uitypes::TColor BackgroundColor = {read=GetBackgroundColor, write=SetBackgroundColor, nodefault};
	__property System::Uitypes::TColor HighlightColor = {read=GetHighlightColor, write=SetHighlightColor, nodefault};
	__property System::Uitypes::TColor TextColor = {read=GetTextColor, write=SetTextColor, nodefault};
	__property Uiribbonapi::_di_IUIFramework Framework = {read=FFramework};
	__property TRibbonApplicationModes ApplicationModes = {read=fApplicationModes, write=Set_ApplicationModes, nodefault};
	
__published:
	__property System::UnicodeString ResourceName = {read=FResourceName, write=FResourceName};
	__property NativeUInt ResourceInstance = {read=FResourceInstance, write=FResourceInstance, stored=false, nodefault};
	__property System::UnicodeString RibbonSettingsFilePath = {read=fRibbonSettingsFilePath, write=fRibbonSettingsFilePath};
	__property Vcl::Actnlist::TCustomActionList* ActionManager = {read=fActionManager, write=fActionManager};
	__property TUIRibbonOptions Options = {read=fOptions, write=fOptions, default=1};
	__property TUIRibbomCommandEvent OnCommandCreate = {read=FOnCommandCreate, write=FOnCommandCreate};
	__property System::Classes::TNotifyEvent OnLoaded = {read=FOnLoaded, write=FOnLoaded};
	__property TUILoadResourceStringEvent OnLoadResourceString = {read=fOnLoadResourceString, write=fOnLoadResourceString};
public:
	/* TWinControl.CreateParented */ inline __fastcall TUIRibbon(HWND ParentWindow) : Vcl::Controls::TWinControl(ParentWindow) { }
	
private:
	void *__IUIApplication;	// Uiribbonapi::IUIApplication 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {D428903C-729A-491D-910D-682A08FF2522}
	operator Uiribbonapi::_di_IUIApplication()
	{
		Uiribbonapi::_di_IUIApplication intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Uiribbonapi::IUIApplication*(void) { return (Uiribbonapi::IUIApplication*)&__IUIApplication; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Uiribbon */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UIRIBBON)
using namespace Uiribbon;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UiribbonHPP
