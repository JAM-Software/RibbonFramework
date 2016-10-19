// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UIRibbonActions.pas' rev: 31.00 (Windows)

#ifndef UiribbonactionsHPP
#define UiribbonactionsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.ActnList.hpp>
#include <Vcl.ActnMan.hpp>
#include <UIRibbonCommands.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Actions.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Uiribbonactions
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUICommandActionLink;
class DELPHICLASS TUICommandEmptyActionLink;
class DELPHICLASS TUICommandActionActionLink;
class DELPHICLASS TUICommandCollectionActionLink;
class DELPHICLASS TUICommandDecimalActionLink;
class DELPHICLASS TUICommandBooleanActionLink;
class DELPHICLASS TUICommandFontActionLink;
class DELPHICLASS TUICommandColorAnchorActionLink;
class DELPHICLASS TUICommandRecentItemsActionLink;
template<typename T> class DELPHICLASS TRibbonAction__1;
class DELPHICLASS TRibbonCollectionAction;
class DELPHICLASS TRibbonPopupMenuAction;
class DELPHICLASS TRibbonColorAction;
class DELPHICLASS TRibbonFontAction;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUICommandActionLink : public Vcl::Actnlist::TActionLink
{
	typedef Vcl::Actnlist::TActionLink inherited;
	
private:
	Uiribboncommands::TUICommand* FClient;
	
protected:
	virtual void __fastcall SetAction(System::Classes::TBasicAction* Value);
	virtual void __fastcall AssignClient(System::TObject* AClient);
	virtual bool __fastcall IsEnabledLinked(void);
	virtual bool __fastcall IsImageIndexLinked(void);
	virtual bool __fastcall IsOnExecuteLinked(void);
	virtual void __fastcall SetCaption(const System::UnicodeString Value);
	virtual void __fastcall SetEnabled(bool Value);
	virtual void __fastcall SetVisible(bool Value);
	virtual void __fastcall SetHint(const System::UnicodeString Value);
	virtual void __fastcall SetShortCut(System::Classes::TShortCut Value);
	virtual void __fastcall SetImageIndex(int Value);
	virtual void __fastcall SetOnExecute(System::Classes::TNotifyEvent Value);
	__property Uiribboncommands::TUICommand* Client = {read=FClient};
	
public:
	__fastcall TUICommandActionLink(Uiribboncommands::TUICommand* const AClient);
	virtual bool __fastcall Update(void);
public:
	/* TBasicActionLink.Destroy */ inline __fastcall virtual ~TUICommandActionLink(void) { }
	
};


class PASCALIMPLEMENTATION TUICommandEmptyActionLink : public TUICommandActionLink
{
	typedef TUICommandActionLink inherited;
	
public:
	/* TUICommandActionLink.Create */ inline __fastcall TUICommandEmptyActionLink(Uiribboncommands::TUICommand* const AClient) : TUICommandActionLink(AClient) { }
	
public:
	/* TBasicActionLink.Destroy */ inline __fastcall virtual ~TUICommandEmptyActionLink(void) { }
	
};


class PASCALIMPLEMENTATION TUICommandActionActionLink : public TUICommandActionLink
{
	typedef TUICommandActionLink inherited;
	
private:
	void __fastcall CommandExecute(const Uiribboncommands::TUICommandActionEventArgs &Args);
	
protected:
	virtual void __fastcall SetAction(System::Classes::TBasicAction* Value);
public:
	/* TUICommandActionLink.Create */ inline __fastcall TUICommandActionActionLink(Uiribboncommands::TUICommand* const AClient) : TUICommandActionLink(AClient) { }
	
public:
	/* TBasicActionLink.Destroy */ inline __fastcall virtual ~TUICommandActionActionLink(void) { }
	
};


class PASCALIMPLEMENTATION TUICommandCollectionActionLink : public TUICommandActionLink
{
	typedef TUICommandActionLink inherited;
	
private:
	void __fastcall CommandSelect(const Uiribboncommands::TUICommandCollectionEventArgs &Args);
	
protected:
	virtual void __fastcall SetAction(System::Classes::TBasicAction* Value);
public:
	/* TUICommandActionLink.Create */ inline __fastcall TUICommandCollectionActionLink(Uiribboncommands::TUICommand* const AClient) : TUICommandActionLink(AClient) { }
	
public:
	/* TBasicActionLink.Destroy */ inline __fastcall virtual ~TUICommandCollectionActionLink(void) { }
	
};


class PASCALIMPLEMENTATION TUICommandDecimalActionLink : public TUICommandActionLink
{
	typedef TUICommandActionLink inherited;
	
private:
	void __fastcall CommandChange(Uiribboncommands::TUICommandDecimal* const Command, const Uiribboncommands::TUICommandVerb Verb, const double Value, Uiribboncommands::TUICommandExecutionProperties* const Properties);
	
protected:
	virtual void __fastcall SetAction(System::Classes::TBasicAction* Value);
public:
	/* TUICommandActionLink.Create */ inline __fastcall TUICommandDecimalActionLink(Uiribboncommands::TUICommand* const AClient) : TUICommandActionLink(AClient) { }
	
public:
	/* TBasicActionLink.Destroy */ inline __fastcall virtual ~TUICommandDecimalActionLink(void) { }
	
};


class PASCALIMPLEMENTATION TUICommandBooleanActionLink : public TUICommandActionLink
{
	typedef TUICommandActionLink inherited;
	
private:
	void __fastcall CommandToggle(const Uiribboncommands::TUICommandBooleanEventArgs &Args);
	
protected:
	virtual void __fastcall SetAction(System::Classes::TBasicAction* Value);
	virtual void __fastcall SetChecked(bool Value);
public:
	/* TUICommandActionLink.Create */ inline __fastcall TUICommandBooleanActionLink(Uiribboncommands::TUICommand* const AClient) : TUICommandActionLink(AClient) { }
	
public:
	/* TBasicActionLink.Destroy */ inline __fastcall virtual ~TUICommandBooleanActionLink(void) { }
	
};


class PASCALIMPLEMENTATION TUICommandFontActionLink : public TUICommandActionLink
{
	typedef TUICommandActionLink inherited;
	
private:
	void __fastcall CommandChanged(const Uiribboncommands::TUICommandFontEventArgs &Args);
	
protected:
	virtual void __fastcall SetAction(System::Classes::TBasicAction* Value);
public:
	/* TUICommandActionLink.Create */ inline __fastcall TUICommandFontActionLink(Uiribboncommands::TUICommand* const AClient) : TUICommandActionLink(AClient) { }
	
public:
	/* TBasicActionLink.Destroy */ inline __fastcall virtual ~TUICommandFontActionLink(void) { }
	
};


class PASCALIMPLEMENTATION TUICommandColorAnchorActionLink : public TUICommandActionLink
{
	typedef TUICommandActionLink inherited;
	
private:
	void __fastcall CommandExecute(const Uiribboncommands::TUICommandColorEventArgs &Args);
	
protected:
	virtual void __fastcall SetAction(System::Classes::TBasicAction* Value);
public:
	/* TUICommandActionLink.Create */ inline __fastcall TUICommandColorAnchorActionLink(Uiribboncommands::TUICommand* const AClient) : TUICommandActionLink(AClient) { }
	
public:
	/* TBasicActionLink.Destroy */ inline __fastcall virtual ~TUICommandColorAnchorActionLink(void) { }
	
};


class PASCALIMPLEMENTATION TUICommandRecentItemsActionLink : public TUICommandActionLink
{
	typedef TUICommandActionLink inherited;
	
private:
	Uiribboncommands::TUIRecentItem* fSelected;
	void __fastcall CommandSelect(Uiribboncommands::TUICommandRecentItems* const Command, const Uiribboncommands::TUICommandVerb Verb, const int ItemIndex, Uiribboncommands::TUICommandExecutionProperties* const Properties);
	
protected:
	virtual void __fastcall SetAction(System::Classes::TBasicAction* Value);
	
public:
	__property Uiribboncommands::TUIRecentItem* Selected = {read=fSelected, write=fSelected};
public:
	/* TUICommandActionLink.Create */ inline __fastcall TUICommandRecentItemsActionLink(Uiribboncommands::TUICommand* const AClient) : TUICommandActionLink(AClient) { }
	
public:
	/* TBasicActionLink.Destroy */ inline __fastcall virtual ~TUICommandRecentItemsActionLink(void) { }
	
};


// Template declaration generated by Delphi parameterized types is
// used only for accessing Delphi variables and fields.
// Don't instantiate with new type parameters in user code.
template<typename T> class PASCALIMPLEMENTATION TRibbonAction__1 : public Vcl::Actnlist::TCustomAction
{
	typedef Vcl::Actnlist::TCustomAction inherited;
	
private:
	T fUICommand;
	
public:
	__property T UICommand = {read=fUICommand, write=fUICommand};
	
__published:
	__property Caption = {default=0};
	__property Enabled = {default=1};
	__property HelpContext = {default=0};
	__property HelpKeyword = {default=0};
	__property HelpType = {default=0};
	__property Hint = {default=0};
	__property SecondaryShortCuts;
	__property ShortCut = {default=0};
	__property OnExecute;
	__property OnHint;
	__property OnUpdate;
public:
	/* TCustomAction.Create */ inline __fastcall virtual TRibbonAction__1(System::Classes::TComponent* AOwner) : Vcl::Actnlist::TCustomAction(AOwner) { }
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TRibbonAction__1(void) { }
	
};


class PASCALIMPLEMENTATION TRibbonCollectionAction : public TRibbonAction__1<Uiribboncommands::TUICommandCollection*>
{
	typedef TRibbonAction__1<Uiribboncommands::TUICommandCollection*> inherited;
	
public:
	System::Generics::Collections::TPair__2<Vcl::Actnlist::TCustomAction*,System::UnicodeString> operator[](int Index) { return this->Items[Index]; }
	
private:
	System::Generics::Collections::TList__1<System::Generics::Collections::TPair__2<Vcl::Actnlist::TCustomAction*,System::UnicodeString> >* fActionList;
	System::Generics::Collections::TPair__2<Vcl::Actnlist::TCustomAction*,System::UnicodeString> __fastcall GetItem(int pIndex);
	
public:
	__fastcall virtual TRibbonCollectionAction(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TRibbonCollectionAction(void);
	void __fastcall Add(Vcl::Actnlist::TCustomAction* pAction, const System::UnicodeString pCategory = System::UnicodeString());
	void __fastcall Clear(void);
	void __fastcall AddRange(System::Generics::Collections::TList__1<System::Generics::Collections::TPair__2<Vcl::Actnlist::TCustomAction*,System::UnicodeString> >* pSource);
	int __fastcall ItemCount(void);
	__property System::Generics::Collections::TPair__2<Vcl::Actnlist::TCustomAction*,System::UnicodeString> Items[int Index] = {read=GetItem/*, default*/};
	void __fastcall RefreshCommandCollection(void);
};


class PASCALIMPLEMENTATION TRibbonPopupMenuAction : public TRibbonCollectionAction
{
	typedef TRibbonCollectionAction inherited;
	
private:
	Vcl::Menus::TPopupMenu* fPopupMenu;
	Vcl::Menus::TMenuChangeEvent fOriginalOnMenuChange;
	
protected:
	void __fastcall SetPopupMenu(Vcl::Menus::TPopupMenu* pValue);
	void __fastcall MenuChange(System::TObject* Sender, Vcl::Menus::TMenuItem* Source, bool Rebuild);
	
__published:
	__property Vcl::Menus::TPopupMenu* Menu = {read=fPopupMenu, write=SetPopupMenu};
public:
	/* TRibbonCollectionAction.Create */ inline __fastcall virtual TRibbonPopupMenuAction(System::Classes::TComponent* AOwner) : TRibbonCollectionAction(AOwner) { }
	/* TRibbonCollectionAction.Destroy */ inline __fastcall virtual ~TRibbonPopupMenuAction(void) { }
	
};


class PASCALIMPLEMENTATION TRibbonColorAction : public TRibbonAction__1<Uiribboncommands::TUICommandColorAnchor*>
{
	typedef TRibbonAction__1<Uiribboncommands::TUICommandColorAnchor*> inherited;
	
public:
	/* TCustomAction.Create */ inline __fastcall virtual TRibbonColorAction(System::Classes::TComponent* AOwner) : TRibbonAction__1<Uiribboncommands::TUICommandColorAnchor*>(AOwner) { }
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TRibbonColorAction(void) { }
	
};


class PASCALIMPLEMENTATION TRibbonFontAction : public TRibbonAction__1<Uiribboncommands::TUICommandFont*>
{
	typedef TRibbonAction__1<Uiribboncommands::TUICommandFont*> inherited;
	
private:
	Uiribboncommands::TUICommandFontChangedEvent fOnChanged;
	
__published:
	__property Uiribboncommands::TUICommandFontChangedEvent OnChanged = {read=fOnChanged, write=fOnChanged};
public:
	/* TCustomAction.Create */ inline __fastcall virtual TRibbonFontAction(System::Classes::TComponent* AOwner) : TRibbonAction__1<Uiribboncommands::TUICommandFont*>(AOwner) { }
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TRibbonFontAction(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Uiribbonactions */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UIRIBBONACTIONS)
using namespace Uiribbonactions;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UiribbonactionsHPP
