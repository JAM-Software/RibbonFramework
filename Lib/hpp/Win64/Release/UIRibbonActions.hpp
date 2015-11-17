// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UIRibbonActions.pas' rev: 30.00 (Windows)

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
#include <Vcl.ActnList.hpp>
#include <UIRibbonCommands.hpp>
#include <System.Actions.hpp>

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
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUICommandActionLink : public Vcl::Actnlist::TActionLink
{
	typedef Vcl::Actnlist::TActionLink inherited;
	
private:
	Uiribboncommands::TUICommand* FClient;
	
protected:
	virtual void __fastcall AssignClient(System::TObject* AClient);
	virtual bool __fastcall IsEnabledLinked(void);
	virtual bool __fastcall IsOnExecuteLinked(void);
	virtual void __fastcall SetCaption(const System::UnicodeString Value);
	virtual void __fastcall SetEnabled(bool Value);
	virtual void __fastcall SetVisible(bool Value);
	virtual void __fastcall SetHint(const System::UnicodeString Value);
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
