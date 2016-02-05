Please see https://github.com/TurboPack/RibbonFramework/ for detailed information 
about this library.

Version history
~~~~~~~~~~~~~~~

28 Jan 2016: Version 2.1.2: 
* Fixed issue #24: Ribbon Designer freezes during build

24 Jan 2016: Version 2.1.1: 
* In the RAD Studio designer the Ribbon Desginer can now be started using the
  right click menu of the TUIRibbon control

09 Dec 2015: Version 2.1: 
* All sample projects are now adapted for V2 and use the TUIRibbon designtime component
* Improved performance of PowerShell script that compiles the ribbon XML files. 
* Added new standard VCL action for collection/list ribbon commands that provides 
  access to the corresponding TUICommandCollection instance.
* Added new standard VCL action TRibbonFontAction for mapping TUICommandColorAnchor 
  to an action. 
* Added TUICommand.OnUpdateImage event
* Added TUICommand.OnUpdateHint propery to allow custom hint processing
* New TUIRibbon.Options property: Introduced roAutoPreserveState option (on by default). 
  When set, SaveRibbonSettings and LoadRibbonSettings are automatically called.
* TUIRibbon.DoCommandCreated() no longer raises an exception if an action for a command could not be found.
* Improved TUICommand.Assign() to not overwrite existing values with empty strings from action.
* The Ribbon Desinger now has a button to generate an unused ID
* The Ribbon Desinger now has a button to download Windows SDK which includes the necessary UICC.EXE

18 Aug 2015: Version 2.0: 
* TUIRibbon is now a control that can be placed on a VCL form.
* TUIRibbonForm is deprecated and no longer needed. 
* Automatic mapping between VCL actions done if a TActionManager is 
  assigned to the TUIRibbon control and the name of the ribbon command 
  matches the name of the VCL action.
  
  
01/14/2015: The project was adopted by JAM Software which continues maintaining
            this project. A V2.0 is expected in summer 2015.            
12/17/2012: Version 1.2: Added Delphi XE3 compatibility (thanks to Cherryl
            Martin for pointing this out).
7/30/2011:  Version 1.1: Added support for linking UI Commands to Delphi
            actions. See the "Text Pad with Action List" sample application
            and the web site for information on how to do this.
            Also, applications using this framework will also run on
            earlier Windows version now, but without ribbon functionality.
            The TUIRibbon.Available property can be used to check if the
            Ribbon Framework is available on the system. If not, you could
            enable a more traditional user interface.
            These changes were inspired by contributions from
            Patrick Kolla-ten Venne. Many thanks for this! 
5/27/2011:  Version 1.0: Initial release by Erik van Bilsen.
            
