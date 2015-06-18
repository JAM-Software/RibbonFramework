# Windows Ribbon Framework for Delphi #

This Delphi library allows Delphi developers to use of the Windows Ribbon Framework in their Delphi applications. This library uses the native Windows library to implement the Ribbon functionality. It does not emulate the Ribbon user interface like other Delphi component sets do (or Delphi's built-in Ribbon emulation components).

The [Windows Ribbon Framework](http://msdn.microsoft.com/en-us/library/dd371191(v=VS.85).aspx) is the new User Interface library introduced with Windows 7, but is also available for Vista users through the [Vista Platform Update](http://support.microsoft.com/kb/971644) (which is installed through regular Windows updates and so present on most Vista and Server 2008 machines). This library allows you to create ribbon applications similar to Office 2010. In fact, the WordPad and Paint accessories that ships with Windows 7 and later use the Ribbon framework, also the [Microsoft Movie Maker](http://windows.microsoft.com/en-us/windows-live/movie-maker) shipped with the Live Essentials Addon.

![https://delphi-ribbon-framework.googlecode.com/svn/wiki/ribbon.gif](https://delphi-ribbon-framework.googlecode.com/svn/wiki/ribbon.gif)

This had the advantage that your ribbon will always look and behave exactly as Microsoft intended it. This also makes it easier to take advantage of improvements that Microsoft will make to the Ribbon Framework in the future. A further advantage is that you can use this library in open source projects since it does not depend on commercial third party libraries.

There are also some disadvantages though. The most notable one is that applications using the Windows Ribbon Framework will only display a ribbon when running on Windows 7 or later, or on Windows Vista with the Platform Update. If your application needs to support older Windows versions too, then you will also need to provide an additional "traditional" user interface, for example consisting of menus and toolbars.

Also, the method for creating Ribbon User Interfaces may be unfamiliar to Delphi developers: a Ribbon User Interface is completely defined in XML (not unlike XAML). There is no way to create ribbon controls at run-time, which also means that there is no way to design a ribbon visually in Delphi. A special designer appplication however ships with these components, it will create the XML for you.

## Features ##

This Delphi library is much more than a simple header translation. It has the following features:

  * Complete translation of the UI Ribbon header files. You can use these for low-level access to the Ribbon API. Although you probably won't need this because the library comes with a class library that is much easier to use.
  * A class library that provides higher-level access to the Ribbon API. This makes working with the Ribbon API much easier and takes care of most of the nasty implementation details.
  * Delphi-versions of the UI Ribbon Samples from the Windows SDK. There are low-level versions of these samples that use the Ribbon API directly, as well as medium-level versions that use the Delphi Ribbon class library. You will see that it takes much less code when you use the class library.
  * A feature-complete semi-visual Ribbon Designer. This application lets you create the Ribbon XML files without needing to write XML. You can completely design and test the ribbon in this application. It will also compile the Ribbon to a resource file and create Delphi source files with the constants you need to access the ribbon commands.
  * The Ribbon Designer comes with a WordPad template that lets you quickly create a Ribbon that looks virtually identical to the WordPad accessory that comes with Windows 7.

