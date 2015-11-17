//---------------------------------------------------------------------------

#include <System.hpp>
#pragma hdrstop
#pragma package(smart_init)
#ifdef _WIN64
#pragma comment(lib, "Shlwapi.a")
#else
#pragma comment(lib, "Shlwapi.lib")
#endif
//---------------------------------------------------------------------------

//   Package-Quelltext.
//---------------------------------------------------------------------------


#pragma argsused
extern "C" int _libmain(unsigned long reason)
{
	return 1;
}
//---------------------------------------------------------------------------
