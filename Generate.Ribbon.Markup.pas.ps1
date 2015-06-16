# This PowerShell script converts an XML of the Windows Ribbon Framework 
# into a binary RES file that needs to be linked into the final EXE file.
# Authors: Daniel Lemke, Sascha Schaefer at JAM Software, Germany


# Stop on errors
$ErrorActionPreference = "Stop"

$appDir = get-location
$prefix =  $args[0]
$workingDir = $args[1]
$RessourceName = $args[2]
$UICCDir = $args[3]


function FindUICCExe($pUICCDir)
{   
    # First check if a valid path was passed via the command line
    $lUICCmd = $pUICCDir + "\UICC.exe"
    if (Test-Path $lUICCmd)
    {
        return $lUICCmd
    }
    # If not, check a few known locations for uicc.exe
    elseif (Test-Path "${env:ProgramFiles(x86)}\Microsoft SDKs\Windows\v7.1A\Bin\uicc.exe")
    {
        return "${env:ProgramFiles(x86)}\Microsoft SDKs\Windows\v7.1A\Bin\uicc.exe"
    }
    elseif (Test-Path "${env:ProgramFiles(x86)}\Windows Kits\8.0\bin\x86\uicc.exe")
    {
        return "${env:ProgramFiles(x86)}\Windows Kits\8.0\bin\x86\uicc.exe"
    }
    elseif (Test-Path "${env:ProgramFiles(x86)}\Windows Kits\8.1\bin\x86\uicc.exe")
    {
        return "${env:ProgramFiles(x86)}\Windows Kits\8.1\bin\x86\uicc.exe"
    }
    # Check %PATH%
    elseif (Test-Path "UICC.exe")
    {
        return "UICC.exe"
    }
    else
    {
        # Nothing found -> exit
        write "Cannot find UICC.exe."
        exit
    }
}

$UICCmd = FindUICCExe("$UICCDir")
write-host "UICC.exe found: Using $UICCmd"


# Create the .bml, .h and .rc file
& $UICCmd "/W0" "$workingDir\$prefix.Ribbon.Markup.xml" "$workingDir\$prefix.Ribbon.Markup.bml" "/header:$workingDir\$prefix.Ribbon.Markup.h" "/res:$workingDir\$prefix.Ribbon.Markup.rc" "/name:$RessourceName"

# Create the .RES resource file
$rcName = $prefix + ".Ribbon.Markup.rc"
rc "$workingDir\$rcName"

# Create a new Markup .pas file that will contain the Ribbon command constants.

$markupFileName = "$prefix.Ribbon.Markup.pas"
[System.Collections.ArrayList]$markupContent = New-Object([System.Collections.ArrayList])

$FileTopPart = @"
unit $prefix.Ribbon.Markup;

// *****************************************************************************
// * This is an automatically generated source file for UI Element definition  *
// * resource symbols and values. Please do not modify manually.               *
// *****************************************************************************

interface

{`$R '$prefix.Ribbon.Markup.res'}

uses
	Generics.Collections, SysUtils, UIRibbon;

const
"@
Set-Content -Path "$workingDir\$markupFileName" -Value $FileTopPart

# Get content of the header file (e.g. TreeSize.Ribbon.Markup.h).
$headerFileName = "$prefix.Ribbon.Markup.h"
$data = Get-Content "$workingDir\$headerFileName"

foreach ($line in $data)
{
	if ($line.Contains("#define"))
	{
		$line = $line.TrimStart("#define")
		$commandId = ([regex]"\b\d{1,5}\b").match($line).groups[0].value
		$commandName = ([regex]"\b\w+\b").match($line).groups[0].value
		$appendLine = "  $commandName = $commandId;"
		Add-Content "$workingDir\$markupFileName" "$appendLine"
        $dummy = $markupContent.Add($appendLine);
	}
}

# Add some additional predefined text.
$FileMiddlePart = @"

function GetElements(): TRibbonMarkupElementList;

implementation

function GetElements(): TRibbonMarkupElementList;
begin
  Result := TRibbonMarkupElementList.Create();
"@

Add-Content "$workingDir\$markupFileName" $FileMiddlePart
    

# Add the mapping by using the previously generated markup content

# Initialization
$commandName = ""
$LabelTitleResourceID = -1
$LabelDescriptionResourceID = -1
$TooltipTitleResourceID = -1
$TooltipDescriptionResourceID = -1

#RegEx for resource IDs
$resourceIdRegexPattern = "\b\d{1,5}\b"

foreach ($line in $markupContent)
{
	if (!($line.Contains("RESID")))
	{       
		if (($commandName) -and ($commandID))
		{
			$appendLine = "  Result.Add(TRibbonMarkupElement.Create('$commandName', $commandId, $LabelTitleResourceID, $LabelDescriptionResourceID, $TooltipTitleResourceID, $TooltipDescriptionResourceID));"
			Add-Content "$workingDir\$markupFileName" "$appendLine"
			$LabelTitleResourceID = -1
            $LabelDescriptionResourceID = -1
            $TooltipTitleResourceID = -1
            $TooltipDescriptionResourceID = -1
		}
	
		$commandName = ([regex]"\b\w+\b").match($line).groups[0].value
		$commandId = ([regex]$resourceIdRegexPattern).match($line).groups[0].value
		continue
	}
	if ($commandName -and $line.Contains($commandName))
	{
		if ($line.Contains("LabelTitle")) 
		{
			$LabelTitleResourceID = ([regex]$resourceIdRegexPattern).match($line).groups[0].value
		}
		elseif ($line.Contains("LabelDescription")) 
		{
			$LabelDescriptionResourceID = ([regex]$resourceIdRegexPattern).match($line).groups[0].value
		}
		elseif ($line.Contains("TooltipTitle")) 
		{
			$TooltipTitleResourceID = ([regex]$resourceIdRegexPattern).match($line).groups[0].value
		}
		elseif ($line.Contains("TooltipDescription")) 
		{
			$TooltipDescriptionResourceID = ([regex]$resourceIdRegexPattern).match($line).groups[0].value
		}
	}
}

if (($commandName) -and ($commandID))
{
	$appendLine = "  Result.Add(TRibbonMarkupElement.Create('$commandName', $commandId, $LabelTitleResourceID, $TooltipTitleResourceID));"    
	Add-Content "$workingDir\$markupFileName" "$appendLine"	
}

# Add the ending part
$FileEndPart = @"
end;

end.
"@

Add-Content "$workingDir\$markupFileName" $FileEndPart 
write-host "Ribbon pascal markup file generation successful: '$markupFileName'"