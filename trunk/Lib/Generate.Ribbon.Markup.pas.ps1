﻿# Stop on errors
$ErrorActionPreference = "Stop"

$xmlFilePath = $args[0]

# Determine the current working directory from the given xml file path
$workingDir = ([System.IO.Path]::GetDirectoryName($xmlFilePath))
if ([string]::IsNullOrEmpty($workingDir))
{
    $workingDir = "."
}
$workingDir = $workingDir + ([System.IO.Path]::DirectorySeparatorChar)

# Prepare file paths for the files that we want to create
$pasFilePath = $workingDir + ([System.IO.Path]::GetFileNameWithoutExtension($xmlFilePath) + ".pas")
$bmlFilePath = $workingDir + ([System.IO.Path]::GetFileNameWithoutExtension($xmlFilePath) + ".bml")
$rcFilePath = $workingDir + ([System.IO.Path]::GetFileNameWithoutExtension($xmlFilePath) + ".rc")
$headerFilePath = $workingDir + ([System.IO.Path]::GetFileNameWithoutExtension($xmlFilePath) + ".h")
$resFileName = ([System.IO.Path]::GetFileNameWithoutExtension($xmlFilePath) + ".res")
$unitName = ([System.IO.Path]::GetFileNameWithoutExtension($xmlFilePath))

$ResourceName = $args[1]
$UICCDir = $args[2]

# Checks if a file exists under a given location. If yes, the path to this file is returned. If not, we lookup several known locations and return those, if the file is found.
function FindFileInLocation($pLocation, $pFileName)
{   
    # First check if a valid path was passed via the command line
    $lPath = $pLocation + "\" +$pFileName
    if (Test-Path $lPath)
    {
        return $lPath
    }
    # Check if the file exists under %PATH%
    if (Get-Command $pFileName -ErrorAction SilentlyContinue)
    {
        return "$pFileName"
    }    
    # If not, check a few known locations for uicc.exe
    elseif (Test-Path "${env:ProgramFiles(x86)}\Microsoft SDKs\Windows\v7.1A\Bin\$pFileName")
    {
        return "${env:ProgramFiles(x86)}\Microsoft SDKs\Windows\v7.1A\Bin\$pFileName"
    }
    elseif (Test-Path "${env:ProgramFiles(x86)}\Windows Kits\8.0\bin\x86\$pFileName")
    {
        return "${env:ProgramFiles(x86)}\Windows Kits\8.0\bin\x86\$pFileName"
    }
    elseif (Test-Path "${env:ProgramFiles(x86)}\Windows Kits\8.1\bin\$pFileName")
    {
        return "${env:ProgramFiles(x86)}\Windows Kits\8.1\bin\x86\$pFileName"
    }    
    else
    {
        # Nothing found -> exit
        write "Cannot find $pFileName. Aborting execution."
        exit
    }
}

# Find UICC.exe
$UICCCmd = FindFileInLocation -pLocation $UICCDir -pFileName "UICC.exe"
write-host "UICC.exe found: Using $UICCCmd"

# Use the provided xml file to Create the .bml, .h and .rc file
& $UICCCmd "/W0" "$xmlFilePath" "$bmlFilePath" "/header:$headerFilePath" "/res:$rcFilePath" "/name:$ResourceName"

# Find rc.exe (Use the same locations as UICC.exe)
$RCCmd = FindFileInLocation -pLocation $UICCDir -pFileName "rc.exe"
write-host "RC.exe found: Using $RCCmd"

# Create the .RES resource file
rc "$rcFilePath"

# Create a new Markup .pas file that will contain the Ribbon command constants.

[System.Collections.ArrayList]$markupContent = New-Object([System.Collections.ArrayList])

$FileTopPart = @"
unit $unitName;

// *****************************************************************************
// * This is an automatically generated source file for UI Element definition  *
// * resource symbols and values. Please do not modify manually.               *
// *****************************************************************************

interface

{`$R '$resFileName'}

uses
	Generics.Collections, SysUtils, UIRibbon;

const
"@

write-host "Setting content to " + $pasFilePath
Set-Content -Path "$pasFilePath" -Value $FileTopPart

# Get content of the header file (e.g. TreeSize.Ribbon.Markup.h).
$data = Get-Content "$headerFilePath"

foreach ($line in $data)
{
	if ($line.Contains("#define"))
	{
		$line = $line.TrimStart("#define")
		$commandId = ([regex]"\b\d{1,5}\b").match($line).groups[0].value
		$commandName = ([regex]"\b\w+\b").match($line).groups[0].value
		$appendLine = "  $commandName = $commandId;"
		Add-Content "$pasFilePath" "$appendLine"
        $dummy = $markupContent.Add($appendLine);
	}
}

# Add some additional predefined text.
$FileMiddlePart = @"

implementation

function RegisterRibbonElements(): TRibbonMarkupElementList;
begin
  Result := TRibbonMarkupElementList.Create('$ResourceName');
"@

Add-Content "$pasFilePath" $FileMiddlePart
    

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
			Add-Content "$pasFilePath" "$appendLine"
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
	Add-Content "$pasFilePath" "$appendLine"	
}

# Add the ending part
$FileEndPart = @"
end;
initialization

  RegisterRibbonElements();
  
end.
"@

Add-Content "$pasFilePath" $FileEndPart 
write-host "Ribbon pascal markup file generation successful: '$pasFilePath'"