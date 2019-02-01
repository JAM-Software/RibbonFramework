unit MarkupGenerator;

interface

uses
  System.Classes, UIRibbon, System.Generics.Collections;

  type

    /// <summary>
    /// Useas an input ribbon markup to generate a .pas file that can be included in the target application.
    /// The pas file, if included, will connect the Delphi actions with their corresponding ribbon commands from the markup.
    /// </summary>
    TMarkupGenerator = class(TObject)
      strict private
        fXMLFilePath: string;
        fWorkingDir: string;
        fHeaderFilePath: string;
        fHeaderFileContent: TStrings;
        fPasFilePath: string;
        fResourceName: string;
        /// Generates the top section of the target pas file
        function GetTopSection: string;
        /// Generates the middle section of the target pas file
        function GetMiddleSection: string;
        /// Generates the end section of the target pas file
        function GetEndSection: string;
        /// Parses the markup header file and generates a list of TRibbonElements that correspond to the definition of the markup file.
        function ExtractMarkupElements: TList<TRibbonMarkupElement>;
        /// Uses the previously read header file to create the command ID declarations within the target pas file.
        function GenerateCommandConstants: string;
        /// Uses the given markup elements to generate the section of the pas file that connects ribbon commands with delphi actions. See also "function ExtractMarkupElements".
        function GenerateObjectDefinitions(pMarkupElements: TEnumerable<TRibbonMarkupElement>): string;
      public
        /// <summary> Creates a new instance of the markup generator. </summary>
        /// <param name="pXMLFilePath">The XML markup file that is used as input. We use this file as reference for all other files that are used, such as the header, or the rc file (since they are located next to the XML file)</param>
        /// <param name="pResourceName">(Optional) The name that should be used as resource identifier for the ribbon.</param>
        constructor Create(const pXMLFilePath: string; const pResourceName: string = 'APPLICATION');
        destructor Destroy; override;
        /// <summary> Converts a markup header file into a Delphi pas file, which can be included into the target form, in order to combine Delphi actions and ribbon commands. </summary>
        procedure GenerateMarkupFiles;
    end;

implementation

uses
  System.SysUtils, Settings, System.IOUtils, Winapi.ShellAPI;

{ TMarkupGenerator }

constructor TMarkupGenerator.Create(const pXMLFilePath: string; const pResourceName: string = 'APPLICATION');
begin
  inherited Create;

  fXMLFilePath := pXMLFilePath;
  if fXMLFilePath = '' then
  begin
    Writeln('Please specify a ribbon XML file as first paramter. Aborting execution.');
    Exit;
  end;

  fWorkingDir := ExtractFilePath(fXMLFilePath);

  fHeaderFilePath := ChangeFileExt(fXMLFilePath, '.h');
  fHeaderFileContent := TStringList.Create;
  fHeaderFileContent.Text := TFile.ReadAllText(fHeaderFilePath);

  fPasFilePath := ChangeFileExt(fXMLFilePath, '.pas');
  fResourceName := pResourceName;
end;

function TMarkupGenerator.GetTopSection: string;
begin
  Result :=  'unit ' + ChangeFileExt(ExtractFileName(fXMLFilePath), '') + ';'                   + sLineBreak
            +                                                                                     sLineBreak
            +'// *****************************************************************************' + sLineBreak
            +'// * This is an automatically generated source file for UI Element definition  *' + sLineBreak
            +'// * resource symbols and values. Please do not modify manually.               *' + sLineBreak
            +'// *****************************************************************************' + sLineBreak
            +                                                                                     sLineBreak
            +'interface'                                                                        + sLineBreak
            +                                                                                     sLineBreak
            +'{$R ''' + ChangeFileExt(ExtractFileName(fXMLFilePath), '.res') + '''}'           +  sLineBreak
            +                                                                                     sLineBreak
            +'uses'                                                                             + sLineBreak
            +'  Generics.Collections, SysUtils, UIRibbon;'                                      + sLineBreak
            +                                                                                     sLineBreak
            +'const'                                                                            + sLineBreak;
end;

function TMarkupGenerator.GetMiddleSection: string;
begin
  Result :=                                                                                       sLineBreak
           +'implementation'                                                                    + sLineBreak
           +                                                                                      sLineBreak
           +'function RegisterRibbonElements(): TRibbonMarkupElementList;'                      + sLineBreak
           +'begin'                                                                             + sLineBreak
           +'  Result := TRibbonMarkupElementList.Create(''' + fResourceName + ''');'           + sLineBreak;
end;

function TMarkupGenerator.GetEndSection: string;
begin
  Result := 'end;'                                                                              + sLineBreak
            +                                                                                     sLineBreak
            +'initialization'                                                                   + sLineBreak
            +                                                                                     sLineBreak
            +'  RegisterRibbonElements();'                                                      + sLineBreak
            +                                                                                     sLineBreak
            +'end.'
end;

destructor TMarkupGenerator.Destroy;
begin
  FreeAndNil(fHeaderFileContent);
  inherited;
end;

function TMarkupGenerator.ExtractMarkupElements: TList<TRibbonMarkupElement>;
var
  lLine: string;
  lLineArray: TArray<string>;
  lElement: TRibbonMarkupElement;
const
  cLabelTitle = 'LabelTitle';
  cTooltipTitle = 'TooltipTitle';
  cLabelDescription = 'LabelDescription';
  cTooltipDescription = 'TooltipDescription';


begin
  Result := TList<TRibbonMarkupElement>.Create;

  for lLine in fHeaderFileContent do
  begin
    if lLine.Contains('#define') then
    begin
      lLineArray := lLine.Split([' ']);

      // It's a command ID (they do not define resource IDs)
      if not lLineArray[1].Contains('RESID') then
      begin
        // TRibbonMarkupElement is a record. We need to fully modify it before adding to the list, otherwise, changes are not applied.
        // If we find a new command and lElement has been fully set during the last round of this loop, we know that lElement is finished and can be added.
        if not lElement.Name.IsEmpty then
          Result.Add(lElement);

        lElement := TRibbonMarkupElement.Create(lLineArray[1], StrToInt(lLineArray[2]));
      end

      else if lLineArray[1].Contains(cLabelTitle) then
        lElement.LabelTitleResourceID := StrToInt(lLineArray[2])

      else if lLineArray[1].Contains(cTooltipTitle) then
        lElement.TooltipTitleResourceID := StrToInt(lLineArray[2])

      else if lLineArray[1].Contains(cLabelDescription) then
        lElement.LabelDescriptionResourceID := StrToInt(lLineArray[2])

      else if lLineArray[1].Contains(cTooltipDescription) then
        lElement.TooltipDescriptionResourceID := StrToInt(lLineArray[2]);
    end;
  end;

  //After the loop -> Add the remaining lElement to the list.
  if not lElement.Name.IsEmpty then
    Result.Add(lElement);
end;

function TMarkupGenerator.GenerateCommandConstants: string;
var
  lLineArray: TArray<string>;
  lLine: string;
begin
  for lLine in fHeaderFileContent do
  begin
    if lLine.Contains('#define') then
    begin
      lLineArray := lLine.Split([' ']);
      Result := Result + '  ' + lLineArray[1] + ' = ' + lLineArray[2] + ';' + sLineBreak;
    end;
  end;
end;

function TMarkupGenerator.GenerateObjectDefinitions(pMarkupElements: TEnumerable<TRibbonMarkupElement>): string;
var
  lElement: TRibbonMarkupElement;
begin
  for lElement in pMarkupElements do
    Result := Result + Format('  Result.Add(TRibbonMarkupElement.Create(''%s'', %d, %d, %d, %d, %d));', [lElement.Name, lElement.ID, lElement.LabelTitleResourceID, lElement.LabelDescriptionResourceID, lElement.TooltipTitleResourceID, lElement.TooltipDescriptionResourceID]) + sLineBreak;
end;

procedure TMarkupGenerator.GenerateMarkupFiles;
var
  lPasFileContent: string;
  lMarkupElements: TList<TRibbonMarkupElement>;
begin
  lMarkupElements := ExtractMarkupElements;
  try
    lPasFileContent := GetTopSection;
    lPasFileContent := lPasFileContent + GenerateCommandConstants;
    lPasFileContent := lPasFileContent + GetMiddleSection;
    lPasFileContent := lPasFileContent + GenerateObjectDefinitions(lMarkupElements);
    lPasFileContent := lPasFileContent + GetEndSection;
  finally
    lMarkupElements.Free;
  end;
  TFile.WriteAllText(fPasFilePath, lPasFileContent);
end;

end.
