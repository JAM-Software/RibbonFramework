unit Settings;

interface

uses
  Classes,
  SysUtils;

type
  TSettings = class
  {$REGION 'Internal Declarations'}
  strict private
    class var FInstance: TSettings;
  strict private
    FSettingsFilename: String;
    FRibbonCompilerPath: String;
    FResourceCompilerPath: String;
    FDelphiCompilerPath: String;
  private
    constructor Create(const Dummy: Integer); overload;
    procedure Load;
  public
    class constructor Create;
    class destructor Destroy;
  {$ENDREGION}
  public
    constructor Create; overload;
    procedure Save;

    function ToolsAvailable: Boolean;

    class property Instance: TSettings read FInstance;

    property RibbonCompilerPath: String read FRibbonCompilerPath write FRibbonCompilerPath;
    property ResourceCompilerPath: String read FResourceCompilerPath write FResourceCompilerPath;
    property DelphiCompilerPath: String read FDelphiCompilerPath write FDelphiCompilerPath;
  end;

resourcestring
  RS_CANNOT_SAVE_SETTINGS = 'Unable to save settings.';

implementation

uses
  Windows,
  Registry,
  IOUtils,
  ShFolder,
  BasicXml;

const // Elemement Names
  EN_SETTINGS = 'Settings';
  EN_SETTING  = 'Setting';

const // Attribute Names
  AN_NAME     = 'name';
  AN_VALUE    = 'value';

const // Setting Names
  SN_RIBBON_COMPILER   = 'RibbonCompiler';
  SN_RESOURCE_COMPILER = 'ResourceCompiler';
  SN_DELPHI_COMPILER   = 'DelphiCompiler';

{ TSettings }

constructor TSettings.Create;
begin
  raise EInvalidOperation.Create('Don''t create a TSettings instance manually. Use TSettings.Instance.');
end;

constructor TSettings.Create(const Dummy: Integer);
var
  Reg: TRegistry;
  SdkPath, BdsKey, BdsPath: String;
  BdsVersion: Integer;
  Path: array [0..MAX_PATH] of Char;
begin
  inherited Create;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if (Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Microsoft SDKs\Windows\v7.0\WinSDKTools')) then
    begin
      SdkPath := Reg.ReadString('InstallationFolder');
      if (SdkPath <> '') then
      begin
        FRibbonCompilerPath := TPath.Combine(SdkPath, 'UICC.exe');
        if (not TFile.Exists(FRibbonCompilerPath)) then
          FRibbonCompilerPath := '';

        FResourceCompilerPath := TPath.Combine(SdkPath, 'RC.exe');
        if (not TFile.Exists(FResourceCompilerPath)) then
          FResourceCompilerPath := '';
      end;
    end;

    if (FRibbonCompilerPath = '') then
    begin
      FRibbonCompilerPath := ExtractFilePath(ParamStr(0)) + 'UICC.exe';
      if (not TFile.Exists(FRibbonCompilerPath)) then
        FRibbonCompilerPath := '';
    end;

    Reg.RootKey := HKEY_CURRENT_USER;
    for BdsVersion := 10 downto 7 do
    begin
      if (BdsVersion = 7) then
        BdsKey := 'Software\CodeGear\BDS\7.0'
      else
        BdsKey := 'Software\Embarcadero\BDS\' + IntToStr(BdsVersion) + '.0';
      if (Reg.OpenKeyReadOnly(BdsKey)) then
      begin
        BdsPath := Reg.ReadString('RootDir');
        if (BdsPath <> '') then
        begin
          BdsPath := TPath.Combine(BdsPath, 'bin');
          if (FResourceCompilerPath = '') then
          begin
            FResourceCompilerPath := TPath.Combine(BdsPath, 'RC.exe');
            if (not TFile.Exists(FResourceCompilerPath)) then
              FResourceCompilerPath := '';
          end;

          if (FDelphiCompilerPath = '') then
          begin
            FDelphiCompilerPath := TPath.Combine(BdsPath, 'DCC32.exe');
            if (not TFile.Exists(FDelphiCompilerPath)) then
              FDelphiCompilerPath := '';
          end;
        end;
      end;

      if (FRibbonCompilerPath <> '') and (FDelphiCompilerPath <> '') then
        Break;
    end;
  finally
    Reg.Free;
  end;

  if Succeeded(SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, 0, Path)) then
  begin
    FSettingsFilename := Path;
    FSettingsFilename := TPath.Combine(FSettingsFilename, 'Ribbon Designer');
    ForceDirectories(FSettingsFilename);
    FSettingsFilename := TPath.Combine(FSettingsFilename, 'Settings.xml');
    Load;
  end;
end;

class constructor TSettings.Create;
begin
  FInstance := TSettings.Create(0);
end;

class destructor TSettings.Destroy;
begin
  FInstance.Free;
end;

procedure TSettings.Load;
var
  Doc: TXmlDocument;
  E: TXmlElement;
  Name, Value: String;
begin
  if (not FileExists(FSettingsFilename)) then
    Exit;

  Doc := TXmlDocument.Create;
  try
    Doc.LoadFromFile(FSettingsFilename);
    if (Doc.Root = nil) or (Doc.Root.Name <> EN_SETTINGS) then
      Exit;

    for E in Doc.Root do
    begin
      if (E.Name = EN_SETTING) then
      begin
        Name := E.AttributeAsString[AN_NAME];
        Value := E.AttributeAsString[AN_VALUE];
        if (Value <> '') then
        begin
          if (Name = SN_RIBBON_COMPILER) and FileExists(Value) then
            FRibbonCompilerPath := Value
          else if (Name = SN_RESOURCE_COMPILER) and FileExists(Value) then
            FResourceCompilerPath := Value
          else if (Name = SN_DELPHI_COMPILER) and FileExists(Value) then
            FDelphiCompilerPath := Value;
        end;
      end;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TSettings.Save;
var
  Writer: TXmlWriter;
  Stream: TFileStream;
  Xml: RawByteString;

  procedure SaveSetting(const Name, Value: String);
  begin
    Writer.WriteStartElement(EN_SETTING);
    Writer.WriteAttribute(AN_NAME, Name);
    Writer.WriteAttribute(AN_VALUE, Value);
    Writer.WriteEndElement;
  end;

begin
  if (FSettingsFilename = '') then
    raise Exception.Create(RS_CANNOT_SAVE_SETTINGS);

  Writer := TXmlWriter.Create;
  try
    Writer.Indent := True;
    Writer.WriteStartElement(EN_SETTINGS);
    SaveSetting(SN_RIBBON_COMPILER, FRibbonCompilerPath);
    SaveSetting(SN_RESOURCE_COMPILER, FResourceCompilerPath);
    SaveSetting(SN_DELPHI_COMPILER, FDelphiCompilerPath);
    Writer.WriteEndElement;
    Xml := Writer.AsXml;
  finally
    Writer.Free;
  end;

  Stream := TFileStream.Create(FSettingsFilename, fmCreate);
  try
    if (Xml <> '') then
      Stream.WriteBuffer(Xml[1], Length(Xml));
  finally
    Stream.Free;
  end;
end;

function TSettings.ToolsAvailable: Boolean;
begin
  Result := (FRibbonCompilerPath <> '') and (FResourceCompilerPath <> '')
    and (FDelphiCompilerPath <> '');
end;

end.
