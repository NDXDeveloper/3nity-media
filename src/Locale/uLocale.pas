{ ═══════════════════════════════════════════════════════════════════════════════
  uLocale.pas - Internationalization (i18n) Support

  Part of 3nity Media - Lazarus Edition

  This unit provides multi-language support for the application.
  Translations are stored in .lang files (INI format) in the Locale folder.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uLocale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, FileUtil, LazFileUtils;

type
  { TLocaleManager }
  TLocaleManager = class
  private
    FCurrentLang: string;
    FLangFile: TIniFile;
    FLangPath: string;
    FAvailableLanguages: TStringList;
    FLanguageNames: TStringList;

    procedure ScanAvailableLanguages;
    function GetLangFilePath(const LangCode: string): string;
    function DetectOSLanguage: string;
  public
    constructor Create;
    destructor Destroy; override;

    { Load a language by code (e.g., 'en', 'fr', 'de') }
    function LoadLanguage(const LangCode: string): Boolean;

    { Get translated string by key, returns default if not found }
    function GetString(const Section, Key: string; const Default: string = ''): string;

    { Shortcut for common sections }
    function Menu(const Key: string; const Default: string = ''): string;
    function Dialog(const Key: string; const Default: string = ''): string;
    function Status(const Key: string; const Default: string = ''): string;
    function Button(const Key: string; const Default: string = ''): string;
    function Label_(const Key: string; const Default: string = ''): string;
    function Message(const Key: string; const Default: string = ''): string;
    function Tooltip(const Key: string; const Default: string = ''): string;

    { Properties }
    property CurrentLanguage: string read FCurrentLang;
    property AvailableLanguages: TStringList read FAvailableLanguages;
    property LanguageNames: TStringList read FLanguageNames;
    property LangPath: string read FLangPath;
  end;

{ Global locale manager instance }
function Locale: TLocaleManager;

{ Shortcut function for translations }
function _T(const Section, Key: string; const Default: string = ''): string;

implementation

var
  FLocaleManager: TLocaleManager = nil;

function Locale: TLocaleManager;
begin
  if FLocaleManager = nil then
    FLocaleManager := TLocaleManager.Create;
  Result := FLocaleManager;
end;

function _T(const Section, Key: string; const Default: string = ''): string;
begin
  Result := Locale.GetString(Section, Key, Default);
end;

{ TLocaleManager }

constructor TLocaleManager.Create;
var
  ExePath: string;
  DetectedLang: string;
  {$IFDEF UNIX}
  SnapPath: string;
  {$ENDIF}
begin
  inherited Create;

  FCurrentLang := 'en';
  FLangFile := nil;
  FAvailableLanguages := TStringList.Create;
  FLanguageNames := TStringList.Create;

  { Determine language files path }
  ExePath := ExtractFilePath(ParamStr(0));
  FLangPath := '';

  { ═══════════════════════════════════════════════════════════════════════════
    LOCALE PATH DETECTION - Priority order:
    1. Portable/Development (relative to exe)
    2. macOS App Bundle
    3. Linux standard installation
    4. Snap package
    5. Flatpak
    6. System-wide fallbacks
    7. Default fallback
    ═══════════════════════════════════════════════════════════════════════════ }

  { 1. Portable/Development - relative to executable (Windows, Linux portable, dev) }
  if DirectoryExists(ExePath + 'locale') then
    FLangPath := ExePath + 'locale' + DirectorySeparator
  else if DirectoryExists(ExePath + 'lang') then
    FLangPath := ExePath + 'lang' + DirectorySeparator

  {$IFDEF DARWIN}
  { 2. macOS App Bundle - Resources folder (standard macOS location) }
  else if DirectoryExists(ExePath + '../Resources/locale') then
    FLangPath := ExePath + '../Resources/locale' + DirectorySeparator
  {$ENDIF}

  {$IFDEF UNIX}
  { 3. Standard Linux/Unix installation (exe in /usr/bin/, /usr/games/, etc.) }
  else if DirectoryExists(ExePath + '../share/3nity-media/locale') then
    FLangPath := ExePath + '../share/3nity-media/locale' + DirectorySeparator

  { 4. Snap package ($SNAP environment variable) }
  else if GetEnvironmentVariable('SNAP') <> '' then
  begin
    SnapPath := GetEnvironmentVariable('SNAP') + '/share/3nity-media/locale';
    if DirectoryExists(SnapPath) then
      FLangPath := SnapPath + '/'
  end

  { 5. Flatpak (/app is the standard Flatpak root) }
  else if DirectoryExists('/app/share/3nity-media/locale') then
    FLangPath := '/app/share/3nity-media/locale/'

  { 6. System-wide fallbacks }
  else if DirectoryExists('/usr/share/3nity-media/locale') then
    FLangPath := '/usr/share/3nity-media/locale/'
  else if DirectoryExists('/usr/local/share/3nity-media/locale') then
    FLangPath := '/usr/local/share/3nity-media/locale/'
  {$ENDIF}

  ;

  { 7. Default fallback - locale folder next to executable }
  if FLangPath = '' then
    FLangPath := ExePath + 'locale' + DirectorySeparator;

  { Scan for available languages }
  ScanAvailableLanguages;

  { Detect OS language and load it (fallback to English if not available) }
  DetectedLang := DetectOSLanguage;
  LoadLanguage(DetectedLang);
end;

destructor TLocaleManager.Destroy;
begin
  FreeAndNil(FLangFile);
  FreeAndNil(FAvailableLanguages);
  FreeAndNil(FLanguageNames);
  inherited Destroy;
end;

procedure TLocaleManager.ScanAvailableLanguages;
var
  SearchRec: TSearchRec;
  LangCode, LangName: string;
  TempIni: TIniFile;
begin
  FAvailableLanguages.Clear;
  FLanguageNames.Clear;

  { Always add English as fallback }
  FAvailableLanguages.Add('en');
  FLanguageNames.Add('English');

  if not DirectoryExists(FLangPath) then
    Exit;

  if FindFirst(FLangPath + '*.lang', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      LangCode := ChangeFileExt(SearchRec.Name, '');
      if LangCode <> 'en' then
      begin
        { Try to read language name from file }
        try
          TempIni := TIniFile.Create(FLangPath + SearchRec.Name);
          try
            LangName := TempIni.ReadString('Language', 'Name', LangCode);
            FAvailableLanguages.Add(LangCode);
            FLanguageNames.Add(LangName);
          finally
            TempIni.Free;
          end;
        except
          { Ignore errors, skip this file }
        end;
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;


function TLocaleManager.GetLangFilePath(const LangCode: string): string;
begin
  Result := FLangPath + LangCode + '.lang';
end;

function TLocaleManager.DetectOSLanguage: string;
var
  LangEnv: string;
  LangCode: string;
begin
  Result := 'en'; { Default fallback }

  { Try LC_ALL first, then LANG }
  LangEnv := GetEnvironmentVariable('LC_ALL');
  if LangEnv = '' then
    LangEnv := GetEnvironmentVariable('LC_MESSAGES');
  if LangEnv = '' then
    LangEnv := GetEnvironmentVariable('LANG');

  if LangEnv = '' then
    Exit;

  { Extract language code (e.g., "fr_FR.UTF-8" -> "fr") }
  LangCode := LowerCase(Copy(LangEnv, 1, 2));

  { Check if we have a .lang file for this language }
  if FileExists(FLangPath + LangCode + '.lang') then
    Result := LangCode;
end;

function TLocaleManager.LoadLanguage(const LangCode: string): Boolean;
var
  FilePath: string;
begin
  Result := False;
  FilePath := GetLangFilePath(LangCode);

  { Free existing language file }
  FreeAndNil(FLangFile);

  { Check if file exists }
  if FileExists(FilePath) then
  begin
    try
      FLangFile := TIniFile.Create(FilePath);
      FCurrentLang := LangCode;
      Result := True;
    except
      FLangFile := nil;
      FCurrentLang := 'en';
    end;
  end
  else
  begin
    { If requested language not found, try English }
    if LangCode <> 'en' then
    begin
      FilePath := GetLangFilePath('en');
      if FileExists(FilePath) then
      begin
        try
          FLangFile := TIniFile.Create(FilePath);
          FCurrentLang := 'en';
          Result := True;
        except
          FLangFile := nil;
        end;
      end;
    end;
  end;
end;

function TLocaleManager.GetString(const Section, Key: string; const Default: string): string;
begin
  if FLangFile <> nil then
    Result := FLangFile.ReadString(Section, Key, Default)
  else
    Result := Default;

  { If empty, return default }
  if Result = '' then
    Result := Default;
end;

function TLocaleManager.Menu(const Key: string; const Default: string): string;
begin
  Result := GetString('Menu', Key, Default);
end;

function TLocaleManager.Dialog(const Key: string; const Default: string): string;
begin
  Result := GetString('Dialog', Key, Default);
end;

function TLocaleManager.Status(const Key: string; const Default: string): string;
begin
  Result := GetString('Status', Key, Default);
end;

function TLocaleManager.Button(const Key: string; const Default: string): string;
begin
  Result := GetString('Button', Key, Default);
end;

function TLocaleManager.Label_(const Key: string; const Default: string): string;
begin
  Result := GetString('Label', Key, Default);
end;

function TLocaleManager.Message(const Key: string; const Default: string): string;
begin
  Result := GetString('Message', Key, Default);
end;

function TLocaleManager.Tooltip(const Key: string; const Default: string): string;
begin
  Result := GetString('Tooltip', Key, Default);
end;

initialization

finalization
  FreeAndNil(FLocaleManager);

end.
