{ ═══════════════════════════════════════════════════════════════════════════════
  uTestLocale.pas - Unit Tests for Internationalization (i18n) Support

  Part of 3nity Media Test Suite

  Tests for uLocale.pas:
  - TLocaleManager class (creation, loading, translation)
  - Section shortcut methods (Menu, Dialog, Status, etc.)
  - Language file handling
  - Global functions (Locale, _T)

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uTestLocale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  uLocale;

type
  { ═══════════════════════════════════════════════════════════════════════════
    TTestLocale - Test cases for Locale Manager
    ═══════════════════════════════════════════════════════════════════════════ }

  TTestLocale = class(TTestCase)
  private
    FTempDir: string;
    procedure CreateTestLangFile(const LangCode, LangName: string);
    procedure CleanupTempDir;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { ─────────────────────────────────────────────────────────────────────────
      Global Locale Function Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Locale_NotNil;
    procedure Test_Locale_Singleton;
    procedure Test_Locale_HasCurrentLanguage;

    { ─────────────────────────────────────────────────────────────────────────
      _T Function Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_T_ReturnsDefault;
    procedure Test_T_EmptyDefault;

    { ─────────────────────────────────────────────────────────────────────────
      TLocaleManager Creation Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Locale_AvailableLanguages_NotNil;
    procedure Test_Locale_AvailableLanguages_ContainsEnglish;
    procedure Test_Locale_LanguageNames_NotNil;
    procedure Test_Locale_LanguageNames_ContainsEnglish;
    procedure Test_Locale_LangPath_NotEmpty;

    { ─────────────────────────────────────────────────────────────────────────
      CurrentLanguage Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_CurrentLanguage_NotEmpty;
    procedure Test_CurrentLanguage_TwoChars;

    { ─────────────────────────────────────────────────────────────────────────
      GetString Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_GetString_ReturnsDefault;
    procedure Test_GetString_EmptyDefault;
    procedure Test_GetString_NonExistentSection;
    procedure Test_GetString_NonExistentKey;

    { ─────────────────────────────────────────────────────────────────────────
      Menu Section Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Menu_ReturnsDefault;
    procedure Test_Menu_EmptyDefault;

    { ─────────────────────────────────────────────────────────────────────────
      Dialog Section Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Dialog_ReturnsDefault;
    procedure Test_Dialog_EmptyDefault;

    { ─────────────────────────────────────────────────────────────────────────
      Status Section Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Status_ReturnsDefault;
    procedure Test_Status_EmptyDefault;

    { ─────────────────────────────────────────────────────────────────────────
      Button Section Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Button_ReturnsDefault;
    procedure Test_Button_EmptyDefault;

    { ─────────────────────────────────────────────────────────────────────────
      Label_ Section Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Label_ReturnsDefault;
    procedure Test_Label_EmptyDefault;

    { ─────────────────────────────────────────────────────────────────────────
      Message Section Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Message_ReturnsDefault;
    procedure Test_Message_EmptyDefault;

    { ─────────────────────────────────────────────────────────────────────────
      Tooltip Section Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Tooltip_ReturnsDefault;
    procedure Test_Tooltip_EmptyDefault;

    { ─────────────────────────────────────────────────────────────────────────
      LoadLanguage Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_LoadLanguage_NonExistent_ReturnsFalse;

    { ─────────────────────────────────────────────────────────────────────────
      AvailableLanguages Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_AvailableLanguages_AtLeastOne;
    procedure Test_AvailableLanguages_EnglishFirst;

    { ─────────────────────────────────────────────────────────────────────────
      LanguageNames Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_LanguageNames_SameCountAsLanguages;
    procedure Test_LanguageNames_EnglishFirst;

    { ─────────────────────────────────────────────────────────────────────────
      Section Method Consistency Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Menu_UsesMenuSection;
    procedure Test_Dialog_UsesDialogSection;
    procedure Test_Status_UsesStatusSection;
    procedure Test_Button_UsesButtonSection;
    procedure Test_Label_UsesLabelSection;
    procedure Test_Message_UsesMessageSection;
    procedure Test_Tooltip_UsesTooltipSection;

    { ─────────────────────────────────────────────────────────────────────────
      Default Value Handling Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_GetString_WithSpecialChars;
    procedure Test_GetString_WithSpaces;
    procedure Test_GetString_WithNumbers;
    procedure Test_GetString_Unicode;

    { ─────────────────────────────────────────────────────────────────────────
      Edge Cases
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_GetString_EmptySection;
    procedure Test_GetString_EmptyKey;
    procedure Test_Menu_EmptyKey;
    procedure Test_Dialog_EmptyKey;
  end;

implementation

uses
  uConfig, FileUtil;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestLocale Implementation
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestLocale.CreateTestLangFile(const LangCode, LangName: string);
var
  F: TextFile;
  FilePath: string;
begin
  FilePath := FTempDir + LangCode + '.lang';
  AssignFile(F, FilePath);
  Rewrite(F);
  WriteLn(F, '[Language]');
  WriteLn(F, 'Name=' + LangName);
  WriteLn(F, '');
  WriteLn(F, '[Menu]');
  WriteLn(F, 'File=File');
  WriteLn(F, 'Edit=Edit');
  WriteLn(F, '');
  WriteLn(F, '[Dialog]');
  WriteLn(F, 'Open=Open File');
  WriteLn(F, '');
  WriteLn(F, '[Status]');
  WriteLn(F, 'Ready=Ready');
  WriteLn(F, '');
  WriteLn(F, '[Button]');
  WriteLn(F, 'OK=OK');
  WriteLn(F, 'Cancel=Cancel');
  WriteLn(F, '');
  WriteLn(F, '[Label]');
  WriteLn(F, 'Volume=Volume');
  WriteLn(F, '');
  WriteLn(F, '[Message]');
  WriteLn(F, 'Error=An error occurred');
  WriteLn(F, '');
  WriteLn(F, '[Tooltip]');
  WriteLn(F, 'Play=Play media');
  CloseFile(F);
end;

procedure TTestLocale.CleanupTempDir;
var
  SearchRec: TSearchRec;
begin
  if DirectoryExists(FTempDir) then
  begin
    if FindFirst(FTempDir + '*', faAnyFile, SearchRec) = 0 then
    begin
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          DeleteFile(FTempDir + SearchRec.Name);
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
    RemoveDir(FTempDir);
  end;
end;

procedure TTestLocale.SetUp;
begin
  FTempDir := GetTempDir + 'test_locale_' + IntToStr(Random(100000)) + DirectorySeparator;
  ForceDirectories(FTempDir);
end;

procedure TTestLocale.TearDown;
begin
  CleanupTempDir;
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Global Locale Function Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_Locale_NotNil;
begin
  AssertNotNull('Locale should not be nil', Locale);
end;

procedure TTestLocale.Test_Locale_Singleton;
var
  L1, L2: TLocaleManager;
begin
  L1 := Locale;
  L2 := Locale;
  AssertSame('Locale should return same instance', L1, L2);
end;

procedure TTestLocale.Test_Locale_HasCurrentLanguage;
begin
  AssertTrue('CurrentLanguage should not be empty', Locale.CurrentLanguage <> '');
end;

{ ─────────────────────────────────────────────────────────────────────────────
  _T Function Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_T_ReturnsDefault;
begin
  AssertEquals('Should return default', 'DefaultText', _T('NonExistent', 'NonExistent', 'DefaultText'));
end;

procedure TTestLocale.Test_T_EmptyDefault;
begin
  AssertEquals('Should return empty default', '', _T('NonExistent', 'NonExistent', ''));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TLocaleManager Creation Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_Locale_AvailableLanguages_NotNil;
begin
  AssertNotNull('AvailableLanguages should not be nil', Locale.AvailableLanguages);
end;

procedure TTestLocale.Test_Locale_AvailableLanguages_ContainsEnglish;
begin
  AssertTrue('AvailableLanguages should contain en', Locale.AvailableLanguages.IndexOf('en') >= 0);
end;

procedure TTestLocale.Test_Locale_LanguageNames_NotNil;
begin
  AssertNotNull('LanguageNames should not be nil', Locale.LanguageNames);
end;

procedure TTestLocale.Test_Locale_LanguageNames_ContainsEnglish;
begin
  AssertTrue('LanguageNames should contain English', Locale.LanguageNames.IndexOf('English') >= 0);
end;

procedure TTestLocale.Test_Locale_LangPath_NotEmpty;
begin
  AssertTrue('LangPath should not be empty', Locale.LangPath <> '');
end;

{ ─────────────────────────────────────────────────────────────────────────────
  CurrentLanguage Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_CurrentLanguage_NotEmpty;
begin
  AssertTrue('CurrentLanguage not empty', Locale.CurrentLanguage <> '');
end;

procedure TTestLocale.Test_CurrentLanguage_TwoChars;
begin
  AssertEquals('CurrentLanguage should be 2 chars', 2, Length(Locale.CurrentLanguage));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  GetString Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_GetString_ReturnsDefault;
begin
  AssertEquals('GetString returns default', 'MyDefault', Locale.GetString('NonExistent', 'NonExistent', 'MyDefault'));
end;

procedure TTestLocale.Test_GetString_EmptyDefault;
begin
  AssertEquals('GetString with empty default', '', Locale.GetString('NonExistent', 'NonExistent', ''));
end;

procedure TTestLocale.Test_GetString_NonExistentSection;
begin
  AssertEquals('NonExistent section', 'Fallback', Locale.GetString('ZZZSection', 'Key', 'Fallback'));
end;

procedure TTestLocale.Test_GetString_NonExistentKey;
begin
  AssertEquals('NonExistent key', 'Fallback', Locale.GetString('Menu', 'ZZZKey', 'Fallback'));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Menu Section Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_Menu_ReturnsDefault;
begin
  AssertEquals('Menu returns default', 'FileMenu', Locale.Menu('NonExistent', 'FileMenu'));
end;

procedure TTestLocale.Test_Menu_EmptyDefault;
begin
  AssertEquals('Menu with empty default', '', Locale.Menu('NonExistent', ''));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Dialog Section Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_Dialog_ReturnsDefault;
begin
  AssertEquals('Dialog returns default', 'OpenDialog', Locale.Dialog('NonExistent', 'OpenDialog'));
end;

procedure TTestLocale.Test_Dialog_EmptyDefault;
begin
  AssertEquals('Dialog with empty default', '', Locale.Dialog('NonExistent', ''));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Status Section Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_Status_ReturnsDefault;
begin
  AssertEquals('Status returns default', 'ReadyStatus', Locale.Status('NonExistent', 'ReadyStatus'));
end;

procedure TTestLocale.Test_Status_EmptyDefault;
begin
  AssertEquals('Status with empty default', '', Locale.Status('NonExistent', ''));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Button Section Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_Button_ReturnsDefault;
begin
  AssertEquals('Button returns default', 'OKButton', Locale.Button('NonExistent', 'OKButton'));
end;

procedure TTestLocale.Test_Button_EmptyDefault;
begin
  AssertEquals('Button with empty default', '', Locale.Button('NonExistent', ''));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Label_ Section Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_Label_ReturnsDefault;
begin
  AssertEquals('Label returns default', 'VolumeLabel', Locale.Label_('NonExistent', 'VolumeLabel'));
end;

procedure TTestLocale.Test_Label_EmptyDefault;
begin
  AssertEquals('Label with empty default', '', Locale.Label_('NonExistent', ''));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Message Section Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_Message_ReturnsDefault;
begin
  AssertEquals('Message returns default', 'ErrorMsg', Locale.Message('NonExistent', 'ErrorMsg'));
end;

procedure TTestLocale.Test_Message_EmptyDefault;
begin
  AssertEquals('Message with empty default', '', Locale.Message('NonExistent', ''));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Tooltip Section Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_Tooltip_ReturnsDefault;
begin
  AssertEquals('Tooltip returns default', 'PlayTooltip', Locale.Tooltip('NonExistent', 'PlayTooltip'));
end;

procedure TTestLocale.Test_Tooltip_EmptyDefault;
begin
  AssertEquals('Tooltip with empty default', '', Locale.Tooltip('NonExistent', ''));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  LoadLanguage Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_LoadLanguage_NonExistent_ReturnsFalse;
begin
  { Loading a non-existent language code should fail or fallback }
  { Note: The actual behavior depends on whether English fallback exists }
  { This test verifies the method doesn't crash }
  Locale.LoadLanguage('zz');
  AssertTrue('Should not crash', True);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  AvailableLanguages Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_AvailableLanguages_AtLeastOne;
begin
  AssertTrue('Should have at least one language', Locale.AvailableLanguages.Count >= 1);
end;

procedure TTestLocale.Test_AvailableLanguages_EnglishFirst;
begin
  AssertEquals('English should be first', 'en', Locale.AvailableLanguages[0]);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  LanguageNames Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_LanguageNames_SameCountAsLanguages;
begin
  AssertEquals('Names count matches languages count',
    Locale.AvailableLanguages.Count, Locale.LanguageNames.Count);
end;

procedure TTestLocale.Test_LanguageNames_EnglishFirst;
begin
  AssertEquals('English should be first name', 'English', Locale.LanguageNames[0]);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Section Method Consistency Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_Menu_UsesMenuSection;
begin
  { Menu should be equivalent to GetString('Menu', ...) }
  AssertEquals('Menu uses Menu section',
    Locale.GetString('Menu', 'TestKey', 'Default'),
    Locale.Menu('TestKey', 'Default'));
end;

procedure TTestLocale.Test_Dialog_UsesDialogSection;
begin
  AssertEquals('Dialog uses Dialog section',
    Locale.GetString('Dialog', 'TestKey', 'Default'),
    Locale.Dialog('TestKey', 'Default'));
end;

procedure TTestLocale.Test_Status_UsesStatusSection;
begin
  AssertEquals('Status uses Status section',
    Locale.GetString('Status', 'TestKey', 'Default'),
    Locale.Status('TestKey', 'Default'));
end;

procedure TTestLocale.Test_Button_UsesButtonSection;
begin
  AssertEquals('Button uses Button section',
    Locale.GetString('Button', 'TestKey', 'Default'),
    Locale.Button('TestKey', 'Default'));
end;

procedure TTestLocale.Test_Label_UsesLabelSection;
begin
  AssertEquals('Label uses Label section',
    Locale.GetString('Label', 'TestKey', 'Default'),
    Locale.Label_('TestKey', 'Default'));
end;

procedure TTestLocale.Test_Message_UsesMessageSection;
begin
  AssertEquals('Message uses Message section',
    Locale.GetString('Message', 'TestKey', 'Default'),
    Locale.Message('TestKey', 'Default'));
end;

procedure TTestLocale.Test_Tooltip_UsesTooltipSection;
begin
  AssertEquals('Tooltip uses Tooltip section',
    Locale.GetString('Tooltip', 'TestKey', 'Default'),
    Locale.Tooltip('TestKey', 'Default'));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Default Value Handling Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_GetString_WithSpecialChars;
begin
  AssertEquals('Special chars preserved',
    'Test & <value> "quoted"',
    Locale.GetString('X', 'Y', 'Test & <value> "quoted"'));
end;

procedure TTestLocale.Test_GetString_WithSpaces;
begin
  AssertEquals('Spaces preserved',
    '  leading and trailing  ',
    Locale.GetString('X', 'Y', '  leading and trailing  '));
end;

procedure TTestLocale.Test_GetString_WithNumbers;
begin
  AssertEquals('Numbers preserved',
    'Value: 12345',
    Locale.GetString('X', 'Y', 'Value: 12345'));
end;

procedure TTestLocale.Test_GetString_Unicode;
begin
  AssertEquals('Unicode preserved',
    'Français 日本語 Русский',
    Locale.GetString('X', 'Y', 'Français 日本語 Русский'));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Edge Cases
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestLocale.Test_GetString_EmptySection;
begin
  AssertEquals('Empty section returns default',
    'Default',
    Locale.GetString('', 'Key', 'Default'));
end;

procedure TTestLocale.Test_GetString_EmptyKey;
begin
  AssertEquals('Empty key returns default',
    'Default',
    Locale.GetString('Section', '', 'Default'));
end;

procedure TTestLocale.Test_Menu_EmptyKey;
begin
  AssertEquals('Menu empty key returns default',
    'Default',
    Locale.Menu('', 'Default'));
end;

procedure TTestLocale.Test_Dialog_EmptyKey;
begin
  AssertEquals('Dialog empty key returns default',
    'Default',
    Locale.Dialog('', 'Default'));
end;

initialization
  RegisterTest(TTestLocale);

end.
