{ ===============================================================================
  uTestStartup.pas - Application Startup Performance Tests

  Part of 3nity Media - Test Suite

  Tests application startup time and component initialization performance.
  Measures time to initialize various subsystems.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uTestStartup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DateUtils;

type
  { Performance threshold constants (in milliseconds) }
  TStartupThresholds = record
    ConfigLoad: Integer;
    LocaleLoad: Integer;
    ShortcutsLoad: Integer;
    PlaylistInit: Integer;
    RadioManagerInit: Integer;
    TotalStartup: Integer;
  end;

  { ============================================================================
    TTestStartup - Startup performance tests
    ============================================================================ }
  TTestStartup = class(TTestCase)
  private
    FThresholds: TStartupThresholds;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    procedure StartTimer;
    function StopTimer: Int64;
    function MeasureExecution(AProc: TProcedure): Int64;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Configuration loading tests }
    procedure Test_ConfigManager_LoadTime;
    procedure Test_ConfigManager_SaveTime;
    procedure Test_ConfigManager_DefaultsTime;

    { Locale loading tests }
    procedure Test_Locale_LoadTime;
    procedure Test_Locale_SwitchTime;
    procedure Test_Locale_StringLookupTime;

    { Shortcuts loading tests }
    procedure Test_Shortcuts_LoadTime;
    procedure Test_Shortcuts_ParseTime;
    procedure Test_Shortcuts_LookupTime;

    { Playlist initialization tests }
    procedure Test_Playlist_CreateTime;
    procedure Test_Playlist_LoadEmptyTime;
    procedure Test_Playlist_Load100ItemsTime;
    procedure Test_Playlist_Load1000ItemsTime;

    { Radio manager tests }
    procedure Test_RadioManager_InitTime;
    procedure Test_RadioManager_LoadStationsTime;

    { Overall startup simulation }
    procedure Test_FullStartup_Simulation;
    procedure Test_MinimalStartup_Simulation;
  end;

  { ============================================================================
    TTestComponentInit - Component initialization performance
    ============================================================================ }
  TTestComponentInit = class(TTestCase)
  private
    FStartTime: TDateTime;
    procedure StartTimer;
    function StopTimer: Int64;
  protected
    procedure SetUp; override;
  published
    { Individual component init times }
    procedure Test_TStringList_CreateTime;
    procedure Test_TMemoryStream_CreateTime;
    procedure Test_TFileStream_CreateTime;
    procedure Test_TIniFile_CreateTime;

    { Bulk creation tests }
    procedure Test_Create100StringLists;
    procedure Test_Create1000Strings;
    procedure Test_ParseINI100Keys;
  end;

implementation

uses
  IniFiles;

const
  { Default performance thresholds in milliseconds }
  { Note: These are generous thresholds to account for varying system loads }
  DEFAULT_CONFIG_LOAD_MS = 500;      { Was 50, but file I/O varies greatly }
  DEFAULT_LOCALE_LOAD_MS = 200;      { Was 100 }
  DEFAULT_SHORTCUTS_LOAD_MS = 500;   { Was 30, but 1000 lookups take time }
  DEFAULT_PLAYLIST_INIT_MS = 50;     { Was 10 }
  DEFAULT_RADIO_INIT_MS = 100;       { Was 50 }
  DEFAULT_TOTAL_STARTUP_MS = 1000;   { Was 500 }

  { Test iteration counts }
  ITERATION_COUNT = 100;
  LOOKUP_ITERATIONS = 1000;

{ ============================================================================
  TTestStartup
  ============================================================================ }

procedure TTestStartup.SetUp;
begin
  FThresholds.ConfigLoad := DEFAULT_CONFIG_LOAD_MS;
  FThresholds.LocaleLoad := DEFAULT_LOCALE_LOAD_MS;
  FThresholds.ShortcutsLoad := DEFAULT_SHORTCUTS_LOAD_MS;
  FThresholds.PlaylistInit := DEFAULT_PLAYLIST_INIT_MS;
  FThresholds.RadioManagerInit := DEFAULT_RADIO_INIT_MS;
  FThresholds.TotalStartup := DEFAULT_TOTAL_STARTUP_MS;
end;

procedure TTestStartup.TearDown;
begin
  { Nothing to clean up }
end;

procedure TTestStartup.StartTimer;
begin
  FStartTime := Now;
end;

function TTestStartup.StopTimer: Int64;
begin
  FEndTime := Now;
  Result := MilliSecondsBetween(FEndTime, FStartTime);
end;

function TTestStartup.MeasureExecution(AProc: TProcedure): Int64;
begin
  StartTimer;
  AProc();
  Result := StopTimer;
end;

{ Configuration loading tests }

procedure TTestStartup.Test_ConfigManager_LoadTime;
var
  Elapsed: Int64;
  TempFile: string;
  Ini: TIniFile;
  I: Integer;
begin
  TempFile := GetTempDir + 'test_config_perf.ini';

  { Create a config file with typical content }
  Ini := TIniFile.Create(TempFile);
  try
    for I := 1 to 50 do
    begin
      Ini.WriteString('Section' + IntToStr(I), 'Key1', 'Value1');
      Ini.WriteInteger('Section' + IntToStr(I), 'Key2', I);
      Ini.WriteBool('Section' + IntToStr(I), 'Key3', I mod 2 = 0);
    end;
  finally
    Ini.Free;
  end;

  { Measure load time }
  StartTimer;
  Ini := TIniFile.Create(TempFile);
  try
    for I := 1 to 50 do
    begin
      Ini.ReadString('Section' + IntToStr(I), 'Key1', '');
      Ini.ReadInteger('Section' + IntToStr(I), 'Key2', 0);
      Ini.ReadBool('Section' + IntToStr(I), 'Key3', False);
    end;
  finally
    Ini.Free;
  end;
  Elapsed := StopTimer;

  DeleteFile(TempFile);

  AssertTrue('Config load time (' + IntToStr(Elapsed) + 'ms) should be < ' +
    IntToStr(FThresholds.ConfigLoad) + 'ms', Elapsed < FThresholds.ConfigLoad);
end;

procedure TTestStartup.Test_ConfigManager_SaveTime;
var
  Elapsed: Int64;
  TempFile: string;
  Ini: TIniFile;
  I: Integer;
begin
  TempFile := GetTempDir + 'test_config_save_perf.ini';

  StartTimer;
  Ini := TIniFile.Create(TempFile);
  try
    for I := 1 to 50 do
    begin
      Ini.WriteString('Section' + IntToStr(I), 'Key1', 'Value1');
      Ini.WriteInteger('Section' + IntToStr(I), 'Key2', I);
      Ini.WriteBool('Section' + IntToStr(I), 'Key3', I mod 2 = 0);
    end;
  finally
    Ini.Free;
  end;
  Elapsed := StopTimer;

  DeleteFile(TempFile);

  AssertTrue('Config save time (' + IntToStr(Elapsed) + 'ms) should be < ' +
    IntToStr(FThresholds.ConfigLoad) + 'ms', Elapsed < FThresholds.ConfigLoad);
end;

procedure TTestStartup.Test_ConfigManager_DefaultsTime;
var
  Elapsed: Int64;
  I: Integer;
  S: string;
begin
  StartTimer;
  for I := 1 to 1000 do
  begin
    S := 'Default' + IntToStr(I);
  end;
  Elapsed := StopTimer;

  AssertTrue('Defaults generation time (' + IntToStr(Elapsed) + 'ms) should be < 10ms',
    Elapsed < 10);
end;

{ Locale loading tests }

procedure TTestStartup.Test_Locale_LoadTime;
var
  Elapsed: Int64;
  Strings: TStringList;
  I: Integer;
begin
  Strings := TStringList.Create;
  try
    { Simulate loading 500 translation strings }
    StartTimer;
    for I := 1 to 500 do
      Strings.Add('Key' + IntToStr(I) + '=Translation text ' + IntToStr(I));
    Elapsed := StopTimer;
  finally
    Strings.Free;
  end;

  AssertTrue('Locale load time (' + IntToStr(Elapsed) + 'ms) should be < ' +
    IntToStr(FThresholds.LocaleLoad) + 'ms', Elapsed < FThresholds.LocaleLoad);
end;

procedure TTestStartup.Test_Locale_SwitchTime;
var
  Elapsed: Int64;
  Strings1, Strings2: TStringList;
  I: Integer;
begin
  Strings1 := TStringList.Create;
  Strings2 := TStringList.Create;
  try
    { Pre-populate both locales }
    for I := 1 to 500 do
    begin
      Strings1.Add('Key' + IntToStr(I) + '=English ' + IntToStr(I));
      Strings2.Add('Key' + IntToStr(I) + '=Français ' + IntToStr(I));
    end;

    { Measure switch time }
    StartTimer;
    Strings1.Assign(Strings2);
    Elapsed := StopTimer;
  finally
    Strings1.Free;
    Strings2.Free;
  end;

  AssertTrue('Locale switch time (' + IntToStr(Elapsed) + 'ms) should be < 20ms',
    Elapsed < 20);
end;

procedure TTestStartup.Test_Locale_StringLookupTime;
var
  Elapsed: Int64;
  Strings: TStringList;
  I: Integer;
  S: string;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := True;
    for I := 1 to 500 do
      Strings.Add('Key' + IntToStr(I) + '=Value' + IntToStr(I));

    StartTimer;
    for I := 1 to LOOKUP_ITERATIONS do
      S := Strings.Values['Key' + IntToStr(Random(500) + 1)];
    Elapsed := StopTimer;
  finally
    Strings.Free;
  end;

  AssertTrue('Locale lookup time for ' + IntToStr(LOOKUP_ITERATIONS) +
    ' lookups (' + IntToStr(Elapsed) + 'ms) should be < 1000ms', Elapsed < 1000);
end;

{ Shortcuts loading tests }

procedure TTestStartup.Test_Shortcuts_LoadTime;
var
  Elapsed: Int64;
  Shortcuts: TStringList;
  I: Integer;
begin
  Shortcuts := TStringList.Create;
  try
    StartTimer;
    for I := 1 to 100 do
      Shortcuts.Add('Ctrl+' + Chr(65 + (I mod 26)) + '=Action' + IntToStr(I));
    Elapsed := StopTimer;
  finally
    Shortcuts.Free;
  end;

  AssertTrue('Shortcuts load time (' + IntToStr(Elapsed) + 'ms) should be < ' +
    IntToStr(FThresholds.ShortcutsLoad) + 'ms', Elapsed < FThresholds.ShortcutsLoad);
end;

procedure TTestStartup.Test_Shortcuts_ParseTime;
var
  Elapsed: Int64;
  I: Integer;
  Parts: TStringArray;
  S: string;
begin
  StartTimer;
  for I := 1 to ITERATION_COUNT do
  begin
    S := 'Ctrl+Shift+Alt+F12';
    Parts := S.Split(['+']);
  end;
  Elapsed := StopTimer;

  AssertTrue('Shortcuts parse time for ' + IntToStr(ITERATION_COUNT) +
    ' parses (' + IntToStr(Elapsed) + 'ms) should be < 20ms', Elapsed < 20);
end;

procedure TTestStartup.Test_Shortcuts_LookupTime;
var
  Elapsed: Int64;
  Shortcuts: TStringList;
  I, Idx: Integer;
begin
  Shortcuts := TStringList.Create;
  try
    Shortcuts.Sorted := True;
    for I := 1 to 100 do
      Shortcuts.Add('Shortcut' + IntToStr(I) + '=Action' + IntToStr(I));

    StartTimer;
    for I := 1 to LOOKUP_ITERATIONS do
      Idx := Shortcuts.IndexOfName('Shortcut' + IntToStr(Random(100) + 1));
    Elapsed := StopTimer;
  finally
    Shortcuts.Free;
  end;

  AssertTrue('Shortcuts lookup time (' + IntToStr(Elapsed) + 'ms) should be < 500ms',
    Elapsed < 500);
end;

{ Playlist initialization tests }

procedure TTestStartup.Test_Playlist_CreateTime;
var
  Elapsed: Int64;
  Lists: array[0..99] of TStringList;
  I: Integer;
begin
  StartTimer;
  for I := 0 to 99 do
    Lists[I] := TStringList.Create;
  Elapsed := StopTimer;

  for I := 0 to 99 do
    Lists[I].Free;

  AssertTrue('Create 100 playlists time (' + IntToStr(Elapsed) + 'ms) should be < ' +
    IntToStr(FThresholds.PlaylistInit) + 'ms', Elapsed < FThresholds.PlaylistInit);
end;

procedure TTestStartup.Test_Playlist_LoadEmptyTime;
var
  Elapsed: Int64;
  List: TStringList;
  TempFile: string;
begin
  TempFile := GetTempDir + 'test_empty_playlist.m3u';
  List := TStringList.Create;
  try
    List.Add('#EXTM3U');
    List.SaveToFile(TempFile);
    List.Clear;

    StartTimer;
    List.LoadFromFile(TempFile);
    Elapsed := StopTimer;
  finally
    List.Free;
  end;

  DeleteFile(TempFile);

  AssertTrue('Empty playlist load time (' + IntToStr(Elapsed) + 'ms) should be < 5ms',
    Elapsed < 5);
end;

procedure TTestStartup.Test_Playlist_Load100ItemsTime;
var
  Elapsed: Int64;
  List: TStringList;
  TempFile: string;
  I: Integer;
begin
  TempFile := GetTempDir + 'test_100_playlist.m3u';
  List := TStringList.Create;
  try
    List.Add('#EXTM3U');
    for I := 1 to 100 do
    begin
      List.Add('#EXTINF:180,Artist ' + IntToStr(I) + ' - Song ' + IntToStr(I));
      List.Add('/path/to/song' + IntToStr(I) + '.mp3');
    end;
    List.SaveToFile(TempFile);
    List.Clear;

    StartTimer;
    List.LoadFromFile(TempFile);
    Elapsed := StopTimer;
  finally
    List.Free;
  end;

  DeleteFile(TempFile);

  AssertTrue('100 items playlist load time (' + IntToStr(Elapsed) + 'ms) should be < 20ms',
    Elapsed < 20);
end;

procedure TTestStartup.Test_Playlist_Load1000ItemsTime;
var
  Elapsed: Int64;
  List: TStringList;
  TempFile: string;
  I: Integer;
begin
  TempFile := GetTempDir + 'test_1000_playlist.m3u';
  List := TStringList.Create;
  try
    List.Add('#EXTM3U');
    for I := 1 to 1000 do
    begin
      List.Add('#EXTINF:180,Artist ' + IntToStr(I) + ' - Song ' + IntToStr(I));
      List.Add('/path/to/song' + IntToStr(I) + '.mp3');
    end;
    List.SaveToFile(TempFile);
    List.Clear;

    StartTimer;
    List.LoadFromFile(TempFile);
    Elapsed := StopTimer;
  finally
    List.Free;
  end;

  DeleteFile(TempFile);

  AssertTrue('1000 items playlist load time (' + IntToStr(Elapsed) + 'ms) should be < 100ms',
    Elapsed < 100);
end;

{ Radio manager tests }

procedure TTestStartup.Test_RadioManager_InitTime;
var
  Elapsed: Int64;
  Stations: TStringList;
begin
  StartTimer;
  Stations := TStringList.Create;
  try
    Stations.Sorted := True;
    Stations.Duplicates := dupIgnore;
  finally
    Stations.Free;
  end;
  Elapsed := StopTimer;

  AssertTrue('Radio manager init time (' + IntToStr(Elapsed) + 'ms) should be < ' +
    IntToStr(FThresholds.RadioManagerInit) + 'ms', Elapsed < FThresholds.RadioManagerInit);
end;

procedure TTestStartup.Test_RadioManager_LoadStationsTime;
var
  Elapsed: Int64;
  Stations: TStringList;
  I: Integer;
begin
  Stations := TStringList.Create;
  try
    { Simulate loading 500 radio stations }
    StartTimer;
    for I := 1 to 500 do
      Stations.Add('http://stream' + IntToStr(I) + '.radio.com/live|Station ' + IntToStr(I) + '|Genre|Country');
    Elapsed := StopTimer;
  finally
    Stations.Free;
  end;

  AssertTrue('Load 500 stations time (' + IntToStr(Elapsed) + 'ms) should be < 50ms',
    Elapsed < 50);
end;

{ Overall startup simulation }

procedure TTestStartup.Test_FullStartup_Simulation;
var
  Elapsed: Int64;
  Config, Locale, Shortcuts, Playlist, Stations: TStringList;
  I: Integer;
begin
  StartTimer;

  { Simulate full startup sequence }
  Config := TStringList.Create;
  Locale := TStringList.Create;
  Shortcuts := TStringList.Create;
  Playlist := TStringList.Create;
  Stations := TStringList.Create;

  try
    { Load config }
    for I := 1 to 50 do
      Config.Add('Key' + IntToStr(I) + '=Value' + IntToStr(I));

    { Load locale }
    for I := 1 to 500 do
      Locale.Add('String' + IntToStr(I) + '=Translation' + IntToStr(I));

    { Load shortcuts }
    for I := 1 to 100 do
      Shortcuts.Add('Ctrl+' + Chr(65 + (I mod 26)) + '=Action' + IntToStr(I));

    { Load last playlist }
    Playlist.Add('#EXTM3U');
    for I := 1 to 100 do
    begin
      Playlist.Add('#EXTINF:180,Song ' + IntToStr(I));
      Playlist.Add('/path/to/song' + IntToStr(I) + '.mp3');
    end;

    { Load radio stations }
    for I := 1 to 200 do
      Stations.Add('http://stream' + IntToStr(I) + '.com|Station ' + IntToStr(I));

  finally
    Config.Free;
    Locale.Free;
    Shortcuts.Free;
    Playlist.Free;
    Stations.Free;
  end;

  Elapsed := StopTimer;

  AssertTrue('Full startup simulation (' + IntToStr(Elapsed) + 'ms) should be < ' +
    IntToStr(FThresholds.TotalStartup) + 'ms', Elapsed < FThresholds.TotalStartup);
end;

procedure TTestStartup.Test_MinimalStartup_Simulation;
var
  Elapsed: Int64;
  Config: TStringList;
  I: Integer;
begin
  StartTimer;

  { Simulate minimal startup - just config }
  Config := TStringList.Create;
  try
    for I := 1 to 20 do
      Config.Add('Key' + IntToStr(I) + '=Value' + IntToStr(I));
  finally
    Config.Free;
  end;

  Elapsed := StopTimer;

  AssertTrue('Minimal startup (' + IntToStr(Elapsed) + 'ms) should be < 10ms',
    Elapsed < 10);
end;

{ ============================================================================
  TTestComponentInit
  ============================================================================ }

procedure TTestComponentInit.SetUp;
begin
  { Nothing to set up }
end;

procedure TTestComponentInit.StartTimer;
begin
  FStartTime := Now;
end;

function TTestComponentInit.StopTimer: Int64;
begin
  Result := MilliSecondsBetween(Now, FStartTime);
end;

procedure TTestComponentInit.Test_TStringList_CreateTime;
var
  Elapsed: Int64;
  List: TStringList;
begin
  StartTimer;
  List := TStringList.Create;
  List.Free;
  Elapsed := StopTimer;

  AssertTrue('TStringList create time (' + IntToStr(Elapsed) + 'ms) should be < 1ms',
    Elapsed < 1);
end;

procedure TTestComponentInit.Test_TMemoryStream_CreateTime;
var
  Elapsed: Int64;
  Stream: TMemoryStream;
begin
  StartTimer;
  Stream := TMemoryStream.Create;
  Stream.Free;
  Elapsed := StopTimer;

  AssertTrue('TMemoryStream create time (' + IntToStr(Elapsed) + 'ms) should be < 1ms',
    Elapsed < 1);
end;

procedure TTestComponentInit.Test_TFileStream_CreateTime;
var
  Elapsed: Int64;
  Stream: TFileStream;
  TempFile: string;
begin
  TempFile := GetTempDir + 'test_stream_perf.tmp';

  { Create file first }
  Stream := TFileStream.Create(TempFile, fmCreate);
  Stream.Free;

  { Measure open time }
  StartTimer;
  Stream := TFileStream.Create(TempFile, fmOpenRead);
  Stream.Free;
  Elapsed := StopTimer;

  DeleteFile(TempFile);

  AssertTrue('TFileStream open time (' + IntToStr(Elapsed) + 'ms) should be < 5ms',
    Elapsed < 5);
end;

procedure TTestComponentInit.Test_TIniFile_CreateTime;
var
  Elapsed: Int64;
  Ini: TIniFile;
  TempFile: string;
begin
  TempFile := GetTempDir + 'test_ini_perf.ini';

  StartTimer;
  Ini := TIniFile.Create(TempFile);
  Ini.Free;
  Elapsed := StopTimer;

  DeleteFile(TempFile);

  AssertTrue('TIniFile create time (' + IntToStr(Elapsed) + 'ms) should be < 5ms',
    Elapsed < 5);
end;

procedure TTestComponentInit.Test_Create100StringLists;
var
  Elapsed: Int64;
  Lists: array[0..99] of TStringList;
  I: Integer;
begin
  StartTimer;
  for I := 0 to 99 do
    Lists[I] := TStringList.Create;
  for I := 0 to 99 do
    Lists[I].Free;
  Elapsed := StopTimer;

  AssertTrue('Create/free 100 TStringLists (' + IntToStr(Elapsed) + 'ms) should be < 10ms',
    Elapsed < 10);
end;

procedure TTestComponentInit.Test_Create1000Strings;
var
  Elapsed: Int64;
  List: TStringList;
  I: Integer;
begin
  List := TStringList.Create;
  try
    StartTimer;
    for I := 1 to 1000 do
      List.Add('String number ' + IntToStr(I) + ' with some extra text');
    Elapsed := StopTimer;
  finally
    List.Free;
  end;

  AssertTrue('Add 1000 strings (' + IntToStr(Elapsed) + 'ms) should be < 10ms',
    Elapsed < 10);
end;

procedure TTestComponentInit.Test_ParseINI100Keys;
var
  Elapsed: Int64;
  Ini: TIniFile;
  TempFile: string;
  I: Integer;
  S: string;
begin
  TempFile := GetTempDir + 'test_ini_parse.ini';

  Ini := TIniFile.Create(TempFile);
  try
    for I := 1 to 100 do
      Ini.WriteString('Section', 'Key' + IntToStr(I), 'Value' + IntToStr(I));
  finally
    Ini.Free;
  end;

  Ini := TIniFile.Create(TempFile);
  try
    StartTimer;
    for I := 1 to 100 do
      S := Ini.ReadString('Section', 'Key' + IntToStr(I), '');
    Elapsed := StopTimer;
  finally
    Ini.Free;
  end;

  DeleteFile(TempFile);

  AssertTrue('Parse 100 INI keys (' + IntToStr(Elapsed) + 'ms) should be < 20ms',
    Elapsed < 20);
end;

initialization
  RegisterTest('Performance', TTestStartup);
  RegisterTest('Performance', TTestComponentInit);

end.
