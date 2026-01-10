{ ═══════════════════════════════════════════════════════════════════════════════
  uConfig.pas - Configuration Management

  Part of 3nity Media - Lazarus Edition

  This unit handles loading and saving application settings to INI file.
  Based on the original Config.pas from 3nity Media.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Graphics, Forms,
  uTypes, uConstants, uMPVConst;

type
  { ═══════════════════════════════════════════════════════════════════════════
    CONFIGURATION MANAGER
    ═══════════════════════════════════════════════════════════════════════════ }
  TConfigManager = class
  private
    FIniFile: TIniFile;
    FConfigPath: string;
    FSettings: TAppSettings;
    FModified: Boolean;

    function GetConfigFilePath: string;
    procedure SetDefaults;

    { Section loaders }
    procedure LoadGeneralSection;
    procedure LoadVideoSection;
    procedure LoadAudioSection;
    procedure LoadSubtitleSection;
    procedure LoadCacheSection;
    procedure LoadWindowSection;
    procedure LoadPlaylistSection;
    procedure LoadEqualizerSection;

    { Section savers }
    procedure SaveGeneralSection;
    procedure SaveVideoSection;
    procedure SaveAudioSection;
    procedure SaveSubtitleSection;
    procedure SaveCacheSection;
    procedure SaveWindowSection;
    procedure SavePlaylistSection;
    procedure SaveEqualizerSection;

  public
    constructor Create;
    destructor Destroy; override;

    { Load/Save all settings }
    procedure Load;
    procedure Save;

    { Access to settings }
    property Settings: TAppSettings read FSettings write FSettings;
    property Modified: Boolean read FModified write FModified;
    property ConfigPath: string read FConfigPath;

    { Convenience access }
    function GetGeneral: TGeneralSettings;
    function GetVideo: TVideoSettings;
    function GetAudio: TAudioSettings;
    function GetSubtitles: TSubtitleSettings;
    function GetCache: TCacheSettings;

    procedure SetGeneral(const Value: TGeneralSettings);
    procedure SetVideo(const Value: TVideoSettings);
    procedure SetAudio(const Value: TAudioSettings);
    procedure SetSubtitles(const Value: TSubtitleSettings);
    procedure SetCache(const Value: TCacheSettings);
    procedure SetPlaybackMode(Value: TPlaybackMode);

    { Window state }
    procedure SaveWindowState(const Section: string; AForm: TForm);
    procedure SaveWindowStateBounds(const Section: string; const Bounds: TRect; State: TWindowState);
    procedure LoadWindowState(const Section: string; AForm: TForm);
    function HasWindowState(const Section: string): Boolean;

    { History }
    procedure AddToHistory(const FileName, Title: string; Position, Duration: Double);
    function GetHistory: THistoryItems;
    procedure ClearHistory;

    { Recent files }
    procedure AddRecentFile(const FileName: string);
    function GetRecentFiles: TStringList;
    procedure RemoveRecentFile(const FileName: string);
    procedure ClearRecentFiles;

    { Playback position (for session restore) }
    procedure SavePlaybackPosition(const FileName: string; Position: Double);
    function GetPlaybackPosition(const FileName: string): Double;
    procedure ClearPlaybackPositions;

    { Bookmarks }
    procedure AddBookmark(const FileName, Name: string; Position: Double);
    function GetBookmarks: TBookmarkItems;
    function GetBookmarksForFile(const FileName: string): TBookmarkItems;
    procedure RemoveBookmark(const FileName: string; Position: Double);
    procedure ClearBookmarks;
    procedure ClearBookmarksForFile(const FileName: string);

    { Favorites }
    procedure AddFavorite(const Name, Path: string; FavType: TFavoriteType; const Category: string = '');
    function GetFavorites: TFavoriteItems;
    function GetFavoritesByCategory(const Category: string): TFavoriteItems;
    function GetFavoritesByType(FavType: TFavoriteType): TFavoriteItems;
    function IsFavorite(const Path: string): Boolean;
    procedure RemoveFavorite(const Path: string);
    procedure UpdateFavoriteLastPlayed(const Path: string);
    procedure ClearFavorites;
    function GetFavoriteCategories: TStringList;

    { Phase 24: Session Playlist Save/Restore }
    procedure SaveSessionPlaylist(const Items: TPlaylistItems; CurrentIndex: Integer;
      PlaybackMode: TPlaybackMode; CurrentPosition: Double);
    function LoadSessionPlaylist(out Items: TPlaylistItems; out CurrentIndex: Integer;
      out PlaybackMode: TPlaybackMode; out CurrentPosition: Double): Boolean;
    procedure ClearSessionPlaylist;
  end;

var
  Config: TConfigManager;

{ Application directories }
function GetAppDataDir: string;
function GetTempDir: string;
function GetConfigDir: string;
function GetUserPicturesDir: string;

implementation

uses
  LCLType, FileUtil;

{ ═══════════════════════════════════════════════════════════════════════════════
  HELPER FUNCTIONS
  ═══════════════════════════════════════════════════════════════════════════════ }

function GetAppDataDir: string;
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA') + PathDelim + '3nity-media' + PathDelim;
  {$ELSE}
  Result := GetEnvironmentVariable('HOME') + PathDelim + '.config' + PathDelim + '3nity-media' + PathDelim;
  {$ENDIF}
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function GetTempDir: string;
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('TEMP') + PathDelim;
  {$ELSE}
  Result := '/tmp/';
  {$ENDIF}
end;

function GetConfigDir: string;
begin
  Result := GetAppDataDir;
end;

function GetUserPicturesDir: string;
{$IFDEF UNIX}
var
  XDGConfig: string;
  Lines: TStringList;
  I: Integer;
  Line, Value: string;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('USERPROFILE') + PathDelim + 'Pictures' + PathDelim;
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := GetEnvironmentVariable('HOME') + PathDelim + 'Pictures' + PathDelim;
  {$ENDIF}
  {$IFDEF LINUX}
  Result := '';
  { Read XDG user-dirs.dirs to get the actual Pictures directory }
  XDGConfig := GetEnvironmentVariable('HOME') + PathDelim + '.config' + PathDelim + 'user-dirs.dirs';
  if FileExists(XDGConfig) then
  begin
    Lines := TStringList.Create;
    try
      Lines.LoadFromFile(XDGConfig);
      for I := 0 to Lines.Count - 1 do
      begin
        Line := Trim(Lines[I]);
        if Pos('XDG_PICTURES_DIR=', Line) = 1 then
        begin
          Value := Copy(Line, 18, Length(Line));
          { Remove quotes }
          Value := StringReplace(Value, '"', '', [rfReplaceAll]);
          { Replace $HOME with actual home path }
          Value := StringReplace(Value, '$HOME', GetEnvironmentVariable('HOME'), [rfReplaceAll]);
          if DirectoryExists(Value) then
            Result := IncludeTrailingPathDelimiter(Value);
          Break;
        end;
      end;
    finally
      Lines.Free;
    end;
  end;
  { Fallback if XDG not found }
  if Result = '' then
    Result := GetEnvironmentVariable('HOME') + PathDelim + 'Pictures' + PathDelim;
  {$ENDIF}
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TConfigManager
  ═══════════════════════════════════════════════════════════════════════════════ }

constructor TConfigManager.Create;
begin
  inherited Create;
  FConfigPath := GetConfigFilePath;
  FModified := False;
  SetDefaults;
end;

destructor TConfigManager.Destroy;
begin
  if FModified then
    Save;
  FreeAndNil(FIniFile);
  inherited;
end;

function TConfigManager.GetConfigFilePath: string;
begin
  Result := GetAppDataDir + CONFIG_FILENAME;
end;

procedure TConfigManager.SetDefaults;
var
  I: Integer;
begin
  FillChar(FSettings, SizeOf(FSettings), 0);

  { General defaults }
  with FSettings.General do
  begin
    Language := '';  { Empty = auto-detect OS language }
    SingleInstance := True;
    ScreenshotPath := GetUserPicturesDir + '3nity' + PathDelim;
    ScreenshotFormat := SCREENSHOT_PNG;
    HistoryEnabled := True;
    HistoryMaxItems := MAX_HISTORY_ITEMS;
    AutoSavePlaylist := True;  { Phase 24 }
  end;

  { Video defaults }
  with FSettings.Video do
  begin
    Brightness := VIDEO_PROP_DEFAULT;
    Contrast := VIDEO_PROP_DEFAULT;
    Saturation := VIDEO_PROP_DEFAULT;
    Hue := VIDEO_PROP_DEFAULT;
    Gamma := VIDEO_PROP_DEFAULT;
    AspectMode := ASPECT_AUTO;
    AspectFactor := -1;
    Deinterlace := DEINT_AUTO;
    DeinterlaceAlg := DEINT_ALG_AUTO;
    VideoOutput := VO_DEFAULT;
    HWAccel := True;
  end;

  { Audio defaults }
  with FSettings.Audio do
  begin
    Volume := VOL_DEFAULT;
    Muted := False;
    AudioOutput := AO_DEFAULT;
    AudioDevice := '';
    Channels := 2;
    Normalize := False;
  end;

  { Subtitle defaults }
  with FSettings.Subtitles do
  begin
    UseDefault := True;      { Use mpv defaults by default }
    FontName := 'Arial';
    FontSize := 24;
    FontColor := clWhite;
    FontBold := False;
    FontItalic := False;
    OutlineColor := clBlack;
    OutlineSize := 2;
    BackgroundColor := clBlack;
    BackgroundOpacity := 0;
    Position := 95;
    Encoding := 'UTF-8';
    AutoLoad := True;
  end;

  { Cache defaults }
  with FSettings.Cache do
  begin
    DefaultSize := DEFAULT_CACHE_DEFAULT;
    FixedSize := DEFAULT_CACHE_FIXED;
    RamdiskSize := DEFAULT_CACHE_RAMDISK;
    CDROMSize := DEFAULT_CACHE_CDROM;
    RemovableSize := DEFAULT_CACHE_REMOVABLE;
    NetworkSize := DEFAULT_CACHE_NETWORK;
    InternetSize := DEFAULT_CACHE_INTERNET;
    DVDSize := DEFAULT_CACHE_DVD;
  end;

  { Playback mode }
  FSettings.PlaybackMode := pmNormal;

  { Equalizer defaults }
  for I := 0 to 9 do
    FSettings.EqualizerBands[I] := 0;
  FSettings.EqualizerEnabled := False;
end;

procedure TConfigManager.Load;
begin
  if not FileExists(FConfigPath) then
  begin
    SetDefaults;
    Save;
    Exit;
  end;

  try
    FIniFile := TIniFile.Create(FConfigPath);
    try
      LoadGeneralSection;
      LoadVideoSection;
      LoadAudioSection;
      LoadSubtitleSection;
      LoadCacheSection;
      LoadWindowSection;
      LoadPlaylistSection;
      LoadEqualizerSection;
    finally
      FreeAndNil(FIniFile);
    end;
  except
    SetDefaults;
  end;

  FModified := False;
end;

procedure TConfigManager.Save;
begin
  try
    FIniFile := TIniFile.Create(FConfigPath);
    try
      SaveGeneralSection;
      SaveVideoSection;
      SaveAudioSection;
      SaveSubtitleSection;
      SaveCacheSection;
      SaveWindowSection;
      SavePlaylistSection;
      SaveEqualizerSection;
      FIniFile.UpdateFile;
    finally
      FreeAndNil(FIniFile);
    end;
  except
    on E: Exception do
      { Log error silently }
  end;

  FModified := False;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  SECTION LOADERS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TConfigManager.LoadGeneralSection;
begin
  with FSettings.General do
  begin
    Language := FIniFile.ReadString(INI_SECTION_GENERAL, 'Language', '');  { Empty = auto-detect }
    SingleInstance := FIniFile.ReadBool(INI_SECTION_GENERAL, 'SingleInstance', True);
    ScreenshotPath := FIniFile.ReadString(INI_SECTION_GENERAL, 'ScreenshotPath',
      GetUserPicturesDir + '3nity' + PathDelim);
    ScreenshotFormat := FIniFile.ReadString(INI_SECTION_GENERAL, 'ScreenshotFormat', SCREENSHOT_PNG);
    HistoryEnabled := FIniFile.ReadBool(INI_SECTION_GENERAL, 'HistoryEnabled', True);
    HistoryMaxItems := FIniFile.ReadInteger(INI_SECTION_GENERAL, 'HistoryMaxItems', MAX_HISTORY_ITEMS);
    AutoSavePlaylist := FIniFile.ReadBool(INI_SECTION_GENERAL, 'AutoSavePlaylist', True);  { Phase 24 }
  end;
end;

procedure TConfigManager.LoadVideoSection;
begin
  with FSettings.Video do
  begin
    Brightness := FIniFile.ReadInteger(INI_SECTION_VIDEO, 'Brightness', VIDEO_PROP_DEFAULT);
    Contrast := FIniFile.ReadInteger(INI_SECTION_VIDEO, 'Contrast', VIDEO_PROP_DEFAULT);
    Saturation := FIniFile.ReadInteger(INI_SECTION_VIDEO, 'Saturation', VIDEO_PROP_DEFAULT);
    Hue := FIniFile.ReadInteger(INI_SECTION_VIDEO, 'Hue', VIDEO_PROP_DEFAULT);
    Gamma := FIniFile.ReadInteger(INI_SECTION_VIDEO, 'Gamma', VIDEO_PROP_DEFAULT);
    AspectMode := FIniFile.ReadInteger(INI_SECTION_VIDEO, 'AspectMode', ASPECT_AUTO);
    AspectFactor := FIniFile.ReadFloat(INI_SECTION_VIDEO, 'AspectFactor', -1);
    Deinterlace := FIniFile.ReadInteger(INI_SECTION_VIDEO, 'Deinterlace', DEINT_AUTO);
    DeinterlaceAlg := FIniFile.ReadInteger(INI_SECTION_VIDEO, 'DeinterlaceAlg', DEINT_ALG_AUTO);
    VideoOutput := FIniFile.ReadString(INI_SECTION_VIDEO, 'VideoOutput', VO_DEFAULT);
    HWAccel := FIniFile.ReadBool(INI_SECTION_VIDEO, 'HWAccel', True);
  end;
end;

procedure TConfigManager.LoadAudioSection;
begin
  with FSettings.Audio do
  begin
    Volume := FIniFile.ReadInteger(INI_SECTION_AUDIO, 'Volume', VOL_DEFAULT);
    Muted := FIniFile.ReadBool(INI_SECTION_AUDIO, 'Muted', False);
    AudioOutput := FIniFile.ReadString(INI_SECTION_AUDIO, 'AudioOutput', AO_DEFAULT);
    AudioDevice := FIniFile.ReadString(INI_SECTION_AUDIO, 'AudioDevice', '');
    Channels := FIniFile.ReadInteger(INI_SECTION_AUDIO, 'Channels', 2);
    Normalize := FIniFile.ReadBool(INI_SECTION_AUDIO, 'Normalize', False);
  end;
end;

procedure TConfigManager.LoadSubtitleSection;
begin
  with FSettings.Subtitles do
  begin
    UseDefault := FIniFile.ReadBool(INI_SECTION_SUBTITLES, 'UseDefault', True);
    FontName := FIniFile.ReadString(INI_SECTION_SUBTITLES, 'FontName', 'Arial');
    FontSize := FIniFile.ReadInteger(INI_SECTION_SUBTITLES, 'FontSize', 24);
    FontColor := TColor(FIniFile.ReadInteger(INI_SECTION_SUBTITLES, 'FontColor', Integer(clWhite)));
    FontBold := FIniFile.ReadBool(INI_SECTION_SUBTITLES, 'FontBold', False);
    FontItalic := FIniFile.ReadBool(INI_SECTION_SUBTITLES, 'FontItalic', False);
    OutlineColor := TColor(FIniFile.ReadInteger(INI_SECTION_SUBTITLES, 'OutlineColor', Integer(clBlack)));
    OutlineSize := FIniFile.ReadInteger(INI_SECTION_SUBTITLES, 'OutlineSize', 2);
    BackgroundColor := TColor(FIniFile.ReadInteger(INI_SECTION_SUBTITLES, 'BackgroundColor', Integer(clBlack)));
    BackgroundOpacity := FIniFile.ReadInteger(INI_SECTION_SUBTITLES, 'BackgroundOpacity', 0);
    Position := FIniFile.ReadInteger(INI_SECTION_SUBTITLES, 'Position', 95);
    Encoding := FIniFile.ReadString(INI_SECTION_SUBTITLES, 'Encoding', 'UTF-8');
    AutoLoad := FIniFile.ReadBool(INI_SECTION_SUBTITLES, 'AutoLoad', True);
  end;
end;

procedure TConfigManager.LoadCacheSection;
begin
  with FSettings.Cache do
  begin
    DefaultSize := FIniFile.ReadInteger(INI_SECTION_CACHE, 'DefaultSize', DEFAULT_CACHE_DEFAULT);
    FixedSize := FIniFile.ReadInteger(INI_SECTION_CACHE, 'FixedSize', DEFAULT_CACHE_FIXED);
    RamdiskSize := FIniFile.ReadInteger(INI_SECTION_CACHE, 'RamdiskSize', DEFAULT_CACHE_RAMDISK);
    CDROMSize := FIniFile.ReadInteger(INI_SECTION_CACHE, 'CDROMSize', DEFAULT_CACHE_CDROM);
    RemovableSize := FIniFile.ReadInteger(INI_SECTION_CACHE, 'RemovableSize', DEFAULT_CACHE_REMOVABLE);
    NetworkSize := FIniFile.ReadInteger(INI_SECTION_CACHE, 'NetworkSize', DEFAULT_CACHE_NETWORK);
    InternetSize := FIniFile.ReadInteger(INI_SECTION_CACHE, 'InternetSize', DEFAULT_CACHE_INTERNET);
    DVDSize := FIniFile.ReadInteger(INI_SECTION_CACHE, 'DVDSize', DEFAULT_CACHE_DVD);
  end;
end;

procedure TConfigManager.LoadWindowSection;
begin
  { Window state is handled by LoadWindowState/SaveWindowState }
end;

procedure TConfigManager.LoadPlaylistSection;
var
  ModeInt: Integer;
begin
  ModeInt := FIniFile.ReadInteger(INI_SECTION_PLAYLIST, 'PlaybackMode', Ord(pmNormal));
  if (ModeInt >= Ord(Low(TPlaybackMode))) and (ModeInt <= Ord(High(TPlaybackMode))) then
    FSettings.PlaybackMode := TPlaybackMode(ModeInt)
  else
    FSettings.PlaybackMode := pmNormal;
end;

procedure TConfigManager.LoadEqualizerSection;
var
  I: Integer;
begin
  FSettings.EqualizerEnabled := FIniFile.ReadBool(INI_SECTION_EQUALIZER, 'Enabled', False);
  for I := 0 to 9 do
    FSettings.EqualizerBands[I] := FIniFile.ReadFloat(INI_SECTION_EQUALIZER,
      'Band' + IntToStr(I), 0);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  SECTION SAVERS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TConfigManager.SaveGeneralSection;
begin
  with FSettings.General do
  begin
    FIniFile.WriteString(INI_SECTION_GENERAL, 'Language', Language);
    FIniFile.WriteBool(INI_SECTION_GENERAL, 'SingleInstance', SingleInstance);
    FIniFile.WriteString(INI_SECTION_GENERAL, 'ScreenshotPath', ScreenshotPath);
    FIniFile.WriteString(INI_SECTION_GENERAL, 'ScreenshotFormat', ScreenshotFormat);
    FIniFile.WriteBool(INI_SECTION_GENERAL, 'HistoryEnabled', HistoryEnabled);
    FIniFile.WriteInteger(INI_SECTION_GENERAL, 'HistoryMaxItems', HistoryMaxItems);
    FIniFile.WriteBool(INI_SECTION_GENERAL, 'AutoSavePlaylist', AutoSavePlaylist);  { Phase 24 }
  end;
end;

procedure TConfigManager.SaveVideoSection;
begin
  with FSettings.Video do
  begin
    FIniFile.WriteInteger(INI_SECTION_VIDEO, 'Brightness', Brightness);
    FIniFile.WriteInteger(INI_SECTION_VIDEO, 'Contrast', Contrast);
    FIniFile.WriteInteger(INI_SECTION_VIDEO, 'Saturation', Saturation);
    FIniFile.WriteInteger(INI_SECTION_VIDEO, 'Hue', Hue);
    FIniFile.WriteInteger(INI_SECTION_VIDEO, 'Gamma', Gamma);
    FIniFile.WriteInteger(INI_SECTION_VIDEO, 'AspectMode', AspectMode);
    FIniFile.WriteFloat(INI_SECTION_VIDEO, 'AspectFactor', AspectFactor);
    FIniFile.WriteInteger(INI_SECTION_VIDEO, 'Deinterlace', Deinterlace);
    FIniFile.WriteInteger(INI_SECTION_VIDEO, 'DeinterlaceAlg', DeinterlaceAlg);
    FIniFile.WriteString(INI_SECTION_VIDEO, 'VideoOutput', VideoOutput);
    FIniFile.WriteBool(INI_SECTION_VIDEO, 'HWAccel', HWAccel);
  end;
end;

procedure TConfigManager.SaveAudioSection;
begin
  with FSettings.Audio do
  begin
    FIniFile.WriteInteger(INI_SECTION_AUDIO, 'Volume', Volume);
    FIniFile.WriteBool(INI_SECTION_AUDIO, 'Muted', Muted);
    FIniFile.WriteString(INI_SECTION_AUDIO, 'AudioOutput', AudioOutput);
    FIniFile.WriteString(INI_SECTION_AUDIO, 'AudioDevice', AudioDevice);
    FIniFile.WriteInteger(INI_SECTION_AUDIO, 'Channels', Channels);
    FIniFile.WriteBool(INI_SECTION_AUDIO, 'Normalize', Normalize);
  end;
end;

procedure TConfigManager.SaveSubtitleSection;
begin
  with FSettings.Subtitles do
  begin
    FIniFile.WriteBool(INI_SECTION_SUBTITLES, 'UseDefault', UseDefault);
    FIniFile.WriteString(INI_SECTION_SUBTITLES, 'FontName', FontName);
    FIniFile.WriteInteger(INI_SECTION_SUBTITLES, 'FontSize', FontSize);
    FIniFile.WriteInteger(INI_SECTION_SUBTITLES, 'FontColor', Integer(FontColor));
    FIniFile.WriteBool(INI_SECTION_SUBTITLES, 'FontBold', FontBold);
    FIniFile.WriteBool(INI_SECTION_SUBTITLES, 'FontItalic', FontItalic);
    FIniFile.WriteInteger(INI_SECTION_SUBTITLES, 'OutlineColor', Integer(OutlineColor));
    FIniFile.WriteInteger(INI_SECTION_SUBTITLES, 'OutlineSize', OutlineSize);
    FIniFile.WriteInteger(INI_SECTION_SUBTITLES, 'BackgroundColor', Integer(BackgroundColor));
    FIniFile.WriteInteger(INI_SECTION_SUBTITLES, 'BackgroundOpacity', BackgroundOpacity);
    FIniFile.WriteInteger(INI_SECTION_SUBTITLES, 'Position', Position);
    FIniFile.WriteString(INI_SECTION_SUBTITLES, 'Encoding', Encoding);
    FIniFile.WriteBool(INI_SECTION_SUBTITLES, 'AutoLoad', AutoLoad);
  end;
end;

procedure TConfigManager.SaveCacheSection;
begin
  with FSettings.Cache do
  begin
    FIniFile.WriteInteger(INI_SECTION_CACHE, 'DefaultSize', DefaultSize);
    FIniFile.WriteInteger(INI_SECTION_CACHE, 'FixedSize', FixedSize);
    FIniFile.WriteInteger(INI_SECTION_CACHE, 'RamdiskSize', RamdiskSize);
    FIniFile.WriteInteger(INI_SECTION_CACHE, 'CDROMSize', CDROMSize);
    FIniFile.WriteInteger(INI_SECTION_CACHE, 'RemovableSize', RemovableSize);
    FIniFile.WriteInteger(INI_SECTION_CACHE, 'NetworkSize', NetworkSize);
    FIniFile.WriteInteger(INI_SECTION_CACHE, 'InternetSize', InternetSize);
    FIniFile.WriteInteger(INI_SECTION_CACHE, 'DVDSize', DVDSize);
  end;
end;

procedure TConfigManager.SaveWindowSection;
begin
  { Window state is handled by LoadWindowState/SaveWindowState }
end;

procedure TConfigManager.SavePlaylistSection;
begin
  FIniFile.WriteInteger(INI_SECTION_PLAYLIST, 'PlaybackMode', Ord(FSettings.PlaybackMode));
end;

procedure TConfigManager.SaveEqualizerSection;
var
  I: Integer;
begin
  FIniFile.WriteBool(INI_SECTION_EQUALIZER, 'Enabled', FSettings.EqualizerEnabled);
  for I := 0 to 9 do
    FIniFile.WriteFloat(INI_SECTION_EQUALIZER, 'Band' + IntToStr(I),
      FSettings.EqualizerBands[I]);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  CONVENIENCE ACCESSORS
  ═══════════════════════════════════════════════════════════════════════════════ }

function TConfigManager.GetGeneral: TGeneralSettings;
begin
  Result := FSettings.General;
end;

function TConfigManager.GetVideo: TVideoSettings;
begin
  Result := FSettings.Video;
end;

function TConfigManager.GetAudio: TAudioSettings;
begin
  Result := FSettings.Audio;
end;

function TConfigManager.GetSubtitles: TSubtitleSettings;
begin
  Result := FSettings.Subtitles;
end;

function TConfigManager.GetCache: TCacheSettings;
begin
  Result := FSettings.Cache;
end;

procedure TConfigManager.SetGeneral(const Value: TGeneralSettings);
begin
  FSettings.General := Value;
  FModified := True;
end;

procedure TConfigManager.SetVideo(const Value: TVideoSettings);
begin
  FSettings.Video := Value;
  FModified := True;
end;

procedure TConfigManager.SetAudio(const Value: TAudioSettings);
begin
  FSettings.Audio := Value;
  FModified := True;
end;

procedure TConfigManager.SetSubtitles(const Value: TSubtitleSettings);
begin
  FSettings.Subtitles := Value;
  FModified := True;
end;

procedure TConfigManager.SetCache(const Value: TCacheSettings);
begin
  FSettings.Cache := Value;
  FModified := True;
end;

procedure TConfigManager.SetPlaybackMode(Value: TPlaybackMode);
begin
  FSettings.PlaybackMode := Value;
  FModified := True;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  WINDOW STATE
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TConfigManager.SaveWindowState(const Section: string; AForm: TForm);
begin
  if AForm = nil then Exit;

  try
    FIniFile := TIniFile.Create(FConfigPath);
    try
      if AForm.WindowState = wsMaximized then
      begin
        FIniFile.WriteBool(Section, 'Maximized', True);
      end
      else
      begin
        FIniFile.WriteBool(Section, 'Maximized', False);
        FIniFile.WriteInteger(Section, 'Left', AForm.Left);
        FIniFile.WriteInteger(Section, 'Top', AForm.Top);
        { Use Width/Height for consistent save/restore }
        FIniFile.WriteInteger(Section, 'Width', AForm.Width);
        FIniFile.WriteInteger(Section, 'Height', AForm.Height);
      end;
      FIniFile.UpdateFile;
    finally
      FreeAndNil(FIniFile);
    end;
  except
  end;
end;

procedure TConfigManager.SaveWindowStateBounds(const Section: string; const Bounds: TRect; State: TWindowState);
var
  W, H: Integer;
begin
  W := Bounds.Right - Bounds.Left;
  H := Bounds.Bottom - Bounds.Top;

  try
    FIniFile := TIniFile.Create(FConfigPath);
    try
      if State = wsMaximized then
      begin
        FIniFile.WriteBool(Section, 'Maximized', True);
      end
      else
      begin
        FIniFile.WriteBool(Section, 'Maximized', False);
        FIniFile.WriteInteger(Section, 'Left', Bounds.Left);
        FIniFile.WriteInteger(Section, 'Top', Bounds.Top);
        FIniFile.WriteInteger(Section, 'Width', W);
        FIniFile.WriteInteger(Section, 'Height', H);
      end;
      FIniFile.UpdateFile;
    finally
      FreeAndNil(FIniFile);
    end;
  except
  end;
end;

procedure TConfigManager.LoadWindowState(const Section: string; AForm: TForm);
var
  L, T, W, H: Integer;
  Max: Boolean;
begin
  if AForm = nil then Exit;

  try
    FIniFile := TIniFile.Create(FConfigPath);
    try
      Max := FIniFile.ReadBool(Section, 'Maximized', False);
      L := FIniFile.ReadInteger(Section, 'Left', AForm.Left);
      T := FIniFile.ReadInteger(Section, 'Top', AForm.Top);
      { Use Width/Height for consistent save/restore }
      W := FIniFile.ReadInteger(Section, 'Width', AForm.Width);
      H := FIniFile.ReadInteger(Section, 'Height', AForm.Height);

      AForm.SetBounds(L, T, W, H);

      if Max then
        AForm.WindowState := wsMaximized;
    finally
      FreeAndNil(FIniFile);
    end;
  except
  end;
end;

function TConfigManager.HasWindowState(const Section: string): Boolean;
begin
  Result := False;
  if not FileExists(FConfigPath) then Exit;

  try
    FIniFile := TIniFile.Create(FConfigPath);
    try
      Result := FIniFile.SectionExists(Section);
    finally
      FreeAndNil(FIniFile);
    end;
  except
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  HISTORY
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TConfigManager.AddToHistory(const FileName, Title: string;
  Position, Duration: Double);
var
  Ini: TIniFile;
  Count, I, MaxItems, FoundIdx: Integer;
  Section: string;
begin
  if not FSettings.General.HistoryEnabled then Exit;

  try
    Ini := TIniFile.Create(GetAppDataDir + HISTORY_FILENAME);
    try
      MaxItems := FSettings.General.HistoryMaxItems;
      Count := Ini.ReadInteger(INI_SECTION_HISTORY, 'Count', 0);

      { Check if already exists }
      FoundIdx := -1;
      for I := 0 to Count - 1 do
      begin
        Section := 'Item' + IntToStr(I);
        if Ini.ReadString(Section, 'FileName', '') = FileName then
        begin
          FoundIdx := I;
          Break;
        end;
      end;

      { Update existing or add new }
      if FoundIdx >= 0 then
        Section := 'Item' + IntToStr(FoundIdx)
      else
      begin
        { Shift items if at max }
        if Count >= MaxItems then
        begin
          for I := 0 to Count - 2 do
          begin
            { Shift items down }
          end;
          Count := MaxItems - 1;
        end;
        Section := 'Item' + IntToStr(Count);
        Inc(Count);
      end;

      Ini.WriteString(Section, 'FileName', FileName);
      Ini.WriteString(Section, 'Title', Title);
      Ini.WriteFloat(Section, 'Position', Position);
      Ini.WriteFloat(Section, 'Duration', Duration);
      Ini.WriteDateTime(Section, 'LastPlayed', Now);

      Ini.WriteInteger(INI_SECTION_HISTORY, 'Count', Count);
      Ini.UpdateFile;
    finally
      Ini.Free;
    end;
  except
  end;
end;

function TConfigManager.GetHistory: THistoryItems;
var
  Ini: TIniFile;
  Count, I: Integer;
  Section: string;
begin
  Result := nil;

  try
    if not FileExists(GetAppDataDir + HISTORY_FILENAME) then Exit;

    Ini := TIniFile.Create(GetAppDataDir + HISTORY_FILENAME);
    try
      Count := Ini.ReadInteger(INI_SECTION_HISTORY, 'Count', 0);
      SetLength(Result, Count);

      for I := 0 to Count - 1 do
      begin
        Section := 'Item' + IntToStr(I);
        Result[I].FileName := Ini.ReadString(Section, 'FileName', '');
        Result[I].Title := Ini.ReadString(Section, 'Title', '');
        Result[I].Position := Ini.ReadFloat(Section, 'Position', 0);
        Result[I].Duration := Ini.ReadFloat(Section, 'Duration', 0);
        Result[I].LastPlayed := Ini.ReadDateTime(Section, 'LastPlayed', Now);
        Result[I].PlayCount := Ini.ReadInteger(Section, 'PlayCount', 1);
      end;
    finally
      Ini.Free;
    end;
  except
    SetLength(Result, 0);
  end;
end;

procedure TConfigManager.ClearHistory;
begin
  try
    if FileExists(GetAppDataDir + HISTORY_FILENAME) then
      DeleteFile(GetAppDataDir + HISTORY_FILENAME);
  except
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  RECENT FILES
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TConfigManager.AddRecentFile(const FileName: string);
var
  RecentFiles: TStringList;
  I, Idx: Integer;
begin
  RecentFiles := GetRecentFiles;
  try
    { Remove if already exists }
    Idx := RecentFiles.IndexOf(FileName);
    if Idx >= 0 then
      RecentFiles.Delete(Idx);

    { Insert at beginning }
    RecentFiles.Insert(0, FileName);

    { Trim to max }
    while RecentFiles.Count > MAX_RECENT_FILES do
      RecentFiles.Delete(RecentFiles.Count - 1);

    { Save }
    try
      FIniFile := TIniFile.Create(FConfigPath);
      try
        FIniFile.EraseSection('RecentFiles');
        FIniFile.WriteInteger('RecentFiles', 'Count', RecentFiles.Count);
        for I := 0 to RecentFiles.Count - 1 do
          FIniFile.WriteString('RecentFiles', 'File' + IntToStr(I), RecentFiles[I]);
        FIniFile.UpdateFile;
      finally
        FreeAndNil(FIniFile);
      end;
    except
    end;
  finally
    RecentFiles.Free;
  end;
end;

function TConfigManager.GetRecentFiles: TStringList;
var
  I, Count: Integer;
begin
  Result := TStringList.Create;

  try
    if not FileExists(FConfigPath) then Exit;

    FIniFile := TIniFile.Create(FConfigPath);
    try
      Count := FIniFile.ReadInteger('RecentFiles', 'Count', 0);
      for I := 0 to Count - 1 do
        Result.Add(FIniFile.ReadString('RecentFiles', 'File' + IntToStr(I), ''));
    finally
      FreeAndNil(FIniFile);
    end;
  except
  end;
end;

procedure TConfigManager.ClearRecentFiles;
begin
  try
    FIniFile := TIniFile.Create(FConfigPath);
    try
      FIniFile.EraseSection('RecentFiles');
      FIniFile.UpdateFile;
    finally
      FreeAndNil(FIniFile);
    end;
  except
  end;
end;

procedure TConfigManager.RemoveRecentFile(const FileName: string);
var
  I, J, Count: Integer;
  RecentFiles: TStringList;
begin
  RecentFiles := GetRecentFiles;
  try
    { Find and remove the file }
    for I := 0 to RecentFiles.Count - 1 do
    begin
      if SameText(RecentFiles[I], FileName) then
      begin
        RecentFiles.Delete(I);
        Break;
      end;
    end;

    { Save updated list }
    FIniFile := TIniFile.Create(FConfigPath);
    try
      FIniFile.EraseSection('RecentFiles');
      FIniFile.WriteInteger('RecentFiles', 'Count', RecentFiles.Count);
      for J := 0 to RecentFiles.Count - 1 do
        FIniFile.WriteString('RecentFiles', 'File' + IntToStr(J), RecentFiles[J]);
      FIniFile.UpdateFile;
    finally
      FreeAndNil(FIniFile);
    end;
  finally
    RecentFiles.Free;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PLAYBACK POSITION (SESSION RESTORE)
  ═══════════════════════════════════════════════════════════════════════════════ }

{ ═══════════════════════════════════════════════════════════════════════════
  StringCRC32 - Calculate a CRC32 hash for a string

  Purpose: Generates a 32-bit hash value for use as an INI file key.
           Used to create safe, fixed-length identifiers from filenames
           that may contain special characters or be very long.

  Algorithm:
    - Polynomial: $1021 (CRC-CCITT variant)
    - Initial value: $FFFFFFFF
    - Final XOR: invert all bits (not Result)
    - Processes one byte at a time

  Parameters:
    - S: The input string to hash

  Returns: 32-bit hash value (LongWord)

  Notes:
    - This is a simple hash, not cryptographically secure
    - Collision probability is low but not zero for different filenames
    - Result is converted to 8-character hex string for INI keys
  ═══════════════════════════════════════════════════════════════════════════ }
function StringCRC32(const S: string): LongWord;
var
  I: Integer;
begin
  Result := $FFFFFFFF;
  for I := 1 to Length(S) do
    Result := (Result shr 8) xor (((Result xor Ord(S[I])) and $FF) * $1021);
  Result := not Result;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  SavePlaybackPosition - Persist playback position for session restore

  Purpose: Saves the current playback position of a media file to INI storage,
           enabling "resume where you left off" functionality.

  Parameters:
    - FileName: Full path to the media file
    - Position: Current playback position in seconds

  Storage format:
    - Key: 8-character hex hash of filename (via StringCRC32)
    - Section: [PlaybackPositions]
    - Entries: <hash>=<position>, <hash>_file=<filename>

  Notes:
    - Threshold: Positions < 5 seconds are ignored (prevents saving very
      short playback sessions or accidental file opens)
    - Stores original filename alongside hash to verify on retrieval
      (handles hash collisions)
    - Silent failure on I/O errors
  ═══════════════════════════════════════════════════════════════════════════ }
procedure TConfigManager.SavePlaybackPosition(const FileName: string; Position: Double);
var
  Key: string;
begin
  if FileName = '' then Exit;
  if Position < 5 then Exit; { Don't save positions less than 5 seconds }

  { Use MD5-style hash of filename as key to avoid special characters }
  Key := IntToHex(Cardinal(StringCRC32(FileName)), 8);

  try
    FIniFile := TIniFile.Create(FConfigPath);
    try
      FIniFile.WriteFloat('PlaybackPositions', Key, Position);
      FIniFile.WriteString('PlaybackPositions', Key + '_file', FileName);
      FIniFile.UpdateFile;
    finally
      FreeAndNil(FIniFile);
    end;
  except
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  GetPlaybackPosition - Retrieve saved playback position for a file

  Purpose: Looks up a previously saved playback position to enable resuming
           playback from where the user left off.

  Parameters:
    - FileName: Full path to the media file

  Returns: Saved position in seconds, or 0 if not found/error

  Notes:
    - Performs filename verification: even if hash matches, the stored
      filename must exactly match to return a position (prevents false
      positives from hash collisions)
    - Returns 0 if config file doesn't exist or on any I/O error
    - Thread-safe: creates own TIniFile instance
  ═══════════════════════════════════════════════════════════════════════════ }
function TConfigManager.GetPlaybackPosition(const FileName: string): Double;
var
  Key: string;
begin
  Result := 0;
  if FileName = '' then Exit;

  Key := IntToHex(Cardinal(StringCRC32(FileName)), 8);

  try
    if not FileExists(FConfigPath) then Exit;

    FIniFile := TIniFile.Create(FConfigPath);
    try
      { Verify it's the same file }
      if FIniFile.ReadString('PlaybackPositions', Key + '_file', '') = FileName then
        Result := FIniFile.ReadFloat('PlaybackPositions', Key, 0);
    finally
      FreeAndNil(FIniFile);
    end;
  except
    Result := 0;
  end;
end;

procedure TConfigManager.ClearPlaybackPositions;
begin
  try
    FIniFile := TIniFile.Create(FConfigPath);
    try
      FIniFile.EraseSection('PlaybackPositions');
      FIniFile.UpdateFile;
    finally
      FreeAndNil(FIniFile);
    end;
  except
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  BOOKMARKS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TConfigManager.AddBookmark(const FileName, Name: string; Position: Double);
var
  Ini: TIniFile;
  Count, I: Integer;
  Section: string;
begin
  try
    Ini := TIniFile.Create(GetAppDataDir + BOOKMARKS_FILENAME);
    try
      Count := Ini.ReadInteger('Bookmarks', 'Count', 0);

      { Check for duplicate (same file + same position within 1 second) }
      for I := 0 to Count - 1 do
      begin
        Section := 'Bookmark' + IntToStr(I);
        if (Ini.ReadString(Section, 'FileName', '') = FileName) and
           (Abs(Ini.ReadFloat(Section, 'Position', 0) - Position) < 1.0) then
        begin
          { Update existing bookmark name }
          Ini.WriteString(Section, 'Name', Name);
          Ini.UpdateFile;
          Exit;
        end;
      end;

      { Add new bookmark }
      Section := 'Bookmark' + IntToStr(Count);
      Ini.WriteString(Section, 'FileName', FileName);
      Ini.WriteString(Section, 'Name', Name);
      Ini.WriteFloat(Section, 'Position', Position);
      Ini.WriteDateTime(Section, 'CreatedAt', Now);

      Inc(Count);
      Ini.WriteInteger('Bookmarks', 'Count', Count);
      Ini.UpdateFile;
    finally
      Ini.Free;
    end;
  except
  end;
end;

function TConfigManager.GetBookmarks: TBookmarkItems;
var
  Ini: TIniFile;
  Count, I: Integer;
  Section: string;
begin
  Result := nil;

  try
    if not FileExists(GetAppDataDir + BOOKMARKS_FILENAME) then Exit;

    Ini := TIniFile.Create(GetAppDataDir + BOOKMARKS_FILENAME);
    try
      Count := Ini.ReadInteger('Bookmarks', 'Count', 0);
      SetLength(Result, Count);

      for I := 0 to Count - 1 do
      begin
        Section := 'Bookmark' + IntToStr(I);
        Result[I].FileName := Ini.ReadString(Section, 'FileName', '');
        Result[I].Name := Ini.ReadString(Section, 'Name', '');
        Result[I].Position := Ini.ReadFloat(Section, 'Position', 0);
        Result[I].CreatedAt := Ini.ReadDateTime(Section, 'CreatedAt', Now);
        Result[I].Thumbnail := Ini.ReadString(Section, 'Thumbnail', '');
      end;
    finally
      Ini.Free;
    end;
  except
    SetLength(Result, 0);
  end;
end;

function TConfigManager.GetBookmarksForFile(const FileName: string): TBookmarkItems;
var
  AllBookmarks: TBookmarkItems;
  I, Count: Integer;
begin
  Result := nil;
  AllBookmarks := GetBookmarks;

  { Count matching bookmarks }
  Count := 0;
  for I := 0 to High(AllBookmarks) do
    if AllBookmarks[I].FileName = FileName then
      Inc(Count);

  if Count = 0 then Exit;

  { Copy matching bookmarks }
  SetLength(Result, Count);
  Count := 0;
  for I := 0 to High(AllBookmarks) do
  begin
    if AllBookmarks[I].FileName = FileName then
    begin
      Result[Count] := AllBookmarks[I];
      Inc(Count);
    end;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  RemoveBookmark - Delete a single bookmark by file and position

  Purpose: Removes a specific bookmark identified by its filename and position.
           Uses position tolerance to handle floating-point imprecision.

  Parameters:
    - FileName: Full path to the media file
    - Position: Bookmark position in seconds

  Algorithm:
    1. Linear search through all bookmarks for matching file+position
    2. Position matching uses 1-second tolerance (Abs(diff) < 1.0)
    3. When found, erase the section and shift all subsequent bookmarks down
    4. Update the total count and persist changes

  Notes:
    - Shift algorithm: Copies [Bookmark(i+1)] → [Bookmark(i)] for all items
      after the removed one, maintaining contiguous numbering
    - Copies all fields: FileName, Name, Position, CreatedAt, Thumbnail
    - Only processes first match (breaks after finding)
    - Silent failure on I/O errors or missing file
  ═══════════════════════════════════════════════════════════════════════════ }
procedure TConfigManager.RemoveBookmark(const FileName: string; Position: Double);
var
  Ini: TIniFile;
  Count, I, J, NewCount: Integer;
  Section, NewSection: string;
  Found: Boolean;
  Bookmarks: TBookmarkItems;
begin
  try
    if not FileExists(GetAppDataDir + BOOKMARKS_FILENAME) then Exit;

    Ini := TIniFile.Create(GetAppDataDir + BOOKMARKS_FILENAME);
    try
      Count := Ini.ReadInteger('Bookmarks', 'Count', 0);

      { Find and remove the bookmark }
      Found := False;
      for I := 0 to Count - 1 do
      begin
        Section := 'Bookmark' + IntToStr(I);
        if (Ini.ReadString(Section, 'FileName', '') = FileName) and
           (Abs(Ini.ReadFloat(Section, 'Position', 0) - Position) < 1.0) then
        begin
          Found := True;
          Ini.EraseSection(Section);

          { Shift remaining bookmarks down }
          for J := I + 1 to Count - 1 do
          begin
            Section := 'Bookmark' + IntToStr(J);
            NewSection := 'Bookmark' + IntToStr(J - 1);

            Ini.WriteString(NewSection, 'FileName', Ini.ReadString(Section, 'FileName', ''));
            Ini.WriteString(NewSection, 'Name', Ini.ReadString(Section, 'Name', ''));
            Ini.WriteFloat(NewSection, 'Position', Ini.ReadFloat(Section, 'Position', 0));
            Ini.WriteDateTime(NewSection, 'CreatedAt', Ini.ReadDateTime(Section, 'CreatedAt', Now));
            Ini.WriteString(NewSection, 'Thumbnail', Ini.ReadString(Section, 'Thumbnail', ''));
            Ini.EraseSection(Section);
          end;

          Dec(Count);
          Ini.WriteInteger('Bookmarks', 'Count', Count);
          Break;
        end;
      end;

      if Found then
        Ini.UpdateFile;
    finally
      Ini.Free;
    end;
  except
  end;
end;

procedure TConfigManager.ClearBookmarks;
begin
  try
    if FileExists(GetAppDataDir + BOOKMARKS_FILENAME) then
      DeleteFile(GetAppDataDir + BOOKMARKS_FILENAME);
  except
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  ClearBookmarksForFile - Remove all bookmarks for a specific file

  Purpose: Deletes all bookmarks associated with a given media file.
           Useful when removing a file from the library or clearing history.

  Parameters:
    - FileName: Full path to the media file

  Algorithm:
    1. First pass: Collect indices of all bookmarks matching the filename
    2. Second pass: Remove in REVERSE order (High → 0) to maintain valid indices
    3. For each removal, shift subsequent bookmarks down to fill gaps
    4. Update total count after all removals

  Notes:
    - Reverse iteration is critical: removing index 2 before 5 would invalidate
      index 5, but removing 5 before 2 keeps index 2 valid
    - Each removal triggers a shift of all subsequent items
    - More efficient than calling RemoveBookmark repeatedly (single file open)
    - Silent failure on I/O errors or missing file
  ═══════════════════════════════════════════════════════════════════════════ }
procedure TConfigManager.ClearBookmarksForFile(const FileName: string);
var
  Ini: TIniFile;
  Count, I, J, NewCount: Integer;
  Section, NewSection: string;
  IndicesToRemove: array of Integer;
begin
  try
    if not FileExists(GetAppDataDir + BOOKMARKS_FILENAME) then Exit;

    Ini := TIniFile.Create(GetAppDataDir + BOOKMARKS_FILENAME);
    try
      Count := Ini.ReadInteger('Bookmarks', 'Count', 0);

      { Find all bookmarks to remove }
      SetLength(IndicesToRemove, 0);
      for I := 0 to Count - 1 do
      begin
        Section := 'Bookmark' + IntToStr(I);
        if Ini.ReadString(Section, 'FileName', '') = FileName then
        begin
          SetLength(IndicesToRemove, Length(IndicesToRemove) + 1);
          IndicesToRemove[High(IndicesToRemove)] := I;
        end;
      end;

      if Length(IndicesToRemove) = 0 then Exit;

      { Remove in reverse order to maintain indices }
      for I := High(IndicesToRemove) downto 0 do
      begin
        Ini.EraseSection('Bookmark' + IntToStr(IndicesToRemove[I]));

        { Shift remaining bookmarks down }
        for J := IndicesToRemove[I] + 1 to Count - 1 do
        begin
          Section := 'Bookmark' + IntToStr(J);
          NewSection := 'Bookmark' + IntToStr(J - 1);

          Ini.WriteString(NewSection, 'FileName', Ini.ReadString(Section, 'FileName', ''));
          Ini.WriteString(NewSection, 'Name', Ini.ReadString(Section, 'Name', ''));
          Ini.WriteFloat(NewSection, 'Position', Ini.ReadFloat(Section, 'Position', 0));
          Ini.WriteDateTime(NewSection, 'CreatedAt', Ini.ReadDateTime(Section, 'CreatedAt', Now));
          Ini.WriteString(NewSection, 'Thumbnail', Ini.ReadString(Section, 'Thumbnail', ''));
          Ini.EraseSection(Section);
        end;

        Dec(Count);
      end;

      Ini.WriteInteger('Bookmarks', 'Count', Count);
      Ini.UpdateFile;
    finally
      Ini.Free;
    end;
  except
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  FAVORITES
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TConfigManager.AddFavorite(const Name, Path: string; FavType: TFavoriteType; const Category: string);
var
  Ini: TIniFile;
  Count, I: Integer;
  Section: string;
begin
  try
    Ini := TIniFile.Create(GetAppDataDir + FAVORITES_FILENAME);
    try
      Count := Ini.ReadInteger('Favorites', 'Count', 0);

      { Check if already exists }
      for I := 0 to Count - 1 do
      begin
        Section := 'Favorite' + IntToStr(I);
        if Ini.ReadString(Section, 'Path', '') = Path then
        begin
          { Update existing favorite }
          Ini.WriteString(Section, 'Name', Name);
          Ini.WriteString(Section, 'Category', Category);
          Ini.UpdateFile;
          Exit;
        end;
      end;

      { Add new favorite }
      Section := 'Favorite' + IntToStr(Count);
      Ini.WriteString(Section, 'Name', Name);
      Ini.WriteString(Section, 'Path', Path);
      Ini.WriteInteger(Section, 'Type', Ord(FavType));
      Ini.WriteString(Section, 'Category', Category);
      Ini.WriteDateTime(Section, 'AddedAt', Now);
      Ini.WriteDateTime(Section, 'LastPlayed', 0);
      Ini.WriteInteger(Section, 'PlayCount', 0);

      Inc(Count);
      Ini.WriteInteger('Favorites', 'Count', Count);
      Ini.UpdateFile;
    finally
      Ini.Free;
    end;
  except
  end;
end;

function TConfigManager.GetFavorites: TFavoriteItems;
var
  Ini: TIniFile;
  Count, I, TypeInt: Integer;
  Section: string;
begin
  Result := nil;

  try
    if not FileExists(GetAppDataDir + FAVORITES_FILENAME) then Exit;

    Ini := TIniFile.Create(GetAppDataDir + FAVORITES_FILENAME);
    try
      Count := Ini.ReadInteger('Favorites', 'Count', 0);
      SetLength(Result, Count);

      for I := 0 to Count - 1 do
      begin
        Section := 'Favorite' + IntToStr(I);
        Result[I].Name := Ini.ReadString(Section, 'Name', '');
        Result[I].Path := Ini.ReadString(Section, 'Path', '');
        TypeInt := Ini.ReadInteger(Section, 'Type', Ord(ftFile));
        if (TypeInt >= Ord(Low(TFavoriteType))) and (TypeInt <= Ord(High(TFavoriteType))) then
          Result[I].FavoriteType := TFavoriteType(TypeInt)
        else
          Result[I].FavoriteType := ftFile;
        Result[I].Category := Ini.ReadString(Section, 'Category', '');
        Result[I].AddedAt := Ini.ReadDateTime(Section, 'AddedAt', Now);
        Result[I].LastPlayed := Ini.ReadDateTime(Section, 'LastPlayed', 0);
        Result[I].PlayCount := Ini.ReadInteger(Section, 'PlayCount', 0);
      end;
    finally
      Ini.Free;
    end;
  except
    SetLength(Result, 0);
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  GetFavoritesByCategory - Filter favorites by category name

  Purpose: Returns all favorites belonging to a specific category.
           Categories are user-defined groupings (e.g., "Music", "Movies").

  Parameters:
    - Category: The category name to filter by (case-sensitive)

  Returns: Dynamic array of TFavoriteItem matching the category

  Algorithm: Two-pass filtering pattern
    1. First pass: Count matching items to pre-allocate exact array size
    2. Second pass: Copy matching items into result array
    This avoids repeated SetLength calls which would cause O(n²) reallocations.

  Notes:
    - Returns empty array if no matches found
    - Loads all favorites from disk via GetFavorites, then filters in memory
  ═══════════════════════════════════════════════════════════════════════════ }
function TConfigManager.GetFavoritesByCategory(const Category: string): TFavoriteItems;
var
  AllFavorites: TFavoriteItems;
  I, Count: Integer;
begin
  Result := nil;
  AllFavorites := GetFavorites;

  { Count matching favorites }
  Count := 0;
  for I := 0 to High(AllFavorites) do
    if AllFavorites[I].Category = Category then
      Inc(Count);

  if Count = 0 then Exit;

  { Copy matching favorites }
  SetLength(Result, Count);
  Count := 0;
  for I := 0 to High(AllFavorites) do
  begin
    if AllFavorites[I].Category = Category then
    begin
      Result[Count] := AllFavorites[I];
      Inc(Count);
    end;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  GetFavoritesByType - Filter favorites by type enumeration

  Purpose: Returns all favorites of a specific type (file, folder, URL, radio).
           Enables type-specific UI views (e.g., "Show all radio stations").

  Parameters:
    - FavType: TFavoriteType enum value (ftFile, ftFolder, ftURL, ftRadio)

  Returns: Dynamic array of TFavoriteItem matching the type

  Algorithm: Same two-pass pattern as GetFavoritesByCategory
    1. First pass: Count matches
    2. Second pass: Copy matches
    Pre-allocation prevents O(n²) reallocation overhead.

  Notes:
    - Returns empty array if no matches found
    - Loads all favorites from disk via GetFavorites, then filters in memory
  ═══════════════════════════════════════════════════════════════════════════ }
function TConfigManager.GetFavoritesByType(FavType: TFavoriteType): TFavoriteItems;
var
  AllFavorites: TFavoriteItems;
  I, Count: Integer;
begin
  Result := nil;
  AllFavorites := GetFavorites;

  { Count matching favorites }
  Count := 0;
  for I := 0 to High(AllFavorites) do
    if AllFavorites[I].FavoriteType = FavType then
      Inc(Count);

  if Count = 0 then Exit;

  { Copy matching favorites }
  SetLength(Result, Count);
  Count := 0;
  for I := 0 to High(AllFavorites) do
  begin
    if AllFavorites[I].FavoriteType = FavType then
    begin
      Result[Count] := AllFavorites[I];
      Inc(Count);
    end;
  end;
end;

function TConfigManager.IsFavorite(const Path: string): Boolean;
var
  Ini: TIniFile;
  Count, I: Integer;
  Section: string;
begin
  Result := False;

  try
    if not FileExists(GetAppDataDir + FAVORITES_FILENAME) then Exit;

    Ini := TIniFile.Create(GetAppDataDir + FAVORITES_FILENAME);
    try
      Count := Ini.ReadInteger('Favorites', 'Count', 0);

      for I := 0 to Count - 1 do
      begin
        Section := 'Favorite' + IntToStr(I);
        if Ini.ReadString(Section, 'Path', '') = Path then
        begin
          Result := True;
          Exit;
        end;
      end;
    finally
      Ini.Free;
    end;
  except
  end;
end;

procedure TConfigManager.RemoveFavorite(const Path: string);
var
  Ini: TIniFile;
  Count, I, J: Integer;
  Section, NewSection: string;
  Found: Boolean;
begin
  try
    if not FileExists(GetAppDataDir + FAVORITES_FILENAME) then Exit;

    Ini := TIniFile.Create(GetAppDataDir + FAVORITES_FILENAME);
    try
      Count := Ini.ReadInteger('Favorites', 'Count', 0);

      { Find and remove the favorite }
      Found := False;
      for I := 0 to Count - 1 do
      begin
        Section := 'Favorite' + IntToStr(I);
        if Ini.ReadString(Section, 'Path', '') = Path then
        begin
          Found := True;
          Ini.EraseSection(Section);

          { Shift remaining favorites down }
          for J := I + 1 to Count - 1 do
          begin
            Section := 'Favorite' + IntToStr(J);
            NewSection := 'Favorite' + IntToStr(J - 1);

            Ini.WriteString(NewSection, 'Name', Ini.ReadString(Section, 'Name', ''));
            Ini.WriteString(NewSection, 'Path', Ini.ReadString(Section, 'Path', ''));
            Ini.WriteInteger(NewSection, 'Type', Ini.ReadInteger(Section, 'Type', 0));
            Ini.WriteString(NewSection, 'Category', Ini.ReadString(Section, 'Category', ''));
            Ini.WriteDateTime(NewSection, 'AddedAt', Ini.ReadDateTime(Section, 'AddedAt', Now));
            Ini.WriteDateTime(NewSection, 'LastPlayed', Ini.ReadDateTime(Section, 'LastPlayed', 0));
            Ini.WriteInteger(NewSection, 'PlayCount', Ini.ReadInteger(Section, 'PlayCount', 0));
            Ini.EraseSection(Section);
          end;

          Dec(Count);
          Ini.WriteInteger('Favorites', 'Count', Count);
          Break;
        end;
      end;

      if Found then
        Ini.UpdateFile;
    finally
      Ini.Free;
    end;
  except
  end;
end;

procedure TConfigManager.UpdateFavoriteLastPlayed(const Path: string);
var
  Ini: TIniFile;
  Count, I, PlayCount: Integer;
  Section: string;
begin
  try
    if not FileExists(GetAppDataDir + FAVORITES_FILENAME) then Exit;

    Ini := TIniFile.Create(GetAppDataDir + FAVORITES_FILENAME);
    try
      Count := Ini.ReadInteger('Favorites', 'Count', 0);

      for I := 0 to Count - 1 do
      begin
        Section := 'Favorite' + IntToStr(I);
        if Ini.ReadString(Section, 'Path', '') = Path then
        begin
          Ini.WriteDateTime(Section, 'LastPlayed', Now);
          PlayCount := Ini.ReadInteger(Section, 'PlayCount', 0);
          Ini.WriteInteger(Section, 'PlayCount', PlayCount + 1);
          Ini.UpdateFile;
          Exit;
        end;
      end;
    finally
      Ini.Free;
    end;
  except
  end;
end;

procedure TConfigManager.ClearFavorites;
begin
  try
    if FileExists(GetAppDataDir + FAVORITES_FILENAME) then
      DeleteFile(GetAppDataDir + FAVORITES_FILENAME);
  except
  end;
end;

function TConfigManager.GetFavoriteCategories: TStringList;
var
  Favorites: TFavoriteItems;
  I: Integer;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;

  Favorites := GetFavorites;
  for I := 0 to High(Favorites) do
  begin
    if Favorites[I].Category <> '' then
      Result.Add(Favorites[I].Category);
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PHASE 24: SESSION PLAYLIST SAVE/RESTORE
  ═══════════════════════════════════════════════════════════════════════════════ }

const
  SESSION_PLAYLIST_FILENAME = 'session_playlist.ini';

procedure TConfigManager.SaveSessionPlaylist(const Items: TPlaylistItems;
  CurrentIndex: Integer; PlaybackMode: TPlaybackMode; CurrentPosition: Double);
var
  Ini: TIniFile;
  I: Integer;
  Section: string;
begin
  try
    Ini := TIniFile.Create(GetAppDataDir + SESSION_PLAYLIST_FILENAME);
    try
      { Clear existing data }
      Ini.EraseSection('Session');

      { Save session info }
      Ini.WriteInteger('Session', 'Count', Length(Items));
      Ini.WriteInteger('Session', 'CurrentIndex', CurrentIndex);
      Ini.WriteInteger('Session', 'PlaybackMode', Ord(PlaybackMode));
      Ini.WriteFloat('Session', 'CurrentPosition', CurrentPosition);

      { Save each playlist item }
      for I := 0 to High(Items) do
      begin
        Section := 'Item' + IntToStr(I);
        Ini.EraseSection(Section);
        Ini.WriteString(Section, 'FileName', Items[I].FileName);
        Ini.WriteString(Section, 'Title', Items[I].Title);
        Ini.WriteString(Section, 'Artist', Items[I].Artist);
        Ini.WriteString(Section, 'Album', Items[I].Album);
        Ini.WriteFloat(Section, 'Duration', Items[I].Duration);
        Ini.WriteString(Section, 'DurationString', Items[I].DurationString);
        Ini.WriteBool(Section, 'Played', Items[I].Played);
      end;

      Ini.UpdateFile;
    finally
      Ini.Free;
    end;
  except
  end;
end;

function TConfigManager.LoadSessionPlaylist(out Items: TPlaylistItems;
  out CurrentIndex: Integer; out PlaybackMode: TPlaybackMode;
  out CurrentPosition: Double): Boolean;
var
  Ini: TIniFile;
  Count, I, ModeInt: Integer;
  Section: string;
begin
  Result := False;
  SetLength(Items, 0);
  CurrentIndex := -1;
  PlaybackMode := pmNormal;
  CurrentPosition := 0;

  try
    if not FileExists(GetAppDataDir + SESSION_PLAYLIST_FILENAME) then Exit;

    Ini := TIniFile.Create(GetAppDataDir + SESSION_PLAYLIST_FILENAME);
    try
      Count := Ini.ReadInteger('Session', 'Count', 0);
      if Count = 0 then Exit;

      CurrentIndex := Ini.ReadInteger('Session', 'CurrentIndex', -1);
      ModeInt := Ini.ReadInteger('Session', 'PlaybackMode', Ord(pmNormal));
      if (ModeInt >= Ord(Low(TPlaybackMode))) and (ModeInt <= Ord(High(TPlaybackMode))) then
        PlaybackMode := TPlaybackMode(ModeInt)
      else
        PlaybackMode := pmNormal;
      CurrentPosition := Ini.ReadFloat('Session', 'CurrentPosition', 0);

      { Load playlist items }
      SetLength(Items, Count);
      for I := 0 to Count - 1 do
      begin
        Section := 'Item' + IntToStr(I);
        Items[I].FileName := Ini.ReadString(Section, 'FileName', '');
        Items[I].Title := Ini.ReadString(Section, 'Title', '');
        Items[I].Artist := Ini.ReadString(Section, 'Artist', '');
        Items[I].Album := Ini.ReadString(Section, 'Album', '');
        Items[I].Duration := Ini.ReadFloat(Section, 'Duration', 0);
        Items[I].DurationString := Ini.ReadString(Section, 'DurationString', '--:--');
        Items[I].Played := Ini.ReadBool(Section, 'Played', False);
        Items[I].Selected := False;
      end;

      Result := True;
    finally
      Ini.Free;
    end;
  except
    SetLength(Items, 0);
    Result := False;
  end;
end;

procedure TConfigManager.ClearSessionPlaylist;
begin
  try
    if FileExists(GetAppDataDir + SESSION_PLAYLIST_FILENAME) then
      DeleteFile(GetAppDataDir + SESSION_PLAYLIST_FILENAME);
  except
  end;
end;

initialization
  Config := TConfigManager.Create;
  Config.Load;

finalization
  FreeAndNil(Config);

end.
