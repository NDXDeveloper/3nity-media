{ ═══════════════════════════════════════════════════════════════════════════════
  uTestConfig.pas - Unit Tests for Configuration Management

  Part of 3nity Media - Test Suite

  Tests for the uConfig unit which handles application settings persistence.
  Also tests related types from uTypes.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uTestConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpcunit, testregistry,
  uConfig, uTypes, uConstants;

type
  { TTestConfig }
  TTestConfig = class(TTestCase)
  published
    { Directory function tests }
    procedure Test_GetAppDataDir_ReturnsNonEmpty;
    procedure Test_GetAppDataDir_EndsWithPathDelim;
    procedure Test_GetTempDir_ReturnsNonEmpty;
    procedure Test_GetTempDir_EndsWithPathDelim;
    procedure Test_GetConfigDir_EqualsAppDataDir;
    procedure Test_GetUserPicturesDir_ReturnsNonEmpty;

    { TPlaybackMode enum tests }
    procedure Test_PlaybackMode_EnumValues;
    procedure Test_PlaybackMode_EnumCount;

    { TFavoriteType enum tests }
    procedure Test_FavoriteType_EnumValues;
    procedure Test_FavoriteType_EnumCount;

    { TSeekMode enum tests }
    procedure Test_SeekMode_EnumValues;

    { TScreenshotMode enum tests }
    procedure Test_ScreenshotMode_EnumValues;

    { TConfigManager default values tests }
    procedure Test_ConfigManager_Exists;
    procedure Test_ConfigManager_DefaultLanguage;
    procedure Test_ConfigManager_DefaultSingleInstance;
    procedure Test_ConfigManager_DefaultHistoryEnabled;
    procedure Test_ConfigManager_DefaultHistoryMaxItems;
    procedure Test_ConfigManager_DefaultAutoSavePlaylist;

    { Video defaults }
    procedure Test_ConfigManager_DefaultVideoBrightness;
    procedure Test_ConfigManager_DefaultVideoContrast;
    procedure Test_ConfigManager_DefaultVideoSaturation;
    procedure Test_ConfigManager_DefaultVideoHue;
    procedure Test_ConfigManager_DefaultVideoGamma;
    procedure Test_ConfigManager_DefaultHWAccel;

    { Audio defaults }
    procedure Test_ConfigManager_DefaultVolume;
    procedure Test_ConfigManager_DefaultMuted;
    procedure Test_ConfigManager_DefaultChannels;
    procedure Test_ConfigManager_DefaultNormalize;

    { Subtitle defaults }
    procedure Test_ConfigManager_DefaultSubUseDefault;
    procedure Test_ConfigManager_DefaultSubFontName;
    procedure Test_ConfigManager_DefaultSubFontSize;
    procedure Test_ConfigManager_DefaultSubFontColor;
    procedure Test_ConfigManager_DefaultSubAutoLoad;

    { Cache defaults }
    procedure Test_ConfigManager_DefaultCacheSizes;

    { Equalizer defaults }
    procedure Test_ConfigManager_DefaultEqualizerEnabled;
    procedure Test_ConfigManager_DefaultEqualizerBands;

    { Playback mode default }
    procedure Test_ConfigManager_DefaultPlaybackMode;

    { Record initialization tests }
    procedure Test_TPlaylistItem_DefaultValues;
    procedure Test_TRadioStation_DefaultValues;
    procedure Test_TBookmarkItem_DefaultValues;
    procedure Test_TFavoriteItem_DefaultValues;
  end;

implementation

uses
  uMPVConst;

{ TTestConfig }

{ ─────────────────────────────────────────────────────────────────────────────
  Directory Function Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestConfig.Test_GetAppDataDir_ReturnsNonEmpty;
begin
  AssertTrue('GetAppDataDir should return non-empty string',
    GetAppDataDir <> '');
end;

procedure TTestConfig.Test_GetAppDataDir_EndsWithPathDelim;
var
  Dir: string;
begin
  Dir := GetAppDataDir;
  AssertTrue('GetAppDataDir should end with path delimiter',
    (Length(Dir) > 0) and (Dir[Length(Dir)] = PathDelim));
end;

procedure TTestConfig.Test_GetTempDir_ReturnsNonEmpty;
begin
  AssertTrue('GetTempDir should return non-empty string',
    GetTempDir <> '');
end;

procedure TTestConfig.Test_GetTempDir_EndsWithPathDelim;
var
  Dir: string;
begin
  Dir := GetTempDir;
  AssertTrue('GetTempDir should end with path delimiter',
    (Length(Dir) > 0) and (Dir[Length(Dir)] = PathDelim));
end;

procedure TTestConfig.Test_GetConfigDir_EqualsAppDataDir;
begin
  AssertEquals('GetConfigDir should equal GetAppDataDir',
    GetAppDataDir, GetConfigDir);
end;

procedure TTestConfig.Test_GetUserPicturesDir_ReturnsNonEmpty;
begin
  AssertTrue('GetUserPicturesDir should return non-empty string',
    GetUserPicturesDir <> '');
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TPlaybackMode Enum Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestConfig.Test_PlaybackMode_EnumValues;
begin
  AssertEquals('pmNormal = 0', 0, Ord(pmNormal));
  AssertEquals('pmRepeatOne = 1', 1, Ord(pmRepeatOne));
  AssertEquals('pmRepeatAll = 2', 2, Ord(pmRepeatAll));
  AssertEquals('pmShuffle = 3', 3, Ord(pmShuffle));
  AssertEquals('pmShuffleRepeat = 4', 4, Ord(pmShuffleRepeat));
end;

procedure TTestConfig.Test_PlaybackMode_EnumCount;
begin
  { Verify enum has exactly 5 values }
  AssertEquals('TPlaybackMode should have 5 values',
    4, Ord(High(TPlaybackMode)));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TFavoriteType Enum Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestConfig.Test_FavoriteType_EnumValues;
begin
  AssertEquals('ftFile = 0', 0, Ord(ftFile));
  AssertEquals('ftURL = 1', 1, Ord(ftURL));
  AssertEquals('ftRadio = 2', 2, Ord(ftRadio));
  AssertEquals('ftDVD = 3', 3, Ord(ftDVD));
  AssertEquals('ftBluray = 4', 4, Ord(ftBluray));
end;

procedure TTestConfig.Test_FavoriteType_EnumCount;
begin
  { Verify enum has exactly 5 values }
  AssertEquals('TFavoriteType should have 5 values',
    4, Ord(High(TFavoriteType)));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TSeekMode Enum Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestConfig.Test_SeekMode_EnumValues;
begin
  AssertEquals('smRelative = 0', 0, Ord(smRelative));
  AssertEquals('smAbsolute = 1', 1, Ord(smAbsolute));
  AssertEquals('smPercent = 2', 2, Ord(smPercent));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TScreenshotMode Enum Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestConfig.Test_ScreenshotMode_EnumValues;
begin
  AssertEquals('ssVideo = 0', 0, Ord(ssVideo));
  AssertEquals('ssSubtitles = 1', 1, Ord(ssSubtitles));
  AssertEquals('ssWindow = 2', 2, Ord(ssWindow));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TConfigManager Default Values Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestConfig.Test_ConfigManager_Exists;
begin
  AssertNotNull('Global Config should exist', Config);
end;

procedure TTestConfig.Test_ConfigManager_DefaultLanguage;
begin
  { Default language should be 'en' }
  AssertEquals('Default language', 'en', Config.GetGeneral.Language);
end;

procedure TTestConfig.Test_ConfigManager_DefaultSingleInstance;
begin
  AssertTrue('Default SingleInstance should be True',
    Config.GetGeneral.SingleInstance);
end;

procedure TTestConfig.Test_ConfigManager_DefaultHistoryEnabled;
begin
  AssertTrue('Default HistoryEnabled should be True',
    Config.GetGeneral.HistoryEnabled);
end;

procedure TTestConfig.Test_ConfigManager_DefaultHistoryMaxItems;
begin
  AssertEquals('Default HistoryMaxItems',
    MAX_HISTORY_ITEMS, Config.GetGeneral.HistoryMaxItems);
end;

procedure TTestConfig.Test_ConfigManager_DefaultAutoSavePlaylist;
begin
  AssertTrue('Default AutoSavePlaylist should be True',
    Config.GetGeneral.AutoSavePlaylist);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Video Default Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestConfig.Test_ConfigManager_DefaultVideoBrightness;
begin
  AssertEquals('Default Brightness',
    VIDEO_PROP_DEFAULT, Config.GetVideo.Brightness);
end;

procedure TTestConfig.Test_ConfigManager_DefaultVideoContrast;
begin
  AssertEquals('Default Contrast',
    VIDEO_PROP_DEFAULT, Config.GetVideo.Contrast);
end;

procedure TTestConfig.Test_ConfigManager_DefaultVideoSaturation;
begin
  AssertEquals('Default Saturation',
    VIDEO_PROP_DEFAULT, Config.GetVideo.Saturation);
end;

procedure TTestConfig.Test_ConfigManager_DefaultVideoHue;
begin
  AssertEquals('Default Hue',
    VIDEO_PROP_DEFAULT, Config.GetVideo.Hue);
end;

procedure TTestConfig.Test_ConfigManager_DefaultVideoGamma;
begin
  AssertEquals('Default Gamma',
    VIDEO_PROP_DEFAULT, Config.GetVideo.Gamma);
end;

procedure TTestConfig.Test_ConfigManager_DefaultHWAccel;
begin
  AssertTrue('Default HWAccel should be True',
    Config.GetVideo.HWAccel);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Audio Default Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestConfig.Test_ConfigManager_DefaultVolume;
begin
  AssertEquals('Default Volume',
    VOL_DEFAULT, Config.GetAudio.Volume);
end;

procedure TTestConfig.Test_ConfigManager_DefaultMuted;
begin
  AssertFalse('Default Muted should be False',
    Config.GetAudio.Muted);
end;

procedure TTestConfig.Test_ConfigManager_DefaultChannels;
begin
  AssertEquals('Default Channels', 2, Config.GetAudio.Channels);
end;

procedure TTestConfig.Test_ConfigManager_DefaultNormalize;
begin
  AssertFalse('Default Normalize should be False',
    Config.GetAudio.Normalize);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Subtitle Default Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestConfig.Test_ConfigManager_DefaultSubUseDefault;
begin
  AssertTrue('Default UseDefault should be True',
    Config.GetSubtitles.UseDefault);
end;

procedure TTestConfig.Test_ConfigManager_DefaultSubFontName;
begin
  AssertEquals('Default FontName', 'Arial', Config.GetSubtitles.FontName);
end;

procedure TTestConfig.Test_ConfigManager_DefaultSubFontSize;
begin
  AssertEquals('Default FontSize', 24, Config.GetSubtitles.FontSize);
end;

procedure TTestConfig.Test_ConfigManager_DefaultSubFontColor;
begin
  AssertEquals('Default FontColor', Integer(clWhite),
    Integer(Config.GetSubtitles.FontColor));
end;

procedure TTestConfig.Test_ConfigManager_DefaultSubAutoLoad;
begin
  AssertTrue('Default AutoLoad should be True',
    Config.GetSubtitles.AutoLoad);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Cache Default Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestConfig.Test_ConfigManager_DefaultCacheSizes;
begin
  AssertEquals('Default DefaultSize',
    DEFAULT_CACHE_DEFAULT, Config.GetCache.DefaultSize);
  AssertEquals('Default FixedSize',
    DEFAULT_CACHE_FIXED, Config.GetCache.FixedSize);
  AssertEquals('Default NetworkSize',
    DEFAULT_CACHE_NETWORK, Config.GetCache.NetworkSize);
  AssertEquals('Default InternetSize',
    DEFAULT_CACHE_INTERNET, Config.GetCache.InternetSize);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Equalizer Default Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestConfig.Test_ConfigManager_DefaultEqualizerEnabled;
begin
  AssertFalse('Default EqualizerEnabled should be False',
    Config.Settings.EqualizerEnabled);
end;

procedure TTestConfig.Test_ConfigManager_DefaultEqualizerBands;
var
  I: Integer;
begin
  for I := 0 to 9 do
    AssertTrue('Default EQ Band ' + IntToStr(I) + ' should be 0',
      Abs(Config.Settings.EqualizerBands[I]) < 0.001);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Playback Mode Default Test
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestConfig.Test_ConfigManager_DefaultPlaybackMode;
begin
  AssertEquals('Default PlaybackMode should be pmNormal',
    Ord(pmNormal), Ord(Config.Settings.PlaybackMode));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Record Initialization Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestConfig.Test_TPlaylistItem_DefaultValues;
var
  Item: TPlaylistItem;
begin
  FillChar(Item, SizeOf(Item), 0);

  AssertEquals('Default FileName', '', Item.FileName);
  AssertEquals('Default Title', '', Item.Title);
  AssertEquals('Default Artist', '', Item.Artist);
  AssertEquals('Default Album', '', Item.Album);
  AssertTrue('Default Duration should be 0', Abs(Item.Duration) < 0.001);
  AssertFalse('Default Played should be False', Item.Played);
  AssertFalse('Default Selected should be False', Item.Selected);
end;

procedure TTestConfig.Test_TRadioStation_DefaultValues;
var
  Station: TRadioStation;
begin
  FillChar(Station, SizeOf(Station), 0);

  AssertEquals('Default Name', '', Station.Name);
  AssertEquals('Default URL', '', Station.URL);
  AssertEquals('Default Genre', '', Station.Genre);
  AssertEquals('Default Bitrate', 0, Station.Bitrate);
  AssertEquals('Default Codec', '', Station.Codec);
  AssertFalse('Default Favorite should be False', Station.Favorite);
end;

procedure TTestConfig.Test_TBookmarkItem_DefaultValues;
var
  Bookmark: TBookmarkItem;
begin
  FillChar(Bookmark, SizeOf(Bookmark), 0);

  AssertEquals('Default FileName', '', Bookmark.FileName);
  AssertEquals('Default Name', '', Bookmark.Name);
  AssertTrue('Default Position should be 0', Abs(Bookmark.Position) < 0.001);
  AssertEquals('Default Thumbnail', '', Bookmark.Thumbnail);
end;

procedure TTestConfig.Test_TFavoriteItem_DefaultValues;
var
  Favorite: TFavoriteItem;
begin
  FillChar(Favorite, SizeOf(Favorite), 0);

  AssertEquals('Default Name', '', Favorite.Name);
  AssertEquals('Default Path', '', Favorite.Path);
  AssertEquals('Default FavoriteType', Ord(ftFile), Ord(Favorite.FavoriteType));
  AssertEquals('Default Category', '', Favorite.Category);
  AssertEquals('Default PlayCount', 0, Favorite.PlayCount);
end;

initialization
  RegisterTest(TTestConfig);

end.
