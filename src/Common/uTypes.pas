{ ═══════════════════════════════════════════════════════════════════════════════
  uTypes.pas - Common Types and Records

  Part of 3nity Media - Lazarus Edition

  This unit defines common types, records, and enumerations used throughout
  the application.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  { ═══════════════════════════════════════════════════════════════════════════
    PLAYLIST ITEM
    ═══════════════════════════════════════════════════════════════════════════ }
  TPlaylistItem = record
    FileName: string;
    Title: string;
    Artist: string;
    Album: string;
    Duration: Double;
    DurationString: string;
    Played: Boolean;
    Selected: Boolean;
  end;

  TPlaylistItems = array of TPlaylistItem;

  { Integer dynamic array for index lists }
  TIntegerDynArray = array of Integer;

  { ═══════════════════════════════════════════════════════════════════════════
    RADIO STATION
    ═══════════════════════════════════════════════════════════════════════════ }
  TRadioStation = record
    Name: string;
    URL: string;
    Genre: string;
    Bitrate: Integer;
    Codec: string;
    Channels: Integer;
    SampleRate: Integer;
    Website: string;
    Description: string;
    Country: string;
    Language: string;
    Favorite: Boolean;
  end;

  TRadioStations = array of TRadioStation;

  { ═══════════════════════════════════════════════════════════════════════════
    EQUALIZER PRESET
    ═══════════════════════════════════════════════════════════════════════════ }
  TEqualizerPreset = record
    Name: string;
    Bands: array[0..9] of Double;
  end;

  TEqualizerPresets = array of TEqualizerPreset;

  { ═══════════════════════════════════════════════════════════════════════════
    PLAYBACK MODE
    ═══════════════════════════════════════════════════════════════════════════ }
  TPlaybackMode = (
    pmNormal,          { Play through playlist once }
    pmRepeatOne,       { Repeat current item }
    pmRepeatAll,       { Repeat entire playlist }
    pmShuffle,         { Random playback }
    pmShuffleRepeat    { Random playback with repeat }
  );

  { ═══════════════════════════════════════════════════════════════════════════
    WINDOW GEOMETRY
    ═══════════════════════════════════════════════════════════════════════════ }
  TWindowGeometry = record
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    Maximized: Boolean;
    Fullscreen: Boolean;
    AlwaysOnTop: Boolean;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    SUBTITLE SETTINGS
    ═══════════════════════════════════════════════════════════════════════════ }
  TSubtitleSettings = record
    UseDefault: Boolean;     { If true, use mpv defaults instead of custom settings }
    FontName: string;
    FontSize: Integer;
    FontColor: TColor;
    FontBold: Boolean;
    FontItalic: Boolean;
    OutlineColor: TColor;
    OutlineSize: Integer;
    BackgroundColor: TColor;
    BackgroundOpacity: Integer;
    Position: Integer;       { 0-100, vertical position }
    Encoding: string;
    AutoLoad: Boolean;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    VIDEO SETTINGS
    ═══════════════════════════════════════════════════════════════════════════ }
  TVideoSettings = record
    Brightness: Integer;
    Contrast: Integer;
    Saturation: Integer;
    Hue: Integer;
    Gamma: Integer;
    AspectMode: Integer;
    AspectFactor: Double;
    Deinterlace: Integer;
    DeinterlaceAlg: Integer;
    VideoOutput: string;
    HWAccel: Boolean;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    AUDIO SETTINGS
    ═══════════════════════════════════════════════════════════════════════════ }
  TAudioSettings = record
    Volume: Integer;
    Muted: Boolean;
    AudioOutput: string;
    AudioDevice: string;
    Channels: Integer;
    Normalize: Boolean;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    CACHE SETTINGS
    ═══════════════════════════════════════════════════════════════════════════ }
  TCacheSettings = record
    DefaultSize: Integer;
    FixedSize: Integer;
    RamdiskSize: Integer;
    CDROMSize: Integer;
    RemovableSize: Integer;
    NetworkSize: Integer;
    InternetSize: Integer;
    DVDSize: Integer;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    GENERAL SETTINGS
    ═══════════════════════════════════════════════════════════════════════════ }
  TGeneralSettings = record
    Language: string;
    SingleInstance: Boolean;
    ScreenshotPath: string;
    ScreenshotFormat: string;
    HistoryEnabled: Boolean;
    HistoryMaxItems: Integer;
    { Phase 24: Auto-save playlist }
    AutoSavePlaylist: Boolean;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    APPLICATION SETTINGS (COMPLETE)
    ═══════════════════════════════════════════════════════════════════════════ }
  TAppSettings = record
    General: TGeneralSettings;
    Video: TVideoSettings;
    Audio: TAudioSettings;
    Subtitles: TSubtitleSettings;
    Cache: TCacheSettings;
    PlaybackMode: TPlaybackMode;
    EqualizerBands: array[0..9] of Double;
    EqualizerEnabled: Boolean;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    HISTORY ITEM
    ═══════════════════════════════════════════════════════════════════════════ }
  THistoryItem = record
    FileName: string;
    Title: string;
    Position: Double;
    Duration: Double;
    LastPlayed: TDateTime;
    PlayCount: Integer;
  end;

  THistoryItems = array of THistoryItem;

  { ═══════════════════════════════════════════════════════════════════════════
    BOOKMARK ITEM
    ═══════════════════════════════════════════════════════════════════════════ }
  TBookmarkItem = record
    FileName: string;      { Full path to media file }
    Name: string;          { User-defined bookmark name }
    Position: Double;      { Position in seconds }
    CreatedAt: TDateTime;  { When bookmark was created }
    Thumbnail: string;     { Optional: path to thumbnail image }
  end;

  TBookmarkItems = array of TBookmarkItem;

  { ═══════════════════════════════════════════════════════════════════════════
    FAVORITE ITEM
    ═══════════════════════════════════════════════════════════════════════════ }
  TFavoriteType = (
    ftFile,            { Local file }
    ftURL,             { Stream/URL }
    ftRadio,           { Radio station }
    ftDVD,             { DVD }
    ftBluray           { Blu-ray }
  );

  TFavoriteItem = record
    Name: string;          { Display name }
    Path: string;          { File path or URL }
    FavoriteType: TFavoriteType;
    Category: string;      { Optional category/folder }
    AddedAt: TDateTime;    { When added to favorites }
    LastPlayed: TDateTime; { Last time played }
    PlayCount: Integer;    { Number of times played }
  end;

  TFavoriteItems = array of TFavoriteItem;

  { ═══════════════════════════════════════════════════════════════════════════
    SEEK MODE
    ═══════════════════════════════════════════════════════════════════════════ }
  TSeekMode = (
    smRelative,        { Seek relative to current position }
    smAbsolute,        { Seek to absolute position }
    smPercent          { Seek to percentage of duration }
  );

  { ═══════════════════════════════════════════════════════════════════════════
    SCREENSHOT MODE
    ═══════════════════════════════════════════════════════════════════════════ }
  TScreenshotMode = (
    ssVideo,           { Video frame only }
    ssSubtitles,       { Video with subtitles }
    ssWindow           { Entire window }
  );

  { ═══════════════════════════════════════════════════════════════════════════
    KEYBOARD SHORTCUT
    ═══════════════════════════════════════════════════════════════════════════ }
  TKeyboardShortcut = record
    Action: string;
    ShortCut: TShortCut;
    Description: string;
  end;

  TKeyboardShortcuts = array of TKeyboardShortcut;

implementation

end.
