{ ═══════════════════════════════════════════════════════════════════════════════
  uMPVEngine.pas - Complete libmpv Wrapper for 3nity Media Player

  Part of 3nity Media - Lazarus Edition

  This unit provides a complete Pascal wrapper around libmpv, equivalent
  to the original mplayer.pas (~4309 lines). It handles all multimedia
  playback functionality including:
  - Video/Audio playback with full format support
  - DVD/Bluray navigation
  - Track selection (audio, subtitles, video)
  - 10-band audio equalizer
  - Video properties (brightness, contrast, saturation, hue, gamma)
  - Streaming with ICY metadata for radios
  - Cache management per source type
  - Comprehensive event system (20+ events)

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uMPVEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LCLIntf, Forms, ExtCtrls,
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  {$IFDEF UNIX}initc, ctypes, x, xlib, xutil,{$ENDIF}
  {$IFDEF LCLQT5}qt5, qtwidgets,{$ENDIF}
  SyncObjs, uLibMPV, uMPVConst, uLog;

{$IFDEF UNIX}
const
  LC_NUMERIC = 1;

function setlocale(category: cint; locale: PAnsiChar): PAnsiChar; cdecl; external 'c' name 'setlocale';
{$ENDIF}

type
  { ═══════════════════════════════════════════════════════════════════════════
    STATUS ENUMERATION (equivalent to TStatus in mplayer.pas)
    ═══════════════════════════════════════════════════════════════════════════ }
  TMPVStatus = (
    msNone,           { No media loaded }
    msOpening,        { Opening media }
    msClosing,        { Closing media }
    msPlayStarting,   { Starting playback }
    msPlaying,        { Currently playing }
    msPaused,         { Paused }
    msStopped,        { Stopped }
    msError,          { Error occurred }
    msErrorRetry      { Error with retry }
  );

  { ═══════════════════════════════════════════════════════════════════════════
    VIDEO INFO RECORD
    ═══════════════════════════════════════════════════════════════════════════ }
  TMPVVideoInfo = record
    Codec: string;
    Decoder: string;
    Width: Integer;
    Height: Integer;
    DisplayWidth: Integer;
    DisplayHeight: Integer;
    FPS: Double;
    Bitrate: Integer;
    Aspect: Double;
    PixelFormat: string;
    HWDecoder: string;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    AUDIO INFO RECORD
    ═══════════════════════════════════════════════════════════════════════════ }
  TMPVAudioInfo = record
    Codec: string;
    Decoder: string;
    Channels: Integer;
    ChannelLayout: string;
    SampleRate: Integer;
    Bitrate: Integer;
    Format: string;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    STREAM INFO RECORD (equivalent to TStreamInfo in mplayer.pas)
    ═══════════════════════════════════════════════════════════════════════════ }
  TMPVStreamInfo = record
    Valid: Boolean;
    FileName: string;
    FileFormat: string;
    FileSize: Int64;
    DurationSec: Double;
    DurationString: string;
    HasAudio: Boolean;
    HasVideo: Boolean;
    Seekable: Boolean;
    IsDVD: Boolean;
    IsDVDNav: Boolean;
    IsBluray: Boolean;
    IsStream: Boolean;
    IsRadio: Boolean;
    IsMp3: Boolean;
    Video: TMPVVideoInfo;
    Audio: TMPVAudioInfo;
    Metadata: array[0..15] of record
      Key: string;
      Value: string;
    end;
    MetadataCount: Integer;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    RENDER INFO RECORD (equivalent to TRenderInfo in mplayer.pas)
    ═══════════════════════════════════════════════════════════════════════════ }
  TMPVRenderInfo = record
    Width: Integer;
    Height: Integer;
    DisplayWidth: Integer;
    DisplayHeight: Integer;
    Aspect: Double;
    TitleID: Integer;
    ChapterID: Integer;
    VideoError: Boolean;
    IsDVDMenu: Boolean;
    Fullscreen: Boolean;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    TRACK INFO RECORD
    ═══════════════════════════════════════════════════════════════════════════ }
  TMPVTrackInfo = record
    ID: Integer;
    TrackType: string;     { 'audio', 'video', 'sub' }
    Title: string;
    Language: string;
    Codec: string;
    IsDefault: Boolean;
    IsForced: Boolean;
    IsExternal: Boolean;
    ExternalFilename: string;
    FFIndex: Integer;
    { Audio specific }
    Channels: Integer;
    ChannelLayout: string;
    SampleRate: Integer;
    { Video specific }
    Width: Integer;
    Height: Integer;
    FPS: Double;
    Bitrate: Integer;
  end;

  TMPVTrackArray = array of TMPVTrackInfo;

  { ═══════════════════════════════════════════════════════════════════════════
    TRACK LIST CLASS (equivalent to TItemIds in mplayer.pas)
    ═══════════════════════════════════════════════════════════════════════════ }
  TMPVTrackList = class
  private
    FItems: TMPVTrackArray;
    FSelectedID: Integer;
    FTrackType: string;
    FCount: Integer;
    function GetItem(Index: Integer): TMPVTrackInfo;
  public
    constructor Create(const ATrackType: string);
    destructor Destroy; override;
    procedure Clear;
    function Add(const Track: TMPVTrackInfo): Integer;
    function FindByID(ID: Integer): Integer;
    function GetSelectedIndex: Integer;
    property Count: Integer read FCount;
    property Items[Index: Integer]: TMPVTrackInfo read GetItem; default;
    property SelectedID: Integer read FSelectedID write FSelectedID;
    property TrackType: string read FTrackType;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    PROPERTY VALUES RECORD (equivalent to TPropertyValues in mplayer.pas)
    ═══════════════════════════════════════════════════════════════════════════ }
  TMPVPropertyValues = record
    Default: Integer;
    Valid: Boolean;
    Value: Integer;
  end;

  { Forward declaration }
  TMPVEngine = class;

  { ═══════════════════════════════════════════════════════════════════════════
    EVENT TYPES
    ═══════════════════════════════════════════════════════════════════════════ }
  TMPVLogEvent = procedure(Sender: TObject; const Msg: string) of object;
  TMPVStatusChangeEvent = procedure(Sender: TObject; OldStatus, NewStatus: TMPVStatus) of object;
  TMPVPositionChangeEvent = procedure(Sender: TObject; PositionSec: Double; PositionPercent: Double) of object;
  TMPVTrackChangeEvent = procedure(Sender: TObject; const TrackType: string; TrackID: Integer) of object;
  TMPVMetadataEvent = procedure(Sender: TObject; const Key, Value: string) of object;
  TMPVVideoResizeEvent = procedure(Sender: TObject; Width, Height: Integer) of object;
  TMPVProgressEvent = procedure(Sender: TObject; const Name, Value: string) of object;
  TMPVLoadNextFileEvent = procedure(Sender: TObject; var FileName: string) of object;
  TMPVFileLoadedEvent = procedure(Sender: TObject; var FileName: string) of object;
  TMPVEndFileEvent = procedure(Sender: TObject; Reason: Integer; ErrorCode: Integer) of object;

  { ═══════════════════════════════════════════════════════════════════════════
    EVENT THREAD CLASS
    ═══════════════════════════════════════════════════════════════════════════ }
  TMPVEventThread = class(TThread)
  private
    FEngine: TMPVEngine;
    FEvent: Pmpv_event;
    procedure SyncEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(AEngine: TMPVEngine);
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    MAIN ENGINE CLASS (equivalent to TMPlayer in mplayer.pas)
    ═══════════════════════════════════════════════════════════════════════════ }
  TMPVEngine = class
  private
    FHandle: Pmpv_handle;
    FStatus: TMPVStatus;
    FStreamInfo: TMPVStreamInfo;
    FRenderInfo: TMPVRenderInfo;
    FWindowHandle: THandle;
    FVideoWindowWidth: Integer;
    FVideoWindowHeight: Integer;
    {$IFDEF UNIX}
    FX11Display: PDisplay;
    FX11VideoWindow: TWindow;
    {$ENDIF}
    FInitialized: Boolean;
    FTerminating: Boolean;

    { Track lists }
    FAudioTracks: TMPVTrackList;
    FSubtitleTracks: TMPVTrackList;
    FVideoTracks: TMPVTrackList;

    { Playback state }
    FVolume: Integer;
    FMuted: Boolean;
    FSpeed: Double;
    FPosition: Double;
    FDuration: Double;
    FPercentPos: Double;
    FPaused: Boolean;

    { Video properties }
    FBrightness: Integer;
    FContrast: Integer;
    FSaturation: Integer;
    FHue: Integer;
    FGamma: Integer;
    FPropertyValues: array[0..MAX_PROP_ENTRYS] of TMPVPropertyValues;

    { Equalizer }
    FEqualizerEnabled: Boolean;
    FEqualizerBands: array[0..EQ_BANDS-1] of Double;
    FPreamp: Double;

    { Subtitles }
    FSubScale: Double;
    FSubDelay: Double;
    FSubVisible: Boolean;

    { Audio }
    FAudioDelay: Double;

    { Cache settings }
    FCacheSize: array[0..MAX_CACHE_ENTRYS] of Integer;

    { DVD/Bluray }
    FTitleCount: Integer;
    FChapterCount: Integer;
    FCurrentTitle: Integer;
    FCurrentChapter: Integer;

    { Media file }
    FMediaFile: string;
    FFileLoaded: string;

    { Aspect }
    FAspectMode: Integer;
    FAspectFactor: Double;

    { Video output }
    FVideoOutput: string;
    FAudioOutput: string;

    { Error tracking }
    FLastError: string;

    { Thread and sync }
    FEventThread: TMPVEventThread;
    FCriticalSection: TCriticalSection;
    FTimer: TTimer;

    { Deinterlace }
    FDeinterlace: Integer;
    FDeinterlaceAlg: Integer;

    { Hardware acceleration }
    FHWAccel: Boolean;

    { Events }
    FOnLog: TMPVLogEvent;
    FOnLogClear: TNotifyEvent;
    FOnStatusChange: TMPVStatusChangeEvent;
    FOnPositionChange: TMPVPositionChangeEvent;
    FOnAudioTrackChange: TMPVTrackChangeEvent;
    FOnSubtitleTrackChange: TMPVTrackChangeEvent;
    FOnVideoTrackChange: TMPVTrackChangeEvent;
    FOnMetadata: TMPVMetadataEvent;
    FOnVideoResize: TMPVVideoResizeEvent;
    FOnProgress: TMPVProgressEvent;
    FOnEndFile: TMPVEndFileEvent;
    FOnStartFile: TNotifyEvent;
    FOnFileLoaded: TMPVFileLoadedEvent;
    FOnSeek: TNotifyEvent;
    FOnPlaybackRestart: TNotifyEvent;
    FOnPropertysRead: TNotifyEvent;
    FOnLoadNextFile: TMPVLoadNextFileEvent;
    FOnChapterChange: TNotifyEvent;

    { Private methods - initialization }
    procedure InitializeDefaults;
    procedure ResetStreamInfo;
    procedure ResetRenderInfo;

    { Private methods - event handling }
    procedure HandleEvent(Event: Pmpv_event);
    procedure HandlePropertyChange(Prop: Pmpv_event_property; UserData: UInt64);
    procedure HandleLogMessage(Msg: Pmpv_event_log_message);
    procedure HandleEndFile(EndFile: Pmpv_event_end_file);
    procedure HandleStartFile;
    procedure HandleFileLoaded;

    { Private methods - track management }
    procedure UpdateTrackList;
    procedure ParseTrackList(Node: Pmpv_node);

    { Private methods - stream info }
    procedure UpdateStreamInfo;
    procedure UpdateMetadata;

    { Private methods - properties }
    procedure ObserveProperties;
    procedure UnobserveProperties;

    { Private methods - equalizer }
    function BuildEqualizerString: string;

    { Private methods - cache }
    function GetCacheForSource(const URL: string): Integer;
    function GetDriveType(const Path: string): Integer;

    { Private methods - events (DoOn* equivalents from mplayer.pas) }
    procedure DoLog(const Msg: string);
    procedure DoLogClear;
    procedure DoStatusChange(OldStatus, NewStatus: TMPVStatus);
    procedure DoPositionChange(PosSec, PosPct: Double);
    procedure DoAudioTrackChange(ID: Integer);
    procedure DoSubtitleTrackChange(ID: Integer);
    procedure DoVideoTrackChange(ID: Integer);
    procedure DoMetadata(const Key, Value: string);
    procedure DoVideoResize(Width, Height: Integer);
    procedure DoProgress(const Name, Value: string);

    { Private methods - setters }
    procedure SetStatus(Value: TMPVStatus);
    procedure SetVolume(Value: Integer);
    procedure SetMuted(Value: Boolean);
    procedure SetSpeed(Value: Double);
    procedure SetBrightness(Value: Integer);
    procedure SetContrast(Value: Integer);
    procedure SetSaturation(Value: Integer);
    procedure SetHue(Value: Integer);
    procedure SetGamma(Value: Integer);
    procedure SetSubScale(Value: Double);
    procedure SetSubDelay(Value: Double);
    procedure SetSubVisible(Value: Boolean);
    procedure SetAudioDelay(Value: Double);
    procedure SetAspectMode(Value: Integer);
    procedure SetAspectFactor(Value: Double);
    procedure SetDeinterlace(Value: Integer);
    procedure SetDeinterlaceAlg(Value: Integer);
    procedure SetHWAccel(Value: Boolean);
    procedure SetVideoOutput(const Value: string);

    { Private methods - cache getters/setters }
    function GetCacheSize(Index: Integer): Integer;
    procedure SetCacheSize(Index: Integer; Value: Integer);

    { Timer event }
    procedure OnTimerTick(Sender: TObject);

  public
    constructor Create;
    destructor Destroy; override;

    { ═══════════════════════════════════════════════════════════════════════════
      INITIALIZATION
      ═══════════════════════════════════════════════════════════════════════════ }
    function Initialize(AWindowHandle: THandle; AWidth: Integer = 0; AHeight: Integer = 0): Boolean;
    procedure Shutdown;
    procedure ResizeVideoWindow(AWidth, AHeight: Integer);
    function IsRunning: Boolean;

    { ═══════════════════════════════════════════════════════════════════════════
      PLAYBACK CONTROL
      ═══════════════════════════════════════════════════════════════════════════ }
    procedure PlayMedia(const URL: string);
    procedure CloseMedia;
    procedure Pause;
    procedure Resume;
    procedure Stop;
    procedure TogglePause;
    procedure SendPlayPause;

    { ═══════════════════════════════════════════════════════════════════════════
      SEEK FUNCTIONS
      ═══════════════════════════════════════════════════════════════════════════ }
    procedure SeekAbsolute(Seconds: Double);
    procedure SeekRelative(Seconds: Double);
    procedure SeekPercent(Percent: Double);
    procedure SeekBy(Value: Integer);
    procedure SeekTo(Value: Int64; Method: Integer);
    procedure FrameStep;
    procedure FrameBackStep;

    { ═══════════════════════════════════════════════════════════════════════════
      AUDIO CONTROL
      ═══════════════════════════════════════════════════════════════════════════ }
    procedure SetAudioTrack(TrackID: Integer);
    function GetAudioID: Integer;
    procedure SetAudioID(Value: Integer);

    { Equalizer }
    procedure SetEqualizerBand(Band: Integer; dB: Double);
    function GetEqualizerBand(Band: Integer): Double;
    procedure SetEqualizerPreset(const Values: string);
    function GetEqualizerPreset: string;
    procedure ResetEqualizer;
    procedure EnableEqualizer(Enable: Boolean);
    procedure SendAE(const Value: string);
    procedure SendAE2(const Value: string);
    function GetEqualizerFilterString: string;  { For lavfi-complex integration }
    procedure SetPreamp(dB: Double);
    procedure ApplyEqualizer;  { Call after changing bands to apply filter }
    property Preamp: Double read FPreamp write SetPreamp;

    { ═══════════════════════════════════════════════════════════════════════════
      SUBTITLE CONTROL
      ═══════════════════════════════════════════════════════════════════════════ }
    procedure SetSubtitleTrack(TrackID: Integer);
    function GetSubID: Integer;
    procedure SetSubID(Value: Integer);
    procedure LoadSubtitle(const FileName: string);
    procedure UnloadSubtitles;

    { ═══════════════════════════════════════════════════════════════════════════
      VIDEO CONTROL
      ═══════════════════════════════════════════════════════════════════════════ }
    procedure SetVideoTrack(TrackID: Integer);
    procedure SetAspectRatio(const Ratio: string);
    procedure Screenshot(const FileName: string = '');
    procedure ScreenshotToFile(const FileName: string; Mode: string = 'video');
    procedure SetScreenshotDirectory(const Path: string);
    procedure SetScreenshotFormat(const Format: string);
    procedure SetCacheSize(SizeKB: Integer);
    { Subtitle settings }
    procedure SetSubFont(const FontName: string);
    procedure SetSubFontSize(Size: Integer);
    procedure SetSubFontColor(Color: Cardinal);
    procedure SetSubBold(Bold: Boolean);
    procedure SetSubItalic(Italic: Boolean);
    procedure SetSubOutlineColor(Color: Cardinal);
    procedure SetSubOutlineSize(Size: Integer);
    procedure SetSubPosition(Position: Integer);
    procedure SetSubEncoding(const Encoding: string);
    procedure SetSubAutoLoad(AutoLoad: Boolean);
    { Audio settings }
    procedure SetAudioOutput(const Driver: string);
    procedure SetAudioDevice(const Device: string);
    procedure SetAudioNormalize(Enable: Boolean);
    procedure SetAudioChannels(Channels: Integer);
    function GetAudioDeviceList: TStringList;
    procedure ReloadCurrentFile;
    procedure ToggleFullscreen;
    procedure SetProperty(Prop: Integer; Value: Integer);
    function GetValidFileInfo: Boolean;

    { ═══════════════════════════════════════════════════════════════════════════
      DVD/BLURAY NAVIGATION
      ═══════════════════════════════════════════════════════════════════════════ }
    function IsDVD: Boolean;
    function IsDVDNav: Boolean;
    function IsBluray: Boolean;
    function DVDGoMenu: Boolean;
    function DVDMenuClick: Boolean;
    function DVDMenuPosition(X, Y: Integer): Boolean;
    procedure DVDMenu;
    procedure DVDMenuSelect;
    procedure DVDMenuUp;
    procedure DVDMenuDown;
    procedure DVDMenuLeft;
    procedure DVDMenuRight;
    procedure DVDTitle(TitleID: Integer);
    procedure DVDChapter(ChapterID: Integer);
    procedure DVDNextChapter;
    procedure DVDPrevChapter;
    function GetTitleID: Integer;
    function GetChapterID: Integer;
    procedure SetTitleID(Value: Integer);
    procedure SetChapterID(Value: Integer);

    { ═══════════════════════════════════════════════════════════════════════════
      GENERIC PROPERTY ACCESS
      ═══════════════════════════════════════════════════════════════════════════ }
    procedure SendCommand(const Args: array of string);
    procedure SendCmd(const Command: string);
    procedure SetOption(const Name, Value: string);
    function GetPropertyString(const Name: string): string;
    function GetPropertyDouble(const Name: string): Double;
    function GetPropertyInt(const Name: string): Int64;
    function GetPropertyBool(const Name: string): Boolean;
    procedure SetPropertyString(const Name, Value: string);
    procedure SetPropertyDouble(const Name: string; Value: Double);
    procedure SetPropertyInt(const Name: string; Value: Int64);
    procedure SetPropertyBool(const Name: string; Value: Boolean);
    function GetPropertyValues(Index: Integer): TMPVPropertyValues;
    function AspectPresetValue: Double;
    function DeinterlaceCmd(Mode, Alg: Integer; DeintDVD, AIsDvd, Interlaced: Boolean): Integer; overload;
    function DeinterlaceCmd(Mode, Alg: Integer; DeintDVD: Boolean): Integer; overload;
    function CacheSizeCmd(const sFile: string): Integer;

    { ═══════════════════════════════════════════════════════════════════════════
      TIMED TASKS
      ═══════════════════════════════════════════════════════════════════════════ }
    procedure TimedTasks;
    procedure QueryPosition;
    procedure StartPropertyChange;
    procedure EndPropertyChange;
    procedure ResetLogLastMsg;

    { ═══════════════════════════════════════════════════════════════════════════
      READ-ONLY PROPERTIES
      ═══════════════════════════════════════════════════════════════════════════ }
    property Handle: Pmpv_handle read FHandle;
    property Status: TMPVStatus read FStatus;
    property StreamInfo: TMPVStreamInfo read FStreamInfo;
    property RenderInfo: TMPVRenderInfo read FRenderInfo;
    property AudioTracks: TMPVTrackList read FAudioTracks;
    property SubtitleTracks: TMPVTrackList read FSubtitleTracks;
    property VideoTracks: TMPVTrackList read FVideoTracks;
    property Position: Double read FPosition;
    property Duration: Double read FDuration;
    property PercentPos: Double read FPercentPos;
    property TitleCount: Integer read FTitleCount;
    property ChapterCount: Integer read FChapterCount;
    property CurrentTitle: Integer read FCurrentTitle;
    property CurrentChapter: Integer read FCurrentChapter;
    property MediaFile: string read FMediaFile;
    property FileLoaded: string read FFileLoaded;
    property Running: Boolean read IsRunning;
    property Initialized: Boolean read FInitialized;
    property LastError: string read FLastError;

    { ═══════════════════════════════════════════════════════════════════════════
      READ-WRITE PROPERTIES
      ═══════════════════════════════════════════════════════════════════════════ }
    property Volume: Integer read FVolume write SetVolume;
    property Muted: Boolean read FMuted write SetMuted;
    property Speed: Double read FSpeed write SetSpeed;
    property Brightness: Integer read FBrightness write SetBrightness;
    property Contrast: Integer read FContrast write SetContrast;
    property Saturation: Integer read FSaturation write SetSaturation;
    property Hue: Integer read FHue write SetHue;
    property Gamma: Integer read FGamma write SetGamma;
    property SubScale: Double read FSubScale write SetSubScale;
    property SubDelay: Double read FSubDelay write SetSubDelay;
    property SubVisible: Boolean read FSubVisible write SetSubVisible;
    property AudioDelay: Double read FAudioDelay write SetAudioDelay;
    property EqualizerEnabled: Boolean read FEqualizerEnabled write EnableEqualizer;
    property WindowHandle: THandle read FWindowHandle;
    property AspectMode: Integer read FAspectMode write SetAspectMode;
    property AspectFactor: Double read FAspectFactor write SetAspectFactor;
    property Aspect: Integer read FAspectMode write SetAspectMode;
    property Deinterlace: Integer read FDeinterlace write SetDeinterlace;
    property DeinterlaceAlg: Integer read FDeinterlaceAlg write SetDeinterlaceAlg;
    property HWAccel: Boolean read FHWAccel write SetHWAccel;
    property VideoOutput: string read FVideoOutput write SetVideoOutput;
    property AudioOutput: string read FAudioOutput write FAudioOutput;

    { Cache by type }
    property CacheSize[Index: Integer]: Integer read GetCacheSize write SetCacheSize;

    { Track IDs }
    property AudioID: Integer read GetAudioID write SetAudioID;
    property SubID: Integer read GetSubID write SetSubID;
    property TitleID: Integer read GetTitleID write SetTitleID;
    property ChapterID: Integer read GetChapterID write SetChapterID;
    property ValidFileInfo: Boolean read GetValidFileInfo;

    { ═══════════════════════════════════════════════════════════════════════════
      EVENTS
      ═══════════════════════════════════════════════════════════════════════════ }
    property OnLog: TMPVLogEvent read FOnLog write FOnLog;
    property OnLogClear: TNotifyEvent read FOnLogClear write FOnLogClear;
    property OnStatusChange: TMPVStatusChangeEvent read FOnStatusChange write FOnStatusChange;
    property OnPositionChange: TMPVPositionChangeEvent read FOnPositionChange write FOnPositionChange;
    property OnAudioTrackChange: TMPVTrackChangeEvent read FOnAudioTrackChange write FOnAudioTrackChange;
    property OnSubtitleTrackChange: TMPVTrackChangeEvent read FOnSubtitleTrackChange write FOnSubtitleTrackChange;
    property OnVideoTrackChange: TMPVTrackChangeEvent read FOnVideoTrackChange write FOnVideoTrackChange;
    property OnMetadata: TMPVMetadataEvent read FOnMetadata write FOnMetadata;
    property OnVideoResize: TMPVVideoResizeEvent read FOnVideoResize write FOnVideoResize;
    property OnProgress: TMPVProgressEvent read FOnProgress write FOnProgress;
    property OnEndFile: TMPVEndFileEvent read FOnEndFile write FOnEndFile;
    property OnStartFile: TNotifyEvent read FOnStartFile write FOnStartFile;
    property OnFileLoaded: TMPVFileLoadedEvent read FOnFileLoaded write FOnFileLoaded;
    property OnSeek: TNotifyEvent read FOnSeek write FOnSeek;
    property OnPlaybackRestart: TNotifyEvent read FOnPlaybackRestart write FOnPlaybackRestart;
    property OnPropertysRead: TNotifyEvent read FOnPropertysRead write FOnPropertysRead;
    property OnLoadNextFile: TMPVLoadNextFileEvent read FOnLoadNextFile write FOnLoadNextFile;
    property OnChapterChange: TNotifyEvent read FOnChapterChange write FOnChapterChange;
  end;

implementation

uses
  Math;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVTrackList
  ═══════════════════════════════════════════════════════════════════════════════ }

constructor TMPVTrackList.Create(const ATrackType: string);
begin
  inherited Create;
  FTrackType := ATrackType;
  FSelectedID := -1;
  FCount := 0;
  SetLength(FItems, 0);
end;

destructor TMPVTrackList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TMPVTrackList.Clear;
begin
  SetLength(FItems, 0);
  FCount := 0;
  FSelectedID := -1;
end;

function TMPVTrackList.Add(const Track: TMPVTrackInfo): Integer;
begin
  SetLength(FItems, FCount + 1);
  FItems[FCount] := Track;
  Result := FCount;
  Inc(FCount);
end;

function TMPVTrackList.FindByID(ID: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FCount - 1 do
    if FItems[I].ID = ID then
    begin
      Result := I;
      Exit;
    end;
end;

function TMPVTrackList.GetSelectedIndex: Integer;
begin
  Result := FindByID(FSelectedID);
end;

function TMPVTrackList.GetItem(Index: Integer): TMPVTrackInfo;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FItems[Index]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEventThread
  ═══════════════════════════════════════════════════════════════════════════════ }

constructor TMPVEventThread.Create(AEngine: TMPVEngine);
begin
  inherited Create(True);
  FEngine := AEngine;
  FEvent := nil;
  FreeOnTerminate := False;
end;

procedure TMPVEventThread.SyncEvent;
begin
  if (FEngine <> nil) and (FEvent <> nil) then
    FEngine.HandleEvent(FEvent);
end;

procedure TMPVEventThread.Execute;
var
  Evt: Pmpv_event;
begin
  while not Terminated do
  begin
    { Exit immediately if terminating }
    if (FEngine = nil) or (FEngine.FHandle = nil) or FEngine.FTerminating then
      Break;

    Evt := mpv_wait_event(FEngine.FHandle, 0.1);
    FEvent := Evt;

    { Don't synchronize if we're terminating }
    if Terminated or FEngine.FTerminating then
      Break;

    if (Evt <> nil) and (Evt^.event_id <> MPV_EVENT_NONE) then
      Synchronize(@SyncEvent);
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Constructor/Destructor
  ═══════════════════════════════════════════════════════════════════════════════ }

constructor TMPVEngine.Create;
begin
  inherited Create;
  FHandle := nil;
  FInitialized := False;
  FTerminating := False;
  FWindowHandle := 0;

  FCriticalSection := TCriticalSection.Create;

  FAudioTracks := TMPVTrackList.Create('audio');
  FSubtitleTracks := TMPVTrackList.Create('sub');
  FVideoTracks := TMPVTrackList.Create('video');

  FEventThread := nil;
  FTimer := nil;

  InitializeDefaults;
end;

destructor TMPVEngine.Destroy;
begin
  Shutdown;

  FreeAndNil(FTimer);
  FreeAndNil(FAudioTracks);
  FreeAndNil(FSubtitleTracks);
  FreeAndNil(FVideoTracks);
  FreeAndNil(FCriticalSection);

  inherited;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Initialization
  ═══════════════════════════════════════════════════════════════════════════════ }

{ ───────────────────────────────────────────────────────────────────────────
  InitializeDefaults - Reset all engine state to default values

  Purpose: Initializes all internal state variables to their default values.
           Called during engine creation and when resetting the player state.

  Notes:
    - Video properties (brightness, contrast, etc.) default to VIDEO_PROP_DEFAULT (0)
    - Cache sizes are configured per source type for optimal streaming performance:
      * CACHE_TYPE_INTERNET: Larger buffer for network latency compensation
      * CACHE_TYPE_DVD: Sized for optical disc read-ahead
      * CACHE_TYPE_NETWORK: For SMB/CIFS shares
    - Hardware acceleration is enabled by default (FHWAccel := True)
  ─────────────────────────────────────────────────────────────────────────── }
procedure TMPVEngine.InitializeDefaults;
var
  I: Integer;
begin
  FStatus := msNone;
  FVolume := VOL_DEFAULT;
  FMuted := False;
  FSpeed := SPEED_DEFAULT;
  FPosition := 0;
  FDuration := 0;
  FPercentPos := 0;
  FPaused := False;

  FBrightness := VIDEO_PROP_DEFAULT;
  FContrast := VIDEO_PROP_DEFAULT;
  FSaturation := VIDEO_PROP_DEFAULT;
  FHue := VIDEO_PROP_DEFAULT;
  FGamma := VIDEO_PROP_DEFAULT;

  for I := 0 to MAX_PROP_ENTRYS do
  begin
    FPropertyValues[I].Default := VIDEO_PROP_DEFAULT;
    FPropertyValues[I].Valid := False;
    FPropertyValues[I].Value := VIDEO_PROP_DEFAULT;
  end;

  FEqualizerEnabled := False;
  for I := 0 to EQ_BANDS - 1 do
    FEqualizerBands[I] := 0.0;
  FPreamp := 0.0;

  FSubScale := SUB_SCALE_DEFAULT;
  FSubDelay := SUB_DELAY_DEFAULT;
  FSubVisible := True;
  FAudioDelay := AUDIO_DELAY_DEFAULT;

  FCacheSize[CACHE_TYPE_DEFAULT] := DEFAULT_CACHE_DEFAULT;
  FCacheSize[CACHE_TYPE_FIXED] := DEFAULT_CACHE_FIXED;
  FCacheSize[CACHE_TYPE_RAMDISK] := DEFAULT_CACHE_RAMDISK;
  FCacheSize[CACHE_TYPE_CDROM] := DEFAULT_CACHE_CDROM;
  FCacheSize[CACHE_TYPE_REMOVABLE] := DEFAULT_CACHE_REMOVABLE;
  FCacheSize[CACHE_TYPE_NETWORK] := DEFAULT_CACHE_NETWORK;
  FCacheSize[CACHE_TYPE_INTERNET] := DEFAULT_CACHE_INTERNET;
  FCacheSize[CACHE_TYPE_DVD] := DEFAULT_CACHE_DVD;

  FTitleCount := 0;
  FChapterCount := 0;
  FCurrentTitle := 0;
  FCurrentChapter := 0;

  FMediaFile := '';
  FFileLoaded := '';

  FAspectMode := ASPECT_AUTO;
  FAspectFactor := -1;

  FVideoOutput := VO_DEFAULT;
  FAudioOutput := AO_DEFAULT;

  FDeinterlace := DEINT_AUTO;
  FDeinterlaceAlg := DEINT_ALG_AUTO;
  FHWAccel := True;  { Enable hardware acceleration by default }

  ResetStreamInfo;
  ResetRenderInfo;
end;

procedure TMPVEngine.ResetStreamInfo;
var
  I: Integer;
begin
  FillChar(FStreamInfo, SizeOf(FStreamInfo), 0);
  FStreamInfo.Valid := False;
  FStreamInfo.DurationSec := 0;
  FStreamInfo.DurationString := '00:00:00';
  for I := 0 to 15 do
  begin
    FStreamInfo.Metadata[I].Key := '';
    FStreamInfo.Metadata[I].Value := '';
  end;
  FStreamInfo.MetadataCount := 0;
end;

procedure TMPVEngine.ResetRenderInfo;
begin
  FillChar(FRenderInfo, SizeOf(FRenderInfo), 0);
  FRenderInfo.Aspect := -1;
end;

{ ───────────────────────────────────────────────────────────────────────────
  Initialize - Initialize the MPV engine and attach to a window

  Purpose: Creates and configures the mpv player instance, attaching video
           output to the specified window handle.

  Parameters:
    - AWindowHandle: Native window handle for video rendering
      * On Qt5/Linux: TQtWidget that will be converted to X11 Window ID
      * On Windows: Direct HWND handle
    - AWidth, AHeight: Initial dimensions for the video output area

  Returns: True if initialization succeeded, False otherwise (check FLastError)

  Platform-specific behavior:
    - Unix: Sets LC_NUMERIC to 'C' locale (required by mpv for number parsing)
    - Qt5: Extracts X11 Window ID from Qt widget via QWidget_winId()
    - Windows: Uses window handle directly

  MPV options configured:
    - input-default-bindings=no: Disable mpv's default keyboard shortcuts
    - input-vo-keyboard=no: Don't capture keyboard in video output
    - keep-open=yes: Keep player open after playback ends
    - idle=yes: Allow player to stay idle without media
    - hwdec=auto: Enable hardware acceleration when available

  Notes:
    - Starts the event processing thread (TMPVEventThread)
    - Starts a 100ms timer for periodic updates (position, etc.)
    - Observes mpv properties for change notifications
  ─────────────────────────────────────────────────────────────────────────── }
function TMPVEngine.Initialize(AWindowHandle: THandle; AWidth: Integer; AHeight: Integer): Boolean;
var
  WIDStr: string;
  Err: Integer;
  {$IFDEF LCLQT5}
  QtWidget: TQtWidget;
  X11WinID: PtrUInt;
  {$ENDIF}
begin
  Result := False;
  FLastError := '';

  if FInitialized then
  begin
    Result := True;
    Exit;
  end;

  {$IFDEF WINDOWS}
  { Mask FPU exceptions - libmpv can trigger invalid floating point operations.
    We keep them masked for the entire session as mpv/ffmpeg may trigger them during playback. }
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  {$ENDIF}

  { Store dimensions }
  FVideoWindowWidth := AWidth;
  FVideoWindowHeight := AHeight;
  {$IFDEF UNIX}
  FX11Display := nil;
  FX11VideoWindow := 0;
  {$ENDIF}

  { Set C locale for numeric parsing (required by mpv) }
  {$IFDEF UNIX}
  setlocale(LC_NUMERIC, 'C');
  {$ENDIF}

  if not LoadLibMPV then
  begin
    FLastError := 'Failed to load libmpv library (' + LIBMPV_DLL + ')';
    DoLog('Error: ' + FLastError);
    Exit;
  end;

  FWindowHandle := AWindowHandle;

  FHandle := mpv_create();
  if FHandle = nil then
  begin
    FLastError := 'mpv_create() returned nil';
    DoLog('Error: ' + FLastError);
    Exit;
  end;

  { Set window ID for video output }
  WriteToFileLog('[MPV Init] FWindowHandle = ' + IntToHex(FWindowHandle, 16));
  WriteToFileLog('[MPV Init] Requested size = ' + IntToStr(AWidth) + 'x' + IntToStr(AHeight));

  if FWindowHandle <> 0 then
  begin
    {$IFDEF LCLQT5}
    { On Qt5, Handle is a TQtWidget - we need to get the X11 Window ID via QWidget_winId }
    QtWidget := TQtWidget(FWindowHandle);
    if QtWidget <> nil then
    begin
      X11WinID := QWidget_winId(QtWidget.Widget);
      WriteToFileLog('[MPV Init] Qt5 Widget winId = ' + IntToStr(X11WinID));
      if X11WinID <> 0 then
        WIDStr := IntToStr(Int64(X11WinID))
      else
      begin
        WriteToFileLog('[MPV Init] ERROR: Qt5 winId is 0');
        WIDStr := '';
      end;
    end
    else
    begin
      WriteToFileLog('[MPV Init] ERROR: Qt5 widget is nil');
      WIDStr := '';
    end;
    {$ELSE}
    { Windows or other platforms - use handle directly }
    WIDStr := IntToStr(Int64(FWindowHandle));
    WriteToFileLog('[MPV Init] Setting wid=' + WIDStr);
    DoLog('Setting wid=' + WIDStr + ' (handle=' + IntToHex(FWindowHandle, 16) + ')');
    {$ENDIF}
    if WIDStr <> '' then
    begin
      WriteToFileLog('[MPV Init] Calling mpv_set_option_string for wid=' + WIDStr);
      mpv_set_option_string(FHandle, 'wid', PAnsiChar(AnsiString(WIDStr)));
    end;
  end
  else
  begin
    WriteToFileLog('[MPV Init] WARNING: No window handle provided');
    DoLog('Warning: No window handle provided for video output');
  end;

  { Set default options }
  mpv_set_option_string(FHandle, 'input-default-bindings', 'no');
  mpv_set_option_string(FHandle, 'input-vo-keyboard', 'no');
  mpv_set_option_string(FHandle, 'input-cursor', 'no');  { Pass mouse events to parent window }
  mpv_set_option_string(FHandle, 'input-media-keys', 'no');
  mpv_set_option_string(FHandle, 'cursor-autohide', 'no');  { Don't hide cursor - let app handle it }
  mpv_set_option_string(FHandle, 'osc', 'no');
  mpv_set_option_string(FHandle, 'keep-open', 'yes');
  mpv_set_option_string(FHandle, 'idle', 'yes');
  mpv_set_option_string(FHandle, 'force-window', 'no');
  { Force video to fit within parent window bounds }
  mpv_set_option_string(FHandle, 'auto-window-resize', 'no');
  mpv_set_option_string(FHandle, 'keepaspect-window', 'no');

  { Set video output - 'auto' or empty means let mpv choose }
  if (FVideoOutput <> '') and (FVideoOutput <> 'auto') then
  begin
    DoLog('Setting vo=' + FVideoOutput);
    mpv_set_option_string(FHandle, 'vo', PAnsiChar(AnsiString(FVideoOutput)));
  end
  else
    DoLog('Video output: auto (mpv default)');

  { Set audio output - 'auto' or empty means let mpv choose }
  if (FAudioOutput <> '') and (FAudioOutput <> 'auto') then
    mpv_set_option_string(FHandle, 'ao', PAnsiChar(AnsiString(FAudioOutput)));

  { Set hardware acceleration }
  if FHWAccel then
    mpv_set_option_string(FHandle, 'hwdec', 'auto')
  else
    mpv_set_option_string(FHandle, 'hwdec', 'no');

  { Enable log messages }
  mpv_request_log_messages(FHandle, 'info');

  { Initialize mpv }
  Err := mpv_initialize(FHandle);
  if Err < 0 then
  begin
    FLastError := 'mpv_initialize() failed: ' + MPVErrorToStr(Err);
    DoLog('Error: ' + FLastError);
    mpv_destroy(FHandle);
    FHandle := nil;
    Exit;
  end;

  { Observe properties for changes }
  ObserveProperties;

  { Start event thread }
  FEventThread := TMPVEventThread.Create(Self);
  FEventThread.Start;

  { Start timer for timed tasks }
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 100;
  FTimer.OnTimer := @OnTimerTick;
  FTimer.Enabled := True;

  FInitialized := True;
  FTerminating := False;
  Result := True;

  DoLog('MPV Engine initialized. API version: ' + GetLibMPVVersion);
end;

procedure TMPVEngine.Shutdown;
var
  QuitArgs: array[0..1] of PAnsiChar;
begin
  if not FInitialized then
    Exit;

  FTerminating := True;

  if FTimer <> nil then
  begin
    FTimer.Enabled := False;
    FreeAndNil(FTimer);
  end;

  { Send quit command to MPV - speeds up shutdown }
  if FHandle <> nil then
  begin
    QuitArgs[0] := 'quit';
    QuitArgs[1] := nil;
    mpv_command_async(FHandle, 0, @QuitArgs[0]);
  end;

  if FEventThread <> nil then
  begin
    FEventThread.Terminate;
    FEventThread.WaitFor;
    FreeAndNil(FEventThread);
  end;

  if FHandle <> nil then
  begin
    UnobserveProperties;
    mpv_terminate_destroy(FHandle);
    FHandle := nil;
  end;

  FInitialized := False;
  SetStatus(msNone);

  FAudioTracks.Clear;
  FSubtitleTracks.Clear;
  FVideoTracks.Clear;

  { Destroy X11 video subwindow if created }
  {$IFDEF UNIX}
  if (FX11Display <> nil) and (FX11VideoWindow <> 0) then
  begin
    XDestroyWindow(FX11Display, FX11VideoWindow);
    FX11VideoWindow := 0;
  end;
  FX11Display := nil;
  {$ENDIF}

  InitializeDefaults;
end;

procedure TMPVEngine.ResizeVideoWindow(AWidth, AHeight: Integer);
begin
  if (AWidth <= 0) or (AHeight <= 0) then Exit;
  if (AWidth = FVideoWindowWidth) and (AHeight = FVideoWindowHeight) then Exit;

  FVideoWindowWidth := AWidth;
  FVideoWindowHeight := AHeight;

  {$IFDEF UNIX}
  if (FX11Display <> nil) and (FX11VideoWindow <> 0) then
  begin
    XResizeWindow(FX11Display, FX11VideoWindow, AWidth, AHeight);
    XSync(FX11Display, False);
    WriteToFileLog('[MPV] Resized X11 video window to ' + IntToStr(AWidth) + 'x' + IntToStr(AHeight));
  end;
  {$ENDIF}
end;

function TMPVEngine.IsRunning: Boolean;
begin
  Result := FInitialized and (FHandle <> nil) and
            (FStatus in [msOpening, msPlayStarting, msPlaying, msPaused]);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Property Observation
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TMPVEngine.ObserveProperties;
begin
  if FHandle = nil then Exit;

  mpv_observe_property(FHandle, OBS_TIME_POS, 'time-pos', MPV_FORMAT_DOUBLE);
  mpv_observe_property(FHandle, OBS_PERCENT_POS, 'percent-pos', MPV_FORMAT_DOUBLE);
  mpv_observe_property(FHandle, OBS_DURATION, 'duration', MPV_FORMAT_DOUBLE);
  mpv_observe_property(FHandle, OBS_PAUSE, 'pause', MPV_FORMAT_FLAG);
  mpv_observe_property(FHandle, OBS_VOLUME, 'volume', MPV_FORMAT_DOUBLE);
  mpv_observe_property(FHandle, OBS_MUTE, 'mute', MPV_FORMAT_FLAG);
  mpv_observe_property(FHandle, OBS_SPEED, 'speed', MPV_FORMAT_DOUBLE);
  mpv_observe_property(FHandle, OBS_TRACK_LIST, 'track-list', MPV_FORMAT_NODE);
  mpv_observe_property(FHandle, OBS_METADATA, 'metadata', MPV_FORMAT_NODE);
  mpv_observe_property(FHandle, OBS_CHAPTER, 'chapter', MPV_FORMAT_INT64);
  mpv_observe_property(FHandle, OBS_CHAPTERS, 'chapters', MPV_FORMAT_INT64);
  mpv_observe_property(FHandle, OBS_AID, 'aid', MPV_FORMAT_INT64);
  mpv_observe_property(FHandle, OBS_SID, 'sid', MPV_FORMAT_INT64);
  mpv_observe_property(FHandle, OBS_VID, 'vid', MPV_FORMAT_INT64);
  mpv_observe_property(FHandle, OBS_WIDTH, 'width', MPV_FORMAT_INT64);
  mpv_observe_property(FHandle, OBS_HEIGHT, 'height', MPV_FORMAT_INT64);
  mpv_observe_property(FHandle, OBS_DWIDTH, 'dwidth', MPV_FORMAT_INT64);
  mpv_observe_property(FHandle, OBS_DHEIGHT, 'dheight', MPV_FORMAT_INT64);
  mpv_observe_property(FHandle, OBS_CORE_IDLE, 'core-idle', MPV_FORMAT_FLAG);
  mpv_observe_property(FHandle, OBS_SEEKABLE, 'seekable', MPV_FORMAT_FLAG);
  mpv_observe_property(FHandle, OBS_FILENAME, 'filename', MPV_FORMAT_STRING);
  mpv_observe_property(FHandle, OBS_MEDIA_TITLE, 'media-title', MPV_FORMAT_STRING);
  mpv_observe_property(FHandle, OBS_FILE_FORMAT, 'file-format', MPV_FORMAT_STRING);
  mpv_observe_property(FHandle, OBS_ICY_METADATA, 'icy-metadata', MPV_FORMAT_NODE);
  mpv_observe_property(FHandle, OBS_EOF_REACHED, 'eof-reached', MPV_FORMAT_FLAG);
end;

procedure TMPVEngine.UnobserveProperties;
begin
  if FHandle = nil then Exit;

  mpv_unobserve_property(FHandle, OBS_TIME_POS);
  mpv_unobserve_property(FHandle, OBS_PERCENT_POS);
  mpv_unobserve_property(FHandle, OBS_DURATION);
  mpv_unobserve_property(FHandle, OBS_PAUSE);
  mpv_unobserve_property(FHandle, OBS_VOLUME);
  mpv_unobserve_property(FHandle, OBS_MUTE);
  mpv_unobserve_property(FHandle, OBS_SPEED);
  mpv_unobserve_property(FHandle, OBS_TRACK_LIST);
  mpv_unobserve_property(FHandle, OBS_METADATA);
  mpv_unobserve_property(FHandle, OBS_CHAPTER);
  mpv_unobserve_property(FHandle, OBS_CHAPTERS);
  mpv_unobserve_property(FHandle, OBS_AID);
  mpv_unobserve_property(FHandle, OBS_SID);
  mpv_unobserve_property(FHandle, OBS_VID);
  mpv_unobserve_property(FHandle, OBS_WIDTH);
  mpv_unobserve_property(FHandle, OBS_HEIGHT);
  mpv_unobserve_property(FHandle, OBS_DWIDTH);
  mpv_unobserve_property(FHandle, OBS_DHEIGHT);
  mpv_unobserve_property(FHandle, OBS_CORE_IDLE);
  mpv_unobserve_property(FHandle, OBS_SEEKABLE);
  mpv_unobserve_property(FHandle, OBS_FILENAME);
  mpv_unobserve_property(FHandle, OBS_MEDIA_TITLE);
  mpv_unobserve_property(FHandle, OBS_FILE_FORMAT);
  mpv_unobserve_property(FHandle, OBS_ICY_METADATA);
  mpv_unobserve_property(FHandle, OBS_EOF_REACHED);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Event Handling
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TMPVEngine.HandleEvent(Event: Pmpv_event);
begin
  if Event = nil then Exit;

  case Event^.event_id of
    MPV_EVENT_LOG_MSG:
      if Event^.data <> nil then
        HandleLogMessage(Pmpv_event_log_message(Event^.data));

    MPV_EVENT_START_FILE:
      HandleStartFile;

    MPV_EVENT_FILE_LOADED:
      HandleFileLoaded;

    MPV_EVENT_END_FILE:
      if Event^.data <> nil then
        HandleEndFile(Pmpv_event_end_file(Event^.data));

    MPV_EVENT_PROPERTY_CHANGE:
      if Event^.data <> nil then
        HandlePropertyChange(Pmpv_event_property(Event^.data), Event^.reply_userdata);

    MPV_EVENT_VIDEO_RECONFIG:
      begin
        UpdateStreamInfo;
        DoVideoResize(FStreamInfo.Video.Width, FStreamInfo.Video.Height);
      end;

    MPV_EVENT_AUDIO_RECONFIG:
      UpdateStreamInfo;

    MPV_EVENT_SEEK:
      if Assigned(FOnSeek) then
        FOnSeek(Self);

    MPV_EVENT_PLAYBACK_RESTART:
      begin
        if FStatus = msPlayStarting then
          SetStatus(msPlaying);
        if Assigned(FOnPlaybackRestart) then
          FOnPlaybackRestart(Self);
      end;

    MPV_EVENT_SHUTDOWN:
      SetStatus(msNone);

    MPV_EVENT_IDLE:
      if FStatus = msPlaying then
        SetStatus(msStopped);
  end;
end;

{ ───────────────────────────────────────────────────────────────────────────
  HandlePropertyChange - Process mpv property change notifications

  Purpose: Dispatches property change events from mpv to update internal state.
           Called by the event thread when MPV_EVENT_PROPERTY_CHANGE occurs.

  Parameters:
    - Prop: Pointer to mpv_event_property containing the changed property data
    - UserData: Observer ID (OBS_* constant) identifying which property changed

  Property observers handled (20+ properties):
    - OBS_TIME_POS, OBS_PERCENT_POS: Playback position updates
    - OBS_DURATION: Media duration (converted to hh:nn:ss via SecsPerDay)
    - OBS_PAUSE: Pause state changes -> triggers status update
    - OBS_VOLUME, OBS_MUTE, OBS_SPEED: Audio properties
    - OBS_TRACK_LIST: Available tracks -> calls ParseTrackList()
    - OBS_CHAPTER, OBS_CHAPTERS: Chapter navigation
    - OBS_AID, OBS_SID, OBS_VID: Selected audio/subtitle/video track IDs
    - OBS_WIDTH, OBS_HEIGHT, OBS_DWIDTH, OBS_DHEIGHT: Video dimensions
    - OBS_SEEKABLE: Whether media supports seeking
    - OBS_FILENAME, OBS_FILE_FORMAT: Media file information
    - OBS_MEDIA_TITLE, OBS_ICY_METADATA: Stream metadata (radio stations)
    - OBS_EOF_REACHED: End of file detection

  Data format handling:
    - MPV_FORMAT_DOUBLE: For time positions, volume, speed (PDouble)
    - MPV_FORMAT_INT64: For track IDs, dimensions (PInt64)
    - MPV_FORMAT_FLAG: For boolean states like pause, mute (PInteger, 0/1)
    - MPV_FORMAT_STRING: For filenames, codec names (PPAnsiChar)
    - MPV_FORMAT_NODE: For complex data like track-list, metadata

  Notes:
    - Duration is divided by SecsPerDay for FormatDateTime compatibility
    - String properties use double indirection (PPAnsiChar) per mpv API
  ─────────────────────────────────────────────────────────────────────────── }
procedure TMPVEngine.HandlePropertyChange(Prop: Pmpv_event_property; UserData: UInt64);
var
  DblVal: Double;
  IntVal: Int64;
  FlagVal: Integer;
  StrVal: PAnsiChar;
begin
  if Prop = nil then Exit;

  case UserData of
    OBS_TIME_POS:
      if (Prop^.format = MPV_FORMAT_DOUBLE) and (Prop^.data <> nil) then
      begin
        DblVal := PDouble(Prop^.data)^;
        FPosition := DblVal;
        DoPositionChange(FPosition, FPercentPos);
      end;

    OBS_PERCENT_POS:
      if (Prop^.format = MPV_FORMAT_DOUBLE) and (Prop^.data <> nil) then
      begin
        DblVal := PDouble(Prop^.data)^;
        FPercentPos := DblVal;
        DoPositionChange(FPosition, FPercentPos);
      end;

    OBS_DURATION:
      if (Prop^.format = MPV_FORMAT_DOUBLE) and (Prop^.data <> nil) then
      begin
        DblVal := PDouble(Prop^.data)^;
        FDuration := DblVal;
        FStreamInfo.DurationSec := FDuration;
        FStreamInfo.DurationString := FormatDateTime('hh:nn:ss',
          FDuration / SecsPerDay);
      end;

    OBS_PAUSE:
      if (Prop^.format = MPV_FORMAT_FLAG) and (Prop^.data <> nil) then
      begin
        FlagVal := PInteger(Prop^.data)^;
        FPaused := FlagVal <> 0;
        if FPaused then
          SetStatus(msPaused)
        else if FStatus = msPaused then
          SetStatus(msPlaying);
      end;

    OBS_VOLUME:
      if (Prop^.format = MPV_FORMAT_DOUBLE) and (Prop^.data <> nil) then
      begin
        DblVal := PDouble(Prop^.data)^;
        FVolume := Round(DblVal);
      end;

    OBS_MUTE:
      if (Prop^.format = MPV_FORMAT_FLAG) and (Prop^.data <> nil) then
      begin
        FlagVal := PInteger(Prop^.data)^;
        FMuted := FlagVal <> 0;
      end;

    OBS_SPEED:
      if (Prop^.format = MPV_FORMAT_DOUBLE) and (Prop^.data <> nil) then
      begin
        DblVal := PDouble(Prop^.data)^;
        FSpeed := DblVal;
      end;

    OBS_TRACK_LIST:
      if Prop^.format = MPV_FORMAT_NODE then
        ParseTrackList(Pmpv_node(Prop^.data));

    OBS_CHAPTER:
      if (Prop^.format = MPV_FORMAT_INT64) and (Prop^.data <> nil) then
      begin
        IntVal := PInt64(Prop^.data)^;
        FCurrentChapter := IntVal;
        FRenderInfo.ChapterID := IntVal;
        if Assigned(FOnChapterChange) then
          FOnChapterChange(Self);
      end;

    OBS_CHAPTERS:
      if (Prop^.format = MPV_FORMAT_INT64) and (Prop^.data <> nil) then
      begin
        IntVal := PInt64(Prop^.data)^;
        FChapterCount := IntVal;
      end;

    OBS_AID:
      if (Prop^.format = MPV_FORMAT_INT64) and (Prop^.data <> nil) then
      begin
        IntVal := PInt64(Prop^.data)^;
        FAudioTracks.SelectedID := IntVal;
        DoAudioTrackChange(IntVal);
      end;

    OBS_SID:
      if (Prop^.format = MPV_FORMAT_INT64) and (Prop^.data <> nil) then
      begin
        IntVal := PInt64(Prop^.data)^;
        FSubtitleTracks.SelectedID := IntVal;
        DoSubtitleTrackChange(IntVal);
      end;

    OBS_VID:
      if (Prop^.format = MPV_FORMAT_INT64) and (Prop^.data <> nil) then
      begin
        IntVal := PInt64(Prop^.data)^;
        FVideoTracks.SelectedID := IntVal;
        DoVideoTrackChange(IntVal);
      end;

    OBS_WIDTH:
      if (Prop^.format = MPV_FORMAT_INT64) and (Prop^.data <> nil) then
      begin
        IntVal := PInt64(Prop^.data)^;
        FStreamInfo.Video.Width := IntVal;
        FRenderInfo.Width := IntVal;
      end;

    OBS_HEIGHT:
      if (Prop^.format = MPV_FORMAT_INT64) and (Prop^.data <> nil) then
      begin
        IntVal := PInt64(Prop^.data)^;
        FStreamInfo.Video.Height := IntVal;
        FRenderInfo.Height := IntVal;
      end;

    OBS_DWIDTH:
      if (Prop^.format = MPV_FORMAT_INT64) and (Prop^.data <> nil) then
      begin
        IntVal := PInt64(Prop^.data)^;
        FStreamInfo.Video.DisplayWidth := IntVal;
        FRenderInfo.DisplayWidth := IntVal;
      end;

    OBS_DHEIGHT:
      if (Prop^.format = MPV_FORMAT_INT64) and (Prop^.data <> nil) then
      begin
        IntVal := PInt64(Prop^.data)^;
        FStreamInfo.Video.DisplayHeight := IntVal;
        FRenderInfo.DisplayHeight := IntVal;
      end;

    OBS_SEEKABLE:
      if (Prop^.format = MPV_FORMAT_FLAG) and (Prop^.data <> nil) then
      begin
        FlagVal := PInteger(Prop^.data)^;
        FStreamInfo.Seekable := FlagVal <> 0;
      end;

    OBS_FILENAME:
      if (Prop^.format = MPV_FORMAT_STRING) and (Prop^.data <> nil) then
      begin
        StrVal := PPAnsiChar(Prop^.data)^;
        if StrVal <> nil then
          FStreamInfo.FileName := string(StrVal);
      end;

    OBS_FILE_FORMAT:
      if (Prop^.format = MPV_FORMAT_STRING) and (Prop^.data <> nil) then
      begin
        StrVal := PPAnsiChar(Prop^.data)^;
        if StrVal <> nil then
          FStreamInfo.FileFormat := string(StrVal);
      end;

    OBS_MEDIA_TITLE:
      if (Prop^.format = MPV_FORMAT_STRING) and (Prop^.data <> nil) then
      begin
        StrVal := PPAnsiChar(Prop^.data)^;
        if StrVal <> nil then
          DoMetadata('icy-title', string(StrVal));
      end;

    OBS_ICY_METADATA:
      if Prop^.format = MPV_FORMAT_NODE then
        UpdateMetadata;

    OBS_EOF_REACHED:
      if (Prop^.format = MPV_FORMAT_FLAG) and (Prop^.data <> nil) then
      begin
        FlagVal := PInteger(Prop^.data)^;
        if FlagVal <> 0 then
        begin
          if Assigned(FOnEndFile) then
            FOnEndFile(Self, MPV_END_FILE_REASON_EOF, 0);
        end;
      end;
  end;
end;

procedure TMPVEngine.HandleLogMessage(Msg: Pmpv_event_log_message);
var
  LogText: string;
begin
  if Msg = nil then Exit;

  LogText := '';
  if Msg^.prefix <> nil then
    LogText := '[' + string(Msg^.prefix) + '] ';
  if Msg^.text <> nil then
    LogText := LogText + string(Msg^.text);

  LogText := Trim(LogText);
  if LogText <> '' then
    DoLog(LogText);
end;

procedure TMPVEngine.HandleStartFile;
begin
  SetStatus(msOpening);
  ResetStreamInfo;
  FAudioTracks.Clear;
  FSubtitleTracks.Clear;
  FVideoTracks.Clear;

  if Assigned(FOnStartFile) then
    FOnStartFile(Self);
end;

procedure TMPVEngine.HandleFileLoaded;
var
  FN: string;
begin
  SetStatus(msPlayStarting);
  FStreamInfo.Valid := True;
  UpdateStreamInfo;
  UpdateTrackList;
  FFileLoaded := FMediaFile;
  FN := FFileLoaded;

  if Assigned(FOnFileLoaded) then
    FOnFileLoaded(Self, FN);

  if Assigned(FOnPropertysRead) then
    FOnPropertysRead(Self);
end;

procedure TMPVEngine.HandleEndFile(EndFile: Pmpv_event_end_file);
begin
  if EndFile = nil then Exit;

  case EndFile^.reason of
    MPV_END_FILE_REASON_EOF:
      SetStatus(msStopped);
    MPV_END_FILE_REASON_STOP:
      SetStatus(msStopped);
    MPV_END_FILE_REASON_ERROR:
      SetStatus(msError);
    MPV_END_FILE_REASON_QUIT:
      SetStatus(msNone);
  end;

  if Assigned(FOnEndFile) then
    FOnEndFile(Self, EndFile^.reason, EndFile^.error);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Track Management
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TMPVEngine.UpdateTrackList;
var
  Str: PAnsiChar;
  Node: mpv_node;
  Err: Integer;
begin
  if FHandle = nil then Exit;

  Err := mpv_get_property(FHandle, 'track-list', MPV_FORMAT_NODE, @Node);
  if Err >= 0 then
  begin
    ParseTrackList(@Node);
    mpv_free_node_contents(@Node);
  end;
end;

{ ───────────────────────────────────────────────────────────────────────────
  ParseTrackList - Parse mpv track-list property into typed track collections

  Purpose: Parses the mpv node tree containing track information and populates
           FAudioTracks, FSubtitleTracks, and FVideoTracks collections.

  Parameters:
    - Node: Pointer to mpv_node containing the track-list (MPV_FORMAT_NODE_ARRAY)

  MPV node structure (nested):
    track-list (NODE_ARRAY)
    └── [0..n] track entries (NODE_MAP each)
        ├── "type" -> "audio" | "video" | "sub"
        ├── "id" -> track ID (int64)
        ├── "title", "lang", "codec" -> strings
        ├── "default", "forced", "external" -> boolean flags
        ├── "demux-channels", "demux-samplerate" -> audio info
        ├── "demux-w", "demux-h", "demux-fps" -> video info
        └── "ff-index", "demux-bitrate" -> misc info

  Pointer arithmetic pattern:
    The mpv_node_list structure stores values as a contiguous array.
    To access element I: Pmpv_node(PtrUInt(List^.values) + I * SizeOf(mpv_node))
    This is required because Pascal doesn't support C-style array indexing on
    dynamically allocated arrays of records.

  Notes:
    - Clears existing track lists before parsing
    - Only tracks with valid ID (>= 0) are added
    - Updates FStreamInfo.HasAudio/HasVideo flags based on parsed tracks
  ─────────────────────────────────────────────────────────────────────────── }
procedure TMPVEngine.ParseTrackList(Node: Pmpv_node);
var
  I, J: Integer;
  List: Pmpv_node_list;
  TrackNode: Pmpv_node;
  TrackMap: Pmpv_node_list;
  Track: TMPVTrackInfo;
  Key: string;
begin
  if (Node = nil) or (Node^.format <> MPV_FORMAT_NODE_ARRAY) then Exit;

  FAudioTracks.Clear;
  FSubtitleTracks.Clear;
  FVideoTracks.Clear;

  List := Node^.list;
  if List = nil then Exit;

  for I := 0 to List^.num - 1 do
  begin
    TrackNode := @Pmpv_node(PtrUInt(List^.values) + PtrUInt(I) * SizeOf(mpv_node))^;
    if TrackNode^.format <> MPV_FORMAT_NODE_MAP then Continue;

    TrackMap := TrackNode^.list;
    if TrackMap = nil then Continue;

    FillChar(Track, SizeOf(Track), 0);
    Track.ID := -1;

    for J := 0 to TrackMap^.num - 1 do
    begin
      Key := string(PPAnsiChar(PtrUInt(TrackMap^.keys) + PtrUInt(J) * SizeOf(PAnsiChar))^);

      case Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.format of
        MPV_FORMAT_STRING:
          begin
            if Key = 'type' then
              Track.TrackType := string(Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.str)
            else if Key = 'title' then
              Track.Title := string(Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.str)
            else if Key = 'lang' then
              Track.Language := string(Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.str)
            else if Key = 'codec' then
              Track.Codec := string(Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.str)
            else if Key = 'external-filename' then
              Track.ExternalFilename := string(Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.str)
            else if Key = 'demux-channel-count' then
              Track.ChannelLayout := string(Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.str);
          end;

        MPV_FORMAT_INT64:
          begin
            if Key = 'id' then
              Track.ID := Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.int64_
            else if Key = 'demux-channels' then
              Track.Channels := Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.int64_
            else if Key = 'demux-samplerate' then
              Track.SampleRate := Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.int64_
            else if Key = 'demux-w' then
              Track.Width := Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.int64_
            else if Key = 'demux-h' then
              Track.Height := Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.int64_
            else if Key = 'ff-index' then
              Track.FFIndex := Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.int64_
            else if Key = 'demux-bitrate' then
              Track.Bitrate := Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.int64_;
          end;

        MPV_FORMAT_DOUBLE:
          begin
            if Key = 'demux-fps' then
              Track.FPS := Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.double_;
          end;

        MPV_FORMAT_FLAG:
          begin
            if Key = 'default' then
              Track.IsDefault := Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.flag <> 0
            else if Key = 'forced' then
              Track.IsForced := Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.flag <> 0
            else if Key = 'external' then
              Track.IsExternal := Pmpv_node(PtrUInt(TrackMap^.values) + PtrUInt(J) * SizeOf(mpv_node))^.flag <> 0;
          end;
      end;
    end;

    if Track.ID >= 0 then
    begin
      if Track.TrackType = 'audio' then
        FAudioTracks.Add(Track)
      else if Track.TrackType = 'sub' then
        FSubtitleTracks.Add(Track)
      else if Track.TrackType = 'video' then
        FVideoTracks.Add(Track);
    end;
  end;

  FStreamInfo.HasAudio := FAudioTracks.Count > 0;
  FStreamInfo.HasVideo := FVideoTracks.Count > 0;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Stream Info
  ═══════════════════════════════════════════════════════════════════════════════ }

{ ───────────────────────────────────────────────────────────────────────────
  UpdateStreamInfo - Gather comprehensive media information from mpv

  Purpose: Queries mpv for detailed codec, format, and stream properties
           to populate FStreamInfo record for display in media info dialog.

  Properties queried:
    - video-codec, audio-codec: Codec names
    - container-fps: Video frame rate
    - video-bitrate, audio-bitrate: Bitrate in bits/sec
    - audio-params/samplerate: Audio sample rate (Hz)
    - audio-params/channel-count: Number of audio channels
    - video-params/aspect: Video aspect ratio
    - file-size: Total file size in bytes
    - hwdec-current: Active hardware decoder (if any)

  Media type detection (based on FMediaFile path):
    - IsDVD: Path contains "dvd://" or "dvdnav://"
    - IsDVDNav: Path contains "dvdnav://" (with menu support)
    - IsBluray: Path contains "bluray://"
    - IsStream: Path contains "://" (any protocol)
    - IsRadio: IsStream AND no video tracks (audio-only stream)
    - IsMp3: File extension is ".mp3"

  Notes:
    - String properties (codecs, hwdec) must be freed with mpv_free()
    - Called after file is loaded to populate media info
  ─────────────────────────────────────────────────────────────────────────── }
procedure TMPVEngine.UpdateStreamInfo;
var
  Str: PAnsiChar;
  DblVal: Double;
  IntVal: Int64;
begin
  if FHandle = nil then Exit;

  { Video codec }
  Str := mpv_get_property_string(FHandle, 'video-codec');
  if Str <> nil then
  begin
    FStreamInfo.Video.Codec := string(Str);
    mpv_free(Str);
  end;

  { Audio codec }
  Str := mpv_get_property_string(FHandle, 'audio-codec');
  if Str <> nil then
  begin
    FStreamInfo.Audio.Codec := string(Str);
    mpv_free(Str);
  end;

  { Video FPS }
  if mpv_get_property(FHandle, 'container-fps', MPV_FORMAT_DOUBLE, @DblVal) >= 0 then
    FStreamInfo.Video.FPS := DblVal;

  { Video bitrate }
  if mpv_get_property(FHandle, 'video-bitrate', MPV_FORMAT_INT64, @IntVal) >= 0 then
    FStreamInfo.Video.Bitrate := IntVal;

  { Audio sample rate }
  if mpv_get_property(FHandle, 'audio-params/samplerate', MPV_FORMAT_INT64, @IntVal) >= 0 then
    FStreamInfo.Audio.SampleRate := IntVal;

  { Audio channels }
  if mpv_get_property(FHandle, 'audio-params/channel-count', MPV_FORMAT_INT64, @IntVal) >= 0 then
    FStreamInfo.Audio.Channels := IntVal;

  { Audio bitrate }
  if mpv_get_property(FHandle, 'audio-bitrate', MPV_FORMAT_INT64, @IntVal) >= 0 then
    FStreamInfo.Audio.Bitrate := IntVal;

  { Video aspect }
  if mpv_get_property(FHandle, 'video-params/aspect', MPV_FORMAT_DOUBLE, @DblVal) >= 0 then
  begin
    FStreamInfo.Video.Aspect := DblVal;
    FRenderInfo.Aspect := DblVal;
  end;

  { File size }
  if mpv_get_property(FHandle, 'file-size', MPV_FORMAT_INT64, @IntVal) >= 0 then
    FStreamInfo.FileSize := IntVal;

  { HW decoder }
  Str := mpv_get_property_string(FHandle, 'hwdec-current');
  if Str <> nil then
  begin
    FStreamInfo.Video.HWDecoder := string(Str);
    mpv_free(Str);
  end;

  { Check if DVD }
  FStreamInfo.IsDVD := (Pos(SRC_FILE_DVD, FMediaFile) > 0) or
                       (Pos(SRC_FILE_DVDNAV, FMediaFile) > 0);
  FStreamInfo.IsDVDNav := Pos(SRC_FILE_DVDNAV, FMediaFile) > 0;
  FStreamInfo.IsBluray := Pos(SRC_FILE_BLURAY, FMediaFile) > 0;

  { Check if stream }
  FStreamInfo.IsStream := (Pos('://', FMediaFile) > 0);
  FStreamInfo.IsRadio := FStreamInfo.IsStream and not FStreamInfo.HasVideo;

  { Check if MP3 }
  FStreamInfo.IsMp3 := LowerCase(ExtractFileExt(FMediaFile)) = '.mp3';
end;

procedure TMPVEngine.UpdateMetadata;
var
  Str: PAnsiChar;
begin
  if FHandle = nil then Exit;

  { Title }
  Str := mpv_get_property_string(FHandle, 'media-title');
  if Str <> nil then
  begin
    DoMetadata('title', string(Str));
    mpv_free(Str);
  end;

  { Artist }
  Str := mpv_get_property_string(FHandle, 'metadata/by-key/artist');
  if Str <> nil then
  begin
    DoMetadata('artist', string(Str));
    mpv_free(Str);
  end;

  { Album }
  Str := mpv_get_property_string(FHandle, 'metadata/by-key/album');
  if Str <> nil then
  begin
    DoMetadata('album', string(Str));
    mpv_free(Str);
  end;

  { ICY name (radio station name) }
  Str := mpv_get_property_string(FHandle, 'metadata/by-key/icy-name');
  if Str <> nil then
  begin
    DoMetadata('icy-name', string(Str));
    mpv_free(Str);
  end;

  { ICY title (current track) }
  Str := mpv_get_property_string(FHandle, 'metadata/by-key/icy-title');
  if Str <> nil then
  begin
    DoMetadata('icy-title', string(Str));
    mpv_free(Str);
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Playback Control
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TMPVEngine.PlayMedia(const URL: string);
var
  Args: array[0..3] of PAnsiChar;
  CacheStr: string;
begin
  if (FHandle = nil) or (URL = '') then Exit;

  FMediaFile := URL;
  ResetStreamInfo;

  DoLog('Loading: ' + URL);

  { Set cache based on source type }
  CacheStr := IntToStr(GetCacheForSource(URL));
  mpv_set_property_string(FHandle, 'cache-secs', PAnsiChar(AnsiString(CacheStr)));

  { Build loadfile command }
  Args[0] := 'loadfile';
  Args[1] := PAnsiChar(AnsiString(URL));
  Args[2] := 'replace';
  Args[3] := nil;

  SetStatus(msOpening);

  if mpv_command(FHandle, @Args[0]) < 0 then
    SetStatus(msError);
end;

procedure TMPVEngine.CloseMedia;
var
  Args: array[0..1] of PAnsiChar;
begin
  if FHandle = nil then Exit;

  SetStatus(msClosing);

  Args[0] := 'stop';
  Args[1] := nil;
  mpv_command(FHandle, @Args[0]);

  ResetStreamInfo;
  FMediaFile := '';
  FFileLoaded := '';

  SetStatus(msNone);
end;

procedure TMPVEngine.Stop;
begin
  CloseMedia;
end;

procedure TMPVEngine.Pause;
begin
  if FHandle = nil then Exit;
  SetPropertyBool('pause', True);
end;

procedure TMPVEngine.Resume;
begin
  if FHandle = nil then Exit;
  SetPropertyBool('pause', False);
end;

procedure TMPVEngine.TogglePause;
begin
  if FPaused then
    Resume
  else
    Pause;
end;

procedure TMPVEngine.SendPlayPause;
begin
  TogglePause;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Seek Functions
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TMPVEngine.SeekAbsolute(Seconds: Double);
var
  Args: array[0..3] of PAnsiChar;
  SecStr: string;
begin
  if FHandle = nil then Exit;

  SecStr := FloatToStr(Seconds);
  Args[0] := 'seek';
  Args[1] := PAnsiChar(AnsiString(SecStr));
  Args[2] := 'absolute';
  Args[3] := nil;
  mpv_command(FHandle, @Args[0]);
end;

procedure TMPVEngine.SeekRelative(Seconds: Double);
var
  Args: array[0..3] of PAnsiChar;
  SecStr: string;
begin
  if FHandle = nil then Exit;

  SecStr := FloatToStr(Seconds);
  Args[0] := 'seek';
  Args[1] := PAnsiChar(AnsiString(SecStr));
  Args[2] := 'relative';
  Args[3] := nil;
  mpv_command(FHandle, @Args[0]);
end;

procedure TMPVEngine.SeekPercent(Percent: Double);
var
  Args: array[0..3] of PAnsiChar;
  PctStr: string;
begin
  if FHandle = nil then Exit;

  PctStr := FloatToStr(Percent);
  Args[0] := 'seek';
  Args[1] := PAnsiChar(AnsiString(PctStr));
  Args[2] := 'absolute-percent';
  Args[3] := nil;
  mpv_command(FHandle, @Args[0]);
end;

procedure TMPVEngine.SeekBy(Value: Integer);
begin
  SeekRelative(Value);
end;

procedure TMPVEngine.SeekTo(Value: Int64; Method: Integer);
begin
  case Method of
    0: SeekRelative(Value);
    1: SeekPercent(Value);
    2: SeekAbsolute(Value);
  end;
end;

procedure TMPVEngine.FrameStep;
var
  Args: array[0..1] of PAnsiChar;
begin
  if FHandle = nil then Exit;

  Args[0] := 'frame-step';
  Args[1] := nil;
  mpv_command(FHandle, @Args[0]);
end;

procedure TMPVEngine.FrameBackStep;
var
  Args: array[0..1] of PAnsiChar;
begin
  if FHandle = nil then Exit;

  Args[0] := 'frame-back-step';
  Args[1] := nil;
  mpv_command(FHandle, @Args[0]);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Audio Control
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TMPVEngine.SetAudioTrack(TrackID: Integer);
begin
  if FHandle = nil then Exit;
  SetPropertyInt('aid', TrackID);
end;

function TMPVEngine.GetAudioID: Integer;
begin
  Result := FAudioTracks.SelectedID;
end;

procedure TMPVEngine.SetAudioID(Value: Integer);
begin
  SetAudioTrack(Value);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Equalizer
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TMPVEngine.SetEqualizerBand(Band: Integer; dB: Double);
begin
  if (Band < 0) or (Band >= EQ_BANDS) then Exit;

  dB := EnsureRange(dB, EQ_MIN_DB, EQ_MAX_DB);
  FEqualizerBands[Band] := dB;

  { Note: ApplyEqualizer is not called here to avoid crackling }
  { The equalizer form should call ApplyEqualizer after a debounce delay }
end;

function TMPVEngine.GetEqualizerBand(Band: Integer): Double;
begin
  if (Band >= 0) and (Band < EQ_BANDS) then
    Result := FEqualizerBands[Band]
  else
    Result := 0;
end;

procedure TMPVEngine.SetEqualizerPreset(const Values: string);
var
  Parts: TStringArray;
  I: Integer;
  dB: Double;
begin
  Parts := Values.Split([':']);
  for I := 0 to Min(High(Parts), EQ_BANDS - 1) do
  begin
    if TryStrToFloat(Parts[I], dB) then
      FEqualizerBands[I] := EnsureRange(dB, EQ_MIN_DB, EQ_MAX_DB);
  end;

  if FEqualizerEnabled then
    ApplyEqualizer;
end;

function TMPVEngine.GetEqualizerPreset: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to EQ_BANDS - 1 do
  begin
    if I > 0 then
      Result := Result + ':';
    Result := Result + FormatFloat('0.0', FEqualizerBands[I]);
  end;
end;

procedure TMPVEngine.ResetEqualizer;
var
  I: Integer;
begin
  for I := 0 to EQ_BANDS - 1 do
    FEqualizerBands[I] := 0;

  if FEqualizerEnabled then
    ApplyEqualizer;
end;

procedure TMPVEngine.EnableEqualizer(Enable: Boolean);
begin
  FEqualizerEnabled := Enable;
  ApplyEqualizer;
end;

procedure TMPVEngine.ApplyEqualizer;
var
  FilterStr: string;
begin
  if FHandle = nil then Exit;

  if FEqualizerEnabled then
  begin
    FilterStr := BuildEqualizerString;
    if FilterStr <> '' then
      SetPropertyString('af', FilterStr);
  end
  else
    SetPropertyString('af', '');
end;

{ ───────────────────────────────────────────────────────────────────────────
  BuildEqualizerString - Generate FFmpeg audio filter chain for 10-band EQ

  Purpose: Constructs a lavfi filter string from the current equalizer band
           settings for use with mpv's 'af' (audio filter) property.

  Returns: FFmpeg lavfi filter string, e.g.:
           "lavfi=[equalizer=f=31:t=o:w=1:g=3.0,equalizer=f=62:t=o:w=1:g=-2.0,...]"

  FFmpeg equalizer filter syntax:
    equalizer=f=FREQ:t=o:w=1:g=GAIN
    - f=FREQ: Center frequency in Hz
    - t=o: Filter type = octave (bandwidth spans one octave)
    - w=1: Width = 1 octave
    - g=GAIN: Gain in dB (positive = boost, negative = cut)

  Band frequencies (standard 10-band):
    31Hz, 62Hz, 125Hz, 250Hz, 500Hz, 1kHz, 2kHz, 4kHz, 8kHz, 16kHz

  Notes:
    - Preamp (master volume) is added as a trailing volume filter if non-zero
    - Entire chain is wrapped in lavfi=[] for mpv compatibility
    - Gain values are formatted with 1 decimal place (%.1f)
  ─────────────────────────────────────────────────────────────────────────── }
function TMPVEngine.BuildEqualizerString: string;
const
  { Center frequencies for 10-band EQ }
  FREQ: array[0..9] of Integer = (31, 62, 125, 250, 500, 1000, 2000, 4000, 8000, 16000);
var
  I: Integer;
  Filters: string;
  FmtSettings: TFormatSettings;
begin
  { Use dot as decimal separator for FFmpeg compatibility (locale-independent) }
  FmtSettings := DefaultFormatSettings;
  FmtSettings.DecimalSeparator := '.';

  { Build chain of FFmpeg equalizer filters }
  { Format: equalizer=f=freq:t=o:w=1:g=gain (octave-based bandwidth) }
  Filters := '';
  for I := 0 to EQ_BANDS - 1 do
  begin
    if I > 0 then
      Filters := Filters + ',';
    Filters := Filters + Format('equalizer=f=%d:t=o:w=1:g=%.1f',
      [FREQ[I], FEqualizerBands[I]], FmtSettings);
  end;

  { Add preamp (volume) at the end if not zero }
  if Abs(FPreamp) > 0.01 then
    Filters := Filters + Format(',volume=%.1fdB', [FPreamp], FmtSettings);

  Result := 'lavfi=[' + Filters + ']';
end;

procedure TMPVEngine.SendAE(const Value: string);
begin
  SetEqualizerPreset(Value);
end;

procedure TMPVEngine.SendAE2(const Value: string);
begin
  SetEqualizerPreset(Value);
end;

function TMPVEngine.GetEqualizerFilterString: string;
const
  { Center frequencies for 10-band EQ }
  FREQ: array[0..9] of Integer = (31, 62, 125, 250, 500, 1000, 2000, 4000, 8000, 16000);
var
  I: Integer;
  FmtSettings: TFormatSettings;
begin
  { Returns equalizer filter string for lavfi-complex integration }
  { Returns just the filter chain without lavfi=[] wrapper }
  if not FEqualizerEnabled then
  begin
    Result := '';
    Exit;
  end;

  { Use dot as decimal separator for FFmpeg compatibility (locale-independent) }
  FmtSettings := DefaultFormatSettings;
  FmtSettings.DecimalSeparator := '.';

  Result := '';
  for I := 0 to EQ_BANDS - 1 do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + Format('equalizer=f=%d:t=o:w=1:g=%.1f',
      [FREQ[I], FEqualizerBands[I]], FmtSettings);
  end;

  { Add preamp (volume) at the end if not zero }
  if Abs(FPreamp) > 0.01 then
    Result := Result + Format(',volume=%.1fdB', [FPreamp], FmtSettings);
end;

procedure TMPVEngine.SetPreamp(dB: Double);
begin
  if dB < -20 then dB := -20;
  if dB > 20 then dB := 20;
  FPreamp := dB;
  { Note: ApplyEqualizer is not called here to avoid crackling }
  { The equalizer form should call ApplyEqualizer after a debounce delay }
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Subtitle Control
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TMPVEngine.SetSubtitleTrack(TrackID: Integer);
begin
  if FHandle = nil then Exit;
  SetPropertyInt('sid', TrackID);
end;

function TMPVEngine.GetSubID: Integer;
begin
  Result := FSubtitleTracks.SelectedID;
end;

procedure TMPVEngine.SetSubID(Value: Integer);
begin
  SetSubtitleTrack(Value);
end;

procedure TMPVEngine.LoadSubtitle(const FileName: string);
var
  Args: array[0..2] of PAnsiChar;
begin
  if (FHandle = nil) or (FileName = '') then Exit;

  Args[0] := 'sub-add';
  Args[1] := PAnsiChar(AnsiString(FileName));
  Args[2] := nil;
  mpv_command(FHandle, @Args[0]);
end;

procedure TMPVEngine.UnloadSubtitles;
begin
  if FHandle = nil then Exit;
  SetPropertyInt('sid', 0);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Video Control
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TMPVEngine.SetVideoTrack(TrackID: Integer);
begin
  if FHandle = nil then Exit;
  SetPropertyInt('vid', TrackID);
end;

procedure TMPVEngine.SetAspectRatio(const Ratio: string);
begin
  if FHandle = nil then Exit;
  SetPropertyString('video-aspect-override', Ratio);
end;

procedure TMPVEngine.Screenshot(const FileName: string);
begin
  ScreenshotToFile(FileName, SCREENSHOT_VIDEO);
end;

procedure TMPVEngine.ScreenshotToFile(const FileName: string; Mode: string);
var
  Args: array[0..3] of PAnsiChar;
begin
  if FHandle = nil then Exit;

  Args[0] := 'screenshot-to-file';
  if FileName <> '' then
    Args[1] := PAnsiChar(AnsiString(FileName))
  else
    Args[1] := '';
  Args[2] := PAnsiChar(AnsiString(Mode));
  Args[3] := nil;

  if FileName = '' then
  begin
    Args[0] := 'screenshot';
    Args[1] := PAnsiChar(AnsiString(Mode));
    Args[2] := nil;
  end;

  mpv_command(FHandle, @Args[0]);
end;

procedure TMPVEngine.SetScreenshotDirectory(const Path: string);
begin
  if FHandle = nil then Exit;
  SetPropertyString('screenshot-directory', Path);
end;

procedure TMPVEngine.SetScreenshotFormat(const Format: string);
var
  Fmt: string;
begin
  if FHandle = nil then Exit;
  { Convert format to mpv format string (png, jpg, webp) }
  Fmt := LowerCase(Format);
  if Fmt = 'jpeg' then Fmt := 'jpg';
  SetPropertyString('screenshot-format', Fmt);
end;

procedure TMPVEngine.SetCacheSize(SizeKB: Integer);
var
  SizeBytes: Int64;
begin
  if FHandle = nil then Exit;
  { Convert KB to bytes }
  SizeBytes := Int64(SizeKB) * 1024;
  { Set demuxer cache size }
  SetPropertyString('demuxer-max-bytes', IntToStr(SizeBytes));
  { Set back buffer to half of forward buffer }
  SetPropertyString('demuxer-max-back-bytes', IntToStr(SizeBytes div 2));
end;

procedure TMPVEngine.ToggleFullscreen;
var
  FS: Boolean;
begin
  if FHandle = nil then Exit;
  FS := GetPropertyBool('fullscreen');
  SetPropertyBool('fullscreen', not FS);
  FRenderInfo.Fullscreen := not FS;
end;

procedure TMPVEngine.SetProperty(Prop: Integer; Value: Integer);
begin
  case Prop of
    PROP_BRIGHTNESS: Brightness := Value;
    PROP_CONTRAST: Contrast := Value;
    PROP_SATURATION: Saturation := Value;
    PROP_HUE: Hue := Value;
    PROP_GAMMA: Gamma := Value;
    PROP_SUB_SCALE: SubScale := Value / 10.0;
  end;
end;

function TMPVEngine.GetValidFileInfo: Boolean;
begin
  Result := FStreamInfo.Valid;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - DVD/Bluray Navigation
  ═══════════════════════════════════════════════════════════════════════════════ }

function TMPVEngine.IsDVD: Boolean;
begin
  Result := FStreamInfo.IsDVD;
end;

function TMPVEngine.IsDVDNav: Boolean;
begin
  Result := FStreamInfo.IsDVDNav;
end;

function TMPVEngine.IsBluray: Boolean;
begin
  Result := FStreamInfo.IsBluray;
end;

function TMPVEngine.DVDGoMenu: Boolean;
var
  Args: array[0..2] of PAnsiChar;
begin
  Result := False;
  if FHandle = nil then Exit;

  Args[0] := 'discnav';
  Args[1] := 'menu';
  Args[2] := nil;
  Result := mpv_command(FHandle, @Args[0]) >= 0;
end;

function TMPVEngine.DVDMenuClick: Boolean;
var
  Args: array[0..2] of PAnsiChar;
begin
  Result := False;
  if FHandle = nil then Exit;

  Args[0] := 'discnav';
  Args[1] := 'select';
  Args[2] := nil;
  Result := mpv_command(FHandle, @Args[0]) >= 0;
end;

function TMPVEngine.DVDMenuPosition(X, Y: Integer): Boolean;
var
  Args: array[0..3] of PAnsiChar;
  PosStr: string;
begin
  Result := False;
  if FHandle = nil then Exit;

  PosStr := IntToStr(X) + ' ' + IntToStr(Y);
  Args[0] := 'discnav';
  Args[1] := 'mouse_move';
  Args[2] := PAnsiChar(AnsiString(PosStr));
  Args[3] := nil;
  Result := mpv_command(FHandle, @Args[0]) >= 0;
end;

procedure TMPVEngine.DVDMenu;
begin
  DVDGoMenu;
end;

procedure TMPVEngine.DVDMenuSelect;
begin
  DVDMenuClick;
end;

procedure TMPVEngine.DVDMenuUp;
var
  Args: array[0..2] of PAnsiChar;
begin
  if FHandle = nil then Exit;

  Args[0] := 'discnav';
  Args[1] := 'up';
  Args[2] := nil;
  mpv_command(FHandle, @Args[0]);
end;

procedure TMPVEngine.DVDMenuDown;
var
  Args: array[0..2] of PAnsiChar;
begin
  if FHandle = nil then Exit;

  Args[0] := 'discnav';
  Args[1] := 'down';
  Args[2] := nil;
  mpv_command(FHandle, @Args[0]);
end;

procedure TMPVEngine.DVDMenuLeft;
var
  Args: array[0..2] of PAnsiChar;
begin
  if FHandle = nil then Exit;

  Args[0] := 'discnav';
  Args[1] := 'left';
  Args[2] := nil;
  mpv_command(FHandle, @Args[0]);
end;

procedure TMPVEngine.DVDMenuRight;
var
  Args: array[0..2] of PAnsiChar;
begin
  if FHandle = nil then Exit;

  Args[0] := 'discnav';
  Args[1] := 'right';
  Args[2] := nil;
  mpv_command(FHandle, @Args[0]);
end;

procedure TMPVEngine.DVDTitle(TitleID: Integer);
begin
  if FHandle = nil then Exit;
  PlayMedia(SRC_FILE_DVD + IntToStr(TitleID));
end;

procedure TMPVEngine.DVDChapter(ChapterID: Integer);
begin
  if FHandle = nil then Exit;
  SetPropertyInt('chapter', ChapterID);
end;

procedure TMPVEngine.DVDNextChapter;
var
  Args: array[0..2] of PAnsiChar;
begin
  if FHandle = nil then Exit;

  Args[0] := 'add';
  Args[1] := 'chapter';
  Args[2] := '1';
  mpv_command(FHandle, @Args[0]);
end;

procedure TMPVEngine.DVDPrevChapter;
var
  Args: array[0..2] of PAnsiChar;
begin
  if FHandle = nil then Exit;

  Args[0] := 'add';
  Args[1] := 'chapter';
  Args[2] := '-1';
  mpv_command(FHandle, @Args[0]);
end;

function TMPVEngine.GetTitleID: Integer;
begin
  Result := FCurrentTitle;
end;

function TMPVEngine.GetChapterID: Integer;
begin
  Result := FCurrentChapter;
end;

procedure TMPVEngine.SetTitleID(Value: Integer);
begin
  DVDTitle(Value);
end;

procedure TMPVEngine.SetChapterID(Value: Integer);
begin
  DVDChapter(Value);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Generic Property Access
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TMPVEngine.SendCommand(const Args: array of string);
var
  CmdArgs: PPAnsiChar;
begin
  if FHandle = nil then Exit;

  CmdArgs := BuildMPVArgs(Args);
  try
    mpv_command(FHandle, CmdArgs);
  finally
    FreeMPVArgs(CmdArgs);
  end;
end;

procedure TMPVEngine.SendCmd(const Command: string);
begin
  if FHandle = nil then Exit;
  mpv_command_string(FHandle, PAnsiChar(AnsiString(Command)));
end;

procedure TMPVEngine.SetOption(const Name, Value: string);
begin
  if FHandle = nil then Exit;
  mpv_set_option_string(FHandle, PAnsiChar(AnsiString(Name)),
    PAnsiChar(AnsiString(Value)));
end;

function TMPVEngine.GetPropertyString(const Name: string): string;
var
  Str: PAnsiChar;
begin
  Result := '';
  if FHandle = nil then Exit;

  Str := mpv_get_property_string(FHandle, PAnsiChar(AnsiString(Name)));
  if Str <> nil then
  begin
    Result := string(Str);
    mpv_free(Str);
  end;
end;

function TMPVEngine.GetPropertyDouble(const Name: string): Double;
begin
  Result := 0;
  if FHandle = nil then Exit;
  mpv_get_property(FHandle, PAnsiChar(AnsiString(Name)), MPV_FORMAT_DOUBLE, @Result);
end;

function TMPVEngine.GetPropertyInt(const Name: string): Int64;
begin
  Result := 0;
  if FHandle = nil then Exit;
  mpv_get_property(FHandle, PAnsiChar(AnsiString(Name)), MPV_FORMAT_INT64, @Result);
end;

function TMPVEngine.GetPropertyBool(const Name: string): Boolean;
var
  Flag: Integer;
begin
  Result := False;
  if FHandle = nil then Exit;

  Flag := 0;
  if mpv_get_property(FHandle, PAnsiChar(AnsiString(Name)), MPV_FORMAT_FLAG, @Flag) >= 0 then
    Result := Flag <> 0;
end;

procedure TMPVEngine.SetPropertyString(const Name, Value: string);
begin
  if FHandle = nil then Exit;
  mpv_set_property_string(FHandle, PAnsiChar(AnsiString(Name)),
    PAnsiChar(AnsiString(Value)));
end;

procedure TMPVEngine.SetPropertyDouble(const Name: string; Value: Double);
begin
  if FHandle = nil then Exit;
  mpv_set_property(FHandle, PAnsiChar(AnsiString(Name)), MPV_FORMAT_DOUBLE, @Value);
end;

procedure TMPVEngine.SetPropertyInt(const Name: string; Value: Int64);
begin
  if FHandle = nil then Exit;
  mpv_set_property(FHandle, PAnsiChar(AnsiString(Name)), MPV_FORMAT_INT64, @Value);
end;

procedure TMPVEngine.SetPropertyBool(const Name: string; Value: Boolean);
var
  Flag: Integer;
begin
  if FHandle = nil then Exit;

  if Value then Flag := 1 else Flag := 0;
  mpv_set_property(FHandle, PAnsiChar(AnsiString(Name)), MPV_FORMAT_FLAG, @Flag);
end;

function TMPVEngine.GetPropertyValues(Index: Integer): TMPVPropertyValues;
begin
  if (Index >= 0) and (Index <= MAX_PROP_ENTRYS) then
    Result := FPropertyValues[Index]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TMPVEngine.AspectPresetValue: Double;
begin
  if (FAspectMode >= 0) and (FAspectMode <= High(ASPECT_VALUES)) then
    Result := ASPECT_VALUES[FAspectMode]
  else
    Result := -1;
end;

function TMPVEngine.DeinterlaceCmd(Mode, Alg: Integer; DeintDVD, AIsDvd, Interlaced: Boolean): Integer;
begin
  Result := Mode;
  if (Mode = DEINT_AUTO) then
  begin
    if AIsDvd and DeintDVD then
      Result := DEINT_ON
    else if Interlaced then
      Result := DEINT_ON
    else
      Result := DEINT_OFF;
  end;
end;

function TMPVEngine.DeinterlaceCmd(Mode, Alg: Integer; DeintDVD: Boolean): Integer;
begin
  Result := DeinterlaceCmd(Mode, Alg, DeintDVD, FStreamInfo.IsDVD, False);
end;

function TMPVEngine.CacheSizeCmd(const sFile: string): Integer;
begin
  Result := GetCacheForSource(sFile);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Timed Tasks
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TMPVEngine.TimedTasks;
begin
  { Timed tasks are handled by the timer event }
end;

procedure TMPVEngine.QueryPosition;
begin
  if FHandle = nil then Exit;
  { Position is updated via property observation }
end;

procedure TMPVEngine.StartPropertyChange;
begin
  FCriticalSection.Enter;
end;

procedure TMPVEngine.EndPropertyChange;
begin
  FCriticalSection.Leave;
end;

procedure TMPVEngine.ResetLogLastMsg;
begin
  DoLogClear;
end;

procedure TMPVEngine.OnTimerTick(Sender: TObject);
begin
  if not FInitialized or (FHandle = nil) then Exit;

  { Update position }
  if FStatus = msPlaying then
  begin
    DoPositionChange(FPosition, FPercentPos);
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Cache Management
  ═══════════════════════════════════════════════════════════════════════════════ }

{ ───────────────────────────────────────────────────────────────────────────
  GetCacheForSource - Determine optimal cache size based on media source type

  Purpose: Returns the appropriate cache size (in KB) based on the URL/path
           pattern, optimizing buffering for different source characteristics.

  Parameters:
    - URL: Media file path or URL to analyze

  Returns: Cache size in KB from FCacheSize array

  Source type detection (by URL pattern):
    - "dvd://", "dvdnav://", "bluray://" -> CACHE_TYPE_DVD
      Optical media benefits from read-ahead buffering
    - "http://", "https://", "rtsp://", "rtmp://", "mms://" -> CACHE_TYPE_INTERNET
      Larger cache compensates for network latency and jitter
    - "smb://" or UNC path "\\\\" -> CACHE_TYPE_NETWORK
      Network shares need buffering for variable throughput
    - All other paths -> CACHE_TYPE_DEFAULT
      Local files typically need minimal caching

  Notes:
    - Cache sizes are configurable via FCacheSize array
    - Larger cache = more memory usage but smoother playback
    - DVD/Bluray use same cache type (optical media characteristics)
  ─────────────────────────────────────────────────────────────────────────── }
function TMPVEngine.GetCacheForSource(const URL: string): Integer;
begin
  if Pos(SRC_FILE_DVD, URL) > 0 then
    Result := FCacheSize[CACHE_TYPE_DVD]
  else if Pos(SRC_FILE_DVDNAV, URL) > 0 then
    Result := FCacheSize[CACHE_TYPE_DVD]
  else if Pos(SRC_FILE_BLURAY, URL) > 0 then
    Result := FCacheSize[CACHE_TYPE_DVD]
  else if (Pos(SRC_FILE_HTTP, URL) > 0) or (Pos(SRC_FILE_HTTPS, URL) > 0) or
          (Pos(SRC_FILE_RTSP, URL) > 0) or (Pos(SRC_FILE_RTMP, URL) > 0) or
          (Pos(SRC_FILE_MMS, URL) > 0) then
    Result := FCacheSize[CACHE_TYPE_INTERNET]
  else if (Pos(SRC_FILE_SMB, URL) > 0) or (Pos('\\', URL) = 1) then
    Result := FCacheSize[CACHE_TYPE_NETWORK]
  else
    Result := FCacheSize[CACHE_TYPE_DEFAULT];
end;

function TMPVEngine.GetDriveType(const Path: string): Integer;
begin
  {$IFDEF WINDOWS}
  Result := Windows.GetDriveType(PChar(ExtractFileDrive(Path) + '\'));
  {$ELSE}
  Result := 3; { DRIVE_FIXED }
  {$ENDIF}
end;

function TMPVEngine.GetCacheSize(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index <= MAX_CACHE_ENTRYS) then
    Result := FCacheSize[Index]
  else
    Result := FCacheSize[CACHE_TYPE_DEFAULT];
end;

procedure TMPVEngine.SetCacheSize(Index: Integer; Value: Integer);
begin
  if (Index >= 0) and (Index <= MAX_CACHE_ENTRYS) then
    FCacheSize[Index] := Value;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Property Setters
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TMPVEngine.SetStatus(Value: TMPVStatus);
var
  OldStatus: TMPVStatus;
begin
  if FStatus <> Value then
  begin
    OldStatus := FStatus;
    FStatus := Value;
    DoStatusChange(OldStatus, Value);
  end;
end;

procedure TMPVEngine.SetVolume(Value: Integer);
begin
  Value := EnsureRange(Value, VOL_MIN, VOL_BOOST);
  FVolume := Value;
  if FHandle <> nil then
    SetPropertyDouble('volume', Value);
end;

procedure TMPVEngine.SetMuted(Value: Boolean);
begin
  FMuted := Value;
  if FHandle <> nil then
    SetPropertyBool('mute', Value);
end;

procedure TMPVEngine.SetSpeed(Value: Double);
begin
  Value := EnsureRange(Value, SPEED_MIN, SPEED_MAX);
  FSpeed := Value;
  if FHandle <> nil then
    SetPropertyDouble('speed', Value);
end;

procedure TMPVEngine.SetBrightness(Value: Integer);
begin
  Value := EnsureRange(Value, VIDEO_PROP_MIN, VIDEO_PROP_MAX);
  FBrightness := Value;
  FPropertyValues[PROP_BRIGHTNESS].Value := Value;
  FPropertyValues[PROP_BRIGHTNESS].Valid := True;
  if FHandle <> nil then
    SetPropertyInt('brightness', Value);
end;

procedure TMPVEngine.SetContrast(Value: Integer);
begin
  Value := EnsureRange(Value, VIDEO_PROP_MIN, VIDEO_PROP_MAX);
  FContrast := Value;
  FPropertyValues[PROP_CONTRAST].Value := Value;
  FPropertyValues[PROP_CONTRAST].Valid := True;
  if FHandle <> nil then
    SetPropertyInt('contrast', Value);
end;

procedure TMPVEngine.SetSaturation(Value: Integer);
begin
  Value := EnsureRange(Value, VIDEO_PROP_MIN, VIDEO_PROP_MAX);
  FSaturation := Value;
  FPropertyValues[PROP_SATURATION].Value := Value;
  FPropertyValues[PROP_SATURATION].Valid := True;
  if FHandle <> nil then
    SetPropertyInt('saturation', Value);
end;

procedure TMPVEngine.SetHue(Value: Integer);
begin
  Value := EnsureRange(Value, VIDEO_PROP_MIN, VIDEO_PROP_MAX);
  FHue := Value;
  FPropertyValues[PROP_HUE].Value := Value;
  FPropertyValues[PROP_HUE].Valid := True;
  if FHandle <> nil then
    SetPropertyInt('hue', Value);
end;

procedure TMPVEngine.SetGamma(Value: Integer);
begin
  Value := EnsureRange(Value, VIDEO_PROP_MIN, VIDEO_PROP_MAX);
  FGamma := Value;
  FPropertyValues[PROP_GAMMA].Value := Value;
  FPropertyValues[PROP_GAMMA].Valid := True;
  if FHandle <> nil then
    SetPropertyInt('gamma', Value);
end;

procedure TMPVEngine.SetSubScale(Value: Double);
begin
  Value := EnsureRange(Value, SUB_SCALE_MIN, SUB_SCALE_MAX);
  FSubScale := Value;
  if FHandle <> nil then
    SetPropertyDouble('sub-scale', Value);
end;

procedure TMPVEngine.SetSubDelay(Value: Double);
begin
  Value := EnsureRange(Value, SUB_DELAY_MIN, SUB_DELAY_MAX);
  FSubDelay := Value;
  if FHandle <> nil then
    SetPropertyDouble('sub-delay', Value);
end;

procedure TMPVEngine.SetSubVisible(Value: Boolean);
begin
  FSubVisible := Value;
  if FHandle <> nil then
    SetPropertyBool('sub-visibility', Value);
end;

procedure TMPVEngine.SetSubFont(const FontName: string);
begin
  if FHandle = nil then Exit;
  SetPropertyString('sub-font', FontName);
end;

procedure TMPVEngine.SetSubFontSize(Size: Integer);
begin
  if FHandle = nil then Exit;
  SetPropertyInt('sub-font-size', Size);
end;

{ ───────────────────────────────────────────────────────────────────────────
  SetSubFontColor - Set subtitle text color

  Purpose: Converts a Windows TColor value to mpv's color format and applies
           it to the subtitle renderer.

  Parameters:
    - Color: Windows TColor value (BGR format: $00BBGGRR)

  Color format conversion:
    - Windows TColor stores colors as BGR (Blue-Green-Red)
    - mpv expects colors as "#RRGGBB" hex string
    - Extraction: R = byte 0, G = byte 1, B = byte 2

  Notes:
    - TColor $00FF0000 (appears red in Delphi) = Blue in mpv without conversion
    - This function handles the byte-order swap automatically
  ─────────────────────────────────────────────────────────────────────────── }
procedure TMPVEngine.SetSubFontColor(Color: Cardinal);
var
  R, G, B: Byte;
  ColorStr: string;
begin
  if FHandle = nil then Exit;
  { Convert TColor (BGR) to mpv format "#RRGGBB" }
  R := Color and $FF;
  G := (Color shr 8) and $FF;
  B := (Color shr 16) and $FF;
  ColorStr := Format('#%.2X%.2X%.2X', [R, G, B]);
  SetPropertyString('sub-color', ColorStr);
end;

procedure TMPVEngine.SetSubBold(Bold: Boolean);
begin
  if FHandle = nil then Exit;
  SetPropertyBool('sub-bold', Bold);
end;

procedure TMPVEngine.SetSubItalic(Italic: Boolean);
begin
  if FHandle = nil then Exit;
  SetPropertyBool('sub-italic', Italic);
end;

procedure TMPVEngine.SetSubOutlineColor(Color: Cardinal);
var
  R, G, B: Byte;
  ColorStr: string;
begin
  if FHandle = nil then Exit;
  { Convert TColor (BGR) to mpv format "#RRGGBB" }
  R := Color and $FF;
  G := (Color shr 8) and $FF;
  B := (Color shr 16) and $FF;
  ColorStr := Format('#%.2X%.2X%.2X', [R, G, B]);
  SetPropertyString('sub-border-color', ColorStr);
end;

procedure TMPVEngine.SetSubOutlineSize(Size: Integer);
begin
  if FHandle = nil then Exit;
  SetPropertyInt('sub-border-size', Size);
end;

procedure TMPVEngine.SetSubPosition(Position: Integer);
begin
  if FHandle = nil then Exit;
  { sub-pos: 0 = top, 100 = bottom }
  Position := EnsureRange(Position, 0, 100);
  SetPropertyInt('sub-pos', Position);
end;

procedure TMPVEngine.SetSubEncoding(const Encoding: string);
begin
  if FHandle = nil then Exit;
  SetPropertyString('sub-codepage', Encoding);
end;

procedure TMPVEngine.SetSubAutoLoad(AutoLoad: Boolean);
begin
  if FHandle = nil then Exit;
  if AutoLoad then
    SetPropertyString('sub-auto', 'fuzzy')
  else
    SetPropertyString('sub-auto', 'no');
end;

procedure TMPVEngine.SetAudioOutput(const Driver: string);
begin
  if FHandle = nil then Exit;
  { 'auto' means let mpv choose - use empty string }
  if (Driver = '') or (Driver = 'auto') then
    SetPropertyString('ao', '')
  else
    SetPropertyString('ao', Driver);
end;

procedure TMPVEngine.SetAudioDevice(const Device: string);
begin
  if FHandle = nil then Exit;
  if Device <> '' then
    SetPropertyString('audio-device', Device)
  else
    SetPropertyString('audio-device', 'auto');
end;

procedure TMPVEngine.SetAudioNormalize(Enable: Boolean);
begin
  if FHandle = nil then Exit;
  if Enable then
    SetPropertyString('af', 'loudnorm=I=-16:TP=-1.5:LRA=11')
  else
    SetPropertyString('af', '');
end;

procedure TMPVEngine.SetAudioChannels(Channels: Integer);
begin
  if FHandle = nil then Exit;
  case Channels of
    1: SetPropertyString('audio-channels', 'mono');
    2: SetPropertyString('audio-channels', 'stereo');
    6: SetPropertyString('audio-channels', '5.1');
    8: SetPropertyString('audio-channels', '7.1');
    else SetPropertyString('audio-channels', 'auto');
  end;
end;

function TMPVEngine.GetAudioDeviceList: TStringList;
var
  DeviceList: string;
  Lines: TStringList;
  I: Integer;
  Line, DeviceName, DeviceDesc: string;
  P: Integer;
begin
  Result := TStringList.Create;
  Result.Add('auto=Auto (default)');

  if FHandle = nil then Exit;

  { Get audio device list from mpv }
  DeviceList := GetPropertyString('audio-device-list');
  if DeviceList = '' then Exit;

  { Parse the device list - format varies by platform }
  Lines := TStringList.Create;
  try
    Lines.Text := DeviceList;
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      if Line <> '' then
      begin
        { Try to parse name=description format }
        P := Pos('=', Line);
        if P > 0 then
        begin
          DeviceName := Copy(Line, 1, P - 1);
          DeviceDesc := Copy(Line, P + 1, Length(Line));
          Result.Add(DeviceName + '=' + DeviceDesc);
        end
        else
          Result.Add(Line + '=' + Line);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TMPVEngine.ReloadCurrentFile;
var
  SavedFile: string;
  SavedPos: Double;
begin
  if FHandle = nil then Exit;
  if FMediaFile = '' then Exit;

  { Save current state }
  SavedFile := FMediaFile;
  SavedPos := FPosition;

  { Reload the file - this will reinitialize audio completely }
  PlayMedia(SavedFile);

  { Seek back to saved position after a short delay }
  if SavedPos > 1 then
    SeekAbsolute(SavedPos);
end;

procedure TMPVEngine.SetAudioDelay(Value: Double);
begin
  Value := EnsureRange(Value, AUDIO_DELAY_MIN, AUDIO_DELAY_MAX);
  FAudioDelay := Value;
  if FHandle <> nil then
    SetPropertyDouble('audio-delay', Value);
end;

procedure TMPVEngine.SetAspectMode(Value: Integer);
begin
  FAspectMode := Value;
  if (Value >= 0) and (Value <= High(ASPECT_VALUES)) then
  begin
    FAspectFactor := ASPECT_VALUES[Value];
    if FHandle <> nil then
    begin
      if FAspectFactor < 0 then
        SetPropertyString('video-aspect-override', '-1')
      else
        SetPropertyDouble('video-aspect-override', FAspectFactor);
    end;
  end;
end;

procedure TMPVEngine.SetAspectFactor(Value: Double);
begin
  FAspectFactor := Value;
  FAspectMode := ASPECT_CUSTOM;
  if FHandle <> nil then
  begin
    if Value < 0 then
      SetPropertyString('video-aspect-override', '-1')
    else
      SetPropertyDouble('video-aspect-override', Value);
  end;
end;

procedure TMPVEngine.SetDeinterlace(Value: Integer);
begin
  FDeinterlace := Value;
  if FHandle <> nil then
  begin
    case Value of
      DEINT_OFF: SetPropertyString('deinterlace', 'no');
      DEINT_ON: SetPropertyString('deinterlace', 'yes');
      DEINT_AUTO: ; { MPV doesn't support 'auto' - leave at default (no) }
    end;
  end;
end;

procedure TMPVEngine.SetDeinterlaceAlg(Value: Integer);
begin
  FDeinterlaceAlg := Value;
  { Algorithm is set via vf filter options }
end;

procedure TMPVEngine.SetHWAccel(Value: Boolean);
begin
  FHWAccel := Value;
  if FHandle <> nil then
  begin
    if Value then
      SetPropertyString('hwdec', 'auto')
    else
      SetPropertyString('hwdec', 'no');
  end;
end;

procedure TMPVEngine.SetVideoOutput(const Value: string);
begin
  FVideoOutput := Value;
  { Note: VideoOutput (vo) can only be set before mpv_initialize.
    Changes will take effect on next engine restart. }
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TMPVEngine - Event Triggers
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TMPVEngine.DoLog(const Msg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Msg);
end;

procedure TMPVEngine.DoLogClear;
begin
  if Assigned(FOnLogClear) then
    FOnLogClear(Self);
end;

procedure TMPVEngine.DoStatusChange(OldStatus, NewStatus: TMPVStatus);
begin
  if Assigned(FOnStatusChange) then
    FOnStatusChange(Self, OldStatus, NewStatus);
end;

procedure TMPVEngine.DoPositionChange(PosSec, PosPct: Double);
begin
  if Assigned(FOnPositionChange) then
    FOnPositionChange(Self, PosSec, PosPct);
end;

procedure TMPVEngine.DoAudioTrackChange(ID: Integer);
begin
  if Assigned(FOnAudioTrackChange) then
    FOnAudioTrackChange(Self, 'audio', ID);
end;

procedure TMPVEngine.DoSubtitleTrackChange(ID: Integer);
begin
  if Assigned(FOnSubtitleTrackChange) then
    FOnSubtitleTrackChange(Self, 'sub', ID);
end;

procedure TMPVEngine.DoVideoTrackChange(ID: Integer);
begin
  if Assigned(FOnVideoTrackChange) then
    FOnVideoTrackChange(Self, 'video', ID);
end;

procedure TMPVEngine.DoMetadata(const Key, Value: string);
var
  I: Integer;
begin
  { Store in StreamInfo }
  for I := 0 to 15 do
  begin
    if (FStreamInfo.Metadata[I].Key = Key) or (FStreamInfo.Metadata[I].Key = '') then
    begin
      FStreamInfo.Metadata[I].Key := Key;
      FStreamInfo.Metadata[I].Value := Value;
      if FStreamInfo.MetadataCount <= I then
        FStreamInfo.MetadataCount := I + 1;
      Break;
    end;
  end;

  if Assigned(FOnMetadata) then
    FOnMetadata(Self, Key, Value);
end;

procedure TMPVEngine.DoVideoResize(Width, Height: Integer);
begin
  if Assigned(FOnVideoResize) then
    FOnVideoResize(Self, Width, Height);
end;

procedure TMPVEngine.DoProgress(const Name, Value: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Name, Value);
end;

end.
