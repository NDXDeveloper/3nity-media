{ ===============================================================================
  uMockMPVEngine.pas - Mock MPV Engine for Testing

  Part of 3nity Media - Test Suite

  Provides a mock implementation of TMPVEngine that simulates playback
  without requiring actual media files or libmpv. Useful for:
  - Unit testing components that depend on MPV
  - Integration testing without media dependencies
  - Simulating various playback states and events

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uMockMPVEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  { Status enum matching TMPVEngine }
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

  { Event types matching TMPVEngine }
  TMockStatusChangeEvent = procedure(Sender: TObject; OldStatus, NewStatus: TMPVStatus) of object;
  TMockPositionChangeEvent = procedure(Sender: TObject; PosSec, PosPct: Double) of object;
  TMockTrackChangeEvent = procedure(Sender: TObject; TrackID: Integer) of object;
  TMockMetadataEvent = procedure(Sender: TObject; const Key, Value: string) of object;
  TMockVideoResizeEvent = procedure(Sender: TObject; Width, Height: Integer) of object;
  TMockEndFileEvent = procedure(Sender: TObject; Reason: Integer) of object;
  TMockFileLoadedEvent = procedure(Sender: TObject; const FileName: string) of object;
  TMockErrorEvent = procedure(Sender: TObject; const Error: string) of object;

  { ===========================================================================
    TMockTrackInfo - Simulated track information
    =========================================================================== }
  TMockTrackInfo = record
    ID: Integer;
    Title: string;
    Language: string;
    Codec: string;
    IsDefault: Boolean;
    IsExternal: Boolean;
  end;

  { ===========================================================================
    TMockTrackList - Simulated track list
    =========================================================================== }
  TMockTrackList = class
  private
    FTracks: array of TMockTrackInfo;
    FCount: Integer;
    FSelectedID: Integer;
    FTrackType: string;
  public
    constructor Create(const ATrackType: string);
    procedure Clear;
    function Add(const Track: TMockTrackInfo): Integer;
    function FindByID(ID: Integer): Integer;
    function GetItem(Index: Integer): TMockTrackInfo;
    property Count: Integer read FCount;
    property Items[Index: Integer]: TMockTrackInfo read GetItem; default;
    property SelectedID: Integer read FSelectedID write FSelectedID;
    property TrackType: string read FTrackType;
  end;

  { ===========================================================================
    TMockStreamInfo - Simulated stream information
    =========================================================================== }
  TMockStreamInfo = record
    Title: string;
    Artist: string;
    Album: string;
    Genre: string;
    Date: string;
    Comment: string;
    Bitrate: Integer;
    SampleRate: Integer;
    Channels: Integer;
    VideoCodec: string;
    AudioCodec: string;
    VideoWidth: Integer;
    VideoHeight: Integer;
    VideoFPS: Double;
    HasVideo: Boolean;
    HasAudio: Boolean;
    IsStream: Boolean;
  end;

  { ===========================================================================
    TMockMPVEngine - Mock implementation of MPV Engine
    =========================================================================== }
  TMockMPVEngine = class
  private
    FStatus: TMPVStatus;
    FPosition: Double;
    FDuration: Double;
    FPercentPos: Double;
    FVolume: Integer;
    FMuted: Boolean;
    FSpeed: Double;
    FBrightness: Integer;
    FContrast: Integer;
    FSaturation: Integer;
    FHue: Integer;
    FGamma: Integer;
    FSubScale: Double;
    FSubDelay: Double;
    FSubVisible: Boolean;
    FAudioDelay: Double;
    FEqualizerEnabled: Boolean;
    FEqualizerBands: array[0..9] of Double;
    FPreamp: Double;
    FMediaFile: string;
    FInitialized: Boolean;
    FLastError: string;
    FHWAccel: Boolean;

    FAudioTracks: TMockTrackList;
    FSubtitleTracks: TMockTrackList;
    FVideoTracks: TMockTrackList;
    FStreamInfo: TMockStreamInfo;

    { Simulation control }
    FSimulatePlayback: Boolean;
    FSimulateError: Boolean;
    FSimulateErrorMessage: string;
    FCallLog: TStringList;

    { Events }
    FOnStatusChange: TMockStatusChangeEvent;
    FOnPositionChange: TMockPositionChangeEvent;
    FOnAudioTrackChange: TMockTrackChangeEvent;
    FOnSubtitleTrackChange: TMockTrackChangeEvent;
    FOnVideoTrackChange: TMockTrackChangeEvent;
    FOnMetadata: TMockMetadataEvent;
    FOnVideoResize: TMockVideoResizeEvent;
    FOnEndFile: TMockEndFileEvent;
    FOnFileLoaded: TMockFileLoadedEvent;
    FOnError: TMockErrorEvent;
    FOnSeek: TNotifyEvent;

    procedure SetStatus(Value: TMPVStatus);
    procedure SetVolume(Value: Integer);
    procedure SetMuted(Value: Boolean);
    procedure SetSpeed(Value: Double);
    procedure SetBrightness(Value: Integer);
    procedure SetContrast(Value: Integer);
    procedure SetSaturation(Value: Integer);
    procedure SetHue(Value: Integer);
    procedure SetGamma(Value: Integer);
    procedure LogCall(const AMethodName: string);
    procedure DoStatusChange(OldStatus, NewStatus: TMPVStatus);
    procedure DoPositionChange;
  public
    constructor Create;
    destructor Destroy; override;

    { Initialization }
    function Initialize(AWindowHandle: THandle; AWidth: Integer = 0; AHeight: Integer = 0): Boolean;
    procedure Shutdown;
    function IsRunning: Boolean;

    { Playback control }
    procedure PlayMedia(const URL: string);
    procedure CloseMedia;
    procedure Pause;
    procedure Resume;
    procedure Stop;
    procedure TogglePause;

    { Seeking }
    procedure SeekAbsolute(Seconds: Double);
    procedure SeekRelative(Seconds: Double);
    procedure SeekPercent(Percent: Double);

    { Equalizer }
    procedure SetEqualizerBand(Band: Integer; dB: Double);
    function GetEqualizerBand(Band: Integer): Double;
    procedure SetEqualizerPreset(const Values: string);
    function GetEqualizerPreset: string;
    procedure ResetEqualizer;
    procedure EnableEqualizer(Enable: Boolean);

    { Track management }
    procedure SetAudioTrack(TrackID: Integer);
    procedure SetSubtitleTrack(TrackID: Integer);
    procedure SetVideoTrack(TrackID: Integer);
    procedure LoadSubtitle(const FileName: string);

    { Simulation control - for tests }
    procedure LoadFile(const FileName: string);
    procedure SimulateFileLoad(const FileName: string; Duration: Double; HasVideo: Boolean = True);
    procedure SimulatePosition(Seconds: Double);
    procedure SimulateEndFile(Reason: Integer = 0);
    procedure SimulateError(const ErrorMsg: string);
    procedure SimulateMetadata(const Title, Artist: string);
    procedure SimulateTrackChange(TrackType: string; TrackID: Integer);
    procedure AddAudioTrack(ID: Integer; const Title, Lang, Codec: string);
    procedure AddVideoTrack(ID: Integer; const Title, Codec: string);
    procedure AddSubtitleTrack(ID: Integer; const Title, Lang: string; IsExternal: Boolean = False);
    procedure ClearTracks;
    function GetCallLog: TStringList;
    procedure ClearCallLog;

    { Read-only properties }
    property Status: TMPVStatus read FStatus write SetStatus;
    property Position: Double read FPosition;
    property Duration: Double read FDuration;
    property PercentPos: Double read FPercentPos;
    property MediaFile: string read FMediaFile;
    property Initialized: Boolean read FInitialized;
    property LastError: string read FLastError;
    property StreamInfo: TMockStreamInfo read FStreamInfo;
    property AudioTracks: TMockTrackList read FAudioTracks;
    property SubtitleTracks: TMockTrackList read FSubtitleTracks;
    property VideoTracks: TMockTrackList read FVideoTracks;

    { Read-write properties }
    property Volume: Integer read FVolume write SetVolume;
    property Muted: Boolean read FMuted write SetMuted;
    property Speed: Double read FSpeed write SetSpeed;
    property Brightness: Integer read FBrightness write SetBrightness;
    property Contrast: Integer read FContrast write SetContrast;
    property Saturation: Integer read FSaturation write SetSaturation;
    property Hue: Integer read FHue write SetHue;
    property Gamma: Integer read FGamma write SetGamma;
    property SubScale: Double read FSubScale write FSubScale;
    property SubDelay: Double read FSubDelay write FSubDelay;
    property SubVisible: Boolean read FSubVisible write FSubVisible;
    property AudioDelay: Double read FAudioDelay write FAudioDelay;
    property EqualizerEnabled: Boolean read FEqualizerEnabled;
    property Preamp: Double read FPreamp write FPreamp;
    property HWAccel: Boolean read FHWAccel write FHWAccel;

    { Events }
    property OnStatusChange: TMockStatusChangeEvent read FOnStatusChange write FOnStatusChange;
    property OnPositionChange: TMockPositionChangeEvent read FOnPositionChange write FOnPositionChange;
    property OnAudioTrackChange: TMockTrackChangeEvent read FOnAudioTrackChange write FOnAudioTrackChange;
    property OnSubtitleTrackChange: TMockTrackChangeEvent read FOnSubtitleTrackChange write FOnSubtitleTrackChange;
    property OnVideoTrackChange: TMockTrackChangeEvent read FOnVideoTrackChange write FOnVideoTrackChange;
    property OnMetadata: TMockMetadataEvent read FOnMetadata write FOnMetadata;
    property OnVideoResize: TMockVideoResizeEvent read FOnVideoResize write FOnVideoResize;
    property OnEndFile: TMockEndFileEvent read FOnEndFile write FOnEndFile;
    property OnFileLoaded: TMockFileLoadedEvent read FOnFileLoaded write FOnFileLoaded;
    property OnError: TMockErrorEvent read FOnError write FOnError;
    property OnSeek: TNotifyEvent read FOnSeek write FOnSeek;
  end;

implementation

{ ===============================================================================
  TMockTrackList
  =============================================================================== }

constructor TMockTrackList.Create(const ATrackType: string);
begin
  inherited Create;
  FTrackType := ATrackType;
  FCount := 0;
  FSelectedID := -1;
  SetLength(FTracks, 0);
end;

procedure TMockTrackList.Clear;
begin
  SetLength(FTracks, 0);
  FCount := 0;
  FSelectedID := -1;
end;

function TMockTrackList.Add(const Track: TMockTrackInfo): Integer;
begin
  Inc(FCount);
  SetLength(FTracks, FCount);
  FTracks[FCount - 1] := Track;
  Result := FCount - 1;
end;

function TMockTrackList.FindByID(ID: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FCount - 1 do
    if FTracks[I].ID = ID then
    begin
      Result := I;
      Exit;
    end;
end;

function TMockTrackList.GetItem(Index: Integer): TMockTrackInfo;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FTracks[Index]
  else
    Result := Default(TMockTrackInfo);
end;

{ ===============================================================================
  TMockMPVEngine
  =============================================================================== }

constructor TMockMPVEngine.Create;
var
  I: Integer;
begin
  inherited Create;
  FStatus := msNone;
  FPosition := 0;
  FDuration := 0;
  FPercentPos := 0;
  FVolume := 100;
  FMuted := False;
  FSpeed := 1.0;
  FBrightness := 0;
  FContrast := 0;
  FSaturation := 0;
  FHue := 0;
  FGamma := 0;
  FSubScale := 1.0;
  FSubDelay := 0;
  FSubVisible := True;
  FAudioDelay := 0;
  FEqualizerEnabled := False;
  FPreamp := 0;
  FInitialized := False;
  FHWAccel := True;
  FMediaFile := '';
  FLastError := '';

  for I := 0 to 9 do
    FEqualizerBands[I] := 0;

  FAudioTracks := TMockTrackList.Create('audio');
  FSubtitleTracks := TMockTrackList.Create('sub');
  FVideoTracks := TMockTrackList.Create('video');

  FCallLog := TStringList.Create;
  FSimulatePlayback := True;
  FSimulateError := False;

  FStreamInfo := Default(TMockStreamInfo);
end;

destructor TMockMPVEngine.Destroy;
begin
  FAudioTracks.Free;
  FSubtitleTracks.Free;
  FVideoTracks.Free;
  FCallLog.Free;
  inherited Destroy;
end;

procedure TMockMPVEngine.LogCall(const AMethodName: string);
begin
  FCallLog.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + AMethodName);
end;

procedure TMockMPVEngine.SetStatus(Value: TMPVStatus);
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

procedure TMockMPVEngine.DoStatusChange(OldStatus, NewStatus: TMPVStatus);
begin
  if Assigned(FOnStatusChange) then
    FOnStatusChange(Self, OldStatus, NewStatus);
end;

procedure TMockMPVEngine.DoPositionChange;
begin
  if FDuration > 0 then
    FPercentPos := (FPosition / FDuration) * 100
  else
    FPercentPos := 0;

  if Assigned(FOnPositionChange) then
    FOnPositionChange(Self, FPosition, FPercentPos);
end;

procedure TMockMPVEngine.SetVolume(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value > 150 then Value := 150;
  FVolume := Value;
  LogCall('SetVolume(' + IntToStr(Value) + ')');
end;

procedure TMockMPVEngine.SetMuted(Value: Boolean);
begin
  FMuted := Value;
  LogCall('SetMuted(' + BoolToStr(Value, True) + ')');
end;

procedure TMockMPVEngine.SetSpeed(Value: Double);
begin
  if Value < 0.1 then Value := 0.1;
  if Value > 4.0 then Value := 4.0;
  FSpeed := Value;
  LogCall('SetSpeed(' + FloatToStr(Value) + ')');
end;

procedure TMockMPVEngine.SetBrightness(Value: Integer);
begin
  if Value < -100 then Value := -100;
  if Value > 100 then Value := 100;
  FBrightness := Value;
  LogCall('SetBrightness(' + IntToStr(Value) + ')');
end;

procedure TMockMPVEngine.SetContrast(Value: Integer);
begin
  if Value < -100 then Value := -100;
  if Value > 100 then Value := 100;
  FContrast := Value;
  LogCall('SetContrast(' + IntToStr(Value) + ')');
end;

procedure TMockMPVEngine.SetSaturation(Value: Integer);
begin
  if Value < -100 then Value := -100;
  if Value > 100 then Value := 100;
  FSaturation := Value;
  LogCall('SetSaturation(' + IntToStr(Value) + ')');
end;

procedure TMockMPVEngine.SetHue(Value: Integer);
begin
  if Value < -100 then Value := -100;
  if Value > 100 then Value := 100;
  FHue := Value;
  LogCall('SetHue(' + IntToStr(Value) + ')');
end;

procedure TMockMPVEngine.SetGamma(Value: Integer);
begin
  if Value < -100 then Value := -100;
  if Value > 100 then Value := 100;
  FGamma := Value;
  LogCall('SetGamma(' + IntToStr(Value) + ')');
end;

function TMockMPVEngine.Initialize(AWindowHandle: THandle; AWidth: Integer; AHeight: Integer): Boolean;
begin
  LogCall('Initialize');
  if FSimulateError then
  begin
    FLastError := FSimulateErrorMessage;
    Result := False;
    Exit;
  end;

  FInitialized := True;
  SetStatus(msNone);
  Result := True;
end;

procedure TMockMPVEngine.Shutdown;
begin
  LogCall('Shutdown');
  FInitialized := False;
  SetStatus(msNone);
  FMediaFile := '';
  FPosition := 0;
  FDuration := 0;
  ClearTracks;
end;

function TMockMPVEngine.IsRunning: Boolean;
begin
  Result := FStatus in [msPlaying, msPaused, msOpening, msPlayStarting];
end;

procedure TMockMPVEngine.PlayMedia(const URL: string);
begin
  LogCall('PlayMedia(' + URL + ')');
  if not FInitialized then
  begin
    FLastError := 'Engine not initialized';
    SetStatus(msError);
    Exit;
  end;

  if FSimulateError then
  begin
    FLastError := FSimulateErrorMessage;
    SetStatus(msError);
    Exit;
  end;

  FMediaFile := URL;
  FPosition := 0;
  SetStatus(msPlaying);

  if Assigned(FOnFileLoaded) then
    FOnFileLoaded(Self, URL);
end;

procedure TMockMPVEngine.CloseMedia;
begin
  LogCall('CloseMedia');
  FMediaFile := '';
  FPosition := 0;
  FDuration := 0;
  SetStatus(msNone);
  ClearTracks;
end;

procedure TMockMPVEngine.Pause;
begin
  LogCall('Pause');
  if FStatus = msPlaying then
    SetStatus(msPaused);
end;

procedure TMockMPVEngine.Resume;
begin
  LogCall('Resume');
  if FStatus = msPaused then
    SetStatus(msPlaying);
end;

procedure TMockMPVEngine.Stop;
begin
  LogCall('Stop');
  SetStatus(msStopped);
  FPosition := 0;
end;

procedure TMockMPVEngine.TogglePause;
begin
  LogCall('TogglePause');
  case FStatus of
    msPlaying: SetStatus(msPaused);
    msPaused: SetStatus(msPlaying);
  end;
end;

procedure TMockMPVEngine.SeekAbsolute(Seconds: Double);
begin
  LogCall('SeekAbsolute(' + FloatToStr(Seconds) + ')');
  if Seconds < 0 then Seconds := 0;
  if (FDuration > 0) and (Seconds > FDuration) then
    Seconds := FDuration;
  FPosition := Seconds;
  DoPositionChange;
  if Assigned(FOnSeek) then
    FOnSeek(Self);
end;

procedure TMockMPVEngine.SeekRelative(Seconds: Double);
begin
  LogCall('SeekRelative(' + FloatToStr(Seconds) + ')');
  SeekAbsolute(FPosition + Seconds);
end;

procedure TMockMPVEngine.SeekPercent(Percent: Double);
begin
  LogCall('SeekPercent(' + FloatToStr(Percent) + ')');
  if FDuration > 0 then
    SeekAbsolute((Percent / 100) * FDuration);
end;

procedure TMockMPVEngine.SetEqualizerBand(Band: Integer; dB: Double);
begin
  if (Band >= 0) and (Band <= 9) then
  begin
    if dB < -12 then dB := -12;
    if dB > 12 then dB := 12;
    FEqualizerBands[Band] := dB;
    LogCall('SetEqualizerBand(' + IntToStr(Band) + ', ' + FloatToStr(dB) + ')');
  end;
end;

function TMockMPVEngine.GetEqualizerBand(Band: Integer): Double;
begin
  if (Band >= 0) and (Band <= 9) then
    Result := FEqualizerBands[Band]
  else
    Result := 0;
end;

procedure TMockMPVEngine.SetEqualizerPreset(const Values: string);
var
  Parts: TStringArray;
  I: Integer;
begin
  LogCall('SetEqualizerPreset(' + Values + ')');
  Parts := Values.Split(':');
  for I := 0 to Min(High(Parts), 9) do
    FEqualizerBands[I] := StrToFloatDef(Parts[I], 0);
end;

function TMockMPVEngine.GetEqualizerPreset: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to 9 do
  begin
    if I > 0 then Result := Result + ':';
    Result := Result + FloatToStr(FEqualizerBands[I]);
  end;
end;

procedure TMockMPVEngine.ResetEqualizer;
var
  I: Integer;
begin
  LogCall('ResetEqualizer');
  for I := 0 to 9 do
    FEqualizerBands[I] := 0;
  FPreamp := 0;
end;

procedure TMockMPVEngine.EnableEqualizer(Enable: Boolean);
begin
  LogCall('EnableEqualizer(' + BoolToStr(Enable, True) + ')');
  FEqualizerEnabled := Enable;
end;

procedure TMockMPVEngine.SetAudioTrack(TrackID: Integer);
begin
  LogCall('SetAudioTrack(' + IntToStr(TrackID) + ')');
  FAudioTracks.SelectedID := TrackID;
  if Assigned(FOnAudioTrackChange) then
    FOnAudioTrackChange(Self, TrackID);
end;

procedure TMockMPVEngine.SetSubtitleTrack(TrackID: Integer);
begin
  LogCall('SetSubtitleTrack(' + IntToStr(TrackID) + ')');
  FSubtitleTracks.SelectedID := TrackID;
  if Assigned(FOnSubtitleTrackChange) then
    FOnSubtitleTrackChange(Self, TrackID);
end;

procedure TMockMPVEngine.SetVideoTrack(TrackID: Integer);
begin
  LogCall('SetVideoTrack(' + IntToStr(TrackID) + ')');
  FVideoTracks.SelectedID := TrackID;
  if Assigned(FOnVideoTrackChange) then
    FOnVideoTrackChange(Self, TrackID);
end;

procedure TMockMPVEngine.LoadSubtitle(const FileName: string);
var
  Track: TMockTrackInfo;
begin
  LogCall('LoadSubtitle(' + FileName + ')');
  Track := Default(TMockTrackInfo);
  Track.ID := FSubtitleTracks.Count + 1;
  Track.Title := ExtractFileName(FileName);
  Track.IsExternal := True;
  FSubtitleTracks.Add(Track);
end;

{ Simulation methods for testing }

procedure TMockMPVEngine.LoadFile(const FileName: string);
begin
  { Simple wrapper - just stores the file name without simulating playback }
  FMediaFile := FileName;
  FCallLog.Add('LoadFile:' + FileName);
end;

procedure TMockMPVEngine.SimulateFileLoad(const FileName: string; Duration: Double; HasVideo: Boolean);
begin
  FMediaFile := FileName;
  FDuration := Duration;
  FPosition := 0;
  FStreamInfo.HasVideo := HasVideo;
  FStreamInfo.HasAudio := True;
  SetStatus(msPlaying);
  if Assigned(FOnFileLoaded) then
    FOnFileLoaded(Self, FileName);
  if HasVideo and Assigned(FOnVideoResize) then
    FOnVideoResize(Self, 1920, 1080);
end;

procedure TMockMPVEngine.SimulatePosition(Seconds: Double);
begin
  FPosition := Seconds;
  DoPositionChange;
end;

procedure TMockMPVEngine.SimulateEndFile(Reason: Integer);
begin
  SetStatus(msStopped);
  if Assigned(FOnEndFile) then
    FOnEndFile(Self, Reason);
end;

procedure TMockMPVEngine.SimulateError(const ErrorMsg: string);
begin
  FLastError := ErrorMsg;
  SetStatus(msError);
  if Assigned(FOnError) then
    FOnError(Self, ErrorMsg);
end;

procedure TMockMPVEngine.SimulateMetadata(const Title, Artist: string);
begin
  FStreamInfo.Title := Title;
  FStreamInfo.Artist := Artist;
  if Assigned(FOnMetadata) then
  begin
    FOnMetadata(Self, 'title', Title);
    FOnMetadata(Self, 'artist', Artist);
  end;
end;

procedure TMockMPVEngine.SimulateTrackChange(TrackType: string; TrackID: Integer);
begin
  if TrackType = 'audio' then
    SetAudioTrack(TrackID)
  else if TrackType = 'sub' then
    SetSubtitleTrack(TrackID)
  else if TrackType = 'video' then
    SetVideoTrack(TrackID);
end;

procedure TMockMPVEngine.AddAudioTrack(ID: Integer; const Title, Lang, Codec: string);
var
  Track: TMockTrackInfo;
begin
  Track := Default(TMockTrackInfo);
  Track.ID := ID;
  Track.Title := Title;
  Track.Language := Lang;
  Track.Codec := Codec;
  Track.IsDefault := FAudioTracks.Count = 0;
  FAudioTracks.Add(Track);
end;

procedure TMockMPVEngine.AddVideoTrack(ID: Integer; const Title, Codec: string);
var
  Track: TMockTrackInfo;
begin
  Track := Default(TMockTrackInfo);
  Track.ID := ID;
  Track.Title := Title;
  Track.Codec := Codec;
  Track.IsDefault := FVideoTracks.Count = 0;
  FVideoTracks.Add(Track);
end;

procedure TMockMPVEngine.AddSubtitleTrack(ID: Integer; const Title, Lang: string; IsExternal: Boolean);
var
  Track: TMockTrackInfo;
begin
  Track := Default(TMockTrackInfo);
  Track.ID := ID;
  Track.Title := Title;
  Track.Language := Lang;
  Track.IsExternal := IsExternal;
  Track.IsDefault := (FSubtitleTracks.Count = 0) and not IsExternal;
  FSubtitleTracks.Add(Track);
end;

procedure TMockMPVEngine.ClearTracks;
begin
  FAudioTracks.Clear;
  FSubtitleTracks.Clear;
  FVideoTracks.Clear;
end;

function TMockMPVEngine.GetCallLog: TStringList;
begin
  Result := FCallLog;
end;

procedure TMockMPVEngine.ClearCallLog;
begin
  FCallLog.Clear;
end;

end.
