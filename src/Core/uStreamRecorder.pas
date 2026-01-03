{ ═══════════════════════════════════════════════════════════════════════════════
  uStreamRecorder.pas - Stream Recording Manager

  Part of 3nity Media - Lazarus Edition

  This unit implements stream recording functionality:
  - Record radio streams to MP3/OGG/FLAC files
  - Record video streams to MP4/MKV/WebM files
  - Automatic file naming with metadata
  - Recording status and progress tracking
  - Split recording by time or size

  Phase 27: Stream Recording

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uStreamRecorder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  { ═══════════════════════════════════════════════════════════════════════════
    RECORDING TYPES
    ═══════════════════════════════════════════════════════════════════════════ }

  { Recording state }
  TRecordingState = (
    rsIdle,           { Not recording }
    rsRecording,      { Currently recording }
    rsPaused,         { Recording paused (if supported) }
    rsStopping,       { Stopping recording }
    rsError           { Error occurred }
  );

  { Recording format for audio streams }
  TAudioRecordFormat = (
    arfMP3,           { MP3 format (default for radio) }
    arfOGG,           { OGG Vorbis }
    arfFLAC,          { FLAC lossless }
    arfWAV,           { WAV uncompressed }
    arfAAC,           { AAC format }
    arfOpus           { Opus format }
  );

  { Recording format for video streams }
  TVideoRecordFormat = (
    vrfMP4,           { MP4 container (default) }
    vrfMKV,           { Matroska container }
    vrfWebM,          { WebM format }
    vrfTS,            { MPEG-TS (for streaming) }
    vrfAVI            { AVI container }
  );

  { Recording settings }
  TRecordingSettings = record
    OutputFolder: string;           { Where to save audio recordings }
    OutputFolderVideo: string;      { Where to save video recordings }
    AudioFormat: TAudioRecordFormat;
    VideoFormat: TVideoRecordFormat;
    UseMetadataInFilename: Boolean; { Include artist/title in filename }
    AutoSplit: Boolean;             { Auto-split recordings }
    SplitSizeMB: Integer;           { Split at this size (MB) }
    SplitTimeMinutes: Integer;      { Split at this time (minutes) }
    AppendTimestamp: Boolean;       { Add timestamp to filename }
    CreateSubfolders: Boolean;      { Create subfolders by date/source }
    OverwriteExisting: Boolean;     { Overwrite or create unique names }
  end;

  { Recording info }
  TRecordingInfo = record
    State: TRecordingState;
    FileName: string;               { Current output file }
    SourceURL: string;              { Source being recorded }
    SourceName: string;             { Friendly name (station name, etc.) }
    StartTime: TDateTime;           { When recording started }
    Duration: TTime;                { Recording duration }
    FileSize: Int64;                { Current file size }
    IsAudioOnly: Boolean;           { True for audio-only streams }
    CurrentTitle: string;           { Current track title (for radio) }
    TrackCount: Integer;            { Number of tracks recorded }
    ErrorMessage: string;           { Last error message }
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    EVENTS
    ═══════════════════════════════════════════════════════════════════════════ }

  TRecordingStateChangeEvent = procedure(Sender: TObject; OldState, NewState: TRecordingState) of object;
  TRecordingProgressEvent = procedure(Sender: TObject; Duration: TTime; FileSize: Int64) of object;
  TRecordingTrackChangeEvent = procedure(Sender: TObject; const Artist, Title: string) of object;
  TRecordingErrorEvent = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TRecordingFileSplitEvent = procedure(Sender: TObject; const OldFile, NewFile: string) of object;

  { ═══════════════════════════════════════════════════════════════════════════
    TSTREAMRECORDER CLASS
    ═══════════════════════════════════════════════════════════════════════════ }

  TStreamRecorder = class
  private
    FSettings: TRecordingSettings;
    FInfo: TRecordingInfo;
    FRecordingPath: string;
    FFileIndex: Integer;

    { Events }
    FOnStateChange: TRecordingStateChangeEvent;
    FOnProgress: TRecordingProgressEvent;
    FOnTrackChange: TRecordingTrackChangeEvent;
    FOnError: TRecordingErrorEvent;
    FOnFileSplit: TRecordingFileSplitEvent;

    procedure SetState(NewState: TRecordingState);
    function GenerateFileName(const SourceName: string; IsAudio: Boolean): string;
    function GetAudioExtension: string;
    function GetVideoExtension: string;
    function SanitizeFileName(const FileName: string): string;
    function EnsureUniqueFileName(const FileName: string): string;
    procedure UpdateFileSize;
    function GetIsRecording: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    { Initialize with default settings }
    procedure Initialize;

    { Load/Save settings }
    procedure LoadSettings(const IniPath: string);
    procedure SaveSettings(const IniPath: string);

    { Recording control }
    function StartRecording(const SourceURL, SourceName: string; IsAudioOnly: Boolean): Boolean;
    procedure StopRecording;
    procedure PauseRecording;
    procedure ResumeRecording;

    { Get the mpv stream-record path for current recording }
    function GetMPVRecordPath: string;

    { Update recording info (call periodically) }
    procedure UpdateProgress;

    { Track change notification (for radio ICY metadata) }
    procedure NotifyTrackChange(const Artist, Title: string);

    { Check if should split file }
    function ShouldSplitFile: Boolean;

    { Split current recording to new file }
    function SplitRecording: string;

    { Properties }
    property Settings: TRecordingSettings read FSettings write FSettings;
    property Info: TRecordingInfo read FInfo;
    property RecordingPath: string read FRecordingPath;
    property IsRecording: Boolean read GetIsRecording;

    { Events }
    property OnStateChange: TRecordingStateChangeEvent read FOnStateChange write FOnStateChange;
    property OnProgress: TRecordingProgressEvent read FOnProgress write FOnProgress;
    property OnTrackChange: TRecordingTrackChangeEvent read FOnTrackChange write FOnTrackChange;
    property OnError: TRecordingErrorEvent read FOnError write FOnError;
    property OnFileSplit: TRecordingFileSplitEvent read FOnFileSplit write FOnFileSplit;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    HELPER FUNCTIONS
    ═══════════════════════════════════════════════════════════════════════════ }

function AudioFormatToString(Format: TAudioRecordFormat): string;
function StringToAudioFormat(const S: string): TAudioRecordFormat;
function VideoFormatToString(Format: TVideoRecordFormat): string;
function StringToVideoFormat(const S: string): TVideoRecordFormat;
function RecordingStateToString(State: TRecordingState): string;
function FormatRecordingDuration(Duration: TTime): string;
function FormatRecordingSize(Size: Int64): string;

implementation

uses
  IniFiles;

{ ═══════════════════════════════════════════════════════════════════════════════
  HELPER FUNCTIONS
  ═══════════════════════════════════════════════════════════════════════════════ }

function AudioFormatToString(Format: TAudioRecordFormat): string;
begin
  case Format of
    arfMP3:  Result := 'MP3';
    arfOGG:  Result := 'OGG';
    arfFLAC: Result := 'FLAC';
    arfWAV:  Result := 'WAV';
    arfAAC:  Result := 'AAC';
    arfOpus: Result := 'Opus';
  else
    Result := 'MP3';
  end;
end;

function StringToAudioFormat(const S: string): TAudioRecordFormat;
var
  LowerS: string;
begin
  LowerS := LowerCase(S);
  if LowerS = 'ogg' then Result := arfOGG
  else if LowerS = 'flac' then Result := arfFLAC
  else if LowerS = 'wav' then Result := arfWAV
  else if LowerS = 'aac' then Result := arfAAC
  else if LowerS = 'opus' then Result := arfOpus
  else Result := arfMP3;
end;

function VideoFormatToString(Format: TVideoRecordFormat): string;
begin
  case Format of
    vrfMP4:  Result := 'MP4';
    vrfMKV:  Result := 'MKV';
    vrfWebM: Result := 'WebM';
    vrfTS:   Result := 'TS';
    vrfAVI:  Result := 'AVI';
  else
    Result := 'MP4';
  end;
end;

function StringToVideoFormat(const S: string): TVideoRecordFormat;
var
  LowerS: string;
begin
  LowerS := LowerCase(S);
  if LowerS = 'mkv' then Result := vrfMKV
  else if LowerS = 'webm' then Result := vrfWebM
  else if LowerS = 'ts' then Result := vrfTS
  else if LowerS = 'avi' then Result := vrfAVI
  else Result := vrfMP4;
end;

function RecordingStateToString(State: TRecordingState): string;
begin
  case State of
    rsIdle:      Result := 'Idle';
    rsRecording: Result := 'Recording';
    rsPaused:    Result := 'Paused';
    rsStopping:  Result := 'Stopping';
    rsError:     Result := 'Error';
  else
    Result := 'Unknown';
  end;
end;

function FormatRecordingDuration(Duration: TTime): string;
var
  H, M, S, MS: Word;
begin
  DecodeTime(Duration, H, M, S, MS);
  if H > 0 then
    Result := Format('%d:%02d:%02d', [H, M, S])
  else
    Result := Format('%02d:%02d', [M, S]);
end;

function FormatRecordingSize(Size: Int64): string;
begin
  if Size < 1024 then
    Result := Format('%d B', [Size])
  else if Size < 1024 * 1024 then
    Result := Format('%.1f KB', [Size / 1024])
  else if Size < 1024 * 1024 * 1024 then
    Result := Format('%.2f MB', [Size / (1024 * 1024)])
  else
    Result := Format('%.2f GB', [Size / (1024 * 1024 * 1024)]);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TSTREAMRECORDER IMPLEMENTATION
  ═══════════════════════════════════════════════════════════════════════════════ }

constructor TStreamRecorder.Create;
begin
  inherited Create;
  Initialize;
end;

destructor TStreamRecorder.Destroy;
begin
  if FInfo.State = rsRecording then
    StopRecording;
  inherited Destroy;
end;

procedure TStreamRecorder.Initialize;
begin
  FFileIndex := 0;
  FRecordingPath := '';

  { Default settings }
  with FSettings do
  begin
    OutputFolder := GetUserDir + 'Music' + DirectorySeparator + '3nity Recordings';
    OutputFolderVideo := GetUserDir + 'Videos' + DirectorySeparator + '3nity Recordings';
    AudioFormat := arfMP3;
    VideoFormat := vrfMP4;
    UseMetadataInFilename := True;
    AutoSplit := False;
    SplitSizeMB := 100;
    SplitTimeMinutes := 60;
    AppendTimestamp := True;
    CreateSubfolders := True;
    OverwriteExisting := False;
  end;

  { Reset info }
  with FInfo do
  begin
    State := rsIdle;
    FileName := '';
    SourceURL := '';
    SourceName := '';
    StartTime := 0;
    Duration := 0;
    FileSize := 0;
    IsAudioOnly := True;
    CurrentTitle := '';
    TrackCount := 0;
    ErrorMessage := '';
  end;
end;

procedure TStreamRecorder.SetState(NewState: TRecordingState);
var
  OldState: TRecordingState;
begin
  if FInfo.State <> NewState then
  begin
    OldState := FInfo.State;
    FInfo.State := NewState;
    if Assigned(FOnStateChange) then
      FOnStateChange(Self, OldState, NewState);
  end;
end;

function TStreamRecorder.GetAudioExtension: string;
begin
  case FSettings.AudioFormat of
    arfMP3:  Result := '.mp3';
    arfOGG:  Result := '.ogg';
    arfFLAC: Result := '.flac';
    arfWAV:  Result := '.wav';
    arfAAC:  Result := '.aac';
    arfOpus: Result := '.opus';
  else
    Result := '.mp3';
  end;
end;

function TStreamRecorder.GetVideoExtension: string;
begin
  case FSettings.VideoFormat of
    vrfMP4:  Result := '.mp4';
    vrfMKV:  Result := '.mkv';
    vrfWebM: Result := '.webm';
    vrfTS:   Result := '.ts';
    vrfAVI:  Result := '.avi';
  else
    Result := '.mp4';
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  SanitizeFileName - Remove invalid characters from filename

  Purpose: Cleans a filename by replacing characters that are invalid on
           Windows/Linux file systems with underscores.

  Parameters:
    - FileName: The original filename (without path)

  Returns: Sanitized filename safe for use on any file system

  Invalid characters replaced:
    - Path separators: / \
    - Reserved chars: : * ? " < > |
    - Control chars: #0..#31 (ASCII control codes)

  Notes:
    - Replaces invalid chars with underscore (_) instead of removing
    - Trims leading/trailing whitespace
    - Truncates to 200 characters max (filesystem safety)
    - Used by GenerateFileName to create valid output filenames
  ═══════════════════════════════════════════════════════════════════════════ }
function TStreamRecorder.SanitizeFileName(const FileName: string): string;
var
  I: Integer;
  InvalidChars: set of Char;
begin
  InvalidChars := ['/', '\', ':', '*', '?', '"', '<', '>', '|', #0..#31];
  Result := '';
  for I := 1 to Length(FileName) do
  begin
    if not (FileName[I] in InvalidChars) then
      Result := Result + FileName[I]
    else
      Result := Result + '_';
  end;
  { Trim and limit length }
  Result := Trim(Result);
  if Length(Result) > 200 then
    Result := Copy(Result, 1, 200);
end;

function TStreamRecorder.EnsureUniqueFileName(const FileName: string): string;
var
  BaseName, Ext, Dir: string;
  Counter: Integer;
begin
  if FSettings.OverwriteExisting or not FileExists(FileName) then
  begin
    Result := FileName;
    Exit;
  end;

  Dir := ExtractFilePath(FileName);
  BaseName := ChangeFileExt(ExtractFileName(FileName), '');
  Ext := ExtractFileExt(FileName);
  Counter := 1;

  repeat
    Result := Dir + BaseName + Format(' (%d)', [Counter]) + Ext;
    Inc(Counter);
  until not FileExists(Result) or (Counter > 999);
end;

{ ═══════════════════════════════════════════════════════════════════════════
  GenerateFileName - Build complete output path for recording

  Purpose: Constructs a full file path for a new recording based on settings,
           source name, and content type (audio or video).

  Parameters:
    - SourceName: Display name of the source (e.g., radio station name)
    - IsAudio: True for audio recordings, False for video

  Returns: Complete file path ready for recording

  Path construction steps:
    1. Select base folder (audio vs video output folder)
    2. Create subfolder if enabled (format: yyyy-mm)
    3. Sanitize source name for filesystem safety
    4. Append timestamp if enabled (format: _yyyy-mm-dd_hhnnss)
    5. Add appropriate extension based on format setting
    6. Ensure unique filename (add (1), (2), etc. if exists)

  Notes:
    - Creates directories if they don't exist (ForceDirectories)
    - Falls back to "Recording" if source name is empty
    - Uses EnsureUniqueFileName to prevent overwrites (unless disabled)
  ═══════════════════════════════════════════════════════════════════════════ }
function TStreamRecorder.GenerateFileName(const SourceName: string; IsAudio: Boolean): string;
var
  BaseFolder, SubFolder, BaseName, Ext, TimeStamp: string;
begin
  { Determine base folder based on content type }
  if IsAudio then
    BaseFolder := FSettings.OutputFolder
  else
    BaseFolder := FSettings.OutputFolderVideo;
  if not DirectoryExists(BaseFolder) then
    ForceDirectories(BaseFolder);

  { Create subfolder if enabled }
  if FSettings.CreateSubfolders then
  begin
    SubFolder := FormatDateTime('yyyy-mm', Now);
    BaseFolder := BaseFolder + DirectorySeparator + SubFolder;
    if not DirectoryExists(BaseFolder) then
      ForceDirectories(BaseFolder);
  end;

  { Build base name }
  BaseName := SanitizeFileName(SourceName);
  if BaseName = '' then
    BaseName := 'Recording';

  { Add timestamp if enabled }
  if FSettings.AppendTimestamp then
  begin
    TimeStamp := FormatDateTime('_yyyy-mm-dd_hhnnss', Now);
    BaseName := BaseName + TimeStamp;
  end;

  { Add extension }
  if IsAudio then
    Ext := GetAudioExtension
  else
    Ext := GetVideoExtension;

  { Build full path }
  Result := BaseFolder + DirectorySeparator + BaseName + Ext;

  { Ensure unique }
  Result := EnsureUniqueFileName(Result);
end;

procedure TStreamRecorder.LoadSettings(const IniPath: string);
var
  Ini: TIniFile;
begin
  if not FileExists(IniPath) then Exit;

  Ini := TIniFile.Create(IniPath);
  try
    with FSettings do
    begin
      OutputFolder := Ini.ReadString('Recording', 'OutputFolder', OutputFolder);
      OutputFolderVideo := Ini.ReadString('Recording', 'OutputFolderVideo', OutputFolderVideo);
      AudioFormat := StringToAudioFormat(Ini.ReadString('Recording', 'AudioFormat', 'MP3'));
      VideoFormat := StringToVideoFormat(Ini.ReadString('Recording', 'VideoFormat', 'MP4'));
      UseMetadataInFilename := Ini.ReadBool('Recording', 'UseMetadataInFilename', True);
      AutoSplit := Ini.ReadBool('Recording', 'AutoSplit', False);
      SplitSizeMB := Ini.ReadInteger('Recording', 'SplitSizeMB', 100);
      SplitTimeMinutes := Ini.ReadInteger('Recording', 'SplitTimeMinutes', 60);
      AppendTimestamp := Ini.ReadBool('Recording', 'AppendTimestamp', True);
      CreateSubfolders := Ini.ReadBool('Recording', 'CreateSubfolders', True);
      OverwriteExisting := Ini.ReadBool('Recording', 'OverwriteExisting', False);
    end;
  finally
    Ini.Free;
  end;
end;

procedure TStreamRecorder.SaveSettings(const IniPath: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(IniPath);
  try
    with FSettings do
    begin
      Ini.WriteString('Recording', 'OutputFolder', OutputFolder);
      Ini.WriteString('Recording', 'OutputFolderVideo', OutputFolderVideo);
      Ini.WriteString('Recording', 'AudioFormat', AudioFormatToString(AudioFormat));
      Ini.WriteString('Recording', 'VideoFormat', VideoFormatToString(VideoFormat));
      Ini.WriteBool('Recording', 'UseMetadataInFilename', UseMetadataInFilename);
      Ini.WriteBool('Recording', 'AutoSplit', AutoSplit);
      Ini.WriteInteger('Recording', 'SplitSizeMB', SplitSizeMB);
      Ini.WriteInteger('Recording', 'SplitTimeMinutes', SplitTimeMinutes);
      Ini.WriteBool('Recording', 'AppendTimestamp', AppendTimestamp);
      Ini.WriteBool('Recording', 'CreateSubfolders', CreateSubfolders);
      Ini.WriteBool('Recording', 'OverwriteExisting', OverwriteExisting);
    end;
  finally
    Ini.Free;
  end;
end;

function TStreamRecorder.StartRecording(const SourceURL, SourceName: string; IsAudioOnly: Boolean): Boolean;
begin
  Result := False;

  { Check if already recording }
  if FInfo.State = rsRecording then
  begin
    FInfo.ErrorMessage := 'Already recording';
    if Assigned(FOnError) then
      FOnError(Self, FInfo.ErrorMessage);
    Exit;
  end;

  { Generate output filename }
  FRecordingPath := GenerateFileName(SourceName, IsAudioOnly);

  { Ensure directory exists }
  if not DirectoryExists(ExtractFilePath(FRecordingPath)) then
  begin
    if not ForceDirectories(ExtractFilePath(FRecordingPath)) then
    begin
      FInfo.ErrorMessage := 'Cannot create output directory';
      if Assigned(FOnError) then
        FOnError(Self, FInfo.ErrorMessage);
      Exit;
    end;
  end;

  { Set recording info }
  FInfo.FileName := FRecordingPath;
  FInfo.SourceURL := SourceURL;
  FInfo.SourceName := SourceName;
  FInfo.StartTime := Now;
  FInfo.Duration := 0;
  FInfo.FileSize := 0;
  FInfo.IsAudioOnly := IsAudioOnly;
  FInfo.CurrentTitle := '';
  FInfo.TrackCount := 0;
  FInfo.ErrorMessage := '';
  FFileIndex := 1;

  { Set state to recording }
  SetState(rsRecording);

  Result := True;
end;

procedure TStreamRecorder.StopRecording;
begin
  if FInfo.State <> rsRecording then Exit;

  SetState(rsStopping);

  { Update final file size }
  UpdateFileSize;

  { Clear recording path }
  FRecordingPath := '';

  SetState(rsIdle);
end;

procedure TStreamRecorder.PauseRecording;
begin
  { Note: mpv doesn't support pause for stream-record, this is a placeholder }
  if FInfo.State = rsRecording then
    SetState(rsPaused);
end;

procedure TStreamRecorder.ResumeRecording;
begin
  if FInfo.State = rsPaused then
    SetState(rsRecording);
end;

function TStreamRecorder.GetMPVRecordPath: string;
begin
  { Return the path that should be used for mpv's stream-record property }
  if FInfo.State = rsRecording then
    Result := FRecordingPath
  else
    Result := '';
end;

procedure TStreamRecorder.UpdateProgress;
var
  ElapsedTime: TDateTime;
begin
  if FInfo.State <> rsRecording then Exit;

  { Calculate duration }
  ElapsedTime := Now - FInfo.StartTime;
  FInfo.Duration := Frac(ElapsedTime); { Get time part only }

  { Update file size }
  UpdateFileSize;

  { Fire progress event }
  if Assigned(FOnProgress) then
    FOnProgress(Self, FInfo.Duration, FInfo.FileSize);

  { Check if we need to split }
  if FSettings.AutoSplit and ShouldSplitFile then
    SplitRecording;
end;

procedure TStreamRecorder.UpdateFileSize;
var
  F: file of Byte;
begin
  if (FRecordingPath <> '') and FileExists(FRecordingPath) then
  begin
    try
      AssignFile(F, FRecordingPath);
      Reset(F);
      FInfo.FileSize := FileSize(F);
      CloseFile(F);
    except
      { Ignore errors, file might be locked }
    end;
  end;
end;

function TStreamRecorder.GetIsRecording: Boolean;
begin
  Result := FInfo.State = rsRecording;
end;

procedure TStreamRecorder.NotifyTrackChange(const Artist, Title: string);
var
  NewTitle: string;
begin
  if Artist <> '' then
    NewTitle := Artist + ' - ' + Title
  else
    NewTitle := Title;

  if NewTitle <> FInfo.CurrentTitle then
  begin
    FInfo.CurrentTitle := NewTitle;
    Inc(FInfo.TrackCount);

    if Assigned(FOnTrackChange) then
      FOnTrackChange(Self, Artist, Title);
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  ShouldSplitFile - Check if recording should be split to new file

  Purpose: Determines whether the current recording has exceeded configured
           limits (size or time) and should be split into a new file.

  Returns: True if split is needed, False otherwise

  Split conditions (checked in order):
    1. AutoSplit must be enabled in settings
    2. Size limit: FInfo.FileSize >= SplitSizeMB (if > 0)
    3. Time limit: Elapsed time >= SplitTimeMinutes (if > 0)

  Notes:
    - Called periodically during recording (from ProcessLoop)
    - Either condition triggers a split (they are OR'd)
    - Setting limit to 0 disables that condition
    - File size converted from bytes to MB for comparison
    - Uses MinutesBetween for time calculation
  ═══════════════════════════════════════════════════════════════════════════ }
function TStreamRecorder.ShouldSplitFile: Boolean;
var
  ElapsedMinutes: Integer;
  FileSizeMB: Double;
begin
  Result := False;

  if not FSettings.AutoSplit then Exit;

  { Check size limit }
  if FSettings.SplitSizeMB > 0 then
  begin
    FileSizeMB := FInfo.FileSize / (1024 * 1024);
    if FileSizeMB >= FSettings.SplitSizeMB then
    begin
      Result := True;
      Exit;
    end;
  end;

  { Check time limit }
  if FSettings.SplitTimeMinutes > 0 then
  begin
    ElapsedMinutes := MinutesBetween(Now, FInfo.StartTime);
    if ElapsedMinutes >= FSettings.SplitTimeMinutes then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  SplitRecording - Create new file and continue recording

  Purpose: Splits the current recording to a new file while maintaining
           continuous capture. Used for long recordings or size limits.

  Returns: Path to the new recording file, or empty string if not recording

  Naming scheme:
    - Appends _p01, _p02, etc. to base filename
    - Format: BaseName_p02.ext (two-digit part number)
    - Removes existing part suffix before adding new one

  Algorithm:
    1. Increment file index counter (FFileIndex)
    2. Extract base filename, removing any existing _pNN suffix
    3. Generate new filename with updated part number
    4. Ensure uniqueness (add (1) etc. if collision)
    5. Update FRecordingPath and FInfo
    6. Reset StartTime and FileSize for new segment
    7. Fire OnFileSplit event for UI notification

  Notes:
    - Does not stop/restart ffmpeg process (handled elsewhere)
    - Caller must redirect ffmpeg output to new file
    - FInfo.StartTime reset enables accurate per-segment timing
  ═══════════════════════════════════════════════════════════════════════════ }
function TStreamRecorder.SplitRecording: string;
var
  OldFile, NewFile, BaseName, Ext, Dir: string;
begin
  Result := '';

  if FInfo.State <> rsRecording then Exit;

  OldFile := FRecordingPath;

  { Generate new filename with index }
  Inc(FFileIndex);
  Dir := ExtractFilePath(OldFile);
  Ext := ExtractFileExt(OldFile);
  BaseName := ChangeFileExt(ExtractFileName(OldFile), '');

  { Remove old index if present }
  if (Length(BaseName) > 4) and (BaseName[Length(BaseName) - 3] = '_') and
     (BaseName[Length(BaseName) - 2] = 'p') then
  begin
    BaseName := Copy(BaseName, 1, Length(BaseName) - 4);
  end;

  NewFile := Dir + BaseName + Format('_p%02d', [FFileIndex]) + Ext;
  NewFile := EnsureUniqueFileName(NewFile);

  { Update recording path }
  FRecordingPath := NewFile;
  FInfo.FileName := NewFile;
  FInfo.StartTime := Now;
  FInfo.FileSize := 0;

  Result := NewFile;

  { Fire split event }
  if Assigned(FOnFileSplit) then
    FOnFileSplit(Self, OldFile, NewFile);
end;

end.
