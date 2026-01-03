{ ═══════════════════════════════════════════════════════════════════════════════
  uTestMPVEngine.pas - Unit Tests for uMPVEngine

  Part of 3nity Media - Test Suite

  Tests MPV engine components including TMPVStatus enum, record types,
  TMPVTrackList class, and TMPVEngine default values.

  Note: These tests do not require libmpv to be loaded. They test the
  data structures and initialization logic that work without mpv.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uTestMPVEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  uMPVEngine, uMPVConst;

type
  { ═══════════════════════════════════════════════════════════════════════════
    TTestMPVStatus - Tests for TMPVStatus Enumeration
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestMPVStatus = class(TTestCase)
  published
    procedure Test_TMPVStatus_msNone;
    procedure Test_TMPVStatus_msOpening;
    procedure Test_TMPVStatus_msClosing;
    procedure Test_TMPVStatus_msPlayStarting;
    procedure Test_TMPVStatus_msPlaying;
    procedure Test_TMPVStatus_msPaused;
    procedure Test_TMPVStatus_msStopped;
    procedure Test_TMPVStatus_msError;
    procedure Test_TMPVStatus_msErrorRetry;
    procedure Test_TMPVStatus_Count;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    TTestMPVTrackList - Tests for TMPVTrackList Class
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestMPVTrackList = class(TTestCase)
  private
    FTrackList: TMPVTrackList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Constructor/Destructor }
    procedure Test_Create_NotNil;
    procedure Test_Create_TrackType;
    procedure Test_Create_CountZero;
    procedure Test_Create_SelectedIDNegative;

    { Clear }
    procedure Test_Clear_EmptiesList;
    procedure Test_Clear_ResetsSelectedID;

    { Add }
    procedure Test_Add_IncreasesCount;
    procedure Test_Add_ReturnsIndex;
    procedure Test_Add_MultipleTracks;
    procedure Test_Add_StoresTrackData;

    { FindByID }
    procedure Test_FindByID_Existing;
    procedure Test_FindByID_NotExisting;
    procedure Test_FindByID_MultipleItems;

    { GetSelectedIndex }
    procedure Test_GetSelectedIndex_NoSelection;
    procedure Test_GetSelectedIndex_WithSelection;
    procedure Test_GetSelectedIndex_InvalidSelection;

    { GetItem }
    procedure Test_GetItem_ValidIndex;
    procedure Test_GetItem_InvalidIndex_Negative;
    procedure Test_GetItem_InvalidIndex_TooLarge;

    { Properties }
    procedure Test_SelectedID_ReadWrite;
    procedure Test_TrackType_Audio;
    procedure Test_TrackType_Sub;
    procedure Test_TrackType_Video;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    TTestMPVTrackInfo - Tests for TMPVTrackInfo Record
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestMPVTrackInfo = class(TTestCase)
  published
    procedure Test_TMPVTrackInfo_DefaultValues;
    procedure Test_TMPVTrackInfo_SetValues;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    TTestMPVVideoInfo - Tests for TMPVVideoInfo Record
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestMPVVideoInfo = class(TTestCase)
  published
    procedure Test_TMPVVideoInfo_DefaultValues;
    procedure Test_TMPVVideoInfo_SetValues;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    TTestMPVAudioInfo - Tests for TMPVAudioInfo Record
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestMPVAudioInfo = class(TTestCase)
  published
    procedure Test_TMPVAudioInfo_DefaultValues;
    procedure Test_TMPVAudioInfo_SetValues;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    TTestMPVStreamInfo - Tests for TMPVStreamInfo Record
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestMPVStreamInfo = class(TTestCase)
  published
    procedure Test_TMPVStreamInfo_DefaultValues;
    procedure Test_TMPVStreamInfo_SetValues;
    procedure Test_TMPVStreamInfo_MetadataArray;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    TTestMPVRenderInfo - Tests for TMPVRenderInfo Record
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestMPVRenderInfo = class(TTestCase)
  published
    procedure Test_TMPVRenderInfo_DefaultValues;
    procedure Test_TMPVRenderInfo_SetValues;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    TTestMPVPropertyValues - Tests for TMPVPropertyValues Record
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestMPVPropertyValues = class(TTestCase)
  published
    procedure Test_TMPVPropertyValues_DefaultValues;
    procedure Test_TMPVPropertyValues_SetValues;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    TTestMPVEngine - Tests for TMPVEngine Class (without libmpv)
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestMPVEngine = class(TTestCase)
  private
    FEngine: TMPVEngine;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Constructor }
    procedure Test_Create_NotNil;
    procedure Test_Create_HandleNil;
    procedure Test_Create_NotInitialized;
    procedure Test_Create_StatusNone;

    { Track Lists }
    procedure Test_Create_AudioTracksNotNil;
    procedure Test_Create_SubtitleTracksNotNil;
    procedure Test_Create_VideoTracksNotNil;
    procedure Test_Create_AudioTracksEmpty;
    procedure Test_Create_SubtitleTracksEmpty;
    procedure Test_Create_VideoTracksEmpty;

    { Default Playback Values }
    procedure Test_Default_Volume;
    procedure Test_Default_Muted;
    procedure Test_Default_Speed;
    procedure Test_Default_Position;
    procedure Test_Default_Duration;
    procedure Test_Default_PercentPos;

    { Default Video Properties }
    procedure Test_Default_Brightness;
    procedure Test_Default_Contrast;
    procedure Test_Default_Saturation;
    procedure Test_Default_Hue;
    procedure Test_Default_Gamma;

    { Default Subtitle Settings }
    procedure Test_Default_SubScale;
    procedure Test_Default_SubDelay;
    procedure Test_Default_SubVisible;

    { Default Audio Settings }
    procedure Test_Default_AudioDelay;

    { Default Equalizer }
    procedure Test_Default_EqualizerEnabled;

    { Default Aspect }
    procedure Test_Default_AspectMode;

    { Default Deinterlace }
    procedure Test_Default_Deinterlace;
    procedure Test_Default_DeinterlaceAlg;

    { Default HWAccel }
    procedure Test_Default_HWAccel;

    { Default Video/Audio Output }
    procedure Test_Default_VideoOutput;
    procedure Test_Default_AudioOutput;

    { Default DVD/Bluray }
    procedure Test_Default_TitleCount;
    procedure Test_Default_ChapterCount;
    procedure Test_Default_CurrentTitle;
    procedure Test_Default_CurrentChapter;

    { Default Media }
    procedure Test_Default_MediaFile;
    procedure Test_Default_FileLoaded;
    procedure Test_Default_LastError;

    { IsRunning }
    procedure Test_IsRunning_NotInitialized;

    { StreamInfo defaults }
    procedure Test_Default_StreamInfo_NotValid;
    procedure Test_Default_StreamInfo_DurationZero;
    procedure Test_Default_StreamInfo_DurationString;

    { Cache defaults }
    procedure Test_Default_CacheSize_Default;
    procedure Test_Default_CacheSize_Fixed;
    procedure Test_Default_CacheSize_Ramdisk;
    procedure Test_Default_CacheSize_CDROM;
    procedure Test_Default_CacheSize_Removable;
    procedure Test_Default_CacheSize_Network;
    procedure Test_Default_CacheSize_Internet;
    procedure Test_Default_CacheSize_DVD;

    { AspectPresetValue }
    procedure Test_AspectPresetValue_Auto;
    procedure Test_AspectPresetValue_16_9;
    procedure Test_AspectPresetValue_4_3;
    procedure Test_AspectPresetValue_2_35_1;
    procedure Test_AspectPresetValue_1_85_1;

    { DeinterlaceCmd }
    procedure Test_DeinterlaceCmd_Off;
    procedure Test_DeinterlaceCmd_On;
    procedure Test_DeinterlaceCmd_Auto_DVD;
    procedure Test_DeinterlaceCmd_Auto_Interlaced;
    procedure Test_DeinterlaceCmd_Auto_Neither;

    { Equalizer functions }
    procedure Test_GetEqualizerBand_Valid;
    procedure Test_GetEqualizerBand_Invalid;
    procedure Test_SetEqualizerBand_Valid;
    procedure Test_SetEqualizerBand_Clamped;
    procedure Test_GetEqualizerPreset_AllZero;
    procedure Test_SetEqualizerPreset_Valid;
    procedure Test_ResetEqualizer;
  end;

implementation

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestMPVStatus
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVStatus.Test_TMPVStatus_msNone;
begin
  AssertEquals('msNone = 0', 0, Ord(msNone));
end;

procedure TTestMPVStatus.Test_TMPVStatus_msOpening;
begin
  AssertEquals('msOpening = 1', 1, Ord(msOpening));
end;

procedure TTestMPVStatus.Test_TMPVStatus_msClosing;
begin
  AssertEquals('msClosing = 2', 2, Ord(msClosing));
end;

procedure TTestMPVStatus.Test_TMPVStatus_msPlayStarting;
begin
  AssertEquals('msPlayStarting = 3', 3, Ord(msPlayStarting));
end;

procedure TTestMPVStatus.Test_TMPVStatus_msPlaying;
begin
  AssertEquals('msPlaying = 4', 4, Ord(msPlaying));
end;

procedure TTestMPVStatus.Test_TMPVStatus_msPaused;
begin
  AssertEquals('msPaused = 5', 5, Ord(msPaused));
end;

procedure TTestMPVStatus.Test_TMPVStatus_msStopped;
begin
  AssertEquals('msStopped = 6', 6, Ord(msStopped));
end;

procedure TTestMPVStatus.Test_TMPVStatus_msError;
begin
  AssertEquals('msError = 7', 7, Ord(msError));
end;

procedure TTestMPVStatus.Test_TMPVStatus_msErrorRetry;
begin
  AssertEquals('msErrorRetry = 8', 8, Ord(msErrorRetry));
end;

procedure TTestMPVStatus.Test_TMPVStatus_Count;
begin
  AssertEquals('TMPVStatus has 9 values', 9, Ord(High(TMPVStatus)) - Ord(Low(TMPVStatus)) + 1);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestMPVTrackList
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVTrackList.SetUp;
begin
  FTrackList := TMPVTrackList.Create('audio');
end;

procedure TTestMPVTrackList.TearDown;
begin
  FTrackList.Free;
end;

procedure TTestMPVTrackList.Test_Create_NotNil;
begin
  AssertNotNull('TrackList should not be nil', FTrackList);
end;

procedure TTestMPVTrackList.Test_Create_TrackType;
begin
  AssertEquals('TrackType', 'audio', FTrackList.TrackType);
end;

procedure TTestMPVTrackList.Test_Create_CountZero;
begin
  AssertEquals('Count should be 0', 0, FTrackList.Count);
end;

procedure TTestMPVTrackList.Test_Create_SelectedIDNegative;
begin
  AssertEquals('SelectedID should be -1', -1, FTrackList.SelectedID);
end;

procedure TTestMPVTrackList.Test_Clear_EmptiesList;
var
  Track: TMPVTrackInfo;
begin
  Track.ID := 1;
  Track.Title := 'Test';
  FTrackList.Add(Track);
  FTrackList.Add(Track);
  AssertEquals('Count before clear', 2, FTrackList.Count);
  FTrackList.Clear;
  AssertEquals('Count after clear', 0, FTrackList.Count);
end;

procedure TTestMPVTrackList.Test_Clear_ResetsSelectedID;
begin
  FTrackList.SelectedID := 5;
  FTrackList.Clear;
  AssertEquals('SelectedID after clear', -1, FTrackList.SelectedID);
end;

procedure TTestMPVTrackList.Test_Add_IncreasesCount;
var
  Track: TMPVTrackInfo;
begin
  FillChar(Track, SizeOf(Track), 0);
  Track.ID := 1;
  FTrackList.Add(Track);
  AssertEquals('Count after add', 1, FTrackList.Count);
end;

procedure TTestMPVTrackList.Test_Add_ReturnsIndex;
var
  Track: TMPVTrackInfo;
  Index: Integer;
begin
  FillChar(Track, SizeOf(Track), 0);
  Track.ID := 1;
  Index := FTrackList.Add(Track);
  AssertEquals('First add returns 0', 0, Index);
  Track.ID := 2;
  Index := FTrackList.Add(Track);
  AssertEquals('Second add returns 1', 1, Index);
end;

procedure TTestMPVTrackList.Test_Add_MultipleTracks;
var
  Track: TMPVTrackInfo;
  I: Integer;
begin
  for I := 1 to 5 do
  begin
    FillChar(Track, SizeOf(Track), 0);
    Track.ID := I;
    FTrackList.Add(Track);
  end;
  AssertEquals('Count after 5 adds', 5, FTrackList.Count);
end;

procedure TTestMPVTrackList.Test_Add_StoresTrackData;
var
  Track: TMPVTrackInfo;
  Retrieved: TMPVTrackInfo;
begin
  FillChar(Track, SizeOf(Track), 0);
  Track.ID := 42;
  Track.Title := 'English';
  Track.Language := 'en';
  Track.Codec := 'aac';
  Track.IsDefault := True;
  FTrackList.Add(Track);
  Retrieved := FTrackList[0];
  AssertEquals('ID', 42, Retrieved.ID);
  AssertEquals('Title', 'English', Retrieved.Title);
  AssertEquals('Language', 'en', Retrieved.Language);
  AssertEquals('Codec', 'aac', Retrieved.Codec);
  AssertTrue('IsDefault', Retrieved.IsDefault);
end;

procedure TTestMPVTrackList.Test_FindByID_Existing;
var
  Track: TMPVTrackInfo;
begin
  FillChar(Track, SizeOf(Track), 0);
  Track.ID := 10;
  FTrackList.Add(Track);
  Track.ID := 20;
  FTrackList.Add(Track);
  Track.ID := 30;
  FTrackList.Add(Track);
  AssertEquals('Find ID 20', 1, FTrackList.FindByID(20));
end;

procedure TTestMPVTrackList.Test_FindByID_NotExisting;
var
  Track: TMPVTrackInfo;
begin
  FillChar(Track, SizeOf(Track), 0);
  Track.ID := 10;
  FTrackList.Add(Track);
  AssertEquals('Find non-existing ID', -1, FTrackList.FindByID(99));
end;

procedure TTestMPVTrackList.Test_FindByID_MultipleItems;
var
  Track: TMPVTrackInfo;
begin
  FillChar(Track, SizeOf(Track), 0);
  Track.ID := 1;
  FTrackList.Add(Track);
  Track.ID := 2;
  FTrackList.Add(Track);
  Track.ID := 3;
  FTrackList.Add(Track);
  AssertEquals('Find ID 1', 0, FTrackList.FindByID(1));
  AssertEquals('Find ID 2', 1, FTrackList.FindByID(2));
  AssertEquals('Find ID 3', 2, FTrackList.FindByID(3));
end;

procedure TTestMPVTrackList.Test_GetSelectedIndex_NoSelection;
begin
  AssertEquals('No selection', -1, FTrackList.GetSelectedIndex);
end;

procedure TTestMPVTrackList.Test_GetSelectedIndex_WithSelection;
var
  Track: TMPVTrackInfo;
begin
  FillChar(Track, SizeOf(Track), 0);
  Track.ID := 5;
  FTrackList.Add(Track);
  Track.ID := 10;
  FTrackList.Add(Track);
  FTrackList.SelectedID := 10;
  AssertEquals('Selected index', 1, FTrackList.GetSelectedIndex);
end;

procedure TTestMPVTrackList.Test_GetSelectedIndex_InvalidSelection;
var
  Track: TMPVTrackInfo;
begin
  FillChar(Track, SizeOf(Track), 0);
  Track.ID := 5;
  FTrackList.Add(Track);
  FTrackList.SelectedID := 99;
  AssertEquals('Invalid selection', -1, FTrackList.GetSelectedIndex);
end;

procedure TTestMPVTrackList.Test_GetItem_ValidIndex;
var
  Track: TMPVTrackInfo;
begin
  FillChar(Track, SizeOf(Track), 0);
  Track.ID := 42;
  Track.Title := 'Test Track';
  FTrackList.Add(Track);
  AssertEquals('Item ID', 42, FTrackList[0].ID);
  AssertEquals('Item Title', 'Test Track', FTrackList[0].Title);
end;

procedure TTestMPVTrackList.Test_GetItem_InvalidIndex_Negative;
var
  Track: TMPVTrackInfo;
begin
  Track := FTrackList[-1];
  AssertEquals('Invalid index returns ID 0', 0, Track.ID);
end;

procedure TTestMPVTrackList.Test_GetItem_InvalidIndex_TooLarge;
var
  Track: TMPVTrackInfo;
begin
  Track := FTrackList[100];
  AssertEquals('Invalid index returns ID 0', 0, Track.ID);
end;

procedure TTestMPVTrackList.Test_SelectedID_ReadWrite;
begin
  FTrackList.SelectedID := 42;
  AssertEquals('SelectedID', 42, FTrackList.SelectedID);
end;

procedure TTestMPVTrackList.Test_TrackType_Audio;
var
  List: TMPVTrackList;
begin
  List := TMPVTrackList.Create('audio');
  try
    AssertEquals('audio', List.TrackType);
  finally
    List.Free;
  end;
end;

procedure TTestMPVTrackList.Test_TrackType_Sub;
var
  List: TMPVTrackList;
begin
  List := TMPVTrackList.Create('sub');
  try
    AssertEquals('sub', List.TrackType);
  finally
    List.Free;
  end;
end;

procedure TTestMPVTrackList.Test_TrackType_Video;
var
  List: TMPVTrackList;
begin
  List := TMPVTrackList.Create('video');
  try
    AssertEquals('video', List.TrackType);
  finally
    List.Free;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestMPVTrackInfo
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVTrackInfo.Test_TMPVTrackInfo_DefaultValues;
var
  Info: TMPVTrackInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  AssertEquals('ID default', 0, Info.ID);
  AssertEquals('TrackType default', '', Info.TrackType);
  AssertEquals('Title default', '', Info.Title);
  AssertEquals('Language default', '', Info.Language);
  AssertFalse('IsDefault default', Info.IsDefault);
  AssertFalse('IsForced default', Info.IsForced);
  AssertFalse('IsExternal default', Info.IsExternal);
end;

procedure TTestMPVTrackInfo.Test_TMPVTrackInfo_SetValues;
var
  Info: TMPVTrackInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.ID := 5;
  Info.TrackType := 'audio';
  Info.Title := 'English Stereo';
  Info.Language := 'en';
  Info.Codec := 'aac';
  Info.IsDefault := True;
  Info.Channels := 2;
  Info.SampleRate := 48000;
  AssertEquals('ID', 5, Info.ID);
  AssertEquals('TrackType', 'audio', Info.TrackType);
  AssertEquals('Title', 'English Stereo', Info.Title);
  AssertEquals('Language', 'en', Info.Language);
  AssertEquals('Codec', 'aac', Info.Codec);
  AssertTrue('IsDefault', Info.IsDefault);
  AssertEquals('Channels', 2, Info.Channels);
  AssertEquals('SampleRate', 48000, Info.SampleRate);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestMPVVideoInfo
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVVideoInfo.Test_TMPVVideoInfo_DefaultValues;
var
  Info: TMPVVideoInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  AssertEquals('Codec default', '', Info.Codec);
  AssertEquals('Width default', 0, Info.Width);
  AssertEquals('Height default', 0, Info.Height);
  AssertEquals('Bitrate default', 0, Info.Bitrate);
end;

procedure TTestMPVVideoInfo.Test_TMPVVideoInfo_SetValues;
var
  Info: TMPVVideoInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.Codec := 'h264';
  Info.Width := 1920;
  Info.Height := 1080;
  Info.FPS := 23.976;
  Info.Bitrate := 5000000;
  Info.Aspect := 1.778;
  AssertEquals('Codec', 'h264', Info.Codec);
  AssertEquals('Width', 1920, Info.Width);
  AssertEquals('Height', 1080, Info.Height);
  AssertTrue('FPS', Abs(Info.FPS - 23.976) < 0.001);
  AssertEquals('Bitrate', 5000000, Info.Bitrate);
  AssertTrue('Aspect', Abs(Info.Aspect - 1.778) < 0.001);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestMPVAudioInfo
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVAudioInfo.Test_TMPVAudioInfo_DefaultValues;
var
  Info: TMPVAudioInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  AssertEquals('Codec default', '', Info.Codec);
  AssertEquals('Channels default', 0, Info.Channels);
  AssertEquals('SampleRate default', 0, Info.SampleRate);
  AssertEquals('Bitrate default', 0, Info.Bitrate);
end;

procedure TTestMPVAudioInfo.Test_TMPVAudioInfo_SetValues;
var
  Info: TMPVAudioInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.Codec := 'aac';
  Info.Channels := 6;
  Info.ChannelLayout := '5.1';
  Info.SampleRate := 48000;
  Info.Bitrate := 384000;
  AssertEquals('Codec', 'aac', Info.Codec);
  AssertEquals('Channels', 6, Info.Channels);
  AssertEquals('ChannelLayout', '5.1', Info.ChannelLayout);
  AssertEquals('SampleRate', 48000, Info.SampleRate);
  AssertEquals('Bitrate', 384000, Info.Bitrate);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestMPVStreamInfo
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVStreamInfo.Test_TMPVStreamInfo_DefaultValues;
var
  Info: TMPVStreamInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  AssertFalse('Valid default', Info.Valid);
  AssertEquals('FileName default', '', Info.FileName);
  AssertEquals('FileSize default', 0, Info.FileSize);
  AssertFalse('HasAudio default', Info.HasAudio);
  AssertFalse('HasVideo default', Info.HasVideo);
  AssertFalse('IsDVD default', Info.IsDVD);
  AssertFalse('IsStream default', Info.IsStream);
end;

procedure TTestMPVStreamInfo.Test_TMPVStreamInfo_SetValues;
var
  Info: TMPVStreamInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.Valid := True;
  Info.FileName := '/path/to/video.mkv';
  Info.FileFormat := 'matroska';
  Info.FileSize := 1073741824;
  Info.DurationSec := 7200.5;
  Info.HasAudio := True;
  Info.HasVideo := True;
  Info.Seekable := True;
  AssertTrue('Valid', Info.Valid);
  AssertEquals('FileName', '/path/to/video.mkv', Info.FileName);
  AssertEquals('FileFormat', 'matroska', Info.FileFormat);
  AssertEquals('FileSize', 1073741824, Info.FileSize);
  AssertTrue('DurationSec', Abs(Info.DurationSec - 7200.5) < 0.01);
  AssertTrue('HasAudio', Info.HasAudio);
  AssertTrue('HasVideo', Info.HasVideo);
  AssertTrue('Seekable', Info.Seekable);
end;

procedure TTestMPVStreamInfo.Test_TMPVStreamInfo_MetadataArray;
var
  Info: TMPVStreamInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.Metadata[0].Key := 'title';
  Info.Metadata[0].Value := 'Test Movie';
  Info.Metadata[1].Key := 'artist';
  Info.Metadata[1].Value := 'Test Artist';
  Info.MetadataCount := 2;
  AssertEquals('Metadata[0].Key', 'title', Info.Metadata[0].Key);
  AssertEquals('Metadata[0].Value', 'Test Movie', Info.Metadata[0].Value);
  AssertEquals('Metadata[1].Key', 'artist', Info.Metadata[1].Key);
  AssertEquals('MetadataCount', 2, Info.MetadataCount);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestMPVRenderInfo
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVRenderInfo.Test_TMPVRenderInfo_DefaultValues;
var
  Info: TMPVRenderInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  AssertEquals('Width default', 0, Info.Width);
  AssertEquals('Height default', 0, Info.Height);
  AssertEquals('TitleID default', 0, Info.TitleID);
  AssertEquals('ChapterID default', 0, Info.ChapterID);
  AssertFalse('VideoError default', Info.VideoError);
  AssertFalse('IsDVDMenu default', Info.IsDVDMenu);
  AssertFalse('Fullscreen default', Info.Fullscreen);
end;

procedure TTestMPVRenderInfo.Test_TMPVRenderInfo_SetValues;
var
  Info: TMPVRenderInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.Width := 1920;
  Info.Height := 1080;
  Info.DisplayWidth := 1920;
  Info.DisplayHeight := 1080;
  Info.Aspect := 1.778;
  Info.TitleID := 1;
  Info.ChapterID := 5;
  Info.Fullscreen := True;
  AssertEquals('Width', 1920, Info.Width);
  AssertEquals('Height', 1080, Info.Height);
  AssertTrue('Aspect', Abs(Info.Aspect - 1.778) < 0.001);
  AssertEquals('TitleID', 1, Info.TitleID);
  AssertEquals('ChapterID', 5, Info.ChapterID);
  AssertTrue('Fullscreen', Info.Fullscreen);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestMPVPropertyValues
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVPropertyValues.Test_TMPVPropertyValues_DefaultValues;
var
  Values: TMPVPropertyValues;
begin
  FillChar(Values, SizeOf(Values), 0);
  AssertEquals('Default default', 0, Values.Default);
  AssertFalse('Valid default', Values.Valid);
  AssertEquals('Value default', 0, Values.Value);
end;

procedure TTestMPVPropertyValues.Test_TMPVPropertyValues_SetValues;
var
  Values: TMPVPropertyValues;
begin
  Values.Default := 50;
  Values.Valid := True;
  Values.Value := 75;
  AssertEquals('Default', 50, Values.Default);
  AssertTrue('Valid', Values.Valid);
  AssertEquals('Value', 75, Values.Value);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestMPVEngine
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVEngine.SetUp;
begin
  FEngine := TMPVEngine.Create;
end;

procedure TTestMPVEngine.TearDown;
begin
  FEngine.Free;
end;

{ Constructor Tests }

procedure TTestMPVEngine.Test_Create_NotNil;
begin
  AssertNotNull('Engine should not be nil', FEngine);
end;

procedure TTestMPVEngine.Test_Create_HandleNil;
begin
  AssertTrue('Handle should be nil before Initialize', FEngine.Handle = nil);
end;

procedure TTestMPVEngine.Test_Create_NotInitialized;
begin
  AssertFalse('Not initialized after Create', FEngine.Initialized);
end;

procedure TTestMPVEngine.Test_Create_StatusNone;
begin
  AssertEquals('Status should be msNone', Ord(msNone), Ord(FEngine.Status));
end;

{ Track Lists Tests }

procedure TTestMPVEngine.Test_Create_AudioTracksNotNil;
begin
  AssertNotNull('AudioTracks not nil', FEngine.AudioTracks);
end;

procedure TTestMPVEngine.Test_Create_SubtitleTracksNotNil;
begin
  AssertNotNull('SubtitleTracks not nil', FEngine.SubtitleTracks);
end;

procedure TTestMPVEngine.Test_Create_VideoTracksNotNil;
begin
  AssertNotNull('VideoTracks not nil', FEngine.VideoTracks);
end;

procedure TTestMPVEngine.Test_Create_AudioTracksEmpty;
begin
  AssertEquals('AudioTracks empty', 0, FEngine.AudioTracks.Count);
end;

procedure TTestMPVEngine.Test_Create_SubtitleTracksEmpty;
begin
  AssertEquals('SubtitleTracks empty', 0, FEngine.SubtitleTracks.Count);
end;

procedure TTestMPVEngine.Test_Create_VideoTracksEmpty;
begin
  AssertEquals('VideoTracks empty', 0, FEngine.VideoTracks.Count);
end;

{ Default Playback Values Tests }

procedure TTestMPVEngine.Test_Default_Volume;
begin
  AssertEquals('Default Volume', VOL_DEFAULT, FEngine.Volume);
end;

procedure TTestMPVEngine.Test_Default_Muted;
begin
  AssertFalse('Default Muted', FEngine.Muted);
end;

procedure TTestMPVEngine.Test_Default_Speed;
begin
  AssertEquals('Default Speed', SPEED_DEFAULT, FEngine.Speed);
end;

procedure TTestMPVEngine.Test_Default_Position;
begin
  AssertEquals('Default Position', 0.0, FEngine.Position);
end;

procedure TTestMPVEngine.Test_Default_Duration;
begin
  AssertEquals('Default Duration', 0.0, FEngine.Duration);
end;

procedure TTestMPVEngine.Test_Default_PercentPos;
begin
  AssertEquals('Default PercentPos', 0.0, FEngine.PercentPos);
end;

{ Default Video Properties Tests }

procedure TTestMPVEngine.Test_Default_Brightness;
begin
  AssertEquals('Default Brightness', VIDEO_PROP_DEFAULT, FEngine.Brightness);
end;

procedure TTestMPVEngine.Test_Default_Contrast;
begin
  AssertEquals('Default Contrast', VIDEO_PROP_DEFAULT, FEngine.Contrast);
end;

procedure TTestMPVEngine.Test_Default_Saturation;
begin
  AssertEquals('Default Saturation', VIDEO_PROP_DEFAULT, FEngine.Saturation);
end;

procedure TTestMPVEngine.Test_Default_Hue;
begin
  AssertEquals('Default Hue', VIDEO_PROP_DEFAULT, FEngine.Hue);
end;

procedure TTestMPVEngine.Test_Default_Gamma;
begin
  AssertEquals('Default Gamma', VIDEO_PROP_DEFAULT, FEngine.Gamma);
end;

{ Default Subtitle Settings Tests }

procedure TTestMPVEngine.Test_Default_SubScale;
begin
  AssertEquals('Default SubScale', SUB_SCALE_DEFAULT, FEngine.SubScale);
end;

procedure TTestMPVEngine.Test_Default_SubDelay;
begin
  AssertEquals('Default SubDelay', SUB_DELAY_DEFAULT, FEngine.SubDelay);
end;

procedure TTestMPVEngine.Test_Default_SubVisible;
begin
  AssertTrue('Default SubVisible', FEngine.SubVisible);
end;

{ Default Audio Settings Tests }

procedure TTestMPVEngine.Test_Default_AudioDelay;
begin
  AssertEquals('Default AudioDelay', AUDIO_DELAY_DEFAULT, FEngine.AudioDelay);
end;

{ Default Equalizer Tests }

procedure TTestMPVEngine.Test_Default_EqualizerEnabled;
begin
  AssertFalse('Default EqualizerEnabled', FEngine.EqualizerEnabled);
end;

{ Default Aspect Tests }

procedure TTestMPVEngine.Test_Default_AspectMode;
begin
  AssertEquals('Default AspectMode', ASPECT_AUTO, FEngine.AspectMode);
end;

{ Default Deinterlace Tests }

procedure TTestMPVEngine.Test_Default_Deinterlace;
begin
  AssertEquals('Default Deinterlace', DEINT_AUTO, FEngine.Deinterlace);
end;

procedure TTestMPVEngine.Test_Default_DeinterlaceAlg;
begin
  AssertEquals('Default DeinterlaceAlg', DEINT_ALG_AUTO, FEngine.DeinterlaceAlg);
end;

{ Default HWAccel Tests }

procedure TTestMPVEngine.Test_Default_HWAccel;
begin
  AssertTrue('Default HWAccel', FEngine.HWAccel);
end;

{ Default Video/Audio Output Tests }

procedure TTestMPVEngine.Test_Default_VideoOutput;
begin
  AssertEquals('Default VideoOutput', VO_DEFAULT, FEngine.VideoOutput);
end;

procedure TTestMPVEngine.Test_Default_AudioOutput;
begin
  AssertEquals('Default AudioOutput', AO_DEFAULT, FEngine.AudioOutput);
end;

{ Default DVD/Bluray Tests }

procedure TTestMPVEngine.Test_Default_TitleCount;
begin
  AssertEquals('Default TitleCount', 0, FEngine.TitleCount);
end;

procedure TTestMPVEngine.Test_Default_ChapterCount;
begin
  AssertEquals('Default ChapterCount', 0, FEngine.ChapterCount);
end;

procedure TTestMPVEngine.Test_Default_CurrentTitle;
begin
  AssertEquals('Default CurrentTitle', 0, FEngine.CurrentTitle);
end;

procedure TTestMPVEngine.Test_Default_CurrentChapter;
begin
  AssertEquals('Default CurrentChapter', 0, FEngine.CurrentChapter);
end;

{ Default Media Tests }

procedure TTestMPVEngine.Test_Default_MediaFile;
begin
  AssertEquals('Default MediaFile', '', FEngine.MediaFile);
end;

procedure TTestMPVEngine.Test_Default_FileLoaded;
begin
  AssertEquals('Default FileLoaded', '', FEngine.FileLoaded);
end;

procedure TTestMPVEngine.Test_Default_LastError;
begin
  AssertEquals('Default LastError', '', FEngine.LastError);
end;

{ IsRunning Tests }

procedure TTestMPVEngine.Test_IsRunning_NotInitialized;
begin
  AssertFalse('IsRunning when not initialized', FEngine.Running);
end;

{ StreamInfo Default Tests }

procedure TTestMPVEngine.Test_Default_StreamInfo_NotValid;
begin
  AssertFalse('StreamInfo not valid', FEngine.StreamInfo.Valid);
end;

procedure TTestMPVEngine.Test_Default_StreamInfo_DurationZero;
begin
  AssertEquals('StreamInfo DurationSec', 0.0, FEngine.StreamInfo.DurationSec);
end;

procedure TTestMPVEngine.Test_Default_StreamInfo_DurationString;
begin
  AssertEquals('StreamInfo DurationString', '00:00:00', FEngine.StreamInfo.DurationString);
end;

{ Cache Default Tests }

procedure TTestMPVEngine.Test_Default_CacheSize_Default;
begin
  AssertEquals('CacheSize Default', DEFAULT_CACHE_DEFAULT, FEngine.CacheSize[CACHE_TYPE_DEFAULT]);
end;

procedure TTestMPVEngine.Test_Default_CacheSize_Fixed;
begin
  AssertEquals('CacheSize Fixed', DEFAULT_CACHE_FIXED, FEngine.CacheSize[CACHE_TYPE_FIXED]);
end;

procedure TTestMPVEngine.Test_Default_CacheSize_Ramdisk;
begin
  AssertEquals('CacheSize Ramdisk', DEFAULT_CACHE_RAMDISK, FEngine.CacheSize[CACHE_TYPE_RAMDISK]);
end;

procedure TTestMPVEngine.Test_Default_CacheSize_CDROM;
begin
  AssertEquals('CacheSize CDROM', DEFAULT_CACHE_CDROM, FEngine.CacheSize[CACHE_TYPE_CDROM]);
end;

procedure TTestMPVEngine.Test_Default_CacheSize_Removable;
begin
  AssertEquals('CacheSize Removable', DEFAULT_CACHE_REMOVABLE, FEngine.CacheSize[CACHE_TYPE_REMOVABLE]);
end;

procedure TTestMPVEngine.Test_Default_CacheSize_Network;
begin
  AssertEquals('CacheSize Network', DEFAULT_CACHE_NETWORK, FEngine.CacheSize[CACHE_TYPE_NETWORK]);
end;

procedure TTestMPVEngine.Test_Default_CacheSize_Internet;
begin
  AssertEquals('CacheSize Internet', DEFAULT_CACHE_INTERNET, FEngine.CacheSize[CACHE_TYPE_INTERNET]);
end;

procedure TTestMPVEngine.Test_Default_CacheSize_DVD;
begin
  AssertEquals('CacheSize DVD', DEFAULT_CACHE_DVD, FEngine.CacheSize[CACHE_TYPE_DVD]);
end;

{ AspectPresetValue Tests }

procedure TTestMPVEngine.Test_AspectPresetValue_Auto;
begin
  { AspectMode is ASPECT_AUTO by default }
  AssertTrue('AspectPresetValue Auto < 0', FEngine.AspectPresetValue < 0);
end;

procedure TTestMPVEngine.Test_AspectPresetValue_16_9;
var
  Engine2: TMPVEngine;
begin
  Engine2 := TMPVEngine.Create;
  try
    Engine2.AspectMode := ASPECT_16_9;
    AssertTrue('AspectPresetValue 16:9', Abs(Engine2.AspectPresetValue - 1.7778) < 0.001);
  finally
    Engine2.Free;
  end;
end;

procedure TTestMPVEngine.Test_AspectPresetValue_4_3;
var
  Engine2: TMPVEngine;
begin
  Engine2 := TMPVEngine.Create;
  try
    Engine2.AspectMode := ASPECT_4_3;
    AssertTrue('AspectPresetValue 4:3', Abs(Engine2.AspectPresetValue - 1.3333) < 0.001);
  finally
    Engine2.Free;
  end;
end;

procedure TTestMPVEngine.Test_AspectPresetValue_2_35_1;
var
  Engine2: TMPVEngine;
begin
  Engine2 := TMPVEngine.Create;
  try
    Engine2.AspectMode := ASPECT_2_35_1;
    AssertTrue('AspectPresetValue 2.35:1', Abs(Engine2.AspectPresetValue - 2.35) < 0.01);
  finally
    Engine2.Free;
  end;
end;

procedure TTestMPVEngine.Test_AspectPresetValue_1_85_1;
var
  Engine2: TMPVEngine;
begin
  Engine2 := TMPVEngine.Create;
  try
    Engine2.AspectMode := ASPECT_1_85_1;
    AssertTrue('AspectPresetValue 1.85:1', Abs(Engine2.AspectPresetValue - 1.85) < 0.01);
  finally
    Engine2.Free;
  end;
end;

{ DeinterlaceCmd Tests }

procedure TTestMPVEngine.Test_DeinterlaceCmd_Off;
begin
  AssertEquals('DeinterlaceCmd Off', DEINT_OFF,
    FEngine.DeinterlaceCmd(DEINT_OFF, DEINT_ALG_AUTO, False, False, False));
end;

procedure TTestMPVEngine.Test_DeinterlaceCmd_On;
begin
  AssertEquals('DeinterlaceCmd On', DEINT_ON,
    FEngine.DeinterlaceCmd(DEINT_ON, DEINT_ALG_AUTO, False, False, False));
end;

procedure TTestMPVEngine.Test_DeinterlaceCmd_Auto_DVD;
begin
  AssertEquals('DeinterlaceCmd Auto + DVD', DEINT_ON,
    FEngine.DeinterlaceCmd(DEINT_AUTO, DEINT_ALG_AUTO, True, True, False));
end;

procedure TTestMPVEngine.Test_DeinterlaceCmd_Auto_Interlaced;
begin
  AssertEquals('DeinterlaceCmd Auto + Interlaced', DEINT_ON,
    FEngine.DeinterlaceCmd(DEINT_AUTO, DEINT_ALG_AUTO, False, False, True));
end;

procedure TTestMPVEngine.Test_DeinterlaceCmd_Auto_Neither;
begin
  AssertEquals('DeinterlaceCmd Auto + neither', DEINT_OFF,
    FEngine.DeinterlaceCmd(DEINT_AUTO, DEINT_ALG_AUTO, False, False, False));
end;

{ Equalizer Tests }

procedure TTestMPVEngine.Test_GetEqualizerBand_Valid;
begin
  { All bands default to 0 }
  AssertEquals('Band 0 default', 0.0, FEngine.GetEqualizerBand(0));
  AssertEquals('Band 5 default', 0.0, FEngine.GetEqualizerBand(5));
  AssertEquals('Band 9 default', 0.0, FEngine.GetEqualizerBand(9));
end;

procedure TTestMPVEngine.Test_GetEqualizerBand_Invalid;
begin
  AssertEquals('Invalid band -1', 0.0, FEngine.GetEqualizerBand(-1));
  AssertEquals('Invalid band 10', 0.0, FEngine.GetEqualizerBand(10));
  AssertEquals('Invalid band 100', 0.0, FEngine.GetEqualizerBand(100));
end;

procedure TTestMPVEngine.Test_SetEqualizerBand_Valid;
begin
  FEngine.SetEqualizerBand(0, 6.0);
  AssertEquals('Band 0 after set', 6.0, FEngine.GetEqualizerBand(0));
  FEngine.SetEqualizerBand(5, -3.5);
  AssertEquals('Band 5 after set', -3.5, FEngine.GetEqualizerBand(5));
end;

procedure TTestMPVEngine.Test_SetEqualizerBand_Clamped;
begin
  { Values should be clamped to EQ_MIN_DB..EQ_MAX_DB }
  FEngine.SetEqualizerBand(0, 20.0);
  AssertEquals('Band clamped to max', EQ_MAX_DB, FEngine.GetEqualizerBand(0));
  FEngine.SetEqualizerBand(1, -20.0);
  AssertEquals('Band clamped to min', EQ_MIN_DB, FEngine.GetEqualizerBand(1));
end;

procedure TTestMPVEngine.Test_GetEqualizerPreset_AllZero;
var
  Preset: string;
begin
  Preset := FEngine.GetEqualizerPreset;
  AssertTrue('Preset contains 0.0', Pos('0.0', Preset) > 0);
  { Should be something like '0.0:0.0:0.0:0.0:0.0:0.0:0.0:0.0:0.0:0.0' }
  AssertEquals('Preset has 9 colons', 9, Length(Preset) - Length(StringReplace(Preset, ':', '', [rfReplaceAll])));
end;

procedure TTestMPVEngine.Test_SetEqualizerPreset_Valid;
begin
  FEngine.SetEqualizerPreset('1.0:2.0:3.0:4.0:5.0:-1.0:-2.0:-3.0:-4.0:-5.0');
  AssertEquals('Band 0', 1.0, FEngine.GetEqualizerBand(0));
  AssertEquals('Band 4', 5.0, FEngine.GetEqualizerBand(4));
  AssertEquals('Band 5', -1.0, FEngine.GetEqualizerBand(5));
  AssertEquals('Band 9', -5.0, FEngine.GetEqualizerBand(9));
end;

procedure TTestMPVEngine.Test_ResetEqualizer;
begin
  FEngine.SetEqualizerBand(0, 6.0);
  FEngine.SetEqualizerBand(5, -3.0);
  FEngine.ResetEqualizer;
  AssertEquals('Band 0 after reset', 0.0, FEngine.GetEqualizerBand(0));
  AssertEquals('Band 5 after reset', 0.0, FEngine.GetEqualizerBand(5));
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Test Registration
  ═══════════════════════════════════════════════════════════════════════════════ }

initialization
  RegisterTest(TTestMPVStatus);
  RegisterTest(TTestMPVTrackList);
  RegisterTest(TTestMPVTrackInfo);
  RegisterTest(TTestMPVVideoInfo);
  RegisterTest(TTestMPVAudioInfo);
  RegisterTest(TTestMPVStreamInfo);
  RegisterTest(TTestMPVRenderInfo);
  RegisterTest(TTestMPVPropertyValues);
  RegisterTest(TTestMPVEngine);

end.
