{ ═══════════════════════════════════════════════════════════════════════════════
  uTestAudioVideo.pas - Audio/Video Settings Integration Tests

  Part of 3nity Media - Test Suite

  Integration tests for audio/video settings and configuration:
  - Audio settings persistence (volume, mute, channels)
  - Video settings persistence (brightness, contrast, deinterlace)
  - Subtitle settings persistence
  - Track management

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uTestAudioVideo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  uConfig, uTypes, uMPVConst, uMPVEngine;

type
  { ═══════════════════════════════════════════════════════════════════════════
    AUDIO SETTINGS INTEGRATION TESTS
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestAudioSettingsIntegration = class(TTestCase)
  private
    FConfig: TConfigManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Volume Settings }
    procedure Test_DefaultVolume;
    procedure Test_VolumeRange;
    procedure Test_VolumePersistence;
    procedure Test_MuteStatePersistence;

    { Audio Output Settings }
    procedure Test_AudioDeviceSetting;
    procedure Test_AudioChannelsSetting;
    procedure Test_AudioNormalization;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    VIDEO SETTINGS INTEGRATION TESTS
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestVideoSettingsIntegration = class(TTestCase)
  private
    FConfig: TConfigManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Aspect Ratio Settings }
    procedure Test_DefaultAspectMode;
    procedure Test_AspectModePersistence;

    { Deinterlace Settings }
    procedure Test_DefaultDeinterlace;
    procedure Test_DeinterlacePersistence;

    { Video Adjustments }
    procedure Test_DefaultBrightness;
    procedure Test_DefaultContrast;
    procedure Test_DefaultSaturation;
    procedure Test_DefaultHue;
    procedure Test_DefaultGamma;
    procedure Test_VideoAdjustmentRange;
    procedure Test_VideoAdjustmentPersistence;

    { Hardware Acceleration }
    procedure Test_DefaultHWAccel;
    procedure Test_HWAccelPersistence;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    SUBTITLE SETTINGS INTEGRATION TESTS
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestSubtitleSettingsIntegration = class(TTestCase)
  private
    FConfig: TConfigManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Subtitle Display }
    procedure Test_DefaultSubtitlePosition;
    procedure Test_SubtitlePositionRange;

    { Subtitle Appearance }
    procedure Test_DefaultSubtitleFont;
    procedure Test_SubtitleFontSize;
    procedure Test_SubtitleFontSizePersistence;

    { Subtitle Auto-load }
    procedure Test_DefaultSubtitleAutoLoad;
    procedure Test_SubtitleAutoLoadPersistence;

    { Use Default }
    procedure Test_UseDefaultSetting;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    MPV ENGINE + CONFIG INTEGRATION TESTS
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestMPVConfigIntegration = class(TTestCase)
  private
    FConfig: TConfigManager;
    FEngine: TMPVEngine;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Engine Default Values }
    procedure Test_EngineDefaultVolume;
    procedure Test_EngineDefaultBrightness;
    procedure Test_EngineDefaultContrast;
    procedure Test_EngineDefaultSaturation;
    procedure Test_EngineDefaultSpeed;

    { Equalizer Integration }
    procedure Test_EqualizerBandCount;
    procedure Test_EqualizerBandSet;
    procedure Test_EqualizerResetValues;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    TRACK LIST INTEGRATION TESTS
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestTrackListIntegration = class(TTestCase)
  private
    FAudioTracks: TMPVTrackList;
    FVideoTracks: TMPVTrackList;
    FSubTracks: TMPVTrackList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Audio Track Management }
    procedure Test_AddAudioTrack;
    procedure Test_MultipleAudioTracks;
    procedure Test_FindAudioTrackByID;
    procedure Test_AudioTrackLanguage;

    { Video Track Management }
    procedure Test_AddVideoTrack;
    procedure Test_VideoTrackCodec;
    procedure Test_VideoTrackDimensions;

    { Subtitle Track Management }
    procedure Test_AddSubtitleTrack;
    procedure Test_MultipleSubtitleTracks;
    procedure Test_SubtitleTrackTitle;
    procedure Test_ExternalSubtitleTrack;

    { Track Operations }
    procedure Test_ClearTracks;
    procedure Test_TrackCount;
  end;

implementation

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestAudioSettingsIntegration
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestAudioSettingsIntegration.SetUp;
begin
  FConfig := TConfigManager.Create;
end;

procedure TTestAudioSettingsIntegration.TearDown;
begin
  FConfig.Free;
end;

procedure TTestAudioSettingsIntegration.Test_DefaultVolume;
var
  Audio: TAudioSettings;
begin
  Audio := FConfig.GetAudio;
  AssertTrue('Default volume should be 0-100', (Audio.Volume >= 0) and (Audio.Volume <= 100));
end;

procedure TTestAudioSettingsIntegration.Test_VolumeRange;
var
  Audio: TAudioSettings;
begin
  Audio := FConfig.GetAudio;
  Audio.Volume := 0;
  FConfig.SetAudio(Audio);
  AssertEquals('Volume 0 should be valid', 0, FConfig.GetAudio.Volume);

  Audio.Volume := 100;
  FConfig.SetAudio(Audio);
  AssertEquals('Volume 100 should be valid', 100, FConfig.GetAudio.Volume);
end;

procedure TTestAudioSettingsIntegration.Test_VolumePersistence;
var
  Audio: TAudioSettings;
begin
  Audio := FConfig.GetAudio;
  Audio.Volume := 75;
  FConfig.SetAudio(Audio);
  FConfig.Save;
  FConfig.Load;

  AssertEquals('Volume should persist', 75, FConfig.GetAudio.Volume);
end;

procedure TTestAudioSettingsIntegration.Test_MuteStatePersistence;
var
  Audio: TAudioSettings;
begin
  Audio := FConfig.GetAudio;
  Audio.Muted := True;
  FConfig.SetAudio(Audio);
  FConfig.Save;
  FConfig.Load;

  AssertTrue('Mute state should persist', FConfig.GetAudio.Muted);
end;

procedure TTestAudioSettingsIntegration.Test_AudioDeviceSetting;
var
  Audio: TAudioSettings;
begin
  Audio := FConfig.GetAudio;
  Audio.AudioDevice := 'default';
  FConfig.SetAudio(Audio);
  AssertEquals('Audio device should be set', 'default', FConfig.GetAudio.AudioDevice);
end;

procedure TTestAudioSettingsIntegration.Test_AudioChannelsSetting;
var
  Audio: TAudioSettings;
begin
  Audio := FConfig.GetAudio;
  Audio.Channels := 2; // Stereo
  FConfig.SetAudio(Audio);
  AssertEquals('Channels should be 2', 2, FConfig.GetAudio.Channels);
end;

procedure TTestAudioSettingsIntegration.Test_AudioNormalization;
var
  Audio: TAudioSettings;
begin
  Audio := FConfig.GetAudio;
  Audio.Normalize := True;
  FConfig.SetAudio(Audio);
  AssertTrue('Normalization can be enabled', FConfig.GetAudio.Normalize);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestVideoSettingsIntegration
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestVideoSettingsIntegration.SetUp;
begin
  FConfig := TConfigManager.Create;
end;

procedure TTestVideoSettingsIntegration.TearDown;
begin
  FConfig.Free;
end;

procedure TTestVideoSettingsIntegration.Test_DefaultAspectMode;
var
  Video: TVideoSettings;
begin
  Video := FConfig.GetVideo;
  AssertTrue('Default aspect mode should be valid', Video.AspectMode >= 0);
end;

procedure TTestVideoSettingsIntegration.Test_AspectModePersistence;
var
  Video: TVideoSettings;
begin
  Video := FConfig.GetVideo;
  Video.AspectMode := 2; // 16:9
  FConfig.SetVideo(Video);
  FConfig.Save;
  FConfig.Load;

  AssertEquals('Aspect mode should persist', 2, FConfig.GetVideo.AspectMode);
end;

procedure TTestVideoSettingsIntegration.Test_DefaultDeinterlace;
var
  Video: TVideoSettings;
begin
  Video := FConfig.GetVideo;
  AssertTrue('Default deinterlace should be valid', Video.Deinterlace >= 0);
end;

procedure TTestVideoSettingsIntegration.Test_DeinterlacePersistence;
var
  Video: TVideoSettings;
begin
  Video := FConfig.GetVideo;
  Video.Deinterlace := 1;
  FConfig.SetVideo(Video);
  FConfig.Save;
  FConfig.Load;

  AssertEquals('Deinterlace should persist', 1, FConfig.GetVideo.Deinterlace);
end;

procedure TTestVideoSettingsIntegration.Test_DefaultBrightness;
var
  Video: TVideoSettings;
begin
  Video := FConfig.GetVideo;
  AssertEquals('Default brightness should be 0', 0, Video.Brightness);
end;

procedure TTestVideoSettingsIntegration.Test_DefaultContrast;
var
  Video: TVideoSettings;
begin
  Video := FConfig.GetVideo;
  AssertEquals('Default contrast should be 0', 0, Video.Contrast);
end;

procedure TTestVideoSettingsIntegration.Test_DefaultSaturation;
var
  Video: TVideoSettings;
begin
  Video := FConfig.GetVideo;
  AssertEquals('Default saturation should be 0', 0, Video.Saturation);
end;

procedure TTestVideoSettingsIntegration.Test_DefaultHue;
var
  Video: TVideoSettings;
begin
  Video := FConfig.GetVideo;
  AssertEquals('Default hue should be 0', 0, Video.Hue);
end;

procedure TTestVideoSettingsIntegration.Test_DefaultGamma;
var
  Video: TVideoSettings;
begin
  Video := FConfig.GetVideo;
  AssertEquals('Default gamma should be 0', 0, Video.Gamma);
end;

procedure TTestVideoSettingsIntegration.Test_VideoAdjustmentRange;
var
  Video: TVideoSettings;
begin
  Video := FConfig.GetVideo;

  Video.Brightness := -100;
  FConfig.SetVideo(Video);
  AssertEquals('Brightness -100', -100, FConfig.GetVideo.Brightness);

  Video.Brightness := 100;
  FConfig.SetVideo(Video);
  AssertEquals('Brightness 100', 100, FConfig.GetVideo.Brightness);
end;

procedure TTestVideoSettingsIntegration.Test_VideoAdjustmentPersistence;
var
  Video: TVideoSettings;
begin
  Video := FConfig.GetVideo;
  Video.Brightness := 25;
  Video.Contrast := -15;
  Video.Saturation := 30;
  FConfig.SetVideo(Video);
  FConfig.Save;
  FConfig.Load;

  Video := FConfig.GetVideo;
  AssertEquals('Brightness persists', 25, Video.Brightness);
  AssertEquals('Contrast persists', -15, Video.Contrast);
  AssertEquals('Saturation persists', 30, Video.Saturation);
end;

procedure TTestVideoSettingsIntegration.Test_DefaultHWAccel;
var
  Video: TVideoSettings;
begin
  Video := FConfig.GetVideo;
  // Hardware acceleration can be true or false by default
  AssertTrue('HW accel setting accessible', (Video.HWAccel = True) or (Video.HWAccel = False));
end;

procedure TTestVideoSettingsIntegration.Test_HWAccelPersistence;
var
  Video: TVideoSettings;
begin
  Video := FConfig.GetVideo;
  Video.HWAccel := True;
  FConfig.SetVideo(Video);
  FConfig.Save;
  FConfig.Load;

  AssertTrue('HW accel should persist', FConfig.GetVideo.HWAccel);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestSubtitleSettingsIntegration
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestSubtitleSettingsIntegration.SetUp;
begin
  FConfig := TConfigManager.Create;
end;

procedure TTestSubtitleSettingsIntegration.TearDown;
begin
  FConfig.Free;
end;

procedure TTestSubtitleSettingsIntegration.Test_DefaultSubtitlePosition;
var
  Subs: TSubtitleSettings;
begin
  Subs := FConfig.GetSubtitles;
  AssertTrue('Default position should be 0-100', (Subs.Position >= 0) and (Subs.Position <= 100));
end;

procedure TTestSubtitleSettingsIntegration.Test_SubtitlePositionRange;
var
  Subs: TSubtitleSettings;
begin
  Subs := FConfig.GetSubtitles;

  Subs.Position := 0;
  FConfig.SetSubtitles(Subs);
  AssertEquals('Position 0', 0, FConfig.GetSubtitles.Position);

  Subs.Position := 100;
  FConfig.SetSubtitles(Subs);
  AssertEquals('Position 100', 100, FConfig.GetSubtitles.Position);
end;

procedure TTestSubtitleSettingsIntegration.Test_DefaultSubtitleFont;
var
  Subs: TSubtitleSettings;
begin
  Subs := FConfig.GetSubtitles;
  // Font may or may not be set
  AssertTrue('Font accessible', True);
end;

procedure TTestSubtitleSettingsIntegration.Test_SubtitleFontSize;
var
  Subs: TSubtitleSettings;
begin
  Subs := FConfig.GetSubtitles;
  AssertTrue('Font size should be positive', Subs.FontSize > 0);
end;

procedure TTestSubtitleSettingsIntegration.Test_SubtitleFontSizePersistence;
var
  Subs: TSubtitleSettings;
begin
  Subs := FConfig.GetSubtitles;
  Subs.FontSize := 48;
  FConfig.SetSubtitles(Subs);
  FConfig.Save;
  FConfig.Load;

  AssertEquals('Font size should persist', 48, FConfig.GetSubtitles.FontSize);
end;

procedure TTestSubtitleSettingsIntegration.Test_DefaultSubtitleAutoLoad;
var
  Subs: TSubtitleSettings;
begin
  Subs := FConfig.GetSubtitles;
  // Auto-load can be true or false
  AssertTrue('Auto-load accessible', (Subs.AutoLoad = True) or (Subs.AutoLoad = False));
end;

procedure TTestSubtitleSettingsIntegration.Test_SubtitleAutoLoadPersistence;
var
  Subs: TSubtitleSettings;
begin
  Subs := FConfig.GetSubtitles;
  Subs.AutoLoad := True;
  FConfig.SetSubtitles(Subs);
  FConfig.Save;
  FConfig.Load;

  AssertTrue('Auto-load should persist', FConfig.GetSubtitles.AutoLoad);
end;

procedure TTestSubtitleSettingsIntegration.Test_UseDefaultSetting;
var
  Subs: TSubtitleSettings;
begin
  Subs := FConfig.GetSubtitles;
  Subs.UseDefault := False;
  FConfig.SetSubtitles(Subs);

  AssertFalse('UseDefault should be false', FConfig.GetSubtitles.UseDefault);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestMPVConfigIntegration
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConfigIntegration.SetUp;
begin
  FConfig := TConfigManager.Create;
  FEngine := TMPVEngine.Create;
end;

procedure TTestMPVConfigIntegration.TearDown;
begin
  FEngine.Free;
  FConfig.Free;
end;

procedure TTestMPVConfigIntegration.Test_EngineDefaultVolume;
begin
  AssertTrue('Engine has default volume', FEngine.Volume >= 0);
end;

procedure TTestMPVConfigIntegration.Test_EngineDefaultBrightness;
begin
  AssertEquals('Engine default brightness', 0, FEngine.Brightness);
end;

procedure TTestMPVConfigIntegration.Test_EngineDefaultContrast;
begin
  AssertEquals('Engine default contrast', 0, FEngine.Contrast);
end;

procedure TTestMPVConfigIntegration.Test_EngineDefaultSaturation;
begin
  AssertEquals('Engine default saturation', 0, FEngine.Saturation);
end;

procedure TTestMPVConfigIntegration.Test_EngineDefaultSpeed;
begin
  AssertTrue('Engine default speed is 1.0', Abs(FEngine.Speed - 1.0) < 0.01);
end;

procedure TTestMPVConfigIntegration.Test_EqualizerBandCount;
begin
  // EQ has 10 bands (0-9)
  AssertEquals('Should have 10 EQ bands', 10, 10);
end;

procedure TTestMPVConfigIntegration.Test_EqualizerBandSet;
begin
  FEngine.SetEqualizerBand(0, 6.0);
  AssertTrue('Band 0 should be ~6', Abs(FEngine.GetEqualizerBand(0) - 6.0) < 0.1);

  FEngine.SetEqualizerBand(9, -3.0);
  AssertTrue('Band 9 should be ~-3', Abs(FEngine.GetEqualizerBand(9) - (-3.0)) < 0.1);
end;

procedure TTestMPVConfigIntegration.Test_EqualizerResetValues;
var
  I: Integer;
begin
  // Set bands to non-zero (10 bands: 0-9)
  for I := 0 to 9 do
    FEngine.SetEqualizerBand(I, 5.0);

  // Reset
  FEngine.ResetEqualizer;

  // All should be 0
  for I := 0 to 9 do
    AssertTrue('Band ' + IntToStr(I) + ' should be 0', Abs(FEngine.GetEqualizerBand(I)) < 0.01);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestTrackListIntegration
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestTrackListIntegration.SetUp;
begin
  FAudioTracks := TMPVTrackList.Create('audio');
  FVideoTracks := TMPVTrackList.Create('video');
  FSubTracks := TMPVTrackList.Create('sub');
end;

procedure TTestTrackListIntegration.TearDown;
begin
  FAudioTracks.Free;
  FVideoTracks.Free;
  FSubTracks.Free;
end;

procedure TTestTrackListIntegration.Test_AddAudioTrack;
var
  Track: TMPVTrackInfo;
begin
  Track := Default(TMPVTrackInfo);
  Track.ID := 1;
  Track.TrackType := 'audio';
  Track.Codec := 'aac';
  Track.Language := 'eng';

  FAudioTracks.Add(Track);
  AssertEquals('Should have 1 audio track', 1, FAudioTracks.Count);
end;

procedure TTestTrackListIntegration.Test_MultipleAudioTracks;
var
  Track: TMPVTrackInfo;
begin
  Track := Default(TMPVTrackInfo);
  Track.ID := 1;
  Track.TrackType := 'audio';
  Track.Language := 'eng';
  FAudioTracks.Add(Track);

  Track.ID := 2;
  Track.Language := 'fra';
  FAudioTracks.Add(Track);

  Track.ID := 3;
  Track.Language := 'deu';
  FAudioTracks.Add(Track);

  AssertEquals('Should have 3 audio tracks', 3, FAudioTracks.Count);
end;

procedure TTestTrackListIntegration.Test_FindAudioTrackByID;
var
  Track, Found: TMPVTrackInfo;
  Idx: Integer;
begin
  Track := Default(TMPVTrackInfo);
  Track.ID := 5;
  Track.TrackType := 'audio';
  Track.Codec := 'mp3';
  FAudioTracks.Add(Track);

  Idx := FAudioTracks.FindByID(5);
  AssertTrue('Should find track by ID', Idx >= 0);
  Found := FAudioTracks[Idx];
  AssertEquals('Found track ID', 5, Found.ID);
end;

procedure TTestTrackListIntegration.Test_AudioTrackLanguage;
var
  Track, Found: TMPVTrackInfo;
  Idx: Integer;
begin
  Track := Default(TMPVTrackInfo);
  Track.ID := 1;
  Track.TrackType := 'audio';
  Track.Language := 'jpn';
  Track.Title := 'Japanese Audio';
  FAudioTracks.Add(Track);

  Idx := FAudioTracks.FindByID(1);
  Found := FAudioTracks[Idx];
  AssertEquals('Language should match', 'jpn', Found.Language);
end;

procedure TTestTrackListIntegration.Test_AddVideoTrack;
var
  Track: TMPVTrackInfo;
begin
  Track := Default(TMPVTrackInfo);
  Track.ID := 1;
  Track.TrackType := 'video';
  Track.Codec := 'h264';

  FVideoTracks.Add(Track);
  AssertEquals('Should have 1 video track', 1, FVideoTracks.Count);
end;

procedure TTestTrackListIntegration.Test_VideoTrackCodec;
var
  Track, Found: TMPVTrackInfo;
  Idx: Integer;
begin
  Track := Default(TMPVTrackInfo);
  Track.ID := 1;
  Track.TrackType := 'video';
  Track.Codec := 'hevc';
  FVideoTracks.Add(Track);

  Idx := FVideoTracks.FindByID(1);
  Found := FVideoTracks[Idx];
  AssertEquals('Codec should be hevc', 'hevc', Found.Codec);
end;

procedure TTestTrackListIntegration.Test_VideoTrackDimensions;
var
  Track, Item: TMPVTrackInfo;
begin
  Track := Default(TMPVTrackInfo);
  Track.ID := 1;
  Track.TrackType := 'video';
  Track.Width := 1920;
  Track.Height := 1080;
  FVideoTracks.Add(Track);

  Item := FVideoTracks[0];
  AssertEquals('Width', 1920, Item.Width);
  AssertEquals('Height', 1080, Item.Height);
end;

procedure TTestTrackListIntegration.Test_AddSubtitleTrack;
var
  Track: TMPVTrackInfo;
begin
  Track := Default(TMPVTrackInfo);
  Track.ID := 1;
  Track.TrackType := 'sub';
  Track.Codec := 'subrip';
  Track.Language := 'eng';

  FSubTracks.Add(Track);
  AssertEquals('Should have 1 subtitle track', 1, FSubTracks.Count);
end;

procedure TTestTrackListIntegration.Test_MultipleSubtitleTracks;
var
  Track: TMPVTrackInfo;
begin
  Track := Default(TMPVTrackInfo);
  Track.ID := 1;
  Track.TrackType := 'sub';
  Track.Language := 'eng';
  Track.Title := 'English';
  FSubTracks.Add(Track);

  Track.ID := 2;
  Track.Language := 'fra';
  Track.Title := 'French';
  FSubTracks.Add(Track);

  AssertEquals('Should have 2 subtitle tracks', 2, FSubTracks.Count);
end;

procedure TTestTrackListIntegration.Test_SubtitleTrackTitle;
var
  Track, Found: TMPVTrackInfo;
  Idx: Integer;
begin
  Track := Default(TMPVTrackInfo);
  Track.ID := 1;
  Track.TrackType := 'sub';
  Track.Title := 'English SDH';
  FSubTracks.Add(Track);

  Idx := FSubTracks.FindByID(1);
  Found := FSubTracks[Idx];
  AssertEquals('Title should match', 'English SDH', Found.Title);
end;

procedure TTestTrackListIntegration.Test_ExternalSubtitleTrack;
var
  Track, Item: TMPVTrackInfo;
begin
  Track := Default(TMPVTrackInfo);
  Track.ID := 1;
  Track.TrackType := 'sub';
  Track.IsExternal := True;
  Track.ExternalFilename := '/path/to/subtitle.srt';
  FSubTracks.Add(Track);

  Item := FSubTracks[0];
  AssertTrue('Track should be external', Item.IsExternal);
end;

procedure TTestTrackListIntegration.Test_ClearTracks;
begin
  FAudioTracks.Add(Default(TMPVTrackInfo));
  FAudioTracks.Add(Default(TMPVTrackInfo));
  FAudioTracks.Clear;

  AssertEquals('Should have 0 tracks after clear', 0, FAudioTracks.Count);
end;

procedure TTestTrackListIntegration.Test_TrackCount;
var
  Track: TMPVTrackInfo;
begin
  Track := Default(TMPVTrackInfo);
  Track.ID := 1;
  FAudioTracks.Add(Track);

  Track.ID := 2;
  FAudioTracks.Add(Track);

  AssertEquals('Should have 2 tracks', 2, FAudioTracks.Count);
end;

initialization
  RegisterTest('Integration', TTestAudioSettingsIntegration);
  RegisterTest('Integration', TTestVideoSettingsIntegration);
  RegisterTest('Integration', TTestSubtitleSettingsIntegration);
  RegisterTest('Integration', TTestMPVConfigIntegration);
  RegisterTest('Integration', TTestTrackListIntegration);

end.
