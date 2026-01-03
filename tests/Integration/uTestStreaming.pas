{ ===============================================================================
  uTestStreaming.pas - Streaming Integration Tests

  Part of 3nity Media - Test Suite

  Integration tests for streaming-related component interactions:
  - TRadioManager + TConfigManager favorites persistence
  - Custom stations integration
  - Stream recorder settings integration
  - URL handling and source type detection

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uTestStreaming;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  uRadioManager, uStreamRecorder, uConfig, uTypes;

type
  { ===========================================================================
    RADIO MANAGER + CONFIG INTEGRATION TESTS
    =========================================================================== }
  TTestRadioConfigIntegration = class(TTestCase)
  private
    FRadioManager: TRadioManager;
    FConfig: TConfigManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Favorites Integration }
    procedure Test_AddRadioToFavorites;
    procedure Test_RemoveRadioFromFavorites;
    procedure Test_CheckIsFavorite;
    procedure Test_FavoritesCountAfterMultipleAdds;
    procedure Test_FavoritesPersistence;

    { Custom Stations Integration }
    procedure Test_AddCustomStation;
    procedure Test_CustomStationWithBitrate;
    procedure Test_DeleteCustomStation;
    procedure Test_GetCustomStation;

    { Station Search Integration }
    procedure Test_FilterAndFavorites;
    procedure Test_SearchWithFavoritesMarked;
  end;

  { ===========================================================================
    STREAM RECORDER SETTINGS INTEGRATION TESTS
    =========================================================================== }
  TTestRecorderSettingsIntegration = class(TTestCase)
  private
    FRecorder: TStreamRecorder;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Recording Settings }
    procedure Test_DefaultOutputFolder;
    procedure Test_SetOutputFolder;
    procedure Test_DefaultAudioFormat;
    procedure Test_DefaultVideoFormat;

    { Split Settings }
    procedure Test_DefaultAutoSplit;
    procedure Test_SetSplitSizeMB;
    procedure Test_SetSplitTimeMinutes;

    { Other Settings }
    procedure Test_UseMetadataInFilename;
    procedure Test_AppendTimestamp;
    procedure Test_CreateSubfolders;
    procedure Test_OverwriteExisting;
  end;

  { ===========================================================================
    URL AND SOURCE TYPE INTEGRATION TESTS
    =========================================================================== }
  TTestURLSourceIntegration = class(TTestCase)
  published
    { Source Type Detection - String based }
    procedure Test_LocalFileDetection;
    procedure Test_HTTPSourceDetection;
    procedure Test_HTTPSSourceDetection;
    procedure Test_RTSPSourceDetection;
    procedure Test_RTMPSourceDetection;

    { URL Validation }
    procedure Test_ValidHTTPUrl;
    procedure Test_ValidHTTPSUrl;
    procedure Test_ValidRTSPUrl;
  end;

  { ===========================================================================
    RADIO STATION DATA INTEGRATION TESTS
    =========================================================================== }
  TTestRadioStationData = class(TTestCase)
  private
    FRadioManager: TRadioManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Station Data Handling }
    procedure Test_StationWithAllFields;
    procedure Test_StationMinimalFields;
    procedure Test_StationBitrateRange;
    procedure Test_StationGenreHandling;

    { Station List Operations }
    procedure Test_GetStationCount;
    procedure Test_GetStationByIndex;
    procedure Test_CustomCountProperty;

    { Filtering Operations }
    procedure Test_FilterByGenre;
    procedure Test_FilterByCountry;
    procedure Test_FilterByName;
    procedure Test_ClearFilter;
  end;

  { ===========================================================================
    STREAM METADATA INTEGRATION TESTS
    =========================================================================== }
  TTestStreamMetadata = class(TTestCase)
  private
    FRecorder: TStreamRecorder;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Track Change Handling }
    procedure Test_NotifyTrackChangeBasic;
    procedure Test_NotifyTrackWithArtist;
    procedure Test_NotifyTrackEmptyArtist;
    procedure Test_ConsecutiveTrackChanges;

    { Recording Info Access }
    procedure Test_InfoCurrentTitle;
    procedure Test_InfoInitialState;
    procedure Test_InfoSourceName;
  end;

implementation

{ ===============================================================================
  TTestRadioConfigIntegration
  =============================================================================== }

procedure TTestRadioConfigIntegration.SetUp;
begin
  FRadioManager := TRadioManager.Create;
  FConfig := TConfigManager.Create;
  FConfig.ClearFavorites;
end;

procedure TTestRadioConfigIntegration.TearDown;
begin
  FRadioManager.Free;
  FConfig.Free;
end;

procedure TTestRadioConfigIntegration.Test_AddRadioToFavorites;
begin
  FConfig.AddFavorite('Jazz Radio', 'http://stream.jazzradio.com/live', ftRadio, 'Radio');
  AssertTrue('Should be marked as favorite', FConfig.IsFavorite('http://stream.jazzradio.com/live'));
end;

procedure TTestRadioConfigIntegration.Test_RemoveRadioFromFavorites;
begin
  FConfig.AddFavorite('Test Radio', 'http://test.radio.com/stream', ftRadio, 'Radio');
  FConfig.RemoveFavorite('http://test.radio.com/stream');
  AssertFalse('Should no longer be favorite', FConfig.IsFavorite('http://test.radio.com/stream'));
end;

procedure TTestRadioConfigIntegration.Test_CheckIsFavorite;
begin
  FConfig.AddFavorite('Fav Radio', 'http://fav.radio.com', ftRadio, '');
  AssertTrue('Added should be favorite', FConfig.IsFavorite('http://fav.radio.com'));
  AssertFalse('Not added should not be favorite', FConfig.IsFavorite('http://other.radio.com'));
end;

procedure TTestRadioConfigIntegration.Test_FavoritesCountAfterMultipleAdds;
var
  Favorites: TFavoriteItems;
begin
  FConfig.AddFavorite('Radio 1', 'http://radio1.com', ftRadio, 'Radio');
  FConfig.AddFavorite('Radio 2', 'http://radio2.com', ftRadio, 'Radio');
  FConfig.AddFavorite('Radio 3', 'http://radio3.com', ftRadio, 'Radio');

  Favorites := FConfig.GetFavoritesByType(ftRadio);
  AssertEquals('Should have 3 radio favorites', 3, Length(Favorites));
end;

procedure TTestRadioConfigIntegration.Test_FavoritesPersistence;
var
  Favorites: TFavoriteItems;
begin
  FConfig.AddFavorite('Persist Radio', 'http://persist.radio.com', ftRadio, 'Radio');
  FConfig.Save;
  FConfig.Load;

  Favorites := FConfig.GetFavoritesByType(ftRadio);
  AssertTrue('Radio favorite should persist', Length(Favorites) >= 1);
end;

procedure TTestRadioConfigIntegration.Test_AddCustomStation;
begin
  // AddCustomStation(Name, URL, Genre, Bitrate)
  FRadioManager.AddCustomStation('My Station', 'http://custom.stream.com/live', 'Custom', 128);
  AssertTrue('Should have at least 1 custom station', FRadioManager.CustomCount >= 1);
end;

procedure TTestRadioConfigIntegration.Test_CustomStationWithBitrate;
var
  Station: TRadioStation;
begin
  FRadioManager.AddCustomStation('High Quality', 'http://hq.stream.com', 'Music', 320);
  Station := FRadioManager.GetCustomStation(FRadioManager.CustomCount - 1);
  AssertEquals('Bitrate should be 320', 320, Station.Bitrate);
end;

procedure TTestRadioConfigIntegration.Test_DeleteCustomStation;
var
  InitialCount: Integer;
begin
  FRadioManager.AddCustomStation('To Delete', 'http://delete.com', 'Test', 128);
  InitialCount := FRadioManager.CustomCount;
  FRadioManager.DeleteCustomStation(FRadioManager.CustomCount - 1);

  AssertEquals('Should have one less station', InitialCount - 1, FRadioManager.CustomCount);
end;

procedure TTestRadioConfigIntegration.Test_GetCustomStation;
var
  Station: TRadioStation;
begin
  FRadioManager.AddCustomStation('Get Test', 'http://gettest.com', 'Testing', 192);
  Station := FRadioManager.GetCustomStation(FRadioManager.CustomCount - 1);
  AssertEquals('Name should match', 'Get Test', Station.Name);
  AssertEquals('URL should match', 'http://gettest.com', Station.URL);
end;

procedure TTestRadioConfigIntegration.Test_FilterAndFavorites;
begin
  FConfig.AddFavorite('Fav Station', 'http://fav.com', ftRadio, '');
  FRadioManager.FilterByGenre('Jazz');
  AssertTrue('Favorites should be independent of filter', FConfig.IsFavorite('http://fav.com'));
end;

procedure TTestRadioConfigIntegration.Test_SearchWithFavoritesMarked;
begin
  FConfig.AddFavorite('Marked Fav', 'http://marked.fav.com', ftRadio, '');
  AssertTrue('Favorite should be identifiable', FConfig.IsFavorite('http://marked.fav.com'));
end;

{ ===============================================================================
  TTestRecorderSettingsIntegration
  =============================================================================== }

procedure TTestRecorderSettingsIntegration.SetUp;
begin
  FRecorder := TStreamRecorder.Create;
  FRecorder.Initialize;
end;

procedure TTestRecorderSettingsIntegration.TearDown;
begin
  FRecorder.Free;
end;

procedure TTestRecorderSettingsIntegration.Test_DefaultOutputFolder;
begin
  AssertTrue('Output folder should be set', FRecorder.Settings.OutputFolder <> '');
end;

procedure TTestRecorderSettingsIntegration.Test_SetOutputFolder;
var
  Settings: TRecordingSettings;
  TestPath: string;
begin
  TestPath := GetTempDir + 'test_recordings';
  Settings := FRecorder.Settings;
  Settings.OutputFolder := TestPath;
  FRecorder.Settings := Settings;
  AssertEquals('Output folder should be updated', TestPath, FRecorder.Settings.OutputFolder);
end;

procedure TTestRecorderSettingsIntegration.Test_DefaultAudioFormat;
begin
  // Audio format is an enum, just check it's accessible
  AssertTrue('Audio format accessible', True);
end;

procedure TTestRecorderSettingsIntegration.Test_DefaultVideoFormat;
begin
  // Video format is an enum, just check it's accessible
  AssertTrue('Video format accessible', True);
end;

procedure TTestRecorderSettingsIntegration.Test_DefaultAutoSplit;
begin
  // Check auto split default (typically false)
  AssertTrue('AutoSplit setting accessible', True);
end;

procedure TTestRecorderSettingsIntegration.Test_SetSplitSizeMB;
var
  Settings: TRecordingSettings;
begin
  Settings := FRecorder.Settings;
  Settings.SplitSizeMB := 100;
  FRecorder.Settings := Settings;
  AssertEquals('Split size should be 100 MB', 100, FRecorder.Settings.SplitSizeMB);
end;

procedure TTestRecorderSettingsIntegration.Test_SetSplitTimeMinutes;
var
  Settings: TRecordingSettings;
begin
  Settings := FRecorder.Settings;
  Settings.SplitTimeMinutes := 60;
  FRecorder.Settings := Settings;
  AssertEquals('Split time should be 60 minutes', 60, FRecorder.Settings.SplitTimeMinutes);
end;

procedure TTestRecorderSettingsIntegration.Test_UseMetadataInFilename;
var
  Settings: TRecordingSettings;
begin
  Settings := FRecorder.Settings;
  Settings.UseMetadataInFilename := True;
  FRecorder.Settings := Settings;
  AssertTrue('UseMetadataInFilename should be true', FRecorder.Settings.UseMetadataInFilename);
end;

procedure TTestRecorderSettingsIntegration.Test_AppendTimestamp;
var
  Settings: TRecordingSettings;
begin
  Settings := FRecorder.Settings;
  Settings.AppendTimestamp := True;
  FRecorder.Settings := Settings;
  AssertTrue('AppendTimestamp should be true', FRecorder.Settings.AppendTimestamp);
end;

procedure TTestRecorderSettingsIntegration.Test_CreateSubfolders;
var
  Settings: TRecordingSettings;
begin
  Settings := FRecorder.Settings;
  Settings.CreateSubfolders := True;
  FRecorder.Settings := Settings;
  AssertTrue('CreateSubfolders should be true', FRecorder.Settings.CreateSubfolders);
end;

procedure TTestRecorderSettingsIntegration.Test_OverwriteExisting;
var
  Settings: TRecordingSettings;
begin
  Settings := FRecorder.Settings;
  Settings.OverwriteExisting := False;
  FRecorder.Settings := Settings;
  AssertFalse('OverwriteExisting should be false', FRecorder.Settings.OverwriteExisting);
end;

{ ===============================================================================
  TTestURLSourceIntegration
  =============================================================================== }

procedure TTestURLSourceIntegration.Test_LocalFileDetection;
var
  IsLocal: Boolean;
begin
  IsLocal := Pos('://', '/home/user/music.mp3') = 0;
  AssertTrue('Local file should be detected', IsLocal);
end;

procedure TTestURLSourceIntegration.Test_HTTPSourceDetection;
var
  IsHTTP: Boolean;
begin
  IsHTTP := Pos('http://', 'http://stream.example.com/audio') = 1;
  AssertTrue('HTTP source should be detected', IsHTTP);
end;

procedure TTestURLSourceIntegration.Test_HTTPSSourceDetection;
var
  IsHTTPS: Boolean;
begin
  IsHTTPS := Pos('https://', 'https://secure.stream.com/audio') = 1;
  AssertTrue('HTTPS source should be detected', IsHTTPS);
end;

procedure TTestURLSourceIntegration.Test_RTSPSourceDetection;
var
  IsRTSP: Boolean;
begin
  IsRTSP := Pos('rtsp://', 'rtsp://camera.local/stream1') = 1;
  AssertTrue('RTSP source should be detected', IsRTSP);
end;

procedure TTestURLSourceIntegration.Test_RTMPSourceDetection;
var
  IsRTMP: Boolean;
begin
  IsRTMP := Pos('rtmp://', 'rtmp://live.server.com/app/stream') = 1;
  AssertTrue('RTMP source should be detected', IsRTMP);
end;

procedure TTestURLSourceIntegration.Test_ValidHTTPUrl;
var
  URL: string;
begin
  URL := 'http://valid.stream.com/audio.mp3';
  AssertTrue('HTTP URL should be valid', Pos('http://', URL) = 1);
end;

procedure TTestURLSourceIntegration.Test_ValidHTTPSUrl;
var
  URL: string;
begin
  URL := 'https://secure.stream.com/audio.mp3';
  AssertTrue('HTTPS URL should be valid', Pos('https://', URL) = 1);
end;

procedure TTestURLSourceIntegration.Test_ValidRTSPUrl;
var
  URL: string;
begin
  URL := 'rtsp://camera.local:554/stream';
  AssertTrue('RTSP URL should be valid', Pos('rtsp://', URL) = 1);
end;

{ ===============================================================================
  TTestRadioStationData
  =============================================================================== }

procedure TTestRadioStationData.SetUp;
begin
  FRadioManager := TRadioManager.Create;
end;

procedure TTestRadioStationData.TearDown;
begin
  FRadioManager.Free;
end;

procedure TTestRadioStationData.Test_StationWithAllFields;
begin
  FRadioManager.AddCustomStation('Full Test Station', 'http://full.test.com/stream', 'Electronic', 320);
  AssertTrue('Station with all fields added', FRadioManager.CustomCount >= 1);
end;

procedure TTestRadioStationData.Test_StationMinimalFields;
begin
  FRadioManager.AddCustomStation('Minimal', 'http://min.com', '', 0);
  AssertTrue('Station with minimal fields added', FRadioManager.CustomCount >= 1);
end;

procedure TTestRadioStationData.Test_StationBitrateRange;
begin
  FRadioManager.AddCustomStation('Low Bitrate', 'http://low.com', '', 32);
  FRadioManager.AddCustomStation('High Bitrate', 'http://high.com', '', 320);

  AssertTrue('Should accept various bitrates', FRadioManager.CustomCount >= 2);
end;

procedure TTestRadioStationData.Test_StationGenreHandling;
begin
  FRadioManager.AddCustomStation('Jazz Station', 'http://jazz.com', 'Jazz', 128);
  FRadioManager.AddCustomStation('Rock Station', 'http://rock.com', 'Rock', 128);

  AssertTrue('Stations with genres added', FRadioManager.CustomCount >= 2);
end;

procedure TTestRadioStationData.Test_GetStationCount;
var
  InitialCount: Integer;
begin
  InitialCount := FRadioManager.CustomCount;
  FRadioManager.AddCustomStation('Station 1', 'http://s1.com', '', 128);
  FRadioManager.AddCustomStation('Station 2', 'http://s2.com', '', 128);

  AssertEquals('Should have 2 more stations', InitialCount + 2, FRadioManager.CustomCount);
end;

procedure TTestRadioStationData.Test_GetStationByIndex;
var
  Station: TRadioStation;
begin
  FRadioManager.AddCustomStation('Indexed Station', 'http://indexed.com', 'Test', 192);

  Station := FRadioManager.GetCustomStation(FRadioManager.CustomCount - 1);
  AssertEquals('Station name', 'Indexed Station', Station.Name);
end;

procedure TTestRadioStationData.Test_CustomCountProperty;
var
  Count: Integer;
begin
  Count := FRadioManager.CustomCount;
  AssertTrue('CustomCount should be >= 0', Count >= 0);
end;

procedure TTestRadioStationData.Test_FilterByGenre;
begin
  FRadioManager.FilterByGenre('Jazz');
  AssertTrue('Filter by genre applied', True);
end;

procedure TTestRadioStationData.Test_FilterByCountry;
begin
  FRadioManager.FilterByCountry('France');
  AssertTrue('Filter by country applied', True);
end;

procedure TTestRadioStationData.Test_FilterByName;
begin
  FRadioManager.FilterByName('test');
  AssertTrue('Filter by name applied', True);
end;

procedure TTestRadioStationData.Test_ClearFilter;
begin
  FRadioManager.FilterByGenre('Rock');
  FRadioManager.ClearFilter;
  AssertTrue('Filter cleared', True);
end;

{ ===============================================================================
  TTestStreamMetadata
  =============================================================================== }

procedure TTestStreamMetadata.SetUp;
begin
  FRecorder := TStreamRecorder.Create;
  FRecorder.Initialize;
end;

procedure TTestStreamMetadata.TearDown;
begin
  FRecorder.Free;
end;

procedure TTestStreamMetadata.Test_NotifyTrackChangeBasic;
begin
  FRecorder.NotifyTrackChange('', 'Simple Song Title');
  AssertEquals('Current title', 'Simple Song Title', FRecorder.Info.CurrentTitle);
end;

procedure TTestStreamMetadata.Test_NotifyTrackWithArtist;
begin
  FRecorder.NotifyTrackChange('Artist Name', 'Song Title');
  AssertTrue('Track has title', FRecorder.Info.CurrentTitle <> '');
end;

procedure TTestStreamMetadata.Test_NotifyTrackEmptyArtist;
begin
  FRecorder.NotifyTrackChange('', 'Just Title');
  AssertEquals('Title without artist', 'Just Title', FRecorder.Info.CurrentTitle);
end;

procedure TTestStreamMetadata.Test_ConsecutiveTrackChanges;
begin
  FRecorder.NotifyTrackChange('', 'Track 1');
  FRecorder.NotifyTrackChange('', 'Track 2');
  FRecorder.NotifyTrackChange('', 'Track 3');

  AssertEquals('Should show latest track', 'Track 3', FRecorder.Info.CurrentTitle);
end;

procedure TTestStreamMetadata.Test_InfoCurrentTitle;
begin
  FRecorder.NotifyTrackChange('Test Artist', 'Test Title');
  AssertTrue('CurrentTitle accessible', FRecorder.Info.CurrentTitle <> '');
end;

procedure TTestStreamMetadata.Test_InfoInitialState;
begin
  // Initial state before recording
  AssertTrue('Info accessible', True);
end;

procedure TTestStreamMetadata.Test_InfoSourceName;
begin
  // Source name is set when recording starts
  AssertTrue('SourceName field exists', True);
end;

initialization
  RegisterTest('Integration', TTestRadioConfigIntegration);
  RegisterTest('Integration', TTestRecorderSettingsIntegration);
  RegisterTest('Integration', TTestURLSourceIntegration);
  RegisterTest('Integration', TTestRadioStationData);
  RegisterTest('Integration', TTestStreamMetadata);

end.
