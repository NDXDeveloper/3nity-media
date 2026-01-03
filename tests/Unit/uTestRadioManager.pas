{ ═══════════════════════════════════════════════════════════════════════════════
  uTestRadioManager.pas - Unit Tests for Radio Manager

  Part of 3nity Media - Test Suite

  Tests for the uRadioManager unit which handles radio station management,
  filtering, favorites, custom stations, and Icecast XML parsing.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uTestRadioManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  uRadioManager, uTypes;

type
  { TTestRadioManager }
  TTestRadioManager = class(TTestCase)
  private
    FRadio: TRadioManager;
    FChangeCount: Integer;
    FLoadCount: Integer;
    FLastLoadedCount: Integer;
    procedure OnChangeHandler(Sender: TObject);
    procedure OnLoadHandler(Sender: TObject; Count: Integer);
    function CreateTestStation(const AName, AURL, AGenre: string; ABitrate: Integer = 128): TRadioStation;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Initial state tests }
    procedure Test_Create_EmptyStations;
    procedure Test_Create_EmptyCustomStations;
    procedure Test_Create_EmptyFavorites;
    procedure Test_Create_EmptyFiltered;
    procedure Test_Create_EmptyGenres;
    procedure Test_Create_NotLoading;
    procedure Test_Create_NotModified;
    procedure Test_Create_EmptyLastError;

    { LoadDefaultStations tests }
    procedure Test_LoadDefaultStations_LoadsTenStations;
    procedure Test_LoadDefaultStations_FirstStation;
    procedure Test_LoadDefaultStations_ExtractsGenres;
    procedure Test_LoadDefaultStations_SetsFilteredCount;

    { Station access tests }
    procedure Test_GetStation_ValidIndex;
    procedure Test_GetStation_InvalidIndex_ReturnsDefault;
    procedure Test_GetFilteredStation_ValidIndex;
    procedure Test_GetFilteredStation_InvalidIndex_ReturnsDefault;

    { Custom stations tests }
    procedure Test_AddCustomStation_IncreasesCount;
    procedure Test_AddCustomStation_ReturnsIndex;
    procedure Test_AddCustomStation_SetsModified;
    procedure Test_AddCustomStation_StoresData;
    procedure Test_EditCustomStation_UpdatesData;
    procedure Test_EditCustomStation_InvalidIndex_NoChange;
    procedure Test_DeleteCustomStation_DecreasesCount;
    procedure Test_DeleteCustomStation_InvalidIndex_NoChange;
    procedure Test_GetCustomStation_ValidIndex;
    procedure Test_GetCustomStation_InvalidIndex_ReturnsDefault;
    procedure Test_CustomStation_AppearsInFiltered;

    { Favorites tests }
    procedure Test_AddToFavorites_IncreasesCount;
    procedure Test_AddToFavorites_SetsModified;
    procedure Test_AddToFavorites_NoDuplicates;
    procedure Test_IsFavorite_True;
    procedure Test_IsFavorite_False;
    procedure Test_RemoveFromFavorites_DecreasesCount;
    procedure Test_RemoveFromFavorites_InvalidIndex_NoChange;
    procedure Test_ToggleFavorite_AddsIfNotFavorite;
    procedure Test_ToggleFavorite_RemovesIfFavorite;
    procedure Test_GetFavorite_ValidIndex;
    procedure Test_GetFavorite_InvalidIndex_ReturnsDefault;

    { Filtering tests }
    procedure Test_FilterByGenre_FiltersCorrectly;
    procedure Test_FilterByGenre_CaseInsensitive;
    procedure Test_FilterByGenre_Empty_ClearsFilter;
    procedure Test_FilterByName_MatchesName;
    procedure Test_FilterByName_MatchesGenre;
    procedure Test_FilterByName_CaseInsensitive;
    procedure Test_FilterByCountry_FiltersCorrectly;
    procedure Test_FilterFavorites_ShowsOnlyFavorites;
    procedure Test_ClearFilter_ShowsAll;
    procedure Test_Filter_IncludesCustomStations;

    { Search tests }
    procedure Test_Search_MatchesName;
    procedure Test_Search_MatchesGenre;
    procedure Test_Search_MatchesCountry;
    procedure Test_Search_CaseInsensitive;
    procedure Test_Search_EmptyReturnsEmpty;
    procedure Test_Search_NoMatchReturnsEmpty;

    { LoadFromXML tests }
    procedure Test_LoadFromXML_EmptyReturnsError;
    procedure Test_LoadFromXML_ValidXML;
    procedure Test_LoadFromXML_ExtractsGenres;
    procedure Test_LoadFromXML_SkipsEmptyURL;

    { Clear tests }
    procedure Test_Clear_EmptiesStations;
    procedure Test_Clear_EmptiesFiltered;
    procedure Test_Clear_EmptiesGenres;

    { Events tests }
    procedure Test_OnChange_TriggeredOnAddCustom;
    procedure Test_OnChange_TriggeredOnFilter;
    procedure Test_OnLoad_TriggeredOnLoadDefault;
    procedure Test_OnLoad_ReceivesCount;

    { Constants tests }
    procedure Test_IcecastURL_Defined;
    procedure Test_DefaultStations_Count;
  end;

implementation

const
  { Sample Icecast XML for testing }
  SAMPLE_ICECAST_XML =
    '<?xml version="1.0"?>' +
    '<icestats>' +
    '  <entry>' +
    '    <server_name>Test Radio 1</server_name>' +
    '    <listen_url>http://test1.example.com:8000/stream</listen_url>' +
    '    <genre>Rock</genre>' +
    '    <bitrate>128</bitrate>' +
    '    <server_type>audio/mpeg</server_type>' +
    '  </entry>' +
    '  <entry>' +
    '    <server_name>Test Radio 2</server_name>' +
    '    <listen_url>http://test2.example.com:8000/stream</listen_url>' +
    '    <genre>Jazz</genre>' +
    '    <bitrate>256</bitrate>' +
    '  </entry>' +
    '  <entry>' +
    '    <server_name>No URL Station</server_name>' +
    '    <genre>Pop</genre>' +
    '  </entry>' +
    '</icestats>';

{ TTestRadioManager }

procedure TTestRadioManager.SetUp;
begin
  FRadio := TRadioManager.Create;
  FChangeCount := 0;
  FLoadCount := 0;
  FLastLoadedCount := 0;
end;

procedure TTestRadioManager.TearDown;
begin
  FRadio.Free;
end;

procedure TTestRadioManager.OnChangeHandler(Sender: TObject);
begin
  Inc(FChangeCount);
end;

procedure TTestRadioManager.OnLoadHandler(Sender: TObject; Count: Integer);
begin
  Inc(FLoadCount);
  FLastLoadedCount := Count;
end;

function TTestRadioManager.CreateTestStation(const AName, AURL, AGenre: string;
  ABitrate: Integer): TRadioStation;
begin
  Result := Default(TRadioStation);
  Result.Name := AName;
  Result.URL := AURL;
  Result.Genre := AGenre;
  Result.Bitrate := ABitrate;
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Initial State Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestRadioManager.Test_Create_EmptyStations;
begin
  AssertEquals('Initial station count', 0, FRadio.Count);
end;

procedure TTestRadioManager.Test_Create_EmptyCustomStations;
begin
  AssertEquals('Initial custom count', 0, FRadio.CustomCount);
end;

procedure TTestRadioManager.Test_Create_EmptyFavorites;
begin
  AssertEquals('Initial favorites count', 0, FRadio.FavoritesCount);
end;

procedure TTestRadioManager.Test_Create_EmptyFiltered;
begin
  AssertEquals('Initial filtered count', 0, FRadio.FilteredCount);
end;

procedure TTestRadioManager.Test_Create_EmptyGenres;
begin
  AssertEquals('Initial genres count', 0, FRadio.Genres.Count);
end;

procedure TTestRadioManager.Test_Create_NotLoading;
begin
  AssertFalse('Not loading initially', FRadio.Loading);
end;

procedure TTestRadioManager.Test_Create_NotModified;
begin
  AssertFalse('Not modified initially', FRadio.Modified);
end;

procedure TTestRadioManager.Test_Create_EmptyLastError;
begin
  AssertEquals('No initial error', '', FRadio.LastError);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  LoadDefaultStations Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestRadioManager.Test_LoadDefaultStations_LoadsTenStations;
begin
  FRadio.LoadDefaultStations;
  AssertEquals('10 default stations', 10, FRadio.Count);
end;

procedure TTestRadioManager.Test_LoadDefaultStations_FirstStation;
begin
  FRadio.LoadDefaultStations;
  AssertEquals('First station name', 'France Inter', FRadio.Stations[0].Name);
  AssertTrue('First station has URL', FRadio.Stations[0].URL <> '');
end;

procedure TTestRadioManager.Test_LoadDefaultStations_ExtractsGenres;
begin
  FRadio.LoadDefaultStations;
  AssertTrue('Genres extracted', FRadio.Genres.Count > 0);
end;

procedure TTestRadioManager.Test_LoadDefaultStations_SetsFilteredCount;
begin
  FRadio.LoadDefaultStations;
  AssertEquals('Filtered equals total', FRadio.Count, FRadio.FilteredCount);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Station Access Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestRadioManager.Test_GetStation_ValidIndex;
begin
  FRadio.LoadDefaultStations;
  AssertTrue('Valid station has name', FRadio.Stations[0].Name <> '');
end;

procedure TTestRadioManager.Test_GetStation_InvalidIndex_ReturnsDefault;
var
  Station: TRadioStation;
begin
  FRadio.LoadDefaultStations;
  Station := FRadio.Stations[100];
  AssertEquals('Invalid index returns empty name', '', Station.Name);
  AssertEquals('Invalid index returns empty URL', '', Station.URL);
end;

procedure TTestRadioManager.Test_GetFilteredStation_ValidIndex;
begin
  FRadio.LoadDefaultStations;
  AssertTrue('Valid filtered station', FRadio.FilteredStations[0].Name <> '');
end;

procedure TTestRadioManager.Test_GetFilteredStation_InvalidIndex_ReturnsDefault;
var
  Station: TRadioStation;
begin
  FRadio.LoadDefaultStations;
  Station := FRadio.FilteredStations[100];
  AssertEquals('Invalid returns empty', '', Station.Name);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Custom Stations Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestRadioManager.Test_AddCustomStation_IncreasesCount;
begin
  FRadio.AddCustomStation('Test', 'http://test.com', 'Rock', 128);
  AssertEquals('Custom count is 1', 1, FRadio.CustomCount);
end;

procedure TTestRadioManager.Test_AddCustomStation_ReturnsIndex;
var
  Idx: Integer;
begin
  Idx := FRadio.AddCustomStation('Test', 'http://test.com', 'Rock', 128);
  AssertEquals('First index is 0', 0, Idx);
  Idx := FRadio.AddCustomStation('Test2', 'http://test2.com', 'Jazz', 256);
  AssertEquals('Second index is 1', 1, Idx);
end;

procedure TTestRadioManager.Test_AddCustomStation_SetsModified;
begin
  AssertFalse('Not modified before', FRadio.Modified);
  FRadio.AddCustomStation('Test', 'http://test.com', 'Rock', 128);
  AssertTrue('Modified after add', FRadio.Modified);
end;

procedure TTestRadioManager.Test_AddCustomStation_StoresData;
var
  Station: TRadioStation;
begin
  FRadio.AddCustomStation('My Station', 'http://my.url', 'Blues', 192);
  Station := FRadio.GetCustomStation(0);
  AssertEquals('Name stored', 'My Station', Station.Name);
  AssertEquals('URL stored', 'http://my.url', Station.URL);
  AssertEquals('Genre stored', 'Blues', Station.Genre);
  AssertEquals('Bitrate stored', 192, Station.Bitrate);
end;

procedure TTestRadioManager.Test_EditCustomStation_UpdatesData;
var
  Station: TRadioStation;
begin
  FRadio.AddCustomStation('Original', 'http://orig.com', 'Rock', 128);
  FRadio.EditCustomStation(0, 'Updated', 'http://new.com', 'Jazz', 256);
  Station := FRadio.GetCustomStation(0);
  AssertEquals('Name updated', 'Updated', Station.Name);
  AssertEquals('URL updated', 'http://new.com', Station.URL);
  AssertEquals('Genre updated', 'Jazz', Station.Genre);
  AssertEquals('Bitrate updated', 256, Station.Bitrate);
end;

procedure TTestRadioManager.Test_EditCustomStation_InvalidIndex_NoChange;
begin
  FRadio.AddCustomStation('Test', 'http://test.com', 'Rock', 128);
  FRadio.EditCustomStation(100, 'Bad', 'http://bad.com', 'Bad', 0);
  AssertEquals('Original name unchanged', 'Test', FRadio.GetCustomStation(0).Name);
end;

procedure TTestRadioManager.Test_DeleteCustomStation_DecreasesCount;
begin
  FRadio.AddCustomStation('Test1', 'http://test1.com', 'Rock', 128);
  FRadio.AddCustomStation('Test2', 'http://test2.com', 'Jazz', 128);
  AssertEquals('Count before delete', 2, FRadio.CustomCount);
  FRadio.DeleteCustomStation(0);
  AssertEquals('Count after delete', 1, FRadio.CustomCount);
end;

procedure TTestRadioManager.Test_DeleteCustomStation_InvalidIndex_NoChange;
begin
  FRadio.AddCustomStation('Test', 'http://test.com', 'Rock', 128);
  FRadio.DeleteCustomStation(100);
  FRadio.DeleteCustomStation(-1);
  AssertEquals('Count unchanged', 1, FRadio.CustomCount);
end;

procedure TTestRadioManager.Test_GetCustomStation_ValidIndex;
var
  Station: TRadioStation;
begin
  FRadio.AddCustomStation('Test', 'http://test.com', 'Rock', 128);
  Station := FRadio.GetCustomStation(0);
  AssertEquals('Retrieved name', 'Test', Station.Name);
end;

procedure TTestRadioManager.Test_GetCustomStation_InvalidIndex_ReturnsDefault;
var
  Station: TRadioStation;
begin
  Station := FRadio.GetCustomStation(0);
  AssertEquals('Empty name for invalid', '', Station.Name);
end;

procedure TTestRadioManager.Test_CustomStation_AppearsInFiltered;
begin
  FRadio.AddCustomStation('Custom', 'http://custom.com', 'Rock', 128);
  FRadio.ClearFilter;
  AssertEquals('Custom appears in filtered', 1, FRadio.FilteredCount);
  AssertEquals('Correct name', 'Custom', FRadio.FilteredStations[0].Name);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Favorites Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestRadioManager.Test_AddToFavorites_IncreasesCount;
var
  Station: TRadioStation;
begin
  Station := CreateTestStation('Fav', 'http://fav.com', 'Rock');
  FRadio.AddToFavorites(Station);
  AssertEquals('Favorites count', 1, FRadio.FavoritesCount);
end;

procedure TTestRadioManager.Test_AddToFavorites_SetsModified;
var
  Station: TRadioStation;
begin
  Station := CreateTestStation('Fav', 'http://fav.com', 'Rock');
  AssertFalse('Not modified before', FRadio.Modified);
  FRadio.AddToFavorites(Station);
  AssertTrue('Modified after', FRadio.Modified);
end;

procedure TTestRadioManager.Test_AddToFavorites_NoDuplicates;
var
  Station: TRadioStation;
begin
  Station := CreateTestStation('Fav', 'http://fav.com', 'Rock');
  FRadio.AddToFavorites(Station);
  FRadio.AddToFavorites(Station);
  AssertEquals('No duplicate', 1, FRadio.FavoritesCount);
end;

procedure TTestRadioManager.Test_IsFavorite_True;
var
  Station: TRadioStation;
begin
  Station := CreateTestStation('Fav', 'http://fav.com', 'Rock');
  FRadio.AddToFavorites(Station);
  AssertTrue('Is favorite', FRadio.IsFavorite('http://fav.com'));
end;

procedure TTestRadioManager.Test_IsFavorite_False;
begin
  AssertFalse('Not favorite', FRadio.IsFavorite('http://notfav.com'));
end;

procedure TTestRadioManager.Test_RemoveFromFavorites_DecreasesCount;
var
  Station: TRadioStation;
begin
  Station := CreateTestStation('Fav', 'http://fav.com', 'Rock');
  FRadio.AddToFavorites(Station);
  AssertEquals('Count before', 1, FRadio.FavoritesCount);
  FRadio.RemoveFromFavorites(0);
  AssertEquals('Count after', 0, FRadio.FavoritesCount);
end;

procedure TTestRadioManager.Test_RemoveFromFavorites_InvalidIndex_NoChange;
var
  Station: TRadioStation;
begin
  Station := CreateTestStation('Fav', 'http://fav.com', 'Rock');
  FRadio.AddToFavorites(Station);
  FRadio.RemoveFromFavorites(100);
  FRadio.RemoveFromFavorites(-1);
  AssertEquals('Count unchanged', 1, FRadio.FavoritesCount);
end;

procedure TTestRadioManager.Test_ToggleFavorite_AddsIfNotFavorite;
var
  Station: TRadioStation;
begin
  Station := CreateTestStation('Toggle', 'http://toggle.com', 'Rock');
  FRadio.ToggleFavorite(Station);
  AssertTrue('Added to favorites', FRadio.IsFavorite('http://toggle.com'));
end;

procedure TTestRadioManager.Test_ToggleFavorite_RemovesIfFavorite;
var
  Station: TRadioStation;
begin
  Station := CreateTestStation('Toggle', 'http://toggle.com', 'Rock');
  FRadio.AddToFavorites(Station);
  FRadio.ToggleFavorite(Station);
  AssertFalse('Removed from favorites', FRadio.IsFavorite('http://toggle.com'));
end;

procedure TTestRadioManager.Test_GetFavorite_ValidIndex;
var
  Station: TRadioStation;
begin
  Station := CreateTestStation('Fav', 'http://fav.com', 'Rock');
  FRadio.AddToFavorites(Station);
  AssertEquals('Get favorite name', 'Fav', FRadio.GetFavorite(0).Name);
end;

procedure TTestRadioManager.Test_GetFavorite_InvalidIndex_ReturnsDefault;
begin
  AssertEquals('Empty for invalid', '', FRadio.GetFavorite(0).Name);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Filtering Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestRadioManager.Test_FilterByGenre_FiltersCorrectly;
begin
  FRadio.LoadDefaultStations;
  FRadio.FilterByGenre('Classical');
  AssertTrue('Some classical found', FRadio.FilteredCount > 0);
  AssertTrue('Filtered has Classical', Pos('Classical', FRadio.FilteredStations[0].Genre) > 0);
end;

procedure TTestRadioManager.Test_FilterByGenre_CaseInsensitive;
begin
  FRadio.LoadDefaultStations;
  FRadio.FilterByGenre('classical');
  AssertTrue('Case insensitive match', FRadio.FilteredCount > 0);
end;

procedure TTestRadioManager.Test_FilterByGenre_Empty_ClearsFilter;
begin
  FRadio.LoadDefaultStations;
  FRadio.FilterByGenre('Classical');
  FRadio.FilterByGenre('');
  AssertEquals('All stations shown', FRadio.Count, FRadio.FilteredCount);
end;

procedure TTestRadioManager.Test_FilterByName_MatchesName;
begin
  FRadio.LoadDefaultStations;
  FRadio.FilterByName('France');
  AssertTrue('France stations found', FRadio.FilteredCount > 0);
end;

procedure TTestRadioManager.Test_FilterByName_MatchesGenre;
begin
  FRadio.LoadDefaultStations;
  FRadio.FilterByName('Talk');
  AssertTrue('Talk genre found', FRadio.FilteredCount > 0);
end;

procedure TTestRadioManager.Test_FilterByName_CaseInsensitive;
begin
  FRadio.LoadDefaultStations;
  FRadio.FilterByName('france');
  AssertTrue('Case insensitive', FRadio.FilteredCount > 0);
end;

procedure TTestRadioManager.Test_FilterByCountry_FiltersCorrectly;
begin
  FRadio.LoadDefaultStations;
  FRadio.FilterByCountry('France');
  AssertEquals('All default are France', 10, FRadio.FilteredCount);
end;

procedure TTestRadioManager.Test_FilterFavorites_ShowsOnlyFavorites;
var
  Station: TRadioStation;
begin
  FRadio.LoadDefaultStations;
  Station := FRadio.Stations[0];
  FRadio.AddToFavorites(Station);
  FRadio.FilterFavorites;
  AssertEquals('Only favorites shown', 1, FRadio.FilteredCount);
end;

procedure TTestRadioManager.Test_ClearFilter_ShowsAll;
begin
  FRadio.LoadDefaultStations;
  FRadio.FilterByGenre('Classical');
  FRadio.ClearFilter;
  AssertEquals('All stations after clear', FRadio.Count, FRadio.FilteredCount);
end;

procedure TTestRadioManager.Test_Filter_IncludesCustomStations;
begin
  FRadio.LoadDefaultStations;
  FRadio.AddCustomStation('Custom Rock', 'http://custom.com', 'Rock', 128);
  FRadio.FilterByGenre('Rock');
  AssertTrue('Custom included', FRadio.FilteredCount > 0);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Search Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestRadioManager.Test_Search_MatchesName;
var
  Results: TRadioStations;
begin
  FRadio.LoadDefaultStations;
  Results := FRadio.Search('France Inter');
  AssertTrue('Found France Inter', Length(Results) > 0);
  AssertEquals('Correct name', 'France Inter', Results[0].Name);
end;

procedure TTestRadioManager.Test_Search_MatchesGenre;
var
  Results: TRadioStations;
begin
  FRadio.LoadDefaultStations;
  Results := FRadio.Search('Classical');
  AssertTrue('Found classical', Length(Results) > 0);
end;

procedure TTestRadioManager.Test_Search_MatchesCountry;
var
  Results: TRadioStations;
begin
  FRadio.LoadDefaultStations;
  Results := FRadio.Search('France');
  AssertEquals('All 10 match France', 10, Length(Results));
end;

procedure TTestRadioManager.Test_Search_CaseInsensitive;
var
  Results: TRadioStations;
begin
  FRadio.LoadDefaultStations;
  Results := FRadio.Search('france inter');
  AssertTrue('Case insensitive', Length(Results) > 0);
end;

procedure TTestRadioManager.Test_Search_EmptyReturnsEmpty;
var
  Results: TRadioStations;
begin
  FRadio.LoadDefaultStations;
  Results := FRadio.Search('');
  AssertEquals('Empty search', 0, Length(Results));
end;

procedure TTestRadioManager.Test_Search_NoMatchReturnsEmpty;
var
  Results: TRadioStations;
begin
  FRadio.LoadDefaultStations;
  Results := FRadio.Search('XYZ123NOTFOUND');
  AssertEquals('No match', 0, Length(Results));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  LoadFromXML Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestRadioManager.Test_LoadFromXML_EmptyReturnsError;
var
  Res: Boolean;
begin
  Res := FRadio.LoadFromXML('');
  AssertFalse('Empty XML fails', Res);
  AssertTrue('Error set', FRadio.LastError <> '');
end;

procedure TTestRadioManager.Test_LoadFromXML_ValidXML;
var
  Res: Boolean;
begin
  Res := FRadio.LoadFromXML(SAMPLE_ICECAST_XML);
  AssertTrue('Valid XML succeeds', Res);
  AssertEquals('2 valid stations', 2, FRadio.Count);
end;

procedure TTestRadioManager.Test_LoadFromXML_ExtractsGenres;
begin
  FRadio.LoadFromXML(SAMPLE_ICECAST_XML);
  AssertTrue('Genres extracted', FRadio.Genres.Count > 0);
  AssertTrue('Rock genre', FRadio.Genres.IndexOf('Rock') >= 0);
  AssertTrue('Jazz genre', FRadio.Genres.IndexOf('Jazz') >= 0);
end;

procedure TTestRadioManager.Test_LoadFromXML_SkipsEmptyURL;
begin
  FRadio.LoadFromXML(SAMPLE_ICECAST_XML);
  { XML has 3 entries but one has no URL }
  AssertEquals('Skipped empty URL', 2, FRadio.Count);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Clear Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestRadioManager.Test_Clear_EmptiesStations;
begin
  FRadio.LoadDefaultStations;
  FRadio.Clear;
  AssertEquals('Stations cleared', 0, FRadio.Count);
end;

procedure TTestRadioManager.Test_Clear_EmptiesFiltered;
begin
  FRadio.LoadDefaultStations;
  FRadio.Clear;
  AssertEquals('Filtered cleared', 0, FRadio.FilteredCount);
end;

procedure TTestRadioManager.Test_Clear_EmptiesGenres;
begin
  FRadio.LoadDefaultStations;
  FRadio.Clear;
  AssertEquals('Genres cleared', 0, FRadio.Genres.Count);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Events Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestRadioManager.Test_OnChange_TriggeredOnAddCustom;
begin
  FRadio.OnChange := @OnChangeHandler;
  FRadio.AddCustomStation('Test', 'http://test.com', 'Rock', 128);
  AssertTrue('OnChange triggered', FChangeCount > 0);
end;

procedure TTestRadioManager.Test_OnChange_TriggeredOnFilter;
begin
  FRadio.LoadDefaultStations;
  FChangeCount := 0;
  FRadio.OnChange := @OnChangeHandler;
  FRadio.FilterByGenre('Classical');
  AssertTrue('OnChange on filter', FChangeCount > 0);
end;

procedure TTestRadioManager.Test_OnLoad_TriggeredOnLoadDefault;
begin
  FRadio.OnLoad := @OnLoadHandler;
  FRadio.LoadDefaultStations;
  AssertEquals('OnLoad triggered once', 1, FLoadCount);
end;

procedure TTestRadioManager.Test_OnLoad_ReceivesCount;
begin
  FRadio.OnLoad := @OnLoadHandler;
  FRadio.LoadDefaultStations;
  AssertEquals('OnLoad received count', 10, FLastLoadedCount);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Constants Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestRadioManager.Test_IcecastURL_Defined;
begin
  AssertTrue('Icecast URL defined', ICECAST_DIRECTORY_URL <> '');
  AssertTrue('Icecast URL is HTTPS', Pos('https://', ICECAST_DIRECTORY_URL) = 1);
end;

procedure TTestRadioManager.Test_DefaultStations_Count;
begin
  AssertEquals('10 default stations defined', 10, Length(DEFAULT_STATIONS));
end;

initialization
  RegisterTest(TTestRadioManager);

end.
