{ ═══════════════════════════════════════════════════════════════════════════════
  uRadioManager.pas - Radio Stations Manager

  Part of 3nity Media - Lazarus Edition

  This unit implements radio station management with support for:
  - Icecast directory XML parsing
  - Custom station management
  - Genre filtering and search
  - Favorites management
  - Station persistence

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uRadioManager;

{$mode objfpc}{$H+}
{$WARN 4105 OFF} // UnicodeString to AnsiString: safe on UTF-8 systems

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, DOM, XMLRead, uTypes;

type
  { ═══════════════════════════════════════════════════════════════════════════
    RADIO MANAGER EVENTS
    ═══════════════════════════════════════════════════════════════════════════ }
  TRadioLoadEvent = procedure(Sender: TObject; Count: Integer) of object;
  TRadioErrorEvent = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TRadioPlayEvent = procedure(Sender: TObject; const Station: TRadioStation) of object;

  { ═══════════════════════════════════════════════════════════════════════════
    RADIO MANAGER CLASS
    ═══════════════════════════════════════════════════════════════════════════ }
  TRadioManager = class
  private
    FStations: TRadioStations;
    FCustomStations: TRadioStations;
    FFavorites: TRadioStations;
    FFilteredStations: TRadioStations;
    FGenres: TStringList;
    FLastError: string;
    FLoading: Boolean;
    FModified: Boolean;

    // Events
    FOnLoad: TRadioLoadEvent;
    FOnError: TRadioErrorEvent;
    FOnPlay: TRadioPlayEvent;
    FOnChange: TNotifyEvent;

    function GetCount: Integer;
    function GetFilteredCount: Integer;
    function GetStation(Index: Integer): TRadioStation;
    function GetFilteredStation(Index: Integer): TRadioStation;
    function GetCustomCount: Integer;
    function GetFavoritesCount: Integer;

    procedure ParseIcecastXML(const XMLContent: string);
    procedure ParseRadioBrowserJSON(const JSONContent: string);
    procedure ExtractGenres;
    procedure DoChange;

  public
    constructor Create;
    destructor Destroy; override;

    { ═══════════════════════════════════════════════════════════════════════════
      LOADING STATIONS
      ═══════════════════════════════════════════════════════════════════════════ }
    function LoadFromIcecast(const URL: string = ''): Boolean;
    function LoadFromFile(const FileName: string): Boolean;
    function LoadFromXML(const XMLContent: string): Boolean;
    procedure LoadDefaultStations;
    procedure Clear;

    { ═══════════════════════════════════════════════════════════════════════════
      FILTERING AND SEARCH
      ═══════════════════════════════════════════════════════════════════════════ }
    procedure FilterByGenre(const Genre: string);
    procedure FilterByName(const SearchText: string);
    procedure FilterByCountry(const Country: string);
    procedure FilterFavorites;
    procedure ClearFilter;
    function Search(const Text: string): TRadioStations;

    { ═══════════════════════════════════════════════════════════════════════════
      CUSTOM STATIONS
      ═══════════════════════════════════════════════════════════════════════════ }
    function AddCustomStation(const AName, AURL, AGenre: string; ABitrate: Integer = 0): Integer;
    procedure EditCustomStation(Index: Integer; const AName, AURL, AGenre: string; ABitrate: Integer);
    procedure DeleteCustomStation(Index: Integer);
    function GetCustomStation(Index: Integer): TRadioStation;

    { ═══════════════════════════════════════════════════════════════════════════
      FAVORITES
      ═══════════════════════════════════════════════════════════════════════════ }
    procedure AddToFavorites(const Station: TRadioStation);
    procedure RemoveFromFavorites(Index: Integer);
    function IsFavorite(const AURL: string): Boolean;
    procedure ToggleFavorite(const Station: TRadioStation);
    function GetFavorite(Index: Integer): TRadioStation;

    { ═══════════════════════════════════════════════════════════════════════════
      PERSISTENCE
      ═══════════════════════════════════════════════════════════════════════════ }
    function SaveCustomStations(const FileName: string): Boolean;
    function LoadCustomStations(const FileName: string): Boolean;
    function SaveFavorites(const FileName: string): Boolean;
    function LoadFavorites(const FileName: string): Boolean;

    { ═══════════════════════════════════════════════════════════════════════════
      PLAY
      ═══════════════════════════════════════════════════════════════════════════ }
    procedure PlayStation(Index: Integer);
    procedure PlayFilteredStation(Index: Integer);

    { ═══════════════════════════════════════════════════════════════════════════
      PROPERTIES
      ═══════════════════════════════════════════════════════════════════════════ }
    property Count: Integer read GetCount;
    property FilteredCount: Integer read GetFilteredCount;
    property CustomCount: Integer read GetCustomCount;
    property FavoritesCount: Integer read GetFavoritesCount;
    property Stations[Index: Integer]: TRadioStation read GetStation;
    property FilteredStations[Index: Integer]: TRadioStation read GetFilteredStation;
    property Genres: TStringList read FGenres;
    property LastError: string read FLastError;
    property Loading: Boolean read FLoading;
    property Modified: Boolean read FModified;

    { ═══════════════════════════════════════════════════════════════════════════
      EVENTS
      ═══════════════════════════════════════════════════════════════════════════ }
    property OnLoad: TRadioLoadEvent read FOnLoad write FOnLoad;
    property OnError: TRadioErrorEvent read FOnError write FOnError;
    property OnPlay: TRadioPlayEvent read FOnPlay write FOnPlay;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

const
  { Default Icecast directory URL }
  ICECAST_DIRECTORY_URL = 'https://dir.xiph.org/yp.xml';

  { Default radio stations }
  DEFAULT_STATIONS: array[0..9] of record
    Name: string;
    URL: string;
    Genre: string;
    Bitrate: Integer;
  end = (
    (Name: 'France Inter'; URL: 'http://icecast.radiofrance.fr/franceinter-midfi.mp3'; Genre: 'Talk/News'; Bitrate: 128),
    (Name: 'France Info'; URL: 'http://icecast.radiofrance.fr/franceinfo-midfi.mp3'; Genre: 'News'; Bitrate: 128),
    (Name: 'France Culture'; URL: 'http://icecast.radiofrance.fr/franceculture-midfi.mp3'; Genre: 'Culture'; Bitrate: 128),
    (Name: 'France Musique'; URL: 'http://icecast.radiofrance.fr/francemusique-midfi.mp3'; Genre: 'Classical'; Bitrate: 128),
    (Name: 'FIP'; URL: 'http://icecast.radiofrance.fr/fip-midfi.mp3'; Genre: 'Eclectic'; Bitrate: 128),
    (Name: 'Mouv'; URL: 'http://icecast.radiofrance.fr/mouv-midfi.mp3'; Genre: 'Urban'; Bitrate: 128),
    (Name: 'RTL'; URL: 'http://streaming.radio.rtl.fr/rtl-1-44-128'; Genre: 'Talk'; Bitrate: 128),
    (Name: 'Europe 1'; URL: 'http://ais-live.cloud-services.paris:8000/europe1.mp3'; Genre: 'Talk/News'; Bitrate: 128),
    (Name: 'RMC'; URL: 'http://rmc.bfmtv.com/rmcinfo-mp3'; Genre: 'Talk/Sport'; Bitrate: 128),
    (Name: 'NRJ'; URL: 'http://cdn.nrjaudio.fm/audio1/fr/30001/mp3_128.mp3'; Genre: 'Pop/Hits'; Bitrate: 128)
  );

implementation

uses
  StrUtils;

{ ═══════════════════════════════════════════════════════════════════════════════
  CONSTRUCTOR / DESTRUCTOR
  ═══════════════════════════════════════════════════════════════════════════════ }

constructor TRadioManager.Create;
begin
  inherited Create;
  SetLength(FStations, 0);
  SetLength(FCustomStations, 0);
  SetLength(FFavorites, 0);
  SetLength(FFilteredStations, 0);
  FGenres := TStringList.Create;
  FGenres.Sorted := True;
  FGenres.Duplicates := dupIgnore;
  FLastError := '';
  FLoading := False;
  FModified := False;
end;

destructor TRadioManager.Destroy;
begin
  FGenres.Free;
  SetLength(FStations, 0);
  SetLength(FCustomStations, 0);
  SetLength(FFavorites, 0);
  SetLength(FFilteredStations, 0);
  inherited Destroy;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PRIVATE METHODS
  ═══════════════════════════════════════════════════════════════════════════════ }

function TRadioManager.GetCount: Integer;
begin
  Result := Length(FStations);
end;

function TRadioManager.GetFilteredCount: Integer;
begin
  Result := Length(FFilteredStations);
end;

function TRadioManager.GetStation(Index: Integer): TRadioStation;
begin
  if (Index >= 0) and (Index < Length(FStations)) then
    Result := FStations[Index]
  else
    Result := Default(TRadioStation);
end;

function TRadioManager.GetFilteredStation(Index: Integer): TRadioStation;
begin
  if (Index >= 0) and (Index < Length(FFilteredStations)) then
    Result := FFilteredStations[Index]
  else
    Result := Default(TRadioStation);
end;

function TRadioManager.GetCustomCount: Integer;
begin
  Result := Length(FCustomStations);
end;

function TRadioManager.GetFavoritesCount: Integer;
begin
  Result := Length(FFavorites);
end;

procedure TRadioManager.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ ═══════════════════════════════════════════════════════════════════════════
  ParseIcecastXML - Parse Icecast directory XML into station list

  Purpose: Parses XML content from Icecast directory servers (yp.xml format)
           and populates the internal station list.

  Parameters:
    - XMLContent: Raw XML string from Icecast directory

  XML Format expected:
    <icestats>
      <entry>
        <server_name>Station Name</server_name>
        <listen_url>http://stream.url:port/mount</listen_url>
        <genre>Genre Name</genre>
        <bitrate>128</bitrate>
        <server_type>audio/mpeg</server_type>
        <channels>2</channels>
        <samplerate>44100</samplerate>
      </entry>
      ...
    </icestats>

  Algorithm:
    1. Parse XML into DOM document
    2. Find all <entry> elements
    3. Extract fields from child nodes (case-insensitive matching)
    4. Compact array: remove entries without valid URL (in-place)

  Notes:
    - Uses DOM parser from Lazarus XML units
    - Entries without URL are filtered out during compaction phase
    - Stations with empty name default to "Unknown Station"
    - Errors are reported via FOnError callback
  ═══════════════════════════════════════════════════════════════════════════ }
procedure TRadioManager.ParseIcecastXML(const XMLContent: string);
var
  Doc: TXMLDocument;
  Stream: TStringStream;
  EntryList: TDOMNodeList;
  Entry, Child: TDOMNode;
  I, J: Integer;
  Station: TRadioStation;
  NodeName, NodeValue: string;
begin
  Doc := nil;
  Stream := TStringStream.Create(XMLContent);
  try
    try
      ReadXMLFile(Doc, Stream);

      { Find all <entry> elements }
      EntryList := Doc.GetElementsByTagName('entry');

      SetLength(FStations, EntryList.Count);

      for I := 0 to EntryList.Count - 1 do
      begin
        Entry := EntryList.Item[I];
        Station := Default(TRadioStation);

        { Parse child elements }
        for J := 0 to Entry.ChildNodes.Count - 1 do
        begin
          Child := Entry.ChildNodes.Item[J];
          NodeName := LowerCase(Child.NodeName);

          if Child.FirstChild <> nil then
            NodeValue := Child.FirstChild.NodeValue
          else
            NodeValue := '';

          if NodeName = 'server_name' then
            Station.Name := NodeValue
          else if NodeName = 'listen_url' then
            Station.URL := NodeValue
          else if NodeName = 'genre' then
            Station.Genre := NodeValue
          else if NodeName = 'bitrate' then
            Station.Bitrate := StrToIntDef(NodeValue, 0)
          else if NodeName = 'server_type' then
            Station.Codec := NodeValue
          else if NodeName = 'channels' then
            Station.Channels := StrToIntDef(NodeValue, 0)
          else if NodeName = 'samplerate' then
            Station.SampleRate := StrToIntDef(NodeValue, 0);
        end;

        { Only add if we have a valid URL }
        if Station.URL <> '' then
        begin
          if Station.Name = '' then
            Station.Name := 'Unknown Station';
          FStations[I] := Station;
        end;
      end;

      { Remove empty entries }
      J := 0;
      for I := 0 to High(FStations) do
      begin
        if FStations[I].URL <> '' then
        begin
          if I <> J then
            FStations[J] := FStations[I];
          Inc(J);
        end;
      end;
      SetLength(FStations, J);

    except
      on E: Exception do
      begin
        FLastError := 'XML Parse Error: ' + E.Message;
        if Assigned(FOnError) then
          FOnError(Self, FLastError);
      end;
    end;
  finally
    if Doc <> nil then
      Doc.Free;
    Stream.Free;
  end;
end;

procedure TRadioManager.ParseRadioBrowserJSON(const JSONContent: string);
begin
  { JSON parsing for radio-browser.info API - placeholder for future implementation }
  { This could use fpjson unit for parsing }
end;

procedure TRadioManager.ExtractGenres;
var
  I: Integer;
  GenreList: TStringList;
  Genre: string;
begin
  FGenres.Clear;
  GenreList := TStringList.Create;
  try
    GenreList.Delimiter := ',';
    GenreList.StrictDelimiter := True;

    for I := 0 to High(FStations) do
    begin
      if FStations[I].Genre <> '' then
      begin
        { Handle multiple genres separated by comma or slash }
        Genre := StringReplace(FStations[I].Genre, '/', ',', [rfReplaceAll]);
        GenreList.DelimitedText := Genre;

        for Genre in GenreList do
        begin
          if Trim(Genre) <> '' then
            FGenres.Add(Trim(Genre));
        end;
      end;
    end;

    { Add custom station genres }
    for I := 0 to High(FCustomStations) do
    begin
      if FCustomStations[I].Genre <> '' then
        FGenres.Add(FCustomStations[I].Genre);
    end;
  finally
    GenreList.Free;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  LOADING STATIONS
  ═══════════════════════════════════════════════════════════════════════════════ }

function TRadioManager.LoadFromIcecast(const URL: string): Boolean;
var
  HTTPClient: TFPHTTPClient;
  Response: string;
  ActualURL: string;
begin
  Result := False;
  FLoading := True;
  FLastError := '';

  if URL = '' then
    ActualURL := ICECAST_DIRECTORY_URL
  else
    ActualURL := URL;

  HTTPClient := TFPHTTPClient.Create(nil);
  try
    try
      HTTPClient.AllowRedirect := True;
      HTTPClient.ConnectTimeout := 30000;
      HTTPClient.IOTimeout := 60000;

      Response := HTTPClient.Get(ActualURL);

      if Response <> '' then
      begin
        ParseIcecastXML(Response);
        ExtractGenres;
        ClearFilter;
        Result := Length(FStations) > 0;

        if Result and Assigned(FOnLoad) then
          FOnLoad(Self, Length(FStations));
      end
      else
      begin
        FLastError := 'Empty response from server';
        if Assigned(FOnError) then
          FOnError(Self, FLastError);
      end;

    except
      on E: Exception do
      begin
        FLastError := 'Network Error: ' + E.Message;
        if Assigned(FOnError) then
          FOnError(Self, FLastError);
      end;
    end;
  finally
    HTTPClient.Free;
    FLoading := False;
  end;
end;

function TRadioManager.LoadFromFile(const FileName: string): Boolean;
var
  SL: TStringList;
begin
  Result := False;
  FLastError := '';

  if not FileExists(FileName) then
  begin
    FLastError := 'File not found: ' + FileName;
    Exit;
  end;

  SL := TStringList.Create;
  try
    try
      SL.LoadFromFile(FileName);
      Result := LoadFromXML(SL.Text);
    except
      on E: Exception do
      begin
        FLastError := 'File Error: ' + E.Message;
        if Assigned(FOnError) then
          FOnError(Self, FLastError);
      end;
    end;
  finally
    SL.Free;
  end;
end;

function TRadioManager.LoadFromXML(const XMLContent: string): Boolean;
begin
  Result := False;
  FLastError := '';

  if XMLContent = '' then
  begin
    FLastError := 'Empty XML content';
    Exit;
  end;

  ParseIcecastXML(XMLContent);
  ExtractGenres;
  ClearFilter;
  Result := Length(FStations) > 0;

  if Result and Assigned(FOnLoad) then
    FOnLoad(Self, Length(FStations));
end;

procedure TRadioManager.LoadDefaultStations;
var
  I: Integer;
begin
  SetLength(FStations, Length(DEFAULT_STATIONS));

  for I := 0 to High(DEFAULT_STATIONS) do
  begin
    FStations[I] := Default(TRadioStation);
    FStations[I].Name := DEFAULT_STATIONS[I].Name;
    FStations[I].URL := DEFAULT_STATIONS[I].URL;
    FStations[I].Genre := DEFAULT_STATIONS[I].Genre;
    FStations[I].Bitrate := DEFAULT_STATIONS[I].Bitrate;
    FStations[I].Country := 'France';
    FStations[I].Language := 'French';
  end;

  ExtractGenres;
  ClearFilter;

  if Assigned(FOnLoad) then
    FOnLoad(Self, Length(FStations));

  DoChange;
end;

procedure TRadioManager.Clear;
begin
  SetLength(FStations, 0);
  SetLength(FFilteredStations, 0);
  FGenres.Clear;
  DoChange;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  FILTERING AND SEARCH
  ═══════════════════════════════════════════════════════════════════════════════ }

{ ═══════════════════════════════════════════════════════════════════════════
  FilterByGenre / FilterByName / FilterByCountry - Filter station list

  Purpose: These three functions filter the station list by different criteria.
           They all follow the same pattern and populate FFilteredStations.

  Common Algorithm:
    1. If search text is empty, call ClearFilter and return all stations
    2. Pre-allocate result array to max possible size (FStations + FCustomStations)
    3. Iterate both FStations and FCustomStations arrays
    4. Use case-insensitive substring matching (Pos + LowerCase)
    5. Compact result array to actual match count
    6. Trigger OnChange event

  Matching behavior:
    - FilterByGenre: Matches genre field only
    - FilterByName: Matches name OR genre (broader search)
    - FilterByCountry: Matches country field only

  Notes:
    - Uses Pos() for substring matching, not exact match
    - Both built-in and custom stations are searched
    - Pre-allocation + compaction avoids O(n) reallocations
  ═══════════════════════════════════════════════════════════════════════════ }
procedure TRadioManager.FilterByGenre(const Genre: string);
var
  I, FilterCount: Integer;
  GenreLower: string;
begin
  if Genre = '' then
  begin
    ClearFilter;
    Exit;
  end;

  GenreLower := LowerCase(Genre);
  SetLength(FFilteredStations, Length(FStations) + Length(FCustomStations));
  FilterCount := 0;

  { Filter main stations }
  for I := 0 to High(FStations) do
  begin
    if Pos(GenreLower, LowerCase(FStations[I].Genre)) > 0 then
    begin
      FFilteredStations[FilterCount] := FStations[I];
      Inc(FilterCount);
    end;
  end;

  { Filter custom stations }
  for I := 0 to High(FCustomStations) do
  begin
    if Pos(GenreLower, LowerCase(FCustomStations[I].Genre)) > 0 then
    begin
      FFilteredStations[FilterCount] := FCustomStations[I];
      Inc(FilterCount);
    end;
  end;

  SetLength(FFilteredStations, FilterCount);
  DoChange;
end;

procedure TRadioManager.FilterByName(const SearchText: string);
var
  I, FilterCount: Integer;
  SearchLower: string;
begin
  if SearchText = '' then
  begin
    ClearFilter;
    Exit;
  end;

  SearchLower := LowerCase(SearchText);
  SetLength(FFilteredStations, Length(FStations) + Length(FCustomStations));
  FilterCount := 0;

  { Filter main stations }
  for I := 0 to High(FStations) do
  begin
    if (Pos(SearchLower, LowerCase(FStations[I].Name)) > 0) or
       (Pos(SearchLower, LowerCase(FStations[I].Genre)) > 0) then
    begin
      FFilteredStations[FilterCount] := FStations[I];
      Inc(FilterCount);
    end;
  end;

  { Filter custom stations }
  for I := 0 to High(FCustomStations) do
  begin
    if (Pos(SearchLower, LowerCase(FCustomStations[I].Name)) > 0) or
       (Pos(SearchLower, LowerCase(FCustomStations[I].Genre)) > 0) then
    begin
      FFilteredStations[FilterCount] := FCustomStations[I];
      Inc(FilterCount);
    end;
  end;

  SetLength(FFilteredStations, FilterCount);
  DoChange;
end;

procedure TRadioManager.FilterByCountry(const Country: string);
var
  I, FilterCount: Integer;
  CountryLower: string;
begin
  if Country = '' then
  begin
    ClearFilter;
    Exit;
  end;

  CountryLower := LowerCase(Country);
  SetLength(FFilteredStations, Length(FStations) + Length(FCustomStations));
  FilterCount := 0;

  for I := 0 to High(FStations) do
  begin
    if Pos(CountryLower, LowerCase(FStations[I].Country)) > 0 then
    begin
      FFilteredStations[FilterCount] := FStations[I];
      Inc(FilterCount);
    end;
  end;

  for I := 0 to High(FCustomStations) do
  begin
    if Pos(CountryLower, LowerCase(FCustomStations[I].Country)) > 0 then
    begin
      FFilteredStations[FilterCount] := FCustomStations[I];
      Inc(FilterCount);
    end;
  end;

  SetLength(FFilteredStations, FilterCount);
  DoChange;
end;

procedure TRadioManager.FilterFavorites;
begin
  SetLength(FFilteredStations, Length(FFavorites));
  if Length(FFavorites) > 0 then
    Move(FFavorites[0], FFilteredStations[0], Length(FFavorites) * SizeOf(TRadioStation));
  DoChange;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  ClearFilter - Reset filter to show all stations

  Purpose: Clears any active filter and repopulates FFilteredStations with
           all available stations (both built-in and custom).

  Algorithm:
    1. Calculate total count (FStations + FCustomStations)
    2. Copy all built-in stations to FFilteredStations[0..N-1]
    3. Append custom stations at FFilteredStations[N..Total-1]
    4. Trigger OnChange event to update UI

  Notes:
    - Called automatically when filter text is empty in FilterBy* methods
    - Maintains station order: built-in first, then custom
    - Does not affect the source arrays (FStations, FCustomStations)
  ═══════════════════════════════════════════════════════════════════════════ }
procedure TRadioManager.ClearFilter;
var
  I, TotalCount: Integer;
begin
  TotalCount := Length(FStations) + Length(FCustomStations);
  SetLength(FFilteredStations, TotalCount);

  { Copy all stations }
  for I := 0 to High(FStations) do
    FFilteredStations[I] := FStations[I];

  { Append custom stations }
  for I := 0 to High(FCustomStations) do
    FFilteredStations[Length(FStations) + I] := FCustomStations[I];

  DoChange;
end;

function TRadioManager.Search(const Text: string): TRadioStations;
var
  I, ResultCount: Integer;
  SearchLower: string;
begin
  Result := nil;
  if Text = '' then Exit;

  SearchLower := LowerCase(Text);
  SetLength(Result, Length(FStations) + Length(FCustomStations));
  ResultCount := 0;

  for I := 0 to High(FStations) do
  begin
    if (Pos(SearchLower, LowerCase(FStations[I].Name)) > 0) or
       (Pos(SearchLower, LowerCase(FStations[I].Genre)) > 0) or
       (Pos(SearchLower, LowerCase(FStations[I].Country)) > 0) then
    begin
      Result[ResultCount] := FStations[I];
      Inc(ResultCount);
    end;
  end;

  for I := 0 to High(FCustomStations) do
  begin
    if (Pos(SearchLower, LowerCase(FCustomStations[I].Name)) > 0) or
       (Pos(SearchLower, LowerCase(FCustomStations[I].Genre)) > 0) then
    begin
      Result[ResultCount] := FCustomStations[I];
      Inc(ResultCount);
    end;
  end;

  SetLength(Result, ResultCount);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  CUSTOM STATIONS
  ═══════════════════════════════════════════════════════════════════════════════ }

function TRadioManager.AddCustomStation(const AName, AURL, AGenre: string; ABitrate: Integer): Integer;
var
  Station: TRadioStation;
begin
  Station := Default(TRadioStation);
  Station.Name := AName;
  Station.URL := AURL;
  Station.Genre := AGenre;
  Station.Bitrate := ABitrate;

  SetLength(FCustomStations, Length(FCustomStations) + 1);
  FCustomStations[High(FCustomStations)] := Station;

  Result := High(FCustomStations);
  FModified := True;

  ExtractGenres;
  ClearFilter;
  DoChange;
end;

procedure TRadioManager.EditCustomStation(Index: Integer; const AName, AURL, AGenre: string; ABitrate: Integer);
begin
  if (Index >= 0) and (Index < Length(FCustomStations)) then
  begin
    FCustomStations[Index].Name := AName;
    FCustomStations[Index].URL := AURL;
    FCustomStations[Index].Genre := AGenre;
    FCustomStations[Index].Bitrate := ABitrate;
    FModified := True;

    ExtractGenres;
    ClearFilter;
    DoChange;
  end;
end;

procedure TRadioManager.DeleteCustomStation(Index: Integer);
var
  I: Integer;
begin
  if (Index >= 0) and (Index < Length(FCustomStations)) then
  begin
    for I := Index to High(FCustomStations) - 1 do
      FCustomStations[I] := FCustomStations[I + 1];
    SetLength(FCustomStations, Length(FCustomStations) - 1);
    FModified := True;

    ExtractGenres;
    ClearFilter;
    DoChange;
  end;
end;

function TRadioManager.GetCustomStation(Index: Integer): TRadioStation;
begin
  if (Index >= 0) and (Index < Length(FCustomStations)) then
    Result := FCustomStations[Index]
  else
    Result := Default(TRadioStation);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  FAVORITES
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TRadioManager.AddToFavorites(const Station: TRadioStation);
begin
  if not IsFavorite(Station.URL) then
  begin
    SetLength(FFavorites, Length(FFavorites) + 1);
    FFavorites[High(FFavorites)] := Station;
    FFavorites[High(FFavorites)].Favorite := True;
    FModified := True;
    DoChange;
  end;
end;

procedure TRadioManager.RemoveFromFavorites(Index: Integer);
var
  I: Integer;
begin
  if (Index >= 0) and (Index < Length(FFavorites)) then
  begin
    for I := Index to High(FFavorites) - 1 do
      FFavorites[I] := FFavorites[I + 1];
    SetLength(FFavorites, Length(FFavorites) - 1);
    FModified := True;
    DoChange;
  end;
end;

function TRadioManager.IsFavorite(const AURL: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(FFavorites) do
  begin
    if FFavorites[I].URL = AURL then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TRadioManager.ToggleFavorite(const Station: TRadioStation);
var
  I: Integer;
begin
  for I := 0 to High(FFavorites) do
  begin
    if FFavorites[I].URL = Station.URL then
    begin
      RemoveFromFavorites(I);
      Exit;
    end;
  end;
  AddToFavorites(Station);
end;

function TRadioManager.GetFavorite(Index: Integer): TRadioStation;
begin
  if (Index >= 0) and (Index < Length(FFavorites)) then
    Result := FFavorites[Index]
  else
    Result := Default(TRadioStation);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PERSISTENCE
  ═══════════════════════════════════════════════════════════════════════════════ }

function TRadioManager.SaveCustomStations(const FileName: string): Boolean;
var
  SL: TStringList;
  I: Integer;
begin
  Result := False;
  SL := TStringList.Create;
  try
    try
      SL.Add('[CustomStations]');
      SL.Add('Count=' + IntToStr(Length(FCustomStations)));

      for I := 0 to High(FCustomStations) do
      begin
        SL.Add('');
        SL.Add('[Station' + IntToStr(I) + ']');
        SL.Add('Name=' + FCustomStations[I].Name);
        SL.Add('URL=' + FCustomStations[I].URL);
        SL.Add('Genre=' + FCustomStations[I].Genre);
        SL.Add('Bitrate=' + IntToStr(FCustomStations[I].Bitrate));
        SL.Add('Country=' + FCustomStations[I].Country);
        SL.Add('Language=' + FCustomStations[I].Language);
      end;

      SL.SaveToFile(FileName);
      FModified := False;
      Result := True;
    except
      on E: Exception do
        FLastError := 'Save Error: ' + E.Message;
    end;
  finally
    SL.Free;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  LoadCustomStations - Load user-defined radio stations from INI-like file

  Purpose: Loads custom stations that users have manually added. These are
           stored separately from the built-in Icecast directory stations.

  Parameters:
    - FileName: Path to the custom stations file

  Returns: True if load succeeded, False otherwise

  File Format (INI-like):
    [CustomStations]
    Count=2

    [Station0]
    Name=My Station
    URL=http://stream.example.com:8000/stream
    Genre=Rock
    Bitrate=128
    Country=USA
    Language=English

  Notes:
    - Uses TStringList for simple key=value parsing
    - Nested GetValue function searches for keys in the loaded content
    - After loading, calls ExtractGenres to update genre list
    - Calls ClearFilter to refresh the filtered view with new stations
    - Errors stored in FLastError property
  ═══════════════════════════════════════════════════════════════════════════ }
function TRadioManager.LoadCustomStations(const FileName: string): Boolean;
var
  SL: TStringList;
  I, StationCount: Integer;
  Section: string;

  function GetValue(const Key: string): string;
  var
    Idx: Integer;
  begin
    Result := '';
    Idx := SL.IndexOfName(Key);
    if Idx >= 0 then
      Result := SL.ValueFromIndex[Idx];
  end;

begin
  Result := False;

  if not FileExists(FileName) then
    Exit;

  SL := TStringList.Create;
  try
    try
      SL.LoadFromFile(FileName);

      StationCount := StrToIntDef(GetValue('Count'), 0);
      SetLength(FCustomStations, StationCount);

      for I := 0 to StationCount - 1 do
      begin
        Section := '[Station' + IntToStr(I) + ']';
        FCustomStations[I] := Default(TRadioStation);
        FCustomStations[I].Name := GetValue('Name');
        FCustomStations[I].URL := GetValue('URL');
        FCustomStations[I].Genre := GetValue('Genre');
        FCustomStations[I].Bitrate := StrToIntDef(GetValue('Bitrate'), 0);
        FCustomStations[I].Country := GetValue('Country');
        FCustomStations[I].Language := GetValue('Language');
      end;

      ExtractGenres;
      ClearFilter;
      Result := True;
    except
      on E: Exception do
        FLastError := 'Load Error: ' + E.Message;
    end;
  finally
    SL.Free;
  end;
end;

function TRadioManager.SaveFavorites(const FileName: string): Boolean;
var
  SL: TStringList;
  I: Integer;
begin
  Result := False;
  SL := TStringList.Create;
  try
    try
      SL.Add('[Favorites]');
      SL.Add('Count=' + IntToStr(Length(FFavorites)));

      for I := 0 to High(FFavorites) do
      begin
        SL.Add('');
        SL.Add('[Favorite' + IntToStr(I) + ']');
        SL.Add('Name=' + FFavorites[I].Name);
        SL.Add('URL=' + FFavorites[I].URL);
        SL.Add('Genre=' + FFavorites[I].Genre);
        SL.Add('Bitrate=' + IntToStr(FFavorites[I].Bitrate));
        SL.Add('Country=' + FFavorites[I].Country);
        SL.Add('Language=' + FFavorites[I].Language);
      end;

      SL.SaveToFile(FileName);
      Result := True;
    except
      on E: Exception do
        FLastError := 'Save Error: ' + E.Message;
    end;
  finally
    SL.Free;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  LoadFavorites - Load user's favorite radio stations from file

  Purpose: Restores the user's saved favorite stations. Favorites persist
           across sessions, allowing quick access to preferred stations.

  Parameters:
    - FileName: Path to the favorites file

  Returns: True if load succeeded, False otherwise

  File Format: Same INI-like format as custom stations, but with
               [Favorites] section and [Favorite0], [Favorite1], etc.

  Notes:
    - Similar to LoadCustomStations but populates FFavorites array
    - Sets Favorite := True flag on each loaded station
    - Does NOT call ClearFilter (favorites are a separate view)
    - Use FilterFavorites to display only favorite stations
    - Errors stored in FLastError property
  ═══════════════════════════════════════════════════════════════════════════ }
function TRadioManager.LoadFavorites(const FileName: string): Boolean;
var
  SL: TStringList;
  I, FavCount: Integer;

  function GetValue(const Key: string): string;
  var
    Idx: Integer;
  begin
    Result := '';
    Idx := SL.IndexOfName(Key);
    if Idx >= 0 then
      Result := SL.ValueFromIndex[Idx];
  end;

begin
  Result := False;

  if not FileExists(FileName) then
    Exit;

  SL := TStringList.Create;
  try
    try
      SL.LoadFromFile(FileName);

      FavCount := StrToIntDef(GetValue('Count'), 0);
      SetLength(FFavorites, FavCount);

      for I := 0 to FavCount - 1 do
      begin
        FFavorites[I] := Default(TRadioStation);
        FFavorites[I].Name := GetValue('Name');
        FFavorites[I].URL := GetValue('URL');
        FFavorites[I].Genre := GetValue('Genre');
        FFavorites[I].Bitrate := StrToIntDef(GetValue('Bitrate'), 0);
        FFavorites[I].Country := GetValue('Country');
        FFavorites[I].Language := GetValue('Language');
        FFavorites[I].Favorite := True;
      end;

      Result := True;
    except
      on E: Exception do
        FLastError := 'Load Error: ' + E.Message;
    end;
  finally
    SL.Free;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PLAY
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TRadioManager.PlayStation(Index: Integer);
begin
  if (Index >= 0) and (Index < Length(FStations)) then
  begin
    if Assigned(FOnPlay) then
      FOnPlay(Self, FStations[Index]);
  end;
end;

procedure TRadioManager.PlayFilteredStation(Index: Integer);
begin
  if (Index >= 0) and (Index < Length(FFilteredStations)) then
  begin
    if Assigned(FOnPlay) then
      FOnPlay(Self, FFilteredStations[Index]);
  end;
end;

end.
