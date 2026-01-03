{ ═══════════════════════════════════════════════════════════════════════════════
  uPlaylistManager.pas - Playlist Manager

  Part of 3nity Media - Lazarus Edition

  This unit implements the playlist management system with support for:
  - M3U, M3U8, PLS playlist formats
  - Shuffle and repeat modes
  - Drag and drop reordering
  - Playlist persistence

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uPlaylistManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Process, uTypes;

type
  { ═══════════════════════════════════════════════════════════════════════════
    PLAYLIST CHANGE EVENTS
    ═══════════════════════════════════════════════════════════════════════════ }
  TPlaylistChangeEvent = procedure(Sender: TObject) of object;
  TPlaylistItemEvent = procedure(Sender: TObject; Index: Integer) of object;
  TPlaylistPlayEvent = procedure(Sender: TObject; Index: Integer;
    const FileName: string) of object;

  { ═══════════════════════════════════════════════════════════════════════════
    PLAYLIST MANAGER CLASS
    ═══════════════════════════════════════════════════════════════════════════ }
  TPlaylistManager = class
  private
    FItems: TPlaylistItems;
    FCurrentIndex: Integer;
    FPlaybackMode: TPlaybackMode;
    FModified: Boolean;
    FFileName: string;
    FShuffleOrder: array of Integer;
    FShuffleIndex: Integer;

    // Events
    FOnChange: TPlaylistChangeEvent;
    FOnItemAdded: TPlaylistItemEvent;
    FOnItemRemoved: TPlaylistItemEvent;
    FOnCurrentChange: TPlaylistItemEvent;
    FOnPlay: TPlaylistPlayEvent;
    FOnClear: TNotifyEvent;

    function GetCount: Integer;
    function GetItem(Index: Integer): TPlaylistItem;
    procedure SetItem(Index: Integer; const Value: TPlaylistItem);
    function GetCurrentItem: TPlaylistItem;
    function GetIsEmpty: Boolean;
    function GetHasNext: Boolean;
    function GetHasPrevious: Boolean;
    procedure SetCurrentIndex(Value: Integer);
    procedure SetPlaybackMode(Value: TPlaybackMode);

    procedure GenerateShuffleOrder;
    function GetShufflePosition(Index: Integer): Integer;
    procedure DoChange;

    // Playlist file parsing
    function ParseM3U(const Content: string): Integer;
    function ParsePLS(const Content: string): Integer;
    function GenerateM3U: string;
    function GeneratePLS: string;

    // File info extraction
    function ExtractTitleFromPath(const AFileName: string): string;
    procedure ExtractMediaMetadata(const AFileName: string; out ATitle, AArtist, AAlbum: string; out ADuration: Double);

  public
    constructor Create;
    destructor Destroy; override;

    { ═══════════════════════════════════════════════════════════════════════════
      ITEM MANAGEMENT
      ═══════════════════════════════════════════════════════════════════════════ }
    function Add(const AFileName: string): Integer; overload;
    function Add(const AItem: TPlaylistItem): Integer; overload;
    procedure AddFiles(const FileNames: TStrings);
    procedure Insert(Index: Integer; const AFileName: string); overload;
    procedure Insert(Index: Integer; const AItem: TPlaylistItem); overload;
    procedure Delete(Index: Integer);
    procedure Remove(const AFileName: string);
    procedure Clear;

    { ═══════════════════════════════════════════════════════════════════════════
      ITEM REORDERING
      ═══════════════════════════════════════════════════════════════════════════ }
    procedure MoveUp(Index: Integer);
    procedure MoveDown(Index: Integer);
    procedure Move(FromIndex, ToIndex: Integer);
    procedure Swap(Index1, Index2: Integer);
    procedure Reverse;
    procedure Sort(Ascending: Boolean = True);
    procedure SortByTitle(Ascending: Boolean = True);
    procedure SortByArtist(Ascending: Boolean = True);
    procedure SortByDuration(Ascending: Boolean = True);
    procedure Randomize;

    { ═══════════════════════════════════════════════════════════════════════════
      NAVIGATION
      ═══════════════════════════════════════════════════════════════════════════ }
    function GetNext: Integer;
    function GetPrevious: Integer;
    function GetFirst: Integer;
    function GetLast: Integer;
    function GetRandom: Integer;
    procedure PlayIndex(Index: Integer);
    procedure PlayNext;
    procedure PlayPrevious;
    procedure PlayFirst;
    procedure PlayLast;

    { ═══════════════════════════════════════════════════════════════════════════
      SEARCH & FIND
      ═══════════════════════════════════════════════════════════════════════════ }
    function Find(const AFileName: string): Integer;
    function FindByTitle(const ATitle: string): Integer;
    function Search(const AText: string): TIntegerDynArray;
    function IndexOf(const AFileName: string): Integer;
    function Contains(const AFileName: string): Boolean;

    { ═══════════════════════════════════════════════════════════════════════════
      SELECTION
      ═══════════════════════════════════════════════════════════════════════════ }
    procedure SelectAll;
    procedure SelectNone;
    procedure SelectInvert;
    procedure SetSelected(Index: Integer; Selected: Boolean);
    function GetSelectedCount: Integer;
    function GetSelectedIndices: TIntegerDynArray;
    procedure DeleteSelected;

    { ═══════════════════════════════════════════════════════════════════════════
      PLAYLIST FILE I/O
      ═══════════════════════════════════════════════════════════════════════════ }
    function LoadFromFile(const AFileName: string): Boolean;
    function SaveToFile(const AFileName: string): Boolean;
    function LoadM3U(const AFileName: string): Boolean;
    function LoadPLS(const AFileName: string): Boolean;
    function SaveM3U(const AFileName: string): Boolean;
    function SavePLS(const AFileName: string): Boolean;

    { ═══════════════════════════════════════════════════════════════════════════
      ITEM INFO UPDATE
      ═══════════════════════════════════════════════════════════════════════════ }
    procedure UpdateItemInfo(Index: Integer; const Title, Artist, Album: string;
      Duration: Double);
    procedure UpdateItemDuration(Index: Integer; Duration: Double);
    procedure MarkAsPlayed(Index: Integer);
    procedure ResetPlayedStatus;

    { ═══════════════════════════════════════════════════════════════════════════
      STATISTICS
      ═══════════════════════════════════════════════════════════════════════════ }
    function GetTotalDuration: Double;
    function GetTotalDurationString: string;
    function GetPlayedCount: Integer;
    function GetUnplayedCount: Integer;

    { ═══════════════════════════════════════════════════════════════════════════
      PROPERTIES
      ═══════════════════════════════════════════════════════════════════════════ }
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TPlaylistItem read GetItem write SetItem; default;
    property CurrentIndex: Integer read FCurrentIndex write SetCurrentIndex;
    property CurrentItem: TPlaylistItem read GetCurrentItem;
    property PlaybackMode: TPlaybackMode read FPlaybackMode write SetPlaybackMode;
    property IsEmpty: Boolean read GetIsEmpty;
    property HasNext: Boolean read GetHasNext;
    property HasPrevious: Boolean read GetHasPrevious;
    property Modified: Boolean read FModified write FModified;
    property FileName: string read FFileName write FFileName;

    { ═══════════════════════════════════════════════════════════════════════════
      EVENTS
      ═══════════════════════════════════════════════════════════════════════════ }
    property OnChange: TPlaylistChangeEvent read FOnChange write FOnChange;
    property OnItemAdded: TPlaylistItemEvent read FOnItemAdded write FOnItemAdded;
    property OnItemRemoved: TPlaylistItemEvent read FOnItemRemoved write FOnItemRemoved;
    property OnCurrentChange: TPlaylistItemEvent read FOnCurrentChange write FOnCurrentChange;
    property OnPlay: TPlaylistPlayEvent read FOnPlay write FOnPlay;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
  end;

{ ═══════════════════════════════════════════════════════════════════════════════
  UTILITY FUNCTIONS
  ═══════════════════════════════════════════════════════════════════════════════ }
function FormatDuration(Seconds: Double): string;
function IsPlaylistFile(const AFileName: string): Boolean;
function IsSupportedMediaFile(const AFileName: string): Boolean;

implementation

uses
  StrUtils;

const
  { Supported media extensions }
  AUDIO_EXTENSIONS = '.mp3.m4a.aac.ogg.opus.flac.wav.wma.ape.mpc.tta.wv.aiff.au.';
  VIDEO_EXTENSIONS = '.mp4.mkv.avi.wmv.mov.flv.webm.m4v.mpg.mpeg.vob.ts.m2ts.3gp.ogv.';
  PLAYLIST_EXTENSIONS = '.m3u.m3u8.pls.';

{ ═══════════════════════════════════════════════════════════════════════════════
  UTILITY FUNCTIONS IMPLEMENTATION
  ═══════════════════════════════════════════════════════════════════════════════ }

function FormatDuration(Seconds: Double): string;
var
  H, M, S: Integer;
begin
  if (Seconds <= 0) or IsNaN(Seconds) or IsInfinite(Seconds) then
  begin
    Result := '--:--';
    Exit;
  end;

  H := Trunc(Seconds) div 3600;
  M := (Trunc(Seconds) mod 3600) div 60;
  S := Trunc(Seconds) mod 60;

  if H > 0 then
    Result := Format('%d:%02d:%02d', [H, M, S])
  else
    Result := Format('%d:%02d', [M, S]);
end;

function IsPlaylistFile(const AFileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));
  Result := Pos(Ext + '.', PLAYLIST_EXTENSIONS) > 0;
end;

function IsSupportedMediaFile(const AFileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));
  Result := (Pos(Ext + '.', AUDIO_EXTENSIONS) > 0) or
            (Pos(Ext + '.', VIDEO_EXTENSIONS) > 0);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TPlaylistManager IMPLEMENTATION
  ═══════════════════════════════════════════════════════════════════════════════ }

constructor TPlaylistManager.Create;
begin
  inherited Create;
  SetLength(FItems, 0);
  SetLength(FShuffleOrder, 0);
  FCurrentIndex := -1;
  FShuffleIndex := -1;
  FPlaybackMode := pmNormal;
  FModified := False;
  FFileName := '';
end;

destructor TPlaylistManager.Destroy;
begin
  SetLength(FItems, 0);
  SetLength(FShuffleOrder, 0);
  inherited Destroy;
end;

function TPlaylistManager.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TPlaylistManager.GetItem(Index: Integer): TPlaylistItem;
begin
  if (Index >= 0) and (Index < Length(FItems)) then
    Result := FItems[Index]
  else
  begin
    Result := Default(TPlaylistItem);
  end;
end;

procedure TPlaylistManager.SetItem(Index: Integer; const Value: TPlaylistItem);
begin
  if (Index >= 0) and (Index < Length(FItems)) then
  begin
    FItems[Index] := Value;
    FModified := True;
    DoChange;
  end;
end;

function TPlaylistManager.GetCurrentItem: TPlaylistItem;
begin
  Result := GetItem(FCurrentIndex);
end;

function TPlaylistManager.GetIsEmpty: Boolean;
begin
  Result := Length(FItems) = 0;
end;

function TPlaylistManager.GetHasNext: Boolean;
begin
  case FPlaybackMode of
    pmNormal:
      Result := (FCurrentIndex >= 0) and (FCurrentIndex < Length(FItems) - 1);
    pmRepeatOne:
      Result := FCurrentIndex >= 0;
    pmRepeatAll, pmShuffle, pmShuffleRepeat:
      Result := Length(FItems) > 0;
  else
    Result := False;
  end;
end;

function TPlaylistManager.GetHasPrevious: Boolean;
begin
  case FPlaybackMode of
    pmNormal:
      Result := FCurrentIndex > 0;
    pmRepeatOne:
      Result := FCurrentIndex >= 0;
    pmRepeatAll, pmShuffle, pmShuffleRepeat:
      Result := Length(FItems) > 0;
  else
    Result := False;
  end;
end;

procedure TPlaylistManager.SetCurrentIndex(Value: Integer);
begin
  if Value < -1 then
    Value := -1;
  if Value >= Length(FItems) then
    Value := Length(FItems) - 1;

  if FCurrentIndex <> Value then
  begin
    FCurrentIndex := Value;
    if Assigned(FOnCurrentChange) then
      FOnCurrentChange(Self, FCurrentIndex);
  end;
end;

procedure TPlaylistManager.SetPlaybackMode(Value: TPlaybackMode);
begin
  if FPlaybackMode <> Value then
  begin
    FPlaybackMode := Value;
    if FPlaybackMode in [pmShuffle, pmShuffleRepeat] then
      GenerateShuffleOrder;
    DoChange;
  end;
end;

{ ───────────────────────────────────────────────────────────────────────────
  GenerateShuffleOrder - Create a randomized playback order

  Purpose: Generates a random permutation of playlist indices using the
           Fisher-Yates shuffle algorithm for unbiased randomization.

  Algorithm: Fisher-Yates (Knuth) shuffle
    - Complexity: O(n)
    - Guarantees uniform distribution of permutations
    - In-place shuffle of index array

  Process:
    1. Initialize FShuffleOrder with sequential indices [0, 1, 2, ..., n-1]
    2. For each position i from end to 1:
       - Pick random j in [0, i]
       - Swap elements at positions i and j
    3. Find current track's position in shuffled order (FShuffleIndex)

  Notes:
    - FShuffleIndex tracks where the currently playing item is in shuffle order
    - Allows seamless continuation when shuffle mode is enabled mid-playback
    - Called when playback mode changes to pmShuffle or pmShuffleRepeat
  ─────────────────────────────────────────────────────────────────────────── }
procedure TPlaylistManager.GenerateShuffleOrder;
var
  I, J, Temp: Integer;
begin
  SetLength(FShuffleOrder, Length(FItems));

  { Initialize with sequential order }
  for I := 0 to High(FShuffleOrder) do
    FShuffleOrder[I] := I;

  { Fisher-Yates shuffle }
  for I := High(FShuffleOrder) downto 1 do
  begin
    J := Random(I + 1);
    Temp := FShuffleOrder[I];
    FShuffleOrder[I] := FShuffleOrder[J];
    FShuffleOrder[J] := Temp;
  end;

  { Find current index position in shuffle order }
  FShuffleIndex := -1;
  if FCurrentIndex >= 0 then
  begin
    for I := 0 to High(FShuffleOrder) do
    begin
      if FShuffleOrder[I] = FCurrentIndex then
      begin
        FShuffleIndex := I;
        Break;
      end;
    end;
  end;
end;

function TPlaylistManager.GetShufflePosition(Index: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FShuffleOrder) do
  begin
    if FShuffleOrder[I] = Index then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TPlaylistManager.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TPlaylistManager.ExtractTitleFromPath(const AFileName: string): string;
begin
  Result := ChangeFileExt(ExtractFileName(AFileName), '');
end;

{ ───────────────────────────────────────────────────────────────────────────
  ExtractMediaMetadata - Extract ID3/metadata tags using ffprobe

  Purpose: Uses ffprobe to extract title, artist, album, and duration from
           a media file without fully decoding it.

  Parameters:
    - AFileName: Path to the media file (local files only, not URLs)
    - ATitle, AArtist, AAlbum: Output strings for tag values
    - ADuration: Output duration in seconds

  FFprobe command:
    ffprobe -v quiet -show_entries format=duration:format_tags=title,artist,album
            -of flat <filename>

  Output format parsed: key="value" (e.g., format.tags.title="Song Name")

  Notes:
    - Skips URLs (Pos('://') > 0) - would be slow on network streams
    - Skips non-existent files to avoid ffprobe errors
    - Silently ignores errors if ffprobe is not installed
    - Returns empty strings/zero if metadata extraction fails
    - poWaitOnExit ensures process completes before reading output
  ─────────────────────────────────────────────────────────────────────────── }
procedure TPlaylistManager.ExtractMediaMetadata(const AFileName: string;
  out ATitle, AArtist, AAlbum: string; out ADuration: Double);
var
  AProcess: TProcess;
  Output: TStringList;
  Line, Key, Value: string;
  I, P: Integer;
begin
  ATitle := '';
  AArtist := '';
  AAlbum := '';
  ADuration := 0;

  { Skip URLs - ffprobe would be slow on network streams }
  if Pos('://', AFileName) > 0 then
    Exit;

  { Skip if file doesn't exist }
  if not FileExists(AFileName) then
    Exit;

  AProcess := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    AProcess.Executable := 'ffprobe';
    AProcess.Parameters.Add('-v');
    AProcess.Parameters.Add('quiet');
    AProcess.Parameters.Add('-show_entries');
    AProcess.Parameters.Add('format=duration:format_tags=title,artist,album');
    AProcess.Parameters.Add('-of');
    AProcess.Parameters.Add('flat');
    AProcess.Parameters.Add(AFileName);
    AProcess.Options := [poUsePipes, poNoConsole, poWaitOnExit];

    try
      AProcess.Execute;

      { Read output }
      Output.LoadFromStream(AProcess.Output);

      { Parse output - format is: key="value" }
      for I := 0 to Output.Count - 1 do
      begin
        Line := Output[I];
        P := Pos('=', Line);
        if P > 0 then
        begin
          Key := LowerCase(Copy(Line, 1, P - 1));
          Value := Copy(Line, P + 1, Length(Line) - P);
          { Remove quotes }
          if (Length(Value) >= 2) and (Value[1] = '"') and (Value[Length(Value)] = '"') then
            Value := Copy(Value, 2, Length(Value) - 2);

          if Key = 'format.duration' then
            ADuration := StrToFloatDef(Value, 0)
          else if Key = 'format.tags.title' then
            ATitle := Value
          else if Key = 'format.tags.artist' then
            AArtist := Value
          else if Key = 'format.tags.album' then
            AAlbum := Value;
        end;
      end;
    except
      { Silently ignore errors - ffprobe might not be installed }
    end;
  finally
    Output.Free;
    AProcess.Free;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  ITEM MANAGEMENT
  ═══════════════════════════════════════════════════════════════════════════════ }

function TPlaylistManager.Add(const AFileName: string): Integer;
var
  Item: TPlaylistItem;
  MetaTitle, MetaArtist, MetaAlbum: string;
  MetaDuration: Double;
begin
  Item := Default(TPlaylistItem);
  Item.FileName := AFileName;
  Item.Played := False;
  Item.Selected := False;

  { Try to extract metadata from file }
  ExtractMediaMetadata(AFileName, MetaTitle, MetaArtist, MetaAlbum, MetaDuration);

  { Use metadata title if available, otherwise use filename }
  if MetaTitle <> '' then
    Item.Title := MetaTitle
  else
    Item.Title := ExtractTitleFromPath(AFileName);

  Item.Artist := MetaArtist;
  Item.Album := MetaAlbum;
  Item.Duration := MetaDuration;
  if MetaDuration > 0 then
    Item.DurationString := FormatDuration(MetaDuration)
  else
    Item.DurationString := '--:--';

  Result := Add(Item);
end;

function TPlaylistManager.Add(const AItem: TPlaylistItem): Integer;
begin
  Result := Length(FItems);
  SetLength(FItems, Result + 1);
  FItems[Result] := AItem;

  FModified := True;

  { Regenerate shuffle order if in shuffle mode }
  if FPlaybackMode in [pmShuffle, pmShuffleRepeat] then
    GenerateShuffleOrder;

  if Assigned(FOnItemAdded) then
    FOnItemAdded(Self, Result);

  DoChange;
end;

procedure TPlaylistManager.AddFiles(const FileNames: TStrings);
var
  I: Integer;
begin
  for I := 0 to FileNames.Count - 1 do
  begin
    if IsPlaylistFile(FileNames[I]) then
      LoadFromFile(FileNames[I])
    else if IsSupportedMediaFile(FileNames[I]) or
            (Pos('://', FileNames[I]) > 0) then
      Add(FileNames[I]);
  end;
end;

procedure TPlaylistManager.Insert(Index: Integer; const AFileName: string);
var
  Item: TPlaylistItem;
  MetaTitle, MetaArtist, MetaAlbum: string;
  MetaDuration: Double;
begin
  Item := Default(TPlaylistItem);
  Item.FileName := AFileName;
  Item.Played := False;

  { Try to extract metadata from file }
  ExtractMediaMetadata(AFileName, MetaTitle, MetaArtist, MetaAlbum, MetaDuration);

  { Use metadata title if available, otherwise use filename }
  if MetaTitle <> '' then
    Item.Title := MetaTitle
  else
    Item.Title := ExtractTitleFromPath(AFileName);

  Item.Artist := MetaArtist;
  Item.Album := MetaAlbum;
  Item.Duration := MetaDuration;
  if MetaDuration > 0 then
    Item.DurationString := FormatDuration(MetaDuration)
  else
    Item.DurationString := '--:--';
  Item.Selected := False;

  Insert(Index, Item);
end;

procedure TPlaylistManager.Insert(Index: Integer; const AItem: TPlaylistItem);
var
  I: Integer;
begin
  if Index < 0 then Index := 0;
  if Index > Length(FItems) then Index := Length(FItems);

  SetLength(FItems, Length(FItems) + 1);

  { Shift items down }
  for I := High(FItems) downto Index + 1 do
    FItems[I] := FItems[I - 1];

  FItems[Index] := AItem;

  { Adjust current index if needed }
  if FCurrentIndex >= Index then
    Inc(FCurrentIndex);

  FModified := True;

  if FPlaybackMode in [pmShuffle, pmShuffleRepeat] then
    GenerateShuffleOrder;

  if Assigned(FOnItemAdded) then
    FOnItemAdded(Self, Index);

  DoChange;
end;

procedure TPlaylistManager.Delete(Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= Length(FItems)) then Exit;

  if Assigned(FOnItemRemoved) then
    FOnItemRemoved(Self, Index);

  { Shift items up }
  for I := Index to High(FItems) - 1 do
    FItems[I] := FItems[I + 1];

  SetLength(FItems, Length(FItems) - 1);

  { Adjust current index }
  if FCurrentIndex = Index then
    FCurrentIndex := -1
  else if FCurrentIndex > Index then
    Dec(FCurrentIndex);

  FModified := True;

  if FPlaybackMode in [pmShuffle, pmShuffleRepeat] then
    GenerateShuffleOrder;

  DoChange;
end;

procedure TPlaylistManager.Remove(const AFileName: string);
var
  Index: Integer;
begin
  Index := Find(AFileName);
  if Index >= 0 then
    Delete(Index);
end;

procedure TPlaylistManager.Clear;
begin
  SetLength(FItems, 0);
  SetLength(FShuffleOrder, 0);
  FCurrentIndex := -1;
  FShuffleIndex := -1;
  FModified := True;

  if Assigned(FOnClear) then
    FOnClear(Self);

  DoChange;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  ITEM REORDERING
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TPlaylistManager.MoveUp(Index: Integer);
begin
  if Index > 0 then
    Move(Index, Index - 1);
end;

procedure TPlaylistManager.MoveDown(Index: Integer);
begin
  if Index < Length(FItems) - 1 then
    Move(Index, Index + 1);
end;

{ ───────────────────────────────────────────────────────────────────────────
  Move - Relocate a playlist item to a new position

  Purpose: Moves an item from one position to another, shifting intermediate
           items to fill the gap. Used for drag-and-drop reordering.

  Parameters:
    - FromIndex: Current position of the item to move
    - ToIndex: Target position for the item

  Algorithm:
    - If moving forward (From < To): Shift items [From+1..To] backward by 1
    - If moving backward (From > To): Shift items [To..From-1] forward by 1
    - Place the moved item at ToIndex

  Current index adjustment (3 cases):
    1. Moving the current item: FCurrentIndex = ToIndex
    2. Moving forward past current: Decrement FCurrentIndex (item shifted left)
    3. Moving backward past current: Increment FCurrentIndex (item shifted right)

  Notes:
    - Validates both indices before any modification
    - No-op if FromIndex = ToIndex
    - Sets FModified flag and triggers OnChange event
  ─────────────────────────────────────────────────────────────────────────── }
procedure TPlaylistManager.Move(FromIndex, ToIndex: Integer);
var
  Item: TPlaylistItem;
  I: Integer;
begin
  if (FromIndex < 0) or (FromIndex >= Length(FItems)) then Exit;
  if (ToIndex < 0) or (ToIndex >= Length(FItems)) then Exit;
  if FromIndex = ToIndex then Exit;

  Item := FItems[FromIndex];

  if FromIndex < ToIndex then
  begin
    for I := FromIndex to ToIndex - 1 do
      FItems[I] := FItems[I + 1];
  end
  else
  begin
    for I := FromIndex downto ToIndex + 1 do
      FItems[I] := FItems[I - 1];
  end;

  FItems[ToIndex] := Item;

  { Adjust current index }
  if FCurrentIndex = FromIndex then
    FCurrentIndex := ToIndex
  else if (FromIndex < ToIndex) and (FCurrentIndex > FromIndex) and (FCurrentIndex <= ToIndex) then
    Dec(FCurrentIndex)
  else if (FromIndex > ToIndex) and (FCurrentIndex >= ToIndex) and (FCurrentIndex < FromIndex) then
    Inc(FCurrentIndex);

  FModified := True;
  DoChange;
end;

procedure TPlaylistManager.Swap(Index1, Index2: Integer);
var
  Temp: TPlaylistItem;
begin
  if (Index1 < 0) or (Index1 >= Length(FItems)) then Exit;
  if (Index2 < 0) or (Index2 >= Length(FItems)) then Exit;
  if Index1 = Index2 then Exit;

  Temp := FItems[Index1];
  FItems[Index1] := FItems[Index2];
  FItems[Index2] := Temp;

  { Adjust current index }
  if FCurrentIndex = Index1 then
    FCurrentIndex := Index2
  else if FCurrentIndex = Index2 then
    FCurrentIndex := Index1;

  FModified := True;
  DoChange;
end;

procedure TPlaylistManager.Reverse;
var
  I: Integer;
  Temp: TPlaylistItem;
begin
  for I := 0 to (Length(FItems) div 2) - 1 do
  begin
    Temp := FItems[I];
    FItems[I] := FItems[Length(FItems) - 1 - I];
    FItems[Length(FItems) - 1 - I] := Temp;
  end;

  { Adjust current index }
  if FCurrentIndex >= 0 then
    FCurrentIndex := Length(FItems) - 1 - FCurrentIndex;

  FModified := True;
  DoChange;
end;

{ ───────────────────────────────────────────────────────────────────────────
  Sort - Sort playlist by filename

  Purpose: Sorts all playlist items alphabetically by filename using a
           bubble sort algorithm.

  Parameters:
    - Ascending: True for A-Z, False for Z-A

  Algorithm: Bubble sort
    - Complexity: O(n²) - acceptable for typical playlist sizes
    - Stable sort: equal elements maintain relative order
    - Uses CompareText for case-insensitive comparison

  Current track preservation:
    - Stores current file path before sorting
    - Uses Find() to restore FCurrentIndex after sort

  Notes:
    - No-op if playlist has 0 or 1 items
    - SortByTitle, SortByArtist, SortByDuration use same algorithm
    - Sets FModified flag for playlist persistence
  ─────────────────────────────────────────────────────────────────────────── }
procedure TPlaylistManager.Sort(Ascending: Boolean);
var
  I, J: Integer;
  Temp: TPlaylistItem;
  CurrentFile: string;
begin
  if Length(FItems) <= 1 then Exit;

  { Remember current file }
  if FCurrentIndex >= 0 then
    CurrentFile := FItems[FCurrentIndex].FileName
  else
    CurrentFile := '';

  { Simple bubble sort by filename }
  for I := 0 to High(FItems) - 1 do
  begin
    for J := 0 to High(FItems) - I - 1 do
    begin
      if Ascending then
      begin
        if CompareText(FItems[J].FileName, FItems[J + 1].FileName) > 0 then
        begin
          Temp := FItems[J];
          FItems[J] := FItems[J + 1];
          FItems[J + 1] := Temp;
        end;
      end
      else
      begin
        if CompareText(FItems[J].FileName, FItems[J + 1].FileName) < 0 then
        begin
          Temp := FItems[J];
          FItems[J] := FItems[J + 1];
          FItems[J + 1] := Temp;
        end;
      end;
    end;
  end;

  { Restore current index }
  if CurrentFile <> '' then
    FCurrentIndex := Find(CurrentFile);

  FModified := True;
  DoChange;
end;

procedure TPlaylistManager.SortByTitle(Ascending: Boolean);
var
  I, J: Integer;
  Temp: TPlaylistItem;
  CurrentFile: string;
begin
  if Length(FItems) <= 1 then Exit;

  if FCurrentIndex >= 0 then
    CurrentFile := FItems[FCurrentIndex].FileName
  else
    CurrentFile := '';

  for I := 0 to High(FItems) - 1 do
  begin
    for J := 0 to High(FItems) - I - 1 do
    begin
      if Ascending then
      begin
        if CompareText(FItems[J].Title, FItems[J + 1].Title) > 0 then
        begin
          Temp := FItems[J];
          FItems[J] := FItems[J + 1];
          FItems[J + 1] := Temp;
        end;
      end
      else
      begin
        if CompareText(FItems[J].Title, FItems[J + 1].Title) < 0 then
        begin
          Temp := FItems[J];
          FItems[J] := FItems[J + 1];
          FItems[J + 1] := Temp;
        end;
      end;
    end;
  end;

  if CurrentFile <> '' then
    FCurrentIndex := Find(CurrentFile);

  FModified := True;
  DoChange;
end;

procedure TPlaylistManager.SortByArtist(Ascending: Boolean);
var
  I, J: Integer;
  Temp: TPlaylistItem;
  CurrentFile: string;
begin
  if Length(FItems) <= 1 then Exit;

  if FCurrentIndex >= 0 then
    CurrentFile := FItems[FCurrentIndex].FileName
  else
    CurrentFile := '';

  for I := 0 to High(FItems) - 1 do
  begin
    for J := 0 to High(FItems) - I - 1 do
    begin
      if Ascending then
      begin
        if CompareText(FItems[J].Artist, FItems[J + 1].Artist) > 0 then
        begin
          Temp := FItems[J];
          FItems[J] := FItems[J + 1];
          FItems[J + 1] := Temp;
        end;
      end
      else
      begin
        if CompareText(FItems[J].Artist, FItems[J + 1].Artist) < 0 then
        begin
          Temp := FItems[J];
          FItems[J] := FItems[J + 1];
          FItems[J + 1] := Temp;
        end;
      end;
    end;
  end;

  if CurrentFile <> '' then
    FCurrentIndex := Find(CurrentFile);

  FModified := True;
  DoChange;
end;

procedure TPlaylistManager.SortByDuration(Ascending: Boolean);
var
  I, J: Integer;
  Temp: TPlaylistItem;
  CurrentFile: string;
begin
  if Length(FItems) <= 1 then Exit;

  if FCurrentIndex >= 0 then
    CurrentFile := FItems[FCurrentIndex].FileName
  else
    CurrentFile := '';

  for I := 0 to High(FItems) - 1 do
  begin
    for J := 0 to High(FItems) - I - 1 do
    begin
      if Ascending then
      begin
        if FItems[J].Duration > FItems[J + 1].Duration then
        begin
          Temp := FItems[J];
          FItems[J] := FItems[J + 1];
          FItems[J + 1] := Temp;
        end;
      end
      else
      begin
        if FItems[J].Duration < FItems[J + 1].Duration then
        begin
          Temp := FItems[J];
          FItems[J] := FItems[J + 1];
          FItems[J + 1] := Temp;
        end;
      end;
    end;
  end;

  if CurrentFile <> '' then
    FCurrentIndex := Find(CurrentFile);

  FModified := True;
  DoChange;
end;

procedure TPlaylistManager.Randomize;
var
  I, J: Integer;
  Temp: TPlaylistItem;
  CurrentFile: string;
begin
  if Length(FItems) <= 1 then Exit;

  if FCurrentIndex >= 0 then
    CurrentFile := FItems[FCurrentIndex].FileName
  else
    CurrentFile := '';

  { Fisher-Yates shuffle }
  for I := High(FItems) downto 1 do
  begin
    J := Random(I + 1);
    Temp := FItems[I];
    FItems[I] := FItems[J];
    FItems[J] := Temp;
  end;

  if CurrentFile <> '' then
    FCurrentIndex := Find(CurrentFile);

  FModified := True;
  DoChange;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  NAVIGATION
  ═══════════════════════════════════════════════════════════════════════════════ }

function TPlaylistManager.GetNext: Integer;
begin
  Result := -1;
  if Length(FItems) = 0 then Exit;

  case FPlaybackMode of
    pmNormal:
      begin
        if FCurrentIndex < Length(FItems) - 1 then
          Result := FCurrentIndex + 1;
      end;

    pmRepeatOne:
      Result := FCurrentIndex;

    pmRepeatAll:
      begin
        if FCurrentIndex < Length(FItems) - 1 then
          Result := FCurrentIndex + 1
        else
          Result := 0;
      end;

    pmShuffle:
      begin
        if FShuffleIndex < Length(FShuffleOrder) - 1 then
        begin
          Inc(FShuffleIndex);
          Result := FShuffleOrder[FShuffleIndex];
        end;
      end;

    pmShuffleRepeat:
      begin
        if FShuffleIndex < Length(FShuffleOrder) - 1 then
          Inc(FShuffleIndex)
        else
        begin
          GenerateShuffleOrder;
          FShuffleIndex := 0;
        end;
        Result := FShuffleOrder[FShuffleIndex];
      end;
  end;
end;

function TPlaylistManager.GetPrevious: Integer;
begin
  Result := -1;
  if Length(FItems) = 0 then Exit;

  case FPlaybackMode of
    pmNormal:
      begin
        if FCurrentIndex > 0 then
          Result := FCurrentIndex - 1;
      end;

    pmRepeatOne:
      Result := FCurrentIndex;

    pmRepeatAll:
      begin
        if FCurrentIndex > 0 then
          Result := FCurrentIndex - 1
        else
          Result := Length(FItems) - 1;
      end;

    pmShuffle, pmShuffleRepeat:
      begin
        if FShuffleIndex > 0 then
        begin
          Dec(FShuffleIndex);
          Result := FShuffleOrder[FShuffleIndex];
        end
        else if FPlaybackMode = pmShuffleRepeat then
        begin
          FShuffleIndex := Length(FShuffleOrder) - 1;
          Result := FShuffleOrder[FShuffleIndex];
        end;
      end;
  end;
end;

function TPlaylistManager.GetFirst: Integer;
begin
  if Length(FItems) > 0 then
    Result := 0
  else
    Result := -1;
end;

function TPlaylistManager.GetLast: Integer;
begin
  if Length(FItems) > 0 then
    Result := Length(FItems) - 1
  else
    Result := -1;
end;

function TPlaylistManager.GetRandom: Integer;
begin
  if Length(FItems) > 0 then
    Result := Random(Length(FItems))
  else
    Result := -1;
end;

procedure TPlaylistManager.PlayIndex(Index: Integer);
begin
  if (Index >= 0) and (Index < Length(FItems)) then
  begin
    SetCurrentIndex(Index);

    { Update shuffle position }
    if FPlaybackMode in [pmShuffle, pmShuffleRepeat] then
      FShuffleIndex := GetShufflePosition(Index);

    if Assigned(FOnPlay) then
      FOnPlay(Self, Index, FItems[Index].FileName);
  end;
end;

procedure TPlaylistManager.PlayNext;
var
  NextIndex: Integer;
begin
  NextIndex := GetNext;
  if NextIndex >= 0 then
    PlayIndex(NextIndex);
end;

procedure TPlaylistManager.PlayPrevious;
var
  PrevIndex: Integer;
begin
  PrevIndex := GetPrevious;
  if PrevIndex >= 0 then
    PlayIndex(PrevIndex);
end;

procedure TPlaylistManager.PlayFirst;
begin
  if Length(FItems) > 0 then
    PlayIndex(0);
end;

procedure TPlaylistManager.PlayLast;
begin
  if Length(FItems) > 0 then
    PlayIndex(Length(FItems) - 1);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  SEARCH & FIND
  ═══════════════════════════════════════════════════════════════════════════════ }

function TPlaylistManager.Find(const AFileName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FItems) do
  begin
    if CompareText(FItems[I].FileName, AFileName) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TPlaylistManager.FindByTitle(const ATitle: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FItems) do
  begin
    if CompareText(FItems[I].Title, ATitle) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

{ ───────────────────────────────────────────────────────────────────────────
  Search - Find playlist items matching a search term

  Purpose: Performs a case-insensitive substring search across multiple
           fields of all playlist items.

  Parameters:
    - AText: Search term to find (case-insensitive)

  Returns: Dynamic array of matching item indices

  Fields searched (in order):
    1. FileName - Full file path
    2. Title - Track title from metadata
    3. Artist - Artist name from metadata
    4. Album - Album name from metadata

  Notes:
    - Returns empty array if AText is empty
    - All fields have equal search priority (first match wins)
    - Uses Pos() for substring matching (not regex)
    - Result array grows dynamically as matches are found
  ─────────────────────────────────────────────────────────────────────────── }
function TPlaylistManager.Search(const AText: string): TIntegerDynArray;
var
  I, MatchCount: Integer;
  SearchLower: string;
begin
  Result := nil;
  if AText = '' then Exit;

  SearchLower := LowerCase(AText);
  MatchCount := 0;

  for I := 0 to High(FItems) do
  begin
    if (Pos(SearchLower, LowerCase(FItems[I].FileName)) > 0) or
       (Pos(SearchLower, LowerCase(FItems[I].Title)) > 0) or
       (Pos(SearchLower, LowerCase(FItems[I].Artist)) > 0) or
       (Pos(SearchLower, LowerCase(FItems[I].Album)) > 0) then
    begin
      SetLength(Result, MatchCount + 1);
      Result[MatchCount] := I;
      Inc(MatchCount);
    end;
  end;
end;

function TPlaylistManager.IndexOf(const AFileName: string): Integer;
begin
  Result := Find(AFileName);
end;

function TPlaylistManager.Contains(const AFileName: string): Boolean;
begin
  Result := Find(AFileName) >= 0;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  SELECTION
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TPlaylistManager.SelectAll;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    FItems[I].Selected := True;
  DoChange;
end;

procedure TPlaylistManager.SelectNone;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    FItems[I].Selected := False;
  DoChange;
end;

procedure TPlaylistManager.SelectInvert;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    FItems[I].Selected := not FItems[I].Selected;
  DoChange;
end;

procedure TPlaylistManager.SetSelected(Index: Integer; Selected: Boolean);
begin
  if (Index >= 0) and (Index < Length(FItems)) then
  begin
    FItems[Index].Selected := Selected;
    DoChange;
  end;
end;

function TPlaylistManager.GetSelectedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FItems) do
    if FItems[I].Selected then
      Inc(Result);
end;

function TPlaylistManager.GetSelectedIndices: TIntegerDynArray;
var
  I, SelCount: Integer;
begin
  Result := nil;
  SelCount := 0;

  for I := 0 to High(FItems) do
  begin
    if FItems[I].Selected then
    begin
      SetLength(Result, SelCount + 1);
      Result[SelCount] := I;
      Inc(SelCount);
    end;
  end;
end;

procedure TPlaylistManager.DeleteSelected;
var
  I: Integer;
begin
  { Delete from end to preserve indices }
  for I := High(FItems) downto 0 do
  begin
    if FItems[I].Selected then
      Delete(I);
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PLAYLIST FILE PARSING
  ═══════════════════════════════════════════════════════════════════════════════ }

{ ───────────────────────────────────────────────────────────────────────────
  ParseM3U - Parse M3U/M3U8 playlist format

  Purpose: Parses M3U playlist content and adds entries to the playlist.
           Supports both simple M3U and extended M3U (EXTM3U) formats.

  Parameters:
    - Content: Raw text content of the M3U file

  Returns: Number of entries successfully added

  M3U Format specification:
    #EXTM3U                     <- Header (optional, indicates extended format)
    #EXTINF:duration,title      <- Extended info: duration in seconds, display title
    path/to/file.mp3            <- File path or URL

  State machine:
    - ExtInf flag tracks if previous line was #EXTINF
    - Duration/Title from #EXTINF apply to the NEXT non-comment line
    - State resets after each file entry is processed

  Lines handled:
    - Empty lines: Skipped
    - #EXTM3U: Header marker, skipped
    - #EXTINF:...: Parse duration and title, set ExtInf=True
    - #...: Other comments, skipped
    - Anything else: Treated as file path/URL

  Notes:
    - Duration format: seconds (integer), -1 for unknown
    - Title after comma is optional
    - Falls back to filename if no title in EXTINF
  ─────────────────────────────────────────────────────────────────────────── }
function TPlaylistManager.ParseM3U(const Content: string): Integer;
var
  Lines: TStringList;
  I: Integer;
  Line, NextLine: string;
  Item: TPlaylistItem;
  ExtInf: Boolean;
  Duration: Integer;
  Title: string;
begin
  Result := 0;
  Lines := TStringList.Create;
  try
    Lines.Text := Content;

    ExtInf := False;
    Duration := 0;
    Title := '';

    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);

      { Skip empty lines and M3U header }
      if (Line = '') or (UpperCase(Line) = '#EXTM3U') then
        Continue;

      { Parse EXTINF line }
      if StartsText('#EXTINF:', Line) then
      begin
        ExtInf := True;
        Line := Copy(Line, 9, Length(Line));

        { Extract duration and title }
        if Pos(',', Line) > 0 then
        begin
          Duration := StrToIntDef(Copy(Line, 1, Pos(',', Line) - 1), 0);
          Title := Trim(Copy(Line, Pos(',', Line) + 1, Length(Line)));
        end
        else
        begin
          Duration := StrToIntDef(Line, 0);
          Title := '';
        end;
        Continue;
      end;

      { Skip other comments }
      if StartsText('#', Line) then
        Continue;

      { This is a file path or URL }
      Item := Default(TPlaylistItem);
      Item.FileName := Line;

      if ExtInf then
      begin
        if Title <> '' then
          Item.Title := Title
        else
          Item.Title := ExtractTitleFromPath(Line);
        Item.Duration := Duration;
        Item.DurationString := FormatDuration(Duration);
      end
      else
      begin
        Item.Title := ExtractTitleFromPath(Line);
        Item.Duration := 0;
        Item.DurationString := '--:--';
      end;

      Add(Item);
      Inc(Result);

      { Reset EXTINF data }
      ExtInf := False;
      Duration := 0;
      Title := '';
    end;
  finally
    Lines.Free;
  end;
end;

{ ───────────────────────────────────────────────────────────────────────────
  ParsePLS - Parse PLS playlist format

  Purpose: Parses PLS (Winamp) playlist content and adds entries to the playlist.
           Uses a two-pass algorithm for reliable entry reconstruction.

  Parameters:
    - Content: Raw text content of the PLS file

  Returns: Number of entries successfully added

  PLS Format specification (INI-like):
    [playlist]
    NumberOfEntries=3
    File1=path/to/first.mp3
    Title1=First Song
    Length1=180
    File2=path/to/second.mp3
    ...
    Version=2

  Two-pass algorithm:
    Pass 1: Determine entry count
      - Look for NumberOfEntries=N
      - Fallback: Count lines starting with "File"

    Pass 2: Parse entries into temporary array
      - FileN -> PLSItems[N-1].FileName
      - TitleN -> PLSItems[N-1].Title
      - LengthN -> PLSItems[N-1].Duration (seconds)

  Notes:
    - Entry numbers are 1-based in PLS, converted to 0-based internally
    - Pre-allocates PLSItems array based on NumberOfEntries for efficiency
    - Only entries with non-empty FileName are added
    - Missing titles fall back to filename extraction
  ─────────────────────────────────────────────────────────────────────────── }
function TPlaylistManager.ParsePLS(const Content: string): Integer;
var
  Lines: TStringList;
  I, NumEntries, EntryNum: Integer;
  Line, Key, Value: string;
  PLSItems: array of TPlaylistItem;
  P: Integer;
begin
  Result := 0;
  Lines := TStringList.Create;
  try
    Lines.Text := Content;

    NumEntries := 0;

    { First pass: count entries }
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      if StartsText('NumberOfEntries=', Line) then
      begin
        NumEntries := StrToIntDef(Copy(Line, 17, Length(Line)), 0);
        Break;
      end;
    end;

    if NumEntries = 0 then
    begin
      { Count File entries instead }
      for I := 0 to Lines.Count - 1 do
      begin
        if StartsText('File', Trim(Lines[I])) then
          Inc(NumEntries);
      end;
    end;

    if NumEntries = 0 then Exit;

    SetLength(PLSItems, NumEntries);
    for I := 0 to NumEntries - 1 do
      PLSItems[I] := Default(TPlaylistItem);

    { Second pass: parse entries }
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);

      P := Pos('=', Line);
      if P <= 0 then Continue;

      Key := Copy(Line, 1, P - 1);
      Value := Copy(Line, P + 1, Length(Line));

      if StartsText('File', Key) then
      begin
        EntryNum := StrToIntDef(Copy(Key, 5, Length(Key)), 0) - 1;
        if (EntryNum >= 0) and (EntryNum < NumEntries) then
          PLSItems[EntryNum].FileName := Value;
      end
      else if StartsText('Title', Key) then
      begin
        EntryNum := StrToIntDef(Copy(Key, 6, Length(Key)), 0) - 1;
        if (EntryNum >= 0) and (EntryNum < NumEntries) then
          PLSItems[EntryNum].Title := Value;
      end
      else if StartsText('Length', Key) then
      begin
        EntryNum := StrToIntDef(Copy(Key, 7, Length(Key)), 0) - 1;
        if (EntryNum >= 0) and (EntryNum < NumEntries) then
        begin
          PLSItems[EntryNum].Duration := StrToIntDef(Value, 0);
          PLSItems[EntryNum].DurationString := FormatDuration(PLSItems[EntryNum].Duration);
        end;
      end;
    end;

    { Add valid entries }
    for I := 0 to NumEntries - 1 do
    begin
      if PLSItems[I].FileName <> '' then
      begin
        if PLSItems[I].Title = '' then
          PLSItems[I].Title := ExtractTitleFromPath(PLSItems[I].FileName);
        if PLSItems[I].DurationString = '' then
          PLSItems[I].DurationString := '--:--';
        Add(PLSItems[I]);
        Inc(Result);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TPlaylistManager.GenerateM3U: string;
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add('#EXTM3U');

    for I := 0 to High(FItems) do
    begin
      SL.Add(Format('#EXTINF:%d,%s', [
        Trunc(FItems[I].Duration),
        FItems[I].Title
      ]));
      SL.Add(FItems[I].FileName);
    end;

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TPlaylistManager.GeneratePLS: string;
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add('[playlist]');

    for I := 0 to High(FItems) do
    begin
      SL.Add(Format('File%d=%s', [I + 1, FItems[I].FileName]));
      SL.Add(Format('Title%d=%s', [I + 1, FItems[I].Title]));
      SL.Add(Format('Length%d=%d', [I + 1, Trunc(FItems[I].Duration)]));
    end;

    SL.Add(Format('NumberOfEntries=%d', [Length(FItems)]));
    SL.Add('Version=2');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PLAYLIST FILE I/O
  ═══════════════════════════════════════════════════════════════════════════════ }

function TPlaylistManager.LoadFromFile(const AFileName: string): Boolean;
var
  Ext: string;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;

  Ext := LowerCase(ExtractFileExt(AFileName));

  if (Ext = '.m3u') or (Ext = '.m3u8') then
    Result := LoadM3U(AFileName)
  else if Ext = '.pls' then
    Result := LoadPLS(AFileName);

  if Result then
    FFileName := AFileName;
end;

function TPlaylistManager.SaveToFile(const AFileName: string): Boolean;
var
  Ext: string;
begin
  Result := False;

  Ext := LowerCase(ExtractFileExt(AFileName));

  if (Ext = '.m3u') or (Ext = '.m3u8') then
    Result := SaveM3U(AFileName)
  else if Ext = '.pls' then
    Result := SavePLS(AFileName);

  if Result then
  begin
    FFileName := AFileName;
    FModified := False;
  end;
end;

function TPlaylistManager.LoadM3U(const AFileName: string): Boolean;
var
  SL: TStringList;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;

  SL := TStringList.Create;
  try
    SL.LoadFromFile(AFileName);
    ParseM3U(SL.Text);
    FFileName := AFileName;
    FModified := False;
    Result := True;
  finally
    SL.Free;
  end;
end;

function TPlaylistManager.LoadPLS(const AFileName: string): Boolean;
var
  SL: TStringList;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;

  SL := TStringList.Create;
  try
    SL.LoadFromFile(AFileName);
    ParsePLS(SL.Text);
    FFileName := AFileName;
    FModified := False;
    Result := True;
  finally
    SL.Free;
  end;
end;

function TPlaylistManager.SaveM3U(const AFileName: string): Boolean;
var
  SL: TStringList;
begin
  Result := False;

  SL := TStringList.Create;
  try
    SL.Text := GenerateM3U;
    SL.SaveToFile(AFileName);
    Result := True;
  finally
    SL.Free;
  end;
end;

function TPlaylistManager.SavePLS(const AFileName: string): Boolean;
var
  SL: TStringList;
begin
  Result := False;

  SL := TStringList.Create;
  try
    SL.Text := GeneratePLS;
    SL.SaveToFile(AFileName);
    Result := True;
  finally
    SL.Free;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  ITEM INFO UPDATE
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TPlaylistManager.UpdateItemInfo(Index: Integer; const Title, Artist,
  Album: string; Duration: Double);
begin
  if (Index >= 0) and (Index < Length(FItems)) then
  begin
    FItems[Index].Title := Title;
    FItems[Index].Artist := Artist;
    FItems[Index].Album := Album;
    FItems[Index].Duration := Duration;
    FItems[Index].DurationString := FormatDuration(Duration);
    DoChange;
  end;
end;

procedure TPlaylistManager.UpdateItemDuration(Index: Integer; Duration: Double);
begin
  if (Index >= 0) and (Index < Length(FItems)) then
  begin
    FItems[Index].Duration := Duration;
    FItems[Index].DurationString := FormatDuration(Duration);
    DoChange;
  end;
end;

procedure TPlaylistManager.MarkAsPlayed(Index: Integer);
begin
  if (Index >= 0) and (Index < Length(FItems)) then
  begin
    FItems[Index].Played := True;
  end;
end;

procedure TPlaylistManager.ResetPlayedStatus;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    FItems[I].Played := False;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  STATISTICS
  ═══════════════════════════════════════════════════════════════════════════════ }

function TPlaylistManager.GetTotalDuration: Double;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FItems) do
    Result := Result + FItems[I].Duration;
end;

function TPlaylistManager.GetTotalDurationString: string;
begin
  Result := FormatDuration(GetTotalDuration);
end;

function TPlaylistManager.GetPlayedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FItems) do
    if FItems[I].Played then
      Inc(Result);
end;

function TPlaylistManager.GetUnplayedCount: Integer;
begin
  Result := Length(FItems) - GetPlayedCount;
end;

initialization
  Randomize;

end.
