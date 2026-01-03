{ ===============================================================================
  uMockPlaylist.pas - Mock Playlist Manager for Testing

  Part of 3nity Media - Test Suite

  Provides a mock implementation of TPlaylistManager that works in-memory
  without file system dependencies. Useful for:
  - Unit testing components that depend on playlist
  - Integration testing without file I/O
  - Simulating various playlist states and events

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uMockPlaylist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uTypes;

type
  { Event types matching TPlaylistManager }
  TMockPlaylistChangeEvent = procedure(Sender: TObject) of object;
  TMockPlaylistItemEvent = procedure(Sender: TObject; Index: Integer) of object;
  TMockPlaylistPlayEvent = procedure(Sender: TObject; Index: Integer; const FileName: string) of object;

  { ===========================================================================
    TMockPlaylistItem - Simplified playlist item
    =========================================================================== }
  TMockPlaylistItem = record
    FileName: string;
    Title: string;
    Artist: string;
    Album: string;
    Duration: Double;
    Selected: Boolean;
    Played: Boolean;
  end;

  TIntegerDynArray = array of Integer;

  { ===========================================================================
    TMockPlaylistManager - Mock implementation of Playlist Manager
    =========================================================================== }
  TMockPlaylistManager = class
  private
    FItems: array of TMockPlaylistItem;
    FCount: Integer;
    FCurrentIndex: Integer;
    FPlaybackMode: TPlaybackMode;
    FModified: Boolean;
    FFileName: string;
    FShuffleOrder: array of Integer;
    FShufflePosition: Integer;

    { Events }
    FOnChange: TMockPlaylistChangeEvent;
    FOnItemAdded: TMockPlaylistItemEvent;
    FOnItemRemoved: TMockPlaylistItemEvent;
    FOnCurrentChange: TMockPlaylistItemEvent;
    FOnPlay: TMockPlaylistPlayEvent;
    FOnClear: TNotifyEvent;

    { Call logging for testing }
    FCallLog: TStringList;

    function GetCount: Integer;
    function GetItem(Index: Integer): TMockPlaylistItem;
    procedure SetItem(Index: Integer; const Value: TMockPlaylistItem);
    function GetCurrentItem: TMockPlaylistItem;
    function GetIsEmpty: Boolean;
    function GetHasNext: Boolean;
    function GetHasPrevious: Boolean;
    procedure SetCurrentIndex(Value: Integer);
    procedure SetPlaybackMode(Value: TPlaybackMode);
    procedure GenerateShuffleOrder;
    procedure DoChange;
    procedure LogCall(const AMethodName: string);
  public
    constructor Create;
    destructor Destroy; override;

    { Item management }
    function Add(const AFileName: string): Integer; overload;
    function Add(const AItem: TMockPlaylistItem): Integer; overload;
    procedure AddFiles(const FileNames: TStrings);
    procedure Insert(Index: Integer; const AFileName: string); overload;
    procedure Insert(Index: Integer; const AItem: TMockPlaylistItem); overload;
    procedure Delete(Index: Integer);
    procedure Remove(const AFileName: string);
    procedure Clear;

    { Ordering }
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

    { Navigation }
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

    { Search }
    function Find(const AFileName: string): Integer;
    function FindByTitle(const ATitle: string): Integer;
    function Search(const AText: string): TIntegerDynArray;
    function IndexOf(const AFileName: string): Integer;
    function Contains(const AFileName: string): Boolean;

    { Selection }
    procedure SelectAll;
    procedure SelectNone;
    procedure SelectInvert;
    procedure SetSelected(Index: Integer; Selected: Boolean);
    function GetSelectedCount: Integer;
    function GetSelectedIndices: TIntegerDynArray;
    procedure DeleteSelected;

    { File I/O - Mock implementations }
    function LoadFromFile(const AFileName: string): Boolean;
    function SaveToFile(const AFileName: string): Boolean;
    function LoadM3U(const AFileName: string): Boolean;
    function LoadPLS(const AFileName: string): Boolean;
    function SaveM3U(const AFileName: string): Boolean;
    function SavePLS(const AFileName: string): Boolean;

    { M3U/PLS parsing - for testing }
    function ParseM3U(const Content: string): Integer;
    function ParsePLS(const Content: string): Integer;
    function GenerateM3U: string;
    function GeneratePLS: string;

    { Item info }
    procedure UpdateItemInfo(Index: Integer; const Title, Artist, Album: string;
      Duration: Double);
    procedure UpdateItemDuration(Index: Integer; Duration: Double);
    procedure MarkAsPlayed(Index: Integer);
    procedure ResetPlayedStatus;

    { Statistics }
    function GetTotalDuration: Double;
    function GetTotalDurationString: string;
    function GetPlayedCount: Integer;
    function GetUnplayedCount: Integer;

    { Test helpers }
    function GetCallLog: TStringList;
    procedure ClearCallLog;
    procedure PopulateWithTestData(ItemCount: Integer);

    { Properties }
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMockPlaylistItem read GetItem write SetItem; default;
    property CurrentIndex: Integer read FCurrentIndex write SetCurrentIndex;
    property CurrentItem: TMockPlaylistItem read GetCurrentItem;
    property PlaybackMode: TPlaybackMode read FPlaybackMode write SetPlaybackMode;
    property IsEmpty: Boolean read GetIsEmpty;
    property HasNext: Boolean read GetHasNext;
    property HasPrevious: Boolean read GetHasPrevious;
    property Modified: Boolean read FModified write FModified;
    property FileName: string read FFileName write FFileName;

    { Events }
    property OnChange: TMockPlaylistChangeEvent read FOnChange write FOnChange;
    property OnItemAdded: TMockPlaylistItemEvent read FOnItemAdded write FOnItemAdded;
    property OnItemRemoved: TMockPlaylistItemEvent read FOnItemRemoved write FOnItemRemoved;
    property OnCurrentChange: TMockPlaylistItemEvent read FOnCurrentChange write FOnCurrentChange;
    property OnPlay: TMockPlaylistPlayEvent read FOnPlay write FOnPlay;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
  end;

{ Helper functions }
function FormatDuration(Seconds: Double): string;
function IsPlaylistFile(const AFileName: string): Boolean;
function IsSupportedMediaFile(const AFileName: string): Boolean;

implementation

uses
  Math;

{ ===============================================================================
  Helper Functions
  =============================================================================== }

function FormatDuration(Seconds: Double): string;
var
  H, M, S: Integer;

  function ZeroPad(N: Integer): string;
  begin
    if N < 10 then
      Result := '0' + IntToStr(N)
    else
      Result := IntToStr(N);
  end;

begin
  if IsNaN(Seconds) or (Seconds < 0) then
    Seconds := 0;

  H := Trunc(Seconds) div 3600;
  M := (Trunc(Seconds) mod 3600) div 60;
  S := Trunc(Seconds) mod 60;

  if H > 0 then
    Result := IntToStr(H) + ':' + ZeroPad(M) + ':' + ZeroPad(S)
  else
    Result := IntToStr(M) + ':' + ZeroPad(S);
end;

function IsPlaylistFile(const AFileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));
  Result := (Ext = '.m3u') or (Ext = '.m3u8') or (Ext = '.pls');
end;

function IsSupportedMediaFile(const AFileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));
  Result := (Ext = '.mp3') or (Ext = '.mp4') or (Ext = '.mkv') or
            (Ext = '.avi') or (Ext = '.wav') or (Ext = '.flac') or
            (Ext = '.ogg') or (Ext = '.webm') or (Ext = '.m4a') or
            (Ext = '.aac') or (Ext = '.wma') or (Ext = '.wmv');
end;

{ ===============================================================================
  TMockPlaylistManager
  =============================================================================== }

constructor TMockPlaylistManager.Create;
begin
  inherited Create;
  FCount := 0;
  FCurrentIndex := -1;
  FPlaybackMode := pmNormal;
  FModified := False;
  FFileName := '';
  FShufflePosition := 0;
  SetLength(FItems, 0);
  SetLength(FShuffleOrder, 0);
  FCallLog := TStringList.Create;
end;

destructor TMockPlaylistManager.Destroy;
begin
  FCallLog.Free;
  inherited Destroy;
end;

procedure TMockPlaylistManager.LogCall(const AMethodName: string);
begin
  FCallLog.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + AMethodName);
end;

procedure TMockPlaylistManager.DoChange;
begin
  FModified := True;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TMockPlaylistManager.GetCount: Integer;
begin
  Result := FCount;
end;

function TMockPlaylistManager.GetItem(Index: Integer): TMockPlaylistItem;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FItems[Index]
  else
    Result := Default(TMockPlaylistItem);
end;

procedure TMockPlaylistManager.SetItem(Index: Integer; const Value: TMockPlaylistItem);
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    FItems[Index] := Value;
    DoChange;
  end;
end;

function TMockPlaylistManager.GetCurrentItem: TMockPlaylistItem;
begin
  Result := GetItem(FCurrentIndex);
end;

function TMockPlaylistManager.GetIsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TMockPlaylistManager.GetHasNext: Boolean;
begin
  case FPlaybackMode of
    pmNormal: Result := FCurrentIndex < FCount - 1;
    pmRepeatAll, pmRepeatOne, pmShuffle: Result := FCount > 0;
  else
    Result := False;
  end;
end;

function TMockPlaylistManager.GetHasPrevious: Boolean;
begin
  case FPlaybackMode of
    pmNormal: Result := FCurrentIndex > 0;
    pmRepeatAll, pmRepeatOne, pmShuffle: Result := FCount > 0;
  else
    Result := False;
  end;
end;

procedure TMockPlaylistManager.SetCurrentIndex(Value: Integer);
begin
  if Value < -1 then Value := -1;
  if Value >= FCount then Value := FCount - 1;
  if FCurrentIndex <> Value then
  begin
    FCurrentIndex := Value;
    if Assigned(FOnCurrentChange) then
      FOnCurrentChange(Self, Value);
  end;
end;

procedure TMockPlaylistManager.SetPlaybackMode(Value: TPlaybackMode);
begin
  if FPlaybackMode <> Value then
  begin
    FPlaybackMode := Value;
    if Value = pmShuffle then
      GenerateShuffleOrder;
    DoChange;
  end;
end;

procedure TMockPlaylistManager.GenerateShuffleOrder;
var
  I, J, Temp: Integer;
begin
  SetLength(FShuffleOrder, FCount);
  for I := 0 to FCount - 1 do
    FShuffleOrder[I] := I;

  { Fisher-Yates shuffle }
  for I := FCount - 1 downto 1 do
  begin
    J := Random(I + 1);
    Temp := FShuffleOrder[I];
    FShuffleOrder[I] := FShuffleOrder[J];
    FShuffleOrder[J] := Temp;
  end;
  FShufflePosition := 0;
end;

function TMockPlaylistManager.Add(const AFileName: string): Integer;
var
  Item: TMockPlaylistItem;
begin
  LogCall('Add(' + AFileName + ')');
  Item := Default(TMockPlaylistItem);
  Item.FileName := AFileName;
  Item.Title := ChangeFileExt(ExtractFileName(AFileName), '');
  Item.Duration := 0;
  Result := Add(Item);
end;

function TMockPlaylistManager.Add(const AItem: TMockPlaylistItem): Integer;
begin
  Inc(FCount);
  SetLength(FItems, FCount);
  FItems[FCount - 1] := AItem;
  Result := FCount - 1;

  if FPlaybackMode = pmShuffle then
    GenerateShuffleOrder;

  if Assigned(FOnItemAdded) then
    FOnItemAdded(Self, Result);
  DoChange;
end;

procedure TMockPlaylistManager.AddFiles(const FileNames: TStrings);
var
  I: Integer;
begin
  LogCall('AddFiles(' + IntToStr(FileNames.Count) + ' files)');
  for I := 0 to FileNames.Count - 1 do
    Add(FileNames[I]);
end;

procedure TMockPlaylistManager.Insert(Index: Integer; const AFileName: string);
var
  Item: TMockPlaylistItem;
begin
  LogCall('Insert(' + IntToStr(Index) + ', ' + AFileName + ')');
  Item := Default(TMockPlaylistItem);
  Item.FileName := AFileName;
  Item.Title := ChangeFileExt(ExtractFileName(AFileName), '');
  Insert(Index, Item);
end;

procedure TMockPlaylistManager.Insert(Index: Integer; const AItem: TMockPlaylistItem);
var
  I: Integer;
begin
  if Index < 0 then Index := 0;
  if Index > FCount then Index := FCount;

  Inc(FCount);
  SetLength(FItems, FCount);

  for I := FCount - 1 downto Index + 1 do
    FItems[I] := FItems[I - 1];

  FItems[Index] := AItem;

  if FCurrentIndex >= Index then
    Inc(FCurrentIndex);

  if FPlaybackMode = pmShuffle then
    GenerateShuffleOrder;

  if Assigned(FOnItemAdded) then
    FOnItemAdded(Self, Index);
  DoChange;
end;

procedure TMockPlaylistManager.Delete(Index: Integer);
var
  I: Integer;
begin
  LogCall('Delete(' + IntToStr(Index) + ')');
  if (Index < 0) or (Index >= FCount) then Exit;

  if Assigned(FOnItemRemoved) then
    FOnItemRemoved(Self, Index);

  for I := Index to FCount - 2 do
    FItems[I] := FItems[I + 1];

  Dec(FCount);
  SetLength(FItems, FCount);

  if FCurrentIndex >= Index then
  begin
    if FCurrentIndex > 0 then
      Dec(FCurrentIndex)
    else if FCount = 0 then
      FCurrentIndex := -1;
  end;

  if FPlaybackMode = pmShuffle then
    GenerateShuffleOrder;

  DoChange;
end;

procedure TMockPlaylistManager.Remove(const AFileName: string);
var
  Index: Integer;
begin
  LogCall('Remove(' + AFileName + ')');
  Index := Find(AFileName);
  if Index >= 0 then
    Delete(Index);
end;

procedure TMockPlaylistManager.Clear;
begin
  LogCall('Clear');
  SetLength(FItems, 0);
  SetLength(FShuffleOrder, 0);
  FCount := 0;
  FCurrentIndex := -1;
  FShufflePosition := 0;

  if Assigned(FOnClear) then
    FOnClear(Self);
  DoChange;
end;

procedure TMockPlaylistManager.MoveUp(Index: Integer);
begin
  LogCall('MoveUp(' + IntToStr(Index) + ')');
  if Index > 0 then
    Swap(Index, Index - 1);
end;

procedure TMockPlaylistManager.MoveDown(Index: Integer);
begin
  LogCall('MoveDown(' + IntToStr(Index) + ')');
  if Index < FCount - 1 then
    Swap(Index, Index + 1);
end;

procedure TMockPlaylistManager.Move(FromIndex, ToIndex: Integer);
var
  Item: TMockPlaylistItem;
  I: Integer;
begin
  LogCall('Move(' + IntToStr(FromIndex) + ', ' + IntToStr(ToIndex) + ')');
  if (FromIndex < 0) or (FromIndex >= FCount) then Exit;
  if (ToIndex < 0) or (ToIndex >= FCount) then Exit;
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
  else if (FromIndex < FCurrentIndex) and (ToIndex >= FCurrentIndex) then
    Dec(FCurrentIndex)
  else if (FromIndex > FCurrentIndex) and (ToIndex <= FCurrentIndex) then
    Inc(FCurrentIndex);

  DoChange;
end;

procedure TMockPlaylistManager.Swap(Index1, Index2: Integer);
var
  Temp: TMockPlaylistItem;
begin
  LogCall('Swap(' + IntToStr(Index1) + ', ' + IntToStr(Index2) + ')');
  if (Index1 < 0) or (Index1 >= FCount) then Exit;
  if (Index2 < 0) or (Index2 >= FCount) then Exit;
  if Index1 = Index2 then Exit;

  Temp := FItems[Index1];
  FItems[Index1] := FItems[Index2];
  FItems[Index2] := Temp;

  { Adjust current index }
  if FCurrentIndex = Index1 then
    FCurrentIndex := Index2
  else if FCurrentIndex = Index2 then
    FCurrentIndex := Index1;

  DoChange;
end;

procedure TMockPlaylistManager.Reverse;
var
  I: Integer;
  Temp: TMockPlaylistItem;
begin
  LogCall('Reverse');
  for I := 0 to (FCount div 2) - 1 do
  begin
    Temp := FItems[I];
    FItems[I] := FItems[FCount - 1 - I];
    FItems[FCount - 1 - I] := Temp;
  end;

  if FCurrentIndex >= 0 then
    FCurrentIndex := FCount - 1 - FCurrentIndex;

  DoChange;
end;

procedure TMockPlaylistManager.Sort(Ascending: Boolean);
var
  I, J: Integer;
  Temp: TMockPlaylistItem;
begin
  LogCall('Sort(' + BoolToStr(Ascending, True) + ')');
  { Simple bubble sort for mock }
  for I := 0 to FCount - 2 do
    for J := 0 to FCount - 2 - I do
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
  DoChange;
end;

procedure TMockPlaylistManager.SortByTitle(Ascending: Boolean);
var
  I, J: Integer;
  Temp: TMockPlaylistItem;
begin
  LogCall('SortByTitle(' + BoolToStr(Ascending, True) + ')');
  for I := 0 to FCount - 2 do
    for J := 0 to FCount - 2 - I do
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
  DoChange;
end;

procedure TMockPlaylistManager.SortByArtist(Ascending: Boolean);
var
  I, J: Integer;
  Temp: TMockPlaylistItem;
begin
  LogCall('SortByArtist(' + BoolToStr(Ascending, True) + ')');
  for I := 0 to FCount - 2 do
    for J := 0 to FCount - 2 - I do
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
  DoChange;
end;

procedure TMockPlaylistManager.SortByDuration(Ascending: Boolean);
var
  I, J: Integer;
  Temp: TMockPlaylistItem;
begin
  LogCall('SortByDuration(' + BoolToStr(Ascending, True) + ')');
  for I := 0 to FCount - 2 do
    for J := 0 to FCount - 2 - I do
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
  DoChange;
end;

procedure TMockPlaylistManager.Randomize;
var
  I, J: Integer;
  Temp: TMockPlaylistItem;
begin
  LogCall('Randomize');
  { Actually shuffle the items array using Fisher-Yates }
  for I := FCount - 1 downto 1 do
  begin
    J := Random(I + 1);
    Temp := FItems[I];
    FItems[I] := FItems[J];
    FItems[J] := Temp;
  end;
  GenerateShuffleOrder;  { Also regenerate shuffle order }
  DoChange;
end;

function TMockPlaylistManager.GetNext: Integer;
begin
  case FPlaybackMode of
    pmNormal:
      if FCurrentIndex < FCount - 1 then
        Result := FCurrentIndex + 1
      else
        Result := -1;
    pmRepeatAll:
      if FCurrentIndex < FCount - 1 then
        Result := FCurrentIndex + 1
      else
        Result := 0;
    pmRepeatOne:
      Result := FCurrentIndex;
    pmShuffle:
      begin
        Inc(FShufflePosition);
        if FShufflePosition >= FCount then
          FShufflePosition := 0;
        if FCount > 0 then
          Result := FShuffleOrder[FShufflePosition]
        else
          Result := -1;
      end;
  else
    Result := -1;
  end;
end;

function TMockPlaylistManager.GetPrevious: Integer;
begin
  case FPlaybackMode of
    pmNormal:
      if FCurrentIndex > 0 then
        Result := FCurrentIndex - 1
      else
        Result := -1;
    pmRepeatAll:
      if FCurrentIndex > 0 then
        Result := FCurrentIndex - 1
      else
        Result := FCount - 1;
    pmRepeatOne:
      Result := FCurrentIndex;
    pmShuffle:
      begin
        Dec(FShufflePosition);
        if FShufflePosition < 0 then
          FShufflePosition := FCount - 1;
        if FCount > 0 then
          Result := FShuffleOrder[FShufflePosition]
        else
          Result := -1;
      end;
  else
    Result := -1;
  end;
end;

function TMockPlaylistManager.GetFirst: Integer;
begin
  if FCount > 0 then
    Result := 0
  else
    Result := -1;
end;

function TMockPlaylistManager.GetLast: Integer;
begin
  if FCount > 0 then
    Result := FCount - 1
  else
    Result := -1;
end;

function TMockPlaylistManager.GetRandom: Integer;
begin
  if FCount > 0 then
    Result := Random(FCount)
  else
    Result := -1;
end;

procedure TMockPlaylistManager.PlayIndex(Index: Integer);
begin
  LogCall('PlayIndex(' + IntToStr(Index) + ')');
  if (Index >= 0) and (Index < FCount) then
  begin
    SetCurrentIndex(Index);
    if Assigned(FOnPlay) then
      FOnPlay(Self, Index, FItems[Index].FileName);
  end;
end;

procedure TMockPlaylistManager.PlayNext;
var
  Index: Integer;
begin
  LogCall('PlayNext');
  Index := GetNext;
  if Index >= 0 then
    PlayIndex(Index);
end;

procedure TMockPlaylistManager.PlayPrevious;
var
  Index: Integer;
begin
  LogCall('PlayPrevious');
  Index := GetPrevious;
  if Index >= 0 then
    PlayIndex(Index);
end;

procedure TMockPlaylistManager.PlayFirst;
begin
  LogCall('PlayFirst');
  PlayIndex(GetFirst);
end;

procedure TMockPlaylistManager.PlayLast;
begin
  LogCall('PlayLast');
  PlayIndex(GetLast);
end;

function TMockPlaylistManager.Find(const AFileName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FCount - 1 do
    if SameText(FItems[I].FileName, AFileName) then
    begin
      Result := I;
      Exit;
    end;
end;

function TMockPlaylistManager.FindByTitle(const ATitle: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FCount - 1 do
    if Pos(LowerCase(ATitle), LowerCase(FItems[I].Title)) > 0 then
    begin
      Result := I;
      Exit;
    end;
end;

function TMockPlaylistManager.Search(const AText: string): TIntegerDynArray;
var
  I, MatchCount: Integer;
  LowerText: string;
begin
  SetLength(Result, 0);
  if AText = '' then Exit;

  LowerText := LowerCase(AText);
  MatchCount := 0;

  for I := 0 to FCount - 1 do
  begin
    if (Pos(LowerText, LowerCase(FItems[I].Title)) > 0) or
       (Pos(LowerText, LowerCase(FItems[I].Artist)) > 0) or
       (Pos(LowerText, LowerCase(FItems[I].FileName)) > 0) then
    begin
      Inc(MatchCount);
      SetLength(Result, MatchCount);
      Result[MatchCount - 1] := I;
    end;
  end;
end;

function TMockPlaylistManager.IndexOf(const AFileName: string): Integer;
begin
  Result := Find(AFileName);
end;

function TMockPlaylistManager.Contains(const AFileName: string): Boolean;
begin
  Result := Find(AFileName) >= 0;
end;

procedure TMockPlaylistManager.SelectAll;
var
  I: Integer;
begin
  LogCall('SelectAll');
  for I := 0 to FCount - 1 do
    FItems[I].Selected := True;
end;

procedure TMockPlaylistManager.SelectNone;
var
  I: Integer;
begin
  LogCall('SelectNone');
  for I := 0 to FCount - 1 do
    FItems[I].Selected := False;
end;

procedure TMockPlaylistManager.SelectInvert;
var
  I: Integer;
begin
  LogCall('SelectInvert');
  for I := 0 to FCount - 1 do
    FItems[I].Selected := not FItems[I].Selected;
end;

procedure TMockPlaylistManager.SetSelected(Index: Integer; Selected: Boolean);
begin
  if (Index >= 0) and (Index < FCount) then
    FItems[Index].Selected := Selected;
end;

function TMockPlaylistManager.GetSelectedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FCount - 1 do
    if FItems[I].Selected then
      Inc(Result);
end;

function TMockPlaylistManager.GetSelectedIndices: TIntegerDynArray;
var
  I, SelCount: Integer;
begin
  SetLength(Result, 0);
  SelCount := 0;
  for I := 0 to FCount - 1 do
    if FItems[I].Selected then
    begin
      Inc(SelCount);
      SetLength(Result, SelCount);
      Result[SelCount - 1] := I;
    end;
end;

procedure TMockPlaylistManager.DeleteSelected;
var
  I: Integer;
begin
  LogCall('DeleteSelected');
  for I := FCount - 1 downto 0 do
    if FItems[I].Selected then
      Delete(I);
end;

function TMockPlaylistManager.LoadFromFile(const AFileName: string): Boolean;
begin
  LogCall('LoadFromFile(' + AFileName + ')');
  FFileName := AFileName;
  Result := True;  { Mock always succeeds }
end;

function TMockPlaylistManager.SaveToFile(const AFileName: string): Boolean;
begin
  LogCall('SaveToFile(' + AFileName + ')');
  FFileName := AFileName;
  FModified := False;
  Result := True;  { Mock always succeeds }
end;

function TMockPlaylistManager.LoadM3U(const AFileName: string): Boolean;
begin
  LogCall('LoadM3U(' + AFileName + ')');
  Result := LoadFromFile(AFileName);
end;

function TMockPlaylistManager.LoadPLS(const AFileName: string): Boolean;
begin
  LogCall('LoadPLS(' + AFileName + ')');
  Result := LoadFromFile(AFileName);
end;

function TMockPlaylistManager.SaveM3U(const AFileName: string): Boolean;
begin
  LogCall('SaveM3U(' + AFileName + ')');
  Result := SaveToFile(AFileName);
end;

function TMockPlaylistManager.SavePLS(const AFileName: string): Boolean;
begin
  LogCall('SavePLS(' + AFileName + ')');
  Result := SaveToFile(AFileName);
end;

function TMockPlaylistManager.ParseM3U(const Content: string): Integer;
var
  Lines: TStringList;
  I: Integer;
  Line: string;
begin
  LogCall('ParseM3U');
  Result := 0;
  Lines := TStringList.Create;
  try
    Lines.Text := Content;
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      if (Line <> '') and (Line[1] <> '#') then
      begin
        Add(Line);
        Inc(Result);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TMockPlaylistManager.ParsePLS(const Content: string): Integer;
var
  Lines: TStringList;
  I: Integer;
  Line, Key, Value: string;
  EqPos: Integer;
begin
  LogCall('ParsePLS');
  Result := 0;
  Lines := TStringList.Create;
  try
    Lines.Text := Content;
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      EqPos := Pos('=', Line);
      if EqPos > 0 then
      begin
        Key := LowerCase(Copy(Line, 1, EqPos - 1));
        Value := Copy(Line, EqPos + 1, Length(Line));
        if Pos('file', Key) = 1 then
        begin
          Add(Value);
          Inc(Result);
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TMockPlaylistManager.GenerateM3U: string;
var
  I: Integer;
begin
  Result := '#EXTM3U' + LineEnding;
  for I := 0 to FCount - 1 do
  begin
    Result := Result + '#EXTINF:' + IntToStr(Round(FItems[I].Duration)) +
              ',' + FItems[I].Title + LineEnding;
    Result := Result + FItems[I].FileName + LineEnding;
  end;
end;

function TMockPlaylistManager.GeneratePLS: string;
var
  I: Integer;
begin
  Result := '[playlist]' + LineEnding;
  for I := 0 to FCount - 1 do
  begin
    Result := Result + 'File' + IntToStr(I + 1) + '=' + FItems[I].FileName + LineEnding;
    Result := Result + 'Title' + IntToStr(I + 1) + '=' + FItems[I].Title + LineEnding;
    Result := Result + 'Length' + IntToStr(I + 1) + '=' + IntToStr(Round(FItems[I].Duration)) + LineEnding;
  end;
  Result := Result + 'NumberOfEntries=' + IntToStr(FCount) + LineEnding;
  Result := Result + 'Version=2' + LineEnding;
end;

procedure TMockPlaylistManager.UpdateItemInfo(Index: Integer; const Title, Artist, Album: string;
  Duration: Double);
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    FItems[Index].Title := Title;
    FItems[Index].Artist := Artist;
    FItems[Index].Album := Album;
    FItems[Index].Duration := Duration;
    DoChange;
  end;
end;

procedure TMockPlaylistManager.UpdateItemDuration(Index: Integer; Duration: Double);
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    FItems[Index].Duration := Duration;
    DoChange;
  end;
end;

procedure TMockPlaylistManager.MarkAsPlayed(Index: Integer);
begin
  if (Index >= 0) and (Index < FCount) then
    FItems[Index].Played := True;
end;

procedure TMockPlaylistManager.ResetPlayedStatus;
var
  I: Integer;
begin
  LogCall('ResetPlayedStatus');
  for I := 0 to FCount - 1 do
    FItems[I].Played := False;
end;

function TMockPlaylistManager.GetTotalDuration: Double;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FCount - 1 do
    Result := Result + FItems[I].Duration;
end;

function TMockPlaylistManager.GetTotalDurationString: string;
begin
  Result := FormatDuration(GetTotalDuration);
end;

function TMockPlaylistManager.GetPlayedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FCount - 1 do
    if FItems[I].Played then
      Inc(Result);
end;

function TMockPlaylistManager.GetUnplayedCount: Integer;
begin
  Result := FCount - GetPlayedCount;
end;

function TMockPlaylistManager.GetCallLog: TStringList;
begin
  Result := FCallLog;
end;

procedure TMockPlaylistManager.ClearCallLog;
begin
  FCallLog.Clear;
end;

procedure TMockPlaylistManager.PopulateWithTestData(ItemCount: Integer);
var
  I: Integer;
  Item: TMockPlaylistItem;
begin
  Clear;
  for I := 1 to ItemCount do
  begin
    Item := Default(TMockPlaylistItem);
    Item.FileName := '/music/track' + IntToStr(I) + '.mp3';
    Item.Title := 'Track ' + IntToStr(I);
    Item.Artist := 'Artist ' + IntToStr((I mod 5) + 1);
    Item.Album := 'Album ' + IntToStr((I mod 3) + 1);
    Item.Duration := 180 + Random(120);  { 3-5 minutes }
    Add(Item);
  end;
end;

end.
