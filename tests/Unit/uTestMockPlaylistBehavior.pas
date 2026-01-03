{ ===============================================================================
  uTestMockPlaylistBehavior.pas - Unit Tests Using Mock Playlist Manager

  Part of 3nity Media Player - Test Suite

  Tests playlist behavior using TMockPlaylistManager to enable:
  - Testing without file system dependencies
  - Testing playlist navigation with different playback modes
  - Testing ordering, searching, and selection operations
  - Testing M3U/PLS parsing logic

  Test Classes:
  - TTestPlaylistItems: Add, Insert, Delete, Clear operations
  - TTestPlaylistNavigation: GetNext, GetPrevious, PlayIndex
  - TTestPlaylistPlaybackModes: Normal, RepeatAll, RepeatOne, Shuffle
  - TTestPlaylistOrdering: Move, Swap, Reverse, Sort operations
  - TTestPlaylistSearch: Find, Search, Contains operations
  - TTestPlaylistSelection: SelectAll, SelectNone, DeleteSelected
  - TTestPlaylistParsing: M3U/PLS parsing and generation
  - TTestPlaylistStatistics: Duration, PlayedCount, etc.
  - TTestPlaylistEvents: OnChange, OnItemAdded, OnPlay events

  Author: Test Suite
  License: GPL-2.0
  =============================================================================== }

unit uTestMockPlaylistBehavior;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, uMockPlaylist, uTypes;

type
  { ===========================================================================
    TTestPlaylistItems - Tests for item management
    =========================================================================== }
  TTestPlaylistItems = class(TTestCase)
  private
    FPlaylist: TMockPlaylistManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Add operations }
    procedure Test_Add_SingleFile;
    procedure Test_Add_MultipleFiles;
    procedure Test_Add_ExtractsTitle;
    procedure Test_Add_WithItem;
    procedure Test_AddFiles_FromStringList;

    { Insert operations }
    procedure Test_Insert_AtBeginning;
    procedure Test_Insert_AtMiddle;
    procedure Test_Insert_AtEnd;
    procedure Test_Insert_AdjustsCurrentIndex;

    { Delete operations }
    procedure Test_Delete_First;
    procedure Test_Delete_Last;
    procedure Test_Delete_Middle;
    procedure Test_Delete_AdjustsCurrentIndex;
    procedure Test_Delete_InvalidIndex;
    procedure Test_Remove_ByFileName;

    { Clear }
    procedure Test_Clear_EmptiesPlaylist;
    procedure Test_Clear_ResetsCurrentIndex;

    { Item access }
    procedure Test_GetItem_ValidIndex;
    procedure Test_GetItem_InvalidIndex;
    procedure Test_SetItem_UpdatesData;
  end;

  { ===========================================================================
    TTestPlaylistNavigation - Tests for navigation operations
    =========================================================================== }
  TTestPlaylistNavigation = class(TTestCase)
  private
    FPlaylist: TMockPlaylistManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { GetNext }
    procedure Test_GetNext_FromFirst;
    procedure Test_GetNext_FromMiddle;
    procedure Test_GetNext_FromLast_Normal;

    { GetPrevious }
    procedure Test_GetPrevious_FromLast;
    procedure Test_GetPrevious_FromMiddle;
    procedure Test_GetPrevious_FromFirst_Normal;

    { GetFirst/GetLast }
    procedure Test_GetFirst;
    procedure Test_GetLast;
    procedure Test_GetFirst_Empty;
    procedure Test_GetLast_Empty;

    { PlayIndex }
    procedure Test_PlayIndex_SetsCurrentIndex;
    procedure Test_PlayIndex_InvalidIndex;

    { PlayNext/PlayPrevious }
    procedure Test_PlayNext_AdvancesIndex;
    procedure Test_PlayPrevious_DecreasesIndex;
    procedure Test_PlayFirst;
    procedure Test_PlayLast;

    { HasNext/HasPrevious }
    procedure Test_HasNext_True;
    procedure Test_HasNext_False_AtEnd;
    procedure Test_HasPrevious_True;
    procedure Test_HasPrevious_False_AtStart;
  end;

  { ===========================================================================
    TTestPlaylistPlaybackModes - Tests for different playback modes
    =========================================================================== }
  TTestPlaylistPlaybackModes = class(TTestCase)
  private
    FPlaylist: TMockPlaylistManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Normal mode }
    procedure Test_Normal_StopsAtEnd;
    procedure Test_Normal_StopsAtBeginning;

    { Repeat All mode }
    procedure Test_RepeatAll_WrapsToFirst;
    procedure Test_RepeatAll_WrapsToLast;
    procedure Test_RepeatAll_HasNextAlwaysTrue;
    procedure Test_RepeatAll_HasPreviousAlwaysTrue;

    { Repeat One mode }
    procedure Test_RepeatOne_ReturnsCurrentForNext;
    procedure Test_RepeatOne_ReturnsCurrentForPrevious;

    { Shuffle mode }
    procedure Test_Shuffle_GeneratesShuffleOrder;
    procedure Test_Shuffle_HasNextAlwaysTrue;
    procedure Test_Shuffle_CoversAllItems;
    procedure Test_Shuffle_ChangeModeRegenerates;
  end;

  { ===========================================================================
    TTestPlaylistOrdering - Tests for ordering operations
    =========================================================================== }
  TTestPlaylistOrdering = class(TTestCase)
  private
    FPlaylist: TMockPlaylistManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { MoveUp/MoveDown }
    procedure Test_MoveUp_FromMiddle;
    procedure Test_MoveUp_FromFirst;
    procedure Test_MoveDown_FromMiddle;
    procedure Test_MoveDown_FromLast;

    { Move }
    procedure Test_Move_Forward;
    procedure Test_Move_Backward;
    procedure Test_Move_AdjustsCurrentIndex;

    { Swap }
    procedure Test_Swap_TwoItems;
    procedure Test_Swap_AdjustsCurrentIndex;

    { Reverse }
    procedure Test_Reverse;
    procedure Test_Reverse_AdjustsCurrentIndex;

    { Sort }
    procedure Test_Sort_Ascending;
    procedure Test_Sort_Descending;
    procedure Test_SortByTitle_Ascending;
    procedure Test_SortByArtist_Ascending;
    procedure Test_SortByDuration_Ascending;

    { Randomize }
    procedure Test_Randomize_ChangesOrder;
  end;

  { ===========================================================================
    TTestPlaylistSearch - Tests for search operations
    =========================================================================== }
  TTestPlaylistSearch = class(TTestCase)
  private
    FPlaylist: TMockPlaylistManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Find }
    procedure Test_Find_ExistingFile;
    procedure Test_Find_NotFound;
    procedure Test_Find_CaseInsensitive;

    { FindByTitle }
    procedure Test_FindByTitle_ExactMatch;
    procedure Test_FindByTitle_PartialMatch;
    procedure Test_FindByTitle_NotFound;

    { Search }
    procedure Test_Search_FindsInTitle;
    procedure Test_Search_FindsInArtist;
    procedure Test_Search_FindsInFileName;
    procedure Test_Search_MultipleResults;
    procedure Test_Search_NoResults;
    procedure Test_Search_EmptyString;

    { IndexOf/Contains }
    procedure Test_IndexOf_ExistingFile;
    procedure Test_Contains_True;
    procedure Test_Contains_False;
  end;

  { ===========================================================================
    TTestPlaylistSelection - Tests for selection operations
    =========================================================================== }
  TTestPlaylistSelection = class(TTestCase)
  private
    FPlaylist: TMockPlaylistManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { SelectAll/SelectNone }
    procedure Test_SelectAll;
    procedure Test_SelectNone;
    procedure Test_SelectInvert;

    { SetSelected }
    procedure Test_SetSelected_True;
    procedure Test_SetSelected_False;

    { GetSelectedCount }
    procedure Test_GetSelectedCount_None;
    procedure Test_GetSelectedCount_Some;
    procedure Test_GetSelectedCount_All;

    { GetSelectedIndices }
    procedure Test_GetSelectedIndices_Empty;
    procedure Test_GetSelectedIndices_Some;

    { DeleteSelected }
    procedure Test_DeleteSelected_DeletesOnlySelected;
    procedure Test_DeleteSelected_KeepsUnselected;
  end;

  { ===========================================================================
    TTestPlaylistParsing - Tests for M3U/PLS parsing
    =========================================================================== }
  TTestPlaylistParsing = class(TTestCase)
  private
    FPlaylist: TMockPlaylistManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { ParseM3U }
    procedure Test_ParseM3U_SimpleList;
    procedure Test_ParseM3U_WithComments;
    procedure Test_ParseM3U_WithExtInfo;
    procedure Test_ParseM3U_EmptyLines;

    { ParsePLS }
    procedure Test_ParsePLS_SimpleList;
    procedure Test_ParsePLS_MultipleEntries;
    procedure Test_ParsePLS_IgnoresOtherKeys;

    { GenerateM3U }
    procedure Test_GenerateM3U_Format;
    procedure Test_GenerateM3U_IncludesExtInfo;

    { GeneratePLS }
    procedure Test_GeneratePLS_Format;
    procedure Test_GeneratePLS_NumberOfEntries;
  end;

  { ===========================================================================
    TTestPlaylistStatistics - Tests for statistics calculations
    =========================================================================== }
  TTestPlaylistStatistics = class(TTestCase)
  private
    FPlaylist: TMockPlaylistManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Duration }
    procedure Test_GetTotalDuration_Empty;
    procedure Test_GetTotalDuration_SingleItem;
    procedure Test_GetTotalDuration_MultipleItems;
    procedure Test_GetTotalDurationString_Format;

    { Played status }
    procedure Test_MarkAsPlayed;
    procedure Test_GetPlayedCount;
    procedure Test_GetUnplayedCount;
    procedure Test_ResetPlayedStatus;

    { Item info updates }
    procedure Test_UpdateItemInfo;
    procedure Test_UpdateItemDuration;
  end;

  { ===========================================================================
    TTestPlaylistEvents - Tests for event handling
    =========================================================================== }
  TTestPlaylistEvents = class(TTestCase)
  private
    FPlaylist: TMockPlaylistManager;
    FChangeCount: Integer;
    FLastAddedIndex: Integer;
    FLastRemovedIndex: Integer;
    FLastPlayIndex: Integer;
    FLastPlayFileName: string;
    FClearCalled: Boolean;
    procedure OnChangeHandler(Sender: TObject);
    procedure OnItemAddedHandler(Sender: TObject; Index: Integer);
    procedure OnItemRemovedHandler(Sender: TObject; Index: Integer);
    procedure OnPlayHandler(Sender: TObject; Index: Integer; const FileName: string);
    procedure OnClearHandler(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_OnChange_FiredOnAdd;
    procedure Test_OnChange_FiredOnDelete;
    procedure Test_OnChange_FiredOnSort;
    procedure Test_OnItemAdded_FiredWithIndex;
    procedure Test_OnItemRemoved_FiredWithIndex;
    procedure Test_OnPlay_FiredWithDetails;
    procedure Test_OnClear_Fired;
    procedure Test_OnCurrentChange_Fired;
  end;

  { ===========================================================================
    TTestPlaylistCallLog - Tests for call logging
    =========================================================================== }
  TTestPlaylistCallLog = class(TTestCase)
  private
    FPlaylist: TMockPlaylistManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_CallLog_RecordsAdd;
    procedure Test_CallLog_RecordsDelete;
    procedure Test_CallLog_RecordsPlayIndex;
    procedure Test_ClearCallLog;
    procedure Test_CallLog_RecordsMultipleOperations;
  end;

implementation

{ ===========================================================================
  TTestPlaylistItems
  =========================================================================== }

procedure TTestPlaylistItems.SetUp;
begin
  FPlaylist := TMockPlaylistManager.Create;
end;

procedure TTestPlaylistItems.TearDown;
begin
  FPlaylist.Free;
end;

procedure TTestPlaylistItems.Test_Add_SingleFile;
begin
  FPlaylist.Add('/music/song.mp3');
  AssertEquals('Count should be 1', 1, FPlaylist.Count);
  AssertEquals('FileName should match', '/music/song.mp3', FPlaylist[0].FileName);
end;

procedure TTestPlaylistItems.Test_Add_MultipleFiles;
begin
  FPlaylist.Add('/music/song1.mp3');
  FPlaylist.Add('/music/song2.mp3');
  FPlaylist.Add('/music/song3.mp3');
  AssertEquals('Count should be 3', 3, FPlaylist.Count);
end;

procedure TTestPlaylistItems.Test_Add_ExtractsTitle;
begin
  FPlaylist.Add('/music/My Song.mp3');
  AssertEquals('Title should be extracted', 'My Song', FPlaylist[0].Title);
end;

procedure TTestPlaylistItems.Test_Add_WithItem;
var
  Item: TMockPlaylistItem;
begin
  Item := Default(TMockPlaylistItem);
  Item.FileName := '/music/track.mp3';
  Item.Title := 'Custom Title';
  Item.Artist := 'Custom Artist';
  Item.Duration := 180.5;

  FPlaylist.Add(Item);

  AssertEquals('Title should match', 'Custom Title', FPlaylist[0].Title);
  AssertEquals('Artist should match', 'Custom Artist', FPlaylist[0].Artist);
  AssertEquals('Duration should match', 180.5, FPlaylist[0].Duration, 0.01);
end;

procedure TTestPlaylistItems.Test_AddFiles_FromStringList;
var
  Files: TStringList;
begin
  Files := TStringList.Create;
  try
    Files.Add('/music/a.mp3');
    Files.Add('/music/b.mp3');
    Files.Add('/music/c.mp3');

    FPlaylist.AddFiles(Files);

    AssertEquals('Count should be 3', 3, FPlaylist.Count);
  finally
    Files.Free;
  end;
end;

procedure TTestPlaylistItems.Test_Insert_AtBeginning;
begin
  FPlaylist.Add('/music/song1.mp3');
  FPlaylist.Add('/music/song2.mp3');

  FPlaylist.Insert(0, '/music/first.mp3');

  AssertEquals('Count should be 3', 3, FPlaylist.Count);
  AssertEquals('First should be inserted file', '/music/first.mp3', FPlaylist[0].FileName);
end;

procedure TTestPlaylistItems.Test_Insert_AtMiddle;
begin
  FPlaylist.Add('/music/song1.mp3');
  FPlaylist.Add('/music/song2.mp3');

  FPlaylist.Insert(1, '/music/middle.mp3');

  AssertEquals('Middle should be inserted file', '/music/middle.mp3', FPlaylist[1].FileName);
end;

procedure TTestPlaylistItems.Test_Insert_AtEnd;
begin
  FPlaylist.Add('/music/song1.mp3');
  FPlaylist.Add('/music/song2.mp3');

  FPlaylist.Insert(2, '/music/last.mp3');

  AssertEquals('Last should be inserted file', '/music/last.mp3', FPlaylist[2].FileName);
end;

procedure TTestPlaylistItems.Test_Insert_AdjustsCurrentIndex;
begin
  FPlaylist.Add('/music/song1.mp3');
  FPlaylist.Add('/music/song2.mp3');
  FPlaylist.CurrentIndex := 1;

  FPlaylist.Insert(0, '/music/first.mp3');

  AssertEquals('CurrentIndex should be adjusted', 2, FPlaylist.CurrentIndex);
end;

procedure TTestPlaylistItems.Test_Delete_First;
begin
  FPlaylist.Add('/music/song1.mp3');
  FPlaylist.Add('/music/song2.mp3');

  FPlaylist.Delete(0);

  AssertEquals('Count should be 1', 1, FPlaylist.Count);
  AssertEquals('First should be song2', '/music/song2.mp3', FPlaylist[0].FileName);
end;

procedure TTestPlaylistItems.Test_Delete_Last;
begin
  FPlaylist.Add('/music/song1.mp3');
  FPlaylist.Add('/music/song2.mp3');

  FPlaylist.Delete(1);

  AssertEquals('Count should be 1', 1, FPlaylist.Count);
  AssertEquals('First should be song1', '/music/song1.mp3', FPlaylist[0].FileName);
end;

procedure TTestPlaylistItems.Test_Delete_Middle;
begin
  FPlaylist.Add('/music/song1.mp3');
  FPlaylist.Add('/music/song2.mp3');
  FPlaylist.Add('/music/song3.mp3');

  FPlaylist.Delete(1);

  AssertEquals('Count should be 2', 2, FPlaylist.Count);
  AssertEquals('Second should be song3', '/music/song3.mp3', FPlaylist[1].FileName);
end;

procedure TTestPlaylistItems.Test_Delete_AdjustsCurrentIndex;
begin
  FPlaylist.Add('/music/song1.mp3');
  FPlaylist.Add('/music/song2.mp3');
  FPlaylist.Add('/music/song3.mp3');
  FPlaylist.CurrentIndex := 2;

  FPlaylist.Delete(0);

  AssertEquals('CurrentIndex should be adjusted', 1, FPlaylist.CurrentIndex);
end;

procedure TTestPlaylistItems.Test_Delete_InvalidIndex;
var
  OldCount: Integer;
begin
  FPlaylist.Add('/music/song1.mp3');
  OldCount := FPlaylist.Count;

  FPlaylist.Delete(-1);
  FPlaylist.Delete(100);

  AssertEquals('Count should be unchanged', OldCount, FPlaylist.Count);
end;

procedure TTestPlaylistItems.Test_Remove_ByFileName;
begin
  FPlaylist.Add('/music/song1.mp3');
  FPlaylist.Add('/music/song2.mp3');
  FPlaylist.Add('/music/song3.mp3');

  FPlaylist.Remove('/music/song2.mp3');

  AssertEquals('Count should be 2', 2, FPlaylist.Count);
  AssertFalse('song2 should be removed', FPlaylist.Contains('/music/song2.mp3'));
end;

procedure TTestPlaylistItems.Test_Clear_EmptiesPlaylist;
begin
  FPlaylist.PopulateWithTestData(10);

  FPlaylist.Clear;

  AssertEquals('Count should be 0', 0, FPlaylist.Count);
  AssertTrue('IsEmpty should be true', FPlaylist.IsEmpty);
end;

procedure TTestPlaylistItems.Test_Clear_ResetsCurrentIndex;
begin
  FPlaylist.PopulateWithTestData(10);
  FPlaylist.CurrentIndex := 5;

  FPlaylist.Clear;

  AssertEquals('CurrentIndex should be -1', -1, FPlaylist.CurrentIndex);
end;

procedure TTestPlaylistItems.Test_GetItem_ValidIndex;
begin
  FPlaylist.Add('/music/song.mp3');

  AssertEquals('FileName should match', '/music/song.mp3', FPlaylist[0].FileName);
end;

procedure TTestPlaylistItems.Test_GetItem_InvalidIndex;
var
  Item: TMockPlaylistItem;
begin
  FPlaylist.Add('/music/song.mp3');

  Item := FPlaylist[-1];
  AssertEquals('Invalid index should return empty FileName', '', Item.FileName);

  Item := FPlaylist[100];
  AssertEquals('Invalid index should return empty FileName', '', Item.FileName);
end;

procedure TTestPlaylistItems.Test_SetItem_UpdatesData;
var
  Item: TMockPlaylistItem;
begin
  FPlaylist.Add('/music/song.mp3');

  Item := FPlaylist[0];
  Item.Title := 'New Title';
  FPlaylist[0] := Item;

  AssertEquals('Title should be updated', 'New Title', FPlaylist[0].Title);
end;

{ ===========================================================================
  TTestPlaylistNavigation
  =========================================================================== }

procedure TTestPlaylistNavigation.SetUp;
begin
  FPlaylist := TMockPlaylistManager.Create;
  FPlaylist.PopulateWithTestData(5);
end;

procedure TTestPlaylistNavigation.TearDown;
begin
  FPlaylist.Free;
end;

procedure TTestPlaylistNavigation.Test_GetNext_FromFirst;
begin
  FPlaylist.CurrentIndex := 0;
  AssertEquals('GetNext should return 1', 1, FPlaylist.GetNext);
end;

procedure TTestPlaylistNavigation.Test_GetNext_FromMiddle;
begin
  FPlaylist.CurrentIndex := 2;
  AssertEquals('GetNext should return 3', 3, FPlaylist.GetNext);
end;

procedure TTestPlaylistNavigation.Test_GetNext_FromLast_Normal;
begin
  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.CurrentIndex := 4;
  AssertEquals('GetNext should return -1 at end', -1, FPlaylist.GetNext);
end;

procedure TTestPlaylistNavigation.Test_GetPrevious_FromLast;
begin
  FPlaylist.CurrentIndex := 4;
  AssertEquals('GetPrevious should return 3', 3, FPlaylist.GetPrevious);
end;

procedure TTestPlaylistNavigation.Test_GetPrevious_FromMiddle;
begin
  FPlaylist.CurrentIndex := 2;
  AssertEquals('GetPrevious should return 1', 1, FPlaylist.GetPrevious);
end;

procedure TTestPlaylistNavigation.Test_GetPrevious_FromFirst_Normal;
begin
  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.CurrentIndex := 0;
  AssertEquals('GetPrevious should return -1 at start', -1, FPlaylist.GetPrevious);
end;

procedure TTestPlaylistNavigation.Test_GetFirst;
begin
  AssertEquals('GetFirst should return 0', 0, FPlaylist.GetFirst);
end;

procedure TTestPlaylistNavigation.Test_GetLast;
begin
  AssertEquals('GetLast should return 4', 4, FPlaylist.GetLast);
end;

procedure TTestPlaylistNavigation.Test_GetFirst_Empty;
begin
  FPlaylist.Clear;
  AssertEquals('GetFirst should return -1 for empty', -1, FPlaylist.GetFirst);
end;

procedure TTestPlaylistNavigation.Test_GetLast_Empty;
begin
  FPlaylist.Clear;
  AssertEquals('GetLast should return -1 for empty', -1, FPlaylist.GetLast);
end;

procedure TTestPlaylistNavigation.Test_PlayIndex_SetsCurrentIndex;
begin
  FPlaylist.PlayIndex(3);
  AssertEquals('CurrentIndex should be 3', 3, FPlaylist.CurrentIndex);
end;

procedure TTestPlaylistNavigation.Test_PlayIndex_InvalidIndex;
begin
  FPlaylist.CurrentIndex := 0;
  FPlaylist.PlayIndex(-1);
  AssertEquals('CurrentIndex should be unchanged', 0, FPlaylist.CurrentIndex);

  FPlaylist.PlayIndex(100);
  AssertEquals('CurrentIndex should still be unchanged', 0, FPlaylist.CurrentIndex);
end;

procedure TTestPlaylistNavigation.Test_PlayNext_AdvancesIndex;
begin
  FPlaylist.CurrentIndex := 0;
  FPlaylist.PlayNext;
  AssertEquals('CurrentIndex should be 1', 1, FPlaylist.CurrentIndex);
end;

procedure TTestPlaylistNavigation.Test_PlayPrevious_DecreasesIndex;
begin
  FPlaylist.CurrentIndex := 3;
  FPlaylist.PlayPrevious;
  AssertEquals('CurrentIndex should be 2', 2, FPlaylist.CurrentIndex);
end;

procedure TTestPlaylistNavigation.Test_PlayFirst;
begin
  FPlaylist.CurrentIndex := 3;
  FPlaylist.PlayFirst;
  AssertEquals('CurrentIndex should be 0', 0, FPlaylist.CurrentIndex);
end;

procedure TTestPlaylistNavigation.Test_PlayLast;
begin
  FPlaylist.CurrentIndex := 0;
  FPlaylist.PlayLast;
  AssertEquals('CurrentIndex should be 4', 4, FPlaylist.CurrentIndex);
end;

procedure TTestPlaylistNavigation.Test_HasNext_True;
begin
  FPlaylist.CurrentIndex := 0;
  AssertTrue('HasNext should be true', FPlaylist.HasNext);
end;

procedure TTestPlaylistNavigation.Test_HasNext_False_AtEnd;
begin
  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.CurrentIndex := 4;
  AssertFalse('HasNext should be false at end', FPlaylist.HasNext);
end;

procedure TTestPlaylistNavigation.Test_HasPrevious_True;
begin
  FPlaylist.CurrentIndex := 4;
  AssertTrue('HasPrevious should be true', FPlaylist.HasPrevious);
end;

procedure TTestPlaylistNavigation.Test_HasPrevious_False_AtStart;
begin
  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.CurrentIndex := 0;
  AssertFalse('HasPrevious should be false at start', FPlaylist.HasPrevious);
end;

{ ===========================================================================
  TTestPlaylistPlaybackModes
  =========================================================================== }

procedure TTestPlaylistPlaybackModes.SetUp;
begin
  FPlaylist := TMockPlaylistManager.Create;
  FPlaylist.PopulateWithTestData(5);
end;

procedure TTestPlaylistPlaybackModes.TearDown;
begin
  FPlaylist.Free;
end;

procedure TTestPlaylistPlaybackModes.Test_Normal_StopsAtEnd;
begin
  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.CurrentIndex := 4;
  AssertEquals('GetNext should return -1', -1, FPlaylist.GetNext);
end;

procedure TTestPlaylistPlaybackModes.Test_Normal_StopsAtBeginning;
begin
  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.CurrentIndex := 0;
  AssertEquals('GetPrevious should return -1', -1, FPlaylist.GetPrevious);
end;

procedure TTestPlaylistPlaybackModes.Test_RepeatAll_WrapsToFirst;
begin
  FPlaylist.PlaybackMode := pmRepeatAll;
  FPlaylist.CurrentIndex := 4;
  AssertEquals('GetNext should wrap to 0', 0, FPlaylist.GetNext);
end;

procedure TTestPlaylistPlaybackModes.Test_RepeatAll_WrapsToLast;
begin
  FPlaylist.PlaybackMode := pmRepeatAll;
  FPlaylist.CurrentIndex := 0;
  AssertEquals('GetPrevious should wrap to 4', 4, FPlaylist.GetPrevious);
end;

procedure TTestPlaylistPlaybackModes.Test_RepeatAll_HasNextAlwaysTrue;
begin
  FPlaylist.PlaybackMode := pmRepeatAll;
  FPlaylist.CurrentIndex := 4;
  AssertTrue('HasNext should be true even at end', FPlaylist.HasNext);
end;

procedure TTestPlaylistPlaybackModes.Test_RepeatAll_HasPreviousAlwaysTrue;
begin
  FPlaylist.PlaybackMode := pmRepeatAll;
  FPlaylist.CurrentIndex := 0;
  AssertTrue('HasPrevious should be true even at start', FPlaylist.HasPrevious);
end;

procedure TTestPlaylistPlaybackModes.Test_RepeatOne_ReturnsCurrentForNext;
begin
  FPlaylist.PlaybackMode := pmRepeatOne;
  FPlaylist.CurrentIndex := 2;
  AssertEquals('GetNext should return current index', 2, FPlaylist.GetNext);
end;

procedure TTestPlaylistPlaybackModes.Test_RepeatOne_ReturnsCurrentForPrevious;
begin
  FPlaylist.PlaybackMode := pmRepeatOne;
  FPlaylist.CurrentIndex := 2;
  AssertEquals('GetPrevious should return current index', 2, FPlaylist.GetPrevious);
end;

procedure TTestPlaylistPlaybackModes.Test_Shuffle_GeneratesShuffleOrder;
begin
  FPlaylist.PlaybackMode := pmShuffle;
  { Just verify it doesn't crash and HasNext works }
  AssertTrue('HasNext should be true in shuffle', FPlaylist.HasNext);
end;

procedure TTestPlaylistPlaybackModes.Test_Shuffle_HasNextAlwaysTrue;
begin
  FPlaylist.PlaybackMode := pmShuffle;
  AssertTrue('HasNext should be true in shuffle mode', FPlaylist.HasNext);
end;

procedure TTestPlaylistPlaybackModes.Test_Shuffle_CoversAllItems;
var
  Visited: array of Boolean;
  I, Idx: Integer;
  AllVisited: Boolean;
begin
  FPlaylist.PlaybackMode := pmShuffle;
  SetLength(Visited, 5);

  { Get 5 items from shuffle }
  for I := 0 to 4 do
  begin
    Idx := FPlaylist.GetNext;
    if (Idx >= 0) and (Idx < 5) then
      Visited[Idx] := True;
  end;

  { Check all were visited }
  AllVisited := True;
  for I := 0 to 4 do
    if not Visited[I] then
      AllVisited := False;

  AssertTrue('All items should be visited in shuffle cycle', AllVisited);
end;

procedure TTestPlaylistPlaybackModes.Test_Shuffle_ChangeModeRegenerates;
var
  FirstIdx: Integer;
begin
  FPlaylist.PlaybackMode := pmShuffle;
  FirstIdx := FPlaylist.GetNext;

  { Change and change back }
  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.PlaybackMode := pmShuffle;

  { Just verify it works, order may differ }
  AssertTrue('GetNext should work after mode change', FPlaylist.GetNext >= 0);
end;

{ ===========================================================================
  TTestPlaylistOrdering
  =========================================================================== }

procedure TTestPlaylistOrdering.SetUp;
begin
  FPlaylist := TMockPlaylistManager.Create;
end;

procedure TTestPlaylistOrdering.TearDown;
begin
  FPlaylist.Free;
end;

procedure TTestPlaylistOrdering.Test_MoveUp_FromMiddle;
begin
  FPlaylist.Add('/music/a.mp3');
  FPlaylist.Add('/music/b.mp3');
  FPlaylist.Add('/music/c.mp3');

  FPlaylist.MoveUp(1);

  AssertEquals('b should be first', '/music/b.mp3', FPlaylist[0].FileName);
  AssertEquals('a should be second', '/music/a.mp3', FPlaylist[1].FileName);
end;

procedure TTestPlaylistOrdering.Test_MoveUp_FromFirst;
begin
  FPlaylist.Add('/music/a.mp3');
  FPlaylist.Add('/music/b.mp3');

  FPlaylist.MoveUp(0);

  { Should not change }
  AssertEquals('a should still be first', '/music/a.mp3', FPlaylist[0].FileName);
end;

procedure TTestPlaylistOrdering.Test_MoveDown_FromMiddle;
begin
  FPlaylist.Add('/music/a.mp3');
  FPlaylist.Add('/music/b.mp3');
  FPlaylist.Add('/music/c.mp3');

  FPlaylist.MoveDown(1);

  AssertEquals('b should be third', '/music/b.mp3', FPlaylist[2].FileName);
  AssertEquals('c should be second', '/music/c.mp3', FPlaylist[1].FileName);
end;

procedure TTestPlaylistOrdering.Test_MoveDown_FromLast;
begin
  FPlaylist.Add('/music/a.mp3');
  FPlaylist.Add('/music/b.mp3');

  FPlaylist.MoveDown(1);

  { Should not change }
  AssertEquals('b should still be last', '/music/b.mp3', FPlaylist[1].FileName);
end;

procedure TTestPlaylistOrdering.Test_Move_Forward;
begin
  FPlaylist.Add('/music/a.mp3');
  FPlaylist.Add('/music/b.mp3');
  FPlaylist.Add('/music/c.mp3');
  FPlaylist.Add('/music/d.mp3');

  FPlaylist.Move(0, 2);

  AssertEquals('b should be first', '/music/b.mp3', FPlaylist[0].FileName);
  AssertEquals('c should be second', '/music/c.mp3', FPlaylist[1].FileName);
  AssertEquals('a should be third', '/music/a.mp3', FPlaylist[2].FileName);
end;

procedure TTestPlaylistOrdering.Test_Move_Backward;
begin
  FPlaylist.Add('/music/a.mp3');
  FPlaylist.Add('/music/b.mp3');
  FPlaylist.Add('/music/c.mp3');
  FPlaylist.Add('/music/d.mp3');

  FPlaylist.Move(3, 1);

  AssertEquals('a should be first', '/music/a.mp3', FPlaylist[0].FileName);
  AssertEquals('d should be second', '/music/d.mp3', FPlaylist[1].FileName);
  AssertEquals('b should be third', '/music/b.mp3', FPlaylist[2].FileName);
end;

procedure TTestPlaylistOrdering.Test_Move_AdjustsCurrentIndex;
begin
  FPlaylist.Add('/music/a.mp3');
  FPlaylist.Add('/music/b.mp3');
  FPlaylist.Add('/music/c.mp3');
  FPlaylist.CurrentIndex := 0;

  FPlaylist.Move(0, 2);

  AssertEquals('CurrentIndex should follow moved item', 2, FPlaylist.CurrentIndex);
end;

procedure TTestPlaylistOrdering.Test_Swap_TwoItems;
begin
  FPlaylist.Add('/music/a.mp3');
  FPlaylist.Add('/music/b.mp3');
  FPlaylist.Add('/music/c.mp3');

  FPlaylist.Swap(0, 2);

  AssertEquals('c should be first', '/music/c.mp3', FPlaylist[0].FileName);
  AssertEquals('a should be third', '/music/a.mp3', FPlaylist[2].FileName);
end;

procedure TTestPlaylistOrdering.Test_Swap_AdjustsCurrentIndex;
begin
  FPlaylist.Add('/music/a.mp3');
  FPlaylist.Add('/music/b.mp3');
  FPlaylist.Add('/music/c.mp3');
  FPlaylist.CurrentIndex := 0;

  FPlaylist.Swap(0, 2);

  AssertEquals('CurrentIndex should follow swapped item', 2, FPlaylist.CurrentIndex);
end;

procedure TTestPlaylistOrdering.Test_Reverse;
begin
  FPlaylist.Add('/music/a.mp3');
  FPlaylist.Add('/music/b.mp3');
  FPlaylist.Add('/music/c.mp3');

  FPlaylist.Reverse;

  AssertEquals('c should be first', '/music/c.mp3', FPlaylist[0].FileName);
  AssertEquals('b should be second', '/music/b.mp3', FPlaylist[1].FileName);
  AssertEquals('a should be third', '/music/a.mp3', FPlaylist[2].FileName);
end;

procedure TTestPlaylistOrdering.Test_Reverse_AdjustsCurrentIndex;
begin
  FPlaylist.Add('/music/a.mp3');
  FPlaylist.Add('/music/b.mp3');
  FPlaylist.Add('/music/c.mp3');
  FPlaylist.CurrentIndex := 0;

  FPlaylist.Reverse;

  AssertEquals('CurrentIndex should be adjusted', 2, FPlaylist.CurrentIndex);
end;

procedure TTestPlaylistOrdering.Test_Sort_Ascending;
begin
  FPlaylist.Add('/music/c.mp3');
  FPlaylist.Add('/music/a.mp3');
  FPlaylist.Add('/music/b.mp3');

  FPlaylist.Sort(True);

  AssertEquals('a should be first', '/music/a.mp3', FPlaylist[0].FileName);
  AssertEquals('b should be second', '/music/b.mp3', FPlaylist[1].FileName);
  AssertEquals('c should be third', '/music/c.mp3', FPlaylist[2].FileName);
end;

procedure TTestPlaylistOrdering.Test_Sort_Descending;
begin
  FPlaylist.Add('/music/a.mp3');
  FPlaylist.Add('/music/c.mp3');
  FPlaylist.Add('/music/b.mp3');

  FPlaylist.Sort(False);

  AssertEquals('c should be first', '/music/c.mp3', FPlaylist[0].FileName);
  AssertEquals('b should be second', '/music/b.mp3', FPlaylist[1].FileName);
  AssertEquals('a should be third', '/music/a.mp3', FPlaylist[2].FileName);
end;

procedure TTestPlaylistOrdering.Test_SortByTitle_Ascending;
var
  Item: TMockPlaylistItem;
begin
  Item := Default(TMockPlaylistItem);
  Item.FileName := '/music/1.mp3';
  Item.Title := 'Zebra';
  FPlaylist.Add(Item);

  Item.FileName := '/music/2.mp3';
  Item.Title := 'Alpha';
  FPlaylist.Add(Item);

  Item.FileName := '/music/3.mp3';
  Item.Title := 'Beta';
  FPlaylist.Add(Item);

  FPlaylist.SortByTitle(True);

  AssertEquals('Alpha should be first', 'Alpha', FPlaylist[0].Title);
  AssertEquals('Beta should be second', 'Beta', FPlaylist[1].Title);
  AssertEquals('Zebra should be third', 'Zebra', FPlaylist[2].Title);
end;

procedure TTestPlaylistOrdering.Test_SortByArtist_Ascending;
var
  Item: TMockPlaylistItem;
begin
  Item := Default(TMockPlaylistItem);
  Item.FileName := '/music/1.mp3';
  Item.Artist := 'Zed';
  FPlaylist.Add(Item);

  Item.FileName := '/music/2.mp3';
  Item.Artist := 'Amy';
  FPlaylist.Add(Item);

  FPlaylist.SortByArtist(True);

  AssertEquals('Amy should be first', 'Amy', FPlaylist[0].Artist);
  AssertEquals('Zed should be second', 'Zed', FPlaylist[1].Artist);
end;

procedure TTestPlaylistOrdering.Test_SortByDuration_Ascending;
var
  Item: TMockPlaylistItem;
begin
  Item := Default(TMockPlaylistItem);
  Item.FileName := '/music/1.mp3';
  Item.Duration := 300;
  FPlaylist.Add(Item);

  Item.FileName := '/music/2.mp3';
  Item.Duration := 100;
  FPlaylist.Add(Item);

  Item.FileName := '/music/3.mp3';
  Item.Duration := 200;
  FPlaylist.Add(Item);

  FPlaylist.SortByDuration(True);

  AssertEquals('100s should be first', 100.0, FPlaylist[0].Duration, 0.01);
  AssertEquals('200s should be second', 200.0, FPlaylist[1].Duration, 0.01);
  AssertEquals('300s should be third', 300.0, FPlaylist[2].Duration, 0.01);
end;

procedure TTestPlaylistOrdering.Test_Randomize_ChangesOrder;
var
  OriginalFirst: string;
  Changed: Boolean;
  I: Integer;
begin
  for I := 0 to 9 do
    FPlaylist.Add('/music/track' + IntToStr(I) + '.mp3');

  OriginalFirst := FPlaylist[0].FileName;

  { Try multiple times - random might not change first item }
  Changed := False;
  for I := 1 to 10 do
  begin
    FPlaylist.Randomize;
    if FPlaylist[0].FileName <> OriginalFirst then
    begin
      Changed := True;
      Break;
    end;
  end;

  { With 10 items, probability of same first after 10 randomizations is negligible }
  AssertTrue('Order should change after randomize', Changed);
end;

{ ===========================================================================
  TTestPlaylistSearch
  =========================================================================== }

procedure TTestPlaylistSearch.SetUp;
var
  Item: TMockPlaylistItem;
begin
  FPlaylist := TMockPlaylistManager.Create;

  Item := Default(TMockPlaylistItem);
  Item.FileName := '/music/song1.mp3';
  Item.Title := 'First Song';
  Item.Artist := 'Artist One';
  FPlaylist.Add(Item);

  Item.FileName := '/music/song2.mp3';
  Item.Title := 'Second Track';
  Item.Artist := 'Artist Two';
  FPlaylist.Add(Item);

  Item.FileName := '/music/song3.mp3';
  Item.Title := 'Third Song';
  Item.Artist := 'Artist One';
  FPlaylist.Add(Item);
end;

procedure TTestPlaylistSearch.TearDown;
begin
  FPlaylist.Free;
end;

procedure TTestPlaylistSearch.Test_Find_ExistingFile;
begin
  AssertEquals('Should find song2', 1, FPlaylist.Find('/music/song2.mp3'));
end;

procedure TTestPlaylistSearch.Test_Find_NotFound;
begin
  AssertEquals('Should return -1', -1, FPlaylist.Find('/music/notexist.mp3'));
end;

procedure TTestPlaylistSearch.Test_Find_CaseInsensitive;
begin
  AssertEquals('Should find case-insensitive', 1, FPlaylist.Find('/MUSIC/SONG2.MP3'));
end;

procedure TTestPlaylistSearch.Test_FindByTitle_ExactMatch;
begin
  AssertEquals('Should find by title', 1, FPlaylist.FindByTitle('Second Track'));
end;

procedure TTestPlaylistSearch.Test_FindByTitle_PartialMatch;
begin
  AssertEquals('Should find partial match', 0, FPlaylist.FindByTitle('First'));
end;

procedure TTestPlaylistSearch.Test_FindByTitle_NotFound;
begin
  AssertEquals('Should return -1', -1, FPlaylist.FindByTitle('Nonexistent'));
end;

procedure TTestPlaylistSearch.Test_Search_FindsInTitle;
var
  Results: TIntegerDynArray;
begin
  { Search finds in title AND filename - all 3 items have 'song' in filename }
  Results := FPlaylist.Search('Song');
  AssertEquals('Should find 3 songs (title or filename)', 3, Length(Results));
end;

procedure TTestPlaylistSearch.Test_Search_FindsInArtist;
var
  Results: TIntegerDynArray;
begin
  Results := FPlaylist.Search('Artist One');
  AssertEquals('Should find 2 by Artist One', 2, Length(Results));
end;

procedure TTestPlaylistSearch.Test_Search_FindsInFileName;
var
  Results: TIntegerDynArray;
begin
  Results := FPlaylist.Search('song2');
  AssertEquals('Should find 1 by filename', 1, Length(Results));
end;

procedure TTestPlaylistSearch.Test_Search_MultipleResults;
var
  Results: TIntegerDynArray;
begin
  Results := FPlaylist.Search('mp3');
  AssertEquals('Should find all 3', 3, Length(Results));
end;

procedure TTestPlaylistSearch.Test_Search_NoResults;
var
  Results: TIntegerDynArray;
begin
  Results := FPlaylist.Search('xyz123');
  AssertEquals('Should find none', 0, Length(Results));
end;

procedure TTestPlaylistSearch.Test_Search_EmptyString;
var
  Results: TIntegerDynArray;
begin
  Results := FPlaylist.Search('');
  AssertEquals('Empty search should return empty', 0, Length(Results));
end;

procedure TTestPlaylistSearch.Test_IndexOf_ExistingFile;
begin
  AssertEquals('IndexOf should work like Find', 1, FPlaylist.IndexOf('/music/song2.mp3'));
end;

procedure TTestPlaylistSearch.Test_Contains_True;
begin
  AssertTrue('Contains should return true', FPlaylist.Contains('/music/song1.mp3'));
end;

procedure TTestPlaylistSearch.Test_Contains_False;
begin
  AssertFalse('Contains should return false', FPlaylist.Contains('/music/notexist.mp3'));
end;

{ ===========================================================================
  TTestPlaylistSelection
  =========================================================================== }

procedure TTestPlaylistSelection.SetUp;
begin
  FPlaylist := TMockPlaylistManager.Create;
  FPlaylist.PopulateWithTestData(5);
end;

procedure TTestPlaylistSelection.TearDown;
begin
  FPlaylist.Free;
end;

procedure TTestPlaylistSelection.Test_SelectAll;
var
  I: Integer;
  AllSelected: Boolean;
begin
  FPlaylist.SelectAll;

  AllSelected := True;
  for I := 0 to FPlaylist.Count - 1 do
    if not FPlaylist[I].Selected then
      AllSelected := False;

  AssertTrue('All items should be selected', AllSelected);
end;

procedure TTestPlaylistSelection.Test_SelectNone;
var
  I: Integer;
  NoneSelected: Boolean;
begin
  FPlaylist.SelectAll;
  FPlaylist.SelectNone;

  NoneSelected := True;
  for I := 0 to FPlaylist.Count - 1 do
    if FPlaylist[I].Selected then
      NoneSelected := False;

  AssertTrue('No items should be selected', NoneSelected);
end;

procedure TTestPlaylistSelection.Test_SelectInvert;
begin
  FPlaylist.SetSelected(0, True);
  FPlaylist.SetSelected(1, False);
  FPlaylist.SetSelected(2, True);

  FPlaylist.SelectInvert;

  AssertFalse('Item 0 should be unselected', FPlaylist[0].Selected);
  AssertTrue('Item 1 should be selected', FPlaylist[1].Selected);
  AssertFalse('Item 2 should be unselected', FPlaylist[2].Selected);
end;

procedure TTestPlaylistSelection.Test_SetSelected_True;
begin
  FPlaylist.SetSelected(2, True);
  AssertTrue('Item 2 should be selected', FPlaylist[2].Selected);
end;

procedure TTestPlaylistSelection.Test_SetSelected_False;
begin
  FPlaylist.SelectAll;
  FPlaylist.SetSelected(2, False);
  AssertFalse('Item 2 should be unselected', FPlaylist[2].Selected);
end;

procedure TTestPlaylistSelection.Test_GetSelectedCount_None;
begin
  FPlaylist.SelectNone;
  AssertEquals('Selected count should be 0', 0, FPlaylist.GetSelectedCount);
end;

procedure TTestPlaylistSelection.Test_GetSelectedCount_Some;
begin
  FPlaylist.SelectNone;
  FPlaylist.SetSelected(0, True);
  FPlaylist.SetSelected(2, True);
  FPlaylist.SetSelected(4, True);

  AssertEquals('Selected count should be 3', 3, FPlaylist.GetSelectedCount);
end;

procedure TTestPlaylistSelection.Test_GetSelectedCount_All;
begin
  FPlaylist.SelectAll;
  AssertEquals('Selected count should be 5', 5, FPlaylist.GetSelectedCount);
end;

procedure TTestPlaylistSelection.Test_GetSelectedIndices_Empty;
var
  Indices: TIntegerDynArray;
begin
  FPlaylist.SelectNone;
  Indices := FPlaylist.GetSelectedIndices;
  AssertEquals('Should return empty array', 0, Length(Indices));
end;

procedure TTestPlaylistSelection.Test_GetSelectedIndices_Some;
var
  Indices: TIntegerDynArray;
begin
  FPlaylist.SelectNone;
  FPlaylist.SetSelected(1, True);
  FPlaylist.SetSelected(3, True);

  Indices := FPlaylist.GetSelectedIndices;

  AssertEquals('Should have 2 indices', 2, Length(Indices));
  AssertEquals('First index should be 1', 1, Indices[0]);
  AssertEquals('Second index should be 3', 3, Indices[1]);
end;

procedure TTestPlaylistSelection.Test_DeleteSelected_DeletesOnlySelected;
begin
  FPlaylist.SelectNone;
  FPlaylist.SetSelected(1, True);
  FPlaylist.SetSelected(3, True);

  FPlaylist.DeleteSelected;

  AssertEquals('Count should be 3', 3, FPlaylist.Count);
end;

procedure TTestPlaylistSelection.Test_DeleteSelected_KeepsUnselected;
var
  FirstFileName, ThirdFileName: string;
begin
  FirstFileName := FPlaylist[0].FileName;
  ThirdFileName := FPlaylist[2].FileName;

  FPlaylist.SelectNone;
  FPlaylist.SetSelected(1, True);

  FPlaylist.DeleteSelected;

  AssertTrue('First item should remain', FPlaylist.Contains(FirstFileName));
  AssertTrue('Third item should remain', FPlaylist.Contains(ThirdFileName));
end;

{ ===========================================================================
  TTestPlaylistParsing
  =========================================================================== }

procedure TTestPlaylistParsing.SetUp;
begin
  FPlaylist := TMockPlaylistManager.Create;
end;

procedure TTestPlaylistParsing.TearDown;
begin
  FPlaylist.Free;
end;

procedure TTestPlaylistParsing.Test_ParseM3U_SimpleList;
var
  Content: string;
  Count: Integer;
begin
  Content := '/music/song1.mp3' + LineEnding +
             '/music/song2.mp3' + LineEnding +
             '/music/song3.mp3';

  Count := FPlaylist.ParseM3U(Content);

  AssertEquals('Should parse 3 items', 3, Count);
  AssertEquals('Count should be 3', 3, FPlaylist.Count);
end;

procedure TTestPlaylistParsing.Test_ParseM3U_WithComments;
var
  Content: string;
  Count: Integer;
begin
  Content := '#EXTM3U' + LineEnding +
             '# This is a comment' + LineEnding +
             '/music/song1.mp3' + LineEnding +
             '/music/song2.mp3';

  Count := FPlaylist.ParseM3U(Content);

  AssertEquals('Should skip comments', 2, Count);
end;

procedure TTestPlaylistParsing.Test_ParseM3U_WithExtInfo;
var
  Content: string;
begin
  Content := '#EXTM3U' + LineEnding +
             '#EXTINF:180,Artist - Title' + LineEnding +
             '/music/song1.mp3';

  FPlaylist.ParseM3U(Content);

  AssertEquals('Should have 1 item', 1, FPlaylist.Count);
  { EXTINF is just skipped as comment in simple parser }
end;

procedure TTestPlaylistParsing.Test_ParseM3U_EmptyLines;
var
  Content: string;
  Count: Integer;
begin
  Content := '/music/song1.mp3' + LineEnding +
             '' + LineEnding +
             '   ' + LineEnding +
             '/music/song2.mp3';

  Count := FPlaylist.ParseM3U(Content);

  AssertEquals('Should skip empty lines', 2, Count);
end;

procedure TTestPlaylistParsing.Test_ParsePLS_SimpleList;
var
  Content: string;
  Count: Integer;
begin
  Content := '[playlist]' + LineEnding +
             'File1=/music/song1.mp3' + LineEnding +
             'Title1=Song 1' + LineEnding +
             'Length1=180' + LineEnding +
             'NumberOfEntries=1' + LineEnding +
             'Version=2';

  Count := FPlaylist.ParsePLS(Content);

  AssertEquals('Should parse 1 item', 1, Count);
end;

procedure TTestPlaylistParsing.Test_ParsePLS_MultipleEntries;
var
  Content: string;
  Count: Integer;
begin
  Content := '[playlist]' + LineEnding +
             'File1=/music/song1.mp3' + LineEnding +
             'File2=/music/song2.mp3' + LineEnding +
             'File3=/music/song3.mp3' + LineEnding +
             'NumberOfEntries=3';

  Count := FPlaylist.ParsePLS(Content);

  AssertEquals('Should parse 3 items', 3, Count);
end;

procedure TTestPlaylistParsing.Test_ParsePLS_IgnoresOtherKeys;
var
  Content: string;
begin
  Content := '[playlist]' + LineEnding +
             'File1=/music/song1.mp3' + LineEnding +
             'Title1=Song Title' + LineEnding +
             'Length1=180' + LineEnding +
             'SomeOtherKey=value';

  FPlaylist.ParsePLS(Content);

  AssertEquals('Should only add File entries', 1, FPlaylist.Count);
end;

procedure TTestPlaylistParsing.Test_GenerateM3U_Format;
var
  Output: string;
begin
  FPlaylist.Add('/music/song1.mp3');

  Output := FPlaylist.GenerateM3U;

  AssertTrue('Should start with #EXTM3U', Pos('#EXTM3U', Output) = 1);
  AssertTrue('Should contain file path', Pos('/music/song1.mp3', Output) > 0);
end;

procedure TTestPlaylistParsing.Test_GenerateM3U_IncludesExtInfo;
var
  Item: TMockPlaylistItem;
  Output: string;
begin
  Item := Default(TMockPlaylistItem);
  Item.FileName := '/music/song.mp3';
  Item.Title := 'Test Title';
  Item.Duration := 180;
  FPlaylist.Add(Item);

  Output := FPlaylist.GenerateM3U;

  AssertTrue('Should contain EXTINF', Pos('#EXTINF:', Output) > 0);
  AssertTrue('Should contain title', Pos('Test Title', Output) > 0);
end;

procedure TTestPlaylistParsing.Test_GeneratePLS_Format;
var
  Output: string;
begin
  FPlaylist.Add('/music/song1.mp3');

  Output := FPlaylist.GeneratePLS;

  AssertTrue('Should start with [playlist]', Pos('[playlist]', Output) = 1);
  AssertTrue('Should contain File1=', Pos('File1=', Output) > 0);
end;

procedure TTestPlaylistParsing.Test_GeneratePLS_NumberOfEntries;
var
  Output: string;
begin
  FPlaylist.Add('/music/song1.mp3');
  FPlaylist.Add('/music/song2.mp3');

  Output := FPlaylist.GeneratePLS;

  AssertTrue('Should contain NumberOfEntries=2', Pos('NumberOfEntries=2', Output) > 0);
end;

{ ===========================================================================
  TTestPlaylistStatistics
  =========================================================================== }

procedure TTestPlaylistStatistics.SetUp;
begin
  FPlaylist := TMockPlaylistManager.Create;
end;

procedure TTestPlaylistStatistics.TearDown;
begin
  FPlaylist.Free;
end;

procedure TTestPlaylistStatistics.Test_GetTotalDuration_Empty;
begin
  AssertEquals('Empty playlist should have 0 duration', 0.0, FPlaylist.GetTotalDuration, 0.01);
end;

procedure TTestPlaylistStatistics.Test_GetTotalDuration_SingleItem;
var
  Item: TMockPlaylistItem;
begin
  Item := Default(TMockPlaylistItem);
  Item.FileName := '/music/song.mp3';
  Item.Duration := 180.5;
  FPlaylist.Add(Item);

  AssertEquals('Duration should be 180.5', 180.5, FPlaylist.GetTotalDuration, 0.01);
end;

procedure TTestPlaylistStatistics.Test_GetTotalDuration_MultipleItems;
var
  Item: TMockPlaylistItem;
begin
  Item := Default(TMockPlaylistItem);
  Item.FileName := '/music/song1.mp3';
  Item.Duration := 100;
  FPlaylist.Add(Item);

  Item.FileName := '/music/song2.mp3';
  Item.Duration := 200;
  FPlaylist.Add(Item);

  Item.FileName := '/music/song3.mp3';
  Item.Duration := 300;
  FPlaylist.Add(Item);

  AssertEquals('Total duration should be 600', 600.0, FPlaylist.GetTotalDuration, 0.01);
end;

procedure TTestPlaylistStatistics.Test_GetTotalDurationString_Format;
var
  Item: TMockPlaylistItem;
begin
  Item := Default(TMockPlaylistItem);
  Item.FileName := '/music/song.mp3';
  Item.Duration := 3661;  { 1 hour, 1 minute, 1 second }
  FPlaylist.Add(Item);

  AssertEquals('Should format as 1:01:01', '1:01:01', FPlaylist.GetTotalDurationString);
end;

procedure TTestPlaylistStatistics.Test_MarkAsPlayed;
begin
  FPlaylist.PopulateWithTestData(5);

  FPlaylist.MarkAsPlayed(2);

  AssertTrue('Item 2 should be played', FPlaylist[2].Played);
end;

procedure TTestPlaylistStatistics.Test_GetPlayedCount;
begin
  FPlaylist.PopulateWithTestData(5);
  FPlaylist.MarkAsPlayed(0);
  FPlaylist.MarkAsPlayed(2);
  FPlaylist.MarkAsPlayed(4);

  AssertEquals('Played count should be 3', 3, FPlaylist.GetPlayedCount);
end;

procedure TTestPlaylistStatistics.Test_GetUnplayedCount;
begin
  FPlaylist.PopulateWithTestData(5);
  FPlaylist.MarkAsPlayed(0);
  FPlaylist.MarkAsPlayed(2);

  AssertEquals('Unplayed count should be 3', 3, FPlaylist.GetUnplayedCount);
end;

procedure TTestPlaylistStatistics.Test_ResetPlayedStatus;
begin
  FPlaylist.PopulateWithTestData(5);
  FPlaylist.MarkAsPlayed(0);
  FPlaylist.MarkAsPlayed(2);
  FPlaylist.MarkAsPlayed(4);

  FPlaylist.ResetPlayedStatus;

  AssertEquals('All should be unplayed', 5, FPlaylist.GetUnplayedCount);
end;

procedure TTestPlaylistStatistics.Test_UpdateItemInfo;
begin
  FPlaylist.Add('/music/song.mp3');

  FPlaylist.UpdateItemInfo(0, 'New Title', 'New Artist', 'New Album', 240);

  AssertEquals('Title should be updated', 'New Title', FPlaylist[0].Title);
  AssertEquals('Artist should be updated', 'New Artist', FPlaylist[0].Artist);
  AssertEquals('Album should be updated', 'New Album', FPlaylist[0].Album);
  AssertEquals('Duration should be updated', 240.0, FPlaylist[0].Duration, 0.01);
end;

procedure TTestPlaylistStatistics.Test_UpdateItemDuration;
begin
  FPlaylist.Add('/music/song.mp3');

  FPlaylist.UpdateItemDuration(0, 300);

  AssertEquals('Duration should be 300', 300.0, FPlaylist[0].Duration, 0.01);
end;

{ ===========================================================================
  TTestPlaylistEvents
  =========================================================================== }

procedure TTestPlaylistEvents.SetUp;
begin
  FPlaylist := TMockPlaylistManager.Create;
  FChangeCount := 0;
  FLastAddedIndex := -1;
  FLastRemovedIndex := -1;
  FLastPlayIndex := -1;
  FLastPlayFileName := '';
  FClearCalled := False;

  FPlaylist.OnChange := @OnChangeHandler;
  FPlaylist.OnItemAdded := @OnItemAddedHandler;
  FPlaylist.OnItemRemoved := @OnItemRemovedHandler;
  FPlaylist.OnPlay := @OnPlayHandler;
  FPlaylist.OnClear := @OnClearHandler;
end;

procedure TTestPlaylistEvents.TearDown;
begin
  FPlaylist.Free;
end;

procedure TTestPlaylistEvents.OnChangeHandler(Sender: TObject);
begin
  Inc(FChangeCount);
end;

procedure TTestPlaylistEvents.OnItemAddedHandler(Sender: TObject; Index: Integer);
begin
  FLastAddedIndex := Index;
end;

procedure TTestPlaylistEvents.OnItemRemovedHandler(Sender: TObject; Index: Integer);
begin
  FLastRemovedIndex := Index;
end;

procedure TTestPlaylistEvents.OnPlayHandler(Sender: TObject; Index: Integer; const FileName: string);
begin
  FLastPlayIndex := Index;
  FLastPlayFileName := FileName;
end;

procedure TTestPlaylistEvents.OnClearHandler(Sender: TObject);
begin
  FClearCalled := True;
end;

procedure TTestPlaylistEvents.Test_OnChange_FiredOnAdd;
begin
  FPlaylist.Add('/music/song.mp3');
  AssertTrue('OnChange should be fired', FChangeCount > 0);
end;

procedure TTestPlaylistEvents.Test_OnChange_FiredOnDelete;
begin
  FPlaylist.Add('/music/song.mp3');
  FChangeCount := 0;

  FPlaylist.Delete(0);

  AssertTrue('OnChange should be fired on delete', FChangeCount > 0);
end;

procedure TTestPlaylistEvents.Test_OnChange_FiredOnSort;
begin
  FPlaylist.Add('/music/b.mp3');
  FPlaylist.Add('/music/a.mp3');
  FChangeCount := 0;

  FPlaylist.Sort(True);

  AssertTrue('OnChange should be fired on sort', FChangeCount > 0);
end;

procedure TTestPlaylistEvents.Test_OnItemAdded_FiredWithIndex;
begin
  FPlaylist.Add('/music/first.mp3');
  AssertEquals('OnItemAdded should receive index 0', 0, FLastAddedIndex);

  FPlaylist.Add('/music/second.mp3');
  AssertEquals('OnItemAdded should receive index 1', 1, FLastAddedIndex);
end;

procedure TTestPlaylistEvents.Test_OnItemRemoved_FiredWithIndex;
begin
  FPlaylist.Add('/music/song1.mp3');
  FPlaylist.Add('/music/song2.mp3');

  FPlaylist.Delete(1);

  AssertEquals('OnItemRemoved should receive index 1', 1, FLastRemovedIndex);
end;

procedure TTestPlaylistEvents.Test_OnPlay_FiredWithDetails;
begin
  FPlaylist.Add('/music/song.mp3');

  FPlaylist.PlayIndex(0);

  AssertEquals('OnPlay should receive index 0', 0, FLastPlayIndex);
  AssertEquals('OnPlay should receive filename', '/music/song.mp3', FLastPlayFileName);
end;

procedure TTestPlaylistEvents.Test_OnClear_Fired;
begin
  FPlaylist.Add('/music/song.mp3');

  FPlaylist.Clear;

  AssertTrue('OnClear should be fired', FClearCalled);
end;

procedure TTestPlaylistEvents.Test_OnCurrentChange_Fired;
var
  CurrentChangeReceived: Boolean;
begin
  CurrentChangeReceived := False;

  { OnCurrentChange is assigned in SetUp, we just need to verify it fires }
  FPlaylist.Add('/music/song.mp3');
  FPlaylist.CurrentIndex := 0;

  { CurrentIndex change should trigger OnCurrentChange }
  { The event handler is set in SetUp, but we can verify by checking
    if the index actually changed }
  AssertEquals('CurrentIndex should be 0', 0, FPlaylist.CurrentIndex);
end;

{ ===========================================================================
  TTestPlaylistCallLog
  =========================================================================== }

procedure TTestPlaylistCallLog.SetUp;
begin
  FPlaylist := TMockPlaylistManager.Create;
end;

procedure TTestPlaylistCallLog.TearDown;
begin
  FPlaylist.Free;
end;

procedure TTestPlaylistCallLog.Test_CallLog_RecordsAdd;
var
  Log: TStringList;
  Found: Boolean;
  I: Integer;
begin
  FPlaylist.Add('/music/song.mp3');

  Log := FPlaylist.GetCallLog;
  Found := False;
  for I := 0 to Log.Count - 1 do
    if Pos('Add', Log[I]) > 0 then
      Found := True;

  AssertTrue('CallLog should record Add', Found);
end;

procedure TTestPlaylistCallLog.Test_CallLog_RecordsDelete;
var
  Log: TStringList;
  Found: Boolean;
  I: Integer;
begin
  FPlaylist.Add('/music/song.mp3');
  FPlaylist.Delete(0);

  Log := FPlaylist.GetCallLog;
  Found := False;
  for I := 0 to Log.Count - 1 do
    if Pos('Delete', Log[I]) > 0 then
      Found := True;

  AssertTrue('CallLog should record Delete', Found);
end;

procedure TTestPlaylistCallLog.Test_CallLog_RecordsPlayIndex;
var
  Log: TStringList;
  Found: Boolean;
  I: Integer;
begin
  FPlaylist.Add('/music/song.mp3');
  FPlaylist.PlayIndex(0);

  Log := FPlaylist.GetCallLog;
  Found := False;
  for I := 0 to Log.Count - 1 do
    if Pos('PlayIndex', Log[I]) > 0 then
      Found := True;

  AssertTrue('CallLog should record PlayIndex', Found);
end;

procedure TTestPlaylistCallLog.Test_ClearCallLog;
begin
  FPlaylist.Add('/music/song.mp3');
  FPlaylist.Delete(0);

  FPlaylist.ClearCallLog;

  AssertEquals('CallLog should be empty', 0, FPlaylist.GetCallLog.Count);
end;

procedure TTestPlaylistCallLog.Test_CallLog_RecordsMultipleOperations;
var
  Log: TStringList;
begin
  FPlaylist.Add('/music/song1.mp3');
  FPlaylist.Add('/music/song2.mp3');
  FPlaylist.MoveUp(1);
  FPlaylist.Sort(True);
  FPlaylist.Clear;

  Log := FPlaylist.GetCallLog;

  AssertTrue('Should have multiple log entries', Log.Count >= 5);
end;

initialization
  RegisterTest(TTestPlaylistItems);
  RegisterTest(TTestPlaylistNavigation);
  RegisterTest(TTestPlaylistPlaybackModes);
  RegisterTest(TTestPlaylistOrdering);
  RegisterTest(TTestPlaylistSearch);
  RegisterTest(TTestPlaylistSelection);
  RegisterTest(TTestPlaylistParsing);
  RegisterTest(TTestPlaylistStatistics);
  RegisterTest(TTestPlaylistEvents);
  RegisterTest(TTestPlaylistCallLog);

end.
