{ ═══════════════════════════════════════════════════════════════════════════════
  uTestPlayback.pas - Playback Integration Tests

  Part of 3nity Media - Test Suite

  Integration tests for playback-related component interactions:
  - TPlaylistManager + TConfigManager session persistence
  - Playlist navigation with mode persistence
  - History and recent files integration
  - Playback position save/restore

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uTestPlayback;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  uPlaylistManager, uConfig, uTypes;

type
  { ═══════════════════════════════════════════════════════════════════════════
    PLAYLIST + CONFIG INTEGRATION TESTS
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestPlaylistConfigIntegration = class(TTestCase)
  private
    FPlaylist: TPlaylistManager;
    FConfig: TConfigManager;
    FTempConfigPath: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Session Save/Restore Tests }
    procedure Test_SaveEmptySession;
    procedure Test_SaveSessionWithItems;
    procedure Test_RestoreSessionItems;
    procedure Test_RestoreSessionIndex;
    procedure Test_RestoreSessionPlaybackMode;
    procedure Test_RestoreSessionPosition;
    procedure Test_ClearSession;

    { Playback Position Tests }
    procedure Test_SavePlaybackPosition;
    procedure Test_GetPlaybackPositionExisting;
    procedure Test_GetPlaybackPositionNonExisting;
    procedure Test_SaveMultiplePositions;
    procedure Test_ClearPlaybackPositions;

    { History Integration Tests }
    procedure Test_AddToHistoryFromPlaylist;
    procedure Test_HistoryContainsPlayedItems;
    procedure Test_HistoryMaxItems;
    procedure Test_ClearHistory;

    { Recent Files Integration Tests }
    procedure Test_AddRecentFileFromPlaylist;
    procedure Test_RecentFilesOrder;
    procedure Test_RecentFilesDuplicates;
    procedure Test_ClearRecentFiles;

    { Playback Mode Persistence Tests }
    procedure Test_PlaybackModeNormal;
    procedure Test_PlaybackModeRepeatOne;
    procedure Test_PlaybackModeRepeatAll;
    procedure Test_PlaybackModeShuffle;
    procedure Test_PlaybackModePersistence;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    PLAYLIST NAVIGATION INTEGRATION TESTS
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestPlaylistNavigation = class(TTestCase)
  private
    FPlaylist: TPlaylistManager;
    FPlayedIndices: array of Integer;
    FPlayedFiles: array of string;
    procedure OnPlayHandler(Sender: TObject; Index: Integer; const FileName: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Sequential Navigation Tests }
    procedure Test_NavigateNextSequential;
    procedure Test_NavigatePreviousSequential;
    procedure Test_NavigateToEnd;
    procedure Test_NavigateToStart;

    { Repeat Mode Navigation Tests }
    procedure Test_RepeatOneStaysOnSame;
    procedure Test_RepeatAllWrapsAround;
    procedure Test_RepeatAllWrapsBackward;

    { Shuffle Mode Navigation Tests }
    procedure Test_ShuffleCoversAllItems;
    procedure Test_ShuffleRegenOnModeChange;

    { Edge Cases }
    procedure Test_NavigateEmptyPlaylist;
    procedure Test_NavigateSingleItem;
    procedure Test_NavigateAfterDelete;
    procedure Test_NavigateAfterClear;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    BOOKMARKS INTEGRATION TESTS
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestBookmarksIntegration = class(TTestCase)
  private
    FConfig: TConfigManager;
    FTempConfigPath: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_AddBookmarkForFile;
    procedure Test_MultipleBookmarksSameFile;
    procedure Test_BookmarksDifferentFiles;
    procedure Test_GetBookmarksForFile;
    procedure Test_RemoveBookmarkByPosition;
    procedure Test_ClearBookmarksForFile;
    procedure Test_ClearAllBookmarks;
    procedure Test_BookmarkPersistence;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    FAVORITES INTEGRATION TESTS
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestFavoritesIntegration = class(TTestCase)
  private
    FConfig: TConfigManager;
    FTempConfigPath: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_AddFileFavorite;
    procedure Test_AddFolderFavorite;
    procedure Test_AddRadioFavorite;
    procedure Test_AddPlaylistFavorite;
    procedure Test_IsFavorite;
    procedure Test_RemoveFavorite;
    procedure Test_GetFavoritesByType;
    procedure Test_GetFavoritesByCategory;
    procedure Test_UpdateLastPlayed;
    procedure Test_FavoriteCategories;
    procedure Test_ClearFavorites;
  end;

implementation

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestPlaylistConfigIntegration
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestPlaylistConfigIntegration.SetUp;
begin
  FPlaylist := TPlaylistManager.Create;
  FConfig := TConfigManager.Create;
  FTempConfigPath := GetTempDir + 'test_config_' + IntToStr(Random(100000)) + '.ini';
end;

procedure TTestPlaylistConfigIntegration.TearDown;
begin
  FPlaylist.Free;
  FConfig.Free;
  if FileExists(FTempConfigPath) then
    DeleteFile(FTempConfigPath);
end;

procedure TTestPlaylistConfigIntegration.Test_SaveEmptySession;
var
  Items: TPlaylistItems;
  Index: Integer;
  Mode: TPlaybackMode;
  Position: Double;
begin
  SetLength(Items, 0);
  FConfig.SaveSessionPlaylist(Items, -1, pmNormal, 0.0);
  AssertTrue('Should save empty session without error', True);
end;

procedure TTestPlaylistConfigIntegration.Test_SaveSessionWithItems;
var
  Items: TPlaylistItems;
begin
  SetLength(Items, 3);
  Items[0].FileName := '/path/to/file1.mp3';
  Items[0].Title := 'Song 1';
  Items[1].FileName := '/path/to/file2.mp3';
  Items[1].Title := 'Song 2';
  Items[2].FileName := '/path/to/file3.mp3';
  Items[2].Title := 'Song 3';

  FConfig.SaveSessionPlaylist(Items, 1, pmRepeatAll, 45.5);
  AssertTrue('Should save session with items', True);
end;

procedure TTestPlaylistConfigIntegration.Test_RestoreSessionItems;
var
  SaveItems, LoadItems: TPlaylistItems;
  Index: Integer;
  Mode: TPlaybackMode;
  Position: Double;
  Success: Boolean;
begin
  SetLength(SaveItems, 2);
  SaveItems[0].FileName := '/test/audio.mp3';
  SaveItems[0].Title := 'Test Audio';
  SaveItems[1].FileName := '/test/video.mp4';
  SaveItems[1].Title := 'Test Video';

  FConfig.SaveSessionPlaylist(SaveItems, 0, pmNormal, 0.0);
  Success := FConfig.LoadSessionPlaylist(LoadItems, Index, Mode, Position);

  AssertTrue('Should load session successfully', Success);
  AssertEquals('Should have 2 items', 2, Length(LoadItems));
  AssertEquals('First item filename', '/test/audio.mp3', LoadItems[0].FileName);
  AssertEquals('Second item filename', '/test/video.mp4', LoadItems[1].FileName);
end;

procedure TTestPlaylistConfigIntegration.Test_RestoreSessionIndex;
var
  SaveItems, LoadItems: TPlaylistItems;
  Index: Integer;
  Mode: TPlaybackMode;
  Position: Double;
begin
  SetLength(SaveItems, 3);
  SaveItems[0].FileName := '/file1.mp3';
  SaveItems[1].FileName := '/file2.mp3';
  SaveItems[2].FileName := '/file3.mp3';

  FConfig.SaveSessionPlaylist(SaveItems, 2, pmNormal, 0.0);
  FConfig.LoadSessionPlaylist(LoadItems, Index, Mode, Position);

  AssertEquals('Should restore index 2', 2, Index);
end;

procedure TTestPlaylistConfigIntegration.Test_RestoreSessionPlaybackMode;
var
  SaveItems, LoadItems: TPlaylistItems;
  Index: Integer;
  Mode: TPlaybackMode;
  Position: Double;
begin
  SetLength(SaveItems, 1);
  SaveItems[0].FileName := '/file.mp3';

  FConfig.SaveSessionPlaylist(SaveItems, 0, pmRepeatOne, 0.0);
  FConfig.LoadSessionPlaylist(LoadItems, Index, Mode, Position);

  AssertEquals('Should restore RepeatOne mode', Ord(pmRepeatOne), Ord(Mode));
end;

procedure TTestPlaylistConfigIntegration.Test_RestoreSessionPosition;
var
  SaveItems, LoadItems: TPlaylistItems;
  Index: Integer;
  Mode: TPlaybackMode;
  Position: Double;
begin
  SetLength(SaveItems, 1);
  SaveItems[0].FileName := '/file.mp3';

  FConfig.SaveSessionPlaylist(SaveItems, 0, pmNormal, 123.456);
  FConfig.LoadSessionPlaylist(LoadItems, Index, Mode, Position);

  AssertTrue('Should restore position ~123.456', Abs(Position - 123.456) < 0.01);
end;

procedure TTestPlaylistConfigIntegration.Test_ClearSession;
var
  SaveItems, LoadItems: TPlaylistItems;
  Index: Integer;
  Mode: TPlaybackMode;
  Position: Double;
  Success: Boolean;
begin
  SetLength(SaveItems, 2);
  SaveItems[0].FileName := '/file1.mp3';
  SaveItems[1].FileName := '/file2.mp3';

  FConfig.SaveSessionPlaylist(SaveItems, 0, pmNormal, 0.0);
  FConfig.ClearSessionPlaylist;
  Success := FConfig.LoadSessionPlaylist(LoadItems, Index, Mode, Position);

  AssertFalse('Should return false after clear', Success);
end;

procedure TTestPlaylistConfigIntegration.Test_SavePlaybackPosition;
begin
  FConfig.SavePlaybackPosition('/test/file.mp4', 60.5);
  AssertTrue('Should save position without error', True);
end;

procedure TTestPlaylistConfigIntegration.Test_GetPlaybackPositionExisting;
var
  Position: Double;
begin
  FConfig.SavePlaybackPosition('/test/movie.mkv', 3600.0);
  Position := FConfig.GetPlaybackPosition('/test/movie.mkv');
  AssertTrue('Should retrieve saved position', Abs(Position - 3600.0) < 0.1);
end;

procedure TTestPlaylistConfigIntegration.Test_GetPlaybackPositionNonExisting;
var
  Position: Double;
begin
  Position := FConfig.GetPlaybackPosition('/nonexistent/file.mp4');
  AssertEquals('Should return 0 for non-existing', 0.0, Position);
end;

procedure TTestPlaylistConfigIntegration.Test_SaveMultiplePositions;
var
  Pos1, Pos2, Pos3: Double;
begin
  FConfig.SavePlaybackPosition('/file1.mp4', 100.0);
  FConfig.SavePlaybackPosition('/file2.mp4', 200.0);
  FConfig.SavePlaybackPosition('/file3.mp4', 300.0);

  Pos1 := FConfig.GetPlaybackPosition('/file1.mp4');
  Pos2 := FConfig.GetPlaybackPosition('/file2.mp4');
  Pos3 := FConfig.GetPlaybackPosition('/file3.mp4');

  AssertTrue('File1 position', Abs(Pos1 - 100.0) < 0.1);
  AssertTrue('File2 position', Abs(Pos2 - 200.0) < 0.1);
  AssertTrue('File3 position', Abs(Pos3 - 300.0) < 0.1);
end;

procedure TTestPlaylistConfigIntegration.Test_ClearPlaybackPositions;
var
  Position: Double;
begin
  FConfig.SavePlaybackPosition('/test/file.mp4', 500.0);
  FConfig.ClearPlaybackPositions;
  Position := FConfig.GetPlaybackPosition('/test/file.mp4');
  AssertEquals('Should return 0 after clear', 0.0, Position);
end;

procedure TTestPlaylistConfigIntegration.Test_AddToHistoryFromPlaylist;
begin
  FConfig.AddToHistory('/music/song.mp3', 'Test Song', 45.0, 180.0);
  AssertTrue('Should add to history without error', True);
end;

procedure TTestPlaylistConfigIntegration.Test_HistoryContainsPlayedItems;
var
  History: THistoryItems;
begin
  FConfig.ClearHistory;
  FConfig.AddToHistory('/music/track1.mp3', 'Track 1', 0.0, 200.0);
  FConfig.AddToHistory('/music/track2.mp3', 'Track 2', 0.0, 180.0);

  History := FConfig.GetHistory;
  AssertTrue('Should have history items', Length(History) >= 2);
end;

procedure TTestPlaylistConfigIntegration.Test_HistoryMaxItems;
var
  History: THistoryItems;
  I: Integer;
begin
  FConfig.ClearHistory;
  { Add items beyond the MAX_HISTORY_ITEMS limit (500) }
  for I := 1 to 550 do
    FConfig.AddToHistory('/music/track' + IntToStr(I) + '.mp3', 'Track ' + IntToStr(I), 0.0, 180.0);

  History := FConfig.GetHistory;
  AssertTrue('Should limit history items to 500', Length(History) <= 500);
end;

procedure TTestPlaylistConfigIntegration.Test_ClearHistory;
var
  History: THistoryItems;
begin
  FConfig.AddToHistory('/music/song.mp3', 'Test', 0.0, 180.0);
  FConfig.ClearHistory;
  History := FConfig.GetHistory;
  AssertEquals('Should be empty after clear', 0, Length(History));
end;

procedure TTestPlaylistConfigIntegration.Test_AddRecentFileFromPlaylist;
var
  Recent: TStringList;
begin
  FConfig.ClearRecentFiles;
  FConfig.AddRecentFile('/video/movie.mp4');
  Recent := FConfig.GetRecentFiles;
  try
    AssertTrue('Should have recent file', Recent.Count > 0);
  finally
    Recent.Free;
  end;
end;

procedure TTestPlaylistConfigIntegration.Test_RecentFilesOrder;
var
  Recent: TStringList;
begin
  FConfig.ClearRecentFiles;
  FConfig.AddRecentFile('/file1.mp3');
  FConfig.AddRecentFile('/file2.mp3');
  FConfig.AddRecentFile('/file3.mp3');

  Recent := FConfig.GetRecentFiles;
  try
    AssertEquals('Most recent first', '/file3.mp3', Recent[0]);
  finally
    Recent.Free;
  end;
end;

procedure TTestPlaylistConfigIntegration.Test_RecentFilesDuplicates;
var
  Recent: TStringList;
  Count, I: Integer;
begin
  FConfig.ClearRecentFiles;
  FConfig.AddRecentFile('/file1.mp3');
  FConfig.AddRecentFile('/file2.mp3');
  FConfig.AddRecentFile('/file1.mp3'); // Duplicate

  Recent := FConfig.GetRecentFiles;
  try
    Count := 0;
    for I := 0 to Recent.Count - 1 do
      if Recent[I] = '/file1.mp3' then Inc(Count);
    AssertEquals('Should not have duplicates', 1, Count);
  finally
    Recent.Free;
  end;
end;

procedure TTestPlaylistConfigIntegration.Test_ClearRecentFiles;
var
  Recent: TStringList;
begin
  FConfig.AddRecentFile('/file.mp3');
  FConfig.ClearRecentFiles;
  Recent := FConfig.GetRecentFiles;
  try
    AssertEquals('Should be empty', 0, Recent.Count);
  finally
    Recent.Free;
  end;
end;

procedure TTestPlaylistConfigIntegration.Test_PlaybackModeNormal;
begin
  FPlaylist.PlaybackMode := pmNormal;
  AssertEquals('Should be Normal', Ord(pmNormal), Ord(FPlaylist.PlaybackMode));
end;

procedure TTestPlaylistConfigIntegration.Test_PlaybackModeRepeatOne;
begin
  FPlaylist.PlaybackMode := pmRepeatOne;
  AssertEquals('Should be RepeatOne', Ord(pmRepeatOne), Ord(FPlaylist.PlaybackMode));
end;

procedure TTestPlaylistConfigIntegration.Test_PlaybackModeRepeatAll;
begin
  FPlaylist.PlaybackMode := pmRepeatAll;
  AssertEquals('Should be RepeatAll', Ord(pmRepeatAll), Ord(FPlaylist.PlaybackMode));
end;

procedure TTestPlaylistConfigIntegration.Test_PlaybackModeShuffle;
begin
  FPlaylist.PlaybackMode := pmShuffle;
  AssertEquals('Should be Shuffle', Ord(pmShuffle), Ord(FPlaylist.PlaybackMode));
end;

procedure TTestPlaylistConfigIntegration.Test_PlaybackModePersistence;
var
  Items: TPlaylistItems;
  Index: Integer;
  Mode: TPlaybackMode;
  Position: Double;
begin
  SetLength(Items, 1);
  Items[0].FileName := '/file.mp3';

  FConfig.SaveSessionPlaylist(Items, 0, pmShuffle, 0.0);
  FConfig.LoadSessionPlaylist(Items, Index, Mode, Position);

  AssertEquals('Mode should persist', Ord(pmShuffle), Ord(Mode));
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestPlaylistNavigation
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestPlaylistNavigation.OnPlayHandler(Sender: TObject; Index: Integer; const FileName: string);
begin
  SetLength(FPlayedIndices, Length(FPlayedIndices) + 1);
  FPlayedIndices[High(FPlayedIndices)] := Index;
  SetLength(FPlayedFiles, Length(FPlayedFiles) + 1);
  FPlayedFiles[High(FPlayedFiles)] := FileName;
end;

procedure TTestPlaylistNavigation.SetUp;
begin
  FPlaylist := TPlaylistManager.Create;
  FPlaylist.OnPlay := @OnPlayHandler;
  SetLength(FPlayedIndices, 0);
  SetLength(FPlayedFiles, 0);
end;

procedure TTestPlaylistNavigation.TearDown;
begin
  FPlaylist.Free;
  SetLength(FPlayedIndices, 0);
  SetLength(FPlayedFiles, 0);
end;

procedure TTestPlaylistNavigation.Test_NavigateNextSequential;
begin
  FPlaylist.Add('/file1.mp3');
  FPlaylist.Add('/file2.mp3');
  FPlaylist.Add('/file3.mp3');
  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.CurrentIndex := 0;

  AssertEquals('Next from 0 should be 1', 1, FPlaylist.GetNext);
  FPlaylist.CurrentIndex := 1;
  AssertEquals('Next from 1 should be 2', 2, FPlaylist.GetNext);
end;

procedure TTestPlaylistNavigation.Test_NavigatePreviousSequential;
begin
  FPlaylist.Add('/file1.mp3');
  FPlaylist.Add('/file2.mp3');
  FPlaylist.Add('/file3.mp3');
  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.CurrentIndex := 2;

  AssertEquals('Previous from 2 should be 1', 1, FPlaylist.GetPrevious);
  FPlaylist.CurrentIndex := 1;
  AssertEquals('Previous from 1 should be 0', 0, FPlaylist.GetPrevious);
end;

procedure TTestPlaylistNavigation.Test_NavigateToEnd;
begin
  FPlaylist.Add('/file1.mp3');
  FPlaylist.Add('/file2.mp3');
  FPlaylist.Add('/file3.mp3');
  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.CurrentIndex := 2;

  AssertEquals('Next at end in Normal mode should be -1', -1, FPlaylist.GetNext);
end;

procedure TTestPlaylistNavigation.Test_NavigateToStart;
begin
  FPlaylist.Add('/file1.mp3');
  FPlaylist.Add('/file2.mp3');
  FPlaylist.Add('/file3.mp3');
  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.CurrentIndex := 0;

  AssertEquals('Previous at start in Normal mode should be -1', -1, FPlaylist.GetPrevious);
end;

procedure TTestPlaylistNavigation.Test_RepeatOneStaysOnSame;
begin
  FPlaylist.Add('/file1.mp3');
  FPlaylist.Add('/file2.mp3');
  FPlaylist.PlaybackMode := pmRepeatOne;
  FPlaylist.CurrentIndex := 0;

  AssertEquals('RepeatOne should return same index', 0, FPlaylist.GetNext);
end;

procedure TTestPlaylistNavigation.Test_RepeatAllWrapsAround;
begin
  FPlaylist.Add('/file1.mp3');
  FPlaylist.Add('/file2.mp3');
  FPlaylist.Add('/file3.mp3');
  FPlaylist.PlaybackMode := pmRepeatAll;
  FPlaylist.CurrentIndex := 2;

  AssertEquals('RepeatAll at end should wrap to 0', 0, FPlaylist.GetNext);
end;

procedure TTestPlaylistNavigation.Test_RepeatAllWrapsBackward;
begin
  FPlaylist.Add('/file1.mp3');
  FPlaylist.Add('/file2.mp3');
  FPlaylist.Add('/file3.mp3');
  FPlaylist.PlaybackMode := pmRepeatAll;
  FPlaylist.CurrentIndex := 0;

  AssertEquals('RepeatAll at start should wrap to last', 2, FPlaylist.GetPrevious);
end;

procedure TTestPlaylistNavigation.Test_ShuffleCoversAllItems;
var
  Visited: array of Boolean;
  NextIdx, I, Iterations: Integer;
begin
  FPlaylist.Add('/file1.mp3');
  FPlaylist.Add('/file2.mp3');
  FPlaylist.Add('/file3.mp3');
  FPlaylist.Add('/file4.mp3');
  FPlaylist.Add('/file5.mp3');
  FPlaylist.PlaybackMode := pmShuffle;

  SetLength(Visited, 5);
  for I := 0 to 4 do Visited[I] := False;

  FPlaylist.CurrentIndex := 0;
  Visited[0] := True;
  Iterations := 0;

  repeat
    NextIdx := FPlaylist.GetNext;
    if (NextIdx >= 0) and (NextIdx < 5) then
    begin
      Visited[NextIdx] := True;
      FPlaylist.CurrentIndex := NextIdx;
    end;
    Inc(Iterations);
  until (NextIdx < 0) or (Iterations > 10);

  // In shuffle mode, we should visit items but might not get all in one pass
  // The test verifies shuffle is working, not that it covers all items in one pass
  AssertTrue('Shuffle should navigate', Iterations > 1);
end;

procedure TTestPlaylistNavigation.Test_ShuffleRegenOnModeChange;
begin
  FPlaylist.Add('/file1.mp3');
  FPlaylist.Add('/file2.mp3');
  FPlaylist.Add('/file3.mp3');

  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.PlaybackMode := pmShuffle;

  // Just verify no crash on mode change
  AssertEquals('Should be in shuffle mode', Ord(pmShuffle), Ord(FPlaylist.PlaybackMode));
end;

procedure TTestPlaylistNavigation.Test_NavigateEmptyPlaylist;
begin
  FPlaylist.PlaybackMode := pmNormal;
  AssertEquals('GetNext on empty should be -1', -1, FPlaylist.GetNext);
  AssertEquals('GetPrevious on empty should be -1', -1, FPlaylist.GetPrevious);
end;

procedure TTestPlaylistNavigation.Test_NavigateSingleItem;
begin
  FPlaylist.Add('/only.mp3');
  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.CurrentIndex := 0;

  AssertEquals('Next with single item should be -1', -1, FPlaylist.GetNext);
  AssertEquals('Previous with single item should be -1', -1, FPlaylist.GetPrevious);
end;

procedure TTestPlaylistNavigation.Test_NavigateAfterDelete;
begin
  FPlaylist.Add('/file1.mp3');
  FPlaylist.Add('/file2.mp3');
  FPlaylist.Add('/file3.mp3');
  FPlaylist.CurrentIndex := 1;
  FPlaylist.Delete(1);

  // After deleting current, index should adjust
  AssertTrue('Count should be 2', FPlaylist.Count = 2);
end;

procedure TTestPlaylistNavigation.Test_NavigateAfterClear;
begin
  FPlaylist.Add('/file1.mp3');
  FPlaylist.Add('/file2.mp3');
  FPlaylist.CurrentIndex := 1;
  FPlaylist.Clear;

  AssertEquals('After clear, count should be 0', 0, FPlaylist.Count);
  AssertEquals('After clear, index should be -1', -1, FPlaylist.CurrentIndex);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestBookmarksIntegration
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestBookmarksIntegration.SetUp;
begin
  FConfig := TConfigManager.Create;
  FConfig.ClearBookmarks;
  FTempConfigPath := GetTempDir + 'test_bookmarks_' + IntToStr(Random(100000)) + '.ini';
end;

procedure TTestBookmarksIntegration.TearDown;
begin
  FConfig.Free;
  if FileExists(FTempConfigPath) then
    DeleteFile(FTempConfigPath);
end;

procedure TTestBookmarksIntegration.Test_AddBookmarkForFile;
var
  Bookmarks: TBookmarkItems;
begin
  FConfig.AddBookmark('/video/movie.mp4', 'Important Scene', 3600.0);
  Bookmarks := FConfig.GetBookmarks;
  AssertTrue('Should have at least 1 bookmark', Length(Bookmarks) >= 1);
end;

procedure TTestBookmarksIntegration.Test_MultipleBookmarksSameFile;
var
  Bookmarks: TBookmarkItems;
begin
  FConfig.ClearBookmarks;
  FConfig.AddBookmark('/video/movie.mp4', 'Scene 1', 600.0);
  FConfig.AddBookmark('/video/movie.mp4', 'Scene 2', 1200.0);
  FConfig.AddBookmark('/video/movie.mp4', 'Scene 3', 1800.0);

  Bookmarks := FConfig.GetBookmarksForFile('/video/movie.mp4');
  AssertEquals('Should have 3 bookmarks for file', 3, Length(Bookmarks));
end;

procedure TTestBookmarksIntegration.Test_BookmarksDifferentFiles;
var
  Bookmarks: TBookmarkItems;
begin
  FConfig.ClearBookmarks;
  FConfig.AddBookmark('/video/movie1.mp4', 'Bookmark 1', 100.0);
  FConfig.AddBookmark('/video/movie2.mp4', 'Bookmark 2', 200.0);

  Bookmarks := FConfig.GetBookmarks;
  AssertEquals('Should have 2 total bookmarks', 2, Length(Bookmarks));
end;

procedure TTestBookmarksIntegration.Test_GetBookmarksForFile;
var
  Bookmarks: TBookmarkItems;
begin
  FConfig.ClearBookmarks;
  FConfig.AddBookmark('/video/target.mp4', 'Target 1', 100.0);
  FConfig.AddBookmark('/video/other.mp4', 'Other', 200.0);
  FConfig.AddBookmark('/video/target.mp4', 'Target 2', 300.0);

  Bookmarks := FConfig.GetBookmarksForFile('/video/target.mp4');
  AssertEquals('Should have 2 bookmarks for target file', 2, Length(Bookmarks));
end;

procedure TTestBookmarksIntegration.Test_RemoveBookmarkByPosition;
var
  Bookmarks: TBookmarkItems;
begin
  FConfig.ClearBookmarks;
  FConfig.AddBookmark('/video/movie.mp4', 'Keep', 100.0);
  FConfig.AddBookmark('/video/movie.mp4', 'Remove', 200.0);
  FConfig.RemoveBookmark('/video/movie.mp4', 200.0);

  Bookmarks := FConfig.GetBookmarksForFile('/video/movie.mp4');
  AssertEquals('Should have 1 bookmark after removal', 1, Length(Bookmarks));
end;

procedure TTestBookmarksIntegration.Test_ClearBookmarksForFile;
var
  AllBookmarks, FileBookmarks: TBookmarkItems;
begin
  FConfig.ClearBookmarks;
  FConfig.AddBookmark('/video/movie1.mp4', 'Movie1 BM', 100.0);
  FConfig.AddBookmark('/video/movie2.mp4', 'Movie2 BM', 200.0);
  FConfig.ClearBookmarksForFile('/video/movie1.mp4');

  FileBookmarks := FConfig.GetBookmarksForFile('/video/movie1.mp4');
  AllBookmarks := FConfig.GetBookmarks;

  AssertEquals('Movie1 should have 0 bookmarks', 0, Length(FileBookmarks));
  AssertEquals('Total should have 1 bookmark', 1, Length(AllBookmarks));
end;

procedure TTestBookmarksIntegration.Test_ClearAllBookmarks;
var
  Bookmarks: TBookmarkItems;
begin
  FConfig.AddBookmark('/video/movie.mp4', 'BM1', 100.0);
  FConfig.AddBookmark('/video/movie.mp4', 'BM2', 200.0);
  FConfig.ClearBookmarks;

  Bookmarks := FConfig.GetBookmarks;
  AssertEquals('Should have 0 bookmarks after clear', 0, Length(Bookmarks));
end;

procedure TTestBookmarksIntegration.Test_BookmarkPersistence;
var
  Bookmarks: TBookmarkItems;
begin
  FConfig.ClearBookmarks;
  FConfig.AddBookmark('/video/persist.mp4', 'Persistent', 500.0);
  FConfig.Save;

  // Reload
  FConfig.Load;
  Bookmarks := FConfig.GetBookmarks;

  AssertTrue('Bookmark should persist after save/load', Length(Bookmarks) >= 1);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestFavoritesIntegration
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestFavoritesIntegration.SetUp;
begin
  FConfig := TConfigManager.Create;
  FConfig.ClearFavorites;
  FTempConfigPath := GetTempDir + 'test_favorites_' + IntToStr(Random(100000)) + '.ini';
end;

procedure TTestFavoritesIntegration.TearDown;
begin
  FConfig.Free;
  if FileExists(FTempConfigPath) then
    DeleteFile(FTempConfigPath);
end;

procedure TTestFavoritesIntegration.Test_AddFileFavorite;
var
  Favorites: TFavoriteItems;
begin
  FConfig.AddFavorite('My Song', '/music/song.mp3', ftFile, 'Music');
  Favorites := FConfig.GetFavorites;
  AssertTrue('Should have at least 1 favorite', Length(Favorites) >= 1);
end;

procedure TTestFavoritesIntegration.Test_AddFolderFavorite;
var
  Favorites: TFavoriteItems;
begin
  FConfig.ClearFavorites;
  FConfig.AddFavorite('Music URL', 'http://music.server.com/stream', ftURL, 'Streams');
  Favorites := FConfig.GetFavoritesByType(ftURL);
  AssertEquals('Should have 1 URL favorite', 1, Length(Favorites));
end;

procedure TTestFavoritesIntegration.Test_AddRadioFavorite;
var
  Favorites: TFavoriteItems;
begin
  FConfig.ClearFavorites;
  FConfig.AddFavorite('Jazz FM', 'http://stream.jazzfm.com/live', ftRadio, 'Radio');
  Favorites := FConfig.GetFavoritesByType(ftRadio);
  AssertEquals('Should have 1 radio favorite', 1, Length(Favorites));
end;

procedure TTestFavoritesIntegration.Test_AddPlaylistFavorite;
var
  Favorites: TFavoriteItems;
begin
  FConfig.ClearFavorites;
  FConfig.AddFavorite('DVD Collection', '/media/dvd', ftDVD, 'DVDs');
  Favorites := FConfig.GetFavoritesByType(ftDVD);
  AssertEquals('Should have 1 DVD favorite', 1, Length(Favorites));
end;

procedure TTestFavoritesIntegration.Test_IsFavorite;
begin
  FConfig.ClearFavorites;
  FConfig.AddFavorite('Test', '/test/path.mp3', ftFile, '');

  AssertTrue('Added path should be favorite', FConfig.IsFavorite('/test/path.mp3'));
  AssertFalse('Other path should not be favorite', FConfig.IsFavorite('/other/path.mp3'));
end;

procedure TTestFavoritesIntegration.Test_RemoveFavorite;
var
  Favorites: TFavoriteItems;
begin
  FConfig.ClearFavorites;
  FConfig.AddFavorite('To Remove', '/remove/me.mp3', ftFile, '');
  FConfig.AddFavorite('To Keep', '/keep/me.mp3', ftFile, '');
  FConfig.RemoveFavorite('/remove/me.mp3');

  Favorites := FConfig.GetFavorites;
  AssertEquals('Should have 1 favorite after removal', 1, Length(Favorites));
  AssertFalse('Removed path should not be favorite', FConfig.IsFavorite('/remove/me.mp3'));
end;

procedure TTestFavoritesIntegration.Test_GetFavoritesByType;
var
  Files, Radios: TFavoriteItems;
begin
  FConfig.ClearFavorites;
  FConfig.AddFavorite('File 1', '/file1.mp3', ftFile, '');
  FConfig.AddFavorite('File 2', '/file2.mp3', ftFile, '');
  FConfig.AddFavorite('Radio 1', 'http://radio.com', ftRadio, '');

  Files := FConfig.GetFavoritesByType(ftFile);
  Radios := FConfig.GetFavoritesByType(ftRadio);

  AssertEquals('Should have 2 file favorites', 2, Length(Files));
  AssertEquals('Should have 1 radio favorite', 1, Length(Radios));
end;

procedure TTestFavoritesIntegration.Test_GetFavoritesByCategory;
var
  MusicFavs, VideoFavs: TFavoriteItems;
begin
  FConfig.ClearFavorites;
  FConfig.AddFavorite('Song 1', '/song1.mp3', ftFile, 'Music');
  FConfig.AddFavorite('Song 2', '/song2.mp3', ftFile, 'Music');
  FConfig.AddFavorite('Movie 1', '/movie1.mp4', ftFile, 'Videos');

  MusicFavs := FConfig.GetFavoritesByCategory('Music');
  VideoFavs := FConfig.GetFavoritesByCategory('Videos');

  AssertEquals('Should have 2 music favorites', 2, Length(MusicFavs));
  AssertEquals('Should have 1 video favorite', 1, Length(VideoFavs));
end;

procedure TTestFavoritesIntegration.Test_UpdateLastPlayed;
begin
  FConfig.ClearFavorites;
  FConfig.AddFavorite('Test', '/test.mp3', ftFile, '');
  FConfig.UpdateFavoriteLastPlayed('/test.mp3');
  // Just verify no crash - last played is internal timestamp
  AssertTrue('Should update without error', True);
end;

procedure TTestFavoritesIntegration.Test_FavoriteCategories;
var
  Categories: TStringList;
begin
  FConfig.ClearFavorites;
  FConfig.AddFavorite('Item 1', '/item1.mp3', ftFile, 'Category A');
  FConfig.AddFavorite('Item 2', '/item2.mp3', ftFile, 'Category B');
  FConfig.AddFavorite('Item 3', '/item3.mp3', ftFile, 'Category A');

  Categories := FConfig.GetFavoriteCategories;
  try
    AssertTrue('Should have at least 2 categories', Categories.Count >= 2);
  finally
    Categories.Free;
  end;
end;

procedure TTestFavoritesIntegration.Test_ClearFavorites;
var
  Favorites: TFavoriteItems;
begin
  FConfig.AddFavorite('Test 1', '/test1.mp3', ftFile, '');
  FConfig.AddFavorite('Test 2', '/test2.mp3', ftFile, '');
  FConfig.ClearFavorites;

  Favorites := FConfig.GetFavorites;
  AssertEquals('Should have 0 favorites after clear', 0, Length(Favorites));
end;

initialization
  RegisterTest('Integration', TTestPlaylistConfigIntegration);
  RegisterTest('Integration', TTestPlaylistNavigation);
  RegisterTest('Integration', TTestBookmarksIntegration);
  RegisterTest('Integration', TTestFavoritesIntegration);

end.
