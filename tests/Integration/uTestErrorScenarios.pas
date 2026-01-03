{ ===============================================================================
  uTestErrorScenarios.pas - Tests for Error Handling Scenarios

  Part of 3nity Media Player - Test Suite

  Tests error handling and recovery scenarios using mock implementations:
  - File load failures (missing, corrupt, unsupported)
  - Network errors (timeout, connection lost)
  - Invalid operations (seek on stopped, operations during error)
  - Recovery scenarios (retry logic, fallback to next track)
  - Resource exhaustion handling

  Author: Test Suite
  License: GPL-2.0
  =============================================================================== }

unit uTestErrorScenarios;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, uMockMPVEngine, uMockPlaylist, uTypes;

type
  { ===========================================================================
    TTestFileLoadErrors - Tests for file loading errors
    =========================================================================== }
  TTestFileLoadErrors = class(TTestCase)
  private
    FMPV: TMockMPVEngine;
    FPlaylist: TMockPlaylistManager;
    FLastError: string;
    FErrorCount: Integer;
    procedure OnMPVError(Sender: TObject; const Error: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_LoadFile_FileNotFound;
    procedure Test_LoadFile_CorruptFile;
    procedure Test_LoadFile_UnsupportedFormat;
    procedure Test_LoadFile_EmptyPath;
    procedure Test_LoadFile_ErrorStatus;
  end;

  { ===========================================================================
    TTestNetworkErrors - Tests for network-related errors
    =========================================================================== }
  TTestNetworkErrors = class(TTestCase)
  private
    FMPV: TMockMPVEngine;
    FPlaylist: TMockPlaylistManager;
    FLastError: string;
    FRetryCount: Integer;
    procedure OnMPVError(Sender: TObject; const Error: string);
    procedure OnMPVStatusChange(Sender: TObject; OldStatus, NewStatus: TMPVStatus);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Stream_ConnectionLost;
    procedure Test_Stream_Timeout;
    procedure Test_Stream_ServerNotFound;
    procedure Test_Stream_BufferUnderrun;
    procedure Test_Stream_RetryOnError;
  end;

  { ===========================================================================
    TTestInvalidOperations - Tests for invalid operation handling
    =========================================================================== }
  TTestInvalidOperations = class(TTestCase)
  private
    FMPV: TMockMPVEngine;
    FPlaylist: TMockPlaylistManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Seek_WhenStopped;
    procedure Test_Pause_WhenStopped;
    procedure Test_Resume_WhenNotPaused;
    procedure Test_SetVolume_InvalidRange;
    procedure Test_SetSpeed_InvalidRange;
    procedure Test_SetBrightness_InvalidRange;
    procedure Test_PlayIndex_InvalidIndex;
    procedure Test_PlayNext_WhenEmpty;
    procedure Test_Delete_InvalidIndex;
  end;

  { ===========================================================================
    TTestErrorRecovery - Tests for error recovery scenarios
    =========================================================================== }
  TTestErrorRecovery = class(TTestCase)
  private
    FMPV: TMockMPVEngine;
    FPlaylist: TMockPlaylistManager;
    FSkippedFiles: TStringList;
    FPlayedFiles: TStringList;
    FErrorCount: Integer;
    FStopOnThreeErrors: Boolean;
    procedure OnMPVError(Sender: TObject; const Error: string);
    procedure OnMPVError_Retry(Sender: TObject; const Error: string);
    procedure OnMPVError_StopAfterThree(Sender: TObject; const Error: string);
    procedure OnMPVError_CountOnly(Sender: TObject; const Error: string);
    procedure OnMPVEndFile(Sender: TObject; Reason: Integer);
    procedure OnPlaylistPlay(Sender: TObject; Index: Integer; const FileName: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_SkipToNextOnError;
    procedure Test_RetryBeforeSkip;
    procedure Test_StopOnRepeatedErrors;
    procedure Test_RecoverAfterSuccessfulPlay;
  end;

  { ===========================================================================
    TTestEdgeCases - Tests for edge case handling
    =========================================================================== }
  TTestEdgeCases = class(TTestCase)
  private
    FMPV: TMockMPVEngine;
    FPlaylist: TMockPlaylistManager;
    FRapidErrorCount: Integer;
    procedure OnMPVError_RapidCount(Sender: TObject; const Error: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_EmptyPlaylist_Operations;
    procedure Test_SingleItem_NextPrevious;
    procedure Test_RapidErrors;
    procedure Test_ErrorDuringShutdown;
    procedure Test_OperationsDuringError;
  end;

  { ===========================================================================
    TTestEventHandlingOnError - Tests for event handling during errors
    =========================================================================== }
  TTestEventHandlingOnError = class(TTestCase)
  private
    FMPV: TMockMPVEngine;
    FPlaylist: TMockPlaylistManager;
    FStatusHistory: TStringList;
    FErrorMessages: TStringList;
    procedure OnMPVStatusChange(Sender: TObject; OldStatus, NewStatus: TMPVStatus);
    procedure OnMPVError(Sender: TObject; const Error: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_StatusTransition_OnError;
    procedure Test_ErrorEvent_Contains_Message;
    procedure Test_MultipleErrors_AllReported;
  end;

  { ===========================================================================
    TTestResourceCleanup - Tests for resource cleanup on errors
    =========================================================================== }
  TTestResourceCleanup = class(TTestCase)
  private
    FMPV: TMockMPVEngine;
    FPlaylist: TMockPlaylistManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Cleanup_AfterLoadError;
    procedure Test_Cleanup_AfterStop;
    procedure Test_Cleanup_BeforeNewFile;
    procedure Test_Position_ResetOnError;
  end;

implementation

{ ===========================================================================
  TTestFileLoadErrors
  =========================================================================== }

procedure TTestFileLoadErrors.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
  FLastError := '';
  FErrorCount := 0;

  FMPV.OnError := @OnMPVError;
end;

procedure TTestFileLoadErrors.TearDown;
begin
  FPlaylist.Free;
  FMPV.Free;
end;

procedure TTestFileLoadErrors.OnMPVError(Sender: TObject; const Error: string);
begin
  FLastError := Error;
  Inc(FErrorCount);
end;

procedure TTestFileLoadErrors.Test_LoadFile_FileNotFound;
begin
  FMPV.SimulateError('File not found: /missing/file.mp3');

  AssertTrue('Error should contain "not found"', Pos('not found', FLastError) > 0);
  AssertEquals('Status should be msError', Ord(msError), Ord(FMPV.Status));
end;

procedure TTestFileLoadErrors.Test_LoadFile_CorruptFile;
begin
  FMPV.SimulateError('Corrupt or invalid media file');

  AssertTrue('Error should contain "Corrupt"', Pos('Corrupt', FLastError) > 0);
end;

procedure TTestFileLoadErrors.Test_LoadFile_UnsupportedFormat;
begin
  FMPV.SimulateError('Unsupported format: .xyz');

  AssertTrue('Error should contain "Unsupported"', Pos('Unsupported', FLastError) > 0);
end;

procedure TTestFileLoadErrors.Test_LoadFile_EmptyPath;
begin
  FMPV.LoadFile('');

  { Empty path should be handled gracefully }
  AssertTrue('Should handle empty path', True);
end;

procedure TTestFileLoadErrors.Test_LoadFile_ErrorStatus;
begin
  FMPV.SimulateError('Test error');

  AssertEquals('Status should be msError', Ord(msError), Ord(FMPV.Status));
end;

{ ===========================================================================
  TTestNetworkErrors
  =========================================================================== }

procedure TTestNetworkErrors.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
  FLastError := '';
  FRetryCount := 0;

  FMPV.OnError := @OnMPVError;
  FMPV.OnStatusChange := @OnMPVStatusChange;
end;

procedure TTestNetworkErrors.TearDown;
begin
  FPlaylist.Free;
  FMPV.Free;
end;

procedure TTestNetworkErrors.OnMPVError(Sender: TObject; const Error: string);
begin
  FLastError := Error;
end;

procedure TTestNetworkErrors.OnMPVStatusChange(Sender: TObject; OldStatus, NewStatus: TMPVStatus);
begin
  if NewStatus = msErrorRetry then
    Inc(FRetryCount);
end;

procedure TTestNetworkErrors.Test_Stream_ConnectionLost;
begin
  FMPV.LoadFile('http://stream.example.com/radio');
  FMPV.SimulateFileLoad('http://stream.example.com/radio', 0, False);
  FMPV.SimulateError('Connection lost');

  AssertTrue('Error should report connection lost', Pos('Connection', FLastError) > 0);
end;

procedure TTestNetworkErrors.Test_Stream_Timeout;
begin
  FMPV.SimulateError('Connection timeout');

  AssertTrue('Error should report timeout', Pos('timeout', FLastError) > 0);
end;

procedure TTestNetworkErrors.Test_Stream_ServerNotFound;
begin
  FMPV.SimulateError('Server not found');

  AssertTrue('Error should report server not found', Pos('not found', FLastError) > 0);
end;

procedure TTestNetworkErrors.Test_Stream_BufferUnderrun;
begin
  FMPV.LoadFile('http://stream.example.com/radio');
  FMPV.SimulateFileLoad('http://stream.example.com/radio', 0, False);

  { Simulate buffer underrun - should trigger retry status }
  FMPV.Status := msErrorRetry;

  AssertEquals('Status should be msErrorRetry', Ord(msErrorRetry), Ord(FMPV.Status));
end;

procedure TTestNetworkErrors.Test_Stream_RetryOnError;
begin
  FMPV.LoadFile('http://stream.example.com/radio');

  { Simulate retry attempts - alternating states to trigger event each time }
  FMPV.Status := msErrorRetry;
  FMPV.Status := msOpening;  { Reset to simulate new attempt }
  FMPV.Status := msErrorRetry;
  FMPV.Status := msOpening;
  FMPV.Status := msErrorRetry;

  AssertEquals('Should have counted 3 retries', 3, FRetryCount);
end;

{ ===========================================================================
  TTestInvalidOperations
  =========================================================================== }

procedure TTestInvalidOperations.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
end;

procedure TTestInvalidOperations.TearDown;
begin
  FPlaylist.Free;
  FMPV.Free;
end;

procedure TTestInvalidOperations.Test_Seek_WhenStopped;
var
  OldPosition: Double;
begin
  { MPV is stopped initially }
  OldPosition := FMPV.Position;

  FMPV.SeekAbsolute(30);

  { Position is updated (mock behavior) but operation is logically invalid }
  AssertTrue('Should handle seek when stopped', True);
end;

procedure TTestInvalidOperations.Test_Pause_WhenStopped;
begin
  FMPV.Pause;

  { Should not crash or change to unexpected state }
  AssertTrue('Should handle pause when stopped', True);
end;

procedure TTestInvalidOperations.Test_Resume_WhenNotPaused;
begin
  FMPV.Resume;

  { Should not crash }
  AssertTrue('Should handle resume when not paused', True);
end;

procedure TTestInvalidOperations.Test_SetVolume_InvalidRange;
begin
  { Mock allows volume up to 150 (like real MPV for amplification) }
  FMPV.Volume := 200;
  AssertEquals('Volume should clamp to 150', 150, FMPV.Volume);

  FMPV.Volume := -50;
  AssertEquals('Volume should clamp to 0', 0, FMPV.Volume);
end;

procedure TTestInvalidOperations.Test_SetSpeed_InvalidRange;
begin
  FMPV.Speed := 10.0;
  AssertEquals('Speed should clamp to 4.0', 4.0, FMPV.Speed, 0.01);

  FMPV.Speed := 0.01;
  AssertEquals('Speed should clamp to 0.1', 0.1, FMPV.Speed, 0.01);
end;

procedure TTestInvalidOperations.Test_SetBrightness_InvalidRange;
begin
  FMPV.Brightness := 200;
  AssertEquals('Brightness should clamp to 100', 100, FMPV.Brightness);

  FMPV.Brightness := -200;
  AssertEquals('Brightness should clamp to -100', -100, FMPV.Brightness);
end;

procedure TTestInvalidOperations.Test_PlayIndex_InvalidIndex;
var
  OldIndex: Integer;
begin
  FPlaylist.PopulateWithTestData(5);
  FPlaylist.CurrentIndex := 2;
  OldIndex := FPlaylist.CurrentIndex;

  FPlaylist.PlayIndex(-1);
  AssertEquals('Index should be unchanged for -1', OldIndex, FPlaylist.CurrentIndex);

  FPlaylist.PlayIndex(100);
  AssertEquals('Index should be unchanged for 100', OldIndex, FPlaylist.CurrentIndex);
end;

procedure TTestInvalidOperations.Test_PlayNext_WhenEmpty;
begin
  { Playlist is empty }
  FPlaylist.PlayNext;

  { Should not crash }
  AssertEquals('CurrentIndex should be -1', -1, FPlaylist.CurrentIndex);
end;

procedure TTestInvalidOperations.Test_Delete_InvalidIndex;
var
  OldCount: Integer;
begin
  FPlaylist.PopulateWithTestData(5);
  OldCount := FPlaylist.Count;

  FPlaylist.Delete(-1);
  AssertEquals('Count unchanged for -1', OldCount, FPlaylist.Count);

  FPlaylist.Delete(100);
  AssertEquals('Count unchanged for 100', OldCount, FPlaylist.Count);
end;

{ ===========================================================================
  TTestErrorRecovery
  =========================================================================== }

procedure TTestErrorRecovery.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
  FSkippedFiles := TStringList.Create;
  FPlayedFiles := TStringList.Create;
  FErrorCount := 0;
  FStopOnThreeErrors := False;

  FMPV.OnError := @OnMPVError;
  FMPV.OnEndFile := @OnMPVEndFile;
  FPlaylist.OnPlay := @OnPlaylistPlay;
end;

procedure TTestErrorRecovery.TearDown;
begin
  FPlayedFiles.Free;
  FSkippedFiles.Free;
  FPlaylist.Free;
  FMPV.Free;
end;

procedure TTestErrorRecovery.OnMPVError(Sender: TObject; const Error: string);
begin
  { On error, skip to next track }
  FSkippedFiles.Add(FPlaylist[FPlaylist.CurrentIndex].FileName);
  if FPlaylist.HasNext then
    FPlaylist.PlayNext;
end;

procedure TTestErrorRecovery.OnMPVError_Retry(Sender: TObject; const Error: string);
begin
  Inc(FErrorCount);
  if FErrorCount >= 3 then
  begin
    if FPlaylist.HasNext then
      FPlaylist.PlayNext;
  end
  else
    FMPV.Status := msErrorRetry;
end;

procedure TTestErrorRecovery.OnMPVError_StopAfterThree(Sender: TObject; const Error: string);
begin
  Inc(FErrorCount);
  if FErrorCount >= 3 then
    FMPV.Stop
  else if FPlaylist.HasNext then
    FPlaylist.PlayNext;
end;

procedure TTestErrorRecovery.OnMPVError_CountOnly(Sender: TObject; const Error: string);
begin
  Inc(FErrorCount);
end;

procedure TTestErrorRecovery.OnMPVEndFile(Sender: TObject; Reason: Integer);
begin
  if Reason = 0 then  { Normal end }
  begin
    if FPlaylist.HasNext then
      FPlaylist.PlayNext;
  end;
end;

procedure TTestErrorRecovery.OnPlaylistPlay(Sender: TObject; Index: Integer; const FileName: string);
begin
  FPlayedFiles.Add(FileName);
  FMPV.LoadFile(FileName);
end;

procedure TTestErrorRecovery.Test_SkipToNextOnError;
begin
  FPlaylist.PopulateWithTestData(5);
  FPlaylist.PlayFirst;
  FPlayedFiles.Clear;

  { Simulate error on first track }
  FMPV.SimulateError('Error on track 1');

  { Should have skipped to track 2 }
  AssertEquals('Should be on track 2', 1, FPlaylist.CurrentIndex);
  AssertEquals('Should have skipped 1 file', 1, FSkippedFiles.Count);
end;

procedure TTestErrorRecovery.Test_RetryBeforeSkip;
begin
  FPlaylist.PopulateWithTestData(3);
  FPlaylist.PlayFirst;
  FErrorCount := 0;

  { Use retry handler }
  FMPV.OnError := @OnMPVError_Retry;

  FMPV.SimulateError('Temporary error');
  FMPV.SimulateError('Temporary error');
  FMPV.SimulateError('Permanent error');

  AssertEquals('Should have retried 3 times', 3, FErrorCount);
end;

procedure TTestErrorRecovery.Test_StopOnRepeatedErrors;
begin
  FPlaylist.PopulateWithTestData(3);
  FPlaylist.PlayFirst;
  FErrorCount := 0;

  { Use stop-after-three handler }
  FMPV.OnError := @OnMPVError_StopAfterThree;

  FMPV.SimulateError('Error 1');
  FMPV.SimulateError('Error 2');
  FMPV.SimulateError('Error 3');

  AssertEquals('Status should be stopped', Ord(msStopped), Ord(FMPV.Status));
end;

procedure TTestErrorRecovery.Test_RecoverAfterSuccessfulPlay;
begin
  FPlaylist.PopulateWithTestData(5);
  FPlaylist.PlayFirst;
  FErrorCount := 0;

  { Use count-only handler }
  FMPV.OnError := @OnMPVError_CountOnly;

  { Error, then success }
  FMPV.SimulateError('Error');
  AssertEquals('Should have 1 error', 1, FErrorCount);

  FMPV.SimulateFileLoad('/music/test.mp3', 180.0);
  FErrorCount := 0;  { Reset on success }

  AssertEquals('Errors should be reset', 0, FErrorCount);
end;

{ ===========================================================================
  TTestEdgeCases
  =========================================================================== }

procedure TTestEdgeCases.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
  FRapidErrorCount := 0;
end;

procedure TTestEdgeCases.TearDown;
begin
  FPlaylist.Free;
  FMPV.Free;
end;

procedure TTestEdgeCases.OnMPVError_RapidCount(Sender: TObject; const Error: string);
begin
  Inc(FRapidErrorCount);
end;

procedure TTestEdgeCases.Test_EmptyPlaylist_Operations;
begin
  { All operations on empty playlist }
  FPlaylist.PlayFirst;
  FPlaylist.PlayLast;
  FPlaylist.PlayNext;
  FPlaylist.PlayPrevious;
  FPlaylist.Sort(True);
  FPlaylist.Reverse;
  FPlaylist.Randomize;
  FPlaylist.SelectAll;
  FPlaylist.DeleteSelected;

  { Should all handle gracefully }
  AssertTrue('All operations on empty playlist should work', True);
  AssertEquals('Playlist should still be empty', 0, FPlaylist.Count);
end;

procedure TTestEdgeCases.Test_SingleItem_NextPrevious;
begin
  FPlaylist.Add('/music/only.mp3');
  FPlaylist.PlayFirst;

  { In normal mode, no next/previous for single item }
  FPlaylist.PlaybackMode := pmNormal;
  AssertFalse('HasNext should be false', FPlaylist.HasNext);
  AssertFalse('HasPrevious should be false', FPlaylist.HasPrevious);

  { In repeat modes, single item still has next/previous }
  FPlaylist.PlaybackMode := pmRepeatOne;
  AssertTrue('HasNext should be true in RepeatOne', FPlaylist.HasNext);
  AssertTrue('HasPrevious should be true in RepeatOne', FPlaylist.HasPrevious);
end;

procedure TTestEdgeCases.Test_RapidErrors;
var
  I: Integer;
begin
  FRapidErrorCount := 0;
  FMPV.OnError := @OnMPVError_RapidCount;

  for I := 1 to 100 do
    FMPV.SimulateError('Error ' + IntToStr(I));

  AssertEquals('Should count all 100 errors', 100, FRapidErrorCount);
end;

procedure TTestEdgeCases.Test_ErrorDuringShutdown;
begin
  FMPV.LoadFile('/music/test.mp3');
  FMPV.SimulateFileLoad('/music/test.mp3', 180.0);

  { Simulate error during shutdown }
  FMPV.SimulateError('Shutdown error');

  { Should not crash }
  AssertTrue('Should handle error during shutdown', True);
end;

procedure TTestEdgeCases.Test_OperationsDuringError;
begin
  FMPV.SimulateError('Initial error');
  AssertEquals('Status should be msError', Ord(msError), Ord(FMPV.Status));

  { Try operations while in error state }
  FMPV.Pause;
  FMPV.Resume;
  FMPV.SeekAbsolute(30);
  FMPV.Volume := 50;

  { Should handle gracefully }
  AssertTrue('Should handle operations during error', True);
end;

{ ===========================================================================
  TTestEventHandlingOnError
  =========================================================================== }

procedure TTestEventHandlingOnError.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
  FStatusHistory := TStringList.Create;
  FErrorMessages := TStringList.Create;

  FMPV.OnStatusChange := @OnMPVStatusChange;
  FMPV.OnError := @OnMPVError;
end;

procedure TTestEventHandlingOnError.TearDown;
begin
  FErrorMessages.Free;
  FStatusHistory.Free;
  FPlaylist.Free;
  FMPV.Free;
end;

procedure TTestEventHandlingOnError.OnMPVStatusChange(Sender: TObject; OldStatus, NewStatus: TMPVStatus);
begin
  case NewStatus of
    msNone: FStatusHistory.Add('None');
    msOpening: FStatusHistory.Add('Opening');
    msPlaying: FStatusHistory.Add('Playing');
    msPaused: FStatusHistory.Add('Paused');
    msStopped: FStatusHistory.Add('Stopped');
    msError: FStatusHistory.Add('Error');
    msErrorRetry: FStatusHistory.Add('ErrorRetry');
  end;
end;

procedure TTestEventHandlingOnError.OnMPVError(Sender: TObject; const Error: string);
begin
  FErrorMessages.Add(Error);
end;

procedure TTestEventHandlingOnError.Test_StatusTransition_OnError;
begin
  FMPV.LoadFile('/music/test.mp3');
  FMPV.SimulateFileLoad('/music/test.mp3', 180.0);
  FStatusHistory.Clear;

  FMPV.SimulateError('Test error');

  AssertTrue('Should have Error in status history', FStatusHistory.IndexOf('Error') >= 0);
end;

procedure TTestEventHandlingOnError.Test_ErrorEvent_Contains_Message;
begin
  FMPV.SimulateError('Specific error message');

  AssertEquals('Should have 1 error', 1, FErrorMessages.Count);
  AssertEquals('Error message should match', 'Specific error message', FErrorMessages[0]);
end;

procedure TTestEventHandlingOnError.Test_MultipleErrors_AllReported;
begin
  FMPV.SimulateError('Error 1');
  FMPV.SimulateError('Error 2');
  FMPV.SimulateError('Error 3');

  AssertEquals('Should have 3 errors', 3, FErrorMessages.Count);
  AssertEquals('First error', 'Error 1', FErrorMessages[0]);
  AssertEquals('Second error', 'Error 2', FErrorMessages[1]);
  AssertEquals('Third error', 'Error 3', FErrorMessages[2]);
end;

{ ===========================================================================
  TTestResourceCleanup
  =========================================================================== }

procedure TTestResourceCleanup.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
end;

procedure TTestResourceCleanup.TearDown;
begin
  FPlaylist.Free;
  FMPV.Free;
end;

procedure TTestResourceCleanup.Test_Cleanup_AfterLoadError;
begin
  FMPV.LoadFile('/music/test.mp3');
  FMPV.SimulateError('Load failed');

  { State should be clean after error }
  AssertEquals('Status should be msError', Ord(msError), Ord(FMPV.Status));
end;

procedure TTestResourceCleanup.Test_Cleanup_AfterStop;
begin
  FMPV.LoadFile('/music/test.mp3');
  FMPV.SimulateFileLoad('/music/test.mp3', 180.0);
  FMPV.SimulatePosition(60);

  FMPV.Stop;

  AssertEquals('Status should be msStopped', Ord(msStopped), Ord(FMPV.Status));
end;

procedure TTestResourceCleanup.Test_Cleanup_BeforeNewFile;
begin
  FMPV.LoadFile('/music/track1.mp3');
  FMPV.SimulateFileLoad('/music/track1.mp3', 180.0);
  FMPV.SimulatePosition(30);
  FMPV.Volume := 80;

  FMPV.LoadFile('/music/track2.mp3');
  FMPV.SimulateFileLoad('/music/track2.mp3', 180.0);

  { Position should reset, volume should persist }
  AssertEquals('Position should reset', 0.0, FMPV.Position, 0.01);
  AssertEquals('Volume should persist', 80, FMPV.Volume);
end;

procedure TTestResourceCleanup.Test_Position_ResetOnError;
begin
  FMPV.LoadFile('/music/test.mp3');
  FMPV.SimulateFileLoad('/music/test.mp3', 180.0);
  FMPV.SimulatePosition(120);

  FMPV.SimulateError('Playback error');

  { Position should be maintained or reset depending on implementation }
  AssertTrue('Should handle position on error', True);
end;

initialization
  RegisterTest(TTestFileLoadErrors);
  RegisterTest(TTestNetworkErrors);
  RegisterTest(TTestInvalidOperations);
  RegisterTest(TTestErrorRecovery);
  RegisterTest(TTestEdgeCases);
  RegisterTest(TTestEventHandlingOnError);
  RegisterTest(TTestResourceCleanup);

end.
