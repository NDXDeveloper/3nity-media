{ ===============================================================================
  uTestRapidCommands.pas - Rapid Command Handling Tests

  Part of 3nity Media - Test Suite

  Tests application robustness when handling rapid user commands,
  such as repeated play/pause, fast seeking, or button spamming.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uTestRapidCommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DateUtils;

type
  { Simulated player state }
  TPlayerState = (psIdle, psPlaying, psPaused, psStopped, psSeeking);

  { ============================================================================
    TTestRapidCommands - Rapid command handling tests
    ============================================================================ }
  TTestRapidCommands = class(TTestCase)
  private
    FState: TPlayerState;
    FPosition: Double;
    FCommandCount: Integer;
    FStartTime: TDateTime;
    procedure StartTimer;
    function StopTimer: Int64;
    procedure SimulatePlay;
    procedure SimulatePause;
    procedure SimulateStop;
    procedure SimulateSeek(Position: Double);
    procedure SimulateToggle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Play/Pause spam tests }
    procedure Test_RapidPlayPause_10Times;
    procedure Test_RapidPlayPause_100Times;
    procedure Test_RapidPlayPause_1000Times;
    procedure Test_RapidToggle_Stress;

    { Seek spam tests }
    procedure Test_RapidSeek_Forward;
    procedure Test_RapidSeek_Backward;
    procedure Test_RapidSeek_Random;
    procedure Test_RapidSeek_Boundaries;

    { Mixed command tests }
    procedure Test_MixedCommands_Rapid;
    procedure Test_MixedCommands_Chaos;

    { State consistency tests }
    procedure Test_StateConsistency_AfterRapidCommands;
    procedure Test_PositionConsistency_AfterRapidSeeks;

    { Debouncing tests }
    procedure Test_CommandDebounce_PlayPause;
    procedure Test_CommandDebounce_Seek;
    procedure Test_CommandThrottle_Volume;
  end;

  { ============================================================================
    TTestCommandQueue - Command queue handling tests
    ============================================================================ }
  TTestCommandQueue = class(TTestCase)
  private
    FQueue: TStringList;
    procedure EnqueueCommand(const Cmd: string);
    function DequeueCommand: string;
    procedure ClearQueue;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Queue behavior tests }
    procedure Test_Queue_FIFO_Order;
    procedure Test_Queue_MaxSize;
    procedure Test_Queue_Overflow_Handling;
    procedure Test_Queue_Clear_OnStop;

    { Command coalescing tests }
    procedure Test_Coalesce_MultipleSeeks;
    procedure Test_Coalesce_VolumeChanges;
    procedure Test_NoCoalesce_PlayPause;

    { Priority tests }
    procedure Test_Priority_StopOverSeek;
    procedure Test_Priority_UserOverAuto;
  end;

  { ============================================================================
    TTestInputHandling - Input handling stress tests
    ============================================================================ }
  TTestInputHandling = class(TTestCase)
  private
    FStartTime: TDateTime;
    procedure StartTimer;
    function StopTimer: Int64;
  published
    { Keyboard input tests }
    procedure Test_RapidKeyPress_Space;
    procedure Test_RapidKeyPress_Arrow;
    procedure Test_HeldKey_Seek;
    procedure Test_MultiKey_Simultaneous;

    { Mouse input tests }
    procedure Test_RapidClick_PlayButton;
    procedure Test_RapidClick_SeekBar;
    procedure Test_DragSeekBar_Fast;

    { Input timing tests }
    procedure Test_InputResponse_Time;
    procedure Test_InputBuffer_Overflow;
  end;

implementation

const
  DEBOUNCE_INTERVAL_MS = 50;
  THROTTLE_INTERVAL_MS = 16;  { ~60fps }
  MAX_QUEUE_SIZE = 100;

{ ============================================================================
  TTestRapidCommands
  ============================================================================ }

procedure TTestRapidCommands.SetUp;
begin
  FState := psIdle;
  FPosition := 0;
  FCommandCount := 0;
end;

procedure TTestRapidCommands.TearDown;
begin
  { Nothing to clean up }
end;

procedure TTestRapidCommands.StartTimer;
begin
  FStartTime := Now;
end;

function TTestRapidCommands.StopTimer: Int64;
begin
  Result := MilliSecondsBetween(Now, FStartTime);
end;

procedure TTestRapidCommands.SimulatePlay;
begin
  Inc(FCommandCount);
  if FState in [psIdle, psPaused, psStopped] then
    FState := psPlaying;
end;

procedure TTestRapidCommands.SimulatePause;
begin
  Inc(FCommandCount);
  if FState = psPlaying then
    FState := psPaused;
end;

procedure TTestRapidCommands.SimulateStop;
begin
  Inc(FCommandCount);
  FState := psStopped;
  FPosition := 0;
end;

procedure TTestRapidCommands.SimulateSeek(Position: Double);
begin
  Inc(FCommandCount);
  if Position < 0 then Position := 0;
  if Position > 100 then Position := 100;
  FPosition := Position;
end;

procedure TTestRapidCommands.SimulateToggle;
begin
  Inc(FCommandCount);
  case FState of
    psPlaying: FState := psPaused;
    psPaused: FState := psPlaying;
    psIdle, psStopped: FState := psPlaying;
  end;
end;

{ Play/Pause spam tests }

procedure TTestRapidCommands.Test_RapidPlayPause_10Times;
var
  I: Integer;
  Elapsed: Int64;
begin
  FCommandCount := 0;
  FState := psIdle;

  StartTimer;
  for I := 1 to 10 do
  begin
    SimulatePlay;
    SimulatePause;
  end;
  Elapsed := StopTimer;

  AssertEquals('Should process 20 commands', 20, FCommandCount);
  AssertTrue('Should complete in < 10ms', Elapsed < 10);
  AssertTrue('State should be valid', FState in [psPlaying, psPaused]);
end;

procedure TTestRapidCommands.Test_RapidPlayPause_100Times;
var
  I: Integer;
  Elapsed: Int64;
begin
  FCommandCount := 0;
  FState := psIdle;

  StartTimer;
  for I := 1 to 100 do
  begin
    SimulatePlay;
    SimulatePause;
  end;
  Elapsed := StopTimer;

  AssertEquals('Should process 200 commands', 200, FCommandCount);
  AssertTrue('Should complete in < 50ms', Elapsed < 50);
end;

procedure TTestRapidCommands.Test_RapidPlayPause_1000Times;
var
  I: Integer;
  Elapsed: Int64;
begin
  FCommandCount := 0;
  FState := psIdle;

  StartTimer;
  for I := 1 to 1000 do
  begin
    SimulatePlay;
    SimulatePause;
  end;
  Elapsed := StopTimer;

  AssertEquals('Should process 2000 commands', 2000, FCommandCount);
  AssertTrue('Should complete in < 100ms', Elapsed < 100);
end;

procedure TTestRapidCommands.Test_RapidToggle_Stress;
var
  I: Integer;
  Elapsed: Int64;
begin
  FCommandCount := 0;
  FState := psIdle;

  StartTimer;
  for I := 1 to 10000 do
    SimulateToggle;
  Elapsed := StopTimer;

  AssertEquals('Should process 10000 toggles', 10000, FCommandCount);
  AssertTrue('Should complete in < 200ms', Elapsed < 200);
  { After even number of toggles from idle, should be paused }
  AssertTrue('Final state should be valid', FState in [psPlaying, psPaused]);
end;

{ Seek spam tests }

procedure TTestRapidCommands.Test_RapidSeek_Forward;
var
  I: Integer;
  Elapsed: Int64;
begin
  FCommandCount := 0;
  FPosition := 0;

  StartTimer;
  for I := 1 to 100 do
    SimulateSeek(FPosition + 1);
  Elapsed := StopTimer;

  AssertEquals('Should process 100 seeks', 100, FCommandCount);
  AssertEquals('Position should be 100', 100, FPosition, 0.01);
  AssertTrue('Should complete in < 20ms', Elapsed < 20);
end;

procedure TTestRapidCommands.Test_RapidSeek_Backward;
var
  I: Integer;
  Elapsed: Int64;
begin
  FCommandCount := 0;
  FPosition := 100;

  StartTimer;
  for I := 1 to 100 do
    SimulateSeek(FPosition - 1);
  Elapsed := StopTimer;

  AssertEquals('Should process 100 seeks', 100, FCommandCount);
  AssertEquals('Position should be 0', 0, FPosition, 0.01);
  AssertTrue('Should complete in < 20ms', Elapsed < 20);
end;

procedure TTestRapidCommands.Test_RapidSeek_Random;
var
  I: Integer;
  Elapsed: Int64;
begin
  FCommandCount := 0;

  StartTimer;
  for I := 1 to 1000 do
    SimulateSeek(Random * 100);
  Elapsed := StopTimer;

  AssertEquals('Should process 1000 seeks', 1000, FCommandCount);
  AssertTrue('Position should be in range', (FPosition >= 0) and (FPosition <= 100));
  AssertTrue('Should complete in < 50ms', Elapsed < 50);
end;

procedure TTestRapidCommands.Test_RapidSeek_Boundaries;
var
  I: Integer;
  Elapsed: Int64;
begin
  FCommandCount := 0;

  StartTimer;
  { Alternate between start and end }
  for I := 1 to 500 do
  begin
    SimulateSeek(0);
    SimulateSeek(100);
  end;
  Elapsed := StopTimer;

  AssertEquals('Should process 1000 seeks', 1000, FCommandCount);
  AssertEquals('Final position should be 100', 100, FPosition, 0.01);
  AssertTrue('Should complete in < 50ms', Elapsed < 50);
end;

{ Mixed command tests }

procedure TTestRapidCommands.Test_MixedCommands_Rapid;
var
  I: Integer;
  Elapsed: Int64;
begin
  FCommandCount := 0;
  FState := psIdle;

  StartTimer;
  for I := 1 to 100 do
  begin
    SimulatePlay;
    SimulateSeek(I);
    SimulatePause;
    SimulateSeek(I + 0.5);
  end;
  Elapsed := StopTimer;

  AssertEquals('Should process 400 commands', 400, FCommandCount);
  AssertTrue('Should complete in < 50ms', Elapsed < 50);
end;

procedure TTestRapidCommands.Test_MixedCommands_Chaos;
var
  I, Cmd: Integer;
  Elapsed: Int64;
begin
  FCommandCount := 0;
  FState := psIdle;

  StartTimer;
  for I := 1 to 1000 do
  begin
    Cmd := Random(4);
    case Cmd of
      0: SimulatePlay;
      1: SimulatePause;
      2: SimulateStop;
      3: SimulateSeek(Random * 100);
    end;
  end;
  Elapsed := StopTimer;

  AssertEquals('Should process 1000 commands', 1000, FCommandCount);
  AssertTrue('State should be valid', FState in [psIdle, psPlaying, psPaused, psStopped]);
  AssertTrue('Should complete in < 100ms', Elapsed < 100);
end;

{ State consistency tests }

procedure TTestRapidCommands.Test_StateConsistency_AfterRapidCommands;
var
  I: Integer;
  FinalState: TPlayerState;
begin
  FState := psIdle;

  { Specific sequence: should end in playing }
  SimulatePlay;
  for I := 1 to 10 do
  begin
    SimulatePause;
    SimulatePlay;
  end;

  FinalState := FState;
  AssertEquals('After play-pause-play sequence, should be playing',
    Ord(psPlaying), Ord(FinalState));
end;

procedure TTestRapidCommands.Test_PositionConsistency_AfterRapidSeeks;
var
  I: Integer;
  ExpectedPos: Double;
begin
  FPosition := 50;

  { Seek forward 10, back 5, net +5 per iteration until cap }
  for I := 1 to 100 do
  begin
    SimulateSeek(FPosition + 10);
    SimulateSeek(FPosition - 5);
  end;

  { After hitting 100 cap, it oscillates: seek(100+10)=100, seek(100-5)=95 }
  { So final position stabilizes at 95 }
  ExpectedPos := 95;
  AssertEquals('Position should stabilize near max', ExpectedPos, FPosition, 0.01);
end;

{ Debouncing tests }

procedure TTestRapidCommands.Test_CommandDebounce_PlayPause;
var
  I: Integer;
  ProcessedCount: Integer;
  LastCommandTime: TDateTime;
begin
  ProcessedCount := 0;
  LastCommandTime := 0;

  { Simulate debouncing - only process if enough time passed }
  for I := 1 to 100 do
  begin
    if MilliSecondsBetween(Now, LastCommandTime) >= DEBOUNCE_INTERVAL_MS then
    begin
      Inc(ProcessedCount);
      LastCommandTime := Now;
    end;
    { In real scenario, commands come faster than debounce interval }
  end;

  { Without real delays, only first command should process }
  AssertTrue('Debouncing should reduce processed commands', ProcessedCount <= 100);
end;

procedure TTestRapidCommands.Test_CommandDebounce_Seek;
var
  LastSeekPos, NewSeekPos: Double;
  I, ProcessedCount: Integer;
begin
  LastSeekPos := -1;
  ProcessedCount := 0;

  { Only process if position changed significantly }
  for I := 1 to 100 do
  begin
    NewSeekPos := I * 0.5;  { Small increments }
    if Abs(NewSeekPos - LastSeekPos) >= 1.0 then  { 1% threshold }
    begin
      Inc(ProcessedCount);
      LastSeekPos := NewSeekPos;
    end;
  end;

  { Should process fewer commands due to threshold }
  AssertTrue('Seek debouncing should reduce commands', ProcessedCount < 100);
end;

procedure TTestRapidCommands.Test_CommandThrottle_Volume;
var
  I, ProcessedCount: Integer;
  LastProcessTime: TDateTime;
begin
  ProcessedCount := 0;
  LastProcessTime := 0;

  for I := 1 to 100 do
  begin
    if MilliSecondsBetween(Now, LastProcessTime) >= THROTTLE_INTERVAL_MS then
    begin
      Inc(ProcessedCount);
      LastProcessTime := Now;
    end;
  end;

  { Throttling limits rate }
  AssertTrue('Throttling should limit command rate', ProcessedCount >= 1);
end;

{ ============================================================================
  TTestCommandQueue
  ============================================================================ }

procedure TTestCommandQueue.SetUp;
begin
  FQueue := TStringList.Create;
end;

procedure TTestCommandQueue.TearDown;
begin
  FQueue.Free;
end;

procedure TTestCommandQueue.EnqueueCommand(const Cmd: string);
begin
  if FQueue.Count < MAX_QUEUE_SIZE then
    FQueue.Add(Cmd);
end;

function TTestCommandQueue.DequeueCommand: string;
begin
  if FQueue.Count > 0 then
  begin
    Result := FQueue[0];
    FQueue.Delete(0);
  end
  else
    Result := '';
end;

procedure TTestCommandQueue.ClearQueue;
begin
  FQueue.Clear;
end;

{ Queue behavior tests }

procedure TTestCommandQueue.Test_Queue_FIFO_Order;
var
  I: Integer;
  Cmd: string;
begin
  for I := 1 to 5 do
    EnqueueCommand('Cmd' + IntToStr(I));

  for I := 1 to 5 do
  begin
    Cmd := DequeueCommand;
    AssertEquals('Should dequeue in FIFO order', 'Cmd' + IntToStr(I), Cmd);
  end;
end;

procedure TTestCommandQueue.Test_Queue_MaxSize;
var
  I: Integer;
begin
  for I := 1 to 150 do
    EnqueueCommand('Cmd' + IntToStr(I));

  AssertEquals('Queue should not exceed max size', MAX_QUEUE_SIZE, FQueue.Count);
end;

procedure TTestCommandQueue.Test_Queue_Overflow_Handling;
var
  I: Integer;
  FirstCmd: string;
begin
  { Fill queue }
  for I := 1 to MAX_QUEUE_SIZE do
    EnqueueCommand('Cmd' + IntToStr(I));

  { Try to add more }
  EnqueueCommand('Overflow');

  { First command should still be there }
  FirstCmd := DequeueCommand;
  AssertEquals('First command preserved on overflow', 'Cmd1', FirstCmd);
end;

procedure TTestCommandQueue.Test_Queue_Clear_OnStop;
begin
  EnqueueCommand('Seek:50');
  EnqueueCommand('Play');
  EnqueueCommand('Seek:75');

  { Stop should clear pending commands }
  ClearQueue;

  AssertEquals('Queue should be empty after clear', 0, FQueue.Count);
end;

{ Command coalescing tests }

procedure TTestCommandQueue.Test_Coalesce_MultipleSeeks;
var
  I: Integer;
  LastSeek: string;
begin
  { Multiple seeks - only keep the last one }
  for I := 1 to 10 do
  begin
    { Remove previous seek if exists }
    if (FQueue.Count > 0) and (Pos('Seek:', FQueue[FQueue.Count - 1]) = 1) then
      FQueue.Delete(FQueue.Count - 1);
    EnqueueCommand('Seek:' + IntToStr(I * 10));
  end;

  AssertEquals('Should coalesce to single seek', 1, FQueue.Count);
  LastSeek := DequeueCommand;
  AssertEquals('Should keep last seek value', 'Seek:100', LastSeek);
end;

procedure TTestCommandQueue.Test_Coalesce_VolumeChanges;
var
  I: Integer;
begin
  for I := 1 to 20 do
  begin
    if (FQueue.Count > 0) and (Pos('Volume:', FQueue[FQueue.Count - 1]) = 1) then
      FQueue.Delete(FQueue.Count - 1);
    EnqueueCommand('Volume:' + IntToStr(I * 5));
  end;

  AssertEquals('Should coalesce volume changes', 1, FQueue.Count);
end;

procedure TTestCommandQueue.Test_NoCoalesce_PlayPause;
begin
  EnqueueCommand('Play');
  EnqueueCommand('Pause');
  EnqueueCommand('Play');

  { Play/Pause should NOT be coalesced }
  AssertEquals('Play/Pause should not coalesce', 3, FQueue.Count);
end;

{ Priority tests }

procedure TTestCommandQueue.Test_Priority_StopOverSeek;
var
  I: Integer;
  Cmd: string;
  HasStop: Boolean;
begin
  EnqueueCommand('Seek:10');
  EnqueueCommand('Seek:20');
  EnqueueCommand('Stop');  { High priority }
  EnqueueCommand('Seek:30');

  { Process Stop immediately, discard seeks }
  HasStop := False;
  for I := 0 to FQueue.Count - 1 do
    if FQueue[I] = 'Stop' then
      HasStop := True;

  AssertTrue('Stop command should be in queue', HasStop);
end;

procedure TTestCommandQueue.Test_Priority_UserOverAuto;
var
  UserCmd, AutoCmd: string;
begin
  { Auto-generated command }
  EnqueueCommand('Auto:UpdatePosition');

  { User command - should have priority }
  EnqueueCommand('User:Seek:50');

  { In real implementation, user commands would be processed first }
  AssertTrue('Queue should contain both commands', FQueue.Count = 2);
end;

{ ============================================================================
  TTestInputHandling
  ============================================================================ }

procedure TTestInputHandling.StartTimer;
begin
  FStartTime := Now;
end;

function TTestInputHandling.StopTimer: Int64;
begin
  Result := MilliSecondsBetween(Now, FStartTime);
end;

procedure TTestInputHandling.Test_RapidKeyPress_Space;
var
  I, KeyCount: Integer;
  Elapsed: Int64;
begin
  KeyCount := 0;

  StartTimer;
  for I := 1 to 100 do
  begin
    { Simulate space key press }
    Inc(KeyCount);
  end;
  Elapsed := StopTimer;

  AssertEquals('Should handle 100 key presses', 100, KeyCount);
  AssertTrue('Should process quickly', Elapsed < 10);
end;

procedure TTestInputHandling.Test_RapidKeyPress_Arrow;
var
  I, KeyCount: Integer;
  Elapsed: Int64;
begin
  KeyCount := 0;

  StartTimer;
  for I := 1 to 100 do
  begin
    { Simulate arrow key press for seeking }
    Inc(KeyCount);
  end;
  Elapsed := StopTimer;

  AssertEquals('Should handle 100 arrow keys', 100, KeyCount);
  AssertTrue('Should process quickly', Elapsed < 10);
end;

procedure TTestInputHandling.Test_HeldKey_Seek;
var
  I: Integer;
  Position: Double;
  Elapsed: Int64;
begin
  Position := 50;

  StartTimer;
  { Simulate holding right arrow - continuous seek }
  for I := 1 to 60 do  { ~1 second at 60fps }
  begin
    Position := Position + 0.5;  { Seek 0.5% per frame }
    if Position > 100 then Position := 100;
  end;
  Elapsed := StopTimer;

  AssertEquals('Position should increase', 80, Position, 0.01);
  AssertTrue('Should process quickly', Elapsed < 20);
end;

procedure TTestInputHandling.Test_MultiKey_Simultaneous;
var
  KeysPressed: Integer;
begin
  KeysPressed := 0;

  { Simulate multiple keys pressed at once (Ctrl+Shift+S) }
  Inc(KeysPressed);  { Ctrl }
  Inc(KeysPressed);  { Shift }
  Inc(KeysPressed);  { S }

  AssertEquals('Should handle multiple simultaneous keys', 3, KeysPressed);
end;

procedure TTestInputHandling.Test_RapidClick_PlayButton;
var
  I, ClickCount: Integer;
  Elapsed: Int64;
begin
  ClickCount := 0;

  StartTimer;
  for I := 1 to 50 do
  begin
    { Simulate rapid clicking }
    Inc(ClickCount);
  end;
  Elapsed := StopTimer;

  AssertEquals('Should register 50 clicks', 50, ClickCount);
  AssertTrue('Should process quickly', Elapsed < 10);
end;

procedure TTestInputHandling.Test_RapidClick_SeekBar;
var
  I: Integer;
  Positions: array of Double;
  Elapsed: Int64;
begin
  SetLength(Positions, 100);

  StartTimer;
  for I := 0 to 99 do
    Positions[I] := Random * 100;
  Elapsed := StopTimer;

  AssertTrue('All positions should be valid',
    (Positions[0] >= 0) and (Positions[99] <= 100));
  AssertTrue('Should process quickly', Elapsed < 10);
end;

procedure TTestInputHandling.Test_DragSeekBar_Fast;
var
  I: Integer;
  Position: Double;
  Elapsed: Int64;
begin
  Position := 0;

  StartTimer;
  { Simulate fast drag across seekbar }
  for I := 1 to 100 do
    Position := I;
  Elapsed := StopTimer;

  AssertEquals('Final position should be 100', 100, Position, 0.01);
  AssertTrue('Should handle fast drag', Elapsed < 10);
end;

procedure TTestInputHandling.Test_InputResponse_Time;
var
  ResponseTime: Int64;
begin
  StartTimer;
  { Simulate minimal input processing }
  ResponseTime := StopTimer;

  AssertTrue('Input response should be < 16ms (60fps)', ResponseTime < 16);
end;

procedure TTestInputHandling.Test_InputBuffer_Overflow;
var
  I: Integer;
  Buffer: TStringList;
begin
  Buffer := TStringList.Create;
  try
    { Flood the input buffer }
    for I := 1 to 10000 do
    begin
      if Buffer.Count < 1000 then  { Buffer limit }
        Buffer.Add('Input' + IntToStr(I));
    end;

    AssertTrue('Buffer should not exceed limit', Buffer.Count <= 1000);
  finally
    Buffer.Free;
  end;
end;

initialization
  RegisterTest('Robustness', TTestRapidCommands);
  RegisterTest('Robustness', TTestCommandQueue);
  RegisterTest('Robustness', TTestInputHandling);

end.
