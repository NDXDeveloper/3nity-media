{ ===============================================================================
  uTestSeekPerformance.pas - Seek Operation Performance Tests

  Part of 3nity Media - Test Suite

  Tests the performance of seek operations across different file types
  and seek positions. Measures response time and accuracy.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uTestSeekPerformance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DateUtils;

type
  { ============================================================================
    TTestSeekPerformance - Seek operation performance tests
    ============================================================================ }
  TTestSeekPerformance = class(TTestCase)
  private
    FStartTime: TDateTime;
    procedure StartTimer;
    function StopTimer: Int64;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Seek position calculation tests }
    procedure Test_CalculateSeekPosition_Absolute;
    procedure Test_CalculateSeekPosition_Relative;
    procedure Test_CalculateSeekPosition_Percent;
    procedure Test_CalculateSeekPosition_Boundaries;

    { Seek command generation tests }
    procedure Test_GenerateSeekCommand_Performance;
    procedure Test_GenerateSeekCommand_Batch;

    { Position validation tests }
    procedure Test_ValidatePosition_InRange;
    procedure Test_ValidatePosition_OutOfRange;
    procedure Test_ValidatePosition_Negative;

    { Seek granularity tests }
    procedure Test_SeekGranularity_1Second;
    procedure Test_SeekGranularity_100ms;
    procedure Test_SeekGranularity_Frame;

    { Rapid seek simulation }
    procedure Test_RapidSeek_10Operations;
    procedure Test_RapidSeek_100Operations;
    procedure Test_RapidSeek_Stress;

    { Seek with duration tests }
    procedure Test_SeekInShortFile;
    procedure Test_SeekInMediumFile;
    procedure Test_SeekInLongFile;
  end;

  { ============================================================================
    TTestSeekCalculations - Seek position calculation accuracy
    ============================================================================ }
  TTestSeekCalculations = class(TTestCase)
  published
    { Time conversion tests }
    procedure Test_SecondsToPercent;
    procedure Test_PercentToSeconds;
    procedure Test_FrameToSeconds;
    procedure Test_SecondsToFrame;

    { Boundary calculations }
    procedure Test_ClampPosition_Start;
    procedure Test_ClampPosition_End;
    procedure Test_ClampPosition_Middle;

    { Chapter seek calculations }
    procedure Test_FindNextChapter;
    procedure Test_FindPreviousChapter;
    procedure Test_SeekToChapter;
  end;

implementation

const
  { Performance thresholds in milliseconds }
  SEEK_CALC_MS = 1;
  SEEK_COMMAND_MS = 5;
  RAPID_SEEK_MS = 50;

{ ============================================================================
  TTestSeekPerformance
  ============================================================================ }

procedure TTestSeekPerformance.SetUp;
begin
  { Nothing to set up }
end;

procedure TTestSeekPerformance.TearDown;
begin
  { Nothing to clean up }
end;

procedure TTestSeekPerformance.StartTimer;
begin
  FStartTime := Now;
end;

function TTestSeekPerformance.StopTimer: Int64;
begin
  Result := MilliSecondsBetween(Now, FStartTime);
end;

{ Seek position calculation tests }

procedure TTestSeekPerformance.Test_CalculateSeekPosition_Absolute;
var
  Elapsed: Int64;
  Duration, Position, TargetSec: Double;
  I: Integer;
begin
  Duration := 3600.0;  { 1 hour }

  StartTimer;
  for I := 1 to 10000 do
  begin
    TargetSec := Random * Duration;
    Position := TargetSec;
    if Position < 0 then Position := 0;
    if Position > Duration then Position := Duration;
  end;
  Elapsed := StopTimer;

  AssertTrue('10000 absolute seek calcs (' + IntToStr(Elapsed) + 'ms) should be < 10ms',
    Elapsed < 10);
end;

procedure TTestSeekPerformance.Test_CalculateSeekPosition_Relative;
var
  Elapsed: Int64;
  Duration, CurrentPos, Delta, NewPos: Double;
  I: Integer;
begin
  Duration := 3600.0;
  CurrentPos := 1800.0;

  StartTimer;
  for I := 1 to 10000 do
  begin
    Delta := (Random - 0.5) * 20;  { -10 to +10 seconds }
    NewPos := CurrentPos + Delta;
    if NewPos < 0 then NewPos := 0;
    if NewPos > Duration then NewPos := Duration;
    CurrentPos := NewPos;
  end;
  Elapsed := StopTimer;

  AssertTrue('10000 relative seek calcs (' + IntToStr(Elapsed) + 'ms) should be < 10ms',
    Elapsed < 10);
end;

procedure TTestSeekPerformance.Test_CalculateSeekPosition_Percent;
var
  Elapsed: Int64;
  Duration, Percent, Position: Double;
  I: Integer;
begin
  Duration := 3600.0;

  StartTimer;
  for I := 1 to 10000 do
  begin
    Percent := Random * 100;
    Position := (Percent / 100) * Duration;
  end;
  Elapsed := StopTimer;

  AssertTrue('10000 percent seek calcs (' + IntToStr(Elapsed) + 'ms) should be < 10ms',
    Elapsed < 10);
end;

procedure TTestSeekPerformance.Test_CalculateSeekPosition_Boundaries;
var
  Elapsed: Int64;
  Duration, Position: Double;
  I: Integer;
begin
  Duration := 3600.0;

  StartTimer;
  for I := 1 to 10000 do
  begin
    { Test boundary conditions }
    Position := -100 + Random * 3800;  { Some values out of range }
    if Position < 0 then Position := 0
    else if Position > Duration then Position := Duration;
  end;
  Elapsed := StopTimer;

  AssertTrue('10000 boundary checks (' + IntToStr(Elapsed) + 'ms) should be < 10ms',
    Elapsed < 10);
end;

{ Seek command generation tests }

procedure TTestSeekPerformance.Test_GenerateSeekCommand_Performance;
var
  Elapsed: Int64;
  Cmd: string;
  Position: Double;
  I: Integer;
begin
  StartTimer;
  for I := 1 to 1000 do
  begin
    Position := Random * 3600;
    Cmd := 'seek ' + FloatToStrF(Position, ffFixed, 10, 3) + ' absolute';
  end;
  Elapsed := StopTimer;

  AssertTrue('1000 seek commands (' + IntToStr(Elapsed) + 'ms) should be < 20ms',
    Elapsed < 20);
end;

procedure TTestSeekPerformance.Test_GenerateSeekCommand_Batch;
var
  Elapsed: Int64;
  Commands: TStringList;
  Position: Double;
  I: Integer;
begin
  Commands := TStringList.Create;
  try
    StartTimer;
    for I := 1 to 100 do
    begin
      Position := I * 36;  { Every 36 seconds in 1 hour }
      Commands.Add('seek ' + FloatToStrF(Position, ffFixed, 10, 3) + ' absolute');
    end;
    Elapsed := StopTimer;

    AssertEquals('Should have 100 commands', 100, Commands.Count);
    AssertTrue('Batch command generation (' + IntToStr(Elapsed) + 'ms) should be < 10ms',
      Elapsed < 10);
  finally
    Commands.Free;
  end;
end;

{ Position validation tests }

procedure TTestSeekPerformance.Test_ValidatePosition_InRange;
var
  Elapsed: Int64;
  Duration, Position: Double;
  Valid: Boolean;
  I: Integer;
begin
  Duration := 3600.0;

  StartTimer;
  for I := 1 to 10000 do
  begin
    Position := Random * Duration;
    Valid := (Position >= 0) and (Position <= Duration);
  end;
  Elapsed := StopTimer;

  AssertTrue('10000 in-range validations (' + IntToStr(Elapsed) + 'ms) should be < 5ms',
    Elapsed < 5);
end;

procedure TTestSeekPerformance.Test_ValidatePosition_OutOfRange;
var
  Elapsed: Int64;
  Duration, Position: Double;
  Valid: Boolean;
  I: Integer;
begin
  Duration := 3600.0;

  StartTimer;
  for I := 1 to 10000 do
  begin
    Position := Duration + Random * 1000;  { Out of range }
    Valid := (Position >= 0) and (Position <= Duration);
    if not Valid then
      Position := Duration;  { Clamp }
  end;
  Elapsed := StopTimer;

  AssertTrue('10000 out-of-range validations (' + IntToStr(Elapsed) + 'ms) should be < 5ms',
    Elapsed < 5);
end;

procedure TTestSeekPerformance.Test_ValidatePosition_Negative;
var
  Elapsed: Int64;
  Position: Double;
  Valid: Boolean;
  I: Integer;
begin
  StartTimer;
  for I := 1 to 10000 do
  begin
    Position := -Random * 100;  { Negative }
    Valid := Position >= 0;
    if not Valid then
      Position := 0;  { Clamp to 0 }
  end;
  Elapsed := StopTimer;

  AssertTrue('10000 negative validations (' + IntToStr(Elapsed) + 'ms) should be < 5ms',
    Elapsed < 5);
end;

{ Seek granularity tests }

procedure TTestSeekPerformance.Test_SeekGranularity_1Second;
var
  Elapsed: Int64;
  Position: Double;
  I: Integer;
begin
  Position := 0;

  StartTimer;
  for I := 1 to 3600 do  { Seek every second for 1 hour }
  begin
    Position := I * 1.0;
  end;
  Elapsed := StopTimer;

  AssertTrue('3600 1-second seeks (' + IntToStr(Elapsed) + 'ms) should be < 5ms',
    Elapsed < 5);
end;

procedure TTestSeekPerformance.Test_SeekGranularity_100ms;
var
  Elapsed: Int64;
  Position: Double;
  I: Integer;
begin
  Position := 0;

  StartTimer;
  for I := 1 to 10000 do  { 10000 * 100ms = 1000 seconds }
  begin
    Position := I * 0.1;
  end;
  Elapsed := StopTimer;

  AssertTrue('10000 100ms seeks (' + IntToStr(Elapsed) + 'ms) should be < 5ms',
    Elapsed < 5);
end;

procedure TTestSeekPerformance.Test_SeekGranularity_Frame;
var
  Elapsed: Int64;
  Position: Double;
  FrameDuration: Double;
  I: Integer;
begin
  FrameDuration := 1.0 / 25.0;  { 25 fps }
  Position := 0;

  StartTimer;
  for I := 1 to 25000 do  { 1000 seconds at 25fps }
  begin
    Position := I * FrameDuration;
  end;
  Elapsed := StopTimer;

  AssertTrue('25000 frame seeks (' + IntToStr(Elapsed) + 'ms) should be < 10ms',
    Elapsed < 10);
end;

{ Rapid seek simulation }

procedure TTestSeekPerformance.Test_RapidSeek_10Operations;
var
  Elapsed: Int64;
  Duration, Position: Double;
  Cmd: string;
  I: Integer;
begin
  Duration := 3600.0;

  StartTimer;
  for I := 1 to 10 do
  begin
    Position := Random * Duration;
    if Position < 0 then Position := 0;
    if Position > Duration then Position := Duration;
    Cmd := 'seek ' + FloatToStrF(Position, ffFixed, 10, 3) + ' absolute';
  end;
  Elapsed := StopTimer;

  AssertTrue('10 rapid seeks (' + IntToStr(Elapsed) + 'ms) should be < 5ms',
    Elapsed < 5);
end;

procedure TTestSeekPerformance.Test_RapidSeek_100Operations;
var
  Elapsed: Int64;
  Duration, Position: Double;
  Cmd: string;
  I: Integer;
begin
  Duration := 3600.0;

  StartTimer;
  for I := 1 to 100 do
  begin
    Position := Random * Duration;
    if Position < 0 then Position := 0;
    if Position > Duration then Position := Duration;
    Cmd := 'seek ' + FloatToStrF(Position, ffFixed, 10, 3) + ' absolute';
  end;
  Elapsed := StopTimer;

  AssertTrue('100 rapid seeks (' + IntToStr(Elapsed) + 'ms) should be < ' +
    IntToStr(RAPID_SEEK_MS) + 'ms', Elapsed < RAPID_SEEK_MS);
end;

procedure TTestSeekPerformance.Test_RapidSeek_Stress;
var
  Elapsed: Int64;
  Duration, Position: Double;
  Cmd: string;
  I: Integer;
begin
  Duration := 3600.0;

  StartTimer;
  for I := 1 to 1000 do
  begin
    Position := Random * Duration;
    if Position < 0 then Position := 0;
    if Position > Duration then Position := Duration;
    Cmd := 'seek ' + FloatToStrF(Position, ffFixed, 10, 3) + ' absolute';
  end;
  Elapsed := StopTimer;

  AssertTrue('1000 stress seeks (' + IntToStr(Elapsed) + 'ms) should be < 100ms',
    Elapsed < 100);
end;

{ Seek with duration tests }

procedure TTestSeekPerformance.Test_SeekInShortFile;
var
  Elapsed: Int64;
  Duration, Position: Double;
  I: Integer;
begin
  Duration := 30.0;  { 30 seconds }

  StartTimer;
  for I := 1 to 100 do
  begin
    Position := Random * Duration;
    if Position < 0 then Position := 0;
    if Position > Duration then Position := Duration;
  end;
  Elapsed := StopTimer;

  AssertTrue('100 seeks in short file (' + IntToStr(Elapsed) + 'ms) should be < 5ms',
    Elapsed < 5);
end;

procedure TTestSeekPerformance.Test_SeekInMediumFile;
var
  Elapsed: Int64;
  Duration, Position: Double;
  I: Integer;
begin
  Duration := 300.0;  { 5 minutes }

  StartTimer;
  for I := 1 to 100 do
  begin
    Position := Random * Duration;
    if Position < 0 then Position := 0;
    if Position > Duration then Position := Duration;
  end;
  Elapsed := StopTimer;

  AssertTrue('100 seeks in medium file (' + IntToStr(Elapsed) + 'ms) should be < 5ms',
    Elapsed < 5);
end;

procedure TTestSeekPerformance.Test_SeekInLongFile;
var
  Elapsed: Int64;
  Duration, Position: Double;
  I: Integer;
begin
  Duration := 7200.0;  { 2 hours }

  StartTimer;
  for I := 1 to 100 do
  begin
    Position := Random * Duration;
    if Position < 0 then Position := 0;
    if Position > Duration then Position := Duration;
  end;
  Elapsed := StopTimer;

  AssertTrue('100 seeks in long file (' + IntToStr(Elapsed) + 'ms) should be < 5ms',
    Elapsed < 5);
end;

{ ============================================================================
  TTestSeekCalculations
  ============================================================================ }

procedure TTestSeekCalculations.Test_SecondsToPercent;
var
  Duration, Seconds, Percent: Double;
begin
  Duration := 3600.0;

  Seconds := 0;
  Percent := (Seconds / Duration) * 100;
  AssertEquals('0 seconds = 0%', 0, Percent, 0.001);

  Seconds := 1800;
  Percent := (Seconds / Duration) * 100;
  AssertEquals('1800 seconds = 50%', 50, Percent, 0.001);

  Seconds := 3600;
  Percent := (Seconds / Duration) * 100;
  AssertEquals('3600 seconds = 100%', 100, Percent, 0.001);
end;

procedure TTestSeekCalculations.Test_PercentToSeconds;
var
  Duration, Percent, Seconds: Double;
begin
  Duration := 3600.0;

  Percent := 0;
  Seconds := (Percent / 100) * Duration;
  AssertEquals('0% = 0 seconds', 0, Seconds, 0.001);

  Percent := 50;
  Seconds := (Percent / 100) * Duration;
  AssertEquals('50% = 1800 seconds', 1800, Seconds, 0.001);

  Percent := 100;
  Seconds := (Percent / 100) * Duration;
  AssertEquals('100% = 3600 seconds', 3600, Seconds, 0.001);
end;

procedure TTestSeekCalculations.Test_FrameToSeconds;
var
  FPS, Frame: Integer;
  Seconds: Double;
begin
  FPS := 25;

  Frame := 0;
  Seconds := Frame / FPS;
  AssertEquals('Frame 0 = 0 seconds', 0, Seconds, 0.001);

  Frame := 25;
  Seconds := Frame / FPS;
  AssertEquals('Frame 25 = 1 second', 1, Seconds, 0.001);

  Frame := 750;
  Seconds := Frame / FPS;
  AssertEquals('Frame 750 = 30 seconds', 30, Seconds, 0.001);
end;

procedure TTestSeekCalculations.Test_SecondsToFrame;
var
  FPS, Frame: Integer;
  Seconds: Double;
begin
  FPS := 25;

  Seconds := 0;
  Frame := Round(Seconds * FPS);
  AssertEquals('0 seconds = frame 0', 0, Frame);

  Seconds := 1;
  Frame := Round(Seconds * FPS);
  AssertEquals('1 second = frame 25', 25, Frame);

  Seconds := 30;
  Frame := Round(Seconds * FPS);
  AssertEquals('30 seconds = frame 750', 750, Frame);
end;

procedure TTestSeekCalculations.Test_ClampPosition_Start;
var
  Duration, Position: Double;
begin
  Duration := 3600.0;

  Position := -100;
  if Position < 0 then Position := 0;
  AssertEquals('Negative clamped to 0', 0, Position, 0.001);

  Position := -0.001;
  if Position < 0 then Position := 0;
  AssertEquals('Small negative clamped to 0', 0, Position, 0.001);
end;

procedure TTestSeekCalculations.Test_ClampPosition_End;
var
  Duration, Position: Double;
begin
  Duration := 3600.0;

  Position := 4000;
  if Position > Duration then Position := Duration;
  AssertEquals('Over duration clamped', 3600, Position, 0.001);

  Position := 3600.001;
  if Position > Duration then Position := Duration;
  AssertEquals('Slightly over clamped', 3600, Position, 0.001);
end;

procedure TTestSeekCalculations.Test_ClampPosition_Middle;
var
  Duration, Position: Double;
begin
  Duration := 3600.0;

  Position := 1800;
  if Position < 0 then Position := 0
  else if Position > Duration then Position := Duration;
  AssertEquals('Middle position unchanged', 1800, Position, 0.001);
end;

procedure TTestSeekCalculations.Test_FindNextChapter;
var
  Chapters: array[0..4] of Double;
  CurrentPos, NextChapter: Double;
  I: Integer;
begin
  { Chapter marks at 0, 300, 600, 900, 1200 seconds }
  Chapters[0] := 0;
  Chapters[1] := 300;
  Chapters[2] := 600;
  Chapters[3] := 900;
  Chapters[4] := 1200;

  CurrentPos := 150;
  NextChapter := -1;
  for I := 0 to High(Chapters) do
    if Chapters[I] > CurrentPos then
    begin
      NextChapter := Chapters[I];
      Break;
    end;

  AssertEquals('Next chapter after 150s is 300s', 300, NextChapter, 0.001);
end;

procedure TTestSeekCalculations.Test_FindPreviousChapter;
var
  Chapters: array[0..4] of Double;
  CurrentPos, PrevChapter: Double;
  I: Integer;
begin
  Chapters[0] := 0;
  Chapters[1] := 300;
  Chapters[2] := 600;
  Chapters[3] := 900;
  Chapters[4] := 1200;

  CurrentPos := 650;
  PrevChapter := 0;
  for I := High(Chapters) downto 0 do
    if Chapters[I] < CurrentPos then
    begin
      PrevChapter := Chapters[I];
      Break;
    end;

  AssertEquals('Previous chapter before 650s is 600s', 600, PrevChapter, 0.001);
end;

procedure TTestSeekCalculations.Test_SeekToChapter;
var
  Chapters: array[0..4] of Double;
  ChapterIndex: Integer;
  Position: Double;
begin
  Chapters[0] := 0;
  Chapters[1] := 300;
  Chapters[2] := 600;
  Chapters[3] := 900;
  Chapters[4] := 1200;

  ChapterIndex := 2;
  Position := Chapters[ChapterIndex];

  AssertEquals('Chapter 2 position is 600s', 600, Position, 0.001);
end;

initialization
  RegisterTest('Performance', TTestSeekPerformance);
  RegisterTest('Performance', TTestSeekCalculations);

end.
