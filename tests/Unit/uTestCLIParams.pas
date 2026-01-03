{ ═══════════════════════════════════════════════════════════════════════════════
  uTestCLIParams.pas - Unit Tests for CLI Parameters

  Part of 3nity Media - Test Suite

  Tests for the uCLIParams unit which handles command-line parameter parsing.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uTestCLIParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  uCLIParams;

type
  { TTestCLIParams }
  TTestCLIParams = class(TTestCase)
  published
    { ParseTimeToSeconds tests }
    procedure Test_ParseTimeToSeconds_SecondsOnly;
    procedure Test_ParseTimeToSeconds_MinutesSeconds;
    procedure Test_ParseTimeToSeconds_HoursMinutesSeconds;
    procedure Test_ParseTimeToSeconds_WithDecimals;
    procedure Test_ParseTimeToSeconds_Zero;
    procedure Test_ParseTimeToSeconds_LargeValues;
    procedure Test_ParseTimeToSeconds_EmptyString;
    procedure Test_ParseTimeToSeconds_InvalidFormat;
    procedure Test_ParseTimeToSeconds_InvalidCharacters;

    { TStartupOptions default values tests }
    procedure Test_StartupOptions_DefaultValues;

    { TCLIAction enum tests }
    procedure Test_CLIAction_EnumValues;
  end;

implementation

{ TTestCLIParams }

{ ─────────────────────────────────────────────────────────────────────────────
  ParseTimeToSeconds Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestCLIParams.Test_ParseTimeToSeconds_SecondsOnly;
begin
  AssertEquals('30 seconds', 30.0, ParseTimeToSeconds('30'));
  AssertEquals('0 seconds', 0.0, ParseTimeToSeconds('0'));
  AssertEquals('1 second', 1.0, ParseTimeToSeconds('1'));
  AssertEquals('59 seconds', 59.0, ParseTimeToSeconds('59'));
  AssertEquals('120 seconds', 120.0, ParseTimeToSeconds('120'));
end;

procedure TTestCLIParams.Test_ParseTimeToSeconds_MinutesSeconds;
begin
  AssertEquals('1:30 = 90s', 90.0, ParseTimeToSeconds('1:30'));
  AssertEquals('0:45 = 45s', 45.0, ParseTimeToSeconds('0:45'));
  AssertEquals('10:00 = 600s', 600.0, ParseTimeToSeconds('10:00'));
  AssertEquals('59:59 = 3599s', 3599.0, ParseTimeToSeconds('59:59'));
  AssertEquals('1:00 = 60s', 60.0, ParseTimeToSeconds('1:00'));
end;

procedure TTestCLIParams.Test_ParseTimeToSeconds_HoursMinutesSeconds;
begin
  AssertEquals('1:00:00 = 3600s', 3600.0, ParseTimeToSeconds('1:00:00'));
  AssertEquals('0:01:30 = 90s', 90.0, ParseTimeToSeconds('0:01:30'));
  AssertEquals('2:30:00 = 9000s', 9000.0, ParseTimeToSeconds('2:30:00'));
  AssertEquals('1:30:45 = 5445s', 5445.0, ParseTimeToSeconds('1:30:45'));
  AssertEquals('0:00:01 = 1s', 1.0, ParseTimeToSeconds('0:00:01'));
end;

procedure TTestCLIParams.Test_ParseTimeToSeconds_WithDecimals;
begin
  AssertEquals('30.5 seconds', 30.5, ParseTimeToSeconds('30.5'));
  AssertEquals('1:30.5 = 90.5s', 90.5, ParseTimeToSeconds('1:30.5'));
  AssertEquals('0.1 seconds', 0.1, ParseTimeToSeconds('0.1'));
end;

procedure TTestCLIParams.Test_ParseTimeToSeconds_Zero;
begin
  AssertEquals('0', 0.0, ParseTimeToSeconds('0'));
  AssertEquals('0:00', 0.0, ParseTimeToSeconds('0:00'));
  AssertEquals('0:00:00', 0.0, ParseTimeToSeconds('0:00:00'));
end;

procedure TTestCLIParams.Test_ParseTimeToSeconds_LargeValues;
begin
  { 99*3600 + 59*60 + 59 = 359999 }
  AssertTrue('99:59:59 = 359999s', Abs(ParseTimeToSeconds('99:59:59') - 359999.0) < 0.001);
  AssertTrue('10:00:00 = 36000s', Abs(ParseTimeToSeconds('10:00:00') - 36000.0) < 0.001);
  AssertTrue('100:00 = 6000s', Abs(ParseTimeToSeconds('100:00') - 6000.0) < 0.001);
end;

procedure TTestCLIParams.Test_ParseTimeToSeconds_EmptyString;
begin
  AssertEquals('Empty string returns -1', -1.0, ParseTimeToSeconds(''));
end;

procedure TTestCLIParams.Test_ParseTimeToSeconds_InvalidFormat;
begin
  AssertEquals('Too many colons', -1.0, ParseTimeToSeconds('1:2:3:4'));
  AssertEquals('Just colons', -1.0, ParseTimeToSeconds('::'));
end;

procedure TTestCLIParams.Test_ParseTimeToSeconds_InvalidCharacters;
begin
  AssertEquals('Letters', -1.0, ParseTimeToSeconds('abc'));
  AssertEquals('Mixed letters', -1.0, ParseTimeToSeconds('1:2a:3'));
  AssertEquals('Special chars', -1.0, ParseTimeToSeconds('1@30'));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TStartupOptions Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestCLIParams.Test_StartupOptions_DefaultValues;
var
  Options: TStartupOptions;
begin
  { Initialize to check default values in the record }
  FillChar(Options, SizeOf(Options), 0);

  { Note: ParseStartupOptions reads actual command line args,
    so we just test the record structure here }
  AssertEquals('Default Fullscreen', False, Options.Fullscreen);
  AssertEquals('Default FullscreenSet', False, Options.FullscreenSet);
  AssertEquals('Default Mute', False, Options.Mute);
  AssertEquals('Default MuteSet', False, Options.MuteSet);
  AssertEquals('Default Loop', False, Options.Loop);
  AssertEquals('Default LoopSet', False, Options.LoopSet);
  AssertEquals('Default SubFile', '', Options.SubFile);
  AssertEquals('Default SubFileSet', False, Options.SubFileSet);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TCLIAction Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestCLIParams.Test_CLIAction_EnumValues;
begin
  { Test enum values are correctly defined }
  AssertEquals('caNoAction = 0', 0, Ord(caNoAction));
  AssertEquals('caShowHelp = 1', 1, Ord(caShowHelp));
  AssertEquals('caShowVersion = 2', 2, Ord(caShowVersion));
  AssertEquals('caShowLicense = 3', 3, Ord(caShowLicense));
end;

initialization
  RegisterTest(TTestCLIParams);

end.
