{ ═══════════════════════════════════════════════════════════════════════════════
  TestRunner.lpr - Main Test Program

  3nity Media - Test Suite

  This is the main entry point for the FPCUnit test framework.

  Usage:
    ./TestRunner --all              Run all tests
    ./TestRunner --suite=Unit       Run unit tests only
    ./TestRunner --suite=TTestCLIParams  Run specific test class
    ./TestRunner --format=xml       Output in XML format

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

program TestRunner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,  { Required for LCL initialization }
  Classes,
  SysUtils,
  CustApp,
  fpcunit,
  fpcunitreport,
  plaintestreport,
  consoletestrunner,
  { Unit tests }
  uTestCLIParams,
  uTestConfig,
  uTestPlaylistManager,
  uTestRadioManager,
  uTestStreamRecorder,
  uTestVisualEffects,
  uTestShortcuts,
  uTestLocale,
  uTestMPVConst,
  uTestMPVEngine,
  uTestMockMPVBehavior,
  uTestMockPlaylistBehavior,
  { Integration tests }
  uTestPlayback,
  uTestAudioVideo,
  uTestStreaming,
  uTestVisualization,
  uTestMPVPlaylistIntegration,
  uTestErrorScenarios,
  { Mocks }
  uMockMPVEngine,
  uMockPlaylist,
  { Performance tests }
  uTestStartup,
  uTestMediaLoading,
  uTestSeekPerformance,
  uTestMemoryUsage,
  { Robustness tests }
  uTestCorruptedFiles,
  uTestNetworkTimeout,
  uTestRapidCommands,
  uTestLongPlayback;

var
  App: TTestRunner;

begin
  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Title := '3nity Media - Test Suite';
  App.Run;
  App.Free;
end.
