{ ===============================================================================
  uCLIParams.pas - Command Line Interface Parameters Handler

  Part of 3nity Media - Lazarus Edition

  This unit handles command-line parameter parsing and execution for CLI-only
  operations (--help, --version, --license) that don't require the GUI.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uCLIParams;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  { CLI action result }
  TCLIAction = (
    caNoAction,       { No CLI-only action, continue with GUI }
    caShowHelp,       { Show help and exit }
    caShowVersion,    { Show version and exit }
    caShowLicense     { Show license and exit }
  );

  { Startup options passed via command line }
  TStartupOptions = record
    Fullscreen: Boolean;       { --fullscreen, -f }
    FullscreenSet: Boolean;    { True if explicitly set via CLI }
    Volume: Integer;           { --volume=N (0-100) }
    VolumeSet: Boolean;        { True if explicitly set via CLI }
    Mute: Boolean;             { --mute }
    MuteSet: Boolean;          { True if explicitly set via CLI }
    Loop: Boolean;             { --loop }
    LoopSet: Boolean;          { True if explicitly set via CLI }
    Speed: Double;             { --speed=N (0.25-4.0) }
    SpeedSet: Boolean;         { True if explicitly set via CLI }
    StartTime: Double;         { --start=TIME in seconds }
    StartTimeSet: Boolean;     { True if explicitly set via CLI }
    SubFile: string;           { --sub=FILE external subtitle }
    SubFileSet: Boolean;       { True if explicitly set via CLI }
  end;

{ Check if a CLI-only action is requested }
function GetCLIAction: TCLIAction;

{ Execute CLI action (outputs to console and returns exit code) }
function ExecuteCLIAction(Action: TCLIAction): Integer;

{ Parse startup options from command line }
function ParseStartupOptions: TStartupOptions;

{ Helper: Check if a specific parameter is present }
function HasParam(const ShortForm, LongForm: string): Boolean;

{ Helper: Get value from parameter like --volume=50 }
function GetParamValue(const LongForm: string): string;

{ Helper: Parse time string (SS, MM:SS, or HH:MM:SS) to seconds }
function ParseTimeToSeconds(const TimeStr: string): Double;

implementation

uses
  uAppVersion, uConstants, uLocale;

const
  LICENSE_TEXT =
    'GNU GENERAL PUBLIC LICENSE' + LineEnding +
    'Version 2, June 1991' + LineEnding +
    LineEnding +
    'Copyright (C) 1989, 1991 Free Software Foundation, Inc.' + LineEnding +
    '51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA' + LineEnding +
    LineEnding +
    'Everyone is permitted to copy and distribute verbatim copies' + LineEnding +
    'of this license document, but changing it is not allowed.' + LineEnding +
    LineEnding +
    '3nity Media is free software; you can redistribute it and/or modify' + LineEnding +
    'it under the terms of the GNU General Public License as published by' + LineEnding +
    'the Free Software Foundation; either version 2 of the License, or' + LineEnding +
    '(at your option) any later version.' + LineEnding +
    LineEnding +
    '3nity Media is distributed in the hope that it will be useful,' + LineEnding +
    'but WITHOUT ANY WARRANTY; without even the implied warranty of' + LineEnding +
    'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the' + LineEnding +
    'GNU General Public License for more details.' + LineEnding +
    LineEnding +
    'You should have received a copy of the GNU General Public License' + LineEnding +
    'along with this program; if not, write to the Free Software' + LineEnding +
    'Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,' + LineEnding +
    'MA 02110-1301, USA.' + LineEnding +
    LineEnding +
    'For the complete license text, visit:' + LineEnding +
    'https://www.gnu.org/licenses/old-licenses/gpl-2.0.html';

function HasParam(const ShortForm, LongForm: string): Boolean;
var
  I: Integer;
  Param: string;
begin
  Result := False;
  for I := 1 to ParamCount do
  begin
    Param := LowerCase(ParamStr(I));
    if (Param = ShortForm) or (Param = LongForm) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function GetParamValue(const LongForm: string): string;
var
  I: Integer;
  Param, Prefix: string;
  EqPos: Integer;
begin
  Result := '';
  Prefix := LowerCase(LongForm) + '=';

  for I := 1 to ParamCount do
  begin
    Param := ParamStr(I);
    if Pos(Prefix, LowerCase(Param)) = 1 then
    begin
      EqPos := Pos('=', Param);
      if EqPos > 0 then
      begin
        Result := Copy(Param, EqPos + 1, Length(Param) - EqPos);
        Exit;
      end;
    end;
  end;
end;

function ParseTimeToSeconds(const TimeStr: string): Double;
var
  Parts: array of string;
  I, ColonCount: Integer;
  Hours, Minutes, Seconds: Double;
  TempStr, Part: string;
  StartPos, EndPos: Integer;
begin
  Result := -1; { Invalid by default }
  if TimeStr = '' then Exit;

  { Count colons to determine format }
  ColonCount := 0;
  for I := 1 to Length(TimeStr) do
    if TimeStr[I] = ':' then Inc(ColonCount);

  { Parse based on format }
  TempStr := TimeStr;
  SetLength(Parts, ColonCount + 1);

  { Split by colon }
  StartPos := 1;
  I := 0;
  while StartPos <= Length(TempStr) do
  begin
    EndPos := StartPos;
    while (EndPos <= Length(TempStr)) and (TempStr[EndPos] <> ':') do
      Inc(EndPos);
    Parts[I] := Copy(TempStr, StartPos, EndPos - StartPos);
    Inc(I);
    StartPos := EndPos + 1;
  end;

  try
    case ColonCount of
      0: begin
        { SS format }
        Result := StrToFloat(Parts[0]);
      end;
      1: begin
        { MM:SS format }
        Minutes := StrToFloat(Parts[0]);
        Seconds := StrToFloat(Parts[1]);
        Result := Minutes * 60 + Seconds;
      end;
      2: begin
        { HH:MM:SS format }
        Hours := StrToFloat(Parts[0]);
        Minutes := StrToFloat(Parts[1]);
        Seconds := StrToFloat(Parts[2]);
        Result := Hours * 3600 + Minutes * 60 + Seconds;
      end;
    end;
  except
    Result := -1; { Invalid format }
  end;
end;

function ParseStartupOptions: TStartupOptions;
var
  VolumeStr, SpeedStr, StartStr: string;
  VolumeVal: Integer;
  SpeedVal, StartVal: Double;
begin
  { Initialize all fields to default }
  Result.Fullscreen := False;
  Result.FullscreenSet := False;
  Result.Volume := 100;
  Result.VolumeSet := False;
  Result.Mute := False;
  Result.MuteSet := False;
  Result.Loop := False;
  Result.LoopSet := False;
  Result.Speed := 1.0;
  Result.SpeedSet := False;
  Result.StartTime := 0;
  Result.StartTimeSet := False;
  Result.SubFile := '';
  Result.SubFileSet := False;

  { Parse --fullscreen / -f }
  if HasParam('-f', '--fullscreen') then
  begin
    Result.Fullscreen := True;
    Result.FullscreenSet := True;
  end;

  { Parse --volume=N }
  VolumeStr := GetParamValue('--volume');
  if VolumeStr <> '' then
  begin
    if TryStrToInt(VolumeStr, VolumeVal) then
    begin
      { Clamp to valid range 0-100 }
      if VolumeVal < 0 then VolumeVal := 0;
      if VolumeVal > 100 then VolumeVal := 100;
      Result.Volume := VolumeVal;
      Result.VolumeSet := True;
    end;
  end;

  { Parse --mute }
  if HasParam('', '--mute') then
  begin
    Result.Mute := True;
    Result.MuteSet := True;
  end;

  { Parse --loop }
  if HasParam('', '--loop') then
  begin
    Result.Loop := True;
    Result.LoopSet := True;
  end;

  { Parse --speed=N }
  SpeedStr := GetParamValue('--speed');
  if SpeedStr <> '' then
  begin
    if TryStrToFloat(SpeedStr, SpeedVal) then
    begin
      { Clamp to valid range 0.25-4.0 }
      if SpeedVal < 0.25 then SpeedVal := 0.25;
      if SpeedVal > 4.0 then SpeedVal := 4.0;
      Result.Speed := SpeedVal;
      Result.SpeedSet := True;
    end;
  end;

  { Parse --start=TIME }
  StartStr := GetParamValue('--start');
  if StartStr <> '' then
  begin
    StartVal := ParseTimeToSeconds(StartStr);
    if StartVal >= 0 then
    begin
      Result.StartTime := StartVal;
      Result.StartTimeSet := True;
    end;
  end;

  { Parse --sub=FILE }
  Result.SubFile := GetParamValue('--sub');
  if Result.SubFile <> '' then
    Result.SubFileSet := True;
end;

function GetCLIAction: TCLIAction;
begin
  Result := caNoAction;

  { Check for help first (highest priority) }
  if HasParam('-h', '--help') then
  begin
    Result := caShowHelp;
    Exit;
  end;

  { Check for version }
  if HasParam('-v', '--version') then
  begin
    Result := caShowVersion;
    Exit;
  end;

  { Check for license }
  if HasParam('', '--license') then
  begin
    Result := caShowLicense;
    Exit;
  end;
end;

procedure ShowHelp;
begin
  WriteLn(GetAppFullVersion);
  WriteLn(APP_COPYRIGHT_BASE + FormatDateTime('yyyy', Now) + APP_AUTHOR);
  WriteLn;
  WriteLn(_T('CLI', 'Usage', 'Usage'), ': 3nity ', _T('CLI', 'Arguments', '[OPTIONS] [FILE|URL|FOLDER]'));
  WriteLn;
  WriteLn(_T('CLI', 'Description', 'A cross-platform multimedia player using libmpv backend.'));
  WriteLn;
  WriteLn(_T('CLI', 'Options', 'OPTIONS'), ':');
  WriteLn('  -h, --help          Show this help message and exit');
  WriteLn('  -v, --version       Show version information and exit');
  WriteLn('      --license       Show license information and exit');
  WriteLn;
  WriteLn('  -f, --fullscreen    Start in fullscreen mode');
  WriteLn('      --volume=N      Set initial volume (0-100)');
  WriteLn('      --mute          Start with audio muted');
  WriteLn('      --loop          Loop media playback');
  WriteLn('      --speed=N       Set playback speed (0.25-4.0)');
  WriteLn;
  WriteLn('      --start=TIME    Start at position (SS, MM:SS, or HH:MM:SS)');
  WriteLn('      --sub=FILE      Load external subtitle file');
  WriteLn('      --enqueue       Add to existing instance playlist');
  WriteLn;
  WriteLn(_T('CLI', 'Examples', 'EXAMPLES'), ':');
  WriteLn('  3nity video.mp4');
  WriteLn('  3nity -f --start=10:00 movie.mkv');
  WriteLn('  3nity --volume=50 --loop music.mp3');
  WriteLn('  3nity --enqueue ~/Music/*.mp3');
  WriteLn('  3nity --sub=movie.srt movie.mkv');
  WriteLn;
  WriteLn(_T('CLI', 'MoreInfo', 'For more information, visit'), ':');
  WriteLn('  ', APP_GITHUB);
end;

procedure ShowVersion;
begin
  WriteLn(GetAppFullVersion);
  WriteLn;
  WriteLn('Build Information:');
  WriteLn('  Version:  ', GetAppVersion);
  WriteLn('  Date:     ', GetBuildDate);
  WriteLn('  Time:     ', GetBuildTime);
  WriteLn('  Commit:   ', GetGitCommit);
  WriteLn('  Branch:   ', GetGitBranch);
  WriteLn;
  WriteLn('Compiler:');
  WriteLn('  FPC:      ', {$I %FPCVERSION%});
  WriteLn('  Target:   ', {$I %FPCTARGETCPU%}, '-', {$I %FPCTARGETOS%});
  WriteLn;
  WriteLn(APP_COPYRIGHT_BASE + FormatDateTime('yyyy', Now) + APP_AUTHOR);
  WriteLn('License: GPL-2.0');
  WriteLn;
  WriteLn(APP_GITHUB);
end;

procedure ShowLicense;
begin
  WriteLn(GetAppFullVersion);
  WriteLn(APP_COPYRIGHT_BASE + FormatDateTime('yyyy', Now) + APP_AUTHOR);
  WriteLn;
  WriteLn(LICENSE_TEXT);
end;

function ExecuteCLIAction(Action: TCLIAction): Integer;
begin
  Result := 0;

  case Action of
    caShowHelp:
      ShowHelp;
    caShowVersion:
      ShowVersion;
    caShowLicense:
      ShowLicense;
    else
      Result := -1; { Should not happen }
  end;
end;

end.
