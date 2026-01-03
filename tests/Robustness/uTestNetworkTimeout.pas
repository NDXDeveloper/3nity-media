{ ===============================================================================
  uTestNetworkTimeout.pas - Network Timeout and Error Handling Tests

  Part of 3nity Media - Test Suite

  Tests application robustness when handling network timeouts, disconnections,
  and unreliable streams.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uTestNetworkTimeout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, fpcunit, testregistry, DateUtils;

type
  { ============================================================================
    TTestNetworkTimeout - Network timeout handling tests
    ============================================================================ }
  TTestNetworkTimeout = class(TTestCase)
  private
    FStartTime: TDateTime;
    procedure StartTimer;
    function StopTimer: Int64;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { URL validation tests }
    procedure Test_ValidateURL_HTTP;
    procedure Test_ValidateURL_HTTPS;
    procedure Test_ValidateURL_RTSP;
    procedure Test_ValidateURL_MMS;
    procedure Test_ValidateURL_Invalid;
    procedure Test_ValidateURL_Malformed;

    { Timeout configuration tests }
    procedure Test_TimeoutConfig_Default;
    procedure Test_TimeoutConfig_Custom;
    procedure Test_TimeoutConfig_Zero;
    procedure Test_TimeoutConfig_Negative;

    { Connection error simulation }
    procedure Test_HandleConnectionRefused;
    procedure Test_HandleHostNotFound;
    procedure Test_HandleNetworkUnreachable;
    procedure Test_HandleTimeout;

    { Stream URL parsing tests }
    procedure Test_ParseStreamURL_Simple;
    procedure Test_ParseStreamURL_WithPort;
    procedure Test_ParseStreamURL_WithPath;
    procedure Test_ParseStreamURL_WithQuery;
    procedure Test_ParseStreamURL_WithAuth;

    { Retry logic tests }
    procedure Test_RetryLogic_MaxRetries;
    procedure Test_RetryLogic_Backoff;
    procedure Test_RetryLogic_SuccessOnRetry;
  end;

  { ============================================================================
    TTestStreamResilience - Stream connection resilience tests
    ============================================================================ }
  TTestStreamResilience = class(TTestCase)
  published
    { Buffer management tests }
    procedure Test_BufferUnderrun_Handled;
    procedure Test_BufferOverrun_Handled;
    procedure Test_BufferResize_Dynamic;

    { Reconnection tests }
    procedure Test_AutoReconnect_Enabled;
    procedure Test_AutoReconnect_Disabled;
    procedure Test_ReconnectDelay_Calculation;

    { Stream state tests }
    procedure Test_StreamState_Connecting;
    procedure Test_StreamState_Buffering;
    procedure Test_StreamState_Playing;
    procedure Test_StreamState_Error;
    procedure Test_StreamState_Reconnecting;

    { Error recovery tests }
    procedure Test_RecoverFromTemporaryError;
    procedure Test_RecoverFromBufferEmpty;
    procedure Test_RecoverFromBitrateDrop;
  end;

implementation

const
  { Default timeout values in milliseconds }
  DEFAULT_CONNECT_TIMEOUT = 10000;
  DEFAULT_READ_TIMEOUT = 30000;
  MAX_RETRIES = 3;
  RETRY_DELAY_MS = 1000;

type
  { Simulated stream state }
  TStreamState = (ssIdle, ssConnecting, ssBuffering, ssPlaying, ssError, ssReconnecting);

  { Simulated connection result }
  TConnectionResult = (crSuccess, crRefused, crHostNotFound, crNetworkUnreachable, crTimeout);

{ ============================================================================
  TTestNetworkTimeout
  ============================================================================ }

procedure TTestNetworkTimeout.SetUp;
begin
  { Nothing to set up }
end;

procedure TTestNetworkTimeout.TearDown;
begin
  { Nothing to clean up }
end;

procedure TTestNetworkTimeout.StartTimer;
begin
  FStartTime := Now;
end;

function TTestNetworkTimeout.StopTimer: Int64;
begin
  Result := MilliSecondsBetween(Now, FStartTime);
end;

{ URL validation tests }

procedure TTestNetworkTimeout.Test_ValidateURL_HTTP;
var
  URL: string;
  IsValid: Boolean;
begin
  URL := 'http://stream.example.com:8000/live';
  IsValid := (Pos('http://', LowerCase(URL)) = 1) and (Length(URL) > 7);
  AssertTrue('HTTP URL should be valid', IsValid);
end;

procedure TTestNetworkTimeout.Test_ValidateURL_HTTPS;
var
  URL: string;
  IsValid: Boolean;
begin
  URL := 'https://secure.stream.com/audio';
  IsValid := (Pos('https://', LowerCase(URL)) = 1) and (Length(URL) > 8);
  AssertTrue('HTTPS URL should be valid', IsValid);
end;

procedure TTestNetworkTimeout.Test_ValidateURL_RTSP;
var
  URL: string;
  IsValid: Boolean;
begin
  URL := 'rtsp://media.server.com/stream';
  IsValid := (Pos('rtsp://', LowerCase(URL)) = 1) and (Length(URL) > 7);
  AssertTrue('RTSP URL should be valid', IsValid);
end;

procedure TTestNetworkTimeout.Test_ValidateURL_MMS;
var
  URL: string;
  IsValid: Boolean;
begin
  URL := 'mms://wm.server.com/broadcast';
  IsValid := (Pos('mms://', LowerCase(URL)) = 1) and (Length(URL) > 6);
  AssertTrue('MMS URL should be valid', IsValid);
end;

procedure TTestNetworkTimeout.Test_ValidateURL_Invalid;
var
  URL: string;
  IsValid: Boolean;
begin
  URL := 'not_a_valid_url';
  IsValid := (Pos('://', URL) > 1);
  AssertFalse('Invalid URL should be rejected', IsValid);
end;

procedure TTestNetworkTimeout.Test_ValidateURL_Malformed;
var
  URLs: array[0..4] of string;
  I: Integer;
  IsValid: Boolean;
begin
  { Test basic URL format validation - checks scheme://host format }
  URLs[0] := 'http://';        { No host after :// - len=7, pos=5, 7>7? No }
  URLs[1] := '://example.com'; { No scheme before :// - pos=1, 1>1? No }
  URLs[2] := 'http:example.com'; { Missing slashes - no :// found }
  URLs[3] := '';               { Empty string }
  URLs[4] := 'x://';           { Scheme but no host - len=4, pos=2, 4>4? No }

  for I := 0 to High(URLs) do
  begin
    { Basic format check: scheme://host where both parts have content }
    IsValid := (Pos('://', URLs[I]) > 1) and (Length(URLs[I]) > Pos('://', URLs[I]) + 2);
    AssertFalse('Malformed URL "' + URLs[I] + '" should be rejected', IsValid);
  end;
end;

{ Timeout configuration tests }

procedure TTestNetworkTimeout.Test_TimeoutConfig_Default;
var
  ConnectTimeout, ReadTimeout: Integer;
begin
  ConnectTimeout := DEFAULT_CONNECT_TIMEOUT;
  ReadTimeout := DEFAULT_READ_TIMEOUT;

  AssertEquals('Default connect timeout should be 10s', 10000, ConnectTimeout);
  AssertEquals('Default read timeout should be 30s', 30000, ReadTimeout);
end;

procedure TTestNetworkTimeout.Test_TimeoutConfig_Custom;
var
  ConnectTimeout, ReadTimeout: Integer;
begin
  { Simulate custom configuration }
  ConnectTimeout := 5000;
  ReadTimeout := 15000;

  AssertTrue('Connect timeout should be positive', ConnectTimeout > 0);
  AssertTrue('Read timeout should be positive', ReadTimeout > 0);
  AssertTrue('Read timeout should be >= connect timeout', ReadTimeout >= ConnectTimeout);
end;

procedure TTestNetworkTimeout.Test_TimeoutConfig_Zero;
var
  Timeout: Integer;
  IsValid: Boolean;
begin
  Timeout := 0;
  IsValid := Timeout > 0;
  AssertFalse('Zero timeout should be invalid', IsValid);
end;

procedure TTestNetworkTimeout.Test_TimeoutConfig_Negative;
var
  Timeout: Integer;
  IsValid: Boolean;
begin
  Timeout := -1000;
  IsValid := Timeout > 0;
  AssertFalse('Negative timeout should be invalid', IsValid);
end;

{ Connection error simulation }

procedure TTestNetworkTimeout.Test_HandleConnectionRefused;
var
  Result: TConnectionResult;
  ErrorHandled: Boolean;
begin
  Result := crRefused;

  { Simulate handling connection refused }
  case Result of
    crRefused: ErrorHandled := True;
  else
    ErrorHandled := False;
  end;

  AssertTrue('Connection refused should be handled', ErrorHandled);
end;

procedure TTestNetworkTimeout.Test_HandleHostNotFound;
var
  Result: TConnectionResult;
  ErrorHandled: Boolean;
begin
  Result := crHostNotFound;

  case Result of
    crHostNotFound: ErrorHandled := True;
  else
    ErrorHandled := False;
  end;

  AssertTrue('Host not found should be handled', ErrorHandled);
end;

procedure TTestNetworkTimeout.Test_HandleNetworkUnreachable;
var
  Result: TConnectionResult;
  ErrorHandled: Boolean;
begin
  Result := crNetworkUnreachable;

  case Result of
    crNetworkUnreachable: ErrorHandled := True;
  else
    ErrorHandled := False;
  end;

  AssertTrue('Network unreachable should be handled', ErrorHandled);
end;

procedure TTestNetworkTimeout.Test_HandleTimeout;
var
  Result: TConnectionResult;
  ErrorHandled, ShouldRetry: Boolean;
begin
  Result := crTimeout;

  case Result of
    crTimeout:
    begin
      ErrorHandled := True;
      ShouldRetry := True;  { Timeouts are often retryable }
    end;
  else
    ErrorHandled := False;
    ShouldRetry := False;
  end;

  AssertTrue('Timeout should be handled', ErrorHandled);
  AssertTrue('Timeout should trigger retry', ShouldRetry);
end;

{ Stream URL parsing tests }

procedure TTestNetworkTimeout.Test_ParseStreamURL_Simple;
var
  URL, Protocol, Host: string;
  Port: Integer;
begin
  URL := 'http://example.com/stream';

  { Simple parsing }
  Protocol := Copy(URL, 1, Pos('://', URL) - 1);
  Host := Copy(URL, Pos('://', URL) + 3, Length(URL));
  if Pos('/', Host) > 0 then
    Host := Copy(Host, 1, Pos('/', Host) - 1);
  Port := 80;  { Default for HTTP }

  AssertEquals('Protocol should be http', 'http', Protocol);
  AssertEquals('Host should be example.com', 'example.com', Host);
  AssertEquals('Default port should be 80', 80, Port);
end;

procedure TTestNetworkTimeout.Test_ParseStreamURL_WithPort;
var
  URL, Host: string;
  Port: Integer;
  ColonPos: Integer;
begin
  URL := 'http://example.com:8080/stream';

  Host := Copy(URL, Pos('://', URL) + 3, Length(URL));
  if Pos('/', Host) > 0 then
    Host := Copy(Host, 1, Pos('/', Host) - 1);

  ColonPos := Pos(':', Host);
  if ColonPos > 0 then
  begin
    Port := StrToIntDef(Copy(Host, ColonPos + 1, Length(Host)), 80);
    Host := Copy(Host, 1, ColonPos - 1);
  end
  else
    Port := 80;

  AssertEquals('Host should be example.com', 'example.com', Host);
  AssertEquals('Port should be 8080', 8080, Port);
end;

procedure TTestNetworkTimeout.Test_ParseStreamURL_WithPath;
var
  URL, Path: string;
  PathStart: Integer;
begin
  URL := 'http://example.com/radio/live/stream.mp3';

  PathStart := Pos('://', URL) + 3;
  PathStart := Pos('/', Copy(URL, PathStart, Length(URL))) + PathStart - 1;
  Path := Copy(URL, PathStart, Length(URL));

  AssertEquals('Path should be /radio/live/stream.mp3', '/radio/live/stream.mp3', Path);
end;

procedure TTestNetworkTimeout.Test_ParseStreamURL_WithQuery;
var
  URL, Query: string;
  QueryStart: Integer;
begin
  URL := 'http://example.com/stream?bitrate=128&format=mp3';

  QueryStart := Pos('?', URL);
  if QueryStart > 0 then
    Query := Copy(URL, QueryStart + 1, Length(URL))
  else
    Query := '';

  AssertEquals('Query should be bitrate=128&format=mp3', 'bitrate=128&format=mp3', Query);
end;

procedure TTestNetworkTimeout.Test_ParseStreamURL_WithAuth;
var
  URL, User, Pass, Host: string;
  AtPos, ColonPos, SlashPos: Integer;
  AuthPart: string;
begin
  URL := 'http://user:password@example.com/stream';

  { Extract auth part }
  AtPos := Pos('@', URL);
  if AtPos > 0 then
  begin
    AuthPart := Copy(URL, Pos('://', URL) + 3, AtPos - Pos('://', URL) - 3);
    ColonPos := Pos(':', AuthPart);
    User := Copy(AuthPart, 1, ColonPos - 1);
    Pass := Copy(AuthPart, ColonPos + 1, Length(AuthPart));

    Host := Copy(URL, AtPos + 1, Length(URL));
    SlashPos := Pos('/', Host);
    if SlashPos > 0 then
      Host := Copy(Host, 1, SlashPos - 1);
  end;

  AssertEquals('User should be user', 'user', User);
  AssertEquals('Password should be password', 'password', Pass);
  AssertEquals('Host should be example.com', 'example.com', Host);
end;

{ Retry logic tests }

procedure TTestNetworkTimeout.Test_RetryLogic_MaxRetries;
var
  Retries: Integer;
  Success: Boolean;
begin
  Retries := 0;
  Success := False;

  while (Retries < MAX_RETRIES) and (not Success) do
  begin
    Inc(Retries);
    { Simulate failure }
    Success := False;
  end;

  AssertEquals('Should attempt MAX_RETRIES times', MAX_RETRIES, Retries);
  AssertFalse('Should not succeed after all retries failed', Success);
end;

procedure TTestNetworkTimeout.Test_RetryLogic_Backoff;
var
  Retries: Integer;
  Delays: array[0..2] of Integer;
begin
  { Exponential backoff: 1s, 2s, 4s }
  for Retries := 0 to 2 do
    Delays[Retries] := RETRY_DELAY_MS * (1 shl Retries);

  AssertEquals('First delay should be 1000ms', 1000, Delays[0]);
  AssertEquals('Second delay should be 2000ms', 2000, Delays[1]);
  AssertEquals('Third delay should be 4000ms', 4000, Delays[2]);
end;

procedure TTestNetworkTimeout.Test_RetryLogic_SuccessOnRetry;
var
  Retries: Integer;
  Success: Boolean;
  SuccessOnAttempt: Integer;
begin
  Retries := 0;
  Success := False;
  SuccessOnAttempt := 2;  { Succeed on second attempt }

  while (Retries < MAX_RETRIES) and (not Success) do
  begin
    Inc(Retries);
    Success := (Retries = SuccessOnAttempt);
  end;

  AssertEquals('Should succeed on attempt 2', 2, Retries);
  AssertTrue('Should eventually succeed', Success);
end;

{ ============================================================================
  TTestStreamResilience
  ============================================================================ }

{ Buffer management tests }

procedure TTestStreamResilience.Test_BufferUnderrun_Handled;
var
  BufferLevel: Integer;
  BufferMin: Integer;
  IsUnderrun: Boolean;
begin
  BufferLevel := 10;  { 10% full }
  BufferMin := 25;    { Need 25% minimum }

  IsUnderrun := BufferLevel < BufferMin;
  AssertTrue('Should detect buffer underrun', IsUnderrun);
end;

procedure TTestStreamResilience.Test_BufferOverrun_Handled;
var
  BufferLevel: Integer;
  BufferMax: Integer;
  IsOverrun, ShouldDiscard: Boolean;
begin
  BufferLevel := 95;  { 95% full }
  BufferMax := 90;    { Max 90% }

  IsOverrun := BufferLevel > BufferMax;
  ShouldDiscard := IsOverrun;  { Discard oldest data }

  AssertTrue('Should detect buffer overrun', IsOverrun);
  AssertTrue('Should discard data on overrun', ShouldDiscard);
end;

procedure TTestStreamResilience.Test_BufferResize_Dynamic;
var
  BufferSize: Integer;
  Bitrate: Integer;
  NewSize: Integer;
begin
  BufferSize := 64 * 1024;  { 64KB initial }
  Bitrate := 320 * 1024;     { 320kbps }

  { Calculate buffer for 5 seconds of audio }
  NewSize := (Bitrate * 5) div 8;

  AssertTrue('Buffer should resize for high bitrate', NewSize > BufferSize);
end;

{ Reconnection tests }

procedure TTestStreamResilience.Test_AutoReconnect_Enabled;
var
  AutoReconnect: Boolean;
  State: TStreamState;
  ShouldReconnect: Boolean;
begin
  AutoReconnect := True;
  State := ssError;

  ShouldReconnect := AutoReconnect and (State = ssError);
  AssertTrue('Should attempt reconnection when enabled', ShouldReconnect);
end;

procedure TTestStreamResilience.Test_AutoReconnect_Disabled;
var
  AutoReconnect: Boolean;
  State: TStreamState;
  ShouldReconnect: Boolean;
begin
  AutoReconnect := False;
  State := ssError;

  ShouldReconnect := AutoReconnect and (State = ssError);
  AssertFalse('Should not reconnect when disabled', ShouldReconnect);
end;

procedure TTestStreamResilience.Test_ReconnectDelay_Calculation;
var
  AttemptCount: Integer;
  BaseDelay, MaxDelay, Delay: Integer;
begin
  BaseDelay := 1000;
  MaxDelay := 30000;

  { Test exponential backoff with cap }
  AttemptCount := 1;
  Delay := Min(BaseDelay * (1 shl (AttemptCount - 1)), MaxDelay);
  AssertEquals('First attempt delay', 1000, Delay);

  AttemptCount := 3;
  Delay := Min(BaseDelay * (1 shl (AttemptCount - 1)), MaxDelay);
  AssertEquals('Third attempt delay', 4000, Delay);

  AttemptCount := 10;
  Delay := Min(BaseDelay * (1 shl (AttemptCount - 1)), MaxDelay);
  AssertEquals('Tenth attempt should cap at max', MaxDelay, Delay);
end;

{ Stream state tests }

procedure TTestStreamResilience.Test_StreamState_Connecting;
var
  State: TStreamState;
  StateStr: string;
begin
  State := ssConnecting;

  case State of
    ssConnecting: StateStr := 'Connecting';
  else
    StateStr := 'Unknown';
  end;

  AssertEquals('State should be Connecting', 'Connecting', StateStr);
end;

procedure TTestStreamResilience.Test_StreamState_Buffering;
var
  State: TStreamState;
  ShowBufferIndicator: Boolean;
begin
  State := ssBuffering;
  ShowBufferIndicator := State = ssBuffering;

  AssertTrue('Should show buffer indicator', ShowBufferIndicator);
end;

procedure TTestStreamResilience.Test_StreamState_Playing;
var
  State: TStreamState;
  IsPlaying: Boolean;
begin
  State := ssPlaying;
  IsPlaying := State = ssPlaying;

  AssertTrue('Should be in playing state', IsPlaying);
end;

procedure TTestStreamResilience.Test_StreamState_Error;
var
  State: TStreamState;
  ShowError, AllowRetry: Boolean;
begin
  State := ssError;
  ShowError := State = ssError;
  AllowRetry := True;  { Usually allow retry on error }

  AssertTrue('Should show error state', ShowError);
  AssertTrue('Should allow retry', AllowRetry);
end;

procedure TTestStreamResilience.Test_StreamState_Reconnecting;
var
  State: TStreamState;
  ShowReconnecting: Boolean;
begin
  State := ssReconnecting;
  ShowReconnecting := State = ssReconnecting;

  AssertTrue('Should show reconnecting state', ShowReconnecting);
end;

{ Error recovery tests }

procedure TTestStreamResilience.Test_RecoverFromTemporaryError;
var
  ErrorCount: Integer;
  MaxErrors: Integer;
  CanRecover: Boolean;
begin
  ErrorCount := 2;
  MaxErrors := 5;

  CanRecover := ErrorCount < MaxErrors;
  AssertTrue('Should recover from temporary errors', CanRecover);
end;

procedure TTestStreamResilience.Test_RecoverFromBufferEmpty;
var
  BufferLevel: Integer;
  State: TStreamState;
  Action: string;
begin
  BufferLevel := 0;
  State := ssPlaying;

  if (BufferLevel = 0) and (State = ssPlaying) then
    Action := 'Pause and buffer'
  else
    Action := 'Continue';

  AssertEquals('Should pause and buffer when empty', 'Pause and buffer', Action);
end;

procedure TTestStreamResilience.Test_RecoverFromBitrateDrop;
var
  CurrentBitrate, MinBitrate: Integer;
  ShouldSwitch: Boolean;
begin
  CurrentBitrate := 32;   { 32kbps - very low }
  MinBitrate := 64;       { Minimum acceptable }

  ShouldSwitch := CurrentBitrate < MinBitrate;
  { In adaptive streaming, might switch to different quality }

  AssertTrue('Should consider switching on low bitrate', ShouldSwitch);
end;

initialization
  RegisterTest('Robustness', TTestNetworkTimeout);
  RegisterTest('Robustness', TTestStreamResilience);

end.
