{ ═══════════════════════════════════════════════════════════════════════════════
  TrinityMedia.lpr - Main Program File

  3nity Media - Lazarus Edition
  Version 0.1.0

  A cross-platform multimedia player using libmpv backend.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

program TrinityMedia;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  BaseUnix,
  Unix,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  Forms,
  SysUtils,
  IniFiles,
  uLibMPV,
  uMPVConst,
  uMPVEngine,
  uPlaylistManager,
  uRadioManager,
  uTypes,
  uConstants,
  uConfig,
  uCLIParams,
  uLocale,
  uMainForm,
  uPlaylist,
  uRadios,
  uEqualizer,
  uOptions,
  uMediaInfo,
  uAbout;

{$R *.res}

const
  { flock constants for Unix systems }
  {$IFDEF UNIX}
  LOCK_EX = 2;  { Exclusive lock }
  LOCK_NB = 4;  { Non-blocking }
  LOCK_UN = 8;  { Unlock }
  {$ENDIF}

var
  I: Integer;
  StartupFile: string;
  SingleInstanceEnabled: Boolean;
  EnqueueMode: Boolean;
  CLIAction: TCLIAction;
  StartupOptions: TStartupOptions;
  {$IFDEF UNIX}
  LockFd: cint = -1;
  LockPath: string;
  {$ENDIF}
  {$IFDEF WINDOWS}
  hMutex: THandle = 0;
  {$ENDIF}

function GetCommandFilePath: string;
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('TEMP') + PathDelim + '3nity-media-command.txt';
  {$ELSE}
  Result := GetEnvironmentVariable('XDG_RUNTIME_DIR');
  if Result <> '' then
    Result := Result + '/3nity-media-command.txt'
  else
    Result := '/tmp/3nity-media-command-' + GetEnvironmentVariable('USER') + '.txt';
  {$ENDIF}
end;

procedure SendCommandToExistingInstance(const Path: string);
var
  F: TextFile;
  CmdPath: string;
begin
  CmdPath := GetCommandFilePath;
  try
    AssignFile(F, CmdPath);
    Rewrite(F);
    WriteLn(F, Path);
    CloseFile(F);
  except
    { Ignore errors }
  end;
end;

function IsSingleInstanceEnabled: Boolean;
var
  IniPath: string;
  Ini: TIniFile;
begin
  Result := True; { Default to single instance }
  {$IFDEF WINDOWS}
  IniPath := GetEnvironmentVariable('APPDATA') + PathDelim + '3nity-media' + PathDelim + 'config.ini';
  {$ELSE}
  IniPath := GetEnvironmentVariable('HOME') + PathDelim + '.config' + PathDelim + '3nity-media' + PathDelim + 'config.ini';
  {$ENDIF}

  if FileExists(IniPath) then
  begin
    try
      Ini := TIniFile.Create(IniPath);
      try
        Result := Ini.ReadBool('General', 'SingleInstance', True);
      finally
        Ini.Free;
      end;
    except
      Result := True;
    end;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Cross-platform Single Instance Implementation

  Windows: Uses a named kernel mutex (CreateMutex)
           - Automatically released when process terminates (even on crash)
           - System-wide unique name ensures single instance

  Unix/macOS: Uses flock() file locking
              - Automatically released when process terminates (even on crash)
              - No stale lock files, no PID checking needed
              - The lock file can be reused by subsequent instances
  ═══════════════════════════════════════════════════════════════════════════════ }

{$IFDEF WINDOWS}
function TryAcquireLock: Boolean;
begin
  Result := False;
  { Create a named mutex - name must be unique to this application }
  hMutex := CreateMutex(nil, True, '3nityMedia_SingleInstance_Mutex');
  if hMutex <> 0 then
  begin
    { Check if mutex already existed (another instance owns it) }
    if GetLastError = ERROR_ALREADY_EXISTS then
    begin
      CloseHandle(hMutex);
      hMutex := 0;
    end
    else
      Result := True;  { We own the mutex }
  end;
end;

procedure ReleaseLock;
begin
  if hMutex <> 0 then
  begin
    ReleaseMutex(hMutex);
    CloseHandle(hMutex);
    hMutex := 0;
  end;
end;
{$ENDIF}

{$IFDEF UNIX}
function TryAcquireLock: Boolean;
begin
  Result := False;

  { Determine lock file path based on platform }
  {$IFDEF DARWIN}
  LockPath := GetEnvironmentVariable('TMPDIR');
  if LockPath = '' then
    LockPath := '/tmp';
  LockPath := LockPath + '/3nity-media-player.lock';
  {$ELSE}
  { Linux: Use XDG_RUNTIME_DIR if available (user-specific, tmpfs) }
  LockPath := GetEnvironmentVariable('XDG_RUNTIME_DIR');
  if LockPath <> '' then
    LockPath := LockPath + '/3nity-media-player.lock'
  else
    LockPath := '/tmp/3nity-media-player-' + GetEnvironmentVariable('USER') + '.lock';
  {$ENDIF}

  { Open or create the lock file }
  LockFd := FpOpen(LockPath, O_CREAT or O_RDWR, &644);
  if LockFd < 0 then
    Exit;

  { Try to acquire exclusive lock (non-blocking)
    flock() is the key: the lock is automatically released when:
    - The file descriptor is closed
    - The process terminates (even on crash!)
    No need for PID files or stale lock cleanup }
  if fpFlock(LockFd, LOCK_EX or LOCK_NB) = 0 then
    Result := True
  else
  begin
    { Another instance holds the lock }
    FpClose(LockFd);
    LockFd := -1;
  end;
end;

procedure ReleaseLock;
begin
  if LockFd >= 0 then
  begin
    fpFlock(LockFd, LOCK_UN);
    FpClose(LockFd);
    LockFd := -1;
  end;
  { Note: We don't delete the lock file - it can be reused by future instances }
end;
{$ENDIF}

begin
  { ═══════════════════════════════════════════════════════════════════════════
    CLI-only actions (--help, --version, --license)
    These are handled first, before any GUI initialization
    ═══════════════════════════════════════════════════════════════════════════ }
  CLIAction := GetCLIAction;
  if CLIAction <> caNoAction then
  begin
    ExecuteCLIAction(CLIAction);
    Halt(0);
  end;

  { Check SingleInstance setting from config }
  SingleInstanceEnabled := IsSingleInstanceEnabled;

  { Check for --enqueue parameter }
  EnqueueMode := HasParam('', '--enqueue');

  { Check for existing instance (cross-platform) }
  if SingleInstanceEnabled then
  begin
    if not TryAcquireLock then
    begin
      { Another instance is running - send command to it }
      StartupFile := '';
      for I := 1 to ParamCount do
      begin
        if (ParamStr(I) <> '') and (ParamStr(I)[1] <> '-') then
        begin
          StartupFile := ParamStr(I);
          Break;
        end;
      end;

      if StartupFile <> '' then
      begin
        { Send with ENQUEUE: prefix if --enqueue is specified }
        if EnqueueMode then
        begin
          SendCommandToExistingInstance('ENQUEUE:' + StartupFile);
          WriteLn(Format(_T('CLI', 'EnqueuedTo', 'Enqueued to existing instance: %s'), [StartupFile]));
        end
        else
        begin
          SendCommandToExistingInstance(StartupFile);
          WriteLn(Format(_T('CLI', 'SentTo', 'Sent to existing instance: %s'), [StartupFile]));
        end;
      end
      else
        WriteLn(_T('CLI', 'AlreadyRunning', '3nity Media is already running.'));

      Halt(0);
    end;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    Early validation: Check file path before creating GUI
    ═══════════════════════════════════════════════════════════════════════════ }
  StartupFile := '';
  for I := 1 to ParamCount do
  begin
    if (ParamStr(I) <> '') and (ParamStr(I)[1] <> '-') then
    begin
      StartupFile := ParamStr(I);
      Break;
    end;
  end;

  { Validate file path exists (skip check for URLs) }
  if (StartupFile <> '') and (Pos('://', StartupFile) = 0) and
     (not FileExists(StartupFile)) and (not DirectoryExists(StartupFile)) then
  begin
    WriteLn(Format(_T('CLI', 'ErrorFileNotFound', 'Error: File or directory not found: %s'), [StartupFile]));
    if SingleInstanceEnabled then
      ReleaseLock;
    Halt(1);
  end;

  { Parse CLI startup options early to validate subtitle file }
  StartupOptions := ParseStartupOptions;

  { Validate subtitle file exists if specified }
  if StartupOptions.SubFileSet and (not FileExists(StartupOptions.SubFile)) then
  begin
    WriteLn(Format(_T('CLI', 'ErrorSubNotFound', 'Error: Subtitle file not found: %s'), [StartupOptions.SubFile]));
    if SingleInstanceEnabled then
      ReleaseLock;
    Halt(1);
  end;

  try
    RequireDerivedFormResource := True;
  Application.Scaled:=True;
    Application.Initialize;
  Application.Title:='3nity Media';
    Application.CreateForm(TfrmMain, frmMain);

    { Apply CLI startup options }
    if StartupOptions.FullscreenSet or StartupOptions.VolumeSet or StartupOptions.MuteSet or
       StartupOptions.LoopSet or StartupOptions.SpeedSet or StartupOptions.StartTimeSet or
       StartupOptions.SubFileSet then
      frmMain.SetStartupOptions(StartupOptions);

    { If a file was passed, play it after form is shown }
    if StartupFile <> '' then
      frmMain.PlayStartupFile(StartupFile);

    Application.Run;
  finally
    if SingleInstanceEnabled then
      ReleaseLock;
  end;
end.
