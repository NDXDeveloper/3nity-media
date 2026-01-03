# Developer Documentation: Visualization + Playlist Playback Fix

## Initial Problem

When playing an audio file from history with visualization enabled (lavfi-complex), when the file ended, the next file wouldn't play and the player became unresponsive.

## Root Cause

MPV's `lavfi-complex` filter uses audio track references like `[aid1]` (audio track 1). When a new file is loaded while the filter is active, the `[aid1]` reference becomes invalid as it points to the old file. MPV can then no longer play the new file.

### MPV Behavior with lavfi-complex

- The `lavfi-complex` filter "locks" the referenced audio track
- When changing files, MPV sends an `end-file` event with `reason=2` (STOP)
- If the filter remains active, the new file cannot initialize correctly
- This is documented in MPV GitHub issues #9588 and #6354

### Specific Issue with showwaves

The FFmpeg `showwaves` filter caused additional problems:
- Even after stopping and clearing the filter, MPV entered an unrecoverable state
- ErrorCode=-17 (MPV_ERROR_LOADING_FAILED) when loading the next file
- Application became completely unresponsive

**Solution**: Use `repeatlast=0` parameter on the overlay filter. This fixes the EOF handling issue.

## Implemented Solution

### 1. Use Timers Instead of Sleep()

`Sleep()` calls block the user interface. We use `TTimer` for non-blocking delays:

- `FVisualLoadTimer` (500ms): Delay before loading a new file after clearing filter
- `FVisualReapplyTimer` (800ms): Delay before reapplying visualization after loading

### 2. Clear Filter BEFORE Loading New File

In `PlayFile` (uMainForm.pas), we stop MPV and clear the filter before loading the new file:

```pascal
{ Clear lavfi-complex filter BEFORE loading new file }
if (FVisualEffects <> nil) and FVisualEffects.Enabled and (FVisualEffects.Mode <> vmNone) then
begin
  { Stop playback first to reset MPV state }
  FMPVEngine.Stop;
  Application.ProcessMessages;
  { Clear the filter }
  FMPVEngine.SetPropertyString('lavfi-complex', '');
  Application.ProcessMessages;
  { Use timer to delay file loading }
  FPendingPlayFile := FileName;
  FIgnoreNextEndFile := True;
  FVisualLoadTimer.Enabled := True;
  Exit;
end;
```

### 3. Reapply Filter After Loading

In `OnMPVFileLoaded`, we start the reapply timer:

```pascal
if (FVisualEffects <> nil) and FVisualEffects.Enabled and
   (FVisualEffects.Mode <> vmNone) and not FChangingVisualization then
begin
  FVisualReapplyTimer.Enabled := False; { Reset if already running }
  FVisualReapplyTimer.Enabled := True;  { Start the timer }
end;
```

### 4. Protection Flags

Two flags are used to avoid cascade issues:

#### FIgnoreNextEndFile
- Set to `True` in `PlayFile` before clearing the filter
- Reset to `False` in `OnMPVFileLoaded` when the file is loaded
- Allows ignoring the `end-file` event that arrives immediately after file change

#### FChangingVisualization
- Set to `True` at the beginning of `ApplyVisualization`
- Reset to `False` after a delay
- Allows ignoring EOF events caused by filter change

### 5. NearEnd Detection for False EOF

The filter can generate false EOF events. We check if we're really near the end:

```pascal
NearEnd := (FileDuration <= 0) or (CurrentPos <= 0) or
           ((FileDuration - CurrentPos) < 2.0) or
           ((CurrentPos / FileDuration) > 0.98);

if (Reason = MPV_END_FILE_REASON_EOF) and not NearEnd then
  Exit;  // False EOF, ignore
```

### 6. Use repeatlast=0 on overlay

The overlay filter needs `repeatlast=0` to properly handle EOF:

```pascal
function TVisualEffects.GetAudioOnlyFilter: string;
begin
  // ...
  Result := Format('[aid1] asplit [ao][a]; color=black:s=%dx%d [bg]; [a] %s [fg]; [bg][fg] overlay=repeatlast=0 [vo]',
    [FSettings.Width, FSettings.Height, BuildSpectrumFilter]);
end;
```

## Execution Flow

```
1. File ends (EOF)
   └── OnMPVEndFile(Reason=0)
       └── PlayNextInPlaylist()
           └── PlayFile(NextFile)
               ├── Stop MPV
               ├── Clear lavfi-complex
               ├── FIgnoreNextEndFile := True
               └── Start FVisualLoadTimer

2. FVisualLoadTimer Timer (500ms)
   └── OnVisualLoadTimer()
       └── PlayMedia(NextFile)

3. MPV sends end-file for old file
   └── OnMPVEndFile(Reason=2) - IGNORED (FIgnoreNextEndFile=True)

4. New file loaded
   └── OnMPVFileLoaded()
       ├── FIgnoreNextEndFile := False
       └── Start FVisualReapplyTimer

5. FVisualReapplyTimer Timer (800ms)
   └── OnVisualReapplyTimer()
       └── ApplyVisualization()
           ├── FChangingVisualization := True
           ├── SetPropertyString('lavfi-complex', FilterStr)
           └── FChangingVisualization := False (after delay)
```

## MPV Event Codes

| Constant | Value | Description |
|----------|-------|-------------|
| MPV_END_FILE_REASON_EOF | 0 | Normal end of file |
| MPV_END_FILE_REASON_STOP | 2 | Stop (user or filter) |
| MPV_END_FILE_REASON_ERROR | 4 | Playback error |

## TMPVStatus

| State | Value | Description |
|-------|-------|-------------|
| msNone | 0 | No state |
| msOpening | 1 | Opening in progress |
| msClosing | 2 | Closing in progress |
| msPlayStarting | 3 | Starting playback |
| msPlaying | 4 | Playing |
| msPaused | 5 | Paused |
| msStopped | 6 | Stopped |
| msError | 7 | Error |

## Important Points

1. **Non-blocking timers**: Use `TTimer` instead of `Sleep()` to avoid blocking the user interface.

2. **Order of operations**: The filter MUST be cleared BEFORE loading the new file, not after.

3. **Stop before Clear**: Call `FMPVEngine.Stop` before clearing the filter to reset MPV state.

4. **repeatlast=0**: The `overlay` filter needs `repeatlast=0` to properly handle EOF.

5. **Error handling**: If a file is invalid (ErrorCode=-17), the system automatically moves to the next file.

## Modified Files

- `src/Forms/uMainForm.pas`:
  - `PlayFile`: Added Stop + filter clearing + timer
  - `OnMPVEndFile`: Flag handling and NearEnd detection
  - `OnMPVFileLoaded`: Starting reapply timer
  - `OnVisualLoadTimer`: Delayed file loading
  - `OnVisualReapplyTimer`: Delayed filter reapplication
  - `ApplyVisualization`: FChangingVisualization flag

- `src/Core/uVisualEffects.pas`:
  - `GetAudioOnlyFilter`: Added repeatlast=0 to overlay filter

## References

- [MPV Issue #9588](https://github.com/mpv-player/mpv/issues/9588) - Clearing lavfi-complex causes audio disappear
- [MPV Issue #6354](https://github.com/mpv-player/mpv/issues/6354) - Can't switch audio tracks with lavfi-complex
- [MPV Issue #7266](https://github.com/mpv-player/mpv/issues/7266) - EOF hanging with lavfi filters
