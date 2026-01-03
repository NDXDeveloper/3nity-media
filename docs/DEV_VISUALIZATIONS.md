# Developer Documentation: Audio Visualization System

## Overview

The 3nity Media audio visualization system uses MPV/FFmpeg `lavfi-complex` filters to generate visual displays from the audio stream. This system is complex as it requires precise management of MPV states and transitions between files.

## Architecture

### Main Components

```
┌─────────────────────────────────────────────────────────────────┐
│                         uMainForm.pas                           │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐  │
│  │ FVisualEffects  │  │ FMPVEngine      │  │ Timers          │  │
│  │ (TVisualEffects)│  │ (TMPVEngine)    │  │                 │  │
│  │                 │  │                 │  │ FVisualLoadTimer│  │
│  │ - Mode          │  │ - lavfi-complex │  │ FVisualReapply  │  │
│  │ - ColorScheme   │  │ - Position      │  │   Timer         │  │
│  │ - FilterString  │  │ - Duration      │  │                 │  │
│  └────────┬────────┘  └────────┬────────┘  └────────┬────────┘  │
│           │                    │                    │           │
│           └────────────────────┼────────────────────┘           │
│                                │                                │
│  ┌─────────────────────────────┴─────────────────────────────┐  │
│  │                    Protection Flags                       │  │
│  │  FChangingVisualization  FIgnoreNextEndFile  FWatchdog... │  │
│  └───────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│                      uVisualEffects.pas                         │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                    TVisualEffects                         │  │
│  │                                                           │  │
│  │  Modes:           FFmpeg Filters:                         │  │
│  │  - vmNone         (none)                                  │  │
│  │  - vmSpectrum     showspectrum                            │  │
│  │  - vmWaves        showwaves                               │  │
│  │  - vmVector       avectorscope                            │  │
│  │  - vmHistogram    ahistogram                              │  │
│  │  - vmVolume       showvolume                              │  │
│  │  - vmCombined     showspectrum + showwaves (vstack)       │  │
│  │                                                           │  │
│  │  Color schemes: Default, Fire, Ice, Rainbow, Green...     │  │
│  └───────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

### Files Involved

| File | Role |
|------|------|
| `Core/uVisualEffects.pas` | TVisualEffects class - mode/color management, FFmpeg filter generation |
| `Forms/uMainForm.pas` | MPV integration, state management, timers, menus |
| `Common/uShortcuts.pas` | Keyboard shortcuts (saVisNextMode, saVisNextColor) |

---

## TVisualEffects Class (uVisualEffects.pas)

### Types and Enumerations

#### TVisualMode (Visualization Modes)

| Mode | Value | Description | FFmpeg Filter |
|------|-------|-------------|---------------|
| vmNone | 0 | No visualization | - |
| vmSpectrum | 1 | Spectrum analyzer (frequency bars) | `showspectrum` |
| vmWaves | 2 | Waveform display | `showwaves` |
| vmVector | 3 | Vector scope (Lissajous) | `avectorscope` |
| vmHistogram | 4 | Audio histogram | `ahistogram` |
| vmVolume | 5 | VU meter | `showvolume` |
| vmCombined | 6 | Spectrum + Wave combined | `showspectrum` + `showwaves` + `vstack` |

#### TVisualColorScheme (Color Schemes)

| Scheme | FFmpeg Color |
|--------|--------------|
| vcsDefault | channel |
| vcsFire | fire |
| vcsIce | cool |
| vcsRainbow | rainbow |
| vcsGreen | green |
| vcsPurple | magenta |
| vcsWhite | white |

#### TSpectrumMode / TWaveformMode

```pascal
TSpectrumMode = (smVertical, smHorizontal, smScroll);
TWaveformMode = (wmPoint, wmLine, wmP2P, wmCline);
```

### TVisualSettings (Configuration Record)

```pascal
TVisualSettings = record
  Mode: TVisualMode;
  SpectrumMode: TSpectrumMode;
  WaveformMode: TWaveformMode;
  ColorScheme: TVisualColorScheme;
  Width: Integer;           // Visualization width (default: 640)
  Height: Integer;          // Visualization height (default: 360)
  BarCount: Integer;        // Number of bars (8-512, default: 64)
  BarWidth: Integer;        // Bar width in pixels
  BarGap: Integer;          // Gap between bars
  ShowPeaks: Boolean;       // Peak indicators
  PeakFalloff: Integer;     // Peak fall speed (1-10)
  WaveScale: Double;        // Amplitude scale
  WaveSpeed: Integer;       // Scroll speed
  Opacity: Integer;         // Opacity (0-100)
  BackgroundColor: TColor;
  ForegroundColor: TColor;
  EnableGlow: Boolean;
  MirrorEffect: Boolean;
  Smoothing: Integer;       // Smoothing (1-10)
end;
```

### Main Methods

| Method | Description |
|--------|-------------|
| `Create` | Constructor - calls Initialize |
| `Initialize` | Initializes with default values |
| `Reset` | Resets to defaults and updates filter |
| `GetFilterString` | Returns current FFmpeg filter |
| `GetAudioOnlyFilter` | Returns complete lavfi-complex filter for audio only |
| `NextMode` | Switches to next mode (cycles) |
| `PreviousMode` | Switches to previous mode |
| `NextColorScheme` | Switches to next color scheme |
| `ApplyPreset(Name)` | Applies a predefined preset |
| `GetPresetNames` | List of preset names |

### Private Filter Building Methods

| Method | Generated FFmpeg Filter |
|--------|------------------------|
| `BuildSpectrumFilter` | `showspectrum=s=WxH:mode=...:color=...:slide=...:scale=log` |
| `BuildWavesFilter` | `showwaves=s=WxH:mode=...:colors=...` |
| `BuildVectorFilter` | `avectorscope=s=WxH:mode=lissajous:draw=line:scale=log` |
| `BuildHistogramFilter` | `ahistogram=s=WxH:slide=replace:scale=log` |
| `BuildVolumeFilter` | `showvolume=w=W:h=H:f=0.5:b=4:m=p` |
| `BuildCombinedFilter` | Combination spectrum + waves with vstack |

### Event

```pascal
property OnFilterChanged: TNotifyEvent read FOnFilterChanged write FOnFilterChanged;
```

Triggered when filter changes (mode, color, parameters). Allows uMainForm to reapply the filter to MPV.

---

## lavfi-complex Filter Structure

### General Format (Audio Only)

```
[aid1] asplit [ao][a]; color=black:s=WxH [bg]; [a] <vis_filter> [fg]; [bg][fg] overlay=repeatlast=0 [vo]
```

**Explanation:**
- `[aid1]` : Input audio track (audio track 1)
- `asplit [ao][a]` : Splits audio into 2 streams (ao=audio output, a=for visualization)
- `color=black:s=WxH [bg]` : Generates black background of size WxH
- `[a] <vis_filter> [fg]` : Applies visualization filter
- `[bg][fg] overlay=repeatlast=0 [vo]` : Overlays visualization on background
- `repeatlast=0` : **IMPORTANT** - Fixes end-of-file blocking issue (MPV issue #7266)

### Example: Spectrum Mode

```
[aid1] asplit [ao][a]; color=black:s=640x360 [bg];
[a] showspectrum=s=640x360:mode=combined:color=channel:slide=replace:scale=log [fg];
[bg][fg] overlay=repeatlast=0 [vo]
```

### Combined Mode (Spectrum + Wave)

```
[aid1] asplit=3 [ao][a1][a2];
color=black:s=640x360 [bg];
[a1] showspectrum=s=640x180:mode=combined:color=channel:slide=replace [v1];
[a2] showwaves=s=640x180:mode=line:colors=channel [v2];
[v1][v2] vstack [fg];
[bg][fg] overlay=shortest=1 [vo]
```

### Equalizer Integration

When the equalizer is active, it is inserted before `asplit` in ApplyVisualization:

```pascal
FilterStr := StringReplace(FilterStr, '[aid1] asplit',
  '[aid1] ' + EqFilter + ' [aeq]; [aeq] asplit', []);
```

Result:
```
[aid1] superequalizer=... [aeq]; [aeq] asplit [ao][a]; ...
```

---

## Integration in uMainForm.pas

### Visualization-Related Fields

```pascal
private
  FVisualEffects: TVisualEffects;           // TVisualEffects instance
  FChangingVisualization: Boolean;          // Flag to ignore EOF during change
  FVisualReapplyTimer: TTimer;              // Timer for reapplication after load
  FVisualReapplyNeeded: Boolean;            // Flag for timer
  FVisualLoadTimer: TTimer;                 // Timer for loading after filter clear
  FPendingPlayFile: string;                 // File pending playback
  FWatchdogTriggered: Boolean;              // Prevents multiple watchdog triggers
  FRestoreVisAfterLoad: Boolean;            // Restore vis after loading
  FRestoreVisMode: TVisualMode;             // Mode to restore
```

### Protection Flags

#### FChangingVisualization

- **Purpose:** Ignore EOF/STOP events generated by filter change
- **Set to True:** Before applying/changing a filter
- **Reset to False:** By timer after MPV stabilization (300-500ms)

#### FIgnoreNextEndFile

- **Purpose:** Ignore end-file event that arrives immediately after PlayFile
- **Set to True:** In PlayFile before loading a new file
- **Reset to False:** In OnMPVFileLoaded when file is loaded

#### FWatchdogTriggered

- **Purpose:** Prevent multiple watchdog triggers
- **Set to True:** When watchdog detects end of file
- **Reset to False:** In OnMPVFileLoaded for the new file

#### FRestoreVisAfterLoad / FRestoreVisMode

- **Purpose:** Save and restore visualization during transitions
- **Used by:** AsyncPlayNextInPlaylist to save/restore mode

### Timers

#### FVisualLoadTimer (500ms)

```pascal
FVisualLoadTimer := TTimer.Create(Self);
FVisualLoadTimer.Interval := 500;
FVisualLoadTimer.Enabled := False;
FVisualLoadTimer.OnTimer := @OnVisualLoadTimer;
```

- **Role:** Delay before loading a new file after clearing filter
- **Trigger:** PlayFile when visualization is active
- **Handler:** `OnVisualLoadTimer` → `PlayMedia(FPendingPlayFile)`

#### FVisualReapplyTimer (300-800ms)

```pascal
FVisualReapplyTimer := TTimer.Create(Self);
FVisualReapplyTimer.Interval := 800;
FVisualReapplyTimer.Enabled := False;
FVisualReapplyTimer.OnTimer := @OnVisualReapplyTimer;
```

- **Role:** Delay before reapplying visualization after loading
- **Trigger:** OnMPVFileLoaded
- **Handler:** `OnVisualReapplyTimer` → `ApplyVisualization` or reset flags

### Main Procedures

#### SetVisualizationMode

```pascal
procedure TfrmMain.SetVisualizationMode(Mode: TVisualMode);
begin
  if FVisualEffects = nil then Exit;

  if Mode = vmNone then
    FVisualEffects.Enabled := False
  else
  begin
    FVisualEffects.Mode := Mode;
    FVisualEffects.Enabled := True;
  end;

  FChangingVisualization := True;
  ApplyVisualization;

  // Timer to reset flag after stabilization
  FVisualReapplyTimer.Enabled := False;
  FVisualReapplyTimer.Interval := 500;
  FVisualReapplyTimer.Enabled := True;
end;
```

#### ApplyVisualization

```pascal
procedure TfrmMain.ApplyVisualization;
var
  FilterStr, EqFilter: string;
begin
  if FMPVEngine = nil then Exit;
  if FVisualEffects = nil then Exit;

  FChangingVisualization := True;

  FilterStr := FVisualEffects.GetAudioOnlyFilter;

  if FilterStr <> '' then
  begin
    // Integrate equalizer if active
    EqFilter := FMPVEngine.GetEqualizerFilterString;
    if EqFilter <> '' then
    begin
      FilterStr := StringReplace(FilterStr, '[aid1] asplit',
        '[aid1] ' + EqFilter + ' [aeq]; [aeq] asplit', []);
    end;
    FMPVEngine.SetPropertyString('lavfi-complex', FilterStr);
  end
  else
  begin
    // Disable - save position, clear filter, reload
    FMPVEngine.SetPropertyString('lavfi-complex', '');
  end;
end;
```

#### OnVisFilterChanged (Callback)

```pascal
procedure TfrmMain.OnVisFilterChanged(Sender: TObject);
begin
  FChangingVisualization := True;
  ApplyVisualization;
  FVisualReapplyTimer.Enabled := False;
  FVisualReapplyTimer.Interval := 500;
  FVisualReapplyTimer.Enabled := True;
end;
```

---

## Execution Flow

### Visualization Mode Change (Menu/Shortcut)

```
1. User clicks menu/shortcut
   └── mnuVisSpectrumClick / mnuVisNextModeClick
       └── SetVisualizationMode(vmSpectrum)
           ├── FChangingVisualization := True
           ├── FVisualEffects.Mode := vmSpectrum
           ├── FVisualEffects.Enabled := True
           ├── ApplyVisualization()
           │   └── FMPVEngine.SetPropertyString('lavfi-complex', FilterStr)
           └── FVisualReapplyTimer.Start (500ms)
               └── FChangingVisualization := False
```

### Transition to Next File (Watchdog)

```
1. Position reaches end of file (Duration - 0.3s)
   └── OnMPVPosition (watchdog in position timer)
       ├── FWatchdogTriggered := True
       └── QueueAsyncCall(AsyncPlayNextInPlaylist)

2. AsyncPlayNextInPlaylist
   ├── Save: FRestoreVisMode := FVisualEffects.Mode
   ├── FRestoreVisAfterLoad := True
   ├── FChangingVisualization := True
   ├── FMPVEngine.Stop
   ├── SetPropertyString('lavfi-complex', '')
   ├── FVisualEffects.Enabled := False
   └── PlayNextInPlaylist()

3. PlayFile (next file)
   ├── FIgnoreNextEndFile := True
   ├── FMPVEngine.Stop
   ├── SetPropertyString('lavfi-complex', '')
   ├── FPendingPlayFile := FileName
   └── FVisualLoadTimer.Start (500ms)

4. OnVisualLoadTimer (500ms later)
   └── FMPVEngine.PlayMedia(FPendingPlayFile)

5. MPV sends end-file (old file)
   └── OnMPVEndFile → Ignored (FIgnoreNextEndFile=True)

6. OnMPVFileLoaded (new file)
   ├── FIgnoreNextEndFile := False
   ├── FWatchdogTriggered := False
   ├── if FRestoreVisAfterLoad:
   │   ├── FRestoreVisAfterLoad := False
   │   ├── FVisualEffects.Mode := FRestoreVisMode
   │   ├── FVisualEffects.Enabled := True
   │   ├── FIgnoreNextEndFile := True
   │   └── ApplyVisualization()
   └── FVisualReapplyTimer.Start (300ms)

7. OnVisualReapplyTimer (300ms later)
   └── FChangingVisualization := False
```

### Normal Playback with Active Visualization

```
1. File ends naturally
   └── OnMPVEndFile(Reason=EOF)
       ├── Check: not FIgnoreNextEndFile
       ├── Check: not FChangingVisualization
       ├── Check: not FWatchdogTriggered
       ├── Check NearEnd (position > 98% or < 2s from end)
       └── if all OK → PlayNextInPlaylist()
```

---

## End-of-File Watchdog

The watchdog is necessary because MPV may not send an EOF event with active lavfi-complex.

```pascal
// In OnMPVPosition (position callback)
if (FVisualEffects <> nil) and FVisualEffects.Enabled and
   (FVisualEffects.Mode <> vmNone) and (FMPVEngine <> nil) and
   not FWatchdogTriggered and not FChangingVisualization then
begin
  Duration := FMPVEngine.Duration;
  // Trigger when position reaches 0.3s before end
  if (Duration > 0) and (PositionSec >= Duration - 0.3) then
  begin
    FWatchdogTriggered := True;
    Application.QueueAsyncCall(@AsyncPlayNextInPlaylist, 0);
  end;
end;
```

---

## Menus and Shortcuts

### Menus (mnuVisualization)

| Menu Item | Variable | Action |
|-----------|----------|--------|
| None | mnuVisNone | SetVisualizationMode(vmNone) |
| Spectrum Analyzer | mnuVisSpectrum | SetVisualizationMode(vmSpectrum) |
| Waveform | mnuVisWaveform | SetVisualizationMode(vmWaves) |
| Vector Scope | mnuVisVector | SetVisualizationMode(vmVector) |
| VU Meter | mnuVisVolume | SetVisualizationMode(vmVolume) |
| Next Mode | mnuVisNextMode | FVisualEffects.NextMode |
| Next Color | mnuVisNextColor | FVisualEffects.NextColorScheme |

### Keyboard Shortcuts (uShortcuts.pas)

| Action | Constant | Default Shortcut |
|--------|----------|------------------|
| Next mode | saVisNextMode | V |
| Next color | saVisNextColor | Shift+V |

### Menu Update

```pascal
procedure TfrmMain.UpdateVisualizationMenu;
begin
  if FVisualEffects = nil then Exit;

  mnuVisNone.Checked := not FVisualEffects.Enabled or (FVisualEffects.Mode = vmNone);
  mnuVisSpectrum.Checked := FVisualEffects.Enabled and (FVisualEffects.Mode = vmSpectrum);
  mnuVisWaveform.Checked := FVisualEffects.Enabled and (FVisualEffects.Mode = vmWaves);
  mnuVisVector.Checked := FVisualEffects.Enabled and (FVisualEffects.Mode = vmVector);
  mnuVisVolume.Checked := FVisualEffects.Enabled and (FVisualEffects.Mode = vmVolume);
end;
```

---

## Available Presets

| Name | Mode | Color | Parameters |
|------|------|-------|------------|
| Classic Spectrum | vmSpectrum | vcsGreen | 64 bars, vertical |
| Fire Spectrum | vmSpectrum | vcsFire | 128 bars |
| Spectrogram | vmSpectrum | vcsRainbow | 256 bars, scroll |
| Oscilloscope | vmWaves | vcsGreen | line |
| Stereo Scope | vmVector | vcsGreen | - |
| VU Meter | vmVolume | vcsDefault | - |
| Rainbow Waves | vmWaves | vcsRainbow | P2P, mirror |
| Minimal | vmWaves | vcsWhite | Cline |

---

## Known Issues and Solutions

### Issue 1: False EOF with lavfi-complex

**Symptom:** MPV sends EOF events when the filter is changed

**Solution:**
- `FChangingVisualization` flag to ignore these events
- `NearEnd` check to detect real EOFs

### Issue 2: Blocking at End of File

**Symptom:** MPV blocks when audio file ends with visualization

**Solution:**
- `repeatlast=0` parameter on `overlay` filter
- Watchdog that detects position near end

### Issue 3: Next File Won't Load

**Symptom:** After EOF with visualization, next file doesn't start

**Cause:** `[aid1]` reference points to old file

**Solution:**
- Clear filter BEFORE loading new file
- Use timers to let MPV stabilize
- Reapply filter AFTER loading new file

### Issue 4: showwaves Causes Errors

**Symptom:** `showwaves` filter can leave MPV in unrecoverable state

**Solution:** `repeatlast=0` parameter on overlay fixes this issue

---

## MPV Event Codes

| Constant | Value | Description |
|----------|-------|-------------|
| MPV_END_FILE_REASON_EOF | 0 | Normal end of file |
| MPV_END_FILE_REASON_STOP | 2 | Stop (user or filter) |
| MPV_END_FILE_REASON_ERROR | 4 | Playback error |

---

## References

- [MPV Issue #9588](https://github.com/mpv-player/mpv/issues/9588) - Clearing lavfi-complex causes audio disappear
- [MPV Issue #6354](https://github.com/mpv-player/mpv/issues/6354) - Can't switch audio tracks with lavfi-complex
- [MPV Issue #7266](https://github.com/mpv-player/mpv/issues/7266) - EOF hanging with lavfi filters
- [FFmpeg Filters Documentation](https://ffmpeg.org/ffmpeg-filters.html)
- [FFmpeg showspectrum](https://ffmpeg.org/ffmpeg-filters.html#showspectrum-1)
- [FFmpeg showwaves](https://ffmpeg.org/ffmpeg-filters.html#showwaves-1)
- [FFmpeg avectorscope](https://ffmpeg.org/ffmpeg-filters.html#avectorscope)
