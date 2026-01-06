# 3nity Media - Developer Documentation: Keyboard Shortcuts

## Shortcuts Architecture

### The X11/MPV Focus Problem

When MPV is embedded in a GTK2 application via X11 (embedding), the MPV video window can capture keyboard focus. This causes a major issue:

1. **X11 Focus**: User clicks on video area → X11 focus goes to MPV window
2. **Lost Events**: Keyboard events are sent to MPV, not to the LCL application
3. **Non-functional Shortcuts**: `FormKeyDown`, `Application.OnShortCut` never receive events

### The Solution: GTK Key Snooper

We use `gtk_key_snooper_install()` which intercepts **ALL** keystrokes at GTK level, **before** they reach any widget:

```pascal
{$IFDEF LCLGTK2}
function GtkKeySnooper(Widget: PGtkWidget; Event: PGdkEventKey; Data: gpointer): gint; cdecl;
begin
  Result := 0; // 0 = pass through, 1 = block

  if Event^._type = GDK_KEY_PRESS then
  begin
    // Intercept Ctrl+M for Mute
    if (Event^.state and GDK_CONTROL_MASK) <> 0 then
    begin
      if Event^.keyval = GDK_KEY_m then
      begin
        frmMain.FMPVEngine.Muted := not frmMain.FMPVEngine.Muted;
        Result := 1; // Block the event
      end;
    end;
  end;
end;
{$ENDIF}
```

**Installation in FormCreate:**
```pascal
GtkKeySnooperId := gtk_key_snooper_install(@GtkKeySnooper, nil);
```

**Uninstallation in FormDestroy:**
```pascal
gtk_key_snooper_remove(GtkKeySnooperId);
```

---

## Shortcut Handling Levels

The application handles shortcuts at **3 levels**:

### 1. GTK Key Snooper (Lowest Level - Linux/GTK2)
- **File**: `uMainForm.pas` (function `GtkKeySnooper`)
- **When**: Intercepts ALL keystrokes, even with focus on MPV
- **Handled Shortcuts** (essential during playback):
  - `Ctrl+M` → Mute
  - `Ctrl+S` → Screenshot
  - `Ctrl+G` → Go to Time
  - `Ctrl+L` → Clear A-B Loop
  - `Ctrl+Z` → Reset Subtitle Delay
  - `Ctrl++` → Reset Audio Delay
  - `Ctrl+←` → Seek -60s
  - `Ctrl+→` → Seek +60s
  - `Ctrl+Shift+V` → Next Visualization Mode
  - `Ctrl+Shift+M` → Next Color Scheme

### 2. Application.OnShortCut (Application Level)
- **File**: `uMainForm.pas` (procedure `AppShortCut`)
- **When**: Before menus process shortcuts
- **Usage**: Fallback for platforms without GTK Key Snooper

### 3. Menu ShortCuts (LCL Level)
- **File**: `uMainForm.lfm` (TMenuItem `ShortCut` property)
- **When**: Standard LCL menu shortcut handling
- **Calculation**: `ShortCut = KeyCode + Modifiers`
  - Ctrl = 16384
  - Shift = 8192
  - Alt = 32768
  - Example: Ctrl+M = 16384 + 77 = 16461

### 4. FormKeyDown (Form Level)
- **File**: `uMainForm.pas` (procedure `FormKeyDown`)
- **When**: When the form has focus
- **Usage**: Shortcuts via ShortcutManager

### 5. ShortcutManager (Customizable Shortcuts)
- **File**: `uShortcuts.pas`
- **When**: User-configurable shortcuts
- **Storage**: `~/.config/3nity-media/shortcuts.ini`

---

## Shortcuts List

### Playback
| Shortcut | Action | ShortcutManager |
|----------|--------|-----------------|
| Space | Play/Pause | saPlayPause |
| S | Stop | saStop |
| P | Previous | saPrevious |
| N | Next | saNext |
| ← | Seek -10s | saSeekBackward |
| → | Seek +10s | saSeekForward |
| Ctrl+← | Seek -60s | saSeekBackward5 |
| Ctrl+→ | Seek +60s | saSeekForward5 |
| ] | Speed Up | saSpeedUp |
| [ | Speed Down | saSpeedDown |
| Backspace | Speed Reset | saSpeedReset |
| L | Set Loop A | saSetLoopA |
| Shift+L | Set Loop B | saSetLoopB |
| Ctrl+L | Clear Loop | saClearLoop |
| . | Frame Forward | saFrameForward |
| , | Frame Backward | saFrameBackward |
| Page Up | Previous Chapter | saPrevChapter |
| Page Down | Next Chapter | saNextChapter |
| Ctrl+G | Go to Time | saGotoTime |

### Audio
| Shortcut | Action | ShortcutManager |
|----------|--------|-----------------|
| ↑ | Volume Up | saVolumeUp |
| ↓ | Volume Down | saVolumeDown |
| **Ctrl+M** | **Mute** | saMute |
| + | Audio Delay + | saAudioDelayPlus |
| - | Audio Delay - | saAudioDelayMinus |
| Ctrl++ | Audio Delay Reset | saAudioDelayReset |

### Video
| Shortcut | Action | ShortcutManager |
|----------|--------|-----------------|
| F | Fullscreen | saFullscreen |
| R | Rotate | saRotate |
| D | Deinterlace | saDeinterlace |
| Num + | Zoom In | saZoomIn |
| Num - | Zoom Out | saZoomOut |
| Num * | Zoom Reset | saZoomReset |
| W | Fit to Video | saFitToVideo |
| Ctrl+S | Screenshot | saScreenshot |

### Visualization
| Shortcut | Action | ShortcutManager |
|----------|--------|-----------------|
| **Ctrl+Shift+V** | **Next Mode** | saVisNextMode |
| **Ctrl+Shift+M** | **Next Color** | saVisNextColor |

### Subtitles
| Shortcut | Action | ShortcutManager |
|----------|--------|-----------------|
| X | Subtitle Delay + | saSubtitleDelayPlus |
| Z | Subtitle Delay - | saSubtitleDelayMinus |
| Ctrl+Z | Subtitle Delay Reset | saSubtitleDelayReset |

### DVD Navigation
| Shortcut | Action | ShortcutManager |
|----------|--------|-----------------|
| Num 0 | DVD Menu | saDVDMenu |
| Num 8 | DVD Up | saDVDUp |
| Num 2 | DVD Down | saDVDDown |
| Num 4 | DVD Left | saDVDLeft |
| Num 6 | DVD Right | saDVDRight |
| Num 5 | DVD Select | saDVDSelect |

### Window
| Shortcut | Action | ShortcutManager |
|----------|--------|-----------------|
| Ctrl+O | Open File | saOpenFile |
| Ctrl+U | Open URL | saOpenURL |
| Ctrl+Shift+P | Playlist | saPlaylist |
| Ctrl+Q | Quit | saQuit |

---

## Adding a New Shortcut

### 1. Add to ShortcutManager (`uShortcuts.pas`)

```pascal
// In TShortcutAction
TShortcutAction = (
  ...
  saNewAction,  // Add here
  ...
);

// In InitializeDefaults
FShortcuts[saNewAction].DefaultKey := Ord('X');
FShortcuts[saNewAction].DefaultShift := [ssCtrl];

// In GetActionName
saNewAction: Result := 'NewAction';

// In GetActionCategory
saNewAction: Result := 'Category';
```

### 2. Add to GTK Key Snooper (`uMainForm.pas`)

```pascal
// In GtkKeySnooper, add:
else if CtrlPressed and (Event^.keyval = GDK_KEY_x) then
begin
  if frmMain <> nil then
  begin
    frmMain.DoNewAction;
    Handled := True;
  end;
end;
```

### 3. Add to FormKeyDown (`uMainForm.pas`)

```pascal
else if ShortcutManager.MatchesShortcut(Key, ModShift, saNewAction) then
  DoNewAction
```

### 4. Add to language files

**en.lang:**
```ini
[Shortcuts]
ActNewAction=New Action Description
```

**fr.lang:**
```ini
[Shortcuts]
ActNewAction=Description nouvelle action
```

---

## Debugging Shortcuts

### Check if GTK Key Snooper receives keystrokes:
```pascal
function GtkKeySnooper(...): gint; cdecl;
begin
  WriteLn('[Snooper] keyval=', Event^.keyval,
          ' state=', Event^.state,
          ' Ctrl=', (Event^.state and GDK_CONTROL_MASK) <> 0);
  ...
end;
```

### Check ShortcutManager:
```pascal
WriteLn('saMute Key=', ShortcutManager.Shortcuts[saMute].Key,
        ' Shift=', ShortcutManager.Shortcuts[saMute].Shift);
```

### Common GDK key codes:
- `GDK_KEY_m` = 109 (lowercase)
- `GDK_KEY_M` = 77 (uppercase)
- `GDK_KEY_t` = 116
- `GDK_KEY_v` = 118
- `GDK_CONTROL_MASK` = 4
- `GDK_SHIFT_MASK` = 1

---

## Important Notes

1. **Shortcuts with Ctrl work better** than simple keys because they are less likely to be captured by MPV

2. **GTK Key Snooper is essential** for Linux/GTK2 when MPV is embedded

3. **Avoid duplicates**: Check that a shortcut is not already in use:
   ```bash
   grep -E "ShortCut = [0-9]+" uMainForm.lfm | sort | uniq -d
   ```

4. **MPV Configuration**: These options are crucial:
   ```pascal
   mpv_set_option_string(FHandle, 'input-default-bindings', 'no');
   mpv_set_option_string(FHandle, 'input-vo-keyboard', 'no');
   ```
