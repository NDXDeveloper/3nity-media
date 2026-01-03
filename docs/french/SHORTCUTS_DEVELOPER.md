# 3nity Media - Documentation Développeur: Raccourcis Clavier

## Architecture des Raccourcis

### Le Problème du Focus X11/MPV

Quand MPV est intégré dans une application GTK2 via X11 (embedding), la fenêtre vidéo MPV peut capturer le focus clavier. Cela pose un problème majeur:

1. **Focus X11**: L'utilisateur clique sur la zone vidéo → le focus X11 va à la fenêtre MPV
2. **Événements perdus**: Les touches clavier sont envoyées à MPV, pas à l'application LCL
3. **Raccourcis inopérants**: `FormKeyDown`, `Application.OnShortCut` ne reçoivent jamais les événements

### La Solution: GTK Key Snooper

Nous utilisons `gtk_key_snooper_install()` qui intercepte **TOUTES** les touches au niveau GTK, **avant** qu'elles n'atteignent un widget:

```pascal
{$IFDEF LCLGTK2}
function GtkKeySnooper(Widget: PGtkWidget; Event: PGdkEventKey; Data: gpointer): gint; cdecl;
begin
  Result := 0; // 0 = laisser passer, 1 = bloquer

  if Event^._type = GDK_KEY_PRESS then
  begin
    // Intercepter Ctrl+M pour Mute
    if (Event^.state and GDK_CONTROL_MASK) <> 0 then
    begin
      if Event^.keyval = GDK_KEY_m then
      begin
        frmMain.FMPVEngine.Muted := not frmMain.FMPVEngine.Muted;
        Result := 1; // Bloquer l'événement
      end;
    end;
  end;
end;
{$ENDIF}
```

**Installation dans FormCreate:**
```pascal
GtkKeySnooperId := gtk_key_snooper_install(@GtkKeySnooper, nil);
```

**Désinstallation dans FormDestroy:**
```pascal
gtk_key_snooper_remove(GtkKeySnooperId);
```

---

## Niveaux de Gestion des Raccourcis

L'application gère les raccourcis à **3 niveaux**:

### 1. GTK Key Snooper (Niveau le plus bas - Linux/GTK2)
- **Fichier**: `uMainForm.pas` (fonction `GtkKeySnooper`)
- **Quand**: Intercepte TOUTES les touches, même avec focus sur MPV
- **Raccourcis gérés** (essentiels pendant la lecture):
  - `Ctrl+M` → Mute
  - `Ctrl+T` → Always on Top
  - `Ctrl+S` → Screenshot
  - `Ctrl+G` → Go to Time
  - `Ctrl+L` → Clear A-B Loop
  - `Ctrl+Z` → Reset Subtitle Delay
  - `Ctrl++` → Reset Audio Delay
  - `Ctrl+←` → Seek -60s
  - `Ctrl+→` → Seek +60s
  - `Ctrl+Shift+V` → Next Visualization Mode
  - `Ctrl+Shift+M` → Next Color Scheme

### 2. Application.OnShortCut (Niveau Application)
- **Fichier**: `uMainForm.pas` (procédure `AppShortCut`)
- **Quand**: Avant que les menus ne traitent les raccourcis
- **Usage**: Backup pour les plateformes sans GTK Key Snooper

### 3. Menu ShortCuts (Niveau LCL)
- **Fichier**: `uMainForm.lfm` (propriété `ShortCut` des TMenuItem)
- **Quand**: Gestion standard LCL des raccourcis menu
- **Calcul**: `ShortCut = KeyCode + Modifiers`
  - Ctrl = 16384
  - Shift = 8192
  - Alt = 32768
  - Exemple: Ctrl+M = 16384 + 77 = 16461

### 4. FormKeyDown (Niveau Formulaire)
- **Fichier**: `uMainForm.pas` (procédure `FormKeyDown`)
- **Quand**: Quand le formulaire a le focus
- **Usage**: Raccourcis via ShortcutManager

### 5. ShortcutManager (Raccourcis Personnalisables)
- **Fichier**: `uShortcuts.pas`
- **Quand**: Raccourcis configurables par l'utilisateur
- **Stockage**: `~/.config/3nity-media/shortcuts.ini`

---

## Liste des Raccourcis

### Lecture (Playback)
| Raccourci | Action | ShortcutManager |
|-----------|--------|-----------------|
| Espace | Play/Pause | saPlayPause |
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
| Raccourci | Action | ShortcutManager |
|-----------|--------|-----------------|
| ↑ | Volume Up | saVolumeUp |
| ↓ | Volume Down | saVolumeDown |
| **Ctrl+M** | **Mute** | saMute |
| + | Audio Delay + | saAudioDelayPlus |
| - | Audio Delay - | saAudioDelayMinus |
| Ctrl++ | Audio Delay Reset | saAudioDelayReset |

### Video
| Raccourci | Action | ShortcutManager |
|-----------|--------|-----------------|
| F | Fullscreen | saFullscreen |
| R | Rotate | saRotate |
| D | Deinterlace | saDeinterlace |
| Num + | Zoom In | saZoomIn |
| Num - | Zoom Out | saZoomOut |
| Num * | Zoom Reset | saZoomReset |
| **Ctrl+T** | **Always on Top** | saAlwaysOnTop |
| W | Fit to Video | saFitToVideo |
| Ctrl+S | Screenshot | saScreenshot |

### Visualisation
| Raccourci | Action | ShortcutManager |
|-----------|--------|-----------------|
| **Ctrl+Shift+V** | **Next Mode** | saVisNextMode |
| **Ctrl+Shift+M** | **Next Color** | saVisNextColor |

### Sous-titres
| Raccourci | Action | ShortcutManager |
|-----------|--------|-----------------|
| X | Subtitle Delay + | saSubtitleDelayPlus |
| Z | Subtitle Delay - | saSubtitleDelayMinus |
| Ctrl+Z | Subtitle Delay Reset | saSubtitleDelayReset |

### Navigation DVD
| Raccourci | Action | ShortcutManager |
|-----------|--------|-----------------|
| Num 0 | DVD Menu | saDVDMenu |
| Num 8 | DVD Up | saDVDUp |
| Num 2 | DVD Down | saDVDDown |
| Num 4 | DVD Left | saDVDLeft |
| Num 6 | DVD Right | saDVDRight |
| Num 5 | DVD Select | saDVDSelect |

### Fenêtre
| Raccourci | Action | ShortcutManager |
|-----------|--------|-----------------|
| Ctrl+O | Open File | saOpenFile |
| Ctrl+U | Open URL | saOpenURL |
| Ctrl+Shift+P | Playlist | saPlaylist |
| Ctrl+Q | Quit | saQuit |

---

## Ajouter un Nouveau Raccourci

### 1. Ajouter au ShortcutManager (`uShortcuts.pas`)

```pascal
// Dans TShortcutAction
TShortcutAction = (
  ...
  saNewAction,  // Ajouter ici
  ...
);

// Dans InitializeDefaults
FShortcuts[saNewAction].DefaultKey := Ord('X');
FShortcuts[saNewAction].DefaultShift := [ssCtrl];

// Dans GetActionName
saNewAction: Result := 'NewAction';

// Dans GetActionCategory
saNewAction: Result := 'Category';
```

### 2. Ajouter au GTK Key Snooper (`uMainForm.pas`)

```pascal
// Dans GtkKeySnooper, ajouter:
else if CtrlPressed and (Event^.keyval = GDK_KEY_x) then
begin
  if frmMain <> nil then
  begin
    frmMain.DoNewAction;
    Handled := True;
  end;
end;
```

### 3. Ajouter au FormKeyDown (`uMainForm.pas`)

```pascal
else if ShortcutManager.MatchesShortcut(Key, ModShift, saNewAction) then
  DoNewAction
```

### 4. Ajouter aux fichiers de langue

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

## Débogage des Raccourcis

### Vérifier si le GTK Key Snooper reçoit les touches:
```pascal
function GtkKeySnooper(...): gint; cdecl;
begin
  WriteLn('[Snooper] keyval=', Event^.keyval,
          ' state=', Event^.state,
          ' Ctrl=', (Event^.state and GDK_CONTROL_MASK) <> 0);
  ...
end;
```

### Vérifier le ShortcutManager:
```pascal
WriteLn('saMute Key=', ShortcutManager.Shortcuts[saMute].Key,
        ' Shift=', ShortcutManager.Shortcuts[saMute].Shift);
```

### Codes de touches GDK courants:
- `GDK_KEY_m` = 109 (minuscule)
- `GDK_KEY_M` = 77 (majuscule)
- `GDK_KEY_t` = 116
- `GDK_KEY_v` = 118
- `GDK_CONTROL_MASK` = 4
- `GDK_SHIFT_MASK` = 1

---

## Notes Importantes

1. **Les raccourcis avec Ctrl fonctionnent mieux** que les touches simples car elles sont moins susceptibles d'être capturées par MPV

2. **Le GTK Key Snooper est essentiel** pour Linux/GTK2 quand MPV est intégré

3. **Éviter les doublons**: Vérifier qu'un raccourci n'est pas déjà utilisé:
   ```bash
   grep -E "ShortCut = [0-9]+" uMainForm.lfm | sort | uniq -d
   ```

4. **Configuration MPV**: Ces options sont cruciales:
   ```pascal
   mpv_set_option_string(FHandle, 'input-default-bindings', 'no');
   mpv_set_option_string(FHandle, 'input-vo-keyboard', 'no');
   ```
