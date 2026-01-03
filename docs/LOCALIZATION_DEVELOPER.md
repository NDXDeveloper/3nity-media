# 3nity Media - Developer Documentation: Localization System

## System Architecture

### Overview

The localization system uses `.lang` files in INI format to store translations. The main manager is `TLocaleManager` in `uLocale.pas`.

```
bin/x86_64-linux/
└── locale/
    ├── en.lang    (English - default language)
    ├── fr.lang    (French)
    └── [xx].lang  (Other languages)
```

### Automatic Language Detection

At startup, the application automatically detects the system language via environment variables:

1. `LC_ALL`
2. `LC_MESSAGES`
3. `LANG`

The language code is extracted (e.g., `fr_FR.UTF-8` → `fr`) and the corresponding file is loaded if it exists.

---

## .lang File Structure

### Format

Files use standard INI format:

```ini
[Section]
Key=Translated value
; This is a comment
```

### Required Section: [Language]

Each file must start with this section:

```ini
[Language]
Name=English
Code=en
Author=Nicolas DEOUX
```

| Key | Description |
|-----|-------------|
| `Name` | Language name (displayed in options) |
| `Code` | ISO 639-1 code (2 letters: en, fr, de, es...) |
| `Author` | Translation author |

### Available Sections

| Section | Description | Shortcut Method |
|---------|-------------|-----------------|
| `[Menu]` | Menu items | `Locale.Menu()` |
| `[Dialog]` | Dialog titles and text | `Locale.Dialog()` |
| `[Status]` | Status bar messages | `Locale.Status()` |
| `[Message]` | User messages | `Locale.Message()` |
| `[Button]` | Button labels | `Locale.Button()` |
| `[Tooltip]` | Tooltips | `Locale.Tooltip()` |
| `[Label]` | General labels | `Locale.Label_()` |
| `[Playlist]` | Playlist window | `Locale.GetString()` |
| `[Equalizer]` | Equalizer window | `Locale.GetString()` |
| `[Options]` | Options window | `Locale.GetString()` |
| `[Shortcuts]` | Shortcuts editor | `Locale.GetString()` |
| `[Radios]` | Radios window | `Locale.GetString()` |
| `[About]` | About window | `Locale.GetString()` |
| `[MediaInfo]` | Media information | `Locale.GetString()` |
| `[Bookmarks]` | Bookmarks | `Locale.GetString()` |
| `[Favorites]` | Favorites | `Locale.GetString()` |
| `[History]` | History | `Locale.GetString()` |
| `[SleepTimer]` | Sleep timer | `Locale.GetString()` |
| `[GotoTime]` | Go to time | `Locale.GetString()` |
| `[Log]` | Log window | `Locale.GetString()` |

---

## Usage in Code

### Global Instance

```pascal
uses uLocale;

// Access via global function
Locale.GetString('Section', 'Key', 'Default value');

// Or via shortcut function
_T('Section', 'Key', 'Default value');
```

### Shortcut Methods

```pascal
// For menus
mnuFile.Caption := Locale.Menu('File', '&File');

// For dialogs
ShowMessage(Locale.Dialog('ConfirmDelete', 'Delete this item?'));

// For status messages
StatusBar.SimpleText := Locale.Status('Ready', 'Ready');

// For buttons
btnOK.Caption := Locale.Button('OK', 'OK');

// For tooltips
btnPlay.Hint := Locale.Tooltip('Play', 'Play media');
```

### Generic Method

```pascal
// For any section
Label1.Caption := Locale.GetString('Options', 'Language', 'Language:');

// For specific sections
edtSearch.TextHint := Locale.GetString('Playlist', 'SearchHint', 'Search...');
```

---

## Naming Conventions

### Menu Keys

Format: `[Parent][Action]`

```ini
[Menu]
File=&File
FileOpen=&Open File...
FileOpenURL=Open &URL...
FileRecent=&Recent Files
FileExit=E&xit

Playback=&Playback
PlaybackPlayPause=&Play/Pause
PlaybackStop=&Stop
```

### Action Keys (Shortcuts)

Format: `Act[ActionName]`

```ini
[Shortcuts]
ActPlayPause=Play / Pause
ActStop=Stop
ActMute=Mute / Unmute
ActFullscreen=Fullscreen
```

### Keyboard Shortcuts in Menus

Use `&` to indicate the Alt shortcut letter:

```ini
FileOpen=&Open File...   ; Alt+O
FileExit=E&xit           ; Alt+X
```

### Format Variables

Use `%s`, `%d`, `%f` for dynamic values:

```ini
[Message]
FilesFound=%d files found
CurrentVolume=Volume: %d%%
PlaybackSpeed=Speed: %.2fx
WelcomeUser=Welcome, %s!
```

---

## Adding a New Language

### 1. Create the File

Copy `en.lang` to `[code].lang`:

```bash
cp locale/en.lang locale/de.lang
```

### 2. Modify the Header

```ini
[Language]
Name=Deutsch
Code=de
Author=Your Name
```

### 3. Translate the Sections

Translate each value while keeping:
- Keys identical
- `&` for Alt shortcuts
- `%s`, `%d`, `%f` for formatting
- Parameter order

### 4. Test

The application will automatically detect the new file at startup.

---

## Adding a New Translatable String

### 1. Add to Language Files

**en.lang:**
```ini
[Section]
NewKey=English text
```

**fr.lang:**
```ini
[Section]
NewKey=French text
```

### 2. Use in Code

```pascal
procedure TMyForm.UpdateLocale;
begin
  lblNew.Caption := Locale.GetString('Section', 'NewKey', 'Default text');
end;
```

### 3. Call on Language Change

In the form's `UpdateLocale` procedure:

```pascal
procedure TfrmMain.UpdateLocale;
begin
  // ... other translations ...
  lblNew.Caption := Locale.GetString('Section', 'NewKey', 'Default text');
end;
```

---

## Best Practices

### 1. Always Provide a Default Value

```pascal
// GOOD
Locale.Menu('FileOpen', '&Open File...');

// BAD - no fallback
Locale.Menu('FileOpen', '');
```

### 2. Keep Keys in English

```ini
; GOOD
FileOpen=Open File...

; BAD
OuvrirFichier=Open File...
```

### 3. Use Comments

```ini
[Menu]
; File menu
File=&File
FileOpen=&Open File...

; Playback menu
Playback=&Playback
```

### 4. Group Related Translations

```ini
[Playlist]
; Toolbar
AddFiles=Add files
RemoveSelected=Remove selected
Clear=Clear all

; Context menu
PlayNow=Play now
RemoveFromList=Remove from list
```

### 5. Check Consistency

Ensure all languages have the same keys:

```bash
# Compare keys between two files
diff <(grep -E '^[A-Za-z]' en.lang | cut -d= -f1 | sort) \
     <(grep -E '^[A-Za-z]' fr.lang | cut -d= -f1 | sort)
```

---

## TLocaleManager Structure

```pascal
TLocaleManager = class
private
  FCurrentLang: string;           // Current language code ('fr', 'en'...)
  FLangFile: TIniFile;            // Loaded INI file
  FLangPath: string;              // Path to locale/ folder
  FAvailableLanguages: TStringList; // List of available codes
  FLanguageNames: TStringList;    // List of language names

public
  function LoadLanguage(const LangCode: string): Boolean;
  function GetString(const Section, Key, Default: string): string;

  // Shortcuts
  function Menu(const Key, Default: string): string;
  function Dialog(const Key, Default: string): string;
  function Status(const Key, Default: string): string;
  function Button(const Key, Default: string): string;
  function Label_(const Key, Default: string): string;
  function Message(const Key, Default: string): string;
  function Tooltip(const Key, Default: string): string;

  property CurrentLanguage: string;
  property AvailableLanguages: TStringList;
  property LanguageNames: TStringList;
end;
```

---

## Language File Locations

The application searches for files in this order:

1. `[ExePath]/locale/`
2. `[ExePath]/lang/`
3. `[ExePath]/../share/3nity/locale/`

For development, place files in:
```
bin/x86_64-linux/locale/
```

---

## Debugging

### Check Loaded Language

```pascal
WriteLn('Current language: ', Locale.CurrentLanguage);
WriteLn('Lang path: ', Locale.LangPath);
```

### List Available Languages

```pascal
var
  I: Integer;
begin
  for I := 0 to Locale.AvailableLanguages.Count - 1 do
    WriteLn(Locale.AvailableLanguages[I], ' = ', Locale.LanguageNames[I]);
end;
```

### Test a Translation

```pascal
WriteLn(Locale.GetString('Menu', 'FileOpen', 'NOT FOUND'));
```
