# 3nity Media - Developer Cookbook

Step-by-step recipes for common development tasks.

---

## Table of Contents

1. [Adding a New Menu Item](#recipe-1-adding-a-new-menu-item)
2. [Adding a New Configuration Option](#recipe-2-adding-a-new-configuration-option)
3. [Adding a New Translation Key](#recipe-3-adding-a-new-translation-key)
4. [Adding a New Keyboard Shortcut](#recipe-4-adding-a-new-keyboard-shortcut)
5. [Adding a New Supported File Format](#recipe-5-adding-a-new-supported-file-format)
6. [Adding a New Form/Dialog](#recipe-6-adding-a-new-formdialog)
7. [Adding a New MPV Property](#recipe-7-adding-a-new-mpv-property)
8. [Adding a New Equalizer Preset](#recipe-8-adding-a-new-equalizer-preset)
9. [Adding a New Playlist Format](#recipe-9-adding-a-new-playlist-format)
10. [Adding Support for a New Disc Format](#recipe-10-adding-support-for-a-new-disc-format)

---

## Recipe 1: Adding a New Menu Item

**Goal:** Add a menu item "Do Something" under the File menu.

### Step 1: Declare the menu item

In `src/Forms/uMainForm.pas`, add to the private section of `TfrmMain`:

```pascal
private
  // ... existing declarations
  mnuDoSomething: TMenuItem;
```

### Step 2: Create the menu item

In `TfrmMain.FormCreate`, add:

```pascal
{ Create "Do Something" menu item }
mnuDoSomething := TMenuItem.Create(mnuFile);
mnuDoSomething.Caption := Locale.Menu('DoSomething', 'Do Something');
mnuDoSomething.ShortCut := ShortCut(VK_D, [ssCtrl, ssShift]); // Ctrl+Shift+D
mnuDoSomething.OnClick := @mnuDoSomethingClick;
mnuFile.Insert(5, mnuDoSomething); // Insert at position 5
```

### Step 3: Implement the handler

Add the procedure declaration and implementation:

```pascal
// In the class declaration (public or published section)
procedure mnuDoSomethingClick(Sender: TObject);

// Implementation
procedure TfrmMain.mnuDoSomethingClick(Sender: TObject);
begin
  ShowMessage('Doing something!');
end;
```

### Step 4: Add translation key

In `resources/locale/en.lang`:
```ini
[Menu]
DoSomething=Do Something
```

### Step 5: Build and test

```bash
make run
```

---

## Recipe 2: Adding a New Configuration Option

**Goal:** Add a boolean option "AutoPlay" that persists between sessions.

### Step 1: Add to settings record

In `src/Common/uConfig.pas`, find `TGeneralSettings` and add:

```pascal
TGeneralSettings = record
  // ... existing fields
  AutoPlay: Boolean;
end;
```

### Step 2: Set default value

In `TConfig.SetDefaults`:

```pascal
procedure TConfig.SetDefaults;
begin
  // ... existing defaults
  FSettings.General.AutoPlay := True; // Default to enabled
end;
```

### Step 3: Load from INI

In `TConfig.LoadSettings`:

```pascal
procedure TConfig.LoadSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetConfigFilePath);
  try
    // ... existing loading
    FSettings.General.AutoPlay := Ini.ReadBool('General', 'AutoPlay', True);
  finally
    Ini.Free;
  end;
end;
```

### Step 4: Save to INI

In `TConfig.SaveSettings`:

```pascal
procedure TConfig.SaveSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetConfigFilePath);
  try
    // ... existing saving
    Ini.WriteBool('General', 'AutoPlay', FSettings.General.AutoPlay);
  finally
    Ini.Free;
  end;
end;
```

### Step 5: Use in code

```pascal
if Config.Settings.General.AutoPlay then
  PlayFile(FileName);
```

### Step 6: Add to Options dialog (optional)

In `src/Forms/uOptions.pas`, add a checkbox and wire it up.

---

## Recipe 3: Adding a New Translation Key

**Goal:** Add a message "FileNotSupported" with translation support.

### Step 1: Add to English language file

In `resources/locale/en.lang`:

```ini
[Messages]
FileNotSupported=This file format is not supported: %s
```

### Step 2: Add to French language file

In `resources/locale/fr.lang`:

```ini
[Messages]
FileNotSupported=Ce format de fichier n'est pas pris en charge : %s
```

### Step 3: Add to other languages as needed

Repeat for other `.lang` files in `resources/locale/`.

### Step 4: Use in code

```pascal
// Simple message
ShowMessage(Locale.Message('FileNotSupported', 'File not supported: %s'));

// With formatting
ShowMessage(Format(
  Locale.Message('FileNotSupported', 'File not supported: %s'),
  [ExtractFileName(FileName)]
));
```

### Translation Key Categories

| Section | Usage | Example |
|---------|-------|---------|
| `[Menu]` | Menu items | `Locale.Menu('Open', 'Open')` |
| `[Buttons]` | Button labels | `Locale.Button('Play', 'Play')` |
| `[Messages]` | User messages | `Locale.Message('Error', 'Error')` |
| `[Dialogs]` | Dialog titles/text | `Locale.Dialog('OpenFile', 'Open File')` |
| `[Hints]` | Tooltips | `Locale.Hint('Play', 'Play media')` |

---

## Recipe 4: Adding a New Keyboard Shortcut

**Goal:** Add a shortcut Ctrl+Shift+R to reload the current file.

### Step 1: Define shortcut action

In `src/Common/uShortcuts.pas`, add to `TShortcutAction`:

```pascal
TShortcutAction = (
  // ... existing actions
  saReloadFile
);
```

### Step 2: Add default shortcut

In `TShortcutManager.SetDefaults`:

```pascal
procedure TShortcutManager.SetDefaults;
begin
  // ... existing defaults
  FShortcuts[saReloadFile] := ShortCut(VK_R, [ssCtrl, ssShift]);
end;
```

### Step 3: Add action name for INI persistence

In `GetActionName`:

```pascal
function GetActionName(Action: TShortcutAction): string;
begin
  case Action of
    // ... existing cases
    saReloadFile: Result := 'ReloadFile';
  end;
end;
```

### Step 4: Handle the shortcut in MainForm

In `TfrmMain.FormKeyDown` or shortcut handler:

```pascal
procedure TfrmMain.HandleShortcut(Action: TShortcutAction);
begin
  case Action of
    // ... existing cases
    saReloadFile:
      if FMPVEngine.FileLoaded <> '' then
        FMPVEngine.ReloadCurrentFile;
  end;
end;
```

### Step 5: Add to shortcuts editor

The shortcut will automatically appear in the shortcuts editor if you add it to the action enum and defaults.

---

## Recipe 5: Adding a New Supported File Format

**Goal:** Add support for `.opus` audio files.

### Step 1: Add to media file detection

In `src/Forms/uMainForm.pas`, find `IsMediaFile`:

```pascal
function TfrmMain.IsMediaFile(const FileName: string): Boolean;
const
  AudioExtensions: array[0..XX] of string = (
    '.mp3', '.flac', '.ogg', '.wav', '.aac', '.m4a', '.wma',
    '.opus'  // Add here
  );
```

### Step 2: Add to file open dialog filter

In `TfrmMain.CreateFileDialogs` or where the open dialog is configured:

```pascal
OpenDialog.Filter :=
  'All Media Files|*.mp3;*.mp4;*.mkv;*.opus;...|' +  // Add .opus
  'Audio Files|*.mp3;*.flac;*.opus;...|' +           // Add .opus
  'All Files|*.*';
```

### Step 3: Update documentation

Add `.opus` to the supported formats in `docs/USER_GUIDE.md`.

**Note:** libmpv already supports most formats. You typically only need to update the file detection and dialog filters.

---

## Recipe 6: Adding a New Form/Dialog

**Goal:** Create a new "Statistics" dialog.

### Step 1: Create the form file

Create `src/Forms/uStatistics.pas`:

```pascal
{ ═══════════════════════════════════════════════════════════════════════════
  uStatistics.pas - Playback Statistics Dialog

  Part of 3nity Media - Lazarus Edition
  ═══════════════════════════════════════════════════════════════════════════ }

unit uStatistics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uLocale;

type
  TfrmStatistics = class(TForm)
    lblTotalPlayed: TLabel;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    procedure ApplyLocale;
  public
    procedure UpdateStats;
  end;

var
  frmStatistics: TfrmStatistics;

implementation

{$R *.lfm}

procedure TfrmStatistics.FormCreate(Sender: TObject);
begin
  ApplyLocale;
end;

procedure TfrmStatistics.ApplyLocale;
begin
  Caption := Locale.Dialog('Statistics', 'Statistics');
  btnClose.Caption := Locale.Button('Close', 'Close');
end;

procedure TfrmStatistics.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmStatistics.UpdateStats;
begin
  lblTotalPlayed.Caption := 'Files played: 42';
end;

end.
```

### Step 2: Create the form design file

Create `src/Forms/uStatistics.lfm` with Lazarus form designer or manually.

### Step 3: Add to project

In `src/TrinityMedia.lpr`, add:

```pascal
uses
  // ... existing uses
  uStatistics;
```

### Step 4: Add menu item to open the dialog

In `TfrmMain`:

```pascal
procedure TfrmMain.mnuStatisticsClick(Sender: TObject);
begin
  if frmStatistics = nil then
    Application.CreateForm(TfrmStatistics, frmStatistics);
  frmStatistics.UpdateStats;
  frmStatistics.ShowModal;
end;
```

---

## Recipe 7: Adding a New MPV Property

**Goal:** Add a property to control audio normalization.

### Step 1: Add property to TMPVEngine

In `src/Core/uMPVEngine.pas`:

```pascal
// Private field
private
  FAudioNormalize: Boolean;

// Public property
public
  property AudioNormalize: Boolean read FAudioNormalize write SetAudioNormalize;

// Setter implementation
procedure TMPVEngine.SetAudioNormalize(Value: Boolean);
begin
  if FAudioNormalize <> Value then
  begin
    FAudioNormalize := Value;
    if FInitialized then
    begin
      if Value then
        mpv_set_option_string(FHandle, 'af', 'loudnorm')
      else
        mpv_set_option_string(FHandle, 'af', '');
    end;
  end;
end;
```

### Step 2: Initialize in constructor

```pascal
constructor TMPVEngine.Create;
begin
  inherited;
  FAudioNormalize := False;
  // ...
end;
```

### Step 3: Use from MainForm

```pascal
FMPVEngine.AudioNormalize := True;
```

---

## Recipe 8: Adding a New Equalizer Preset

**Goal:** Add a "Vocal Boost" preset.

### Step 1: Define the preset

In `src/Forms/uEqualizer.pas`, find the presets array:

```pascal
const
  PRESET_NAMES: array[0..XX] of string = (
    'Flat', 'Rock', 'Pop', 'Jazz', 'Classical',
    'Vocal Boost'  // Add here
  );

  PRESET_VALUES: array[0..XX] of array[0..9] of Double = (
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),       // Flat
    (4, 3, 2, 1, 0, -1, 1, 2, 3, 4),      // Rock
    // ... existing presets
    (-2, -1, 0, 3, 5, 5, 3, 0, -1, -2)    // Vocal Boost (boost mids)
  );
```

### Step 2: Add translation

In `resources/locale/en.lang`:
```ini
[Equalizer]
VocalBoost=Vocal Boost
```

### Step 3: Update preset loading logic

Ensure the preset combo box is populated with the new preset name.

---

## Recipe 9: Adding a New Playlist Format

**Goal:** Add support for XSPF playlist format.

### Step 1: Add format detection

In `src/Core/uPlaylistManager.pas`:

```pascal
function TPlaylistManager.IsPlaylistFile(const FileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := (Ext = '.m3u') or (Ext = '.m3u8') or (Ext = '.pls') or (Ext = '.xspf');
end;
```

### Step 2: Add load function

```pascal
function TPlaylistManager.LoadXSPF(const FileName: string): Boolean;
var
  Doc: TXMLDocument;
  TrackList, Track, Location: TDOMNode;
begin
  Result := False;
  try
    ReadXMLFile(Doc, FileName);
    try
      TrackList := Doc.DocumentElement.FindNode('trackList');
      if TrackList <> nil then
      begin
        Track := TrackList.FirstChild;
        while Track <> nil do
        begin
          if Track.NodeName = 'track' then
          begin
            Location := Track.FindNode('location');
            if Location <> nil then
              Add(Location.TextContent);
          end;
          Track := Track.NextSibling;
        end;
        Result := True;
      end;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;
```

### Step 3: Update LoadFromFile

```pascal
function TPlaylistManager.LoadFromFile(const FileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  case Ext of
    '.m3u', '.m3u8': Result := LoadM3U(FileName);
    '.pls': Result := LoadPLS(FileName);
    '.xspf': Result := LoadXSPF(FileName);
  else
    Result := False;
  end;
end;
```

### Step 4: Add save function (optional)

Implement `SaveXSPF` following the XSPF specification.

---

## Recipe 10: Adding Support for a New Disc Format

**Goal:** Add support for HD-DVD (hypothetical example).

### Step 1: Add menu item

In `TfrmMain.FormCreate`:

```pascal
mnuOpenHDDVD := TMenuItem.Create(mnuFile);
mnuOpenHDDVD.Caption := Locale.Menu('OpenHDDVD', 'Open HD-DVD...');
mnuOpenHDDVD.OnClick := @mnuOpenHDDVDClick;
mnuFile.Insert(4, mnuOpenHDDVD);
```

### Step 2: Implement the open procedure

```pascal
procedure TfrmMain.OpenHDDVD;
var
  HDDVDPath, ContentPath: string;
begin
  HDDVDPath := '';
  if SelectDirectory(
    Locale.Dialog('OpenHDDVD', 'Select HD-DVD Folder'),
    '', HDDVDPath) then
  begin
    { Check for HD-DVD structure }
    ContentPath := IncludeTrailingPathDelimiter(HDDVDPath) + 'HVDVD_TS';

    if DirectoryExists(ContentPath) then
    begin
      { Try native protocol first }
      PlayFile('hddvd://' + HDDVDPath);
    end
    else
    begin
      { Fallback: find and play largest EVO file }
      PlayLargestFile(ContentPath, '*.evo');
    end;
  end;
end;
```

### Step 3: Add hr-seek for the format

In `src/Core/uMPVEngine.pas`, add the file extension:

```pascal
FileExt := LowerCase(ExtractFileExt(URL));
if (FileExt = '.m2ts') or (FileExt = '.vob') or (FileExt = '.evo') then
  mpv_set_property_string(FHandle, 'hr-seek', 'yes');
```

### Step 4: Add translation keys

```ini
[Menu]
OpenHDDVD=Open HD-DVD...

[Dialogs]
OpenHDDVD=Select HD-DVD Folder
```

---

## See Also

- [QUICKSTART_DEVELOPER.md](QUICKSTART_DEVELOPER.md) - Getting started
- [ARCHITECTURE.md](ARCHITECTURE.md) - System architecture
- [API_REFERENCE.md](API_REFERENCE.md) - API documentation
- [CONTRIBUTING.md](CONTRIBUTING.md) - Code style guidelines

---

*Last updated: 2026-01-07*
