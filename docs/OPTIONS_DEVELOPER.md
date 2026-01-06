# 3nity Media - Developer Documentation: Options System

## System Architecture

### Overview

The options system uses three main components:

1. **TConfigManager** (`uConfig.pas`) - Configuration manager
2. **TAppSettings** (`uTypes.pas`) - Settings structure
3. **TfrmOptions** (`uOptions.pas`) - User interface

```
┌─────────────────────────────────────────────────────────────────┐
│                        Data Flow                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   INI File  ←──────────→  TConfigManager  ←──────────→  TfrmOptions 
│      │                         │                          │     │
│      │                         │                          │     │
│      ▼                         ▼                          ▼     │
│ config.ini          TAppSettings                UI Controls     │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Configuration File Locations

### Platform-Specific Paths

| Platform | Path |
|----------|------|
| Windows | `%APPDATA%\3nity-media\` |
| Linux | `~/.config/3nity-media/` |
| macOS | `~/.config/3nity-media/` |

### Configuration Files

| File | Description |
|------|-------------|
| `config.ini` | Main configuration |
| `history.ini` | Playback history |
| `bookmarks.ini` | Bookmarks |
| `favorites.ini` | Favorites |
| `shortcuts.ini` | Custom shortcuts |
| `session_playlist.ini` | Session playlist |

---

## INI File Structure

### Available Sections

```ini
[General]
Language=en
SingleInstance=1
ScreenshotPath=/home/user/Pictures/3nity/
ScreenshotFormat=png
HistoryEnabled=1
HistoryMaxItems=500
AutoSavePlaylist=1

[Video]
Brightness=0
Contrast=0
Saturation=0
Hue=0
Gamma=0
AspectMode=0
AspectFactor=-1
Deinterlace=0
DeinterlaceAlg=0
VideoOutput=auto
HWAccel=1

[Audio]
Volume=100
Muted=0
AudioOutput=auto
AudioDevice=
Channels=2
Normalize=0

[Subtitles]
UseDefault=1
FontName=Arial
FontSize=24
FontColor=16777215
FontBold=0
FontItalic=0
OutlineColor=0
OutlineSize=2
BackgroundColor=0
BackgroundOpacity=0
Position=95
Encoding=UTF-8
AutoLoad=1

[Cache]
DefaultSize=8192
FixedSize=8192
RamdiskSize=0
CDROMSize=4096
RemovableSize=4096
NetworkSize=16384
InternetSize=32768
DVDSize=8192

[Window]
MainLeft=100
MainTop=100
MainWidth=800
MainHeight=600
MainMaximized=0
PlaylistLeft=0
PlaylistTop=0
PlaylistWidth=350
PlaylistHeight=500

[Playlist]
PlaybackMode=0

[Equalizer]
Enabled=0
Band0=0
Band1=0
...
Band9=0

[RecentFiles]
Count=5
File0=/path/to/file1.mp4
File1=/path/to/file2.mkv
...
```

---

## TAppSettings Structure

Defined in `uTypes.pas`:

```pascal
TAppSettings = record
  General: TGeneralSettings;
  Video: TVideoSettings;
  Audio: TAudioSettings;
  Subtitles: TSubtitleSettings;
  Cache: TCacheSettings;
  MainWindow: TWindowGeometry;
  PlaylistWindow: TWindowGeometry;
  EqualizerWindow: TWindowGeometry;
  PlaybackMode: TPlaybackMode;
  EqualizerBands: array[0..9] of Double;
  EqualizerEnabled: Boolean;
end;
```

### Sub-structures

```pascal
TGeneralSettings = record
  Language: string;
  SingleInstance: Boolean;
  ScreenshotPath: string;
  ScreenshotFormat: string;
  HistoryEnabled: Boolean;
  HistoryMaxItems: Integer;
  AutoSavePlaylist: Boolean;
end;

TVideoSettings = record
  Brightness: Integer;      // -100 to +100
  Contrast: Integer;        // -100 to +100
  Saturation: Integer;      // -100 to +100
  Hue: Integer;             // -100 to +100
  Gamma: Integer;           // -100 to +100
  AspectMode: Integer;
  AspectFactor: Double;
  Deinterlace: Integer;
  DeinterlaceAlg: Integer;
  VideoOutput: string;
  HWAccel: Boolean;
end;

TAudioSettings = record
  Volume: Integer;          // 0 to 150
  Muted: Boolean;
  AudioOutput: string;
  AudioDevice: string;
  Channels: Integer;
  Normalize: Boolean;
end;
```

---

## TConfigManager - Configuration Manager

### Global Instance

```pascal
uses uConfig;

// Access via global variable
Config.Settings.General.Language;
Config.Settings.Video.Brightness;
Config.Save;
```

### Main Methods

```pascal
TConfigManager = class
public
  procedure Load;           // Load from INI file
  procedure Save;           // Save to INI file

  property Settings: TAppSettings;
  property Modified: Boolean;
  property ConfigPath: string;

  // Convenience accessors
  function GetGeneral: TGeneralSettings;
  function GetVideo: TVideoSettings;
  function GetAudio: TAudioSettings;
  function GetSubtitles: TSubtitleSettings;
  function GetCache: TCacheSettings;

  procedure SetGeneral(const Value: TGeneralSettings);
  procedure SetVideo(const Value: TVideoSettings);
  // ...
end;
```

### Initialization and Destruction

The `TConfigManager` is created automatically in the `initialization` section:

```pascal
initialization
  Config := TConfigManager.Create;
  Config.Load;

finalization
  FreeAndNil(Config);
```

---

## Options Dialog (uOptions.pas)

### The Three Buttons

The options dialog has three buttons with different behaviors:

| Button | Action |
|--------|--------|
| **OK** | Save + Apply + Close |
| **Cancel** | Close without saving |
| **Apply** | Save + Apply (stays open) |

### Button Code

```pascal
procedure TfrmOptions.btnOKClick(Sender: TObject);
begin
  SaveSettings;     // Save to Config
  ApplySettings;    // Apply to MPVEngine
  Close;            // Close dialog
end;

procedure TfrmOptions.btnCancelClick(Sender: TObject);
begin
  Close;            // Close without action
end;

procedure TfrmOptions.btnApplyClick(Sender: TObject);
begin
  SaveSettings;     // Save to Config
  ApplySettings;    // Apply to MPVEngine
  FModified := False;
end;
```

---

## The Importance of the "Apply" Button

### Why ApplySettings is Essential

The "Apply" button is crucial because it allows changes to be **immediately effective** without closing the dialog. This enables users to:

1. See the effect of changes in real-time
2. Adjust settings iteratively
3. Cancel if the result is not satisfactory

### Data Flow

```
┌────────────────┐     ┌─────────────────┐     ┌──────────────┐
│ UI Controls    │────→│ SaveSettings()  │────→│ Config.Save  │
│ (TrackBars,    │     │ Copy to         │     │ Write INI    │
│  CheckBoxes)   │     │ Config.Settings │     │              │
└────────────────┘     └─────────────────┘     └──────────────┘
                              │
                              ▼
┌────────────────┐     ┌─────────────────┐
│ MPVEngine      │←────│ ApplySettings() │
│ (Playback)     │     │ Apply to MPV    │
└────────────────┘     └─────────────────┘
```

---

## Adding a New Option

### Step 1: Add to Settings Record

In `uTypes.pas`, add the field to the appropriate structure:

```pascal
TGeneralSettings = record
  // ... existing fields ...
  NewOption: Boolean;  // New option
end;
```

### Step 2: Define Default Value

In `uConfig.pas`, in `SetDefaults`:

```pascal
procedure TConfigManager.SetDefaults;
begin
  // ...
  with FSettings.General do
  begin
    // ... existing values ...
    NewOption := True;  // Default value
  end;
end;
```

### Step 3: Load from INI

In `uConfig.pas`, in `LoadGeneralSection`:

```pascal
procedure TConfigManager.LoadGeneralSection;
begin
  with FSettings.General do
  begin
    // ... existing loads ...
    NewOption := FIniFile.ReadBool(INI_SECTION_GENERAL, 'NewOption', True);
  end;
end;
```

### Step 4: Save to INI

In `uConfig.pas`, in `SaveGeneralSection`:

```pascal
procedure TConfigManager.SaveGeneralSection;
begin
  with FSettings.General do
  begin
    // ... existing saves ...
    FIniFile.WriteBool(INI_SECTION_GENERAL, 'NewOption', NewOption);
  end;
end;
```

### Step 5: Add UI Control

In `uOptions.pas`, add the control and connect it:

```pascal
// Declaration in class
chkNewOption: TCheckBox;

// In LoadSettings
procedure TfrmOptions.LoadSettings;
begin
  // ...
  chkNewOption.Checked := Settings.General.NewOption;
end;

// In SaveSettings
procedure TfrmOptions.SaveSettings;
begin
  // ...
  Settings.General.NewOption := chkNewOption.Checked;
end;

// In ApplySettings (if applicable)
procedure TfrmOptions.ApplySettings;
begin
  // ...
  if Config.Settings.General.NewOption then
    frmMain.MPVEngine.EnableFeature
  else
    frmMain.MPVEngine.DisableFeature;
end;
```

### Step 6: Add Translation

In language files (`en.lang`, `fr.lang`):

```ini
[Options]
NewOption=Enable new feature
```

In `ApplyLocale`:

```pascal
chkNewOption.Caption := _T('Options', 'NewOption', 'Enable new feature');
```

---

## ApplySettings Procedure - Complete Code

The `ApplySettings` procedure applies saved settings to `MPVEngine`:

```pascal
procedure TfrmOptions.ApplySettings;
begin
  if (frmMain <> nil) and (frmMain.MPVEngine <> nil) then
  begin
    { Screenshot settings }
    if not DirectoryExists(Config.Settings.General.ScreenshotPath) then
      ForceDirectories(Config.Settings.General.ScreenshotPath);
    frmMain.MPVEngine.SetScreenshotDirectory(Config.Settings.General.ScreenshotPath);
    frmMain.MPVEngine.SetScreenshotFormat(Config.Settings.General.ScreenshotFormat);

    { Cache settings }
    frmMain.MPVEngine.SetCacheSize(Config.Settings.Cache.DefaultSize);

    { Audio settings }
    frmMain.MPVEngine.SetAudioOutput(Config.Settings.Audio.AudioOutput);
    frmMain.MPVEngine.SetAudioDevice(Config.Settings.Audio.AudioDevice);
    frmMain.MPVEngine.SetAudioNormalize(Config.Settings.Audio.Normalize);
    frmMain.MPVEngine.SetAudioChannels(Config.Settings.Audio.Channels);
    frmMain.MPVEngine.ReloadCurrentFile;  // Required for audio changes

    { Subtitle settings }
    if not Config.Settings.Subtitles.UseDefault then
    begin
      frmMain.MPVEngine.SetSubFont(Config.Settings.Subtitles.FontName);
      frmMain.MPVEngine.SetSubFontSize(Config.Settings.Subtitles.FontSize);
      frmMain.MPVEngine.SetSubFontColor(Config.Settings.Subtitles.FontColor);
      frmMain.MPVEngine.SetSubBold(Config.Settings.Subtitles.FontBold);
      frmMain.MPVEngine.SetSubItalic(Config.Settings.Subtitles.FontItalic);
      frmMain.MPVEngine.SetSubOutlineColor(Config.Settings.Subtitles.OutlineColor);
      frmMain.MPVEngine.SetSubOutlineSize(Config.Settings.Subtitles.OutlineSize);
      frmMain.MPVEngine.SetSubPosition(Config.Settings.Subtitles.Position);
      frmMain.MPVEngine.SetSubEncoding(Config.Settings.Subtitles.Encoding);
      frmMain.MPVEngine.SetSubAutoLoad(Config.Settings.Subtitles.AutoLoad);
    end
    else
    begin
      { Reset to mpv default subtitle settings }
      frmMain.MPVEngine.SetSubFont('');
      frmMain.MPVEngine.SetSubFontSize(55);
      // ...
    end;
  end;
end;
```

---

## Best Practices

### 1. Always Check MPVEngine

```pascal
// GOOD
if (frmMain <> nil) and (frmMain.MPVEngine <> nil) then
  frmMain.MPVEngine.SetVolume(100);

// BAD - may cause error
frmMain.MPVEngine.SetVolume(100);
```

### 2. Mark as Modified

```pascal
procedure TfrmOptions.tbBrightnessChange(Sender: TObject);
begin
  lblBrightnessValue.Caption := IntToStr(tbBrightness.Position);
  FModified := True;  // Important to track modifications
end;
```

### 3. Use Sensible Default Values

```pascal
// In LoadSettings
Idx := cmbVideoOutput.Items.IndexOf(Settings.Video.VideoOutput);
if Idx >= 0 then
  cmbVideoOutput.ItemIndex := Idx
else
  cmbVideoOutput.ItemIndex := 0;  // Fallback to 'auto'
```

### 4. Validate Paths

```pascal
// In ApplySettings
if not DirectoryExists(Config.Settings.General.ScreenshotPath) then
  ForceDirectories(Config.Settings.General.ScreenshotPath);
```

---

## Debugging

### Check Loaded Settings

```pascal
WriteLn('Language: ', Config.Settings.General.Language);
WriteLn('Volume: ', Config.Settings.Audio.Volume);
WriteLn('Config path: ', Config.ConfigPath);
```

### Check for Pending Changes

```pascal
if Config.Modified then
  WriteLn('Config has unsaved changes');
```

### Force Save

```pascal
Config.Modified := True;
Config.Save;
```

---

## Summary: Standard Pattern for a New Option

```pascal
// 1. uTypes.pas - Add field
TXxxSettings = record
  NewOption: OptionType;
end;

// 2. uConfig.pas - SetDefaults
NewOption := DefaultValue;

// 3. uConfig.pas - LoadXxxSection
NewOption := FIniFile.ReadXxx(Section, 'NewOption', Default);

// 4. uConfig.pas - SaveXxxSection
FIniFile.WriteXxx(Section, 'NewOption', NewOption);

// 5. uOptions.pas - LoadSettings
ctrlNewOption.Value := Settings.Xxx.NewOption;

// 6. uOptions.pas - SaveSettings
Settings.Xxx.NewOption := ctrlNewOption.Value;

// 7. uOptions.pas - ApplySettings (if immediate effect required)
frmMain.MPVEngine.SetNewOption(Config.Settings.Xxx.NewOption);

// 8. .lang files - Translation
[Options]
NewOption=Translated description
```
