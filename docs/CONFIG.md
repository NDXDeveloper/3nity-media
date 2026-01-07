# Configuration Guide - 3nity Media

This guide covers all configuration options for 3nity Media, including settings accessible through the GUI and manual INI file editing.

## Table of Contents

- [Configuration Files](#configuration-files)
- [General Settings](#general-settings)
- [Video Settings](#video-settings)
- [Audio Settings](#audio-settings)
- [Subtitle Settings](#subtitle-settings)
- [Cache Settings](#cache-settings)
- [Equalizer Settings](#equalizer-settings)
- [Playback Mode](#playback-mode)
- [History & Recent Files](#history--recent-files)
- [Bookmarks](#bookmarks)
- [Favorites](#favorites)
- [Session Restore](#session-restore)
- [INI File Structure](#ini-file-structure)
- [Advanced Configuration](#advanced-configuration)

---

## Configuration Files

### File Locations

| Platform | Configuration Directory |
|----------|------------------------|
| Linux | `~/.config/3nity-media/` |
| Windows | `%APPDATA%\3nity-media\` |
| Snap | `~/snap/3nity-media/current/.config/` |

### Configuration Files

| File | Purpose |
|------|---------|
| `config.ini` | Main configuration (all settings sections) |
| `history.ini` | Playback history |
| `bookmarks.ini` | Media bookmarks |
| `favorites.ini` | Favorites list |
| `shortcuts.ini` | Custom keyboard shortcuts |
| `session_playlist.ini` | Session restore data |

---

## General Settings

Accessible via **Options → General** or in `[General]` section.

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `Language` | string | `en` | Interface language code (en, fr, etc.) |
| `SingleInstance` | boolean | `true` | Only allow one instance of the application |
| `ScreenshotPath` | string | `~/Pictures/3nity/` | Directory for screenshot saves |
| `ScreenshotFormat` | string | `png` | Screenshot format: `png`, `jpg`, or `webp` |
| `HistoryEnabled` | boolean | `true` | Enable playback history tracking |
| `HistoryMaxItems` | integer | `500` | Maximum history entries to keep |
| `AutoSavePlaylist` | boolean | `true` | Auto-save playlist on exit for session restore |

### Language Codes

| Code | Language |
|------|----------|
| `en` | English |
| `fr` | French |

---

## Video Settings

Accessible via **Options → Video** or in `[Video]` section.

### Video Properties

| Setting | Type | Range | Default | Description |
|---------|------|-------|---------|-------------|
| `Brightness` | integer | -100 to 100 | `0` | Video brightness adjustment |
| `Contrast` | integer | -100 to 100 | `0` | Video contrast adjustment |
| `Saturation` | integer | -100 to 100 | `0` | Color saturation adjustment |
| `Hue` | integer | -100 to 100 | `0` | Color hue rotation |
| `Gamma` | integer | -100 to 100 | `0` | Gamma correction |

### Aspect Ratio

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `AspectMode` | integer | `0` | Aspect ratio preset (see table below) |
| `AspectFactor` | double | `-1` | Custom aspect ratio value |

**Aspect Mode Values:**

| Value | Aspect Ratio |
|-------|--------------|
| 0 | Auto (original) |
| 1 | 16:9 |
| 2 | 4:3 |
| 3 | 2.35:1 (Cinemascope) |
| 4 | 1.85:1 |
| 5 | Custom (uses AspectFactor) |

### Deinterlacing

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `Deinterlace` | integer | `2` | Deinterlace mode |
| `DeinterlaceAlg` | integer | `0` | Deinterlace algorithm |

**Deinterlace Mode Values:**

| Value | Mode |
|-------|------|
| 0 | Off |
| 1 | On (always) |
| 2 | Auto (detect interlaced content) |

**Deinterlace Algorithm Values:**

| Value | Algorithm |
|-------|-----------|
| 0 | Auto |
| 1 | Yadif |
| 2 | Bwdif |
| 3 | Weave |

### Video Output

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `VideoOutput` | string | `auto` | Video output driver |
| `HWAccel` | boolean | `true` | Enable hardware acceleration |

**Video Output Values:**

| Platform | Options |
|----------|---------|
| Linux | `auto`, `gpu`, `gpu-next`, `x11`, `wayland`, `xv` |
| Windows | `auto`, `gpu`, `gpu-next`, `d3d11`, `opengl` |

---

## Audio Settings

Accessible via **Options → Audio** or in `[Audio]` section.

| Setting | Type | Range | Default | Description |
|---------|------|-------|---------|-------------|
| `Volume` | integer | 0-150 | `100` | Volume level (>100 uses software boost) |
| `Muted` | boolean | - | `false` | Mute state |
| `AudioOutput` | string | - | `auto` | Audio output driver |
| `AudioDevice` | string | - | `` | Specific audio device (empty = default) |
| `Channels` | integer | 1-8 | `2` | Audio channel count |
| `Normalize` | boolean | - | `false` | Enable audio normalization |

**Audio Output Values:**

| Platform | Options |
|----------|---------|
| Linux | `auto`, `pulse`, `pipewire`, `alsa` |
| Windows | `auto`, `wasapi`, `dsound` |

---

## Subtitle Settings

Accessible via **Options → Subtitles** or in `[Subtitles]` section.

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `UseDefault` | boolean | `true` | Use mpv default subtitle styling |
| `FontName` | string | `Arial` | Subtitle font family |
| `FontSize` | integer | `24` | Font size in points |
| `FontColor` | integer | `16777215` | Font color (TColor value, white) |
| `FontBold` | boolean | `false` | Bold text |
| `FontItalic` | boolean | `false` | Italic text |
| `OutlineColor` | integer | `0` | Outline color (TColor value, black) |
| `OutlineSize` | integer | `2` | Outline thickness in pixels |
| `BackgroundColor` | integer | `0` | Background box color |
| `BackgroundOpacity` | integer | `0` | Background opacity (0-255) |
| `Position` | integer | `95` | Vertical position (0-100, bottom to top) |
| `Encoding` | string | `UTF-8` | Character encoding |
| `AutoLoad` | boolean | `true` | Auto-load matching subtitle files |

### Color Values

Colors are stored as TColor integers. Common values:

| Color | Value |
|-------|-------|
| White | `16777215` ($FFFFFF) |
| Black | `0` ($000000) |
| Yellow | `65535` ($00FFFF) |
| Red | `255` ($0000FF) |
| Green | `32768` ($008000) |

---

## Cache Settings

Accessible via **Options → Cache** or in `[Cache]` section.

Cache sizes are in **kilobytes**.

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `DefaultSize` | integer | `4096` | Default cache (4 MB) |
| `FixedSize` | integer | `2048` | Fixed/local disk (2 MB) |
| `RamdiskSize` | integer | `512` | RAM disk (512 KB) |
| `CDROMSize` | integer | `4096` | CD-ROM drives (4 MB) |
| `RemovableSize` | integer | `2048` | USB/removable media (2 MB) |
| `NetworkSize` | integer | `8192` | Network shares/NFS/SMB (8 MB) |
| `InternetSize` | integer | `16384` | HTTP/HTTPS streaming (16 MB) |
| `DVDSize` | integer | `8192` | DVD/Blu-ray (8 MB) |

### Cache Type Detection

The application automatically detects the source type:

| Pattern | Cache Type |
|---------|------------|
| `http://`, `https://` | Internet |
| `smb://`, `nfs://` | Network |
| `dvd://`, `dvdnav://`, `bd://` | DVD |
| `cdda://` | CDROM |
| USB drives | Removable |
| Local files | Fixed |

---

## Equalizer Settings

Stored in `[Equalizer]` section.

| Setting | Type | Range | Default | Description |
|---------|------|-------|---------|-------------|
| `Enabled` | boolean | - | `false` | Enable equalizer |
| `Band0` to `Band9` | double | -12 to +12 | `0` | Band gain in dB |

### Equalizer Bands

| Band | Frequency | Label |
|------|-----------|-------|
| 0 | 31 Hz | 31 |
| 1 | 62 Hz | 62 |
| 2 | 125 Hz | 125 |
| 3 | 250 Hz | 250 |
| 4 | 500 Hz | 500 |
| 5 | 1000 Hz | 1K |
| 6 | 2000 Hz | 2K |
| 7 | 4000 Hz | 4K |
| 8 | 8000 Hz | 8K |
| 9 | 16000 Hz | 16K |

---

## Playback Mode

Stored in `[Playlist]` section.

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `PlaybackMode` | integer | `0` | Current playback mode |

**Playback Mode Values:**

| Value | Mode | Description |
|-------|------|-------------|
| 0 | Normal | Play through playlist once |
| 1 | Repeat One | Repeat current track |
| 2 | Repeat All | Loop entire playlist |
| 3 | Shuffle | Random order, stop at end |
| 4 | Shuffle Repeat | Random order, loop forever |

---

## History & Recent Files

### History

Stored in `history.ini` file.

Each history entry contains:
- `FileName`: Full path to media file
- `Title`: Media title
- `Position`: Last playback position (seconds)
- `Duration`: Total duration (seconds)
- `LastPlayed`: DateTime of last playback
- `PlayCount`: Number of times played

### Recent Files

Stored in `[RecentFiles]` section of main config.

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `Count` | integer | - | Number of recent files |
| `File0` to `FileN` | string | - | Recent file paths |

Maximum: 20 files (oldest removed automatically)

---

## Bookmarks

Stored in `bookmarks.ini` file.

Each bookmark contains:
- `FileName`: Full path to media file
- `Name`: User-defined bookmark name
- `Position`: Bookmark position (seconds)
- `CreatedAt`: DateTime when created
- `Thumbnail`: Optional thumbnail path

### Bookmark Operations

- **Add**: Ctrl+B at current position
- **Go to**: Select from Bookmarks menu
- **Remove**: Right-click bookmark in menu

---

## Favorites

Stored in `favorites.ini` file.

Each favorite contains:
- `Name`: Display name
- `Path`: File path or URL
- `Type`: Favorite type (see below)
- `Category`: Optional category name
- `AddedAt`: DateTime when added
- `LastPlayed`: DateTime of last playback
- `PlayCount`: Number of times played

**Favorite Types:**

| Value | Type |
|-------|------|
| 0 | Local file |
| 1 | URL/Stream |
| 2 | Radio station |
| 3 | DVD |
| 4 | Blu-ray |

---

## Session Restore

Stored in `session_playlist.ini` file.

When `AutoSavePlaylist` is enabled, the application saves:
- Current playlist items
- Current playing index
- Current playback position
- Playback mode

On next launch, the session is automatically restored.

### Playback Positions

Individual file positions are stored in `[PlaybackPositions]` section of main config using CRC32 hash of filename as key.

- Minimum position: 5 seconds (shorter sessions not saved)
- Used for "Resume playback" feature

---

## INI File Structure

### Example `config.ini`

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
Deinterlace=2
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
DefaultSize=4096
FixedSize=2048
RamdiskSize=512
CDROMSize=4096
RemovableSize=2048
NetworkSize=8192
InternetSize=16384
DVDSize=8192

[Playlist]
PlaybackMode=0

[Equalizer]
Enabled=0
Band0=0
Band1=0
Band2=0
Band3=0
Band4=0
Band5=0
Band6=0
Band7=0
Band8=0
Band9=0

[MainWindow]
Left=100
Top=100
Width=800
Height=600
Maximized=0

[RecentFiles]
Count=3
File0=/path/to/video1.mp4
File1=/path/to/video2.mkv
File2=/path/to/music.mp3
```

---

## Advanced Configuration

### Environment Variables

| Variable | Purpose |
|----------|---------|
| `HOME` (Linux) | Used to locate config directory |
| `APPDATA` (Windows) | Used to locate config directory |
| `XDG_PICTURES_DIR` | Used for default screenshot path |

### Command Line Override

Some settings can be overridden via command line:

```bash
# Start with specific config directory
3nity --config-dir=/path/to/config

# Start with specific file
3nity /path/to/video.mp4
```

### Resetting Configuration

To reset all settings to defaults:

```bash
# Linux
rm -rf ~/.config/3nity-media/

# Windows (PowerShell)
Remove-Item -Recurse "$env:APPDATA\3nity-media"
```

The application will recreate default configuration on next launch.

---

## See Also

- [User Guide](USER_GUIDE.md)
- [Keyboard Shortcuts](SHORTCUTS.md)
- [API Reference](API_REFERENCE.md)
- [CLI Parameters](CLI_PARAMETERS.md)

---

## Version Information

- **Last Updated:** 2026-01-02
- **Applies to:** 3nity Media v0.x and later
