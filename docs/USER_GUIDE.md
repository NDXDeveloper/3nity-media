# 3nity Media - User Guide

A complete guide to using 3nity Media, a lightweight cross-platform multimedia player.

---

## Table of Contents

1. [Getting Started](#getting-started)
   - [Installation](#installation)
   - [First Launch](#first-launch)
   - [Interface Overview](#interface-overview)
2. [Playing Media](#playing-media)
   - [Opening Files](#opening-files)
   - [Opening DVD and Blu-ray](#opening-dvd-and-blu-ray)
   - [Opening URLs and Streams](#opening-urls-and-streams)
   - [Drag and Drop](#drag-and-drop)
   - [Playback Controls](#playback-controls)
3. [Playlist](#playlist)
   - [Adding Items](#adding-items)
   - [Managing the Playlist](#managing-the-playlist)
   - [Saving and Loading Playlists](#saving-and-loading-playlists)
   - [Shuffle and Repeat](#shuffle-and-repeat)
4. [Internet Radio](#internet-radio)
   - [Browsing Stations](#browsing-stations)
   - [Searching and Filtering](#searching-and-filtering)
   - [Custom Stations](#custom-stations)
   - [Recording Streams](#recording-streams)
5. [Audio Features](#audio-features)
   - [Volume Control](#volume-control)
   - [Equalizer](#equalizer)
   - [Audio Delay](#audio-delay)
   - [Audio Visualizations](#audio-visualizations)
6. [Video Features](#video-features)
   - [Fullscreen Mode](#fullscreen-mode)
   - [Video Adjustments](#video-adjustments)
   - [Zoom and Rotation](#zoom-and-rotation)
   - [Screenshots](#screenshots)
   - [Subtitles](#subtitles)
7. [Advanced Playback](#advanced-playback)
   - [Seeking](#seeking)
   - [Playback Speed](#playback-speed)
   - [A-B Loop](#a-b-loop)
   - [Frame-by-Frame](#frame-by-frame)
   - [Chapters](#chapters)
   - [DVD and Blu-ray Navigation](#dvd-and-blu-ray-navigation)
8. [Favorites and Bookmarks](#favorites-and-bookmarks)
   - [Managing Favorites](#managing-favorites)
   - [Creating Bookmarks](#creating-bookmarks)
9. [History](#history)
10. [Settings](#settings)
    - [General Options](#general-options)
    - [Keyboard Shortcuts](#keyboard-shortcuts)
    - [Language](#language)
11. [Keyboard Shortcuts Reference](#keyboard-shortcuts-reference)
12. [Command Line Usage](#command-line-usage)
13. [Troubleshooting](#troubleshooting)

---

## Getting Started

### Installation

#### Linux

**From DEB Package (Ubuntu/Debian):**
```bash
sudo apt install ./3nity-media_amd64.deb
```

**From AppImage:**
```bash
chmod +x 3nity-Media-x86_64.AppImage
./3nity-Media-x86_64.AppImage
```

**Dependencies:**
```bash
sudo apt install libmpv2 libqt5pas1
```

#### Windows

- **Installer:** Run `3nity-Media-Setup.exe`
- **Portable:** Extract `3nity-media-windows-portable.zip` and run `3nity.exe`

### First Launch

When you first launch 3nity Media, you'll see the main player window. The interface is designed to be clean and unobtrusive, putting your media front and center.

### Interface Overview

```
┌─────────────────────────────────────────────────────────┐
│  Menu Bar                                               │
├─────────────────────────────────────────────────────────┤
│                                                         │
│                                                         │
│                    Video Area                           │
│              (or Audio Visualization)                   │
│                                                         │
│                                                         │
├─────────────────────────────────────────────────────────┤
│   advancement bar                 │ Duration            │
├─────────────────────────────────────────────────────────┤
│  ◀◀  ▶/❚❚  ▶▶  │  🔊 Volume  │  Track Info           │
└─────────────────────────────────────────────────────────┘
```

**Main Areas:**
- **Menu Bar:** Access all features and settings
- **Video Area:** Displays video content or audio visualizations
- **Progress Bar:** Shows playback position (click to seek)
- **Control Bar:** Playback controls, volume, and track information

---

## Playing Media

### Opening Files

**Method 1: Menu**
1. Go to `File` → `Open File` (or press `Ctrl+O`)
2. Select one or more files
3. Click `Open`

**Method 2: Double-click**
- Double-click any supported media file to open it with 3nity Media

**Supported Formats:**
- **Video:** MP4, MKV, AVI, MOV, WMV, FLV, WebM, and more
- **Audio:** MP3, FLAC, OGG, WAV, AAC, M4A, WMA, and more
- **Playlists:** M3U, M3U8, PLS, XSPF
- **Disc formats:** DVD (VIDEO_TS/VOB), Blu-ray (BDMV/M2TS)

### Opening DVD and Blu-ray

3nity Media supports playing DVD and Blu-ray content from folders (ripped discs or mounted ISO images).

**Opening a DVD:**
1. Go to `File` → `Open DVD`
2. Select the DVD folder (the one containing the `VIDEO_TS` subfolder)
3. The main movie (largest title) will play automatically

**Opening a Blu-ray:**
1. Go to `File` → `Open Blu-ray`
2. Select the Blu-ray folder (the one containing the `BDMV` subfolder)
3. The main movie (largest .m2ts file) will play automatically

**Fallback Mode:**
If native DVD/Blu-ray protocols (dvdnav://, bluray://) are unavailable on your system, 3nity Media automatically uses fallback mode:
- **DVD:** All VOB files from the main title are added to the playlist and played sequentially
- **Blu-ray:** The largest .m2ts file from the STREAM folder is played directly

This ensures playback works even without libbluray or libdvdnav installed.

### Opening URLs and Streams

1. Go to `File` → `Open URL` (or press `Ctrl+U`)
2. Enter the URL (e.g., `https://example.com/stream.mp3`)
3. Click `OK`

**Supported URL types:**
- HTTP/HTTPS streams
- YouTube (if youtube-dl/yt-dlp is installed)
- Internet radio streams
- Direct media links

### Drag and Drop

Drag files or folders onto the player:

- **Drop on video area:** Files are added to playlist and playback starts immediately
- **Drop elsewhere on main window:** Files are added to playlist without playing
- **Drop on playlist window:** Files are added to playlist without playing
- **Folders:** All media files are scanned recursively (up to 10 levels deep)

### Playback Controls

| Control | Action |
|---------|--------|
| `Space` or Play button | Play / Pause |
| Stop button | Stop playback |
| Previous button | Previous track |
| Next button | Next track |
| Progress bar | Click to seek |

---

## Playlist

### Adding Items

**From Menu:**
- `Playlist` → `Add Files` - Add specific files
- `Playlist` → `Add Folder` - Add all files from a folder

**From Drag and Drop:**
- Drag files or folders onto the playlist window (adds without playing)

**From File Browser:**
- Right-click a file and select "Add to Playlist"

### Managing the Playlist

Open the playlist panel with `View` → `Playlist` or press the playlist button.

**Available Actions:**
- **Play:** Double-click an item or select and press Enter
- **Remove:** Select item(s) and press Delete
- **Move:** Drag items to reorder
- **Select All:** `Ctrl+A`
- **Clear:** Remove all items from the playlist

**Right-click Menu:**
- Play
- Remove from playlist
- Remove duplicates
- Show in file manager
- Media information

### Saving and Loading Playlists

**Save Playlist:**
1. Go to `Playlist` → `Save Playlist`
2. Choose format (M3U, M3U8, PLS, or XSPF)
3. Enter filename and save

**Load Playlist:**
1. Go to `Playlist` → `Open Playlist`
2. Select a playlist file
3. Items are added to the current playlist

### Shuffle and Repeat

Access from `Playback` menu or playlist toolbar:

| Mode | Description |
|------|-------------|
| **Repeat Off** | Stop after last track |
| **Repeat All** | Loop entire playlist |
| **Repeat One** | Loop current track |
| **Shuffle** | Random playback order |

---

## Internet Radio

### Browsing Stations

1. Go to `View` → `Internet Radio` (or `Tools` → `Radio`)
2. Wait for the station list to load from Icecast directory
3. Browse or search for stations

### Searching and Filtering

**Search:**
- Type in the search box to filter stations by name, genre, or country

**Filter by Genre:**
- Use the genre dropdown to show only stations of a specific genre

### Custom Stations

**Add Custom Station:**
1. Go to `Radio` → `Add Custom Station`
2. Enter station name and URL
3. Click `OK`

Custom stations appear in a separate section and are saved locally.

### Recording Streams

While playing an internet radio stream:
1. Click the Record button or go to `Tools` → `Record Stream`
2. Recording saves to your configured output folder
3. Click Stop Recording when done

Recordings are saved as MP3 or the stream's native format.

---

## Audio Features

### Volume Control

**Adjust Volume:**
- Use the volume slider in the control bar
- Press `+` or `Up Arrow` to increase
- Press `-` or `Down Arrow` to decrease
- Press `M` to mute/unmute

**Volume Range:** 0% to 100% (can be boosted higher in settings)

### Equalizer

1. Go to `Tools` → `Equalizer` (or press `E`)
2. Adjust the 10 frequency bands:
   - 31 Hz, 62 Hz, 125 Hz, 250 Hz, 500 Hz
   - 1 kHz, 2 kHz, 4 kHz, 8 kHz, 16 kHz

**Presets:**
- Flat, Rock, Pop, Jazz, Classical, Bass Boost, Treble Boost, and more

**Custom Presets:**
- Adjust sliders to your preference
- Click "Save Preset" to save your settings

### Audio Delay

Useful for fixing audio/video sync issues:
- Press `Ctrl+Plus` to increase audio delay
- Press `Ctrl+Minus` to decrease audio delay
- Press `Ctrl+0` to reset delay

### Audio Visualizations

For audio files, 3nity Media displays visualizations:

1. Go to `View` → `Visualizations`
2. Choose a visualization mode:
   - **Spectrum:** Frequency spectrum analyzer
   - **Waveform:** Audio waveform display
   - **Combined:** Spectrum and waveform together

**Change Colors:**
- Press `V` to cycle through color schemes
- Or go to `View` → `Visualization Colors`

---

## Video Features

### Fullscreen Mode

- Press `F` or `F11` to toggle fullscreen
- Double-click the video area
- Go to `View` → `Fullscreen`

**Exit Fullscreen:**
- Press `F`, `F11`, or `Escape`
- Double-click the video area

### Video Adjustments

1. Go to `Tools` → `Video Adjustments`
2. Adjust the following:
   - **Brightness:** -100 to +100
   - **Contrast:** -100 to +100
   - **Saturation:** -100 to +100
   - **Gamma:** 0.1 to 10.0
   - **Hue:** -180 to +180

Click "Reset" to restore default values.

### Zoom and Rotation

**Zoom:**
- Press `Ctrl+Plus` to zoom in
- Press `Ctrl+Minus` to zoom out
- Press `Ctrl+0` to reset zoom

**Rotation:**
- Press `R` to rotate video 90° clockwise
- Or go to `Video` → `Rotate`

**Fit to Video:**
- Press `Ctrl+F` to resize window to match video dimensions

### Screenshots

- Press `S` or go to `Video` → `Screenshot`
- Screenshots are saved to your Pictures folder (configurable in settings)

### Subtitles

**Load Subtitles:**
- Subtitles are loaded automatically if they have the same name as the video
- Go to `Subtitles` → `Load Subtitle File` to load manually

**Subtitle Delay:**
- Press `Z` to decrease delay (subtitles appear earlier)
- Press `X` to increase delay (subtitles appear later)
- Press `Shift+Z` to reset delay

**Subtitle Track:**
- Go to `Subtitles` → Select track (for embedded subtitles)

---

## Advanced Playback

### Seeking

| Action | Shortcut |
|--------|----------|
| Seek forward 10 seconds | `Right Arrow` |
| Seek backward 10 seconds | `Left Arrow` |
| Seek forward 1 minute | `Shift+Right` |
| Seek backward 1 minute | `Shift+Left` |
| Go to specific time | `Ctrl+G` |

### Playback Speed

| Action | Shortcut |
|--------|----------|
| Speed up (10%) | `]` |
| Slow down (10%) | `[` |
| Reset to normal speed | `Backspace` |

Speed range: 0.25x to 4.0x

### A-B Loop

Repeat a section of the media:

1. Press `L` at the start point (sets point A)
2. Press `L` again at the end point (sets point B)
3. The section between A and B will loop
4. Press `L` a third time to clear the loop

### Frame-by-Frame

For precise video navigation:
- Press `.` (period) to advance one frame
- Press `,` (comma) to go back one frame

Note: Playback must be paused for frame stepping.

### Chapters

For media with chapters (DVDs, MKV files):
- Press `Page Up` for previous chapter
- Press `Page Down` for next chapter
- Go to `Playback` → `Chapters` to see chapter list

### DVD and Blu-ray Navigation

**Menu Navigation (when using native protocols):**

| Action | Shortcut |
|--------|----------|
| DVD/Blu-ray Menu | `Ctrl+M` |
| Navigate Up | `Arrow Up` |
| Navigate Down | `Arrow Down` |
| Navigate Left | `Arrow Left` |
| Navigate Right | `Arrow Right` |
| Select | `Enter` |

**Note:** Menu navigation is only available when playing via native protocols (dvdnav://, bluray://). In fallback mode, the content plays directly without menu support.

**Fallback Mode Playback:**
- For DVDs, all VOB files of the main title are queued in the playlist
- Use `N` (Next) and `P` (Previous) to switch between VOB segments
- Seeking works normally within each segment

---

## Favorites and Bookmarks

### Managing Favorites

**Add to Favorites:**
1. While playing a file or radio station
2. Go to `Favorites` → `Add to Favorites`
3. Optionally organize into categories

**Access Favorites:**
- Go to `Favorites` menu to see your saved items
- Click any favorite to start playing

**Remove Favorite:**
- Right-click a favorite and select "Remove"

### Creating Bookmarks

Bookmarks save your position in a file:

**Add Bookmark:**
1. While playing, go to `Playback` → `Add Bookmark`
2. Enter a name for the bookmark
3. The current position is saved

**Go to Bookmark:**
- Go to `Playback` → `Bookmarks` → Select bookmark

**Automatic Position Saving:**
- 3nity Media remembers where you stopped in each file
- Resume playback automatically when you reopen the file

---

## History

3nity Media keeps track of recently played files:

1. Go to `File` → `History` (or `View` → `History`)
2. See your recent playback history
3. Click any item to play it again

**Clear History:**
- Go to `File` → `History` → `Clear History`

---

## Settings

### General Options

Go to `Tools` → `Options` to access settings:

**Playback:**
- Resume playback position
- Auto-play on startup
- Default volume level

**Interface:**
- Show/hide elements
- System tray behavior

**Paths:**
- Screenshot folder
- Recording folder

### Keyboard Shortcuts

1. Go to `Tools` → `Keyboard Shortcuts`
2. Click on any action to customize its shortcut
3. Press the new key combination
4. Click "Apply" to save

**Reset to Defaults:**
- Click "Reset All" to restore default shortcuts

### Language

1. Go to `Tools` → `Options` → `Language`
2. Select your preferred language from the list
3. Click "Apply"

3nity Media supports 99 languages.

---

## Keyboard Shortcuts Reference

### Playback

| Action | Default Shortcut |
|--------|------------------|
| Play / Pause | `Space` |
| Stop | `S` |
| Previous track | `P` |
| Next track | `N` |
| Seek forward 10s | `Right` |
| Seek backward 10s | `Left` |
| Seek forward 1min | `Shift+Right` |
| Seek backward 1min | `Shift+Left` |
| Go to time | `Ctrl+G` |

### Speed

| Action | Default Shortcut |
|--------|------------------|
| Speed up | `]` |
| Slow down | `[` |
| Reset speed | `Backspace` |

### Volume

| Action | Default Shortcut |
|--------|------------------|
| Volume up | `+` or `Up` |
| Volume down | `-` or `Down` |
| Mute | `M` |

### Video

| Action | Default Shortcut |
|--------|------------------|
| Fullscreen | `F` or `F11` |
| Rotate | `R` |
| Screenshot | `S` |
| Zoom in | `Ctrl+Plus` |
| Zoom out | `Ctrl+Minus` |
| Reset zoom | `Ctrl+0` |
| Fit to video | `Ctrl+F` |

### Subtitles

| Action | Default Shortcut |
|--------|------------------|
| Delay + | `X` |
| Delay - | `Z` |
| Reset delay | `Shift+Z` |

### Advanced

| Action | Default Shortcut |
|--------|------------------|
| Set Loop A | `L` |
| Set Loop B | `L` |
| Clear Loop | `L` |
| Frame forward | `.` |
| Frame backward | `,` |
| Previous chapter | `Page Up` |
| Next chapter | `Page Down` |

### Application

| Action | Default Shortcut |
|--------|------------------|
| Open file | `Ctrl+O` |
| Open URL | `Ctrl+U` |
| Show playlist | `Ctrl+L` |
| Quit | `Ctrl+Q` or `Alt+F4` |

---

## Command Line Usage

3nity Media can be controlled from the command line:

```bash
# Play a file
3nity /path/to/file.mp4

# Play a URL
3nity https://example.com/stream.mp3

# Play a folder (adds all media files)
3nity /path/to/music/folder

# Add files to existing instance (enqueue)
3nity --enqueue ~/Music/*.mp3

# Play in fullscreen with options
3nity -f --volume=50 --start=2:00 movie.mkv

# Play with external subtitles
3nity --sub=movie.srt movie.mkv

# Show help
3nity --help
```

For detailed CLI documentation, see [CLI Parameters](CLI_PARAMETERS.md).

---

## Troubleshooting

### No Sound

1. Check system volume
2. Check 3nity Media volume (not muted)
3. Verify audio output device in system settings
4. Try a different audio file to isolate the issue

### Video Not Playing

1. Ensure libmpv is installed correctly
2. Check if the video format is supported
3. Try updating graphics drivers
4. Check `View` → `Logs` for error messages

### Subtitles Not Showing

1. Ensure subtitle file is in the same folder as the video
2. Check subtitle encoding (try UTF-8)
3. Go to `Subtitles` → `Load Subtitle File` to load manually
4. Verify subtitle track is selected in `Subtitles` menu

### High CPU Usage

1. Disable hardware acceleration if problematic
2. Reduce visualization complexity
3. Close other resource-intensive applications

### Application Won't Start

1. Check if dependencies are installed:
   ```bash
   sudo apt install libmpv2 libqt5pas1
   ```
2. Run from terminal to see error messages:
   ```bash
   3nity-media
   ```
3. Check for conflicting instances

### Internet Radio Not Loading

1. Check your internet connection
2. The Icecast directory may be temporarily unavailable
3. Try adding stations manually via custom stations

---

## Getting Help

- **Bug Reports:** [GitHub Issues](https://github.com/NDXDeveloper/3nity-media/issues)
- **Documentation:** [Project Docs](https://github.com/NDXDeveloper/3nity-media/tree/main/docs)

---

*3nity Media - Simple, powerful, yours.*

## Version Information

- **Last updated:** 2026-01-01
- **Applies to:** 3nity Media v0.x and later
