# Command Line Interface (CLI) Parameters

This document describes all command-line parameters supported by 3nity Media.

## Synopsis

```
3nity [OPTIONS] [FILE|URL|FOLDER]
```

## Quick Reference

```
OPTIONS:
  -h, --help          Show help message
  -v, --version       Show version information
      --license       Show GPL-2.0 license

  -f, --fullscreen    Start in fullscreen mode
      --volume=N      Set initial volume (0-100)
      --mute          Start muted
      --loop          Loop playback
      --speed=N       Playback speed (0.25-4.0)

      --start=TIME    Start position (e.g., 1:30:00)
      --sub=FILE      Load subtitle file
      --enqueue       Add to existing instance playlist
```

## Parameters by Category

### 1. Information

| Parameter | Short | Description |
|-----------|-------|-------------|
| `--help` | `-h` | Display help message with all available parameters |
| `--version` | `-v` | Display application version and build information |
| `--license` | | Display the GPL-2.0 license text |

**Examples:**
```bash
3nity --help
3nity -h
3nity --version
3nity -v
3nity --license
```

### 2. Playback Control

| Parameter | Description | Default |
|-----------|-------------|---------|
| `--fullscreen`, `-f` | Start playback in fullscreen mode | Windowed |
| `--volume=N` | Set initial volume level (0-100) | Last used |
| `--mute` | Start with audio muted | Unmuted |
| `--loop` | Loop the media file continuously | No loop |
| `--speed=N` | Set playback speed (0.25 to 4.0) | 1.0 |

**Examples:**
```bash
# Start video in fullscreen
3nity -f movie.mkv
3nity --fullscreen movie.mkv

# Set volume to 50%
3nity --volume=50 music.mp3

# Start muted (useful for late-night viewing)
3nity --mute video.mp4

# Loop a short video
3nity --loop intro.mp4

# Play at 1.5x speed
3nity --speed=1.5 podcast.mp3

# Combine multiple options
3nity -f --volume=30 --speed=1.25 lecture.mp4
```

### 3. Position

| Parameter | Description | Format |
|-----------|-------------|--------|
| `--start=TIME` | Start playback at specified position | `SS`, `MM:SS`, or `HH:MM:SS` |

**Time Format Examples:**
- `--start=90` - Start at 90 seconds
- `--start=1:30` - Start at 1 minute 30 seconds
- `--start=01:30:00` - Start at 1 hour 30 minutes

**Examples:**
```bash
# Skip intro (start at 2 minutes)
3nity --start=2:00 movie.mkv

# Resume from specific timestamp
3nity --start=01:23:45 movie.mkv

# Start at 30 seconds
3nity --start=30 clip.mp4
```

### 4. Subtitles

| Parameter | Description |
|-----------|-------------|
| `--sub=FILE` | Load an external subtitle file |

**Supported Formats:** SRT, ASS, SSA, SUB, VTT, SUP

**Examples:**
```bash
# Load subtitle file
3nity movie.mkv --sub=movie.srt

# Load subtitle with different name
3nity movie.mkv --sub=subtitles/english.srt

# Full path
3nity movie.mkv --sub=/home/user/subs/movie.fr.srt
```

### 5. Instance Control

| Parameter | Description |
|-----------|-------------|
| `--enqueue` | Add files to the playlist of an already running instance |

When `--enqueue` is used:
- If 3nity Media is already running: files are added to its playlist
- If no instance is running: a new instance starts with the files

**Examples:**
```bash
# Add a single file to running instance
3nity --enqueue newfile.mp3

# Add all MP3 files from a folder
3nity --enqueue ~/Music/*.mp3
```

## Complete Examples

### Basic Usage

```bash
# Play a video file
3nity video.mp4

# Play an audio file
3nity music.mp3

# Play from URL
3nity https://example.com/stream.m3u8

# Open a folder (adds all media files to playlist)
3nity ~/Videos/
```

### Movie Night

```bash
# Fullscreen with reduced volume, starting after intro
3nity -f --volume=40 --start=2:30 movie.mkv
```

### Music Playback

```bash
# Loop an album at 50% volume
3nity --volume=50 --loop album.m3u

# Add more tracks to current session
3nity --enqueue more_songs/*.mp3
```

### Language Learning

```bash
# Play video with external subtitles at slower speed
3nity --speed=0.75 --sub=lesson.srt lesson.mp4
```

### Podcast/Audiobook

```bash
# Resume podcast at 1.25x speed
3nity --speed=1.25 --start=45:30 podcast.mp3
```

### Background Music

```bash
# Start muted, unmute when ready
3nity --mute --loop playlist.m3u
```

## Input Types

3nity Media accepts various input types:

| Type | Example |
|------|---------|
| Local file | `3nity video.mp4` |
| Folder | `3nity ~/Videos/` |
| Playlist file | `3nity playlist.m3u` |
| URL/Stream | `3nity https://stream.example.com/live.m3u8` |
| Glob pattern | `3nity ~/Music/*.mp3` |

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Error (invalid parameter, file not found, etc.) |

## Notes

- Parameters can appear before or after the file path
- Long parameters use `=` for values: `--volume=50`
- Short parameters: `-f` (no value) or `-h`
- Multiple parameters can be combined
- File paths with spaces should be quoted: `"my video.mp4"`

## Environment Variables

| Variable | Description |
|----------|-------------|
| `XDG_CONFIG_HOME` | Config directory (Linux, default: `~/.config`) |
| `APPDATA` | Config directory (Windows) |

## See Also

- [User Guide](USER_GUIDE.md)
- [Keyboard Shortcuts](SHORTCUTS.md)
- [Configuration Guide](CONFIG.md)

## Version Information

- **Last updated:** 2026-01-01
- **Applies to:** 3nity Media v0.x and later
