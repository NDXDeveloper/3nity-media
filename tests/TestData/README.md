# Test Data

This directory contains sample media files and test data used by the test suite.

## Directory Structure

```
TestData/
├── audio/          # Audio test files
├── video/          # Video test files
├── playlists/      # Playlist test files
└── subtitles/      # Subtitle test files
```

## Contents

### Audio (`audio/`)

Sample audio files for testing playback:

| File | Format | Duration | Description |
|------|--------|----------|-------------|
| `sample.mp3` | MP3 | 5 sec | Basic MP3 test |
| `sample.flac` | FLAC | 5 sec | Lossless audio test |
| `sample.ogg` | OGG Vorbis | 5 sec | OGG format test |
| `sample.wav` | WAV | 5 sec | Uncompressed audio |
| `sample.m4a` | AAC | 5 sec | AAC format test |

### Video (`video/`)

Sample video files for testing video playback:

| File | Format | Duration | Resolution | Description |
|------|--------|----------|------------|-------------|
| `sample.mp4` | H.264/AAC | 5 sec | 1280x720 | Basic MP4 test |
| `sample.mkv` | H.264/AAC | 5 sec | 1920x1080 | MKV container test |
| `sample.webm` | VP9/Opus | 5 sec | 1280x720 | WebM format test |
| `sample.avi` | MPEG4/MP3 | 5 sec | 640x480 | AVI format test |

### Playlists (`playlists/`)

Sample playlist files for testing playlist parsing:

| File | Format | Description |
|------|--------|-------------|
| `basic.m3u` | M3U | Simple M3U playlist |
| `extended.m3u` | Extended M3U | M3U with metadata |
| `basic.pls` | PLS | PLS format playlist |
| `unicode.m3u` | M3U | Playlist with Unicode paths |
| `relative.m3u` | M3U | Playlist with relative paths |
| `absolute.m3u` | M3U | Playlist with absolute paths |

### Subtitles (`subtitles/`)

Sample subtitle files for testing subtitle support:

| File | Format | Description |
|------|--------|-------------|
| `sample.srt` | SRT | Basic SRT subtitles |
| `sample.ass` | ASS/SSA | Advanced SubStation Alpha |
| `sample.vtt` | WebVTT | Web Video Text Tracks |
| `unicode.srt` | SRT | Subtitles with Unicode |

## File Generation

Test files can be generated using the provided script:

```bash
./generate_test_files.sh
```

This script creates:
- Silent audio files of various formats
- Test pattern video files
- Sample playlists pointing to generated files
- Basic subtitle files

## Requirements

For generating test files:
- FFmpeg (for audio/video generation)
- Basic shell utilities

## Usage in Tests

### Loading Test Files

```pascal
const
  TEST_DATA_DIR = '../TestData/';

procedure TMyTest.Test_LoadMP3;
begin
  FPlayer.LoadFile(TEST_DATA_DIR + 'audio/sample.mp3');
  AssertEquals('Should load successfully', Ord(msPlaying), Ord(FPlayer.Status));
end;
```

### Using Playlists

```pascal
procedure TMyTest.Test_ParseM3U;
begin
  FPlaylist.LoadFromFile(TEST_DATA_DIR + 'playlists/basic.m3u');
  AssertTrue('Should have items', FPlaylist.Count > 0);
end;
```

## Notes

- Test files are intentionally small (5 seconds) for fast test execution
- All formats are chosen for broad compatibility
- Generated files use royalty-free content
- Some tests may use mock data instead of real files

## Adding Test Files

When adding new test files:

1. Keep files small (under 1MB if possible)
2. Use royalty-free or generated content
3. Update this README with file details
4. Ensure cross-platform compatibility
5. Add corresponding test cases

## Licensing

All test files in this directory are either:
- Generated synthetically (no copyright)
- Public domain / CC0 licensed
- Created specifically for testing purposes

These files are not intended for distribution outside the test suite.
