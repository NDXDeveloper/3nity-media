# ===============================================================================
# generate_test_files_win.ps1 - Generate Test Media Files (Windows PowerShell)
#
# Part of 3nity Media - Test Suite
#
# Generates test audio and video files for automated testing using FFmpeg.
# Creates various formats and configurations for comprehensive testing.
#
# Requirements:
#   - ffmpeg (with libx264, libx265, libmp3lame, libvorbis, libopus)
#   - PowerShell 5.1+ (included in Windows 10/11)
#
# Usage:
#   .\generate_test_files_win.ps1 [-All] [-Audio] [-Video] [-Subtitles] [-Playlists]
#
# Author: Nicolas DEOUX (NDXDev@gmail.com)
# License: GPL-2.0
# ===============================================================================

[CmdletBinding()]
param(
    [switch]$All,
    [switch]$Audio,
    [switch]$Video,
    [switch]$Subtitles,
    [switch]$Playlists
)

$ErrorActionPreference = "Stop"

# ===============================================================================
# Directories
# ===============================================================================
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$TestDataDir = Join-Path $ScriptDir "TestData"
$AudioDir = Join-Path $TestDataDir "audio"
$VideoDir = Join-Path $TestDataDir "video"
$SubtitlesDir = Join-Path $TestDataDir "subtitles"
$PlaylistsDir = Join-Path $TestDataDir "playlists"

# ===============================================================================
# Helper Functions
# ===============================================================================

function Write-Info {
    param([string]$Message)
    Write-Host "[INFO] " -ForegroundColor Blue -NoNewline
    Write-Host $Message
}

function Write-Success {
    param([string]$Message)
    Write-Host "[OK] " -ForegroundColor Green -NoNewline
    Write-Host $Message
}

function Write-Warning {
    param([string]$Message)
    Write-Host "[WARN] " -ForegroundColor Yellow -NoNewline
    Write-Host $Message
}

function Write-Error {
    param([string]$Message)
    Write-Host "[ERROR] " -ForegroundColor Red -NoNewline
    Write-Host $Message
}

function Test-FFmpeg {
    try {
        $version = & ffmpeg -version 2>&1 | Select-Object -First 1
        Write-Success "ffmpeg found: $version"
        return $true
    }
    catch {
        Write-Error "ffmpeg is not installed or not in PATH."
        Write-Host "  Download from: https://ffmpeg.org/download.html"
        Write-Host "  Or use: winget install ffmpeg"
        Write-Host "  Or use: choco install ffmpeg"
        return $false
    }
}

function New-Directories {
    Write-Info "Creating test data directories..."
    @($AudioDir, $VideoDir, $SubtitlesDir, $PlaylistsDir) | ForEach-Object {
        if (-not (Test-Path $_)) {
            New-Item -ItemType Directory -Path $_ -Force | Out-Null
        }
    }
    Write-Success "Directories created"
}

# ===============================================================================
# Audio File Generation
# ===============================================================================

function New-AudioFiles {
    Write-Info "Generating audio test files..."

    $DurationShort = 10
    $DurationMedium = 30
    $DurationLong = 60

    # 1. MP3 44.1kHz Stereo (standard)
    Write-Info "  Creating test_44100_stereo.mp3..."
    & ffmpeg -y -f lavfi -i "sine=frequency=440:duration=$DurationMedium" `
        -f lavfi -i "sine=frequency=880:duration=$DurationMedium" `
        -filter_complex "[0][1]amerge=inputs=2[out]" -map "[out]" `
        -ar 44100 -ac 2 -b:a 192k `
        -metadata title="Test Audio 44.1kHz Stereo" `
        -metadata artist="3nity Media Test Suite" `
        -metadata album="Test Files" `
        "$AudioDir\test_44100_stereo.mp3" 2>$null
    Write-Success "  test_44100_stereo.mp3 created"

    # 2. MP3 48kHz Stereo
    Write-Info "  Creating test_48000_stereo.mp3..."
    & ffmpeg -y -f lavfi -i "sine=frequency=440:duration=$DurationMedium`:sample_rate=48000" `
        -f lavfi -i "sine=frequency=880:duration=$DurationMedium`:sample_rate=48000" `
        -filter_complex "[0][1]amerge=inputs=2[out]" -map "[out]" `
        -ar 48000 -ac 2 -b:a 256k `
        -metadata title="Test Audio 48kHz Stereo" `
        -metadata artist="3nity Media Test Suite" `
        "$AudioDir\test_48000_stereo.mp3" 2>$null
    Write-Success "  test_48000_stereo.mp3 created"

    # 3. MP3 VBR (Variable Bit Rate)
    Write-Info "  Creating test_vbr.mp3..."
    & ffmpeg -y -f lavfi -i "sine=frequency=440:duration=$DurationLong" `
        -ar 44100 -ac 2 -q:a 2 `
        -metadata title="Test Audio VBR" `
        -metadata artist="3nity Media Test Suite" `
        "$AudioDir\test_vbr.mp3" 2>$null
    Write-Success "  test_vbr.mp3 created"

    # 4. OGG Vorbis
    Write-Info "  Creating test_vorbis.ogg..."
    & ffmpeg -y -f lavfi -i "sine=frequency=440:duration=$DurationMedium" `
        -ar 44100 -ac 2 -c:a libvorbis -q:a 5 `
        -metadata title="Test Audio Vorbis" `
        -metadata artist="3nity Media Test Suite" `
        "$AudioDir\test_vorbis.ogg" 2>$null
    Write-Success "  test_vorbis.ogg created"

    # 5. FLAC (lossless)
    Write-Info "  Creating test_lossless.flac..."
    & ffmpeg -y -f lavfi -i "sine=frequency=440:duration=$DurationShort" `
        -ar 44100 -ac 2 -c:a flac `
        -metadata title="Test Audio FLAC" `
        -metadata artist="3nity Media Test Suite" `
        "$AudioDir\test_lossless.flac" 2>$null
    Write-Success "  test_lossless.flac created"

    # 6. WAV (uncompressed)
    Write-Info "  Creating test_pcm.wav..."
    & ffmpeg -y -f lavfi -i "sine=frequency=440:duration=$DurationShort" `
        -ar 44100 -ac 2 -c:a pcm_s16le `
        "$AudioDir\test_pcm.wav" 2>$null
    Write-Success "  test_pcm.wav created"

    # 7. M4A (AAC)
    Write-Info "  Creating test_aac.m4a..."
    & ffmpeg -y -f lavfi -i "sine=frequency=440:duration=$DurationMedium" `
        -ar 44100 -ac 2 -c:a aac -b:a 192k `
        -metadata title="Test Audio AAC" `
        -metadata artist="3nity Media Test Suite" `
        "$AudioDir\test_aac.m4a" 2>$null
    Write-Success "  test_aac.m4a created"

    # 8. Opus (modern codec)
    Write-Info "  Creating test_opus.opus..."
    & ffmpeg -y -f lavfi -i "sine=frequency=440:duration=$DurationMedium" `
        -ar 48000 -ac 2 -c:a libopus -b:a 128k `
        -metadata title="Test Audio Opus" `
        -metadata artist="3nity Media Test Suite" `
        "$AudioDir\test_opus.opus" 2>$null
    Write-Success "  test_opus.opus created"

    # 9. Silence (for edge case testing)
    Write-Info "  Creating test_silence.mp3..."
    & ffmpeg -y -f lavfi -i "anullsrc=r=44100:cl=stereo:d=$DurationShort" `
        -c:a libmp3lame -b:a 128k `
        -metadata title="Silence" `
        "$AudioDir\test_silence.mp3" 2>$null
    Write-Success "  test_silence.mp3 created"

    # 10. Short file (1 second - for quick tests)
    Write-Info "  Creating test_short.mp3..."
    & ffmpeg -y -f lavfi -i "sine=frequency=1000:duration=1" `
        -ar 44100 -ac 2 -b:a 192k `
        "$AudioDir\test_short.mp3" 2>$null
    Write-Success "  test_short.mp3 created"

    $count = (Get-ChildItem -Path $AudioDir -File).Count
    Write-Success "Audio test files generated: $count files"
}

# ===============================================================================
# Video File Generation
# ===============================================================================

function New-VideoFiles {
    Write-Info "Generating video test files..."

    $DurationVideo = 10

    # 1. MP4 720p H.264
    Write-Info "  Creating test_720p.mp4..."
    & ffmpeg -y -f lavfi -i "testsrc=duration=$DurationVideo`:size=1280x720:rate=30" `
        -f lavfi -i "sine=frequency=440:duration=$DurationVideo" `
        -c:v libx264 -preset fast -crf 23 `
        -c:a aac -b:a 128k `
        -pix_fmt yuv420p `
        -metadata title="Test Video 720p" `
        -metadata artist="3nity Media Test Suite" `
        "$VideoDir\test_720p.mp4" 2>$null
    Write-Success "  test_720p.mp4 created"

    # 2. MP4 1080p H.264
    Write-Info "  Creating test_1080p.mp4..."
    & ffmpeg -y -f lavfi -i "testsrc=duration=$DurationVideo`:size=1920x1080:rate=30" `
        -f lavfi -i "sine=frequency=440:duration=$DurationVideo" `
        -c:v libx264 -preset fast -crf 23 `
        -c:a aac -b:a 192k `
        -pix_fmt yuv420p `
        -metadata title="Test Video 1080p" `
        "$VideoDir\test_1080p.mp4" 2>$null
    Write-Success "  test_1080p.mp4 created"

    # 3. MKV with H.265/HEVC (if available)
    Write-Info "  Creating test_hevc.mkv..."
    $encoders = & ffmpeg -encoders 2>&1
    if ($encoders -match "libx265") {
        & ffmpeg -y -f lavfi -i "testsrc=duration=$DurationVideo`:size=1920x1080:rate=30" `
            -f lavfi -i "sine=frequency=440:duration=$DurationVideo" `
            -c:v libx265 -preset fast -crf 28 `
            -c:a libvorbis -q:a 5 `
            -pix_fmt yuv420p `
            -metadata title="Test Video HEVC" `
            "$VideoDir\test_hevc.mkv" 2>$null
        Write-Success "  test_hevc.mkv created"
    }
    else {
        Write-Warning "  libx265 not available, skipping HEVC test file"
    }

    # 4. WebM VP9
    Write-Info "  Creating test_vp9.webm..."
    & ffmpeg -y -f lavfi -i "testsrc=duration=$DurationVideo`:size=1280x720:rate=30" `
        -f lavfi -i "sine=frequency=440:duration=$DurationVideo" `
        -c:v libvpx-vp9 -crf 30 -b:v 0 -deadline good `
        -c:a libopus -b:a 128k `
        -metadata title="Test Video VP9" `
        "$VideoDir\test_vp9.webm" 2>$null
    Write-Success "  test_vp9.webm created"

    # 5. AVI (legacy format)
    Write-Info "  Creating test_legacy.avi..."
    & ffmpeg -y -f lavfi -i "testsrc=duration=$DurationVideo`:size=640x480:rate=25" `
        -f lavfi -i "sine=frequency=440:duration=$DurationVideo" `
        -c:v mpeg4 -q:v 5 `
        -c:a mp3 -b:a 128k `
        -metadata title="Test Video AVI" `
        "$VideoDir\test_legacy.avi" 2>$null
    Write-Success "  test_legacy.avi created"

    # 6. MKV with multiple audio tracks
    Write-Info "  Creating test_multi_audio.mkv..."
    & ffmpeg -y -f lavfi -i "testsrc=duration=$DurationVideo`:size=1280x720:rate=30" `
        -f lavfi -i "sine=frequency=440:duration=$DurationVideo" `
        -f lavfi -i "sine=frequency=880:duration=$DurationVideo" `
        -map 0 -map 1 -map 2 `
        -c:v libx264 -preset fast -crf 23 `
        -c:a:0 aac -b:a:0 128k -metadata:s:a:0 language=eng -metadata:s:a:0 title="English" `
        -c:a:1 aac -b:a:1 128k -metadata:s:a:1 language=fra -metadata:s:a:1 title="French" `
        -pix_fmt yuv420p `
        -metadata title="Test Multi-Audio" `
        "$VideoDir\test_multi_audio.mkv" 2>$null
    Write-Success "  test_multi_audio.mkv created"

    # 7. Video without audio
    Write-Info "  Creating test_video_only.mp4..."
    & ffmpeg -y -f lavfi -i "testsrc=duration=$DurationVideo`:size=1280x720:rate=30" `
        -c:v libx264 -preset fast -crf 23 `
        -pix_fmt yuv420p `
        -an `
        -metadata title="Test Video Only" `
        "$VideoDir\test_video_only.mp4" 2>$null
    Write-Success "  test_video_only.mp4 created"

    # 8. 4:3 aspect ratio
    Write-Info "  Creating test_4x3.mp4..."
    & ffmpeg -y -f lavfi -i "testsrc=duration=$DurationVideo`:size=640x480:rate=30" `
        -f lavfi -i "sine=frequency=440:duration=$DurationVideo" `
        -c:v libx264 -preset fast -crf 23 `
        -c:a aac -b:a 128k `
        -pix_fmt yuv420p `
        -metadata title="Test Video 4:3" `
        "$VideoDir\test_4x3.mp4" 2>$null
    Write-Success "  test_4x3.mp4 created"

    $count = (Get-ChildItem -Path $VideoDir -File).Count
    Write-Success "Video test files generated: $count files"
}

# ===============================================================================
# Subtitle File Generation
# ===============================================================================

function New-SubtitleFiles {
    Write-Info "Generating subtitle test files..."

    # 1. SRT Format
    Write-Info "  Creating test.srt..."
    @"
1
00:00:01,000 --> 00:00:04,000
This is the first subtitle line.

2
00:00:05,000 --> 00:00:08,000
This is the second subtitle line.
With multiple lines of text.

3
00:00:09,000 --> 00:00:12,000
Testing special characters: eaue n (c) (R) TM

4
00:00:13,000 --> 00:00:16,000
<i>Italics</i> and <b>Bold</b> text.

5
00:00:17,000 --> 00:00:20,000
Last subtitle entry for testing.
"@ | Out-File -FilePath "$SubtitlesDir\test.srt" -Encoding UTF8
    Write-Success "  test.srt created"

    # 2. ASS/SSA Format
    Write-Info "  Creating test.ass..."
    @"
[Script Info]
Title: Test Subtitles
ScriptType: v4.00+
Collisions: Normal
PlayDepth: 0

[V4+ Styles]
Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding
Style: Default,Arial,20,&H00FFFFFF,&H000000FF,&H00000000,&H00000000,0,0,0,0,100,100,0,0,1,2,2,2,10,10,10,1

[Events]
Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text
Dialogue: 0,0:00:01.00,0:00:04.00,Default,,0,0,0,,This is the first subtitle line.
Dialogue: 0,0:00:05.00,0:00:08.00,Default,,0,0,0,,This is the second subtitle line.\NWith multiple lines.
Dialogue: 0,0:00:09.00,0:00:12.00,Default,,0,0,0,,Testing ASS formatting: {\b1}Bold{\b0} and {\i1}Italic{\i0}
Dialogue: 0,0:00:13.00,0:00:16.00,Default,,0,0,0,,{\c&H0000FF&}Red text{\c} normal text
Dialogue: 0,0:00:17.00,0:00:20.00,Default,,0,0,0,,Last subtitle entry.
"@ | Out-File -FilePath "$SubtitlesDir\test.ass" -Encoding UTF8
    Write-Success "  test.ass created"

    # 3. VTT Format (WebVTT)
    Write-Info "  Creating test.vtt..."
    @"
WEBVTT

1
00:00:01.000 --> 00:00:04.000
This is the first subtitle line.

2
00:00:05.000 --> 00:00:08.000
This is the second subtitle line.
With multiple lines of text.

3
00:00:09.000 --> 00:00:12.000
Testing special characters: eaue n (c) (R) TM

4
00:00:13.000 --> 00:00:16.000
<i>Italics</i> and <b>Bold</b> text.

5
00:00:17.000 --> 00:00:20.000
Last subtitle entry for testing.
"@ | Out-File -FilePath "$SubtitlesDir\test.vtt" -Encoding UTF8
    Write-Success "  test.vtt created"

    # 4. French subtitles
    Write-Info "  Creating test_fr.srt..."
    @"
1
00:00:01,000 --> 00:00:04,000
Ceci est la premiere ligne de sous-titre.

2
00:00:05,000 --> 00:00:08,000
Ceci est la deuxieme ligne.
Avec plusieurs lignes de texte.

3
00:00:09,000 --> 00:00:12,000
Test des caracteres speciaux: eauc oe ae

4
00:00:13,000 --> 00:00:16,000
<i>Italique</i> et <b>Gras</b>.

5
00:00:17,000 --> 00:00:20,000
Derniere entree de sous-titre.
"@ | Out-File -FilePath "$SubtitlesDir\test_fr.srt" -Encoding UTF8
    Write-Success "  test_fr.srt created"

    $count = (Get-ChildItem -Path $SubtitlesDir -File).Count
    Write-Success "Subtitle test files generated: $count files"
}

# ===============================================================================
# Playlist File Generation
# ===============================================================================

function New-PlaylistFiles {
    Write-Info "Generating playlist test files..."

    # 1. Simple M3U
    Write-Info "  Creating test_simple.m3u..."
    @"
$AudioDir\test_44100_stereo.mp3
$AudioDir\test_vorbis.ogg
$AudioDir\test_lossless.flac
"@ | Out-File -FilePath "$PlaylistsDir\test_simple.m3u" -Encoding UTF8
    Write-Success "  test_simple.m3u created"

    # 2. Extended M3U
    Write-Info "  Creating test_extended.m3u..."
    @"
#EXTM3U
#EXTINF:30,Test Audio 44.1kHz Stereo - 3nity Media Test Suite
$AudioDir\test_44100_stereo.mp3
#EXTINF:30,Test Audio Vorbis - 3nity Media Test Suite
$AudioDir\test_vorbis.ogg
#EXTINF:10,Test Audio FLAC - 3nity Media Test Suite
$AudioDir\test_lossless.flac
#EXTINF:30,Test Audio AAC - 3nity Media Test Suite
$AudioDir\test_aac.m4a
"@ | Out-File -FilePath "$PlaylistsDir\test_extended.m3u" -Encoding UTF8
    Write-Success "  test_extended.m3u created"

    # 3. M3U8 (UTF-8)
    Write-Info "  Creating test_utf8.m3u8..."
    @"
#EXTM3U
#EXTINF:30,Piste Audio Francaise - eauc
$AudioDir\test_44100_stereo.mp3
#EXTINF:30,Japanese Test
$AudioDir\test_vorbis.ogg
#EXTINF:10,Russian Test
$AudioDir\test_lossless.flac
"@ | Out-File -FilePath "$PlaylistsDir\test_utf8.m3u8" -Encoding UTF8
    Write-Success "  test_utf8.m3u8 created"

    # 4. PLS Format
    Write-Info "  Creating test.pls..."
    @"
[playlist]
File1=$AudioDir\test_44100_stereo.mp3
Title1=Test Audio 44.1kHz Stereo
Length1=30
File2=$AudioDir\test_vorbis.ogg
Title2=Test Audio Vorbis
Length2=30
File3=$AudioDir\test_lossless.flac
Title3=Test Audio FLAC
Length3=10
NumberOfEntries=3
Version=2
"@ | Out-File -FilePath "$PlaylistsDir\test.pls" -Encoding UTF8
    Write-Success "  test.pls created"

    # 5. Video playlist
    Write-Info "  Creating test_video.m3u..."
    @"
#EXTM3U
#EXTINF:10,Test Video 720p
$VideoDir\test_720p.mp4
#EXTINF:10,Test Video 1080p
$VideoDir\test_1080p.mp4
#EXTINF:10,Test Video VP9
$VideoDir\test_vp9.webm
"@ | Out-File -FilePath "$PlaylistsDir\test_video.m3u" -Encoding UTF8
    Write-Success "  test_video.m3u created"

    # 6. Mixed playlist
    Write-Info "  Creating test_mixed.m3u..."
    @"
#EXTM3U
#EXTINF:30,Audio Track
$AudioDir\test_44100_stereo.mp3
#EXTINF:10,Video Track
$VideoDir\test_720p.mp4
#EXTINF:30,Another Audio
$AudioDir\test_vorbis.ogg
"@ | Out-File -FilePath "$PlaylistsDir\test_mixed.m3u" -Encoding UTF8
    Write-Success "  test_mixed.m3u created"

    # 7. Empty playlist
    Write-Info "  Creating test_empty.m3u..."
    "#EXTM3U" | Out-File -FilePath "$PlaylistsDir\test_empty.m3u" -Encoding UTF8
    Write-Success "  test_empty.m3u created"

    # 8. Stream URLs playlist
    Write-Info "  Creating test_streams.m3u..."
    @"
#EXTM3U
#EXTINF:-1,SomaFM - Groove Salad
http://ice1.somafm.com/groovesalad-128-mp3
#EXTINF:-1,SomaFM - Drone Zone
http://ice1.somafm.com/dronezone-128-mp3
#EXTINF:-1,SomaFM - Deep Space One
http://ice1.somafm.com/deepspaceone-128-mp3
"@ | Out-File -FilePath "$PlaylistsDir\test_streams.m3u" -Encoding UTF8
    Write-Success "  test_streams.m3u created"

    $count = (Get-ChildItem -Path $PlaylistsDir -File).Count
    Write-Success "Playlist test files generated: $count files"
}

# ===============================================================================
# Summary
# ===============================================================================

function Show-Summary {
    Write-Host ""
    Write-Host "========================================"
    Write-Host "Test Files Generation Summary"
    Write-Host "========================================"
    Write-Host ""

    if (Test-Path $AudioDir) {
        Write-Host "Audio files ($AudioDir):"
        Get-ChildItem -Path $AudioDir -File | ForEach-Object {
            $size = "{0:N2} KB" -f ($_.Length / 1KB)
            Write-Host "  $($_.Name) ($size)"
        }
        Write-Host ""
    }

    if (Test-Path $VideoDir) {
        Write-Host "Video files ($VideoDir):"
        Get-ChildItem -Path $VideoDir -File | ForEach-Object {
            $size = "{0:N2} MB" -f ($_.Length / 1MB)
            Write-Host "  $($_.Name) ($size)"
        }
        Write-Host ""
    }

    if (Test-Path $SubtitlesDir) {
        Write-Host "Subtitle files ($SubtitlesDir):"
        Get-ChildItem -Path $SubtitlesDir -File | ForEach-Object {
            $size = "{0:N2} KB" -f ($_.Length / 1KB)
            Write-Host "  $($_.Name) ($size)"
        }
        Write-Host ""
    }

    if (Test-Path $PlaylistsDir) {
        Write-Host "Playlist files ($PlaylistsDir):"
        Get-ChildItem -Path $PlaylistsDir -File | ForEach-Object {
            $size = "{0:N2} KB" -f ($_.Length / 1KB)
            Write-Host "  $($_.Name) ($size)"
        }
        Write-Host ""
    }

    $totalFiles = (Get-ChildItem -Path $TestDataDir -Recurse -File).Count
    $totalSize = "{0:N2} MB" -f ((Get-ChildItem -Path $TestDataDir -Recurse -File | Measure-Object -Property Length -Sum).Sum / 1MB)
    Write-Host "Total: $totalFiles files, $totalSize"
    Write-Host ""
    Write-Success "All test files generated successfully!"
}

# ===============================================================================
# Main
# ===============================================================================

Write-Host ""
Write-Host "========================================"
Write-Host "3nity Media - Test File Generator"
Write-Host "(Windows PowerShell)"
Write-Host "========================================"
Write-Host ""

if (-not (Test-FFmpeg)) {
    exit 1
}

New-Directories

# Default to All if no switches specified
if (-not ($Audio -or $Video -or $Subtitles -or $Playlists)) {
    $All = $true
}

if ($All -or $Audio) {
    New-AudioFiles
}

if ($All -or $Video) {
    New-VideoFiles
}

if ($All -or $Subtitles) {
    New-SubtitleFiles
}

if ($All -or $Playlists) {
    New-PlaylistFiles
}

Show-Summary
