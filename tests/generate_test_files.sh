#!/bin/bash
# ===============================================================================
# generate_test_files.sh - Generate Test Media Files
#
# Part of 3nity Media - Test Suite
#
# Generates test audio and video files for automated testing using FFmpeg.
# Creates various formats and configurations for comprehensive testing.
#
# Requirements:
#   - ffmpeg (with libx264, libx265, libmp3lame, libvorbis, libopus)
#
# Usage:
#   ./generate_test_files.sh [--all|--audio|--video|--subtitles|--playlists]
#
# Author: Nicolas DEOUX (NDXDev@gmail.com)
# License: GPL-2.0
# ===============================================================================

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Directories
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TESTDATA_DIR="${SCRIPT_DIR}/TestData"
AUDIO_DIR="${TESTDATA_DIR}/audio"
VIDEO_DIR="${TESTDATA_DIR}/video"
SUBTITLES_DIR="${TESTDATA_DIR}/subtitles"
PLAYLISTS_DIR="${TESTDATA_DIR}/playlists"

# ===============================================================================
# Helper Functions
# ===============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[OK]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_ffmpeg() {
    if ! command -v ffmpeg &> /dev/null; then
        log_error "ffmpeg is not installed. Please install ffmpeg first."
        echo "  Ubuntu/Debian: sudo apt install ffmpeg"
        echo "  Fedora: sudo dnf install ffmpeg"
        echo "  Arch: sudo pacman -S ffmpeg"
        exit 1
    fi
    log_success "ffmpeg found: $(ffmpeg -version | head -n1)"
}

create_directories() {
    log_info "Creating test data directories..."
    mkdir -p "$AUDIO_DIR"
    mkdir -p "$VIDEO_DIR"
    mkdir -p "$SUBTITLES_DIR"
    mkdir -p "$PLAYLISTS_DIR"
    log_success "Directories created"
}

# ===============================================================================
# Audio File Generation
# ===============================================================================

generate_audio_files() {
    log_info "Generating audio test files..."

    # Test tone parameters
    DURATION_SHORT=10
    DURATION_MEDIUM=30
    DURATION_LONG=60

    # 1. MP3 44.1kHz Stereo (standard)
    log_info "  Creating test_44100_stereo.mp3..."
    ffmpeg -y -f lavfi -i "sine=frequency=440:duration=${DURATION_MEDIUM}" \
           -f lavfi -i "sine=frequency=880:duration=${DURATION_MEDIUM}" \
           -filter_complex "[0][1]amerge=inputs=2[out]" -map "[out]" \
           -ar 44100 -ac 2 -b:a 192k \
           -metadata title="Test Audio 44.1kHz Stereo" \
           -metadata artist="3nity Media Test Suite" \
           -metadata album="Test Files" \
           "${AUDIO_DIR}/test_44100_stereo.mp3" 2>/dev/null
    log_success "  test_44100_stereo.mp3 created"

    # 2. MP3 48kHz Stereo
    log_info "  Creating test_48000_stereo.mp3..."
    ffmpeg -y -f lavfi -i "sine=frequency=440:duration=${DURATION_MEDIUM}:sample_rate=48000" \
           -f lavfi -i "sine=frequency=880:duration=${DURATION_MEDIUM}:sample_rate=48000" \
           -filter_complex "[0][1]amerge=inputs=2[out]" -map "[out]" \
           -ar 48000 -ac 2 -b:a 256k \
           -metadata title="Test Audio 48kHz Stereo" \
           -metadata artist="3nity Media Test Suite" \
           "${AUDIO_DIR}/test_48000_stereo.mp3" 2>/dev/null
    log_success "  test_48000_stereo.mp3 created"

    # 3. MP3 VBR (Variable Bit Rate)
    log_info "  Creating test_vbr.mp3..."
    ffmpeg -y -f lavfi -i "sine=frequency=440:duration=${DURATION_LONG}" \
           -ar 44100 -ac 2 -q:a 2 \
           -metadata title="Test Audio VBR" \
           -metadata artist="3nity Media Test Suite" \
           "${AUDIO_DIR}/test_vbr.mp3" 2>/dev/null
    log_success "  test_vbr.mp3 created"

    # 4. OGG Vorbis
    log_info "  Creating test_vorbis.ogg..."
    ffmpeg -y -f lavfi -i "sine=frequency=440:duration=${DURATION_MEDIUM}" \
           -ar 44100 -ac 2 -c:a libvorbis -q:a 5 \
           -metadata title="Test Audio Vorbis" \
           -metadata artist="3nity Media Test Suite" \
           "${AUDIO_DIR}/test_vorbis.ogg" 2>/dev/null
    log_success "  test_vorbis.ogg created"

    # 5. FLAC (lossless)
    log_info "  Creating test_lossless.flac..."
    ffmpeg -y -f lavfi -i "sine=frequency=440:duration=${DURATION_SHORT}" \
           -ar 44100 -ac 2 -c:a flac \
           -metadata title="Test Audio FLAC" \
           -metadata artist="3nity Media Test Suite" \
           "${AUDIO_DIR}/test_lossless.flac" 2>/dev/null
    log_success "  test_lossless.flac created"

    # 6. WAV (uncompressed)
    log_info "  Creating test_pcm.wav..."
    ffmpeg -y -f lavfi -i "sine=frequency=440:duration=${DURATION_SHORT}" \
           -ar 44100 -ac 2 -c:a pcm_s16le \
           "${AUDIO_DIR}/test_pcm.wav" 2>/dev/null
    log_success "  test_pcm.wav created"

    # 7. M4A (AAC)
    log_info "  Creating test_aac.m4a..."
    ffmpeg -y -f lavfi -i "sine=frequency=440:duration=${DURATION_MEDIUM}" \
           -ar 44100 -ac 2 -c:a aac -b:a 192k \
           -metadata title="Test Audio AAC" \
           -metadata artist="3nity Media Test Suite" \
           "${AUDIO_DIR}/test_aac.m4a" 2>/dev/null
    log_success "  test_aac.m4a created"

    # 8. Opus (modern codec)
    log_info "  Creating test_opus.opus..."
    ffmpeg -y -f lavfi -i "sine=frequency=440:duration=${DURATION_MEDIUM}" \
           -ar 48000 -ac 2 -c:a libopus -b:a 128k \
           -metadata title="Test Audio Opus" \
           -metadata artist="3nity Media Test Suite" \
           "${AUDIO_DIR}/test_opus.opus" 2>/dev/null
    log_success "  test_opus.opus created"

    # 9. Silence (for edge case testing)
    log_info "  Creating test_silence.mp3..."
    ffmpeg -y -f lavfi -i "anullsrc=r=44100:cl=stereo:d=${DURATION_SHORT}" \
           -c:a libmp3lame -b:a 128k \
           -metadata title="Silence" \
           "${AUDIO_DIR}/test_silence.mp3" 2>/dev/null
    log_success "  test_silence.mp3 created"

    # 10. Short file (1 second - for quick tests)
    log_info "  Creating test_short.mp3..."
    ffmpeg -y -f lavfi -i "sine=frequency=1000:duration=1" \
           -ar 44100 -ac 2 -b:a 192k \
           "${AUDIO_DIR}/test_short.mp3" 2>/dev/null
    log_success "  test_short.mp3 created"

    log_success "Audio test files generated: $(ls -1 ${AUDIO_DIR}/*.* 2>/dev/null | wc -l) files"
}

# ===============================================================================
# Video File Generation
# ===============================================================================

generate_video_files() {
    log_info "Generating video test files..."

    DURATION_VIDEO=10

    # 1. MP4 720p H.264
    log_info "  Creating test_720p.mp4..."
    ffmpeg -y -f lavfi -i "testsrc=duration=${DURATION_VIDEO}:size=1280x720:rate=30" \
           -f lavfi -i "sine=frequency=440:duration=${DURATION_VIDEO}" \
           -c:v libx264 -preset fast -crf 23 \
           -c:a aac -b:a 128k \
           -pix_fmt yuv420p \
           -metadata title="Test Video 720p" \
           -metadata artist="3nity Media Test Suite" \
           "${VIDEO_DIR}/test_720p.mp4" 2>/dev/null
    log_success "  test_720p.mp4 created"

    # 2. MP4 1080p H.264
    log_info "  Creating test_1080p.mp4..."
    ffmpeg -y -f lavfi -i "testsrc=duration=${DURATION_VIDEO}:size=1920x1080:rate=30" \
           -f lavfi -i "sine=frequency=440:duration=${DURATION_VIDEO}" \
           -c:v libx264 -preset fast -crf 23 \
           -c:a aac -b:a 192k \
           -pix_fmt yuv420p \
           -metadata title="Test Video 1080p" \
           "${VIDEO_DIR}/test_1080p.mp4" 2>/dev/null
    log_success "  test_1080p.mp4 created"

    # 3. MKV with H.265/HEVC (if available)
    log_info "  Creating test_hevc.mkv..."
    if ffmpeg -encoders 2>/dev/null | grep -q libx265; then
        ffmpeg -y -f lavfi -i "testsrc=duration=${DURATION_VIDEO}:size=1920x1080:rate=30" \
               -f lavfi -i "sine=frequency=440:duration=${DURATION_VIDEO}" \
               -c:v libx265 -preset fast -crf 28 \
               -c:a libvorbis -q:a 5 \
               -pix_fmt yuv420p \
               -metadata title="Test Video HEVC" \
               "${VIDEO_DIR}/test_hevc.mkv" 2>/dev/null
        log_success "  test_hevc.mkv created"
    else
        log_warning "  libx265 not available, skipping HEVC test file"
    fi

    # 4. WebM VP9
    log_info "  Creating test_vp9.webm..."
    ffmpeg -y -f lavfi -i "testsrc=duration=${DURATION_VIDEO}:size=1280x720:rate=30" \
           -f lavfi -i "sine=frequency=440:duration=${DURATION_VIDEO}" \
           -c:v libvpx-vp9 -crf 30 -b:v 0 -deadline good \
           -c:a libopus -b:a 128k \
           -metadata title="Test Video VP9" \
           "${VIDEO_DIR}/test_vp9.webm" 2>/dev/null
    log_success "  test_vp9.webm created"

    # 5. AVI (legacy format)
    log_info "  Creating test_legacy.avi..."
    ffmpeg -y -f lavfi -i "testsrc=duration=${DURATION_VIDEO}:size=640x480:rate=25" \
           -f lavfi -i "sine=frequency=440:duration=${DURATION_VIDEO}" \
           -c:v mpeg4 -q:v 5 \
           -c:a mp3 -b:a 128k \
           -metadata title="Test Video AVI" \
           "${VIDEO_DIR}/test_legacy.avi" 2>/dev/null
    log_success "  test_legacy.avi created"

    # 6. MKV with multiple audio tracks
    log_info "  Creating test_multi_audio.mkv..."
    ffmpeg -y -f lavfi -i "testsrc=duration=${DURATION_VIDEO}:size=1280x720:rate=30" \
           -f lavfi -i "sine=frequency=440:duration=${DURATION_VIDEO}" \
           -f lavfi -i "sine=frequency=880:duration=${DURATION_VIDEO}" \
           -map 0 -map 1 -map 2 \
           -c:v libx264 -preset fast -crf 23 \
           -c:a:0 aac -b:a:0 128k -metadata:s:a:0 language=eng -metadata:s:a:0 title="English" \
           -c:a:1 aac -b:a:1 128k -metadata:s:a:1 language=fra -metadata:s:a:1 title="French" \
           -pix_fmt yuv420p \
           -metadata title="Test Multi-Audio" \
           "${VIDEO_DIR}/test_multi_audio.mkv" 2>/dev/null
    log_success "  test_multi_audio.mkv created"

    # 7. Video without audio
    log_info "  Creating test_video_only.mp4..."
    ffmpeg -y -f lavfi -i "testsrc=duration=${DURATION_VIDEO}:size=1280x720:rate=30" \
           -c:v libx264 -preset fast -crf 23 \
           -pix_fmt yuv420p \
           -an \
           -metadata title="Test Video Only" \
           "${VIDEO_DIR}/test_video_only.mp4" 2>/dev/null
    log_success "  test_video_only.mp4 created"

    # 8. 4:3 aspect ratio
    log_info "  Creating test_4x3.mp4..."
    ffmpeg -y -f lavfi -i "testsrc=duration=${DURATION_VIDEO}:size=640x480:rate=30" \
           -f lavfi -i "sine=frequency=440:duration=${DURATION_VIDEO}" \
           -c:v libx264 -preset fast -crf 23 \
           -c:a aac -b:a 128k \
           -pix_fmt yuv420p \
           -metadata title="Test Video 4:3" \
           "${VIDEO_DIR}/test_4x3.mp4" 2>/dev/null
    log_success "  test_4x3.mp4 created"

    log_success "Video test files generated: $(ls -1 ${VIDEO_DIR}/*.* 2>/dev/null | wc -l) files"
}

# ===============================================================================
# Subtitle File Generation
# ===============================================================================

generate_subtitle_files() {
    log_info "Generating subtitle test files..."

    # 1. SRT Format
    log_info "  Creating test.srt..."
    cat > "${SUBTITLES_DIR}/test.srt" << 'EOF'
1
00:00:01,000 --> 00:00:04,000
This is the first subtitle line.

2
00:00:05,000 --> 00:00:08,000
This is the second subtitle line.
With multiple lines of text.

3
00:00:09,000 --> 00:00:12,000
Testing special characters: éàü ñ © ® ™

4
00:00:13,000 --> 00:00:16,000
<i>Italics</i> and <b>Bold</b> text.

5
00:00:17,000 --> 00:00:20,000
Last subtitle entry for testing.
EOF
    log_success "  test.srt created"

    # 2. ASS/SSA Format
    log_info "  Creating test.ass..."
    cat > "${SUBTITLES_DIR}/test.ass" << 'EOF'
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
EOF
    log_success "  test.ass created"

    # 3. VTT Format (WebVTT)
    log_info "  Creating test.vtt..."
    cat > "${SUBTITLES_DIR}/test.vtt" << 'EOF'
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
Testing special characters: éàü ñ © ® ™

4
00:00:13.000 --> 00:00:16.000
<i>Italics</i> and <b>Bold</b> text.

5
00:00:17.000 --> 00:00:20.000
Last subtitle entry for testing.
EOF
    log_success "  test.vtt created"

    # 4. French subtitles
    log_info "  Creating test_fr.srt..."
    cat > "${SUBTITLES_DIR}/test_fr.srt" << 'EOF'
1
00:00:01,000 --> 00:00:04,000
Ceci est la première ligne de sous-titre.

2
00:00:05,000 --> 00:00:08,000
Ceci est la deuxième ligne.
Avec plusieurs lignes de texte.

3
00:00:09,000 --> 00:00:12,000
Test des caractères spéciaux: éàùç œ æ

4
00:00:13,000 --> 00:00:16,000
<i>Italique</i> et <b>Gras</b>.

5
00:00:17,000 --> 00:00:20,000
Dernière entrée de sous-titre.
EOF
    log_success "  test_fr.srt created"

    log_success "Subtitle test files generated: $(ls -1 ${SUBTITLES_DIR}/*.* 2>/dev/null | wc -l) files"
}

# ===============================================================================
# Playlist File Generation
# ===============================================================================

generate_playlist_files() {
    log_info "Generating playlist test files..."

    # 1. Simple M3U
    log_info "  Creating test_simple.m3u..."
    cat > "${PLAYLISTS_DIR}/test_simple.m3u" << EOF
${AUDIO_DIR}/test_44100_stereo.mp3
${AUDIO_DIR}/test_vorbis.ogg
${AUDIO_DIR}/test_lossless.flac
EOF
    log_success "  test_simple.m3u created"

    # 2. Extended M3U
    log_info "  Creating test_extended.m3u..."
    cat > "${PLAYLISTS_DIR}/test_extended.m3u" << EOF
#EXTM3U
#EXTINF:30,Test Audio 44.1kHz Stereo - 3nity Media Test Suite
${AUDIO_DIR}/test_44100_stereo.mp3
#EXTINF:30,Test Audio Vorbis - 3nity Media Test Suite
${AUDIO_DIR}/test_vorbis.ogg
#EXTINF:10,Test Audio FLAC - 3nity Media Test Suite
${AUDIO_DIR}/test_lossless.flac
#EXTINF:30,Test Audio AAC - 3nity Media Test Suite
${AUDIO_DIR}/test_aac.m4a
EOF
    log_success "  test_extended.m3u created"

    # 3. M3U8 (UTF-8)
    log_info "  Creating test_utf8.m3u8..."
    cat > "${PLAYLISTS_DIR}/test_utf8.m3u8" << EOF
#EXTM3U
#EXTINF:30,Piste Audio Française - éàùç
${AUDIO_DIR}/test_44100_stereo.mp3
#EXTINF:30,日本語テスト - Japanese Test
${AUDIO_DIR}/test_vorbis.ogg
#EXTINF:10,Тест на русском - Russian Test
${AUDIO_DIR}/test_lossless.flac
EOF
    log_success "  test_utf8.m3u8 created"

    # 4. PLS Format
    log_info "  Creating test.pls..."
    cat > "${PLAYLISTS_DIR}/test.pls" << EOF
[playlist]
File1=${AUDIO_DIR}/test_44100_stereo.mp3
Title1=Test Audio 44.1kHz Stereo
Length1=30
File2=${AUDIO_DIR}/test_vorbis.ogg
Title2=Test Audio Vorbis
Length2=30
File3=${AUDIO_DIR}/test_lossless.flac
Title3=Test Audio FLAC
Length3=10
NumberOfEntries=3
Version=2
EOF
    log_success "  test.pls created"

    # 5. Video playlist
    log_info "  Creating test_video.m3u..."
    cat > "${PLAYLISTS_DIR}/test_video.m3u" << EOF
#EXTM3U
#EXTINF:10,Test Video 720p
${VIDEO_DIR}/test_720p.mp4
#EXTINF:10,Test Video 1080p
${VIDEO_DIR}/test_1080p.mp4
#EXTINF:10,Test Video VP9
${VIDEO_DIR}/test_vp9.webm
EOF
    log_success "  test_video.m3u created"

    # 6. Mixed playlist
    log_info "  Creating test_mixed.m3u..."
    cat > "${PLAYLISTS_DIR}/test_mixed.m3u" << EOF
#EXTM3U
#EXTINF:30,Audio Track
${AUDIO_DIR}/test_44100_stereo.mp3
#EXTINF:10,Video Track
${VIDEO_DIR}/test_720p.mp4
#EXTINF:30,Another Audio
${AUDIO_DIR}/test_vorbis.ogg
EOF
    log_success "  test_mixed.m3u created"

    # 7. Empty playlist
    log_info "  Creating test_empty.m3u..."
    echo "#EXTM3U" > "${PLAYLISTS_DIR}/test_empty.m3u"
    log_success "  test_empty.m3u created"

    # 8. Stream URLs playlist
    log_info "  Creating test_streams.m3u..."
    cat > "${PLAYLISTS_DIR}/test_streams.m3u" << 'EOF'
#EXTM3U
#EXTINF:-1,SomaFM - Groove Salad
http://ice1.somafm.com/groovesalad-128-mp3
#EXTINF:-1,SomaFM - Drone Zone
http://ice1.somafm.com/dronezone-128-mp3
#EXTINF:-1,SomaFM - Deep Space One
http://ice1.somafm.com/deepspaceone-128-mp3
EOF
    log_success "  test_streams.m3u created"

    log_success "Playlist test files generated: $(ls -1 ${PLAYLISTS_DIR}/*.* 2>/dev/null | wc -l) files"
}

# ===============================================================================
# Summary
# ===============================================================================

show_summary() {
    echo ""
    echo "========================================"
    echo "Test Files Generation Summary"
    echo "========================================"
    echo ""

    if [ -d "$AUDIO_DIR" ]; then
        echo "Audio files (${AUDIO_DIR}):"
        ls -lh "$AUDIO_DIR"/*.* 2>/dev/null | awk '{print "  " $9 " (" $5 ")"}'
        echo ""
    fi

    if [ -d "$VIDEO_DIR" ]; then
        echo "Video files (${VIDEO_DIR}):"
        ls -lh "$VIDEO_DIR"/*.* 2>/dev/null | awk '{print "  " $9 " (" $5 ")"}'
        echo ""
    fi

    if [ -d "$SUBTITLES_DIR" ]; then
        echo "Subtitle files (${SUBTITLES_DIR}):"
        ls -lh "$SUBTITLES_DIR"/*.* 2>/dev/null | awk '{print "  " $9 " (" $5 ")"}'
        echo ""
    fi

    if [ -d "$PLAYLISTS_DIR" ]; then
        echo "Playlist files (${PLAYLISTS_DIR}):"
        ls -lh "$PLAYLISTS_DIR"/*.* 2>/dev/null | awk '{print "  " $9 " (" $5 ")"}'
        echo ""
    fi

    TOTAL=$(find "$TESTDATA_DIR" -type f | wc -l)
    SIZE=$(du -sh "$TESTDATA_DIR" 2>/dev/null | cut -f1)
    echo "Total: ${TOTAL} files, ${SIZE}"
    echo ""
    log_success "All test files generated successfully!"
}

# ===============================================================================
# Main
# ===============================================================================

main() {
    echo ""
    echo "========================================"
    echo "3nity Media - Test File Generator"
    echo "========================================"
    echo ""

    check_ffmpeg
    create_directories

    case "${1:-all}" in
        --audio)
            generate_audio_files
            ;;
        --video)
            generate_video_files
            ;;
        --subtitles)
            generate_subtitle_files
            ;;
        --playlists)
            generate_playlist_files
            ;;
        --all|*)
            generate_audio_files
            generate_video_files
            generate_subtitle_files
            generate_playlist_files
            ;;
    esac

    show_summary
}

main "$@"
