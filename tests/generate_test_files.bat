@echo off
REM ===============================================================================
REM generate_test_files.bat - Generate Test Media Files (Windows Batch)
REM
REM Part of 3nity Media - Test Suite
REM
REM Generates test audio and video files for automated testing using FFmpeg.
REM Creates various formats and configurations for comprehensive testing.
REM
REM Requirements:
REM   - ffmpeg (must be in PATH or in same directory)
REM
REM Usage:
REM   generate_test_files.bat [all|audio|video|subtitles|playlists]
REM
REM Author: Nicolas DEOUX (NDXDev@gmail.com)
REM License: GPL-2.0
REM ===============================================================================

setlocal EnableDelayedExpansion

REM Directories
set "SCRIPT_DIR=%~dp0"
set "TESTDATA_DIR=%SCRIPT_DIR%TestData"
set "AUDIO_DIR=%TESTDATA_DIR%\audio"
set "VIDEO_DIR=%TESTDATA_DIR%\video"
set "SUBTITLES_DIR=%TESTDATA_DIR%\subtitles"
set "PLAYLISTS_DIR=%TESTDATA_DIR%\playlists"

REM Durations
set DURATION_SHORT=10
set DURATION_MEDIUM=30
set DURATION_LONG=60
set DURATION_VIDEO=10

echo.
echo ========================================
echo 3nity Media - Test File Generator
echo (Windows Batch)
echo ========================================
echo.

REM Check ffmpeg
where ffmpeg >nul 2>nul
if %ERRORLEVEL% neq 0 (
    echo [ERROR] ffmpeg is not installed or not in PATH.
    echo   Download from: https://ffmpeg.org/download.html
    echo   Or use: winget install ffmpeg
    echo   Or use: choco install ffmpeg
    exit /b 1
)
echo [OK] ffmpeg found

REM Create directories
echo [INFO] Creating test data directories...
if not exist "%AUDIO_DIR%" mkdir "%AUDIO_DIR%"
if not exist "%VIDEO_DIR%" mkdir "%VIDEO_DIR%"
if not exist "%SUBTITLES_DIR%" mkdir "%SUBTITLES_DIR%"
if not exist "%PLAYLISTS_DIR%" mkdir "%PLAYLISTS_DIR%"
echo [OK] Directories created

REM Parse arguments
set "MODE=%~1"
if "%MODE%"=="" set "MODE=all"

if /i "%MODE%"=="audio" goto :audio
if /i "%MODE%"=="video" goto :video
if /i "%MODE%"=="subtitles" goto :subtitles
if /i "%MODE%"=="playlists" goto :playlists
if /i "%MODE%"=="all" goto :all

:all
call :generate_audio
call :generate_video
call :generate_subtitles
call :generate_playlists
goto :summary

:audio
call :generate_audio
goto :summary

:video
call :generate_video
goto :summary

:subtitles
call :generate_subtitles
goto :summary

:playlists
call :generate_playlists
goto :summary

REM ===============================================================================
REM Audio File Generation
REM ===============================================================================

:generate_audio
echo.
echo [INFO] Generating audio test files...

echo [INFO]   Creating test_44100_stereo.mp3...
ffmpeg -y -f lavfi -i "sine=frequency=440:duration=%DURATION_MEDIUM%" -f lavfi -i "sine=frequency=880:duration=%DURATION_MEDIUM%" -filter_complex "[0][1]amerge=inputs=2[out]" -map "[out]" -ar 44100 -ac 2 -b:a 192k -metadata title="Test Audio 44.1kHz Stereo" -metadata artist="3nity Media Test Suite" -metadata album="Test Files" "%AUDIO_DIR%\test_44100_stereo.mp3" 2>nul
echo [OK]   test_44100_stereo.mp3 created

echo [INFO]   Creating test_48000_stereo.mp3...
ffmpeg -y -f lavfi -i "sine=frequency=440:duration=%DURATION_MEDIUM%:sample_rate=48000" -f lavfi -i "sine=frequency=880:duration=%DURATION_MEDIUM%:sample_rate=48000" -filter_complex "[0][1]amerge=inputs=2[out]" -map "[out]" -ar 48000 -ac 2 -b:a 256k -metadata title="Test Audio 48kHz Stereo" -metadata artist="3nity Media Test Suite" "%AUDIO_DIR%\test_48000_stereo.mp3" 2>nul
echo [OK]   test_48000_stereo.mp3 created

echo [INFO]   Creating test_vbr.mp3...
ffmpeg -y -f lavfi -i "sine=frequency=440:duration=%DURATION_LONG%" -ar 44100 -ac 2 -q:a 2 -metadata title="Test Audio VBR" -metadata artist="3nity Media Test Suite" "%AUDIO_DIR%\test_vbr.mp3" 2>nul
echo [OK]   test_vbr.mp3 created

echo [INFO]   Creating test_vorbis.ogg...
ffmpeg -y -f lavfi -i "sine=frequency=440:duration=%DURATION_MEDIUM%" -ar 44100 -ac 2 -c:a libvorbis -q:a 5 -metadata title="Test Audio Vorbis" -metadata artist="3nity Media Test Suite" "%AUDIO_DIR%\test_vorbis.ogg" 2>nul
echo [OK]   test_vorbis.ogg created

echo [INFO]   Creating test_lossless.flac...
ffmpeg -y -f lavfi -i "sine=frequency=440:duration=%DURATION_SHORT%" -ar 44100 -ac 2 -c:a flac -metadata title="Test Audio FLAC" -metadata artist="3nity Media Test Suite" "%AUDIO_DIR%\test_lossless.flac" 2>nul
echo [OK]   test_lossless.flac created

echo [INFO]   Creating test_pcm.wav...
ffmpeg -y -f lavfi -i "sine=frequency=440:duration=%DURATION_SHORT%" -ar 44100 -ac 2 -c:a pcm_s16le "%AUDIO_DIR%\test_pcm.wav" 2>nul
echo [OK]   test_pcm.wav created

echo [INFO]   Creating test_aac.m4a...
ffmpeg -y -f lavfi -i "sine=frequency=440:duration=%DURATION_MEDIUM%" -ar 44100 -ac 2 -c:a aac -b:a 192k -metadata title="Test Audio AAC" -metadata artist="3nity Media Test Suite" "%AUDIO_DIR%\test_aac.m4a" 2>nul
echo [OK]   test_aac.m4a created

echo [INFO]   Creating test_opus.opus...
ffmpeg -y -f lavfi -i "sine=frequency=440:duration=%DURATION_MEDIUM%" -ar 48000 -ac 2 -c:a libopus -b:a 128k -metadata title="Test Audio Opus" -metadata artist="3nity Media Test Suite" "%AUDIO_DIR%\test_opus.opus" 2>nul
echo [OK]   test_opus.opus created

echo [INFO]   Creating test_silence.mp3...
ffmpeg -y -f lavfi -i "anullsrc=r=44100:cl=stereo:d=%DURATION_SHORT%" -c:a libmp3lame -b:a 128k -metadata title="Silence" "%AUDIO_DIR%\test_silence.mp3" 2>nul
echo [OK]   test_silence.mp3 created

echo [INFO]   Creating test_short.mp3...
ffmpeg -y -f lavfi -i "sine=frequency=1000:duration=1" -ar 44100 -ac 2 -b:a 192k "%AUDIO_DIR%\test_short.mp3" 2>nul
echo [OK]   test_short.mp3 created

echo [OK] Audio test files generated
goto :eof

REM ===============================================================================
REM Video File Generation
REM ===============================================================================

:generate_video
echo.
echo [INFO] Generating video test files...

echo [INFO]   Creating test_720p.mp4...
ffmpeg -y -f lavfi -i "testsrc=duration=%DURATION_VIDEO%:size=1280x720:rate=30" -f lavfi -i "sine=frequency=440:duration=%DURATION_VIDEO%" -c:v libx264 -preset fast -crf 23 -c:a aac -b:a 128k -pix_fmt yuv420p -metadata title="Test Video 720p" -metadata artist="3nity Media Test Suite" "%VIDEO_DIR%\test_720p.mp4" 2>nul
echo [OK]   test_720p.mp4 created

echo [INFO]   Creating test_1080p.mp4...
ffmpeg -y -f lavfi -i "testsrc=duration=%DURATION_VIDEO%:size=1920x1080:rate=30" -f lavfi -i "sine=frequency=440:duration=%DURATION_VIDEO%" -c:v libx264 -preset fast -crf 23 -c:a aac -b:a 192k -pix_fmt yuv420p -metadata title="Test Video 1080p" "%VIDEO_DIR%\test_1080p.mp4" 2>nul
echo [OK]   test_1080p.mp4 created

echo [INFO]   Creating test_hevc.mkv...
ffmpeg -y -f lavfi -i "testsrc=duration=%DURATION_VIDEO%:size=1920x1080:rate=30" -f lavfi -i "sine=frequency=440:duration=%DURATION_VIDEO%" -c:v libx265 -preset fast -crf 28 -c:a libvorbis -q:a 5 -pix_fmt yuv420p -metadata title="Test Video HEVC" "%VIDEO_DIR%\test_hevc.mkv" 2>nul
if %ERRORLEVEL% neq 0 (
    echo [WARN]   libx265 not available, skipping HEVC test file
) else (
    echo [OK]   test_hevc.mkv created
)

echo [INFO]   Creating test_vp9.webm...
ffmpeg -y -f lavfi -i "testsrc=duration=%DURATION_VIDEO%:size=1280x720:rate=30" -f lavfi -i "sine=frequency=440:duration=%DURATION_VIDEO%" -c:v libvpx-vp9 -crf 30 -b:v 0 -deadline good -c:a libopus -b:a 128k -metadata title="Test Video VP9" "%VIDEO_DIR%\test_vp9.webm" 2>nul
echo [OK]   test_vp9.webm created

echo [INFO]   Creating test_legacy.avi...
ffmpeg -y -f lavfi -i "testsrc=duration=%DURATION_VIDEO%:size=640x480:rate=25" -f lavfi -i "sine=frequency=440:duration=%DURATION_VIDEO%" -c:v mpeg4 -q:v 5 -c:a mp3 -b:a 128k -metadata title="Test Video AVI" "%VIDEO_DIR%\test_legacy.avi" 2>nul
echo [OK]   test_legacy.avi created

echo [INFO]   Creating test_multi_audio.mkv...
ffmpeg -y -f lavfi -i "testsrc=duration=%DURATION_VIDEO%:size=1280x720:rate=30" -f lavfi -i "sine=frequency=440:duration=%DURATION_VIDEO%" -f lavfi -i "sine=frequency=880:duration=%DURATION_VIDEO%" -map 0 -map 1 -map 2 -c:v libx264 -preset fast -crf 23 -c:a:0 aac -b:a:0 128k -metadata:s:a:0 language=eng -metadata:s:a:0 title="English" -c:a:1 aac -b:a:1 128k -metadata:s:a:1 language=fra -metadata:s:a:1 title="French" -pix_fmt yuv420p -metadata title="Test Multi-Audio" "%VIDEO_DIR%\test_multi_audio.mkv" 2>nul
echo [OK]   test_multi_audio.mkv created

echo [INFO]   Creating test_video_only.mp4...
ffmpeg -y -f lavfi -i "testsrc=duration=%DURATION_VIDEO%:size=1280x720:rate=30" -c:v libx264 -preset fast -crf 23 -pix_fmt yuv420p -an -metadata title="Test Video Only" "%VIDEO_DIR%\test_video_only.mp4" 2>nul
echo [OK]   test_video_only.mp4 created

echo [INFO]   Creating test_4x3.mp4...
ffmpeg -y -f lavfi -i "testsrc=duration=%DURATION_VIDEO%:size=640x480:rate=30" -f lavfi -i "sine=frequency=440:duration=%DURATION_VIDEO%" -c:v libx264 -preset fast -crf 23 -c:a aac -b:a 128k -pix_fmt yuv420p -metadata title="Test Video 4:3" "%VIDEO_DIR%\test_4x3.mp4" 2>nul
echo [OK]   test_4x3.mp4 created

echo [OK] Video test files generated
goto :eof

REM ===============================================================================
REM Subtitle File Generation
REM ===============================================================================

:generate_subtitles
echo.
echo [INFO] Generating subtitle test files...

echo [INFO]   Creating test.srt...
(
echo 1
echo 00:00:01,000 --^> 00:00:04,000
echo This is the first subtitle line.
echo.
echo 2
echo 00:00:05,000 --^> 00:00:08,000
echo This is the second subtitle line.
echo With multiple lines of text.
echo.
echo 3
echo 00:00:09,000 --^> 00:00:12,000
echo Testing special characters: eaue n ^(c^) ^(R^) TM
echo.
echo 4
echo 00:00:13,000 --^> 00:00:16,000
echo ^<i^>Italics^</i^> and ^<b^>Bold^</b^> text.
echo.
echo 5
echo 00:00:17,000 --^> 00:00:20,000
echo Last subtitle entry for testing.
) > "%SUBTITLES_DIR%\test.srt"
echo [OK]   test.srt created

echo [INFO]   Creating test.ass...
(
echo [Script Info]
echo Title: Test Subtitles
echo ScriptType: v4.00+
echo Collisions: Normal
echo PlayDepth: 0
echo.
echo [V4+ Styles]
echo Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding
echo Style: Default,Arial,20,^&H00FFFFFF,^&H000000FF,^&H00000000,^&H00000000,0,0,0,0,100,100,0,0,1,2,2,2,10,10,10,1
echo.
echo [Events]
echo Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text
echo Dialogue: 0,0:00:01.00,0:00:04.00,Default,,0,0,0,,This is the first subtitle line.
echo Dialogue: 0,0:00:05.00,0:00:08.00,Default,,0,0,0,,This is the second subtitle line.\NWith multiple lines.
echo Dialogue: 0,0:00:09.00,0:00:12.00,Default,,0,0,0,,Testing ASS formatting: {\b1}Bold{\b0} and {\i1}Italic{\i0}
echo Dialogue: 0,0:00:13.00,0:00:16.00,Default,,0,0,0,,{\c^&H0000FF^&}Red text{\c} normal text
echo Dialogue: 0,0:00:17.00,0:00:20.00,Default,,0,0,0,,Last subtitle entry.
) > "%SUBTITLES_DIR%\test.ass"
echo [OK]   test.ass created

echo [INFO]   Creating test.vtt...
(
echo WEBVTT
echo.
echo 1
echo 00:00:01.000 --^> 00:00:04.000
echo This is the first subtitle line.
echo.
echo 2
echo 00:00:05.000 --^> 00:00:08.000
echo This is the second subtitle line.
echo With multiple lines of text.
echo.
echo 3
echo 00:00:09.000 --^> 00:00:12.000
echo Testing special characters: eaue n ^(c^) ^(R^) TM
echo.
echo 4
echo 00:00:13.000 --^> 00:00:16.000
echo ^<i^>Italics^</i^> and ^<b^>Bold^</b^> text.
echo.
echo 5
echo 00:00:17.000 --^> 00:00:20.000
echo Last subtitle entry for testing.
) > "%SUBTITLES_DIR%\test.vtt"
echo [OK]   test.vtt created

echo [INFO]   Creating test_fr.srt...
(
echo 1
echo 00:00:01,000 --^> 00:00:04,000
echo Ceci est la premiere ligne de sous-titre.
echo.
echo 2
echo 00:00:05,000 --^> 00:00:08,000
echo Ceci est la deuxieme ligne.
echo Avec plusieurs lignes de texte.
echo.
echo 3
echo 00:00:09,000 --^> 00:00:12,000
echo Test des caracteres speciaux: eauc oe ae
echo.
echo 4
echo 00:00:13,000 --^> 00:00:16,000
echo ^<i^>Italique^</i^> et ^<b^>Gras^</b^>.
echo.
echo 5
echo 00:00:17,000 --^> 00:00:20,000
echo Derniere entree de sous-titre.
) > "%SUBTITLES_DIR%\test_fr.srt"
echo [OK]   test_fr.srt created

echo [OK] Subtitle test files generated
goto :eof

REM ===============================================================================
REM Playlist File Generation
REM ===============================================================================

:generate_playlists
echo.
echo [INFO] Generating playlist test files...

echo [INFO]   Creating test_simple.m3u...
(
echo %AUDIO_DIR%\test_44100_stereo.mp3
echo %AUDIO_DIR%\test_vorbis.ogg
echo %AUDIO_DIR%\test_lossless.flac
) > "%PLAYLISTS_DIR%\test_simple.m3u"
echo [OK]   test_simple.m3u created

echo [INFO]   Creating test_extended.m3u...
(
echo #EXTM3U
echo #EXTINF:30,Test Audio 44.1kHz Stereo - 3nity Media Test Suite
echo %AUDIO_DIR%\test_44100_stereo.mp3
echo #EXTINF:30,Test Audio Vorbis - 3nity Media Test Suite
echo %AUDIO_DIR%\test_vorbis.ogg
echo #EXTINF:10,Test Audio FLAC - 3nity Media Test Suite
echo %AUDIO_DIR%\test_lossless.flac
echo #EXTINF:30,Test Audio AAC - 3nity Media Test Suite
echo %AUDIO_DIR%\test_aac.m4a
) > "%PLAYLISTS_DIR%\test_extended.m3u"
echo [OK]   test_extended.m3u created

echo [INFO]   Creating test_utf8.m3u8...
(
echo #EXTM3U
echo #EXTINF:30,Piste Audio Francaise
echo %AUDIO_DIR%\test_44100_stereo.mp3
echo #EXTINF:30,Japanese Test
echo %AUDIO_DIR%\test_vorbis.ogg
echo #EXTINF:10,Russian Test
echo %AUDIO_DIR%\test_lossless.flac
) > "%PLAYLISTS_DIR%\test_utf8.m3u8"
echo [OK]   test_utf8.m3u8 created

echo [INFO]   Creating test.pls...
(
echo [playlist]
echo File1=%AUDIO_DIR%\test_44100_stereo.mp3
echo Title1=Test Audio 44.1kHz Stereo
echo Length1=30
echo File2=%AUDIO_DIR%\test_vorbis.ogg
echo Title2=Test Audio Vorbis
echo Length2=30
echo File3=%AUDIO_DIR%\test_lossless.flac
echo Title3=Test Audio FLAC
echo Length3=10
echo NumberOfEntries=3
echo Version=2
) > "%PLAYLISTS_DIR%\test.pls"
echo [OK]   test.pls created

echo [INFO]   Creating test_video.m3u...
(
echo #EXTM3U
echo #EXTINF:10,Test Video 720p
echo %VIDEO_DIR%\test_720p.mp4
echo #EXTINF:10,Test Video 1080p
echo %VIDEO_DIR%\test_1080p.mp4
echo #EXTINF:10,Test Video VP9
echo %VIDEO_DIR%\test_vp9.webm
) > "%PLAYLISTS_DIR%\test_video.m3u"
echo [OK]   test_video.m3u created

echo [INFO]   Creating test_mixed.m3u...
(
echo #EXTM3U
echo #EXTINF:30,Audio Track
echo %AUDIO_DIR%\test_44100_stereo.mp3
echo #EXTINF:10,Video Track
echo %VIDEO_DIR%\test_720p.mp4
echo #EXTINF:30,Another Audio
echo %AUDIO_DIR%\test_vorbis.ogg
) > "%PLAYLISTS_DIR%\test_mixed.m3u"
echo [OK]   test_mixed.m3u created

echo [INFO]   Creating test_empty.m3u...
echo #EXTM3U> "%PLAYLISTS_DIR%\test_empty.m3u"
echo [OK]   test_empty.m3u created

echo [INFO]   Creating test_streams.m3u...
(
echo #EXTM3U
echo #EXTINF:-1,SomaFM - Groove Salad
echo http://ice1.somafm.com/groovesalad-128-mp3
echo #EXTINF:-1,SomaFM - Drone Zone
echo http://ice1.somafm.com/dronezone-128-mp3
echo #EXTINF:-1,SomaFM - Deep Space One
echo http://ice1.somafm.com/deepspaceone-128-mp3
) > "%PLAYLISTS_DIR%\test_streams.m3u"
echo [OK]   test_streams.m3u created

echo [OK] Playlist test files generated
goto :eof

REM ===============================================================================
REM Summary
REM ===============================================================================

:summary
echo.
echo ========================================
echo Test Files Generation Summary
echo ========================================
echo.

if exist "%AUDIO_DIR%" (
    echo Audio files:
    for %%f in ("%AUDIO_DIR%\*.*") do echo   %%~nxf
    echo.
)

if exist "%VIDEO_DIR%" (
    echo Video files:
    for %%f in ("%VIDEO_DIR%\*.*") do echo   %%~nxf
    echo.
)

if exist "%SUBTITLES_DIR%" (
    echo Subtitle files:
    for %%f in ("%SUBTITLES_DIR%\*.*") do echo   %%~nxf
    echo.
)

if exist "%PLAYLISTS_DIR%" (
    echo Playlist files:
    for %%f in ("%PLAYLISTS_DIR%\*.*") do echo   %%~nxf
    echo.
)

echo [OK] All test files generated successfully!
echo.

endlocal
