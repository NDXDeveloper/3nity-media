# Changelog

All notable changes to 3nity Media are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [Unreleased]

---

## [0.2.4] - 2026-01-10

### Fixed
- Snap: Auto-detect OS language on first launch (was defaulting to English)
- Snap: Add gsettings-desktop-schemas to fix GSettings crash

---

## [0.2.3] - 2026-01-09

### Changed
- Documentation: Add Snap Store as recommended installation method
- Documentation: Add screenshots to README

---

## [0.2.2] - 2026-01-08

### Fixed
- Snap: Move store assets out of snap/gui to fix build

---

## [0.2.1] - 2026-01-08

### Added
- Snap Store assets (icon, screenshots)

---

## [0.2.0] - 2026-01-07

### Added
- **Drag & drop support for media files**
  - Drop on video panel: add files to playlist and play first item immediately
  - Drop elsewhere on main window: add files to playlist without playing
  - Drop on playlist window: add files to playlist without playing
  - Folders are scanned recursively (max 10 levels depth)
  - Only media files accepted (playlist files like M3U, PLS, XSPF are ignored)
  - Files sorted alphabetically and added at end of playlist

---

## [0.1.70] - 2026-01-07

### Changed
- Windows installer: Desktop shortcut option enabled by default

---

## [0.1.69] - 2026-01-07

### Fixed
- Build: Added wget for OpenSSL download in CI/CD

---

## [0.1.68] - 2026-01-07

### Fixed
- Snap/AppImage: Uses OpenSSL 1.1.x for FPC HTTPS support

---

## [0.1.67] - 2026-01-07

### Fixed
- Snap: Switched to OpenSSL 1.1.x instead of 3.x for compatibility

---

## [0.1.66] - 2026-01-07

### Fixed
- Snap: Removed cleanup step to debug libmpv issue

---

## [0.1.65] - 2026-01-07

### Fixed
- Snap: Clear snapcraft cache before build to avoid stale package references

---

## [0.1.64] - 2026-01-07

### Fixed
- Snap: Updated documentation paths

---

## [0.1.63] - 2026-01-07

### Fixed
- Snap: Aligned local configuration with CI/CD

---

## [0.1.62] - 2026-01-07

### Fixed
- Snap: Added cleanup procedures to CI/CD

---

## [0.1.61] - 2026-01-07

### Fixed
- Snap: SSL certificates configuration

---

## [0.1.60] - 2026-01-07

### Fixed
- Snap: SSL support for Icecast streams
- Snap: Qt plugin configuration

---

## [0.1.59] - 2026-01-07

### Fixed
- Snap: Remove prime section filtering Qt plugins

---

## [0.1.58] - 2026-01-07

### Fixed
- Snap: Configuration for CI/CD

---

## [0.1.57] - 2026-01-07

### Fixed
- Flatpak: Use KDE Platform 5.15 (Qt5) instead of 6.x (Qt6)

---

## [0.1.56] - 2026-01-07

### Added
- Developer documentation: QUICKSTART_DEVELOPER.md, COOKBOOK.md, DEPENDENCIES.md, TROUBLESHOOTING_DEV.md
- DVD/Blu-ray architecture section in ARCHITECTURE.md
- DVD folder playback with automatic fallback mode
- Blu-ray folder playback with automatic fallback mode
- DVD/Blu-ray translations for all 99 language files
- VOB and M2TS file hr-seek support for accurate seeking

### Fixed
- DVD fallback now uses playlist approach (MPV doesn't support concat: protocol)
- Blu-ray fallback plays largest M2TS file from BDMV/STREAM folder

---

## [0.1.55] - 2026-01-07

### Fixed
- Windows: Seekbar locale issue - use dot as decimal separator for MPV commands

---

## [0.1.54] - 2026-01-06

### Fixed
- Windows: Equalizer mouse wheel support for band sliders
- Windows: Media info dialog deadlock issue

---

## [0.1.53] - 2026-01-06

### Fixed
- Windows: Equalizer locale issue - use dot as decimal separator for FFmpeg filter

---

## [0.1.52] - 2026-01-06

### Changed
- Disabled AlwaysOnTop feature (unreliable across platforms)

---

## [0.1.51] - 2026-01-06

### Fixed
- Windows: Fullscreen using SetWindowLong to preserve MPV rendering context

---

## [0.1.50] - 2026-01-06

### Fixed
- Windows: Fullscreen using bsNone + wsMaximized to properly hide taskbar

---

## [0.1.49] - 2026-01-06

### Fixed
- Windows: OpenSSL updated to ICS/Overbyte 1.1.1w (FireDaemon 404 error)

---

## [0.1.46] - 2026-01-06

### Fixed
- Windows: AlwaysOnTop implementation with WS_EX_TOPMOST
- Windows: OpenSSL 1.1.x support for FPC 3.2.2
- Windows: HTTPS support via OpenSSL download from FireDaemon

---

## [0.1.42] - 2026-01-06

### Fixed
- Windows: FPU exception masking for libmpv compatibility
- Windows: HWND type casting and Windows unit conflicts
- Windows: Correct libmpv DLL name (mpv-2.dll)

---

## [0.1.38] - 2026-01-05

### Fixed
- Windows: Win32 widgetset to avoid Qt5 DLL dependencies
- Seekbar: Prevent double audio playback on seek click

---

## [0.1.35] - 2026-01-05

### Fixed
- Flatpak: Free disk space before build
- Windows: Remove hardcoded LCLQT5 from project options

---

## [0.1.30] - 2026-01-05

### Fixed
- Snap: Add udisks2, pulseaudio, screen-inhibit plugs
- Snap: Qt5 menu bar black background in Snap environment
- Snap: Proper Qt theming with kde-neon extension

---

## [0.1.20] - 2026-01-05

### Fixed
- Snap: Core24 base for libmpv2 compatibility
- Snap: LD_LIBRARY_PATH configuration
- Flatpak: Valid app-id and SDK 6.7/6.8

---

## [0.1.10] - 2026-01-04

### Added
- Complete installation documentation (INSTALL.md)
- Snap Store and Flathub publishing instructions

### Fixed
- RPM: Accept ffmpeg-free on Fedora
- Release builds: Disable debug output

---

## [0.1.5] - 2026-01-04

### Fixed
- Windows build: ShellApi unit for ShellExecute
- Windows build: TCriticalSection conflict between SyncObjs and Windows units
- Windows build: Version file generation with make

---

## [0.1.1] - 2026-01-03

### Added
- CI/CD workflows for automated builds
- DEB, AppImage, Snap, Flatpak packaging for Linux
- Windows installer (system and user) and portable builds

---

## [0.1.0] - 2026-01-03

### Added
- Initial release of 3nity Media - Lazarus Edition
- Media playback (audio/video) via libmpv
- Playlist management (M3U, M3U8, PLS, XSPF formats)
- Internet radio browser with Icecast directory
- 10-band audio equalizer with presets
- Audio visualizations
- Video adjustments (brightness, contrast, saturation, hue, gamma)
- Stream recording to disk
- 99 language translations
- Customizable keyboard shortcuts
- Playback history with resume support
- Position bookmarks per file
- Favorites for files and radio stations
- Sleep timer
- Screenshot capture
- Drag and drop support
- Command-line interface
- Cross-platform support (Linux with Qt5, Windows with Win32)

### Technical
- Hardware acceleration support (hwdec=auto)
- Dynamic libmpv loading
- INI-based configuration persistence
- 1,686 automated tests

---

## Version History Summary

| Version | Date | Highlights |
|---------|------|------------|
| 0.2.4 | 2026-01-10 | **Snap: Locale auto-detection + GSettings fix** |
| 0.2.1-3 | 2026-01-08-09 | Snap Store assets and documentation |
| 0.2.0 | 2026-01-07 | **Drag & drop support for media files** |
| 0.1.70 | 2026-01-07 | Windows installer desktop shortcut default |
| 0.1.57-69 | 2026-01-07 | Snap/Flatpak/AppImage OpenSSL and Qt fixes |
| 0.1.56 | 2026-01-07 | DVD/Blu-ray folder playback |
| 0.1.50-55 | 2026-01-06-07 | Windows fixes (fullscreen, equalizer, seekbar) |
| 0.1.40-49 | 2026-01-06 | Windows OpenSSL/HTTPS, AlwaysOnTop |
| 0.1.30-39 | 2026-01-05 | Windows build, FPU exceptions, seekbar |
| 0.1.20-29 | 2026-01-05 | Snap/Flatpak fixes, Qt theming |
| 0.1.10-19 | 2026-01-04-05 | Core24, packaging improvements |
| 0.1.1-9 | 2026-01-03-04 | Windows build fixes, CI/CD |
| 0.1.0 | 2026-01-03 | Initial release |

---

## Links

- [GitHub Releases](https://github.com/NDXDeveloper/3nity-media/releases)
- [Issue Tracker](https://github.com/NDXDeveloper/3nity-media/issues)
- [Full Commit History](https://github.com/NDXDeveloper/3nity-media/commits/main)

---

*Last updated: 2026-01-10 (v0.2.4)*
