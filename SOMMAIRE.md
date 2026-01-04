# 3nity Media - Table of Contents

## Documentation

- [README.md](/README.md) - Project overview and quick start
- [INSTALL](/INSTALL) - Quick installation guide (included in releases)
- [LICENSE](/LICENSE) - GPL-2.0 License
- [SOMMAIRE.md](/SOMMAIRE.md) - This file

---

## Source Code

### Main Application

| File | Description |
|------|-------------|
| [src/TrinityMedia.lpr](/src/TrinityMedia.lpr) | Main program entry point |
| [src/TrinityMedia.lpi](/src/TrinityMedia.lpi) | Lazarus project file |

### Core Modules

| File | Description |
|------|-------------|
| [src/Core/uLibMPV.pas](/src/Core/uLibMPV.pas) | libmpv dynamic loading and bindings |
| [src/Core/uMPVConst.pas](/src/Core/uMPVConst.pas) | MPV constants and types |
| [src/Core/uMPVEngine.pas](/src/Core/uMPVEngine.pas) | MPV engine wrapper |
| [src/Core/uPlaylistManager.pas](/src/Core/uPlaylistManager.pas) | Playlist management |
| [src/Core/uRadioManager.pas](/src/Core/uRadioManager.pas) | Internet radio/Icecast support |
| [src/Core/uStreamRecorder.pas](/src/Core/uStreamRecorder.pas) | Stream recording functionality |
| [src/Core/uVisualEffects.pas](/src/Core/uVisualEffects.pas) | Audio visualizations |
| [src/Core/uAppVersion.pas](/src/Core/uAppVersion.pas) | Application version management |

### Forms (User Interface)

| File | Description |
|------|-------------|
| [src/Forms/uMainForm.pas](/src/Forms/uMainForm.pas) | Main application window |
| [src/Forms/uPlaylist.pas](/src/Forms/uPlaylist.pas) | Playlist window |
| [src/Forms/uEqualizer.pas](/src/Forms/uEqualizer.pas) | Audio equalizer |
| [src/Forms/uVideoAdjust.pas](/src/Forms/uVideoAdjust.pas) | Video adjustments |
| [src/Forms/uOptions.pas](/src/Forms/uOptions.pas) | Settings/preferences |
| [src/Forms/uAbout.pas](/src/Forms/uAbout.pas) | About dialog |
| [src/Forms/uMediaInfo.pas](/src/Forms/uMediaInfo.pas) | Media information display |
| [src/Forms/uRadios.pas](/src/Forms/uRadios.pas) | Internet radio browser |
| [src/Forms/uHistory.pas](/src/Forms/uHistory.pas) | Playback history |
| [src/Forms/uBookmarks.pas](/src/Forms/uBookmarks.pas) | Bookmark management |
| [src/Forms/uFavorites.pas](/src/Forms/uFavorites.pas) | Favorites management |
| [src/Forms/uGotoTime.pas](/src/Forms/uGotoTime.pas) | Go to time dialog |
| [src/Forms/uOpenURL.pas](/src/Forms/uOpenURL.pas) | Open URL dialog |
| [src/Forms/uSleepTimer.pas](/src/Forms/uSleepTimer.pas) | Sleep timer |
| [src/Forms/uShortcutsEditor.pas](/src/Forms/uShortcutsEditor.pas) | Keyboard shortcuts editor |
| [src/Forms/uLog.pas](/src/Forms/uLog.pas) | Debug log viewer |

### Common Utilities

| File | Description |
|------|-------------|
| [src/Common/uTypes.pas](/src/Common/uTypes.pas) | Common types and enumerations |
| [src/Common/uConstants.pas](/src/Common/uConstants.pas) | Application constants |
| [src/Common/uConfig.pas](/src/Common/uConfig.pas) | Configuration management |
| [src/Common/uShortcuts.pas](/src/Common/uShortcuts.pas) | Keyboard shortcuts |
| [src/Common/uFileUtils.pas](/src/Common/uFileUtils.pas) | File utilities |

### Localization

| File | Description |
|------|-------------|
| [src/Locale/uLocale.pas](/src/Locale/uLocale.pas) | Localization system |

### Custom Controls

| File | Description |
|------|-------------|
| [src/Controls/uVssScrollbar.pas](/src/Controls/uVssScrollbar.pas) | Custom scrollbar control |

---

## Tests

> **1,686 tests** across all categories (100% passing)

### Test Framework

| File | Description |
|------|-------------|
| [tests/TestRunner.lpr](/tests/TestRunner.lpr) | Main test runner program |
| [tests/TestRunner.lpi](/tests/TestRunner.lpi) | Test project configuration |
| [tests/README.md](/tests/README.md) | Test suite documentation |

### Unit Tests (940+ tests)

| File | Description |
|------|-------------|
| [tests/Unit/uTestCLIParams.pas](/tests/Unit/uTestCLIParams.pas) | CLI parameters tests (11) |
| [tests/Unit/uTestConfig.pas](/tests/Unit/uTestConfig.pas) | Configuration tests (41) |
| [tests/Unit/uTestPlaylistManager.pas](/tests/Unit/uTestPlaylistManager.pas) | Playlist manager tests (97) |
| [tests/Unit/uTestRadioManager.pas](/tests/Unit/uTestRadioManager.pas) | Radio/streaming tests (67) |
| [tests/Unit/uTestStreamRecorder.pas](/tests/Unit/uTestStreamRecorder.pas) | Stream recording tests (87) |
| [tests/Unit/uTestVisualEffects.pas](/tests/Unit/uTestVisualEffects.pas) | Visualization tests (166) |
| [tests/Unit/uTestShortcuts.pas](/tests/Unit/uTestShortcuts.pas) | Keyboard shortcuts tests (151) |
| [tests/Unit/uTestLocale.pas](/tests/Unit/uTestLocale.pas) | Localization tests (50) |
| [tests/Unit/uTestMPVConst.pas](/tests/Unit/uTestMPVConst.pas) | MPV constants tests (156) |
| [tests/Unit/uTestMPVEngine.pas](/tests/Unit/uTestMPVEngine.pas) | MPV engine tests (114) |
| [tests/Unit/uTestMockMPVBehavior.pas](/tests/Unit/uTestMockMPVBehavior.pas) | Mock MPV behavior tests |
| [tests/Unit/uTestMockPlaylistBehavior.pas](/tests/Unit/uTestMockPlaylistBehavior.pas) | Mock playlist behavior tests |

### Integration Tests (198+ tests)

| File | Description |
|------|-------------|
| [tests/Integration/uTestPlayback.pas](/tests/Integration/uTestPlayback.pas) | Full playback tests (~80) |
| [tests/Integration/uTestAudioVideo.pas](/tests/Integration/uTestAudioVideo.pas) | Audio/video track tests (~60) |
| [tests/Integration/uTestStreaming.pas](/tests/Integration/uTestStreaming.pas) | Streaming tests (~30) |
| [tests/Integration/uTestVisualization.pas](/tests/Integration/uTestVisualization.pas) | Visualization integration (~28) |
| [tests/Integration/uTestMPVPlaylistIntegration.pas](/tests/Integration/uTestMPVPlaylistIntegration.pas) | MPV + Playlist integration |
| [tests/Integration/uTestErrorScenarios.pas](/tests/Integration/uTestErrorScenarios.pas) | Error handling tests |

### Performance Tests (252+ tests)

| File | Description |
|------|-------------|
| [tests/Performance/uTestStartup.pas](/tests/Performance/uTestStartup.pas) | Startup time benchmarks |
| [tests/Performance/uTestMediaLoading.pas](/tests/Performance/uTestMediaLoading.pas) | Media loading performance |
| [tests/Performance/uTestSeekPerformance.pas](/tests/Performance/uTestSeekPerformance.pas) | Seek operation benchmarks |
| [tests/Performance/uTestMemoryUsage.pas](/tests/Performance/uTestMemoryUsage.pas) | Memory allocation tests |

### Robustness Tests (296+ tests)

| File | Description |
|------|-------------|
| [tests/Robustness/uTestCorruptedFiles.pas](/tests/Robustness/uTestCorruptedFiles.pas) | Corrupted file handling |
| [tests/Robustness/uTestNetworkTimeout.pas](/tests/Robustness/uTestNetworkTimeout.pas) | Network error recovery |
| [tests/Robustness/uTestRapidCommands.pas](/tests/Robustness/uTestRapidCommands.pas) | Rapid command stability |
| [tests/Robustness/uTestLongPlayback.pas](/tests/Robustness/uTestLongPlayback.pas) | Extended playback tests |

### Functional Tests (planned)

| File | Description |
|------|-------------|
| tests/Functional/uTestMainForm.pas | Main form tests |
| tests/Functional/uTestPlaylistForm.pas | Playlist form tests |
| tests/Functional/uTestEqualizerForm.pas | Equalizer form tests |

### Mocks

| File | Description |
|------|-------------|
| [tests/Mocks/uMockMPVEngine.pas](/tests/Mocks/uMockMPVEngine.pas) | MPV engine mock (TMockMPVEngine) |
| [tests/Mocks/uMockPlaylist.pas](/tests/Mocks/uMockPlaylist.pas) | Playlist mock (TMockPlaylistManager) |

### Test Data

| Directory | Description |
|-----------|-------------|
| [tests/TestData/](/tests/TestData/) | Test data root |
| tests/TestData/audio/ | Audio test files |
| tests/TestData/video/ | Video test files |
| tests/TestData/subtitles/ | Subtitle test files |
| tests/TestData/playlists/ | Playlist test files |

### Test File Generation Scripts

| Script | Platform | Description |
|--------|----------|-------------|
| [tests/generate_test_files.sh](/tests/generate_test_files.sh) | Linux (bash) | Native bash script |
| [tests/generate_test_files_linux.ps1](/tests/generate_test_files_linux.ps1) | Linux (PowerShell) | PowerShell Core script |
| [tests/generate_test_files_win.ps1](/tests/generate_test_files_win.ps1) | Windows (PowerShell) | PowerShell 5.1+ script |
| [tests/generate_test_files.bat](/tests/generate_test_files.bat) | Windows (CMD) | Batch script |

---

## Documentation

### English (docs/)

| File | Description |
|------|-------------|
| [docs/USER_GUIDE.md](/docs/USER_GUIDE.md) | User guide |
| [docs/INSTALL.md](/docs/INSTALL.md) | Installation instructions |
| [docs/CLI_PARAMETERS.md](/docs/CLI_PARAMETERS.md) | Command-line parameters |
| [docs/CONFIG.md](/docs/CONFIG.md) | Configuration reference |
| [docs/TESTING.md](/docs/TESTING.md) | Test documentation |
| [docs/ARCHITECTURE.md](/docs/ARCHITECTURE.md) | Architecture overview |
| [docs/CONTRIBUTING.md](/docs/CONTRIBUTING.md) | Contribution guidelines |
| [docs/API_REFERENCE.md](/docs/API_REFERENCE.md) | API reference |
| [docs/WORKFLOWS.md](/docs/WORKFLOWS.md) | GitHub Actions CI/CD workflows |
| [docs/FLATHUB.md](/docs/FLATHUB.md) | Publishing to Flathub |
| [docs/SNAPCRAFT.md](/docs/SNAPCRAFT.md) | Publishing to Snap Store |

### Developer Documentation (docs/)

| File | Description |
|------|-------------|
| [docs/CICD_DEVELOPER.md](/docs/CICD_DEVELOPER.md) | CI/CD pipeline documentation |
| [docs/LOCALIZATION_DEVELOPER.md](/docs/LOCALIZATION_DEVELOPER.md) | Localization system |
| [docs/SHORTCUTS_DEVELOPER.md](/docs/SHORTCUTS_DEVELOPER.md) | Shortcuts system |
| [docs/OPTIONS_DEVELOPER.md](/docs/OPTIONS_DEVELOPER.md) | Options system |
| [docs/VERSIONING_DEVELOPER.md](/docs/VERSIONING_DEVELOPER.md) | Version management |
| [docs/RESOURCES_DEVELOPER.md](/docs/RESOURCES_DEVELOPER.md) | Embedded resources (images) |
| [docs/LOCALE_PATHS_DEVELOPER.md](/docs/LOCALE_PATHS_DEVELOPER.md) | Locale paths |
| [docs/DEV_VISUALIZATIONS.md](/docs/DEV_VISUALIZATIONS.md) | Visualizations system |
| [docs/DEV_VISUALIZATION_PLAYBACK_FIX.md](/docs/DEV_VISUALIZATION_PLAYBACK_FIX.md) | Visualization fix notes |

### Other Languages

Documentation is also available in other languages in subdirectories:

| Directory | Language |
|-----------|----------|
| [docs/french/](/docs/french/) | French (Français) |

---

## Build System

| File | Description |
|------|-------------|
| [Makefile](/Makefile) | Build automation |

### GitHub Actions Workflows

| File | Description |
|------|-------------|
| [.github/workflows/README.md](/.github/workflows/README.md) | Workflows documentation |
| [.github/workflows/build.yml](/.github/workflows/build.yml) | Main CI/CD pipeline |
| [.github/workflows/check-linux-deps.yml](/.github/workflows/check-linux-deps.yml) | Linux dependencies check |
| [.github/workflows/check-windows-deps.yml](/.github/workflows/check-windows-deps.yml) | Windows dependencies check |

### Make Targets

| Target | Description |
|--------|-------------|
| `make` | Build application (Debug) |
| `make build-release` | Build application (Release) |
| `make build-tests` | Build test runner |
| `make test` | Run all tests |
| `make quick` | Run unit tests only |
| `make run` | Build and run application |
| `make clean` | Clean test files |
| `make clean-all` | Clean all build artifacts |
| `make help` | Show all available targets |

---

## Output Directories

| Directory | Description |
|-----------|-------------|
| bin/x86_64-linux/ | Linux binaries |
| bin/x86_64-win64/ | Windows binaries |
| lib/ | Compiled units |
| releases/ | Release packages |
| tests/results/ | Test results |

---

## Configuration Files

| File | Description |
|------|-------------|
| 3nity.ini | User configuration (created at runtime) |
| shortcuts.ini | Custom keyboard shortcuts |
| history.json | Playback history |
| favorites.json | User favorites |

---

## Resources

| Directory | Description |
|-----------|-------------|
| resources/ | Application icons and assets |
| src/Forms/logo.lrs | Embedded logo resource (for About dialog) |

### Regenerating logo.lrs

If you modify `resources/logo.png`, regenerate the embedded resource with:

```bash
lazres src/Forms/logo.lrs resources/logo.png=logo
```

---

## Version Information

| File | Description |
|------|-------------|
| [src/Core/uVersion.inc](/src/Core/uVersion.inc) | Auto-generated version constants |

Generated by Makefile with:
- `APP_VERSION` - Version from git tag
- `APP_BUILD_DATE` - Build date
- `APP_BUILD_TIME` - Build time
- `APP_GIT_COMMIT` - Git commit hash
- `APP_GIT_BRANCH` - Git branch name

---

## Package Configuration

### Flatpak

| File | Description |
|------|-------------|
| [flatpak/com.ndxdev.3nity-media.yml](/flatpak/com.ndxdev.3nity-media.yml) | Flatpak manifest |
| [flatpak/com.ndxdev.3nity-media.desktop](/flatpak/com.ndxdev.3nity-media.desktop) | Desktop file |
| [flatpak/com.ndxdev.3nity-media.metainfo.xml](/flatpak/com.ndxdev.3nity-media.metainfo.xml) | AppStream metadata |

### Snap

| File | Description |
|------|-------------|
| [snap/README.md](/snap/README.md) | Snap build instructions |
| snap/snapcraft.yaml | Snapcraft manifest (generated by CI/CD) |
| snap/gui/3nity-media.desktop | Desktop file |

---

## Packages (CI/CD Generated)

### Linux

| Package | Description |
|---------|-------------|
| 3nity-media_X.Y.Z_amd64.deb | Debian/Ubuntu package |
| 3nity-Media-X.Y.Z-x86_64.AppImage | AppImage (portable) |
| 3nity-media_X.Y.Z_amd64.snap | Snap package |
| 3nity-media-X.Y.Z.flatpak | Flatpak package |
| 3nity-media-linux-portable-X.Y.Z.tar.gz | Portable archive |

### Windows

| Package | Description |
|---------|-------------|
| 3nity-Media-Setup-X.Y.Z.exe | System installer |
| 3nity-Media-Setup-User-X.Y.Z.exe | User installer (no admin) |
| 3nity-media-windows-portable-X.Y.Z.zip | Portable archive |

---

<div align="center">

**[Back to README](/README.md)**

</div>
