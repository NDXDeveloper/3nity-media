# CI/CD Developer Documentation

This document describes the GitHub Actions CI/CD pipeline for 3nity Media, including dependency verification, build process, and release automation.

## Workflow Files

| File | Purpose | Trigger |
|------|---------|---------|
| `build.yml` | Main build pipeline | Push to main/develop, tags, PRs |
| `check-linux-deps.yml` | Linux dependencies health check | Weekly (Monday 6:00 UTC), manual |
| `check-windows-deps.yml` | Windows dependencies health check | Weekly (Monday 6:00 UTC), manual |

## Build Pipeline Structure

```
preflight-check-linux        preflight-check-windows
         │                            │
         ▼                            │
    build-linux ◄─────────────────────┤
         │                            │
         │                            ▼
         └──────────────────► build-windows
                                     │
                   ┌─────────────────┘
                   ▼
            create-release (tags only)
```

### Job Dependencies

| Job | Depends On | Description |
|-----|------------|-------------|
| `preflight-check-linux` | - | Verifies Linux APT packages availability |
| `preflight-check-windows` | - | Verifies Windows download URLs |
| `build-linux` | `preflight-check-linux` | Compiles Linux binaries and packages |
| `build-windows` | `preflight-check-windows` + `build-linux` | Compiles Windows binaries and installers |
| `create-release` | `build-linux` + `build-windows` | Creates GitHub release (tags only) |

### Behavior

- Both preflight checks run **in parallel**
- If Linux preflight fails → Linux build blocked, but Windows preflight continues
- If Windows preflight fails → Windows build blocked, but Linux build can succeed
- `build-windows` waits for `build-linux` to get the version number

## Verified Dependencies

### Linux Dependencies (APT Packages)

| Package | Purpose | Required |
|---------|---------|----------|
| `libmpv2` or `libmpv1` | MPV media playback library | Yes |
| `libqt5pas1` | Qt5 Pascal bindings | Yes |
| `ffmpeg` | Multimedia framework (includes ffprobe) | Yes |
| `lazarus-ide-qt5` | Lazarus IDE with Qt5 widgetset | Yes (build only) |
| `libqt5pas-dev` | Qt5 Pascal development files | Yes (build only) |
| `libmpv-dev` | MPV development files | Yes (build only) |

### Windows Dependencies (External URLs)

| Component | Source | URL Pattern |
|-----------|--------|-------------|
| libmpv-2.dll | SourceForge | `https://sourceforge.net/projects/mpv-player-windows/files/libmpv/mpv-dev-x86_64-YYYYMMDD-git-HASH.7z/download` |
| ffprobe.exe | gyan.dev | `https://www.gyan.dev/ffmpeg/builds/ffmpeg-release-essentials.zip` |

#### libmpv Download Strategy

The Windows build dynamically discovers the latest libmpv version:

1. Scrapes SourceForge directory listing
2. Finds latest `mpv-dev-x86_64-YYYYMMDD-git-HASH.7z` file
3. Falls back to known working version if scraping fails

```powershell
# Pattern used to find latest version
$pattern = 'mpv-dev-x86_64-\d{8}-git-[a-f0-9]+\.7z'
```

#### Fallback URL

If dynamic discovery fails:
```
https://sourceforge.net/projects/mpv-player-windows/files/libmpv/mpv-dev-x86_64-20251228-git-a58dd8a.7z/download
```

## Build Outputs

### Linux Packages

| Format | File Pattern | Target |
|--------|--------------|--------|
| DEB | `3nity-media_VERSION_amd64.deb` | Debian, Ubuntu, Mint |
| AppImage | `3nity-Media-VERSION-x86_64.AppImage` | Universal portable |
| Snap | `3nity-media_VERSION_amd64.snap` | Snap Store |
| Portable | `3nity-media-linux-portable-VERSION.tar.gz` | Generic Linux |

### Windows Packages

| Format | File Pattern | Target |
|--------|--------------|--------|
| Installer (Admin) | `3nity-Media-Setup-VERSION.exe` | System-wide install |
| Installer (User) | `3nity-Media-Setup-User-VERSION.exe` | Per-user install (no admin) |
| Portable | `3nity-media-windows-portable-VERSION.zip` | No installation required |

### Windows Package Contents

All Windows packages include:
- `3nity.exe` - Main application
- `libmpv-2.dll` - MPV library
- `ffprobe.exe` - Metadata extraction tool
- `README.md` - Documentation

## Health Check Workflows

### check-linux-deps.yml

Runs weekly to verify:

**Bash checks:**
- APT package availability (libmpv, libqt5pas1, ffmpeg)
- Actual installation test
- Source URLs (FFmpeg, MPV)

**PowerShell checks:**
- Same APT packages via pwsh
- Source URLs
- AppImage tools (linuxdeploy, appimagetool)

### check-windows-deps.yml

Runs weekly to verify:

**Bash checks:**
- SourceForge libmpv directory scraping
- Download URL accessibility
- gyan.dev ffprobe URL

**PowerShell checks:**
- Same URL checks
- Actual download and extraction test
- File verification (libmpv-2.dll, ffprobe.exe exist)

### Failure Notification

If any health check fails:
1. GitHub Issue is automatically created
2. Labels: `dependencies`, `automated`, `linux`/`windows`
3. Subsequent failures add comments to existing issue

## Verified URLs

### Currently Verified (as of build)

| URL | Status | Purpose |
|-----|--------|---------|
| `https://sourceforge.net/projects/mpv-player-windows/files/libmpv/` | Active | libmpv directory |
| `https://www.gyan.dev/ffmpeg/builds/ffmpeg-release-essentials.zip` | Active | FFmpeg/ffprobe |
| `https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage` | Active | AppImage builder |
| `https://github.com/linuxdeploy/linuxdeploy-plugin-qt/releases/download/continuous/linuxdeploy-plugin-qt-x86_64.AppImage` | Active | Qt plugin for AppImage |
| `https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage` | Active | AppImage tool |

## Manual Workflow Triggers

All workflows support manual triggering via `workflow_dispatch`:

```bash
# Trigger via GitHub CLI
gh workflow run build.yml
gh workflow run check-linux-deps.yml
gh workflow run check-windows-deps.yml
```

Or via GitHub UI: Actions → Select workflow → Run workflow

## Troubleshooting

### Preflight Check Failures

**Linux APT package not found:**
- Check Ubuntu repository status
- Verify package name hasn't changed
- Check if package was moved to universe/multiverse

**SourceForge libmpv not found:**
- Check if mpv-player-windows project is still active
- Verify file naming pattern hasn't changed
- Try fallback URL manually

**gyan.dev unreachable:**
- Check if site is temporarily down
- Look for alternative FFmpeg Windows builds
- Consider BtbN builds as alternative: `https://github.com/BtbN/FFmpeg-Builds/releases`

### Build Failures After Preflight Success

If build fails despite preflight success:
- Preflight only checks availability, not content integrity
- Download may have been corrupted
- Archive structure may have changed
- Re-run workflow or check full health check workflow

## Adding New Dependencies

1. Add URL check to appropriate preflight job in `build.yml`
2. Add full verification to `check-linux-deps.yml` or `check-windows-deps.yml`
3. Update this documentation
4. Test with manual workflow trigger

## Version Information

- **Last updated:** 2026-01-01
- **Workflow version:** Compatible with GitHub Actions runner ubuntu-latest, windows-latest
- **PowerShell requirement:** 7.x (pre-installed on GitHub runners)
