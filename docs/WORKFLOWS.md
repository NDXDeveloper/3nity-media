# GitHub Actions Workflows

This document describes the CI/CD workflows for 3nity Media.

## Overview

| Workflow | Trigger | Purpose |
|----------|---------|---------|
| `build.yml` | Push, PR, Tags | Main build pipeline |
| `check-linux-deps.yml` | Weekly, Manual | Verify Linux dependencies |
| `check-windows-deps.yml` | Weekly, Manual | Verify Windows dependencies |

---

## build.yml - Main Build Pipeline

The primary CI/CD workflow that builds, tests, and packages 3nity Media.

### Triggers

| Event | Condition |
|-------|-----------|
| Push | `main`, `develop` branches |
| Tags | `v*` (e.g., `v1.0.0`) |
| Pull Request | To `main` branch |

### Jobs

1. **Preflight Check (Linux)** - Verify APT packages availability
2. **Preflight Check (Windows)** - Verify external dependencies
3. **Build Linux** - Compile with Lazarus/FPC for Linux x86_64
4. **Build Windows** - Cross-compile for Windows x86_64
5. **Package Linux** - Create distribution packages
6. **Package Windows** - Create Windows installer/portable
7. **Release** - Publish release on tag push

### Artifacts Generated

| Platform | Artifacts |
|----------|-----------|
| Linux | AppImage, DEB, Snap, tar.gz |
| Windows | Portable ZIP, Installer (NSIS) |

### Environment Variables

| Variable | Description |
|----------|-------------|
| `APP_NAME` | Application name (`3nity-media`) |
| `BINARY_NAME` | Executable name (`3nity-media`) |

### Secrets Required

| Secret | Purpose | Required For |
|--------|---------|--------------|
| `SNAPCRAFT_STORE_CREDENTIALS` | Snap Store authentication | Snap publishing |

---

## check-linux-deps.yml - Linux Dependency Check

Monitors availability of Linux dependencies in APT repositories.

### Triggers

- **Scheduled**: Every Monday at 6:00 UTC
- **Manual**: Via workflow_dispatch

### Dependencies Checked

| Package | Purpose |
|---------|---------|
| `libmpv1` / `libmpv2` | Media playback engine |
| `libqt5pas1` | Qt5 Pascal bindings |
| `ffmpeg` | Media encoding/decoding |
| `lazarus-ide-qt5` | Lazarus IDE with Qt5 |

### Runners

- Bash checks on `ubuntu-latest`
- PowerShell checks on `ubuntu-latest`

---

## check-windows-deps.yml - Windows Dependency Check

Monitors availability of Windows dependencies from external sources.

### Triggers

- **Scheduled**: Every Monday at 6:00 UTC
- **Manual**: Via workflow_dispatch

### Dependencies Checked

| Dependency | Source |
|------------|--------|
| libmpv | SourceForge (mpv-player-windows) |
| ffmpeg | GitHub (BtbN/FFmpeg-Builds) |

### Runners

- Bash checks on `ubuntu-latest`
- PowerShell checks on `ubuntu-latest` and `windows-latest`

---

## Manual Workflow Dispatch

To manually trigger a workflow:

### Via GitHub UI

1. Go to **Actions** tab in GitHub
2. Select the workflow
3. Click **Run workflow**
4. Select branch and click **Run workflow**

### Via GitHub CLI

```bash
# Trigger build
gh workflow run build.yml

# Trigger dependency check
gh workflow run check-linux-deps.yml
gh workflow run check-windows-deps.yml
```

---

## Status Badges

Add these badges to your README.md:

```markdown
![Build](https://github.com/NDXDeveloper/3nity-media/actions/workflows/build.yml/badge.svg)
![Linux Deps](https://github.com/NDXDeveloper/3nity-media/actions/workflows/check-linux-deps.yml/badge.svg)
![Windows Deps](https://github.com/NDXDeveloper/3nity-media/actions/workflows/check-windows-deps.yml/badge.svg)
```

---

## Troubleshooting

### Build fails with "libmpv not found"

The preflight check should catch this. If it passes but build fails:
- Check if APT package name changed
- Verify Ubuntu version compatibility

### Windows cross-compilation fails

- Ensure `lazbuild` supports cross-compilation target
- Check FPC cross-compiler packages

### Snap build fails

- Verify `snapcraft.yaml` syntax
- Check base image compatibility (core22)

### Release not created

- Ensure tag follows `v*` pattern (e.g., `v1.0.0`)
- Check `contents: write` permission is set

---

## File Structure

```
.github/
└── workflows/
    ├── README.md              # Quick reference
    ├── build.yml              # Main pipeline (72 KB)
    ├── check-linux-deps.yml   # Linux check (15 KB)
    └── check-windows-deps.yml # Windows check (11 KB)
```

---

## See Also

- [Snapcraft Documentation](SNAPCRAFT.md) - Publishing to Snap Store
