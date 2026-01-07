# Dependencies

This document lists all dependencies required to build and run 3nity Media.

---

## Table of Contents

- [Build-Time Dependencies](#build-time-dependencies)
- [Runtime Dependencies](#runtime-dependencies)
- [Optional Dependencies](#optional-dependencies)
- [Dependency Matrix](#dependency-matrix)
- [Installation by Platform](#installation-by-platform)

---

## Build-Time Dependencies

### Required for Compilation

| Dependency | Version | Purpose |
|------------|---------|---------|
| **Free Pascal Compiler (FPC)** | 3.2.0+ | Pascal compiler |
| **Lazarus IDE** | 3.0+ | RAD IDE and LCL framework |
| **LCL (Lazarus Component Library)** | - | Cross-platform UI toolkit |
| **Qt5Pas** | 1.2.6+ | Qt5 bindings for LCL (Linux) |
| **libmpv-dev** | 0.35+ | MPV development headers |

### Source URLs

| Dependency | Download |
|------------|----------|
| Lazarus | https://www.lazarus-ide.org/index.php?page=downloads |
| Free Pascal | https://www.freepascal.org/download.html |
| Qt5Pas | Included with Lazarus (Linux: `libqt5pas-dev`) |

---

## Runtime Dependencies

### Required at Runtime

| Dependency | Version | Library File | Purpose |
|------------|---------|--------------|---------|
| **libmpv** | 0.35+ | `libmpv.so.2` (Linux) / `mpv-2.dll` (Windows) | Media playback engine |
| **Qt5** | 5.12+ | Various Qt5 libraries | UI rendering (Linux) |
| **Qt5Pas** | 1.2.6+ | `libqt5pas.so.1` | Qt5 Pascal bindings |

### Library Loading

The application loads libmpv dynamically at runtime:

```pascal
// uLibMPV.pas
const
  {$IFDEF WINDOWS}
  LIBMPV_DLL = 'mpv-2.dll';
  {$ELSE}
  LIBMPV_DLL = 'libmpv.so.2';
  {$ENDIF}
```

If libmpv is not found, the application will show an error message and exit.

---

## Optional Dependencies

These are not required but enable additional functionality:

| Dependency | Purpose | Without It |
|------------|---------|------------|
| **libdvdnav** | Native DVD menu navigation | DVD plays via fallback mode |
| **libbluray** | Native Blu-ray navigation | Blu-ray plays via fallback mode |
| **ffprobe** | Metadata extraction | Reduced metadata display |
| **youtube-dl/yt-dlp** | YouTube streaming | YouTube URLs won't work |

### DVD/Blu-ray Fallback Mode

When libdvdnav/libbluray are not available, 3nity Media uses fallback mode:
- **DVD**: Detects main title and queues VOB files to playlist
- **Blu-ray**: Plays the largest M2TS file from BDMV/STREAM

---

## Dependency Matrix

### Linux (Ubuntu/Debian)

| Package | Type | Install Command |
|---------|------|-----------------|
| `lazarus-ide-qt5` | Build | `apt install` |
| `lazarus-src` | Build | `apt install` |
| `lcl-qt5` | Build | `apt install` |
| `fpc` | Build | `apt install` |
| `libqt5pas-dev` | Build | `apt install` |
| `libmpv-dev` | Build | `apt install` |
| `libmpv2` | Runtime | `apt install` |
| `libqt5pas1` | Runtime | `apt install` |
| `ffmpeg` | Optional | `apt install` |
| `git` | Development | `apt install` |
| `make` | Development | `apt install` |

### Linux (Fedora)

| Package | Type | Install Command |
|---------|------|-----------------|
| `lazarus` | Build | `dnf install` |
| `fpc` | Build | `dnf install` |
| `qt5pas-devel` | Build | `dnf install` |
| `mpv-libs-devel` | Build | `dnf install` |
| `mpv-libs` | Runtime | `dnf install` |
| `qt5pas` | Runtime | `dnf install` |

### Linux (Arch)

| Package | Type | Install Command |
|---------|------|-----------------|
| `lazarus` | Build | `pacman -S` |
| `fpc` | Build | `pacman -S` |
| `qt5pas` | Build+Runtime | `pacman -S` |
| `mpv` | Runtime | `pacman -S` |

### Windows

| File | Type | Source |
|------|------|--------|
| Lazarus installer | Build | lazarus-ide.org |
| `mpv-2.dll` | Runtime | SourceForge mpv builds |
| Visual C++ Runtime | Runtime | Microsoft |

---

## Installation by Platform

### Ubuntu/Debian

```bash
# All dependencies (build + runtime)
sudo apt update
sudo apt install lazarus-ide-qt5 lazarus-src lcl-qt5 fpc \
  libqt5pas1 libqt5pas-dev libmpv-dev libmpv2 ffmpeg git make

# Runtime only
sudo apt install libmpv2 libqt5pas1
```

### Fedora

```bash
# All dependencies
sudo dnf install lazarus fpc qt5pas-devel mpv-libs-devel ffmpeg git make

# Runtime only
sudo dnf install mpv-libs qt5pas
```

### Arch Linux

```bash
# All dependencies
sudo pacman -S lazarus fpc qt5pas mpv ffmpeg git make

# Runtime only
sudo pacman -S mpv qt5pas
```

### Windows

1. Download and install [Lazarus IDE](https://www.lazarus-ide.org/)
2. Download [mpv-2.dll](https://sourceforge.net/projects/mpv-player-windows/files/libmpv/)
3. Place `mpv-2.dll` in `bin/x86_64-win64/` folder
4. Optionally install [Git](https://git-scm.com/) for version control

---

## Version Compatibility

### Minimum Versions Tested

| Component | Minimum | Recommended |
|-----------|---------|-------------|
| FPC | 3.2.0 | 3.2.2+ |
| Lazarus | 3.0 | 3.6+ |
| libmpv | 0.35.0 | 0.37+ |
| Qt5 | 5.12 | 5.15+ |
| Qt5Pas | 1.2.6 | 1.2.15+ |

### Known Compatibility Issues

| Issue | Affected | Solution |
|-------|----------|----------|
| Qt5Pas segfault on exit | Ubuntu 20.04 | Upgrade to 22.04+ |
| mpv-2.dll missing | Windows | Download from SourceForge |
| libmpv.so.1 vs .so.2 | Older distros | Install mpv from backports |

---

## Checking Installed Versions

### Linux

```bash
# Free Pascal Compiler
fpc -v

# Lazarus
lazbuild --version

# libmpv
pkg-config --modversion mpv

# Qt5
qmake --version

# Qt5Pas
dpkg -l | grep qt5pas  # Debian/Ubuntu
```

### Windows

- FPC: `fpc -v` in command prompt
- Lazarus: Help > About in IDE
- mpv-2.dll: Check file properties

---

## Troubleshooting Dependencies

### libmpv not found

**Linux:**
```bash
# Check if installed
ldconfig -p | grep libmpv

# Install if missing
sudo apt install libmpv2  # Debian/Ubuntu
sudo dnf install mpv-libs # Fedora
```

**Windows:**
- Ensure `mpv-2.dll` is in the same folder as the executable
- Or add its location to the system PATH

### Qt5Pas not found

**Linux:**
```bash
# Check if installed
ldconfig -p | grep qt5pas

# Install
sudo apt install libqt5pas1 libqt5pas-dev
```

### Build errors with LCL-Qt5

Ensure you're using the correct Lazarus packages:
```bash
sudo apt install lcl-qt5 lazarus-ide-qt5
```

---

## See Also

- [QUICKSTART_DEVELOPER.md](QUICKSTART_DEVELOPER.md) - Quick start guide
- [INSTALL.md](INSTALL.md) - Installation instructions
- [TROUBLESHOOTING_DEV.md](TROUBLESHOOTING_DEV.md) - Development troubleshooting

---

*Last updated: 2026-01-07*
