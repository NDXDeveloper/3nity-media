# Installation Guide - 3nity Media

This guide covers all installation methods for 3nity Media on Linux and Windows.

## Table of Contents

- [Prerequisites](#prerequisites)
- [System Requirements](#system-requirements)
- [Dependencies](#dependencies)
- [Package Installation](#package-installation)
  - [DEB (Ubuntu/Debian)](#deb-ubuntudebian)
  - [AppImage (Universal)](#appimage-universal)
  - [Snap](#snap)
  - [Windows](#windows-1)
- [Building from Source](#building-from-source)
  - [Linux](#linux-build)
  - [Windows](#windows-build)
- [Post-Installation](#post-installation)
- [Troubleshooting](#troubleshooting)
- [Uninstallation](#uninstallation)

---

## Prerequisites

### Install sudo (if not available)

Some minimal installations don't include sudo. Install it as root:

```bash
# Debian (as root)
su -
apt install sudo
usermod -aG sudo your_username
# Log out and log in

# Arch Linux (as root)
su -
pacman -S sudo
echo "your_username ALL=(ALL:ALL) ALL" >> /etc/sudoers.d/your_username
# Log out and log in

# Fedora (as root, if minimal install)
su -
dnf install sudo
usermod -aG wheel your_username
# Log out and log in

# openSUSE (as root, if minimal install)
su -
zypper install sudo
usermod -aG wheel your_username
# Log out and log in
```

---

## System Requirements

### Minimum
- **OS**: Linux (kernel 4.15+) or Windows 10
- **CPU**: x86_64 (64-bit)
- **RAM**: 512 MB
- **Disk**: 50 MB free space
- **Display**: X11 or Wayland (Linux), DirectX 11 (Windows)

### Recommended
- **RAM**: 2 GB or more
- **GPU**: Hardware video acceleration support (VA-API, VDPAU, or Vulkan)
- **Audio**: PulseAudio, PipeWire, or ALSA (Linux)

---

## Dependencies

### Linux - Debian/Ubuntu-based

| Package | Purpose | Required |
|---------|---------|----------|
| `libmpv2` or `libmpv1` | Media playback engine | Yes |
| `libqt5pas1` | Qt5 bindings for Pascal | Yes |
| `ffmpeg` | Metadata extraction (ffprobe) | Yes |
| `yt-dlp` | YouTube/streaming support | Recommended |

Install all dependencies:
```bash
sudo apt update
sudo apt install libmpv2 libqt5pas1 ffmpeg yt-dlp
```

If `libmpv2` is not available:
```bash
sudo apt install libmpv1 libqt5pas1 ffmpeg yt-dlp
```

### Windows

Windows dependencies are bundled with the installer:
- `libmpv-2.dll` - Media playback engine
- `ffprobe.exe` - Metadata extraction

Optional: Install [yt-dlp](https://github.com/yt-dlp/yt-dlp/releases) for YouTube support.

---

## Package Installation

### DEB (Ubuntu/Debian)

Supported distributions: Ubuntu 20.04+, Debian 11+, Linux Mint 20+, Pop!_OS, elementary OS

**Method 1: Download and install**
```bash
# Download the package
wget https://github.com/NDXDeveloper/3nity-media/releases/latest/download/3nity-media_VERSION_amd64.deb

# Install with dependencies
sudo apt install ./3nity-media_VERSION_amd64.deb
```

**Method 2: Using dpkg**
```bash
# Install dependencies first
sudo apt install libmpv2 libqt5pas1 ffmpeg

# Install the package
sudo dpkg -i 3nity-media_VERSION_amd64.deb

# Fix any missing dependencies
sudo apt install -f
```

### AppImage (Universal)

Works on any Linux distribution with FUSE support.

```bash
# Download
wget https://github.com/NDXDeveloper/3nity-media/releases/latest/download/3nity-Media-VERSION-x86_64.AppImage

# Make executable
chmod +x 3nity-Media-VERSION-x86_64.AppImage

# Run
./3nity-Media-VERSION-x86_64.AppImage
```

**Optional: Desktop integration**
```bash
# Using AppImageLauncher (recommended)
sudo apt install appimagelauncher  # Ubuntu/Debian
sudo dnf install appimagelauncher  # Fedora

# Or manually create a desktop entry
mkdir -p ~/.local/share/applications
cat > ~/.local/share/applications/3nity-media.desktop << 'EOF'
[Desktop Entry]
Type=Application
Name=3nity Media
Exec=/path/to/3nity-Media-VERSION-x86_64.AppImage %F
Icon=3nity-media
Categories=AudioVideo;Audio;Video;Player;
MimeType=audio/*;video/*;
EOF
```

### Snap

**Install snapd first (if not available):**

```bash
# Ubuntu (pre-installed)

# Debian
sudo apt install snapd

# Fedora
sudo dnf install snapd
sudo ln -s /var/lib/snapd/snap /snap
sudo systemctl enable --now snapd.socket

# openSUSE
sudo zypper install snapd
sudo systemctl enable --now snapd

# Arch Linux
sudo pacman -S snapd
sudo systemctl enable --now snapd.socket
sudo ln -s /var/lib/snapd/snap /snap
```

**Install 3nity Media:**

```bash
# From Snap Store (recommended - includes auto-updates)
sudo snap install 3nity-media
```

Or visit: https://snapcraft.io/3nity-media

```bash
# From downloaded file (manual installation)
wget https://github.com/NDXDeveloper/3nity-media/releases/latest/download/3nity-media_VERSION_amd64.snap
sudo snap install --dangerous 3nity-media_VERSION_amd64.snap
```

**Create short alias (optional):**

```bash
sudo snap alias 3nity-media 3nity
# Then you can run with just: 3nity
```

### Windows

**Method 1: System installer (recommended)**

Download and run `3nity-Media-Setup-VERSION.exe`
- Requires administrator privileges
- Installs to `C:\Program Files\3nity Media`
- Creates Start Menu shortcuts
- Optional desktop shortcut

**Method 2: User installer (no admin required)**

Download and run `3nity-Media-Setup-User-VERSION.exe`
- No administrator privileges needed
- Installs to `%LOCALAPPDATA%\Programs\3nity Media`
- Creates user-specific shortcuts

**Method 3: Portable**

Download and extract `3nity-media-windows-portable-VERSION.zip`
- No installation required
- Run `3nity.exe` directly
- Settings stored in the same folder

---

## Building from Source

### Linux Build

#### Prerequisites

Install the Lazarus IDE and development dependencies:

**Ubuntu/Debian:**
```bash
sudo apt update
sudo apt install \
  lazarus-ide-qt5 \
  lazarus-src \
  lcl-qt5 \
  lcl-utils \
  fpc \
  libqt5pas1 \
  libqt5pas-dev \
  libmpv-dev \
  ffmpeg \
  git \
  make
```

**Fedora:**
```bash
sudo dnf install \
  lazarus \
  fpc \
  fpc-src \
  qt5-qtbase-devel \
  mpv-libs-devel \
  ffmpeg \
  git \
  make
```

**Arch Linux:**
```bash
sudo pacman -S lazarus fpc qt5pas mpv ffmpeg git make
```

#### Clone and Build

```bash
# Clone the repository
git clone https://github.com/NDXDeveloper/3nity-media.git
cd 3nity-media

# Build using Makefile (recommended)
make build-release

# Or build using lazbuild directly
cd src
lazbuild --build-mode=Release TrinityMedia.lpi
```

The binary will be created at `bin/x86_64-linux/3nity-media`

#### Build Options

```bash
# Debug build (with debug symbols)
make build-app

# Release build (optimized)
make build-release

# Clean and rebuild
make rebuild

# Build with specific version
make release V=0.1.0

# Show all available targets
make help
```

### Windows Build

#### Prerequisites

1. **Install Lazarus**
   - Download from [lazarus-ide.org](https://www.lazarus-ide.org/index.php?page=downloads)
   - Or use Chocolatey: `choco install lazarus`

2. **Install Git** (optional, for cloning)
   - Download from [git-scm.com](https://git-scm.com/download/win)
   - Or use Chocolatey: `choco install git`

#### Build Steps

```powershell
# Clone the repository
git clone https://github.com/NDXDeveloper/3nity-media.git
cd 3nity-media\src

# Build with lazbuild
& "C:\lazarus\lazbuild.exe" --build-mode=Release TrinityMedia.lpi
```

The binary will be created at `bin\x86_64-win64\3nity.exe`

#### Required DLLs

Copy these files to the same folder as `3nity.exe`:

1. **libmpv-2.dll**
   - Download from [SourceForge mpv-player-windows](https://sourceforge.net/projects/mpv-player-windows/files/libmpv/)
   - Extract and copy `libmpv-2.dll`

2. **ffprobe.exe**
   - Download from [gyan.dev](https://www.gyan.dev/ffmpeg/builds/)
   - Extract `ffprobe.exe` from the archive

---

## Post-Installation

### First Launch

1. Launch 3nity Media from your applications menu or command line
2. On first run, default settings are created
3. The application will detect your language from system settings

### Configuration Location

| Platform | Location |
|----------|----------|
| Linux | `~/.config/3nity-media/` |
| Windows | `%APPDATA%\3nity-media\` |
| Snap | `~/snap/3nity-media/current/.config/` |

Configuration files:
- `config.ini` - General settings
- `shortcuts.ini` - Custom keyboard shortcuts
- `favorites.ini` - Favorite stations/files
- `history.ini` - Playback history

### File Associations

**Linux (DEB):**
File associations are automatically configured during installation.

**Linux (AppImage/Portable):**
```bash
# Create MIME associations
xdg-mime default 3nity-media.desktop audio/mpeg
xdg-mime default 3nity-media.desktop video/mp4
# Add more types as needed
```

**Windows:**
Right-click a media file → Open with → Choose another app → Select 3nity → Check "Always use this app"

### GPU Acceleration

**Linux - VA-API (Intel/AMD):**
```bash
# Install VA-API drivers
sudo apt install va-driver-all vainfo  # Ubuntu/Debian
sudo dnf install libva-utils           # Fedora
```

**Linux - VDPAU (NVIDIA):**
```bash
# Install VDPAU drivers
sudo apt install vdpau-driver-all vdpauinfo  # Ubuntu/Debian
```

---

## Troubleshooting

### General

```bash
# Check if application is running
pgrep -a 3nity

# View logs
journalctl -f | grep 3nity
```

### DEB package issues

```bash
# Fix dependency errors
sudo apt-get install -f

# Check installed dependencies
dpkg -l | grep -E "libmpv|libqt5pas"
apt-cache depends 3nity-media

# Reinstall package
sudo apt install --reinstall ./3nity-media_*_amd64.deb
```

### "libmpv.so not found"

The mpv library is missing. Install it:
```bash
# Ubuntu/Debian
sudo apt install libmpv2

# Fedora
sudo dnf install mpv-libs

# Check installation
ldconfig -p | grep mpv
```

### "libQt5Pas.so not found"

The Qt5 Pascal bindings are missing. Install them:
```bash
# Ubuntu/Debian
sudo apt install libqt5pas1

# Check installation
ldconfig -p | grep qt5pas
```

> **Note:** libQt5Pas is not available on Fedora/RHEL/openSUSE. Use AppImage or Snap instead.

### No sound

1. Check that PulseAudio/PipeWire is running:
   ```bash
   pactl info
   ```

2. Check volume levels:
   ```bash
   pavucontrol  # Install: apt install pavucontrol
   ```

3. Verify ALSA devices:
   ```bash
   aplay -l
   ```

### Video playback issues

1. Check GPU acceleration:
   ```bash
   vainfo      # VA-API
   vdpauinfo   # VDPAU
   vulkaninfo  # Vulkan
   ```

2. Try software rendering:
   - Options → Video → Hardware Decoding → Off

### AppImage won't run

Install FUSE:
```bash
# Ubuntu/Debian
sudo apt install fuse libfuse2

# Fedora
sudo dnf install fuse fuse-libs

# Arch Linux
sudo pacman -S fuse2

# Alternative: run with extraction
./3nity-Media-*-x86_64.AppImage --appimage-extract-and-run
```

### Snap issues

```bash
# Check permissions
snap connections 3nity-media

# Connect missing permissions
sudo snap connect 3nity-media:audio-playback
sudo snap connect 3nity-media:pulseaudio
sudo snap connect 3nity-media:removable-media
sudo snap connect 3nity-media:optical-drive
sudo snap connect 3nity-media:udisks2

# View logs
snap logs 3nity-media

# Check confinement issues
journalctl -f | grep -i apparmor
```

**Known issue - locale-gen error:**

When running the snap, you may see:
```
locale-gen: No such file or directory
ERROR: locale-gen exited abnormally with status 127
```

This is a cosmetic error from the kde-neon extension and does not prevent the application from working.

### Windows issues

```powershell
# Check DLLs present
dir "C:\Program Files\3nity Media\*.dll"

# Run from command line to see errors
cd "C:\Program Files\3nity Media"
.\3nity-media.exe

# Check Windows Event Viewer
eventvwr.msc
```

---

## Uninstallation

### DEB
```bash
sudo apt remove 3nity-media
sudo apt autoremove  # Remove unused dependencies
```

### AppImage
```bash
rm ~/Applications/3nity-Media-*.AppImage
rm ~/.local/share/applications/3nity-media.desktop
```

### Snap
```bash
sudo snap remove 3nity-media
```

### Windows
- **Installer version**: Control Panel → Programs → Uninstall "3nity Media"
- **Portable version**: Delete the folder

### Remove configuration (all platforms)

```bash
# Linux
rm -rf ~/.config/3nity-media/

# Windows (PowerShell)
Remove-Item -Recurse "$env:APPDATA\3nity-media"
```

---

## See Also

- [User Guide](USER_GUIDE.md)
- [CLI Parameters](CLI_PARAMETERS.md)
- [Keyboard Shortcuts](SHORTCUTS.md)
- [Configuration Guide](CONFIG.md)

---

## Version Information

- **Last updated:** 2026-01-02
- **Applies to:** 3nity Media v0.x and later
