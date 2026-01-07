# Snap Package Build Instructions

This directory contains the Snapcraft configuration for building the 3nity Media snap package.

## Prerequisites

### Install snapd

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

### Install snapcraft

```bash
sudo snap install snapcraft --classic
```

## Building the Snap

From the project root directory:

```bash
# Using make (recommended - handles iptables and cache automatically)
make snap

# Clean previous build artifacts and rebuild
make snap-clean

# Or manually with snapcraft
snapcraft --use-lxd
```

## Testing Locally

```bash
# Install the locally built snap
sudo snap install 3nity-media_*.snap --dangerous

# Run the application
3nity-media
```

## Icon Preparation

**Important:** Before building, prepare a square icon (256x256 or 512x512 PNG):

```bash
# If you have ImageMagick installed, create a square icon from the logo:
convert src/logo.png -gravity center -background transparent -extent 512x512 snap/gui/3nity-media.png

# Or manually create a 256x256 or 512x512 square PNG and place it at:
# snap/gui/3nity-media.png
```

## Publishing to Snap Store

### Snapcraft Authentication

1. **Create Ubuntu One account**: Register at https://login.ubuntu.com

2. **Login via CLI**:
   ```bash
   snapcraft login
   ```
   This opens a browser for OAuth authentication (no SSH key needed).

3. **For CI/CD (GitHub Actions)**: Generate an export token:
   ```bash
   snapcraft export-login --snaps=3nity-media --acls=package_upload snapcraft-credentials.txt
   ```
   Then add the file contents as a GitHub secret named `SNAPCRAFT_STORE_CREDENTIALS`.

### Register the snap name

Before the first publication:

```bash
snapcraft register 3nity-media
```

### Publish

```bash
snapcraft upload 3nity-media_*.snap --release=stable
```

Or to a specific channel:

```bash
snapcraft upload 3nity-media_*.snap --release=edge
```

## Snap Channels

| Channel | Purpose |
|---------|---------|
| `edge` | Development builds (auto-published from CI) |
| `beta` | Pre-release testing |
| `candidate` | Release candidates |
| `stable` | Production releases |

## Confinement

The snap uses `strict` confinement with the following plugs:

| Plug | Purpose |
|------|---------|
| `home` | Access to home directory |
| `audio-playback` | Audio output |
| `audio-record` | Audio input (for visualizations) |
| `pulseaudio` | PulseAudio access |
| `network` | Internet streaming |
| `removable-media` | USB drives, external disks |
| `optical-drive` | DVD/CD playback |
| `opengl` | Hardware acceleration |
| `x11` / `wayland` | Display server access |
| `udisks2` | Disk information access |
| `screen-inhibit-control` | Prevent screen sleep during playback |
| `mount-observe` | Monitor mount events |

## Creating an Alias

To use the short command `3nity` instead of `3nity-media`:

```bash
sudo snap alias 3nity-media 3nity
```

## Known Issues

### locale-gen Error

When running the snap, you may see:
```
locale-gen: No such file or directory
ERROR: locale-gen exited abnormally with status 127
```

This is a cosmetic error from the kde-neon extension and does not prevent the application from working.

## Troubleshooting

### Audio not working

```bash
# Check audio connections
snap connections 3nity-media | grep audio

# Connect manually if needed
sudo snap connect 3nity-media:audio-playback
sudo snap connect 3nity-media:pulseaudio
```

### Cannot access external drives

```bash
sudo snap connect 3nity-media:removable-media
```

### DVD playback not working

```bash
sudo snap connect 3nity-media:optical-drive
```

### UDisks2 errors (disk enumeration)

```bash
sudo snap connect 3nity-media:udisks2
```

### Hardware acceleration issues

```bash
# Check OpenGL connection
snap connections 3nity-media | grep opengl

# Verify VA-API/VDPAU drivers are available
vainfo
vdpauinfo
```

## File Locations

| Content | Path in Snap |
|---------|--------------|
| Binary | `$SNAP/usr/bin/3nity-media` |
| Locale files | `$SNAP/usr/share/3nity-media/locale/` |
| Desktop file | `$SNAP/usr/share/applications/3nity-media.desktop` |
| Icon | `$SNAP/usr/share/icons/hicolor/256x256/apps/3nity-media.png` |

## Configuration Storage

User configuration is stored in:
- `$SNAP_USER_DATA/.config/3nity-media/`

This maps to:
- `~/snap/3nity-media/current/.config/3nity-media/`
