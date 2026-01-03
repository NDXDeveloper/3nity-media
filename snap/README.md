# Snap Package Build Instructions

This directory contains the Snapcraft configuration for building the 3nity Media snap package.

## Prerequisites

Install snapcraft:

```bash
sudo snap install snapcraft --classic
```

## Building the Snap

From the project root directory:

```bash
# Build the snap
snapcraft

# Or use LXD for clean builds (recommended)
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

1. Register the snap name:
   ```bash
   snapcraft register 3nity-media
   ```

2. Login to your Snapcraft account:
   ```bash
   snapcraft login
   ```

3. Upload and release:
   ```bash
   snapcraft upload 3nity-media_0.1.0_amd64.snap --release=stable
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
| Binary | `$SNAP/bin/3nity-media` |
| Locale files | `$SNAP/share/3nity-media/locale/` |
| Desktop file | `$SNAP/share/applications/3nity-media.desktop` |
| Icon | `$SNAP/share/icons/hicolor/256x256/apps/3nity-media.png` |

## Configuration Storage

User configuration is stored in:
- `$SNAP_USER_DATA/.config/3nity-media/`

This maps to:
- `~/snap/3nity-media/current/.config/3nity-media/`
