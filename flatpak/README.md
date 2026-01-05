# Flatpak Build Instructions

This directory contains the Flatpak manifest and metadata files for building 3nity Media as a Flatpak package.

## Prerequisites

Install Flatpak and flatpak-builder on your system:

```bash
# Ubuntu/Debian
sudo apt install flatpak flatpak-builder

# Fedora (flatpak pre-installed, just add flatpak-builder)
sudo dnf install flatpak-builder

# Arch Linux
sudo pacman -S flatpak flatpak-builder

# openSUSE
sudo zypper install flatpak flatpak-builder
```

**Note:** After installing Flatpak for the first time, restart your session (log out and log in) or reboot.

Add the Flathub repository and install the KDE SDK:

```bash
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
flatpak install flathub org.kde.Platform//6.8 org.kde.Sdk//6.8
```

## Building

### Step 1: Build the application with Lazarus

First, compile 3nity Media from source:

```bash
cd /path/to/3nity-media
cd src
lazbuild --build-mode=Release TrinityMedia.lpi
```

The binary will be created at `bin/x86_64-linux/3nity-media`.

### Step 2: Build the Flatpak

From the repository root:

```bash
cd flatpak
flatpak-builder --user --install --force-clean build-dir com.ndxdev.trinitymedia.yml
```

Or to create a distributable bundle:

```bash
flatpak-builder --repo=repo --force-clean build-dir com.ndxdev.trinitymedia.yml
flatpak build-bundle repo 3nity-media.flatpak com.ndxdev.trinitymedia
```

## Running

After installation:

```bash
flatpak run com.ndxdev.trinitymedia
```

## Files

| File | Description |
|------|-------------|
| `com.ndxdev.trinitymedia.yml` | Flatpak manifest (build configuration) |
| `com.ndxdev.trinitymedia.desktop` | Desktop entry file |
| `com.ndxdev.trinitymedia.metainfo.xml` | AppStream metadata for software centers |

## Permissions

The Flatpak requests the following permissions (defined in `finish-args`):

| Permission | Purpose |
|------------|---------|
| `--socket=x11` / `--socket=wayland` | Display access |
| `--socket=pulseaudio` | Audio playback |
| `--device=all` | Hardware access (GPU, CD/DVD drives) |
| `--share=network` | Internet streaming |
| `--filesystem=home` | Access to home directory |
| `--filesystem=/media` | Access to mounted drives |
| `--filesystem=/run/media` | Access to removable media |
| `--talk-name=org.freedesktop.Notifications` | Desktop notifications |
| `--talk-name=org.freedesktop.ScreenSaver` | Prevent screen sleep during playback |
| `--system-talk-name=org.freedesktop.UDisks2` | Disk enumeration |

## Notes

- The manifest uses KDE Platform 6.8 runtime
- FFmpeg and libmpv are built from source for codec support
- libQt5Pas (Pascal Qt5 bindings) is downloaded from Lazarus project

## Updating libQt5Pas hash

If you need to update the qt5pas version, compute the new SHA256 hash:

```bash
# Download the archive
curl -L -o qt5pas.tar.gz "https://downloads.sourceforge.net/project/lazarus/Lazarus%20Releases/VERSION/qt5pas-X.X-X.tar.gz"

# Compute SHA256
sha256sum qt5pas.tar.gz
```

Then update the `sha256:` value in `com.ndxdev.trinitymedia.yml`.

## Updating version before building

Unlike Snap (which supports `version: git`), Flatpak requires manual version updates in the metainfo file.

**Before building**, edit `com.ndxdev.trinitymedia.metainfo.xml` and update the `<release>` entry:

```xml
<releases>
  <release version="X.Y.Z" date="YYYY-MM-DD">
    <description>
      <p>Release description here</p>
    </description>
  </release>
</releases>
```

Replace `x.x.x` with your version number (e.g., `0.1.3`) and `YYYY-MM-DD` with the release date.

## Publishing to Flathub

### Prerequisites

1. **GitHub account**: Required for submitting to Flathub
2. **Tested Flatpak**: Make sure your app builds and runs correctly locally

### Submission Process

1. **Fork the Flathub repository**:
   - Go to https://github.com/flathub/flathub
   - Click "Fork" to create your own copy

2. **Create a new repository for your app**:
   ```bash
   # Clone your fork
   git clone git@github.com:YOUR_USERNAME/flathub.git
   cd flathub

   # Create a branch with your app-id
   git checkout -b com.ndxdev.trinitymedia
   ```

3. **Add your manifest files**:
   - Copy `com.ndxdev.trinitymedia.yml` to the repository
   - The manifest must fetch sources from URLs (not local paths)
   - Include `metainfo.xml` and `desktop` files as sources

4. **Submit a Pull Request**:
   - Push your branch to your fork
   - Create a PR to https://github.com/flathub/flathub
   - Fill in the submission checklist

5. **Review Process**:
   - Flathub maintainers will review your submission
   - Address any feedback or requested changes
   - Once approved, a dedicated repo is created at `github.com/flathub/com.ndxdev.trinitymedia`

### After Acceptance

Once accepted, your app will have its own repository:
- https://github.com/flathub/com.ndxdev.trinitymedia

To publish updates:
```bash
# Clone your Flathub app repo
git clone git@github.com:flathub/com.ndxdev.trinitymedia.git
cd com.ndxdev.trinitymedia

# Update manifest with new version/sources
# Update metainfo.xml with new release entry
git add .
git commit -m "Update to version X.Y.Z"
git push
```

Flathub will automatically build and publish the new version.

### Flathub Guidelines

- App must have valid AppStream metainfo (`metainfo.xml`)
- App must have a desktop file
- App must have an icon (at least 128x128)
- All sources must be fetchable via URLs
- No bundled dependencies that are in the runtime

See: https://docs.flathub.org/docs/for-app-authors/submission

## Troubleshooting

### Runtime not found

If you get an error about missing runtime, install it:

```bash
flatpak install flathub org.kde.Platform//6.8 org.kde.Sdk//6.8
```

### Build fails on SourceForge download

SourceForge downloads can sometimes fail. If libqt5pas download fails, try again or download manually and use a local source.
