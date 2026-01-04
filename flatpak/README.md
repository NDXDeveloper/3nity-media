# Flatpak Build Instructions

This directory contains the Flatpak manifest and metadata files for building 3nity Media as a Flatpak package.

## Prerequisites

Install Flatpak and flatpak-builder on your system:

```bash
# Ubuntu/Debian
sudo apt install flatpak flatpak-builder

# Fedora
sudo dnf install flatpak flatpak-builder

# Arch Linux
sudo pacman -S flatpak flatpak-builder

# openSUSE
sudo zypper install flatpak flatpak-builder
```

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

## Troubleshooting

### Runtime not found

If you get an error about missing runtime, install it:

```bash
flatpak install flathub org.kde.Platform//6.8 org.kde.Sdk//6.8
```

### Build fails on SourceForge download

SourceForge downloads can sometimes fail. If libqt5pas download fails, try again or download manually and use a local source.
