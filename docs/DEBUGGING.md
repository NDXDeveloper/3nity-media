# 3nity Media - Debugging Guide

This guide provides tools and techniques for debugging 3nity Media across different installation methods.

---

## Table of Contents

- [Library Dependencies](#library-dependencies)
- [Snap Debugging](#snap-debugging)
- [Flatpak Debugging](#flatpak-debugging)
- [AppImage Debugging](#appimage-debugging)
- [Environment Variables](#environment-variables)
- [Common Errors](#common-errors)

---

## Library Dependencies

### Check Missing Dependencies

Use `ldd` to list all shared library dependencies and find missing ones:

```bash
# For system installation
ldd /usr/bin/3nity-media | grep "not found"

# For a specific library
ldd /usr/lib/x86_64-linux-gnu/libmpv.so.2 | grep "not found"
```

### Find Which Package Provides a Library

```bash
# Debian/Ubuntu
apt-file search libpulsecommon-16.1.so
dpkg -S libmpv.so.2

# Fedora
dnf provides */libmpv.so.2
```

### Verify Library is Loadable

```bash
# Check if library can be loaded
ldconfig -p | grep mpv
ldconfig -p | grep qt5pas
```

---

## Snap Debugging

### Enter Snap Shell

Run commands inside the snap's confined environment:

```bash
snap run --shell 3nity-media
```

### Check Environment Variables

```bash
snap run --shell 3nity-media -c 'echo $LD_LIBRARY_PATH'
snap run --shell 3nity-media -c 'echo $QT_PLUGIN_PATH'
```

### Check Library Dependencies Inside Snap

```bash
# List libmpv dependencies
snap run --shell 3nity-media -c 'ldd /snap/3nity-media/current/usr/lib/x86_64-linux-gnu/libmpv.so.2'

# Find missing dependencies
snap run --shell 3nity-media -c 'ldd /snap/3nity-media/current/usr/lib/x86_64-linux-gnu/libmpv.so.2 | grep "not found"'
```

### Debug Library Loading

```bash
# Show library search process
snap run --shell 3nity-media -c 'LD_DEBUG=libs 3nity-media 2>&1 | grep -i mpv | head -20'

# Full library debug output
snap run --shell 3nity-media -c 'LD_DEBUG=libs 3nity-media 2>&1 | head -100'
```

### Explore Snap Contents

```bash
# List snap contents
ls -laR /snap/3nity-media/current/ | head -100

# Find specific files
find /snap/3nity-media/current -name "libmpv*"
find /snap/3nity-media/current -name "*.lang"

# Check binary location
ls -la /snap/3nity-media/current/usr/bin/
```

### Check Snap Permissions

```bash
# List connected interfaces
snap connections 3nity-media

# Connect missing permissions
sudo snap connect 3nity-media:audio-playback
sudo snap connect 3nity-media:pulseaudio
sudo snap connect 3nity-media:removable-media
```

### View Snap Logs

```bash
snap logs 3nity-media
journalctl -f | grep -i 3nity
journalctl -f | grep -i apparmor
```

---

## Flatpak Debugging

### Enter Flatpak Shell

```bash
flatpak run --command=sh com.github.nicod3v.3nity-media
```

### Check Environment Inside Flatpak

```bash
flatpak run --command=sh com.github.nicod3v.3nity-media -c 'echo $LD_LIBRARY_PATH'
flatpak run --command=sh com.github.nicod3v.3nity-media -c 'ls -la /app/lib/'
```

### Debug Library Loading

```bash
flatpak run --command=sh com.github.nicod3v.3nity-media -c 'ldd /app/bin/3nity-media | grep "not found"'
flatpak run --command=sh com.github.nicod3v.3nity-media -c 'LD_DEBUG=libs /app/bin/3nity-media 2>&1 | head -50'
```

### Run with Verbose Output

```bash
flatpak run --verbose com.github.nicod3v.3nity-media
```

### Check Flatpak Permissions

```bash
# Grant filesystem access
flatpak override --user --filesystem=/media com.github.nicod3v.3nity-media
flatpak override --user --filesystem=/run/media com.github.nicod3v.3nity-media

# Show current overrides
flatpak override --user --show com.github.nicod3v.3nity-media
```

---

## AppImage Debugging

### Extract and Run

```bash
# Extract AppImage contents
./3nity-Media-*.AppImage --appimage-extract

# Run extracted version
./squashfs-root/AppRun

# Or with debug
LD_DEBUG=libs ./squashfs-root/AppRun 2>&1 | head -50
```

### Check AppImage Contents

```bash
./3nity-Media-*.AppImage --appimage-extract
ls -laR squashfs-root/ | head -100
find squashfs-root -name "libmpv*"
```

### Run Without FUSE

```bash
./3nity-Media-*.AppImage --appimage-extract-and-run
```

---

## Environment Variables

### Library Loading Debug

| Variable | Description | Example |
|----------|-------------|---------|
| `LD_DEBUG=libs` | Show library search/loading | `LD_DEBUG=libs ./3nity-media 2>&1` |
| `LD_DEBUG=files` | Show file operations | `LD_DEBUG=files ./3nity-media 2>&1` |
| `LD_DEBUG=all` | Full debug output | `LD_DEBUG=all ./3nity-media 2>&1` |
| `LD_LIBRARY_PATH` | Additional library search paths | `LD_LIBRARY_PATH=/opt/lib ./3nity-media` |

### Qt Debug

| Variable | Description | Example |
|----------|-------------|---------|
| `QT_DEBUG_PLUGINS=1` | Debug Qt plugin loading | `QT_DEBUG_PLUGINS=1 ./3nity-media` |
| `QT_PLUGIN_PATH` | Qt plugins location | `echo $QT_PLUGIN_PATH` |
| `QT_QPA_PLATFORM` | Force Qt platform | `QT_QPA_PLATFORM=xcb ./3nity-media` |

### Force Language

```bash
LANG=fr_FR.UTF-8 ./3nity-media
LANG=en_US.UTF-8 ./3nity-media
```

---

## Common Errors

### "Failed to load libmpv library"

**Cause:** libmpv.so.2 or its dependencies not found.

**Debug:**
```bash
# Check if libmpv exists
ldconfig -p | grep mpv

# Check dependencies
ldd /usr/lib/x86_64-linux-gnu/libmpv.so.2 | grep "not found"
```

**Fix:**
```bash
# Debian/Ubuntu
sudo apt install libmpv2

# Or libmpv1 on older systems
sudo apt install libmpv1
```

### "Could not find Qt platform plugin xcb"

**Cause:** Qt platform plugins not in search path.

**Debug:**
```bash
QT_DEBUG_PLUGINS=1 ./3nity-media 2>&1 | grep -i platform
echo $QT_QPA_PLATFORM_PLUGIN_PATH
```

**Fix (for Snap):** Add to snapcraft.yaml:
```yaml
environment:
  QT_PLUGIN_PATH: $SNAP/usr/lib/x86_64-linux-gnu/qt5/plugins
  QT_QPA_PLATFORM_PLUGIN_PATH: $SNAP/usr/lib/x86_64-linux-gnu/qt5/plugins/platforms
```

### "libQt5Pas.so.1: cannot open shared object file"

**Cause:** Qt5 Pascal bindings not found.

**Debug:**
```bash
ldconfig -p | grep qt5pas
ldd /usr/bin/3nity-media | grep -i qt5pas
```

**Fix:**
```bash
sudo apt install libqt5pas1
```

### Snap: Missing Dependencies

**Debug:**
```bash
snap run --shell 3nity-media -c 'ldd /snap/3nity-media/current/usr/lib/x86_64-linux-gnu/libmpv.so.2 | grep "not found"'
```

**Fix:** Add missing packages to `stage-packages` in snapcraft.yaml.

### No Audio Output

**Debug:**
```bash
# Check PulseAudio
pactl info

# Check ALSA devices
aplay -l

# Check snap audio permissions
snap connections 3nity-media | grep audio
```

**Fix:**
```bash
sudo snap connect 3nity-media:audio-playback
sudo snap connect 3nity-media:pulseaudio
```

---

## Reporting Issues

When reporting bugs, include:

1. **Version:** `3nity-media --version`
2. **Installation method:** DEB, Snap, Flatpak, AppImage, or source
3. **Distribution:** `cat /etc/os-release`
4. **Error message:** Full terminal output
5. **Library check:** `ldd /path/to/3nity-media | grep "not found"`

Report issues at: https://github.com/NDXDeveloper/3nity-media/issues

---

## See Also

- [Installation Guide](INSTALL.md)
- [Locale Paths Developer Guide](LOCALE_PATHS_DEVELOPER.md)
- [Configuration Guide](CONFIG.md)
