# Publishing to Flathub

This guide details the steps to publish 3nity Media on Flathub.

---

## Phase 1: Preparation (in your repo)

### 1. Add screenshots

```bash
mkdir -p screenshots
# Add screenshots:
# - screenshots/main-window.png (main window)
# - screenshots/playlist.png (playlist)
# - screenshots/equalizer.png (equalizer)
```

**Recommended dimensions:** 1920x1080 or 1280x720 (16:9 format)

### 2. Update libqt5pas sha256

```bash
# Download and calculate the hash
wget https://downloads.sourceforge.net/project/lazarus/Lazarus%20Releases/2.2.6/qt5pas-2.6-1.tar.gz
sha256sum qt5pas-2.6-1.tar.gz
# Update in flatpak/com.ndxdev.3nity-media.yml
```

### 3. Test the build locally

```bash
cd flatpak
flatpak-builder --user --install-deps-from=flathub --force-clean build com.ndxdev.3nity-media.yml

# Test the app
flatpak-builder --run build com.ndxdev.3nity-media.yml 3nity-media
```

### 4. Validate AppStream metadata

```bash
appstream-util validate com.ndxdev.3nity-media.metainfo.xml
```

---

## Phase 2: Flathub Submission

### 5. Create a submission issue

- Go to: https://github.com/flathub/flathub/issues/new?template=submission.yml
- Fill out the form:

| Field | Value |
|-------|-------|
| **App ID** | `com.ndxdev.3nity-media` |
| **App name** | `3nity Media` |
| **Summary** | `Modern multimedia player powered by libmpv` |
| **Homepage** | `https://github.com/NDXDeveloper/3nity-media` |
| **License** | `GPL-2.0` |

- Check the required boxes (no trademark issues, etc.)

### 6. Wait for approval

- The Flathub team reviews the request (a few days)
- They may request modifications

---

## Phase 3: After Approval

### 7. Flathub creates the repo

- A repo `flathub/com.ndxdev.3nity-media` is created
- You receive push rights

### 8. Push the files

```bash
git clone git@github.com:flathub/com.ndxdev.3nity-media.git
cd com.ndxdev.3nity-media

# Copy files (Flathub structure = flat, no subdirectory)
cp /path/to/your-repo/flatpak/com.ndxdev.3nity-media.yml .
cp /path/to/your-repo/flatpak/com.ndxdev.3nity-media.metainfo.xml .
# Note: the .desktop is referenced from your source repo

git add .
git commit -m "Initial Flathub submission"
git push
```

### 9. Automatic build starts

- Flathub builds the package automatically
- Check at: https://flathub.org/builds

---

## Phase 4: Maintenance

For each new version:

1. Update `metainfo.xml` (add a `<release>`)
2. Update the sources in the manifest if necessary
3. Push to the Flathub repo
4. The build triggers automatically

---

## Final Structure on flathub/com.ndxdev.3nity-media

```
com.ndxdev.3nity-media/
├── com.ndxdev.3nity-media.yml          # Manifest
├── com.ndxdev.3nity-media.metainfo.xml # Metadata
└── flathub.json                        # (optional) Flathub config
```

> **Note:** The `.desktop` file and icons are referenced from your source repo in the manifest.

---

## Local Files

Flatpak files are in the `flatpak/` folder of the project:

| File | Description |
|------|-------------|
| `com.ndxdev.3nity-media.yml` | Flatpak manifest |
| `com.ndxdev.3nity-media.desktop` | Desktop file |
| `com.ndxdev.3nity-media.metainfo.xml` | AppStream metadata |

---

## Useful Links

- [Flathub Documentation](https://docs.flathub.org/)
- [App Submission](https://github.com/flathub/flathub/issues/new?template=submission.yml)
- [AppStream Documentation](https://www.freedesktop.org/software/appstream/docs/)
- [Flatpak Builder](https://docs.flatpak.org/en/latest/flatpak-builder.html)
