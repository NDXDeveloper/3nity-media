# 3nity Media

![License](https://img.shields.io/badge/License-GPL--2.0-blue.svg)
![Platform](https://img.shields.io/badge/Platform-Linux%20%7C%20Windows-lightgrey.svg)
![Lazarus](https://img.shields.io/badge/Lazarus-3.6-orange.svg)
![libmpv](https://img.shields.io/badge/Powered%20by-libmpv-purple.svg)

**A lightweight, cross-platform media player built with Lazarus/Free Pascal and powered by libmpv.**

---

## About

3nity Media is a simple yet capable media player that aims to provide a clean, distraction-free experience for playing your audio and video files. Built with Free Pascal and the Lazarus IDE, it leverages the power of libmpv to handle virtually any media format you throw at it.

**What it offers:**

- Audio and video playback via libmpv
- DVD and Blu-ray folder playback (with automatic fallback mode)
- Playlist management (M3U, M3U8, PLS, XSPF)
- Internet radio streaming with ICY metadata
- 10-band audio equalizer
- Video adjustments (brightness, contrast, saturation, hue)
- Audio visualizations
- Customizable keyboard shortcuts
- Multi-language interface
- Playback history and bookmarks

This is a personal project, shared in the hope it might be useful to others. It's not trying to compete with established players - just offering another option for those who appreciate simplicity.

---

## Screenshots

*Coming soon*

---

## Installation

### Linux

#### From DEB Package (Ubuntu/Debian)

```bash
wget https://github.com/NDXDeveloper/3nity-media/releases/latest/download/3nity-media_amd64.deb
sudo apt install ./3nity-media_amd64.deb
```

#### From AppImage

```bash
wget https://github.com/NDXDeveloper/3nity-media/releases/latest/download/3nity-Media-x86_64.AppImage
chmod +x 3nity-Media-x86_64.AppImage
./3nity-Media-x86_64.AppImage
```

#### From Snap

```bash
wget https://github.com/NDXDeveloper/3nity-media/releases/latest/download/3nity-media_amd64.snap
sudo snap install --dangerous 3nity-media_amd64.snap
```

### Windows

Download the installer from the [Releases](https://github.com/NDXDeveloper/3nity-media/releases) page:

- **System installer** - `3nity-Media-Setup.exe` (requires admin)
- **User installer** - `3nity-Media-Setup-User.exe` (no admin required)
- **Portable** - `3nity-media-windows-portable.zip`

### Dependencies

- **libmpv** - The media playback engine
- **Qt5** - User interface toolkit (Linux)

On Linux, install dependencies with:
```bash
sudo apt install libmpv2 libqt5pas1
```

> **📖 Full Installation Guide:** See [docs/INSTALL.md](docs/INSTALL.md) for complete instructions including all Linux distributions, prerequisites (sudo, snapd), and troubleshooting. Each release also includes an `INSTALL` file with quick reference commands.

---

## Building from Source

### Prerequisites

- [Lazarus IDE](https://www.lazarus-ide.org/) 3.0+
- [Free Pascal Compiler](https://www.freepascal.org/) 3.2+
- libmpv development files
- Qt5 Pascal bindings (Linux)

### Linux

```bash
# Install dependencies
sudo apt install lazarus-ide-qt5 libqt5pas-dev libmpv-dev

# Clone the repository
git clone https://github.com/NDXDeveloper/3nity-media.git
cd 3nity-media

# Build (Debug)
make build-app

# Build (Release - optimized)
make build-release

# Run
make run
```

### Windows

1. Install Lazarus IDE
2. Open `src/TrinityMedia.lpi` in Lazarus
3. Build with `Run > Build` or press `Shift+F9`

---

## Usage

### Basic Controls

| Action | Shortcut |
|--------|----------|
| Play/Pause | `Space` |
| Stop | `S` |
| Next track | `N` |
| Previous track | `P` |
| Volume up | `+` or `Up` |
| Volume down | `-` or `Down` |
| Mute | `M` |
| Fullscreen | `F` or `F11` |
| Seek forward | `Right` |
| Seek backward | `Left` |

### Opening Files

- Drag and drop files or folders onto the video area to play immediately
- Drag and drop elsewhere on the window to add to playlist without playing
- Use `File > Open` or press `Ctrl+O`
- Use `File > Open URL` for streams

### Playlist

- Drag and drop files onto the playlist window to add without playing
- Add files via the playlist menu
- Save playlists in M3U, M3U8, PLS, or XSPF format
- Shuffle and repeat modes available

---

## Project Structure

```
3nity-media/
├── src/
│   ├── Core/           # MPV engine, playlist, radio, visualizations
│   ├── Forms/          # UI forms (main, playlist, equalizer, etc.)
│   ├── Common/         # Configuration, types, utilities
│   ├── Locale/         # Localization system
│   └── Controls/       # Custom UI controls
├── tests/              # Test suite (1,686 tests)
│   ├── Unit/           # Unit tests (940+)
│   ├── Integration/    # Integration tests (198+)
│   ├── Performance/    # Performance tests (252+)
│   ├── Robustness/     # Robustness tests (296+)
│   └── Mocks/          # Mock implementations
├── docs/               # Documentation (English)
│   └── french/         # Documentation (French)
├── resources/          # Icons and assets
└── Makefile            # Build automation
```

---

## Testing

The project includes a comprehensive test suite with **1,686 tests** using FPCUnit:

- **Unit Tests**: 940+ tests (12 test files)
- **Integration Tests**: 198+ tests (6 test files)
- **Performance Tests**: 252+ tests (4 test files)
- **Robustness Tests**: 296+ tests (4 test files)

```bash
# Run all tests
make test

# Quick tests (unit only)
make quick

# Specific test categories
make test-unit
make test-integration
make test-performance
make test-robustness

# Generate HTML report
make report
```

See [docs/TESTING.md](/docs/TESTING.md) for detailed test documentation.

---

## Contributing

Contributions are welcome! Whether it's bug reports, feature suggestions, or pull requests.

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/my-feature`)
3. Commit your changes (`git commit -m 'Add my feature'`)
4. Push to the branch (`git push origin feature/my-feature`)
5. Open a Pull Request

Please keep in mind this is a hobby project maintained in spare time.

---

## Documentation

### English

| Document | Description |
|----------|-------------|
| [SOMMAIRE.md](/SOMMAIRE.md) | Full table of contents |
| [docs/USER_GUIDE.md](/docs/USER_GUIDE.md) | User guide |
| [docs/INSTALL.md](/docs/INSTALL.md) | Installation guide |
| [docs/TESTING.md](/docs/TESTING.md) | Test documentation |
| [docs/ARCHITECTURE.md](/docs/ARCHITECTURE.md) | Architecture overview |
| [docs/CONTRIBUTING.md](/docs/CONTRIBUTING.md) | Contribution guidelines |

### French

| Document | Description |
|----------|-------------|
| [docs/french/USER_GUIDE.md](/docs/french/USER_GUIDE.md) | Guide utilisateur |
| [docs/french/INSTALL.md](/docs/french/INSTALL.md) | Guide d'installation |
| [docs/french/TESTING.md](/docs/french/TESTING.md) | Documentation des tests |

---

## Roadmap

Some ideas for future development (no promises):

- [ ] macOS support
- [ ] Video filters and effects
- [ ] Podcast support
- [ ] Media library with metadata
- [ ] Plugin system

---

## License

3nity Media is released under the **GNU General Public License v2.0**.

You are free to use, modify, and distribute this software under the terms of the GPL-2.0 license. See the [LICENSE](/LICENSE) file for details.

---

## Author

**Nicolas DEOUX**

- [NDXDev@gmail.com](mailto:NDXDev@gmail.com)
- [LinkedIn](https://www.linkedin.com/in/nicolas-deoux-ab295980/)
- [GitHub](https://github.com/NDXDeveloper)

---

## Acknowledgments

This project wouldn't exist without:

- [mpv](https://mpv.io/) - The excellent media player and libmpv library
- [Lazarus](https://www.lazarus-ide.org/) - The Free Pascal RAD IDE
- [Free Pascal](https://www.freepascal.org/) - The compiler that makes it all work
- The open source community for inspiration and shared knowledge

---

<div align="center">

**Happy listening!**

[![Star on GitHub](https://img.shields.io/github/stars/NDXDeveloper/3nity-media?style=social)](https://github.com/NDXDeveloper/3nity-media)

*If you find this useful, a star is always appreciated.*

</div>
