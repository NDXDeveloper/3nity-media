# 3nity Media - Developer Quick Start

Get up and running with 3nity Media development in 10 minutes.

---

## Prerequisites

### Linux (Ubuntu/Debian)

```bash
sudo apt install lazarus-ide-qt5 lazarus-src lcl-qt5 fpc \
  libqt5pas1 libqt5pas-dev libmpv-dev ffmpeg git make
```

### Windows

1. Install [Lazarus IDE](https://www.lazarus-ide.org/) (includes Free Pascal)
2. Install [Git](https://git-scm.com/)
3. Download [libmpv](https://sourceforge.net/projects/mpv-player-windows/files/libmpv/) and place `mpv-2.dll` in the project's `bin/x86_64-win64/` folder

---

## Step 1: Clone and Build (2 minutes)

```bash
# Clone the repository
git clone https://github.com/NDXDeveloper/3nity-media.git
cd 3nity-media

# Build the application (Debug mode)
make build-app

# Or build Release version
make build-release
```

**Output:** Binary created in `bin/x86_64-linux/3nity-media`

---

## Step 2: Run the Application (30 seconds)

```bash
make run
```

Or directly:
```bash
./bin/x86_64-linux/3nity-media
```

---

## Step 3: Run the Tests (1 minute)

```bash
# Run all 1,686 tests
make test

# Or quick unit tests only
make quick
```

---

## Step 4: Open in Lazarus IDE (Optional)

1. Open Lazarus IDE
2. File → Open Project
3. Select `src/TrinityMedia.lpi`
4. Press `F9` to compile and run

---

## Project Structure at a Glance

```
3nity-media/
├── src/
│   ├── TrinityMedia.lpr      # Entry point
│   ├── Core/                 # Core logic (MPV, Playlist, Radio)
│   │   ├── uMPVEngine.pas    # MPV wrapper (THE main class)
│   │   ├── uPlaylistManager.pas
│   │   └── uRadioManager.pas
│   ├── Forms/                # UI (MainForm, Playlist, Equalizer...)
│   │   └── uMainForm.pas     # Main window (5000+ lines)
│   ├── Common/               # Config, Types, Utils
│   └── Locale/               # Translation system
├── resources/locale/         # 99 language files (.lang)
├── tests/                    # 1,686 tests
├── docs/                     # Documentation
└── Makefile                  # Build automation
```

---

## Key Files to Know

| File | What it does | Lines |
|------|--------------|-------|
| `src/Core/uMPVEngine.pas` | Wraps libmpv - all playback logic | ~2800 |
| `src/Forms/uMainForm.pas` | Main UI, menus, controls, DVD/Blu-ray | ~6000 |
| `src/Core/uPlaylistManager.pas` | Playlist logic, shuffle, M3U | ~1200 |
| `src/Common/uConfig.pas` | Settings persistence (INI) | ~800 |
| `src/Locale/uLocale.pas` | Translation system | ~400 |

---

## Make Your First Change (5 minutes)

### Example: Add a log message when playing a file

1. Open `src/Forms/uMainForm.pas`

2. Find the `PlayFile` procedure (around line 2000):
   ```pascal
   procedure TfrmMain.PlayFile(const FileName: string);
   ```

3. Add a debug log at the beginning:
   ```pascal
   procedure TfrmMain.PlayFile(const FileName: string);
   begin
     WriteLn('[DEBUG] PlayFile called with: ', FileName);  // Add this line
     // ... rest of the code
   ```

4. Rebuild and run:
   ```bash
   make run
   ```

5. Open a file and check the terminal for your log message.

---

## Common Development Tasks

| Task | Command |
|------|---------|
| Build debug | `make build-app` |
| Build release | `make build-release` |
| Run application | `make run` |
| Run all tests | `make test` |
| Run unit tests only | `make quick` |
| Clean build | `make clean-all` |
| See all targets | `make help` |

---

## Understanding the Main Flow

```
User opens file
      │
      ▼
TfrmMain.PlayFile(FileName)
      │
      ▼
TMPVEngine.PlayMedia(FileName)
      │
      ▼
libmpv (mpv_command "loadfile")
      │
      ▼
mpv events → TMPVEngine.ProcessEvents
      │
      ▼
OnPositionChange, OnFileLoaded, etc.
      │
      ▼
UI updates (seekbar, time, metadata)
```

---

## Quick Reference: Event System

The application uses events to communicate between components:

```pascal
// In MainForm initialization
FMPVEngine.OnPositionChange := @OnMPVPositionChange;
FMPVEngine.OnFileLoaded := @OnMPVFileLoaded;
FMPVEngine.OnEndFile := @OnMPVEndFile;

// Event handler example
procedure TfrmMain.OnMPVPositionChange(Sender: TObject; Position: Double);
begin
  // Update seekbar
  tbSeekbar.Position := Round(Position);
end;
```

---

## Quick Reference: Adding a Translation Key

1. Add key to `resources/locale/en.lang`:
   ```ini
   [Messages]
   MyNewMessage=This is my new message
   ```

2. Use in code:
   ```pascal
   ShowMessage(Locale.Message('MyNewMessage', 'Default text'));
   ```

3. Add translations to other `.lang` files as needed.

---

## Quick Reference: Adding a Menu Item

1. In Lazarus IDE, open `uMainForm.pas` (Form view)
2. Add a `TMenuItem` to the desired menu
3. Double-click to create the `OnClick` handler
4. Implement your logic

Or manually in code:
```pascal
// In FormCreate
mnuMyItem := TMenuItem.Create(mnuFile);
mnuMyItem.Caption := Locale.Menu('MyItem', 'My Item');
mnuMyItem.OnClick := @mnuMyItemClick;
mnuFile.Add(mnuMyItem);
```

---

## Debugging Tips

### Enable MPV Logging

In `uMPVEngine.pas`, the log level is set in `Initialize`:
```pascal
mpv_set_option_string(FHandle, 'msg-level', 'all=v');  // verbose
```

### View Logs in Application

Menu → View → Logs (or the log viewer form)

### Terminal Output

Run from terminal to see all `WriteLn` output:
```bash
./bin/x86_64-linux/3nity-media 2>&1 | tee debug.log
```

---

## Next Steps

- Read [ARCHITECTURE.md](ARCHITECTURE.md) for deep understanding
- Read [CONTRIBUTING.md](CONTRIBUTING.md) for code style
- Read [API_REFERENCE.md](API_REFERENCE.md) for class documentation
- Read [COOKBOOK.md](COOKBOOK.md) for step-by-step recipes

---

## Getting Help

- Check existing [Issues](https://github.com/NDXDeveloper/3nity-media/issues)
- Read the [full documentation](/TOC.md)
- Look at test files in `tests/` for usage examples

---


