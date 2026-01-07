# Developer Troubleshooting Guide

Common development issues and their solutions for 3nity Media.

---

## Table of Contents

- [Build Errors](#build-errors)
- [Runtime Errors](#runtime-errors)
- [IDE Issues](#ide-issues)
- [Platform-Specific Issues](#platform-specific-issues)
- [MPV Integration Issues](#mpv-integration-issues)
- [Testing Issues](#testing-issues)
- [Debug Techniques](#debug-techniques)

---

## Build Errors

### Error: Unit not found: qt5

**Symptom:**
```
Fatal: Can't find unit qt5 used by TrinityMedia
```

**Cause:** LCL-Qt5 widget set not installed or not selected.

**Solution:**
```bash
# Install Qt5 LCL packages
sudo apt install lcl-qt5 lazarus-ide-qt5

# Or in Lazarus: Project > Project Options > Additions and Overrides
# Set LCLWidgetType to qt5
```

---

### Error: Identifier not found "mpv_create"

**Symptom:**
```
Error: Identifier not found "mpv_create"
```

**Cause:** uLibMPV.pas not included in uses clause.

**Solution:**
Add to uses clause:
```pascal
uses
  uLibMPV;
```

---

### Error: Can't find unit LCL

**Symptom:**
```
Fatal: Can't find unit LCL
```

**Cause:** Lazarus source packages not installed.

**Solution:**
```bash
sudo apt install lazarus-src
```

---

### Error: Linker error - undefined reference

**Symptom:**
```
/usr/bin/ld: undefined reference to `qt5_*'
```

**Cause:** Qt5Pas development library missing.

**Solution:**
```bash
sudo apt install libqt5pas-dev
```

---

### Error: Wrong number of parameters

**Symptom:**
```
Error: Wrong number of parameters specified for call to "SomeMethod"
```

**Cause:** Method signature changed but callers not updated.

**Solution:**
1. Check the method signature in the unit
2. Update all callers to match
3. Use "Find in Files" (Ctrl+Shift+F) to locate all usages

---

### Error: Duplicate identifier

**Symptom:**
```
Error: Duplicate identifier "SomeName"
```

**Cause:** Two units define the same identifier, or a variable is declared twice.

**Solution:**
- Qualify the identifier with unit name: `uTypes.TPlaylistItem`
- Or rename one of the conflicting identifiers

---

## Runtime Errors

### Error: libmpv.so.2 not found

**Symptom:**
```
Error loading library: libmpv.so.2
Cannot initialize MPV
```

**Cause:** libmpv runtime library not installed.

**Solution:**
```bash
# Linux
sudo apt install libmpv2

# Check it's installed
ldconfig -p | grep libmpv
```

---

### Error: Access violation at address

**Symptom:**
```
An unhandled exception occurred at $XXXXXXXX:
EAccessViolation: Access violation
```

**Cause:** Null pointer dereference or use-after-free.

**Debug Steps:**
1. Enable heap trace: Add `-gh` to compiler options
2. Run with debug info: `make build-app` (not release)
3. Check the stack trace for the offending line
4. Common causes:
   - Using an object before `Create`
   - Using an object after `Free`
   - Event handler on destroyed form

---

### Error: External SIGSEGV

**Symptom:**
```
External: SIGSEGV
```

**Cause:** Segmentation fault, usually from invalid memory access.

**Common Causes & Solutions:**

| Cause | Solution |
|-------|----------|
| Qt5Pas bug on exit | Known issue, usually harmless |
| Invalid form handle | Check form exists before accessing |
| MPV callback on destroyed engine | Check `Assigned(FMPVEngine)` |
| Thread accessing UI | Use `Application.QueueAsyncCall` |

---

### Error: Cannot open file 'concat:...'

**Symptom:**
```
Cannot open file 'concat:/path/to/file1.vob|/path/to/file2.vob'
```

**Cause:** MPV does not support the concat: protocol (unlike ffmpeg).

**Solution:**
Use playlist approach instead:
```pascal
// Wrong
PlayFile('concat:' + File1 + '|' + File2);

// Correct
AddFilesToPlaylist([File1, File2], True);
```

---

### Application hangs on startup

**Possible Causes:**

1. **MPV initialization timeout**
   - Check libmpv is installed
   - Check video output is available

2. **Infinite loop in FormCreate**
   - Add WriteLn statements to identify location
   - Use debugger with breakpoints

3. **Blocking network call**
   - Radio station loading should be async
   - Check internet connectivity

---

## IDE Issues

### Lazarus doesn't find project files

**Symptom:** Red underlines everywhere, units not found.

**Solution:**
1. Open correct `.lpi` file: `src/TrinityMedia.lpi`
2. Check Project > Project Inspector for all units
3. Rebuild: Run > Clean Up and Build

---

### Form designer shows blank form

**Symptom:** Opening a form shows empty designer.

**Solution:**
1. Check `.lfm` file exists alongside `.pas`
2. Try closing and reopening the form
3. Check for syntax errors in the `.pas` file

---

### Code completion not working

**Symptom:** Ctrl+Space does nothing.

**Solution:**
1. Tools > Options > Editor > Code Completion
2. Enable "Identifier completion"
3. Project > Build (compilation populates symbols)

---

### Debugger won't start

**Symptom:** F9 does nothing or shows error.

**Solution:**
```bash
# Install GDB
sudo apt install gdb

# In Lazarus: Tools > Options > Debugger
# Set "Debugger type and path" to /usr/bin/gdb
```

---

## Platform-Specific Issues

### Windows: mpv-2.dll not found

**Symptom:** Application crashes on start with DLL error.

**Solution:**
1. Download mpv-2.dll from [SourceForge](https://sourceforge.net/projects/mpv-player-windows/files/libmpv/)
2. Place in `bin/x86_64-win64/` alongside the executable
3. Or place in system PATH

---

### Windows: Build fails with missing units

**Cause:** Lazarus was installed without all components.

**Solution:**
1. Run Lazarus installer again
2. Select all components
3. Or install missing packages from OPM (Online Package Manager)

---

### Linux: Wrong Qt version

**Symptom:**
```
libqt5pas.so: undefined symbol: _ZN*
```

**Cause:** Qt5Pas compiled against different Qt5 version.

**Solution:**
```bash
# Reinstall Qt5Pas
sudo apt install --reinstall libqt5pas1 libqt5pas-dev
```

---

### Linux: No sound output

**Symptom:** Video plays but no audio.

**Cause:** MPV audio output misconfigured.

**Debug:**
```bash
# Test mpv directly
mpv --ao=help  # List available outputs
mpv test.mp3   # Test playback
```

**Solution in code:**
```pascal
// In TMPVEngine.Initialize
mpv_set_option_string(FHandle, 'ao', 'pulse,alsa');
```

---

## MPV Integration Issues

### MPV events not firing

**Symptom:** OnPositionChange, OnFileLoaded not called.

**Cause:** Event processing timer not running or events not connected.

**Checklist:**
1. Is `tmrMPVEvents` enabled?
2. Is `ProcessEvents` being called?
3. Are event handlers assigned in `InitializeMPV`?

```pascal
// Check event assignment
FMPVEngine.OnPositionChange := @OnMPVPositionChange;  // Correct (@ required)
FMPVEngine.OnPositionChange := OnMPVPositionChange;   // Wrong
```

---

### MPV property returns 0

**Symptom:** `FMPVEngine.Duration` always returns 0.

**Cause:** Property queried before file loaded.

**Solution:**
Query properties in `OnFileLoaded` event:
```pascal
procedure TfrmMain.OnMPVFileLoaded(Sender: TObject);
begin
  // Now Duration is available
  tbPosition.Max := Round(FMPVEngine.Duration);
end;
```

---

### Video not displaying

**Symptom:** Audio plays but no video window.

**Cause:** Wrong window handle passed to MPV.

**Solution:**
```pascal
// Ensure video panel exists and has valid handle
FMPVEngine := TMPVEngine.Create(pnlVideo.Handle);

// pnlVideo must be:
// - Created (not nil)
// - Visible
// - Have valid Handle (check pnlVideo.HandleAllocated)
```

---

## Testing Issues

### Tests won't compile

**Symptom:** Errors when running `make test`.

**Solution:**
```bash
# Clean and rebuild test suite
make clean-all
make build-tests
```

---

### Tests fail with "unit not found"

**Cause:** Test project not finding main project units.

**Solution:**
Check `tests/TestProject.lpi` includes correct search paths:
- `../src/Core`
- `../src/Forms`
- `../src/Common`
- `../src/Locale`

---

### Mock objects not working

**Symptom:** Tests use real objects instead of mocks.

**Solution:**
Ensure mocks are used via dependency injection:
```pascal
// In test setup
FMockMPV := TMockMPVEngine.Create;
FMainForm := TfrmMain.Create(nil, FMockMPV);  // Inject mock
```

---

## Debug Techniques

### Enable verbose MPV logging

```pascal
// In TMPVEngine.Initialize
mpv_set_option_string(FHandle, 'msg-level', 'all=v');
```

### Add debug output

```pascal
// Temporary debug (remove before commit)
WriteLn('[DEBUG] ', SomeVariable);

// Or use the log system
if Assigned(OnLog) then
  OnLog(Self, 'debug', 'My message');
```

### Run with terminal output

```bash
# See all WriteLn output
./bin/x86_64-linux/3nity-media 2>&1 | tee debug.log
```

### Use heap trace for memory issues

In Project Options > Compiler Options > Debugging:
- Enable "Use Heaptrc unit"
- Enable "Use Valgrind"

Or add to uses:
```pascal
uses
  {$IFDEF DEBUG}heaptrc,{$ENDIF}
```

### Breakpoint debugging

1. In Lazarus IDE, click in gutter to set breakpoint
2. Press F9 to run with debugger
3. Use F7 (step into), F8 (step over) to trace
4. Watch window: View > Debug Windows > Watches

---

## Getting Help

If none of these solutions work:

1. Search [existing issues](https://github.com/NDXDeveloper/3nity-media/issues)
2. Check [Lazarus Forums](https://forum.lazarus.freepascal.org/)
3. Check [MPV documentation](https://mpv.io/manual/stable/)
4. Open a new issue with:
   - Error message (full)
   - Steps to reproduce
   - Platform and versions
   - Relevant code snippet

---

## See Also

- [QUICKSTART_DEVELOPER.md](QUICKSTART_DEVELOPER.md) - Getting started
- [DEPENDENCIES.md](DEPENDENCIES.md) - Required dependencies
- [DEBUGGING.md](DEBUGGING.md) - Debug tools reference
- [API_REFERENCE.md](API_REFERENCE.md) - Class documentation

---

*Last updated: 2026-01-07*
