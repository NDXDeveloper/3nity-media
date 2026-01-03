# 3nity Media - Developer Documentation: Language File Paths

## Overview

This document describes how the application searches for and loads translation files (`.lang`) across different platforms and installation types.

The system is implemented in `uLocale.pas` and uses automatic multi-platform detection.

---

## Search Priority Order

The application searches for language files in the following order:

| Priority | Condition | Path |
|----------|-----------|------|
| 1 | Portable/Development | `[ExePath]/locale/` |
| 2 | Portable (alternative) | `[ExePath]/lang/` |
| 3 | macOS App Bundle | `[ExePath]/../Resources/locale/` |
| 4 | Linux standard | `[ExePath]/../share/3nity-media/locale/` |
| 5 | Snap package | `$SNAP/share/3nity-media/locale/` |
| 6 | Flatpak | `/app/share/3nity-media/locale/` |
| 7 | System (global) | `/usr/share/3nity-media/locale/` |
| 8 | System (local) | `/usr/local/share/3nity-media/locale/` |
| 9 | Fallback | `[ExePath]/locale/` |

> **Note:** `[ExePath]` represents the folder containing the application executable.

---

## Supported Platforms

### Windows

#### Portable Installation (ZIP)

```
C:\3nity-media\
├── 3nity-media.exe
├── libmpv-2.dll
└── locale\
    ├── en.lang
    ├── fr.lang
    └── de.lang
```

**Detected path:** `C:\3nity-media\locale\`

#### Standard Installation (NSIS/Inno Setup)

```
C:\Program Files\3nity-media\
├── 3nity-media.exe
├── libmpv-2.dll
└── locale\
    ├── en.lang
    └── fr.lang
```

**Detected path:** `C:\Program Files\3nity-media\locale\`

---

### macOS

#### App Bundle (.app)

```
3nity-media.app/
└── Contents/
    ├── MacOS/
    │   └── 3nity-media          ← Executable
    ├── Resources/
    │   └── locale/             ← Standard macOS location
    │       ├── en.lang
    │       └── fr.lang
    ├── Frameworks/
    │   └── libmpv.dylib
    └── Info.plist
```

**Detected path:** `3nity-media.app/Contents/Resources/locale/`

> **Note:** On macOS, the `Resources` folder is the standard location for application data files.

#### Homebrew

```
/usr/local/bin/3nity-media                    ← Executable (symlink)
/usr/local/Cellar/3nity-media/1.0/bin/3nity-media
/usr/local/share/3nity-media/locale/
├── en.lang
└── fr.lang
```

**Detected path:** `/usr/local/share/3nity-media/locale/`

---

### Linux

#### Portable Installation (tar.gz)

```
~/Apps/3nity-media/
├── 3nity-media                  ← Executable
├── libmpv.so.2
└── locale/
    ├── en.lang
    └── fr.lang
```

**Detected path:** `~/Apps/3nity-media/locale/`

#### Debian/Ubuntu Package (.deb)

```
/usr/bin/3nity-media             ← Executable
/usr/lib/x86_64-linux-gnu/libmpv.so.2
/usr/share/3nity-media/
└── locale/
    ├── en.lang
    └── fr.lang
/usr/share/applications/3nity-media.desktop
/usr/share/icons/hicolor/256x256/apps/3nity-media.png
```

**Detected path:** `/usr/share/3nity-media/locale/`

##### debian/install file (example)

```
bin/3nity-media                          usr/bin
share/3nity-media/locale/*.lang          usr/share/3nity-media/locale
share/applications/3nity-media.desktop   usr/share/applications
share/icons/3nity-media.png              usr/share/icons/hicolor/256x256/apps
```

#### Snap Package

```
/snap/3nity-media/current/
├── bin/
│   └── 3nity-media              ← Executable
├── lib/
│   └── libmpv.so.2
├── share/
│   └── 3nity-media/
│       └── locale/
│           ├── en.lang
│           └── fr.lang
└── meta/
    └── snap.yaml
```

**Environment variable:** `$SNAP=/snap/3nity-media/current`

**Detected path:** `/snap/3nity-media/current/share/3nity-media/locale/`

##### snapcraft.yaml (example)

```yaml
name: 3nity-media
version: '0.1.0'
summary: Modern lightweight media player
description: |
  3nity Media is a modern, lightweight media player powered by libmpv.

base: core22
confinement: strict

apps:
  3nity-media:
    command: bin/3nity-media
    plugs:
      - desktop
      - audio-playback
      - opengl
      - home

parts:
  3nity-media:
    plugin: dump
    source: .
    organize:
      bin/3nity-media: bin/3nity-media
      lib/*: lib/
      locale/*.lang: share/3nity-media/locale/
    stage-packages:
      - libmpv2
```

#### Flatpak

```
/app/
├── bin/
│   └── 3nity-media              ← Executable
├── lib/
│   └── libmpv.so.2
└── share/
    └── 3nity-media/
        └── locale/
            ├── en.lang
            └── fr.lang
```

**Detected path:** `/app/share/3nity-media/locale/`

##### Flatpak Manifest (example)

```yaml
app-id: com.github.3nity-media
runtime: org.freedesktop.Platform
runtime-version: '23.08'
sdk: org.freedesktop.Sdk

command: 3nity-media

finish-args:
  - --share=ipc
  - --socket=x11
  - --socket=pulseaudio
  - --device=dri
  - --filesystem=home

modules:
  - name: 3nity-media
    buildsystem: simple
    build-commands:
      - install -D bin/3nity-media /app/bin/3nity-media
      - install -D -t /app/share/3nity-media/locale/ locale/*.lang
```

#### AppImage

Internal structure of the AppImage (mounted):

```
/tmp/.mount_3nityXXXXXX/
└── usr/
    ├── bin/
    │   └── 3nity-media          ← Executable
    ├── lib/
    │   └── libmpv.so.2
    └── share/
        └── 3nity-media/
            └── locale/
                ├── en.lang
                └── fr.lang
```

**Detected path:** `/tmp/.mount_3nityXXXXXX/usr/share/3nity-media/locale/`

##### AppDir structure

```
3nity-media.AppDir/
├── AppRun
├── 3nity-media.desktop
├── 3nity-media.png
└── usr/
    ├── bin/
    │   └── 3nity-media
    └── share/
        └── 3nity-media/
            └── locale/
                ├── en.lang
                └── fr.lang
```

---

## Technical Implementation

### Source Code (uLocale.pas)

```pascal
constructor TLocaleManager.Create;
var
  ExePath: string;
  DetectedLang: string;
  {$IFDEF UNIX}
  SnapPath: string;
  {$ENDIF}
begin
  inherited Create;

  FCurrentLang := 'en';
  FLangFile := nil;
  FAvailableLanguages := TStringList.Create;
  FLanguageNames := TStringList.Create;

  ExePath := ExtractFilePath(ParamStr(0));
  FLangPath := '';

  { 1. Portable/Development }
  if DirectoryExists(ExePath + 'locale') then
    FLangPath := ExePath + 'locale' + DirectorySeparator
  else if DirectoryExists(ExePath + 'lang') then
    FLangPath := ExePath + 'lang' + DirectorySeparator

  {$IFDEF DARWIN}
  { 2. macOS App Bundle }
  else if DirectoryExists(ExePath + '../Resources/locale') then
    FLangPath := ExePath + '../Resources/locale' + DirectorySeparator
  {$ENDIF}

  {$IFDEF UNIX}
  { 3. Linux standard installation }
  else if DirectoryExists(ExePath + '../share/3nity-media/locale') then
    FLangPath := ExePath + '../share/3nity-media/locale' + DirectorySeparator

  { 4. Snap package }
  else if GetEnvironmentVariable('SNAP') <> '' then
  begin
    SnapPath := GetEnvironmentVariable('SNAP') + '/share/3nity-media/locale';
    if DirectoryExists(SnapPath) then
      FLangPath := SnapPath + '/'
  end

  { 5. Flatpak }
  else if DirectoryExists('/app/share/3nity-media/locale') then
    FLangPath := '/app/share/3nity-media/locale/'

  { 6-7. System-wide fallbacks }
  else if DirectoryExists('/usr/share/3nity-media/locale') then
    FLangPath := '/usr/share/3nity-media/locale/'
  else if DirectoryExists('/usr/local/share/3nity-media/locale') then
    FLangPath := '/usr/local/share/3nity-media/locale/'
  {$ENDIF}

  ;

  { 8. Default fallback }
  if FLangPath = '' then
    FLangPath := ExePath + 'locale' + DirectorySeparator;

  ScanAvailableLanguages;
  DetectedLang := DetectOSLanguage;
  LoadLanguage(DetectedLang);
end;
```

### Compiler Directives

| Directive | Platform |
|-----------|----------|
| `{$IFDEF DARWIN}` | macOS only |
| `{$IFDEF UNIX}` | Linux, macOS, BSD |
| `{$IFDEF WINDOWS}` | Windows only |

---

## Automatic Language Detection

### Linux/Unix

The application detects the system language via environment variables:

1. `LC_ALL`
2. `LC_MESSAGES`
3. `LANG`

Example: `fr_FR.UTF-8` → extracts `fr` → loads `fr.lang`

### Windows

On Windows, detection uses `GetUserDefaultUILanguage` or regional settings.

### macOS

macOS uses the same variables as Linux, plus system preferences.

---

## Debugging

### Check the Used Path

```pascal
WriteLn('Locale path: ', Locale.LangPath);
WriteLn('Current language: ', Locale.CurrentLanguage);
```

### List Available Languages

```pascal
var
  I: Integer;
begin
  WriteLn('Available languages:');
  for I := 0 to Locale.AvailableLanguages.Count - 1 do
    WriteLn('  ', Locale.AvailableLanguages[I], ' = ', Locale.LanguageNames[I]);
end;
```

### Useful Environment Variables

```bash
# Linux - Force a language
LANG=de_DE.UTF-8 ./3nity-media

# Snap - Check the path
echo $SNAP

# Display all locale variables
locale
```

---

## Best Practices for Packaging

### 1. Always Include `en.lang`

English is the default fallback language. It must always be present.

### 2. Use the Correct Path

| Package type | Locale path |
|--------------|-------------|
| Portable | `locale/` (next to exe) |
| .deb / .rpm | `/usr/share/3nity-media/locale/` |
| Snap | `share/3nity-media/locale/` (relative to `$SNAP`) |
| Flatpak | `/app/share/3nity-media/locale/` |
| AppImage | `usr/share/3nity-media/locale/` (in AppDir) |
| macOS .app | `Contents/Resources/locale/` |

### 3. Check Permissions

The `.lang` files must be readable by the user:
```bash
chmod 644 /usr/share/3nity-media/locale/*.lang
```

### 4. Test on Each Platform

Before publishing, verify that:
- [ ] Language files are found
- [ ] System language is detected
- [ ] Language switching works
- [ ] Fallback to English works

---

## Summary Table

| Platform | Executable | Locales | Works |
|----------|------------|---------|-------|
| Windows Portable | `.\3nity-media.exe` | `.\locale\` | ✅ |
| Windows Installed | `C:\Program Files\...\3nity-media.exe` | `...\locale\` | ✅ |
| macOS App Bundle | `.app/Contents/MacOS/3nity-media` | `.app/Contents/Resources/locale/` | ✅ |
| macOS Homebrew | `/usr/local/bin/3nity-media` | `/usr/local/share/3nity-media/locale/` | ✅ |
| Linux Portable | `./3nity-media` | `./locale/` | ✅ |
| Debian/Ubuntu | `/usr/bin/3nity-media` | `/usr/share/3nity-media/locale/` | ✅ |
| Snap | `$SNAP/bin/3nity-media` | `$SNAP/share/3nity-media/locale/` | ✅ |
| Flatpak | `/app/bin/3nity-media` | `/app/share/3nity-media/locale/` | ✅ |
| AppImage | `.mount.../usr/bin/3nity-media` | `.mount.../usr/share/3nity-media/locale/` | ✅ |
