# 3nity Media - Documentation Développeur : Chemins des Fichiers de Langue

## Vue d'ensemble

Ce document décrit comment l'application recherche et charge les fichiers de traduction (`.lang`) sur les différentes plateformes et types d'installation.

Le système est implémenté dans `uLocale.pas` et utilise une détection automatique multi-plateforme.

---

## Ordre de Priorité de Recherche

L'application recherche les fichiers de langue dans l'ordre suivant :

| Priorité | Condition | Chemin |
|----------|-----------|--------|
| 1 | Portable/Développement | `[ExePath]/locale/` |
| 2 | Portable (alternatif) | `[ExePath]/lang/` |
| 3 | macOS App Bundle | `[ExePath]/../Resources/locale/` |
| 4 | Linux standard | `[ExePath]/../share/3nity-media/locale/` |
| 5 | Snap package | `$SNAP/share/3nity-media/locale/` |
| 6 | Flatpak | `/app/share/3nity-media/locale/` |
| 7 | Système (global) | `/usr/share/3nity-media/locale/` |
| 8 | Système (local) | `/usr/local/share/3nity-media/locale/` |
| 9 | Fallback | `[ExePath]/locale/` |

> **Note :** `[ExePath]` représente le dossier contenant l'exécutable de l'application.

---

## Plateformes Supportées

### Windows

#### Installation Portable (ZIP)

```
C:\3nity-media\
├── 3nity-media.exe
├── libmpv-2.dll
└── locale\
    ├── en.lang
    ├── fr.lang
    └── de.lang
```

**Chemin détecté :** `C:\3nity-media\locale\`

#### Installation Standard (NSIS/Inno Setup)

```
C:\Program Files\3nity-media\
├── 3nity-media.exe
├── libmpv-2.dll
└── locale\
    ├── en.lang
    └── fr.lang
```

**Chemin détecté :** `C:\Program Files\3nity-media\locale\`

---

### macOS

#### App Bundle (.app)

```
3nity-media.app/
└── Contents/
    ├── MacOS/
    │   └── 3nity-media          ← Exécutable
    ├── Resources/
    │   └── locale/             ← Emplacement standard macOS
    │       ├── en.lang
    │       └── fr.lang
    ├── Frameworks/
    │   └── libmpv.dylib
    └── Info.plist
```

**Chemin détecté :** `3nity-media.app/Contents/Resources/locale/`

> **Note :** Sur macOS, le dossier `Resources` est l'emplacement standard pour les fichiers de données d'une application.

#### Homebrew

```
/usr/local/bin/3nity-media                    ← Exécutable (symlink)
/usr/local/Cellar/3nity-media/1.0/bin/3nity-media
/usr/local/share/3nity-media/locale/
├── en.lang
└── fr.lang
```

**Chemin détecté :** `/usr/local/share/3nity-media/locale/`

---

### Linux

#### Installation Portable (tar.gz)

```
~/Apps/3nity-media/
├── 3nity-media                  ← Exécutable
├── libmpv.so.2
└── locale/
    ├── en.lang
    └── fr.lang
```

**Chemin détecté :** `~/Apps/3nity-media/locale/`

#### Package Debian/Ubuntu (.deb)

```
/usr/bin/3nity-media             ← Exécutable
/usr/lib/x86_64-linux-gnu/libmpv.so.2
/usr/share/3nity-media/
└── locale/
    ├── en.lang
    └── fr.lang
/usr/share/applications/3nity-media.desktop
/usr/share/icons/hicolor/256x256/apps/3nity-media.png
```

**Chemin détecté :** `/usr/share/3nity-media/locale/`

##### Fichier debian/install (exemple)

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
│   └── 3nity-media              ← Exécutable
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

**Variable d'environnement :** `$SNAP=/snap/3nity-media/current`

**Chemin détecté :** `/snap/3nity-media/current/share/3nity-media/locale/`

##### snapcraft.yaml (exemple)

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
│   └── 3nity-media              ← Exécutable
├── lib/
│   └── libmpv.so.2
└── share/
    └── 3nity-media/
        └── locale/
            ├── en.lang
            └── fr.lang
```

**Chemin détecté :** `/app/share/3nity-media/locale/`

##### Manifest Flatpak (exemple)

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

Structure interne de l'AppImage (montée) :

```
/tmp/.mount_3nityXXXXXX/
└── usr/
    ├── bin/
    │   └── 3nity-media          ← Exécutable
    ├── lib/
    │   └── libmpv.so.2
    └── share/
        └── 3nity-media/
            └── locale/
                ├── en.lang
                └── fr.lang
```

**Chemin détecté :** `/tmp/.mount_3nityXXXXXX/usr/share/3nity-media/locale/`

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

## Implémentation Technique

### Code Source (uLocale.pas)

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

### Directives de Compilation

| Directive | Plateforme |
|-----------|------------|
| `{$IFDEF DARWIN}` | macOS uniquement |
| `{$IFDEF UNIX}` | Linux, macOS, BSD |
| `{$IFDEF WINDOWS}` | Windows uniquement |

---

## Détection Automatique de la Langue

### Linux/Unix

L'application détecte la langue du système via les variables d'environnement :

1. `LC_ALL`
2. `LC_MESSAGES`
3. `LANG`

Exemple : `fr_FR.UTF-8` → extrait `fr` → charge `fr.lang`

### Windows

Sur Windows, la détection utilise `GetUserDefaultUILanguage` ou les paramètres régionaux.

### macOS

macOS utilise les mêmes variables que Linux, plus les préférences système.

---

## Débogage

### Vérifier le Chemin Utilisé

```pascal
WriteLn('Chemin locale: ', Locale.LangPath);
WriteLn('Langue actuelle: ', Locale.CurrentLanguage);
```

### Lister les Langues Disponibles

```pascal
var
  I: Integer;
begin
  WriteLn('Langues disponibles:');
  for I := 0 to Locale.AvailableLanguages.Count - 1 do
    WriteLn('  ', Locale.AvailableLanguages[I], ' = ', Locale.LanguageNames[I]);
end;
```

### Variables d'Environnement Utiles

```bash
# Linux - Forcer une langue
LANG=de_DE.UTF-8 ./3nity-media

# Snap - Vérifier le chemin
echo $SNAP

# Afficher toutes les variables de locale
locale
```

---

## Bonnes Pratiques pour le Packaging

### 1. Toujours Inclure `en.lang`

L'anglais est la langue de fallback par défaut. Il doit toujours être présent.

### 2. Utiliser le Bon Chemin

| Type de package | Chemin des locales |
|-----------------|-------------------|
| Portable | `locale/` (à côté de l'exe) |
| .deb / .rpm | `/usr/share/3nity-media/locale/` |
| Snap | `share/3nity-media/locale/` (relatif à `$SNAP`) |
| Flatpak | `/app/share/3nity-media/locale/` |
| AppImage | `usr/share/3nity-media/locale/` (dans AppDir) |
| macOS .app | `Contents/Resources/locale/` |

### 3. Vérifier les Permissions

Les fichiers `.lang` doivent être lisibles par l'utilisateur :
```bash
chmod 644 /usr/share/3nity-media/locale/*.lang
```

### 4. Tester sur Chaque Plateforme

Avant de publier, vérifiez que :
- [ ] Les fichiers de langue sont trouvés
- [ ] La langue du système est détectée
- [ ] Le changement de langue fonctionne
- [ ] Le fallback vers l'anglais fonctionne

---

## Tableau Récapitulatif

| Plateforme | Exécutable | Locales | Fonctionne |
|------------|------------|---------|------------|
| Windows Portable | `.\3nity-media.exe` | `.\locale\` | ✅ |
| Windows Installé | `C:\Program Files\...\3nity-media.exe` | `...\locale\` | ✅ |
| macOS App Bundle | `.app/Contents/MacOS/3nity-media` | `.app/Contents/Resources/locale/` | ✅ |
| macOS Homebrew | `/usr/local/bin/3nity-media` | `/usr/local/share/3nity-media/locale/` | ✅ |
| Linux Portable | `./3nity-media` | `./locale/` | ✅ |
| Debian/Ubuntu | `/usr/bin/3nity-media` | `/usr/share/3nity-media/locale/` | ✅ |
| Snap | `$SNAP/bin/3nity-media` | `$SNAP/share/3nity-media/locale/` | ✅ |
| Flatpak | `/app/bin/3nity-media` | `/app/share/3nity-media/locale/` | ✅ |
| AppImage | `.mount.../usr/bin/3nity-media` | `.mount.../usr/share/3nity-media/locale/` | ✅ |
