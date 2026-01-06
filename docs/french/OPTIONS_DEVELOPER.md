# 3nity Media - Documentation Developpeur: Systeme d'Options

## Architecture du Systeme

### Vue d'ensemble

Le systeme d'options utilise trois composants principaux:

1. **TConfigManager** (`uConfig.pas`) - Gestionnaire de configuration
2. **TAppSettings** (`uTypes.pas`) - Structure des parametres
3. **TfrmOptions** (`uOptions.pas`) - Interface utilisateur

```
┌─────────────────────────────────────────────────────────────────┐
│                     Flux de donnees                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Fichier INI  ←──────→  TConfigManager  ←──────→  TfrmOptions │
│      │                        │                        │        │
│      │                        │                        │        │
│      ▼                        ▼                        ▼        │
│ config.ini        TAppSettings              Controles UI  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Emplacements des Fichiers de Configuration

### Chemins par Plateforme

| Plateforme | Chemin |
|------------|--------|
| Windows | `%APPDATA%\3nity-media\` |
| Linux | `~/.config/3nity-media/` |
| macOS | `~/.config/3nity-media/` |

### Fichiers de Configuration

| Fichier | Description |
|---------|-------------|
| `config.ini` | Configuration principale |
| `history.ini` | Historique de lecture |
| `bookmarks.ini` | Signets |
| `favorites.ini` | Favoris |
| `shortcuts.ini` | Raccourcis personnalises |
| `session_playlist.ini` | Playlist de session |

---

## Structure du Fichier INI

### Sections Disponibles

```ini
[General]
Language=fr
SingleInstance=1
ScreenshotPath=/home/user/Pictures/3nity/
ScreenshotFormat=png
HistoryEnabled=1
HistoryMaxItems=500
AutoSavePlaylist=1

[Video]
Brightness=0
Contrast=0
Saturation=0
Hue=0
Gamma=0
AspectMode=0
AspectFactor=-1
Deinterlace=0
DeinterlaceAlg=0
VideoOutput=auto
HWAccel=1

[Audio]
Volume=100
Muted=0
AudioOutput=auto
AudioDevice=
Channels=2
Normalize=0

[Subtitles]
UseDefault=1
FontName=Arial
FontSize=24
FontColor=16777215
FontBold=0
FontItalic=0
OutlineColor=0
OutlineSize=2
BackgroundColor=0
BackgroundOpacity=0
Position=95
Encoding=UTF-8
AutoLoad=1

[Cache]
DefaultSize=8192
FixedSize=8192
RamdiskSize=0
CDROMSize=4096
RemovableSize=4096
NetworkSize=16384
InternetSize=32768
DVDSize=8192

[Window]
MainLeft=100
MainTop=100
MainWidth=800
MainHeight=600
MainMaximized=0
PlaylistLeft=0
PlaylistTop=0
PlaylistWidth=350
PlaylistHeight=500

[Playlist]
PlaybackMode=0

[Equalizer]
Enabled=0
Band0=0
Band1=0
...
Band9=0

[RecentFiles]
Count=5
File0=/chemin/vers/fichier1.mp4
File1=/chemin/vers/fichier2.mkv
...
```

---

## Structure TAppSettings

Definie dans `uTypes.pas`:

```pascal
TAppSettings = record
  General: TGeneralSettings;
  Video: TVideoSettings;
  Audio: TAudioSettings;
  Subtitles: TSubtitleSettings;
  Cache: TCacheSettings;
  MainWindow: TWindowGeometry;
  PlaylistWindow: TWindowGeometry;
  EqualizerWindow: TWindowGeometry;
  PlaybackMode: TPlaybackMode;
  EqualizerBands: array[0..9] of Double;
  EqualizerEnabled: Boolean;
end;
```

### Sous-structures

```pascal
TGeneralSettings = record
  Language: string;
  SingleInstance: Boolean;
  ScreenshotPath: string;
  ScreenshotFormat: string;
  HistoryEnabled: Boolean;
  HistoryMaxItems: Integer;
  AutoSavePlaylist: Boolean;
end;

TVideoSettings = record
  Brightness: Integer;      // -100 a +100
  Contrast: Integer;        // -100 a +100
  Saturation: Integer;      // -100 a +100
  Hue: Integer;             // -100 a +100
  Gamma: Integer;           // -100 a +100
  AspectMode: Integer;
  AspectFactor: Double;
  Deinterlace: Integer;
  DeinterlaceAlg: Integer;
  VideoOutput: string;
  HWAccel: Boolean;
end;

TAudioSettings = record
  Volume: Integer;          // 0 a 150
  Muted: Boolean;
  AudioOutput: string;
  AudioDevice: string;
  Channels: Integer;
  Normalize: Boolean;
end;
```

---

## TConfigManager - Gestionnaire de Configuration

### Instance Globale

```pascal
uses uConfig;

// Acces via la variable globale
Config.Settings.General.Language;
Config.Settings.Video.Brightness;
Config.Save;
```

### Methodes Principales

```pascal
TConfigManager = class
public
  procedure Load;           // Charge depuis le fichier INI
  procedure Save;           // Sauvegarde vers le fichier INI

  property Settings: TAppSettings;
  property Modified: Boolean;
  property ConfigPath: string;

  // Accesseurs pratiques
  function GetGeneral: TGeneralSettings;
  function GetVideo: TVideoSettings;
  function GetAudio: TAudioSettings;
  function GetSubtitles: TSubtitleSettings;
  function GetCache: TCacheSettings;

  procedure SetGeneral(const Value: TGeneralSettings);
  procedure SetVideo(const Value: TVideoSettings);
  // ...
end;
```

### Initialisation et Destruction

Le `TConfigManager` est cree automatiquement dans la section `initialization`:

```pascal
initialization
  Config := TConfigManager.Create;
  Config.Load;

finalization
  FreeAndNil(Config);
```

---

## Dialogue des Options (uOptions.pas)

### Les Trois Boutons

Le dialogue d'options a trois boutons avec des comportements differents:

| Bouton | Action |
|--------|--------|
| **OK** | Sauvegarde + Applique + Ferme |
| **Cancel** | Ferme sans sauvegarder |
| **Apply** | Sauvegarde + Applique (reste ouvert) |

### Code des Boutons

```pascal
procedure TfrmOptions.btnOKClick(Sender: TObject);
begin
  SaveSettings;     // Sauvegarde dans Config
  ApplySettings;    // Applique a MPVEngine
  Close;            // Ferme le dialogue
end;

procedure TfrmOptions.btnCancelClick(Sender: TObject);
begin
  Close;            // Ferme sans rien faire
end;

procedure TfrmOptions.btnApplyClick(Sender: TObject);
begin
  SaveSettings;     // Sauvegarde dans Config
  ApplySettings;    // Applique a MPVEngine
  FModified := False;
end;
```

---

## L'Importance du Bouton "Appliquer"

### Pourquoi ApplySettings est Essentiel

Le bouton "Appliquer" est crucial car il permet aux changements d'etre **immediatement effectifs** sans fermer le dialogue. Cela permet a l'utilisateur de:

1. Voir l'effet des changements en temps reel
2. Ajuster les parametres iterativement
3. Annuler si le resultat n'est pas satisfaisant

### Flux de Donnees

```
┌────────────────┐     ┌─────────────────┐     ┌──────────────┐
│ Controles UI   │────→│ SaveSettings()  │────→│ Config.Save  │
│ (TrackBars,    │     │ Copie vers      │     │ Ecrit INI    │
│  CheckBoxes)   │     │ Config.Settings │     │              │
└────────────────┘     └─────────────────┘     └──────────────┘
                              │
                              ▼
┌────────────────┐     ┌─────────────────┐
│ MPVEngine      │←────│ ApplySettings() │
│ (Lecture)      │     │ Applique a MPV  │
└────────────────┘     └─────────────────┘
```

---

## Ajouter une Nouvelle Option

### Etape 1: Ajouter au Record de Parametres

Dans `uTypes.pas`, ajoutez le champ dans la structure appropriee:

```pascal
TGeneralSettings = record
  // ... champs existants ...
  NewOption: Boolean;  // Nouvelle option
end;
```

### Etape 2: Definir la Valeur par Defaut

Dans `uConfig.pas`, dans `SetDefaults`:

```pascal
procedure TConfigManager.SetDefaults;
begin
  // ...
  with FSettings.General do
  begin
    // ... valeurs existantes ...
    NewOption := True;  // Valeur par defaut
  end;
end;
```

### Etape 3: Charger depuis l'INI

Dans `uConfig.pas`, dans `LoadGeneralSection`:

```pascal
procedure TConfigManager.LoadGeneralSection;
begin
  with FSettings.General do
  begin
    // ... chargements existants ...
    NewOption := FIniFile.ReadBool(INI_SECTION_GENERAL, 'NewOption', True);
  end;
end;
```

### Etape 4: Sauvegarder vers l'INI

Dans `uConfig.pas`, dans `SaveGeneralSection`:

```pascal
procedure TConfigManager.SaveGeneralSection;
begin
  with FSettings.General do
  begin
    // ... sauvegardes existantes ...
    FIniFile.WriteBool(INI_SECTION_GENERAL, 'NewOption', NewOption);
  end;
end;
```

### Etape 5: Ajouter le Controle UI

Dans `uOptions.pas`, ajoutez le controle et connectez-le:

```pascal
// Declaration dans la classe
chkNewOption: TCheckBox;

// Dans LoadSettings
procedure TfrmOptions.LoadSettings;
begin
  // ...
  chkNewOption.Checked := Settings.General.NewOption;
end;

// Dans SaveSettings
procedure TfrmOptions.SaveSettings;
begin
  // ...
  Settings.General.NewOption := chkNewOption.Checked;
end;

// Dans ApplySettings (si applicable)
procedure TfrmOptions.ApplySettings;
begin
  // ...
  if Config.Settings.General.NewOption then
    frmMain.MPVEngine.EnableFeature
  else
    frmMain.MPVEngine.DisableFeature;
end;
```

### Etape 6: Ajouter la Traduction

Dans les fichiers de langue (`en.lang`, `fr.lang`):

```ini
[Options]
NewOption=Enable new feature
```

Dans `ApplyLocale`:

```pascal
chkNewOption.Caption := _T('Options', 'NewOption', 'Enable new feature');
```

---

## Procedure ApplySettings - Code Complet

La procedure `ApplySettings` applique les parametres sauvegardes a `MPVEngine`:

```pascal
procedure TfrmOptions.ApplySettings;
begin
  if (frmMain <> nil) and (frmMain.MPVEngine <> nil) then
  begin
    { Parametres de capture d'ecran }
    if not DirectoryExists(Config.Settings.General.ScreenshotPath) then
      ForceDirectories(Config.Settings.General.ScreenshotPath);
    frmMain.MPVEngine.SetScreenshotDirectory(Config.Settings.General.ScreenshotPath);
    frmMain.MPVEngine.SetScreenshotFormat(Config.Settings.General.ScreenshotFormat);

    { Parametres de cache }
    frmMain.MPVEngine.SetCacheSize(Config.Settings.Cache.DefaultSize);

    { Parametres audio }
    frmMain.MPVEngine.SetAudioOutput(Config.Settings.Audio.AudioOutput);
    frmMain.MPVEngine.SetAudioDevice(Config.Settings.Audio.AudioDevice);
    frmMain.MPVEngine.SetAudioNormalize(Config.Settings.Audio.Normalize);
    frmMain.MPVEngine.SetAudioChannels(Config.Settings.Audio.Channels);
    frmMain.MPVEngine.ReloadCurrentFile;  // Requis pour les changements audio

    { Parametres des sous-titres }
    if not Config.Settings.Subtitles.UseDefault then
    begin
      frmMain.MPVEngine.SetSubFont(Config.Settings.Subtitles.FontName);
      frmMain.MPVEngine.SetSubFontSize(Config.Settings.Subtitles.FontSize);
      frmMain.MPVEngine.SetSubFontColor(Config.Settings.Subtitles.FontColor);
      frmMain.MPVEngine.SetSubBold(Config.Settings.Subtitles.FontBold);
      frmMain.MPVEngine.SetSubItalic(Config.Settings.Subtitles.FontItalic);
      frmMain.MPVEngine.SetSubOutlineColor(Config.Settings.Subtitles.OutlineColor);
      frmMain.MPVEngine.SetSubOutlineSize(Config.Settings.Subtitles.OutlineSize);
      frmMain.MPVEngine.SetSubPosition(Config.Settings.Subtitles.Position);
      frmMain.MPVEngine.SetSubEncoding(Config.Settings.Subtitles.Encoding);
      frmMain.MPVEngine.SetSubAutoLoad(Config.Settings.Subtitles.AutoLoad);
    end
    else
    begin
      { Reinitialiser aux valeurs par defaut mpv }
      frmMain.MPVEngine.SetSubFont('');
      frmMain.MPVEngine.SetSubFontSize(55);
      // ...
    end;
  end;
end;
```

---

## Bonnes Pratiques

### 1. Toujours Verifier MPVEngine

```pascal
// BON
if (frmMain <> nil) and (frmMain.MPVEngine <> nil) then
  frmMain.MPVEngine.SetVolume(100);

// MAUVAIS - peut causer une erreur
frmMain.MPVEngine.SetVolume(100);
```

### 2. Marquer comme Modifie

```pascal
procedure TfrmOptions.tbBrightnessChange(Sender: TObject);
begin
  lblBrightnessValue.Caption := IntToStr(tbBrightness.Position);
  FModified := True;  // Important pour suivre les modifications
end;
```

### 3. Utiliser des Valeurs par Defaut Sensibles

```pascal
// Dans LoadSettings
Idx := cmbVideoOutput.Items.IndexOf(Settings.Video.VideoOutput);
if Idx >= 0 then
  cmbVideoOutput.ItemIndex := Idx
else
  cmbVideoOutput.ItemIndex := 0;  // Fallback sur 'auto'
```

### 4. Valider les Chemins

```pascal
// Dans ApplySettings
if not DirectoryExists(Config.Settings.General.ScreenshotPath) then
  ForceDirectories(Config.Settings.General.ScreenshotPath);
```

---

## Debogage

### Verifier les Parametres Charges

```pascal
WriteLn('Language: ', Config.Settings.General.Language);
WriteLn('Volume: ', Config.Settings.Audio.Volume);
WriteLn('Config path: ', Config.ConfigPath);
```

### Verifier si des Modifications sont en Attente

```pascal
if Config.Modified then
  WriteLn('Config has unsaved changes');
```

### Forcer la Sauvegarde

```pascal
Config.Modified := True;
Config.Save;
```

---

## Resume: Pattern Standard pour une Nouvelle Option

```pascal
// 1. uTypes.pas - Ajouter le champ
TXxxSettings = record
  NewOption: TypeOption;
end;

// 2. uConfig.pas - SetDefaults
NewOption := DefaultValue;

// 3. uConfig.pas - LoadXxxSection
NewOption := FIniFile.ReadXxx(Section, 'NewOption', Default);

// 4. uConfig.pas - SaveXxxSection
FIniFile.WriteXxx(Section, 'NewOption', NewOption);

// 5. uOptions.pas - LoadSettings
ctrlNewOption.Value := Settings.Xxx.NewOption;

// 6. uOptions.pas - SaveSettings
Settings.Xxx.NewOption := ctrlNewOption.Value;

// 7. uOptions.pas - ApplySettings (si effet immediat requis)
frmMain.MPVEngine.SetNewOption(Config.Settings.Xxx.NewOption);

// 8. Fichiers .lang - Traduction
[Options]
NewOption=Description traduite
```
