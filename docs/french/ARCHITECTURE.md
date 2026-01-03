# Vue d'Ensemble de l'Architecture - 3nity Media

Ce document fournit une vue d'ensemble technique de l'architecture de 3nity Media, incluant l'organisation des modules, le flux de données et l'intégration libmpv.

## Table des Matières

- [Architecture de Haut Niveau](#architecture-de-haut-niveau)
- [Diagramme des Modules](#diagramme-des-modules)
- [Description des Couches](#description-des-couches)
- [Modules Principaux](#modules-principaux)
- [Flux de Données](#flux-de-donn%C3%A9es)
- [Intégration MPV](#int%C3%A9gration-mpv)
- [Système d'Événements](#syst%C3%A8me-d%C3%A9v%C3%A9nements)
- [Système de Configuration](#syst%C3%A8me-de-configuration)
- [Système de Localisation](#syst%C3%A8me-de-localisation)
- [Modèle de Threading](#mod%C3%A8le-de-threading)

---

## Architecture de Haut Niveau

3nity Media suit un pattern d'architecture en couches avec une séparation claire des responsabilités :

```
┌─────────────────────────────────────────────────────────────────────┐
│                      COUCHE PRÉSENTATION                            │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐       │
│  │MainForm │ │Playlist │ │ Radios  │ │Equalizer│ │ Options │  ...  │
│  └────┬────┘ └────┬────┘ └────┬────┘ └────┬────┘ └────┬────┘       │
└───────┼──────────┼──────────┼──────────┼──────────┼────────────────┘
        │          │          │          │          │
┌───────┴──────────┴──────────┴──────────┴──────────┴────────────────┐
│                         COUCHE MÉTIER                               │
│  ┌──────────────┐ ┌──────────────┐ ┌──────────────┐                │
│  │  MPVEngine   │ │PlaylistMgr  │ │ RadioManager │                │
│  │              │ │              │ │              │                │
│  │ - Lecture    │ │ - Éléments   │ │ - Stations   │                │
│  │ - Pistes     │ │ - Navigation │ │ - Filtrage   │                │
│  │ - Égaliseur  │ │ - Shuffle    │ │ - Favoris    │                │
│  └──────┬───────┘ └──────────────┘ └──────────────┘                │
│         │                                                           │
│  ┌──────┴───────┐ ┌──────────────┐ ┌──────────────┐                │
│  │VisualEffects│ │StreamRecorder│ │   Shortcuts  │                │
│  └──────────────┘ └──────────────┘ └──────────────┘                │
└───────┬─────────────────────────────────────────────────────────────┘
        │
┌───────┴─────────────────────────────────────────────────────────────┐
│                         COUCHE COMMUNE                              │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐       │
│  │ uTypes  │ │ uConfig │ │ uLocale │ │uConstants│ │FileUtils│       │
│  └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘       │
└───────┬─────────────────────────────────────────────────────────────┘
        │
┌───────┴─────────────────────────────────────────────────────────────┐
│                      COUCHE INTÉGRATION                             │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │                         uLibMPV                              │   │
│  │           Bindings Pascal pour l'API C libmpv                │   │
│  └──────────────────────────┬──────────────────────────────────┘   │
└─────────────────────────────┼───────────────────────────────────────┘
                              │
┌─────────────────────────────┴───────────────────────────────────────┐
│                      BIBLIOTHÈQUES EXTERNES                         │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐                 │
│  │   libmpv    │  │   Qt5/LCL   │  │   ffprobe   │                 │
│  │  (lecture)  │  │    (GUI)    │  │ (métadonnées)│                 │
│  └─────────────┘  └─────────────┘  └─────────────┘                 │
└─────────────────────────────────────────────────────────────────────┘
```

---

## Diagramme des Modules

### Structure du Répertoire Source

```
src/
├── TrinityMedia.lpr            # Point d'entrée principal
│
├── Core/                       # Fonctionnalités principales
│   ├── uLibMPV.pas            # Bindings API C libmpv
│   ├── uMPVConst.pas          # Constantes et valeurs par défaut MPV
│   ├── uMPVEngine.pas         # Wrapper MPV haut niveau (2800+ lignes)
│   ├── uPlaylistManager.pas   # Gestion de playlist
│   ├── uRadioManager.pas      # Gestion radio internet
│   ├── uStreamRecorder.pas    # Enregistrement de flux
│   ├── uVisualEffects.pas     # Visualisations audio
│   └── uAppVersion.pas        # Informations de version
│
├── Forms/                      # Formulaires et dialogues GUI
│   ├── uMainForm.pas          # Fenêtre principale
│   ├── uPlaylist.pas          # Fenêtre playlist
│   ├── uRadios.pas            # Navigateur radio
│   ├── uEqualizer.pas         # Égaliseur 10 bandes
│   ├── uOptions.pas           # Dialogue paramètres
│   ├── uVideoAdjust.pas       # Ajustements vidéo
│   ├── uMediaInfo.pas         # Informations média
│   ├── uHistory.pas           # Historique de lecture
│   ├── uBookmarks.pas         # Gestionnaire signets
│   ├── uFavorites.pas         # Gestionnaire favoris
│   ├── uGotoTime.pas          # Dialogue aller à
│   ├── uSleepTimer.pas        # Minuterie de veille
│   ├── uShortcutsEditor.pas   # Éditeur raccourcis clavier
│   ├── uOpenURL.pas           # Dialogue saisie URL
│   ├── uLog.pas               # Visualiseur de log debug
│   └── uAbout.pas             # Dialogue à propos
│
├── Common/                     # Utilitaires partagés
│   ├── uTypes.pas             # Définitions de types et records
│   ├── uConstants.pas         # Constantes application
│   ├── uConfig.pas            # Gestion de configuration
│   ├── uShortcuts.pas         # Système de raccourcis clavier
│   ├── uFileUtils.pas         # Utilitaires fichiers
│   └── uCLIParams.pas         # Parsing ligne de commande
│
├── Locale/                     # Internationalisation
│   └── uLocale.pas            # Système de traduction
│
└── Controls/                   # Contrôles UI personnalisés
    └── uVssScrollbar.pas      # Scrollbar personnalisée
```

### Dépendances entre Modules

```
                    ┌─────────────────┐
                    │TrinityMedia.lpr│
                    └────────┬────────┘
                             │
              ┌──────────────┼──────────────┐
              │              │              │
              ▼              ▼              ▼
        ┌──────────┐  ┌──────────┐  ┌──────────┐
        │uMainForm │  │ uConfig  │  │ uLocale  │
        └────┬─────┘  └──────────┘  └──────────┘
             │
    ┌────────┼────────┬────────────┬───────────┐
    │        │        │            │           │
    ▼        ▼        ▼            ▼           ▼
┌───────┐┌───────┐┌───────┐  ┌─────────┐ ┌─────────┐
│MPV    ││Playlist││Radio  │  │Equalizer│ │Playlist │
│Engine ││Manager ││Manager│  │  Form   │ │  Form   │
└───┬───┘└───────┘└───────┘  └─────────┘ └─────────┘
    │
    ▼
┌───────────┐
│  uLibMPV  │──────► libmpv.so / mpv-2.dll
└───────────┘
```

---

## Description des Couches

### Couche Présentation (`Forms/`)

Gère tous les composants d'interface utilisateur via la Lazarus Component Library (LCL) avec le widget set Qt5.

| Formulaire | Fonction |
|------------|----------|
| `TfrmMain` | Fenêtre principale avec affichage vidéo, contrôles, menus |
| `TfrmPlaylist` | Gestion de playlist avec filtrage et tri |
| `TfrmRadios` | Navigateur radio internet avec répertoire Icecast |
| `TfrmEqualizer` | Égaliseur audio 10 bandes avec presets |
| `TfrmOptions` | Paramètres de l'application |
| `TfrmVideoAdjust` | Luminosité, contraste, saturation, teinte, gamma |
| `TfrmHistory` | Historique de lecture avec support reprise |
| `TfrmBookmarks` | Signets de position par fichier |
| `TfrmFavorites` | Fichiers et stations favoris |

### Couche Métier (`Core/`)

Contient la logique applicative principale, indépendante de l'UI.

| Module | Responsabilité |
|--------|----------------|
| `TMPVEngine` | Wrapper libmpv complet avec système d'événements |
| `TPlaylistManager` | Éléments playlist, navigation, shuffle, tri |
| `TRadioManager` | Chargement stations Icecast, filtrage, favoris |
| `TStreamRecorder` | Enregistrement de flux sur disque |
| `TVisualEffects` | Chaînes de filtres FFmpeg pour visualisations |

### Couche Commune (`Common/`)

Types partagés, utilitaires et services utilisés dans toutes les couches.

| Module | Fonction |
|--------|----------|
| `uTypes` | Définitions de records (`TPlaylistItem`, `TRadioStation`, etc.) |
| `uConfig` | Persistence de configuration basée sur INI |
| `uConstants` | Constantes globales de l'application |
| `uShortcuts` | Gestion des raccourcis clavier |
| `uCLIParams` | Parsing des arguments en ligne de commande |

### Couche Intégration (`Core/uLibMPV.pas`)

Bindings Pascal pour l'API C libmpv, fournissant :
- Définitions de types correspondant à `client.h`
- Déclarations de fonctions pour chargement dynamique
- Chargement de bibliothèque multi-plateforme (Windows, Linux, macOS)

---

## Modules Principaux

### TMPVEngine (`uMPVEngine.pas`)

Le composant central encapsulant les fonctionnalités libmpv.

**Responsabilités :**
- Initialiser et gérer le contexte mpv
- Gérer la lecture (play, pause, stop, seek)
- Gérer les pistes audio/vidéo/sous-titres
- Appliquer l'égaliseur et les ajustements vidéo
- Traiter les événements mpv et les dispatcher à l'application
- Gérer la navigation DVD/Bluray
- Gérer les paramètres de cache par type de source

**Propriétés Clés :**
```pascal
property Status: TMPVStatus;           // État de lecture actuel
property Position: Double;             // Position actuelle en secondes
property Duration: Double;             // Durée totale en secondes
property Volume: Integer;              // Niveau de volume (0-100)
property Muted: Boolean;               // État muet
property Speed: Double;                // Vitesse de lecture
property StreamInfo: TMPVStreamInfo;   // Informations média actuelles
property AudioTracks: TMPVTrackList;   // Pistes audio disponibles
property SubtitleTracks: TMPVTrackList;// Pistes sous-titres disponibles
```

**Méthodes Clés :**
```pascal
procedure Play(const FileName: string);
procedure Pause;
procedure Stop;
procedure Seek(Seconds: Double; Mode: TSeekMode);
procedure SetAudioTrack(ID: Integer);
procedure SetSubtitleTrack(ID: Integer);
procedure SetEqualizerBand(Band: Integer; Value: Double);
procedure SetVideoProperty(Prop: string; Value: Integer);
```

### TPlaylistManager (`uPlaylistManager.pas`)

Gère la playlist avec support de divers formats et modes de lecture.

**Fonctionnalités :**
- Ajouter/supprimer/réorganiser les éléments
- Parser les formats M3U, M3U8, PLS
- Shuffle avec algorithme Fisher-Yates
- Modes de répétition (un, tous, shuffle)
- Recherche et filtre
- Gestion de la sélection

**Événements Clés :**
```pascal
property OnChange: TNotifyEvent;
property OnPlay: TPlaylistPlayEvent;
property OnCurrentChange: TPlaylistItemEvent;
```

### TRadioManager (`uRadioManager.pas`)

Gère les stations radio internet.

**Fonctionnalités :**
- Parser le répertoire XML Icecast
- Filtrer par genre, pays, nom
- Gérer les stations personnalisées (format INI)
- Persistence des favoris

---

## Flux de Données

### Flux de Lecture Média

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│ Utilisateur │────►│  MainForm   │────►│ MPVEngine   │
│(Ouvrir fich)│     │             │     │             │
└─────────────┘     └──────┬──────┘     └──────┬──────┘
                           │                   │
                           │                   ▼
                           │            ┌─────────────┐
                           │            │   libmpv    │
                           │            │             │
                           │            │ - Demuxage  │
                           │            │ - Décodage  │
                           │            │ - Rendu     │
                           │            └──────┬──────┘
                           │                   │
                           │   Événements      │
                           │◄──────────────────┘
                           │
                           ▼
                    ┌─────────────┐
                    │  Mise à jour│
                    │     UI      │
                    │ - Position  │
                    │ - Durée     │
                    │ - Métadonnées│
                    └─────────────┘
```

### Flux Playlist

```
┌──────────────┐
│ Action       │
│ Utilisateur  │
│(Ajout fichier)│
└──────┬───────┘
       │
       ▼
┌──────────────┐     ┌──────────────┐
│ MainForm     │────►│PlaylistManager│
│ (Drop/Open)  │     │              │
└──────────────┘     └──────┬───────┘
                            │
              ┌─────────────┼─────────────┐
              │             │             │
              ▼             ▼             ▼
       ┌──────────┐  ┌──────────┐  ┌──────────┐
       │ Ajouter  │  │ Extraire │  │ Mettre à │
       │ élément  │  │ Métadonnées│ │ jour     │
       │ au tableau│ │ (ffprobe)│  │ Shuffle  │
       └──────────┘  └──────────┘  └──────────┘
                            │
                            ▼
                     ┌──────────────┐
                     │  Événement   │
                     │  OnChange    │
                     └──────┬───────┘
                            │
                            ▼
                     ┌──────────────┐
                     │ Mise à jour  │
                     │ UI (ListView │
                     │  Playlist)   │
                     └──────────────┘
```

### Flux de Configuration

```
┌──────────────┐                    ┌──────────────┐
│ Démarrage    │                    │    Disque    │
│ Application  │                    │              │
└──────┬───────┘                    │ config.ini   │
       │                            │ shortcuts.ini│
       ▼                            │ favorites.ini│
┌──────────────┐  Chargement        │ history.ini  │
│   TConfig    │◄───────────────────┤              │
│              │                    └──────────────┘
│ LoadSettings │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Appliquer à  │
│ - MPVEngine  │
│ - MainForm   │
│ - Playlist   │
└──────────────┘


┌──────────────┐                    ┌──────────────┐
│ Utilisateur  │                    │    Disque    │
│ Modifie      │                    │              │
│ Paramètres   │                    │ config.ini   │
└──────┬───────┘                    │              │
       │                            │              │
       ▼                            │              │
┌──────────────┐  Sauvegarde        │              │
│   TConfig    │───────────────────►│              │
│              │                    │              │
│ SaveSettings │                    │              │
└──────────────┘                    └──────────────┘
```

---

## Intégration MPV

### Chargement de la Bibliothèque

```pascal
// uLibMPV.pas - Chargement dynamique de bibliothèque
const
  {$IFDEF WINDOWS}
  LIBMPV_DLL = 'mpv-2.dll';
  {$ELSE}
  LIBMPV_DLL = 'libmpv.so.2';
  {$ENDIF}

// Charger la bibliothèque à l'exécution
FLibHandle := LoadLibrary(LIBMPV_DLL);
if FLibHandle <> NilHandle then
begin
  mpv_create := GetProcAddress(FLibHandle, 'mpv_create');
  mpv_initialize := GetProcAddress(FLibHandle, 'mpv_initialize');
  // ... charger toutes les fonctions
end;
```

### Cycle de Vie du Contexte MPV

```
┌─────────────────────────────────────────────────────────────┐
│                   CYCLE DE VIE MPV                          │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  1. Créer le Contexte                                       │
│     mpv_create() ──► FHandle                                │
│                                                             │
│  2. Configurer les Options                                  │
│     mpv_set_option_string("wid", WindowHandle)              │
│     mpv_set_option_string("hwdec", "auto")                  │
│     mpv_set_option_string("cache", "yes")                   │
│                                                             │
│  3. Initialiser                                             │
│     mpv_initialize()                                        │
│                                                             │
│  4. Enregistrer le Callback d'Événements                    │
│     mpv_set_wakeup_callback()                               │
│                                                             │
│  5. Observer les Propriétés                                 │
│     mpv_observe_property("time-pos", ...)                   │
│     mpv_observe_property("duration", ...)                   │
│     mpv_observe_property("pause", ...)                      │
│                                                             │
│  6. Boucle d'Événements (basée sur Timer)                   │
│     while mpv_wait_event() do                               │
│       HandleEvent()                                         │
│                                                             │
│  7. Nettoyage                                               │
│     mpv_terminate_destroy()                                 │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Interface de Commandes

```pascal
// Exécuter des commandes mpv via l'interface de commandes
procedure TMPVEngine.SendCommand(const Args: array of string);
var
  Cmd: array of PAnsiChar;
  I: Integer;
begin
  SetLength(Cmd, Length(Args) + 1);
  for I := 0 to High(Args) do
    Cmd[I] := PAnsiChar(AnsiString(Args[I]));
  Cmd[High(Cmd)] := nil;

  mpv_command(FHandle, @Cmd[0]);
end;

// Exemples d'utilisation
SendCommand(['loadfile', FileName, 'replace']);
SendCommand(['seek', FloatToStr(Seconds), 'absolute']);
SendCommand(['set', 'volume', IntToStr(Volume)]);
```

### Système de Propriétés

```pascal
// Obtenir une propriété
function TMPVEngine.GetDoubleProperty(const Name: string): Double;
var
  Value: Double;
begin
  if mpv_get_property(FHandle, PAnsiChar(Name), MPV_FORMAT_DOUBLE, @Value) >= 0 then
    Result := Value
  else
    Result := 0;
end;

// Définir une propriété
procedure TMPVEngine.SetIntProperty(const Name: string; Value: Int64);
begin
  mpv_set_property(FHandle, PAnsiChar(Name), MPV_FORMAT_INT64, @Value);
end;

// Observer les changements de propriétés
mpv_observe_property(FHandle, 0, 'time-pos', MPV_FORMAT_DOUBLE);
mpv_observe_property(FHandle, 0, 'pause', MPV_FORMAT_FLAG);
```

---

## Système d'Événements

### Événements MPV vers Événements Application

```
┌────────────────────┐     ┌────────────────────┐     ┌────────────────────┐
│  Événements mpv    │     │   TMPVEngine       │     │   Événements       │
│                    │     │   Handler          │     │   Application      │
├────────────────────┤     ├────────────────────┤     ├────────────────────┤
│ MPV_EVENT_FILE_    │────►│ HandleFileLoaded   │────►│ OnFileLoaded       │
│   LOADED           │     │                    │     │                    │
├────────────────────┤     ├────────────────────┤     ├────────────────────┤
│ MPV_EVENT_END_FILE │────►│ HandleEndFile      │────►│ OnEndFile          │
├────────────────────┤     ├────────────────────┤     ├────────────────────┤
│ MPV_EVENT_PROPERTY │────►│ HandleProperty     │────►│ OnPositionChange   │
│   _CHANGE          │     │   Change           │     │ OnMetadata         │
│   (time-pos)       │     │                    │     │ OnStatusChange     │
├────────────────────┤     ├────────────────────┤     ├────────────────────┤
│ MPV_EVENT_LOG_MSG  │────►│ HandleLogMessage   │────►│ OnLog              │
└────────────────────┘     └────────────────────┘     └────────────────────┘
```

### Types d'Événements dans TMPVEngine

```pascal
// Événements de statut et lecture
property OnStatusChange: TMPVStatusChangeEvent;
property OnPositionChange: TMPVPositionChangeEvent;
property OnFileLoaded: TMPVFileLoadedEvent;
property OnEndFile: TMPVEndFileEvent;
property OnStartFile: TNotifyEvent;

// Événements de pistes
property OnAudioTrackChange: TMPVTrackChangeEvent;
property OnSubtitleTrackChange: TMPVTrackChangeEvent;
property OnVideoTrackChange: TMPVTrackChangeEvent;

// Événements d'information média
property OnMetadata: TMPVMetadataEvent;
property OnVideoResize: TMPVVideoResizeEvent;
property OnChapterChange: TNotifyEvent;

// Événements de navigation
property OnSeek: TNotifyEvent;
property OnPlaybackRestart: TNotifyEvent;
property OnLoadNextFile: TMPVLoadNextFileEvent;

// Journalisation
property OnLog: TMPVLogEvent;
```

### Gestion des Événements dans MainForm

```pascal
procedure TfrmMain.InitializeMPV;
begin
  FMPVEngine := TMPVEngine.Create(pnlVideo.Handle);

  // Connecter les événements
  FMPVEngine.OnStatusChange := @OnMPVStatusChange;
  FMPVEngine.OnPositionChange := @OnMPVPositionChange;
  FMPVEngine.OnFileLoaded := @OnMPVFileLoaded;
  FMPVEngine.OnEndFile := @OnMPVEndFile;
  FMPVEngine.OnMetadata := @OnMPVMetadata;
  FMPVEngine.OnVideoResize := @OnMPVVideoResize;

  FMPVEngine.Initialize;
end;

procedure TfrmMain.OnMPVPositionChange(Sender: TObject; Position: Double);
begin
  // Mettre à jour le slider de position
  tbPosition.Position := Round(Position);

  // Mettre à jour l'affichage du temps
  lblTime.Caption := FormatTime(Position) + ' / ' + FormatTime(FMPVEngine.Duration);
end;

procedure TfrmMain.OnMPVEndFile(Sender: TObject; Reason: Integer; Error: Integer);
begin
  case Reason of
    MPV_END_FILE_REASON_EOF:
      FPlaylist.PlayNext;  // Passage automatique au suivant
    MPV_END_FILE_REASON_ERROR:
      ShowError('Erreur de lecture : ' + IntToStr(Error));
  end;
end;
```

---

## Système de Configuration

### Fichiers de Configuration

| Fichier | Contenu |
|---------|---------|
| `config.ini` | Paramètres généraux, vidéo, audio, cache |
| `shortcuts.ini` | Raccourcis clavier personnalisés |
| `favorites.ini` | Fichiers et stations favoris |
| `history.ini` | Historique de lecture avec positions |
| `bookmarks.ini` | Signets de position par fichier |
| `playlist.m3u` | Playlist sauvegardée automatiquement |

### Structure TConfig

```pascal
TConfig = class
private
  FConfigPath: string;
  FSettings: TAppSettings;

public
  // Général
  property Language: string;
  property SingleInstance: Boolean;
  property ScreenshotPath: string;

  // Vidéo
  property Brightness: Integer;
  property Contrast: Integer;
  property HWAccel: Boolean;

  // Audio
  property Volume: Integer;
  property EqualizerBands: array[0..9] of Double;

  // Persistence
  procedure LoadSettings;
  procedure SaveSettings;

  // Historique
  procedure AddToHistory(const Item: THistoryItem);
  function GetHistory: THistoryItems;

  // Signets
  procedure AddBookmark(const Item: TBookmarkItem);
  function GetBookmarksForFile(const FileName: string): TBookmarkItems;

  // Favoris
  procedure AddFavorite(const Item: TFavoriteItem);
  function GetFavorites: TFavoriteItems;
end;
```

---

## Système de Localisation

### Chargement des Traductions

```pascal
// uLocale.pas
procedure LoadLanguage(const LangCode: string);
var
  LangFile: string;
  Ini: TIniFile;
begin
  LangFile := GetLangPath + LangCode + '.lang';
  if FileExists(LangFile) then
  begin
    Ini := TIniFile.Create(LangFile);
    try
      // Charger toutes les sections
      LoadSection(Ini, 'Menu');
      LoadSection(Ini, 'Buttons');
      LoadSection(Ini, 'Messages');
      // ...
    finally
      Ini.Free;
    end;
  end;
end;

// Utilisation
Caption := _T('Menu', 'File');  // Retourne "Fichier" traduit
```

### Format des Fichiers de Langue

```ini
[Main]
Language=Français
LanguageCode=fr
Author=Nicolas DEOUX

[Menu]
File=Fichier
Open=Ouvrir
OpenURL=Ouvrir une URL
Exit=Quitter

[Buttons]
Play=Lecture
Pause=Pause
Stop=Arrêt

[Messages]
ErrorFileNotFound=Fichier non trouvé : %s
ConfirmExit=Êtes-vous sûr de vouloir quitter ?
```

---

## Modèle de Threading

### Thread Principal

- Toutes les opérations UI
- Gestion des événements mpv
- Gestion de la configuration
- Gestion de la playlist

### Thread MPV

- Géré en interne par libmpv
- Décodage audio/vidéo
- Rendu
- Opérations réseau (streaming)

### Traitement des Événements Basé sur Timer

```pascal
// Le timer de MainForm interroge les événements mpv
procedure TfrmMain.tmrMPVEventsTimer(Sender: TObject);
begin
  if Assigned(FMPVEngine) then
    FMPVEngine.ProcessEvents;  // Non-bloquant
end;

// MPVEngine traite les événements en file d'attente
procedure TMPVEngine.ProcessEvents;
var
  Event: Pmpv_event;
begin
  repeat
    Event := mpv_wait_event(FHandle, 0);  // Non-bloquant
    if Event^.event_id <> MPV_EVENT_NONE then
      HandleEvent(Event);
  until Event^.event_id = MPV_EVENT_NONE;
end;
```

### Thread Safety

- Les propriétés MPV sont thread-safe
- Mises à jour UI via `Application.QueueAsyncCall` si nécessaire
- Accès à la configuration synchronisé sur le thread principal
- Modifications de playlist synchronisées sur le thread principal

---

## Voir Aussi

- [Guide de Contribution](CONTRIBUTING.md) - Style de code et directives de développement
- [Guide d'Installation](INSTALL.md) - Compilation depuis les sources
- [Guide Utilisateur](USER_GUIDE.md) - Documentation utilisateur final

---

## Informations de Version

- **Dernière mise à jour :** 2026-01-02
- **S'applique à :** 3nity Media v0.x et versions ultérieures
