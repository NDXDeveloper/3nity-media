# Référence API - 3nity Media

Ce document fournit une référence pour les classes principales de 3nity Media. Ces classes forment l'architecture centrale de l'application.

## Table des Matières

- [TMPVEngine](#tmpvengine)
- [TPlaylistManager](#tplaylistmanager)
- [TRadioManager](#tradiomanager)
- [TConfigManager](#tconfigmanager)

---

## TMPVEngine

**Unité :** `Core/uMPVEngine.pas`

Wrapper complet pour libmpv fournissant toutes les fonctionnalités de lecture multimédia.

### Aperçu

TMPVEngine est le cœur du lecteur multimédia, encapsulant la bibliothèque libmpv. Il gère :
- Lecture vidéo/audio avec support complet des formats
- Navigation DVD/Bluray
- Sélection des pistes (audio, sous-titres, vidéo)
- Égaliseur audio 10 bandes
- Propriétés vidéo (luminosité, contraste, saturation, teinte, gamma)
- Streaming avec métadonnées ICY pour les radios
- Gestion du cache par type de source

### Énumération des États

```pascal
TMPVStatus = (
  msNone,           // Aucun média chargé
  msOpening,        // Ouverture du média
  msClosing,        // Fermeture du média
  msPlayStarting,   // Démarrage de la lecture
  msPlaying,        // En cours de lecture
  msPaused,         // En pause
  msStopped,        // Arrêté
  msError,          // Erreur survenue
  msErrorRetry      // Erreur avec nouvelle tentative
);
```

### Constructeur/Destructeur

| Méthode | Description |
|---------|-------------|
| `Create` | Crée une nouvelle instance de TMPVEngine |
| `Destroy` | Libère les ressources et arrête mpv |

### Méthodes d'Initialisation

| Méthode | Paramètres | Retour | Description |
|---------|------------|--------|-------------|
| `Initialize` | `AWindowHandle: THandle; AWidth, AHeight: Integer` | `Boolean` | Initialise mpv avec un handle de fenêtre pour la sortie vidéo |
| `Shutdown` | - | - | Arrête le moteur mpv |
| `ResizeVideoWindow` | `AWidth, AHeight: Integer` | - | Redimensionne la zone de sortie vidéo |
| `IsRunning` | - | `Boolean` | Vérifie si mpv est en cours d'exécution |

### Méthodes de Contrôle de Lecture

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `PlayMedia` | `URL: string` | Charge et lit un fichier média ou URL |
| `CloseMedia` | - | Ferme le média actuel |
| `Pause` | - | Met en pause la lecture |
| `Resume` | - | Reprend la lecture |
| `Stop` | - | Arrête complètement la lecture |
| `TogglePause` | - | Bascule entre pause et lecture |
| `SendPlayPause` | - | Envoie la commande lecture/pause |

### Méthodes de Navigation

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `SeekAbsolute` | `Seconds: Double` | Se déplace à une position absolue en secondes |
| `SeekRelative` | `Seconds: Double` | Se déplace relativement à la position actuelle |
| `SeekPercent` | `Percent: Double` | Se déplace à un pourcentage de la durée |
| `SeekBy` | `Value: Integer` | Se déplace d'un nombre de secondes |
| `SeekTo` | `Value: Int64; Method: Integer` | Se déplace à une position avec méthode spécifiée |
| `FrameStep` | - | Avance d'une image |
| `FrameBackStep` | - | Recule d'une image |

### Méthodes de Contrôle Audio

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `SetAudioTrack` | `TrackID: Integer` | Sélectionne une piste audio |
| `GetAudioID` | - | Obtient l'ID de la piste audio actuelle |
| `SetAudioID` | `Value: Integer` | Définit la piste audio par ID |
| `SetAudioOutput` | `Driver: string` | Définit le pilote de sortie audio |
| `SetAudioDevice` | `Device: string` | Définit le périphérique de sortie audio |
| `SetAudioNormalize` | `Enable: Boolean` | Active/désactive la normalisation audio |
| `SetAudioChannels` | `Channels: Integer` | Définit le nombre de canaux audio |
| `GetAudioDeviceList` | - | Obtient la liste des périphériques audio |

### Méthodes de l'Égaliseur

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `SetEqualizerBand` | `Band: Integer; dB: Double` | Définit une bande spécifique (-12 à +12 dB) |
| `GetEqualizerBand` | `Band: Integer` | Obtient la valeur de la bande actuelle |
| `SetEqualizerPreset` | `Values: string` | Définit toutes les bandes depuis une chaîne de preset |
| `GetEqualizerPreset` | - | Obtient le preset actuel sous forme de chaîne |
| `ResetEqualizer` | - | Réinitialise toutes les bandes à 0 dB |
| `EnableEqualizer` | `Enable: Boolean` | Active ou désactive l'égaliseur |
| `ApplyEqualizer` | - | Applique les paramètres actuels de l'égaliseur |
| `SetPreamp` | `dB: Double` | Définit le gain du préamplificateur |

### Méthodes de Contrôle des Sous-titres

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `SetSubtitleTrack` | `TrackID: Integer` | Sélectionne une piste de sous-titres |
| `LoadSubtitle` | `FileName: string` | Charge un fichier de sous-titres externe |
| `UnloadSubtitles` | - | Supprime tous les sous-titres chargés |
| `SetSubFont` | `FontName: string` | Définit la police des sous-titres |
| `SetSubFontSize` | `Size: Integer` | Définit la taille de la police |
| `SetSubFontColor` | `Color: Cardinal` | Définit la couleur de la police |
| `SetSubBold` | `Bold: Boolean` | Définit le style gras |
| `SetSubItalic` | `Italic: Boolean` | Définit le style italique |
| `SetSubOutlineColor` | `Color: Cardinal` | Définit la couleur du contour |
| `SetSubOutlineSize` | `Size: Integer` | Définit la taille du contour |
| `SetSubPosition` | `Position: Integer` | Définit la position verticale (0-100) |
| `SetSubEncoding` | `Encoding: string` | Définit l'encodage des caractères |
| `SetSubAutoLoad` | `AutoLoad: Boolean` | Active/désactive le chargement automatique |

### Méthodes de Contrôle Vidéo

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `SetVideoTrack` | `TrackID: Integer` | Sélectionne une piste vidéo |
| `SetAspectRatio` | `Ratio: string` | Définit le rapport d'aspect (ex: "16:9") |
| `Screenshot` | `FileName: string` | Prend une capture d'écran |
| `ScreenshotToFile` | `FileName, Mode: string` | Prend une capture avec mode |
| `SetScreenshotDirectory` | `Path: string` | Définit le répertoire de sauvegarde |
| `SetScreenshotFormat` | `Format: string` | Définit le format (png/jpg) |
| `ToggleFullscreen` | - | Bascule le mode plein écran |
| `ReloadCurrentFile` | - | Recharge le fichier média actuel |

### Méthodes de Navigation DVD/Bluray

| Méthode | Paramètres | Retour | Description |
|---------|------------|--------|-------------|
| `IsDVD` | - | `Boolean` | Vérifie si un DVD est en lecture |
| `IsDVDNav` | - | `Boolean` | Vérifie si la navigation DVD est active |
| `IsBluray` | - | `Boolean` | Vérifie si un Bluray est en lecture |
| `DVDGoMenu` | - | `Boolean` | Va au menu DVD |
| `DVDMenu` | - | - | Ouvre le menu DVD |
| `DVDMenuSelect` | - | - | Sélectionne l'élément de menu actuel |
| `DVDMenuUp/Down/Left/Right` | - | - | Navigue dans le menu |
| `DVDTitle` | `TitleID: Integer` | - | Va au titre |
| `DVDChapter` | `ChapterID: Integer` | - | Va au chapitre |
| `DVDNextChapter` | - | - | Chapitre suivant |
| `DVDPrevChapter` | - | - | Chapitre précédent |

### Accès Générique aux Propriétés

| Méthode | Paramètres | Retour | Description |
|---------|------------|--------|-------------|
| `SendCommand` | `Args: array of string` | - | Envoie une commande avec arguments |
| `SendCmd` | `Command: string` | - | Envoie une chaîne de commande |
| `SetOption` | `Name, Value: string` | - | Définit une option mpv |
| `GetPropertyString` | `Name: string` | `string` | Obtient une propriété chaîne |
| `GetPropertyDouble` | `Name: string` | `Double` | Obtient une propriété double |
| `GetPropertyInt` | `Name: string` | `Int64` | Obtient une propriété entière |
| `GetPropertyBool` | `Name: string` | `Boolean` | Obtient une propriété booléenne |
| `SetPropertyString` | `Name, Value: string` | - | Définit une propriété chaîne |
| `SetPropertyDouble` | `Name: string; Value: Double` | - | Définit une propriété double |
| `SetPropertyInt` | `Name: string; Value: Int64` | - | Définit une propriété entière |
| `SetPropertyBool` | `Name: string; Value: Boolean` | - | Définit une propriété booléenne |

### Propriétés en Lecture Seule

| Propriété | Type | Description |
|-----------|------|-------------|
| `Handle` | `Pmpv_handle` | Handle interne mpv |
| `Status` | `TMPVStatus` | État actuel de la lecture |
| `StreamInfo` | `TMPVStreamInfo` | Informations sur le flux actuel |
| `RenderInfo` | `TMPVRenderInfo` | Informations de rendu vidéo |
| `AudioTracks` | `TMPVTrackList` | Liste des pistes audio |
| `SubtitleTracks` | `TMPVTrackList` | Liste des pistes de sous-titres |
| `VideoTracks` | `TMPVTrackList` | Liste des pistes vidéo |
| `Position` | `Double` | Position actuelle en secondes |
| `Duration` | `Double` | Durée totale en secondes |
| `PercentPos` | `Double` | Position actuelle en pourcentage |
| `TitleCount` | `Integer` | Nombre de titres DVD/BD |
| `ChapterCount` | `Integer` | Nombre de chapitres |
| `CurrentTitle` | `Integer` | Numéro du titre actuel |
| `CurrentChapter` | `Integer` | Numéro du chapitre actuel |
| `MediaFile` | `string` | Chemin du fichier média actuel |
| `FileLoaded` | `string` | Chemin du fichier chargé |
| `Initialized` | `Boolean` | Si mpv est initialisé |
| `LastError` | `string` | Dernier message d'erreur |

### Propriétés en Lecture-Écriture

| Propriété | Type | Description |
|-----------|------|-------------|
| `Volume` | `Integer` | Niveau de volume (0-100) |
| `Muted` | `Boolean` | État muet |
| `Speed` | `Double` | Vitesse de lecture (1.0 = normal) |
| `Brightness` | `Integer` | Luminosité vidéo (-100 à 100) |
| `Contrast` | `Integer` | Contraste vidéo (-100 à 100) |
| `Saturation` | `Integer` | Saturation vidéo (-100 à 100) |
| `Hue` | `Integer` | Teinte vidéo (-100 à 100) |
| `Gamma` | `Integer` | Gamma vidéo (-100 à 100) |
| `SubScale` | `Double` | Facteur d'échelle des sous-titres |
| `SubDelay` | `Double` | Délai des sous-titres en secondes |
| `SubVisible` | `Boolean` | Visibilité des sous-titres |
| `AudioDelay` | `Double` | Délai audio en secondes |
| `EqualizerEnabled` | `Boolean` | Égaliseur activé/désactivé |
| `AspectMode` | `Integer` | Mode de rapport d'aspect |
| `AspectFactor` | `Double` | Facteur de rapport d'aspect personnalisé |
| `Deinterlace` | `Integer` | Mode de désentrelacement |
| `DeinterlaceAlg` | `Integer` | Algorithme de désentrelacement |
| `HWAccel` | `Boolean` | Accélération matérielle activée/désactivée |
| `VideoOutput` | `string` | Pilote de sortie vidéo |
| `AudioOutput` | `string` | Pilote de sortie audio |
| `CacheSize[Index]` | `Integer` | Taille du cache par type de source |

### Événements

| Événement | Type | Description |
|-----------|------|-------------|
| `OnLog` | `TMPVLogEvent` | Message de log reçu |
| `OnLogClear` | `TNotifyEvent` | Log effacé |
| `OnStatusChange` | `TMPVStatusChangeEvent` | État de lecture modifié |
| `OnPositionChange` | `TMPVPositionChangeEvent` | Position mise à jour |
| `OnAudioTrackChange` | `TMPVTrackChangeEvent` | Piste audio modifiée |
| `OnSubtitleTrackChange` | `TMPVTrackChangeEvent` | Piste de sous-titres modifiée |
| `OnVideoTrackChange` | `TMPVTrackChangeEvent` | Piste vidéo modifiée |
| `OnMetadata` | `TMPVMetadataEvent` | Métadonnées reçues (ICY) |
| `OnVideoResize` | `TMPVVideoResizeEvent` | Taille vidéo modifiée |
| `OnProgress` | `TMPVProgressEvent` | Mise à jour de progression |
| `OnEndFile` | `TMPVEndFileEvent` | Fin de lecture du fichier |
| `OnStartFile` | `TNotifyEvent` | Début du chargement du fichier |
| `OnFileLoaded` | `TMPVFileLoadedEvent` | Fichier chargé avec succès |
| `OnSeek` | `TNotifyEvent` | Opération de navigation démarrée |
| `OnPlaybackRestart` | `TNotifyEvent` | Lecture redémarrée après navigation |
| `OnChapterChange` | `TNotifyEvent` | Chapitre modifié |
| `OnLoadNextFile` | `TMPVLoadNextFileEvent` | Demande du fichier suivant |

---

## TPlaylistManager

**Unité :** `Core/uPlaylistManager.pas`

Gestion des listes de lecture avec support M3U/PLS, modes aléatoires et extraction de métadonnées.

### Aperçu

TPlaylistManager gère :
- Formats de playlist M3U, M3U8 et PLS
- Modes de lecture aléatoire et répétition (Normal, Répéter un, Répéter tout, Aléatoire, Aléatoire avec répétition)
- Réorganisation par glisser-déposer
- Extraction de métadonnées via ffprobe
- Persistance des listes de lecture

### Modes de Lecture

```pascal
TPlaybackMode = (
  pmNormal,        // Lecture séquentielle, arrêt à la fin
  pmRepeatOne,     // Répéter la piste actuelle
  pmRepeatAll,     // Boucler toute la playlist
  pmShuffle,       // Ordre aléatoire, sans répétition
  pmShuffleRepeat  // Ordre aléatoire avec répétition
);
```

### Constructeur/Destructeur

| Méthode | Description |
|---------|-------------|
| `Create` | Crée un nouveau gestionnaire de playlist |
| `Destroy` | Libère toutes les ressources |

### Méthodes de Gestion des Éléments

| Méthode | Paramètres | Retour | Description |
|---------|------------|--------|-------------|
| `Add` | `AFileName: string` | `Integer` | Ajoute un fichier et extrait les métadonnées |
| `Add` | `AItem: TPlaylistItem` | `Integer` | Ajoute un élément pré-construit |
| `AddFiles` | `FileNames: TStrings` | - | Ajoute plusieurs fichiers/playlists |
| `Insert` | `Index: Integer; AFileName: string` | - | Insère à une position |
| `Delete` | `Index: Integer` | - | Supprime l'élément à l'index |
| `Remove` | `AFileName: string` | - | Supprime par nom de fichier |
| `Clear` | - | - | Supprime tous les éléments |

### Méthodes de Réorganisation

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `MoveUp` | `Index: Integer` | Déplace l'élément d'une position vers le haut |
| `MoveDown` | `Index: Integer` | Déplace l'élément d'une position vers le bas |
| `Move` | `FromIndex, ToIndex: Integer` | Déplace l'élément à une nouvelle position |
| `Swap` | `Index1, Index2: Integer` | Échange deux éléments |
| `Reverse` | - | Inverse l'ordre de la playlist |
| `Sort` | `Ascending: Boolean` | Trie par nom de fichier |
| `SortByTitle` | `Ascending: Boolean` | Trie par titre |
| `SortByArtist` | `Ascending: Boolean` | Trie par artiste |
| `SortByDuration` | `Ascending: Boolean` | Trie par durée |
| `Randomize` | - | Mélange l'ordre de la playlist |

### Méthodes de Navigation

| Méthode | Paramètres | Retour | Description |
|---------|------------|--------|-------------|
| `GetNext` | - | `Integer` | Obtient l'index suivant selon le mode |
| `GetPrevious` | - | `Integer` | Obtient l'index précédent selon le mode |
| `GetFirst` | - | `Integer` | Obtient le premier index (0) |
| `GetLast` | - | `Integer` | Obtient le dernier index |
| `GetRandom` | - | `Integer` | Obtient un index aléatoire |
| `PlayIndex` | `Index: Integer` | - | Lit un index spécifique |
| `PlayNext` | - | - | Lit la piste suivante |
| `PlayPrevious` | - | - | Lit la piste précédente |
| `PlayFirst` | - | - | Lit la première piste |
| `PlayLast` | - | - | Lit la dernière piste |

### Méthodes de Recherche

| Méthode | Paramètres | Retour | Description |
|---------|------------|--------|-------------|
| `Find` | `AFileName: string` | `Integer` | Recherche par nom de fichier, retourne -1 si non trouvé |
| `FindByTitle` | `ATitle: string` | `Integer` | Recherche par titre |
| `Search` | `AText: string` | `TIntegerDynArray` | Recherche dans tous les champs |
| `IndexOf` | `AFileName: string` | `Integer` | Alias pour Find |
| `Contains` | `AFileName: string` | `Boolean` | Vérifie si le fichier existe |

### Méthodes de Sélection

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `SelectAll` | - | Sélectionne tous les éléments |
| `SelectNone` | - | Désélectionne tous les éléments |
| `SelectInvert` | - | Inverse la sélection |
| `SetSelected` | `Index: Integer; Selected: Boolean` | Définit la sélection d'un élément |
| `GetSelectedCount` | - | Obtient le nombre d'éléments sélectionnés |
| `GetSelectedIndices` | - | Obtient le tableau des indices sélectionnés |
| `DeleteSelected` | - | Supprime tous les éléments sélectionnés |

### Méthodes d'E/S de Playlist

| Méthode | Paramètres | Retour | Description |
|---------|------------|--------|-------------|
| `LoadFromFile` | `AFileName: string` | `Boolean` | Charge un fichier M3U ou PLS |
| `SaveToFile` | `AFileName: string` | `Boolean` | Sauvegarde en M3U ou PLS |
| `LoadM3U` | `AFileName: string` | `Boolean` | Charge le format M3U |
| `LoadPLS` | `AFileName: string` | `Boolean` | Charge le format PLS |
| `SaveM3U` | `AFileName: string` | `Boolean` | Sauvegarde en M3U |
| `SavePLS` | `AFileName: string` | `Boolean` | Sauvegarde en PLS |

### Méthodes de Mise à Jour des Infos

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `UpdateItemInfo` | `Index, Title, Artist, Album, Duration` | Met à jour les métadonnées |
| `UpdateItemDuration` | `Index: Integer; Duration: Double` | Met à jour la durée uniquement |
| `MarkAsPlayed` | `Index: Integer` | Marque l'élément comme lu |
| `ResetPlayedStatus` | - | Réinitialise tous les drapeaux de lecture |

### Méthodes de Statistiques

| Méthode | Retour | Description |
|---------|--------|-------------|
| `GetTotalDuration` | `Double` | Durée totale en secondes |
| `GetTotalDurationString` | `string` | Durée totale formatée |
| `GetPlayedCount` | `Integer` | Nombre d'éléments lus |
| `GetUnplayedCount` | `Integer` | Nombre d'éléments non lus |

### Propriétés

| Propriété | Type | Description |
|-----------|------|-------------|
| `Count` | `Integer` | Nombre d'éléments |
| `Items[Index]` | `TPlaylistItem` | Accès à un élément par index (propriété par défaut) |
| `CurrentIndex` | `Integer` | Index en cours de lecture |
| `CurrentItem` | `TPlaylistItem` | Élément en cours de lecture |
| `PlaybackMode` | `TPlaybackMode` | Mode de lecture actuel |
| `IsEmpty` | `Boolean` | True si aucun élément |
| `HasNext` | `Boolean` | True si suivant existe pour le mode actuel |
| `HasPrevious` | `Boolean` | True si précédent existe pour le mode actuel |
| `Modified` | `Boolean` | True si la playlist a été modifiée |
| `FileName` | `string` | Chemin du fichier playlist chargé |

### Événements

| Événement | Type | Description |
|-----------|------|-------------|
| `OnChange` | `TPlaylistChangeEvent` | La playlist a été modifiée |
| `OnItemAdded` | `TPlaylistItemEvent` | Un élément a été ajouté |
| `OnItemRemoved` | `TPlaylistItemEvent` | Un élément a été supprimé |
| `OnCurrentChange` | `TPlaylistItemEvent` | L'index actuel a changé |
| `OnPlay` | `TPlaylistPlayEvent` | La lecture a été déclenchée |
| `OnClear` | `TNotifyEvent` | La playlist a été vidée |

### Fonctions Utilitaires

```pascal
function FormatDuration(Seconds: Double): string;
function IsPlaylistFile(const AFileName: string): Boolean;
function IsSupportedMediaFile(const AFileName: string): Boolean;
```

---

## TRadioManager

**Unité :** `Core/uRadioManager.pas`

Gestion des stations radio avec support du répertoire Icecast et favoris.

### Aperçu

TRadioManager fournit :
- Parsing XML du répertoire Icecast
- Gestion des stations personnalisées
- Filtrage par genre et recherche
- Gestion des favoris
- Persistance des stations

### Constructeur/Destructeur

| Méthode | Description |
|---------|-------------|
| `Create` | Crée un nouveau gestionnaire radio |
| `Destroy` | Libère toutes les ressources |

### Méthodes de Chargement des Stations

| Méthode | Paramètres | Retour | Description |
|---------|------------|--------|-------------|
| `LoadFromIcecast` | `URL: string` | `Boolean` | Charge depuis le répertoire Icecast |
| `LoadFromFile` | `FileName: string` | `Boolean` | Charge depuis un fichier XML local |
| `LoadFromXML` | `XMLContent: string` | `Boolean` | Parse le contenu XML |
| `LoadDefaultStations` | - | - | Charge les stations françaises intégrées |
| `Clear` | - | - | Efface toutes les stations |

### Méthodes de Filtrage et Recherche

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `FilterByGenre` | `Genre: string` | Affiche uniquement le genre correspondant |
| `FilterByName` | `SearchText: string` | Recherche dans le nom et le genre |
| `FilterByCountry` | `Country: string` | Filtre par pays |
| `FilterFavorites` | - | Affiche uniquement les favoris |
| `ClearFilter` | - | Affiche toutes les stations |
| `Search` | `Text: string` | Recherche dans tous les champs |

### Méthodes de Stations Personnalisées

| Méthode | Paramètres | Retour | Description |
|---------|------------|--------|-------------|
| `AddCustomStation` | `Name, URL, Genre: string; Bitrate: Integer` | `Integer` | Ajoute une station personnalisée |
| `EditCustomStation` | `Index: Integer; Name, URL, Genre: string; Bitrate: Integer` | - | Modifie une station |
| `DeleteCustomStation` | `Index: Integer` | - | Supprime une station |
| `GetCustomStation` | `Index: Integer` | `TRadioStation` | Obtient une station par index |

### Méthodes des Favoris

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `AddToFavorites` | `Station: TRadioStation` | Ajoute aux favoris |
| `RemoveFromFavorites` | `Index: Integer` | Supprime des favoris |
| `IsFavorite` | `AURL: string` | Vérifie si l'URL est en favoris |
| `ToggleFavorite` | `Station: TRadioStation` | Bascule l'état favori |
| `GetFavorite` | `Index: Integer` | Obtient un favori par index |

### Méthodes de Persistance

| Méthode | Paramètres | Retour | Description |
|---------|------------|--------|-------------|
| `SaveCustomStations` | `FileName: string` | `Boolean` | Sauvegarde les stations personnalisées |
| `LoadCustomStations` | `FileName: string` | `Boolean` | Charge les stations personnalisées |
| `SaveFavorites` | `FileName: string` | `Boolean` | Sauvegarde les favoris |
| `LoadFavorites` | `FileName: string` | `Boolean` | Charge les favoris |

### Méthodes de Lecture

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `PlayStation` | `Index: Integer` | Lit une station de la liste complète |
| `PlayFilteredStation` | `Index: Integer` | Lit une station de la liste filtrée |

### Propriétés

| Propriété | Type | Description |
|-----------|------|-------------|
| `Count` | `Integer` | Nombre total de stations |
| `FilteredCount` | `Integer` | Nombre de stations filtrées |
| `CustomCount` | `Integer` | Nombre de stations personnalisées |
| `FavoritesCount` | `Integer` | Nombre de favoris |
| `Stations[Index]` | `TRadioStation` | Accès à une station par index |
| `FilteredStations[Index]` | `TRadioStation` | Accès à une station filtrée |
| `Genres` | `TStringList` | Genres disponibles |
| `LastError` | `string` | Dernier message d'erreur |
| `Loading` | `Boolean` | True si en chargement |
| `Modified` | `Boolean` | True si modifié |

### Événements

| Événement | Type | Description |
|-----------|------|-------------|
| `OnLoad` | `TRadioLoadEvent` | Stations chargées |
| `OnError` | `TRadioErrorEvent` | Erreur survenue |
| `OnPlay` | `TRadioPlayEvent` | Lecture de station demandée |
| `OnChange` | `TNotifyEvent` | Liste de stations modifiée |

### Constantes

```pascal
ICECAST_DIRECTORY_URL = 'https://dir.xiph.org/yp.xml';
```

---

## TConfigManager

**Unité :** `Common/uConfig.pas`

Gestion de la configuration de l'application avec persistance en fichier INI.

### Aperçu

TConfigManager gère :
- Chargement et sauvegarde des paramètres en fichier INI
- Persistance de l'état des fenêtres
- Gestion de l'historique et des fichiers récents
- Signets et favoris
- Sauvegarde/restauration de la playlist de session

### Constructeur/Destructeur

| Méthode | Description |
|---------|-------------|
| `Create` | Crée le gestionnaire de configuration avec les valeurs par défaut |
| `Destroy` | Sauvegarde les modifications en attente et libère les ressources |

### Méthodes Charger/Sauvegarder

| Méthode | Description |
|---------|-------------|
| `Load` | Charge tous les paramètres depuis le fichier INI |
| `Save` | Sauvegarde tous les paramètres dans le fichier INI |

### Accesseurs des Paramètres

| Méthode | Retour | Description |
|---------|--------|-------------|
| `GetGeneral` | `TGeneralSettings` | Obtient les paramètres généraux |
| `GetVideo` | `TVideoSettings` | Obtient les paramètres vidéo |
| `GetAudio` | `TAudioSettings` | Obtient les paramètres audio |
| `GetSubtitles` | `TSubtitleSettings` | Obtient les paramètres de sous-titres |
| `GetCache` | `TCacheSettings` | Obtient les paramètres de cache |
| `SetGeneral` | - | Définit les paramètres généraux |
| `SetVideo` | - | Définit les paramètres vidéo |
| `SetAudio` | - | Définit les paramètres audio |
| `SetSubtitles` | - | Définit les paramètres de sous-titres |
| `SetCache` | - | Définit les paramètres de cache |
| `SetPlaybackMode` | - | Définit le mode de lecture |

### Méthodes d'État des Fenêtres

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `SaveWindowState` | `Section: string; AForm: TForm` | Sauvegarde la position/taille de la fenêtre |
| `SaveWindowStateBounds` | `Section: string; Bounds: TRect; State: TWindowState` | Sauvegarde les limites |
| `LoadWindowState` | `Section: string; AForm: TForm` | Restaure la position/taille de la fenêtre |
| `HasWindowState` | `Section: string` | Vérifie si l'état existe |

### Méthodes de l'Historique

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `AddToHistory` | `FileName, Title: string; Position, Duration: Double` | Ajoute à l'historique |
| `GetHistory` | - | Obtient tous les éléments de l'historique |
| `ClearHistory` | - | Efface tout l'historique |

### Méthodes des Fichiers Récents

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `AddRecentFile` | `FileName: string` | Ajoute aux fichiers récents |
| `GetRecentFiles` | - | Obtient la liste des fichiers récents |
| `RemoveRecentFile` | `FileName: string` | Supprime des fichiers récents |
| `ClearRecentFiles` | - | Efface les fichiers récents |

### Méthodes de Position de Lecture

| Méthode | Paramètres | Retour | Description |
|---------|------------|--------|-------------|
| `SavePlaybackPosition` | `FileName: string; Position: Double` | - | Sauvegarde la position de reprise |
| `GetPlaybackPosition` | `FileName: string` | `Double` | Obtient la position sauvegardée |
| `ClearPlaybackPositions` | - | - | Efface toutes les positions |

### Méthodes des Signets

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `AddBookmark` | `FileName, Name: string; Position: Double` | Ajoute un signet |
| `GetBookmarks` | - | Obtient tous les signets |
| `GetBookmarksForFile` | `FileName: string` | Obtient les signets d'un fichier |
| `RemoveBookmark` | `FileName: string; Position: Double` | Supprime un signet |
| `ClearBookmarks` | - | Efface tous les signets |
| `ClearBookmarksForFile` | `FileName: string` | Efface les signets d'un fichier |

### Méthodes des Favoris

| Méthode | Paramètres | Description |
|---------|------------|-------------|
| `AddFavorite` | `Name, Path: string; FavType; Category: string` | Ajoute un favori |
| `GetFavorites` | - | Obtient tous les favoris |
| `GetFavoritesByCategory` | `Category: string` | Filtre par catégorie |
| `GetFavoritesByType` | `FavType: TFavoriteType` | Filtre par type |
| `IsFavorite` | `Path: string` | Vérifie si le chemin est un favori |
| `RemoveFavorite` | `Path: string` | Supprime un favori |
| `UpdateFavoriteLastPlayed` | `Path: string` | Met à jour la dernière lecture |
| `ClearFavorites` | - | Efface tous les favoris |
| `GetFavoriteCategories` | - | Obtient la liste des catégories |

### Méthodes de Playlist de Session

| Méthode | Paramètres | Retour | Description |
|---------|------------|--------|-------------|
| `SaveSessionPlaylist` | `Items, CurrentIndex, PlaybackMode, CurrentPosition` | - | Sauvegarde la session |
| `LoadSessionPlaylist` | `out Items, CurrentIndex, PlaybackMode, CurrentPosition` | `Boolean` | Restaure la session |
| `ClearSessionPlaylist` | - | - | Efface la session sauvegardée |

### Propriétés

| Propriété | Type | Description |
|-----------|------|-------------|
| `Settings` | `TAppSettings` | Enregistrement complet des paramètres |
| `Modified` | `Boolean` | True si les paramètres ont changé |
| `ConfigPath` | `string` | Chemin du répertoire de configuration |

### Fonctions Globales

```pascal
function GetAppDataDir: string;     // Répertoire des données de l'application
function GetTempDir: string;        // Répertoire temporaire système
function GetConfigDir: string;      // Répertoire de configuration
function GetUserPicturesDir: string; // Dossier Images de l'utilisateur
```

### Fichiers de Configuration

| Fichier | Emplacement | Fonction |
|---------|-------------|----------|
| `config.ini` | `~/.config/3nity-media/` | Paramètres principaux |
| `history.ini` | `~/.config/3nity-media/` | Historique de lecture |
| `bookmarks.ini` | `~/.config/3nity-media/` | Signets |
| `favorites.ini` | `~/.config/3nity-media/` | Favoris |
| `session_playlist.ini` | `~/.config/3nity-media/` | Restauration de session |

---

## Voir Aussi

- [Guide Utilisateur](USER_GUIDE.md)
- [Guide de Configuration](CONFIG.md)
- [Guide de Contribution](CONTRIBUTING.md)

---

## Informations de Version

- **Dernière mise à jour :** 2026-01-02
- **S'applique à :** 3nity Media v0.x et versions ultérieures
