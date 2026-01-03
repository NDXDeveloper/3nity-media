# 3nity Media - Documentation des Tests

## Apercu

Ce document decrit le framework de test, la structure des tests et les procedures pour 3nity Media.

## Etat Actuel

> **Statut :** Le framework de tests est **entierement implemente** avec **1 686 tests** repartis sur toutes les categories. Tous les tests passent avec succes.

### Resume de l'Implementation

#### Tests Unitaires (940+ tests)

| Unite de Test | Tests | Statut |
|---------------|-------|--------|
| uTestCLIParams | 11 | ✅ Implemente |
| uTestConfig | 41 | ✅ Implemente |
| uTestPlaylistManager | 97 | ✅ Implemente |
| uTestRadioManager | 67 | ✅ Implemente |
| uTestStreamRecorder | 87 | ✅ Implemente |
| uTestVisualEffects | 166 | ✅ Implemente |
| uTestShortcuts | 151 | ✅ Implemente |
| uTestLocale | 50 | ✅ Implemente |
| uTestMPVConst | 156 | ✅ Implemente |
| uTestMPVEngine | 114 | ✅ Implemente |
| uTestMockMPVBehavior | ~50 | ✅ Implemente |
| uTestMockPlaylistBehavior | ~50 | ✅ Implemente |
| **Sous-total** | **940+** | **Tous reussis** |

#### Tests d'Integration (198+ tests)

| Unite de Test | Tests | Statut |
|---------------|-------|--------|
| uTestPlayback | ~80 | ✅ Implemente |
| uTestAudioVideo | ~60 | ✅ Implemente |
| uTestStreaming | ~30 | ✅ Implemente |
| uTestVisualization | ~28 | ✅ Implemente |
| uTestMPVPlaylistIntegration | ~50 | ✅ Implemente |
| uTestErrorScenarios | ~50 | ✅ Implemente |
| **Sous-total** | **198+** | **Tous reussis** |

#### Tests de Performance (252+ tests)

| Unite de Test | Tests | Statut |
|---------------|-------|--------|
| uTestStartup | ~60 | ✅ Implemente |
| uTestMediaLoading | ~70 | ✅ Implemente |
| uTestSeekPerformance | ~60 | ✅ Implemente |
| uTestMemoryUsage | ~62 | ✅ Implemente |
| **Sous-total** | **252+** | **Tous reussis** |

#### Tests de Robustesse (296+ tests)

| Unite de Test | Tests | Statut |
|---------------|-------|--------|
| uTestCorruptedFiles | ~80 | ✅ Implemente |
| uTestNetworkTimeout | ~70 | ✅ Implemente |
| uTestRapidCommands | ~80 | ✅ Implemente |
| uTestLongPlayback | ~66 | ✅ Implemente |
| **Sous-total** | **296+** | **Tous reussis** |

| **TOTAL** | **1 686+** | **100% reussis** |

**Structure existante :**
- `tests/Unit/` - 12 fichiers de tests unitaires implementes
- `tests/Integration/` - 6 fichiers de tests d'integration implementes
- `tests/Mocks/` - 2 implementations mock (TMockMPVEngine, TMockPlaylistManager)
- `tests/Performance/` - 4 fichiers de tests de performance implementes
- `tests/Robustness/` - 4 fichiers de tests de robustesse implementes
- `tests/Functional/` - Prevu pour les tests GUI
- `tests/TestData/` - Sous-repertoires crees (audio, video, subtitles, playlists)

## Framework de Test

3nity Media utilise **FPCUnit**, le framework de tests unitaires integre a Lazarus/Free Pascal.

```pascal
uses
  fpcunit, testregistry, testutils;

type
  TTestMPVEngine = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Create;
    procedure Test_Initialize;
    procedure Test_PlayLocalFile;
  end;
```

---

## Structure du Projet

```
3nity-media/
├── tests/
│   ├── TestRunner.lpr ✓            # Programme principal de tests
│   ├── TestRunner.lpi ✓            # Configuration projet tests
│   │
│   ├── Unit/ ✓                     # Tests unitaires (12 fichiers implementes)
│   │   ├── uTestCLIParams.pas ✓    # Tests parametres CLI (11 tests)
│   │   ├── uTestConfig.pas ✓       # Tests configuration INI (41 tests)
│   │   ├── uTestPlaylistManager.pas ✓ # Tests gestionnaire playlist (97 tests)
│   │   ├── uTestRadioManager.pas ✓ # Tests radios Icecast (67 tests)
│   │   ├── uTestStreamRecorder.pas ✓ # Tests enregistrement flux (87 tests)
│   │   ├── uTestVisualEffects.pas ✓ # Tests visualisations audio (166 tests)
│   │   ├── uTestShortcuts.pas ✓    # Tests gestionnaire raccourcis (151 tests)
│   │   ├── uTestLocale.pas ✓       # Tests localisation (50 tests)
│   │   ├── uTestMPVConst.pas ✓     # Tests constantes MPV (156 tests)
│   │   ├── uTestMPVEngine.pas ✓    # Tests moteur MPV (114 tests)
│   │   ├── uTestMockMPVBehavior.pas ✓ # Tests comportement mock MPV (~50 tests)
│   │   └── uTestMockPlaylistBehavior.pas ✓ # Tests comportement mock playlist (~50 tests)
│   │
│   ├── Integration/ ✓              # Tests d'integration (6 fichiers implementes)
│   │   ├── uTestPlayback.pas ✓     # Tests lecture complete (~80 tests)
│   │   ├── uTestAudioVideo.pas ✓   # Tests pistes audio/video (~60 tests)
│   │   ├── uTestStreaming.pas ✓    # Tests flux radio/HTTP (~30 tests)
│   │   ├── uTestVisualization.pas ✓ # Tests visualisation + lecture (~28 tests)
│   │   ├── uTestMPVPlaylistIntegration.pas ✓ # Integration MPV + Playlist (~50 tests)
│   │   └── uTestErrorScenarios.pas ✓ # Tests gestion erreurs (~50 tests)
│   │
│   ├── Performance/ ✓              # Tests de performance (4 fichiers implementes)
│   │   ├── uTestStartup.pas ✓      # Benchmarks temps demarrage (~60 tests)
│   │   ├── uTestMediaLoading.pas ✓ # Performance chargement media (~70 tests)
│   │   ├── uTestSeekPerformance.pas ✓ # Benchmarks operations seek (~60 tests)
│   │   └── uTestMemoryUsage.pas ✓  # Tests allocation memoire (~62 tests)
│   │
│   ├── Robustness/ ✓               # Tests de robustesse (4 fichiers implementes)
│   │   ├── uTestCorruptedFiles.pas ✓ # Gestion fichiers corrompus (~80 tests)
│   │   ├── uTestNetworkTimeout.pas ✓ # Recuperation erreurs reseau (~70 tests)
│   │   ├── uTestRapidCommands.pas ✓ # Stabilite commandes rapides (~80 tests)
│   │   └── uTestLongPlayback.pas ✓ # Tests lecture prolongee (~66 tests)
│   │
│   ├── Functional/ ✓               # Tests fonctionnels (planifie)
│   │   ├── uTestMainForm.pas       # Tests fenetre principale
│   │   ├── uTestPlaylistForm.pas   # Tests fenetre playlist
│   │   ├── uTestEqualizerForm.pas  # Tests fenetre egaliseur
│   │   ├── uTestOptionsForm.pas    # Tests dialogue options
│   │   ├── uTestRadiosForm.pas     # Tests navigateur radios
│   │   ├── uTestBookmarksForm.pas  # Tests dialogue signets
│   │   ├── uTestFavoritesForm.pas  # Tests dialogue favoris
│   │   └── uTestMediaInfoForm.pas  # Tests dialogue infos media
│   │
│   ├── Mocks/ ✓                    # Implementations mock (entierement implemente)
│   │   ├── uMockMPVEngine.pas ✓    # Mock moteur MPV (TMockMPVEngine)
│   │   └── uMockPlaylist.pas ✓     # Mock gestionnaire playlist (TMockPlaylistManager)
│   │
│   └── TestData/ ✓                 # Fichiers de test
│       ├── audio/ ✓                # Fichiers audio de test
│       ├── video/ ✓                # Fichiers video de test
│       ├── subtitles/ ✓            # Fichiers sous-titres de test
│       ├── playlists/ ✓            # Fichiers playlist de test
│       └── streams.txt             # URLs de flux de test
```

---

## Tests Unitaires

### uTestLibMPV.pas - Chargement de la Bibliotheque

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_LoadLibrary` | Charger libmpv (mpv-2.dll / libmpv.so.2) | Pas d'exception, handle ≠ nil |
| `Test_CreateContext` | `mpv_create()` | Retourne handle valide |
| `Test_Initialize` | `mpv_initialize()` | Retourne `MPV_ERROR_SUCCESS` (0) |
| `Test_SetOptionString` | `mpv_set_option_string()` | Retourne 0 pour options valides |
| `Test_Terminate` | `mpv_terminate_destroy()` | Pas de crash, liberation memoire |
| `Test_ErrorString` | `mpv_error_string()` | Retourne chaine non vide |

### uTestMPVEngine.pas - Wrapper du Moteur

#### Initialisation

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_Create` | Creer instance TMPVEngine | Pas d'exception |
| `Test_Initialize` | Initialize(WindowHandle) | Retourne True, Status = msNone |
| `Test_InitializeTwice` | Appeler Initialize deux fois | Pas de crash, ignore 2eme appel |
| `Test_Shutdown` | Shutdown apres Initialize | Status = msNone, Handle = nil |
| `Test_IsRunning` | Verifier IsRunning | False avant Play, True apres |

#### Lecture

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_PlayLocalFile` | PlayMedia(fichier local) | Status → msPlaying, Duration > 0 |
| `Test_PlayInvalidFile` | PlayMedia(fichier inexistant) | Status → msError |
| `Test_PlayURL` | PlayMedia(URL http) | Status → msPlaying |
| `Test_Stop` | Stop() pendant lecture | Status → msStopped |
| `Test_Pause` | Pause() pendant lecture | Status → msPaused |
| `Test_Resume` | Resume() apres Pause | Status → msPlaying |
| `Test_TogglePause` | TogglePause() deux fois | msPlaying → msPaused → msPlaying |

#### Navigation (Seek)

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_SeekAbsolute` | SeekAbsolute(10.0) | Position ≈ 10.0 (±0.5s) |
| `Test_SeekRelative` | SeekRelative(5.0) | Position augmente de ~5s |
| `Test_SeekPercent` | SeekPercent(50.0) | Position ≈ Duration/2 |
| `Test_SeekBeyondEnd` | SeekAbsolute(Duration + 10) | Pas de crash, position ≤ Duration |
| `Test_SeekNegative` | SeekAbsolute(-5) | Position = 0 ou erreur geree |

#### Volume

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_SetVolume` | Volume := 50 | GetPropertyInt('volume') = 50 |
| `Test_VolumeRange` | Volume := 0, 100, 150 | Valeurs acceptees (0-150) |
| `Test_VolumeClamp` | Volume := 200 | Valeur limitee a 150 |
| `Test_Mute` | Muted := True | GetPropertyBool('mute') = True |
| `Test_Unmute` | Muted := False apres True | Volume audible restaure |

#### Egaliseur

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_SetEqualizerBand` | SetEqualizerBand(0, 6.0) | Bande 31Hz = +6dB |
| `Test_EqualizerRange` | Valeurs -12 a +12 dB | Pas d'erreur |
| `Test_EqualizerPreset` | SetEqualizerPreset("0:0:0:0:0:0:0:0:0:0") | Toutes bandes a 0 |
| `Test_ResetEqualizer` | ResetEqualizer() | Toutes bandes a 0 |
| `Test_EnableEqualizer` | EnableEqualizer(True/False) | Filtre active/desactive |

#### Proprietes Video

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_Brightness` | Brightness := 10 | Valeur appliquee |
| `Test_Contrast` | Contrast := -20 | Valeur appliquee |
| `Test_Saturation` | Saturation := 30 | Valeur appliquee |
| `Test_Hue` | Hue := -50 | Valeur appliquee |
| `Test_Gamma` | Gamma := 15 | Valeur appliquee |
| `Test_PropertyRange` | Valeurs -100 a +100 | Pas d'erreur |
| `Test_PropertyClamp` | Brightness := 150 | Limite a 100 |

#### Sous-titres

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_LoadSubtitleSRT` | LoadSubtitle("test.srt") | Sous-titre ajoute a la liste |
| `Test_LoadSubtitleASS` | LoadSubtitle("test.ass") | Sous-titre ajoute |
| `Test_SetSubtitleTrack` | SetSubtitleTrack(1) | Piste selectionnee |
| `Test_SubScale` | SubScale := 1.5 | Taille augmentee |
| `Test_SubDelay` | SubDelay := 0.5 | Delai applique |
| `Test_SubVisible` | SubVisible := False | Sous-titres masques |

#### Pistes Audio

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_AudioTrackCount` | Fichier multi-audio | AudioTracks.Count > 1 |
| `Test_SetAudioTrack` | SetAudioTrack(2) | Piste 2 active |
| `Test_AudioTrackInfo` | Lire infos piste | Language, Codec non vides |

#### Evenements

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_OnStatusChange` | Ecouter changements status | Callback appele |
| `Test_OnPositionChange` | Ecouter position | Callback appele periodiquement |
| `Test_OnFileLoaded` | Ecouter chargement | Callback apres PlayMedia |
| `Test_OnEndFile` | Ecouter fin fichier | Callback a la fin |
| `Test_OnMetadata` | Ecouter metadonnees | Callback avec titre/artiste |
| `Test_OnVideoResize` | Ecouter redimensionnement | Callback avec dimensions |

### uTestVisualEffects.pas - Visualisations

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_Create` | Creer instance TVisualEffects | Pas d'exception |
| `Test_BuildSpectrumFilter` | Generer filtre spectrum | Chaine FFmpeg valide contenant "showspectrum" |
| `Test_BuildWavesFilter` | Generer filtre waves | Chaine FFmpeg valide contenant "showwaves" |
| `Test_BuildVectorFilter` | Generer filtre vector | Chaine FFmpeg valide contenant "avectorscope" |
| `Test_BuildHistogramFilter` | Generer filtre histogram | Chaine FFmpeg valide contenant "ahistogram" |
| `Test_BuildVolumeFilter` | Generer filtre volume | Chaine FFmpeg valide contenant "showvolume" |
| `Test_BuildCombinedFilter` | Generer filtre combine | Chaine FFmpeg valide avec plusieurs filtres |
| `Test_GetAudioOnlyFilter` | Generer filtre audio-only | Chaine FFmpeg lavfi valide |
| `Test_ColorSchemeDefault` | Schema couleur par defaut | Couleurs correctes |
| `Test_ColorSchemeFire` | Schema couleur feu | Couleurs rouges/oranges |
| `Test_ColorSchemeIce` | Schema couleur glace | Couleurs bleues/cyan |
| `Test_ColorSchemeRainbow` | Schema couleur arc-en-ciel | Mode rainbow active |
| `Test_SetDimensions` | Definir largeur/hauteur | Dimensions dans le filtre |
| `Test_EnableDisable` | Activer/desactiver | Enabled change d'etat |
| `Test_ModeChange` | Changer de mode | Mode applique |
| `Test_ApplyPreset` | Appliquer preset visualisation | Parametres preset appliques |
| `Test_RepeatLastZero` | Verifier repeatlast=0 | Present dans overlay filter |

### uTestPlaylist.pas - Gestionnaire de Playlist

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_AddFile` | Ajouter fichier | Count = 1 |
| `Test_AddMultiple` | Ajouter 10 fichiers | Count = 10 |
| `Test_RemoveFile` | Supprimer fichier | Count diminue |
| `Test_Clear` | Vider playlist | Count = 0 |
| `Test_MoveUp` | Deplacer vers haut | Index change |
| `Test_MoveDown` | Deplacer vers bas | Index change |
| `Test_Shuffle` | Activer shuffle | Ordre aleatoire |
| `Test_Loop` | Mode boucle | Revient au debut |
| `Test_LoadM3U` | Charger fichier M3U | Entrees chargees |
| `Test_LoadM3U8` | Charger fichier M3U8 | Entrees UTF-8 chargees |
| `Test_LoadPLS` | Charger fichier PLS | Entrees chargees |
| `Test_SaveM3U` | Sauvegarder M3U | Fichier cree valide |
| `Test_GetNext` | Obtenir suivant | Index +1 |
| `Test_GetPrevious` | Obtenir precedent | Index -1 |
| `Test_GetNextShuffle` | Suivant en shuffle | Index aleatoire |
| `Test_GetNextLoop` | Suivant en boucle | Revient a 0 apres dernier |

### uTestConfig.pas - Configuration

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_LoadDefaults` | Charger config par defaut | Valeurs initialisees |
| `Test_LoadFromINI` | Charger depuis INI | Valeurs restaurees |
| `Test_SaveToINI` | Sauvegarder vers INI | Fichier cree |
| `Test_GetString` | Lire chaine | Valeur correcte |
| `Test_GetInteger` | Lire entier | Valeur correcte |
| `Test_GetBoolean` | Lire booleen | Valeur correcte |
| `Test_SetValue` | Definir valeur | Valeur stockee |
| `Test_SectionExists` | Verifier section | True/False correct |

### uTestShortcuts.pas - Raccourcis Clavier

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_LoadDefaults` | Charger raccourcis par defaut | Raccourcis initialises |
| `Test_LoadFromINI` | Charger depuis fichier INI | Raccourcis personnalises charges |
| `Test_SaveToINI` | Sauvegarder vers INI | Fichier cree/modifie |
| `Test_MatchShortcut` | Reconnaitre raccourci | Action correcte retournee |
| `Test_GetShortcutText` | Obtenir texte raccourci | "Ctrl+P" pour Play |
| `Test_DuplicateDetection` | Detecter doublons | Erreur si meme raccourci |
| `Test_ResetToDefaults` | Reinitialiser | Valeurs par defaut restaurees |

### uTestRadioManager.pas - Radio/Streaming

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_LoadIcecastXML` | Parser XML Icecast | Stations chargees |
| `Test_FilterByGenre` | Filtrer par genre | Resultats filtres |
| `Test_SearchByName` | Rechercher par nom | Resultats trouves |
| `Test_GetStationInfo` | Obtenir infos station | URL, bitrate, genre |
| `Test_PlayStation` | Lire flux radio | Lecture demarre |
| `Test_ParseICYMetadata` | Parser metadonnees ICY | Titre extrait |

### uTestStreamRecorder.pas - Enregistrement de Flux

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_Create` | Creer instance TStreamRecorder | Pas d'exception |
| `Test_Initialize` | Initialiser enregistreur | Etat pret |
| `Test_StartRecording` | Demarrer enregistrement | Etat enregistrement, fichier cree |
| `Test_StopRecording` | Arreter enregistrement | Fichier sauvegarde, etat reinitialise |
| `Test_PauseResume` | Pause/reprise enregistrement | Etats basculent correctement |
| `Test_GenerateFileName` | Generer nom fichier sortie | Chemin valide avec timestamp |
| `Test_SanitizeFileName` | Nettoyer caracteres invalides | Pas de caracteres invalides |
| `Test_ShouldSplitFile` | Verifier conditions split | Detection seuil correcte |
| `Test_SplitRecording` | Diviser en nouveau fichier | Nouveau fichier cree, index incremente |
| `Test_NotifyTrackChange` | Gerer changement piste ICY | Metadonnees mises a jour |
| `Test_AudioFormat` | Tester formats audio (MP3, AAC, OGG) | Extensions correctes |
| `Test_VideoFormat` | Tester formats video (MP4, MKV, WebM) | Extensions correctes |

### uTestCLIParams.pas - Parametres Ligne de Commande

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_HasParam` | Detecter presence parametre | True pour params existants |
| `Test_GetParamValue` | Obtenir valeur parametre | Valeur correcte retournee |
| `Test_ParseTimeToSeconds` | Parser chaines temps | "1:30" → 90.0, "1:00:00" → 3600.0 |
| `Test_ParseStartupOptions` | Parser toutes options demarrage | Toutes options extraites |
| `Test_GetCLIAction` | Detecter action CLI | Action correcte (help/version/license) |
| `Test_ExecuteCLIAction` | Executer action CLI | Sortie generee |
| `Test_InvalidParam` | Gerer parametres invalides | Gestion gracieuse |
| `Test_MissingValue` | Gerer valeurs manquantes | Defaut ou erreur |

### uTestLocale.pas - Localisation

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_Create` | Creer instance TLocaleManager | Pas d'exception |
| `Test_LoadLanguage` | Charger fichier langue | Chaines chargees |
| `Test_GetString` | Obtenir chaine localisee | Traduction correcte |
| `Test_DefaultFallback` | Retour au defaut | Defaut retourne si cle manquante |
| `Test_ScanAvailableLanguages` | Scanner fichiers .lang | Langues detectees |
| `Test_DetectOSLanguage` | Detecter langue OS | Code langue valide |
| `Test_Menu` | Obtenir chaine menu | Traduction menu correcte |
| `Test_Dialog` | Obtenir chaine dialogue | Traduction dialogue correcte |
| `Test_T_Function` | Tester raccourci _T() | Traduction correcte |

---

## Tests d'Integration

> **Statut :** Les 6 fichiers de tests d'integration sont **entierement implementes** avec **198+ tests** reussis.

### uTestPlayback.pas - Integration Lecture (~80 tests)

Tests MPVEngine + PlaylistManager + ConfigManager :

| Classe de Test | Description | Tests |
|----------------|-------------|-------|
| TTestMPVPlaylistIntegration | MPV + Navigation playlist | ~25 |
| TTestMPVConfigIntegration | MPV + Persistance config | ~20 |
| TTestPlaylistConfigIntegration | Playlist + Config + Historique | ~25 |
| TTestPlaybackModeIntegration | Modes lecture (Normal, Repeat, Shuffle) | ~10 |

Scenarios de test cles :
- Navigation playlist avec MPV (Suivant/Precedent/Premier/Dernier)
- Persistance volume et mute entre sessions
- Sauvegarde/restauration position lecture
- Suivi historique et limites (MAX_HISTORY_ITEMS = 500)
- Integration fichiers recents
- Generation et persistance ordre shuffle

### uTestAudioVideo.pas - Integration Audio/Video (~60 tests)

Tests TMPVEngine + TMPVTrackList + gestion pistes :

| Classe de Test | Description | Tests |
|----------------|-------------|-------|
| TTestTrackListIntegration | Operations liste pistes | ~20 |
| TTestAudioTrackIntegration | Selection/info piste audio | ~15 |
| TTestVideoPropertiesIntegration | Proprietes video (luminosite, contraste, etc.) | ~15 |
| TTestEqualizerIntegration | Gestion bandes egaliseur | ~10 |

Scenarios de test cles :
- Operations Add/Remove/Clear liste pistes
- FindByID avec retour index
- Gestion types pistes Audio/Video/Sous-titres
- Plages proprietes video (-100 a +100)
- Configuration egaliseur 10 bandes
- Detection pistes externes (propriete IsExternal)

### uTestStreaming.pas - Integration Streaming (~30 tests)

Tests TRadioManager + TStreamRecorder + TConfigManager :

| Classe de Test | Description | Tests |
|----------------|-------------|-------|
| TTestRadioConfigIntegration | Radio + Persistance config | ~10 |
| TTestRecorderConfigIntegration | Persistance parametres enregistrement | ~10 |
| TTestStreamMetadata | Gestion metadonnees ICY | ~10 |

Scenarios de test cles :
- Gestion stations personnalisees (AddCustomStation, DeleteCustomStation)
- Persistance favoris
- Parametres enregistrement (SplitSizeMB, SplitTimeMinutes)
- Notifications changement piste (NotifyTrackChange avec Artiste/Titre)
- Machine a etats enregistrement

### uTestVisualization.pas - Integration Visualisation (~28 tests)

Tests TVisualEffects + TConfigManager :

| Classe de Test | Description | Tests |
|----------------|-------------|-------|
| TTestVisualizationConfigIntegration | Integration mode et schema couleurs | ~12 |
| TTestFilterGeneration | Generation chaine filtre FFmpeg | ~8 |
| TTestVisualSettings | Acces enregistrement parametres visuels | ~8 |
| TTestModeCycling | Cycle mode/schema | ~4 |
| TTestPresets | Presets visualisation | ~3 |
| TTestVisualizationHelpers | Conversion chaine mode/schema | ~8 |

Scenarios de test cles :
- Changement mode visuel (vmNone, vmSpectrum, vmWaves, vmVector, vmVolume, vmCombined)
- Application schema couleurs (vcsDefault, vcsFire, vcsIce, vcsRainbow)
- Generation chaine filtre (necessite Enabled = True)
- Modification enregistrement parametres (Width, Height, BarCount)
- Cycle NextMode/PreviousMode/NextColorScheme
- Application presets

---

## Objets Mock

> **Statut :** Les implementations mock sont **entierement implementees** et utilisees par les tests unitaires et d'integration.

### TMockMPVEngine (`Mocks/uMockMPVEngine.pas`)

Une implementation mock complete du moteur MPV qui simule le comportement de lecture sans necessiter libmpv.

**Fonctionnalites :**
- Simulation de lecture (charger, lire, pause, stop, seek)
- Gestion des proprietes (volume, vitesse, parametres video, egaliseur)
- Gestion des pistes (audio, video, sous-titres)
- Systeme d'evenements (changement statut, position, fichier charge, fin fichier, erreurs)
- Journal des appels pour verification

**Methodes Cles :**
```pascal
procedure SimulateFileLoad(const FileName: string; Duration: Double; HasVideo: Boolean = True);
procedure SimulatePosition(Seconds: Double);
procedure SimulateEndFile(Reason: Integer = 0);
procedure SimulateError(const ErrorMsg: string);
procedure SimulateMetadata(const Title, Artist: string);
```

### TMockPlaylistManager (`Mocks/uMockPlaylist.pas`)

Une implementation mock complete du gestionnaire de playlist qui fonctionne entierement en memoire.

**Fonctionnalites :**
- Gestion des items (ajouter, supprimer, deplacer, vider)
- Navigation (suivant, precedent, premier, dernier, jouer par index)
- Modes de lecture (normal, repeter tout, repeter un, aleatoire)
- Tri (par nom, inverser, aleatoire)
- Recherche (par nom de fichier, titre, ou recherche generale)
- Systeme d'evenements (changement, lecture, changement courant)
- Journal des appels pour verification

**Methodes Cles :**
```pascal
function Add(const AFileName: string): Integer;
procedure Delete(Index: Integer);
procedure Move(FromIndex, ToIndex: Integer);
procedure PlayFirst; procedure PlayLast; procedure PlayNext; procedure PlayPrevious;
procedure Sort(Ascending: Boolean = True);
procedure Randomize;
function Search(const AQuery: string): TIntegerDynArray;
procedure PopulateWithTestData(ItemCount: Integer);
```

### Exemple d'Utilisation

```pascal
procedure TMyTest.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
  FMPV.OnStatusChange := @OnMPVStatusChange;
  FMPV.OnEndFile := @OnMPVEndFile;
end;

procedure TMyTest.Test_AutoPlayNext;
begin
  FPlaylist.PopulateWithTestData(5);
  FPlaylist.PlayFirst;
  FMPV.SimulateFileLoad(FPlaylist.CurrentItem.FileName, 180.0);

  // Simuler fin de fichier
  FMPV.SimulateEndFile(0);

  // Verifier que la piste suivante a ete lancee
  AssertEquals('Devrait avancer au suivant', 1, FPlaylist.CurrentIndex);
end;
```

---

## Tests Fonctionnels (GUI)

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_OpenFileDialog` | Menu → Ouvrir | Fichier charge |
| `Test_PlayPauseButton` | Clic bouton Play/Pause | Etat change |
| `Test_VolumeSlider` | Deplacer slider volume | Volume change |
| `Test_SeekBar` | Clic sur barre progression | Position change |
| `Test_FullscreenToggle` | Touche F ou F11 | Mode plein ecran |
| `Test_EscapeFullscreen` | Touche Echap | Quitte plein ecran |
| `Test_PlaylistDragDrop` | Glisser-deposer fichiers | Fichiers ajoutes |
| `Test_KeyboardShortcuts` | Tous raccourcis clavier | Actions correctes |
| `Test_ContextMenu` | Clic droit | Menu contextuel affiche |
| `Test_EqualizerSliders` | Manipuler sliders egaliseur | Valeurs changent |
| `Test_VisualizationMenu` | Menu visualisation | Modes disponibles |

---

## Tests de Performance

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_StartupTime` | Temps demarrage application | < 2 secondes |
| `Test_FileLoadTime` | Temps chargement fichier local | < 1 seconde |
| `Test_SeekResponseTime` | Temps reponse seek | < 500ms |
| `Test_MemoryUsage` | Memoire apres 1h lecture | < 500 MB |
| `Test_CPUUsage` | CPU pendant lecture video | < 30% (avec HW decode) |
| `Test_VisualizationCPU` | CPU avec visualisation | < 50% |
| `Test_PlaylistLoadTime` | Charger playlist 1000 entrees | < 2 secondes |

---

## Tests de Robustesse

| Test | Description | Critere de Succes |
|------|-------------|-------------------|
| `Test_CorruptedFile` | Fichier corrompu | Erreur geree, pas de crash |
| `Test_NetworkTimeout` | URL inaccessible | Timeout apres 30s, erreur |
| `Test_RapidCommands` | Spam Play/Pause/Seek | Pas de crash |
| `Test_LargePlaylist` | Playlist 10000 entrees | Reactif |
| `Test_LongPlayback` | Lecture continue 8h | Stable |
| `Test_RapidVisualizationToggle` | Activer/desactiver visu rapidement | Pas de crash |
| `Test_FileNotFound` | Fichier supprime pendant lecture | Erreur geree |
| `Test_LibMPVMissing` | libmpv non installe | Message d'erreur clair |

---

## Execution des Tests

### Avec Make

Les tests sont geres par un `Makefile` dedie dans le repertoire `tests/`, separe du `Makefile` principal de compilation.

```bash
# Depuis le repertoire tests/
cd tests/

# Compiler le programme de tests
make build

# Executer tous les tests
make test

# Tests rapides (unitaires uniquement)
make quick

# Tests par categorie
make unit               # Tests unitaires
make integration        # Tests d'integration
make functional         # Tests GUI
make performance        # Tests de performance
make robustness         # Tests de robustesse

# Tests specifiques
make test-mpv           # Tests MPVEngine
make test-visual        # Tests Visualisations
make test-playlist      # Tests Playlist
make test-config        # Tests Configuration
make test-radio         # Tests Radio/Streaming

# Mode verbose
make verbose

# Generer rapport HTML
make report

# Rapport de couverture
make coverage

# Nettoyer les artefacts de test
make clean
```

### Execution Directe

```bash
# Compiler le programme de tests
cd tests/
lazbuild TestRunner.lpi

# Executer tous les tests
./TestRunner --all --format=xml --output=results.xml

# Executer une categorie
./TestRunner --suite=Unit
./TestRunner --suite=Integration
./TestRunner --suite=Functional

# Executer un fichier de test specifique
./TestRunner --suite=TTestMPVEngine
./TestRunner --suite=TTestVisualEffects

# Executer un test specifique
./TestRunner --test=Test_PlayLocalFile

# Mode verbose
./TestRunner --all --verbose

# Generer rapport HTML
./TestRunner --all --format=html --output=results.html
```

---

## Generation des Donnees de Test

### Avec Make

```bash
# Depuis le repertoire tests/
cd tests/

# Generer les fichiers de test
make testdata

# Verifier les fichiers de test
make check-testdata
```

### Generation Manuelle

Executer le script `tests/generate_test_files.sh` (a creer) :

```bash
cd tests
chmod +x generate_test_files.sh
./generate_test_files.sh
```

### Fichiers de Test Requis

| Fichier | Description | Source |
|---------|-------------|--------|
| `test_44100_stereo.mp3` | MP3 44.1kHz stereo, 30s | Genere (ffmpeg) |
| `test_48000_51.mp3` | MP3 48kHz 5.1, 30s | Genere |
| `test_vbr.mp3` | MP3 VBR, 60s | Genere |
| `test_720p.mp4` | H.264 720p, 30s | Genere |
| `test_1080p_hevc.mkv` | HEVC 1080p, 30s | Genere |
| `test_with_subs.mkv` | MKV + 2 pistes sub | Genere |
| `test.srt` | Sous-titres SRT | Cree manuellement |
| `test.ass` | Sous-titres ASS | Cree manuellement |
| `test.vtt` | Sous-titres WebVTT | Cree manuellement |
| `test.m3u` | Playlist M3U | Cree manuellement |
| `test.m3u8` | Playlist M3U8 UTF-8 | Cree manuellement |
| `test.pls` | Playlist PLS | Cree manuellement |

---

## Objectifs de Couverture de Code

| Module | Couverture Cible |
|--------|------------------|
| uLibMPV.pas | 90% |
| uMPVEngine.pas | 85% |
| uPlaylistManager.pas | 90% |
| uRadioManager.pas | 80% |
| uVisualEffects.pas | 85% |
| uConfig.pas | 90% |
| uShortcutManager.pas | 85% |
| Forms/*.pas | 70% |
| **Global** | **80%** |

---

## Integration CI/CD

Les tests sont automatiquement executes via GitHub Actions sur :
- Push vers les branches `main` ou `develop`
- Pull requests vers `main`

Voir `.github/workflows/build.yml` pour la configuration complete du CI/CD.

---

## Notes

### Considerations

- Les tests GUI necessitent un environnement graphique (X11/Wayland)
- Les tests d'integration avec MPV necessitent libmpv installe
- Utiliser les mocks pour les tests unitaires isoles
- Les tests de streaming necessitent une connexion reseau

### Ordre d'Implementation Recommande

1. **Phase 1 - Tests critiques**
   - uTestLibMPV.pas (chargement bibliotheque)
   - uTestMPVEngine.pas (fonctions de base)
   - uTestPlaylist.pas (gestion playlist)

2. **Phase 2 - Tests fonctionnels**
   - uTestConfig.pas
   - uTestShortcuts.pas
   - uTestVisualEffects.pas

3. **Phase 3 - Tests d'integration**
   - uTestPlayback.pas
   - uTestVisualization.pas

4. **Phase 4 - Tests GUI**
   - uTestMainForm.pas
   - uTestPlaylistForm.pas
