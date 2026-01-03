# Documentation Développeur : Système de Visualisation Audio

## Vue d'Ensemble

Le système de visualisation audio de 3nity Media utilise les filtres `lavfi-complex` de MPV/FFmpeg pour générer des affichages visuels à partir du flux audio. Ce système est complexe car il nécessite une gestion précise des états MPV et des transitions entre fichiers.

## Architecture

### Composants Principaux

```
┌─────────────────────────────────────────────────────────────────┐
│                         uMainForm.pas                            │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐  │
│  │ FVisualEffects  │  │ FMPVEngine      │  │ Timers          │  │
│  │ (TVisualEffects)│  │ (TMPVEngine)    │  │                 │  │
│  │                 │  │                 │  │ FVisualLoadTimer│  │
│  │ - Mode          │  │ - lavfi-complex │  │ FVisualReapply  │  │
│  │ - ColorScheme   │  │ - Position      │  │   Timer         │  │
│  │ - FilterString  │  │ - Duration      │  │                 │  │
│  └────────┬────────┘  └────────┬────────┘  └────────┬────────┘  │
│           │                    │                    │           │
│           └────────────────────┼────────────────────┘           │
│                                │                                │
│  ┌─────────────────────────────┴─────────────────────────────┐  │
│  │                    Flags de Protection                     │  │
│  │  FChangingVisualization  FIgnoreNextEndFile  FWatchdog... │  │
│  └───────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│                      uVisualEffects.pas                          │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                    TVisualEffects                          │  │
│  │                                                            │  │
│  │  Modes:           Filtres FFmpeg:                         │  │
│  │  - vmNone         (aucun)                                 │  │
│  │  - vmSpectrum     showspectrum                            │  │
│  │  - vmWaves        showwaves                               │  │
│  │  - vmVector       avectorscope                            │  │
│  │  - vmHistogram    ahistogram                              │  │
│  │  - vmVolume       showvolume                              │  │
│  │  - vmCombined     showspectrum + showwaves (vstack)       │  │
│  │                                                            │  │
│  │  Schémas couleurs: Default, Fire, Ice, Rainbow, Green...  │  │
│  └───────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

### Fichiers Impliqués

| Fichier | Rôle |
|---------|------|
| `Core/uVisualEffects.pas` | Classe TVisualEffects - gestion modes/couleurs, génération filtres FFmpeg |
| `Forms/uMainForm.pas` | Intégration avec MPV, gestion états, timers, menus |
| `Common/uShortcuts.pas` | Raccourcis clavier (saVisNextMode, saVisNextColor) |

---

## Classe TVisualEffects (uVisualEffects.pas)

### Types et Énumérations

#### TVisualMode (Modes de Visualisation)

| Mode | Valeur | Description | Filtre FFmpeg |
|------|--------|-------------|---------------|
| vmNone | 0 | Aucune visualisation | - |
| vmSpectrum | 1 | Analyseur de spectre (barres de fréquence) | `showspectrum` |
| vmWaves | 2 | Forme d'onde | `showwaves` |
| vmVector | 3 | Oscilloscope vectoriel (Lissajous) | `avectorscope` |
| vmHistogram | 4 | Histogramme audio | `ahistogram` |
| vmVolume | 5 | VU-mètre | `showvolume` |
| vmCombined | 6 | Spectre + Onde combinés | `showspectrum` + `showwaves` + `vstack` |

#### TVisualColorScheme (Schémas de Couleurs)

| Schéma | Couleur FFmpeg |
|--------|----------------|
| vcsDefault | channel |
| vcsFire | fire |
| vcsIce | cool |
| vcsRainbow | rainbow |
| vcsGreen | green |
| vcsPurple | magenta |
| vcsWhite | white |

#### TSpectrumMode / TWaveformMode

```pascal
TSpectrumMode = (smVertical, smHorizontal, smScroll);
TWaveformMode = (wmPoint, wmLine, wmP2P, wmCline);
```

### TVisualSettings (Record de Configuration)

```pascal
TVisualSettings = record
  Mode: TVisualMode;
  SpectrumMode: TSpectrumMode;
  WaveformMode: TWaveformMode;
  ColorScheme: TVisualColorScheme;
  Width: Integer;           // Largeur visualisation (défaut: 640)
  Height: Integer;          // Hauteur visualisation (défaut: 360)
  BarCount: Integer;        // Nombre de barres (8-512, défaut: 64)
  BarWidth: Integer;        // Largeur barre en pixels
  BarGap: Integer;          // Espace entre barres
  ShowPeaks: Boolean;       // Indicateurs de crête
  PeakFalloff: Integer;     // Vitesse chute crêtes (1-10)
  WaveScale: Double;        // Échelle amplitude
  WaveSpeed: Integer;       // Vitesse défilement
  Opacity: Integer;         // Opacité (0-100)
  BackgroundColor: TColor;
  ForegroundColor: TColor;
  EnableGlow: Boolean;
  MirrorEffect: Boolean;
  Smoothing: Integer;       // Lissage (1-10)
end;
```

### Méthodes Principales

| Méthode | Description |
|---------|-------------|
| `Create` | Constructeur - appelle Initialize |
| `Initialize` | Initialise avec valeurs par défaut |
| `Reset` | Remet à zéro et met à jour le filtre |
| `GetFilterString` | Retourne le filtre FFmpeg actuel |
| `GetAudioOnlyFilter` | Retourne le filtre lavfi-complex complet pour audio seul |
| `NextMode` | Passe au mode suivant (cycle) |
| `PreviousMode` | Passe au mode précédent |
| `NextColorScheme` | Passe au schéma couleur suivant |
| `ApplyPreset(Name)` | Applique un preset prédéfini |
| `GetPresetNames` | Liste des noms de presets |

### Méthodes Privées de Construction des Filtres

| Méthode | Filtre FFmpeg généré |
|---------|---------------------|
| `BuildSpectrumFilter` | `showspectrum=s=WxH:mode=...:color=...:slide=...:scale=log` |
| `BuildWavesFilter` | `showwaves=s=WxH:mode=...:colors=...` |
| `BuildVectorFilter` | `avectorscope=s=WxH:mode=lissajous:draw=line:scale=log` |
| `BuildHistogramFilter` | `ahistogram=s=WxH:slide=replace:scale=log` |
| `BuildVolumeFilter` | `showvolume=w=W:h=H:f=0.5:b=4:m=p` |
| `BuildCombinedFilter` | Combinaison spectrum + waves avec vstack |

### Événement

```pascal
property OnFilterChanged: TNotifyEvent read FOnFilterChanged write FOnFilterChanged;
```

Déclenché quand le filtre change (mode, couleur, paramètres). Permet à uMainForm de réappliquer le filtre à MPV.

---

## Structure du Filtre lavfi-complex

### Format Général (Audio Seul)

```
[aid1] asplit [ao][a]; color=black:s=WxH [bg]; [a] <vis_filter> [fg]; [bg][fg] overlay=repeatlast=0 [vo]
```

**Explication:**
- `[aid1]` : Piste audio d'entrée (audio track 1)
- `asplit [ao][a]` : Divise l'audio en 2 flux (ao=sortie audio, a=pour visualisation)
- `color=black:s=WxH [bg]` : Génère un fond noir de taille WxH
- `[a] <vis_filter> [fg]` : Applique le filtre de visualisation
- `[bg][fg] overlay=repeatlast=0 [vo]` : Superpose visualisation sur fond
- `repeatlast=0` : **IMPORTANT** - Corrige le problème de blocage à la fin (MPV issue #7266)

### Exemple: Mode Spectrum

```
[aid1] asplit [ao][a]; color=black:s=640x360 [bg];
[a] showspectrum=s=640x360:mode=combined:color=channel:slide=replace:scale=log [fg];
[bg][fg] overlay=repeatlast=0 [vo]
```

### Mode Combined (Spectre + Onde)

```
[aid1] asplit=3 [ao][a1][a2];
color=black:s=640x360 [bg];
[a1] showspectrum=s=640x180:mode=combined:color=channel:slide=replace [v1];
[a2] showwaves=s=640x180:mode=line:colors=channel [v2];
[v1][v2] vstack [fg];
[bg][fg] overlay=shortest=1 [vo]
```

### Intégration de l'Égaliseur

Quand l'égaliseur est actif, il est inséré avant `asplit` dans ApplyVisualization:

```pascal
FilterStr := StringReplace(FilterStr, '[aid1] asplit',
  '[aid1] ' + EqFilter + ' [aeq]; [aeq] asplit', []);
```

Résultat:
```
[aid1] superequalizer=... [aeq]; [aeq] asplit [ao][a]; ...
```

---

## Intégration dans uMainForm.pas

### Champs Liés aux Visualisations

```pascal
private
  FVisualEffects: TVisualEffects;           // Instance TVisualEffects
  FChangingVisualization: Boolean;          // Flag ignorer EOF pendant changement
  FVisualReapplyTimer: TTimer;              // Timer réapplication après chargement
  FVisualReapplyNeeded: Boolean;            // Flag pour timer
  FVisualLoadTimer: TTimer;                 // Timer chargement après clear filtre
  FPendingPlayFile: string;                 // Fichier en attente de lecture
  FWatchdogTriggered: Boolean;              // Empêche déclenchements multiples watchdog
  FRestoreVisAfterLoad: Boolean;            // Restaurer vis après chargement
  FRestoreVisMode: TVisualMode;             // Mode à restaurer
```

### Flags de Protection

#### FChangingVisualization

- **But:** Ignorer les événements EOF/STOP générés par le changement de filtre
- **Mis à True:** Avant d'appliquer/changer un filtre
- **Remis à False:** Par le timer après stabilisation de MPV (300-500ms)

#### FIgnoreNextEndFile

- **But:** Ignorer l'événement end-file qui arrive immédiatement après PlayFile
- **Mis à True:** Dans PlayFile avant de charger un nouveau fichier
- **Remis à False:** Dans OnMPVFileLoaded quand le fichier est chargé

#### FWatchdogTriggered

- **But:** Éviter les déclenchements multiples du watchdog
- **Mis à True:** Quand le watchdog détecte la fin du fichier
- **Remis à False:** Dans OnMPVFileLoaded pour le nouveau fichier

#### FRestoreVisAfterLoad / FRestoreVisMode

- **But:** Sauvegarder et restaurer la visualisation lors des transitions
- **Utilisé par:** AsyncPlayNextInPlaylist pour sauvegarder/restaurer le mode

### Timers

#### FVisualLoadTimer (500ms)

```pascal
FVisualLoadTimer := TTimer.Create(Self);
FVisualLoadTimer.Interval := 500;
FVisualLoadTimer.Enabled := False;
FVisualLoadTimer.OnTimer := @OnVisualLoadTimer;
```

- **Rôle:** Délai avant de charger un nouveau fichier après effacement du filtre
- **Déclencheur:** PlayFile quand visualisation active
- **Handler:** `OnVisualLoadTimer` → `PlayMedia(FPendingPlayFile)`

#### FVisualReapplyTimer (300-800ms)

```pascal
FVisualReapplyTimer := TTimer.Create(Self);
FVisualReapplyTimer.Interval := 800;
FVisualReapplyTimer.Enabled := False;
FVisualReapplyTimer.OnTimer := @OnVisualReapplyTimer;
```

- **Rôle:** Délai avant de réappliquer la visualisation après chargement
- **Déclencheur:** OnMPVFileLoaded
- **Handler:** `OnVisualReapplyTimer` → `ApplyVisualization` ou reset flags

### Procédures Principales

#### SetVisualizationMode

```pascal
procedure TfrmMain.SetVisualizationMode(Mode: TVisualMode);
begin
  if FVisualEffects = nil then Exit;

  if Mode = vmNone then
    FVisualEffects.Enabled := False
  else
  begin
    FVisualEffects.Mode := Mode;
    FVisualEffects.Enabled := True;
  end;

  FChangingVisualization := True;
  ApplyVisualization;

  // Timer pour reset le flag après stabilisation
  FVisualReapplyTimer.Enabled := False;
  FVisualReapplyTimer.Interval := 500;
  FVisualReapplyTimer.Enabled := True;
end;
```

#### ApplyVisualization

```pascal
procedure TfrmMain.ApplyVisualization;
var
  FilterStr, EqFilter: string;
begin
  if FMPVEngine = nil then Exit;
  if FVisualEffects = nil then Exit;

  FChangingVisualization := True;

  FilterStr := FVisualEffects.GetAudioOnlyFilter;

  if FilterStr <> '' then
  begin
    // Intégrer égaliseur si actif
    EqFilter := FMPVEngine.GetEqualizerFilterString;
    if EqFilter <> '' then
    begin
      FilterStr := StringReplace(FilterStr, '[aid1] asplit',
        '[aid1] ' + EqFilter + ' [aeq]; [aeq] asplit', []);
    end;
    FMPVEngine.SetPropertyString('lavfi-complex', FilterStr);
  end
  else
  begin
    // Désactiver - sauvegarder position, clear filtre, recharger
    FMPVEngine.SetPropertyString('lavfi-complex', '');
  end;
end;
```

#### OnVisFilterChanged (Callback)

```pascal
procedure TfrmMain.OnVisFilterChanged(Sender: TObject);
begin
  FChangingVisualization := True;
  ApplyVisualization;
  FVisualReapplyTimer.Enabled := False;
  FVisualReapplyTimer.Interval := 500;
  FVisualReapplyTimer.Enabled := True;
end;
```

---

## Flux d'Exécution

### Changement de Mode Visualisation (Menu/Raccourci)

```
1. Utilisateur clique menu/raccourci
   └── mnuVisSpectrumClick / mnuVisNextModeClick
       └── SetVisualizationMode(vmSpectrum)
           ├── FChangingVisualization := True
           ├── FVisualEffects.Mode := vmSpectrum
           ├── FVisualEffects.Enabled := True
           ├── ApplyVisualization()
           │   └── FMPVEngine.SetPropertyString('lavfi-complex', FilterStr)
           └── FVisualReapplyTimer.Start (500ms)
               └── FChangingVisualization := False
```

### Transition vers Fichier Suivant (Watchdog)

```
1. Position atteint fin de fichier (Duration - 0.3s)
   └── OnMPVPosition (watchdog dans timer position)
       ├── FWatchdogTriggered := True
       └── QueueAsyncCall(AsyncPlayNextInPlaylist)

2. AsyncPlayNextInPlaylist
   ├── Sauvegarde: FRestoreVisMode := FVisualEffects.Mode
   ├── FRestoreVisAfterLoad := True
   ├── FChangingVisualization := True
   ├── FMPVEngine.Stop
   ├── SetPropertyString('lavfi-complex', '')
   ├── FVisualEffects.Enabled := False
   └── PlayNextInPlaylist()

3. PlayFile (fichier suivant)
   ├── FIgnoreNextEndFile := True
   ├── FMPVEngine.Stop
   ├── SetPropertyString('lavfi-complex', '')
   ├── FPendingPlayFile := FileName
   └── FVisualLoadTimer.Start (500ms)

4. OnVisualLoadTimer (500ms plus tard)
   └── FMPVEngine.PlayMedia(FPendingPlayFile)

5. MPV envoie end-file (ancien fichier)
   └── OnMPVEndFile → Ignoré (FIgnoreNextEndFile=True)

6. OnMPVFileLoaded (nouveau fichier)
   ├── FIgnoreNextEndFile := False
   ├── FWatchdogTriggered := False
   ├── if FRestoreVisAfterLoad:
   │   ├── FRestoreVisAfterLoad := False
   │   ├── FVisualEffects.Mode := FRestoreVisMode
   │   ├── FVisualEffects.Enabled := True
   │   ├── FIgnoreNextEndFile := True
   │   └── ApplyVisualization()
   └── FVisualReapplyTimer.Start (300ms)

7. OnVisualReapplyTimer (300ms plus tard)
   └── FChangingVisualization := False
```

### Lecture Normale avec Visualisation Active

```
1. Fichier se termine naturellement
   └── OnMPVEndFile(Reason=EOF)
       ├── Vérification: pas FIgnoreNextEndFile
       ├── Vérification: pas FChangingVisualization
       ├── Vérification: pas FWatchdogTriggered
       ├── Vérification NearEnd (position > 98% ou < 2s de la fin)
       └── si tout OK → PlayNextInPlaylist()
```

---

## Watchdog de Fin de Fichier

Le watchdog est nécessaire car MPV peut ne pas envoyer d'événement EOF avec lavfi-complex actif.

```pascal
// Dans OnMPVPosition (callback position)
if (FVisualEffects <> nil) and FVisualEffects.Enabled and
   (FVisualEffects.Mode <> vmNone) and (FMPVEngine <> nil) and
   not FWatchdogTriggered and not FChangingVisualization then
begin
  Duration := FMPVEngine.Duration;
  // Déclencher quand position atteint 0.3s avant la fin
  if (Duration > 0) and (PositionSec >= Duration - 0.3) then
  begin
    FWatchdogTriggered := True;
    Application.QueueAsyncCall(@AsyncPlayNextInPlaylist, 0);
  end;
end;
```

---

## Menus et Raccourcis

### Menus (mnuVisualization)

| Menu Item | Variable | Action |
|-----------|----------|--------|
| None | mnuVisNone | SetVisualizationMode(vmNone) |
| Spectrum Analyzer | mnuVisSpectrum | SetVisualizationMode(vmSpectrum) |
| Waveform | mnuVisWaveform | SetVisualizationMode(vmWaves) |
| Vector Scope | mnuVisVector | SetVisualizationMode(vmVector) |
| VU Meter | mnuVisVolume | SetVisualizationMode(vmVolume) |
| Next Mode | mnuVisNextMode | FVisualEffects.NextMode |
| Next Color | mnuVisNextColor | FVisualEffects.NextColorScheme |

### Raccourcis Clavier (uShortcuts.pas)

| Action | Constante | Raccourci par défaut |
|--------|-----------|---------------------|
| Mode suivant | saVisNextMode | V |
| Couleur suivante | saVisNextColor | Shift+V |

### Mise à Jour des Menus

```pascal
procedure TfrmMain.UpdateVisualizationMenu;
begin
  if FVisualEffects = nil then Exit;

  mnuVisNone.Checked := not FVisualEffects.Enabled or (FVisualEffects.Mode = vmNone);
  mnuVisSpectrum.Checked := FVisualEffects.Enabled and (FVisualEffects.Mode = vmSpectrum);
  mnuVisWaveform.Checked := FVisualEffects.Enabled and (FVisualEffects.Mode = vmWaves);
  mnuVisVector.Checked := FVisualEffects.Enabled and (FVisualEffects.Mode = vmVector);
  mnuVisVolume.Checked := FVisualEffects.Enabled and (FVisualEffects.Mode = vmVolume);
end;
```

---

## Presets Disponibles

| Nom | Mode | Couleur | Paramètres |
|-----|------|---------|------------|
| Classic Spectrum | vmSpectrum | vcsGreen | 64 barres, vertical |
| Fire Spectrum | vmSpectrum | vcsFire | 128 barres |
| Spectrogram | vmSpectrum | vcsRainbow | 256 barres, scroll |
| Oscilloscope | vmWaves | vcsGreen | ligne |
| Stereo Scope | vmVector | vcsGreen | - |
| VU Meter | vmVolume | vcsDefault | - |
| Rainbow Waves | vmWaves | vcsRainbow | P2P, miroir |
| Minimal | vmWaves | vcsWhite | Cline |

---

## Problèmes Connus et Solutions

### Problème 1: Faux EOF avec lavfi-complex

**Symptôme:** MPV envoie des événements EOF quand le filtre est changé

**Solution:**
- Flag `FChangingVisualization` pour ignorer ces événements
- Vérification `NearEnd` pour détecter les vrais EOF

### Problème 2: Blocage à la Fin du Fichier

**Symptôme:** MPV se bloque quand le fichier audio se termine avec visualisation

**Solution:**
- Paramètre `repeatlast=0` sur le filtre `overlay`
- Watchdog qui détecte la position proche de la fin

### Problème 3: Fichier Suivant ne se Charge Pas

**Symptôme:** Après EOF avec visualisation, le fichier suivant ne démarre pas

**Cause:** La référence `[aid1]` pointe vers l'ancien fichier

**Solution:**
- Effacer le filtre AVANT de charger le nouveau fichier
- Utiliser des timers pour laisser MPV se stabiliser
- Réappliquer le filtre APRÈS le chargement du nouveau fichier

### Problème 4: showwaves Cause des Erreurs

**Symptôme:** Le filtre `showwaves` peut laisser MPV dans un état irrécupérable

**Solution:** Le paramètre `repeatlast=0` sur overlay résout ce problème

---

## Codes d'Événements MPV

| Constante | Valeur | Description |
|-----------|--------|-------------|
| MPV_END_FILE_REASON_EOF | 0 | Fin normale du fichier |
| MPV_END_FILE_REASON_STOP | 2 | Arrêt (utilisateur ou filtre) |
| MPV_END_FILE_REASON_ERROR | 4 | Erreur de lecture |

---

## Références

- [MPV Issue #9588](https://github.com/mpv-player/mpv/issues/9588) - Clearing lavfi-complex causes audio disappear
- [MPV Issue #6354](https://github.com/mpv-player/mpv/issues/6354) - Can't switch audio tracks with lavfi-complex
- [MPV Issue #7266](https://github.com/mpv-player/mpv/issues/7266) - EOF hanging with lavfi filters
- [FFmpeg Filters Documentation](https://ffmpeg.org/ffmpeg-filters.html)
- [FFmpeg showspectrum](https://ffmpeg.org/ffmpeg-filters.html#showspectrum-1)
- [FFmpeg showwaves](https://ffmpeg.org/ffmpeg-filters.html#showwaves-1)
- [FFmpeg avectorscope](https://ffmpeg.org/ffmpeg-filters.html#avectorscope)
