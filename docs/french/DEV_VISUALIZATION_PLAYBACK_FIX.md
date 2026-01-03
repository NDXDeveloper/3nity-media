# Documentation Développeur : Correction Visualisation + Lecture Playlist

## Problème Initial

Lors de la lecture d'un fichier audio depuis l'historique avec la visualisation activée (lavfi-complex), quand le fichier se terminait, le fichier suivant ne se lançait pas et le lecteur devenait non réactif.

## Cause Racine

Le filtre `lavfi-complex` de MPV utilise des références de piste audio comme `[aid1]` (piste audio 1). Quand un nouveau fichier est chargé alors que le filtre est actif, la référence `[aid1]` devient invalide car elle pointe vers l'ancien fichier. MPV ne peut alors plus lire le nouveau fichier.

### Comportement de MPV avec lavfi-complex

- Le filtre `lavfi-complex` "verrouille" la piste audio référencée
- Lors du changement de fichier, MPV envoie un événement `end-file` avec `reason=2` (STOP)
- Si le filtre reste actif, le nouveau fichier ne peut pas s'initialiser correctement
- Ceci est documenté dans les issues GitHub MPV #9588 et #6354

### Problème Spécifique avec showwaves

Le filtre FFmpeg `showwaves` causait des problèmes supplémentaires :
- Même après l'arrêt et l'effacement du filtre, MPV entrait dans un état irrécupérable
- ErrorCode=-17 (MPV_ERROR_LOADING_FAILED) lors du chargement du fichier suivant
- L'application devenait complètement non réactive

**Solution** : Utiliser le paramètre `repeatlast=0` sur le filtre overlay. Cela corrige le problème de gestion EOF.

## Solution Implémentée

### 1. Utiliser des Timers au lieu de Sleep()

Les appels `Sleep()` bloquent l'interface utilisateur. Nous utilisons `TTimer` pour des délais non bloquants :

- `FVisualLoadTimer` (500ms) : Délai avant le chargement d'un nouveau fichier après effacement du filtre
- `FVisualReapplyTimer` (800ms) : Délai avant la réapplication de la visualisation après chargement

### 2. Effacer le Filtre AVANT de Charger le Nouveau Fichier

Dans `PlayFile` (uMainForm.pas), nous arrêtons MPV et effaçons le filtre avant de charger le nouveau fichier :

```pascal
{ Effacer le filtre lavfi-complex AVANT de charger le nouveau fichier }
if (FVisualEffects <> nil) and FVisualEffects.Enabled and (FVisualEffects.Mode <> vmNone) then
begin
  { Arrêter la lecture d'abord pour réinitialiser l'état MPV }
  FMPVEngine.Stop;
  Application.ProcessMessages;
  { Effacer le filtre }
  FMPVEngine.SetPropertyString('lavfi-complex', '');
  Application.ProcessMessages;
  { Utiliser un timer pour retarder le chargement du fichier }
  FPendingPlayFile := FileName;
  FIgnoreNextEndFile := True;
  FVisualLoadTimer.Enabled := True;
  Exit;
end;
```

### 3. Réappliquer le Filtre Après Chargement

Dans `OnMPVFileLoaded`, nous démarrons le timer de réapplication :

```pascal
if (FVisualEffects <> nil) and FVisualEffects.Enabled and
   (FVisualEffects.Mode <> vmNone) and not FChangingVisualization then
begin
  FVisualReapplyTimer.Enabled := False; { Réinitialiser si déjà en cours }
  FVisualReapplyTimer.Enabled := True;  { Démarrer le timer }
end;
```

### 4. Drapeaux de Protection

Deux drapeaux sont utilisés pour éviter les problèmes en cascade :

#### FIgnoreNextEndFile
- Mis à `True` dans `PlayFile` avant d'effacer le filtre
- Remis à `False` dans `OnMPVFileLoaded` quand le fichier est chargé
- Permet d'ignorer l'événement `end-file` qui arrive immédiatement après le changement de fichier

#### FChangingVisualization
- Mis à `True` au début de `ApplyVisualization`
- Remis à `False` après un délai
- Permet d'ignorer les événements EOF causés par le changement de filtre

### 5. Détection NearEnd pour les Faux EOF

Le filtre peut générer de faux événements EOF. Nous vérifions si nous sommes vraiment près de la fin :

```pascal
NearEnd := (FileDuration <= 0) or (CurrentPos <= 0) or
           ((FileDuration - CurrentPos) < 2.0) or
           ((CurrentPos / FileDuration) > 0.98);

if (Reason = MPV_END_FILE_REASON_EOF) and not NearEnd then
  Exit;  // Faux EOF, ignorer
```

### 6. Utiliser repeatlast=0 sur overlay

Le filtre overlay nécessite `repeatlast=0` pour gérer correctement EOF :

```pascal
function TVisualEffects.GetAudioOnlyFilter: string;
begin
  // ...
  Result := Format('[aid1] asplit [ao][a]; color=black:s=%dx%d [bg]; [a] %s [fg]; [bg][fg] overlay=repeatlast=0 [vo]',
    [FSettings.Width, FSettings.Height, BuildSpectrumFilter]);
end;
```

## Flux d'Exécution

```
1. Fichier terminé (EOF)
   └── OnMPVEndFile(Reason=0)
       └── PlayNextInPlaylist()
           └── PlayFile(NextFile)
               ├── Arrêter MPV
               ├── Effacer lavfi-complex
               ├── FIgnoreNextEndFile := True
               └── Démarrer FVisualLoadTimer

2. Timer FVisualLoadTimer (500ms)
   └── OnVisualLoadTimer()
       └── PlayMedia(NextFile)

3. MPV envoie end-file pour l'ancien fichier
   └── OnMPVEndFile(Reason=2) - IGNORÉ (FIgnoreNextEndFile=True)

4. Nouveau fichier chargé
   └── OnMPVFileLoaded()
       ├── FIgnoreNextEndFile := False
       └── Démarrer FVisualReapplyTimer

5. Timer FVisualReapplyTimer (800ms)
   └── OnVisualReapplyTimer()
       └── ApplyVisualization()
           ├── FChangingVisualization := True
           ├── SetPropertyString('lavfi-complex', FilterStr)
           └── FChangingVisualization := False (après délai)
```

## Codes d'Événement MPV

| Constante | Valeur | Description |
|-----------|--------|-------------|
| MPV_END_FILE_REASON_EOF | 0 | Fin normale du fichier |
| MPV_END_FILE_REASON_STOP | 2 | Arrêt (utilisateur ou filtre) |
| MPV_END_FILE_REASON_ERROR | 4 | Erreur de lecture |

## TMPVStatus

| État | Valeur | Description |
|------|--------|-------------|
| msNone | 0 | Aucun état |
| msOpening | 1 | Ouverture en cours |
| msClosing | 2 | Fermeture en cours |
| msPlayStarting | 3 | Démarrage lecture |
| msPlaying | 4 | En lecture |
| msPaused | 5 | En pause |
| msStopped | 6 | Arrêté |
| msError | 7 | Erreur |

## Points Importants

1. **Timers non bloquants** : Utiliser `TTimer` au lieu de `Sleep()` pour éviter de bloquer l'interface utilisateur.

2. **Ordre des opérations** : Le filtre DOIT être effacé AVANT de charger le nouveau fichier, pas après.

3. **Stop avant Clear** : Appeler `FMPVEngine.Stop` avant d'effacer le filtre pour réinitialiser l'état MPV.

4. **repeatlast=0** : Le filtre `overlay` nécessite `repeatlast=0` pour gérer correctement EOF.

5. **Gestion des erreurs** : Si un fichier est invalide (ErrorCode=-17), le système passe automatiquement au fichier suivant.

## Fichiers Modifiés

- `src/Forms/uMainForm.pas` :
  - `PlayFile` : Ajout Stop + effacement filtre + timer
  - `OnMPVEndFile` : Gestion des drapeaux et détection NearEnd
  - `OnMPVFileLoaded` : Démarrage timer réapplication
  - `OnVisualLoadTimer` : Chargement retardé du fichier
  - `OnVisualReapplyTimer` : Réapplication retardée du filtre
  - `ApplyVisualization` : Drapeau FChangingVisualization

- `src/Core/uVisualEffects.pas` :
  - `GetAudioOnlyFilter` : Ajout repeatlast=0 au filtre overlay

## Références

- [MPV Issue #9588](https://github.com/mpv-player/mpv/issues/9588) - Clearing lavfi-complex causes audio disappear
- [MPV Issue #6354](https://github.com/mpv-player/mpv/issues/6354) - Can't switch audio tracks with lavfi-complex
- [MPV Issue #7266](https://github.com/mpv-player/mpv/issues/7266) - EOF hanging with lavfi filters
