# Paramètres en Ligne de Commande (CLI)

Ce document décrit tous les paramètres en ligne de commande supportés par 3nity Media.

## Synopsis

```
3nity [OPTIONS] [FICHIER|URL|DOSSIER]
```

## Référence Rapide

```
OPTIONS:
  -h, --help          Affiche l'aide
  -v, --version       Affiche la version
      --license       Affiche la licence GPL-2.0

  -f, --fullscreen    Démarre en plein écran
      --volume=N      Volume initial (0-100)
      --mute          Démarre en sourdine
      --loop          Boucle la lecture
      --speed=N       Vitesse de lecture (0.25-4.0)

      --start=TIME    Position de départ (ex: 1:30:00)
      --sub=FILE      Charge un fichier de sous-titres
      --enqueue       Ajoute à l'instance existante
```

## Paramètres par Catégorie

### 1. Information

| Paramètre | Court | Description |
|-----------|-------|-------------|
| `--help` | `-h` | Affiche l'aide avec tous les paramètres disponibles |
| `--version` | `-v` | Affiche la version et les informations de build |
| `--license` | | Affiche le texte de la licence GPL-2.0 |

**Exemples :**
```bash
3nity --help
3nity -h
3nity --version
3nity -v
3nity --license
```

### 2. Contrôle de Lecture

| Paramètre | Description | Défaut |
|-----------|-------------|--------|
| `--fullscreen`, `-f` | Démarre la lecture en plein écran | Fenêtré |
| `--volume=N` | Définit le volume initial (0-100) | Dernier utilisé |
| `--mute` | Démarre avec le son coupé | Son actif |
| `--loop` | Boucle le fichier média en continu | Pas de boucle |
| `--speed=N` | Définit la vitesse de lecture (0.25 à 4.0) | 1.0 |

**Exemples :**
```bash
# Démarrer une vidéo en plein écran
3nity -f film.mkv
3nity --fullscreen film.mkv

# Régler le volume à 50%
3nity --volume=50 musique.mp3

# Démarrer en sourdine (utile pour visionnage tardif)
3nity --mute video.mp4

# Boucler une courte vidéo
3nity --loop intro.mp4

# Lire à vitesse 1.5x
3nity --speed=1.5 podcast.mp3

# Combiner plusieurs options
3nity -f --volume=30 --speed=1.25 cours.mp4
```

### 3. Position

| Paramètre | Description | Format |
|-----------|-------------|--------|
| `--start=TIME` | Démarre la lecture à la position spécifiée | `SS`, `MM:SS`, ou `HH:MM:SS` |

**Exemples de format de temps :**
- `--start=90` - Démarre à 90 secondes
- `--start=1:30` - Démarre à 1 minute 30 secondes
- `--start=01:30:00` - Démarre à 1 heure 30 minutes

**Exemples :**
```bash
# Sauter l'intro (démarrer à 2 minutes)
3nity --start=2:00 film.mkv

# Reprendre à un timestamp précis
3nity --start=01:23:45 film.mkv

# Démarrer à 30 secondes
3nity --start=30 clip.mp4
```

### 4. Sous-titres

| Paramètre | Description |
|-----------|-------------|
| `--sub=FILE` | Charge un fichier de sous-titres externe |

**Formats supportés :** SRT, ASS, SSA, SUB, VTT, SUP

**Exemples :**
```bash
# Charger un fichier de sous-titres
3nity film.mkv --sub=film.srt

# Charger des sous-titres avec un nom différent
3nity film.mkv --sub=soustitres/francais.srt

# Chemin complet
3nity film.mkv --sub=/home/user/subs/film.fr.srt
```

### 5. Contrôle d'Instance

| Paramètre | Description |
|-----------|-------------|
| `--enqueue` | Ajoute les fichiers à la playlist d'une instance déjà en cours |

Quand `--enqueue` est utilisé :
- Si 3nity Media est déjà en cours : les fichiers sont ajoutés à sa playlist
- Si aucune instance n'est en cours : une nouvelle instance démarre avec les fichiers

**Exemples :**
```bash
# Ajouter un fichier à l'instance en cours
3nity --enqueue nouveau_fichier.mp3

# Ajouter tous les MP3 d'un dossier
3nity --enqueue ~/Musique/*.mp3
```

## Exemples Complets

### Utilisation Basique

```bash
# Lire un fichier vidéo
3nity video.mp4

# Lire un fichier audio
3nity musique.mp3

# Lire depuis une URL
3nity https://exemple.com/stream.m3u8

# Ouvrir un dossier (ajoute tous les fichiers média à la playlist)
3nity ~/Vidéos/
```

### Soirée Film

```bash
# Plein écran avec volume réduit, démarrant après l'intro
3nity -f --volume=40 --start=2:30 film.mkv
```

### Lecture Musicale

```bash
# Boucler un album à 50% du volume
3nity --volume=50 --loop album.m3u

# Ajouter d'autres pistes à la session en cours
3nity --enqueue autres_chansons/*.mp3
```

### Apprentissage des Langues

```bash
# Lire une vidéo avec sous-titres externes à vitesse réduite
3nity --speed=0.75 --sub=lecon.srt lecon.mp4
```

### Podcast/Livre Audio

```bash
# Reprendre un podcast à vitesse 1.25x
3nity --speed=1.25 --start=45:30 podcast.mp3
```

### Musique de Fond

```bash
# Démarrer en sourdine, activer le son quand prêt
3nity --mute --loop playlist.m3u
```

## Types d'Entrée

3nity Media accepte différents types d'entrée :

| Type | Exemple |
|------|---------|
| Fichier local | `3nity video.mp4` |
| Dossier | `3nity ~/Vidéos/` |
| Fichier playlist | `3nity playlist.m3u` |
| URL/Stream | `3nity https://stream.exemple.com/live.m3u8` |
| Motif glob | `3nity ~/Musique/*.mp3` |

## Codes de Sortie

| Code | Signification |
|------|---------------|
| 0 | Succès |
| 1 | Erreur (paramètre invalide, fichier non trouvé, etc.) |

## Notes

- Les paramètres peuvent apparaître avant ou après le chemin du fichier
- Les paramètres longs utilisent `=` pour les valeurs : `--volume=50`
- Paramètres courts : `-f` (sans valeur) ou `-h`
- Plusieurs paramètres peuvent être combinés
- Les chemins de fichiers avec espaces doivent être entre guillemets : `"ma vidéo.mp4"`

## Variables d'Environnement

| Variable | Description |
|----------|-------------|
| `XDG_CONFIG_HOME` | Répertoire de config (Linux, défaut : `~/.config`) |
| `APPDATA` | Répertoire de config (Windows) |

## Voir Aussi

- [Guide Utilisateur](USER_GUIDE.md)
- [Raccourcis Clavier](SHORTCUTS.md)
- [Guide de Configuration](CONFIG.md)

## Informations de Version

- **Dernière mise à jour :** 2026-01-01
- **S'applique à :** 3nity Media v0.x et versions ultérieures
