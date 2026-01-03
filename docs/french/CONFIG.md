# Guide de Configuration - 3nity Media

Ce guide couvre toutes les options de configuration de 3nity Media, y compris les paramètres accessibles via l'interface graphique et l'édition manuelle des fichiers INI.

## Table des Matières

- [Fichiers de Configuration](#fichiers-de-configuration)
- [Paramètres Généraux](#param%C3%A8tres-g%C3%A9n%C3%A9raux)
- [Paramètres Vidéo](#param%C3%A8tres-vid%C3%A9o)
- [Paramètres Audio](#param%C3%A8tres-audio)
- [Paramètres des Sous-titres](#param%C3%A8tres-des-sous-titres)
- [Paramètres de Cache](#param%C3%A8tres-de-cache)
- [Paramètres de l'Égaliseur](#param%C3%A8tres-de-l%C3%A9galiseur)
- [Mode de Lecture](#mode-de-lecture)
- [Historique et Fichiers Récents](#historique-et-fichiers-r%C3%A9cents)
- [Signets](#signets)
- [Favoris](#favoris)
- [Restauration de Session](#restauration-de-session)
- [Structure du Fichier INI](#structure-du-fichier-ini)
- [Configuration Avancée](#configuration-avanc%C3%A9e)

---

## Fichiers de Configuration

### Emplacements des Fichiers

| Plateforme | Répertoire de Configuration |
|------------|----------------------------|
| Linux | `~/.config/3nity-media/` |
| Windows | `%APPDATA%\3nity-media\` |
| Flatpak | `~/.var/app/com.ndxdev.3nity-media/config/` |
| Snap | `~/snap/3nity-media/current/.config/` |

### Fichiers de Configuration

| Fichier | Fonction |
|---------|----------|
| `config.ini` | Configuration principale (toutes les sections) |
| `history.ini` | Historique de lecture |
| `bookmarks.ini` | Signets des médias |
| `favorites.ini` | Liste des favoris |
| `shortcuts.ini` | Raccourcis clavier personnalisés |
| `session_playlist.ini` | Données de restauration de session |

---

## Paramètres Généraux

Accessible via **Options → Général** ou dans la section `[General]`.

| Paramètre | Type | Défaut | Description |
|-----------|------|--------|-------------|
| `Language` | chaîne | `en` | Code de langue de l'interface (en, fr, etc.) |
| `SingleInstance` | booléen | `true` | N'autoriser qu'une seule instance de l'application |
| `ScreenshotPath` | chaîne | `~/Pictures/3nity/` | Répertoire pour les captures d'écran |
| `ScreenshotFormat` | chaîne | `png` | Format de capture : `png`, `jpg` ou `webp` |
| `HistoryEnabled` | booléen | `true` | Activer le suivi de l'historique de lecture |
| `HistoryMaxItems` | entier | `500` | Nombre maximum d'entrées d'historique |
| `AutoSavePlaylist` | booléen | `true` | Sauvegarder la playlist à la fermeture pour restauration |

### Codes de Langue

| Code | Langue |
|------|--------|
| `en` | Anglais |
| `fr` | Français |

---

## Paramètres Vidéo

Accessible via **Options → Vidéo** ou dans la section `[Video]`.

### Propriétés Vidéo

| Paramètre | Type | Plage | Défaut | Description |
|-----------|------|-------|--------|-------------|
| `Brightness` | entier | -100 à 100 | `0` | Ajustement de la luminosité |
| `Contrast` | entier | -100 à 100 | `0` | Ajustement du contraste |
| `Saturation` | entier | -100 à 100 | `0` | Ajustement de la saturation des couleurs |
| `Hue` | entier | -100 à 100 | `0` | Rotation de la teinte |
| `Gamma` | entier | -100 à 100 | `0` | Correction gamma |

### Rapport d'Aspect

| Paramètre | Type | Défaut | Description |
|-----------|------|--------|-------------|
| `AspectMode` | entier | `0` | Préréglage du rapport d'aspect (voir tableau) |
| `AspectFactor` | double | `-1` | Valeur personnalisée du rapport d'aspect |

**Valeurs du Mode d'Aspect :**

| Valeur | Rapport d'Aspect |
|--------|------------------|
| 0 | Auto (original) |
| 1 | 16:9 |
| 2 | 4:3 |
| 3 | 2.35:1 (Cinémascope) |
| 4 | 1.85:1 |
| 5 | Personnalisé (utilise AspectFactor) |

### Désentrelacement

| Paramètre | Type | Défaut | Description |
|-----------|------|--------|-------------|
| `Deinterlace` | entier | `2` | Mode de désentrelacement |
| `DeinterlaceAlg` | entier | `0` | Algorithme de désentrelacement |

**Valeurs du Mode de Désentrelacement :**

| Valeur | Mode |
|--------|------|
| 0 | Désactivé |
| 1 | Activé (toujours) |
| 2 | Auto (détection du contenu entrelacé) |

**Valeurs de l'Algorithme de Désentrelacement :**

| Valeur | Algorithme |
|--------|------------|
| 0 | Auto |
| 1 | Yadif |
| 2 | Bwdif |
| 3 | Weave |

### Sortie Vidéo

| Paramètre | Type | Défaut | Description |
|-----------|------|--------|-------------|
| `VideoOutput` | chaîne | `auto` | Pilote de sortie vidéo |
| `HWAccel` | booléen | `true` | Activer l'accélération matérielle |

**Valeurs de Sortie Vidéo :**

| Plateforme | Options |
|------------|---------|
| Linux | `auto`, `gpu`, `gpu-next`, `x11`, `wayland`, `xv` |
| Windows | `auto`, `gpu`, `gpu-next`, `d3d11`, `opengl` |

---

## Paramètres Audio

Accessible via **Options → Audio** ou dans la section `[Audio]`.

| Paramètre | Type | Plage | Défaut | Description |
|-----------|------|-------|--------|-------------|
| `Volume` | entier | 0-150 | `100` | Niveau de volume (>100 utilise l'amplification logicielle) |
| `Muted` | booléen | - | `false` | État muet |
| `AudioOutput` | chaîne | - | `auto` | Pilote de sortie audio |
| `AudioDevice` | chaîne | - | `` | Périphérique audio spécifique (vide = défaut) |
| `Channels` | entier | 1-8 | `2` | Nombre de canaux audio |
| `Normalize` | booléen | - | `false` | Activer la normalisation audio |

**Valeurs de Sortie Audio :**

| Plateforme | Options |
|------------|---------|
| Linux | `auto`, `pulse`, `pipewire`, `alsa` |
| Windows | `auto`, `wasapi`, `dsound` |

---

## Paramètres des Sous-titres

Accessible via **Options → Sous-titres** ou dans la section `[Subtitles]`.

| Paramètre | Type | Défaut | Description |
|-----------|------|--------|-------------|
| `UseDefault` | booléen | `true` | Utiliser le style par défaut de mpv |
| `FontName` | chaîne | `Arial` | Famille de police |
| `FontSize` | entier | `24` | Taille de police en points |
| `FontColor` | entier | `16777215` | Couleur de police (valeur TColor, blanc) |
| `FontBold` | booléen | `false` | Texte en gras |
| `FontItalic` | booléen | `false` | Texte en italique |
| `OutlineColor` | entier | `0` | Couleur du contour (valeur TColor, noir) |
| `OutlineSize` | entier | `2` | Épaisseur du contour en pixels |
| `BackgroundColor` | entier | `0` | Couleur du fond |
| `BackgroundOpacity` | entier | `0` | Opacité du fond (0-255) |
| `Position` | entier | `95` | Position verticale (0-100, bas vers haut) |
| `Encoding` | chaîne | `UTF-8` | Encodage des caractères |
| `AutoLoad` | booléen | `true` | Charger automatiquement les fichiers de sous-titres correspondants |

### Valeurs de Couleur

Les couleurs sont stockées en tant qu'entiers TColor. Valeurs courantes :

| Couleur | Valeur |
|---------|--------|
| Blanc | `16777215` ($FFFFFF) |
| Noir | `0` ($000000) |
| Jaune | `65535` ($00FFFF) |
| Rouge | `255` ($0000FF) |
| Vert | `32768` ($008000) |

---

## Paramètres de Cache

Accessible via **Options → Cache** ou dans la section `[Cache]`.

Les tailles de cache sont en **kilo-octets**.

| Paramètre | Type | Défaut | Description |
|-----------|------|--------|-------------|
| `DefaultSize` | entier | `4096` | Cache par défaut (4 Mo) |
| `FixedSize` | entier | `2048` | Disque fixe/local (2 Mo) |
| `RamdiskSize` | entier | `512` | Disque RAM (512 Ko) |
| `CDROMSize` | entier | `4096` | Lecteurs CD-ROM (4 Mo) |
| `RemovableSize` | entier | `2048` | Médias USB/amovibles (2 Mo) |
| `NetworkSize` | entier | `8192` | Partages réseau/NFS/SMB (8 Mo) |
| `InternetSize` | entier | `16384` | Streaming HTTP/HTTPS (16 Mo) |
| `DVDSize` | entier | `8192` | DVD/Blu-ray (8 Mo) |

### Détection du Type de Cache

L'application détecte automatiquement le type de source :

| Motif | Type de Cache |
|-------|---------------|
| `http://`, `https://` | Internet |
| `smb://`, `nfs://` | Réseau |
| `dvd://`, `dvdnav://`, `bd://` | DVD |
| `cdda://` | CD-ROM |
| Clés USB | Amovible |
| Fichiers locaux | Fixe |

---

## Paramètres de l'Égaliseur

Stockés dans la section `[Equalizer]`.

| Paramètre | Type | Plage | Défaut | Description |
|-----------|------|-------|--------|-------------|
| `Enabled` | booléen | - | `false` | Activer l'égaliseur |
| `Band0` à `Band9` | double | -12 à +12 | `0` | Gain de la bande en dB |

### Bandes de l'Égaliseur

| Bande | Fréquence | Étiquette |
|-------|-----------|-----------|
| 0 | 31 Hz | 31 |
| 1 | 62 Hz | 62 |
| 2 | 125 Hz | 125 |
| 3 | 250 Hz | 250 |
| 4 | 500 Hz | 500 |
| 5 | 1000 Hz | 1K |
| 6 | 2000 Hz | 2K |
| 7 | 4000 Hz | 4K |
| 8 | 8000 Hz | 8K |
| 9 | 16000 Hz | 16K |

---

## Mode de Lecture

Stocké dans la section `[Playlist]`.

| Paramètre | Type | Défaut | Description |
|-----------|------|--------|-------------|
| `PlaybackMode` | entier | `0` | Mode de lecture actuel |

**Valeurs du Mode de Lecture :**

| Valeur | Mode | Description |
|--------|------|-------------|
| 0 | Normal | Lecture séquentielle, une seule fois |
| 1 | Répéter un | Répéter la piste actuelle |
| 2 | Répéter tout | Boucler toute la playlist |
| 3 | Aléatoire | Ordre aléatoire, arrêt à la fin |
| 4 | Aléatoire + Répéter | Ordre aléatoire, boucle infinie |

---

## Historique et Fichiers Récents

### Historique

Stocké dans le fichier `history.ini`.

Chaque entrée d'historique contient :
- `FileName` : Chemin complet du fichier média
- `Title` : Titre du média
- `Position` : Dernière position de lecture (secondes)
- `Duration` : Durée totale (secondes)
- `LastPlayed` : Date/heure de dernière lecture
- `PlayCount` : Nombre de fois joué

### Fichiers Récents

Stockés dans la section `[RecentFiles]` de la configuration principale.

| Paramètre | Type | Défaut | Description |
|-----------|------|--------|-------------|
| `Count` | entier | - | Nombre de fichiers récents |
| `File0` à `FileN` | chaîne | - | Chemins des fichiers récents |

Maximum : 20 fichiers (les plus anciens sont supprimés automatiquement)

---

## Signets

Stockés dans le fichier `bookmarks.ini`.

Chaque signet contient :
- `FileName` : Chemin complet du fichier média
- `Name` : Nom du signet défini par l'utilisateur
- `Position` : Position du signet (secondes)
- `CreatedAt` : Date/heure de création
- `Thumbnail` : Chemin de la miniature (optionnel)

### Opérations sur les Signets

- **Ajouter** : Ctrl+B à la position actuelle
- **Aller à** : Sélectionner dans le menu Signets
- **Supprimer** : Clic droit sur le signet dans le menu

---

## Favoris

Stockés dans le fichier `favorites.ini`.

Chaque favori contient :
- `Name` : Nom d'affichage
- `Path` : Chemin du fichier ou URL
- `Type` : Type de favori (voir ci-dessous)
- `Category` : Nom de catégorie (optionnel)
- `AddedAt` : Date/heure d'ajout
- `LastPlayed` : Date/heure de dernière lecture
- `PlayCount` : Nombre de fois joué

**Types de Favoris :**

| Valeur | Type |
|--------|------|
| 0 | Fichier local |
| 1 | URL/Stream |
| 2 | Station radio |
| 3 | DVD |
| 4 | Blu-ray |

---

## Restauration de Session

Stockée dans le fichier `session_playlist.ini`.

Quand `AutoSavePlaylist` est activé, l'application sauvegarde :
- Éléments de la playlist actuelle
- Index de lecture actuel
- Position de lecture actuelle
- Mode de lecture

Au prochain lancement, la session est automatiquement restaurée.

### Positions de Lecture

Les positions individuelles des fichiers sont stockées dans la section `[PlaybackPositions]` de la configuration principale en utilisant un hash CRC32 du nom de fichier comme clé.

- Position minimale : 5 secondes (les sessions plus courtes ne sont pas sauvegardées)
- Utilisé pour la fonction "Reprendre la lecture"

---

## Structure du Fichier INI

### Exemple `config.ini`

```ini
[General]
Language=fr
SingleInstance=1
ScreenshotPath=/home/utilisateur/Images/3nity/
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
Deinterlace=2
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
DefaultSize=4096
FixedSize=2048
RamdiskSize=512
CDROMSize=4096
RemovableSize=2048
NetworkSize=8192
InternetSize=16384
DVDSize=8192

[Playlist]
PlaybackMode=0

[Equalizer]
Enabled=0
Band0=0
Band1=0
Band2=0
Band3=0
Band4=0
Band5=0
Band6=0
Band7=0
Band8=0
Band9=0

[MainWindow]
Left=100
Top=100
Width=800
Height=600
Maximized=0

[RecentFiles]
Count=3
File0=/chemin/vers/video1.mp4
File1=/chemin/vers/video2.mkv
File2=/chemin/vers/musique.mp3
```

---

## Configuration Avancée

### Variables d'Environnement

| Variable | Utilisation |
|----------|-------------|
| `HOME` (Linux) | Utilisée pour localiser le répertoire de configuration |
| `APPDATA` (Windows) | Utilisée pour localiser le répertoire de configuration |
| `XDG_PICTURES_DIR` | Utilisée pour le chemin par défaut des captures d'écran |

### Remplacement en Ligne de Commande

Certains paramètres peuvent être remplacés via la ligne de commande :

```bash
# Démarrer avec un répertoire de configuration spécifique
3nity --config-dir=/chemin/vers/config

# Démarrer avec un fichier spécifique
3nity /chemin/vers/video.mp4
```

### Réinitialisation de la Configuration

Pour réinitialiser tous les paramètres aux valeurs par défaut :

```bash
# Linux
rm -rf ~/.config/3nity-media/

# Windows (PowerShell)
Remove-Item -Recurse "$env:APPDATA\3nity-media"
```

L'application recréera la configuration par défaut au prochain lancement.

---

## Voir Aussi

- [Guide Utilisateur](USER_GUIDE.md)
- [Raccourcis Clavier](SHORTCUTS.md)
- [Référence API](API_REFERENCE.md)
- [Paramètres CLI](CLI_PARAMETERS.md)

---

## Informations de Version

- **Dernière mise à jour :** 2026-01-02
- **S'applique à :** 3nity Media v0.x et versions ultérieures
