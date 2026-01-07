# 3nity Media - Guide Utilisateur

Guide complet pour utiliser 3nity Media, un lecteur multimédia léger et multiplateforme.

---

## Table des matières

1. [Premiers pas](#premiers-pas)
   - [Installation](#installation)
   - [Premier lancement](#premier-lancement)
   - [Vue d'ensemble de l'interface](#vue-densemble-de-linterface)
2. [Lecture de médias](#lecture-de-médias)
   - [Ouvrir des fichiers](#ouvrir-des-fichiers)
   - [Ouvrir un DVD ou Blu-ray](#ouvrir-un-dvd-ou-blu-ray)
   - [Ouvrir des URLs et flux](#ouvrir-des-urls-et-flux)
   - [Glisser-déposer](#glisser-déposer)
   - [Contrôles de lecture](#contrôles-de-lecture)
3. [Playlist](#playlist)
   - [Ajouter des éléments](#ajouter-des-éléments)
   - [Gérer la playlist](#gérer-la-playlist)
   - [Sauvegarder et charger des playlists](#sauvegarder-et-charger-des-playlists)
   - [Lecture aléatoire et répétition](#lecture-aléatoire-et-répétition)
4. [Radio Internet](#radio-internet)
   - [Parcourir les stations](#parcourir-les-stations)
   - [Recherche et filtrage](#recherche-et-filtrage)
   - [Stations personnalisées](#stations-personnalisées)
   - [Enregistrer les flux](#enregistrer-les-flux)
5. [Fonctionnalités audio](#fonctionnalités-audio)
   - [Contrôle du volume](#contrôle-du-volume)
   - [Égaliseur](#égaliseur)
   - [Délai audio](#délai-audio)
   - [Visualisations audio](#visualisations-audio)
6. [Fonctionnalités vidéo](#fonctionnalités-vidéo)
   - [Mode plein écran](#mode-plein-écran)
   - [Ajustements vidéo](#ajustements-vidéo)
   - [Zoom et rotation](#zoom-et-rotation)
   - [Captures d'écran](#captures-décran)
   - [Sous-titres](#sous-titres)
7. [Lecture avancée](#lecture-avancée)
   - [Navigation temporelle](#navigation-temporelle)
   - [Vitesse de lecture](#vitesse-de-lecture)
   - [Boucle A-B](#boucle-a-b)
   - [Image par image](#image-par-image)
   - [Chapitres](#chapitres)
   - [Navigation DVD et Blu-ray](#navigation-dvd-et-blu-ray)
8. [Favoris et signets](#favoris-et-signets)
   - [Gérer les favoris](#gérer-les-favoris)
   - [Créer des signets](#créer-des-signets)
9. [Historique](#historique)
10. [Paramètres](#paramètres)
    - [Options générales](#options-générales)
    - [Raccourcis clavier](#raccourcis-clavier)
    - [Langue](#langue)
11. [Référence des raccourcis clavier](#référence-des-raccourcis-clavier)
12. [Utilisation en ligne de commande](#utilisation-en-ligne-de-commande)
13. [Dépannage](#dépannage)

---

## Premiers pas

### Installation

#### Linux

**Depuis un paquet DEB (Ubuntu/Debian) :**
```bash
sudo apt install ./3nity-media_amd64.deb
```

**Depuis un AppImage :**
```bash
chmod +x 3nity-Media-x86_64.AppImage
./3nity-Media-x86_64.AppImage
```

**Dépendances :**
```bash
sudo apt install libmpv2 libqt5pas1
```

#### Windows

- **Installateur :** Exécutez `3nity-Media-Setup.exe`
- **Portable :** Extrayez `3nity-media-windows-portable.zip` et lancez `3nity.exe`

### Premier lancement

Lors du premier lancement de 3nity Media, vous verrez la fenêtre principale du lecteur. L'interface est conçue pour être épurée et discrète, mettant votre contenu multimédia au premier plan.

### Vue d'ensemble de l'interface

```
┌─────────────────────────────────────────────────────────┐
│  Barre de menus                                         │
├─────────────────────────────────────────────────────────┤
│                                                         │
│                                                         │
│                    Zone vidéo                           │
│              (ou visualisation audio)                   │
│                                                         │
│                                                         │
├─────────────────────────────────────────────────────────┤
│   barre de progression              │ Durée             │
├─────────────────────────────────────────────────────────┤
│  ◀◀  ▶/❚❚  ▶▶  │  🔊 Volume  │  Info piste            │
└─────────────────────────────────────────────────────────┘
```

**Zones principales :**
- **Barre de menus :** Accès à toutes les fonctionnalités et paramètres
- **Zone vidéo :** Affiche le contenu vidéo ou les visualisations audio
- **Barre de progression :** Indique la position de lecture (cliquez pour naviguer)
- **Barre de contrôle :** Contrôles de lecture, volume et informations sur la piste

---

## Lecture de médias

### Ouvrir des fichiers

**Méthode 1 : Menu**
1. Allez dans `Fichier` → `Ouvrir un fichier` (ou appuyez sur `Ctrl+O`)
2. Sélectionnez un ou plusieurs fichiers
3. Cliquez sur `Ouvrir`

**Méthode 2 : Double-clic**
- Double-cliquez sur n'importe quel fichier multimédia pris en charge pour l'ouvrir avec 3nity Media

**Formats pris en charge :**
- **Vidéo :** MP4, MKV, AVI, MOV, WMV, FLV, WebM, et plus
- **Audio :** MP3, FLAC, OGG, WAV, AAC, M4A, WMA, et plus
- **Playlists :** M3U, M3U8, PLS, XSPF
- **Formats disque :** DVD (VIDEO_TS/VOB), Blu-ray (BDMV/M2TS)

### Ouvrir un DVD ou Blu-ray

3nity Media prend en charge la lecture de contenu DVD et Blu-ray depuis des dossiers (disques rippés ou images ISO montées).

**Ouvrir un DVD :**
1. Allez dans `Fichier` → `Ouvrir DVD`
2. Sélectionnez le dossier du DVD (celui contenant le sous-dossier `VIDEO_TS`)
3. Le film principal (titre le plus volumineux) sera lu automatiquement

**Ouvrir un Blu-ray :**
1. Allez dans `Fichier` → `Ouvrir Blu-ray`
2. Sélectionnez le dossier du Blu-ray (celui contenant le sous-dossier `BDMV`)
3. Le film principal (fichier .m2ts le plus volumineux) sera lu automatiquement

**Mode de repli :**
Si les protocoles natifs DVD/Blu-ray (dvdnav://, bluray://) ne sont pas disponibles sur votre système, 3nity Media utilise automatiquement le mode de repli :
- **DVD :** Tous les fichiers VOB du titre principal sont ajoutés à la playlist et lus séquentiellement
- **Blu-ray :** Le fichier .m2ts le plus volumineux du dossier STREAM est lu directement

Cela garantit la lecture même sans libbluray ou libdvdnav installés.

### Ouvrir des URLs et flux

1. Allez dans `Fichier` → `Ouvrir une URL` (ou appuyez sur `Ctrl+U`)
2. Entrez l'URL (ex : `https://exemple.com/flux.mp3`)
3. Cliquez sur `OK`

**Types d'URL pris en charge :**
- Flux HTTP/HTTPS
- YouTube (si youtube-dl/yt-dlp est installé)
- Flux de radio internet
- Liens directs vers des médias

### Glisser-déposer

Glissez des fichiers ou dossiers sur le lecteur :

- **Déposer sur la zone vidéo :** Les fichiers sont ajoutés et la lecture démarre immédiatement
- **Déposer ailleurs sur la fenêtre principale :** Les fichiers sont ajoutés sans lecture
- **Déposer sur la fenêtre playlist :** Les fichiers sont ajoutés sans lecture
- **Dossiers :** Tous les fichiers multimédias sont scannés récursivement (jusqu'à 10 niveaux)

### Contrôles de lecture

| Contrôle | Action |
|----------|--------|
| `Espace` ou bouton Lecture | Lecture / Pause |
| Bouton Stop | Arrêter la lecture |
| Bouton Précédent | Piste précédente |
| Bouton Suivant | Piste suivante |
| Barre de progression | Cliquez pour naviguer |

---

## Playlist

### Ajouter des éléments

**Depuis le menu :**
- `Playlist` → `Ajouter des fichiers` - Ajouter des fichiers spécifiques
- `Playlist` → `Ajouter un dossier` - Ajouter tous les fichiers d'un dossier

**Par glisser-déposer :**
- Glissez des fichiers ou dossiers sur la fenêtre playlist (ajoute sans lecture)

**Depuis l'explorateur de fichiers :**
- Clic droit sur un fichier et sélectionnez "Ajouter à la playlist"

### Gérer la playlist

Ouvrez le panneau de playlist avec `Affichage` → `Playlist` ou appuyez sur le bouton playlist.

**Actions disponibles :**
- **Lire :** Double-cliquez sur un élément ou sélectionnez et appuyez sur Entrée
- **Supprimer :** Sélectionnez le(s) élément(s) et appuyez sur Suppr
- **Déplacer :** Faites glisser les éléments pour les réorganiser
- **Tout sélectionner :** `Ctrl+A`
- **Vider :** Supprimer tous les éléments de la playlist

**Menu contextuel (clic droit) :**
- Lire
- Supprimer de la playlist
- Supprimer les doublons
- Afficher dans le gestionnaire de fichiers
- Informations sur le média

### Sauvegarder et charger des playlists

**Sauvegarder une playlist :**
1. Allez dans `Playlist` → `Sauvegarder la playlist`
2. Choisissez le format (M3U, M3U8, PLS ou XSPF)
3. Entrez le nom du fichier et sauvegardez

**Charger une playlist :**
1. Allez dans `Playlist` → `Ouvrir une playlist`
2. Sélectionnez un fichier de playlist
3. Les éléments sont ajoutés à la playlist actuelle

### Lecture aléatoire et répétition

Accessible depuis le menu `Lecture` ou la barre d'outils de la playlist :

| Mode | Description |
|------|-------------|
| **Répétition désactivée** | Arrêt après la dernière piste |
| **Répéter tout** | Boucle sur toute la playlist |
| **Répéter un** | Boucle sur la piste actuelle |
| **Aléatoire** | Ordre de lecture aléatoire |

---

## Radio Internet

### Parcourir les stations

1. Allez dans `Affichage` → `Radio Internet` (ou `Outils` → `Radio`)
2. Attendez que la liste des stations se charge depuis le répertoire Icecast
3. Parcourez ou recherchez des stations

### Recherche et filtrage

**Recherche :**
- Tapez dans la zone de recherche pour filtrer les stations par nom, genre ou pays

**Filtrer par genre :**
- Utilisez le menu déroulant des genres pour n'afficher que les stations d'un genre spécifique

### Stations personnalisées

**Ajouter une station personnalisée :**
1. Allez dans `Radio` → `Ajouter une station personnalisée`
2. Entrez le nom de la station et l'URL
3. Cliquez sur `OK`

Les stations personnalisées apparaissent dans une section séparée et sont sauvegardées localement.

### Enregistrer les flux

Pendant la lecture d'un flux de radio internet :
1. Cliquez sur le bouton Enregistrer ou allez dans `Outils` → `Enregistrer le flux`
2. L'enregistrement est sauvegardé dans votre dossier de sortie configuré
3. Cliquez sur Arrêter l'enregistrement quand vous avez terminé

Les enregistrements sont sauvegardés en MP3 ou dans le format natif du flux.

---

## Fonctionnalités audio

### Contrôle du volume

**Ajuster le volume :**
- Utilisez le curseur de volume dans la barre de contrôle
- Appuyez sur `+` ou `Flèche Haut` pour augmenter
- Appuyez sur `-` ou `Flèche Bas` pour diminuer
- Appuyez sur `M` pour couper/rétablir le son

**Plage de volume :** 0% à 100% (peut être amplifié davantage dans les paramètres)

### Égaliseur

1. Allez dans `Outils` → `Égaliseur` (ou appuyez sur `E`)
2. Ajustez les 10 bandes de fréquences :
   - 31 Hz, 62 Hz, 125 Hz, 250 Hz, 500 Hz
   - 1 kHz, 2 kHz, 4 kHz, 8 kHz, 16 kHz

**Préréglages :**
- Plat, Rock, Pop, Jazz, Classique, Boost Basses, Boost Aigus, et plus

**Préréglages personnalisés :**
- Ajustez les curseurs selon vos préférences
- Cliquez sur "Sauvegarder le préréglage" pour sauvegarder vos paramètres

### Délai audio

Utile pour corriger les problèmes de synchronisation audio/vidéo :
- Appuyez sur `Ctrl+Plus` pour augmenter le délai audio
- Appuyez sur `Ctrl+Moins` pour diminuer le délai audio
- Appuyez sur `Ctrl+0` pour réinitialiser le délai

### Visualisations audio

Pour les fichiers audio, 3nity Media affiche des visualisations :

1. Allez dans `Affichage` → `Visualisations`
2. Choisissez un mode de visualisation :
   - **Spectre :** Analyseur de spectre de fréquences
   - **Forme d'onde :** Affichage de la forme d'onde audio
   - **Combiné :** Spectre et forme d'onde ensemble

**Changer les couleurs :**
- Appuyez sur `V` pour parcourir les schémas de couleurs
- Ou allez dans `Affichage` → `Couleurs de visualisation`

---

## Fonctionnalités vidéo

### Mode plein écran

- Appuyez sur `F` ou `F11` pour basculer en plein écran
- Double-cliquez sur la zone vidéo
- Allez dans `Affichage` → `Plein écran`

**Quitter le plein écran :**
- Appuyez sur `F`, `F11` ou `Échap`
- Double-cliquez sur la zone vidéo

### Ajustements vidéo

1. Allez dans `Outils` → `Ajustements vidéo`
2. Ajustez les paramètres suivants :
   - **Luminosité :** -100 à +100
   - **Contraste :** -100 à +100
   - **Saturation :** -100 à +100
   - **Gamma :** 0.1 à 10.0
   - **Teinte :** -180 à +180

Cliquez sur "Réinitialiser" pour restaurer les valeurs par défaut.

### Zoom et rotation

**Zoom :**
- Appuyez sur `Ctrl+Plus` pour zoomer
- Appuyez sur `Ctrl+Moins` pour dézoomer
- Appuyez sur `Ctrl+0` pour réinitialiser le zoom

**Rotation :**
- Appuyez sur `R` pour faire pivoter la vidéo de 90° dans le sens horaire
- Ou allez dans `Vidéo` → `Rotation`

**Adapter à la vidéo :**
- Appuyez sur `Ctrl+F` pour redimensionner la fenêtre aux dimensions de la vidéo

### Captures d'écran

- Appuyez sur `S` ou allez dans `Vidéo` → `Capture d'écran`
- Les captures sont sauvegardées dans votre dossier Images (configurable dans les paramètres)

### Sous-titres

**Charger des sous-titres :**
- Les sous-titres sont chargés automatiquement s'ils ont le même nom que la vidéo
- Allez dans `Sous-titres` → `Charger un fichier de sous-titres` pour charger manuellement

**Délai des sous-titres :**
- Appuyez sur `Z` pour diminuer le délai (les sous-titres apparaissent plus tôt)
- Appuyez sur `X` pour augmenter le délai (les sous-titres apparaissent plus tard)
- Appuyez sur `Maj+Z` pour réinitialiser le délai

**Piste de sous-titres :**
- Allez dans `Sous-titres` → Sélectionnez la piste (pour les sous-titres intégrés)

---

## Lecture avancée

### Navigation temporelle

| Action | Raccourci |
|--------|-----------|
| Avancer de 10 secondes | `Flèche Droite` |
| Reculer de 10 secondes | `Flèche Gauche` |
| Avancer d'1 minute | `Maj+Droite` |
| Reculer d'1 minute | `Maj+Gauche` |
| Aller à un moment précis | `Ctrl+G` |

### Vitesse de lecture

| Action | Raccourci |
|--------|-----------|
| Accélérer (10%) | `]` |
| Ralentir (10%) | `[` |
| Réinitialiser à la vitesse normale | `Retour arrière` |

Plage de vitesse : 0.25x à 4.0x

### Boucle A-B

Répéter une section du média :

1. Appuyez sur `L` au point de départ (définit le point A)
2. Appuyez à nouveau sur `L` au point de fin (définit le point B)
3. La section entre A et B sera répétée en boucle
4. Appuyez une troisième fois sur `L` pour effacer la boucle

### Image par image

Pour une navigation vidéo précise :
- Appuyez sur `.` (point) pour avancer d'une image
- Appuyez sur `,` (virgule) pour reculer d'une image

Note : La lecture doit être en pause pour le défilement image par image.

### Chapitres

Pour les médias avec chapitres (DVDs, fichiers MKV) :
- Appuyez sur `Page Haut` pour le chapitre précédent
- Appuyez sur `Page Bas` pour le chapitre suivant
- Allez dans `Lecture` → `Chapitres` pour voir la liste des chapitres

### Navigation DVD et Blu-ray

**Navigation dans les menus (avec les protocoles natifs) :**

| Action | Raccourci |
|--------|-----------|
| Menu DVD/Blu-ray | `Ctrl+M` |
| Naviguer Haut | `Flèche Haut` |
| Naviguer Bas | `Flèche Bas` |
| Naviguer Gauche | `Flèche Gauche` |
| Naviguer Droite | `Flèche Droite` |
| Sélectionner | `Entrée` |

**Note :** La navigation dans les menus n'est disponible qu'avec les protocoles natifs (dvdnav://, bluray://). En mode de repli, le contenu est lu directement sans support des menus.

**Lecture en mode de repli :**
- Pour les DVD, tous les fichiers VOB du titre principal sont mis en file d'attente dans la playlist
- Utilisez `N` (Suivant) et `P` (Précédent) pour passer d'un segment VOB à l'autre
- La navigation temporelle fonctionne normalement dans chaque segment

---

## Favoris et signets

### Gérer les favoris

**Ajouter aux favoris :**
1. Pendant la lecture d'un fichier ou d'une station radio
2. Allez dans `Favoris` → `Ajouter aux favoris`
3. Organisez optionnellement en catégories

**Accéder aux favoris :**
- Allez dans le menu `Favoris` pour voir vos éléments sauvegardés
- Cliquez sur n'importe quel favori pour lancer la lecture

**Supprimer un favori :**
- Clic droit sur un favori et sélectionnez "Supprimer"

### Créer des signets

Les signets sauvegardent votre position dans un fichier :

**Ajouter un signet :**
1. Pendant la lecture, allez dans `Lecture` → `Ajouter un signet`
2. Entrez un nom pour le signet
3. La position actuelle est sauvegardée

**Aller à un signet :**
- Allez dans `Lecture` → `Signets` → Sélectionnez le signet

**Sauvegarde automatique de la position :**
- 3nity Media se souvient où vous vous êtes arrêté dans chaque fichier
- Reprend automatiquement la lecture quand vous rouvrez le fichier

---

## Historique

3nity Media garde une trace des fichiers récemment lus :

1. Allez dans `Fichier` → `Historique` (ou `Affichage` → `Historique`)
2. Consultez votre historique de lecture récent
3. Cliquez sur n'importe quel élément pour le relire

**Effacer l'historique :**
- Allez dans `Fichier` → `Historique` → `Effacer l'historique`

---

## Paramètres

### Options générales

Allez dans `Outils` → `Options` pour accéder aux paramètres :

**Lecture :**
- Reprendre la position de lecture
- Lecture automatique au démarrage
- Niveau de volume par défaut

**Interface :**
- Afficher/masquer les éléments
- Toujours au premier plan
- Comportement de la zone de notification

**Chemins :**
- Dossier des captures d'écran
- Dossier des enregistrements

### Raccourcis clavier

1. Allez dans `Outils` → `Raccourcis clavier`
2. Cliquez sur n'importe quelle action pour personnaliser son raccourci
3. Appuyez sur la nouvelle combinaison de touches
4. Cliquez sur "Appliquer" pour sauvegarder

**Réinitialiser les valeurs par défaut :**
- Cliquez sur "Tout réinitialiser" pour restaurer les raccourcis par défaut

### Langue

1. Allez dans `Outils` → `Options` → `Langue`
2. Sélectionnez votre langue préférée dans la liste
3. Cliquez sur "Appliquer"

3nity Media prend en charge 99 langues.

---

## Référence des raccourcis clavier

### Lecture

| Action | Raccourci par défaut |
|--------|----------------------|
| Lecture / Pause | `Espace` |
| Arrêt | `S` |
| Piste précédente | `P` |
| Piste suivante | `N` |
| Avancer de 10s | `Droite` |
| Reculer de 10s | `Gauche` |
| Avancer d'1 min | `Maj+Droite` |
| Reculer d'1 min | `Maj+Gauche` |
| Aller au temps | `Ctrl+G` |

### Vitesse

| Action | Raccourci par défaut |
|--------|----------------------|
| Accélérer | `]` |
| Ralentir | `[` |
| Réinitialiser la vitesse | `Retour arrière` |

### Volume

| Action | Raccourci par défaut |
|--------|----------------------|
| Volume + | `+` ou `Haut` |
| Volume - | `-` ou `Bas` |
| Sourdine | `M` |

### Vidéo

| Action | Raccourci par défaut |
|--------|----------------------|
| Plein écran | `F` ou `F11` |
| Rotation | `R` |
| Capture d'écran | `S` |
| Zoom + | `Ctrl+Plus` |
| Zoom - | `Ctrl+Moins` |
| Réinitialiser zoom | `Ctrl+0` |
| Toujours au premier plan | `T` |
| Adapter à la vidéo | `Ctrl+F` |

### Sous-titres

| Action | Raccourci par défaut |
|--------|----------------------|
| Délai + | `X` |
| Délai - | `Z` |
| Réinitialiser délai | `Maj+Z` |

### Avancé

| Action | Raccourci par défaut |
|--------|----------------------|
| Définir Boucle A | `L` |
| Définir Boucle B | `L` |
| Effacer la boucle | `L` |
| Image suivante | `.` |
| Image précédente | `,` |
| Chapitre précédent | `Page Haut` |
| Chapitre suivant | `Page Bas` |

### Application

| Action | Raccourci par défaut |
|--------|----------------------|
| Ouvrir un fichier | `Ctrl+O` |
| Ouvrir une URL | `Ctrl+U` |
| Afficher la playlist | `Ctrl+L` |
| Quitter | `Ctrl+Q` ou `Alt+F4` |

---

## Utilisation en ligne de commande

3nity Media peut être contrôlé depuis la ligne de commande :

```bash
# Lire un fichier
3nity /chemin/vers/fichier.mp4

# Lire une URL
3nity https://exemple.com/flux.mp3

# Lire un dossier (ajoute tous les fichiers médias)
3nity /chemin/vers/dossier/musique

# Ajouter des fichiers à l'instance existante (file d'attente)
3nity --enqueue ~/Musique/*.mp3

# Lire en plein écran avec options
3nity -f --volume=50 --start=2:00 film.mkv

# Lire avec sous-titres externes
3nity --sub=film.srt film.mkv

# Afficher l'aide
3nity --help
```

Pour une documentation CLI détaillée, voir [Paramètres CLI](CLI_PARAMETERS.md).

---

## Dépannage

### Pas de son

1. Vérifiez le volume système
2. Vérifiez le volume de 3nity Media (pas en sourdine)
3. Vérifiez le périphérique de sortie audio dans les paramètres système
4. Essayez un autre fichier audio pour isoler le problème

### La vidéo ne se lit pas

1. Assurez-vous que libmpv est correctement installé
2. Vérifiez si le format vidéo est pris en charge
3. Essayez de mettre à jour les pilotes graphiques
4. Consultez `Affichage` → `Journaux` pour les messages d'erreur

### Les sous-titres ne s'affichent pas

1. Assurez-vous que le fichier de sous-titres est dans le même dossier que la vidéo
2. Vérifiez l'encodage des sous-titres (essayez UTF-8)
3. Allez dans `Sous-titres` → `Charger un fichier de sous-titres` pour charger manuellement
4. Vérifiez que la piste de sous-titres est sélectionnée dans le menu `Sous-titres`

### Utilisation CPU élevée

1. Désactivez l'accélération matérielle si problématique
2. Réduisez la complexité des visualisations
3. Fermez les autres applications gourmandes en ressources

### L'application ne démarre pas

1. Vérifiez si les dépendances sont installées :
   ```bash
   sudo apt install libmpv2 libqt5pas1
   ```
2. Lancez depuis le terminal pour voir les messages d'erreur :
   ```bash
   3nity-media
   ```
3. Vérifiez s'il y a des instances en conflit

### La radio internet ne charge pas

1. Vérifiez votre connexion internet
2. Le répertoire Icecast peut être temporairement indisponible
3. Essayez d'ajouter des stations manuellement via les stations personnalisées

---

## Obtenir de l'aide

- **Signalement de bugs :** [GitHub Issues](https://github.com/NDXDeveloper/3nity-media/issues)
- **Documentation :** [Docs du projet](https://github.com/NDXDeveloper/3nity-media/tree/main/docs)

---

*3nity Media - Simple, puissant, le vôtre.*

## Informations de Version

- **Dernière mise à jour :** 2026-01-01
- **S'applique à :** 3nity Media v0.x et versions ultérieures
