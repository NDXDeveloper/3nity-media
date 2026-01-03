# Guide d'Installation - 3nity Media

Ce guide couvre toutes les méthodes d'installation de 3nity Media sur Linux et Windows.

## Table des Matières

- [Configuration Requise](#configuration-requise)
- [Dépendances](#d%C3%A9pendances)
- [Installation par Paquet](#installation-par-paquet)
  - [DEB (Ubuntu/Debian)](#deb-ubuntudebian)
  - [RPM (Fedora/openSUSE)](#rpm-fedoraopensuse)
  - [AppImage (Universel)](#appimage-universel)
  - [Snap](#snap)
  - [Flatpak](#flatpak)
  - [Windows](#windows-1)
- [Compilation depuis les Sources](#compilation-depuis-les-sources)
  - [Linux](#compilation-linux)
  - [Windows](#compilation-windows)
- [Post-Installation](#post-installation)
- [Dépannage](#d%C3%A9pannage)
- [Désinstallation](#d%C3%A9sinstallation)

---

## Configuration Requise

### Minimum
- **OS** : Linux (kernel 4.15+) ou Windows 10
- **CPU** : x86_64 (64 bits)
- **RAM** : 512 Mo
- **Disque** : 50 Mo d'espace libre
- **Affichage** : X11 ou Wayland (Linux), DirectX 11 (Windows)

### Recommandé
- **RAM** : 2 Go ou plus
- **GPU** : Support de l'accélération vidéo matérielle (VA-API, VDPAU ou Vulkan)
- **Audio** : PulseAudio, PipeWire ou ALSA (Linux)

---

## Dépendances

### Linux - Distributions Debian/Ubuntu

| Paquet | Fonction | Requis |
|--------|----------|--------|
| `libmpv2` ou `libmpv1` | Moteur de lecture multimédia | Oui |
| `libqt5pas1` | Bindings Qt5 pour Pascal | Oui |
| `ffmpeg` | Extraction des métadonnées (ffprobe) | Oui |
| `yt-dlp` | Support YouTube/streaming | Recommandé |

Installer toutes les dépendances :
```bash
sudo apt update
sudo apt install libmpv2 libqt5pas1 ffmpeg yt-dlp
```

Si `libmpv2` n'est pas disponible :
```bash
sudo apt install libmpv1 libqt5pas1 ffmpeg yt-dlp
```

### Linux - Distributions Fedora/RHEL

| Paquet | Fonction | Requis |
|--------|----------|--------|
| `mpv-libs` | Moteur de lecture multimédia | Oui |
| `qt5-qtbase` | Framework Qt5 | Oui |
| `ffmpeg` | Extraction des métadonnées (ffprobe) | Oui |
| `yt-dlp` | Support YouTube/streaming | Recommandé |

Installer toutes les dépendances :
```bash
# Activer RPM Fusion pour ffmpeg
sudo dnf install https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm

sudo dnf install mpv-libs qt5-qtbase ffmpeg yt-dlp
```

### Linux - openSUSE

```bash
sudo zypper install libmpv2 libQt5Core5 ffmpeg yt-dlp
```

### Windows

Les dépendances Windows sont incluses avec l'installateur :
- `libmpv-2.dll` - Moteur de lecture multimédia
- `ffprobe.exe` - Extraction des métadonnées

Optionnel : Installer [yt-dlp](https://github.com/yt-dlp/yt-dlp/releases) pour le support YouTube.

---

## Installation par Paquet

### DEB (Ubuntu/Debian)

Distributions supportées : Ubuntu 20.04+, Debian 11+, Linux Mint 20+, Pop!_OS, elementary OS

**Méthode 1 : Télécharger et installer**
```bash
# Télécharger le paquet
wget https://github.com/NDXDeveloper/3nity-media/releases/latest/download/3nity-media_VERSION_amd64.deb

# Installer avec les dépendances
sudo apt install ./3nity-media_VERSION_amd64.deb
```

**Méthode 2 : Avec dpkg**
```bash
# Installer les dépendances d'abord
sudo apt install libmpv2 libqt5pas1 ffmpeg

# Installer le paquet
sudo dpkg -i 3nity-media_VERSION_amd64.deb

# Corriger les dépendances manquantes
sudo apt install -f
```

### RPM (Fedora/openSUSE)

Distributions supportées : Fedora 38+, openSUSE Leap 15.5+, openSUSE Tumbleweed, RHEL 9+, CentOS Stream 9+

**Fedora :**
```bash
# Télécharger le paquet
wget https://github.com/NDXDeveloper/3nity-media/releases/latest/download/3nity-media-VERSION-1.x86_64.rpm

# Installer avec les dépendances
sudo dnf install ./3nity-media-VERSION-1.x86_64.rpm
```

**openSUSE :**
```bash
# Télécharger le paquet
wget https://github.com/NDXDeveloper/3nity-media/releases/latest/download/3nity-media-VERSION-1.x86_64.rpm

# Installer avec les dépendances
sudo zypper install ./3nity-media-VERSION-1.x86_64.rpm
```

### AppImage (Universel)

Fonctionne sur toute distribution Linux avec support FUSE.

```bash
# Télécharger
wget https://github.com/NDXDeveloper/3nity-media/releases/latest/download/3nity-Media-VERSION-x86_64.AppImage

# Rendre exécutable
chmod +x 3nity-Media-VERSION-x86_64.AppImage

# Lancer
./3nity-Media-VERSION-x86_64.AppImage
```

**Optionnel : Intégration au bureau**
```bash
# Avec AppImageLauncher (recommandé)
sudo apt install appimagelauncher  # Ubuntu/Debian
sudo dnf install appimagelauncher  # Fedora

# Ou créer manuellement une entrée de bureau
mkdir -p ~/.local/share/applications
cat > ~/.local/share/applications/3nity-media.desktop << 'EOF'
[Desktop Entry]
Type=Application
Name=3nity Media
Exec=/chemin/vers/3nity-Media-VERSION-x86_64.AppImage %F
Icon=3nity-media
Categories=AudioVideo;Audio;Video;Player;
MimeType=audio/*;video/*;
EOF
```

### Snap

```bash
# Depuis le Snap Store (quand publié)
sudo snap install 3nity-media

# Depuis un fichier téléchargé
wget https://github.com/NDXDeveloper/3nity-media/releases/latest/download/3nity-media_VERSION_amd64.snap
sudo snap install --dangerous 3nity-media_VERSION_amd64.snap
```

### Flatpak

```bash
# Ajouter Flathub (si pas déjà fait)
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

# Depuis un fichier téléchargé
wget https://github.com/NDXDeveloper/3nity-media/releases/latest/download/3nity-media-VERSION.flatpak
flatpak install 3nity-media-VERSION.flatpak
```

### Windows

**Méthode 1 : Installateur système (recommandé)**

Télécharger et exécuter `3nity-Media-Setup-VERSION.exe`
- Nécessite les droits administrateur
- S'installe dans `C:\Program Files\3nity Media`
- Crée des raccourcis dans le menu Démarrer
- Raccourci bureau optionnel

**Méthode 2 : Installateur utilisateur (sans droits admin)**

Télécharger et exécuter `3nity-Media-Setup-User-VERSION.exe`
- Aucun droit administrateur requis
- S'installe dans `%LOCALAPPDATA%\Programs\3nity Media`
- Crée des raccourcis spécifiques à l'utilisateur

**Méthode 3 : Portable**

Télécharger et extraire `3nity-media-windows-portable-VERSION.zip`
- Aucune installation requise
- Lancer `3nity.exe` directement
- Paramètres stockés dans le même dossier

---

## Compilation depuis les Sources

### Compilation Linux

#### Prérequis

Installer l'IDE Lazarus et les dépendances de développement :

**Ubuntu/Debian :**
```bash
sudo apt update
sudo apt install \
  lazarus-ide-qt5 \
  lazarus-src \
  lcl-qt5 \
  lcl-utils \
  fpc \
  libqt5pas1 \
  libqt5pas-dev \
  libmpv-dev \
  ffmpeg \
  git \
  make
```

**Fedora :**
```bash
sudo dnf install \
  lazarus \
  fpc \
  fpc-src \
  qt5-qtbase-devel \
  mpv-libs-devel \
  ffmpeg \
  git \
  make
```

**Arch Linux :**
```bash
sudo pacman -S lazarus fpc qt5pas mpv ffmpeg git make
```

#### Cloner et Compiler

```bash
# Cloner le dépôt
git clone https://github.com/NDXDeveloper/3nity-media.git
cd 3nity-media

# Compiler avec le Makefile (recommandé)
make build-release

# Ou compiler avec lazbuild directement
cd src
lazbuild --build-mode=Release TrinityMedia.lpi
```

Le binaire sera créé dans `bin/x86_64-linux/3nity-media`

#### Options de Compilation

```bash
# Build debug (avec symboles de débogage)
make build-app

# Build release (optimisé)
make build-release

# Nettoyer et recompiler
make rebuild

# Compiler avec une version spécifique
make release V=0.1.0

# Afficher toutes les cibles disponibles
make help
```

### Compilation Windows

#### Prérequis

1. **Installer Lazarus**
   - Télécharger depuis [lazarus-ide.org](https://www.lazarus-ide.org/index.php?page=downloads)
   - Ou via Chocolatey : `choco install lazarus`

2. **Installer Git** (optionnel, pour cloner)
   - Télécharger depuis [git-scm.com](https://git-scm.com/download/win)
   - Ou via Chocolatey : `choco install git`

#### Étapes de Compilation

```powershell
# Cloner le dépôt
git clone https://github.com/NDXDeveloper/3nity-media.git
cd 3nity-media\src

# Compiler avec lazbuild
& "C:\lazarus\lazbuild.exe" --build-mode=Release TrinityMedia.lpi
```

Le binaire sera créé dans `bin\x86_64-win64\3nity.exe`

#### DLLs Requises

Copier ces fichiers dans le même dossier que `3nity.exe` :

1. **libmpv-2.dll**
   - Télécharger depuis [SourceForge mpv-player-windows](https://sourceforge.net/projects/mpv-player-windows/files/libmpv/)
   - Extraire et copier `libmpv-2.dll`

2. **ffprobe.exe**
   - Télécharger depuis [gyan.dev](https://www.gyan.dev/ffmpeg/builds/)
   - Extraire `ffprobe.exe` de l'archive

---

## Post-Installation

### Premier Lancement

1. Lancer 3nity Media depuis le menu applications ou la ligne de commande
2. Au premier lancement, les paramètres par défaut sont créés
3. L'application détecte automatiquement la langue système

### Emplacement de la Configuration

| Plateforme | Emplacement |
|------------|-------------|
| Linux | `~/.config/3nity-media/` |
| Windows | `%APPDATA%\3nity-media\` |
| Flatpak | `~/.var/app/com.ndxdev.3nity-media/config/` |
| Snap | `~/snap/3nity-media/current/.config/` |

Fichiers de configuration :
- `config.ini` - Paramètres généraux
- `shortcuts.ini` - Raccourcis clavier personnalisés
- `favorites.ini` - Stations/fichiers favoris
- `history.ini` - Historique de lecture

### Associations de Fichiers

**Linux (DEB/RPM) :**
Les associations de fichiers sont automatiquement configurées lors de l'installation.

**Linux (AppImage/Portable) :**
```bash
# Créer des associations MIME
xdg-mime default 3nity-media.desktop audio/mpeg
xdg-mime default 3nity-media.desktop video/mp4
# Ajouter d'autres types selon besoin
```

**Windows :**
Clic droit sur un fichier média → Ouvrir avec → Choisir une autre application → Sélectionner 3nity → Cocher "Toujours utiliser cette application"

### Accélération GPU

**Linux - VA-API (Intel/AMD) :**
```bash
# Installer les pilotes VA-API
sudo apt install va-driver-all vainfo  # Ubuntu/Debian
sudo dnf install libva-utils           # Fedora
```

**Linux - VDPAU (NVIDIA) :**
```bash
# Installer les pilotes VDPAU
sudo apt install vdpau-driver-all vdpauinfo  # Ubuntu/Debian
```

---

## Dépannage

### "libmpv.so not found"

La bibliothèque mpv est manquante. L'installer :
```bash
# Ubuntu/Debian
sudo apt install libmpv2

# Fedora
sudo dnf install mpv-libs
```

### "libQt5Pas.so not found"

Les bindings Qt5 Pascal sont manquants. Les installer :
```bash
# Ubuntu/Debian
sudo apt install libqt5pas1

# Fedora - vous devrez peut-être compiler depuis les sources ou utiliser AppImage
```

### Pas de son

1. Vérifier que PulseAudio/PipeWire fonctionne :
   ```bash
   pactl info
   ```

2. Vérifier les niveaux de volume :
   ```bash
   pavucontrol  # Installer : apt install pavucontrol
   ```

3. Vérifier les périphériques ALSA :
   ```bash
   aplay -l
   ```

### Problèmes de lecture vidéo

1. Vérifier l'accélération GPU :
   ```bash
   vainfo      # VA-API
   vdpauinfo   # VDPAU
   vulkaninfo  # Vulkan
   ```

2. Essayer le rendu logiciel :
   - Options → Vidéo → Décodage matériel → Désactivé

### L'AppImage ne se lance pas

Installer FUSE :
```bash
# Ubuntu/Debian
sudo apt install libfuse2

# Fedora
sudo dnf install fuse-libs
```

### Problèmes de permissions Flatpak

Accorder des permissions supplémentaires :
```bash
# Accès aux disques externes
flatpak override --user --filesystem=/media com.ndxdev.3nity-media

# Accès aux partages réseau
flatpak override --user --filesystem=/mnt com.ndxdev.3nity-media
```

---

## Désinstallation

### DEB
```bash
sudo apt remove 3nity-media
sudo apt autoremove  # Supprimer les dépendances inutilisées
```

### RPM
```bash
# Fedora
sudo dnf remove 3nity-media

# openSUSE
sudo zypper remove 3nity-media
```

### AppImage
```bash
rm ~/Applications/3nity-Media-*.AppImage
rm ~/.local/share/applications/3nity-media.desktop
```

### Snap
```bash
sudo snap remove 3nity-media
```

### Flatpak
```bash
flatpak uninstall com.ndxdev.3nity-media
```

### Windows
- **Version installateur** : Panneau de configuration → Programmes → Désinstaller "3nity Media"
- **Version portable** : Supprimer le dossier

### Supprimer la configuration (toutes plateformes)

```bash
# Linux
rm -rf ~/.config/3nity-media/

# Windows (PowerShell)
Remove-Item -Recurse "$env:APPDATA\3nity-media"
```

---

## Voir Aussi

- [Guide Utilisateur](USER_GUIDE.md)
- [Paramètres CLI](CLI_PARAMETERS.md)
- [Raccourcis Clavier](SHORTCUTS.md)
- [Guide de Configuration](CONFIG.md)

---

## Informations de Version

- **Dernière mise à jour :** 2026-01-02
- **S'applique à :** 3nity Media v0.x et versions ultérieures
