# 3nity Media - Guide de Débogage

Ce guide fournit des outils et techniques pour déboguer 3nity Media selon les différentes méthodes d'installation.

---

## Table des Matières

- [Dépendances des Bibliothèques](#dépendances-des-bibliothèques)
- [Débogage Snap](#débogage-snap)
- [Débogage Flatpak](#débogage-flatpak)
- [Débogage AppImage](#débogage-appimage)
- [Variables d'Environnement](#variables-denvironnement)
- [Erreurs Courantes](#erreurs-courantes)

---

## Dépendances des Bibliothèques

### Vérifier les Dépendances Manquantes

Utilisez `ldd` pour lister toutes les dépendances de bibliothèques partagées et trouver celles qui manquent :

```bash
# Pour une installation système
ldd /usr/bin/3nity-media | grep "not found"

# Pour une bibliothèque spécifique
ldd /usr/lib/x86_64-linux-gnu/libmpv.so.2 | grep "not found"
```

### Trouver Quel Paquet Fournit une Bibliothèque

```bash
# Debian/Ubuntu
apt-file search libpulsecommon-16.1.so
dpkg -S libmpv.so.2

# Fedora
dnf provides */libmpv.so.2
```

### Vérifier qu'une Bibliothèque est Chargeable

```bash
# Vérifier si la bibliothèque peut être chargée
ldconfig -p | grep mpv
ldconfig -p | grep qt5pas
```

---

## Débogage Snap

### Entrer dans le Shell Snap

Exécuter des commandes dans l'environnement confiné du snap :

```bash
snap run --shell 3nity-media
```

### Vérifier les Variables d'Environnement

```bash
snap run --shell 3nity-media -c 'echo $LD_LIBRARY_PATH'
snap run --shell 3nity-media -c 'echo $QT_PLUGIN_PATH'
```

### Vérifier les Dépendances des Bibliothèques dans le Snap

```bash
# Lister les dépendances de libmpv
snap run --shell 3nity-media -c 'ldd /snap/3nity-media/current/usr/lib/x86_64-linux-gnu/libmpv.so.2'

# Trouver les dépendances manquantes
snap run --shell 3nity-media -c 'ldd /snap/3nity-media/current/usr/lib/x86_64-linux-gnu/libmpv.so.2 | grep "not found"'
```

### Déboguer le Chargement des Bibliothèques

```bash
# Afficher le processus de recherche des bibliothèques
snap run --shell 3nity-media -c 'LD_DEBUG=libs 3nity-media 2>&1 | grep -i mpv | head -20'

# Sortie de débogage complète
snap run --shell 3nity-media -c 'LD_DEBUG=libs 3nity-media 2>&1 | head -100'
```

### Explorer le Contenu du Snap

```bash
# Lister le contenu du snap
ls -laR /snap/3nity-media/current/ | head -100

# Trouver des fichiers spécifiques
find /snap/3nity-media/current -name "libmpv*"
find /snap/3nity-media/current -name "*.lang"

# Vérifier l'emplacement du binaire
ls -la /snap/3nity-media/current/usr/bin/
```

### Vérifier les Permissions du Snap

```bash
# Lister les interfaces connectées
snap connections 3nity-media

# Connecter les permissions manquantes
sudo snap connect 3nity-media:audio-playback
sudo snap connect 3nity-media:pulseaudio
sudo snap connect 3nity-media:removable-media
```

### Voir les Logs du Snap

```bash
snap logs 3nity-media
journalctl -f | grep -i 3nity
journalctl -f | grep -i apparmor
```

---

## Débogage Flatpak

### Entrer dans le Shell Flatpak

```bash
flatpak run --command=sh com.github.nicod3v.3nity-media
```

### Vérifier l'Environnement dans Flatpak

```bash
flatpak run --command=sh com.github.nicod3v.3nity-media -c 'echo $LD_LIBRARY_PATH'
flatpak run --command=sh com.github.nicod3v.3nity-media -c 'ls -la /app/lib/'
```

### Déboguer le Chargement des Bibliothèques

```bash
flatpak run --command=sh com.github.nicod3v.3nity-media -c 'ldd /app/bin/3nity-media | grep "not found"'
flatpak run --command=sh com.github.nicod3v.3nity-media -c 'LD_DEBUG=libs /app/bin/3nity-media 2>&1 | head -50'
```

### Exécuter avec Sortie Verbeuse

```bash
flatpak run --verbose com.github.nicod3v.3nity-media
```

### Vérifier les Permissions Flatpak

```bash
# Accorder l'accès au système de fichiers
flatpak override --user --filesystem=/media com.github.nicod3v.3nity-media
flatpak override --user --filesystem=/run/media com.github.nicod3v.3nity-media

# Afficher les overrides actuels
flatpak override --user --show com.github.nicod3v.3nity-media
```

---

## Débogage AppImage

### Extraire et Exécuter

```bash
# Extraire le contenu de l'AppImage
./3nity-Media-*.AppImage --appimage-extract

# Exécuter la version extraite
./squashfs-root/AppRun

# Ou avec débogage
LD_DEBUG=libs ./squashfs-root/AppRun 2>&1 | head -50
```

### Vérifier le Contenu de l'AppImage

```bash
./3nity-Media-*.AppImage --appimage-extract
ls -laR squashfs-root/ | head -100
find squashfs-root -name "libmpv*"
```

### Exécuter Sans FUSE

```bash
./3nity-Media-*.AppImage --appimage-extract-and-run
```

---

## Variables d'Environnement

### Débogage du Chargement des Bibliothèques

| Variable | Description | Exemple |
|----------|-------------|---------|
| `LD_DEBUG=libs` | Affiche la recherche/chargement des bibliothèques | `LD_DEBUG=libs ./3nity-media 2>&1` |
| `LD_DEBUG=files` | Affiche les opérations sur les fichiers | `LD_DEBUG=files ./3nity-media 2>&1` |
| `LD_DEBUG=all` | Sortie de débogage complète | `LD_DEBUG=all ./3nity-media 2>&1` |
| `LD_LIBRARY_PATH` | Chemins de recherche supplémentaires | `LD_LIBRARY_PATH=/opt/lib ./3nity-media` |

### Débogage Qt

| Variable | Description | Exemple |
|----------|-------------|---------|
| `QT_DEBUG_PLUGINS=1` | Débogue le chargement des plugins Qt | `QT_DEBUG_PLUGINS=1 ./3nity-media` |
| `QT_PLUGIN_PATH` | Emplacement des plugins Qt | `echo $QT_PLUGIN_PATH` |
| `QT_QPA_PLATFORM` | Forcer la plateforme Qt | `QT_QPA_PLATFORM=xcb ./3nity-media` |

### Forcer la Langue

```bash
LANG=fr_FR.UTF-8 ./3nity-media
LANG=en_US.UTF-8 ./3nity-media
```

---

## Erreurs Courantes

### "Failed to load libmpv library"

**Cause :** libmpv.so.2 ou ses dépendances non trouvées.

**Débogage :**
```bash
# Vérifier si libmpv existe
ldconfig -p | grep mpv

# Vérifier les dépendances
ldd /usr/lib/x86_64-linux-gnu/libmpv.so.2 | grep "not found"
```

**Solution :**
```bash
# Debian/Ubuntu
sudo apt install libmpv2

# Ou libmpv1 sur les systèmes plus anciens
sudo apt install libmpv1
```

### "Could not find Qt platform plugin xcb"

**Cause :** Les plugins de plateforme Qt ne sont pas dans le chemin de recherche.

**Débogage :**
```bash
QT_DEBUG_PLUGINS=1 ./3nity-media 2>&1 | grep -i platform
echo $QT_QPA_PLATFORM_PLUGIN_PATH
```

**Solution (pour Snap) :** Ajouter dans snapcraft.yaml :
```yaml
environment:
  QT_PLUGIN_PATH: $SNAP/usr/lib/x86_64-linux-gnu/qt5/plugins
  QT_QPA_PLATFORM_PLUGIN_PATH: $SNAP/usr/lib/x86_64-linux-gnu/qt5/plugins/platforms
```

### "libQt5Pas.so.1: cannot open shared object file"

**Cause :** Les bindings Qt5 Pascal non trouvés.

**Débogage :**
```bash
ldconfig -p | grep qt5pas
ldd /usr/bin/3nity-media | grep -i qt5pas
```

**Solution :**
```bash
sudo apt install libqt5pas1
```

### Snap : Dépendances Manquantes

**Débogage :**
```bash
snap run --shell 3nity-media -c 'ldd /snap/3nity-media/current/usr/lib/x86_64-linux-gnu/libmpv.so.2 | grep "not found"'
```

**Solution :** Ajouter les paquets manquants dans `stage-packages` du snapcraft.yaml.

### Pas de Sortie Audio

**Débogage :**
```bash
# Vérifier PulseAudio
pactl info

# Vérifier les périphériques ALSA
aplay -l

# Vérifier les permissions audio du snap
snap connections 3nity-media | grep audio
```

**Solution :**
```bash
sudo snap connect 3nity-media:audio-playback
sudo snap connect 3nity-media:pulseaudio
```

---

## Signaler des Problèmes

Lors du signalement de bugs, inclure :

1. **Version :** `3nity-media --version`
2. **Méthode d'installation :** DEB, Snap, Flatpak, AppImage, ou source
3. **Distribution :** `cat /etc/os-release`
4. **Message d'erreur :** Sortie complète du terminal
5. **Vérification des bibliothèques :** `ldd /chemin/vers/3nity-media | grep "not found"`

Signaler les problèmes sur : https://github.com/NDXDeveloper/3nity-media/issues

---

## Voir Aussi

- [Guide d'Installation](INSTALL.md)
- [Guide Développeur des Chemins de Locale](LOCALE_PATHS_DEVELOPER.md)
- [Guide de Configuration](CONFIG.md)
