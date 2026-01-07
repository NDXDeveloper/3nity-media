# Documentation Développeur CI/CD

Ce document décrit le pipeline CI/CD GitHub Actions pour 3nity Media, incluant la vérification des dépendances, le processus de build et l'automatisation des releases.

## Fichiers Workflow

| Fichier | Objectif | Déclencheur |
|---------|----------|-------------|
| `build.yml` | Pipeline de build principal | Push sur main/develop, tags, PRs |
| `check-linux-deps.yml` | Vérification santé dépendances Linux | Hebdomadaire (Lundi 6:00 UTC), manuel |
| `check-windows-deps.yml` | Vérification santé dépendances Windows | Hebdomadaire (Lundi 6:00 UTC), manuel |

## Structure du Pipeline de Build

```
preflight-check-linux        preflight-check-windows
         │                            │
         ▼                            │
    build-linux ◄─────────────────────┤
         │                            │
         │                            ▼
         └──────────────────► build-windows
                                     │
                   ┌─────────────────┘
                   ▼
            create-release (tags uniquement)
```

### Dépendances des Jobs

| Job | Dépend de | Description |
|-----|-----------|-------------|
| `preflight-check-linux` | - | Vérifie la disponibilité des paquets APT Linux |
| `preflight-check-windows` | - | Vérifie les URLs de téléchargement Windows |
| `build-linux` | `preflight-check-linux` | Compile les binaires et paquets Linux |
| `build-windows` | `preflight-check-windows` + `build-linux` | Compile les binaires et installeurs Windows |
| `create-release` | `build-linux` + `build-windows` | Crée la release GitHub (tags uniquement) |

### Comportement

- Les deux preflight checks s'exécutent **en parallèle**
- Si le preflight Linux échoue → build Linux bloqué, mais le preflight Windows continue
- Si le preflight Windows échoue → build Windows bloqué, mais le build Linux peut réussir
- `build-windows` attend `build-linux` pour récupérer le numéro de version

## Dépendances Vérifiées

### Dépendances Linux (Paquets APT)

| Paquet | Objectif | Requis |
|--------|----------|--------|
| `libmpv2` ou `libmpv1` | Bibliothèque de lecture MPV | Oui |
| `libqt5pas1` | Bindings Pascal Qt5 | Oui |
| `ffmpeg` | Framework multimédia (inclut ffprobe) | Oui |
| `lazarus-ide-qt5` | IDE Lazarus avec widgetset Qt5 | Oui (build uniquement) |
| `libqt5pas-dev` | Fichiers de développement Qt5 Pascal | Oui (build uniquement) |
| `libmpv-dev` | Fichiers de développement MPV | Oui (build uniquement) |

### Dépendances Windows (URLs Externes)

| Composant | Source | Modèle d'URL |
|-----------|--------|--------------|
| libmpv-2.dll | SourceForge | `https://sourceforge.net/projects/mpv-player-windows/files/libmpv/mpv-dev-x86_64-AAAAMMJJ-git-HASH.7z/download` |
| ffprobe.exe | gyan.dev | `https://www.gyan.dev/ffmpeg/builds/ffmpeg-release-essentials.zip` |

#### Stratégie de Téléchargement libmpv

Le build Windows découvre dynamiquement la dernière version de libmpv :

1. Scrape le listing du répertoire SourceForge
2. Trouve le dernier fichier `mpv-dev-x86_64-AAAAMMJJ-git-HASH.7z`
3. Se rabat sur une version connue fonctionnelle si le scraping échoue

```powershell
# Pattern utilisé pour trouver la dernière version
$pattern = 'mpv-dev-x86_64-\d{8}-git-[a-f0-9]+\.7z'
```

#### URL de Secours

Si la découverte dynamique échoue :
```
https://sourceforge.net/projects/mpv-player-windows/files/libmpv/mpv-dev-x86_64-20251228-git-a58dd8a.7z/download
```

## Sorties de Build

### Paquets Linux

| Format | Modèle de fichier | Cible |
|--------|-------------------|-------|
| DEB | `3nity-media_VERSION_amd64.deb` | Debian, Ubuntu, Mint |
| AppImage | `3nity-Media-VERSION-x86_64.AppImage` | Portable universel |
| Snap | `3nity-media_VERSION_amd64.snap` | Snap Store |
| Portable | `3nity-media-linux-portable-VERSION.tar.gz` | Linux générique |

### Paquets Windows

| Format | Modèle de fichier | Cible |
|--------|-------------------|-------|
| Installeur (Admin) | `3nity-Media-Setup-VERSION.exe` | Installation système |
| Installeur (User) | `3nity-Media-Setup-User-VERSION.exe` | Installation utilisateur (sans admin) |
| Portable | `3nity-media-windows-portable-VERSION.zip` | Aucune installation requise |

### Contenu des Paquets Windows

Tous les paquets Windows incluent :
- `3nity.exe` - Application principale
- `libmpv-2.dll` - Bibliothèque MPV
- `ffprobe.exe` - Outil d'extraction de métadonnées
- `README.md` - Documentation

## Workflows de Vérification de Santé

### check-linux-deps.yml

S'exécute chaque semaine pour vérifier :

**Vérifications Bash :**
- Disponibilité des paquets APT (libmpv, libqt5pas1, ffmpeg)
- Test d'installation réelle
- URLs sources (FFmpeg, MPV)

**Vérifications PowerShell :**
- Mêmes paquets APT via pwsh
- URLs sources
- Outils AppImage (linuxdeploy, appimagetool)

### check-windows-deps.yml

S'exécute chaque semaine pour vérifier :

**Vérifications Bash :**
- Scraping du répertoire SourceForge libmpv
- Accessibilité des URLs de téléchargement
- URL ffprobe gyan.dev

**Vérifications PowerShell :**
- Mêmes vérifications d'URLs
- Test de téléchargement et extraction réels
- Vérification des fichiers (libmpv-2.dll, ffprobe.exe existent)

### Notification d'Échec

Si une vérification de santé échoue :
1. Une Issue GitHub est automatiquement créée
2. Labels : `dependencies`, `automated`, `linux`/`windows`
3. Les échecs suivants ajoutent des commentaires à l'issue existante

## URLs Vérifiées

### Actuellement Vérifiées (à la date du build)

| URL | Statut | Objectif |
|-----|--------|----------|
| `https://sourceforge.net/projects/mpv-player-windows/files/libmpv/` | Actif | Répertoire libmpv |
| `https://www.gyan.dev/ffmpeg/builds/ffmpeg-release-essentials.zip` | Actif | FFmpeg/ffprobe |
| `https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage` | Actif | Constructeur AppImage |
| `https://github.com/linuxdeploy/linuxdeploy-plugin-qt/releases/download/continuous/linuxdeploy-plugin-qt-x86_64.AppImage` | Actif | Plugin Qt pour AppImage |
| `https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage` | Actif | Outil AppImage |

## Déclenchement Manuel des Workflows

Tous les workflows supportent le déclenchement manuel via `workflow_dispatch` :

```bash
# Déclencher via GitHub CLI
gh workflow run build.yml
gh workflow run check-linux-deps.yml
gh workflow run check-windows-deps.yml
```

Ou via l'interface GitHub : Actions → Sélectionner le workflow → Run workflow

## Dépannage

### Échecs du Preflight Check

**Paquet APT Linux non trouvé :**
- Vérifier le statut du dépôt Ubuntu
- Vérifier que le nom du paquet n'a pas changé
- Vérifier si le paquet a été déplacé vers universe/multiverse

**libmpv SourceForge non trouvé :**
- Vérifier si le projet mpv-player-windows est toujours actif
- Vérifier que le pattern de nommage des fichiers n'a pas changé
- Essayer l'URL de secours manuellement

**gyan.dev injoignable :**
- Vérifier si le site est temporairement indisponible
- Chercher des builds Windows FFmpeg alternatifs
- Considérer les builds BtbN comme alternative : `https://github.com/BtbN/FFmpeg-Builds/releases`

### Échecs de Build Après Succès du Preflight

Si le build échoue malgré le succès du preflight :
- Le preflight vérifie uniquement la disponibilité, pas l'intégrité du contenu
- Le téléchargement a pu être corrompu
- La structure de l'archive a pu changer
- Relancer le workflow ou vérifier le workflow de vérification complète

## Ajouter de Nouvelles Dépendances

1. Ajouter la vérification d'URL au job preflight approprié dans `build.yml`
2. Ajouter la vérification complète à `check-linux-deps.yml` ou `check-windows-deps.yml`
3. Mettre à jour cette documentation
4. Tester avec un déclenchement manuel du workflow

## Informations de Version

- **Dernière mise à jour :** 2026-01-01
- **Version du workflow :** Compatible avec les runners GitHub Actions ubuntu-latest, windows-latest
- **Prérequis PowerShell :** 7.x (pré-installé sur les runners GitHub)
