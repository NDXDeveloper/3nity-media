# Workflows GitHub Actions

Ce document décrit les workflows CI/CD de 3nity Media.

## Vue d'ensemble

| Workflow | Déclencheur | Objectif |
|----------|-------------|----------|
| `build.yml` | Push, PR, Tags | Pipeline de build principal |
| `check-linux-deps.yml` | Hebdomadaire, Manuel | Vérifier les dépendances Linux |
| `check-windows-deps.yml` | Hebdomadaire, Manuel | Vérifier les dépendances Windows |

---

## build.yml - Pipeline de Build Principal

Le workflow CI/CD principal qui compile, teste et package 3nity Media.

### Déclencheurs

| Événement | Condition |
|-----------|-----------|
| Push | Branches `main`, `develop` |
| Tags | `v*` (ex: `v1.0.0`) |
| Pull Request | Vers la branche `main` |

### Jobs

1. **Preflight Check (Linux)** - Vérifier la disponibilité des packages APT
2. **Preflight Check (Windows)** - Vérifier les dépendances externes
3. **Build Linux** - Compiler avec Lazarus/FPC pour Linux x86_64
4. **Build Windows** - Cross-compilation pour Windows x86_64
5. **Package Linux** - Créer les packages de distribution
6. **Package Windows** - Créer l'installateur/portable Windows
7. **Release** - Publier la release lors d'un push de tag

### Artefacts Générés

| Plateforme | Artefacts |
|------------|-----------|
| Linux | AppImage, DEB, Snap, tar.gz |
| Windows | ZIP portable, Installateur (NSIS) |

### Variables d'Environnement

| Variable | Description |
|----------|-------------|
| `APP_NAME` | Nom de l'application (`3nity-media`) |
| `BINARY_NAME` | Nom de l'exécutable (`3nity-media`) |

### Secrets Requis

| Secret | Utilisation | Requis pour |
|--------|-------------|-------------|
| `SNAPCRAFT_STORE_CREDENTIALS` | Authentification Snap Store | Publication Snap |

---

## check-linux-deps.yml - Vérification Dépendances Linux

Surveille la disponibilité des dépendances Linux dans les dépôts APT.

### Déclencheurs

- **Planifié** : Chaque lundi à 6h00 UTC
- **Manuel** : Via workflow_dispatch

### Dépendances Vérifiées

| Package | Utilisation |
|---------|-------------|
| `libmpv1` / `libmpv2` | Moteur de lecture média |
| `libqt5pas1` | Bindings Qt5 pour Pascal |
| `ffmpeg` | Encodage/décodage média |
| `lazarus-ide-qt5` | IDE Lazarus avec Qt5 |

### Runners

- Vérifications Bash sur `ubuntu-latest`
- Vérifications PowerShell sur `ubuntu-latest`

---

## check-windows-deps.yml - Vérification Dépendances Windows

Surveille la disponibilité des dépendances Windows depuis les sources externes.

### Déclencheurs

- **Planifié** : Chaque lundi à 6h00 UTC
- **Manuel** : Via workflow_dispatch

### Dépendances Vérifiées

| Dépendance | Source |
|------------|--------|
| libmpv | SourceForge (mpv-player-windows) |
| ffmpeg | GitHub (BtbN/FFmpeg-Builds) |

### Runners

- Vérifications Bash sur `ubuntu-latest`
- Vérifications PowerShell sur `ubuntu-latest` et `windows-latest`

---

## Déclenchement Manuel

Pour déclencher manuellement un workflow :

### Via l'interface GitHub

1. Aller dans l'onglet **Actions** sur GitHub
2. Sélectionner le workflow
3. Cliquer sur **Run workflow**
4. Sélectionner la branche et cliquer sur **Run workflow**

### Via GitHub CLI

```bash
# Déclencher le build
gh workflow run build.yml

# Déclencher la vérification des dépendances
gh workflow run check-linux-deps.yml
gh workflow run check-windows-deps.yml
```

---

## Badges de Statut

Ajoutez ces badges à votre README.md :

```markdown
![Build](https://github.com/NDXDeveloper/3nity-media/actions/workflows/build.yml/badge.svg)
![Linux Deps](https://github.com/NDXDeveloper/3nity-media/actions/workflows/check-linux-deps.yml/badge.svg)
![Windows Deps](https://github.com/NDXDeveloper/3nity-media/actions/workflows/check-windows-deps.yml/badge.svg)
```

---

## Résolution de Problèmes

### Le build échoue avec "libmpv not found"

Le preflight check devrait détecter ce problème. S'il passe mais que le build échoue :
- Vérifier si le nom du package APT a changé
- Vérifier la compatibilité avec la version d'Ubuntu

### La cross-compilation Windows échoue

- S'assurer que `lazbuild` supporte la cible de cross-compilation
- Vérifier les packages du cross-compilateur FPC

### Le build Snap échoue

- Vérifier la syntaxe de `snapcraft.yaml`
- Vérifier la compatibilité de l'image de base (core22)

### La release n'est pas créée

- S'assurer que le tag suit le pattern `v*` (ex: `v1.0.0`)
- Vérifier que la permission `contents: write` est définie

---

## Structure des Fichiers

```
.github/
└── workflows/
    ├── README.md              # Documentation (anglais)
    ├── build.yml              # Pipeline principal (72 KB)
    ├── check-linux-deps.yml   # Vérification Linux (15 KB)
    └── check-windows-deps.yml # Vérification Windows (11 KB)
```

---

## Voir Aussi

- [Documentation Snapcraft](SNAPCRAFT.md) - Publication sur le Snap Store
