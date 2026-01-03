# Procédure de publication sur Flathub

Ce guide détaille les étapes pour publier 3nity Media sur Flathub.

---

## Phase 1: Préparation (dans votre repo)

### 1. Ajouter les screenshots

```bash
mkdir -p screenshots
# Ajouter les captures d'écran:
# - screenshots/main-window.png (fenêtre principale)
# - screenshots/playlist.png (playlist)
# - screenshots/equalizer.png (égaliseur)
```

**Dimensions recommandées:** 1920x1080 ou 1280x720 (format 16:9)

### 2. Mettre à jour le sha256 de libqt5pas

```bash
# Télécharger et calculer le hash
wget https://downloads.sourceforge.net/project/lazarus/Lazarus%20Releases/2.2.6/qt5pas-2.6-1.tar.gz
sha256sum qt5pas-2.6-1.tar.gz
# Mettre à jour dans flatpak/com.ndxdev.3nity-media.yml
```

### 3. Tester le build localement

```bash
cd flatpak
flatpak-builder --user --install-deps-from=flathub --force-clean build com.ndxdev.3nity-media.yml

# Tester l'app
flatpak-builder --run build com.ndxdev.3nity-media.yml 3nity-media
```

### 4. Valider les métadonnées AppStream

```bash
appstream-util validate com.ndxdev.3nity-media.metainfo.xml
```

---

## Phase 2: Soumission Flathub

### 5. Créer une issue de soumission

- Aller sur: https://github.com/flathub/flathub/issues/new?template=submission.yml
- Remplir le formulaire:

| Champ | Valeur |
|-------|--------|
| **App ID** | `com.ndxdev.3nity-media` |
| **App name** | `3nity Media` |
| **Summary** | `Modern multimedia player powered by libmpv` |
| **Homepage** | `https://github.com/NDXDeveloper/3nity-media` |
| **License** | `GPL-2.0` |

- Cocher les cases requises (pas de trademark issues, etc.)

### 6. Attendre l'approbation

- L'équipe Flathub review la demande (quelques jours)
- Ils peuvent demander des modifications

---

## Phase 3: Après approbation

### 7. Flathub crée le repo

- Un repo `flathub/com.ndxdev.3nity-media` est créé
- Vous recevez les droits de push

### 8. Pousser les fichiers

```bash
git clone git@github.com:flathub/com.ndxdev.3nity-media.git
cd com.ndxdev.3nity-media

# Copier les fichiers (structure Flathub = flat, pas de sous-dossier)
cp /path/to/votre-repo/flatpak/com.ndxdev.3nity-media.yml .
cp /path/to/votre-repo/flatpak/com.ndxdev.3nity-media.metainfo.xml .
# Note: le .desktop est référencé depuis votre repo source

git add .
git commit -m "Initial Flathub submission"
git push
```

### 9. Le build automatique démarre

- Flathub build le package automatiquement
- Vérifier sur: https://flathub.org/builds

---

## Phase 4: Maintenance

Pour chaque nouvelle version:

1. Mettre à jour `metainfo.xml` (ajouter un `<release>`)
2. Mettre à jour les sources dans le manifest si nécessaire
3. Push sur le repo Flathub
4. Le build se déclenche automatiquement

---

## Structure finale sur flathub/com.ndxdev.3nity-media

```
com.ndxdev.3nity-media/
├── com.ndxdev.3nity-media.yml          # Manifest
├── com.ndxdev.3nity-media.metainfo.xml # Métadonnées
└── flathub.json                        # (optionnel) config Flathub
```

> **Note:** Le fichier `.desktop` et les icônes sont référencés depuis votre repo source dans le manifest.

---

## Fichiers locaux

Les fichiers Flatpak sont dans le dossier `flatpak/` du projet:

| Fichier | Description |
|---------|-------------|
| `com.ndxdev.3nity-media.yml` | Manifest Flatpak |
| `com.ndxdev.3nity-media.desktop` | Fichier desktop |
| `com.ndxdev.3nity-media.metainfo.xml` | Métadonnées AppStream |

---

## Liens utiles

- [Documentation Flathub](https://docs.flathub.org/)
- [Soumission d'app](https://github.com/flathub/flathub/issues/new?template=submission.yml)
- [AppStream Documentation](https://www.freedesktop.org/software/appstream/docs/)
- [Flatpak Builder](https://docs.flatpak.org/en/latest/flatpak-builder.html)
