# Contribuer à 3nity Media

Merci de votre intérêt pour contribuer à 3nity Media ! Ce document fournit les directives et instructions pour contribuer au projet.

## Table des Matières

- [Code de Conduite](#code-de-conduite)
- [Premiers Pas](#premiers-pas)
- [Comment Contribuer](#comment-contribuer)
  - [Signaler un Bug](#signaler-un-bug)
  - [Proposer une Fonctionnalité](#proposer-une-fonctionnalité)
  - [Contribuer du Code](#contribuer-du-code)
- [Environnement de Développement](#environnement-de-développement)
- [Guide de Style de Code](#guide-de-style-de-code)
- [Processus de Pull Request](#processus-de-pull-request)
- [Tests](#tests)
- [Documentation](#documentation)
- [Traduction](#traduction)

---

## Code de Conduite

Ce projet suit un code de conduite simple :

- **Soyez respectueux** - Traitez tout le monde avec respect et gentillesse
- **Soyez constructif** - Fournissez des commentaires et suggestions utiles
- **Soyez patient** - Les mainteneurs sont des bénévoles avec un temps limité
- **Soyez inclusif** - Accueillez les nouveaux venus et aidez-les à contribuer

---

## Premiers Pas

1. **Forkez le dépôt** sur GitHub
2. **Clonez votre fork** localement :
   ```bash
   git clone https://github.com/VOTRE-NOM/3nity-media.git
   cd 3nity-media
   ```
3. **Configurez l'environnement de développement** (voir [Environnement de Développement](#environnement-de-développement))
4. **Créez une branche** pour vos modifications :
   ```bash
   git checkout -b feature/nom-de-votre-fonctionnalite
   # ou
   git checkout -b fix/description-du-probleme
   ```

---

## Comment Contribuer

### Signaler un Bug

Avant de signaler un bug, veuillez :

1. **Rechercher les issues existantes** pour éviter les doublons
2. **Tester avec la dernière version** pour vérifier que le bug existe toujours
3. **Rassembler les informations pertinentes** sur votre environnement

**Créez un rapport de bug** en utilisant ce modèle :

```markdown
## Description du Bug
Une description claire et concise du bug.

## Étapes pour Reproduire
1. Aller à '...'
2. Cliquer sur '...'
3. Lire '...'
4. Voir l'erreur

## Comportement Attendu
Ce que vous attendiez qu'il se passe.

## Comportement Réel
Ce qui s'est réellement passé.

## Environnement
- OS : [ex: Ubuntu 24.04, Windows 11]
- Version 3nity Media : [ex: 0.1.0]
- Méthode d'installation : [ex: DEB, AppImage, Portable]
- Environnement de bureau : [ex: GNOME, KDE, XFCE]

## Contexte Additionnel
- Sortie de log pertinente (Options → Voir le Log)
- Captures d'écran si applicable
- Fichier média exemple si le problème est spécifique à un format
```

**Labels de rapport de bug :**
- `bug` - Bug confirmé
- `needs-info` - Plus d'informations requises
- `cannot-reproduce` - Impossible de reproduire le problème
- `duplicate` - Déjà signalé
- `wontfix` - Ne sera pas corrigé (avec explication)

### Proposer une Fonctionnalité

Nous accueillons les suggestions de fonctionnalités ! Avant de proposer :

1. **Consultez la feuille de route** pour les fonctionnalités planifiées
2. **Recherchez les issues existantes** pour éviter les doublons
3. **Considérez la portée** - Est-ce que cela correspond aux objectifs du projet ?

**Créez une demande de fonctionnalité** en utilisant ce modèle :

```markdown
## Description de la Fonctionnalité
Une description claire et concise de la fonctionnalité souhaitée.

## Cas d'Utilisation
Décrivez le problème que cette fonctionnalité résoudrait ou l'avantage qu'elle apporterait.
Exemple : "En tant qu'utilisateur qui écoute des podcasts, j'aimerais..."

## Solution Proposée
Votre approche d'implémentation suggérée (optionnel mais utile).

## Alternatives Considérées
Toutes solutions ou fonctionnalités alternatives que vous avez considérées.

## Contexte Additionnel
- Maquettes ou captures d'écran de fonctionnalités similaires
- Liens vers des fonctionnalités similaires dans d'autres applications
- Impact sur les fonctionnalités existantes
```

**Facteurs de priorité des fonctionnalités :**
- Nombre d'utilisateurs demandant la fonctionnalité
- Complexité d'implémentation
- Alignement avec les objectifs du projet
- Disponibilité des contributeurs

### Contribuer du Code

Nous acceptons les contributions de code pour :

- **Corrections de bugs** - Corrections de fonctionnalités existantes
- **Nouvelles fonctionnalités** - Après discussion dans une issue
- **Améliorations de performance** - Avec benchmarks
- **Documentation** - Corrections et ajouts
- **Traductions** - Nouvelles langues ou améliorations
- **Tests** - Tests unitaires, d'intégration et fonctionnels

---

## Environnement de Développement

### Prérequis

**Linux (Ubuntu/Debian) :**
```bash
sudo apt install \
  lazarus-ide-qt5 \
  lazarus-src \
  lcl-qt5 \
  fpc \
  libqt5pas1 \
  libqt5pas-dev \
  libmpv-dev \
  ffmpeg \
  git \
  make
```

**Windows :**
1. Installez [Lazarus IDE](https://www.lazarus-ide.org/)
2. Installez [Git](https://git-scm.com/)
3. Téléchargez [libmpv](https://sourceforge.net/projects/mpv-player-windows/files/libmpv/)

### Compilation

```bash
# Cloner et entrer dans le répertoire
git clone https://github.com/VOTRE-NOM/3nity-media.git
cd 3nity-media

# Compiler la version debug
make build-app

# Compiler la version release
make build-release

# Lancer l'application
make run

# Nettoyer les artefacts de compilation
make clean-all
```

### Structure du Projet

```
3nity-media/
├── src/                    # Code source
│   ├── Core/              # Fonctionnalités principales (MPV, playlist, radio)
│   ├── Forms/             # Formulaires et dialogues GUI
│   ├── Common/            # Utilitaires et types partagés
│   ├── Locale/            # Système de localisation
│   ├── Controls/          # Contrôles UI personnalisés
│   └── TrinityMedia.lpi   # Fichier projet Lazarus
├── bin/                    # Binaires compilés
├── lib/                    # Unités compilées
├── docs/                   # Documentation
├── tests/                  # Suite de tests
├── lang/                   # Fichiers de traduction
├── resources/              # Icônes et ressources
└── Makefile               # Automatisation de la compilation
```

---

## Guide de Style de Code

### Principes Généraux

- **Lisibilité d'abord** - Le code doit être auto-explicatif
- **Cohérence** - Suivez les patterns existants dans le codebase
- **Simplicité** - Préférez les solutions simples aux solutions astucieuses
- **Documentation** - Documentez la logique complexe et les API publiques

### Conventions de Nommage Pascal

| Élément | Convention | Exemple |
|---------|------------|---------|
| Unités | Préfixe `u` + PascalCase | `uPlaylistManager.pas` |
| Classes | Préfixe `T` + PascalCase | `TPlaylistManager` |
| Interfaces | Préfixe `I` + PascalCase | `IMediaPlayer` |
| Records | Préfixe `T` + PascalCase | `TPlaylistItem` |
| Énumérations | Préfixe `T` + PascalCase | `TPlaybackMode` |
| Valeurs d'énum | Préfixe `pm`, `ss`, etc. | `pmNormal`, `ssVideo` |
| Champs privés | Préfixe `F` + PascalCase | `FCurrentIndex` |
| Propriétés | PascalCase | `CurrentIndex` |
| Méthodes | PascalCase | `GetNextItem` |
| Variables locales | camelCase | `itemCount` |
| Constantes | MAJUSCULES_AVEC_UNDERSCORES | `MAX_PLAYLIST_SIZE` |
| Paramètres | Préfixe `A` + PascalCase | `AFileName` |

### Modèle d'En-tête de Fichier

Chaque unité Pascal doit inclure cet en-tête :

```pascal
{ ═══════════════════════════════════════════════════════════════════════════════
  uNomUnite.pas - Brève Description

  Part of 3nity Media - Lazarus Edition

  Description détaillée de ce que fait cette unité, son but, et toutes notes
  importantes sur son implémentation.

  Author: Votre Nom (votre.email@example.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uNomUnite;

{$mode objfpc}{$H+}

interface
```

### Séparateurs de Section

Utilisez ces séparateurs pour organiser les sections de code :

```pascal
{ ═══════════════════════════════════════════════════════════════════════════
  NOM DE LA SECTION
  ═══════════════════════════════════════════════════════════════════════════ }
```

### Exemples de Code

**Bon :**
```pascal
{ ═══════════════════════════════════════════════════════════════════════════
  NAVIGATION
  ═══════════════════════════════════════════════════════════════════════════ }

function TPlaylistManager.GetNext: Integer;
var
  nextIndex: Integer;
begin
  if FItems = nil then
    Exit(-1);

  case FPlaybackMode of
    pmNormal:
      begin
        nextIndex := FCurrentIndex + 1;
        if nextIndex >= Length(FItems) then
          Result := -1  { Fin de la playlist }
        else
          Result := nextIndex;
      end;
    pmRepeatOne:
      Result := FCurrentIndex;
    pmRepeatAll:
      Result := (FCurrentIndex + 1) mod Length(FItems);
    pmShuffle, pmShuffleRepeat:
      Result := GetShuffleNext;
  end;
end;
```

**Mauvais :**
```pascal
function TPlaylistManager.GetNext: Integer;
begin
  if FItems=nil then exit(-1);
  case FPlaybackMode of pmNormal: begin nextIndex:=FCurrentIndex+1;
  if nextIndex>=Length(FItems) then Result:=-1 else Result:=nextIndex; end;
  // ... condensé, difficile à lire
end;
```

### Commentaires

- Utilisez `{ }` pour les commentaires mono et multi-lignes
- Utilisez `//` avec parcimonie, uniquement pour de brèves notes inline
- Documentez le "pourquoi", pas le "quoi" (le code montre le quoi)

```pascal
{ Calcule l'index shuffle suivant, recommence si tous les éléments ont été joués }
function TPlaylistManager.GetShuffleNext: Integer;
begin
  // Régénérer le shuffle si épuisé
  if FShuffleIndex >= Length(FShuffleOrder) then
    GenerateShuffleOrder;

  Result := FShuffleOrder[FShuffleIndex];
  Inc(FShuffleIndex);
end;
```

### Gestion des Erreurs

```pascal
function TPlaylistManager.LoadFromFile(const AFileName: string): Boolean;
begin
  Result := False;

  if not FileExists(AFileName) then
  begin
    Log('Fichier playlist non trouvé : ' + AFileName);
    Exit;
  end;

  try
    // Charger le contenu du fichier
    Result := ParsePlaylist(AFileName);
  except
    on E: Exception do
    begin
      Log('Erreur lors du chargement de la playlist : ' + E.Message);
      Result := False;
    end;
  end;
end;
```

### Gestion de la Mémoire

- Libérez toujours les objets dans des blocs `finally` ou utilisez `try..finally`
- Utilisez `FreeAndNil()` au lieu de `Free` quand approprié
- Mettez les tableaux à `nil` avec `SetLength(arr, 0)` ou `arr := nil`

```pascal
procedure TPlaylistManager.Clear;
begin
  FItems := nil;  { Tableau dynamique - automatiquement libéré }
  FShuffleOrder := nil;
  FCurrentIndex := -1;
  FModified := False;
  DoChange;
end;
```

---

## Processus de Pull Request

### Avant de Créer une PR

1. **Assurez-vous que votre code compile** sans erreurs ni warnings
2. **Exécutez les tests existants** pour vérifier que rien n'est cassé
3. **Ajoutez des tests** pour les nouvelles fonctionnalités
4. **Mettez à jour la documentation** si nécessaire
5. **Suivez le guide de style de code**

### Créer une Pull Request

1. **Poussez votre branche** vers votre fork :
   ```bash
   git push origin feature/nom-de-votre-fonctionnalite
   ```

2. **Ouvrez une PR** sur GitHub avec ce modèle :

```markdown
## Description
Brève description de ce que fait cette PR.

## Issue Liée
Corrige #123 (ou "En lien avec #123" si ne corrige pas complètement)

## Type de Changement
- [ ] Correction de bug (changement non cassant qui corrige un problème)
- [ ] Nouvelle fonctionnalité (changement non cassant qui ajoute une fonctionnalité)
- [ ] Changement cassant (correction ou fonctionnalité qui casserait une fonctionnalité existante)
- [ ] Mise à jour de documentation
- [ ] Mise à jour de traduction

## Changements Effectués
- Liste des changements spécifiques
- Un autre changement
- ...

## Tests Effectués
- [ ] Testé sur Linux (précisez la distro)
- [ ] Testé sur Windows
- [ ] Tests unitaires ajoutés
- [ ] Suite de tests existante exécutée

## Captures d'Écran (si applicable)
Captures avant/après pour les changements d'UI.

## Checklist
- [ ] Mon code suit le style de code du projet
- [ ] J'ai effectué une auto-revue de mon code
- [ ] J'ai commenté mon code où nécessaire
- [ ] J'ai mis à jour la documentation
- [ ] Mes changements ne génèrent pas de nouveaux warnings
- [ ] J'ai ajouté des tests qui prouvent que ma correction/fonctionnalité fonctionne
- [ ] Les tests unitaires nouveaux et existants passent localement
```

### Processus de Revue de PR

1. **Vérifications automatisées** s'exécutent sur votre PR (build, tests)
2. **Revue du mainteneur** - des retours peuvent être demandés
3. **Répondre aux retours** - effectuez les changements et poussez sur la même branche
4. **Approbation et merge** - une fois approuvée, la PR sera mergée

### Après le Merge

- Supprimez votre branche de fonctionnalité
- Tirez le dernier `main` dans votre dépôt local
- Célébrez votre contribution !

---

## Tests

### Structure des Tests

```
tests/
├── TestRunner.lpi         # Projet de test
├── TestRunner.lpr         # Fichier principal de test
├── Unit/                  # Tests unitaires
│   ├── TestPlaylist.pas
│   ├── TestConfig.pas
│   └── ...
├── Integration/           # Tests d'intégration
├── Functional/            # Tests GUI
└── TestData/             # Fichiers média de test
```

### Exécuter les Tests

```bash
# Compiler et exécuter tous les tests
make test

# Tests rapides (unitaires uniquement)
make quick

# Suites de tests spécifiques
make test-mpv
make test-playlist
make test-config
make test-radio

# Générer un rapport HTML
make report
```

### Écrire des Tests

```pascal
unit TestPlaylist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, uPlaylistManager;

type
  TTestPlaylist = class(TTestCase)
  private
    FPlaylist: TPlaylistManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddItem;
    procedure TestRemoveItem;
    procedure TestShuffleMode;
  end;

implementation

procedure TTestPlaylist.SetUp;
begin
  FPlaylist := TPlaylistManager.Create;
end;

procedure TTestPlaylist.TearDown;
begin
  FPlaylist.Free;
end;

procedure TTestPlaylist.TestAddItem;
begin
  FPlaylist.Add('/chemin/vers/fichier.mp3');
  AssertEquals('Le nombre d''éléments devrait être 1', 1, FPlaylist.Count);
end;
```

---

## Documentation

### Types de Documentation

| Type | Emplacement | But |
|------|-------------|-----|
| Guides utilisateur | `docs/USER_GUIDE_*.md` | Documentation utilisateur final |
| Guides d'installation | `docs/INSTALL_*.md` | Instructions d'installation |
| Docs API | `docs/API_*.md` | Référence développeur |
| Commentaires de code | Fichiers source | Documentation dans le code |

### Style de Documentation

- Utilisez **Markdown** pour tous les fichiers de documentation
- Fournissez des versions **anglaise** (`_en.md`) et **française** (`_fr.md`)
- Incluez des **exemples de code** quand utile
- Utilisez des **tableaux** pour les informations de référence
- Ajoutez une **table des matières** pour les longs documents

---

## Traduction

### Ajouter une Nouvelle Langue

1. Copiez `lang/en.lang` vers `lang/XX.lang` (où XX est le code langue)
2. Traduisez toutes les valeurs (gardez les clés inchangées)
3. Mettez à jour `src/Locale/uLocale.pas` pour inclure la nouvelle langue
4. Testez l'application avec la nouvelle langue

### Format des Fichiers de Traduction

```ini
[Main]
Language=Français
LanguageCode=fr
Author=Nicolas DEOUX

[Menu]
File=Fichier
Open=Ouvrir
OpenURL=Ouvrir une URL
...
```

### Directives de Traduction

- Gardez les traductions **concises** (l'espace UI est limité)
- Utilisez un langage **formel** quand approprié
- Préservez les indicateurs de **raccourcis clavier** (ex: `&Ouvrir` pour Alt+O)
- Testez les traductions **en contexte** pour vérifier qu'elles s'adaptent

---

## Questions ?

- Ouvrez une issue pour les questions liées au projet
- Rejoignez les discussions sur GitHub Discussions
- Contactez le mainteneur par email pour les demandes privées

Merci de contribuer à 3nity Media !
