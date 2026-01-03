# 3nity Media - Documentation Développeur: Système de Localisation

## Architecture du Système

### Vue d'ensemble

Le système de localisation utilise des fichiers `.lang` au format INI pour stocker les traductions. Le gestionnaire principal est `TLocaleManager` dans `uLocale.pas`.

```
bin/x86_64-linux/
└── locale/
    ├── en.lang    (Anglais - langue par défaut)
    ├── fr.lang    (Français)
    └── [xx].lang  (Autres langues)
```

### Détection Automatique de la Langue

Au démarrage, l'application détecte automatiquement la langue du système via les variables d'environnement:

1. `LC_ALL`
2. `LC_MESSAGES`
3. `LANG`

Le code langue est extrait (ex: `fr_FR.UTF-8` → `fr`) et le fichier correspondant est chargé s'il existe.

---

## Structure des Fichiers .lang

### Format

Les fichiers utilisent le format INI standard:

```ini
[Section]
Clé=Valeur traduite
; Ceci est un commentaire
```

### Section Obligatoire: [Language]

Chaque fichier doit commencer par cette section:

```ini
[Language]
Name=Français
Code=fr
Author=Nicolas DEOUX
```

| Clé | Description |
|-----|-------------|
| `Name` | Nom de la langue (affiché dans les options) |
| `Code` | Code ISO 639-1 (2 lettres: en, fr, de, es...) |
| `Author` | Auteur de la traduction |

### Sections Disponibles

| Section | Description | Méthode Raccourci |
|---------|-------------|-------------------|
| `[Menu]` | Éléments de menu | `Locale.Menu()` |
| `[Dialog]` | Titres et textes des dialogues | `Locale.Dialog()` |
| `[Status]` | Messages de la barre d'état | `Locale.Status()` |
| `[Message]` | Messages à l'utilisateur | `Locale.Message()` |
| `[Button]` | Libellés des boutons | `Locale.Button()` |
| `[Tooltip]` | Infobulles | `Locale.Tooltip()` |
| `[Label]` | Libellés généraux | `Locale.Label_()` |
| `[Playlist]` | Fenêtre playlist | `Locale.GetString()` |
| `[Equalizer]` | Fenêtre égaliseur | `Locale.GetString()` |
| `[Options]` | Fenêtre options | `Locale.GetString()` |
| `[Shortcuts]` | Éditeur de raccourcis | `Locale.GetString()` |
| `[Radios]` | Fenêtre radios | `Locale.GetString()` |
| `[About]` | Fenêtre À propos | `Locale.GetString()` |
| `[MediaInfo]` | Informations média | `Locale.GetString()` |
| `[Bookmarks]` | Signets | `Locale.GetString()` |
| `[Favorites]` | Favoris | `Locale.GetString()` |
| `[History]` | Historique | `Locale.GetString()` |
| `[SleepTimer]` | Minuterie de veille | `Locale.GetString()` |
| `[GotoTime]` | Aller à un temps | `Locale.GetString()` |
| `[Log]` | Fenêtre de log | `Locale.GetString()` |

---

## Utilisation dans le Code

### Instance Globale

```pascal
uses uLocale;

// Accès via la fonction globale
Locale.GetString('Section', 'Clé', 'Valeur par défaut');

// Ou via la fonction raccourci
_T('Section', 'Clé', 'Valeur par défaut');
```

### Méthodes Raccourcis

```pascal
// Pour les menus
mnuFile.Caption := Locale.Menu('File', '&File');

// Pour les dialogues
ShowMessage(Locale.Dialog('ConfirmDelete', 'Delete this item?'));

// Pour les messages de statut
StatusBar.SimpleText := Locale.Status('Ready', 'Ready');

// Pour les boutons
btnOK.Caption := Locale.Button('OK', 'OK');

// Pour les infobulles
btnPlay.Hint := Locale.Tooltip('Play', 'Play media');
```

### Méthode Générique

```pascal
// Pour toute section
Label1.Caption := Locale.GetString('Options', 'Language', 'Language:');

// Pour les sections spécifiques
edtSearch.TextHint := Locale.GetString('Playlist', 'SearchHint', 'Search...');
```

---

## Conventions de Nommage

### Clés de Menu

Format: `[Parent][Action]`

```ini
[Menu]
File=&Fichier
FileOpen=&Ouvrir un fichier...
FileOpenURL=Ouvrir une &URL...
FileRecent=Fichiers &récents
FileExit=&Quitter

Playback=&Lecture
PlaybackPlayPause=&Lecture/Pause
PlaybackStop=&Arrêter
```

### Clés d'Actions (Shortcuts)

Format: `Act[NomAction]`

```ini
[Shortcuts]
ActPlayPause=Lecture / Pause
ActStop=Arrêter
ActMute=Muet / Son
ActFullscreen=Plein écran
```

### Raccourcis Clavier dans les Menus

Utilisez `&` pour indiquer la lettre du raccourci Alt:

```ini
FileOpen=&Ouvrir un fichier...   ; Alt+O
FileExit=&Quitter                ; Alt+Q
```

### Variables de Format

Utilisez `%s`, `%d`, `%f` pour les valeurs dynamiques:

```ini
[Message]
FilesFound=%d fichiers trouvés
CurrentVolume=Volume: %d%%
PlaybackSpeed=Vitesse: %.2fx
WelcomeUser=Bienvenue, %s!
```

---

## Ajouter une Nouvelle Langue

### 1. Créer le Fichier

Copiez `en.lang` vers `[code].lang`:

```bash
cp locale/en.lang locale/de.lang
```

### 2. Modifier l'En-tête

```ini
[Language]
Name=Deutsch
Code=de
Author=Votre Nom
```

### 3. Traduire les Sections

Traduisez chaque valeur tout en conservant:
- Les clés identiques
- Les `&` pour les raccourcis Alt
- Les `%s`, `%d`, `%f` pour le formatage
- L'ordre des paramètres

### 4. Tester

L'application détectera automatiquement le nouveau fichier au démarrage.

---

## Ajouter une Nouvelle Chaîne Traduisible

### 1. Ajouter aux Fichiers de Langue

**en.lang:**
```ini
[Section]
NewKey=English text
```

**fr.lang:**
```ini
[Section]
NewKey=Texte français
```

### 2. Utiliser dans le Code

```pascal
procedure TMyForm.UpdateLocale;
begin
  lblNew.Caption := Locale.GetString('Section', 'NewKey', 'Default text');
end;
```

### 3. Appeler lors du Changement de Langue

Dans la procédure `UpdateLocale` du formulaire:

```pascal
procedure TfrmMain.UpdateLocale;
begin
  // ... autres traductions ...
  lblNew.Caption := Locale.GetString('Section', 'NewKey', 'Default text');
end;
```

---

## Bonnes Pratiques

### 1. Toujours Fournir une Valeur par Défaut

```pascal
// BON
Locale.Menu('FileOpen', '&Open File...');

// MAUVAIS - pas de fallback
Locale.Menu('FileOpen', '');
```

### 2. Garder les Clés en Anglais

```ini
; BON
FileOpen=Ouvrir un fichier...

; MAUVAIS
OuvrirFichier=Ouvrir un fichier...
```

### 3. Utiliser des Commentaires

```ini
[Menu]
; File menu
File=&Fichier
FileOpen=&Ouvrir un fichier...

; Playback menu
Playback=&Lecture
```

### 4. Grouper les Traductions Liées

```ini
[Playlist]
; Toolbar
AddFiles=Ajouter des fichiers
RemoveSelected=Supprimer la sélection
Clear=Effacer tout

; Context menu
PlayNow=Lire maintenant
RemoveFromList=Retirer de la liste
```

### 5. Vérifier la Cohérence

Assurez-vous que toutes les langues ont les mêmes clés:

```bash
# Comparer les clés entre deux fichiers
diff <(grep -E '^[A-Za-z]' en.lang | cut -d= -f1 | sort) \
     <(grep -E '^[A-Za-z]' fr.lang | cut -d= -f1 | sort)
```

---

## Structure de TLocaleManager

```pascal
TLocaleManager = class
private
  FCurrentLang: string;           // Code langue actuel ('fr', 'en'...)
  FLangFile: TIniFile;            // Fichier INI chargé
  FLangPath: string;              // Chemin vers le dossier locale/
  FAvailableLanguages: TStringList; // Liste des codes disponibles
  FLanguageNames: TStringList;    // Liste des noms de langues

public
  function LoadLanguage(const LangCode: string): Boolean;
  function GetString(const Section, Key, Default: string): string;

  // Raccourcis
  function Menu(const Key, Default: string): string;
  function Dialog(const Key, Default: string): string;
  function Status(const Key, Default: string): string;
  function Button(const Key, Default: string): string;
  function Label_(const Key, Default: string): string;
  function Message(const Key, Default: string): string;
  function Tooltip(const Key, Default: string): string;

  property CurrentLanguage: string;
  property AvailableLanguages: TStringList;
  property LanguageNames: TStringList;
end;
```

---

## Emplacements des Fichiers de Langue

L'application recherche les fichiers dans cet ordre:

1. `[ExePath]/locale/`
2. `[ExePath]/lang/`
3. `[ExePath]/../share/3nity/locale/`

Pour le développement, placez les fichiers dans:
```
bin/x86_64-linux/locale/
```

---

## Débogage

### Vérifier la Langue Chargée

```pascal
WriteLn('Current language: ', Locale.CurrentLanguage);
WriteLn('Lang path: ', Locale.LangPath);
```

### Lister les Langues Disponibles

```pascal
var
  I: Integer;
begin
  for I := 0 to Locale.AvailableLanguages.Count - 1 do
    WriteLn(Locale.AvailableLanguages[I], ' = ', Locale.LanguageNames[I]);
end;
```

### Tester une Traduction

```pascal
WriteLn(Locale.GetString('Menu', 'FileOpen', 'NOT FOUND'));
```
