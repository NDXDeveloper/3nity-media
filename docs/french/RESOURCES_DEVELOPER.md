# Documentation Développeur - Ressources

Ce document décrit le système de ressources embarquées pour 3nity Media, notamment comment les images et autres assets sont intégrés dans le binaire de l'application.

## Vue d'ensemble

3nity Media utilise des **fichiers de ressources Lazarus (.lrs)** pour embarquer les images directement dans l'exécutable. Cela élimine le besoin de fichiers images externes à l'exécution.

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│  resources/     │────▶│     lazres      │────▶│    logo.lrs     │
│  logo.png       │     │    (outil)      │     │  (code Pascal)  │
└─────────────────┘     └─────────────────┘     └─────────────────┘
                                                        │
                                                        ▼
                              ┌─────────────────────────────────────┐
                              │           uAbout.pas                │
                              │      {$I logo.lrs}                  │
                              │      section initialization         │
                              └─────────────────────────────────────┘
                                                        │
                                                        ▼
                              ┌─────────────────────────────────────┐
                              │        Exécutable compilé           │
                              │    (données image embarquées)       │
                              └─────────────────────────────────────┘
```

## Structure des fichiers

### Images sources

| Fichier | Description |
|---------|-------------|
| `resources/logo.png` | Logo de l'application (affiché dans la boîte À propos) |
| `resources/icon.png` | Icône de l'application (source) |
| `resources/icons/*.png` | Icônes multi-résolution (16x16 à 256x256) |

### Ressources générées

| Fichier | Description |
|---------|-------------|
| `src/Forms/logo.lrs` | Ressource logo embarquée (générée depuis logo.png) |
| `src/TrinityMedia.res` | Ressource Windows compilée (icônes, infos version) |

### Fichiers icônes

| Fichier | Description |
|---------|-------------|
| `resources/icons/3nity-media.ico` | Icône Windows (multi-résolution) |
| `src/3nity-media.ico` | Copie pour le projet |

## L'outil lazres

`lazres` est un outil en ligne de commande Lazarus qui convertit des fichiers en code Pascal contenant les données binaires sous forme de tableau constant.

### Syntaxe

```bash
lazres <sortie.lrs> <fichier_entree>=<nom_ressource> [fichiers supplémentaires...]
```

### Exemples

```bash
# Ressource unique
lazres src/Forms/logo.lrs resources/logo.png=logo

# Plusieurs ressources dans un fichier
lazres src/Forms/resources.lrs image1.png=img1 image2.png=img2
```

### Format de sortie

Le fichier `.lrs` généré contient du code Pascal :

```pascal
LazarusResources.Add('logo','PNG',[
  #137'PNG'#13#10#26#10#0#0#0#13'IHDR'#0#0#0#128#0#0#0#128#8#6#0#0#0...
  // (données binaires encodées en littéraux de chaîne Pascal)
]);
```

## Utilisation des ressources embarquées

### Étape 1 : Inclure le fichier .lrs

Dans la section `initialization` de l'unité :

```pascal
unit uAbout;

// ... code de l'unité ...

initialization
  {$I logo.lrs}

end.
```

### Étape 2 : Charger la ressource

Utilisez `TLazarusResourceStream` pour charger les données embarquées :

```pascal
uses
  LResources;

procedure TfrmAbout.LoadLogo;
var
  ResStream: TLazarusResourceStream;
begin
  try
    ResStream := TLazarusResourceStream.Create('logo', nil);
    try
      imgLogo.Picture.LoadFromStream(ResStream);
    finally
      ResStream.Free;
    end;
  except
    // Ressource non trouvée ou erreur de chargement - ignorer
  end;
end;
```

### Alternative : Accès direct à la ressource

```pascal
var
  Res: TLResource;
begin
  Res := LazarusResources.Find('logo');
  if Res <> nil then
  begin
    // Accéder à Res.Value (chaîne avec données binaires)
    // Accéder à Res.ValueType (ex: 'PNG')
  end;
end;
```

## Régénération des ressources

### Quand régénérer

Régénérez les fichiers `.lrs` quand :
- L'image source est modifiée
- Vous voulez changer l'image embarquée
- Le fichier `.lrs` est corrompu ou supprimé

### Commandes

#### Ressource logo

```bash
# Depuis la racine du projet
lazres src/Forms/logo.lrs resources/logo.png=logo
```

#### Vérifier la génération

```bash
# Vérifier que le fichier généré existe et a du contenu
ls -la src/Forms/logo.lrs
head -5 src/Forms/logo.lrs
```

Sortie attendue :
```
LazarusResources.Add('logo','PNG',[
  #137'PNG'#13#10#26#10...
```

### Après régénération

1. Recompiler l'application :
   ```bash
   make build-app
   ```

2. Tester la boîte À propos pour vérifier que l'image s'affiche correctement

## Formats d'image supportés

L'outil `lazres` supporte tout type de fichier, mais les formats d'image courants sont :

| Format | Extension | Notes |
|--------|-----------|-------|
| PNG | `.png` | Recommandé (supporte la transparence) |
| JPEG | `.jpg`, `.jpeg` | Bon pour les photos |
| BMP | `.bmp` | Taille de fichier importante |
| GIF | `.gif` | Couleurs limitées |
| ICO | `.ico` | Icônes Windows |

## Bonnes pratiques

### Optimisation des images

1. **Optimiser les PNG** avant de les embarquer :
   ```bash
   optipng -o7 resources/logo.png
   # ou
   pngcrush -brute resources/logo.png resources/logo_optimized.png
   ```

2. **Garder les images petites** - les ressources embarquées augmentent la taille de l'exécutable

3. **Utiliser une résolution appropriée** - 128x128 ou 256x256 est généralement suffisant pour les logos

### Nommage des ressources

1. Utiliser des noms en **minuscules**
2. Utiliser des noms **descriptifs** mais courts : `logo`, `icon`, `splash`
3. Éviter les espaces et caractères spéciaux

### Organisation des fichiers

```
projet/
├── resources/              # Images sources (modifiables)
│   ├── logo.png
│   ├── icon.png
│   └── icons/
│       ├── 16x16.png
│       ├── 32x32.png
│       └── ...
└── src/
    └── Forms/
        └── logo.lrs       # Généré (ne pas modifier manuellement)
```

## Dépannage

### Erreur "Resource not found"

1. Vérifier que le fichier `.lrs` est inclus :
   ```pascal
   initialization
     {$I logo.lrs}  // Vérifier que cette ligne existe
   ```

2. Vérifier que le nom de la ressource correspond :
   ```pascal
   // Dans la génération .lrs :
   lazres ... logo.png=logo  // Ressource nommée 'logo'

   // Dans le code :
   TLazarusResourceStream.Create('logo', nil);  // Doit correspondre
   ```

3. S'assurer que l'unité avec `{$I logo.lrs}` est dans la clause uses

### L'image ne s'affiche pas

1. Vérifier que le format d'image est supporté par le composant cible
2. Vérifier que l'image source n'est pas corrompue
3. Essayer de charger avec un format explicite :
   ```pascal
   imgLogo.Picture.LoadFromStreamWithFileExt(ResStream, 'png');
   ```

### Erreurs de compilation après régénération

1. Nettoyer et recompiler :
   ```bash
   make clean-all && make build-app
   ```

2. Vérifier les erreurs de syntaxe dans le fichier `.lrs` généré
3. S'assurer que `lazres` s'est terminé sans erreur

## Ajouter de nouvelles ressources

### Étape 1 : Ajouter l'image source

Placer l'image dans `resources/` :
```bash
cp monimage.png resources/
```

### Étape 2 : Générer le fichier .lrs

```bash
lazres src/Forms/monimage.lrs resources/monimage.png=monimage
```

### Étape 3 : Inclure dans l'unité

```pascal
unit uMonForm;

// ... code ...

initialization
  {$I monimage.lrs}

end.
```

### Étape 4 : Charger et utiliser

```pascal
procedure TfrmMonForm.LoadImage;
var
  ResStream: TLazarusResourceStream;
begin
  ResStream := TLazarusResourceStream.Create('monimage', nil);
  try
    Image1.Picture.LoadFromStream(ResStream);
  finally
    ResStream.Free;
  end;
end;
```

## Informations de version

- **Dernière mise à jour :** 2026-01-03
- **S'applique à :** 3nity Media v0.x et ultérieur
