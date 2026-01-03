# Resources Developer Documentation

This document describes the embedded resource system for 3nity Media, including how images and other assets are embedded into the application binary.

## Overview

3nity Media uses **Lazarus Resource files (.lrs)** to embed images directly into the executable. This eliminates the need for external image files at runtime.

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│  resources/     │────▶│     lazres      │────▶│    logo.lrs     │
│  logo.png       │     │    (tool)       │     │  (Pascal code)  │
└─────────────────┘     └─────────────────┘     └─────────────────┘
                                                        │
                                                        ▼
                              ┌─────────────────────────────────────┐
                              │           uAbout.pas                │
                              │      {$I logo.lrs}                  │
                              │      initialization section         │
                              └─────────────────────────────────────┘
                                                        │
                                                        ▼
                              ┌─────────────────────────────────────┐
                              │        Compiled Executable          │
                              │    (image data embedded)            │
                              └─────────────────────────────────────┘
```

## File Structure

### Source Images

| File | Description |
|------|-------------|
| `resources/logo.png` | Application logo (displayed in About dialog) |
| `resources/icon.png` | Application icon (source) |
| `resources/icons/*.png` | Multi-resolution icons (16x16 to 256x256) |

### Generated Resources

| File | Description |
|------|-------------|
| `src/Forms/logo.lrs` | Embedded logo resource (generated from logo.png) |
| `src/TrinityMedia.res` | Compiled Windows resource (icons, version info) |

### Icon Files

| File | Description |
|------|-------------|
| `resources/icons/3nity-media.ico` | Windows icon (multi-resolution) |
| `src/3nity-media.ico` | Copy for project |

## The lazres Tool

`lazres` is a Lazarus command-line tool that converts files into Pascal code containing the binary data as a constant array.

### Syntax

```bash
lazres <output.lrs> <input_file>=<resource_name> [additional files...]
```

### Examples

```bash
# Single resource
lazres src/Forms/logo.lrs resources/logo.png=logo

# Multiple resources in one file
lazres src/Forms/resources.lrs image1.png=img1 image2.png=img2
```

### Output Format

The generated `.lrs` file contains Pascal code:

```pascal
LazarusResources.Add('logo','PNG',[
  #137'PNG'#13#10#26#10#0#0#0#13'IHDR'#0#0#0#128#0#0#0#128#8#6#0#0#0...
  // (binary data encoded as Pascal string literals)
]);
```

## Using Embedded Resources

### Step 1: Include the .lrs File

In the unit's `initialization` section:

```pascal
unit uAbout;

// ... unit code ...

initialization
  {$I logo.lrs}

end.
```

### Step 2: Load the Resource

Use `TLazarusResourceStream` to load the embedded data:

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
    // Resource not found or error loading - ignore
  end;
end;
```

### Alternative: Direct Resource Access

```pascal
var
  Res: TLResource;
begin
  Res := LazarusResources.Find('logo');
  if Res <> nil then
  begin
    // Access Res.Value (string with binary data)
    // Access Res.ValueType (e.g., 'PNG')
  end;
end;
```

## Regenerating Resources

### When to Regenerate

Regenerate `.lrs` files when:
- The source image is modified
- You want to change the embedded image
- The `.lrs` file is corrupted or deleted

### Commands

#### Logo Resource

```bash
# From project root
lazres src/Forms/logo.lrs resources/logo.png=logo
```

#### Verify Generation

```bash
# Check the generated file exists and has content
ls -la src/Forms/logo.lrs
head -5 src/Forms/logo.lrs
```

Expected output:
```
LazarusResources.Add('logo','PNG',[
  #137'PNG'#13#10#26#10...
```

### After Regenerating

1. Rebuild the application:
   ```bash
   make build-app
   ```

2. Test the About dialog to verify the image displays correctly

## Supported Image Formats

The `lazres` tool supports any file type, but common image formats are:

| Format | Extension | Notes |
|--------|-----------|-------|
| PNG | `.png` | Recommended (supports transparency) |
| JPEG | `.jpg`, `.jpeg` | Good for photos |
| BMP | `.bmp` | Large file size |
| GIF | `.gif` | Limited colors |
| ICO | `.ico` | Windows icons |

## Best Practices

### Image Optimization

1. **Optimize PNGs** before embedding:
   ```bash
   optipng -o7 resources/logo.png
   # or
   pngcrush -brute resources/logo.png resources/logo_optimized.png
   ```

2. **Keep images small** - embedded resources increase executable size

3. **Use appropriate resolution** - 128x128 or 256x256 is usually sufficient for logos

### Resource Naming

1. Use **lowercase** resource names
2. Use **descriptive** but short names: `logo`, `icon`, `splash`
3. Avoid spaces and special characters

### File Organization

```
project/
├── resources/              # Source images (editable)
│   ├── logo.png
│   ├── icon.png
│   └── icons/
│       ├── 16x16.png
│       ├── 32x32.png
│       └── ...
└── src/
    └── Forms/
        └── logo.lrs       # Generated (do not edit manually)
```

## Troubleshooting

### "Resource not found" Error

1. Verify the `.lrs` file is included:
   ```pascal
   initialization
     {$I logo.lrs}  // Check this line exists
   ```

2. Check resource name matches:
   ```pascal
   // In .lrs generation:
   lazres ... logo.png=logo  // Resource named 'logo'

   // In code:
   TLazarusResourceStream.Create('logo', nil);  // Must match
   ```

3. Ensure the unit with `{$I logo.lrs}` is in the uses clause

### Image Not Displaying

1. Check the image format is supported by the target component
2. Verify the source image is not corrupted
3. Try loading with explicit format:
   ```pascal
   imgLogo.Picture.LoadFromStreamWithFileExt(ResStream, 'png');
   ```

### Build Errors After Regeneration

1. Clean and rebuild:
   ```bash
   make clean-all && make build-app
   ```

2. Check for syntax errors in generated `.lrs` file
3. Ensure `lazres` completed without errors

## Adding New Resources

### Step 1: Add Source Image

Place the image in `resources/`:
```bash
cp myimage.png resources/
```

### Step 2: Generate .lrs File

```bash
lazres src/Forms/myimage.lrs resources/myimage.png=myimage
```

### Step 3: Include in Unit

```pascal
unit uMyForm;

// ... code ...

initialization
  {$I myimage.lrs}

end.
```

### Step 4: Load and Use

```pascal
procedure TfrmMyForm.LoadImage;
var
  ResStream: TLazarusResourceStream;
begin
  ResStream := TLazarusResourceStream.Create('myimage', nil);
  try
    Image1.Picture.LoadFromStream(ResStream);
  finally
    ResStream.Free;
  end;
end;
```

## Version Information

- **Last updated:** 2026-01-03
- **Applies to:** 3nity Media v0.x and later
