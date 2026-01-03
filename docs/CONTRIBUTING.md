# Contributing to 3nity Media

Thank you for your interest in contributing to 3nity Media! This document provides guidelines and instructions for contributing to the project.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [How to Contribute](#how-to-contribute)
  - [Reporting Bugs](#reporting-bugs)
  - [Suggesting Features](#suggesting-features)
  - [Contributing Code](#contributing-code)
- [Development Setup](#development-setup)
- [Code Style Guide](#code-style-guide)
- [Pull Request Process](#pull-request-process)
- [Testing](#testing)
- [Documentation](#documentation)
- [Translation](#translation)

---

## Code of Conduct

This project follows a simple code of conduct:

- **Be respectful** - Treat everyone with respect and kindness
- **Be constructive** - Provide helpful feedback and suggestions
- **Be patient** - Maintainers are volunteers with limited time
- **Be inclusive** - Welcome newcomers and help them contribute

---

## Getting Started

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```bash
   git clone https://github.com/YOUR-USERNAME/3nity-media.git
   cd 3nity-media
   ```
3. **Set up the development environment** (see [Development Setup](#development-setup))
4. **Create a branch** for your changes:
   ```bash
   git checkout -b feature/your-feature-name
   # or
   git checkout -b fix/issue-description
   ```

---

## How to Contribute

### Reporting Bugs

Before reporting a bug, please:

1. **Search existing issues** to avoid duplicates
2. **Test with the latest version** to ensure the bug still exists
3. **Gather relevant information** about your environment

**Create a bug report** using this template:

```markdown
## Bug Description
A clear and concise description of what the bug is.

## Steps to Reproduce
1. Go to '...'
2. Click on '...'
3. Play '...'
4. See error

## Expected Behavior
What you expected to happen.

## Actual Behavior
What actually happened.

## Environment
- OS: [e.g., Ubuntu 24.04, Windows 11]
- 3nity Media version: [e.g., 0.1.0]
- Installation method: [e.g., DEB, AppImage, Portable]
- Desktop environment: [e.g., GNOME, KDE, XFCE]

## Additional Context
- Relevant log output (Options → View Log)
- Screenshots if applicable
- Sample media file if the issue is format-specific
```

**Bug report labels:**
- `bug` - Confirmed bug
- `needs-info` - More information required
- `cannot-reproduce` - Unable to reproduce the issue
- `duplicate` - Already reported
- `wontfix` - Will not be fixed (with explanation)

### Suggesting Features

We welcome feature suggestions! Before suggesting:

1. **Check the roadmap** for planned features
2. **Search existing issues** to avoid duplicates
3. **Consider the scope** - Does it fit the project's goals?

**Create a feature request** using this template:

```markdown
## Feature Description
A clear and concise description of the feature you'd like.

## Use Case
Describe the problem this feature would solve or the benefit it would provide.
Example: "As a user who listens to podcasts, I would like to..."

## Proposed Solution
Your suggested implementation approach (optional but helpful).

## Alternatives Considered
Any alternative solutions or features you've considered.

## Additional Context
- Mockups or screenshots of similar features
- Links to related features in other applications
- Impact on existing functionality
```

**Feature priority factors:**
- Number of users requesting the feature
- Implementation complexity
- Alignment with project goals
- Availability of contributors

### Contributing Code

We accept code contributions for:

- **Bug fixes** - Corrections to existing functionality
- **New features** - After discussion in an issue
- **Performance improvements** - With benchmarks
- **Documentation** - Corrections and additions
- **Translations** - New languages or improvements
- **Tests** - Unit, integration, and functional tests

---

## Development Setup

### Prerequisites

**Linux (Ubuntu/Debian):**
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

**Windows:**
1. Install [Lazarus IDE](https://www.lazarus-ide.org/)
2. Install [Git](https://git-scm.com/)
3. Download [libmpv](https://sourceforge.net/projects/mpv-player-windows/files/libmpv/)

### Building

```bash
# Clone and enter directory
git clone https://github.com/YOUR-USERNAME/3nity-media.git
cd 3nity-media

# Build debug version
make build-app

# Build release version
make build-release

# Run the application
make run

# Clean build artifacts
make clean-all
```

### Project Structure

```
3nity-media/
├── src/                    # Source code
│   ├── Core/              # Core functionality (MPV, playlist, radio)
│   ├── Forms/             # GUI forms and dialogs
│   ├── Common/            # Shared utilities and types
│   ├── Locale/            # Localization system
│   ├── Controls/          # Custom UI controls
│   └── TrinityMedia.lpi   # Lazarus project file
├── bin/                    # Compiled binaries
├── lib/                    # Compiled units
├── docs/                   # Documentation
├── tests/                  # Test suite
├── lang/                   # Translation files
├── resources/              # Icons and assets
└── Makefile               # Build automation
```

---

## Code Style Guide

### General Principles

- **Readability first** - Code should be self-explanatory
- **Consistency** - Follow existing patterns in the codebase
- **Simplicity** - Prefer simple solutions over clever ones
- **Documentation** - Document complex logic and public APIs

### Pascal Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| Units | `u` prefix + PascalCase | `uPlaylistManager.pas` |
| Classes | `T` prefix + PascalCase | `TPlaylistManager` |
| Interfaces | `I` prefix + PascalCase | `IMediaPlayer` |
| Records | `T` prefix + PascalCase | `TPlaylistItem` |
| Enums | `T` prefix + PascalCase | `TPlaybackMode` |
| Enum values | `pm`, `ss`, etc. prefix | `pmNormal`, `ssVideo` |
| Private fields | `F` prefix + PascalCase | `FCurrentIndex` |
| Properties | PascalCase | `CurrentIndex` |
| Methods | PascalCase | `GetNextItem` |
| Local variables | camelCase | `itemCount` |
| Constants | ALL_CAPS with underscores | `MAX_PLAYLIST_SIZE` |
| Parameters | `A` prefix + PascalCase | `AFileName` |

### File Header Template

Every Pascal unit must include this header:

```pascal
{ ═══════════════════════════════════════════════════════════════════════════════
  uUnitName.pas - Brief Description

  Part of 3nity Media - Lazarus Edition

  Detailed description of what this unit does, its purpose, and any important
  notes about its implementation.

  Author: Your Name (your.email@example.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uUnitName;

{$mode objfpc}{$H+}

interface
```

### Section Separators

Use these separators to organize code sections:

```pascal
{ ═══════════════════════════════════════════════════════════════════════════
  SECTION NAME
  ═══════════════════════════════════════════════════════════════════════════ }
```

### Code Examples

**Good:**
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
          Result := -1  { End of playlist }
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

**Bad:**
```pascal
function TPlaylistManager.GetNext: Integer;
begin
  if FItems=nil then exit(-1);
  case FPlaybackMode of pmNormal: begin nextIndex:=FCurrentIndex+1;
  if nextIndex>=Length(FItems) then Result:=-1 else Result:=nextIndex; end;
  // ... cramped, hard to read
end;
```

### Comments

- Use `{ }` for single-line and multi-line comments
- Use `//` sparingly, only for brief inline notes
- Document "why", not "what" (the code shows what)

```pascal
{ Calculate the next shuffle index, wrapping around if we've played all items }
function TPlaylistManager.GetShuffleNext: Integer;
begin
  // Regenerate shuffle if exhausted
  if FShuffleIndex >= Length(FShuffleOrder) then
    GenerateShuffleOrder;

  Result := FShuffleOrder[FShuffleIndex];
  Inc(FShuffleIndex);
end;
```

### Error Handling

```pascal
function TPlaylistManager.LoadFromFile(const AFileName: string): Boolean;
begin
  Result := False;

  if not FileExists(AFileName) then
  begin
    Log('Playlist file not found: ' + AFileName);
    Exit;
  end;

  try
    // Load file content
    Result := ParsePlaylist(AFileName);
  except
    on E: Exception do
    begin
      Log('Error loading playlist: ' + E.Message);
      Result := False;
    end;
  end;
end;
```

### Memory Management

- Always free objects in `finally` blocks or use `try..finally`
- Use `FreeAndNil()` instead of `Free` when appropriate
- Set arrays to `nil` with `SetLength(arr, 0)` or `arr := nil`

```pascal
procedure TPlaylistManager.Clear;
begin
  FItems := nil;  { Dynamic array - automatically freed }
  FShuffleOrder := nil;
  FCurrentIndex := -1;
  FModified := False;
  DoChange;
end;
```

---

## Pull Request Process

### Before Creating a PR

1. **Ensure your code compiles** without errors or warnings
2. **Run existing tests** to verify nothing is broken
3. **Add tests** for new functionality
4. **Update documentation** if needed
5. **Follow the code style guide**

### Creating a Pull Request

1. **Push your branch** to your fork:
   ```bash
   git push origin feature/your-feature-name
   ```

2. **Open a PR** on GitHub with this template:

```markdown
## Description
Brief description of what this PR does.

## Related Issue
Fixes #123 (or "Related to #123" if not fully fixing)

## Type of Change
- [ ] Bug fix (non-breaking change that fixes an issue)
- [ ] New feature (non-breaking change that adds functionality)
- [ ] Breaking change (fix or feature that would break existing functionality)
- [ ] Documentation update
- [ ] Translation update

## Changes Made
- List of specific changes
- Another change
- ...

## Testing Done
- [ ] Tested on Linux (specify distro)
- [ ] Tested on Windows
- [ ] Added unit tests
- [ ] Ran existing test suite

## Screenshots (if applicable)
Before/after screenshots for UI changes.

## Checklist
- [ ] My code follows the project's code style
- [ ] I have performed a self-review of my code
- [ ] I have commented my code where necessary
- [ ] I have updated the documentation
- [ ] My changes generate no new warnings
- [ ] I have added tests that prove my fix/feature works
- [ ] New and existing unit tests pass locally
```

### PR Review Process

1. **Automated checks** run on your PR (build, tests)
2. **Maintainer review** - feedback may be requested
3. **Address feedback** - make changes and push to the same branch
4. **Approval and merge** - once approved, the PR will be merged

### After Merge

- Delete your feature branch
- Pull the latest `main` into your local repository
- Celebrate your contribution!

---

## Testing

### Test Structure

```
tests/
├── TestRunner.lpi         # Test project
├── TestRunner.lpr         # Test main file
├── Unit/                  # Unit tests
│   ├── TestPlaylist.pas
│   ├── TestConfig.pas
│   └── ...
├── Integration/           # Integration tests
├── Functional/            # GUI tests
└── TestData/             # Test media files
```

### Running Tests

```bash
# Build and run all tests
make test

# Quick tests (unit only)
make quick

# Specific test suites
make test-mpv
make test-playlist
make test-config
make test-radio

# Generate HTML report
make report
```

### Writing Tests

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
  FPlaylist.Add('/path/to/file.mp3');
  AssertEquals('Item count should be 1', 1, FPlaylist.Count);
end;
```

---

## Documentation

### Types of Documentation

| Type | Location | Purpose |
|------|----------|---------|
| User guides | `docs/USER_GUIDE_*.md` | End-user documentation |
| Install guides | `docs/INSTALL_*.md` | Installation instructions |
| API docs | `docs/API_*.md` | Developer reference |
| Code comments | Source files | In-code documentation |

### Documentation Style

- Use **Markdown** for all documentation files
- Provide both **English** (`_en.md`) and **French** (`_fr.md`) versions
- Include **code examples** where helpful
- Use **tables** for reference information
- Add a **table of contents** for long documents

---

## Translation

### Adding a New Language

1. Copy `lang/en.lang` to `lang/XX.lang` (where XX is the language code)
2. Translate all values (keep keys unchanged)
3. Update `src/Locale/uLocale.pas` to include the new language
4. Test the application with the new language

### Translation File Format

```ini
[Main]
Language=English
LanguageCode=en
Author=Nicolas DEOUX

[Menu]
File=File
Open=Open
OpenURL=Open URL
...
```

### Translation Guidelines

- Keep translations **concise** (UI space is limited)
- Use **formal** language where appropriate
- Preserve **keyboard shortcuts** indicators (e.g., `&Open` for Alt+O)
- Test translations **in context** to verify they fit

---

## Questions?

- Open an issue for project-related questions
- Join discussions on GitHub Discussions
- Email the maintainer for private inquiries

Thank you for contributing to 3nity Media!
