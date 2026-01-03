# Functional Tests

This directory is reserved for end-to-end functional tests that verify complete user workflows in 3nity Media.

## Status

**Currently empty** - Functional tests are planned for future implementation.

## Planned Test Coverage

### User Workflow Tests

| Workflow | Description |
|----------|-------------|
| First Launch | Initial setup and configuration |
| Open Media | File selection and playback |
| Playlist Management | Create, save, load playlists |
| Radio Streaming | Browse and play internet radio |
| Recording | Stream recording workflow |
| Settings | Configuration changes |

### UI Interaction Tests

| Area | Description |
|------|-------------|
| Main Window | Player controls, display |
| Playlist Panel | Playlist operations |
| Radio Panel | Radio station browsing |
| Settings Dialog | Configuration UI |
| Dialogs | File dialogs, messages |

### Cross-Platform Tests

| Platform | Description |
|----------|-------------|
| Linux GTK2 | GTK2 widget set |
| Linux Qt5 | Qt5 widget set |
| Windows | Windows native |
| macOS | Cocoa widget set |

## Implementation Notes

Functional tests require:
- GUI automation framework
- Platform-specific test runners
- Test environment setup
- Clean state management

## Future Implementation

When implementing functional tests, consider:

1. **Test Isolation**: Each test should start from clean state
2. **Deterministic**: Tests should produce consistent results
3. **Platform Aware**: Handle platform-specific behaviors
4. **Timeout Handling**: GUI operations may have delays
5. **Screenshot on Failure**: Capture state for debugging

## Example Structure (Planned)

```pascal
unit uTestMediaPlaybackWorkflow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Forms;

type
  TTestMediaPlaybackWorkflow = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_OpenFile_PlaysSuccessfully;
    procedure Test_PauseResume_TogglesPlayback;
    procedure Test_SeekSlider_UpdatesPosition;
    procedure Test_VolumeSlider_ChangesVolume;
  end;

implementation

// Implementation would use GUI automation

end.
```

## Running Functional Tests

Once implemented:

```bash
# Run all functional tests
./bin/TestRunner --suite=Functional

# Run specific workflow
./bin/TestRunner --suite=TTestMediaPlaybackWorkflow
```

## Contributing

To add functional tests:

1. Create test unit in this directory
2. Use appropriate GUI automation approach
3. Ensure tests are platform-aware
4. Add to TestRunner project
5. Document in this README
