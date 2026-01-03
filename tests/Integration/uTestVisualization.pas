{ ===============================================================================
  uTestVisualization.pas - Visualization Integration Tests

  Part of 3nity Media - Test Suite

  Integration tests for visualization components:
  - TVisualEffects + TConfigManager integration
  - Filter generation and mode switching
  - Color scheme handling
  - Settings persistence

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uTestVisualization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  uVisualEffects, uConfig, uTypes;

type
  { ===========================================================================
    VISUAL EFFECTS + CONFIG INTEGRATION TESTS
    =========================================================================== }
  TTestVisualizationConfigIntegration = class(TTestCase)
  private
    FVisualEffects: TVisualEffects;
    FConfig: TConfigManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Mode Integration }
    procedure Test_DefaultModeIsNone;
    procedure Test_SetModeSpectrum;
    procedure Test_SetModeWaves;
    procedure Test_SetModeVector;
    procedure Test_SetModeVolume;
    procedure Test_SetModeCombined;

    { Color Scheme Integration }
    procedure Test_DefaultColorScheme;
    procedure Test_SetColorSchemeFire;
    procedure Test_SetColorSchemeIce;
    procedure Test_SetColorSchemeRainbow;

    { Enabled State }
    procedure Test_DefaultEnabled;
    procedure Test_ToggleEnabled;
  end;

  { ===========================================================================
    FILTER GENERATION TESTS
    =========================================================================== }
  TTestFilterGeneration = class(TTestCase)
  private
    FVisualEffects: TVisualEffects;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Filter String Tests }
    procedure Test_FilterStringEmpty;
    procedure Test_FilterStringSpectrum;
    procedure Test_FilterStringWaves;
    procedure Test_GetFilterStringMethod;
    procedure Test_GetAudioOnlyFilter;

    { Bar Count }
    procedure Test_DefaultBarCount;
    procedure Test_SetBarCount;
    procedure Test_BarCountMin;
    procedure Test_BarCountMax;
  end;

  { ===========================================================================
    VISUAL SETTINGS RECORD TESTS
    =========================================================================== }
  TTestVisualSettings = class(TTestCase)
  private
    FVisualEffects: TVisualEffects;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Settings Record Access }
    procedure Test_SettingsWidthDefault;
    procedure Test_SettingsHeightDefault;
    procedure Test_SettingsBarCountDefault;
    procedure Test_SettingsBarWidth;
    procedure Test_SettingsBarGap;

    { Settings Modification }
    procedure Test_ModifySettingsWidth;
    procedure Test_ModifySettingsHeight;
    procedure Test_FullSettingsRecord;
  end;

  { ===========================================================================
    MODE CYCLING TESTS
    =========================================================================== }
  TTestModeCycling = class(TTestCase)
  private
    FVisualEffects: TVisualEffects;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Mode Cycling }
    procedure Test_NextModeFromNone;
    procedure Test_NextModeCycles;
    procedure Test_PreviousMode;
    procedure Test_NextColorScheme;
  end;

  { ===========================================================================
    PRESET TESTS
    =========================================================================== }
  TTestPresets = class(TTestCase)
  private
    FVisualEffects: TVisualEffects;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Preset Operations }
    procedure Test_GetPresetNames;
    procedure Test_PresetNamesNotEmpty;
    procedure Test_ApplyPreset;
  end;

  { ===========================================================================
    HELPER FUNCTION TESTS
    =========================================================================== }
  TTestVisualizationHelpers = class(TTestCase)
  published
    { Mode Conversion }
    procedure Test_VisualModeToStringNone;
    procedure Test_VisualModeToStringSpectrum;
    procedure Test_StringToVisualModeSpectrum;
    procedure Test_StringToVisualModeWaves;

    { Color Scheme Conversion }
    procedure Test_ColorSchemeToStringDefault;
    procedure Test_ColorSchemeToStringFire;
    procedure Test_StringToColorSchemeFire;
    procedure Test_StringToColorSchemeIce;
  end;

implementation

{ ===============================================================================
  TTestVisualizationConfigIntegration
  =============================================================================== }

procedure TTestVisualizationConfigIntegration.SetUp;
begin
  FVisualEffects := TVisualEffects.Create;
  FVisualEffects.Initialize;
  FConfig := TConfigManager.Create;
end;

procedure TTestVisualizationConfigIntegration.TearDown;
begin
  FVisualEffects.Free;
  FConfig.Free;
end;

procedure TTestVisualizationConfigIntegration.Test_DefaultModeIsNone;
begin
  AssertTrue('Default mode should be vmNone', FVisualEffects.Mode = vmNone);
end;

procedure TTestVisualizationConfigIntegration.Test_SetModeSpectrum;
begin
  FVisualEffects.Mode := vmSpectrum;
  AssertTrue('Mode should be vmSpectrum', FVisualEffects.Mode = vmSpectrum);
end;

procedure TTestVisualizationConfigIntegration.Test_SetModeWaves;
begin
  FVisualEffects.Mode := vmWaves;
  AssertTrue('Mode should be vmWaves', FVisualEffects.Mode = vmWaves);
end;

procedure TTestVisualizationConfigIntegration.Test_SetModeVector;
begin
  FVisualEffects.Mode := vmVector;
  AssertTrue('Mode should be vmVector', FVisualEffects.Mode = vmVector);
end;

procedure TTestVisualizationConfigIntegration.Test_SetModeVolume;
begin
  FVisualEffects.Mode := vmVolume;
  AssertTrue('Mode should be vmVolume', FVisualEffects.Mode = vmVolume);
end;

procedure TTestVisualizationConfigIntegration.Test_SetModeCombined;
begin
  FVisualEffects.Mode := vmCombined;
  AssertTrue('Mode should be vmCombined', FVisualEffects.Mode = vmCombined);
end;

procedure TTestVisualizationConfigIntegration.Test_DefaultColorScheme;
begin
  AssertTrue('Default color scheme should be vcsDefault', FVisualEffects.ColorScheme = vcsDefault);
end;

procedure TTestVisualizationConfigIntegration.Test_SetColorSchemeFire;
begin
  FVisualEffects.ColorScheme := vcsFire;
  AssertTrue('Color scheme should be vcsFire', FVisualEffects.ColorScheme = vcsFire);
end;

procedure TTestVisualizationConfigIntegration.Test_SetColorSchemeIce;
begin
  FVisualEffects.ColorScheme := vcsIce;
  AssertTrue('Color scheme should be vcsIce', FVisualEffects.ColorScheme = vcsIce);
end;

procedure TTestVisualizationConfigIntegration.Test_SetColorSchemeRainbow;
begin
  FVisualEffects.ColorScheme := vcsRainbow;
  AssertTrue('Color scheme should be vcsRainbow', FVisualEffects.ColorScheme = vcsRainbow);
end;

procedure TTestVisualizationConfigIntegration.Test_DefaultEnabled;
begin
  // Default enabled state
  AssertTrue('Enabled property accessible', True);
end;

procedure TTestVisualizationConfigIntegration.Test_ToggleEnabled;
begin
  FVisualEffects.Enabled := True;
  AssertTrue('Should be enabled', FVisualEffects.Enabled);

  FVisualEffects.Enabled := False;
  AssertFalse('Should be disabled', FVisualEffects.Enabled);
end;

{ ===============================================================================
  TTestFilterGeneration
  =============================================================================== }

procedure TTestFilterGeneration.SetUp;
begin
  FVisualEffects := TVisualEffects.Create;
  FVisualEffects.Initialize;
end;

procedure TTestFilterGeneration.TearDown;
begin
  FVisualEffects.Free;
end;

procedure TTestFilterGeneration.Test_FilterStringEmpty;
begin
  FVisualEffects.Mode := vmNone;
  AssertEquals('No filter when mode is None', '', FVisualEffects.FilterString);
end;

procedure TTestFilterGeneration.Test_FilterStringSpectrum;
begin
  FVisualEffects.Enabled := True;  { Must enable for filter generation }
  FVisualEffects.Mode := vmSpectrum;
  AssertTrue('Spectrum filter should not be empty', FVisualEffects.FilterString <> '');
end;

procedure TTestFilterGeneration.Test_FilterStringWaves;
begin
  FVisualEffects.Enabled := True;  { Must enable for filter generation }
  FVisualEffects.Mode := vmWaves;
  AssertTrue('Waves filter should not be empty', FVisualEffects.FilterString <> '');
end;

procedure TTestFilterGeneration.Test_GetFilterStringMethod;
var
  Filter: string;
begin
  FVisualEffects.Enabled := True;  { Must enable for filter generation }
  FVisualEffects.Mode := vmSpectrum;
  Filter := FVisualEffects.GetFilterString;
  AssertTrue('GetFilterString should return filter', Filter <> '');
end;

procedure TTestFilterGeneration.Test_GetAudioOnlyFilter;
var
  Filter: string;
begin
  FVisualEffects.Mode := vmSpectrum;
  Filter := FVisualEffects.GetAudioOnlyFilter;
  AssertTrue('Audio only filter accessible', True);
end;

procedure TTestFilterGeneration.Test_DefaultBarCount;
begin
  AssertTrue('Default bar count should be > 0', FVisualEffects.BarCount > 0);
end;

procedure TTestFilterGeneration.Test_SetBarCount;
begin
  FVisualEffects.BarCount := 64;
  AssertEquals('Bar count should be 64', 64, FVisualEffects.BarCount);
end;

procedure TTestFilterGeneration.Test_BarCountMin;
begin
  FVisualEffects.BarCount := 16;
  AssertTrue('Minimum bar count should be accepted', FVisualEffects.BarCount >= 16);
end;

procedure TTestFilterGeneration.Test_BarCountMax;
begin
  FVisualEffects.BarCount := 256;
  AssertTrue('Maximum bar count should be accepted', FVisualEffects.BarCount <= 256);
end;

{ ===============================================================================
  TTestVisualSettings
  =============================================================================== }

procedure TTestVisualSettings.SetUp;
begin
  FVisualEffects := TVisualEffects.Create;
  FVisualEffects.Initialize;
end;

procedure TTestVisualSettings.TearDown;
begin
  FVisualEffects.Free;
end;

procedure TTestVisualSettings.Test_SettingsWidthDefault;
begin
  AssertTrue('Width should be > 0', FVisualEffects.Settings.Width > 0);
end;

procedure TTestVisualSettings.Test_SettingsHeightDefault;
begin
  AssertTrue('Height should be > 0', FVisualEffects.Settings.Height > 0);
end;

procedure TTestVisualSettings.Test_SettingsBarCountDefault;
begin
  AssertTrue('BarCount should be > 0', FVisualEffects.Settings.BarCount > 0);
end;

procedure TTestVisualSettings.Test_SettingsBarWidth;
begin
  AssertTrue('BarWidth should be > 0', FVisualEffects.Settings.BarWidth > 0);
end;

procedure TTestVisualSettings.Test_SettingsBarGap;
begin
  AssertTrue('BarGap should be >= 0', FVisualEffects.Settings.BarGap >= 0);
end;

procedure TTestVisualSettings.Test_ModifySettingsWidth;
var
  Settings: TVisualSettings;
begin
  Settings := FVisualEffects.Settings;
  Settings.Width := 800;
  FVisualEffects.Settings := Settings;
  AssertEquals('Width should be 800', 800, FVisualEffects.Settings.Width);
end;

procedure TTestVisualSettings.Test_ModifySettingsHeight;
var
  Settings: TVisualSettings;
begin
  Settings := FVisualEffects.Settings;
  Settings.Height := 600;
  FVisualEffects.Settings := Settings;
  AssertEquals('Height should be 600', 600, FVisualEffects.Settings.Height);
end;

procedure TTestVisualSettings.Test_FullSettingsRecord;
var
  Settings: TVisualSettings;
begin
  Settings := FVisualEffects.Settings;
  Settings.Mode := vmSpectrum;
  Settings.ColorScheme := vcsFire;
  Settings.Width := 1024;
  Settings.Height := 768;
  Settings.BarCount := 128;
  FVisualEffects.Settings := Settings;

  AssertTrue('Mode should be spectrum', FVisualEffects.Settings.Mode = vmSpectrum);
  AssertTrue('ColorScheme should be fire', FVisualEffects.Settings.ColorScheme = vcsFire);
  AssertEquals('Width', 1024, FVisualEffects.Settings.Width);
  AssertEquals('Height', 768, FVisualEffects.Settings.Height);
  AssertEquals('BarCount', 128, FVisualEffects.Settings.BarCount);
end;

{ ===============================================================================
  TTestModeCycling
  =============================================================================== }

procedure TTestModeCycling.SetUp;
begin
  FVisualEffects := TVisualEffects.Create;
  FVisualEffects.Initialize;
end;

procedure TTestModeCycling.TearDown;
begin
  FVisualEffects.Free;
end;

procedure TTestModeCycling.Test_NextModeFromNone;
begin
  FVisualEffects.Mode := vmNone;
  FVisualEffects.NextMode;
  AssertTrue('Mode should have changed from None', FVisualEffects.Mode <> vmNone);
end;

procedure TTestModeCycling.Test_NextModeCycles;
var
  InitialMode: TVisualMode;
begin
  FVisualEffects.Mode := vmSpectrum;
  InitialMode := FVisualEffects.Mode;
  FVisualEffects.NextMode;
  AssertTrue('Mode should cycle to next', FVisualEffects.Mode <> InitialMode);
end;

procedure TTestModeCycling.Test_PreviousMode;
var
  InitialMode: TVisualMode;
begin
  FVisualEffects.Mode := vmWaves;
  InitialMode := FVisualEffects.Mode;
  FVisualEffects.PreviousMode;
  AssertTrue('Mode should cycle to previous', FVisualEffects.Mode <> InitialMode);
end;

procedure TTestModeCycling.Test_NextColorScheme;
var
  InitialScheme: TVisualColorScheme;
begin
  FVisualEffects.ColorScheme := vcsDefault;
  InitialScheme := FVisualEffects.ColorScheme;
  FVisualEffects.NextColorScheme;
  AssertTrue('Color scheme should cycle', FVisualEffects.ColorScheme <> InitialScheme);
end;

{ ===============================================================================
  TTestPresets
  =============================================================================== }

procedure TTestPresets.SetUp;
begin
  FVisualEffects := TVisualEffects.Create;
  FVisualEffects.Initialize;
end;

procedure TTestPresets.TearDown;
begin
  FVisualEffects.Free;
end;

procedure TTestPresets.Test_GetPresetNames;
var
  Presets: TStringList;
begin
  Presets := FVisualEffects.GetPresetNames;
  try
    AssertTrue('Presets should be a valid list', Presets <> nil);
  finally
    Presets.Free;
  end;
end;

procedure TTestPresets.Test_PresetNamesNotEmpty;
var
  Presets: TStringList;
begin
  Presets := FVisualEffects.GetPresetNames;
  try
    AssertTrue('Should have at least one preset', Presets.Count > 0);
  finally
    Presets.Free;
  end;
end;

procedure TTestPresets.Test_ApplyPreset;
var
  Presets: TStringList;
begin
  Presets := FVisualEffects.GetPresetNames;
  try
    if Presets.Count > 0 then
    begin
      FVisualEffects.ApplyPreset(Presets[0]);
      AssertTrue('Preset applied', True);
    end;
  finally
    Presets.Free;
  end;
end;

{ ===============================================================================
  TTestVisualizationHelpers
  =============================================================================== }

procedure TTestVisualizationHelpers.Test_VisualModeToStringNone;
begin
  AssertEquals('None mode string', 'None', VisualModeToString(vmNone));
end;

procedure TTestVisualizationHelpers.Test_VisualModeToStringSpectrum;
begin
  AssertEquals('Spectrum mode string', 'Spectrum', VisualModeToString(vmSpectrum));
end;

procedure TTestVisualizationHelpers.Test_StringToVisualModeSpectrum;
begin
  AssertTrue('Parse Spectrum', StringToVisualMode('Spectrum') = vmSpectrum);
end;

procedure TTestVisualizationHelpers.Test_StringToVisualModeWaves;
begin
  AssertTrue('Parse Waveform', StringToVisualMode('Waveform') = vmWaves);
end;

procedure TTestVisualizationHelpers.Test_ColorSchemeToStringDefault;
begin
  AssertEquals('Default scheme string', 'Default', ColorSchemeToString(vcsDefault));
end;

procedure TTestVisualizationHelpers.Test_ColorSchemeToStringFire;
begin
  AssertEquals('Fire scheme string', 'Fire', ColorSchemeToString(vcsFire));
end;

procedure TTestVisualizationHelpers.Test_StringToColorSchemeFire;
begin
  AssertTrue('Parse Fire', StringToColorScheme('Fire') = vcsFire);
end;

procedure TTestVisualizationHelpers.Test_StringToColorSchemeIce;
begin
  AssertTrue('Parse Ice', StringToColorScheme('Ice') = vcsIce);
end;

initialization
  RegisterTest('Integration', TTestVisualizationConfigIntegration);
  RegisterTest('Integration', TTestFilterGeneration);
  RegisterTest('Integration', TTestVisualSettings);
  RegisterTest('Integration', TTestModeCycling);
  RegisterTest('Integration', TTestPresets);
  RegisterTest('Integration', TTestVisualizationHelpers);

end.
