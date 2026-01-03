{ ═══════════════════════════════════════════════════════════════════════════════
  uTestVisualEffects.pas - Unit Tests for Visual Effects

  Part of 3nity Media Test Suite

  Tests for uVisualEffects.pas:
  - Helper functions (VisualModeToString, StringToVisualMode, etc.)
  - Enum values (TVisualMode, TSpectrumMode, TWaveformMode, TVisualColorScheme)
  - TVisualEffects class (creation, initialization, settings, filters)
  - Presets and cycling functions
  - Event handling

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uTestVisualEffects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Graphics,
  uVisualEffects;

type
  { ═══════════════════════════════════════════════════════════════════════════
    TTestVisualEffects - Test cases for Visual Effects
    ═══════════════════════════════════════════════════════════════════════════ }

  TTestVisualEffects = class(TTestCase)
  private
    FEffects: TVisualEffects;
    FFilterChangedCount: Integer;
    procedure OnFilterChangedHandler(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { ─────────────────────────────────────────────────────────────────────────
      VisualModeToString Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_VisualModeToString_None;
    procedure Test_VisualModeToString_Spectrum;
    procedure Test_VisualModeToString_Waves;
    procedure Test_VisualModeToString_Vector;
    procedure Test_VisualModeToString_Histogram;
    procedure Test_VisualModeToString_Volume;
    procedure Test_VisualModeToString_Combined;

    { ─────────────────────────────────────────────────────────────────────────
      StringToVisualMode Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_StringToVisualMode_Spectrum;
    procedure Test_StringToVisualMode_Waveform;
    procedure Test_StringToVisualMode_Waves;
    procedure Test_StringToVisualMode_Vector;
    procedure Test_StringToVisualMode_Histogram;
    procedure Test_StringToVisualMode_Volume;
    procedure Test_StringToVisualMode_Combined;
    procedure Test_StringToVisualMode_Unknown;
    procedure Test_StringToVisualMode_CaseInsensitive;

    { ─────────────────────────────────────────────────────────────────────────
      ColorSchemeToString Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_ColorSchemeToString_Default;
    procedure Test_ColorSchemeToString_Fire;
    procedure Test_ColorSchemeToString_Ice;
    procedure Test_ColorSchemeToString_Rainbow;
    procedure Test_ColorSchemeToString_Green;
    procedure Test_ColorSchemeToString_Purple;
    procedure Test_ColorSchemeToString_White;

    { ─────────────────────────────────────────────────────────────────────────
      StringToColorScheme Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_StringToColorScheme_Fire;
    procedure Test_StringToColorScheme_Ice;
    procedure Test_StringToColorScheme_Rainbow;
    procedure Test_StringToColorScheme_Green;
    procedure Test_StringToColorScheme_Purple;
    procedure Test_StringToColorScheme_White;
    procedure Test_StringToColorScheme_Unknown;
    procedure Test_StringToColorScheme_CaseInsensitive;

    { ─────────────────────────────────────────────────────────────────────────
      Enum Value Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_TVisualMode_Count;
    procedure Test_TSpectrumMode_Count;
    procedure Test_TWaveformMode_Count;
    procedure Test_TVisualColorScheme_Count;

    { ─────────────────────────────────────────────────────────────────────────
      TVisualEffects Creation Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Create_NotNil;
    procedure Test_Create_NotEnabled;
    procedure Test_Create_ModeNone;
    procedure Test_Create_EmptyFilterString;

    { ─────────────────────────────────────────────────────────────────────────
      TVisualEffects Default Settings Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Default_SpectrumMode;
    procedure Test_Default_WaveformMode;
    procedure Test_Default_ColorScheme;
    procedure Test_Default_Width;
    procedure Test_Default_Height;
    procedure Test_Default_BarCount;
    procedure Test_Default_BarWidth;
    procedure Test_Default_BarGap;
    procedure Test_Default_ShowPeaks;
    procedure Test_Default_PeakFalloff;
    procedure Test_Default_WaveScale;
    procedure Test_Default_WaveSpeed;
    procedure Test_Default_Opacity;
    procedure Test_Default_BackgroundColor;
    procedure Test_Default_ForegroundColor;
    procedure Test_Default_EnableGlow;
    procedure Test_Default_MirrorEffect;
    procedure Test_Default_Smoothing;

    { ─────────────────────────────────────────────────────────────────────────
      SetEnabled Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_SetEnabled_True;
    procedure Test_SetEnabled_False;
    procedure Test_SetEnabled_TriggersEvent;

    { ─────────────────────────────────────────────────────────────────────────
      SetMode Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_SetMode_Spectrum;
    procedure Test_SetMode_Waves;
    procedure Test_SetMode_Vector;
    procedure Test_SetMode_Histogram;
    procedure Test_SetMode_Volume;
    procedure Test_SetMode_Combined;
    procedure Test_SetMode_TriggersEvent;

    { ─────────────────────────────────────────────────────────────────────────
      SetColorScheme Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_SetColorScheme_Fire;
    procedure Test_SetColorScheme_Ice;
    procedure Test_SetColorScheme_Rainbow;
    procedure Test_SetColorScheme_TriggersEvent;

    { ─────────────────────────────────────────────────────────────────────────
      SetBarCount Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_SetBarCount_Valid;
    procedure Test_SetBarCount_MinBound;
    procedure Test_SetBarCount_MaxBound;
    procedure Test_SetBarCount_BelowMin;
    procedure Test_SetBarCount_AboveMax;
    procedure Test_SetBarCount_TriggersEvent;

    { ─────────────────────────────────────────────────────────────────────────
      SetSpectrumMode Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_SetSpectrumMode_Vertical;
    procedure Test_SetSpectrumMode_Horizontal;
    procedure Test_SetSpectrumMode_Scroll;
    procedure Test_SetSpectrumMode_TriggersEvent;

    { ─────────────────────────────────────────────────────────────────────────
      SetWaveformMode Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_SetWaveformMode_Point;
    procedure Test_SetWaveformMode_Line;
    procedure Test_SetWaveformMode_P2P;
    procedure Test_SetWaveformMode_Cline;
    procedure Test_SetWaveformMode_TriggersEvent;

    { ─────────────────────────────────────────────────────────────────────────
      Reset Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Reset_RestoresDefaults;
    procedure Test_Reset_TriggersEvent;

    { ─────────────────────────────────────────────────────────────────────────
      GetFilterString Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_GetFilterString_Disabled;
    procedure Test_GetFilterString_ModeNone;
    procedure Test_GetFilterString_Spectrum_ContainsShowspectrum;
    procedure Test_GetFilterString_Spectrum_ContainsDimensions;
    procedure Test_GetFilterString_Waves_ContainsShowwaves;
    procedure Test_GetFilterString_Vector_ContainsAvectorscope;
    procedure Test_GetFilterString_Histogram_ContainsAhistogram;
    procedure Test_GetFilterString_Volume_ContainsShowvolume;
    procedure Test_GetFilterString_Combined_ContainsAsplit;

    { ─────────────────────────────────────────────────────────────────────────
      GetAudioOnlyFilter Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_GetAudioOnlyFilter_Disabled;
    procedure Test_GetAudioOnlyFilter_ModeNone;
    procedure Test_GetAudioOnlyFilter_Spectrum_ContainsAsplit;
    procedure Test_GetAudioOnlyFilter_Spectrum_ContainsAo;
    procedure Test_GetAudioOnlyFilter_Spectrum_ContainsVo;
    procedure Test_GetAudioOnlyFilter_Spectrum_ContainsOverlay;
    procedure Test_GetAudioOnlyFilter_Volume_ContainsYOffset;
    procedure Test_GetAudioOnlyFilter_Combined;

    { ─────────────────────────────────────────────────────────────────────────
      NextMode / PreviousMode Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_NextMode_FromNone;
    procedure Test_NextMode_FromSpectrum;
    procedure Test_NextMode_Wraps;
    procedure Test_PreviousMode_FromSpectrum;
    procedure Test_PreviousMode_Wraps;

    { ─────────────────────────────────────────────────────────────────────────
      NextColorScheme Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_NextColorScheme_FromDefault;
    procedure Test_NextColorScheme_Wraps;

    { ─────────────────────────────────────────────────────────────────────────
      GetPresetNames Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_GetPresetNames_NotNil;
    procedure Test_GetPresetNames_Count;
    procedure Test_GetPresetNames_ContainsClassicSpectrum;
    procedure Test_GetPresetNames_ContainsFireSpectrum;
    procedure Test_GetPresetNames_ContainsSpectrogram;
    procedure Test_GetPresetNames_ContainsOscilloscope;
    procedure Test_GetPresetNames_ContainsStereoScope;
    procedure Test_GetPresetNames_ContainsVUMeter;
    procedure Test_GetPresetNames_ContainsRainbowWaves;
    procedure Test_GetPresetNames_ContainsMinimal;

    { ─────────────────────────────────────────────────────────────────────────
      ApplyPreset Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_ApplyPreset_ClassicSpectrum_Mode;
    procedure Test_ApplyPreset_ClassicSpectrum_SpectrumMode;
    procedure Test_ApplyPreset_ClassicSpectrum_ColorScheme;
    procedure Test_ApplyPreset_ClassicSpectrum_BarCount;
    procedure Test_ApplyPreset_ClassicSpectrum_Enabled;
    procedure Test_ApplyPreset_FireSpectrum_Mode;
    procedure Test_ApplyPreset_FireSpectrum_ColorScheme;
    procedure Test_ApplyPreset_FireSpectrum_BarCount;
    procedure Test_ApplyPreset_Spectrogram_SpectrumMode;
    procedure Test_ApplyPreset_Spectrogram_ColorScheme;
    procedure Test_ApplyPreset_Spectrogram_BarCount;
    procedure Test_ApplyPreset_Oscilloscope_Mode;
    procedure Test_ApplyPreset_Oscilloscope_WaveformMode;
    procedure Test_ApplyPreset_StereoScope_Mode;
    procedure Test_ApplyPreset_VUMeter_Mode;
    procedure Test_ApplyPreset_RainbowWaves_Mode;
    procedure Test_ApplyPreset_RainbowWaves_WaveformMode;
    procedure Test_ApplyPreset_RainbowWaves_MirrorEffect;
    procedure Test_ApplyPreset_Minimal_WaveformMode;
    procedure Test_ApplyPreset_Minimal_ColorScheme;
    procedure Test_ApplyPreset_CaseInsensitive;
    procedure Test_ApplyPreset_UnknownIgnored;
    procedure Test_ApplyPreset_TriggersEvent;

    { ─────────────────────────────────────────────────────────────────────────
      Filter Content Tests - Spectrum
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_SpectrumFilter_ContainsScale;
    procedure Test_SpectrumFilter_ContainsColorChannel;
    procedure Test_SpectrumFilter_ContainsColorFire;
    procedure Test_SpectrumFilter_ScrollMode;

    { ─────────────────────────────────────────────────────────────────────────
      Filter Content Tests - Waves
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_WavesFilter_ContainsMode;
    procedure Test_WavesFilter_PointMode;
    procedure Test_WavesFilter_LineMode;
    procedure Test_WavesFilter_P2PMode;
    procedure Test_WavesFilter_ClineMode;

    { ─────────────────────────────────────────────────────────────────────────
      Filter Content Tests - Vector
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_VectorFilter_ContainsLissajous;
    procedure Test_VectorFilter_ContainsScale;

    { ─────────────────────────────────────────────────────────────────────────
      Filter Content Tests - Histogram
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_HistogramFilter_ContainsSlide;
    procedure Test_HistogramFilter_ContainsScale;

    { ─────────────────────────────────────────────────────────────────────────
      Filter Content Tests - Volume
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_VolumeFilter_ContainsWidth;
    procedure Test_VolumeFilter_ContainsHeight;

    { ─────────────────────────────────────────────────────────────────────────
      Filter Content Tests - Combined
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_CombinedFilter_ContainsVstack;
    procedure Test_CombinedFilter_ContainsShowspectrum;
    procedure Test_CombinedFilter_ContainsShowwaves;
    procedure Test_CombinedFilter_ContainsOverlay;

    { ─────────────────────────────────────────────────────────────────────────
      Settings Property Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Settings_Read;
    procedure Test_Settings_Write;
  end;

implementation

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestVisualEffects Implementation
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestVisualEffects.OnFilterChangedHandler(Sender: TObject);
begin
  Inc(FFilterChangedCount);
end;

procedure TTestVisualEffects.SetUp;
begin
  FEffects := TVisualEffects.Create;
  FFilterChangedCount := 0;
end;

procedure TTestVisualEffects.TearDown;
begin
  FEffects.Free;
  FEffects := nil;
end;

{ ─────────────────────────────────────────────────────────────────────────────
  VisualModeToString Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_VisualModeToString_None;
begin
  AssertEquals('None', 'None', VisualModeToString(vmNone));
end;

procedure TTestVisualEffects.Test_VisualModeToString_Spectrum;
begin
  AssertEquals('Spectrum', 'Spectrum', VisualModeToString(vmSpectrum));
end;

procedure TTestVisualEffects.Test_VisualModeToString_Waves;
begin
  AssertEquals('Waveform', 'Waveform', VisualModeToString(vmWaves));
end;

procedure TTestVisualEffects.Test_VisualModeToString_Vector;
begin
  AssertEquals('Vector', 'Vector', VisualModeToString(vmVector));
end;

procedure TTestVisualEffects.Test_VisualModeToString_Histogram;
begin
  AssertEquals('Histogram', 'Histogram', VisualModeToString(vmHistogram));
end;

procedure TTestVisualEffects.Test_VisualModeToString_Volume;
begin
  AssertEquals('Volume', 'Volume', VisualModeToString(vmVolume));
end;

procedure TTestVisualEffects.Test_VisualModeToString_Combined;
begin
  AssertEquals('Combined', 'Combined', VisualModeToString(vmCombined));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  StringToVisualMode Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_StringToVisualMode_Spectrum;
begin
  AssertEquals('Spectrum', Ord(vmSpectrum), Ord(StringToVisualMode('spectrum')));
end;

procedure TTestVisualEffects.Test_StringToVisualMode_Waveform;
begin
  AssertEquals('Waveform', Ord(vmWaves), Ord(StringToVisualMode('waveform')));
end;

procedure TTestVisualEffects.Test_StringToVisualMode_Waves;
begin
  AssertEquals('Waves', Ord(vmWaves), Ord(StringToVisualMode('waves')));
end;

procedure TTestVisualEffects.Test_StringToVisualMode_Vector;
begin
  AssertEquals('Vector', Ord(vmVector), Ord(StringToVisualMode('vector')));
end;

procedure TTestVisualEffects.Test_StringToVisualMode_Histogram;
begin
  AssertEquals('Histogram', Ord(vmHistogram), Ord(StringToVisualMode('histogram')));
end;

procedure TTestVisualEffects.Test_StringToVisualMode_Volume;
begin
  AssertEquals('Volume', Ord(vmVolume), Ord(StringToVisualMode('volume')));
end;

procedure TTestVisualEffects.Test_StringToVisualMode_Combined;
begin
  AssertEquals('Combined', Ord(vmCombined), Ord(StringToVisualMode('combined')));
end;

procedure TTestVisualEffects.Test_StringToVisualMode_Unknown;
begin
  AssertEquals('Unknown', Ord(vmNone), Ord(StringToVisualMode('unknown')));
end;

procedure TTestVisualEffects.Test_StringToVisualMode_CaseInsensitive;
begin
  AssertEquals('SPECTRUM', Ord(vmSpectrum), Ord(StringToVisualMode('SPECTRUM')));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  ColorSchemeToString Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_ColorSchemeToString_Default;
begin
  AssertEquals('Default', 'Default', ColorSchemeToString(vcsDefault));
end;

procedure TTestVisualEffects.Test_ColorSchemeToString_Fire;
begin
  AssertEquals('Fire', 'Fire', ColorSchemeToString(vcsFire));
end;

procedure TTestVisualEffects.Test_ColorSchemeToString_Ice;
begin
  AssertEquals('Ice', 'Ice', ColorSchemeToString(vcsIce));
end;

procedure TTestVisualEffects.Test_ColorSchemeToString_Rainbow;
begin
  AssertEquals('Rainbow', 'Rainbow', ColorSchemeToString(vcsRainbow));
end;

procedure TTestVisualEffects.Test_ColorSchemeToString_Green;
begin
  AssertEquals('Green', 'Green', ColorSchemeToString(vcsGreen));
end;

procedure TTestVisualEffects.Test_ColorSchemeToString_Purple;
begin
  AssertEquals('Purple', 'Purple', ColorSchemeToString(vcsPurple));
end;

procedure TTestVisualEffects.Test_ColorSchemeToString_White;
begin
  AssertEquals('White', 'White', ColorSchemeToString(vcsWhite));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  StringToColorScheme Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_StringToColorScheme_Fire;
begin
  AssertEquals('Fire', Ord(vcsFire), Ord(StringToColorScheme('fire')));
end;

procedure TTestVisualEffects.Test_StringToColorScheme_Ice;
begin
  AssertEquals('Ice', Ord(vcsIce), Ord(StringToColorScheme('ice')));
end;

procedure TTestVisualEffects.Test_StringToColorScheme_Rainbow;
begin
  AssertEquals('Rainbow', Ord(vcsRainbow), Ord(StringToColorScheme('rainbow')));
end;

procedure TTestVisualEffects.Test_StringToColorScheme_Green;
begin
  AssertEquals('Green', Ord(vcsGreen), Ord(StringToColorScheme('green')));
end;

procedure TTestVisualEffects.Test_StringToColorScheme_Purple;
begin
  AssertEquals('Purple', Ord(vcsPurple), Ord(StringToColorScheme('purple')));
end;

procedure TTestVisualEffects.Test_StringToColorScheme_White;
begin
  AssertEquals('White', Ord(vcsWhite), Ord(StringToColorScheme('white')));
end;

procedure TTestVisualEffects.Test_StringToColorScheme_Unknown;
begin
  AssertEquals('Unknown', Ord(vcsDefault), Ord(StringToColorScheme('unknown')));
end;

procedure TTestVisualEffects.Test_StringToColorScheme_CaseInsensitive;
begin
  AssertEquals('FIRE', Ord(vcsFire), Ord(StringToColorScheme('FIRE')));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Enum Value Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_TVisualMode_Count;
begin
  AssertEquals('TVisualMode count', 7, Ord(High(TVisualMode)) - Ord(Low(TVisualMode)) + 1);
end;

procedure TTestVisualEffects.Test_TSpectrumMode_Count;
begin
  AssertEquals('TSpectrumMode count', 3, Ord(High(TSpectrumMode)) - Ord(Low(TSpectrumMode)) + 1);
end;

procedure TTestVisualEffects.Test_TWaveformMode_Count;
begin
  AssertEquals('TWaveformMode count', 4, Ord(High(TWaveformMode)) - Ord(Low(TWaveformMode)) + 1);
end;

procedure TTestVisualEffects.Test_TVisualColorScheme_Count;
begin
  AssertEquals('TVisualColorScheme count', 7, Ord(High(TVisualColorScheme)) - Ord(Low(TVisualColorScheme)) + 1);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TVisualEffects Creation Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_Create_NotNil;
begin
  AssertNotNull('Should create instance', FEffects);
end;

procedure TTestVisualEffects.Test_Create_NotEnabled;
begin
  AssertFalse('Should not be enabled', FEffects.Enabled);
end;

procedure TTestVisualEffects.Test_Create_ModeNone;
begin
  AssertEquals('Mode should be None', Ord(vmNone), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_Create_EmptyFilterString;
begin
  AssertEquals('Filter should be empty', '', FEffects.FilterString);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TVisualEffects Default Settings Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_Default_SpectrumMode;
begin
  AssertEquals('Default SpectrumMode', Ord(smVertical), Ord(FEffects.SpectrumMode));
end;

procedure TTestVisualEffects.Test_Default_WaveformMode;
begin
  AssertEquals('Default WaveformMode', Ord(wmLine), Ord(FEffects.WaveformMode));
end;

procedure TTestVisualEffects.Test_Default_ColorScheme;
begin
  AssertEquals('Default ColorScheme', Ord(vcsDefault), Ord(FEffects.ColorScheme));
end;

procedure TTestVisualEffects.Test_Default_Width;
begin
  AssertEquals('Default Width', 640, FEffects.Settings.Width);
end;

procedure TTestVisualEffects.Test_Default_Height;
begin
  AssertEquals('Default Height', 360, FEffects.Settings.Height);
end;

procedure TTestVisualEffects.Test_Default_BarCount;
begin
  AssertEquals('Default BarCount', 64, FEffects.BarCount);
end;

procedure TTestVisualEffects.Test_Default_BarWidth;
begin
  AssertEquals('Default BarWidth', 8, FEffects.Settings.BarWidth);
end;

procedure TTestVisualEffects.Test_Default_BarGap;
begin
  AssertEquals('Default BarGap', 2, FEffects.Settings.BarGap);
end;

procedure TTestVisualEffects.Test_Default_ShowPeaks;
begin
  AssertTrue('Default ShowPeaks', FEffects.Settings.ShowPeaks);
end;

procedure TTestVisualEffects.Test_Default_PeakFalloff;
begin
  AssertEquals('Default PeakFalloff', 3, FEffects.Settings.PeakFalloff);
end;

procedure TTestVisualEffects.Test_Default_WaveScale;
begin
  AssertEquals('Default WaveScale', 1.0, FEffects.Settings.WaveScale, 0.001);
end;

procedure TTestVisualEffects.Test_Default_WaveSpeed;
begin
  AssertEquals('Default WaveSpeed', 1, FEffects.Settings.WaveSpeed);
end;

procedure TTestVisualEffects.Test_Default_Opacity;
begin
  AssertEquals('Default Opacity', 100, FEffects.Settings.Opacity);
end;

procedure TTestVisualEffects.Test_Default_BackgroundColor;
begin
  AssertEquals('Default BackgroundColor', clBlack, FEffects.Settings.BackgroundColor);
end;

procedure TTestVisualEffects.Test_Default_ForegroundColor;
begin
  AssertEquals('Default ForegroundColor', clLime, FEffects.Settings.ForegroundColor);
end;

procedure TTestVisualEffects.Test_Default_EnableGlow;
begin
  AssertFalse('Default EnableGlow', FEffects.Settings.EnableGlow);
end;

procedure TTestVisualEffects.Test_Default_MirrorEffect;
begin
  AssertFalse('Default MirrorEffect', FEffects.Settings.MirrorEffect);
end;

procedure TTestVisualEffects.Test_Default_Smoothing;
begin
  AssertEquals('Default Smoothing', 5, FEffects.Settings.Smoothing);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  SetEnabled Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_SetEnabled_True;
begin
  FEffects.Enabled := True;
  AssertTrue('Should be enabled', FEffects.Enabled);
end;

procedure TTestVisualEffects.Test_SetEnabled_False;
begin
  FEffects.Enabled := True;
  FEffects.Enabled := False;
  AssertFalse('Should be disabled', FEffects.Enabled);
end;

procedure TTestVisualEffects.Test_SetEnabled_TriggersEvent;
begin
  FEffects.OnFilterChanged := @OnFilterChangedHandler;
  FEffects.Enabled := True;
  AssertEquals('Event should trigger', 1, FFilterChangedCount);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  SetMode Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_SetMode_Spectrum;
begin
  FEffects.Mode := vmSpectrum;
  AssertEquals('Mode should be Spectrum', Ord(vmSpectrum), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_SetMode_Waves;
begin
  FEffects.Mode := vmWaves;
  AssertEquals('Mode should be Waves', Ord(vmWaves), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_SetMode_Vector;
begin
  FEffects.Mode := vmVector;
  AssertEquals('Mode should be Vector', Ord(vmVector), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_SetMode_Histogram;
begin
  FEffects.Mode := vmHistogram;
  AssertEquals('Mode should be Histogram', Ord(vmHistogram), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_SetMode_Volume;
begin
  FEffects.Mode := vmVolume;
  AssertEquals('Mode should be Volume', Ord(vmVolume), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_SetMode_Combined;
begin
  FEffects.Mode := vmCombined;
  AssertEquals('Mode should be Combined', Ord(vmCombined), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_SetMode_TriggersEvent;
begin
  FEffects.OnFilterChanged := @OnFilterChangedHandler;
  FEffects.Mode := vmSpectrum;
  AssertEquals('Event should trigger', 1, FFilterChangedCount);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  SetColorScheme Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_SetColorScheme_Fire;
begin
  FEffects.ColorScheme := vcsFire;
  AssertEquals('ColorScheme should be Fire', Ord(vcsFire), Ord(FEffects.ColorScheme));
end;

procedure TTestVisualEffects.Test_SetColorScheme_Ice;
begin
  FEffects.ColorScheme := vcsIce;
  AssertEquals('ColorScheme should be Ice', Ord(vcsIce), Ord(FEffects.ColorScheme));
end;

procedure TTestVisualEffects.Test_SetColorScheme_Rainbow;
begin
  FEffects.ColorScheme := vcsRainbow;
  AssertEquals('ColorScheme should be Rainbow', Ord(vcsRainbow), Ord(FEffects.ColorScheme));
end;

procedure TTestVisualEffects.Test_SetColorScheme_TriggersEvent;
begin
  FEffects.OnFilterChanged := @OnFilterChangedHandler;
  FEffects.ColorScheme := vcsFire;
  AssertEquals('Event should trigger', 1, FFilterChangedCount);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  SetBarCount Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_SetBarCount_Valid;
begin
  FEffects.BarCount := 128;
  AssertEquals('BarCount should be 128', 128, FEffects.BarCount);
end;

procedure TTestVisualEffects.Test_SetBarCount_MinBound;
begin
  FEffects.BarCount := 8;
  AssertEquals('BarCount should be 8', 8, FEffects.BarCount);
end;

procedure TTestVisualEffects.Test_SetBarCount_MaxBound;
begin
  FEffects.BarCount := 512;
  AssertEquals('BarCount should be 512', 512, FEffects.BarCount);
end;

procedure TTestVisualEffects.Test_SetBarCount_BelowMin;
begin
  FEffects.BarCount := 2;
  AssertEquals('BarCount should clamp to 8', 8, FEffects.BarCount);
end;

procedure TTestVisualEffects.Test_SetBarCount_AboveMax;
begin
  FEffects.BarCount := 1000;
  AssertEquals('BarCount should clamp to 512', 512, FEffects.BarCount);
end;

procedure TTestVisualEffects.Test_SetBarCount_TriggersEvent;
begin
  FEffects.OnFilterChanged := @OnFilterChangedHandler;
  FEffects.BarCount := 128;
  AssertEquals('Event should trigger', 1, FFilterChangedCount);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  SetSpectrumMode Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_SetSpectrumMode_Vertical;
begin
  FEffects.SpectrumMode := smVertical;
  AssertEquals('SpectrumMode should be Vertical', Ord(smVertical), Ord(FEffects.SpectrumMode));
end;

procedure TTestVisualEffects.Test_SetSpectrumMode_Horizontal;
begin
  FEffects.SpectrumMode := smHorizontal;
  AssertEquals('SpectrumMode should be Horizontal', Ord(smHorizontal), Ord(FEffects.SpectrumMode));
end;

procedure TTestVisualEffects.Test_SetSpectrumMode_Scroll;
begin
  FEffects.SpectrumMode := smScroll;
  AssertEquals('SpectrumMode should be Scroll', Ord(smScroll), Ord(FEffects.SpectrumMode));
end;

procedure TTestVisualEffects.Test_SetSpectrumMode_TriggersEvent;
begin
  FEffects.OnFilterChanged := @OnFilterChangedHandler;
  FEffects.SpectrumMode := smHorizontal;
  AssertEquals('Event should trigger', 1, FFilterChangedCount);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  SetWaveformMode Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_SetWaveformMode_Point;
begin
  FEffects.WaveformMode := wmPoint;
  AssertEquals('WaveformMode should be Point', Ord(wmPoint), Ord(FEffects.WaveformMode));
end;

procedure TTestVisualEffects.Test_SetWaveformMode_Line;
begin
  FEffects.WaveformMode := wmLine;
  AssertEquals('WaveformMode should be Line', Ord(wmLine), Ord(FEffects.WaveformMode));
end;

procedure TTestVisualEffects.Test_SetWaveformMode_P2P;
begin
  FEffects.WaveformMode := wmP2P;
  AssertEquals('WaveformMode should be P2P', Ord(wmP2P), Ord(FEffects.WaveformMode));
end;

procedure TTestVisualEffects.Test_SetWaveformMode_Cline;
begin
  FEffects.WaveformMode := wmCline;
  AssertEquals('WaveformMode should be Cline', Ord(wmCline), Ord(FEffects.WaveformMode));
end;

procedure TTestVisualEffects.Test_SetWaveformMode_TriggersEvent;
begin
  FEffects.OnFilterChanged := @OnFilterChangedHandler;
  FEffects.WaveformMode := wmPoint;
  AssertEquals('Event should trigger', 1, FFilterChangedCount);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Reset Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_Reset_RestoresDefaults;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmSpectrum;
  FEffects.ColorScheme := vcsFire;
  FEffects.BarCount := 256;
  FEffects.Reset;
  AssertFalse('Enabled should be False', FEffects.Enabled);
  AssertEquals('Mode should be None', Ord(vmNone), Ord(FEffects.Mode));
  AssertEquals('ColorScheme should be Default', Ord(vcsDefault), Ord(FEffects.ColorScheme));
  AssertEquals('BarCount should be 64', 64, FEffects.BarCount);
end;

procedure TTestVisualEffects.Test_Reset_TriggersEvent;
begin
  FEffects.OnFilterChanged := @OnFilterChangedHandler;
  FEffects.Reset;
  AssertEquals('Event should trigger', 1, FFilterChangedCount);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  GetFilterString Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_GetFilterString_Disabled;
begin
  FEffects.Enabled := False;
  FEffects.Mode := vmSpectrum;
  AssertEquals('Filter should be empty when disabled', '', FEffects.GetFilterString);
end;

procedure TTestVisualEffects.Test_GetFilterString_ModeNone;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmNone;
  AssertEquals('Filter should be empty for vmNone', '', FEffects.GetFilterString);
end;

procedure TTestVisualEffects.Test_GetFilterString_Spectrum_ContainsShowspectrum;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmSpectrum;
  AssertTrue('Should contain showspectrum', Pos('showspectrum', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_GetFilterString_Spectrum_ContainsDimensions;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmSpectrum;
  AssertTrue('Should contain dimensions', Pos('640x360', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_GetFilterString_Waves_ContainsShowwaves;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmWaves;
  AssertTrue('Should contain showwaves', Pos('showwaves', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_GetFilterString_Vector_ContainsAvectorscope;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmVector;
  AssertTrue('Should contain avectorscope', Pos('avectorscope', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_GetFilterString_Histogram_ContainsAhistogram;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmHistogram;
  AssertTrue('Should contain ahistogram', Pos('ahistogram', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_GetFilterString_Volume_ContainsShowvolume;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmVolume;
  AssertTrue('Should contain showvolume', Pos('showvolume', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_GetFilterString_Combined_ContainsAsplit;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmCombined;
  AssertTrue('Should contain asplit', Pos('asplit', FEffects.GetFilterString) > 0);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  GetAudioOnlyFilter Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_GetAudioOnlyFilter_Disabled;
begin
  FEffects.Enabled := False;
  FEffects.Mode := vmSpectrum;
  AssertEquals('Filter should be empty when disabled', '', FEffects.GetAudioOnlyFilter);
end;

procedure TTestVisualEffects.Test_GetAudioOnlyFilter_ModeNone;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmNone;
  AssertEquals('Filter should be empty for vmNone', '', FEffects.GetAudioOnlyFilter);
end;

procedure TTestVisualEffects.Test_GetAudioOnlyFilter_Spectrum_ContainsAsplit;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmSpectrum;
  AssertTrue('Should contain asplit', Pos('asplit', FEffects.GetAudioOnlyFilter) > 0);
end;

procedure TTestVisualEffects.Test_GetAudioOnlyFilter_Spectrum_ContainsAo;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmSpectrum;
  AssertTrue('Should contain [ao]', Pos('[ao]', FEffects.GetAudioOnlyFilter) > 0);
end;

procedure TTestVisualEffects.Test_GetAudioOnlyFilter_Spectrum_ContainsVo;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmSpectrum;
  AssertTrue('Should contain [vo]', Pos('[vo]', FEffects.GetAudioOnlyFilter) > 0);
end;

procedure TTestVisualEffects.Test_GetAudioOnlyFilter_Spectrum_ContainsOverlay;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmSpectrum;
  AssertTrue('Should contain overlay', Pos('overlay', FEffects.GetAudioOnlyFilter) > 0);
end;

procedure TTestVisualEffects.Test_GetAudioOnlyFilter_Volume_ContainsYOffset;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmVolume;
  AssertTrue('Should contain y offset', Pos('y=', FEffects.GetAudioOnlyFilter) > 0);
end;

procedure TTestVisualEffects.Test_GetAudioOnlyFilter_Combined;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmCombined;
  AssertTrue('Should contain vstack', Pos('vstack', FEffects.GetAudioOnlyFilter) > 0);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  NextMode / PreviousMode Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_NextMode_FromNone;
begin
  FEffects.Mode := vmNone;
  FEffects.NextMode;
  AssertEquals('Should advance to Spectrum', Ord(vmSpectrum), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_NextMode_FromSpectrum;
begin
  FEffects.Mode := vmSpectrum;
  FEffects.NextMode;
  AssertEquals('Should advance to Waves', Ord(vmWaves), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_NextMode_Wraps;
begin
  FEffects.Mode := vmCombined;
  FEffects.NextMode;
  AssertEquals('Should wrap to None', Ord(vmNone), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_PreviousMode_FromSpectrum;
begin
  FEffects.Mode := vmSpectrum;
  FEffects.PreviousMode;
  AssertEquals('Should go back to None', Ord(vmNone), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_PreviousMode_Wraps;
begin
  FEffects.Mode := vmNone;
  FEffects.PreviousMode;
  AssertEquals('Should wrap to Combined', Ord(vmCombined), Ord(FEffects.Mode));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  NextColorScheme Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_NextColorScheme_FromDefault;
begin
  FEffects.ColorScheme := vcsDefault;
  FEffects.NextColorScheme;
  AssertEquals('Should advance to Fire', Ord(vcsFire), Ord(FEffects.ColorScheme));
end;

procedure TTestVisualEffects.Test_NextColorScheme_Wraps;
begin
  FEffects.ColorScheme := vcsWhite;
  FEffects.NextColorScheme;
  AssertEquals('Should wrap to Default', Ord(vcsDefault), Ord(FEffects.ColorScheme));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  GetPresetNames Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_GetPresetNames_NotNil;
var
  Names: TStringList;
begin
  Names := FEffects.GetPresetNames;
  try
    AssertNotNull('Should return list', Names);
  finally
    Names.Free;
  end;
end;

procedure TTestVisualEffects.Test_GetPresetNames_Count;
var
  Names: TStringList;
begin
  Names := FEffects.GetPresetNames;
  try
    AssertEquals('Should have 8 presets', 8, Names.Count);
  finally
    Names.Free;
  end;
end;

procedure TTestVisualEffects.Test_GetPresetNames_ContainsClassicSpectrum;
var
  Names: TStringList;
begin
  Names := FEffects.GetPresetNames;
  try
    AssertTrue('Should contain Classic Spectrum', Names.IndexOf('Classic Spectrum') >= 0);
  finally
    Names.Free;
  end;
end;

procedure TTestVisualEffects.Test_GetPresetNames_ContainsFireSpectrum;
var
  Names: TStringList;
begin
  Names := FEffects.GetPresetNames;
  try
    AssertTrue('Should contain Fire Spectrum', Names.IndexOf('Fire Spectrum') >= 0);
  finally
    Names.Free;
  end;
end;

procedure TTestVisualEffects.Test_GetPresetNames_ContainsSpectrogram;
var
  Names: TStringList;
begin
  Names := FEffects.GetPresetNames;
  try
    AssertTrue('Should contain Spectrogram', Names.IndexOf('Spectrogram') >= 0);
  finally
    Names.Free;
  end;
end;

procedure TTestVisualEffects.Test_GetPresetNames_ContainsOscilloscope;
var
  Names: TStringList;
begin
  Names := FEffects.GetPresetNames;
  try
    AssertTrue('Should contain Oscilloscope', Names.IndexOf('Oscilloscope') >= 0);
  finally
    Names.Free;
  end;
end;

procedure TTestVisualEffects.Test_GetPresetNames_ContainsStereoScope;
var
  Names: TStringList;
begin
  Names := FEffects.GetPresetNames;
  try
    AssertTrue('Should contain Stereo Scope', Names.IndexOf('Stereo Scope') >= 0);
  finally
    Names.Free;
  end;
end;

procedure TTestVisualEffects.Test_GetPresetNames_ContainsVUMeter;
var
  Names: TStringList;
begin
  Names := FEffects.GetPresetNames;
  try
    AssertTrue('Should contain VU Meter', Names.IndexOf('VU Meter') >= 0);
  finally
    Names.Free;
  end;
end;

procedure TTestVisualEffects.Test_GetPresetNames_ContainsRainbowWaves;
var
  Names: TStringList;
begin
  Names := FEffects.GetPresetNames;
  try
    AssertTrue('Should contain Rainbow Waves', Names.IndexOf('Rainbow Waves') >= 0);
  finally
    Names.Free;
  end;
end;

procedure TTestVisualEffects.Test_GetPresetNames_ContainsMinimal;
var
  Names: TStringList;
begin
  Names := FEffects.GetPresetNames;
  try
    AssertTrue('Should contain Minimal', Names.IndexOf('Minimal') >= 0);
  finally
    Names.Free;
  end;
end;

{ ─────────────────────────────────────────────────────────────────────────────
  ApplyPreset Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_ApplyPreset_ClassicSpectrum_Mode;
begin
  FEffects.ApplyPreset('Classic Spectrum');
  AssertEquals('Mode should be Spectrum', Ord(vmSpectrum), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_ApplyPreset_ClassicSpectrum_SpectrumMode;
begin
  FEffects.ApplyPreset('Classic Spectrum');
  AssertEquals('SpectrumMode should be Vertical', Ord(smVertical), Ord(FEffects.SpectrumMode));
end;

procedure TTestVisualEffects.Test_ApplyPreset_ClassicSpectrum_ColorScheme;
begin
  FEffects.ApplyPreset('Classic Spectrum');
  AssertEquals('ColorScheme should be Green', Ord(vcsGreen), Ord(FEffects.ColorScheme));
end;

procedure TTestVisualEffects.Test_ApplyPreset_ClassicSpectrum_BarCount;
begin
  FEffects.ApplyPreset('Classic Spectrum');
  AssertEquals('BarCount should be 64', 64, FEffects.BarCount);
end;

procedure TTestVisualEffects.Test_ApplyPreset_ClassicSpectrum_Enabled;
begin
  FEffects.ApplyPreset('Classic Spectrum');
  AssertTrue('Should be enabled', FEffects.Enabled);
end;

procedure TTestVisualEffects.Test_ApplyPreset_FireSpectrum_Mode;
begin
  FEffects.ApplyPreset('Fire Spectrum');
  AssertEquals('Mode should be Spectrum', Ord(vmSpectrum), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_ApplyPreset_FireSpectrum_ColorScheme;
begin
  FEffects.ApplyPreset('Fire Spectrum');
  AssertEquals('ColorScheme should be Fire', Ord(vcsFire), Ord(FEffects.ColorScheme));
end;

procedure TTestVisualEffects.Test_ApplyPreset_FireSpectrum_BarCount;
begin
  FEffects.ApplyPreset('Fire Spectrum');
  AssertEquals('BarCount should be 128', 128, FEffects.BarCount);
end;

procedure TTestVisualEffects.Test_ApplyPreset_Spectrogram_SpectrumMode;
begin
  FEffects.ApplyPreset('Spectrogram');
  AssertEquals('SpectrumMode should be Scroll', Ord(smScroll), Ord(FEffects.SpectrumMode));
end;

procedure TTestVisualEffects.Test_ApplyPreset_Spectrogram_ColorScheme;
begin
  FEffects.ApplyPreset('Spectrogram');
  AssertEquals('ColorScheme should be Rainbow', Ord(vcsRainbow), Ord(FEffects.ColorScheme));
end;

procedure TTestVisualEffects.Test_ApplyPreset_Spectrogram_BarCount;
begin
  FEffects.ApplyPreset('Spectrogram');
  AssertEquals('BarCount should be 256', 256, FEffects.BarCount);
end;

procedure TTestVisualEffects.Test_ApplyPreset_Oscilloscope_Mode;
begin
  FEffects.ApplyPreset('Oscilloscope');
  AssertEquals('Mode should be Waves', Ord(vmWaves), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_ApplyPreset_Oscilloscope_WaveformMode;
begin
  FEffects.ApplyPreset('Oscilloscope');
  AssertEquals('WaveformMode should be Line', Ord(wmLine), Ord(FEffects.WaveformMode));
end;

procedure TTestVisualEffects.Test_ApplyPreset_StereoScope_Mode;
begin
  FEffects.ApplyPreset('Stereo Scope');
  AssertEquals('Mode should be Vector', Ord(vmVector), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_ApplyPreset_VUMeter_Mode;
begin
  FEffects.ApplyPreset('VU Meter');
  AssertEquals('Mode should be Volume', Ord(vmVolume), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_ApplyPreset_RainbowWaves_Mode;
begin
  FEffects.ApplyPreset('Rainbow Waves');
  AssertEquals('Mode should be Waves', Ord(vmWaves), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_ApplyPreset_RainbowWaves_WaveformMode;
begin
  FEffects.ApplyPreset('Rainbow Waves');
  AssertEquals('WaveformMode should be P2P', Ord(wmP2P), Ord(FEffects.WaveformMode));
end;

procedure TTestVisualEffects.Test_ApplyPreset_RainbowWaves_MirrorEffect;
begin
  FEffects.ApplyPreset('Rainbow Waves');
  AssertTrue('MirrorEffect should be True', FEffects.Settings.MirrorEffect);
end;

procedure TTestVisualEffects.Test_ApplyPreset_Minimal_WaveformMode;
begin
  FEffects.ApplyPreset('Minimal');
  AssertEquals('WaveformMode should be Cline', Ord(wmCline), Ord(FEffects.WaveformMode));
end;

procedure TTestVisualEffects.Test_ApplyPreset_Minimal_ColorScheme;
begin
  FEffects.ApplyPreset('Minimal');
  AssertEquals('ColorScheme should be White', Ord(vcsWhite), Ord(FEffects.ColorScheme));
end;

procedure TTestVisualEffects.Test_ApplyPreset_CaseInsensitive;
begin
  FEffects.ApplyPreset('CLASSIC SPECTRUM');
  AssertEquals('Should work case-insensitive', Ord(vmSpectrum), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_ApplyPreset_UnknownIgnored;
var
  OldMode: TVisualMode;
begin
  OldMode := FEffects.Mode;
  FEffects.ApplyPreset('Unknown Preset');
  { Unknown preset should still enable but not change mode }
  AssertEquals('Mode should not change for unknown preset', Ord(OldMode), Ord(FEffects.Mode));
end;

procedure TTestVisualEffects.Test_ApplyPreset_TriggersEvent;
begin
  FEffects.OnFilterChanged := @OnFilterChangedHandler;
  FEffects.ApplyPreset('Classic Spectrum');
  AssertEquals('Event should trigger', 1, FFilterChangedCount);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Filter Content Tests - Spectrum
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_SpectrumFilter_ContainsScale;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmSpectrum;
  AssertTrue('Should contain scale=log', Pos('scale=log', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_SpectrumFilter_ContainsColorChannel;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmSpectrum;
  FEffects.ColorScheme := vcsDefault;
  AssertTrue('Should contain color=channel', Pos('color=channel', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_SpectrumFilter_ContainsColorFire;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmSpectrum;
  FEffects.ColorScheme := vcsFire;
  AssertTrue('Should contain color=fire', Pos('color=fire', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_SpectrumFilter_ScrollMode;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmSpectrum;
  FEffects.SpectrumMode := smScroll;
  AssertTrue('Should contain slide=scroll', Pos('slide=scroll', FEffects.GetFilterString) > 0);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Filter Content Tests - Waves
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_WavesFilter_ContainsMode;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmWaves;
  AssertTrue('Should contain mode=', Pos('mode=', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_WavesFilter_PointMode;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmWaves;
  FEffects.WaveformMode := wmPoint;
  AssertTrue('Should contain mode=point', Pos('mode=point', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_WavesFilter_LineMode;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmWaves;
  FEffects.WaveformMode := wmLine;
  AssertTrue('Should contain mode=line', Pos('mode=line', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_WavesFilter_P2PMode;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmWaves;
  FEffects.WaveformMode := wmP2P;
  AssertTrue('Should contain mode=p2p', Pos('mode=p2p', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_WavesFilter_ClineMode;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmWaves;
  FEffects.WaveformMode := wmCline;
  AssertTrue('Should contain mode=cline', Pos('mode=cline', FEffects.GetFilterString) > 0);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Filter Content Tests - Vector
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_VectorFilter_ContainsLissajous;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmVector;
  AssertTrue('Should contain mode=lissajous', Pos('mode=lissajous', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_VectorFilter_ContainsScale;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmVector;
  AssertTrue('Should contain scale=log', Pos('scale=log', FEffects.GetFilterString) > 0);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Filter Content Tests - Histogram
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_HistogramFilter_ContainsSlide;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmHistogram;
  AssertTrue('Should contain slide=', Pos('slide=', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_HistogramFilter_ContainsScale;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmHistogram;
  AssertTrue('Should contain scale=log', Pos('scale=log', FEffects.GetFilterString) > 0);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Filter Content Tests - Volume
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_VolumeFilter_ContainsWidth;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmVolume;
  AssertTrue('Should contain w=', Pos('w=', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_VolumeFilter_ContainsHeight;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmVolume;
  AssertTrue('Should contain h=', Pos('h=', FEffects.GetFilterString) > 0);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Filter Content Tests - Combined
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_CombinedFilter_ContainsVstack;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmCombined;
  AssertTrue('Should contain vstack', Pos('vstack', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_CombinedFilter_ContainsShowspectrum;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmCombined;
  AssertTrue('Should contain showspectrum', Pos('showspectrum', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_CombinedFilter_ContainsShowwaves;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmCombined;
  AssertTrue('Should contain showwaves', Pos('showwaves', FEffects.GetFilterString) > 0);
end;

procedure TTestVisualEffects.Test_CombinedFilter_ContainsOverlay;
begin
  FEffects.Enabled := True;
  FEffects.Mode := vmCombined;
  AssertTrue('Should contain overlay', Pos('overlay', FEffects.GetFilterString) > 0);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Settings Property Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestVisualEffects.Test_Settings_Read;
var
  S: TVisualSettings;
begin
  S := FEffects.Settings;
  AssertEquals('Should read Width', 640, S.Width);
  AssertEquals('Should read Height', 360, S.Height);
end;

procedure TTestVisualEffects.Test_Settings_Write;
var
  S: TVisualSettings;
begin
  S := FEffects.Settings;
  S.Width := 1280;
  S.Height := 720;
  FEffects.Settings := S;
  AssertEquals('Should write Width', 1280, FEffects.Settings.Width);
  AssertEquals('Should write Height', 720, FEffects.Settings.Height);
end;

initialization
  RegisterTest(TTestVisualEffects);

end.
