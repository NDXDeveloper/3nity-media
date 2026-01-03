{ ═══════════════════════════════════════════════════════════════════════════════
  uVisualEffects.pas - Audio Visualization Effects

  Part of 3nity Media - Lazarus Edition

  This unit implements audio visualization effects using mpv's lavfi filters:
  - Spectrum analyzer (frequency bars)
  - Waveform display
  - Vector scope
  - Frequency histogram
  - Various color schemes and display modes

  Phase 26: Audio Visual Effects

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uVisualEffects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  { ═══════════════════════════════════════════════════════════════════════════
    VISUALIZATION TYPES
    ═══════════════════════════════════════════════════════════════════════════ }

  { Visualization mode }
  TVisualMode = (
    vmNone,           { No visualization }
    vmSpectrum,       { Spectrum analyzer (frequency bars) }
    vmWaves,          { Waveform display }
    vmVector,         { Vector scope (Lissajous) }
    vmHistogram,      { Audio histogram }
    vmVolume,         { VU meter / Volume bars }
    vmCombined        { Combined spectrum + waves }
  );

  { Spectrum display mode }
  TSpectrumMode = (
    smVertical,       { Vertical bars }
    smHorizontal,     { Horizontal bars }
    smScroll          { Scrolling spectrogram }
  );

  { Waveform display mode }
  TWaveformMode = (
    wmPoint,          { Point display }
    wmLine,           { Line display }
    wmP2P,            { Point to point }
    wmCline           { Centered line }
  );

  { Color scheme }
  TVisualColorScheme = (
    vcsDefault,       { Default colors }
    vcsFire,          { Fire colors (red/yellow) }
    vcsIce,           { Ice colors (blue/cyan) }
    vcsRainbow,       { Rainbow gradient }
    vcsGreen,         { Matrix green }
    vcsPurple,        { Purple haze }
    vcsWhite          { White/Gray }
  );

  { ═══════════════════════════════════════════════════════════════════════════
    VISUALIZATION SETTINGS
    ═══════════════════════════════════════════════════════════════════════════ }

  TVisualSettings = record
    Mode: TVisualMode;
    SpectrumMode: TSpectrumMode;
    WaveformMode: TWaveformMode;
    ColorScheme: TVisualColorScheme;
    { Size and position }
    Width: Integer;
    Height: Integer;
    { Spectrum settings }
    BarCount: Integer;        { Number of frequency bars (16-256) }
    BarWidth: Integer;        { Width of each bar in pixels }
    BarGap: Integer;          { Gap between bars }
    ShowPeaks: Boolean;       { Show peak indicators }
    PeakFalloff: Integer;     { Peak fall speed (1-10) }
    { Waveform settings }
    WaveScale: Double;        { Waveform amplitude scale }
    WaveSpeed: Integer;       { Scrolling speed for spectrogram }
    { General settings }
    Opacity: Integer;         { Overlay opacity (0-100) }
    BackgroundColor: TColor;  { Background color }
    ForegroundColor: TColor;  { Primary foreground color }
    EnableGlow: Boolean;      { Enable glow effect }
    MirrorEffect: Boolean;    { Mirror visualization }
    Smoothing: Integer;       { Smoothing factor (1-10) }
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    TVISUALEFFECTS CLASS
    ═══════════════════════════════════════════════════════════════════════════ }

  TVisualEffects = class
  private
    FEnabled: Boolean;
    FSettings: TVisualSettings;
    FFilterString: string;
    FOnFilterChanged: TNotifyEvent;

    procedure SetEnabled(Value: Boolean);
    procedure SetMode(Value: TVisualMode);
    procedure SetColorScheme(Value: TVisualColorScheme);
    procedure SetBarCount(Value: Integer);
    procedure SetSpectrumMode(Value: TSpectrumMode);
    procedure SetWaveformMode(Value: TWaveformMode);

    function BuildSpectrumFilter: string;
    function BuildWavesFilter: string;
    function BuildVectorFilter: string;
    function BuildHistogramFilter: string;
    function BuildVolumeFilter: string;
    function BuildCombinedFilter: string;
    function GetColorString: string;
    function GetModeString: string;
    procedure UpdateFilter;

  public
    constructor Create;
    destructor Destroy; override;

    { Initialize with default settings }
    procedure Initialize;

    { Reset to defaults }
    procedure Reset;

    { Build the lavfi filter string for mpv }
    function GetFilterString: string;

    { Get lavfi-complex filter for audio-only visualization }
    function GetAudioOnlyFilter: string;

    { Cycle through visualization modes }
    procedure NextMode;
    procedure PreviousMode;

    { Cycle through color schemes }
    procedure NextColorScheme;

    { Presets }
    procedure ApplyPreset(const PresetName: string);
    function GetPresetNames: TStringList;

    { Properties }
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Mode: TVisualMode read FSettings.Mode write SetMode;
    property ColorScheme: TVisualColorScheme read FSettings.ColorScheme write SetColorScheme;
    property SpectrumMode: TSpectrumMode read FSettings.SpectrumMode write SetSpectrumMode;
    property WaveformMode: TWaveformMode read FSettings.WaveformMode write SetWaveformMode;
    property BarCount: Integer read FSettings.BarCount write SetBarCount;
    property Settings: TVisualSettings read FSettings write FSettings;
    property FilterString: string read FFilterString;

    { Events }
    property OnFilterChanged: TNotifyEvent read FOnFilterChanged write FOnFilterChanged;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    HELPER FUNCTIONS
    ═══════════════════════════════════════════════════════════════════════════ }

function VisualModeToString(Mode: TVisualMode): string;
function StringToVisualMode(const S: string): TVisualMode;
function ColorSchemeToString(Scheme: TVisualColorScheme): string;
function StringToColorScheme(const S: string): TVisualColorScheme;

implementation

{ ═══════════════════════════════════════════════════════════════════════════════
  HELPER FUNCTIONS
  ═══════════════════════════════════════════════════════════════════════════════ }

function VisualModeToString(Mode: TVisualMode): string;
begin
  case Mode of
    vmNone:      Result := 'None';
    vmSpectrum:  Result := 'Spectrum';
    vmWaves:     Result := 'Waveform';
    vmVector:    Result := 'Vector';
    vmHistogram: Result := 'Histogram';
    vmVolume:    Result := 'Volume';
    vmCombined:  Result := 'Combined';
  else
    Result := 'None';
  end;
end;

function StringToVisualMode(const S: string): TVisualMode;
var
  LowerS: string;
begin
  LowerS := LowerCase(S);
  if LowerS = 'spectrum' then Result := vmSpectrum
  else if LowerS = 'waveform' then Result := vmWaves
  else if LowerS = 'waves' then Result := vmWaves
  else if LowerS = 'vector' then Result := vmVector
  else if LowerS = 'histogram' then Result := vmHistogram
  else if LowerS = 'volume' then Result := vmVolume
  else if LowerS = 'combined' then Result := vmCombined
  else Result := vmNone;
end;

function ColorSchemeToString(Scheme: TVisualColorScheme): string;
begin
  case Scheme of
    vcsDefault:  Result := 'Default';
    vcsFire:     Result := 'Fire';
    vcsIce:      Result := 'Ice';
    vcsRainbow:  Result := 'Rainbow';
    vcsGreen:    Result := 'Green';
    vcsPurple:   Result := 'Purple';
    vcsWhite:    Result := 'White';
  else
    Result := 'Default';
  end;
end;

function StringToColorScheme(const S: string): TVisualColorScheme;
var
  LowerS: string;
begin
  LowerS := LowerCase(S);
  if LowerS = 'fire' then Result := vcsFire
  else if LowerS = 'ice' then Result := vcsIce
  else if LowerS = 'rainbow' then Result := vcsRainbow
  else if LowerS = 'green' then Result := vcsGreen
  else if LowerS = 'purple' then Result := vcsPurple
  else if LowerS = 'white' then Result := vcsWhite
  else Result := vcsDefault;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TVISUALEFFECTS IMPLEMENTATION
  ═══════════════════════════════════════════════════════════════════════════════ }

constructor TVisualEffects.Create;
begin
  inherited Create;
  Initialize;
end;

destructor TVisualEffects.Destroy;
begin
  inherited Destroy;
end;

procedure TVisualEffects.Initialize;
begin
  FEnabled := False;
  FFilterString := '';

  with FSettings do
  begin
    Mode := vmNone;
    SpectrumMode := smVertical;
    WaveformMode := wmLine;
    ColorScheme := vcsDefault;
    Width := 640;
    Height := 360;
    BarCount := 64;
    BarWidth := 8;
    BarGap := 2;
    ShowPeaks := True;
    PeakFalloff := 3;
    WaveScale := 1.0;
    WaveSpeed := 1;
    Opacity := 100;
    BackgroundColor := clBlack;
    ForegroundColor := clLime;
    EnableGlow := False;
    MirrorEffect := False;
    Smoothing := 5;
  end;
end;

procedure TVisualEffects.Reset;
begin
  Initialize;
  UpdateFilter;
end;

procedure TVisualEffects.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    UpdateFilter;
  end;
end;

procedure TVisualEffects.SetMode(Value: TVisualMode);
begin
  if FSettings.Mode <> Value then
  begin
    FSettings.Mode := Value;
    UpdateFilter;
  end;
end;

procedure TVisualEffects.SetColorScheme(Value: TVisualColorScheme);
begin
  if FSettings.ColorScheme <> Value then
  begin
    FSettings.ColorScheme := Value;
    UpdateFilter;
  end;
end;

procedure TVisualEffects.SetBarCount(Value: Integer);
begin
  if Value < 8 then Value := 8;
  if Value > 512 then Value := 512;
  if FSettings.BarCount <> Value then
  begin
    FSettings.BarCount := Value;
    UpdateFilter;
  end;
end;

procedure TVisualEffects.SetSpectrumMode(Value: TSpectrumMode);
begin
  if FSettings.SpectrumMode <> Value then
  begin
    FSettings.SpectrumMode := Value;
    UpdateFilter;
  end;
end;

procedure TVisualEffects.SetWaveformMode(Value: TWaveformMode);
begin
  if FSettings.WaveformMode <> Value then
  begin
    FSettings.WaveformMode := Value;
    UpdateFilter;
  end;
end;

function TVisualEffects.GetColorString: string;
begin
  { Returns the FFmpeg color mode string for showspectrum/showwaves }
  case FSettings.ColorScheme of
    vcsDefault:  Result := 'channel';
    vcsFire:     Result := 'fire';
    vcsIce:      Result := 'cool';
    vcsRainbow:  Result := 'rainbow';
    vcsGreen:    Result := 'green';
    vcsPurple:   Result := 'magenta';
    vcsWhite:    Result := 'white';
  else
    Result := 'channel';
  end;
end;

function TVisualEffects.GetModeString: string;
begin
  { Returns the display mode string for showspectrum }
  case FSettings.SpectrumMode of
    smVertical:   Result := 'combined';
    smHorizontal: Result := 'separate';
    smScroll:     Result := 'scroll';
  else
    Result := 'combined';
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  BuildSpectrumFilter - Create FFmpeg showspectrum filter string

  Purpose: Generates the filter string for spectrum analyzer visualization
           using FFmpeg's showspectrum audio filter.

  Returns: FFmpeg filter string like "showspectrum=s=1280x720:mode=combined:..."

  FFmpeg filter syntax:
    showspectrum=s=WIDTHxHEIGHT:mode=MODE:color=COLOR:slide=SLIDE:scale=log

  Parameters mapped from FSettings:
    - Width, Height: Display dimensions
    - SpectrumMode → slide: 'replace' for static, 'scroll' for scrolling
    - ColorScheme → color: Color palette name
    - mode: Determined by GetModeString (combined, separate, etc.)

  Notes:
    - Uses logarithmic scale (scale=log) for better low-frequency visibility
    - Slide mode controls how new data appears (replace vs scroll)
    - Optional frequency scale (fscale=log) when Smoothing > 1
  ═══════════════════════════════════════════════════════════════════════════ }
function TVisualEffects.BuildSpectrumFilter: string;
var
  SlideMode: string;
begin
  { Build showspectrum filter }
  { Syntax: showspectrum=s=WIDTHxHEIGHT:mode=MODE:color=COLOR:slide=SLIDE }

  case FSettings.SpectrumMode of
    smVertical:   SlideMode := 'replace';
    smHorizontal: SlideMode := 'replace';
    smScroll:     SlideMode := 'scroll';
  else
    SlideMode := 'replace';
  end;

  Result := Format('showspectrum=s=%dx%d:mode=%s:color=%s:slide=%s:scale=log',
    [FSettings.Width, FSettings.Height, GetModeString, GetColorString, SlideMode]);

  { Add smoothing }
  if FSettings.Smoothing > 1 then
    Result := Result + Format(':fscale=log', []);
end;

function TVisualEffects.BuildWavesFilter: string;
var
  WaveMode: string;
begin
  { Build showwaves filter for waveform display }
  { Match the style used in vmCombined for consistency }
  case FSettings.WaveformMode of
    wmPoint: WaveMode := 'point';
    wmLine:  WaveMode := 'line';
    wmP2P:   WaveMode := 'p2p';
    wmCline: WaveMode := 'cline';
  else
    WaveMode := 'line';
  end;

  Result := Format('showwaves=s=%dx%d:mode=%s:colors=%s',
    [FSettings.Width, FSettings.Height, WaveMode, GetColorString]);
end;

function TVisualEffects.BuildVectorFilter: string;
begin
  { Build avectorscope filter (Lissajous display) }
  { Syntax: avectorscope=s=WIDTHxHEIGHT:mode=MODE:draw=MODE }

  Result := Format('avectorscope=s=%dx%d:mode=lissajous:draw=line:scale=log:rc=2:gc=200:bc=10',
    [FSettings.Width, FSettings.Height]);
end;

function TVisualEffects.BuildHistogramFilter: string;
begin
  { Build ahistogram filter }
  { Syntax: ahistogram=s=WIDTHxHEIGHT:slide=MODE:scale=MODE }
  { Using simpler parameters for better compatibility }

  Result := Format('ahistogram=s=%dx%d:slide=replace:scale=log',
    [FSettings.Width, FSettings.Height]);
end;

function TVisualEffects.BuildVolumeFilter: string;
begin
  { Build showvolume filter (VU meter style) }
  { Syntax: showvolume=w=WIDTH:h=HEIGHT }
  { Using simple parameters to avoid filter parsing issues }

  Result := Format('showvolume=w=%d:h=%d:f=0.5:b=4:m=p',
    [FSettings.Width, FSettings.Height div 3]);
end;

{ ═══════════════════════════════════════════════════════════════════════════
  BuildCombinedFilter - Create stacked spectrum + waveform visualization

  Purpose: Generates a complex filter that displays both spectrum analyzer
           and waveform simultaneously, stacked vertically.

  Returns: FFmpeg filter_complex string for MPV's lavfi-complex option

  Filter graph structure:
    [aid1] ─┬─► [ao] (audio output - preserved for playback)
            ├─► [a1] ─► showspectrum ─► [v1] ─┐
            └─► [a2] ─► showwaves ───► [v2] ─┴─► vstack ─► [fg]
    color=black ─────────────────────────► [bg] ─┴─► overlay ─► [vo]

  Key elements:
    - asplit=3: Splits audio into 3 copies (audio out + 2 visualizations)
    - vstack: Vertically stacks spectrum (top) and waves (bottom)
    - color=black: Creates solid background for GPU video output mode
    - overlay=shortest=1: Combines background with stacked visualizations

  Dimensions:
    - Each visualization: Width × (Height ÷ 2)
    - Total output: Width × Height
  ═══════════════════════════════════════════════════════════════════════════ }
function TVisualEffects.BuildCombinedFilter: string;
begin
  { Combined: spectrum on top, waves on bottom }
  { Use filter_complex to stack them }
  { IMPORTANT: Use asplit=3 to preserve audio output [ao] alongside video }
  { Black background ensures proper display in GPU video output mode }

  Result := Format(
    '[aid1] asplit=3 [ao][a1][a2]; ' +
    'color=black:s=%dx%d [bg]; ' +
    '[a1] showspectrum=s=%dx%d:mode=combined:color=%s:slide=replace [v1]; ' +
    '[a2] showwaves=s=%dx%d:mode=line:colors=%s [v2]; ' +
    '[v1][v2] vstack [fg]; [bg][fg] overlay=shortest=1 [vo]',
    [FSettings.Width, FSettings.Height,
     FSettings.Width, FSettings.Height div 2, GetColorString,
     FSettings.Width, FSettings.Height div 2, GetColorString]);
end;

procedure TVisualEffects.UpdateFilter;
begin
  if not FEnabled then
  begin
    FFilterString := '';
  end
  else
  begin
    case FSettings.Mode of
      vmNone:      FFilterString := '';
      vmSpectrum:  FFilterString := BuildSpectrumFilter;
      vmWaves:     FFilterString := BuildWavesFilter;
      vmVector:    FFilterString := BuildVectorFilter;
      vmHistogram: FFilterString := BuildHistogramFilter;
      vmVolume:    FFilterString := BuildVolumeFilter;
      vmCombined:  FFilterString := BuildCombinedFilter;
    else
      FFilterString := '';
    end;
  end;

  if Assigned(FOnFilterChanged) then
    FOnFilterChanged(Self);
end;

function TVisualEffects.GetFilterString: string;
begin
  Result := FFilterString;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  GetAudioOnlyFilter - Convert audio stream to video visualization

  Purpose: Generates a complete lavfi-complex filter string that creates
           video output from an audio-only file while preserving audio.

  Returns: MPV lavfi-complex filter string, or empty if disabled

  Filter pattern for single visualizations:
    [aid1] asplit [ao][a]; color=black:s=WxH [bg]; [a] <filter> [fg]; [bg][fg] overlay [vo]

  Components:
    - [aid1]: Input audio stream
    - asplit: Splits audio → [ao] (playback) + [a] (visualization)
    - color=black: Creates solid background for GPU video mode
    - <filter>: Visualization filter (spectrum, waves, vector, etc.)
    - overlay: Combines background and visualization
    - repeatlast=0: Fixes EOF hanging (MPV issue #7266)

  Special cases:
    - vmVolume: Adds y offset to center the VU meter bars
    - vmCombined: Uses BuildCombinedFilter for dual visualization

  Notes:
    - Returns empty string if not enabled or mode is vmNone
    - The [ao] label is critical for audio output to continue
    - The [vo] label provides video output to MPV
  ═══════════════════════════════════════════════════════════════════════════ }
function TVisualEffects.GetAudioOnlyFilter: string;
begin
  { For audio-only files, we need lavfi-complex to create video from audio }
  { IMPORTANT: We must use asplit to preserve audio output [ao] alongside video [vo] }
  if not FEnabled or (FSettings.Mode = vmNone) then
  begin
    Result := '';
    Exit;
  end;

  { Build lavfi-complex filter that creates video from audio while preserving audio output }
  { Format: [aid1] asplit [ao][a]; color=black:s=WxH [bg]; [a] <vis_filter> [fg]; [bg][fg] overlay [vo] }
  { The black background ensures proper display in GPU video output mode }
  { repeatlast=0 fixes EOF hanging issue (see MPV issue #7266) }
  case FSettings.Mode of
    vmSpectrum:
      Result := Format('[aid1] asplit [ao][a]; color=black:s=%dx%d [bg]; [a] %s [fg]; [bg][fg] overlay=repeatlast=0 [vo]',
        [FSettings.Width, FSettings.Height, BuildSpectrumFilter]);
    vmWaves:
      Result := Format('[aid1] asplit [ao][a]; color=black:s=%dx%d [bg]; [a] %s [fg]; [bg][fg] overlay=repeatlast=0 [vo]',
        [FSettings.Width, FSettings.Height, BuildWavesFilter]);
    vmVector:
      Result := Format('[aid1] asplit [ao][a]; color=black:s=%dx%d [bg]; [a] %s [fg]; [bg][fg] overlay=repeatlast=0 [vo]',
        [FSettings.Width, FSettings.Height, BuildVectorFilter]);
    vmHistogram:
      Result := Format('[aid1] asplit [ao][a]; color=black:s=%dx%d [bg]; [a] %s [fg]; [bg][fg] overlay=repeatlast=0 [vo]',
        [FSettings.Width, FSettings.Height, BuildHistogramFilter]);
    vmVolume:
      { Center volume bar vertically: for stereo (2 channels), total bar height = h * 2 }
      { y = (total_height - 2 * bar_height) / 2 = Height/6 }
      Result := Format('[aid1] asplit [ao][a]; color=black:s=%dx%d [bg]; [a] %s [fg]; [bg][fg] overlay=repeatlast=0:y=%d [vo]',
        [FSettings.Width, FSettings.Height, BuildVolumeFilter,
         FSettings.Height div 6]);
    vmCombined:
      Result := BuildCombinedFilter;
  else
    Result := '';
  end;
end;

procedure TVisualEffects.NextMode;
begin
  if FSettings.Mode = High(TVisualMode) then
    Mode := Low(TVisualMode)
  else
    Mode := Succ(FSettings.Mode);
end;

procedure TVisualEffects.PreviousMode;
begin
  if FSettings.Mode = Low(TVisualMode) then
    Mode := High(TVisualMode)
  else
    Mode := Pred(FSettings.Mode);
end;

procedure TVisualEffects.NextColorScheme;
begin
  if FSettings.ColorScheme = High(TVisualColorScheme) then
    ColorScheme := Low(TVisualColorScheme)
  else
    ColorScheme := Succ(FSettings.ColorScheme);
end;

{ ═══════════════════════════════════════════════════════════════════════════
  ApplyPreset - Apply a predefined visualization configuration

  Purpose: Quickly configures the visualization with commonly used settings.
           Provides user-friendly presets that combine mode, color, and options.

  Parameters:
    - PresetName: Case-insensitive preset name

  Available presets:
    - "Classic Spectrum": Green vertical spectrum, 64 bars
    - "Fire Spectrum": Fire-colored vertical spectrum, 128 bars
    - "Spectrogram": Rainbow scrolling spectrum, 256 bars
    - "Oscilloscope": Green line waveform
    - "Stereo Scope": Green Lissajous/vector display
    - "VU Meter": Default-colored volume display
    - "Rainbow Waves": Rainbow P2P waveform with mirror effect
    - "Minimal": White center-line waveform, no mirror

  Notes:
    - Uses LowerCase comparison for case-insensitive matching
    - Automatically enables visualization (FEnabled := True)
    - Calls UpdateFilter to apply changes immediately
    - Unknown preset names are silently ignored
    - Use GetPresetNames to get list of available presets
  ═══════════════════════════════════════════════════════════════════════════ }
procedure TVisualEffects.ApplyPreset(const PresetName: string);
var
  LowerName: string;
begin
  LowerName := LowerCase(PresetName);

  if LowerName = 'classic spectrum' then
  begin
    FSettings.Mode := vmSpectrum;
    FSettings.SpectrumMode := smVertical;
    FSettings.ColorScheme := vcsGreen;
    FSettings.BarCount := 64;
  end
  else if LowerName = 'fire spectrum' then
  begin
    FSettings.Mode := vmSpectrum;
    FSettings.SpectrumMode := smVertical;
    FSettings.ColorScheme := vcsFire;
    FSettings.BarCount := 128;
  end
  else if LowerName = 'spectrogram' then
  begin
    FSettings.Mode := vmSpectrum;
    FSettings.SpectrumMode := smScroll;
    FSettings.ColorScheme := vcsRainbow;
    FSettings.BarCount := 256;
  end
  else if LowerName = 'oscilloscope' then
  begin
    FSettings.Mode := vmWaves;
    FSettings.WaveformMode := wmLine;
    FSettings.ColorScheme := vcsGreen;
  end
  else if LowerName = 'stereo scope' then
  begin
    FSettings.Mode := vmVector;
    FSettings.ColorScheme := vcsGreen;
  end
  else if LowerName = 'vu meter' then
  begin
    FSettings.Mode := vmVolume;
    FSettings.ColorScheme := vcsDefault;
  end
  else if LowerName = 'rainbow waves' then
  begin
    FSettings.Mode := vmWaves;
    FSettings.WaveformMode := wmP2P;
    FSettings.ColorScheme := vcsRainbow;
    FSettings.MirrorEffect := True;
  end
  else if LowerName = 'minimal' then
  begin
    FSettings.Mode := vmWaves;
    FSettings.WaveformMode := wmCline;
    FSettings.ColorScheme := vcsWhite;
    FSettings.MirrorEffect := False;
  end;

  FEnabled := True;
  UpdateFilter;
end;

function TVisualEffects.GetPresetNames: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('Classic Spectrum');
  Result.Add('Fire Spectrum');
  Result.Add('Spectrogram');
  Result.Add('Oscilloscope');
  Result.Add('Stereo Scope');
  Result.Add('VU Meter');
  Result.Add('Rainbow Waves');
  Result.Add('Minimal');
end;

end.
