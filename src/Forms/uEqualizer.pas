{ ═══════════════════════════════════════════════════════════════════════════════
  uEqualizer.pas - 10-Band Equalizer Form

  Part of 3nity Media - Lazarus Edition

  This form provides a 10-band graphic equalizer with preset support.
  Frequencies: 31Hz, 62Hz, 125Hz, 250Hz, 500Hz, 1kHz, 2kHz, 4kHz, 8kHz, 16kHz

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uEqualizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, Math, LCLType, uTypes, uMPVEngine, uLocale;

const
  { Equalizer band frequencies }
  EQ_BAND_COUNT = 10;
  EQ_FREQUENCIES: array[0..EQ_BAND_COUNT-1] of string = (
    '31', '62', '125', '250', '500', '1k', '2k', '4k', '8k', '16k'
  );

  { dB range }
  EQ_MIN_DB = -12;
  EQ_MAX_DB = 12;
  EQ_SLIDER_RANGE = 240; { -12 to +12 with 0.1 precision }

type
  { TfrmEqualizer }
  TfrmEqualizer = class(TForm)
    { Top panel with enable and presets }
    pnlTop: TPanel;
    chkEnabled: TCheckBox;
    lblPreset: TLabel;
    cmbPreset: TComboBox;
    btnSavePreset: TSpeedButton;
    btnDeletePreset: TSpeedButton;
    btnReset: TButton;

    { Bands panel }
    pnlBands: TPanel;

    { Bottom panel with preamp }
    pnlBottom: TPanel;
    lblPreamp: TLabel;
    tbPreamp: TTrackBar;
    lblPreampValue: TLabel;

    { Status bar }
    StatusBar: TStatusBar;

    { Band controls - created dynamically }

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure chkEnabledChange(Sender: TObject);
    procedure cmbPresetChange(Sender: TObject);
    procedure btnSavePresetClick(Sender: TObject);
    procedure btnDeletePresetClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure tbPreampChange(Sender: TObject);
    procedure tbPreampMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  private
    FMPVEngine: TMPVEngine;
    FPresets: TEqualizerPresets;
    FBandSliders: array[0..EQ_BAND_COUNT-1] of TTrackBar;
    FBandLabels: array[0..EQ_BAND_COUNT-1] of TLabel;
    FBandValueLabels: array[0..EQ_BAND_COUNT-1] of TLabel;
    FUpdating: Boolean;
    FModified: Boolean;
    FOnEqualizerChange: TNotifyEvent;
    FApplyTimer: TTimer;
    FPendingApply: Boolean;

    procedure CreateBandControls;
    procedure BandSliderChange(Sender: TObject);
    procedure BandSliderEnter(Sender: TObject);
    procedure BandSliderMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UpdateBandValueLabel(BandIndex: Integer);
    procedure ApplyCurrentSettings;
    procedure ApplyTimerEvent(Sender: TObject);
    procedure ScheduleApply;
    procedure LoadPresets;
    procedure SavePresets;
    procedure ApplyPreset(PresetIndex: Integer);
    function GetCurrentValues: string;
    procedure SetBandValues(const Values: string);
    function SliderToDb(APosition: Integer): Double;
    function DbToSlider(dB: Double): Integer;
    procedure DoEqualizerChange;

  public
    property MPVEngine: TMPVEngine read FMPVEngine write FMPVEngine;
    property OnEqualizerChange: TNotifyEvent read FOnEqualizerChange write FOnEqualizerChange;

    procedure ApplyLocale;
  end;

var
  frmEqualizer: TfrmEqualizer;

{ Helper procedure to save equalizer values to config }
procedure SaveEqualizerToConfig(const Values: string);

const
  { Default presets }
  DEFAULT_PRESETS: array[0..9] of record
    Name: string;
    Values: string;
  end = (
    (Name: 'Flat'; Values: '0:0:0:0:0:0:0:0:0:0'),
    (Name: 'Rock'; Values: '4:3:2:0:-1:0:2:3:4:4'),
    (Name: 'Pop'; Values: '-1:1:3:4:3:1:-1:-2:-2:-1'),
    (Name: 'Jazz'; Values: '3:2:1:2:-2:-2:0:1:2:3'),
    (Name: 'Classical'; Values: '4:3:2:1:-1:-1:0:2:3:4'),
    (Name: 'Dance'; Values: '5:4:2:0:0:-2:0:2:4:5'),
    (Name: 'Bass Boost'; Values: '6:5:4:2:0:0:0:0:0:0'),
    (Name: 'Treble Boost'; Values: '0:0:0:0:0:0:2:4:5:6'),
    (Name: 'Vocal'; Values: '-2:-1:0:2:4:4:3:1:0:-2'),
    (Name: 'Loudness'; Values: '4:3:0:-1:0:0:-1:0:3:4')
  );

implementation

{$R *.lfm}

uses
  uConfig;

{ ═══════════════════════════════════════════════════════════════════════════════
  FORM EVENTS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmEqualizer.FormCreate(Sender: TObject);
begin
  FUpdating := False;
  FModified := False;
  FPendingApply := False;
  SetLength(FPresets, 0);

  { Create debounce timer for smooth slider operation }
  FApplyTimer := TTimer.Create(Self);
  FApplyTimer.Enabled := False;
  FApplyTimer.Interval := 150;  { 150ms debounce }
  FApplyTimer.OnTimer := @ApplyTimerEvent;

  { Create band slider controls }
  CreateBandControls;

  { Load presets }
  LoadPresets;

  ApplyLocale;
end;

procedure TfrmEqualizer.ApplyLocale;
begin
  Caption := _T('Equalizer', 'Title', 'Equalizer');

  { Top panel }
  chkEnabled.Caption := _T('Equalizer', 'Enable', 'Enable Equalizer');
  lblPreset.Caption := _T('Equalizer', 'Presets', 'Presets:');
  btnSavePreset.Caption := _T('Equalizer', 'Save', 'Save');
  btnSavePreset.Hint := _T('Equalizer', 'SavePreset', 'Save Preset');
  btnDeletePreset.Caption := _T('Equalizer', 'Delete', 'Delete');
  btnDeletePreset.Hint := _T('Equalizer', 'DeletePreset', 'Delete Preset');
  btnReset.Caption := _T('Equalizer', 'Reset', 'Reset');

  { Bottom panel }
  lblPreamp.Caption := _T('Equalizer', 'Preamp', 'Preamp:');

  { Status bar }
  StatusBar.SimpleText := _T('Equalizer', 'Ready', 'Ready');
end;

procedure TfrmEqualizer.FormDestroy(Sender: TObject);
begin
  SetLength(FPresets, 0);
end;

procedure TfrmEqualizer.FormShow(Sender: TObject);
begin
  ApplyLocale;

  FUpdating := True;
  try
    { Initialize from MPV engine state }
    if FMPVEngine <> nil then
    begin
      chkEnabled.Checked := FMPVEngine.EqualizerEnabled;

      { Load current band values from config }
      if Config.Settings.EqualizerEnabled then
      begin
        SetBandValues(GetCurrentValues);
      end;
    end;
  finally
    FUpdating := False;
  end;
end;

procedure TfrmEqualizer.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  { Save presets if modified }
  if FModified then
    SavePresets;

  CloseAction := caHide;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  CONTROL EVENTS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmEqualizer.chkEnabledChange(Sender: TObject);
var
  TmpSettings: TAppSettings;
begin
  if FUpdating then Exit;

  if FMPVEngine <> nil then
  begin
    FMPVEngine.EnableEqualizer(chkEnabled.Checked);

    { Save state to config }
    TmpSettings := Config.Settings;
    TmpSettings.EqualizerEnabled := chkEnabled.Checked;
    Config.Settings := TmpSettings;
    Config.Modified := True;
  end;

  { Enable/disable band sliders }
  pnlBands.Enabled := chkEnabled.Checked;
  tbPreamp.Enabled := chkEnabled.Checked;

  { Notify main form to update visualization filter if active }
  DoEqualizerChange;
end;

procedure TfrmEqualizer.cmbPresetChange(Sender: TObject);
begin
  if FUpdating then Exit;

  if cmbPreset.ItemIndex > 0 then
    ApplyPreset(cmbPreset.ItemIndex - 1);  { -1 because first item is "(Custom)" }
end;

procedure TfrmEqualizer.btnSavePresetClick(Sender: TObject);
var
  PresetName: string;
  I, J: Integer;
  NewPreset: TEqualizerPreset;
begin
  PresetName := '';
  if InputQuery(_T('Equalizer', 'SavePresetTitle', 'Save Preset'),
     _T('Equalizer', 'PresetNamePrompt', 'Preset name:'), PresetName) then
  begin
    if PresetName = '' then
    begin
      ShowMessage(_T('Equalizer', 'EnterPresetName', 'Please enter a preset name.'));
      Exit;
    end;

    { Check if preset already exists }
    for I := 0 to High(FPresets) do
    begin
      if SameText(FPresets[I].Name, PresetName) then
      begin
        if MessageDlg(_T('Equalizer', 'OverwritePresetTitle', 'Overwrite Preset'),
          _T('Equalizer', 'OverwritePresetConfirm', 'A preset with this name already exists. Overwrite?'),
          mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          { Update existing preset - use separate loop variable }
          for J := 0 to EQ_BAND_COUNT - 1 do
            FPresets[I].Bands[J] := SliderToDb(FBandSliders[J].Position);
          FModified := True;
          StatusBar.SimpleText := Format(_T('Equalizer', 'PresetUpdated', 'Preset updated: %s'), [PresetName]);
          Exit;
        end
        else
          Exit;
      end;
    end;

    { Create new preset }
    NewPreset.Name := PresetName;
    for I := 0 to EQ_BAND_COUNT - 1 do
      NewPreset.Bands[I] := SliderToDb(FBandSliders[I].Position);

    SetLength(FPresets, Length(FPresets) + 1);
    FPresets[High(FPresets)] := NewPreset;

    { Update combo box }
    cmbPreset.Items.Add(PresetName);

    FModified := True;
    StatusBar.SimpleText := Format(_T('Equalizer', 'PresetSaved', 'Preset saved: %s'), [PresetName]);
  end;
end;

procedure TfrmEqualizer.btnDeletePresetClick(Sender: TObject);
var
  I, PresetIndex: Integer;
begin
  if cmbPreset.ItemIndex <= 0 then
  begin
    ShowMessage(_T('Equalizer', 'SelectPresetToDelete', 'Please select a preset to delete.'));
    Exit;
  end;

  PresetIndex := cmbPreset.ItemIndex - 1;

  { Don't allow deleting default presets }
  if PresetIndex < Length(DEFAULT_PRESETS) then
  begin
    ShowMessage(_T('Equalizer', 'CannotDeleteDefault', 'Cannot delete default presets.'));
    Exit;
  end;

  if MessageDlg(_T('Equalizer', 'DeletePresetTitle', 'Delete Preset'),
    _T('Equalizer', 'DeletePresetConfirm', 'Are you sure you want to delete this preset?'),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    { Remove from array }
    for I := PresetIndex to High(FPresets) - 1 do
      FPresets[I] := FPresets[I + 1];
    SetLength(FPresets, Length(FPresets) - 1);

    { Remove from combo box }
    cmbPreset.Items.Delete(cmbPreset.ItemIndex);
    cmbPreset.ItemIndex := 0;

    FModified := True;
    StatusBar.SimpleText := _T('Equalizer', 'PresetDeleted', 'Preset deleted');
  end;
end;

procedure TfrmEqualizer.btnResetClick(Sender: TObject);
var
  I: Integer;
begin
  FUpdating := True;
  try
    { Reset all bands to 0 }
    for I := 0 to EQ_BAND_COUNT - 1 do
    begin
      FBandSliders[I].Position := EQ_SLIDER_RANGE div 2; { Center = 0dB }
      UpdateBandValueLabel(I);
    end;

    { Reset preamp }
    tbPreamp.Position := 100;
    lblPreampValue.Caption := '0 dB';

    { Select flat preset }
    cmbPreset.ItemIndex := 1; { "Flat" }
  finally
    FUpdating := False;
  end;

  ApplyCurrentSettings;
  StatusBar.SimpleText := _T('Equalizer', 'ResetToFlat', 'Equalizer reset to flat');
end;

procedure TfrmEqualizer.tbPreampChange(Sender: TObject);
var
  dB: Double;
begin
  if FUpdating then Exit;

  dB := (tbPreamp.Position - 100) / 10.0;
  lblPreampValue.Caption := Format('%.1f dB', [dB]);

  { Store preamp value and schedule debounced apply }
  if FMPVEngine <> nil then
  begin
    FMPVEngine.Preamp := dB;
    ScheduleApply;
  end;
end;

procedure TfrmEqualizer.tbPreampMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  { Wheel up = increase preamp, wheel down = decrease preamp }
  if WheelDelta > 0 then
    tbPreamp.Position := tbPreamp.Position + 5
  else
    tbPreamp.Position := tbPreamp.Position - 5;

  Handled := True;
end;

procedure TfrmEqualizer.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Slider: TTrackBar;
  Step, BandIndex, NewIndex: Integer;
begin
  { Handle arrow keys for TTrackBar sliders manually (GTK2 focus issue) }
  if (ActiveControl is TTrackBar) then
  begin
    Slider := TTrackBar(ActiveControl);

    { Handle vertical band sliders }
    if Slider.Orientation = trVertical then
    begin
      BandIndex := Slider.Tag;
      Step := 5;  { 0.5 dB step }
      if ssShift in Shift then
        Step := 20;  { 2 dB step with Shift }

      case Key of
        VK_UP:
          begin
            { Up = increase dB value (visually slider moves up)
              In GTK2 vertical TTrackBar, Min is at TOP, Max is at BOTTOM
              So we DECREASE Position to move slider UP visually }
            Slider.Position := Max(Slider.Min, Slider.Position - Step);
            Key := 0;
          end;
        VK_DOWN:
          begin
            { Down = decrease dB value (visually slider moves down) }
            Slider.Position := Min(Slider.Max, Slider.Position + Step);
            Key := 0;
          end;
        VK_LEFT:
          begin
            { Left = move to previous band slider }
            NewIndex := BandIndex - 1;
            if NewIndex >= 0 then
            begin
              Self.ActiveControl := FBandSliders[NewIndex];
              Key := 0;
            end;
          end;
        VK_RIGHT:
          begin
            { Right = move to next band slider }
            NewIndex := BandIndex + 1;
            if NewIndex < EQ_BAND_COUNT then
            begin
              Self.ActiveControl := FBandSliders[NewIndex];
              Key := 0;
            end;
          end;
        VK_HOME:
          begin
            { Home = max dB value (slider at top visually = Min Position in GTK2) }
            Slider.Position := Slider.Min;
            Key := 0;
          end;
        VK_END:
          begin
            { End = min dB value (slider at bottom visually = Max Position in GTK2) }
            Slider.Position := Slider.Max;
            Key := 0;
          end;
      end;
    end
    else
    begin
      { Handle horizontal slider (tbPreamp) }
      Step := 1;  { 0.1 dB step for keyboard }
      if ssShift in Shift then
        Step := 10;  { 1 dB step with Shift }

      case Key of
        VK_RIGHT, VK_UP:
          begin
            { Right/Up = increase value }
            Slider.Position := Min(Slider.Max, Slider.Position + Step);
            Key := 0;
          end;
        VK_LEFT, VK_DOWN:
          begin
            { Left/Down = decrease value }
            Slider.Position := Max(Slider.Min, Slider.Position - Step);
            Key := 0;
          end;
        VK_HOME:
          begin
            Slider.Position := Slider.Max;
            Key := 0;
          end;
        VK_END:
          begin
            Slider.Position := Slider.Min;
            Key := 0;
          end;
      end;
    end;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PRIVATE METHODS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmEqualizer.CreateBandControls;
var
  I: Integer;
  BandWidth: Integer;
  XPos: Integer;
  SliderPanel: TPanel;
begin
  BandWidth := (pnlBands.ClientWidth - 20) div EQ_BAND_COUNT;

  for I := 0 to EQ_BAND_COUNT - 1 do
  begin
    XPos := 10 + I * BandWidth;

    { Create container panel for each band }
    SliderPanel := TPanel.Create(pnlBands);
    SliderPanel.Parent := pnlBands;
    SliderPanel.Left := XPos;
    SliderPanel.Top := 5;
    SliderPanel.Width := BandWidth - 5;
    SliderPanel.Height := pnlBands.ClientHeight - 10;
    SliderPanel.BevelOuter := bvNone;
    SliderPanel.TabStop := False;  { Don't let panel intercept focus }

    { Frequency label at top }
    FBandLabels[I] := TLabel.Create(SliderPanel);
    FBandLabels[I].Parent := SliderPanel;
    FBandLabels[I].Caption := EQ_FREQUENCIES[I];
    FBandLabels[I].Alignment := taCenter;
    FBandLabels[I].AutoSize := False;
    FBandLabels[I].Width := SliderPanel.Width;
    FBandLabels[I].Left := 0;
    FBandLabels[I].Top := 0;

    { Slider }
    FBandSliders[I] := TTrackBar.Create(SliderPanel);
    FBandSliders[I].Parent := SliderPanel;
    FBandSliders[I].Name := 'tbBand' + IntToStr(I);  { Name required for proper focus handling }
    FBandSliders[I].Orientation := trVertical;
    FBandSliders[I].Min := 0;
    FBandSliders[I].Max := EQ_SLIDER_RANGE;
    FBandSliders[I].Position := EQ_SLIDER_RANGE div 2; { Center = 0dB }
    FBandSliders[I].Left := (SliderPanel.Width - 30) div 2;
    FBandSliders[I].Top := 20;
    FBandSliders[I].Width := 30;
    FBandSliders[I].Height := SliderPanel.Height - 55;
    FBandSliders[I].TickStyle := tsNone;
    FBandSliders[I].Tag := I;
    FBandSliders[I].TabStop := True;
    FBandSliders[I].TabOrder := I;
    FBandSliders[I].OnChange := @BandSliderChange;
    FBandSliders[I].OnEnter := @BandSliderEnter;
    FBandSliders[I].OnMouseDown := @BandSliderMouseDown;

    { Value label at bottom }
    FBandValueLabels[I] := TLabel.Create(SliderPanel);
    FBandValueLabels[I].Parent := SliderPanel;
    FBandValueLabels[I].Caption := '0';
    FBandValueLabels[I].Alignment := taCenter;
    FBandValueLabels[I].AutoSize := False;
    FBandValueLabels[I].Width := SliderPanel.Width;
    FBandValueLabels[I].Left := 0;
    FBandValueLabels[I].Top := SliderPanel.Height - 30;
  end;
end;

procedure TfrmEqualizer.BandSliderChange(Sender: TObject);
var
  Slider: TTrackBar;
  BandIndex: Integer;
  dB: Double;
begin
  if FUpdating then Exit;

  Slider := Sender as TTrackBar;
  BandIndex := Slider.Tag;

  UpdateBandValueLabel(BandIndex);

  { Store value in MPV engine (doesn't apply immediately) }
  if FMPVEngine <> nil then
  begin
    dB := SliderToDb(Slider.Position);
    FMPVEngine.SetEqualizerBand(BandIndex, dB);
  end;

  { Set preset to Custom }
  cmbPreset.ItemIndex := 0;

  { Schedule debounced apply to avoid crackling }
  ScheduleApply;
end;

procedure TfrmEqualizer.BandSliderEnter(Sender: TObject);
begin
  { Event handler for slider enter - focus is handled in BandSliderMouseDown }
end;

procedure TfrmEqualizer.BandSliderMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Slider: TTrackBar;
begin
  Slider := Sender as TTrackBar;
  { Force focus to the slider when clicked - use ActiveControl assignment }
  if Slider.CanFocus then
    Self.ActiveControl := Slider;
end;

procedure TfrmEqualizer.UpdateBandValueLabel(BandIndex: Integer);
var
  dB: Double;
begin
  dB := SliderToDb(FBandSliders[BandIndex].Position);
  if dB >= 0 then
    FBandValueLabels[BandIndex].Caption := Format('+%.0f', [dB])
  else
    FBandValueLabels[BandIndex].Caption := Format('%.0f', [dB]);
end;

procedure TfrmEqualizer.ApplyCurrentSettings;
var
  Values: string;
begin
  if FMPVEngine = nil then Exit;

  Values := GetCurrentValues;
  FMPVEngine.SetEqualizerPreset(Values);

  { Save to config }
  SaveEqualizerToConfig(Values);

  { Notify main form to update visualization filter if active }
  DoEqualizerChange;
end;

procedure TfrmEqualizer.DoEqualizerChange;
begin
  if Assigned(FOnEqualizerChange) then
    FOnEqualizerChange(Self);
end;

procedure TfrmEqualizer.ScheduleApply;
begin
  { Reset timer to debounce rapid slider changes }
  FPendingApply := True;
  FApplyTimer.Enabled := False;
  FApplyTimer.Enabled := True;
end;

procedure TfrmEqualizer.ApplyTimerEvent(Sender: TObject);
begin
  FApplyTimer.Enabled := False;
  if FPendingApply and (FMPVEngine <> nil) then
  begin
    FMPVEngine.ApplyEqualizer;
    DoEqualizerChange;
    FPendingApply := False;
  end;
end;

procedure TfrmEqualizer.LoadPresets;
var
  I: Integer;
  PresetFile: string;
  SL: TStringList;
  PresetCount, J: Integer;
  PresetName, PresetValues: string;
  NewPreset: TEqualizerPreset;
  ValParts: TStringList;
begin
  { Load default presets }
  SetLength(FPresets, Length(DEFAULT_PRESETS));
  for I := 0 to High(DEFAULT_PRESETS) do
  begin
    FPresets[I].Name := DEFAULT_PRESETS[I].Name;

    { Parse values string }
    ValParts := TStringList.Create;
    try
      ValParts.Delimiter := ':';
      ValParts.StrictDelimiter := True;
      ValParts.DelimitedText := DEFAULT_PRESETS[I].Values;

      for J := 0 to Min(EQ_BAND_COUNT - 1, ValParts.Count - 1) do
        FPresets[I].Bands[J] := StrToFloatDef(ValParts[J], 0);
    finally
      ValParts.Free;
    end;
  end;

  { Try to load custom presets from file }
  PresetFile := GetConfigDir + 'equalizer_presets.ini';
  if FileExists(PresetFile) then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(PresetFile);
      PresetCount := StrToIntDef(SL.Values['Count'], 0);

      for I := 0 to PresetCount - 1 do
      begin
        PresetName := SL.Values['Name' + IntToStr(I)];
        PresetValues := SL.Values['Values' + IntToStr(I)];

        if (PresetName <> '') and (PresetValues <> '') then
        begin
          NewPreset.Name := PresetName;

          ValParts := TStringList.Create;
          try
            ValParts.Delimiter := ':';
            ValParts.StrictDelimiter := True;
            ValParts.DelimitedText := PresetValues;

            for J := 0 to Min(EQ_BAND_COUNT - 1, ValParts.Count - 1) do
              NewPreset.Bands[J] := StrToFloatDef(ValParts[J], 0);
          finally
            ValParts.Free;
          end;

          SetLength(FPresets, Length(FPresets) + 1);
          FPresets[High(FPresets)] := NewPreset;
        end;
      end;
    finally
      SL.Free;
    end;
  end;

  { Populate combo box }
  cmbPreset.Items.Clear;
  cmbPreset.Items.Add('(Custom)');
  for I := 0 to High(FPresets) do
    cmbPreset.Items.Add(FPresets[I].Name);
  cmbPreset.ItemIndex := 0;
end;

procedure TfrmEqualizer.SavePresets;
var
  SL: TStringList;
  PresetFile: string;
  I, J, CustomCount: Integer;
  ValStr: string;
begin
  PresetFile := GetConfigDir + 'equalizer_presets.ini';

  SL := TStringList.Create;
  try
    { Only save custom presets (skip default ones) }
    CustomCount := 0;
    for I := Length(DEFAULT_PRESETS) to High(FPresets) do
    begin
      ValStr := '';
      for J := 0 to EQ_BAND_COUNT - 1 do
      begin
        if J > 0 then ValStr := ValStr + ':';
        ValStr := ValStr + FloatToStr(FPresets[I].Bands[J]);
      end;

      SL.Add('Name' + IntToStr(CustomCount) + '=' + FPresets[I].Name);
      SL.Add('Values' + IntToStr(CustomCount) + '=' + ValStr);
      Inc(CustomCount);
    end;

    SL.Insert(0, 'Count=' + IntToStr(CustomCount));
    SL.SaveToFile(PresetFile);
  finally
    SL.Free;
  end;
end;

procedure TfrmEqualizer.ApplyPreset(PresetIndex: Integer);
var
  I: Integer;
begin
  if (PresetIndex < 0) or (PresetIndex > High(FPresets)) then Exit;

  FUpdating := True;
  try
    for I := 0 to EQ_BAND_COUNT - 1 do
    begin
      FBandSliders[I].Position := DbToSlider(FPresets[PresetIndex].Bands[I]);
      UpdateBandValueLabel(I);
    end;
  finally
    FUpdating := False;
  end;

  ApplyCurrentSettings;
  StatusBar.SimpleText := Format(_T('Equalizer', 'PresetApplied', 'Preset applied: %s'), [FPresets[PresetIndex].Name]);
end;

function TfrmEqualizer.GetCurrentValues: string;
var
  I: Integer;
  dB: Double;
begin
  Result := '';
  for I := 0 to EQ_BAND_COUNT - 1 do
  begin
    if I > 0 then Result := Result + ':';
    dB := SliderToDb(FBandSliders[I].Position);
    Result := Result + FloatToStr(dB);
  end;
end;

procedure TfrmEqualizer.SetBandValues(const Values: string);
var
  Parts: TStringList;
  I: Integer;
  dB: Double;
begin
  Parts := TStringList.Create;
  try
    Parts.Delimiter := ':';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Values;

    FUpdating := True;
    try
      for I := 0 to Min(EQ_BAND_COUNT - 1, Parts.Count - 1) do
      begin
        dB := StrToFloatDef(Parts[I], 0);
        FBandSliders[I].Position := DbToSlider(dB);
        UpdateBandValueLabel(I);
      end;
    finally
      FUpdating := False;
    end;
  finally
    Parts.Free;
  end;
end;

function TfrmEqualizer.SliderToDb(APosition: Integer): Double;
begin
  { Convert slider position (0..240) to dB (-12..+12) }
  Result := (APosition - (EQ_SLIDER_RANGE div 2)) / 10.0;
  if Result < EQ_MIN_DB then Result := EQ_MIN_DB;
  if Result > EQ_MAX_DB then Result := EQ_MAX_DB;
end;

function TfrmEqualizer.DbToSlider(dB: Double): Integer;
begin
  { Convert dB (-12..+12) to slider position (0..240) }
  Result := Round((dB * 10) + (EQ_SLIDER_RANGE div 2));
  if Result < 0 then Result := 0;
  if Result > EQ_SLIDER_RANGE then Result := EQ_SLIDER_RANGE;
end;

procedure SaveEqualizerToConfig(const Values: string);
var
  TmpSettings: TAppSettings;
  Parts: TStringList;
  I: Integer;
begin
  Parts := TStringList.Create;
  try
    Parts.Delimiter := ':';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Values;

    TmpSettings := Config.Settings;
    for I := 0 to Min(9, Parts.Count - 1) do
      TmpSettings.EqualizerBands[I] := StrToFloatDef(Parts[I], 0);
    Config.Settings := TmpSettings;
    Config.Modified := True;
  finally
    Parts.Free;
  end;
end;

end.
