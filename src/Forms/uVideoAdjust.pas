{ ===============================================================================
  uVideoAdjust.pas - Video Adjustments Dialog

  Part of 3nity Media - Lazarus Edition

  This form provides real-time video color adjustments:
  - Brightness, Contrast, Saturation, Hue, Gamma

  Changes are applied immediately when sliders are moved.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uVideoAdjust;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, uMPVEngine, uLocale, uTypes;

type
  { TfrmVideoAdjust }
  TfrmVideoAdjust = class(TForm)
    { Main panel with sliders }
    pnlMain: TPanel;

    { Brightness }
    lblBrightness: TLabel;
    tbBrightness: TTrackBar;
    lblBrightnessValue: TLabel;
    btnBrightnessMinus: TSpeedButton;
    btnBrightnessPlus: TSpeedButton;

    { Contrast }
    lblContrast: TLabel;
    tbContrast: TTrackBar;
    lblContrastValue: TLabel;
    btnContrastMinus: TSpeedButton;
    btnContrastPlus: TSpeedButton;

    { Saturation }
    lblSaturation: TLabel;
    tbSaturation: TTrackBar;
    lblSaturationValue: TLabel;
    btnSaturationMinus: TSpeedButton;
    btnSaturationPlus: TSpeedButton;

    { Hue }
    lblHue: TLabel;
    tbHue: TTrackBar;
    lblHueValue: TLabel;
    btnHueMinus: TSpeedButton;
    btnHuePlus: TSpeedButton;

    { Gamma }
    lblGamma: TLabel;
    tbGamma: TTrackBar;
    lblGammaValue: TLabel;
    btnGammaMinus: TSpeedButton;
    btnGammaPlus: TSpeedButton;

    { Bottom panel with reset button }
    pnlBottom: TPanel;
    btnResetAll: TButton;
    btnClose: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    { Slider change handlers }
    procedure tbBrightnessChange(Sender: TObject);
    procedure tbContrastChange(Sender: TObject);
    procedure tbSaturationChange(Sender: TObject);
    procedure tbHueChange(Sender: TObject);
    procedure tbGammaChange(Sender: TObject);

    { +/- button handlers }
    procedure btnBrightnessMinusClick(Sender: TObject);
    procedure btnBrightnessPlusClick(Sender: TObject);
    procedure btnContrastMinusClick(Sender: TObject);
    procedure btnContrastPlusClick(Sender: TObject);
    procedure btnSaturationMinusClick(Sender: TObject);
    procedure btnSaturationPlusClick(Sender: TObject);
    procedure btnHueMinusClick(Sender: TObject);
    procedure btnHuePlusClick(Sender: TObject);
    procedure btnGammaMinusClick(Sender: TObject);
    procedure btnGammaPlusClick(Sender: TObject);

    { Reset and Close }
    procedure btnResetAllClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);

  private
    FMPVEngine: TMPVEngine;
    FUpdating: Boolean;

    procedure ApplyLocale;
    procedure LoadCurrentValues;
    procedure UpdateValueLabels;
    procedure ApplyBrightness;
    procedure ApplyContrast;
    procedure ApplySaturation;
    procedure ApplyHue;
    procedure ApplyGamma;

  public
    property MPVEngine: TMPVEngine read FMPVEngine write FMPVEngine;
  end;

var
  frmVideoAdjust: TfrmVideoAdjust;

implementation

uses
  uConfig;

{$R *.lfm}

{ ===============================================================================
  FORM EVENTS
  =============================================================================== }

procedure TfrmVideoAdjust.FormCreate(Sender: TObject);
begin
  FUpdating := False;
  ApplyLocale;
end;

procedure TfrmVideoAdjust.FormShow(Sender: TObject);
begin
  ApplyLocale;
  LoadCurrentValues;
end;

procedure TfrmVideoAdjust.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

{ ===============================================================================
  SLIDER CHANGE HANDLERS
  =============================================================================== }

procedure TfrmVideoAdjust.tbBrightnessChange(Sender: TObject);
begin
  if FUpdating then Exit;
  lblBrightnessValue.Caption := IntToStr(tbBrightness.Position);
  ApplyBrightness;
end;

procedure TfrmVideoAdjust.tbContrastChange(Sender: TObject);
begin
  if FUpdating then Exit;
  lblContrastValue.Caption := IntToStr(tbContrast.Position);
  ApplyContrast;
end;

procedure TfrmVideoAdjust.tbSaturationChange(Sender: TObject);
begin
  if FUpdating then Exit;
  lblSaturationValue.Caption := IntToStr(tbSaturation.Position);
  ApplySaturation;
end;

procedure TfrmVideoAdjust.tbHueChange(Sender: TObject);
begin
  if FUpdating then Exit;
  lblHueValue.Caption := IntToStr(tbHue.Position);
  ApplyHue;
end;

procedure TfrmVideoAdjust.tbGammaChange(Sender: TObject);
begin
  if FUpdating then Exit;
  lblGammaValue.Caption := IntToStr(tbGamma.Position);
  ApplyGamma;
end;

{ ===============================================================================
  +/- BUTTON HANDLERS
  =============================================================================== }

procedure TfrmVideoAdjust.btnBrightnessMinusClick(Sender: TObject);
begin
  if tbBrightness.Position > tbBrightness.Min then
    tbBrightness.Position := tbBrightness.Position - 1;
end;

procedure TfrmVideoAdjust.btnBrightnessPlusClick(Sender: TObject);
begin
  if tbBrightness.Position < tbBrightness.Max then
    tbBrightness.Position := tbBrightness.Position + 1;
end;

procedure TfrmVideoAdjust.btnContrastMinusClick(Sender: TObject);
begin
  if tbContrast.Position > tbContrast.Min then
    tbContrast.Position := tbContrast.Position - 1;
end;

procedure TfrmVideoAdjust.btnContrastPlusClick(Sender: TObject);
begin
  if tbContrast.Position < tbContrast.Max then
    tbContrast.Position := tbContrast.Position + 1;
end;

procedure TfrmVideoAdjust.btnSaturationMinusClick(Sender: TObject);
begin
  if tbSaturation.Position > tbSaturation.Min then
    tbSaturation.Position := tbSaturation.Position - 1;
end;

procedure TfrmVideoAdjust.btnSaturationPlusClick(Sender: TObject);
begin
  if tbSaturation.Position < tbSaturation.Max then
    tbSaturation.Position := tbSaturation.Position + 1;
end;

procedure TfrmVideoAdjust.btnHueMinusClick(Sender: TObject);
begin
  if tbHue.Position > tbHue.Min then
    tbHue.Position := tbHue.Position - 1;
end;

procedure TfrmVideoAdjust.btnHuePlusClick(Sender: TObject);
begin
  if tbHue.Position < tbHue.Max then
    tbHue.Position := tbHue.Position + 1;
end;

procedure TfrmVideoAdjust.btnGammaMinusClick(Sender: TObject);
begin
  if tbGamma.Position > tbGamma.Min then
    tbGamma.Position := tbGamma.Position - 1;
end;

procedure TfrmVideoAdjust.btnGammaPlusClick(Sender: TObject);
begin
  if tbGamma.Position < tbGamma.Max then
    tbGamma.Position := tbGamma.Position + 1;
end;

{ ===============================================================================
  RESET AND CLOSE
  =============================================================================== }

procedure TfrmVideoAdjust.btnResetAllClick(Sender: TObject);
var
  Settings: TAppSettings;
begin
  FUpdating := True;
  try
    tbBrightness.Position := 0;
    tbContrast.Position := 0;
    tbSaturation.Position := 0;
    tbHue.Position := 0;
    tbGamma.Position := 0;
    UpdateValueLabels;
  finally
    FUpdating := False;
  end;

  { Apply all resets }
  if FMPVEngine <> nil then
  begin
    FMPVEngine.Brightness := 0;
    FMPVEngine.Contrast := 0;
    FMPVEngine.Saturation := 0;
    FMPVEngine.Hue := 0;
    FMPVEngine.Gamma := 0;
  end;

  { Save to config }
  Settings := Config.Settings;
  Settings.Video.Brightness := 0;
  Settings.Video.Contrast := 0;
  Settings.Video.Saturation := 0;
  Settings.Video.Hue := 0;
  Settings.Video.Gamma := 0;
  Config.Settings := Settings;
  Config.Save;
end;

procedure TfrmVideoAdjust.btnCloseClick(Sender: TObject);
begin
  Close;
end;

{ ===============================================================================
  PRIVATE METHODS
  =============================================================================== }

procedure TfrmVideoAdjust.ApplyLocale;
begin
  Caption := _T('VideoAdjust', 'Title', 'Video Adjustments');

  lblBrightness.Caption := _T('VideoAdjust', 'Brightness', 'Brightness:');
  lblContrast.Caption := _T('VideoAdjust', 'Contrast', 'Contrast:');
  lblSaturation.Caption := _T('VideoAdjust', 'Saturation', 'Saturation:');
  lblHue.Caption := _T('VideoAdjust', 'Hue', 'Hue:');
  lblGamma.Caption := _T('VideoAdjust', 'Gamma', 'Gamma:');

  btnResetAll.Caption := _T('VideoAdjust', 'ResetAll', 'Reset All Colors');
  btnClose.Caption := _T('Button', 'Close', 'Close');
end;

procedure TfrmVideoAdjust.LoadCurrentValues;
begin
  FUpdating := True;
  try
    tbBrightness.Position := Config.Settings.Video.Brightness;
    tbContrast.Position := Config.Settings.Video.Contrast;
    tbSaturation.Position := Config.Settings.Video.Saturation;
    tbHue.Position := Config.Settings.Video.Hue;
    tbGamma.Position := Config.Settings.Video.Gamma;
    UpdateValueLabels;
  finally
    FUpdating := False;
  end;
end;

procedure TfrmVideoAdjust.UpdateValueLabels;
begin
  lblBrightnessValue.Caption := IntToStr(tbBrightness.Position);
  lblContrastValue.Caption := IntToStr(tbContrast.Position);
  lblSaturationValue.Caption := IntToStr(tbSaturation.Position);
  lblHueValue.Caption := IntToStr(tbHue.Position);
  lblGammaValue.Caption := IntToStr(tbGamma.Position);
end;

procedure TfrmVideoAdjust.ApplyBrightness;
var
  Settings: TAppSettings;
begin
  if FMPVEngine <> nil then
    FMPVEngine.Brightness := tbBrightness.Position;
  Settings := Config.Settings;
  Settings.Video.Brightness := tbBrightness.Position;
  Config.Settings := Settings;
  Config.Save;
end;

procedure TfrmVideoAdjust.ApplyContrast;
var
  Settings: TAppSettings;
begin
  if FMPVEngine <> nil then
    FMPVEngine.Contrast := tbContrast.Position;
  Settings := Config.Settings;
  Settings.Video.Contrast := tbContrast.Position;
  Config.Settings := Settings;
  Config.Save;
end;

procedure TfrmVideoAdjust.ApplySaturation;
var
  Settings: TAppSettings;
begin
  if FMPVEngine <> nil then
    FMPVEngine.Saturation := tbSaturation.Position;
  Settings := Config.Settings;
  Settings.Video.Saturation := tbSaturation.Position;
  Config.Settings := Settings;
  Config.Save;
end;

procedure TfrmVideoAdjust.ApplyHue;
var
  Settings: TAppSettings;
begin
  if FMPVEngine <> nil then
    FMPVEngine.Hue := tbHue.Position;
  Settings := Config.Settings;
  Settings.Video.Hue := tbHue.Position;
  Config.Settings := Settings;
  Config.Save;
end;

procedure TfrmVideoAdjust.ApplyGamma;
var
  Settings: TAppSettings;
begin
  if FMPVEngine <> nil then
    FMPVEngine.Gamma := tbGamma.Position;
  Settings := Config.Settings;
  Settings.Video.Gamma := tbGamma.Position;
  Config.Settings := Settings;
  Config.Save;
end;

end.
