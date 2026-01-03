{ ===============================================================================
  uOptions.pas - Options/Settings Dialog

  Part of 3nity Media - Lazarus Edition

  This form provides a tabbed interface for configuring all application settings:
  - General settings (language, behavior, OSD)
  - Video settings (brightness, contrast, saturation, deinterlace)
  - Audio settings (volume, output device, normalization)
  - Subtitle settings (font, color, position, encoding)
  - Cache settings (buffer sizes for different media types)

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Spin, ColorBox, Buttons,
  uTypes, uConstants, uConfig, uMPVConst, uLocale;

type
  { TfrmOptions }
  TfrmOptions = class(TForm)
    { Page control for tabs }
    PageControl: TPageControl;

    { Tab sheets }
    tsGeneral: TTabSheet;
    tsVideo: TTabSheet;
    tsAudio: TTabSheet;
    tsSubtitles: TTabSheet;
    tsCache: TTabSheet;

    { Bottom panel with buttons }
    pnlBottom: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;

    { General tab controls }
    gbBehavior: TGroupBox;
    chkSingleInstance: TCheckBox;
    chkAutoSavePlaylist: TCheckBox;  { Phase 24 }

    gbScreenshots: TGroupBox;
    lblScreenshotPath: TLabel;
    edtScreenshotPath: TEdit;
    btnBrowseScreenshot: TSpeedButton;
    lblScreenshotFormat: TLabel;
    cmbScreenshotFormat: TComboBox;

    gbHistory: TGroupBox;
    chkHistoryEnabled: TCheckBox;
    lblHistoryMaxItems: TLabel;
    seHistoryMaxItems: TSpinEdit;

    gbLanguage: TGroupBox;
    lblLanguage: TLabel;
    cmbLanguage: TComboBox;
    lblRestartRequired: TLabel;

    { Video tab controls }
    gbVideoAdjust: TGroupBox;
    lblBrightness: TLabel;
    tbBrightness: TTrackBar;
    lblBrightnessValue: TLabel;
    lblContrast: TLabel;
    tbContrast: TTrackBar;
    lblContrastValue: TLabel;
    lblSaturation: TLabel;
    tbSaturation: TTrackBar;
    lblSaturationValue: TLabel;
    lblHue: TLabel;
    tbHue: TTrackBar;
    lblHueValue: TLabel;
    lblGamma: TLabel;
    tbGamma: TTrackBar;
    lblGammaValue: TLabel;

    gbVideoOutput: TGroupBox;
    lblVideoOutput: TLabel;
    cmbVideoOutput: TComboBox;
    lblVideoOutputWarning: TLabel;
    chkHWAccel: TCheckBox;

    gbDeinterlace: TGroupBox;
    lblDeinterlace: TLabel;
    cmbDeinterlace: TComboBox;

    { Audio tab controls }
    gbAudioOutput: TGroupBox;
    lblAudioOutput: TLabel;
    cmbAudioOutput: TComboBox;
    lblAudioDevice: TLabel;
    cmbAudioDevice: TComboBox;

    gbAudioOptions: TGroupBox;
    chkNormalize: TCheckBox;
    lblChannels: TLabel;
    cmbChannels: TComboBox;

    { Subtitle tab controls }
    chkSubUseDefault: TCheckBox;
    gbSubtitleFont: TGroupBox;
    lblSubFontName: TLabel;
    cmbSubFontName: TComboBox;
    lblSubFontSize: TLabel;
    seSubFontSize: TSpinEdit;
    lblSubFontColor: TLabel;
    cbSubFontColor: TColorBox;
    chkSubFontBold: TCheckBox;
    chkSubFontItalic: TCheckBox;

    gbSubtitleOutline: TGroupBox;
    lblSubOutlineColor: TLabel;
    cbSubOutlineColor: TColorBox;
    lblSubOutlineSize: TLabel;
    seSubOutlineSize: TSpinEdit;

    gbSubtitlePosition: TGroupBox;
    lblSubPosition: TLabel;
    tbSubPosition: TTrackBar;
    lblSubPositionValue: TLabel;

    gbSubtitleOptions: TGroupBox;
    lblSubEncoding: TLabel;
    cmbSubEncoding: TComboBox;
    chkSubAutoLoad: TCheckBox;

    { Cache tab controls }
    gbCacheSettings: TGroupBox;
    lblCacheDefault: TLabel;
    seCacheDefault: TSpinEdit;
    lblCacheFixed: TLabel;
    seCacheFixed: TSpinEdit;
    lblCacheNetwork: TLabel;
    seCacheNetwork: TSpinEdit;
    lblCacheInternet: TLabel;
    seCacheInternet: TSpinEdit;
    lblCacheDVD: TLabel;
    seCacheDVD: TSpinEdit;
    lblCacheCDROM: TLabel;
    seCacheCDROM: TSpinEdit;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnBrowseScreenshotClick(Sender: TObject);

    { Video adjustment handlers }
    procedure tbBrightnessChange(Sender: TObject);
    procedure tbContrastChange(Sender: TObject);
    procedure tbSaturationChange(Sender: TObject);
    procedure tbHueChange(Sender: TObject);
    procedure tbGammaChange(Sender: TObject);

    { Subtitle position handler }
    procedure tbSubPositionChange(Sender: TObject);

    { Subtitle default handler }
    procedure chkSubUseDefaultChange(Sender: TObject);

    { Video output handler }
    procedure cmbVideoOutputChange(Sender: TObject);

    { Common mouse wheel handler for trackbars }
    procedure TrackBarMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

  private
    FModified: Boolean;
    FOriginalLanguage: string;
    procedure UpdateSubtitleControlsState;
    procedure UpdateVideoOutputWarning;

    procedure LoadSettings;
    procedure SaveSettings;
    procedure ApplySettings;

    procedure PopulateComboBoxes;
    procedure PopulateLanguages;
    procedure UpdateVideoLabels;

  public
    property Modified: Boolean read FModified;

    procedure ApplyLocale;
  end;

var
  frmOptions: TfrmOptions;

implementation

uses
  uMainForm;

{$R *.lfm}

{ ===============================================================================
  FORM EVENTS
  =============================================================================== }

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  FModified := False;
  FOriginalLanguage := '';

  { Initialize page control }
  PageControl.ActivePageIndex := 0;

  { Populate combo boxes }
  PopulateComboBoxes;
  PopulateLanguages;

  { Apply translations }
  ApplyLocale;
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
  { Apply current translations (in case language changed) }
  ApplyLocale;

  LoadSettings;
  FModified := False;
end;

procedure TfrmOptions.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

{ ===============================================================================
  BUTTON HANDLERS
  =============================================================================== }

procedure TfrmOptions.btnOKClick(Sender: TObject);
begin
  SaveSettings;
  ApplySettings;
  Close;
end;

procedure TfrmOptions.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmOptions.btnApplyClick(Sender: TObject);
begin
  SaveSettings;
  ApplySettings;
  FModified := False;
end;

procedure TfrmOptions.btnBrowseScreenshotClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := edtScreenshotPath.Text;
  if SelectDirectory('Select Screenshot Folder', '', Dir) then
  begin
    edtScreenshotPath.Text := Dir;
    FModified := True;
  end;
end;

{ ===============================================================================
  VIDEO ADJUSTMENT HANDLERS
  =============================================================================== }

procedure TfrmOptions.tbBrightnessChange(Sender: TObject);
begin
  lblBrightnessValue.Caption := IntToStr(tbBrightness.Position);
  FModified := True;
end;

procedure TfrmOptions.tbContrastChange(Sender: TObject);
begin
  lblContrastValue.Caption := IntToStr(tbContrast.Position);
  FModified := True;
end;

procedure TfrmOptions.tbSaturationChange(Sender: TObject);
begin
  lblSaturationValue.Caption := IntToStr(tbSaturation.Position);
  FModified := True;
end;

procedure TfrmOptions.tbHueChange(Sender: TObject);
begin
  lblHueValue.Caption := IntToStr(tbHue.Position);
  FModified := True;
end;

procedure TfrmOptions.tbGammaChange(Sender: TObject);
begin
  lblGammaValue.Caption := IntToStr(tbGamma.Position);
  FModified := True;
end;

procedure TfrmOptions.tbSubPositionChange(Sender: TObject);
begin
  lblSubPositionValue.Caption := IntToStr(tbSubPosition.Position) + '%';
  FModified := True;
end;

procedure TfrmOptions.chkSubUseDefaultChange(Sender: TObject);
begin
  UpdateSubtitleControlsState;
  FModified := True;
end;

procedure TfrmOptions.cmbVideoOutputChange(Sender: TObject);
begin
  UpdateVideoOutputWarning;
  FModified := True;
end;

procedure TfrmOptions.TrackBarMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  TB: TTrackBar;
  Step: Integer;
begin
  TB := Sender as TTrackBar;
  Step := 1;
  if ssShift in Shift then
    Step := 5;

  { Wheel up = increase value, wheel down = decrease value }
  if WheelDelta > 0 then
    TB.Position := TB.Position + Step
  else
    TB.Position := TB.Position - Step;

  Handled := True;
end;

procedure TfrmOptions.UpdateVideoOutputWarning;
var
  SelectedVO: string;
begin
  if cmbVideoOutput.ItemIndex >= 0 then
    SelectedVO := LowerCase(cmbVideoOutput.Text)
  else
    SelectedVO := 'auto';

  { Show warning based on selected video output }
  if SelectedVO = 'x11' then
  begin
    lblVideoOutputWarning.Caption := _T('Options', 'VOWarningX11',
      'Warning: x11 does not support video adjustments (brightness, contrast, etc.)');
    lblVideoOutputWarning.Visible := True;
    lblVideoOutputWarning.Font.Color := clMaroon;
  end
  else if SelectedVO = 'wayland' then
  begin
    lblVideoOutputWarning.Caption := _T('Options', 'VOWarningWayland',
      'Warning: wayland requires a Wayland session (not X11/XOrg)');
    lblVideoOutputWarning.Visible := True;
    lblVideoOutputWarning.Font.Color := clMaroon;
  end
  else
  begin
    lblVideoOutputWarning.Caption := '';
    lblVideoOutputWarning.Visible := False;
  end;
end;

procedure TfrmOptions.UpdateSubtitleControlsState;
var
  CustomEnabled: Boolean;
begin
  { Enable custom subtitle controls only when "Use Default" is unchecked }
  CustomEnabled := not chkSubUseDefault.Checked;

  gbSubtitleFont.Enabled := CustomEnabled;
  cmbSubFontName.Enabled := CustomEnabled;
  seSubFontSize.Enabled := CustomEnabled;
  cbSubFontColor.Enabled := CustomEnabled;
  chkSubFontBold.Enabled := CustomEnabled;
  chkSubFontItalic.Enabled := CustomEnabled;

  gbSubtitleOutline.Enabled := CustomEnabled;
  cbSubOutlineColor.Enabled := CustomEnabled;
  seSubOutlineSize.Enabled := CustomEnabled;

  gbSubtitlePosition.Enabled := CustomEnabled;
  tbSubPosition.Enabled := CustomEnabled;

  gbSubtitleOptions.Enabled := CustomEnabled;
  cmbSubEncoding.Enabled := CustomEnabled;
  chkSubAutoLoad.Enabled := CustomEnabled;
end;

{ ===============================================================================
  PRIVATE METHODS
  =============================================================================== }

procedure TfrmOptions.PopulateComboBoxes;
begin
  { Screenshot formats }
  cmbScreenshotFormat.Items.Clear;
  cmbScreenshotFormat.Items.Add('PNG');
  cmbScreenshotFormat.Items.Add('JPEG');
  cmbScreenshotFormat.Items.Add('WebP');

  { Video output }
  cmbVideoOutput.Items.Clear;
  cmbVideoOutput.Items.Add('auto');
  {$IFDEF WINDOWS}
  cmbVideoOutput.Items.Add('gpu');
  cmbVideoOutput.Items.Add('direct3d');
  {$ELSE}
  cmbVideoOutput.Items.Add('gpu');
  cmbVideoOutput.Items.Add('x11');
  cmbVideoOutput.Items.Add('wayland');
  {$ENDIF}

  { Deinterlace modes - must match DEINT_OFF=0, DEINT_ON=1, DEINT_AUTO=2 }
  cmbDeinterlace.Items.Clear;
  cmbDeinterlace.Items.Add('Off');    // Index 0 = DEINT_OFF
  cmbDeinterlace.Items.Add('On');     // Index 1 = DEINT_ON
  cmbDeinterlace.Items.Add('Auto');   // Index 2 = DEINT_AUTO

  { Audio output }
  cmbAudioOutput.Items.Clear;
  cmbAudioOutput.Items.Add('auto');
  {$IFDEF WINDOWS}
  cmbAudioOutput.Items.Add('wasapi');
  cmbAudioOutput.Items.Add('dsound');
  {$ELSE}
  cmbAudioOutput.Items.Add('pulse');
  cmbAudioOutput.Items.Add('alsa');
  cmbAudioOutput.Items.Add('pipewire');
  {$ENDIF}

  { Audio device - will be populated dynamically }
  cmbAudioDevice.Items.Clear;
  cmbAudioDevice.Items.Add('(Default)');

  { Audio channels }
  cmbChannels.Items.Clear;
  cmbChannels.Items.Add('Stereo (2)');
  cmbChannels.Items.Add('Mono (1)');
  cmbChannels.Items.Add('5.1 Surround (6)');
  cmbChannels.Items.Add('7.1 Surround (8)');

  { Subtitle fonts - use system fonts }
  cmbSubFontName.Items.Clear;
  cmbSubFontName.Items.Add('Arial');
  cmbSubFontName.Items.Add('Verdana');
  cmbSubFontName.Items.Add('Tahoma');
  cmbSubFontName.Items.Add('Helvetica');
  cmbSubFontName.Items.Add('DejaVu Sans');
  cmbSubFontName.Items.Add('Liberation Sans');
  cmbSubFontName.Items.Add('Noto Sans');

  { Subtitle encodings }
  cmbSubEncoding.Items.Clear;
  cmbSubEncoding.Items.Add('UTF-8');
  cmbSubEncoding.Items.Add('UTF-16');
  cmbSubEncoding.Items.Add('ISO-8859-1');
  cmbSubEncoding.Items.Add('ISO-8859-15');
  cmbSubEncoding.Items.Add('Windows-1252');
  cmbSubEncoding.Items.Add('CP1250');
  cmbSubEncoding.Items.Add('CP1251');
  cmbSubEncoding.Items.Add('CP1256');
end;

procedure TfrmOptions.PopulateLanguages;
var
  I: Integer;
begin
  cmbLanguage.Items.Clear;
  for I := 0 to Locale.AvailableLanguages.Count - 1 do
  begin
    cmbLanguage.Items.AddObject(Locale.LanguageNames[I],
      TObject(PtrInt(I)));
  end;
  if cmbLanguage.Items.Count > 0 then
    cmbLanguage.ItemIndex := 0;
end;

procedure TfrmOptions.ApplyLocale;
begin
  { Dialog title }
  Caption := _T('Options', 'Title', 'Options');

  { Tab captions }
  tsGeneral.Caption := _T('Options', 'General', 'General');
  tsVideo.Caption := _T('Options', 'Video', 'Video');
  tsAudio.Caption := _T('Options', 'Audio', 'Audio');
  tsSubtitles.Caption := _T('Options', 'Subtitles', 'Subtitles');
  tsCache.Caption := _T('Options', 'Cache', 'Cache');

  { Buttons }
  btnOK.Caption := _T('Button', 'OK', 'OK');
  btnCancel.Caption := _T('Button', 'Cancel', 'Cancel');
  btnApply.Caption := _T('Button', 'Apply', 'Apply');

  { General tab - Behavior }
  gbBehavior.Caption := _T('Options', 'Behavior', 'Behavior');
  chkSingleInstance.Caption := _T('Options', 'SingleInstance', 'Allow only one instance');
  chkAutoSavePlaylist.Caption := _T('Options', 'AutoSavePlaylist', 'Save playlist on exit');  { Phase 24 }

  { General tab - Screenshots }
  gbScreenshots.Caption := _T('Options', 'Screenshots', 'Screenshots');
  lblScreenshotPath.Caption := _T('Options', 'ScreenshotPath', 'Save folder:');
  lblScreenshotFormat.Caption := _T('Options', 'ScreenshotFormat', 'Format:');

  { General tab - History }
  gbHistory.Caption := _T('Options', 'HistorySettings', 'History');
  chkHistoryEnabled.Caption := _T('Options', 'HistoryEnabled', 'Enable playback history');
  lblHistoryMaxItems.Caption := _T('Options', 'HistoryMaxItems', 'Max items:');

  { General tab - Language }
  gbLanguage.Caption := _T('Options', 'LanguageGroup', 'Language');
  lblLanguage.Caption := _T('Options', 'Language', 'Language:');
  lblRestartRequired.Caption := _T('Options', 'RestartRequired', '(Restart required for changes)');

  { Video tab }
  gbVideoAdjust.Caption := _T('Options', 'VideoAdjust', 'Video Adjustments');
  lblBrightness.Caption := _T('Options', 'Brightness', 'Brightness:');
  lblContrast.Caption := _T('Options', 'Contrast', 'Contrast:');
  lblSaturation.Caption := _T('Options', 'Saturation', 'Saturation:');
  lblHue.Caption := _T('Options', 'Hue', 'Hue:');
  lblGamma.Caption := _T('Options', 'Gamma', 'Gamma:');

  gbVideoOutput.Caption := _T('Options', 'VideoOutput', 'Video Output');
  lblVideoOutput.Caption := _T('Options', 'Output', 'Output:');
  chkHWAccel.Caption := _T('Options', 'HWAccel', 'Enable hardware acceleration');

  gbDeinterlace.Caption := _T('Options', 'Deinterlacing', 'Deinterlacing');
  lblDeinterlace.Caption := _T('Options', 'Mode', 'Mode:');

  { Audio tab }
  gbAudioOutput.Caption := _T('Options', 'AudioOutput', 'Audio Output');
  lblAudioOutput.Caption := _T('Options', 'OutputDriver', 'Output driver:');
  lblAudioDevice.Caption := _T('Options', 'Device', 'Device:');

  gbAudioOptions.Caption := _T('Options', 'AudioOptions', 'Audio Options');
  chkNormalize.Caption := _T('Options', 'NormalizeAudio', 'Normalize audio volume');
  lblChannels.Caption := _T('Options', 'Channels', 'Channels:');

  { Subtitles tab }
  chkSubUseDefault.Caption := _T('Options', 'UseDefaultSubtitles', 'Use default subtitle settings');
  gbSubtitleFont.Caption := _T('Options', 'Font', 'Font');
  lblSubFontName.Caption := _T('Options', 'FontName', 'Font:');
  lblSubFontSize.Caption := _T('Options', 'FontSize', 'Size:');
  lblSubFontColor.Caption := _T('Options', 'FontColor', 'Color:');
  chkSubFontBold.Caption := _T('Options', 'Bold', 'Bold');
  chkSubFontItalic.Caption := _T('Options', 'Italic', 'Italic');

  gbSubtitleOutline.Caption := _T('Options', 'Outline', 'Outline');
  lblSubOutlineColor.Caption := _T('Options', 'OutlineColor', 'Color:');
  lblSubOutlineSize.Caption := _T('Options', 'OutlineSize', 'Size:');

  gbSubtitlePosition.Caption := _T('Options', 'Position', 'Position');
  lblSubPosition.Caption := _T('Options', 'VerticalPosition', 'Vertical position:');

  gbSubtitleOptions.Caption := _T('Options', 'SubOptions', 'Options');
  lblSubEncoding.Caption := _T('Options', 'Encoding', 'Encoding:');
  chkSubAutoLoad.Caption := _T('Options', 'AutoLoadSubs', 'Automatically load subtitle files');

  { Cache tab }
  gbCacheSettings.Caption := _T('Options', 'CacheSettings', 'Cache Buffer Sizes (KB)');
  lblCacheDefault.Caption := _T('Options', 'CacheDefault', 'Default cache:');
  lblCacheFixed.Caption := _T('Options', 'CacheLocal', 'Local files:');
  lblCacheNetwork.Caption := _T('Options', 'CacheNetwork', 'Network files:');
  lblCacheInternet.Caption := _T('Options', 'CacheInternet', 'Internet streams:');
  lblCacheDVD.Caption := _T('Options', 'CacheDVD', 'DVD:');
  lblCacheCDROM.Caption := _T('Options', 'CacheCDROM', 'CD-ROM:');
end;

procedure TfrmOptions.LoadSettings;
var
  Settings: TAppSettings;
  Idx, I: Integer;
  LangCode: string;
  DeviceList: TStringList;
  DeviceName, DeviceDesc: string;
  P: Integer;
begin
  Settings := Config.Settings;

  { General tab }
  chkSingleInstance.Checked := Settings.General.SingleInstance;
  chkAutoSavePlaylist.Checked := Settings.General.AutoSavePlaylist;  { Phase 24 }

  { Language selection }
  LangCode := Settings.General.Language;
  if LangCode = '' then
    LangCode := Locale.CurrentLanguage;
  FOriginalLanguage := LangCode;

  { Find and select current language in combo }
  for I := 0 to Locale.AvailableLanguages.Count - 1 do
  begin
    if Locale.AvailableLanguages[I] = LangCode then
    begin
      cmbLanguage.ItemIndex := I;
      Break;
    end;
  end;

  edtScreenshotPath.Text := Settings.General.ScreenshotPath;
  Idx := cmbScreenshotFormat.Items.IndexOf(UpperCase(Settings.General.ScreenshotFormat));
  if Idx >= 0 then cmbScreenshotFormat.ItemIndex := Idx else cmbScreenshotFormat.ItemIndex := 0;

  chkHistoryEnabled.Checked := Settings.General.HistoryEnabled;
  seHistoryMaxItems.Value := Settings.General.HistoryMaxItems;

  { Video tab }
  tbBrightness.Position := Settings.Video.Brightness;
  tbContrast.Position := Settings.Video.Contrast;
  tbSaturation.Position := Settings.Video.Saturation;
  tbHue.Position := Settings.Video.Hue;
  tbGamma.Position := Settings.Video.Gamma;
  UpdateVideoLabels;

  Idx := cmbVideoOutput.Items.IndexOf(Settings.Video.VideoOutput);
  if Idx >= 0 then cmbVideoOutput.ItemIndex := Idx else cmbVideoOutput.ItemIndex := 0;
  UpdateVideoOutputWarning;
  chkHWAccel.Checked := Settings.Video.HWAccel;

  cmbDeinterlace.ItemIndex := Settings.Video.Deinterlace;

  { Audio tab }
  Idx := cmbAudioOutput.Items.IndexOf(Settings.Audio.AudioOutput);
  if Idx >= 0 then cmbAudioOutput.ItemIndex := Idx else cmbAudioOutput.ItemIndex := 0;

  { Populate audio device list dynamically from MPV }
  cmbAudioDevice.Items.Clear;
  cmbAudioDevice.Items.Add('(Default)');
  if (frmMain <> nil) and (frmMain.MPVEngine <> nil) then
  begin
    DeviceList := frmMain.MPVEngine.GetAudioDeviceList;
    try
      for I := 0 to DeviceList.Count - 1 do
      begin
        { Parse name=description format }
        P := Pos('=', DeviceList[I]);
        if P > 0 then
        begin
          DeviceName := Copy(DeviceList[I], 1, P - 1);
          DeviceDesc := Copy(DeviceList[I], P + 1, Length(DeviceList[I]));
          cmbAudioDevice.Items.Add(DeviceDesc);
        end;
      end;
    finally
      DeviceList.Free;
    end;
  end;

  { Select saved audio device }
  if Settings.Audio.AudioDevice = '' then
    cmbAudioDevice.ItemIndex := 0
  else
  begin
    Idx := -1;
    for I := 0 to cmbAudioDevice.Items.Count - 1 do
    begin
      if Pos(Settings.Audio.AudioDevice, cmbAudioDevice.Items[I]) > 0 then
      begin
        Idx := I;
        Break;
      end;
    end;
    if Idx >= 0 then
      cmbAudioDevice.ItemIndex := Idx
    else
      cmbAudioDevice.ItemIndex := 0;
  end;

  chkNormalize.Checked := Settings.Audio.Normalize;

  case Settings.Audio.Channels of
    1: cmbChannels.ItemIndex := 1;
    6: cmbChannels.ItemIndex := 2;
    8: cmbChannels.ItemIndex := 3;
    else cmbChannels.ItemIndex := 0; { Stereo }
  end;

  { Subtitle tab }
  chkSubUseDefault.Checked := Settings.Subtitles.UseDefault;

  Idx := cmbSubFontName.Items.IndexOf(Settings.Subtitles.FontName);
  if Idx >= 0 then cmbSubFontName.ItemIndex := Idx
  else begin
    cmbSubFontName.Items.Add(Settings.Subtitles.FontName);
    cmbSubFontName.ItemIndex := cmbSubFontName.Items.Count - 1;
  end;

  seSubFontSize.Value := Settings.Subtitles.FontSize;
  cbSubFontColor.Selected := Settings.Subtitles.FontColor;
  chkSubFontBold.Checked := Settings.Subtitles.FontBold;
  chkSubFontItalic.Checked := Settings.Subtitles.FontItalic;

  cbSubOutlineColor.Selected := Settings.Subtitles.OutlineColor;
  seSubOutlineSize.Value := Settings.Subtitles.OutlineSize;

  tbSubPosition.Position := Settings.Subtitles.Position;
  lblSubPositionValue.Caption := IntToStr(Settings.Subtitles.Position) + '%';

  Idx := cmbSubEncoding.Items.IndexOf(Settings.Subtitles.Encoding);
  if Idx >= 0 then cmbSubEncoding.ItemIndex := Idx else cmbSubEncoding.ItemIndex := 0;
  chkSubAutoLoad.Checked := Settings.Subtitles.AutoLoad;

  { Update enabled state of subtitle controls }
  UpdateSubtitleControlsState;

  { Cache tab }
  seCacheDefault.Value := Settings.Cache.DefaultSize;
  seCacheFixed.Value := Settings.Cache.FixedSize;
  seCacheNetwork.Value := Settings.Cache.NetworkSize;
  seCacheInternet.Value := Settings.Cache.InternetSize;
  seCacheDVD.Value := Settings.Cache.DVDSize;
  seCacheCDROM.Value := Settings.Cache.CDROMSize;
end;

procedure TfrmOptions.SaveSettings;
var
  Settings: TAppSettings;
begin
  Settings := Config.Settings;

  { General tab }
  Settings.General.SingleInstance := chkSingleInstance.Checked;
  Settings.General.AutoSavePlaylist := chkAutoSavePlaylist.Checked;  { Phase 24 }

  Settings.General.ScreenshotPath := edtScreenshotPath.Text;
  if cmbScreenshotFormat.ItemIndex >= 0 then
    Settings.General.ScreenshotFormat := LowerCase(cmbScreenshotFormat.Text);

  Settings.General.HistoryEnabled := chkHistoryEnabled.Checked;
  Settings.General.HistoryMaxItems := seHistoryMaxItems.Value;

  { Language - save selected language }
  if (cmbLanguage.ItemIndex >= 0) and
     (cmbLanguage.ItemIndex < Locale.AvailableLanguages.Count) then
  begin
    Settings.General.Language := Locale.AvailableLanguages[cmbLanguage.ItemIndex];
  end;

  { Video tab }
  Settings.Video.Brightness := tbBrightness.Position;
  Settings.Video.Contrast := tbContrast.Position;
  Settings.Video.Saturation := tbSaturation.Position;
  Settings.Video.Hue := tbHue.Position;
  Settings.Video.Gamma := tbGamma.Position;

  if cmbVideoOutput.ItemIndex >= 0 then
    Settings.Video.VideoOutput := cmbVideoOutput.Text;
  Settings.Video.HWAccel := chkHWAccel.Checked;

  Settings.Video.Deinterlace := cmbDeinterlace.ItemIndex;

  { Audio tab }
  if cmbAudioOutput.ItemIndex >= 0 then
    Settings.Audio.AudioOutput := cmbAudioOutput.Text;

  { Save audio device - store the device description for matching }
  if cmbAudioDevice.ItemIndex <= 0 then
    Settings.Audio.AudioDevice := ''
  else if cmbAudioDevice.ItemIndex >= 0 then
    Settings.Audio.AudioDevice := cmbAudioDevice.Text;

  Settings.Audio.Normalize := chkNormalize.Checked;

  case cmbChannels.ItemIndex of
    1: Settings.Audio.Channels := 1;
    2: Settings.Audio.Channels := 6;
    3: Settings.Audio.Channels := 8;
    else Settings.Audio.Channels := 2;
  end;

  { Subtitle tab }
  Settings.Subtitles.UseDefault := chkSubUseDefault.Checked;
  if cmbSubFontName.ItemIndex >= 0 then
    Settings.Subtitles.FontName := cmbSubFontName.Text;
  Settings.Subtitles.FontSize := seSubFontSize.Value;
  Settings.Subtitles.FontColor := cbSubFontColor.Selected;
  Settings.Subtitles.FontBold := chkSubFontBold.Checked;
  Settings.Subtitles.FontItalic := chkSubFontItalic.Checked;

  Settings.Subtitles.OutlineColor := cbSubOutlineColor.Selected;
  Settings.Subtitles.OutlineSize := seSubOutlineSize.Value;

  Settings.Subtitles.Position := tbSubPosition.Position;

  if cmbSubEncoding.ItemIndex >= 0 then
    Settings.Subtitles.Encoding := cmbSubEncoding.Text;
  Settings.Subtitles.AutoLoad := chkSubAutoLoad.Checked;

  { Cache tab }
  Settings.Cache.DefaultSize := seCacheDefault.Value;
  Settings.Cache.FixedSize := seCacheFixed.Value;
  Settings.Cache.NetworkSize := seCacheNetwork.Value;
  Settings.Cache.InternetSize := seCacheInternet.Value;
  Settings.Cache.DVDSize := seCacheDVD.Value;
  Settings.Cache.CDROMSize := seCacheCDROM.Value;

  { Save to config }
  Config.Settings := Settings;
  Config.Modified := True;
  Config.Save;
end;

procedure TfrmOptions.ApplySettings;
begin
  if (frmMain <> nil) and (frmMain.MPVEngine <> nil) then
  begin
    { Apply screenshot settings }
    if not DirectoryExists(Config.Settings.General.ScreenshotPath) then
      ForceDirectories(Config.Settings.General.ScreenshotPath);
    frmMain.MPVEngine.SetScreenshotDirectory(Config.Settings.General.ScreenshotPath);
    frmMain.MPVEngine.SetScreenshotFormat(Config.Settings.General.ScreenshotFormat);

    { Apply video settings }
    frmMain.MPVEngine.Brightness := Config.Settings.Video.Brightness;
    frmMain.MPVEngine.Contrast := Config.Settings.Video.Contrast;
    frmMain.MPVEngine.Saturation := Config.Settings.Video.Saturation;
    frmMain.MPVEngine.Hue := Config.Settings.Video.Hue;
    frmMain.MPVEngine.Gamma := Config.Settings.Video.Gamma;
    frmMain.MPVEngine.Deinterlace := Config.Settings.Video.Deinterlace;
    frmMain.MPVEngine.HWAccel := Config.Settings.Video.HWAccel;
    { VideoOutput changes require engine restart - store for next initialization }
    frmMain.MPVEngine.VideoOutput := Config.Settings.Video.VideoOutput;

    { Apply cache settings - use default cache size }
    frmMain.MPVEngine.SetCacheSize(Config.Settings.Cache.DefaultSize);

    { Apply audio settings }
    frmMain.MPVEngine.SetAudioOutput(Config.Settings.Audio.AudioOutput);
    frmMain.MPVEngine.SetAudioDevice(Config.Settings.Audio.AudioDevice);
    frmMain.MPVEngine.SetAudioNormalize(Config.Settings.Audio.Normalize);
    frmMain.MPVEngine.SetAudioChannels(Config.Settings.Audio.Channels);
    { Reload current file to apply audio changes (required for driver changes) }
    frmMain.MPVEngine.ReloadCurrentFile;

    { Apply subtitle settings }
    if not Config.Settings.Subtitles.UseDefault then
    begin
      { Apply custom subtitle settings }
      frmMain.MPVEngine.SetSubFont(Config.Settings.Subtitles.FontName);
      frmMain.MPVEngine.SetSubFontSize(Config.Settings.Subtitles.FontSize);
      frmMain.MPVEngine.SetSubFontColor(Config.Settings.Subtitles.FontColor);
      frmMain.MPVEngine.SetSubBold(Config.Settings.Subtitles.FontBold);
      frmMain.MPVEngine.SetSubItalic(Config.Settings.Subtitles.FontItalic);
      frmMain.MPVEngine.SetSubOutlineColor(Config.Settings.Subtitles.OutlineColor);
      frmMain.MPVEngine.SetSubOutlineSize(Config.Settings.Subtitles.OutlineSize);
      frmMain.MPVEngine.SetSubPosition(Config.Settings.Subtitles.Position);
      frmMain.MPVEngine.SetSubEncoding(Config.Settings.Subtitles.Encoding);
      frmMain.MPVEngine.SetSubAutoLoad(Config.Settings.Subtitles.AutoLoad);
    end
    else
    begin
      { Reset to mpv default subtitle settings }
      frmMain.MPVEngine.SetSubFont('');           { Empty = mpv default (sans-serif) }
      frmMain.MPVEngine.SetSubFontSize(55);       { mpv default }
      frmMain.MPVEngine.SetSubFontColor($FFFFFF); { White }
      frmMain.MPVEngine.SetSubBold(False);
      frmMain.MPVEngine.SetSubItalic(False);
      frmMain.MPVEngine.SetSubOutlineColor($000000); { Black }
      frmMain.MPVEngine.SetSubOutlineSize(3);     { mpv default }
      frmMain.MPVEngine.SetSubPosition(100);      { mpv default = bottom }
      frmMain.MPVEngine.SetSubEncoding('auto');   { mpv default }
      frmMain.MPVEngine.SetSubAutoLoad(True);
    end;
  end;
end;

procedure TfrmOptions.UpdateVideoLabels;
begin
  lblBrightnessValue.Caption := IntToStr(tbBrightness.Position);
  lblContrastValue.Caption := IntToStr(tbContrast.Position);
  lblSaturationValue.Caption := IntToStr(tbSaturation.Position);
  lblHueValue.Caption := IntToStr(tbHue.Position);
  lblGammaValue.Caption := IntToStr(tbGamma.Position);
end;

end.
