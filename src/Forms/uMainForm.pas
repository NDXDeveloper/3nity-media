{ ═══════════════════════════════════════════════════════════════════════════════
  uMainForm.pas - Main Application Window

  Part of 3nity Media - Lazarus Edition

  This is the main form of the application, containing the video display,
  control bar, and menus.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  StdCtrls, ComCtrls, Buttons, LCLType, LCLIntf, LMessages, Math, Clipbrd,
  {$IFDEF WINDOWS}ShellApi,{$ENDIF}
  {$IFDEF LCLQt5}qt5, qtwidgets,{$ENDIF}
  uLibMPV, uMPVConst, uMPVEngine, uPlaylistManager, uRadioManager, uTypes, uConstants, uConfig,
  uPlaylist, uRadios, uEqualizer, uOptions, uMediaInfo, uAbout, uVideoAdjust, uLocale,
  uGotoTime, uHistory, uBookmarks, uFavorites, uSleepTimer, uShortcuts, uShortcutsEditor,
  uLog,  { Phase 25 }
  uVisualEffects,  { Phase 26 }
  uStreamRecorder,  { Phase 27 }
  uOpenURL,
  uVssScrollbar,
  uCLIParams;

type
  { Dock side for child windows }
  TDockSide = (dsNone, dsLeft, dsRight);
  { TfrmMain }
  TfrmMain = class(TForm)
    imgLogo: TImage;
    MainMenu: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileOpenURL: TMenuItem;
    mnuFileOpenClipboard: TMenuItem;
    mnuFileOpenFolder: TMenuItem;
    mnuFileSep1: TMenuItem;
    mnuFileOpenDVD: TMenuItem;
    mnuFileOpenBluray: TMenuItem;
    mnuFileSep3: TMenuItem;
    mnuFileRecent: TMenuItem;
    mnuFileSep2: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuPlayback: TMenuItem;
    mnuPlaybackPlayPause: TMenuItem;
    mnuPlaybackStop: TMenuItem;
    mnuPlaybackSep1: TMenuItem;
    mnuPlaybackPrevious: TMenuItem;
    mnuPlaybackNext: TMenuItem;
    mnuPlaybackSep2: TMenuItem;
    mnuPlaybackSeekFwd: TMenuItem;
    mnuPlaybackSeekBack: TMenuItem;
    mnuPlaybackSep3: TMenuItem;
    mnuPlaybackSpeed: TMenuItem;
    mnuPlaybackSpeed050: TMenuItem;
    mnuPlaybackSpeed075: TMenuItem;
    mnuPlaybackSpeed100: TMenuItem;
    mnuPlaybackSpeed125: TMenuItem;
    mnuPlaybackSpeed150: TMenuItem;
    mnuPlaybackSpeed200: TMenuItem;
    mnuPlaybackSep4: TMenuItem;
    mnuPlaybackABLoop: TMenuItem;
    mnuPlaybackSetA: TMenuItem;
    mnuPlaybackSetB: TMenuItem;
    mnuPlaybackClearAB: TMenuItem;
    mnuPlaybackSep5: TMenuItem;
    mnuPlaybackChapters: TMenuItem;
    mnuPlaybackPrevChapter: TMenuItem;
    mnuPlaybackNextChapter: TMenuItem;
    mnuPlaybackSep6: TMenuItem;
    mnuPlaybackFrameStep: TMenuItem;
    mnuPlaybackFrameBack: TMenuItem;
    { Phase 12: DVD Navigation }
    mnuPlaybackSep7: TMenuItem;
    mnuPlaybackDVD: TMenuItem;
    mnuDVDMenu: TMenuItem;
    mnuDVDSep1: TMenuItem;
    mnuDVDUp: TMenuItem;
    mnuDVDDown: TMenuItem;
    mnuDVDLeft: TMenuItem;
    mnuDVDRight: TMenuItem;
    mnuDVDSep2: TMenuItem;
    mnuDVDSelect: TMenuItem;
    mnuPlaybackDVDTitles: TMenuItem;
    { Phase 13: Go to Time }
    mnuPlaybackSep8: TMenuItem;
    mnuPlaybackGotoTime: TMenuItem;
    { Phase 15: Bookmarks }
    mnuPlaybackSep9: TMenuItem;
    mnuPlaybackAddBookmark: TMenuItem;
    mnuPlaybackBookmarks: TMenuItem;
    mnuAudio: TMenuItem;
    mnuAudioTrack: TMenuItem;
    mnuAudioVolUp: TMenuItem;
    mnuAudioVolDown: TMenuItem;
    mnuAudioMute: TMenuItem;
    mnuAudioSep1: TMenuItem;
    mnuAudioEqualizer: TMenuItem;
    mnuAudioSep2: TMenuItem;
    mnuAudioDelay: TMenuItem;
    mnuAudioDelayPlus: TMenuItem;
    mnuAudioDelayMinus: TMenuItem;
    mnuAudioDelayReset: TMenuItem;
    { Phase 10: Audio normalization }
    mnuAudioSep3: TMenuItem;
    mnuAudioNormalize: TMenuItem;
    mnuVideo: TMenuItem;
    mnuVideoFullscreen: TMenuItem;
    mnuVideoSep1: TMenuItem;
    mnuVideoAspect: TMenuItem;
    mnuVideoAspectAuto: TMenuItem;
    mnuVideoAspect169: TMenuItem;
    mnuVideoAspect43: TMenuItem;
    mnuVideoAspect235: TMenuItem;
    mnuVideoSep2: TMenuItem;
    mnuVideoScreenshot: TMenuItem;
    mnuVideoSep3: TMenuItem;
    mnuVideoRotation: TMenuItem;
    mnuVideoRotate0: TMenuItem;
    mnuVideoRotate90: TMenuItem;
    mnuVideoRotate180: TMenuItem;
    mnuVideoRotate270: TMenuItem;
    mnuVideoFlip: TMenuItem;
    mnuVideoFlipH: TMenuItem;
    mnuVideoFlipV: TMenuItem;
    mnuVideoSep4: TMenuItem;
    mnuVideoDeinterlace: TMenuItem;
    mnuVideoSep5: TMenuItem;
    mnuVideoZoom: TMenuItem;
    mnuVideoZoomIn: TMenuItem;
    mnuVideoZoomOut: TMenuItem;
    mnuVideoZoomReset: TMenuItem;
    mnuVideoPan: TMenuItem;
    mnuVideoPanLeft: TMenuItem;
    mnuVideoPanRight: TMenuItem;
    mnuVideoPanUp: TMenuItem;
    mnuVideoPanDown: TMenuItem;
    mnuVideoPanReset: TMenuItem;
    { Phase 9: Window options }
    mnuView: TMenuItem;
    mnuViewAlwaysOnTop: TMenuItem;
    mnuViewSep1: TMenuItem;
    mnuViewFitToVideo: TMenuItem;
    mnuViewScale: TMenuItem;
    mnuViewScale50: TMenuItem;
    mnuViewScale100: TMenuItem;
    mnuViewScale150: TMenuItem;
    mnuViewScale200: TMenuItem;
    { Phase 26: Visualization }
    mnuViewSep2: TMenuItem;
    mnuViewVisualization: TMenuItem;
    mnuVisNone: TMenuItem;
    mnuVisSpectrum: TMenuItem;
    mnuVisWaveform: TMenuItem;
    mnuVisVector: TMenuItem;
    mnuVisVolume: TMenuItem;
    mnuVisSep1: TMenuItem;
    mnuVisNextMode: TMenuItem;
    mnuVisNextColor: TMenuItem;
    mnuVideoScreenshotAs: TMenuItem;
    { Phase 10: Video color adjustments }
    mnuVideoSep6: TMenuItem;
    mnuVideoColors: TMenuItem;
    mnuVideoBrightness: TMenuItem;
    mnuVideoBrightnessUp: TMenuItem;
    mnuVideoBrightnessDown: TMenuItem;
    mnuVideoBrightnessReset: TMenuItem;
    mnuVideoContrast: TMenuItem;
    mnuVideoContrastUp: TMenuItem;
    mnuVideoContrastDown: TMenuItem;
    mnuVideoContrastReset: TMenuItem;
    mnuVideoSaturation: TMenuItem;
    mnuVideoSaturationUp: TMenuItem;
    mnuVideoSaturationDown: TMenuItem;
    mnuVideoSaturationReset: TMenuItem;
    mnuVideoHue: TMenuItem;
    mnuVideoHueUp: TMenuItem;
    mnuVideoHueDown: TMenuItem;
    mnuVideoHueReset: TMenuItem;
    mnuVideoGamma: TMenuItem;
    mnuVideoGammaUp: TMenuItem;
    mnuVideoGammaDown: TMenuItem;
    mnuVideoGammaReset: TMenuItem;
    mnuVideoColorsReset: TMenuItem;
    mnuVideoTrack: TMenuItem;
    mnuVideoSep7: TMenuItem;
    mnuVideoAdjustDialog: TMenuItem;
    SaveDialog: TSaveDialog;
    mnuSubtitle: TMenuItem;
    mnuSubtitleTrack: TMenuItem;
    mnuSubtitleLoad: TMenuItem;
    mnuSubtitleSep1: TMenuItem;
    mnuSubtitleDelay: TMenuItem;
    mnuSubtitleDelayPlus: TMenuItem;
    mnuSubtitleDelayMinus: TMenuItem;
    mnuSubtitleDelayReset: TMenuItem;
    mnuTools: TMenuItem;
    mnuToolsPlaylist: TMenuItem;
    mnuToolsRadios: TMenuItem;
    mnuToolsHistory: TMenuItem;
    mnuToolsFavorites: TMenuItem;
    mnuToolsAddFavorite: TMenuItem;
    mnuToolsSep1: TMenuItem;
    mnuToolsSleepTimer: TMenuItem;
    mnuToolsCancelTimer: TMenuItem;
    { Phase 27: Stream Recording }
    mnuToolsSep4: TMenuItem;
    mnuToolsRecord: TMenuItem;
    mnuToolsStopRecord: TMenuItem;
    mnuToolsSep3: TMenuItem;
    mnuToolsLanguage: TMenuItem;
    mnuToolsSep2: TMenuItem;
    mnuToolsShortcuts: TMenuItem;
    mnuToolsOptions: TMenuItem;
    mnuHelp: TMenuItem;
    mnuHelpMediaInfo: TMenuItem;
    mnuHelpLog: TMenuItem;  { Phase 25 }
    mnuHelpSep1: TMenuItem;
    mnuHelpAbout: TMenuItem;
    OpenDialog: TOpenDialog;
    pnlVideo: TPanel;
    pnlControls: TPanel;
    pnlSeek: TPanel;
    lblTime: TLabel;
    lblDuration: TLabel;
    pnlButtons: TPanel;
    btnPlay: TSpeedButton;
    btnStop: TSpeedButton;
    btnPrevious: TSpeedButton;
    btnNext: TSpeedButton;
    btnMute: TSpeedButton;
    tbVolume: TTrackBar;
    lblVolume: TLabel;
    btnFullscreen: TSpeedButton;
    StatusBar: TStatusBar;
    TimerPosition: TTimer;
    TimerHideControls: TTimer;
    TimerRestoreBounds: TTimer;
    { Phase 14: System Tray }
    TrayIcon: TTrayIcon;
    TrayPopupMenu: TPopupMenu;
    mnuTrayShow: TMenuItem;
    mnuTraySep1: TMenuItem;
    mnuTrayPlayPause: TMenuItem;
    mnuTrayStop: TMenuItem;
    mnuTrayPrevious: TMenuItem;
    mnuTrayNext: TMenuItem;
    mnuTraySep2: TMenuItem;
    mnuTrayMute: TMenuItem;
    mnuTraySep3: TMenuItem;
    mnuTrayExit: TMenuItem;

    { Phase 20: Video Context Menu }
    VideoPopupMenu: TPopupMenu;
    mnuCtxPlayPause: TMenuItem;
    mnuCtxStop: TMenuItem;
    mnuCtxSep1: TMenuItem;
    mnuCtxPrevious: TMenuItem;
    mnuCtxNext: TMenuItem;
    mnuCtxSep2: TMenuItem;
    mnuCtxFullscreen: TMenuItem;
    mnuCtxAlwaysOnTop: TMenuItem;
    mnuCtxSep3: TMenuItem;
    mnuCtxAspect: TMenuItem;
    mnuCtxAspectAuto: TMenuItem;
    mnuCtxAspect169: TMenuItem;
    mnuCtxAspect43: TMenuItem;
    mnuCtxAspect235: TMenuItem;
    mnuCtxRotation: TMenuItem;
    mnuCtxRotate0: TMenuItem;
    mnuCtxRotate90: TMenuItem;
    mnuCtxRotate180: TMenuItem;
    mnuCtxRotate270: TMenuItem;
    mnuCtxSep4: TMenuItem;
    mnuCtxAudioTrack: TMenuItem;
    mnuCtxSubtitleTrack: TMenuItem;
    mnuCtxSep5: TMenuItem;
    mnuCtxMute: TMenuItem;
    mnuCtxSep6: TMenuItem;
    mnuCtxScreenshot: TMenuItem;
    mnuCtxMediaInfo: TMenuItem;

    { Phase 17: Sleep Timer }
    SleepTimer: TTimer;

    { IPC Timer for external commands }
    IPCTimer: TTimer;
    procedure IPCTimerTimer(Sender: TObject);

    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AppShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);

    { Menu handlers }
    procedure mnuFileOpenClick(Sender: TObject);
    procedure mnuFileOpenURLClick(Sender: TObject);
    procedure mnuFileOpenFolderClick(Sender: TObject);
    procedure mnuFileExitClick(Sender: TObject);
    procedure mnuPlaybackPlayPauseClick(Sender: TObject);
    procedure mnuPlaybackStopClick(Sender: TObject);
    procedure mnuPlaybackSeekFwdClick(Sender: TObject);
    procedure mnuPlaybackSeekBackClick(Sender: TObject);
    procedure mnuAudioVolUpClick(Sender: TObject);
    procedure mnuAudioVolDownClick(Sender: TObject);
    procedure mnuAudioMuteClick(Sender: TObject);
    procedure mnuVideoFullscreenClick(Sender: TObject);
    procedure mnuVideoAspectAutoClick(Sender: TObject);
    procedure mnuVideoAspect169Click(Sender: TObject);
    procedure mnuVideoAspect43Click(Sender: TObject);
    procedure mnuVideoAspect235Click(Sender: TObject);
    procedure mnuVideoScreenshotClick(Sender: TObject);
    procedure mnuVideoRotate0Click(Sender: TObject);
    procedure mnuVideoRotate90Click(Sender: TObject);
    procedure mnuVideoRotate180Click(Sender: TObject);
    procedure mnuVideoRotate270Click(Sender: TObject);
    procedure mnuVideoFlipHClick(Sender: TObject);
    procedure mnuVideoFlipVClick(Sender: TObject);
    procedure mnuVideoDeinterlaceClick(Sender: TObject);
    procedure mnuPlaybackSpeed050Click(Sender: TObject);
    procedure mnuPlaybackSpeed075Click(Sender: TObject);
    procedure mnuPlaybackSpeed100Click(Sender: TObject);
    procedure mnuPlaybackSpeed125Click(Sender: TObject);
    procedure mnuPlaybackSpeed150Click(Sender: TObject);
    procedure mnuPlaybackSpeed200Click(Sender: TObject);
    procedure mnuPlaybackSetAClick(Sender: TObject);
    procedure mnuPlaybackSetBClick(Sender: TObject);
    procedure mnuPlaybackClearABClick(Sender: TObject);
    procedure mnuSubtitleLoadClick(Sender: TObject);
    procedure mnuToolsPlaylistClick(Sender: TObject);
    procedure mnuToolsRadiosClick(Sender: TObject);
    procedure mnuAudioEqualizerClick(Sender: TObject);
    procedure mnuToolsOptionsClick(Sender: TObject);
    procedure mnuHelpMediaInfoClick(Sender: TObject);
    procedure mnuHelpLogClick(Sender: TObject);  { Phase 25 }
    procedure mnuHelpAboutClick(Sender: TObject);

    { Button handlers }
    procedure btnPlayClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnMuteClick(Sender: TObject);
    procedure btnFullscreenClick(Sender: TObject);

    { Trackbar handlers }
    procedure SeekBarChange(Sender: TObject);
    procedure SeekBarScrollEnd(Sender: TObject);
    procedure tbVolumeChange(Sender: TObject);
    procedure tbVolumeMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    { Timer handlers }
    procedure TimerPositionTimer(Sender: TObject);
    procedure TimerHideControlsTimer(Sender: TObject);
    procedure TimerRestoreBoundsTimer(Sender: TObject);

    { Video panel handlers }
    procedure pnlVideoClick(Sender: TObject);
    procedure pnlVideoDblClick(Sender: TObject);
    procedure pnlVideoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pnlVideoMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure pnlSeekMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    { Phase 8: Chapter and frame navigation }
    procedure mnuPlaybackPrevChapterClick(Sender: TObject);
    procedure mnuPlaybackNextChapterClick(Sender: TObject);
    procedure mnuPlaybackFrameStepClick(Sender: TObject);
    procedure mnuPlaybackFrameBackClick(Sender: TObject);

    { Phase 8: Audio delay }
    procedure mnuAudioDelayPlusClick(Sender: TObject);
    procedure mnuAudioDelayMinusClick(Sender: TObject);
    procedure mnuAudioDelayResetClick(Sender: TObject);

    { Phase 8: Subtitle delay }
    procedure mnuSubtitleDelayPlusClick(Sender: TObject);
    procedure mnuSubtitleDelayMinusClick(Sender: TObject);
    procedure mnuSubtitleDelayResetClick(Sender: TObject);

    { Phase 8: Video zoom/pan }
    procedure mnuVideoZoomInClick(Sender: TObject);
    procedure mnuVideoZoomOutClick(Sender: TObject);
    procedure mnuVideoZoomResetClick(Sender: TObject);
    procedure mnuVideoPanLeftClick(Sender: TObject);
    procedure mnuVideoPanRightClick(Sender: TObject);
    procedure mnuVideoPanUpClick(Sender: TObject);
    procedure mnuVideoPanDownClick(Sender: TObject);
    procedure mnuVideoPanResetClick(Sender: TObject);

    { Phase 9: View and window options }
    procedure mnuViewAlwaysOnTopClick(Sender: TObject);
    procedure mnuViewFitToVideoClick(Sender: TObject);
    procedure mnuViewScale50Click(Sender: TObject);
    procedure mnuViewScale100Click(Sender: TObject);
    procedure mnuViewScale150Click(Sender: TObject);
    procedure mnuViewScale200Click(Sender: TObject);
    { Phase 26: Visualization }
    procedure mnuVisNoneClick(Sender: TObject);
    procedure mnuVisSpectrumClick(Sender: TObject);
    procedure mnuVisWaveformClick(Sender: TObject);
    procedure mnuVisVectorClick(Sender: TObject);
    procedure mnuVisVolumeClick(Sender: TObject);
    procedure mnuVisNextModeClick(Sender: TObject);
    procedure mnuVisNextColorClick(Sender: TObject);
    procedure mnuVideoScreenshotAsClick(Sender: TObject);

    { Phase 10: Video color adjustments }
    procedure mnuVideoBrightnessUpClick(Sender: TObject);
    procedure mnuVideoBrightnessDownClick(Sender: TObject);
    procedure mnuVideoBrightnessResetClick(Sender: TObject);
    procedure mnuVideoContrastUpClick(Sender: TObject);
    procedure mnuVideoContrastDownClick(Sender: TObject);
    procedure mnuVideoContrastResetClick(Sender: TObject);
    procedure mnuVideoSaturationUpClick(Sender: TObject);
    procedure mnuVideoSaturationDownClick(Sender: TObject);
    procedure mnuVideoSaturationResetClick(Sender: TObject);
    procedure mnuVideoHueUpClick(Sender: TObject);
    procedure mnuVideoHueDownClick(Sender: TObject);
    procedure mnuVideoHueResetClick(Sender: TObject);
    procedure mnuVideoGammaUpClick(Sender: TObject);
    procedure mnuVideoGammaDownClick(Sender: TObject);
    procedure mnuVideoGammaResetClick(Sender: TObject);
    procedure mnuVideoColorsResetClick(Sender: TObject);
    procedure mnuVideoAdjustDialogClick(Sender: TObject);

    { Phase 10: Audio normalization }
    procedure mnuAudioNormalizeClick(Sender: TObject);

    { Phase 10: Clipboard URL }
    procedure mnuFileOpenClipboardClick(Sender: TObject);

    { Phase 12: DVD/Bluray }
    procedure mnuFileOpenDVDClick(Sender: TObject);
    procedure mnuFileOpenBlurayClick(Sender: TObject);
    procedure mnuDVDMenuClick(Sender: TObject);
    procedure mnuDVDUpClick(Sender: TObject);
    procedure mnuDVDDownClick(Sender: TObject);
    procedure mnuDVDLeftClick(Sender: TObject);
    procedure mnuDVDRightClick(Sender: TObject);
    procedure mnuDVDSelectClick(Sender: TObject);

    { Phase 13: Go to Time and History }
    procedure mnuPlaybackGotoTimeClick(Sender: TObject);
    procedure mnuToolsHistoryClick(Sender: TObject);

    { Phase 14: System Tray }
    procedure TrayIconClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure mnuTrayShowClick(Sender: TObject);
    procedure mnuTrayPlayPauseClick(Sender: TObject);
    procedure mnuTrayStopClick(Sender: TObject);
    procedure mnuTrayPreviousClick(Sender: TObject);
    procedure mnuTrayNextClick(Sender: TObject);
    procedure mnuTrayMuteClick(Sender: TObject);
    procedure mnuTrayExitClick(Sender: TObject);

    { Phase 15: Bookmarks }
    procedure mnuPlaybackAddBookmarkClick(Sender: TObject);
    procedure mnuPlaybackBookmarksClick(Sender: TObject);

    { Phase 16: Favorites }
    procedure mnuToolsFavoritesClick(Sender: TObject);
    procedure mnuToolsAddFavoriteClick(Sender: TObject);

    { Phase 17: Sleep Timer }
    procedure mnuToolsSleepTimerClick(Sender: TObject);
    procedure mnuToolsCancelTimerClick(Sender: TObject);
    procedure SleepTimerTimer(Sender: TObject);

    { Phase 18: Keyboard Shortcuts }
    procedure mnuToolsShortcutsClick(Sender: TObject);

    { Phase 27: Stream Recording }
    procedure mnuToolsRecordClick(Sender: TObject);
    procedure mnuToolsStopRecordClick(Sender: TObject);

    { Phase 20: Video Context Menu }
    procedure VideoPopupMenuPopup(Sender: TObject);

  private
    FMPVEngine: TMPVEngine;
    FMPVInitialized: Boolean;  { Track if MPV was initialized }
    FPlaylistManager: TPlaylistManager;
    FPlaylistForm: TfrmPlaylist;
    FRadioManager: TRadioManager;
    FRadiosForm: TfrmRadios;
    FEqualizerForm: TfrmEqualizer;
    FOptionsForm: TfrmOptions;
    FMediaInfoForm: TfrmMediaInfo;
    FAboutForm: TfrmAbout;
    FGotoTimeForm: TfrmGotoTime;
    FHistoryForm: TfrmHistory;
    FBookmarksForm: TfrmBookmarks;
    FFavoritesForm: TfrmFavorites;
    FCurrentFileName: string;
    FStartupPath: string;  { Path to play after form is shown }
    FStartupOptions: TStartupOptions;  { CLI startup options }
    FStartupOptionsSet: Boolean;  { True if CLI options were set }
    FStartTimeApplied: Boolean;  { True if CLI start time was applied }
    FSubFileApplied: Boolean;  { True if CLI subtitle was applied }
    FFullscreen: Boolean;
    FExitingFullscreen: Boolean;
    FBoundsToRestore: TRect;
    FRestoreBoundsRetries: Integer;
    FSeekBar: TVssScrollbar;
    FLastSeekPosition: Integer;  { Track last seeked position to prevent double-seek }
    FLastMouseMove: TDateTime;
    FLastMousePos: TPoint;
    FControlsVisible: Boolean;
    FSavedBounds: TRect;
    FSavedWindowState: TWindowState;
    FPlaybackSpeed: Double;
    FABLoopA: Double;
    FABLoopB: Double;
    FABLoopActive: Boolean;
    FVideoRotation: Integer;
    FVideoFlipH: Boolean;
    FVideoFlipV: Boolean;
    FDeinterlace: Boolean;
    FAudioDelay: Double;
    FSubtitleDelay: Double;
    FVideoZoom: Double;
    FVideoPanX: Double;
    FVideoPanY: Double;
    FAlwaysOnTop: Boolean;
    FAudioNormalize: Boolean;
    FClosingToTray: Boolean;
    FForceClose: Boolean;
    { Phase 17: Sleep Timer }
    FSleepTimerAction: TSleepTimerAction;
    FSleepTimerRemaining: Integer;  { Seconds remaining }
    { Phase 26: Visualization }
    FVisualEffects: TVisualEffects;
    FChangingVisualization: Boolean;  { Flag to ignore EOF during filter change }
    FIgnoreNextEndFile: Boolean;  { Flag to ignore EndFile event after PlayFile }
    FVisualReapplyTimer: TTimer;  { Timer to reapply visualization after file load }
    FVisualReapplyNeeded: Boolean;  { True = apply filter, False = just reset flag }
    FVisualLoadTimer: TTimer;  { Timer to load file after filter clear }
    FPendingPlayFile: string;  { File to play after filter clear delay }
    FWatchdogTriggered: Boolean;  { Prevent multiple watchdog triggers for showwaves EOF }
    FRestoreVisAfterLoad: Boolean;  { Flag to restore visualization after file loads }
    FRestoreVisMode: TVisualMode;  { Mode to restore after file loads }
    { Phase 27: Stream Recording }
    FStreamRecorder: TStreamRecorder;
    { First launch timer for GTK2 window sizing }
    FFirstLaunchTimer: TTimer;
    { CLI fullscreen timer - delay to ensure window is fully realized }
    FFullscreenTimer: TTimer;
    { Playlist docking }
    FPlaylistDockSide: TDockSide;
    FUpdatingDockedWindows: Boolean;
    FSavedPlaylistBounds: TRect;
    FSavedPlaylistDockSide: TDockSide;
    { Saved menu shortcuts for single-key items }
    FSavedShortcuts: array of record
      MenuItem: TMenuItem;
      ShortCut: TShortCut;
    end;

    procedure InitializeMPV;
    procedure SaveSingleKeyShortcuts;
    procedure DisableSingleKeyShortcuts;
    procedure RestoreSingleKeyShortcuts;
    procedure ShutdownMPV;

    { MPV event handlers }
    procedure OnMPVLog(Sender: TObject; const Msg: string);
    procedure OnMPVStatusChange(Sender: TObject; OldStatus, NewStatus: TMPVStatus);
    procedure OnMPVPositionChange(Sender: TObject; PositionSec, PositionPercent: Double);
    procedure OnMPVEndFile(Sender: TObject; Reason, ErrorCode: Integer);
    procedure OnMPVFileLoaded(Sender: TObject; var FileName: string);
    procedure OnMPVVideoResize(Sender: TObject; AWidth, AHeight: Integer);
    procedure OnMPVMetadata(Sender: TObject; const Key, Value: string);

    { Phase 25: Log command handler }
    procedure OnLogSendCommand(Sender: TObject);

    procedure PlayFile(const FileName: string);
    procedure UpdateTimeDisplay;
    procedure UpdateVolumeDisplay;
    procedure UpdatePlayPauseButton;
    procedure SetFullscreen(AFullscreen: Boolean);
    procedure ShowControls;
    procedure HideControls;
    function FormatTime(Seconds: Double): string;

    { Playlist handlers }
    procedure OnPlaylistPlay(Sender: TObject; Index: Integer; const FileName: string);
    procedure PlayNextInPlaylist;
    procedure PlayPreviousInPlaylist;

    { Radio handlers }
    procedure OnRadioPlay(Sender: TObject; const Station: TRadioStation);

    { Recent files and track menus }
    procedure UpdateRecentFilesMenu;
    procedure OnRecentFileClick(Sender: TObject);
    procedure UpdateAudioTrackMenu;
    procedure UpdateSubtitleTrackMenu;
    procedure OnAudioTrackClick(Sender: TObject);
    procedure OnSubtitleTrackClick(Sender: TObject);

    { Phase 7: Advanced playback features }
    procedure SetPlaybackSpeed(Speed: Double);
    procedure SetABLoopA;
    procedure SetABLoopB;
    procedure ClearABLoop;
    procedure SetVideoRotation(Angle: Integer);
    procedure ToggleVideoFlipH;
    procedure ToggleVideoFlipV;
    procedure UpdateVideoFilters;
    procedure UpdateAspectMenuChecks(Mode: Integer);
    procedure ToggleDeinterlace;
    procedure SavePlaybackPosition;
    procedure RestorePlaybackPosition;
    procedure PlayFileAsync(Data: PtrInt);
    procedure PlayFolderAsync(Data: PtrInt);
    procedure RefreshSeekPanelAsync(Data: PtrInt);
    procedure ApplyVisualizationAsync(Data: PtrInt);
    procedure AsyncPlayNextInPlaylist(Data: PtrInt);
    procedure OnFirstLaunchTimer(Sender: TObject);
    procedure OnFullscreenTimer(Sender: TObject);
    procedure OnVisualReapplyTimer(Sender: TObject);
    procedure OnVisualLoadTimer(Sender: TObject);

    { Phase 8: Advanced navigation and controls }
    procedure UpdateChaptersMenu;
    procedure OnChapterClick(Sender: TObject);
    procedure FrameStep(Forward: Boolean);
    procedure SetAudioDelay(Delay: Double);
    procedure SetSubtitleDelay(Delay: Double);
    procedure SetVideoZoom(Zoom: Double);
    procedure SetVideoPan(X, Y: Double);

    { Phase 9: View and window options }
    procedure ToggleAlwaysOnTop;
    procedure FitWindowToVideo;
    procedure ScaleWindow(Scale: Double);
    procedure TakeScreenshotWithDialog;

    { Phase 10: Video color adjustments }
    procedure AdjustBrightness(Delta: Integer);
    procedure AdjustContrast(Delta: Integer);
    procedure AdjustSaturation(Delta: Integer);
    procedure AdjustHue(Delta: Integer);
    procedure AdjustGamma(Delta: Integer);
    procedure ResetAllColors;
    procedure UpdateVideoTrackMenu;
    procedure OnVideoTrackClick(Sender: TObject);
    procedure ToggleAudioNormalize;
    procedure OpenClipboardURL;

    { Phase 11: Internationalization }
    procedure InitializeLanguage;
    procedure UpdateLanguageMenu;
    procedure OnLanguageClick(Sender: TObject);
    procedure ApplyTranslations;

    { Phase 12: DVD/Bluray }
    procedure OpenDVD;
    procedure OpenBluray;
    procedure UpdateDVDTitlesMenu;
    procedure OnDVDTitleClick(Sender: TObject);
    procedure UpdateDVDMenuState;

    { Phase 13: History }
    procedure OnHistoryPlay(Sender: TObject; const FileName: string);

    { Phase 14: System Tray }
    procedure InitializeTrayIcon;
    procedure UpdateTrayIconHint;
    procedure ShowMainWindow;
    procedure HideToTray;

    { Phase 15: Bookmarks }
    procedure OnBookmarkGoto(Sender: TObject; const FileName: string; BookmarkPos: Double);
    procedure UpdateBookmarksMenu;
    procedure OnBookmarkMenuClick(Sender: TObject);

    { Phase 16: Favorites }
    procedure OnFavoritePlay(Sender: TObject; const Path: string);
    function GetCurrentFavoriteType: TFavoriteType;

    { Phase 17: Sleep Timer }
    procedure UpdateSleepTimerDisplay;
    procedure CancelSleepTimer;
    procedure ExecuteSleepTimerAction;

    { Phase 21: Open Folder and Enhanced Drag & Drop }
    function IsMediaFile(const FileName: string): Boolean;
    procedure ScanFolderForMedia(const FolderPath: string; FileList: TStrings; Recursive: Boolean);
    procedure AddFilesToPlaylist(const FileNames: array of string; ClearFirst: Boolean);
    procedure OpenFolder;
    function GetCacheSizeForMedia(const FileName: string): Integer;

    { Phase 24: Session Playlist Save/Restore }
    procedure SaveSessionPlaylist;
    procedure RestoreSessionPlaylist;

    { Phase 26: Visualization }
    procedure SetVisualizationMode(Mode: TVisualMode);
    procedure ApplyVisualization;
    procedure UpdateVisualizationMenu;
    procedure UpdateVisualizationEnabled;
    procedure OnVisFilterChanged(Sender: TObject);
    procedure OnEqualizerChange(Sender: TObject);

    { Phase 27: Stream Recording }
    procedure StartStreamRecording;
    procedure StopStreamRecording;
    procedure UpdateRecordingMenu;
    procedure OnRecordingStateChange(Sender: TObject; OldState, NewState: TRecordingState);
    procedure OnRecordingProgress(Sender: TObject; Duration: TTime; FileSize: Int64);

    { Playlist docking }
    procedure UpdateDockedWindows;
    procedure CheckPlaylistDocking;

  protected
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer; KeepBase: Boolean); override;

  public
    property MPVEngine: TMPVEngine read FMPVEngine;
    procedure PlayStartupFile(const FileName: string);
    procedure AddToPlaylistFromExternal(const Path: string);
    procedure EnqueueToPlaylistFromExternal(const Path: string);
    procedure SetStartupOptions(const Options: TStartupOptions);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{$IFDEF LCLQt5}
{ Fix menu bar background color for Qt5 in Snap/sandboxed environments }
procedure FixMenuBarPalette(AMainMenu: TMainMenu);
var
  QtMenuBar: TQtMenuBar;
  Palette: QPaletteH;
  BgColor, FgColor: TQColor;
begin
  if not Assigned(AMainMenu) then Exit;
  if not AMainMenu.HandleAllocated then Exit;

  QtMenuBar := TQtMenuBar(AMainMenu.Handle);
  if QtMenuBar = nil then Exit;

  { Create palette with explicit colors }
  Palette := QPalette_create();
  try
    { Light gray background (standard menu color) }
    QColor_fromRgb(@BgColor, 239, 239, 239);  { #EFEFEF }
    QColor_fromRgb(@FgColor, 0, 0, 0);        { Black text }

    QPalette_setColor(Palette, QPaletteWindow, @BgColor);
    QPalette_setColor(Palette, QPaletteButton, @BgColor);
    QPalette_setColor(Palette, QPaletteBase, @BgColor);
    QPalette_setColor(Palette, QPaletteWindowText, @FgColor);
    QPalette_setColor(Palette, QPaletteButtonText, @FgColor);
    QPalette_setColor(Palette, QPaletteText, @FgColor);

    { Apply palette to menu bar widget }
    QWidget_setPalette(QtMenuBar.Widget, Palette);
    QWidget_setAutoFillBackground(QtMenuBar.Widget, True);
  finally
    QPalette_destroy(Palette);
  end;
end;
{$ENDIF}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  { Fix control order for proper alignment: alBottom controls must be processed
    before alClient. Move pnlVideo to end so it fills remaining space after
    StatusBar and pnlControls have claimed their space. }
  pnlVideo.Align := alNone;
  StatusBar.Align := alBottom;
  pnlControls.Align := alBottom;
  pnlVideo.Align := alClient;

  FFullscreen := False;
  FControlsVisible := True;
  FLastMouseMove := Now;
  FPlaybackSpeed := 1.0;
  FABLoopA := -1;
  FABLoopB := -1;
  FPlaylistDockSide := dsNone;
  FUpdatingDockedWindows := False;
  FABLoopActive := False;
  FVideoRotation := 0;
  FVideoFlipH := False;
  FVideoFlipV := False;
  FDeinterlace := False;
  UpdateAspectMenuChecks(ASPECT_AUTO);
  FAudioDelay := 0;
  FSubtitleDelay := 0;
  FVideoZoom := 1.0;
  FVideoPanX := 0;
  FVideoPanY := 0;
  FAlwaysOnTop := False;
  {$IFNDEF WINDOWS}
  { Always On Top only works reliably on Windows - Qt5 widgetset recreates window }
  mnuViewAlwaysOnTop.Visible := False;
  mnuViewAlwaysOnTop.ShortCut := 0;
  mnuCtxAlwaysOnTop.Visible := False;
  {$ENDIF}
  FAudioNormalize := False;
  FClosingToTray := False;
  FForceClose := False;
  FSleepTimerAction := staStopPlayback;
  FSleepTimerRemaining := 0;

  { Phase 26: Initialize Visual Effects }
  FVisualEffects := TVisualEffects.Create;
  FVisualEffects.OnFilterChanged := @OnVisFilterChanged;
  FChangingVisualization := False;

  { Phase 27: Initialize Stream Recorder }
  FStreamRecorder := TStreamRecorder.Create;
  FStreamRecorder.OnStateChange := @OnRecordingStateChange;
  FStreamRecorder.OnProgress := @OnRecordingProgress;

  { Set window title }
  Caption := APP_NAME;

  { Fix menu bar background color for Qt5 in Snap environment }
  {$IFDEF LCLQt5}
  FixMenuBarPalette(MainMenu);
  {$ENDIF}

  { Set minimum size }
  Constraints.MinWidth := DEF_MAIN_MIN_WIDTH;
  Constraints.MinHeight := DEF_MAIN_MIN_HEIGHT;

  { Enable drag & drop }
  AllowDropFiles := True;

  { Initialize volume slider }
  tbVolume.Min := VOL_MIN;
  tbVolume.Max := VOL_MAX;
  tbVolume.Position := Config.Settings.Audio.Volume;

  { Create and initialize custom seek bar }
  FSeekBar := TVssScrollbar.Create(Self);
  FSeekBar.Parent := pnlSeek;
  FSeekBar.Left := 80;
  FSeekBar.Top := 10;
  FSeekBar.Width := pnlSeek.Width - 160;
  FSeekBar.Height := 20;
  FSeekBar.Anchors := [akLeft, akTop, akRight];
  FSeekBar.Min := 0;
  FSeekBar.Max := 1000;
  FSeekBar.Position := 0;
  FSeekBar.SliderWidth := 14;
  FSeekBar.SliderHeight := 18;
  FSeekBar.SliderLineHeight := 8;
  FSeekBar.ScrollButtonColor := $FFCC99;  { Light blue accent }
  FSeekBar.BackgroundColor := COLOR_PANEL;
  FSeekBar.OnChange := @SeekBarChange;
  FSeekBar.OnScrollEnd := @SeekBarScrollEnd;
  FSeekBar.OnMouseWheel := @pnlSeekMouseWheel;
  FLastSeekPosition := -1;  { Initialize seek position tracking }

  { Set panel colors }
  pnlVideo.Color := clBlack;
  pnlControls.Color := COLOR_PANEL;
  pnlSeek.Color := COLOR_PANEL;
  pnlButtons.Color := COLOR_PANEL;

  { Assign mouse wheel handler }
  pnlVideo.OnMouseWheel := @pnlVideoMouseWheel;

  { Prepare window position mode (actual positioning done in FormShow for GTK2) }
  if Config.HasWindowState('MainWindow') then
    Position := poDesigned  { Will use explicit Left/Top from LoadWindowState }
  else
    Position := poScreenCenter;  { Let LCL center the window }

  { MPV will be initialized in FormShow when the widget is realized }
  FMPVInitialized := False;

  { Initialize Playlist Manager }
  FPlaylistManager := TPlaylistManager.Create;
  FPlaylistManager.OnPlay := @OnPlaylistPlay;
  FPlaylistManager.PlaybackMode := Config.Settings.PlaybackMode;

  { Create Playlist Form (hidden by default) }
  FPlaylistForm := TfrmPlaylist.Create(Self);
  FPlaylistForm.PlaylistManager := FPlaylistManager;
  FPlaylistForm.OnPlayRequest := @OnPlaylistPlay;
  FPlaylistForm.UpdateToolbarState;

  { Initialize Radio Manager }
  FRadioManager := TRadioManager.Create;

  { Create Radios Form (hidden by default) }
  FRadiosForm := TfrmRadios.Create(Self);
  FRadiosForm.RadioManager := FRadioManager;
  FRadiosForm.OnPlayRequest := @OnRadioPlay;

  { Create Equalizer Form (hidden by default) }
  FEqualizerForm := TfrmEqualizer.Create(Self);
  FEqualizerForm.MPVEngine := FMPVEngine;
  FEqualizerForm.OnEqualizerChange := @OnEqualizerChange;

  { Create Options Form (hidden by default) }
  FOptionsForm := TfrmOptions.Create(Self);

  { Create Media Info Form (hidden by default) }
  FMediaInfoForm := TfrmMediaInfo.Create(Self);

  { Create About Form (hidden by default) }
  FAboutForm := TfrmAbout.Create(Self);

  { Initialize current file name }
  FCurrentFileName := '';
  FStartupPath := '';

  { Initialize language system }
  InitializeLanguage;

  { Update recent files menu }
  UpdateRecentFilesMenu;

  { Initialize system tray icon }
  InitializeTrayIcon;

  { Start timers }
  TimerPosition.Interval := TIMER_POSITION_UPDATE;
  TimerPosition.Enabled := True;

  TimerHideControls.Interval := 200; { Check mouse position every 200ms }
  TimerHideControls.Enabled := True;

  { Timer for restoring bounds after fullscreen exit }
  TimerRestoreBounds := TTimer.Create(Self);
  TimerRestoreBounds.Interval := 150; { Wait 150ms for window manager to finish }
  TimerRestoreBounds.Enabled := False;
  TimerRestoreBounds.OnTimer := @TimerRestoreBoundsTimer;

  { Phase 24: Restore session playlist }
  RestoreSessionPlaylist;

  { IPC Timer for receiving commands from other instances }
  IPCTimer := TTimer.Create(Self);
  IPCTimer.Interval := 500; { Check every 500ms }
  IPCTimer.Enabled := True;
  IPCTimer.OnTimer := @IPCTimerTimer;

  { Timer for reapplying visualization after file load }
  FVisualReapplyTimer := TTimer.Create(Self);
  FVisualReapplyTimer.Interval := 800; { Wait 800ms for MPV to stabilize }
  FVisualReapplyTimer.Enabled := False;
  FVisualReapplyTimer.OnTimer := @OnVisualReapplyTimer;

  { Timer for loading file after filter clear }
  FVisualLoadTimer := TTimer.Create(Self);
  FVisualLoadTimer.Interval := 500; { Wait 500ms after stopping and clearing filter }
  FVisualLoadTimer.Enabled := False;
  FVisualLoadTimer.OnTimer := @OnVisualLoadTimer;

  { Timer for first launch window sizing (GTK2 workaround) }
  FFirstLaunchTimer := TTimer.Create(Self);
  FFirstLaunchTimer.Interval := 1; { Minimal delay - fires after GTK2 realizes window }
  FFirstLaunchTimer.Enabled := False;
  FFirstLaunchTimer.OnTimer := @OnFirstLaunchTimer;

  { Timer for CLI fullscreen - delay to ensure window is fully realized }
  FFullscreenTimer := TTimer.Create(Self);
  FFullscreenTimer.Interval := 150; { Delay after FormShow to ensure window is ready }
  FFullscreenTimer.Enabled := False;
  FFullscreenTimer.OnTimer := @OnFullscreenTimer;

  { Capture shortcuts at application level to work even when MPV has focus }
  Application.OnShortCut := @AppShortCut;
end;

procedure TfrmMain.AppShortCut(var Msg: TLMKey; var Handled: Boolean);
var
  Key: Word;
  Shift: TShiftState;
begin
  { Don't intercept shortcuts when another form is active }
  if Screen.ActiveForm <> Self then
    Exit;

  Key := Msg.CharCode;
  Shift := KeyDataToShiftState(Msg.KeyData);

  { Handle our custom shortcuts at application level }
  if ShortcutManager.MatchesShortcut(Key, Shift * [ssCtrl, ssAlt, ssShift], saMute) then
  begin
    mnuAudioMuteClick(nil);
    Handled := True;
  end
  {$IFDEF WINDOWS}
  else if ShortcutManager.MatchesShortcut(Key, Shift * [ssCtrl, ssAlt, ssShift], saAlwaysOnTop) then
  begin
    ToggleAlwaysOnTop;
    Handled := True;
  end
  {$ENDIF}
  else if ShortcutManager.MatchesShortcut(Key, Shift * [ssCtrl, ssAlt, ssShift], saVisNextMode) then
  begin
    mnuVisNextModeClick(nil);
    Handled := True;
  end
  else if ShortcutManager.MatchesShortcut(Key, Shift * [ssCtrl, ssAlt, ssShift], saVisNextColor) then
  begin
    mnuVisNextColorClick(nil);
    Handled := True;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  { Restore window position (must be done in FormShow for GTK2, only once) }
  if not FMPVInitialized then
  begin
    if Config.HasWindowState('MainWindow') then
      Config.LoadWindowState('MainWindow', Self)
    else
    begin
      { Use timer to center window after GTK2 has fully realized it (Lazarus wiki workaround) }
      FFirstLaunchTimer.Enabled := True;
    end;
  end;

  { Initialize MPV now that the form and video panel are visible/realized }
  if not FMPVInitialized then
  begin
    { Force layout update to ensure pnlVideo has correct size }
    Application.ProcessMessages;

    { Fix menu bar background color for Qt5 in Snap environment (retry after widgets realized) }
    {$IFDEF LCLQt5}
    FixMenuBarPalette(MainMenu);
    {$ENDIF}

    { Fix Qt5 TTrackBar vertical offset - Qt5 theme adds ~2px internal margin }
    tbVolume.Top := 12;  { LFM has 10, but Qt5 needs +2 to align visually }

    InitializeMPV;
    FMPVInitialized := True;

    { Update equalizer form with the now-initialized MPV engine }
    if FEqualizerForm <> nil then
      FEqualizerForm.MPVEngine := FMPVEngine;

    { Queue refresh of seek panel to fix initial drawing issues }
    Application.QueueAsyncCall(@RefreshSeekPanelAsync, 0);

    { Initialize visualization menu state (disabled when nothing playing) }
    UpdateVisualizationEnabled;

    { Apply CLI fullscreen option if set (timer delay to ensure window is fully realized) }
    if FStartupOptionsSet and FStartupOptions.FullscreenSet and FStartupOptions.Fullscreen then
      FFullscreenTimer.Enabled := True;

    { Handle startup path (from command line) now that MPV is ready }
    if FStartupPath <> '' then
    begin
      FCurrentFileName := FStartupPath;
      FStartupPath := '';  { Clear to avoid re-triggering }
      if DirectoryExists(FCurrentFileName) then
        Application.QueueAsyncCall(@PlayFolderAsync, 0)
      else
        Application.QueueAsyncCall(@PlayFileAsync, 0);
    end;
  end;

  { Ensure form has keyboard focus for shortcuts }
  Self.SetFocus;
  Self.Invalidate;
  Self.FSeekBar.Invalidate;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  { Restore single-key shortcuts when main form is activated }
  RestoreSingleKeyShortcuts;
  { Ensure form has keyboard focus when activated }
  Self.SetFocus;
end;

procedure TfrmMain.FormDeactivate(Sender: TObject);
begin
  { Disable single-key shortcuts when another form gets focus }
  DisableSingleKeyShortcuts;
end;

procedure TfrmMain.SaveSingleKeyShortcuts;

  procedure CheckMenuItem(Item: TMenuItem);
  var
    I, Idx: Integer;
    Key: Word;
    Shift: TShiftState;
  begin
    { Check if this menu item has a single-key shortcut (no modifiers) }
    if Item.ShortCut <> 0 then
    begin
      ShortCutToKey(Item.ShortCut, Key, Shift);
      if Shift = [] then  { No Ctrl, Alt, or Shift }
      begin
        Idx := Length(FSavedShortcuts);
        SetLength(FSavedShortcuts, Idx + 1);
        FSavedShortcuts[Idx].MenuItem := Item;
        FSavedShortcuts[Idx].ShortCut := Item.ShortCut;
      end;
    end;
    { Recurse into sub-menus }
    for I := 0 to Item.Count - 1 do
      CheckMenuItem(Item.Items[I]);
  end;

var
  I: Integer;
begin
  if Length(FSavedShortcuts) > 0 then Exit;  { Already saved }
  for I := 0 to MainMenu.Items.Count - 1 do
    CheckMenuItem(MainMenu.Items[I]);
end;

procedure TfrmMain.DisableSingleKeyShortcuts;
var
  I: Integer;
begin
  { Save shortcuts first if not done yet }
  SaveSingleKeyShortcuts;
  { Disable all single-key shortcuts }
  for I := 0 to High(FSavedShortcuts) do
    FSavedShortcuts[I].MenuItem.ShortCut := 0;
end;

procedure TfrmMain.RestoreSingleKeyShortcuts;
var
  I: Integer;
begin
  { Restore all single-key shortcuts }
  for I := 0 to High(FSavedShortcuts) do
    FSavedShortcuts[I].MenuItem.ShortCut := FSavedShortcuts[I].ShortCut;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  TimerPosition.Enabled := False;
  TimerHideControls.Enabled := False;
  TimerRestoreBounds.Enabled := False;

  { Save window state }
  Config.SaveWindowState('MainWindow', Self);

  { Save playback mode before freeing }
  if FPlaylistManager <> nil then
    Config.SetPlaybackMode(FPlaylistManager.PlaybackMode);
  Config.Save;

  { Free playlist manager }
  FreeAndNil(FPlaylistManager);

  { Free radio manager }
  FreeAndNil(FRadioManager);

  { Phase 26: Free visual effects }
  FreeAndNil(FVisualEffects);

  { Phase 27: Free stream recorder }
  FreeAndNil(FStreamRecorder);

  { Shutdown MPV }
  ShutdownMPV;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  { Resize the MPV video window to match panel size }
  if FMPVEngine <> nil then
    FMPVEngine.ResizeVideoWindow(pnlVideo.Width, pnlVideo.Height);

  { Continuously save bounds when NOT in fullscreen for proper restore }
  { Skip saving during fullscreen exit to avoid overwriting the restored bounds }
  if (not FFullscreen) and (not FExitingFullscreen) and (WindowState = wsNormal) then
    FSavedBounds := BoundsRect;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ModShift: TShiftState;
begin
  { Don't process keys if another form is active }
  if Screen.ActiveForm <> Self then
    Exit;

  { Only use Ctrl, Alt, Shift for matching }
  ModShift := Shift * [ssCtrl, ssAlt, ssShift];

  { Handle Escape specially for exiting fullscreen }
  if (Key = VK_ESCAPE) and FFullscreen then
  begin
    SetFullscreen(False);
    Exit;
  end;

  { Handle Alt+Enter specially for fullscreen }
  if (Key = VK_RETURN) and (ssAlt in Shift) then
  begin
    SetFullscreen(not FFullscreen);
    Exit;
  end;

  { Handle F11 for fullscreen (standard key) }
  if Key = VK_F11 then
  begin
    if (not FFullscreen) then
    begin

    SetFullscreen(not FFullscreen);
    Exit;
    end;
  end;

  { Use ShortcutManager for all other shortcuts }
  if ShortcutManager.MatchesShortcut(Key, ModShift, saPlayPause) then
    mnuPlaybackPlayPauseClick(nil)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saStop) then
    mnuPlaybackStopClick(nil)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saPrevious) then
    btnPreviousClick(nil)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saNext) then
    btnNextClick(nil)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saSeekForward) then
  begin
    if FMPVEngine <> nil then
      FMPVEngine.SeekRelative(SEEK_SMALL);
  end
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saSeekBackward) then
  begin
    if FMPVEngine <> nil then
      FMPVEngine.SeekRelative(-SEEK_SMALL);
  end
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saSeekForward5) then
  begin
    if FMPVEngine <> nil then
      FMPVEngine.SeekRelative(SEEK_LARGE);
  end
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saSeekBackward5) then
  begin
    if FMPVEngine <> nil then
      FMPVEngine.SeekRelative(-SEEK_LARGE);
  end
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saSpeedUp) then
    SetPlaybackSpeed(FPlaybackSpeed + 0.1)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saSpeedDown) then
    SetPlaybackSpeed(FPlaybackSpeed - 0.1)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saSpeedReset) then
    SetPlaybackSpeed(1.0)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saSetLoopA) then
  begin
    if FABLoopA < 0 then
      SetABLoopA
    else if FABLoopB < 0 then
      SetABLoopB
    else
      ClearABLoop;
  end
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saClearLoop) then
    ClearABLoop
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saFrameForward) then
    FrameStep(True)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saFrameBackward) then
    FrameStep(False)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saPrevChapter) then
    mnuPlaybackPrevChapterClick(nil)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saNextChapter) then
    mnuPlaybackNextChapterClick(nil)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saGotoTime) then
    mnuPlaybackGotoTimeClick(nil)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saVolumeUp) then
  begin
    if FMPVEngine <> nil then
    begin
      FMPVEngine.Volume := FMPVEngine.Volume + 5;
      tbVolume.Position := FMPVEngine.Volume;
    end;
  end
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saVolumeDown) then
  begin
    if FMPVEngine <> nil then
    begin
      FMPVEngine.Volume := FMPVEngine.Volume - 5;
      tbVolume.Position := FMPVEngine.Volume;
    end;
  end
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saMute) then
    mnuAudioMuteClick(nil)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saAudioDelayPlus) then
    SetAudioDelay(FAudioDelay + 0.1)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saAudioDelayMinus) then
    SetAudioDelay(FAudioDelay - 0.1)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saAudioDelayReset) then
    SetAudioDelay(0)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saFullscreen) then
    SetFullscreen(not FFullscreen)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saRotate) then
    SetVideoRotation((FVideoRotation + 90) mod 360)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saDeinterlace) then
    ToggleDeinterlace
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saZoomIn) then
    SetVideoZoom(FVideoZoom + 0.1)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saZoomOut) then
    SetVideoZoom(FVideoZoom - 0.1)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saZoomReset) then
  begin
    SetVideoZoom(1.0);
    SetVideoPan(0, 0);
  end
  {$IFDEF WINDOWS}
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saAlwaysOnTop) then
    ToggleAlwaysOnTop
  {$ENDIF}
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saFitToVideo) then
    FitWindowToVideo
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saScreenshot) then
    mnuVideoScreenshotClick(nil)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saVideoAdjust) then
    mnuVideoAdjustDialogClick(nil)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saSubtitleDelayPlus) then
    SetSubtitleDelay(FSubtitleDelay + 0.1)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saSubtitleDelayMinus) then
    SetSubtitleDelay(FSubtitleDelay - 0.1)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saSubtitleDelayReset) then
    SetSubtitleDelay(0)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saDVDMenu) then
  begin
    if (FMPVEngine <> nil) and FMPVEngine.IsDVD then
      FMPVEngine.DVDGoMenu;
  end
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saDVDUp) then
  begin
    if (FMPVEngine <> nil) and FMPVEngine.IsDVD then
      FMPVEngine.DVDMenuUp;
  end
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saDVDDown) then
  begin
    if (FMPVEngine <> nil) and FMPVEngine.IsDVD then
      FMPVEngine.DVDMenuDown;
  end
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saDVDLeft) then
  begin
    if (FMPVEngine <> nil) and FMPVEngine.IsDVD then
      FMPVEngine.DVDMenuLeft;
  end
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saDVDRight) then
  begin
    if (FMPVEngine <> nil) and FMPVEngine.IsDVD then
      FMPVEngine.DVDMenuRight;
  end
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saDVDSelect) then
  begin
    if (FMPVEngine <> nil) and FMPVEngine.IsDVD then
      FMPVEngine.DVDMenuSelect;
  end
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saOpenFile) then
    mnuFileOpenClick(nil)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saOpenURL) then
    mnuFileOpenURLClick(nil)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saPlaylist) then
    mnuToolsPlaylistClick(nil)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saVisNextMode) then
    mnuVisNextModeClick(nil)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saVisNextColor) then
    mnuVisNextColorClick(nil)
  else if ShortcutManager.MatchesShortcut(Key, ModShift, saQuit) then
    Close;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  FolderPath: string;
  FileList: TStringList;
  I: Integer;
  FileArray: array of string;
begin
  if Length(FileNames) = 0 then Exit;

  { Check if a single folder was dropped }
  if (Length(FileNames) = 1) and DirectoryExists(FileNames[0]) then
  begin
    FolderPath := FileNames[0];
    FileList := TStringList.Create;
    try
      ScanFolderForMedia(FolderPath, FileList, True);
      FileList.Sort;
      if FileList.Count > 0 then
      begin
        SetLength(FileArray, FileList.Count);
        for I := 0 to FileList.Count - 1 do
          FileArray[I] := FileList[I];
        AddFilesToPlaylist(FileArray, True);
        StatusBar.SimpleText := Format(Locale.Message('FolderOpened', 'Opened folder: %d files'), [FileList.Count]);
      end
      else
        StatusBar.SimpleText := Locale.Message('NoMediaFound', 'No media files found');
    finally
      FileList.Free;
    end;
  end
  else if Length(FileNames) = 1 then
  begin
    { Single file - just play it }
    PlayFile(FileNames[0]);
  end
  else
  begin
    { Multiple files - add all to playlist }
    AddFilesToPlaylist(FileNames, True);
    StatusBar.SimpleText := Format(Locale.Message('FilesAdded', 'Added %d files to playlist'), [Length(FileNames)]);
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  MPV INITIALIZATION
  ═══════════════════════════════════════════════════════════════════════════════ }

{ ───────────────────────────────────────────────────────────────────────────
  InitializeMPV - Create and configure the MPV engine

  Purpose: Creates the TMPVEngine instance, connects event handlers, and
           applies all user settings from the configuration.

  Initialization sequence:
    1. Create TMPVEngine instance
    2. Connect event handlers (OnLog, OnStatusChange, OnPositionChange, etc.)
    3. Set init-time options (video/audio output, HW acceleration)
    4. Call Initialize() with video panel handle
    5. Apply runtime settings (volume, video adjustments, cache, etc.)
    6. Configure subtitles if custom settings enabled

  Error handling:
    - Shows error message if libmpv fails to initialize
    - Displays FMPVEngine.LastError for diagnostics

  Notes:
    - VideoOutput/AudioOutput must be set BEFORE Initialize()
    - Volume and video properties require valid FHandle (after Initialize)
    - Screenshot directory is created if it doesn't exist
  ─────────────────────────────────────────────────────────────────────────── }
procedure TfrmMain.InitializeMPV;
begin
  FMPVEngine := TMPVEngine.Create;

  { Set event handlers }
  FMPVEngine.OnLog := @OnMPVLog;
  FMPVEngine.OnStatusChange := @OnMPVStatusChange;
  FMPVEngine.OnPositionChange := @OnMPVPositionChange;
  FMPVEngine.OnEndFile := @OnMPVEndFile;
  FMPVEngine.OnFileLoaded := @OnMPVFileLoaded;
  FMPVEngine.OnVideoResize := @OnMPVVideoResize;
  FMPVEngine.OnMetadata := @OnMPVMetadata;

  { Set video/audio output BEFORE Initialize - these are init-time options }
  FMPVEngine.VideoOutput := Config.Settings.Video.VideoOutput;
  FMPVEngine.AudioOutput := Config.Settings.Audio.AudioOutput;
  FMPVEngine.HWAccel := Config.Settings.Video.HWAccel;

  { Initialize with video panel handle and dimensions }
  if not FMPVEngine.Initialize(pnlVideo.Handle, pnlVideo.Width, pnlVideo.Height) then
  begin
    StatusBar.SimpleText := Locale.Status('Error', 'Error');
    ShowMessage(Locale.Message('MPVInitFailed', 'Failed to initialize libmpv.') + LineEnding +
                Locale.Message('LastError', 'Last error:') + ' ' + FMPVEngine.LastError + LineEnding +
                Locale.Message('EnsureMPV', 'Please ensure libmpv is installed.'));
  end
  else
  begin
    StatusBar.SimpleText := Locale.Status('Ready', 'Ready');

    { Apply settings from config - must be after Initialize when FHandle is valid }
    FMPVEngine.Volume := Config.Settings.Audio.Volume;
    FMPVEngine.Muted := Config.Settings.Audio.Muted;

    { Override with CLI startup options if set }
    if FStartupOptionsSet then
    begin
      if FStartupOptions.VolumeSet then
      begin
        FMPVEngine.Volume := FStartupOptions.Volume;
        tbVolume.Position := FStartupOptions.Volume;
      end;
      if FStartupOptions.MuteSet then
        FMPVEngine.Muted := FStartupOptions.Mute;
      { Update volume display to reflect CLI options }
      UpdateVolumeDisplay;
      { Apply loop option }
      if FStartupOptions.LoopSet and FStartupOptions.Loop then
        FMPVEngine.SetPropertyString('loop-file', 'inf');
      { Apply speed option (use SetPlaybackSpeed to update menu too) }
      if FStartupOptions.SpeedSet then
        SetPlaybackSpeed(FStartupOptions.Speed);
    end;

    FMPVEngine.Brightness := Config.Settings.Video.Brightness;
    FMPVEngine.Contrast := Config.Settings.Video.Contrast;
    FMPVEngine.Saturation := Config.Settings.Video.Saturation;
    FMPVEngine.Hue := Config.Settings.Video.Hue;
    FMPVEngine.Gamma := Config.Settings.Video.Gamma;

    { Configure screenshot settings }
    if not DirectoryExists(Config.Settings.General.ScreenshotPath) then
      ForceDirectories(Config.Settings.General.ScreenshotPath);
    FMPVEngine.SetScreenshotDirectory(Config.Settings.General.ScreenshotPath);
    FMPVEngine.SetScreenshotFormat(Config.Settings.General.ScreenshotFormat);

    { Configure cache settings }
    FMPVEngine.SetCacheSize(Config.Settings.Cache.DefaultSize);

    { Configure audio settings }
    FMPVEngine.SetAudioOutput(Config.Settings.Audio.AudioOutput);
    FMPVEngine.SetAudioDevice(Config.Settings.Audio.AudioDevice);
    FMPVEngine.SetAudioNormalize(Config.Settings.Audio.Normalize);
    FMPVEngine.SetAudioChannels(Config.Settings.Audio.Channels);

    { Configure subtitle settings only if not using defaults }
    if not Config.Settings.Subtitles.UseDefault then
    begin
      FMPVEngine.SetSubFont(Config.Settings.Subtitles.FontName);
      FMPVEngine.SetSubFontSize(Config.Settings.Subtitles.FontSize);
      FMPVEngine.SetSubFontColor(Config.Settings.Subtitles.FontColor);
      FMPVEngine.SetSubBold(Config.Settings.Subtitles.FontBold);
      FMPVEngine.SetSubItalic(Config.Settings.Subtitles.FontItalic);
      FMPVEngine.SetSubOutlineColor(Config.Settings.Subtitles.OutlineColor);
      FMPVEngine.SetSubOutlineSize(Config.Settings.Subtitles.OutlineSize);
      FMPVEngine.SetSubPosition(Config.Settings.Subtitles.Position);
      FMPVEngine.SetSubEncoding(Config.Settings.Subtitles.Encoding);
      FMPVEngine.SetSubAutoLoad(Config.Settings.Subtitles.AutoLoad);
    end;
  end;
end;

{ ───────────────────────────────────────────────────────────────────────────
  ShutdownMPV - Clean shutdown of the MPV engine

  Purpose: Saves current audio state and cleanly shuts down the MPV engine.

  Shutdown sequence:
    1. Save current volume and mute state to config
    2. Mark config as modified for persistence
    3. Call FMPVEngine.Shutdown() to stop playback and cleanup
    4. Free the engine instance

  Notes:
    - Safe to call even if FMPVEngine is nil
    - Called during application close and when reinitializing
  ─────────────────────────────────────────────────────────────────────────── }
procedure TfrmMain.ShutdownMPV;
var
  TmpSettings: TAppSettings;
begin
  if FMPVEngine <> nil then
  begin
    { Save current settings }
    TmpSettings := Config.Settings;
    TmpSettings.Audio.Volume := FMPVEngine.Volume;
    TmpSettings.Audio.Muted := FMPVEngine.Muted;
    Config.Settings := TmpSettings;
    Config.Modified := True;

    FMPVEngine.Shutdown;
    FreeAndNil(FMPVEngine);
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  MPV EVENT HANDLERS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.OnMPVLog(Sender: TObject; const Msg: string);
begin
  { Log to the log window }
  if Assigned(frmLog) then
    frmLog.AddLine('[MPV] ' + Msg);
end;

procedure TfrmMain.OnMPVStatusChange(Sender: TObject; OldStatus, NewStatus: TMPVStatus);
begin
  case NewStatus of
    msNone:
      StatusBar.SimpleText := Locale.Status('Ready', 'Ready');
    msOpening:
      StatusBar.SimpleText := Locale.Status('Opening', 'Opening...');
    msPlaying:
      begin
        StatusBar.SimpleText := Locale.Status('Playing', 'Playing');
        { Update visualization menu when playback starts (tracks are now available) }
        UpdateVisualizationEnabled;
      end;
    msPaused:
      StatusBar.SimpleText := Locale.Status('Paused', 'Paused');
    msStopped:
      begin
        StatusBar.SimpleText := Locale.Status('Stopped', 'Stopped');
        UpdateVisualizationEnabled;
      end;
    msError:
      StatusBar.SimpleText := Locale.Status('Error', 'Error');
  end;

  UpdatePlayPauseButton;
end;

{ ───────────────────────────────────────────────────────────────────────────
  OnMPVPositionChange - Handle playback position updates

  Purpose: Updates seek bar and time display, and implements a watchdog
           mechanism for visualization modes where MPV may not send EOF events.

  Parameters:
    - PositionSec: Current position in seconds
    - PositionPercent: Current position as percentage (0-100)

  Watchdog mechanism (for visualizations):
    When lavfi-complex filters are active (visualizations), MPV sometimes
    doesn't send proper EOF events. This watchdog detects when position
    reaches within 0.3 seconds of the end and triggers playlist advance.

  Watchdog conditions (ALL must be true):
    - FVisualEffects is enabled and Mode != vmNone
    - FWatchdogTriggered is False (prevents multiple triggers)
    - FChangingVisualization is False (not mid-transition)
    - Position >= Duration - 0.3 seconds

  Notes:
    - Seek bar not updated while user is actively scrolling
    - Uses QueueAsyncCall to avoid calling PlayNext from callback context
  ─────────────────────────────────────────────────────────────────────────── }
procedure TfrmMain.OnMPVPositionChange(Sender: TObject; PositionSec, PositionPercent: Double);
var
  Duration: Double;
begin
  { Don't update while user is scrolling }
  if (FSeekBar <> nil) and not FSeekBar.Scrolling then
  begin
    FSeekBar.Position := Round(PositionPercent * 10);
    UpdateTimeDisplay;
  end;

  { Watchdog for all visualization modes - MPV may not send EOF events with lavfi-complex filters }
  { When any visualization is active, we must manually detect end of track }
  if (FVisualEffects <> nil) and FVisualEffects.Enabled and
     (FVisualEffects.Mode <> vmNone) and (FMPVEngine <> nil) and
     not FWatchdogTriggered and not FChangingVisualization then
  begin
    Duration := FMPVEngine.Duration;
    { Trigger when position reaches within 0.3 seconds of end }
    if (Duration > 0) and (PositionSec >= Duration - 0.3) then
    begin
      FWatchdogTriggered := True;  { Prevent multiple triggers }
      {$IFDEF DEBUG}
      WriteLn('[DEBUG] Watchdog triggered: PositionSec=', PositionSec:0:2, ' Duration=', Duration:0:2,
              ' Mode=', Ord(FVisualEffects.Mode));
      {$ENDIF}
      { Post message to play next to avoid calling from position callback }
      Application.QueueAsyncCall(@AsyncPlayNextInPlaylist, 0);
    end;
  end;
end;

{ ───────────────────────────────────────────────────────────────────────────
  OnMPVEndFile - Handle end-of-file and stop events from MPV

  Purpose: Processes file end events, filtering out false positives from
           visualization filter changes, and advancing the playlist.

  Parameters:
    - Reason: MPV_END_FILE_REASON_* constant (EOF, STOP, ERROR, etc.)
    - ErrorCode: Error code if Reason is ERROR

  Ignore conditions (checked in order):
    1. FIgnoreNextEndFile: Set after PlayFile to ignore stale events
    2. FChangingVisualization: Filter change in progress
    3. FWatchdogTriggered AND VisEnabled: Watchdog already handling transition

  False EOF detection:
    When lavfi-complex filters change, MPV may send EOF/STOP with position
    far from the actual end. This is detected by checking if position is
    within 2 seconds or 98% of duration ("near end" heuristic).

  Reason handling:
    - MPV_END_FILE_REASON_EOF: Natural end -> PlayNextInPlaylist
    - MPV_END_FILE_REASON_STOP: Manual stop OR filter change (check NearEnd)
    - MPV_END_FILE_REASON_ERROR: Playback error -> skip to next

  Notes:
    - Debug output helps trace EOF handling issues with visualizations
    - NearEnd defaults to True if duration/position unavailable
  ─────────────────────────────────────────────────────────────────────────── }
procedure TfrmMain.OnMPVEndFile(Sender: TObject; Reason, ErrorCode: Integer);
var
  FileDuration, CurrentPos: Double;
  NearEnd: Boolean;
  VisEnabled: Boolean;
begin
  VisEnabled := (FVisualEffects <> nil) and FVisualEffects.Enabled and (FVisualEffects.Mode <> vmNone);
  {$IFDEF DEBUG}
  WriteLn('[DEBUG] OnMPVEndFile: Reason=', Reason, ' VisEnabled=', VisEnabled,
          ' IgnoreNext=', FIgnoreNextEndFile, ' Changing=', FChangingVisualization,
          ' WatchdogTriggered=', FWatchdogTriggered);
  {$ENDIF}

  { Ignore EndFile event that arrives immediately after PlayFile call }
  { This prevents cascade issue where old file's duration causes false NearEnd }
  if FIgnoreNextEndFile then
  begin
    {$IFDEF DEBUG}
    WriteLn('[DEBUG] Ignoring (FIgnoreNextEndFile)');
    {$ENDIF}
    FIgnoreNextEndFile := False;
    Exit;
  end;

  { Ignore EOF events caused by visualization filter changes }
  if FChangingVisualization then
  begin
    {$IFDEF DEBUG}
    WriteLn('[DEBUG] Ignoring (FChangingVisualization)');
    {$ENDIF}
    Exit;
  end;

  { Ignore EOF if watchdog already triggered - it will handle the transition }
  if FWatchdogTriggered and VisEnabled then
  begin
    {$IFDEF DEBUG}
    WriteLn('[DEBUG] Ignoring (FWatchdogTriggered - watchdog handles transition)');
    {$ENDIF}
    Exit;
  end;

  { Check if we're actually near the end of the file }
  { False EOF/STOP can occur when changing lavfi-complex filters }
  NearEnd := True;  { Default to true if we can't determine }
  if (Reason in [MPV_END_FILE_REASON_EOF, MPV_END_FILE_REASON_STOP]) and (FMPVEngine <> nil) then
  begin
    FileDuration := FMPVEngine.Duration;
    CurrentPos := FMPVEngine.Position;
    {$IFDEF DEBUG}
    WriteLn('[DEBUG] Duration=', FileDuration:0:2, ' Position=', CurrentPos:0:2);
    {$ENDIF}
    { Consider "near end" if within last 2 seconds or 98% of duration }
    NearEnd := (FileDuration <= 0) or (CurrentPos <= 0) or
               ((FileDuration - CurrentPos) < 2.0) or
               ((CurrentPos / FileDuration) > 0.98);
    {$IFDEF DEBUG}
    WriteLn('[DEBUG] NearEnd=', NearEnd);
    {$ENDIF}
    if (Reason = MPV_END_FILE_REASON_EOF) and not NearEnd then
    begin
      {$IFDEF DEBUG}
      WriteLn('[DEBUG] Ignoring false EOF (not near end)');
      {$ENDIF}
      Exit;  { False EOF from filter change, ignore it }
    end;
  end;
  {$IFDEF DEBUG}
  WriteLn('[DEBUG] Proceeding with EndFile handling');
  {$ENDIF}

  FSeekBar.Position := 0;
  UpdateTimeDisplay;
  UpdatePlayPauseButton;

  { Handle different end reasons }
  if FPlaylistManager <> nil then
  begin
    case Reason of
      MPV_END_FILE_REASON_EOF:
        PlayNextInPlaylist;

      MPV_END_FILE_REASON_STOP:
        begin
          { STOP can occur when lavfi-complex filter is active - treat as EOF if near end }
          if VisEnabled and NearEnd then
            PlayNextInPlaylist
          else
            StatusBar.SimpleText := Locale.Status('Stopped', 'Stopped');
        end;

      MPV_END_FILE_REASON_ERROR:
        begin
          if Assigned(frmLog) then
            frmLog.AddLine('[Playlist] Skipping unplayable file, trying next...');
          PlayNextInPlaylist;
        end;

      else
        StatusBar.SimpleText := Locale.Status('Stopped', 'Stopped');
    end;
  end
  else
    StatusBar.SimpleText := Locale.Status('Stopped', 'Stopped');
end;

{ ═══════════════════════════════════════════════════════════════════════════
  OnMPVFileLoaded - Handle successful file load event from MPV

  Purpose: Called when MPV successfully loads and starts playing a file.
           Performs all necessary UI updates and state management:
           1. Updates window caption and recent files list
           2. Refreshes all track menus (audio, subtitle, video, chapters)
           3. Resets state flags (FIgnoreNextEndFile, FWatchdogTriggered)
           4. Updates playlist item metadata from MPV if not already set by ffprobe
           5. Handles visualization restoration if it was disabled by watchdog
           6. Reapplies visualization filter for the new file

  Parameters:
    - Sender: The TMPVEngine instance
    - FileName: The path of the loaded file

  Notes:
    - Metadata fallback: Only queries MPV for metadata if ffprobe didn't extract
      it (detected by Duration <= 0)
    - Visualization restoration: If FRestoreVisAfterLoad is set (by watchdog),
      restores the saved visualization mode and exits early
    - Normal visualization reapply: Uses FVisualReapplyTimer to delay filter
      application until MPV finishes initialization
    - ProcessMessages call allows tracks to become available before menu update
  ═══════════════════════════════════════════════════════════════════════════ }
procedure TfrmMain.OnMPVFileLoaded(Sender: TObject; var FileName: string);
var
  Artist, Album, Title: string;
  Duration: Double;
begin
  Caption := APP_NAME + ' - ' + ExtractFileName(FileName);
  Config.AddRecentFile(FileName);
  UpdateTimeDisplay;
  UpdateRecentFilesMenu;
  { Delay track menu updates slightly to ensure tracks are available }
  Application.ProcessMessages;
  UpdateAudioTrackMenu;
  UpdateSubtitleTrackMenu;
  UpdateVideoTrackMenu;
  UpdateChaptersMenu;
  { Phase 12: Update DVD menu state }
  UpdateDVDMenuState;
  { Phase 26: Update visualization menu enabled state }
  UpdateVisualizationEnabled;

  { Apply CLI start time option (only once, for first file) }
  if FStartupOptionsSet and FStartupOptions.StartTimeSet and (not FStartTimeApplied) then
  begin
    FStartTimeApplied := True;
    if FStartupOptions.StartTime > 0 then
      FMPVEngine.SeekAbsolute(FStartupOptions.StartTime);
  end;

  { Apply CLI subtitle option (only once, for first file) }
  if FStartupOptionsSet and FStartupOptions.SubFileSet and (not FSubFileApplied) then
  begin
    FSubFileApplied := True;
    FMPVEngine.LoadSubtitle(FStartupOptions.SubFile);
    UpdateSubtitleTrackMenu;
  end;

  { Reset the ignore flag - file is now properly loaded }
  FIgnoreNextEndFile := False;
  { Reset seek position tracking for new file }
  FLastSeekPosition := -1;
  { Reset watchdog flag for new track }
  FWatchdogTriggered := False;

  { Update playlist item metadata only if not already set by ffprobe }
  if (FPlaylistManager <> nil) and (FPlaylistManager.CurrentIndex >= 0) and
     (FPlaylistManager.CurrentIndex < FPlaylistManager.Count) then
  begin
    { Only update if duration is not set (meaning ffprobe didn't extract metadata) }
    if FPlaylistManager.Items[FPlaylistManager.CurrentIndex].Duration <= 0 then
    begin
      Artist := FMPVEngine.GetPropertyString('metadata/by-key/artist');
      Album := FMPVEngine.GetPropertyString('metadata/by-key/album');
      Title := FMPVEngine.GetPropertyString('media-title');
      Duration := FMPVEngine.Duration;

      { Use filename as title if no media-title available }
      if Title = '' then
        Title := ExtractFileName(FileName);

      FPlaylistManager.UpdateItemInfo(FPlaylistManager.CurrentIndex, Title, Artist, Album, Duration);
    end;
  end;

  { Restore visualization if it was disabled by watchdog for showwaves EOF handling }
  if FRestoreVisAfterLoad and (FVisualEffects <> nil) then
  begin
    {$IFDEF DEBUG}
    WriteLn('[DEBUG] Restoring visualization mode: ', Ord(FRestoreVisMode));
    {$ENDIF}
    FRestoreVisAfterLoad := False;

    { Restore visualization settings }
    FVisualEffects.Mode := FRestoreVisMode;
    FVisualEffects.Enabled := True;

    { Ignore any EOF events that may be generated by applying the filter }
    FIgnoreNextEndFile := True;

    { Apply filter immediately }
    { FChangingVisualization is already True from AsyncPlayNextInPlaylist }
    ApplyVisualization;
    { Use timer to reset flag after MPV has processed the filter change }
    FVisualReapplyNeeded := False;  { Timer should just reset flag, not reapply }
    FVisualReapplyTimer.Enabled := False;
    FVisualReapplyTimer.Interval := 300;
    FVisualReapplyTimer.Enabled := True;
    Exit;  { Don't fall through to normal reapply logic }
  end;

  { Reapply visualization filter if enabled (filter needs to be set for each new file) }
  { Use a timer to delay the filter application until MPV finishes initialization }
  if (FVisualEffects <> nil) and FVisualEffects.Enabled and
     (FVisualEffects.Mode <> vmNone) and not FChangingVisualization then
  begin
    FVisualReapplyNeeded := True;  { Timer should apply visualization }
    FVisualReapplyTimer.Enabled := False; { Reset if already running }
    FVisualReapplyTimer.Enabled := True;  { Start the timer }
  end;
end;

procedure TfrmMain.OnMPVVideoResize(Sender: TObject; AWidth, AHeight: Integer);
begin
  StatusBar.Panels[1].Text := Format('%dx%d', [AWidth, AHeight]);
end;

procedure TfrmMain.OnMPVMetadata(Sender: TObject; const Key, Value: string);
begin
  if (Key = 'title') or (Key = 'icy-title') then
    Caption := APP_NAME + ' - ' + Value;
end;

{ Phase 25: Log command handler }
procedure TfrmMain.OnLogSendCommand(Sender: TObject);
var
  Cmd: string;
begin
  if (FMPVEngine = nil) or (frmLog = nil) then Exit;

  Cmd := frmLog.LastCommand;
  if Cmd = '' then Exit;

  { Send command to MPV }
  FMPVEngine.SendCmd(Cmd);

  { Log confirmation }
  frmLog.AddLine('[Command sent]');
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  MENU HANDLERS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.mnuFileOpenClick(Sender: TObject);
begin
  OpenDialog.Filter := DIALOG_FILTER_MEDIA;
  OpenDialog.Title := Locale.Dialog('OpenFile', 'Open File');

  if OpenDialog.Execute then
    PlayFile(OpenDialog.FileName);
end;

procedure TfrmMain.mnuFileOpenURLClick(Sender: TObject);
var
  URL: string;
begin
  URL := ShowOpenURLDialog;
  if URL <> '' then
    PlayFile(URL);
end;

procedure TfrmMain.mnuFileExitClick(Sender: TObject);
begin
  Close;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  mnuPlaybackPlayPauseClick - Handle play/pause toggle action

  Purpose: Implements a state machine for playback control based on current
           MPV status. Handles three distinct scenarios:
           1. Playing → Pause (toggle to paused state)
           2. Paused → Resume (toggle back to playing)
           3. Stopped/None → Start playback (replay last file or first item)

  Parameters:
    - Sender: The menu item or button that triggered the action

  Notes:
    - When stopped, prioritizes FCurrentFileName (last played file) over
      playlist, ensuring consistent behavior after explicit stop
    - If no current file and playlist exists, resets to first item
    - Always updates play/pause button icon after state change
  ═══════════════════════════════════════════════════════════════════════════ }
procedure TfrmMain.mnuPlaybackPlayPauseClick(Sender: TObject);
begin
  if FMPVEngine = nil then Exit;

  case FMPVEngine.Status of
    msPlaying:
      FMPVEngine.Pause;
    msPaused:
      FMPVEngine.Resume;
    msNone, msStopped:
      begin
        { After stop, replay the last file or first playlist item }
        if FCurrentFileName <> '' then
          PlayFile(FCurrentFileName)
        else if (FPlaylistManager <> nil) and (FPlaylistManager.Count > 0) then
        begin
          FPlaylistManager.CurrentIndex := 0;
          PlayFile(FPlaylistManager.Items[0].FileName);
        end;
      end;
  end;

  UpdatePlayPauseButton;
end;

procedure TfrmMain.mnuPlaybackStopClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
  begin
    FMPVEngine.Stop;
    { Disable visualization menu when stopped }
    UpdateVisualizationEnabled;
  end;
end;

procedure TfrmMain.mnuPlaybackSeekFwdClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
    FMPVEngine.SeekRelative(SEEK_MEDIUM);
end;

procedure TfrmMain.mnuPlaybackSeekBackClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
    FMPVEngine.SeekRelative(-SEEK_MEDIUM);
end;

procedure TfrmMain.mnuAudioVolUpClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
  begin
    FMPVEngine.Volume := FMPVEngine.Volume + 5;
    tbVolume.Position := FMPVEngine.Volume;
    UpdateVolumeDisplay;
  end;
end;

procedure TfrmMain.mnuAudioVolDownClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
  begin
    FMPVEngine.Volume := FMPVEngine.Volume - 5;
    tbVolume.Position := FMPVEngine.Volume;
    UpdateVolumeDisplay;
  end;
end;

procedure TfrmMain.mnuAudioMuteClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
  begin
    FMPVEngine.Muted := not FMPVEngine.Muted;
    UpdateVolumeDisplay;
  end;
end;

procedure TfrmMain.mnuVideoFullscreenClick(Sender: TObject);
begin
  SetFullscreen(not FFullscreen);
end;

procedure TfrmMain.UpdateAspectMenuChecks(Mode: Integer);
begin
  mnuVideoAspectAuto.Checked := (Mode = ASPECT_AUTO);
  mnuVideoAspect169.Checked := (Mode = ASPECT_16_9);
  mnuVideoAspect43.Checked := (Mode = ASPECT_4_3);
  mnuVideoAspect235.Checked := (Mode = ASPECT_2_35_1);
  { Sync context menu }
  mnuCtxAspectAuto.Checked := mnuVideoAspectAuto.Checked;
  mnuCtxAspect169.Checked := mnuVideoAspect169.Checked;
  mnuCtxAspect43.Checked := mnuVideoAspect43.Checked;
  mnuCtxAspect235.Checked := mnuVideoAspect235.Checked;
end;

procedure TfrmMain.mnuVideoAspectAutoClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
    FMPVEngine.AspectMode := ASPECT_AUTO;
  UpdateAspectMenuChecks(ASPECT_AUTO);
end;

procedure TfrmMain.mnuVideoAspect169Click(Sender: TObject);
begin
  if FMPVEngine <> nil then
    FMPVEngine.AspectMode := ASPECT_16_9;
  UpdateAspectMenuChecks(ASPECT_16_9);
end;

procedure TfrmMain.mnuVideoAspect43Click(Sender: TObject);
begin
  if FMPVEngine <> nil then
    FMPVEngine.AspectMode := ASPECT_4_3;
  UpdateAspectMenuChecks(ASPECT_4_3);
end;

procedure TfrmMain.mnuVideoAspect235Click(Sender: TObject);
begin
  if FMPVEngine <> nil then
    FMPVEngine.AspectMode := ASPECT_2_35_1;
  UpdateAspectMenuChecks(ASPECT_2_35_1);
end;

procedure TfrmMain.mnuVideoScreenshotClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
    FMPVEngine.Screenshot;
end;

procedure TfrmMain.mnuSubtitleLoadClick(Sender: TObject);
begin
  OpenDialog.Filter := DIALOG_FILTER_SUBTITLE;
  OpenDialog.Title := Locale.Dialog('LoadSubtitle', 'Load Subtitle File');

  if OpenDialog.Execute then
    if FMPVEngine <> nil then
    begin
      FMPVEngine.LoadSubtitle(OpenDialog.FileName);
      { Refresh subtitle track menu to show newly loaded subtitle }
      UpdateSubtitleTrackMenu;
    end;
end;

procedure TfrmMain.mnuToolsPlaylistClick(Sender: TObject);
begin
  if FPlaylistForm <> nil then
  begin
    FPlaylistForm.Show;
    FPlaylistForm.BringToFront;
  end;
end;

procedure TfrmMain.mnuToolsRadiosClick(Sender: TObject);
begin
  if FRadiosForm <> nil then
  begin
    FRadiosForm.Show;
    FRadiosForm.BringToFront;
  end;
end;

procedure TfrmMain.mnuAudioEqualizerClick(Sender: TObject);
begin
  if FEqualizerForm <> nil then
  begin
    FEqualizerForm.Show;
    FEqualizerForm.BringToFront;
  end;
end;

procedure TfrmMain.mnuToolsOptionsClick(Sender: TObject);
begin
  if FOptionsForm <> nil then
  begin
    FOptionsForm.ShowModal;
    { Reload settings that might have changed }
    if FMPVEngine <> nil then
    begin
      FMPVEngine.Brightness := Config.Settings.Video.Brightness;
      FMPVEngine.Contrast := Config.Settings.Video.Contrast;
      FMPVEngine.Saturation := Config.Settings.Video.Saturation;
      FMPVEngine.Hue := Config.Settings.Video.Hue;
      FMPVEngine.Gamma := Config.Settings.Video.Gamma;
    end;
  end;
end;

procedure TfrmMain.mnuHelpMediaInfoClick(Sender: TObject);
begin
  if (FMediaInfoForm <> nil) and (FCurrentFileName <> '') then
  begin
    FMediaInfoForm.LoadMediaInfo(FMPVEngine, FCurrentFileName);
    FMediaInfoForm.ShowModal;
  end
  else
    ShowMessage(Locale.Message('NoMediaLoaded', 'No media file is currently loaded.'));
end;

{ Phase 25: Log window }
procedure TfrmMain.mnuHelpLogClick(Sender: TObject);
begin
  if frmLog = nil then
  begin
    Application.CreateForm(TfrmLog, frmLog);
    frmLog.OnSendCommand := @OnLogSendCommand;
  end;
  frmLog.Show;
end;

procedure TfrmMain.mnuHelpAboutClick(Sender: TObject);
begin
  if FAboutForm <> nil then
  begin
    FAboutForm.ShowModal;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  BUTTON HANDLERS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.btnPlayClick(Sender: TObject);
begin
  mnuPlaybackPlayPauseClick(nil);
end;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  mnuPlaybackStopClick(nil);
end;

procedure TfrmMain.btnPreviousClick(Sender: TObject);
begin
  PlayPreviousInPlaylist;
end;

procedure TfrmMain.btnNextClick(Sender: TObject);
begin
  PlayNextInPlaylist;
end;

procedure TfrmMain.btnMuteClick(Sender: TObject);
begin
  mnuAudioMuteClick(nil);
end;

procedure TfrmMain.btnFullscreenClick(Sender: TObject);
begin
  mnuVideoFullscreenClick(nil);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TRACKBAR HANDLERS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.SeekBarChange(Sender: TObject);
var
  HasVisualization: Boolean;
  NewPos: Integer;
begin
  { During scrolling, seek immediately for live feedback }
  if FSeekBar.Scrolling and (FMPVEngine <> nil) then
  begin
    NewPos := FSeekBar.Position;

    { Prevent double-seek: only seek if position changed significantly (> 5 out of 1000) }
    { This prevents the MouseDown+MouseUp double-seek on simple clicks }
    if Abs(NewPos - FLastSeekPosition) <= 5 then
    begin
      UpdateTimeDisplay;
      Exit;
    end;

    HasVisualization := (FVisualEffects <> nil) and FVisualEffects.Enabled and
                        (FVisualEffects.Mode <> vmNone);

    { When visualization is active, don't seek during drag to preserve audio/visual }
    { Seek will be done in SeekBarScrollEnd }
    if HasVisualization then
    begin
      { Just update time display during drag, seek will happen at end }
      UpdateTimeDisplay;
    end
    else
    begin
      { No visualization: seek immediately for live feedback }
      FLastSeekPosition := NewPos;
      FMPVEngine.SeekPercent(NewPos / 10.0);
      UpdateTimeDisplay;
    end;
  end;
end;

procedure TfrmMain.SeekBarScrollEnd(Sender: TObject);
var
  HasVisualization: Boolean;
begin
  { Final seek when scrolling ends }
  if FMPVEngine <> nil then
  begin
    HasVisualization := (FVisualEffects <> nil) and FVisualEffects.Enabled and
                        (FVisualEffects.Mode <> vmNone);

    if HasVisualization then
    begin
      { Clear filter, seek, then reapply visualization }
      FChangingVisualization := True;
      FIgnoreNextEndFile := True;
      FMPVEngine.SetPropertyString('lavfi-complex', '');
      FMPVEngine.SeekPercent(FSeekBar.Position / 10.0);
      { Use timer to let MPV stabilize after seek before reapplying filter }
      FVisualReapplyNeeded := True;  { Timer should apply visualization }
      FVisualReapplyTimer.Enabled := False;
      FVisualReapplyTimer.Enabled := True;
    end;
    { Note: No seek needed here when no visualization - SeekBarChange already }
    { handled the seek during MouseUp (while Scrolling was still true) }
  end;
end;

procedure TfrmMain.tbVolumeChange(Sender: TObject);
begin
  if FMPVEngine <> nil then
  begin
    FMPVEngine.Volume := tbVolume.Position;
    UpdateVolumeDisplay;
  end;
end;

procedure TfrmMain.tbVolumeMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if FMPVEngine = nil then Exit;

  { Wheel up = increase volume, wheel down = decrease volume }
  if WheelDelta > 0 then
    tbVolume.Position := tbVolume.Position + 5
  else
    tbVolume.Position := tbVolume.Position - 5;

  Handled := True;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TIMER HANDLERS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.TimerPositionTimer(Sender: TObject);
begin
  UpdateTimeDisplay;
  { Check playlist docking state }
  CheckPlaylistDocking;
end;

procedure TfrmMain.TimerHideControlsTimer(Sender: TObject);
var
  CurrentPos: TPoint;
  ScreenHeight: Integer;
  BottomZone: Integer;
  MouseInBottomZone: Boolean;
begin
  if not FFullscreen then Exit;

  { Get current mouse position and screen dimensions }
  CurrentPos := Mouse.CursorPos;
  ScreenHeight := Screen.Height;
  BottomZone := 150; { Height of the bottom zone where controls appear }

  { Check if mouse is in bottom zone of screen }
  MouseInBottomZone := CurrentPos.Y >= (ScreenHeight - BottomZone);

  if MouseInBottomZone then
  begin
    { Mouse is in bottom zone - show controls }
    if not FControlsVisible then
      ShowControls;
  end
  else
  begin
    { Mouse left the bottom zone - hide controls immediately }
    if FControlsVisible then
      HideControls;
  end;
end;

procedure TfrmMain.TimerRestoreBoundsTimer(Sender: TObject);
var
  CurrentWidth, CurrentHeight: Integer;
  TargetWidth, TargetHeight: Integer;
  BoundsMatch: Boolean;
begin
  { Calculate current and target sizes }
  CurrentWidth := BoundsRect.Right - BoundsRect.Left;
  CurrentHeight := BoundsRect.Bottom - BoundsRect.Top;
  TargetWidth := FBoundsToRestore.Right - FBoundsToRestore.Left;
  TargetHeight := FBoundsToRestore.Bottom - FBoundsToRestore.Top;

  { Check if bounds already match (with small tolerance) }
  BoundsMatch := (Abs(CurrentWidth - TargetWidth) < 10) and
                 (Abs(CurrentHeight - TargetHeight) < 10) and
                 (Abs(BoundsRect.Left - FBoundsToRestore.Left) < 10) and
                 (Abs(BoundsRect.Top - FBoundsToRestore.Top) < 10);

  if BoundsMatch then
  begin
    { Bounds are correct, stop timer and restore minimum constraints }
    TimerRestoreBounds.Enabled := False;
    FExitingFullscreen := False;
    Constraints.MinWidth := DEF_MAIN_MIN_WIDTH;
    Constraints.MinHeight := DEF_MAIN_MIN_HEIGHT;
    Constraints.MaxWidth := 0;
    Constraints.MaxHeight := 0;

    { Restore playlist position after fullscreen exit }
    if (FPlaylistForm <> nil) and FPlaylistForm.Visible then
    begin
      FUpdatingDockedWindows := True;
      try
        FPlaylistForm.BoundsRect := FSavedPlaylistBounds;
        FPlaylistDockSide := FSavedPlaylistDockSide;
      finally
        FUpdatingDockedWindows := False;
      end;
    end;
  end
  else
  begin
    Dec(FRestoreBoundsRetries);

    { Try to restore bounds }
    if WindowState <> wsNormal then
    begin
      WindowState := wsNormal;
      Exit;
    end;

    { Temporarily clear constraints for bounds restoration }
    Constraints.MinWidth := 0;
    Constraints.MinHeight := 0;
    Constraints.MaxWidth := 0;
    Constraints.MaxHeight := 0;

    { Set bounds directly }
    BoundsRect := FBoundsToRestore;

    { Restore minimum constraints }
    Constraints.MinWidth := DEF_MAIN_MIN_WIDTH;
    Constraints.MinHeight := DEF_MAIN_MIN_HEIGHT;

    if FRestoreBoundsRetries <= 0 then
    begin
      { Out of retries, give up }
      TimerRestoreBounds.Enabled := False;
      FExitingFullscreen := False;
    end;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  VIDEO PANEL HANDLERS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.pnlVideoClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
    FMPVEngine.TogglePause;
  { Ensure form keeps keyboard focus after clicking on video }
  Self.SetFocus;
end;

procedure TfrmMain.pnlVideoDblClick(Sender: TObject);
begin
  SetFullscreen(not FFullscreen);
  { Ensure form keeps keyboard focus }
  Self.SetFocus;
end;

procedure TfrmMain.pnlVideoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  { Mouse movement is now handled by TimerHideControlsTimer for fullscreen }
  { Only update last mouse move time for non-fullscreen mode }
  if not FFullscreen then
    FLastMouseMove := Now;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  HELPER METHODS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.PlayFile(const FileName: string);
var
  ItemIndex: Integer;
  CacheSize: Integer;
begin
  if FMPVEngine <> nil then
  begin
    { Add to playlist if not already present }
    if FPlaylistManager <> nil then
    begin
      ItemIndex := FPlaylistManager.IndexOf(FileName);
      if ItemIndex < 0 then
      begin
        { Add new item and set as current }
        FPlaylistManager.Add(FileName);
        FPlaylistManager.CurrentIndex := FPlaylistManager.Count - 1;
      end
      else
      begin
        { Already in playlist, just set as current }
        FPlaylistManager.CurrentIndex := ItemIndex;
      end;
      { Refresh playlist window if visible }
      if (FPlaylistForm <> nil) and FPlaylistForm.Visible then
        FPlaylistForm.RefreshList;
    end;

    { Apply appropriate cache size for this media type }
    CacheSize := GetCacheSizeForMedia(FileName);
    FMPVEngine.SetCacheSize(CacheSize);

    { Handle visualization filter before loading new file }
    { For all visualization modes, clear the filter before loading new file }
    if (FVisualEffects <> nil) and FVisualEffects.Enabled and (FVisualEffects.Mode <> vmNone) then
    begin
      { Stop playback and clear the filter }
      FMPVEngine.Stop;
      Application.ProcessMessages;
      FMPVEngine.SetPropertyString('lavfi-complex', '');
      Application.ProcessMessages;
      FPendingPlayFile := FileName;
      FIgnoreNextEndFile := True;
      FVisualLoadTimer.Enabled := False;
      FVisualLoadTimer.Enabled := True;
      Exit;
    end;

    FCurrentFileName := FileName;
    FIgnoreNextEndFile := True;  { Ignore the immediate EndFile from previous file/filter }
    FMPVEngine.PlayMedia(FileName);
  end;
end;

procedure TfrmMain.UpdateTimeDisplay;
var
  PosStr, DurStr: string;
begin
  if (FMPVEngine <> nil) and (FMPVEngine.Status in [msPlaying, msPaused]) then
  begin
    PosStr := FormatTime(FMPVEngine.Position);
    DurStr := FormatTime(FMPVEngine.Duration);
    lblTime.Caption := PosStr;
    lblDuration.Caption := DurStr;
  end
  else
  begin
    lblTime.Caption := '00:00:00';
    lblDuration.Caption := '00:00:00';
  end;
end;

procedure TfrmMain.UpdateVolumeDisplay;
begin
  if FMPVEngine <> nil then
  begin
    if FMPVEngine.Muted then
    begin
      lblVolume.Caption := Locale.Label_('Muted', 'Muted');
      btnMute.Caption := '🔇';
    end
    else
    begin
      lblVolume.Caption := IntToStr(FMPVEngine.Volume) + '%';
      btnMute.Caption := '🔊';
    end;

    tbVolume.Position := FMPVEngine.Volume;
  end;
end;

procedure TfrmMain.UpdatePlayPauseButton;
begin
  if (FMPVEngine <> nil) and (FMPVEngine.Status = msPlaying) then
    btnPlay.Caption := '||'
  else
    btnPlay.Caption := '>';
end;

procedure TfrmMain.SetFullscreen(AFullscreen: Boolean);
begin
  if AFullscreen = FFullscreen then Exit;

  FFullscreen := AFullscreen;

  if FFullscreen then
  begin
    { Save current state - FSavedBounds is already updated by FormResize }
    FSavedWindowState := WindowState;

    { Save playlist position before fullscreen }
    if (FPlaylistForm <> nil) and FPlaylistForm.Visible then
    begin
      FSavedPlaylistBounds := FPlaylistForm.BoundsRect;
      FSavedPlaylistDockSide := FPlaylistDockSide;
    end;

    { Hide controls and status bar }
    pnlControls.Visible := False;
    StatusBar.Visible := False;
    FControlsVisible := False;

    { Hide cursor }
    pnlVideo.Cursor := crNone;

    { Hide menu }
    Menu := nil;

    { Use wsFullScreen for true fullscreen }
    WindowState := wsFullScreen;

    { Initialize mouse tracking for auto-hide }
    FLastMousePos := Mouse.CursorPos;
    FLastMouseMove := Now;
  end
  else
  begin
    { Set flag to prevent FormResize from overwriting saved bounds }
    FExitingFullscreen := True;

    { Save bounds and start timer to restore after WM finishes transition }
    FBoundsToRestore := FSavedBounds;
    FRestoreBoundsRetries := 15;

    { Exit fullscreen }
    WindowState := wsNormal;

    { Start timer to restore bounds using GTK functions }
    TimerRestoreBounds.Interval := 100;
    TimerRestoreBounds.Enabled := True;

    { Show menu }
    Menu := MainMenu;

    { Show controls - use DisableAlign to prevent z-order issues }
    DisableAlign;
    try
      StatusBar.Visible := True;
      pnlControls.Visible := True;
      FControlsVisible := True;
    finally
      EnableAlign;
    end;

    { Restore cursor }
    pnlVideo.Cursor := crDefault;
  end;
end;

procedure TfrmMain.ShowControls;
begin
  if not FControlsVisible then
  begin
    pnlControls.Visible := True;
    { In fullscreen, only show pnlControls (with pnlSeek/pnlButtons), not StatusBar }
    if not FFullscreen then
      StatusBar.Visible := True;
    FControlsVisible := True;
    { Restore cursor }
    pnlVideo.Cursor := crDefault;
    { In fullscreen, resize video window to make room for controls }
    if FFullscreen and (FMPVEngine <> nil) then
      FMPVEngine.ResizeVideoWindow(Screen.Width, Screen.Height - pnlControls.Height);
  end;
end;

procedure TfrmMain.HideControls;
begin
  if FControlsVisible and FFullscreen then
  begin
    pnlControls.Visible := False;
    StatusBar.Visible := False;
    FControlsVisible := False;
    { Hide cursor in fullscreen }
    pnlVideo.Cursor := crNone;
    { In fullscreen, resize video window back to full screen }
    if FMPVEngine <> nil then
      FMPVEngine.ResizeVideoWindow(Screen.Width, Screen.Height);
  end;
end;

function TfrmMain.FormatTime(Seconds: Double): string;
var
  H, M, S: Integer;
begin
  if (Seconds < 0) or IsNaN(Seconds) or IsInfinite(Seconds) then
    Seconds := 0;

  H := Trunc(Seconds) div 3600;
  M := (Trunc(Seconds) mod 3600) div 60;
  S := Trunc(Seconds) mod 60;

  Result := Format('%.2d:%.2d:%.2d', [H, M, S]);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PLAYLIST HANDLERS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.OnPlaylistPlay(Sender: TObject; Index: Integer; const FileName: string);
begin
  if FileName <> '' then
  begin
    if FPlaylistManager <> nil then
      FPlaylistManager.CurrentIndex := Index;
    PlayFile(FileName);
  end;
end;

procedure TfrmMain.PlayNextInPlaylist;
var
  NextIdx: Integer;
  Item: TPlaylistItem;
begin
  if FPlaylistManager = nil then Exit;
  if FPlaylistManager.Count = 0 then Exit;

  NextIdx := FPlaylistManager.GetNext;
  if NextIdx >= 0 then
  begin
    FPlaylistManager.CurrentIndex := NextIdx;
    Item := FPlaylistManager.Items[NextIdx];
    if Item.FileName <> '' then
      PlayFile(Item.FileName);
  end
  else
    StatusBar.SimpleText := Locale.Message('EndOfPlaylist', 'End of playlist');
end;

procedure TfrmMain.PlayPreviousInPlaylist;
var
  PrevIdx: Integer;
  Item: TPlaylistItem;
begin
  if FPlaylistManager = nil then Exit;
  if FPlaylistManager.Count = 0 then Exit;

  PrevIdx := FPlaylistManager.GetPrevious;
  if PrevIdx >= 0 then
  begin
    FPlaylistManager.CurrentIndex := PrevIdx;
    Item := FPlaylistManager.Items[PrevIdx];
    if Item.FileName <> '' then
      PlayFile(Item.FileName);
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  RADIO HANDLERS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.OnRadioPlay(Sender: TObject; const Station: TRadioStation);
begin
  if Station.URL <> '' then
  begin
    Caption := APP_NAME + ' - ' + Station.Name;
    StatusBar.SimpleText := Format(Locale.Message('PlayingRadio', 'Playing radio: %s'), [Station.Name]);
    PlayFile(Station.URL);
  end;
end;

{ ===============================================================================
  RECENT FILES AND TRACK MENUS
  =============================================================================== }

procedure TfrmMain.UpdateRecentFilesMenu;
var
  RecentFiles: TStringList;
  I: Integer;
  MenuItem: TMenuItem;
begin
  { Clear existing recent file items }
  mnuFileRecent.Clear;

  { Get recent files from config }
  RecentFiles := Config.GetRecentFiles;
  try
    if RecentFiles.Count = 0 then
    begin
      MenuItem := TMenuItem.Create(mnuFileRecent);
      MenuItem.Caption := Locale.Dialog('NoRecentFiles', '(No recent files)');
      MenuItem.Enabled := False;
      mnuFileRecent.Add(MenuItem);
    end
    else
    begin
      for I := 0 to RecentFiles.Count - 1 do
      begin
        MenuItem := TMenuItem.Create(mnuFileRecent);
        MenuItem.Caption := Format('&%d. %s', [I + 1, ExtractFileName(RecentFiles[I])]);
        MenuItem.Hint := RecentFiles[I];
        MenuItem.Tag := I;
        MenuItem.OnClick := @OnRecentFileClick;
        mnuFileRecent.Add(MenuItem);
      end;

      { Add separator and clear option }
      MenuItem := TMenuItem.Create(mnuFileRecent);
      MenuItem.Caption := '-';
      mnuFileRecent.Add(MenuItem);

      MenuItem := TMenuItem.Create(mnuFileRecent);
      MenuItem.Caption := _T('Menu', 'ClearRecentFiles', 'Clear Recent Files');
      MenuItem.Tag := -1;
      MenuItem.OnClick := @OnRecentFileClick;
      mnuFileRecent.Add(MenuItem);
    end;
  finally
    RecentFiles.Free;
  end;
end;

procedure TfrmMain.OnRecentFileClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  RecentFiles: TStringList;
begin
  MenuItem := Sender as TMenuItem;

  if MenuItem.Tag = -1 then
  begin
    { Clear recent files }
    Config.ClearRecentFiles;
    UpdateRecentFilesMenu;
  end
  else
  begin
    { Play the selected file }
    RecentFiles := Config.GetRecentFiles;
    try
      if (MenuItem.Tag >= 0) and (MenuItem.Tag < RecentFiles.Count) then
        PlayFile(RecentFiles[MenuItem.Tag]);
    finally
      RecentFiles.Free;
    end;
  end;
end;

procedure TfrmMain.UpdateAudioTrackMenu;
var
  TrackCount, I, CurrentTrack: Integer;
  MenuItem: TMenuItem;
  TrackTitle, TrackLang: string;
begin
  mnuAudioTrack.Clear;

  if FMPVEngine = nil then Exit;

  TrackCount := FMPVEngine.GetPropertyInt('track-list/count');
  CurrentTrack := FMPVEngine.GetPropertyInt('aid');

  for I := 0 to TrackCount - 1 do
  begin
    if FMPVEngine.GetPropertyString('track-list/' + IntToStr(I) + '/type') = 'audio' then
    begin
      TrackTitle := FMPVEngine.GetPropertyString('track-list/' + IntToStr(I) + '/title');
      TrackLang := FMPVEngine.GetPropertyString('track-list/' + IntToStr(I) + '/lang');

      MenuItem := TMenuItem.Create(mnuAudioTrack);
      if TrackTitle <> '' then
        MenuItem.Caption := Format(Locale.Menu('TrackWithTitle', 'Track %d: %s'), [I + 1, TrackTitle])
      else if TrackLang <> '' then
        MenuItem.Caption := Format(Locale.Menu('TrackWithLang', 'Track %d (%s)'), [I + 1, TrackLang])
      else
        MenuItem.Caption := Format(Locale.Menu('TrackNumber', 'Track %d'), [I + 1]);

      MenuItem.Tag := FMPVEngine.GetPropertyInt('track-list/' + IntToStr(I) + '/id');
      MenuItem.RadioItem := True;
      MenuItem.Checked := (MenuItem.Tag = CurrentTrack);
      MenuItem.OnClick := @OnAudioTrackClick;
      mnuAudioTrack.Add(MenuItem);
    end;
  end;

  if mnuAudioTrack.Count = 0 then
  begin
    MenuItem := TMenuItem.Create(mnuAudioTrack);
    MenuItem.Caption := Locale.Dialog('NoAudioTracks', '(No audio tracks)');
    MenuItem.Enabled := False;
    mnuAudioTrack.Add(MenuItem);
  end;
end;

procedure TfrmMain.UpdateSubtitleTrackMenu;
var
  TrackCount, I, CurrentTrack: Integer;
  MenuItem: TMenuItem;
  TrackTitle, TrackLang: string;
begin
  mnuSubtitleTrack.Clear;

  { Add "Disable" option }
  MenuItem := TMenuItem.Create(mnuSubtitleTrack);
  MenuItem.Caption := Locale.Menu('SubtitleDisable', 'Disable');
  MenuItem.Tag := 0;
  MenuItem.RadioItem := True;
  MenuItem.OnClick := @OnSubtitleTrackClick;
  mnuSubtitleTrack.Add(MenuItem);

  if FMPVEngine = nil then Exit;

  TrackCount := FMPVEngine.GetPropertyInt('track-list/count');
  CurrentTrack := FMPVEngine.GetPropertyInt('sid');

  MenuItem.Checked := (CurrentTrack = 0);

  for I := 0 to TrackCount - 1 do
  begin
    if FMPVEngine.GetPropertyString('track-list/' + IntToStr(I) + '/type') = 'sub' then
    begin
      TrackTitle := FMPVEngine.GetPropertyString('track-list/' + IntToStr(I) + '/title');
      TrackLang := FMPVEngine.GetPropertyString('track-list/' + IntToStr(I) + '/lang');

      MenuItem := TMenuItem.Create(mnuSubtitleTrack);
      if TrackTitle <> '' then
        MenuItem.Caption := Format(Locale.Menu('TrackWithTitle', 'Track %d: %s'), [I + 1, TrackTitle])
      else if TrackLang <> '' then
        MenuItem.Caption := Format(Locale.Menu('TrackWithLang', 'Track %d (%s)'), [I + 1, TrackLang])
      else
        MenuItem.Caption := Format(Locale.Menu('TrackNumber', 'Track %d'), [I + 1]);

      MenuItem.Tag := FMPVEngine.GetPropertyInt('track-list/' + IntToStr(I) + '/id');
      MenuItem.RadioItem := True;
      MenuItem.Checked := (MenuItem.Tag = CurrentTrack);
      MenuItem.OnClick := @OnSubtitleTrackClick;
      mnuSubtitleTrack.Add(MenuItem);
    end;
  end;
end;

procedure TfrmMain.OnAudioTrackClick(Sender: TObject);
var
  MenuItem: TMenuItem;
begin
  MenuItem := Sender as TMenuItem;
  if FMPVEngine <> nil then
    FMPVEngine.SetPropertyInt('aid', MenuItem.Tag);
  UpdateAudioTrackMenu;
end;

procedure TfrmMain.OnSubtitleTrackClick(Sender: TObject);
var
  MenuItem: TMenuItem;
begin
  MenuItem := Sender as TMenuItem;
  if FMPVEngine <> nil then
    FMPVEngine.SetPropertyInt('sid', MenuItem.Tag);
  UpdateSubtitleTrackMenu;
end;

{ ===============================================================================
  PHASE 7: PLAYBACK SPEED CONTROL
  =============================================================================== }

procedure TfrmMain.mnuPlaybackSpeed050Click(Sender: TObject);
begin
  SetPlaybackSpeed(0.5);
end;

procedure TfrmMain.mnuPlaybackSpeed075Click(Sender: TObject);
begin
  SetPlaybackSpeed(0.75);
end;

procedure TfrmMain.mnuPlaybackSpeed100Click(Sender: TObject);
begin
  SetPlaybackSpeed(1.0);
end;

procedure TfrmMain.mnuPlaybackSpeed125Click(Sender: TObject);
begin
  SetPlaybackSpeed(1.25);
end;

procedure TfrmMain.mnuPlaybackSpeed150Click(Sender: TObject);
begin
  SetPlaybackSpeed(1.5);
end;

procedure TfrmMain.mnuPlaybackSpeed200Click(Sender: TObject);
begin
  SetPlaybackSpeed(2.0);
end;

procedure TfrmMain.SetPlaybackSpeed(Speed: Double);
begin
  if Speed < 0.25 then Speed := 0.25;
  if Speed > 4.0 then Speed := 4.0;

  FPlaybackSpeed := Speed;

  if FMPVEngine <> nil then
    FMPVEngine.SetPropertyDouble('speed', FPlaybackSpeed);

  StatusBar.SimpleText := Format(Locale.Message('SpeedFormat', 'Speed: %.2fx'), [FPlaybackSpeed]);

  { Update menu checkmarks }
  mnuPlaybackSpeed050.Checked := Abs(FPlaybackSpeed - 0.5) < 0.01;
  mnuPlaybackSpeed075.Checked := Abs(FPlaybackSpeed - 0.75) < 0.01;
  mnuPlaybackSpeed100.Checked := Abs(FPlaybackSpeed - 1.0) < 0.01;
  mnuPlaybackSpeed125.Checked := Abs(FPlaybackSpeed - 1.25) < 0.01;
  mnuPlaybackSpeed150.Checked := Abs(FPlaybackSpeed - 1.5) < 0.01;
  mnuPlaybackSpeed200.Checked := Abs(FPlaybackSpeed - 2.0) < 0.01;
end;

{ ===============================================================================
  PHASE 7: A-B LOOP
  =============================================================================== }

procedure TfrmMain.mnuPlaybackSetAClick(Sender: TObject);
begin
  SetABLoopA;
end;

procedure TfrmMain.mnuPlaybackSetBClick(Sender: TObject);
begin
  SetABLoopB;
end;

procedure TfrmMain.mnuPlaybackClearABClick(Sender: TObject);
begin
  ClearABLoop;
end;

procedure TfrmMain.SetABLoopA;
begin
  if FMPVEngine = nil then Exit;

  FABLoopA := FMPVEngine.Position;
  FMPVEngine.SetPropertyDouble('ab-loop-a', FABLoopA);

  StatusBar.SimpleText := Format(Locale.Message('LoopASet', 'Loop A set at %s'), [FormatTime(FABLoopA)]);
  mnuPlaybackSetA.Caption := Format(Locale.Menu('SetAWithTime', 'Set A [%s]'), [FormatTime(FABLoopA)]);
end;

procedure TfrmMain.SetABLoopB;
begin
  if FMPVEngine = nil then Exit;
  if FABLoopA < 0 then
  begin
    StatusBar.SimpleText := Locale.Message('SetAFirst', 'Set point A first (press L)');
    Exit;
  end;

  FABLoopB := FMPVEngine.Position;
  FMPVEngine.SetPropertyDouble('ab-loop-b', FABLoopB);
  FABLoopActive := True;

  StatusBar.SimpleText := Format(Locale.Message('LoopBSet', 'Loop B set at %s - Loop active'), [FormatTime(FABLoopB)]);
  mnuPlaybackSetB.Caption := Format(Locale.Menu('SetBWithTime', 'Set B [%s]'), [FormatTime(FABLoopB)]);
end;

procedure TfrmMain.ClearABLoop;
begin
  if FMPVEngine = nil then Exit;

  FABLoopA := -1;
  FABLoopB := -1;
  FABLoopActive := False;

  FMPVEngine.SetPropertyString('ab-loop-a', 'no');
  FMPVEngine.SetPropertyString('ab-loop-b', 'no');

  StatusBar.SimpleText := Locale.Message('LoopCleared', 'A-B Loop cleared');
  mnuPlaybackSetA.Caption := Locale.Menu('PlaybackSetA', 'Set &A');
  mnuPlaybackSetB.Caption := Locale.Menu('PlaybackSetB', 'Set &B');
end;

{ ===============================================================================
  PHASE 7: VIDEO ROTATION AND FLIP
  =============================================================================== }

procedure TfrmMain.mnuVideoRotate0Click(Sender: TObject);
begin
  SetVideoRotation(0);
end;

procedure TfrmMain.mnuVideoRotate90Click(Sender: TObject);
begin
  SetVideoRotation(90);
end;

procedure TfrmMain.mnuVideoRotate180Click(Sender: TObject);
begin
  SetVideoRotation(180);
end;

procedure TfrmMain.mnuVideoRotate270Click(Sender: TObject);
begin
  SetVideoRotation(270);
end;

procedure TfrmMain.mnuVideoFlipHClick(Sender: TObject);
begin
  ToggleVideoFlipH;
end;

procedure TfrmMain.mnuVideoFlipVClick(Sender: TObject);
begin
  ToggleVideoFlipV;
end;

procedure TfrmMain.SetVideoRotation(Angle: Integer);
begin
  FVideoRotation := Angle mod 360;

  { Apply rotation via UpdateVideoFilters (uses lavfi filters for hw decoding compatibility) }
  UpdateVideoFilters;

  StatusBar.SimpleText := Format(Locale.Message('RotationFormat', 'Rotation: %d°'), [FVideoRotation]);

  { Update menu checkmarks }
  mnuVideoRotate0.Checked := (FVideoRotation = 0);
  mnuVideoRotate90.Checked := (FVideoRotation = 90);
  mnuVideoRotate180.Checked := (FVideoRotation = 180);
  mnuVideoRotate270.Checked := (FVideoRotation = 270);
end;

procedure TfrmMain.UpdateVideoFilters;
var
  Filters: string;

  procedure AddFilter(const F: string);
  begin
    if Filters <> '' then
      Filters := Filters + ',' + F
    else
      Filters := F;
  end;

begin
  if FMPVEngine = nil then Exit;

  { Build filter chain combining all active filters }
  Filters := '';

  { Rotation first (transpose: 1=90°CW, 2=90°CCW/270°CW) }
  case FVideoRotation of
    90:  AddFilter('transpose=1');
    180: AddFilter('transpose=1,transpose=1');
    270: AddFilter('transpose=2');
  end;

  { Then flips }
  if FVideoFlipH then
    AddFilter('hflip');
  if FVideoFlipV then
    AddFilter('vflip');

  { Prepend format conversion for hardware decoding compatibility }
  if Filters <> '' then
    Filters := 'format=fmt=yuv420p,' + Filters;

  { Set video filter property }
  FMPVEngine.SetPropertyString('vf', Filters);

  { Force video refresh }
  FMPVEngine.SeekRelative(0);
end;

procedure TfrmMain.ToggleVideoFlipH;
begin
  FVideoFlipH := not FVideoFlipH;
  UpdateVideoFilters;

  mnuVideoFlipH.Checked := FVideoFlipH;
  if FVideoFlipH then
    StatusBar.SimpleText := Locale.Message('FlipHOn', 'Horizontal flip: ON')
  else
    StatusBar.SimpleText := Locale.Message('FlipHOff', 'Horizontal flip: OFF');
end;

procedure TfrmMain.ToggleVideoFlipV;
begin
  FVideoFlipV := not FVideoFlipV;
  UpdateVideoFilters;

  mnuVideoFlipV.Checked := FVideoFlipV;
  if FVideoFlipV then
    StatusBar.SimpleText := Locale.Message('FlipVOn', 'Vertical flip: ON')
  else
    StatusBar.SimpleText := Locale.Message('FlipVOff', 'Vertical flip: OFF');
end;

{ ===============================================================================
  PHASE 7: DEINTERLACING
  =============================================================================== }

procedure TfrmMain.mnuVideoDeinterlaceClick(Sender: TObject);
begin
  ToggleDeinterlace;
end;

procedure TfrmMain.ToggleDeinterlace;
begin
  FDeinterlace := not FDeinterlace;

  if FMPVEngine <> nil then
  begin
    if FDeinterlace then
      FMPVEngine.SetPropertyString('deinterlace', 'yes')
    else
      FMPVEngine.SetPropertyString('deinterlace', 'no');
  end;

  mnuVideoDeinterlace.Checked := FDeinterlace;
  if FDeinterlace then
    StatusBar.SimpleText := Locale.Message('DeinterlaceOn', 'Deinterlace: ON')
  else
    StatusBar.SimpleText := Locale.Message('DeinterlaceOff', 'Deinterlace: OFF');
end;

{ ===============================================================================
  PHASE 7: SESSION RESTORE
  =============================================================================== }

procedure TfrmMain.SavePlaybackPosition;
begin
  if (FMPVEngine <> nil) and (FCurrentFileName <> '') then
  begin
    if FMPVEngine.Status in [msPlaying, msPaused] then
      Config.SavePlaybackPosition(FCurrentFileName, FMPVEngine.Position);
  end;
end;

procedure TfrmMain.RestorePlaybackPosition;
var
  SavedPos: Double;
begin
  if (FMPVEngine = nil) or (FCurrentFileName = '') then Exit;

  SavedPos := Config.GetPlaybackPosition(FCurrentFileName);
  if SavedPos > 0 then
  begin
    { Ask user if they want to resume }
    if MessageDlg(Locale.Dialog('ResumePlayback', 'Resume Playback'),
      Format(Locale.Dialog('ResumeFrom', 'Resume from %s?'), [FormatTime(SavedPos)]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      FMPVEngine.SeekAbsolute(SavedPos);
      StatusBar.SimpleText := Format(Locale.Message('ResumedAt', 'Resumed at %s'), [FormatTime(SavedPos)]);
    end;
  end;
end;

{ ===============================================================================
  PHASE 7: STARTUP FILE AND PUBLIC METHODS
  =============================================================================== }

procedure TfrmMain.PlayStartupFile(const FileName: string);
begin
  { Store the startup path - playback will be triggered in FormShow after MPV is initialized }
  if DirectoryExists(FileName) or FileExists(FileName) or (Pos('://', FileName) > 0) then
    FStartupPath := FileName;
end;

procedure TfrmMain.SetStartupOptions(const Options: TStartupOptions);
begin
  { Store CLI startup options - they will be applied in InitializeMPV and FormShow }
  FStartupOptions := Options;
  FStartupOptionsSet := True;
end;

procedure TfrmMain.PlayFolderAsync(Data: PtrInt);
var
  FileList: TStringList;
  FileArray: array of string;
  I: Integer;
begin
  if FCurrentFileName = '' then Exit;

  FileList := TStringList.Create;
  try
    ScanFolderForMedia(FCurrentFileName, FileList, True);
    FileList.Sort;

    if FileList.Count > 0 then
    begin
      SetLength(FileArray, FileList.Count);
      for I := 0 to FileList.Count - 1 do
        FileArray[I] := FileList[I];

      { Clear playlist and add files - this also starts playback }
      AddFilesToPlaylist(FileArray, True);
      StatusBar.SimpleText := Format(Locale.Message('FolderOpened', 'Opened folder: %d files'), [FileList.Count]);
    end;
  finally
    FileList.Free;
  end;
end;

procedure TfrmMain.AddToPlaylistFromExternal(const Path: string);
var
  FileList: TStringList;
  FileArray: array of string;
  I, FirstNewIndex: Integer;
begin
  if FPlaylistManager = nil then Exit;

  { Remember where new items will start }
  FirstNewIndex := FPlaylistManager.Count;

  if DirectoryExists(Path) then
  begin
    { Add folder contents to existing playlist }
    FileList := TStringList.Create;
    try
      ScanFolderForMedia(Path, FileList, True);
      FileList.Sort;

      if FileList.Count > 0 then
      begin
        SetLength(FileArray, FileList.Count);
        for I := 0 to FileList.Count - 1 do
          FileArray[I] := FileList[I];

        { Add to playlist without clearing }
        AddFilesToPlaylist(FileArray, False);
        StatusBar.SimpleText := Format(Locale.Message('FolderAdded', 'Added folder: %d files'), [FileList.Count]);
      end;
    finally
      FileList.Free;
    end;
  end
  else if FileExists(Path) or (Pos('://', Path) > 0) then
  begin
    { Add single file to playlist }
    if IsMediaFile(Path) or (Pos('://', Path) > 0) then
      FPlaylistManager.Add(Path);
  end;

  { Play from first newly added item }
  if (FPlaylistManager.Count > FirstNewIndex) then
  begin
    FPlaylistManager.CurrentIndex := FirstNewIndex;
    PlayFile(FPlaylistManager.Items[FirstNewIndex].FileName);
  end;

  { Bring window to front }
  Application.BringToFront;
  BringToFront;
end;

procedure TfrmMain.EnqueueToPlaylistFromExternal(const Path: string);
var
  FileList: TStringList;
  FileArray: array of string;
  I, AddedCount: Integer;
begin
  if FPlaylistManager = nil then Exit;

  AddedCount := 0;

  if DirectoryExists(Path) then
  begin
    { Add folder contents to existing playlist }
    FileList := TStringList.Create;
    try
      ScanFolderForMedia(Path, FileList, True);
      FileList.Sort;

      if FileList.Count > 0 then
      begin
        SetLength(FileArray, FileList.Count);
        for I := 0 to FileList.Count - 1 do
          FileArray[I] := FileList[I];

        { Add to playlist without clearing and without starting playback }
        AddFilesToPlaylist(FileArray, False);
        AddedCount := FileList.Count;
      end;
    finally
      FileList.Free;
    end;
  end
  else if FileExists(Path) or (Pos('://', Path) > 0) then
  begin
    { Add single file to playlist }
    if IsMediaFile(Path) or (Pos('://', Path) > 0) then
    begin
      FPlaylistManager.Add(Path);
      AddedCount := 1;
    end;
  end;

  { Update status and show playlist if items were added }
  if AddedCount > 0 then
  begin
    StatusBar.SimpleText := Format(Locale.Message('FilesEnqueued', 'Enqueued: %d file(s)'), [AddedCount]);
    { Show playlist form to indicate items were added }
    if (FPlaylistForm <> nil) and (not FPlaylistForm.Visible) then
    begin
      FPlaylistForm.Show;
      FPlaylistForm.BringToFront;
    end;
  end;

  { Bring window to front }
  Application.BringToFront;
  BringToFront;
end;

function GetIPCCommandFilePath: string;
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('TEMP') + PathDelim + '3nity-media-command.txt';
  {$ELSE}
  Result := GetEnvironmentVariable('XDG_RUNTIME_DIR');
  if Result <> '' then
    Result := Result + '/3nity-media-command.txt'
  else
    Result := '/tmp/3nity-media-command-' + GetEnvironmentVariable('USER') + '.txt';
  {$ENDIF}
end;

procedure TfrmMain.IPCTimerTimer(Sender: TObject);
const
  ENQUEUE_PREFIX = 'ENQUEUE:';
var
  CmdPath, Path: string;
  F: TextFile;
  IsEnqueue: Boolean;
begin
  CmdPath := GetIPCCommandFilePath;

  if not FileExists(CmdPath) then
    Exit;

  try
    { Read the command file }
    AssignFile(F, CmdPath);
    Reset(F);
    if not EOF(F) then
      ReadLn(F, Path);
    CloseFile(F);

    { Delete the command file immediately }
    DeleteFile(CmdPath);

    { Process the path if not empty }
    if Path <> '' then
    begin
      { Check for ENQUEUE: prefix }
      IsEnqueue := (Pos(ENQUEUE_PREFIX, Path) = 1);
      if IsEnqueue then
      begin
        { Remove prefix and enqueue without playing }
        Path := Copy(Path, Length(ENQUEUE_PREFIX) + 1, Length(Path));
        EnqueueToPlaylistFromExternal(Path);
      end
      else
        AddToPlaylistFromExternal(Path);
    end;
  except
    { Ignore errors }
  end;
end;

procedure TfrmMain.PlayFileAsync(Data: PtrInt);
begin
  if FCurrentFileName <> '' then
    PlayFile(FCurrentFileName);
end;

procedure TfrmMain.RefreshSeekPanelAsync(Data: PtrInt);
begin
  pnlSeek.Invalidate;
  pnlControls.Invalidate;
end;

procedure TfrmMain.OnFirstLaunchTimer(Sender: TObject);
begin
  { Disable timer immediately - this should only fire once }
  FFirstLaunchTimer.Enabled := False;

  { Force default size - use Width/Height not ClientWidth/ClientHeight
    to avoid GTK2 issues with ClientHeight timing }
  Width := DEF_MAIN_WIDTH;
  Height := DEF_MAIN_HEIGHT;

  { Center on screen }
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TfrmMain.ApplyVisualizationAsync(Data: PtrInt);
begin
  if FMPVEngine = nil then Exit;

  { The filter was cleared in PlayFile, file loaded normally. Now reapply the filter. }
  if (FVisualEffects <> nil) and FVisualEffects.Enabled and
     (FVisualEffects.Mode <> vmNone) and not FChangingVisualization then
  begin
    ApplyVisualization;
  end;
end;

procedure TfrmMain.AsyncPlayNextInPlaylist(Data: PtrInt);
begin
  { Called from watchdog when any visualization reaches end of track }
  {$IFDEF DEBUG}
  WriteLn('[DEBUG] AsyncPlayNextInPlaylist called');
  {$ENDIF}

  { For all visualization modes, we must completely disable visualization before loading next file }
  { because lavfi-complex filters can leave MPV in a state where it can't load new files }
  if (FVisualEffects <> nil) and FVisualEffects.Enabled and
     (FVisualEffects.Mode <> vmNone) then
  begin
    {$IFDEF DEBUG}
    WriteLn('[DEBUG] Disabling visualization (mode=', Ord(FVisualEffects.Mode), ') before next track');
    {$ENDIF}
    { Save current mode to restore later }
    FRestoreVisMode := FVisualEffects.Mode;
    FRestoreVisAfterLoad := True;

    { Keep FChangingVisualization=True until visualization is fully restored }
    { This prevents false EOF events from triggering playlist advance }
    FChangingVisualization := True;

    { Stop MPV first }
    if FMPVEngine <> nil then
    begin
      FMPVEngine.Stop;
      Application.ProcessMessages;
      { Clear the lavfi-complex filter }
      FMPVEngine.SetPropertyString('lavfi-complex', '');
      Application.ProcessMessages;
    end;

    { Disable visualization without triggering filter update }
    { Note: FChangingVisualization stays True - will be reset after filter reapply }
    FVisualEffects.Enabled := False;
    FVisualEffects.Mode := vmNone;
  end;

  PlayNextInPlaylist;
end;

procedure TfrmMain.OnFullscreenTimer(Sender: TObject);
begin
  { Disable timer - one-shot only }
  FFullscreenTimer.Enabled := False;

  { Apply fullscreen after form is fully realized }
  if FStartupOptionsSet and FStartupOptions.FullscreenSet and FStartupOptions.Fullscreen then
    SetFullscreen(True);
end;

procedure TfrmMain.OnVisualReapplyTimer(Sender: TObject);
begin
  { Disable timer - this is a one-shot timer }
  FVisualReapplyTimer.Enabled := False;

  if FMPVEngine = nil then
  begin
    FChangingVisualization := False;
    Exit;
  end;

  if FVisualReapplyNeeded then
  begin
    { Apply the visualization filter }
    if (FVisualEffects <> nil) and FVisualEffects.Enabled and
       (FVisualEffects.Mode <> vmNone) then
    begin
      FIgnoreNextEndFile := True;
      ApplyVisualization;
      { Schedule another timer tick to reset the flag after filter stabilizes }
      FVisualReapplyNeeded := False;
      FVisualReapplyTimer.Interval := 300;
      FVisualReapplyTimer.Enabled := True;
    end
    else
      FChangingVisualization := False;
  end
  else
  begin
    { Just reset the changing flag - filter was already applied }
    FChangingVisualization := False;
  end;
end;

procedure TfrmMain.OnVisualLoadTimer(Sender: TObject);
begin
  { Disable timer - this is a one-shot timer }
  FVisualLoadTimer.Enabled := False;

  if FMPVEngine = nil then Exit;
  if FPendingPlayFile = '' then Exit;

  { Now load the file - filter has been cleared and MPV has stabilized }
  FCurrentFileName := FPendingPlayFile;
  FMPVEngine.PlayMedia(FPendingPlayFile);

  FPendingPlayFile := '';
end;

{ ===============================================================================
  PHASE 8: MOUSE WHEEL HANDLER
  =============================================================================== }

procedure TfrmMain.pnlVideoMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if FMPVEngine = nil then Exit;

  if ssCtrl in Shift then
  begin
    { Ctrl + Wheel = Zoom }
    if WheelDelta > 0 then
      SetVideoZoom(FVideoZoom + 0.1)
    else
      SetVideoZoom(FVideoZoom - 0.1);
  end
  else if ssShift in Shift then
  begin
    { Shift + Wheel = Seek }
    if WheelDelta > 0 then
      FMPVEngine.SeekRelative(SEEK_SMALL)
    else
      FMPVEngine.SeekRelative(-SEEK_SMALL);
  end
  else
  begin
    { Wheel = Volume }
    if WheelDelta > 0 then
      FMPVEngine.Volume := FMPVEngine.Volume + 5
    else
      FMPVEngine.Volume := FMPVEngine.Volume - 5;
    tbVolume.Position := FMPVEngine.Volume;
    UpdateVolumeDisplay;
  end;

  Handled := True;
end;

procedure TfrmMain.pnlSeekMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Step: Integer;
  HadVisualization: Boolean;
begin
  if FMPVEngine = nil then Exit;

  { Mouse wheel on seek bar = Seek position }
  Step := (FSeekBar.Max - FSeekBar.Min) div 100;
  if Step < 1 then Step := 1;

  if WheelDelta > 0 then
    FSeekBar.Position := FSeekBar.Position + Step
  else
    FSeekBar.Position := FSeekBar.Position - Step;

  { Handle visualization filter during seek }
  HadVisualization := (FVisualEffects <> nil) and FVisualEffects.Enabled and
                      (FVisualEffects.Mode <> vmNone);
  if HadVisualization and not FChangingVisualization then
  begin
    FChangingVisualization := True;
    FIgnoreNextEndFile := True;
    FMPVEngine.SetPropertyString('lavfi-complex', '');
  end;

  FMPVEngine.SeekPercent(FSeekBar.Position / 10.0);

  { Reapply visualization after seek }
  if HadVisualization then
  begin
    FVisualReapplyNeeded := True;  { Timer should apply visualization }
    FVisualReapplyTimer.Enabled := False;
    FVisualReapplyTimer.Enabled := True;
    { Don't reset FChangingVisualization here - timer will do it }
  end;

  Handled := True;
end;

{ ===============================================================================
  PHASE 8: CHAPTER NAVIGATION
  =============================================================================== }

procedure TfrmMain.mnuPlaybackPrevChapterClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
  begin
    FMPVEngine.SendCmd('add chapter -1');
    StatusBar.SimpleText := Locale.Message('PreviousChapter', 'Previous chapter');
  end;
end;

procedure TfrmMain.mnuPlaybackNextChapterClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
  begin
    FMPVEngine.SendCmd('add chapter 1');
    StatusBar.SimpleText := Locale.Message('NextChapter', 'Next chapter');
  end;
end;

procedure TfrmMain.UpdateChaptersMenu;
var
  ChapterCount, I, CurrentChapter: Integer;
  MenuItem: TMenuItem;
  ChapterTitle: string;
  ChapterTime: Double;
begin
  mnuPlaybackChapters.Clear;

  if FMPVEngine = nil then Exit;

  ChapterCount := FMPVEngine.GetPropertyInt('chapter-list/count');
  CurrentChapter := FMPVEngine.GetPropertyInt('chapter');

  if ChapterCount = 0 then
  begin
    MenuItem := TMenuItem.Create(mnuPlaybackChapters);
    MenuItem.Caption := Locale.Dialog('NoChapters', '(No chapters)');
    MenuItem.Enabled := False;
    mnuPlaybackChapters.Add(MenuItem);
    Exit;
  end;

  for I := 0 to ChapterCount - 1 do
  begin
    ChapterTitle := FMPVEngine.GetPropertyString('chapter-list/' + IntToStr(I) + '/title');
    ChapterTime := FMPVEngine.GetPropertyDouble('chapter-list/' + IntToStr(I) + '/time');

    MenuItem := TMenuItem.Create(mnuPlaybackChapters);
    if ChapterTitle <> '' then
      MenuItem.Caption := Format(Locale.Message('ChapterWithTitle', '%d. %s (%s)'), [I + 1, ChapterTitle, FormatTime(ChapterTime)])
    else
      MenuItem.Caption := Format(Locale.Message('ChapterFormat', 'Chapter %d (%s)'), [I + 1, FormatTime(ChapterTime)]);

    MenuItem.Tag := I;
    MenuItem.RadioItem := True;
    MenuItem.Checked := (I = CurrentChapter);
    MenuItem.OnClick := @OnChapterClick;
    mnuPlaybackChapters.Add(MenuItem);
  end;
end;

procedure TfrmMain.OnChapterClick(Sender: TObject);
var
  MenuItem: TMenuItem;
begin
  MenuItem := Sender as TMenuItem;
  if FMPVEngine <> nil then
    FMPVEngine.SetPropertyInt('chapter', MenuItem.Tag);
  UpdateChaptersMenu;
end;

{ ===============================================================================
  PHASE 8: FRAME STEPPING
  =============================================================================== }

procedure TfrmMain.mnuPlaybackFrameStepClick(Sender: TObject);
begin
  FrameStep(True);
end;

procedure TfrmMain.mnuPlaybackFrameBackClick(Sender: TObject);
begin
  FrameStep(False);
end;

procedure TfrmMain.FrameStep(Forward: Boolean);
begin
  if FMPVEngine = nil then Exit;

  if Forward then
  begin
    FMPVEngine.SendCmd('frame-step');
    StatusBar.SimpleText := Locale.Message('FrameStepForward', 'Frame step forward');
  end
  else
  begin
    FMPVEngine.SendCmd('frame-back-step');
    StatusBar.SimpleText := Locale.Message('FrameStepBackward', 'Frame step backward');
  end;
end;

{ ===============================================================================
  PHASE 8: AUDIO DELAY
  =============================================================================== }

procedure TfrmMain.mnuAudioDelayPlusClick(Sender: TObject);
begin
  SetAudioDelay(FAudioDelay + 0.1);
end;

procedure TfrmMain.mnuAudioDelayMinusClick(Sender: TObject);
begin
  SetAudioDelay(FAudioDelay - 0.1);
end;

procedure TfrmMain.mnuAudioDelayResetClick(Sender: TObject);
begin
  SetAudioDelay(0);
end;

procedure TfrmMain.SetAudioDelay(Delay: Double);
begin
  if Delay < -10 then Delay := -10;
  if Delay > 10 then Delay := 10;

  FAudioDelay := Delay;

  if FMPVEngine <> nil then
    FMPVEngine.SetPropertyDouble('audio-delay', FAudioDelay);

  StatusBar.SimpleText := Format(Locale.Message('AudioDelayFormat', 'Audio delay: %.1f s'), [FAudioDelay]);
end;

{ ===============================================================================
  PHASE 8: SUBTITLE DELAY
  =============================================================================== }

procedure TfrmMain.mnuSubtitleDelayPlusClick(Sender: TObject);
begin
  SetSubtitleDelay(FSubtitleDelay + 0.1);
end;

procedure TfrmMain.mnuSubtitleDelayMinusClick(Sender: TObject);
begin
  SetSubtitleDelay(FSubtitleDelay - 0.1);
end;

procedure TfrmMain.mnuSubtitleDelayResetClick(Sender: TObject);
begin
  SetSubtitleDelay(0);
end;

procedure TfrmMain.SetSubtitleDelay(Delay: Double);
begin
  if Delay < -10 then Delay := -10;
  if Delay > 10 then Delay := 10;

  FSubtitleDelay := Delay;

  if FMPVEngine <> nil then
    FMPVEngine.SetPropertyDouble('sub-delay', FSubtitleDelay);

  StatusBar.SimpleText := Format(Locale.Message('SubtitleDelayFormat', 'Subtitle delay: %.1f s'), [FSubtitleDelay]);
end;

{ ===============================================================================
  PHASE 8: VIDEO ZOOM
  =============================================================================== }

procedure TfrmMain.mnuVideoZoomInClick(Sender: TObject);
begin
  SetVideoZoom(FVideoZoom + 0.1);
end;

procedure TfrmMain.mnuVideoZoomOutClick(Sender: TObject);
begin
  SetVideoZoom(FVideoZoom - 0.1);
end;

procedure TfrmMain.mnuVideoZoomResetClick(Sender: TObject);
begin
  SetVideoZoom(1.0);
  SetVideoPan(0, 0);
end;

procedure TfrmMain.SetVideoZoom(Zoom: Double);
begin
  if Zoom < 0.5 then Zoom := 0.5;
  if Zoom > 4.0 then Zoom := 4.0;

  FVideoZoom := Zoom;

  if FMPVEngine <> nil then
    FMPVEngine.SetPropertyDouble('video-zoom', Ln(FVideoZoom) / Ln(2));

  StatusBar.SimpleText := Format(Locale.Message('ZoomFormat', 'Zoom: %.0f%%'), [FVideoZoom * 100]);
end;

{ ===============================================================================
  PHASE 8: VIDEO PAN
  =============================================================================== }

procedure TfrmMain.mnuVideoPanLeftClick(Sender: TObject);
begin
  SetVideoPan(FVideoPanX - 0.05, FVideoPanY);
end;

procedure TfrmMain.mnuVideoPanRightClick(Sender: TObject);
begin
  SetVideoPan(FVideoPanX + 0.05, FVideoPanY);
end;

procedure TfrmMain.mnuVideoPanUpClick(Sender: TObject);
begin
  SetVideoPan(FVideoPanX, FVideoPanY - 0.05);
end;

procedure TfrmMain.mnuVideoPanDownClick(Sender: TObject);
begin
  SetVideoPan(FVideoPanX, FVideoPanY + 0.05);
end;

procedure TfrmMain.mnuVideoPanResetClick(Sender: TObject);
begin
  SetVideoPan(0, 0);
end;

procedure TfrmMain.SetVideoPan(X, Y: Double);
begin
  if X < -1 then X := -1;
  if X > 1 then X := 1;
  if Y < -1 then Y := -1;
  if Y > 1 then Y := 1;

  FVideoPanX := X;
  FVideoPanY := Y;

  if FMPVEngine <> nil then
  begin
    FMPVEngine.SetPropertyDouble('video-pan-x', FVideoPanX);
    FMPVEngine.SetPropertyDouble('video-pan-y', FVideoPanY);
  end;

  if (Abs(FVideoPanX) < 0.01) and (Abs(FVideoPanY) < 0.01) then
    StatusBar.SimpleText := Locale.Message('PanCenter', 'Pan: Center')
  else
    StatusBar.SimpleText := Format(Locale.Message('PanFormat', 'Pan: X=%.0f%% Y=%.0f%%'), [FVideoPanX * 100, FVideoPanY * 100]);
end;

{ ===============================================================================
  PHASE 9: VIEW AND WINDOW OPTIONS
  =============================================================================== }

procedure TfrmMain.mnuViewAlwaysOnTopClick(Sender: TObject);
begin
  ToggleAlwaysOnTop;
end;

procedure TfrmMain.mnuViewFitToVideoClick(Sender: TObject);
begin
  FitWindowToVideo;
end;

procedure TfrmMain.mnuViewScale50Click(Sender: TObject);
begin
  ScaleWindow(0.5);
end;

procedure TfrmMain.mnuViewScale100Click(Sender: TObject);
begin
  ScaleWindow(1.0);
end;

procedure TfrmMain.mnuViewScale150Click(Sender: TObject);
begin
  ScaleWindow(1.5);
end;

procedure TfrmMain.mnuViewScale200Click(Sender: TObject);
begin
  ScaleWindow(2.0);
end;

procedure TfrmMain.mnuVideoScreenshotAsClick(Sender: TObject);
begin
  TakeScreenshotWithDialog;
end;

procedure TfrmMain.ToggleAlwaysOnTop;
begin
  FAlwaysOnTop := not FAlwaysOnTop;

  if FAlwaysOnTop then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;

  mnuViewAlwaysOnTop.Checked := FAlwaysOnTop;
  mnuCtxAlwaysOnTop.Checked := FAlwaysOnTop;

  if FAlwaysOnTop then
    StatusBar.SimpleText := Locale.GetString('Status', 'AlwaysOnTopOn', 'Always on top: ON')
  else
    StatusBar.SimpleText := Locale.GetString('Status', 'AlwaysOnTopOff', 'Always on top: OFF');
end;

procedure TfrmMain.FitWindowToVideo;
var
  VideoWidth, VideoHeight: Integer;
  NewWidth, NewHeight: Integer;
  ControlsHeight: Integer;
begin
  if FMPVEngine = nil then Exit;

  VideoWidth := FMPVEngine.GetPropertyInt('width');
  VideoHeight := FMPVEngine.GetPropertyInt('height');

  if (VideoWidth <= 0) or (VideoHeight <= 0) then
  begin
    StatusBar.SimpleText := Locale.Message('NoVideoLoaded', 'No video loaded');
    Exit;
  end;

  { Calculate controls height (status bar + control panel) }
  ControlsHeight := StatusBar.Height + pnlControls.Height;

  { Calculate new window size }
  NewWidth := VideoWidth;
  NewHeight := VideoHeight + ControlsHeight;

  { Ensure minimum size }
  if NewWidth < DEF_MAIN_MIN_WIDTH then NewWidth := DEF_MAIN_MIN_WIDTH;
  if NewHeight < DEF_MAIN_MIN_HEIGHT then NewHeight := DEF_MAIN_MIN_HEIGHT;

  { Apply new size }
  if not FFullscreen then
  begin
    Width := NewWidth;
    Height := NewHeight;
    StatusBar.SimpleText := Format(Locale.Message('WindowFitted', 'Window fitted to %dx%d'), [VideoWidth, VideoHeight]);
  end;
end;

procedure TfrmMain.ScaleWindow(Scale: Double);
var
  VideoWidth, VideoHeight: Integer;
  NewWidth, NewHeight: Integer;
  ControlsHeight: Integer;
begin
  if FMPVEngine = nil then Exit;

  VideoWidth := FMPVEngine.GetPropertyInt('width');
  VideoHeight := FMPVEngine.GetPropertyInt('height');

  if (VideoWidth <= 0) or (VideoHeight <= 0) then
  begin
    StatusBar.SimpleText := Locale.Message('NoVideoLoaded', 'No video loaded');
    Exit;
  end;

  { Calculate controls height (status bar + control panel) }
  ControlsHeight := StatusBar.Height + pnlControls.Height;

  { Calculate new window size with scale }
  NewWidth := Round(VideoWidth * Scale);
  NewHeight := Round(VideoHeight * Scale) + ControlsHeight;

  { Ensure minimum size }
  if NewWidth < DEF_MAIN_MIN_WIDTH then NewWidth := DEF_MAIN_MIN_WIDTH;
  if NewHeight < DEF_MAIN_MIN_HEIGHT then NewHeight := DEF_MAIN_MIN_HEIGHT;

  { Apply new size }
  if not FFullscreen then
  begin
    Width := NewWidth;
    Height := NewHeight;
    StatusBar.SimpleText := Format(Locale.Message('WindowScale', 'Window scale: %.0f%%'), [Scale * 100]);
  end;

  { Update menu checkmarks }
  mnuViewScale50.Checked := Abs(Scale - 0.5) < 0.01;
  mnuViewScale100.Checked := Abs(Scale - 1.0) < 0.01;
  mnuViewScale150.Checked := Abs(Scale - 1.5) < 0.01;
  mnuViewScale200.Checked := Abs(Scale - 2.0) < 0.01;
end;

procedure TfrmMain.TakeScreenshotWithDialog;
var
  ScreenshotPath: string;
  ScreenshotDir: string;
begin
  if FMPVEngine = nil then Exit;

  { Use configured screenshot path }
  ScreenshotDir := Config.Settings.General.ScreenshotPath;
  if not DirectoryExists(ScreenshotDir) then
    ForceDirectories(ScreenshotDir);

  SaveDialog.InitialDir := ScreenshotDir;
  SaveDialog.FileName := 'screenshot_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.png';
  SaveDialog.Filter := Locale.Dialog('ScreenshotFilter', 'PNG Image|*.png|JPEG Image|*.jpg;*.jpeg|All Files|*.*');
  SaveDialog.DefaultExt := 'png';
  SaveDialog.Title := Locale.Dialog('SaveScreenshot', 'Save Screenshot');

  if SaveDialog.Execute then
  begin
    ScreenshotPath := SaveDialog.FileName;
    FMPVEngine.ScreenshotToFile(ScreenshotPath, 'video');
    StatusBar.SimpleText := Format(Locale.Message('ScreenshotSaved', 'Screenshot saved: %s'), [ExtractFileName(ScreenshotPath)]);
  end;
end;

{ ===============================================================================
  PHASE 10: VIDEO COLOR ADJUSTMENTS
  =============================================================================== }

procedure TfrmMain.mnuVideoBrightnessUpClick(Sender: TObject);
begin
  AdjustBrightness(5);
end;

procedure TfrmMain.mnuVideoBrightnessDownClick(Sender: TObject);
begin
  AdjustBrightness(-5);
end;

procedure TfrmMain.mnuVideoBrightnessResetClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
  begin
    FMPVEngine.Brightness := 0;
    StatusBar.SimpleText := Format(Locale.Message('BrightnessFormat', 'Brightness: %d'), [0]);
  end;
end;

procedure TfrmMain.mnuVideoContrastUpClick(Sender: TObject);
begin
  AdjustContrast(5);
end;

procedure TfrmMain.mnuVideoContrastDownClick(Sender: TObject);
begin
  AdjustContrast(-5);
end;

procedure TfrmMain.mnuVideoContrastResetClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
  begin
    FMPVEngine.Contrast := 0;
    StatusBar.SimpleText := Format(Locale.Message('ContrastFormat', 'Contrast: %d'), [0]);
  end;
end;

procedure TfrmMain.mnuVideoSaturationUpClick(Sender: TObject);
begin
  AdjustSaturation(5);
end;

procedure TfrmMain.mnuVideoSaturationDownClick(Sender: TObject);
begin
  AdjustSaturation(-5);
end;

procedure TfrmMain.mnuVideoSaturationResetClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
  begin
    FMPVEngine.Saturation := 0;
    StatusBar.SimpleText := Format(Locale.Message('SaturationFormat', 'Saturation: %d'), [0]);
  end;
end;

procedure TfrmMain.mnuVideoHueUpClick(Sender: TObject);
begin
  AdjustHue(5);
end;

procedure TfrmMain.mnuVideoHueDownClick(Sender: TObject);
begin
  AdjustHue(-5);
end;

procedure TfrmMain.mnuVideoHueResetClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
  begin
    FMPVEngine.Hue := 0;
    StatusBar.SimpleText := Format(Locale.Message('HueFormat', 'Hue: %d'), [0]);
  end;
end;

procedure TfrmMain.mnuVideoGammaUpClick(Sender: TObject);
begin
  AdjustGamma(5);
end;

procedure TfrmMain.mnuVideoGammaDownClick(Sender: TObject);
begin
  AdjustGamma(-5);
end;

procedure TfrmMain.mnuVideoGammaResetClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
  begin
    FMPVEngine.Gamma := 0;
    StatusBar.SimpleText := Format(Locale.Message('GammaFormat', 'Gamma: %d'), [0]);
  end;
end;

procedure TfrmMain.mnuVideoColorsResetClick(Sender: TObject);
begin
  ResetAllColors;
end;

procedure TfrmMain.mnuVideoAdjustDialogClick(Sender: TObject);
begin
  if frmVideoAdjust = nil then
    Application.CreateForm(TfrmVideoAdjust, frmVideoAdjust);
  frmVideoAdjust.MPVEngine := FMPVEngine;
  frmVideoAdjust.Show;
end;

procedure TfrmMain.AdjustBrightness(Delta: Integer);
var
  NewValue: Integer;
begin
  if FMPVEngine = nil then Exit;

  NewValue := FMPVEngine.Brightness + Delta;
  if NewValue < -100 then NewValue := -100;
  if NewValue > 100 then NewValue := 100;

  FMPVEngine.Brightness := NewValue;
  StatusBar.SimpleText := Format(Locale.Message('BrightnessFormat', 'Brightness: %d'), [NewValue]);
end;

procedure TfrmMain.AdjustContrast(Delta: Integer);
var
  NewValue: Integer;
begin
  if FMPVEngine = nil then Exit;

  NewValue := FMPVEngine.Contrast + Delta;
  if NewValue < -100 then NewValue := -100;
  if NewValue > 100 then NewValue := 100;

  FMPVEngine.Contrast := NewValue;
  StatusBar.SimpleText := Format(Locale.Message('ContrastFormat', 'Contrast: %d'), [NewValue]);
end;

procedure TfrmMain.AdjustSaturation(Delta: Integer);
var
  NewValue: Integer;
begin
  if FMPVEngine = nil then Exit;

  NewValue := FMPVEngine.Saturation + Delta;
  if NewValue < -100 then NewValue := -100;
  if NewValue > 100 then NewValue := 100;

  FMPVEngine.Saturation := NewValue;
  StatusBar.SimpleText := Format(Locale.Message('SaturationFormat', 'Saturation: %d'), [NewValue]);
end;

procedure TfrmMain.AdjustHue(Delta: Integer);
var
  NewValue: Integer;
begin
  if FMPVEngine = nil then Exit;

  NewValue := FMPVEngine.Hue + Delta;
  if NewValue < -100 then NewValue := -100;
  if NewValue > 100 then NewValue := 100;

  FMPVEngine.Hue := NewValue;
  StatusBar.SimpleText := Format(Locale.Message('HueFormat', 'Hue: %d'), [NewValue]);
end;

procedure TfrmMain.AdjustGamma(Delta: Integer);
var
  NewValue: Integer;
begin
  if FMPVEngine = nil then Exit;

  NewValue := FMPVEngine.Gamma + Delta;
  if NewValue < -100 then NewValue := -100;
  if NewValue > 100 then NewValue := 100;

  FMPVEngine.Gamma := NewValue;
  StatusBar.SimpleText := Format(Locale.Message('GammaFormat', 'Gamma: %d'), [NewValue]);
end;

procedure TfrmMain.ResetAllColors;
begin
  if FMPVEngine = nil then Exit;

  FMPVEngine.Brightness := 0;
  FMPVEngine.Contrast := 0;
  FMPVEngine.Saturation := 0;
  FMPVEngine.Hue := 0;
  FMPVEngine.Gamma := 0;

  StatusBar.SimpleText := Locale.Message('ColorsReset', 'Video colors reset');
end;

{ ===============================================================================
  PHASE 10: VIDEO TRACK MENU
  =============================================================================== }

procedure TfrmMain.UpdateVideoTrackMenu;
var
  TrackCount, I, CurrentTrack: Integer;
  MenuItem: TMenuItem;
  TrackTitle, TrackLang: string;
begin
  mnuVideoTrack.Clear;

  if FMPVEngine = nil then Exit;

  TrackCount := FMPVEngine.GetPropertyInt('track-list/count');
  CurrentTrack := FMPVEngine.GetPropertyInt('vid');

  for I := 0 to TrackCount - 1 do
  begin
    if FMPVEngine.GetPropertyString('track-list/' + IntToStr(I) + '/type') = 'video' then
    begin
      TrackTitle := FMPVEngine.GetPropertyString('track-list/' + IntToStr(I) + '/title');
      TrackLang := FMPVEngine.GetPropertyString('track-list/' + IntToStr(I) + '/lang');

      MenuItem := TMenuItem.Create(mnuVideoTrack);
      if TrackTitle <> '' then
        MenuItem.Caption := Format(Locale.Menu('TrackWithTitle', 'Track %d: %s'), [I + 1, TrackTitle])
      else if TrackLang <> '' then
        MenuItem.Caption := Format(Locale.Menu('TrackWithLang', 'Track %d (%s)'), [I + 1, TrackLang])
      else
        MenuItem.Caption := Format(Locale.Menu('TrackNumber', 'Track %d'), [I + 1]);

      MenuItem.Tag := FMPVEngine.GetPropertyInt('track-list/' + IntToStr(I) + '/id');
      MenuItem.RadioItem := True;
      MenuItem.Checked := (MenuItem.Tag = CurrentTrack);
      MenuItem.OnClick := @OnVideoTrackClick;
      mnuVideoTrack.Add(MenuItem);
    end;
  end;

  if mnuVideoTrack.Count = 0 then
  begin
    MenuItem := TMenuItem.Create(mnuVideoTrack);
    MenuItem.Caption := Locale.Dialog('NoVideoTracks', '(No video tracks)');
    MenuItem.Enabled := False;
    mnuVideoTrack.Add(MenuItem);
  end;
end;

procedure TfrmMain.OnVideoTrackClick(Sender: TObject);
var
  MenuItem: TMenuItem;
begin
  MenuItem := Sender as TMenuItem;
  if FMPVEngine <> nil then
    FMPVEngine.SetPropertyInt('vid', MenuItem.Tag);
  UpdateVideoTrackMenu;
end;

{ ===============================================================================
  PHASE 10: AUDIO NORMALIZATION
  =============================================================================== }

procedure TfrmMain.mnuAudioNormalizeClick(Sender: TObject);
begin
  ToggleAudioNormalize;
end;

procedure TfrmMain.ToggleAudioNormalize;
begin
  if FMPVEngine = nil then Exit;

  FAudioNormalize := not FAudioNormalize;

  if FAudioNormalize then
  begin
    FMPVEngine.SetPropertyString('af', 'loudnorm=I=-16:TP=-1.5:LRA=11');
    StatusBar.SimpleText := Locale.Message('AudioNormalizeOn', 'Audio normalization: ON');
  end
  else
  begin
    FMPVEngine.SetPropertyString('af', '');
    StatusBar.SimpleText := Locale.Message('AudioNormalizeOff', 'Audio normalization: OFF');
  end;

  mnuAudioNormalize.Checked := FAudioNormalize;
end;

{ ===============================================================================
  PHASE 10: OPEN URL FROM CLIPBOARD
  =============================================================================== }

procedure TfrmMain.mnuFileOpenClipboardClick(Sender: TObject);
begin
  OpenClipboardURL;
end;

procedure TfrmMain.OpenClipboardURL;
var
  ClipText: string;
begin
  if Clipboard.HasFormat(CF_TEXT) then
  begin
    ClipText := Trim(Clipboard.AsText);

    { Check if it looks like a URL or file path }
    if (Pos('://', ClipText) > 0) or
       (Pos('/', ClipText) = 1) or
       (Pos('\', ClipText) > 0) or
       FileExists(ClipText) then
    begin
      PlayFile(ClipText);
      StatusBar.SimpleText := Locale.Message('PlayingFromClipboard', 'Playing from clipboard: %s');
      StatusBar.SimpleText := Format(StatusBar.SimpleText, [ExtractFileName(ClipText)]);
    end
    else
    begin
      StatusBar.SimpleText := Locale.Message('ClipboardInvalid', 'Clipboard does not contain a valid URL or path');
    end;
  end
  else
  begin
    StatusBar.SimpleText := Locale.Message('ClipboardEmpty', 'Clipboard is empty or does not contain text');
  end;
end;

{ ===============================================================================
  PHASE 11: INTERNATIONALIZATION
  =============================================================================== }

procedure TfrmMain.InitializeLanguage;
var
  SavedLang: string;
begin
  { Get saved language from config }
  SavedLang := Config.Settings.General.Language;
  if SavedLang = '' then
    SavedLang := 'en';

  { Load the language }
  Locale.LoadLanguage(SavedLang);

  { Update language menu }
  UpdateLanguageMenu;

  { Apply translations to UI }
  ApplyTranslations;
end;
      {
procedure TfrmMain.UpdateLanguageMenu;
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  mnuToolsLanguage.Clear;

  for I := 0 to Locale.AvailableLanguages.Count - 1 do
  begin
    MenuItem := TMenuItem.Create(mnuToolsLanguage);
    MenuItem.Caption := Locale.LanguageNames[I];
    MenuItem.Tag := I;
    MenuItem.RadioItem := True;
    MenuItem.Checked := (Locale.AvailableLanguages[I] = Locale.CurrentLanguage);
    MenuItem.OnClick := @OnLanguageClick;
    mnuToolsLanguage.Add(MenuItem);
  end;
end;  }

procedure TfrmMain.UpdateLanguageMenu;
var
  I, OriginalIndex: Integer;
  MenuItem: TMenuItem;
  SortedList: TStringList;
begin
  mnuToolsLanguage.Clear;

  SortedList := TStringList.Create;
  try
    // Ajouter les langues avec leur index comme "Objects"
    for I := 0 to Locale.AvailableLanguages.Count - 1 do
      SortedList.AddObject(Locale.LanguageNames[I], TObject(PtrInt(I)));

    // Tri automatique
    SortedList.Sort;

    // Créer les MenuItem dans l'ordre alphabétique
    for I := 0 to SortedList.Count - 1 do
    begin
      OriginalIndex := PtrInt(SortedList.Objects[I]);
      MenuItem := TMenuItem.Create(mnuToolsLanguage);
      MenuItem.Caption := SortedList[I];
      MenuItem.Tag := OriginalIndex;
      MenuItem.RadioItem := True;
      MenuItem.Checked := (Locale.AvailableLanguages[OriginalIndex] = Locale.CurrentLanguage);
      MenuItem.OnClick := @OnLanguageClick;
      mnuToolsLanguage.Add(MenuItem);
    end;
  finally
    SortedList.Free;
  end;
end;

procedure TfrmMain.OnLanguageClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  LangCode: string;
  Settings: TAppSettings;
begin
  MenuItem := Sender as TMenuItem;
  if MenuItem.Tag < Locale.AvailableLanguages.Count then
  begin
    LangCode := Locale.AvailableLanguages[MenuItem.Tag];

    { Save language preference }
    Settings := Config.Settings;
    Settings.General.Language := LangCode;
    Config.Settings := Settings;
    Config.Modified := True;

    { Load the new language }
    Locale.LoadLanguage(LangCode);

    { Update UI }
    UpdateLanguageMenu;
    ApplyTranslations;

    { Show restart message }
    StatusBar.SimpleText := Locale.GetString('Options', 'RestartRequired',
      'Restart required for changes to take effect');
  end;
end;

procedure TfrmMain.ApplyTranslations;
begin
  { Apply translations to main menu items }

  { File menu }
  mnuFile.Caption := Locale.Menu('File', '&File');
  mnuFileOpen.Caption := Locale.Menu('FileOpen', '&Open File...');
  mnuFileOpenURL.Caption := Locale.Menu('FileOpenURL', 'Open &URL...');
  mnuFileOpenClipboard.Caption := Locale.Menu('FileOpenClipboard', 'Open from &Clipboard');
  mnuFileOpenFolder.Caption := Locale.Menu('FileOpenFolder', 'Open F&older...');
  mnuFileRecent.Caption := Locale.Menu('FileRecent', '&Recent Files');
  mnuFileExit.Caption := Locale.Menu('FileExit', 'E&xit');

  { Playback menu }
  mnuPlayback.Caption := Locale.Menu('Playback', '&Playback');
  mnuPlaybackPlayPause.Caption := Locale.Menu('PlaybackPlayPause', '&Play/Pause');
  mnuPlaybackStop.Caption := Locale.Menu('PlaybackStop', '&Stop');
  mnuPlaybackPrevious.Caption := Locale.Menu('PlaybackPrevious', 'Pre&vious');
  mnuPlaybackNext.Caption := Locale.Menu('PlaybackNext', '&Next');
  mnuPlaybackSeekFwd.Caption := Locale.Menu('PlaybackSeekForward', 'Seek &Forward');
  mnuPlaybackSeekBack.Caption := Locale.Menu('PlaybackSeekBackward', 'Seek &Backward');
  mnuPlaybackSpeed.Caption := Locale.Menu('PlaybackSpeed', '&Speed');
  mnuPlaybackSpeed050.Caption := Locale.Menu('Speed050', '0.5x');
  mnuPlaybackSpeed075.Caption := Locale.Menu('Speed075', '0.75x');
  mnuPlaybackSpeed100.Caption := Locale.Menu('Speed100', '1.0x (Normal)');
  mnuPlaybackSpeed125.Caption := Locale.Menu('Speed125', '1.25x');
  mnuPlaybackSpeed150.Caption := Locale.Menu('Speed150', '1.5x');
  mnuPlaybackSpeed200.Caption := Locale.Menu('Speed200', '2.0x');
  mnuPlaybackABLoop.Caption := Locale.Menu('PlaybackABLoop', 'A-&B Loop');
  mnuPlaybackSetA.Caption := Locale.Menu('PlaybackSetA', 'Set &A');
  mnuPlaybackSetB.Caption := Locale.Menu('PlaybackSetB', 'Set &B');
  mnuPlaybackClearAB.Caption := Locale.Menu('PlaybackClearAB', '&Clear A-B');
  mnuPlaybackChapters.Caption := Locale.Menu('PlaybackChapters', '&Chapters');
  mnuPlaybackPrevChapter.Caption := Locale.Menu('PlaybackPrevChapter', 'Previous Chapter');
  mnuPlaybackNextChapter.Caption := Locale.Menu('PlaybackNextChapter', 'Next Chapter');
  mnuPlaybackFrameStep.Caption := Locale.Menu('PlaybackFrameStep', 'Frame Step &Forward');
  mnuPlaybackFrameBack.Caption := Locale.Menu('PlaybackFrameBack', 'Frame Step &Backward');
  mnuPlaybackGotoTime.Caption := Locale.Menu('PlaybackGotoTime', '&Go to Time...');
  mnuPlaybackAddBookmark.Caption := Locale.Menu('PlaybackAddBookmark', 'Add &Bookmark...');
  mnuPlaybackBookmarks.Caption := Locale.Menu('PlaybackBookmarks', 'Book&marks');

  { Audio menu }
  mnuAudio.Caption := Locale.Menu('Audio', '&Audio');
  mnuAudioTrack.Caption := Locale.Menu('AudioTrack', 'Audio &Track');
  mnuAudioVolUp.Caption := Locale.Menu('AudioVolumeUp', 'Volume &Up');
  mnuAudioVolDown.Caption := Locale.Menu('AudioVolumeDown', 'Volume &Down');
  mnuAudioMute.Caption := Locale.Menu('AudioMute', '&Mute');
  mnuAudioEqualizer.Caption := Locale.Menu('AudioEqualizer', '&Equalizer');
  mnuAudioDelay.Caption := Locale.Menu('AudioDelay', 'Audio &Delay');
  mnuAudioDelayPlus.Caption := Locale.Menu('AudioDelayPlus', 'Delay +0.1s');
  mnuAudioDelayMinus.Caption := Locale.Menu('AudioDelayMinus', 'Delay -0.1s');
  mnuAudioDelayReset.Caption := Locale.Menu('AudioDelayReset', 'Reset Delay');
  mnuAudioNormalize.Caption := Locale.Menu('AudioNormalize', '&Normalize Volume');

  { Video menu }
  mnuVideo.Caption := Locale.Menu('Video', '&Video');
  mnuVideoFullscreen.Caption := Locale.Menu('VideoFullscreen', '&Fullscreen');
  mnuVideoAspect.Caption := Locale.Menu('VideoAspect', '&Aspect Ratio');
  mnuVideoAspectAuto.Caption := Locale.Menu('VideoAspectAuto', '&Auto');
  mnuVideoAspect169.Caption := Locale.Menu('VideoAspect169', '&16:9');
  mnuVideoAspect43.Caption := Locale.Menu('VideoAspect43', '&4:3');
  mnuVideoAspect235.Caption := Locale.Menu('VideoAspect235', '&2.35:1');
  mnuVideoScreenshot.Caption := Locale.Menu('VideoScreenshot', '&Screenshot');
  mnuVideoScreenshotAs.Caption := Locale.Menu('VideoScreenshotAs', 'Screenshot &As...');
  mnuVideoRotation.Caption := Locale.Menu('VideoRotation', '&Rotation');
  mnuVideoRotate0.Caption := Locale.Menu('VideoRotate0', '&0° (Normal)');
  mnuVideoRotate90.Caption := Locale.Menu('VideoRotate90', '&90°');
  mnuVideoRotate180.Caption := Locale.Menu('VideoRotate180', '&180°');
  mnuVideoRotate270.Caption := Locale.Menu('VideoRotate270', '&270°');
  mnuVideoFlip.Caption := Locale.Menu('VideoFlip', '&Flip');
  mnuVideoFlipH.Caption := Locale.Menu('VideoFlipH', 'Flip &Horizontal');
  mnuVideoFlipV.Caption := Locale.Menu('VideoFlipV', 'Flip &Vertical');
  mnuVideoDeinterlace.Caption := Locale.Menu('VideoDeinterlace', '&Deinterlace');
  mnuVideoZoom.Caption := Locale.Menu('VideoZoom', '&Zoom');
  mnuVideoZoomIn.Caption := Locale.Menu('VideoZoomIn', 'Zoom &In');
  mnuVideoZoomOut.Caption := Locale.Menu('VideoZoomOut', 'Zoom &Out');
  mnuVideoZoomReset.Caption := Locale.Menu('VideoZoomReset', '&Reset Zoom');
  mnuVideoPan.Caption := Locale.Menu('VideoPan', '&Pan');
  mnuVideoPanLeft.Caption := Locale.Menu('VideoPanLeft', 'Pan &Left');
  mnuVideoPanRight.Caption := Locale.Menu('VideoPanRight', 'Pan &Right');
  mnuVideoPanUp.Caption := Locale.Menu('VideoPanUp', 'Pan &Up');
  mnuVideoPanDown.Caption := Locale.Menu('VideoPanDown', 'Pan &Down');
  mnuVideoPanReset.Caption := Locale.Menu('VideoPanReset', 'R&eset Pan');
  mnuVideoTrack.Caption := Locale.Menu('VideoTrack', 'Video &Track');
  mnuVideoAdjustDialog.Caption := Locale.Menu('VideoAdjustDialog', 'Video &Adjustments...');
  mnuVideoColors.Caption := Locale.Menu('VideoColors', 'Color &Adjustments');
  mnuVideoBrightness.Caption := Locale.Menu('VideoBrightness', '&Brightness');
  mnuVideoBrightnessUp.Caption := Locale.Menu('VideoIncrease', 'Increase (+5)');
  mnuVideoBrightnessDown.Caption := Locale.Menu('VideoDecrease', 'Decrease (-5)');
  mnuVideoBrightnessReset.Caption := Locale.Menu('VideoReset', 'Reset');
  mnuVideoContrast.Caption := Locale.Menu('VideoContrast', '&Contrast');
  mnuVideoContrastUp.Caption := Locale.Menu('VideoIncrease', 'Increase (+5)');
  mnuVideoContrastDown.Caption := Locale.Menu('VideoDecrease', 'Decrease (-5)');
  mnuVideoContrastReset.Caption := Locale.Menu('VideoReset', 'Reset');
  mnuVideoSaturation.Caption := Locale.Menu('VideoSaturation', '&Saturation');
  mnuVideoSaturationUp.Caption := Locale.Menu('VideoIncrease', 'Increase (+5)');
  mnuVideoSaturationDown.Caption := Locale.Menu('VideoDecrease', 'Decrease (-5)');
  mnuVideoSaturationReset.Caption := Locale.Menu('VideoReset', 'Reset');
  mnuVideoHue.Caption := Locale.Menu('VideoHue', '&Hue');
  mnuVideoHueUp.Caption := Locale.Menu('VideoIncrease', 'Increase (+5)');
  mnuVideoHueDown.Caption := Locale.Menu('VideoDecrease', 'Decrease (-5)');
  mnuVideoHueReset.Caption := Locale.Menu('VideoReset', 'Reset');
  mnuVideoGamma.Caption := Locale.Menu('VideoGamma', '&Gamma');
  mnuVideoGammaUp.Caption := Locale.Menu('VideoIncrease', 'Increase (+5)');
  mnuVideoGammaDown.Caption := Locale.Menu('VideoDecrease', 'Decrease (-5)');
  mnuVideoGammaReset.Caption := Locale.Menu('VideoReset', 'Reset');
  mnuVideoColorsReset.Caption := Locale.Menu('VideoColorsReset', 'Reset &All Colors');

  { View menu }
  mnuView.Caption := Locale.Menu('View', 'Vi&ew');
  mnuViewAlwaysOnTop.Caption := Locale.Menu('ViewAlwaysOnTop', '&Always on Top');
  mnuViewFitToVideo.Caption := Locale.Menu('ViewFitToVideo', '&Fit Window to Video');
  mnuViewScale.Caption := Locale.Menu('ViewScale', 'Window &Scale');
  mnuViewScale50.Caption := Locale.Menu('Scale50', '&50%');
  mnuViewScale100.Caption := Locale.Menu('Scale100', '&100%');
  mnuViewScale150.Caption := Locale.Menu('Scale150', '1&50%');
  mnuViewScale200.Caption := Locale.Menu('Scale200', '&200%');

  { Phase 26: Visualization menu }
  mnuViewVisualization.Caption := Locale.Menu('ViewVisualization', '&Visualization');
  mnuVisNone.Caption := Locale.Menu('VisNone', '&None');
  mnuVisSpectrum.Caption := Locale.Menu('VisSpectrum', '&Spectrum Analyzer');
  mnuVisWaveform.Caption := Locale.Menu('VisWaveform', '&Waveform');
  mnuVisVector.Caption := Locale.Menu('VisVector', '&Vector Scope');
  mnuVisVolume.Caption := Locale.Menu('VisVolume', 'V&U Meter');
  mnuVisNextMode.Caption := Locale.Menu('VisNextMode', 'Next &Mode');
  mnuVisNextColor.Caption := Locale.Menu('VisNextColor', 'Next &Color Scheme');

  { Subtitles menu }
  mnuSubtitle.Caption := Locale.Menu('Subtitles', '&Subtitles');
  mnuSubtitleTrack.Caption := Locale.Menu('SubtitleTrack', 'Subtitle &Track');
  mnuSubtitleLoad.Caption := Locale.Menu('SubtitleLoad', '&Load Subtitle...');
  mnuSubtitleDelay.Caption := Locale.Menu('SubtitleDelay', 'Subtitle &Delay');
  mnuSubtitleDelayPlus.Caption := Locale.Menu('SubtitleDelayPlus', 'Delay +0.1s (X)');
  mnuSubtitleDelayMinus.Caption := Locale.Menu('SubtitleDelayMinus', 'Delay -0.1s (Z)');
  mnuSubtitleDelayReset.Caption := Locale.Menu('SubtitleDelayReset', 'Reset Delay');

  { Tools menu }
  mnuTools.Caption := Locale.Menu('Tools', '&Tools');
  mnuToolsPlaylist.Caption := Locale.Menu('ToolsPlaylist', '&Playlist');
  mnuToolsRadios.Caption := Locale.Menu('ToolsRadios', '&Radios');
  mnuToolsHistory.Caption := Locale.Menu('ToolsHistory', '&History');
  mnuToolsFavorites.Caption := Locale.Menu('ToolsFavorites', '&Favorites');
  mnuToolsAddFavorite.Caption := Locale.Menu('ToolsAddFavorite', 'Add to Favo&rites');
  mnuToolsSleepTimer.Caption := Locale.Menu('ToolsSleepTimer', '&Sleep Timer...');
  mnuToolsCancelTimer.Caption := Locale.Menu('ToolsCancelTimer', 'Cancel &Timer');
  mnuToolsShortcuts.Caption := Locale.Menu('ToolsShortcuts', '&Keyboard Shortcuts...');
  mnuToolsLanguage.Caption := Locale.GetString('Options', 'Language', 'Language');
  mnuToolsOptions.Caption := Locale.Menu('ToolsOptions', '&Options');

  { Phase 27: Recording menu }
  mnuToolsRecord.Caption := Locale.Menu('ToolsRecord', '&Record Stream...');
  mnuToolsStopRecord.Caption := Locale.Menu('ToolsStopRecord', 'S&top Recording');

  { Help menu }
  mnuHelp.Caption := Locale.Menu('Help', '&Help');
  mnuHelpMediaInfo.Caption := Locale.Menu('HelpMediaInfo', '&Media Info');
  mnuHelpLog.Caption := Locale.Menu('HelpLog', '&Log Window');  { Phase 25 }
  mnuHelpAbout.Caption := Locale.Menu('HelpAbout', '&About');

  { Status bar }
  if FMPVEngine = nil then
    StatusBar.SimpleText := Locale.Status('Ready', 'Ready')
  else
    case FMPVEngine.Status of
      msNone: StatusBar.SimpleText := Locale.Status('Ready', 'Ready');
      msOpening: StatusBar.SimpleText := Locale.Status('Opening', 'Opening...');
      msPlaying: StatusBar.SimpleText := Locale.Status('Playing', 'Playing');
      msPaused: StatusBar.SimpleText := Locale.Status('Paused', 'Paused');
      msStopped: StatusBar.SimpleText := Locale.Status('Stopped', 'Stopped');
      msError: StatusBar.SimpleText := Locale.Status('Error', 'Error');
    end;

  { Button tooltips }
  btnPlay.Hint := Locale.Tooltip('Play', 'Play/Pause');
  btnStop.Hint := Locale.Tooltip('Stop', 'Stop playback');
  btnPrevious.Hint := Locale.Tooltip('Previous', 'Previous track');
  btnNext.Hint := Locale.Tooltip('Next', 'Next track');
  btnMute.Hint := Locale.Tooltip('Mute', 'Toggle mute');
  btnFullscreen.Hint := Locale.Tooltip('Fullscreen', 'Toggle fullscreen');

  { DVD Navigation menu }
  mnuPlaybackDVD.Caption := Locale.Menu('PlaybackDVD', 'DVD &Navigation');
  mnuDVDMenu.Caption := Locale.Menu('DVDMenu', '&Menu');
  mnuDVDUp.Caption := Locale.Menu('DVDUp', '&Up');
  mnuDVDDown.Caption := Locale.Menu('DVDDown', '&Down');
  mnuDVDLeft.Caption := Locale.Menu('DVDLeft', '&Left');
  mnuDVDRight.Caption := Locale.Menu('DVDRight', '&Right');
  mnuDVDSelect.Caption := Locale.Menu('DVDSelect', '&Select');
  mnuPlaybackDVDTitles.Caption := Locale.Menu('PlaybackDVDTitles', 'DVD &Titles');

  { File menu DVD/Bluray }
  mnuFileOpenDVD.Caption := Locale.Menu('FileOpenDVD', 'Open &DVD...');
  mnuFileOpenBluray.Caption := Locale.Menu('FileOpenBluray', 'Open &Blu-ray...');

  { Phase 20: Video Context Menu }
  mnuCtxPlayPause.Caption := Locale.Menu('PlaybackPlayPause', '&Play/Pause');
  mnuCtxStop.Caption := Locale.Menu('PlaybackStop', '&Stop');
  mnuCtxPrevious.Caption := Locale.Menu('PlaybackPrevious', 'Pre&vious');
  mnuCtxNext.Caption := Locale.Menu('PlaybackNext', '&Next');
  mnuCtxFullscreen.Caption := Locale.Menu('VideoFullscreen', '&Fullscreen');
  mnuCtxAlwaysOnTop.Caption := Locale.Menu('ViewAlwaysOnTop', '&Always on Top');
  mnuCtxAspect.Caption := Locale.Menu('VideoAspect', 'Aspect &Ratio');
  mnuCtxAspectAuto.Caption := Locale.Menu('VideoAspectAuto', '&Auto');
  mnuCtxAspect169.Caption := Locale.Menu('VideoAspect169', '&16:9');
  mnuCtxAspect43.Caption := Locale.Menu('VideoAspect43', '&4:3');
  mnuCtxAspect235.Caption := Locale.Menu('VideoAspect235', '&2.35:1');
  mnuCtxRotation.Caption := Locale.Menu('VideoRotation', 'R&otation');
  mnuCtxRotate0.Caption := Locale.Menu('VideoRotate0', '&0° (Normal)');
  mnuCtxRotate90.Caption := Locale.Menu('VideoRotate90', '&90°');
  mnuCtxRotate180.Caption := Locale.Menu('VideoRotate180', '&180°');
  mnuCtxRotate270.Caption := Locale.Menu('VideoRotate270', '&270°');
  mnuCtxAudioTrack.Caption := Locale.Menu('AudioTrack', 'A&udio Track');
  mnuCtxSubtitleTrack.Caption := Locale.Menu('SubtitleTrack', 'S&ubtitle Track');
  mnuCtxMute.Caption := Locale.Menu('AudioMute', '&Mute');
  mnuCtxScreenshot.Caption := Locale.Menu('VideoScreenshot', 'Sc&reenshot');
  mnuCtxMediaInfo.Caption := Locale.Menu('HelpMediaInfo', 'Media &Info');

  { Tray popup menu }
  mnuTrayShow.Caption := Locale.Menu('TrayShow', '&Show/Hide');
  mnuTrayPlayPause.Caption := Locale.Menu('TrayPlayPause', '&Play/Pause');
  mnuTrayStop.Caption := Locale.Menu('TrayStop', 'S&top');
  mnuTrayPrevious.Caption := Locale.Menu('TrayPrevious', 'Pre&vious');
  mnuTrayNext.Caption := Locale.Menu('TrayNext', '&Next');
  mnuTrayMute.Caption := Locale.Menu('TrayMute', '&Mute');
  mnuTrayExit.Caption := Locale.Menu('TrayExit', 'E&xit');

  { Apply translations to all child forms }
  if FPlaylistForm <> nil then
    FPlaylistForm.ApplyLocale;
  if FRadiosForm <> nil then
    FRadiosForm.ApplyLocale;
  if FEqualizerForm <> nil then
    FEqualizerForm.ApplyLocale;
  if FOptionsForm <> nil then
    FOptionsForm.ApplyLocale;
  if FMediaInfoForm <> nil then
    FMediaInfoForm.ApplyLocale;
  if FAboutForm <> nil then
    FAboutForm.ApplyLocale;
  if Assigned(frmLog) then
    frmLog.ApplyLocale;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PHASE 12: DVD/BLURAY SUPPORT
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.mnuFileOpenDVDClick(Sender: TObject);
begin
  OpenDVD;
end;

procedure TfrmMain.mnuFileOpenBlurayClick(Sender: TObject);
begin
  OpenBluray;
end;

procedure TfrmMain.mnuDVDMenuClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
    FMPVEngine.DVDGoMenu;
end;

procedure TfrmMain.mnuDVDUpClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
    FMPVEngine.DVDMenuUp;
end;

procedure TfrmMain.mnuDVDDownClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
    FMPVEngine.DVDMenuDown;
end;

procedure TfrmMain.mnuDVDLeftClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
    FMPVEngine.DVDMenuLeft;
end;

procedure TfrmMain.mnuDVDRightClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
    FMPVEngine.DVDMenuRight;
end;

procedure TfrmMain.mnuDVDSelectClick(Sender: TObject);
begin
  if FMPVEngine <> nil then
    FMPVEngine.DVDMenuSelect;
end;

procedure TfrmMain.OpenDVD;
var
  DVDPath: string;
begin
  { Open DVD dialog to select DVD drive or folder }
  OpenDialog.Title := Locale.Dialog('OpenDVD', 'Open DVD');
  OpenDialog.Filter := Locale.Dialog('DVDFilter', 'DVD Folder|VIDEO_TS|All Files|*.*');
  OpenDialog.FileName := '';

  if OpenDialog.Execute then
  begin
    DVDPath := OpenDialog.FileName;
    { If VIDEO_TS folder was selected, use its parent }
    if ExtractFileName(DVDPath) = 'VIDEO_TS' then
      DVDPath := ExtractFileDir(DVDPath);

    { Play DVD using dvdnav:// protocol }
    PlayFile('dvdnav://' + DVDPath);
    StatusBar.SimpleText := Format(Locale.Message('PlayingDVD', 'Playing DVD: %s'), [ExtractFileName(DVDPath)]);
  end;
end;

procedure TfrmMain.OpenBluray;
var
  BDPath: string;
begin
  { Open Bluray dialog to select Bluray folder }
  OpenDialog.Title := Locale.Dialog('OpenBluray', 'Open Blu-ray');
  OpenDialog.Filter := Locale.Dialog('BlurayFilter', 'Bluray Folder|BDMV|All Files|*.*');
  OpenDialog.FileName := '';

  if OpenDialog.Execute then
  begin
    BDPath := OpenDialog.FileName;
    { If BDMV folder was selected, use its parent }
    if ExtractFileName(BDPath) = 'BDMV' then
      BDPath := ExtractFileDir(BDPath);

    { Play Bluray using bd:// protocol }
    PlayFile('bd://' + BDPath);
    StatusBar.SimpleText := Format(Locale.Message('PlayingBluray', 'Playing Blu-ray: %s'), [ExtractFileName(BDPath)]);
  end;
end;

procedure TfrmMain.UpdateDVDTitlesMenu;
var
  i, TitleCount: Integer;
  MenuItem: TMenuItem;
begin
  { Clear existing items }
  mnuPlaybackDVDTitles.Clear;

  if (FMPVEngine = nil) or not FMPVEngine.IsDVD then
  begin
    { Add disabled placeholder }
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := Locale.Dialog('NoDVD', '(No DVD loaded)');
    MenuItem.Enabled := False;
    mnuPlaybackDVDTitles.Add(MenuItem);
    Exit;
  end;

  { Get DVD title count from MPV }
  TitleCount := FMPVEngine.GetPropertyInt('disc-titles');
  if TitleCount <= 0 then
    TitleCount := 10; { Default to 10 titles if count unavailable }

  for i := 1 to TitleCount do
  begin
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := Format(Locale.Dialog('DVDTitle', 'Title %d'), [i]);
    MenuItem.Tag := i;
    MenuItem.OnClick := @OnDVDTitleClick;
    mnuPlaybackDVDTitles.Add(MenuItem);
  end;
end;

procedure TfrmMain.OnDVDTitleClick(Sender: TObject);
var
  TitleID: Integer;
begin
  if not (Sender is TMenuItem) then Exit;
  if FMPVEngine = nil then Exit;

  TitleID := TMenuItem(Sender).Tag;
  FMPVEngine.DVDTitle(TitleID);
  StatusBar.SimpleText := Format(Locale.Message('DVDTitlePlaying', 'DVD Title %d'), [TitleID]);
end;

procedure TfrmMain.UpdateDVDMenuState;
var
  IsDVD: Boolean;
begin
  IsDVD := (FMPVEngine <> nil) and FMPVEngine.IsDVD;

  { Enable/disable DVD navigation menu items based on whether DVD is playing }
  mnuPlaybackDVD.Enabled := IsDVD;
  mnuPlaybackDVDTitles.Enabled := IsDVD;

  if IsDVD then
    UpdateDVDTitlesMenu;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PHASE 13: GO TO TIME AND HISTORY
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.mnuPlaybackGotoTimeClick(Sender: TObject);
begin
  if FMPVEngine = nil then Exit;

  if FGotoTimeForm = nil then
    FGotoTimeForm := TfrmGotoTime.Create(Self);

  FGotoTimeForm.CurrentPosition := FMPVEngine.Position;
  FGotoTimeForm.Duration := FMPVEngine.Duration;

  if FGotoTimeForm.ShowModal = mrOK then
  begin
    FMPVEngine.SeekAbsolute(FGotoTimeForm.TargetPosition);
    StatusBar.SimpleText := Format(Locale.Message('SeekTo', 'Seeking to %s'),
      [FormatTime(FGotoTimeForm.TargetPosition)]);
  end;
end;

procedure TfrmMain.mnuToolsHistoryClick(Sender: TObject);
begin
  if FHistoryForm = nil then
  begin
    FHistoryForm := TfrmHistory.Create(Self);
    FHistoryForm.OnPlay := @OnHistoryPlay;
  end;

  FHistoryForm.RefreshHistory;
  FHistoryForm.Show;
  FHistoryForm.BringToFront;
end;

procedure TfrmMain.OnHistoryPlay(Sender: TObject; const FileName: string);
begin
  PlayFile(FileName);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PHASE 15: BOOKMARKS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.mnuPlaybackAddBookmarkClick(Sender: TObject);
var
  BookmarkName: string;
  CurrentPos: Double;
begin
  if FMPVEngine = nil then Exit;
  if FCurrentFileName = '' then Exit;

  CurrentPos := FMPVEngine.Position;
  BookmarkName := Format(Locale.Message('BookmarkDefaultName', 'Bookmark at %s'), [FormatTime(CurrentPos)]);

  { Ask for bookmark name }
  if InputQuery(Locale.Menu('AddBookmarkTitle', 'Add Bookmark'),
                Locale.Menu('AddBookmarkPrompt', 'Enter bookmark name:'), BookmarkName) then
  begin
    Config.AddBookmark(FCurrentFileName, BookmarkName, CurrentPos);
    StatusBar.SimpleText := Format(Locale.Menu('BookmarkAdded', 'Bookmark added at %s'), [FormatTime(CurrentPos)]);
  end;
end;

procedure TfrmMain.mnuPlaybackBookmarksClick(Sender: TObject);
begin
  if FBookmarksForm = nil then
  begin
    FBookmarksForm := TfrmBookmarks.Create(Self);
    FBookmarksForm.OnGoto := @OnBookmarkGoto;
  end;

  { Show all bookmarks or only for current file }
  FBookmarksForm.ShowAllFiles := True;
  FBookmarksForm.CurrentFile := FCurrentFileName;
  FBookmarksForm.RefreshBookmarks;
  FBookmarksForm.Show;
  FBookmarksForm.BringToFront;
end;

procedure TfrmMain.OnBookmarkGoto(Sender: TObject; const FileName: string; BookmarkPos: Double);
begin
  { If it's a different file, load it first }
  if FileName <> FCurrentFileName then
    PlayFile(FileName);

  { Seek to the bookmark position }
  if FMPVEngine <> nil then
  begin
    FMPVEngine.SeekAbsolute(BookmarkPos);
    StatusBar.SimpleText := Format(Locale.Menu('BookmarkJumped', 'Jumped to bookmark at %s'), [FormatTime(BookmarkPos)]);
  end;
end;

procedure TfrmMain.UpdateBookmarksMenu;
var
  Bookmarks: TBookmarkItems;
  I: Integer;
  MenuItem: TMenuItem;
begin
  { Clear existing bookmark items (keep first two static items) }
  while mnuPlaybackBookmarks.Count > 0 do
    mnuPlaybackBookmarks.Delete(0);

  if FCurrentFileName = '' then
  begin
    MenuItem := TMenuItem.Create(mnuPlaybackBookmarks);
    MenuItem.Caption := Locale.Menu('NoFileLoaded', '(No file loaded)');
    MenuItem.Enabled := False;
    mnuPlaybackBookmarks.Add(MenuItem);
    Exit;
  end;

  { Get bookmarks for current file }
  Bookmarks := Config.GetBookmarksForFile(FCurrentFileName);

  if Length(Bookmarks) = 0 then
  begin
    MenuItem := TMenuItem.Create(mnuPlaybackBookmarks);
    MenuItem.Caption := Locale.Menu('NoBookmarks', '(No bookmarks)');
    MenuItem.Enabled := False;
    mnuPlaybackBookmarks.Add(MenuItem);
    Exit;
  end;

  { Add bookmark items }
  for I := 0 to High(Bookmarks) do
  begin
    MenuItem := TMenuItem.Create(mnuPlaybackBookmarks);
    MenuItem.Caption := Format('%s (%s)', [Bookmarks[I].Name, FormatTime(Bookmarks[I].Position)]);
    MenuItem.Tag := I;
    MenuItem.OnClick := @OnBookmarkMenuClick;
    mnuPlaybackBookmarks.Add(MenuItem);
  end;

  { Add separator and "Show All..." option }
  MenuItem := TMenuItem.Create(mnuPlaybackBookmarks);
  MenuItem.Caption := '-';
  mnuPlaybackBookmarks.Add(MenuItem);

  MenuItem := TMenuItem.Create(mnuPlaybackBookmarks);
  MenuItem.Caption := Locale.Menu('ShowAllBookmarks', 'Show All Bookmarks...');
  MenuItem.OnClick := @mnuPlaybackBookmarksClick;
  mnuPlaybackBookmarks.Add(MenuItem);
end;

procedure TfrmMain.OnBookmarkMenuClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  Bookmarks: TBookmarkItems;
  Idx: Integer;
begin
  if not (Sender is TMenuItem) then Exit;

  MenuItem := TMenuItem(Sender);
  Idx := MenuItem.Tag;

  Bookmarks := Config.GetBookmarksForFile(FCurrentFileName);
  if (Idx >= 0) and (Idx <= High(Bookmarks)) then
  begin
    if FMPVEngine <> nil then
    begin
      FMPVEngine.SeekAbsolute(Bookmarks[Idx].Position);
      StatusBar.SimpleText := Format(Locale.Message('JumpedToNamed', 'Jumped to: %s'), [Bookmarks[Idx].Name]);
    end;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PHASE 16: FAVORITES
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.mnuToolsFavoritesClick(Sender: TObject);
begin
  if FFavoritesForm = nil then
  begin
    FFavoritesForm := TfrmFavorites.Create(Self);
    FFavoritesForm.OnPlay := @OnFavoritePlay;
  end;

  FFavoritesForm.RefreshFavorites;
  FFavoritesForm.Show;
  FFavoritesForm.BringToFront;
end;

procedure TfrmMain.mnuToolsAddFavoriteClick(Sender: TObject);
var
  FavName: string;
  FavType: TFavoriteType;
begin
  if FCurrentFileName = '' then
  begin
    StatusBar.SimpleText := Locale.Menu('NoFileToFavorite', 'No file loaded to add to favorites');
    Exit;
  end;

  { Get favorite type based on current media }
  FavType := GetCurrentFavoriteType;

  { Default name }
  if FavType = ftURL then
    FavName := FCurrentFileName
  else
    FavName := ExtractFileName(FCurrentFileName);

  { Check if already a favorite }
  if Config.IsFavorite(FCurrentFileName) then
  begin
    StatusBar.SimpleText := Locale.Menu('AlreadyFavorite', 'Already in favorites');
    Exit;
  end;

  { Ask for name }
  if InputQuery(Locale.Menu('AddFavoriteTitle', 'Add to Favorites'),
                Locale.Menu('AddFavoritePrompt', 'Enter name for favorite:'), FavName) then
  begin
    Config.AddFavorite(FavName, FCurrentFileName, FavType);
    StatusBar.SimpleText := Format(Locale.Menu('AddedToFavorites', 'Added to favorites: %s'), [FavName]);
  end;
end;

procedure TfrmMain.OnFavoritePlay(Sender: TObject; const Path: string);
begin
  PlayFile(Path);
end;

function TfrmMain.GetCurrentFavoriteType: TFavoriteType;
begin
  Result := ftFile;

  if FCurrentFileName = '' then Exit;

  { Check if it's a URL }
  if (Pos('http://', LowerCase(FCurrentFileName)) = 1) or
     (Pos('https://', LowerCase(FCurrentFileName)) = 1) or
     (Pos('rtsp://', LowerCase(FCurrentFileName)) = 1) or
     (Pos('rtmp://', LowerCase(FCurrentFileName)) = 1) then
    Result := ftURL
  { Check if it's a DVD }
  else if (Pos('dvd://', LowerCase(FCurrentFileName)) = 1) or
          (Pos('/dev/sr', FCurrentFileName) > 0) or
          (Pos('/dev/dvd', FCurrentFileName) > 0) then
    Result := ftDVD
  { Check if it's a Blu-ray }
  else if (Pos('bd://', LowerCase(FCurrentFileName)) = 1) or
          (Pos('bluray://', LowerCase(FCurrentFileName)) = 1) then
    Result := ftBluray
  else
    Result := ftFile;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PHASE 14: SYSTEM TRAY
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  { Phase 24: Save session playlist before closing }
  SaveSessionPlaylist;

  { If force close is set, allow closing }
  if FForceClose then
  begin
    CanClose := True;
    Exit;
  end;

  { Always allow closing - tray icon is informational only }
  CanClose := True;
end;

procedure TfrmMain.InitializeTrayIcon;
begin
  { Assign application icon to tray icon }
  TrayIcon.Icon.Assign(Application.Icon);
  TrayIcon.Visible := True;
  TrayIcon.Hint := APP_NAME;
  UpdateTrayIconHint;
end;

procedure TfrmMain.UpdateTrayIconHint;
var
  HintText: string;
begin
  HintText := APP_NAME;

  if (FMPVEngine <> nil) and (FMPVEngine.Status = msPlaying) then
  begin
    if FCurrentFileName <> '' then
      HintText := ExtractFileName(FCurrentFileName) + ' - ' + APP_NAME;
  end;

  TrayIcon.Hint := HintText;
end;

procedure TfrmMain.ShowMainWindow;
begin
  Show;
  WindowState := wsNormal;
  BringToFront;
  Application.BringToFront;
end;

procedure TfrmMain.HideToTray;
begin
  FClosingToTray := True;
  try
    Hide;
    { Show balloon notification on hide }
    if TrayIcon.Visible then
    begin
      TrayIcon.BalloonHint := Locale.Message('MinimizedToTray', 'Minimized to tray. Double-click to restore.');
      TrayIcon.BalloonTitle := APP_NAME;
      TrayIcon.ShowBalloonHint;
    end;
  finally
    FClosingToTray := False;
  end;
end;

procedure TfrmMain.TrayIconClick(Sender: TObject);
begin
  { Single click - do nothing (popup menu will show) }
end;

procedure TfrmMain.TrayIconDblClick(Sender: TObject);
begin
  { Double click - show/hide main window }
  if Visible then
    HideToTray
  else
    ShowMainWindow;
end;

procedure TfrmMain.mnuTrayShowClick(Sender: TObject);
begin
  if Visible then
    HideToTray
  else
    ShowMainWindow;
end;

procedure TfrmMain.mnuTrayPlayPauseClick(Sender: TObject);
begin
  mnuPlaybackPlayPauseClick(nil);
end;

procedure TfrmMain.mnuTrayStopClick(Sender: TObject);
begin
  mnuPlaybackStopClick(nil);
end;

procedure TfrmMain.mnuTrayPreviousClick(Sender: TObject);
begin
  btnPreviousClick(nil);
end;

procedure TfrmMain.mnuTrayNextClick(Sender: TObject);
begin
  btnNextClick(nil);
end;

procedure TfrmMain.mnuTrayMuteClick(Sender: TObject);
begin
  mnuAudioMuteClick(nil);
end;

procedure TfrmMain.mnuTrayExitClick(Sender: TObject);
begin
  { Force close the application }
  FForceClose := True;
  Close;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Phase 17: Sleep Timer
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.mnuToolsSleepTimerClick(Sender: TObject);
var
  SleepTimerForm: TfrmSleepTimer;
begin
  SleepTimerForm := TfrmSleepTimer.Create(Self);
  try
    if SleepTimerForm.ShowModal = mrOK then
    begin
      { Set up the sleep timer }
      FSleepTimerAction := SleepTimerForm.TimerAction;
      FSleepTimerRemaining := SleepTimerForm.TimerMinutes * 60; { Convert to seconds }

      { Enable the timer (fires every second) }
      SleepTimer.Interval := 1000;
      SleepTimer.Enabled := True;

      { Update menu state }
      mnuToolsCancelTimer.Enabled := True;

      { Update status display }
      UpdateSleepTimerDisplay;

      StatusBar.SimpleText := Format(Locale.GetString('Message', 'SleepTimerSet', 'Sleep timer set: %s'),
        [FormatTime(FSleepTimerRemaining)]);
    end;
  finally
    SleepTimerForm.Free;
  end;
end;

procedure TfrmMain.mnuToolsCancelTimerClick(Sender: TObject);
begin
  CancelSleepTimer;
  StatusBar.SimpleText := Locale.GetString('Message', 'SleepTimerCancelled', 'Sleep timer cancelled');
end;

procedure TfrmMain.SleepTimerTimer(Sender: TObject);
begin
  if FSleepTimerRemaining > 0 then
  begin
    Dec(FSleepTimerRemaining);
    UpdateSleepTimerDisplay;

    { Show warning at 1 minute remaining }
    if FSleepTimerRemaining = 60 then
      StatusBar.SimpleText := Locale.GetString('Message', 'SleepTimerWarning', 'Sleep timer: 1 minute remaining');

    { Show warning at 10 seconds remaining }
    if FSleepTimerRemaining = 10 then
      StatusBar.SimpleText := Locale.GetString('Message', 'SleepTimerFinal', 'Sleep timer: 10 seconds remaining');
  end
  else
  begin
    { Timer expired - execute the action }
    SleepTimer.Enabled := False;
    ExecuteSleepTimerAction;
  end;
end;

procedure TfrmMain.UpdateSleepTimerDisplay;
var
  H, M, S: Integer;
  TimeStr: string;
begin
  if FSleepTimerRemaining > 0 then
  begin
    H := FSleepTimerRemaining div 3600;
    M := (FSleepTimerRemaining mod 3600) div 60;
    S := FSleepTimerRemaining mod 60;

    if H > 0 then
      TimeStr := Format('%d:%02d:%02d', [H, M, S])
    else
      TimeStr := Format('%d:%02d', [M, S]);

    { Update menu caption with remaining time }
    mnuToolsSleepTimer.Caption := Format(Locale.GetString('Menu', 'ToolsSleepTimerActive', '&Sleep Timer [%s]'), [TimeStr]);
  end
  else
  begin
    { Reset menu caption }
    mnuToolsSleepTimer.Caption := Locale.GetString('Menu', 'ToolsSleepTimer', '&Sleep Timer...');
  end;
end;

procedure TfrmMain.CancelSleepTimer;
begin
  SleepTimer.Enabled := False;
  FSleepTimerRemaining := 0;
  mnuToolsCancelTimer.Enabled := False;
  UpdateSleepTimerDisplay;
end;

procedure TfrmMain.ExecuteSleepTimerAction;
begin
  { Reset timer state }
  FSleepTimerRemaining := 0;
  mnuToolsCancelTimer.Enabled := False;
  UpdateSleepTimerDisplay;

  case FSleepTimerAction of
    staStopPlayback:
      begin
        { Stop playback }
        if Assigned(FMPVEngine) then
          FMPVEngine.Stop;
        StatusBar.SimpleText := Locale.GetString('Message', 'SleepTimerStopped', 'Sleep timer: Playback stopped');
      end;

    staCloseApp:
      begin
        { Close the application }
        StatusBar.SimpleText := Locale.GetString('Message', 'SleepTimerClosing', 'Sleep timer: Closing application');
        FForceClose := True;
        Close;
      end;

    staShutdown:
      begin
        { Shutdown the computer }
        StatusBar.SimpleText := Locale.GetString('Message', 'SleepTimerShutdown', 'Sleep timer: Shutting down computer');
        {$IFDEF WINDOWS}
        { Windows shutdown }
        ShellExecute(0, 'open', 'shutdown', '/s /t 60', nil, 0);
        {$ELSE}
        { Linux/Unix shutdown (requires appropriate permissions) }
        { Use fpSystem to call shutdown - user may need to configure sudoers }
        { For safety, we'll just close the app and show a message }
        if FMPVEngine <> nil then
          FMPVEngine.Stop;
        Application.MessageBox(
          PChar(Locale.GetString('Message', 'ShutdownManual', 'Please shutdown your computer manually.')),
          PChar(Locale.GetString('Message', 'SleepTimerExpired', 'Sleep Timer Expired')),
          MB_OK + MB_ICONINFORMATION);
        FForceClose := True;
        Close;
        {$ENDIF}
      end;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Phase 18: Keyboard Shortcuts
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.mnuToolsShortcutsClick(Sender: TObject);
var
  ShortcutsEditor: TfrmShortcutsEditor;
begin
  ShortcutsEditor := TfrmShortcutsEditor.Create(Self);
  try
    ShortcutsEditor.ShowModal;
  finally
    ShortcutsEditor.Free;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PHASE 20: VIDEO CONTEXT MENU
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.VideoPopupMenuPopup(Sender: TObject);
var
  I: Integer;
  SrcItem, NewItem: TMenuItem;
begin
  { Sync checkmarks with current state }
  mnuCtxFullscreen.Checked := FFullscreen;
  mnuCtxAlwaysOnTop.Checked := FAlwaysOnTop;
  mnuCtxMute.Checked := (FMPVEngine <> nil) and FMPVEngine.Muted;

  { Sync aspect ratio checkmarks }
  mnuCtxAspectAuto.Checked := mnuVideoAspectAuto.Checked;
  mnuCtxAspect169.Checked := mnuVideoAspect169.Checked;
  mnuCtxAspect43.Checked := mnuVideoAspect43.Checked;
  mnuCtxAspect235.Checked := mnuVideoAspect235.Checked;

  { Sync rotation checkmarks }
  mnuCtxRotate0.Checked := (FVideoRotation = 0);
  mnuCtxRotate90.Checked := (FVideoRotation = 90);
  mnuCtxRotate180.Checked := (FVideoRotation = 180);
  mnuCtxRotate270.Checked := (FVideoRotation = 270);

  { Copy audio track menu items from main menu }
  mnuCtxAudioTrack.Clear;
  for I := 0 to mnuAudioTrack.Count - 1 do
  begin
    SrcItem := mnuAudioTrack.Items[I];
    NewItem := TMenuItem.Create(mnuCtxAudioTrack);
    NewItem.Caption := SrcItem.Caption;
    NewItem.Tag := SrcItem.Tag;
    NewItem.RadioItem := SrcItem.RadioItem;
    NewItem.Checked := SrcItem.Checked;
    NewItem.Enabled := SrcItem.Enabled;
    NewItem.OnClick := SrcItem.OnClick;
    mnuCtxAudioTrack.Add(NewItem);
  end;

  { Copy subtitle track menu items from main menu }
  mnuCtxSubtitleTrack.Clear;
  for I := 0 to mnuSubtitleTrack.Count - 1 do
  begin
    SrcItem := mnuSubtitleTrack.Items[I];
    NewItem := TMenuItem.Create(mnuCtxSubtitleTrack);
    NewItem.Caption := SrcItem.Caption;
    NewItem.Tag := SrcItem.Tag;
    NewItem.RadioItem := SrcItem.RadioItem;
    NewItem.Checked := SrcItem.Checked;
    NewItem.Enabled := SrcItem.Enabled;
    NewItem.OnClick := SrcItem.OnClick;
    mnuCtxSubtitleTrack.Add(NewItem);
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PHASE 21: OPEN FOLDER AND ENHANCED DRAG & DROP
  ═══════════════════════════════════════════════════════════════════════════════ }

function TfrmMain.IsMediaFile(const FileName: string): Boolean;
const
  MediaExtensions: array[0..31] of string = (
    '.mp4', '.mkv', '.avi', '.wmv', '.mov', '.flv', '.webm', '.m4v',
    '.mpg', '.mpeg', '.3gp', '.ts', '.m2ts', '.vob', '.ogv', '.divx',
    '.mp3', '.flac', '.wav', '.ogg', '.m4a', '.aac', '.wma', '.opus',
    '.ape', '.mka', '.ac3', '.dts', '.aiff', '.mid', '.wv', '.ra'
  );
var
  Ext: string;
  I: Integer;
begin
  Result := False;
  Ext := LowerCase(ExtractFileExt(FileName));
  for I := Low(MediaExtensions) to High(MediaExtensions) do
  begin
    if Ext = MediaExtensions[I] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TfrmMain.GetCacheSizeForMedia(const FileName: string): Integer;
var
  LowerName: string;
begin
  LowerName := LowerCase(FileName);

  { DVD }
  if Pos('dvd://', LowerName) = 1 then
    Result := Config.Settings.Cache.DVDSize
  { CD-ROM / Audio CD }
  else if (Pos('cdda://', LowerName) = 1) or (Pos('cd://', LowerName) = 1) then
    Result := Config.Settings.Cache.CDROMSize
  { Internet streams (HTTP/HTTPS/RTSP/RTMP) }
  else if (Pos('http://', LowerName) = 1) or (Pos('https://', LowerName) = 1) or
          (Pos('rtsp://', LowerName) = 1) or (Pos('rtmp://', LowerName) = 1) then
    Result := Config.Settings.Cache.InternetSize
  { Network files (SMB/NFS/FTP) }
  else if (Pos('smb://', LowerName) = 1) or (Pos('nfs://', LowerName) = 1) or
          (Pos('ftp://', LowerName) = 1) then
    Result := Config.Settings.Cache.NetworkSize
  { Local files }
  else if FileExists(FileName) then
    Result := Config.Settings.Cache.FixedSize
  { Default }
  else
    Result := Config.Settings.Cache.DefaultSize;
end;

procedure TfrmMain.ScanFolderForMedia(const FolderPath: string; FileList: TStrings; Recursive: Boolean);
var
  SearchRec: TSearchRec;
  FullPath: string;
begin
  if not DirectoryExists(FolderPath) then Exit;

  { Scan for files }
  if FindFirst(IncludeTrailingPathDelimiter(FolderPath) + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
          Continue;

        FullPath := IncludeTrailingPathDelimiter(FolderPath) + SearchRec.Name;

        if (SearchRec.Attr and faDirectory) <> 0 then
        begin
          { Recursively scan subdirectories }
          if Recursive then
            ScanFolderForMedia(FullPath, FileList, True);
        end
        else
        begin
          { Check if it's a media file }
          if IsMediaFile(SearchRec.Name) then
            FileList.Add(FullPath);
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

procedure TfrmMain.AddFilesToPlaylist(const FileNames: array of string; ClearFirst: Boolean);
var
  I: Integer;
begin
  if FPlaylistManager = nil then Exit;

  if ClearFirst then
    FPlaylistManager.Clear;

  for I := Low(FileNames) to High(FileNames) do
  begin
    if IsMediaFile(FileNames[I]) or (Pos('://', FileNames[I]) > 0) then
      FPlaylistManager.Add(FileNames[I]);
  end;

  { Play first file if playlist was cleared or if nothing is playing }
  if ClearFirst and (FPlaylistManager.Count > 0) then
  begin
    FPlaylistManager.CurrentIndex := 0;
    PlayFile(FPlaylistManager.Items[0].FileName);
  end
  else if (FMPVEngine = nil) or (FMPVEngine.Status = msNone) or (FMPVEngine.Status = msStopped) then
  begin
    if FPlaylistManager.Count > 0 then
    begin
      FPlaylistManager.CurrentIndex := 0;
      PlayFile(FPlaylistManager.Items[0].FileName);
    end;
  end;
end;

procedure TfrmMain.OpenFolder;
var
  FolderPath: string;
  FileList: TStringList;
  I: Integer;
  FileArray: array of string;
begin
  FolderPath := '';
  if SelectDirectory(Locale.Dialog('OpenFolder', 'Open Folder'), '', FolderPath) then
  begin
    FileList := TStringList.Create;
    try
      { Scan folder for media files (recursive) }
      ScanFolderForMedia(FolderPath, FileList, True);

      { Sort files alphabetically }
      FileList.Sort;

      if FileList.Count > 0 then
      begin
        { Convert to array for AddFilesToPlaylist }
        SetLength(FileArray, FileList.Count);
        for I := 0 to FileList.Count - 1 do
          FileArray[I] := FileList[I];

        { Add to playlist and play first file }
        AddFilesToPlaylist(FileArray, True);

        { Show message }
        StatusBar.SimpleText := Format(Locale.Message('FolderOpened', 'Opened folder: %d files'), [FileList.Count]);
      end
      else
      begin
        StatusBar.SimpleText := Locale.Message('NoMediaFound', 'No media files found');
      end;
    finally
      FileList.Free;
    end;
  end;
end;

procedure TfrmMain.mnuFileOpenFolderClick(Sender: TObject);
begin
  OpenFolder;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PHASE 24: SESSION PLAYLIST SAVE/RESTORE
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.SaveSessionPlaylist;
var
  Items: TPlaylistItems;
  I: Integer;
  CurrentPosition: Double;
begin
  if not Config.Settings.General.AutoSavePlaylist then Exit;
  if FPlaylistManager = nil then Exit;

  { If playlist is empty, clear the session file }
  if FPlaylistManager.Count = 0 then
  begin
    Config.ClearSessionPlaylist;
    Exit;
  end;

  { Get playlist items }
  SetLength(Items, FPlaylistManager.Count);
  for I := 0 to FPlaylistManager.Count - 1 do
    Items[I] := FPlaylistManager.Items[I];

  { Get current playback position }
  if (FMPVEngine <> nil) and (FMPVEngine.Status = msPlaying) then
    CurrentPosition := FMPVEngine.Position
  else
    CurrentPosition := 0;

  { Save session }
  Config.SaveSessionPlaylist(Items, FPlaylistManager.CurrentIndex,
    FPlaylistManager.PlaybackMode, CurrentPosition);
end;

procedure TfrmMain.RestoreSessionPlaylist;
var
  Items: TPlaylistItems;
  CurrentIndex: Integer;
  PlaybackMode: TPlaybackMode;
  CurrentPosition: Double;
  I: Integer;
begin
  if not Config.Settings.General.AutoSavePlaylist then Exit;
  if FPlaylistManager = nil then Exit;

  { Load saved session }
  if not Config.LoadSessionPlaylist(Items, CurrentIndex, PlaybackMode, CurrentPosition) then
    Exit;

  if Length(Items) = 0 then Exit;

  { Restore playlist items }
  for I := 0 to High(Items) do
    FPlaylistManager.Add(Items[I]);

  { Restore playback mode }
  FPlaylistManager.PlaybackMode := PlaybackMode;

  { Restore current index and optionally resume playback }
  if (CurrentIndex >= 0) and (CurrentIndex < FPlaylistManager.Count) then
  begin
    FPlaylistManager.CurrentIndex := CurrentIndex;

    { Resume playback if we have a saved position }
    if CurrentPosition > 5 then
    begin
      { Play and seek to position }
      PlayFile(FPlaylistManager.Items[CurrentIndex].FileName);
      { The position will be restored by RestorePlaybackPosition called from OnMPVFileLoaded }
    end;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PHASE 26: VISUALIZATION
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.mnuVisNoneClick(Sender: TObject);
begin
  SetVisualizationMode(vmNone);
end;

procedure TfrmMain.mnuVisSpectrumClick(Sender: TObject);
begin
  SetVisualizationMode(vmSpectrum);
end;

procedure TfrmMain.mnuVisWaveformClick(Sender: TObject);
begin
  SetVisualizationMode(vmWaves);
end;

procedure TfrmMain.mnuVisVectorClick(Sender: TObject);
begin
  SetVisualizationMode(vmVector);
end;

procedure TfrmMain.mnuVisVolumeClick(Sender: TObject);
begin
  SetVisualizationMode(vmVolume);
end;

procedure TfrmMain.mnuVisNextModeClick(Sender: TObject);
begin
  if FVisualEffects = nil then Exit;
  FVisualEffects.NextMode;
  UpdateVisualizationMenu;
end;

procedure TfrmMain.mnuVisNextColorClick(Sender: TObject);
begin
  if FVisualEffects = nil then Exit;
  FVisualEffects.NextColorScheme;
end;

procedure TfrmMain.SetVisualizationMode(Mode: TVisualMode);
begin
  if FVisualEffects = nil then Exit;

  if Mode = vmNone then
  begin
    FVisualEffects.Enabled := False;
  end
  else
  begin
    FVisualEffects.Mode := Mode;
    FVisualEffects.Enabled := True;
  end;

  UpdateVisualizationMenu;

  { Apply visualization with proper flag management }
  FChangingVisualization := True;
  FIgnoreNextEndFile := True;
  ApplyVisualization;
  { Use timer to reset flag after MPV has processed the filter change }
  FVisualReapplyNeeded := False;  { Timer should just reset flag, not reapply }
  FVisualReapplyTimer.Enabled := False;
  FVisualReapplyTimer.Interval := 500;
  FVisualReapplyTimer.Enabled := True;
end;

procedure TfrmMain.ApplyVisualization;
var
  FilterStr, EqFilter: string;
  CurrentPos: Double;
  WasPlaying: Boolean;
begin
  if FMPVEngine = nil then Exit;
  if FVisualEffects = nil then Exit;

  { Set flag to ignore EOF events caused by filter change }
  FChangingVisualization := True;

  { Get the filter string for current mode }
  FilterStr := FVisualEffects.GetAudioOnlyFilter;

  { Apply to MPV using lavfi-complex }
  if FilterStr <> '' then
  begin
    { Integrate equalizer into the lavfi-complex chain if enabled }
    EqFilter := FMPVEngine.GetEqualizerFilterString;
    if EqFilter <> '' then
    begin
      { Insert equalizer before asplit: [aid1] eq [e]; [e] asplit... }
      FilterStr := StringReplace(FilterStr, '[aid1] asplit',
        '[aid1] ' + EqFilter + ' [aeq]; [aeq] asplit', []);
      { Also handle asplit=3 for combined mode }
      FilterStr := StringReplace(FilterStr, '[aid1] asplit=3',
        '[aid1] ' + EqFilter + ' [aeq]; [aeq] asplit=3', []);
    end;
    FMPVEngine.SetPropertyString('lavfi-complex', FilterStr);
  end
  else
  begin
    { Disable filter - save position first }
    CurrentPos := FMPVEngine.Position;
    WasPlaying := FMPVEngine.Status = msPlaying;

    { Clear lavfi-complex filter }
    FMPVEngine.SetPropertyString('lavfi-complex', '');

    { Reload the file to properly reinitialize audio pipeline }
    if (FCurrentFileName <> '') and WasPlaying then
    begin
      Application.ProcessMessages;
      Sleep(50);
      FMPVEngine.PlayMedia(FCurrentFileName);
      Application.ProcessMessages;
      Sleep(100);
      { Seek back to saved position }
      if CurrentPos > 1 then
        FMPVEngine.SeekAbsolute(CurrentPos);
    end;
  end;

  { Don't reset FChangingVisualization here - let the caller handle it }
  { This prevents race conditions with async MPV events }
  Application.ProcessMessages;
end;

procedure TfrmMain.UpdateVisualizationMenu;
begin
  if FVisualEffects = nil then Exit;

  mnuVisNone.Checked := not FVisualEffects.Enabled or (FVisualEffects.Mode = vmNone);
  mnuVisSpectrum.Checked := FVisualEffects.Enabled and (FVisualEffects.Mode = vmSpectrum);
  mnuVisWaveform.Checked := FVisualEffects.Enabled and (FVisualEffects.Mode = vmWaves);
  mnuVisVector.Checked := FVisualEffects.Enabled and (FVisualEffects.Mode = vmVector);
  mnuVisVolume.Checked := FVisualEffects.Enabled and (FVisualEffects.Mode = vmVolume);
end;

procedure TfrmMain.UpdateVisualizationEnabled;
var
  HasRealVideo: Boolean;
  I, TrackCount: Integer;
  TrackType: string;
  IsAlbumArt: Boolean;
begin
  { Default: disable visualization menu when nothing is loaded }
  if (FMPVEngine = nil) or (FMPVEngine.Status in [msNone, msStopped, msError]) then
  begin
    mnuViewVisualization.Enabled := False;
    Exit;
  end;

  { Check if media has real video tracks (not album art) }
  HasRealVideo := False;
  TrackCount := FMPVEngine.GetPropertyInt('track-list/count');
  for I := 0 to TrackCount - 1 do
  begin
    TrackType := FMPVEngine.GetPropertyString('track-list/' + IntToStr(I) + '/type');
    if TrackType = 'video' then
    begin
      { Check if this is album art (attached picture) }
      IsAlbumArt := FMPVEngine.GetPropertyBool('track-list/' + IntToStr(I) + '/albumart');
      if not IsAlbumArt then
      begin
        HasRealVideo := True;
        Break;
      end;
    end;
  end;

  { Visualization is only for audio-only content (no real video) }
  mnuViewVisualization.Enabled := not HasRealVideo;

  { If visualization was enabled but now we have video, disable it }
  if HasRealVideo and (FVisualEffects <> nil) and FVisualEffects.Enabled then
  begin
    SetVisualizationMode(vmNone);
  end;
end;

procedure TfrmMain.OnVisFilterChanged(Sender: TObject);
begin
  FChangingVisualization := True;
  FIgnoreNextEndFile := True;
  ApplyVisualization;
  { Use timer to reset flag after MPV has processed the filter change }
  FVisualReapplyNeeded := False;  { Timer should just reset flag, not reapply }
  FVisualReapplyTimer.Enabled := False;
  FVisualReapplyTimer.Interval := 500;
  FVisualReapplyTimer.Enabled := True;
end;

procedure TfrmMain.OnEqualizerChange(Sender: TObject);
begin
  { Re-apply visualization to include updated equalizer settings in lavfi-complex }
  if (FVisualEffects <> nil) and FVisualEffects.Enabled and (FVisualEffects.Mode <> vmNone) then
  begin
    FChangingVisualization := True;
    FIgnoreNextEndFile := True;
    ApplyVisualization;
    { Use timer to reset flag after MPV has processed the filter change }
    FVisualReapplyNeeded := False;  { Timer should just reset flag, not reapply }
    FVisualReapplyTimer.Enabled := False;
    FVisualReapplyTimer.Interval := 500;
    FVisualReapplyTimer.Enabled := True;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PHASE 27: STREAM RECORDING
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.mnuToolsRecordClick(Sender: TObject);
begin
  StartStreamRecording;
end;

procedure TfrmMain.mnuToolsStopRecordClick(Sender: TObject);
begin
  StopStreamRecording;
end;

procedure TfrmMain.StartStreamRecording;
var
  SourceURL, SourceName: string;
  IsAudioOnly: Boolean;
begin
  if FStreamRecorder = nil then Exit;
  if FMPVEngine = nil then Exit;

  { Check if something is playing }
  if FMPVEngine.Status <> msPlaying then
  begin
    ShowMessage(Locale.GetString('Message', 'RecordNeedPlayback', 'Start playback before recording.'));
    Exit;
  end;

  { Get source info }
  SourceURL := FCurrentFileName;
  if FMPVEngine.StreamInfo.IsRadio then
    SourceName := FMPVEngine.StreamInfo.Metadata[0].Value  { Station name }
  else
    SourceName := ExtractFileName(SourceURL);

  { More robust video detection for HLS streams }
  IsAudioOnly := not FMPVEngine.StreamInfo.HasVideo and
                 (FMPVEngine.StreamInfo.Video.Width = 0) and
                 (FMPVEngine.StreamInfo.Video.Height = 0);

  { Start recording }
  if FStreamRecorder.StartRecording(SourceURL, SourceName, IsAudioOnly) then
  begin
    { Apply to MPV }
    FMPVEngine.SetPropertyString('stream-record', FStreamRecorder.GetMPVRecordPath);
    UpdateRecordingMenu;
    StatusBar.Panels[0].Text := Locale.GetString('Message', 'RecordingStarted', 'Recording started');
  end
  else
  begin
    ShowMessage(FStreamRecorder.Info.ErrorMessage);
  end;
end;

procedure TfrmMain.StopStreamRecording;
begin
  if FStreamRecorder = nil then Exit;
  if FMPVEngine = nil then Exit;

  { Stop recording }
  FStreamRecorder.StopRecording;

  { Clear mpv stream-record }
  FMPVEngine.SetPropertyString('stream-record', '');

  UpdateRecordingMenu;
  StatusBar.Panels[0].Text := Locale.GetString('Message', 'RecordingStopped', 'Recording stopped');
end;

procedure TfrmMain.UpdateRecordingMenu;
var
  IsRecording: Boolean;
begin
  if FStreamRecorder = nil then Exit;

  IsRecording := FStreamRecorder.Info.State = rsRecording;
  mnuToolsRecord.Enabled := not IsRecording;
  mnuToolsStopRecord.Enabled := IsRecording;

  if IsRecording then
    mnuToolsRecord.Caption := Locale.Menu('ToolsRecording', '&Recording...')
  else
    mnuToolsRecord.Caption := Locale.Menu('ToolsRecord', '&Record Stream...');
end;

procedure TfrmMain.OnRecordingStateChange(Sender: TObject; OldState, NewState: TRecordingState);
begin
  UpdateRecordingMenu;
end;

procedure TfrmMain.OnRecordingProgress(Sender: TObject; Duration: TTime; FileSize: Int64);
var
  StatusText: string;
begin
  if FStreamRecorder = nil then Exit;
  if FStreamRecorder.Info.State <> rsRecording then Exit;

  { Update status bar with recording info }
  StatusText := Format('%s - %s (%s)',
    [Locale.GetString('Status', 'Recording', 'Recording'),
     FormatRecordingDuration(Duration),
     FormatRecordingSize(FileSize)]);

  StatusBar.Panels[0].Text := StatusText;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PLAYLIST DOCKING
  ═══════════════════════════════════════════════════════════════════════════════ }

const
  DOCK_SNAP_DISTANCE = 20;  { Pixels within which windows will snap/dock }

procedure TfrmMain.ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer; KeepBase: Boolean);
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);

  { Update docked windows when main form moves }
  if not FUpdatingDockedWindows then
    UpdateDockedWindows;
end;

procedure TfrmMain.UpdateDockedWindows;
begin
  if FUpdatingDockedWindows then Exit;
  if FFullscreen then Exit;  { Don't move playlist during fullscreen }
  if FPlaylistForm = nil then Exit;
  if not FPlaylistForm.Visible then Exit;
  if FPlaylistDockSide = dsNone then Exit;

  FUpdatingDockedWindows := True;
  try
    case FPlaylistDockSide of
      dsLeft:
        FPlaylistForm.SetBounds(
          Self.Left - FPlaylistForm.Width,
          Self.Top,  { Align tops }
          FPlaylistForm.Width,
          FPlaylistForm.Height
        );
      dsRight:
        FPlaylistForm.SetBounds(
          Self.Left + Self.Width,
          Self.Top,  { Align tops }
          FPlaylistForm.Width,
          FPlaylistForm.Height
        );
    end;
  finally
    FUpdatingDockedWindows := False;
  end;
end;

procedure TfrmMain.CheckPlaylistDocking;
var
  DistLeft, DistRight: Integer;
  ExpectedLeft: Integer;
begin
  if FPlaylistForm = nil then Exit;
  if not FPlaylistForm.Visible then Exit;
  if FUpdatingDockedWindows then Exit;

  { If already docked, only check for undocking (user dragged it away) }
  if FPlaylistDockSide <> dsNone then
  begin
    case FPlaylistDockSide of
      dsRight: ExpectedLeft := Self.Left + Self.Width;
      dsLeft: ExpectedLeft := Self.Left - FPlaylistForm.Width;
      else ExpectedLeft := FPlaylistForm.Left;
    end;

    { Only undock if playlist is significantly far from expected docked position }
    { This happens when user manually drags the playlist away }
    if Abs(FPlaylistForm.Left - ExpectedLeft) > DOCK_SNAP_DISTANCE * 3 then
      FPlaylistDockSide := dsNone;

    Exit;
  end;

  { Not docked - check if we should dock }
  DistRight := Abs(FPlaylistForm.Left - (Self.Left + Self.Width));
  DistLeft := Abs((FPlaylistForm.Left + FPlaylistForm.Width) - Self.Left);

  { Check for docking on right side }
  if DistRight <= DOCK_SNAP_DISTANCE then
  begin
    FPlaylistDockSide := dsRight;
    { Snap to exact position with aligned tops }
    FUpdatingDockedWindows := True;
    try
      FPlaylistForm.SetBounds(
        Self.Left + Self.Width,
        Self.Top,
        FPlaylistForm.Width,
        FPlaylistForm.Height
      );
    finally
      FUpdatingDockedWindows := False;
    end;
  end
  { Check for docking on left side }
  else if DistLeft <= DOCK_SNAP_DISTANCE then
  begin
    FPlaylistDockSide := dsLeft;
    { Snap to exact position with aligned tops }
    FUpdatingDockedWindows := True;
    try
      FPlaylistForm.SetBounds(
        Self.Left - FPlaylistForm.Width,
        Self.Top,
        FPlaylistForm.Width,
        FPlaylistForm.Height
      );
    finally
      FUpdatingDockedWindows := False;
    end;
  end;
end;

end.
