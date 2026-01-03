{ ═══════════════════════════════════════════════════════════════════════════════
  uMPVConst.pas - Constants for MPV Engine

  Part of 3nity Media - Lazarus Edition

  This unit defines constants used throughout the player, including
  cache types, property entries, source types, and observation IDs.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uMPVConst;

{$mode objfpc}{$H+}

interface

const
  { ═══════════════════════════════════════════════════════════════════════════
    SOURCE FILE PREFIXES (equivalent to mplayer.pas SRC_FILE_*)
    ═══════════════════════════════════════════════════════════════════════════ }
  SRC_FILE_DVDNAV = 'dvdnav://';
  SRC_FILE_DVD = 'dvd://';
  SRC_FILE_BLURAY = 'bd://';
  SRC_FILE_CDDA = 'cdda://';
  SRC_FILE_VCD = 'vcd://';
  SRC_FILE_TV = 'tv://';
  SRC_FILE_DVB = 'dvb://';
  SRC_FILE_HTTP = 'http://';
  SRC_FILE_HTTPS = 'https://';
  SRC_FILE_RTSP = 'rtsp://';
  SRC_FILE_RTMP = 'rtmp://';
  SRC_FILE_MMS = 'mms://';
  SRC_FILE_FTP = 'ftp://';
  SRC_FILE_SMB = 'smb://';

  { ═══════════════════════════════════════════════════════════════════════════
    VIDEO PROPERTY INDICES (equivalent to mplayer.pas property indices)
    ═══════════════════════════════════════════════════════════════════════════ }
  PROP_BRIGHTNESS = 0;
  PROP_CONTRAST   = 1;
  PROP_SATURATION = 2;
  PROP_HUE        = 3;
  PROP_GAMMA      = 4;
  PROP_SUB_SCALE  = 5;
  MAX_PROP_ENTRYS = 5;

  { Default sub-scale value when loading external subtitles }
  PROP_SUB_SCALE_LOADSUB_VALUE = 1.56;

  { ═══════════════════════════════════════════════════════════════════════════
    CACHE TYPE INDICES (equivalent to mplayer.pas cache types)
    ═══════════════════════════════════════════════════════════════════════════ }
  CACHE_TYPE_DEFAULT   = 0;
  CACHE_TYPE_FIXED     = 1;
  CACHE_TYPE_RAMDISK   = 2;
  CACHE_TYPE_CDROM     = 3;
  CACHE_TYPE_REMOVABLE = 4;
  CACHE_TYPE_NETWORK   = 5;
  CACHE_TYPE_INTERNET  = 6;
  CACHE_TYPE_DVD       = 7;
  MAX_CACHE_ENTRYS     = 7;

  { Cache type names }
  CACHE_ENTRYS: array[0..MAX_CACHE_ENTRYS] of string = (
    'default',
    'fixed',
    'ramdisk',
    'cdrom',
    'removable',
    'network',
    'internet',
    'dvd'
  );

  { Default cache sizes (in kilobytes) }
  DEFAULT_CACHE_DEFAULT   = 4096;   { 4 MB }
  DEFAULT_CACHE_FIXED     = 2048;   { 2 MB }
  DEFAULT_CACHE_RAMDISK   = 512;    { 512 KB }
  DEFAULT_CACHE_CDROM     = 4096;   { 4 MB }
  DEFAULT_CACHE_REMOVABLE = 2048;   { 2 MB }
  DEFAULT_CACHE_NETWORK   = 8192;   { 8 MB }
  DEFAULT_CACHE_INTERNET  = 16384;  { 16 MB for streaming }
  DEFAULT_CACHE_DVD       = 8192;   { 8 MB }

  { ═══════════════════════════════════════════════════════════════════════════
    TRACK TYPE INDICES (equivalent to mplayer.pas IDS_TYPE_*)
    ═══════════════════════════════════════════════════════════════════════════ }
  IDS_TYPE_AUDIO      = 1;
  IDS_TYPE_SUB        = 2;
  IDS_TYPE_VIDEO      = 3;
  IDS_TYPE_DVDTITLE   = 10;
  IDS_TYPE_DVDCHAPTER = 20;

  { ═══════════════════════════════════════════════════════════════════════════
    OBSERVATION IDS (for mpv_observe_property reply_userdata)
    ═══════════════════════════════════════════════════════════════════════════ }
  OBS_TIME_POS        = 1;
  OBS_PERCENT_POS     = 2;
  OBS_DURATION        = 3;
  OBS_PAUSE           = 4;
  OBS_VOLUME          = 5;
  OBS_MUTE            = 6;
  OBS_SPEED           = 7;
  OBS_EOF_REACHED     = 8;
  OBS_TRACK_LIST      = 9;
  OBS_METADATA        = 10;
  OBS_VIDEO_PARAMS    = 11;
  OBS_AUDIO_PARAMS    = 12;
  OBS_CHAPTER         = 13;
  OBS_CHAPTERS        = 14;
  OBS_PLAYLIST_COUNT  = 15;
  OBS_PLAYLIST_POS    = 16;
  OBS_AID             = 17;
  OBS_SID             = 18;
  OBS_VID             = 19;
  OBS_IDLE_ACTIVE     = 20;
  OBS_CORE_IDLE       = 21;
  OBS_SEEKABLE        = 22;
  OBS_PARTIALLY_SEEKABLE = 23;
  OBS_PLAYBACK_TIME   = 24;
  OBS_DEMUXER_CACHE_STATE = 25;
  OBS_CACHE_SPEED     = 26;
  OBS_CACHE_BUFFERING_STATE = 27;
  OBS_FILENAME        = 28;
  OBS_MEDIA_TITLE     = 29;
  OBS_FILE_FORMAT     = 30;
  OBS_VIDEO_CODEC     = 31;
  OBS_AUDIO_CODEC     = 32;
  OBS_WIDTH           = 33;
  OBS_HEIGHT          = 34;
  OBS_DWIDTH          = 35;
  OBS_DHEIGHT         = 36;
  OBS_BRIGHTNESS      = 37;
  OBS_CONTRAST        = 38;
  OBS_SATURATION      = 39;
  OBS_HUE             = 40;
  OBS_GAMMA           = 41;
  OBS_SUB_SCALE       = 42;
  OBS_SUB_DELAY       = 43;
  OBS_SUB_VISIBILITY  = 44;
  OBS_AUDIO_DELAY     = 45;
  OBS_ICY_METADATA    = 46;
  OBS_DVD_TITLE       = 47;
  OBS_DVD_CHAPTERS    = 48;

  { ═══════════════════════════════════════════════════════════════════════════
    DEINTERLACE MODES
    ═══════════════════════════════════════════════════════════════════════════ }
  DEINT_OFF  = 0;
  DEINT_ON   = 1;
  DEINT_AUTO = 2;

  { Deinterlace algorithm indices }
  DEINT_ALG_AUTO   = 0;
  DEINT_ALG_YADIF  = 1;
  DEINT_ALG_BWDIF  = 2;
  DEINT_ALG_WEAVE  = 3;

  { ═══════════════════════════════════════════════════════════════════════════
    ASPECT RATIO PRESETS
    ═══════════════════════════════════════════════════════════════════════════ }
  ASPECT_AUTO      = 0;
  ASPECT_16_9      = 1;
  ASPECT_4_3       = 2;
  ASPECT_2_35_1    = 3;
  ASPECT_1_85_1    = 4;
  ASPECT_CUSTOM    = 5;

  ASPECT_VALUES: array[0..4] of Double = (
    -1.0,      { Auto }
    1.7778,    { 16:9 }
    1.3333,    { 4:3 }
    2.35,      { 2.35:1 }
    1.85       { 1.85:1 }
  );

  ASPECT_NAMES: array[0..4] of string = (
    'Auto',
    '16:9',
    '4:3',
    '2.35:1',
    '1.85:1'
  );

  { ═══════════════════════════════════════════════════════════════════════════
    EQUALIZER CONSTANTS
    ═══════════════════════════════════════════════════════════════════════════ }
  EQ_BANDS = 10;  { 10-band equalizer }

  { Center frequencies for 10-band equalizer (Hz) }
  EQ_FREQUENCIES: array[0..EQ_BANDS-1] of Integer = (
    31, 62, 125, 250, 500, 1000, 2000, 4000, 8000, 16000
  );

  { Frequency labels for display }
  EQ_FREQ_LABELS: array[0..EQ_BANDS-1] of string = (
    '31', '62', '125', '250', '500', '1K', '2K', '4K', '8K', '16K'
  );

  { Min/Max dB values }
  EQ_MIN_DB = -12.0;
  EQ_MAX_DB = 12.0;

  { ═══════════════════════════════════════════════════════════════════════════
    VOLUME CONSTANTS
    ═══════════════════════════════════════════════════════════════════════════ }
  VOL_MIN     = 0;
  VOL_MAX     = 100;
  VOL_BOOST   = 150;   { Maximum with software boost }
  VOL_DEFAULT = 100;

  { ═══════════════════════════════════════════════════════════════════════════
    SPEED CONSTANTS
    ═══════════════════════════════════════════════════════════════════════════ }
  SPEED_MIN     = 0.01;
  SPEED_MAX     = 100.0;
  SPEED_DEFAULT = 1.0;

  { Common speed presets }
  SPEED_PRESETS: array[0..8] of Double = (
    0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 4.0
  );

  { ═══════════════════════════════════════════════════════════════════════════
    SUBTITLE CONSTANTS
    ═══════════════════════════════════════════════════════════════════════════ }
  SUB_SCALE_MIN = 0.1;
  SUB_SCALE_MAX = 10.0;
  SUB_SCALE_DEFAULT = 1.0;

  SUB_DELAY_MIN = -300.0;  { seconds }
  SUB_DELAY_MAX = 300.0;
  SUB_DELAY_DEFAULT = 0.0;

  { ═══════════════════════════════════════════════════════════════════════════
    AUDIO CONSTANTS
    ═══════════════════════════════════════════════════════════════════════════ }
  AUDIO_DELAY_MIN = -300.0;  { seconds }
  AUDIO_DELAY_MAX = 300.0;
  AUDIO_DELAY_DEFAULT = 0.0;

  { ═══════════════════════════════════════════════════════════════════════════
    VIDEO PROPERTY RANGES
    ═══════════════════════════════════════════════════════════════════════════ }
  VIDEO_PROP_MIN = -100;
  VIDEO_PROP_MAX = 100;
  VIDEO_PROP_DEFAULT = 0;

  { ═══════════════════════════════════════════════════════════════════════════
    MPV VIDEO OUTPUTS
    ═══════════════════════════════════════════════════════════════════════════ }
  VO_AUTO = 'auto';
  VO_GPU = 'gpu';
  VO_GPU_NEXT = 'gpu-next';
  {$IFDEF WINDOWS}
  VO_D3D11 = 'd3d11';
  VO_OPENGL = 'opengl';
  VO_DEFAULT = VO_AUTO;
  {$ELSE}
  VO_X11 = 'x11';
  VO_WAYLAND = 'wayland';
  VO_XV = 'xv';
  VO_DEFAULT = VO_AUTO;  { Let MPV choose the best output }
  {$ENDIF}

  { ═══════════════════════════════════════════════════════════════════════════
    MPV AUDIO OUTPUTS
    ═══════════════════════════════════════════════════════════════════════════ }
  AO_AUTO = 'auto';
  {$IFDEF WINDOWS}
  AO_WASAPI = 'wasapi';
  AO_DSOUND = 'dsound';
  AO_DEFAULT = AO_AUTO;  { Let MPV choose the best output }
  {$ELSE}
  AO_PULSE = 'pulse';
  AO_ALSA = 'alsa';
  AO_PIPEWIRE = 'pipewire';
  AO_DEFAULT = AO_AUTO;  { Let MPV choose the best output }
  {$ENDIF}

  { ═══════════════════════════════════════════════════════════════════════════
    FILE EXTENSIONS
    ═══════════════════════════════════════════════════════════════════════════ }
  VIDEO_EXTENSIONS = '*.avi;*.mkv;*.mp4;*.m4v;*.mov;*.wmv;*.flv;*.webm;' +
    '*.mpeg;*.mpg;*.m2v;*.vob;*.ts;*.m2ts;*.mts;*.divx;*.xvid;*.3gp;*.3g2;' +
    '*.ogv;*.ogm;*.rmvb;*.rm;*.asf';

  AUDIO_EXTENSIONS = '*.mp3;*.flac;*.ogg;*.opus;*.m4a;*.aac;*.wma;*.wav;' +
    '*.aiff;*.ape;*.mpc;*.wv;*.tta;*.ac3;*.dts;*.mka;*.mid;*.midi';

  SUBTITLE_EXTENSIONS = '*.srt;*.ass;*.ssa;*.sub;*.idx;*.vtt;*.pgs;*.sup';

  PLAYLIST_EXTENSIONS = '*.m3u;*.m3u8;*.pls;*.xspf;*.cue';

  ALL_MEDIA_EXTENSIONS = VIDEO_EXTENSIONS + ';' + AUDIO_EXTENSIONS;

  { ═══════════════════════════════════════════════════════════════════════════
    SEEK CONSTANTS
    ═══════════════════════════════════════════════════════════════════════════ }
  SEEK_RELATIVE = 'relative';
  SEEK_ABSOLUTE = 'absolute';
  SEEK_ABSOLUTE_PERCENT = 'absolute-percent';
  SEEK_RELATIVE_PERCENT = 'relative-percent';
  SEEK_KEYFRAMES = 'keyframes';
  SEEK_EXACT = 'exact';

  { Default seek increments (seconds) }
  SEEK_SMALL  = 5;
  SEEK_MEDIUM = 30;
  SEEK_LARGE  = 60;

  { ═══════════════════════════════════════════════════════════════════════════
    DVD/BLURAY MENU COMMANDS
    ═══════════════════════════════════════════════════════════════════════════ }
  DVD_MENU_ROOT   = 'menu';
  DVD_MENU_TITLE  = 'menu';
  DVD_MENU_SELECT = 'select';
  DVD_MENU_UP     = 'up';
  DVD_MENU_DOWN   = 'down';
  DVD_MENU_LEFT   = 'left';
  DVD_MENU_RIGHT  = 'right';
  DVD_MENU_MOUSE  = 'mouse';
  DVD_MENU_PREV   = 'prev';

  { ═══════════════════════════════════════════════════════════════════════════
    SCREENSHOT FORMATS
    ═══════════════════════════════════════════════════════════════════════════ }
  SCREENSHOT_JPG = 'jpg';
  SCREENSHOT_PNG = 'png';
  SCREENSHOT_WEBP = 'webp';
  SCREENSHOT_DEFAULT = SCREENSHOT_PNG;

  { Screenshot modes }
  SCREENSHOT_VIDEO = 'video';           { Without subtitles }
  SCREENSHOT_SUBTITLES = 'subtitles';   { With subtitles }
  SCREENSHOT_WINDOW = 'window';         { Entire window }

  { ═══════════════════════════════════════════════════════════════════════════
    TIMEOUT CONSTANTS (milliseconds)
    ═══════════════════════════════════════════════════════════════════════════ }
  EVENT_TIMEOUT_MS = 100;
  NETWORK_TIMEOUT_SEC = 30;

  { ═══════════════════════════════════════════════════════════════════════════
    ICECAST/RADIO CONSTANTS
    ═══════════════════════════════════════════════════════════════════════════ }
  ICECAST_DIR_URL = 'http://dir.xiph.org/yp.xml';
  RADIO_BROWSER_URL = 'https://de1.api.radio-browser.info';

  { ═══════════════════════════════════════════════════════════════════════════
    CONFIG FILE NAMES
    ═══════════════════════════════════════════════════════════════════════════ }
  CONFIG_FILENAME = 'config.ini';
  HISTORY_FILENAME = 'history.ini';
  BOOKMARKS_FILENAME = 'bookmarks.ini';
  FAVORITES_FILENAME = 'favorites.ini';
  SHORTCUTS_FILENAME = 'shortcuts.ini';

implementation

end.
