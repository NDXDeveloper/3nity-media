{ ═══════════════════════════════════════════════════════════════════════════════
  uTestMPVConst.pas - Unit Tests for uMPVConst

  Part of 3nity Media - Test Suite

  Tests MPV engine constants including source prefixes, cache types,
  observation IDs, aspect ratios, equalizer bands, and various limits.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uTestMPVConst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  uMPVConst;

type
  { ═══════════════════════════════════════════════════════════════════════════
    TTestMPVConst - Tests for MPV Constants
    ═══════════════════════════════════════════════════════════════════════════ }
  TTestMPVConst = class(TTestCase)
  published
    { Source File Prefixes }
    procedure Test_SRC_FILE_DVDNAV;
    procedure Test_SRC_FILE_DVD;
    procedure Test_SRC_FILE_BLURAY;
    procedure Test_SRC_FILE_CDDA;
    procedure Test_SRC_FILE_VCD;
    procedure Test_SRC_FILE_TV;
    procedure Test_SRC_FILE_DVB;
    procedure Test_SRC_FILE_HTTP;
    procedure Test_SRC_FILE_HTTPS;
    procedure Test_SRC_FILE_RTSP;
    procedure Test_SRC_FILE_RTMP;
    procedure Test_SRC_FILE_MMS;
    procedure Test_SRC_FILE_FTP;
    procedure Test_SRC_FILE_SMB;
    procedure Test_SourcePrefixesEndWithColon;

    { Property Indices }
    procedure Test_PROP_BRIGHTNESS;
    procedure Test_PROP_CONTRAST;
    procedure Test_PROP_SATURATION;
    procedure Test_PROP_HUE;
    procedure Test_PROP_GAMMA;
    procedure Test_PROP_SUB_SCALE;
    procedure Test_MAX_PROP_ENTRYS;
    procedure Test_PROP_SUB_SCALE_LOADSUB_VALUE;

    { Cache Types }
    procedure Test_CACHE_TYPE_DEFAULT;
    procedure Test_CACHE_TYPE_FIXED;
    procedure Test_CACHE_TYPE_RAMDISK;
    procedure Test_CACHE_TYPE_CDROM;
    procedure Test_CACHE_TYPE_REMOVABLE;
    procedure Test_CACHE_TYPE_NETWORK;
    procedure Test_CACHE_TYPE_INTERNET;
    procedure Test_CACHE_TYPE_DVD;
    procedure Test_MAX_CACHE_ENTRYS;
    procedure Test_CACHE_ENTRYS_ArrayLength;
    procedure Test_CACHE_ENTRYS_NotEmpty;
    procedure Test_DefaultCacheSizes_Positive;
    procedure Test_DefaultCacheSizes_Ordering;

    { Track Types }
    procedure Test_IDS_TYPE_AUDIO;
    procedure Test_IDS_TYPE_SUB;
    procedure Test_IDS_TYPE_VIDEO;
    procedure Test_IDS_TYPE_DVDTITLE;
    procedure Test_IDS_TYPE_DVDCHAPTER;

    { Observation IDs }
    procedure Test_OBS_TIME_POS;
    procedure Test_OBS_DVD_CHAPTERS;
    procedure Test_OBS_IDs_Unique;
    procedure Test_OBS_IDs_Positive;

    { Deinterlace Modes }
    procedure Test_DEINT_OFF;
    procedure Test_DEINT_ON;
    procedure Test_DEINT_AUTO;
    procedure Test_DEINT_ALG_AUTO;
    procedure Test_DEINT_ALG_YADIF;
    procedure Test_DEINT_ALG_BWDIF;
    procedure Test_DEINT_ALG_WEAVE;

    { Aspect Ratios }
    procedure Test_ASPECT_AUTO;
    procedure Test_ASPECT_16_9;
    procedure Test_ASPECT_4_3;
    procedure Test_ASPECT_2_35_1;
    procedure Test_ASPECT_1_85_1;
    procedure Test_ASPECT_CUSTOM;
    procedure Test_ASPECT_VALUES_ArrayLength;
    procedure Test_ASPECT_NAMES_ArrayLength;
    procedure Test_ASPECT_VALUES_Auto_Negative;
    procedure Test_ASPECT_VALUES_Others_Positive;
    procedure Test_ASPECT_NAMES_NotEmpty;
    procedure Test_ASPECT_16_9_Value;
    procedure Test_ASPECT_4_3_Value;

    { Equalizer Constants }
    procedure Test_EQ_BANDS;
    procedure Test_EQ_FREQUENCIES_ArrayLength;
    procedure Test_EQ_FREQ_LABELS_ArrayLength;
    procedure Test_EQ_FREQUENCIES_Increasing;
    procedure Test_EQ_FREQUENCIES_Positive;
    procedure Test_EQ_FREQ_LABELS_NotEmpty;
    procedure Test_EQ_MIN_DB;
    procedure Test_EQ_MAX_DB;
    procedure Test_EQ_DB_Range;

    { Volume Constants }
    procedure Test_VOL_MIN;
    procedure Test_VOL_MAX;
    procedure Test_VOL_BOOST;
    procedure Test_VOL_DEFAULT;
    procedure Test_VOL_Range;

    { Speed Constants }
    procedure Test_SPEED_MIN;
    procedure Test_SPEED_MAX;
    procedure Test_SPEED_DEFAULT;
    procedure Test_SPEED_Range;
    procedure Test_SPEED_PRESETS_ArrayLength;
    procedure Test_SPEED_PRESETS_Increasing;
    procedure Test_SPEED_PRESETS_Contains_1_0;

    { Subtitle Constants }
    procedure Test_SUB_SCALE_MIN;
    procedure Test_SUB_SCALE_MAX;
    procedure Test_SUB_SCALE_DEFAULT;
    procedure Test_SUB_SCALE_Range;
    procedure Test_SUB_DELAY_MIN;
    procedure Test_SUB_DELAY_MAX;
    procedure Test_SUB_DELAY_DEFAULT;

    { Audio Constants }
    procedure Test_AUDIO_DELAY_MIN;
    procedure Test_AUDIO_DELAY_MAX;
    procedure Test_AUDIO_DELAY_DEFAULT;

    { Video Property Ranges }
    procedure Test_VIDEO_PROP_MIN;
    procedure Test_VIDEO_PROP_MAX;
    procedure Test_VIDEO_PROP_DEFAULT;
    procedure Test_VIDEO_PROP_Range;

    { Video/Audio Outputs }
    procedure Test_VO_AUTO;
    procedure Test_VO_GPU;
    procedure Test_VO_GPU_NEXT;
    procedure Test_VO_DEFAULT;
    procedure Test_AO_AUTO;
    procedure Test_AO_DEFAULT;

    { File Extensions }
    procedure Test_VIDEO_EXTENSIONS_NotEmpty;
    procedure Test_VIDEO_EXTENSIONS_Contains_AVI;
    procedure Test_VIDEO_EXTENSIONS_Contains_MKV;
    procedure Test_VIDEO_EXTENSIONS_Contains_MP4;
    procedure Test_AUDIO_EXTENSIONS_NotEmpty;
    procedure Test_AUDIO_EXTENSIONS_Contains_MP3;
    procedure Test_AUDIO_EXTENSIONS_Contains_FLAC;
    procedure Test_SUBTITLE_EXTENSIONS_NotEmpty;
    procedure Test_SUBTITLE_EXTENSIONS_Contains_SRT;
    procedure Test_PLAYLIST_EXTENSIONS_NotEmpty;
    procedure Test_PLAYLIST_EXTENSIONS_Contains_M3U;
    procedure Test_ALL_MEDIA_EXTENSIONS_NotEmpty;
    procedure Test_ALL_MEDIA_EXTENSIONS_Contains_Video;
    procedure Test_ALL_MEDIA_EXTENSIONS_Contains_Audio;

    { Seek Constants }
    procedure Test_SEEK_RELATIVE;
    procedure Test_SEEK_ABSOLUTE;
    procedure Test_SEEK_ABSOLUTE_PERCENT;
    procedure Test_SEEK_RELATIVE_PERCENT;
    procedure Test_SEEK_KEYFRAMES;
    procedure Test_SEEK_EXACT;
    procedure Test_SEEK_SMALL;
    procedure Test_SEEK_MEDIUM;
    procedure Test_SEEK_LARGE;
    procedure Test_SEEK_Increments_Increasing;

    { DVD Menu Commands }
    procedure Test_DVD_MENU_ROOT;
    procedure Test_DVD_MENU_TITLE;
    procedure Test_DVD_MENU_SELECT;
    procedure Test_DVD_MENU_UP;
    procedure Test_DVD_MENU_DOWN;
    procedure Test_DVD_MENU_LEFT;
    procedure Test_DVD_MENU_RIGHT;
    procedure Test_DVD_MENU_MOUSE;
    procedure Test_DVD_MENU_PREV;

    { Screenshot Formats }
    procedure Test_SCREENSHOT_JPG;
    procedure Test_SCREENSHOT_PNG;
    procedure Test_SCREENSHOT_WEBP;
    procedure Test_SCREENSHOT_DEFAULT;
    procedure Test_SCREENSHOT_VIDEO;
    procedure Test_SCREENSHOT_SUBTITLES;
    procedure Test_SCREENSHOT_WINDOW;

    { Timeout Constants }
    procedure Test_EVENT_TIMEOUT_MS;
    procedure Test_NETWORK_TIMEOUT_SEC;

    { Radio URLs }
    procedure Test_ICECAST_DIR_URL;
    procedure Test_RADIO_BROWSER_URL;

    { Config Filenames }
    procedure Test_CONFIG_FILENAME;
    procedure Test_HISTORY_FILENAME;
    procedure Test_BOOKMARKS_FILENAME;
    procedure Test_FAVORITES_FILENAME;
    procedure Test_SHORTCUTS_FILENAME;
    procedure Test_ConfigFilenames_EndWith_INI;
  end;

implementation

{ ═══════════════════════════════════════════════════════════════════════════════
  Source File Prefixes Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_SRC_FILE_DVDNAV;
begin
  AssertEquals('SRC_FILE_DVDNAV', 'dvdnav://', SRC_FILE_DVDNAV);
end;

procedure TTestMPVConst.Test_SRC_FILE_DVD;
begin
  AssertEquals('SRC_FILE_DVD', 'dvd://', SRC_FILE_DVD);
end;

procedure TTestMPVConst.Test_SRC_FILE_BLURAY;
begin
  AssertEquals('SRC_FILE_BLURAY', 'bd://', SRC_FILE_BLURAY);
end;

procedure TTestMPVConst.Test_SRC_FILE_CDDA;
begin
  AssertEquals('SRC_FILE_CDDA', 'cdda://', SRC_FILE_CDDA);
end;

procedure TTestMPVConst.Test_SRC_FILE_VCD;
begin
  AssertEquals('SRC_FILE_VCD', 'vcd://', SRC_FILE_VCD);
end;

procedure TTestMPVConst.Test_SRC_FILE_TV;
begin
  AssertEquals('SRC_FILE_TV', 'tv://', SRC_FILE_TV);
end;

procedure TTestMPVConst.Test_SRC_FILE_DVB;
begin
  AssertEquals('SRC_FILE_DVB', 'dvb://', SRC_FILE_DVB);
end;

procedure TTestMPVConst.Test_SRC_FILE_HTTP;
begin
  AssertEquals('SRC_FILE_HTTP', 'http://', SRC_FILE_HTTP);
end;

procedure TTestMPVConst.Test_SRC_FILE_HTTPS;
begin
  AssertEquals('SRC_FILE_HTTPS', 'https://', SRC_FILE_HTTPS);
end;

procedure TTestMPVConst.Test_SRC_FILE_RTSP;
begin
  AssertEquals('SRC_FILE_RTSP', 'rtsp://', SRC_FILE_RTSP);
end;

procedure TTestMPVConst.Test_SRC_FILE_RTMP;
begin
  AssertEquals('SRC_FILE_RTMP', 'rtmp://', SRC_FILE_RTMP);
end;

procedure TTestMPVConst.Test_SRC_FILE_MMS;
begin
  AssertEquals('SRC_FILE_MMS', 'mms://', SRC_FILE_MMS);
end;

procedure TTestMPVConst.Test_SRC_FILE_FTP;
begin
  AssertEquals('SRC_FILE_FTP', 'ftp://', SRC_FILE_FTP);
end;

procedure TTestMPVConst.Test_SRC_FILE_SMB;
begin
  AssertEquals('SRC_FILE_SMB', 'smb://', SRC_FILE_SMB);
end;

procedure TTestMPVConst.Test_SourcePrefixesEndWithColon;
begin
  AssertTrue('DVDNAV ends with ://', Pos('://', SRC_FILE_DVDNAV) > 0);
  AssertTrue('DVD ends with ://', Pos('://', SRC_FILE_DVD) > 0);
  AssertTrue('BLURAY ends with ://', Pos('://', SRC_FILE_BLURAY) > 0);
  AssertTrue('CDDA ends with ://', Pos('://', SRC_FILE_CDDA) > 0);
  AssertTrue('VCD ends with ://', Pos('://', SRC_FILE_VCD) > 0);
  AssertTrue('TV ends with ://', Pos('://', SRC_FILE_TV) > 0);
  AssertTrue('DVB ends with ://', Pos('://', SRC_FILE_DVB) > 0);
  AssertTrue('HTTP ends with ://', Pos('://', SRC_FILE_HTTP) > 0);
  AssertTrue('HTTPS ends with ://', Pos('://', SRC_FILE_HTTPS) > 0);
  AssertTrue('RTSP ends with ://', Pos('://', SRC_FILE_RTSP) > 0);
  AssertTrue('RTMP ends with ://', Pos('://', SRC_FILE_RTMP) > 0);
  AssertTrue('MMS ends with ://', Pos('://', SRC_FILE_MMS) > 0);
  AssertTrue('FTP ends with ://', Pos('://', SRC_FILE_FTP) > 0);
  AssertTrue('SMB ends with ://', Pos('://', SRC_FILE_SMB) > 0);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Property Indices Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_PROP_BRIGHTNESS;
begin
  AssertEquals('PROP_BRIGHTNESS', 0, PROP_BRIGHTNESS);
end;

procedure TTestMPVConst.Test_PROP_CONTRAST;
begin
  AssertEquals('PROP_CONTRAST', 1, PROP_CONTRAST);
end;

procedure TTestMPVConst.Test_PROP_SATURATION;
begin
  AssertEquals('PROP_SATURATION', 2, PROP_SATURATION);
end;

procedure TTestMPVConst.Test_PROP_HUE;
begin
  AssertEquals('PROP_HUE', 3, PROP_HUE);
end;

procedure TTestMPVConst.Test_PROP_GAMMA;
begin
  AssertEquals('PROP_GAMMA', 4, PROP_GAMMA);
end;

procedure TTestMPVConst.Test_PROP_SUB_SCALE;
begin
  AssertEquals('PROP_SUB_SCALE', 5, PROP_SUB_SCALE);
end;

procedure TTestMPVConst.Test_MAX_PROP_ENTRYS;
begin
  AssertEquals('MAX_PROP_ENTRYS equals PROP_SUB_SCALE', PROP_SUB_SCALE, MAX_PROP_ENTRYS);
end;

procedure TTestMPVConst.Test_PROP_SUB_SCALE_LOADSUB_VALUE;
begin
  AssertTrue('PROP_SUB_SCALE_LOADSUB_VALUE > 1', PROP_SUB_SCALE_LOADSUB_VALUE > 1.0);
  AssertTrue('PROP_SUB_SCALE_LOADSUB_VALUE < 2', PROP_SUB_SCALE_LOADSUB_VALUE < 2.0);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Cache Types Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_CACHE_TYPE_DEFAULT;
begin
  AssertEquals('CACHE_TYPE_DEFAULT', 0, CACHE_TYPE_DEFAULT);
end;

procedure TTestMPVConst.Test_CACHE_TYPE_FIXED;
begin
  AssertEquals('CACHE_TYPE_FIXED', 1, CACHE_TYPE_FIXED);
end;

procedure TTestMPVConst.Test_CACHE_TYPE_RAMDISK;
begin
  AssertEquals('CACHE_TYPE_RAMDISK', 2, CACHE_TYPE_RAMDISK);
end;

procedure TTestMPVConst.Test_CACHE_TYPE_CDROM;
begin
  AssertEquals('CACHE_TYPE_CDROM', 3, CACHE_TYPE_CDROM);
end;

procedure TTestMPVConst.Test_CACHE_TYPE_REMOVABLE;
begin
  AssertEquals('CACHE_TYPE_REMOVABLE', 4, CACHE_TYPE_REMOVABLE);
end;

procedure TTestMPVConst.Test_CACHE_TYPE_NETWORK;
begin
  AssertEquals('CACHE_TYPE_NETWORK', 5, CACHE_TYPE_NETWORK);
end;

procedure TTestMPVConst.Test_CACHE_TYPE_INTERNET;
begin
  AssertEquals('CACHE_TYPE_INTERNET', 6, CACHE_TYPE_INTERNET);
end;

procedure TTestMPVConst.Test_CACHE_TYPE_DVD;
begin
  AssertEquals('CACHE_TYPE_DVD', 7, CACHE_TYPE_DVD);
end;

procedure TTestMPVConst.Test_MAX_CACHE_ENTRYS;
begin
  AssertEquals('MAX_CACHE_ENTRYS', 7, MAX_CACHE_ENTRYS);
end;

procedure TTestMPVConst.Test_CACHE_ENTRYS_ArrayLength;
begin
  AssertEquals('CACHE_ENTRYS length', MAX_CACHE_ENTRYS + 1, Length(CACHE_ENTRYS));
end;

procedure TTestMPVConst.Test_CACHE_ENTRYS_NotEmpty;
var
  I: Integer;
begin
  for I := 0 to MAX_CACHE_ENTRYS do
    AssertTrue('CACHE_ENTRYS[' + IntToStr(I) + '] not empty', CACHE_ENTRYS[I] <> '');
end;

procedure TTestMPVConst.Test_DefaultCacheSizes_Positive;
begin
  AssertTrue('DEFAULT_CACHE_DEFAULT > 0', DEFAULT_CACHE_DEFAULT > 0);
  AssertTrue('DEFAULT_CACHE_FIXED > 0', DEFAULT_CACHE_FIXED > 0);
  AssertTrue('DEFAULT_CACHE_RAMDISK > 0', DEFAULT_CACHE_RAMDISK > 0);
  AssertTrue('DEFAULT_CACHE_CDROM > 0', DEFAULT_CACHE_CDROM > 0);
  AssertTrue('DEFAULT_CACHE_REMOVABLE > 0', DEFAULT_CACHE_REMOVABLE > 0);
  AssertTrue('DEFAULT_CACHE_NETWORK > 0', DEFAULT_CACHE_NETWORK > 0);
  AssertTrue('DEFAULT_CACHE_INTERNET > 0', DEFAULT_CACHE_INTERNET > 0);
  AssertTrue('DEFAULT_CACHE_DVD > 0', DEFAULT_CACHE_DVD > 0);
end;

procedure TTestMPVConst.Test_DefaultCacheSizes_Ordering;
begin
  { Internet streaming needs most cache }
  AssertTrue('Internet cache >= Network cache',
    DEFAULT_CACHE_INTERNET >= DEFAULT_CACHE_NETWORK);
  { Ramdisk is fastest, needs less cache }
  AssertTrue('Ramdisk cache <= Fixed cache',
    DEFAULT_CACHE_RAMDISK <= DEFAULT_CACHE_FIXED);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Track Types Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_IDS_TYPE_AUDIO;
begin
  AssertEquals('IDS_TYPE_AUDIO', 1, IDS_TYPE_AUDIO);
end;

procedure TTestMPVConst.Test_IDS_TYPE_SUB;
begin
  AssertEquals('IDS_TYPE_SUB', 2, IDS_TYPE_SUB);
end;

procedure TTestMPVConst.Test_IDS_TYPE_VIDEO;
begin
  AssertEquals('IDS_TYPE_VIDEO', 3, IDS_TYPE_VIDEO);
end;

procedure TTestMPVConst.Test_IDS_TYPE_DVDTITLE;
begin
  AssertEquals('IDS_TYPE_DVDTITLE', 10, IDS_TYPE_DVDTITLE);
end;

procedure TTestMPVConst.Test_IDS_TYPE_DVDCHAPTER;
begin
  AssertEquals('IDS_TYPE_DVDCHAPTER', 20, IDS_TYPE_DVDCHAPTER);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Observation IDs Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_OBS_TIME_POS;
begin
  AssertEquals('OBS_TIME_POS', 1, OBS_TIME_POS);
end;

procedure TTestMPVConst.Test_OBS_DVD_CHAPTERS;
begin
  AssertEquals('OBS_DVD_CHAPTERS', 48, OBS_DVD_CHAPTERS);
end;

procedure TTestMPVConst.Test_OBS_IDs_Unique;
var
  IDs: array[1..48] of Integer;
  I, J: Integer;
begin
  { Collect all observation IDs }
  IDs[1] := OBS_TIME_POS;
  IDs[2] := OBS_PERCENT_POS;
  IDs[3] := OBS_DURATION;
  IDs[4] := OBS_PAUSE;
  IDs[5] := OBS_VOLUME;
  IDs[6] := OBS_MUTE;
  IDs[7] := OBS_SPEED;
  IDs[8] := OBS_EOF_REACHED;
  IDs[9] := OBS_TRACK_LIST;
  IDs[10] := OBS_METADATA;
  IDs[11] := OBS_VIDEO_PARAMS;
  IDs[12] := OBS_AUDIO_PARAMS;
  IDs[13] := OBS_CHAPTER;
  IDs[14] := OBS_CHAPTERS;
  IDs[15] := OBS_PLAYLIST_COUNT;
  IDs[16] := OBS_PLAYLIST_POS;
  IDs[17] := OBS_AID;
  IDs[18] := OBS_SID;
  IDs[19] := OBS_VID;
  IDs[20] := OBS_IDLE_ACTIVE;
  IDs[21] := OBS_CORE_IDLE;
  IDs[22] := OBS_SEEKABLE;
  IDs[23] := OBS_PARTIALLY_SEEKABLE;
  IDs[24] := OBS_PLAYBACK_TIME;
  IDs[25] := OBS_DEMUXER_CACHE_STATE;
  IDs[26] := OBS_CACHE_SPEED;
  IDs[27] := OBS_CACHE_BUFFERING_STATE;
  IDs[28] := OBS_FILENAME;
  IDs[29] := OBS_MEDIA_TITLE;
  IDs[30] := OBS_FILE_FORMAT;
  IDs[31] := OBS_VIDEO_CODEC;
  IDs[32] := OBS_AUDIO_CODEC;
  IDs[33] := OBS_WIDTH;
  IDs[34] := OBS_HEIGHT;
  IDs[35] := OBS_DWIDTH;
  IDs[36] := OBS_DHEIGHT;
  IDs[37] := OBS_BRIGHTNESS;
  IDs[38] := OBS_CONTRAST;
  IDs[39] := OBS_SATURATION;
  IDs[40] := OBS_HUE;
  IDs[41] := OBS_GAMMA;
  IDs[42] := OBS_SUB_SCALE;
  IDs[43] := OBS_SUB_DELAY;
  IDs[44] := OBS_SUB_VISIBILITY;
  IDs[45] := OBS_AUDIO_DELAY;
  IDs[46] := OBS_ICY_METADATA;
  IDs[47] := OBS_DVD_TITLE;
  IDs[48] := OBS_DVD_CHAPTERS;

  { Check uniqueness }
  for I := 1 to 48 do
    for J := I + 1 to 48 do
      AssertTrue('OBS IDs should be unique: ' + IntToStr(I) + ' vs ' + IntToStr(J),
        IDs[I] <> IDs[J]);
end;

procedure TTestMPVConst.Test_OBS_IDs_Positive;
begin
  AssertTrue('OBS_TIME_POS > 0', OBS_TIME_POS > 0);
  AssertTrue('OBS_DVD_CHAPTERS > 0', OBS_DVD_CHAPTERS > 0);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Deinterlace Modes Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_DEINT_OFF;
begin
  AssertEquals('DEINT_OFF', 0, DEINT_OFF);
end;

procedure TTestMPVConst.Test_DEINT_ON;
begin
  AssertEquals('DEINT_ON', 1, DEINT_ON);
end;

procedure TTestMPVConst.Test_DEINT_AUTO;
begin
  AssertEquals('DEINT_AUTO', 2, DEINT_AUTO);
end;

procedure TTestMPVConst.Test_DEINT_ALG_AUTO;
begin
  AssertEquals('DEINT_ALG_AUTO', 0, DEINT_ALG_AUTO);
end;

procedure TTestMPVConst.Test_DEINT_ALG_YADIF;
begin
  AssertEquals('DEINT_ALG_YADIF', 1, DEINT_ALG_YADIF);
end;

procedure TTestMPVConst.Test_DEINT_ALG_BWDIF;
begin
  AssertEquals('DEINT_ALG_BWDIF', 2, DEINT_ALG_BWDIF);
end;

procedure TTestMPVConst.Test_DEINT_ALG_WEAVE;
begin
  AssertEquals('DEINT_ALG_WEAVE', 3, DEINT_ALG_WEAVE);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Aspect Ratios Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_ASPECT_AUTO;
begin
  AssertEquals('ASPECT_AUTO', 0, ASPECT_AUTO);
end;

procedure TTestMPVConst.Test_ASPECT_16_9;
begin
  AssertEquals('ASPECT_16_9', 1, ASPECT_16_9);
end;

procedure TTestMPVConst.Test_ASPECT_4_3;
begin
  AssertEquals('ASPECT_4_3', 2, ASPECT_4_3);
end;

procedure TTestMPVConst.Test_ASPECT_2_35_1;
begin
  AssertEquals('ASPECT_2_35_1', 3, ASPECT_2_35_1);
end;

procedure TTestMPVConst.Test_ASPECT_1_85_1;
begin
  AssertEquals('ASPECT_1_85_1', 4, ASPECT_1_85_1);
end;

procedure TTestMPVConst.Test_ASPECT_CUSTOM;
begin
  AssertEquals('ASPECT_CUSTOM', 5, ASPECT_CUSTOM);
end;

procedure TTestMPVConst.Test_ASPECT_VALUES_ArrayLength;
begin
  AssertEquals('ASPECT_VALUES length', 5, Length(ASPECT_VALUES));
end;

procedure TTestMPVConst.Test_ASPECT_NAMES_ArrayLength;
begin
  AssertEquals('ASPECT_NAMES length', 5, Length(ASPECT_NAMES));
end;

procedure TTestMPVConst.Test_ASPECT_VALUES_Auto_Negative;
begin
  AssertTrue('Auto aspect is negative (special value)', ASPECT_VALUES[ASPECT_AUTO] < 0);
end;

procedure TTestMPVConst.Test_ASPECT_VALUES_Others_Positive;
var
  I: Integer;
begin
  for I := 1 to 4 do
    AssertTrue('ASPECT_VALUES[' + IntToStr(I) + '] > 0', ASPECT_VALUES[I] > 0);
end;

procedure TTestMPVConst.Test_ASPECT_NAMES_NotEmpty;
var
  I: Integer;
begin
  for I := 0 to 4 do
    AssertTrue('ASPECT_NAMES[' + IntToStr(I) + '] not empty', ASPECT_NAMES[I] <> '');
end;

procedure TTestMPVConst.Test_ASPECT_16_9_Value;
begin
  { 16/9 ≈ 1.7778 }
  AssertTrue('16:9 ratio ≈ 1.7778', Abs(ASPECT_VALUES[ASPECT_16_9] - 1.7778) < 0.001);
end;

procedure TTestMPVConst.Test_ASPECT_4_3_Value;
begin
  { 4/3 ≈ 1.3333 }
  AssertTrue('4:3 ratio ≈ 1.3333', Abs(ASPECT_VALUES[ASPECT_4_3] - 1.3333) < 0.001);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Equalizer Constants Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_EQ_BANDS;
begin
  AssertEquals('EQ_BANDS', 10, EQ_BANDS);
end;

procedure TTestMPVConst.Test_EQ_FREQUENCIES_ArrayLength;
begin
  AssertEquals('EQ_FREQUENCIES length', EQ_BANDS, Length(EQ_FREQUENCIES));
end;

procedure TTestMPVConst.Test_EQ_FREQ_LABELS_ArrayLength;
begin
  AssertEquals('EQ_FREQ_LABELS length', EQ_BANDS, Length(EQ_FREQ_LABELS));
end;

procedure TTestMPVConst.Test_EQ_FREQUENCIES_Increasing;
var
  I: Integer;
begin
  for I := 1 to EQ_BANDS - 1 do
    AssertTrue('Frequencies should increase: ' + IntToStr(I),
      EQ_FREQUENCIES[I] > EQ_FREQUENCIES[I - 1]);
end;

procedure TTestMPVConst.Test_EQ_FREQUENCIES_Positive;
var
  I: Integer;
begin
  for I := 0 to EQ_BANDS - 1 do
    AssertTrue('EQ_FREQUENCIES[' + IntToStr(I) + '] > 0', EQ_FREQUENCIES[I] > 0);
end;

procedure TTestMPVConst.Test_EQ_FREQ_LABELS_NotEmpty;
var
  I: Integer;
begin
  for I := 0 to EQ_BANDS - 1 do
    AssertTrue('EQ_FREQ_LABELS[' + IntToStr(I) + '] not empty', EQ_FREQ_LABELS[I] <> '');
end;

procedure TTestMPVConst.Test_EQ_MIN_DB;
begin
  AssertEquals('EQ_MIN_DB', -12.0, EQ_MIN_DB);
end;

procedure TTestMPVConst.Test_EQ_MAX_DB;
begin
  AssertEquals('EQ_MAX_DB', 12.0, EQ_MAX_DB);
end;

procedure TTestMPVConst.Test_EQ_DB_Range;
begin
  AssertTrue('EQ_MIN_DB < 0', EQ_MIN_DB < 0);
  AssertTrue('EQ_MAX_DB > 0', EQ_MAX_DB > 0);
  AssertTrue('EQ range is symmetric', Abs(EQ_MIN_DB) = EQ_MAX_DB);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Volume Constants Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_VOL_MIN;
begin
  AssertEquals('VOL_MIN', 0, VOL_MIN);
end;

procedure TTestMPVConst.Test_VOL_MAX;
begin
  AssertEquals('VOL_MAX', 100, VOL_MAX);
end;

procedure TTestMPVConst.Test_VOL_BOOST;
begin
  AssertEquals('VOL_BOOST', 150, VOL_BOOST);
end;

procedure TTestMPVConst.Test_VOL_DEFAULT;
begin
  AssertEquals('VOL_DEFAULT', 100, VOL_DEFAULT);
end;

procedure TTestMPVConst.Test_VOL_Range;
begin
  AssertTrue('VOL_MIN < VOL_DEFAULT', VOL_MIN < VOL_DEFAULT);
  AssertTrue('VOL_DEFAULT <= VOL_MAX', VOL_DEFAULT <= VOL_MAX);
  AssertTrue('VOL_MAX < VOL_BOOST', VOL_MAX < VOL_BOOST);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Speed Constants Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_SPEED_MIN;
begin
  AssertEquals('SPEED_MIN', 0.01, SPEED_MIN);
end;

procedure TTestMPVConst.Test_SPEED_MAX;
begin
  AssertEquals('SPEED_MAX', 100.0, SPEED_MAX);
end;

procedure TTestMPVConst.Test_SPEED_DEFAULT;
begin
  AssertEquals('SPEED_DEFAULT', 1.0, SPEED_DEFAULT);
end;

procedure TTestMPVConst.Test_SPEED_Range;
begin
  AssertTrue('SPEED_MIN < SPEED_DEFAULT', SPEED_MIN < SPEED_DEFAULT);
  AssertTrue('SPEED_DEFAULT < SPEED_MAX', SPEED_DEFAULT < SPEED_MAX);
end;

procedure TTestMPVConst.Test_SPEED_PRESETS_ArrayLength;
begin
  AssertEquals('SPEED_PRESETS length', 9, Length(SPEED_PRESETS));
end;

procedure TTestMPVConst.Test_SPEED_PRESETS_Increasing;
var
  I: Integer;
begin
  for I := 1 to High(SPEED_PRESETS) do
    AssertTrue('Speed presets should increase: ' + IntToStr(I),
      SPEED_PRESETS[I] > SPEED_PRESETS[I - 1]);
end;

procedure TTestMPVConst.Test_SPEED_PRESETS_Contains_1_0;
var
  I: Integer;
  Found: Boolean;
begin
  Found := False;
  for I := 0 to High(SPEED_PRESETS) do
    if Abs(SPEED_PRESETS[I] - 1.0) < 0.001 then
    begin
      Found := True;
      Break;
    end;
  AssertTrue('Speed presets should contain 1.0 (normal speed)', Found);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Subtitle Constants Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_SUB_SCALE_MIN;
begin
  AssertEquals('SUB_SCALE_MIN', 0.1, SUB_SCALE_MIN);
end;

procedure TTestMPVConst.Test_SUB_SCALE_MAX;
begin
  AssertEquals('SUB_SCALE_MAX', 10.0, SUB_SCALE_MAX);
end;

procedure TTestMPVConst.Test_SUB_SCALE_DEFAULT;
begin
  AssertEquals('SUB_SCALE_DEFAULT', 1.0, SUB_SCALE_DEFAULT);
end;

procedure TTestMPVConst.Test_SUB_SCALE_Range;
begin
  AssertTrue('SUB_SCALE_MIN < SUB_SCALE_DEFAULT', SUB_SCALE_MIN < SUB_SCALE_DEFAULT);
  AssertTrue('SUB_SCALE_DEFAULT < SUB_SCALE_MAX', SUB_SCALE_DEFAULT < SUB_SCALE_MAX);
end;

procedure TTestMPVConst.Test_SUB_DELAY_MIN;
begin
  AssertEquals('SUB_DELAY_MIN', -300.0, SUB_DELAY_MIN);
end;

procedure TTestMPVConst.Test_SUB_DELAY_MAX;
begin
  AssertEquals('SUB_DELAY_MAX', 300.0, SUB_DELAY_MAX);
end;

procedure TTestMPVConst.Test_SUB_DELAY_DEFAULT;
begin
  AssertEquals('SUB_DELAY_DEFAULT', 0.0, SUB_DELAY_DEFAULT);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Audio Constants Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_AUDIO_DELAY_MIN;
begin
  AssertEquals('AUDIO_DELAY_MIN', -300.0, AUDIO_DELAY_MIN);
end;

procedure TTestMPVConst.Test_AUDIO_DELAY_MAX;
begin
  AssertEquals('AUDIO_DELAY_MAX', 300.0, AUDIO_DELAY_MAX);
end;

procedure TTestMPVConst.Test_AUDIO_DELAY_DEFAULT;
begin
  AssertEquals('AUDIO_DELAY_DEFAULT', 0.0, AUDIO_DELAY_DEFAULT);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Video Property Ranges Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_VIDEO_PROP_MIN;
begin
  AssertEquals('VIDEO_PROP_MIN', -100, VIDEO_PROP_MIN);
end;

procedure TTestMPVConst.Test_VIDEO_PROP_MAX;
begin
  AssertEquals('VIDEO_PROP_MAX', 100, VIDEO_PROP_MAX);
end;

procedure TTestMPVConst.Test_VIDEO_PROP_DEFAULT;
begin
  AssertEquals('VIDEO_PROP_DEFAULT', 0, VIDEO_PROP_DEFAULT);
end;

procedure TTestMPVConst.Test_VIDEO_PROP_Range;
begin
  AssertTrue('VIDEO_PROP_MIN < VIDEO_PROP_DEFAULT', VIDEO_PROP_MIN < VIDEO_PROP_DEFAULT);
  AssertTrue('VIDEO_PROP_DEFAULT < VIDEO_PROP_MAX', VIDEO_PROP_DEFAULT < VIDEO_PROP_MAX);
  AssertTrue('Range is symmetric', Abs(VIDEO_PROP_MIN) = VIDEO_PROP_MAX);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Video/Audio Outputs Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_VO_AUTO;
begin
  AssertEquals('VO_AUTO', 'auto', VO_AUTO);
end;

procedure TTestMPVConst.Test_VO_GPU;
begin
  AssertEquals('VO_GPU', 'gpu', VO_GPU);
end;

procedure TTestMPVConst.Test_VO_GPU_NEXT;
begin
  AssertEquals('VO_GPU_NEXT', 'gpu-next', VO_GPU_NEXT);
end;

procedure TTestMPVConst.Test_VO_DEFAULT;
begin
  AssertTrue('VO_DEFAULT not empty', VO_DEFAULT <> '');
end;

procedure TTestMPVConst.Test_AO_AUTO;
begin
  AssertEquals('AO_AUTO', 'auto', AO_AUTO);
end;

procedure TTestMPVConst.Test_AO_DEFAULT;
begin
  AssertTrue('AO_DEFAULT not empty', AO_DEFAULT <> '');
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  File Extensions Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_VIDEO_EXTENSIONS_NotEmpty;
begin
  AssertTrue('VIDEO_EXTENSIONS not empty', VIDEO_EXTENSIONS <> '');
end;

procedure TTestMPVConst.Test_VIDEO_EXTENSIONS_Contains_AVI;
begin
  AssertTrue('VIDEO_EXTENSIONS contains *.avi', Pos('*.avi', VIDEO_EXTENSIONS) > 0);
end;

procedure TTestMPVConst.Test_VIDEO_EXTENSIONS_Contains_MKV;
begin
  AssertTrue('VIDEO_EXTENSIONS contains *.mkv', Pos('*.mkv', VIDEO_EXTENSIONS) > 0);
end;

procedure TTestMPVConst.Test_VIDEO_EXTENSIONS_Contains_MP4;
begin
  AssertTrue('VIDEO_EXTENSIONS contains *.mp4', Pos('*.mp4', VIDEO_EXTENSIONS) > 0);
end;

procedure TTestMPVConst.Test_AUDIO_EXTENSIONS_NotEmpty;
begin
  AssertTrue('AUDIO_EXTENSIONS not empty', AUDIO_EXTENSIONS <> '');
end;

procedure TTestMPVConst.Test_AUDIO_EXTENSIONS_Contains_MP3;
begin
  AssertTrue('AUDIO_EXTENSIONS contains *.mp3', Pos('*.mp3', AUDIO_EXTENSIONS) > 0);
end;

procedure TTestMPVConst.Test_AUDIO_EXTENSIONS_Contains_FLAC;
begin
  AssertTrue('AUDIO_EXTENSIONS contains *.flac', Pos('*.flac', AUDIO_EXTENSIONS) > 0);
end;

procedure TTestMPVConst.Test_SUBTITLE_EXTENSIONS_NotEmpty;
begin
  AssertTrue('SUBTITLE_EXTENSIONS not empty', SUBTITLE_EXTENSIONS <> '');
end;

procedure TTestMPVConst.Test_SUBTITLE_EXTENSIONS_Contains_SRT;
begin
  AssertTrue('SUBTITLE_EXTENSIONS contains *.srt', Pos('*.srt', SUBTITLE_EXTENSIONS) > 0);
end;

procedure TTestMPVConst.Test_PLAYLIST_EXTENSIONS_NotEmpty;
begin
  AssertTrue('PLAYLIST_EXTENSIONS not empty', PLAYLIST_EXTENSIONS <> '');
end;

procedure TTestMPVConst.Test_PLAYLIST_EXTENSIONS_Contains_M3U;
begin
  AssertTrue('PLAYLIST_EXTENSIONS contains *.m3u', Pos('*.m3u', PLAYLIST_EXTENSIONS) > 0);
end;

procedure TTestMPVConst.Test_ALL_MEDIA_EXTENSIONS_NotEmpty;
begin
  AssertTrue('ALL_MEDIA_EXTENSIONS not empty', ALL_MEDIA_EXTENSIONS <> '');
end;

procedure TTestMPVConst.Test_ALL_MEDIA_EXTENSIONS_Contains_Video;
begin
  AssertTrue('ALL_MEDIA_EXTENSIONS contains VIDEO_EXTENSIONS',
    Pos('*.avi', ALL_MEDIA_EXTENSIONS) > 0);
end;

procedure TTestMPVConst.Test_ALL_MEDIA_EXTENSIONS_Contains_Audio;
begin
  AssertTrue('ALL_MEDIA_EXTENSIONS contains AUDIO_EXTENSIONS',
    Pos('*.mp3', ALL_MEDIA_EXTENSIONS) > 0);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Seek Constants Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_SEEK_RELATIVE;
begin
  AssertEquals('SEEK_RELATIVE', 'relative', SEEK_RELATIVE);
end;

procedure TTestMPVConst.Test_SEEK_ABSOLUTE;
begin
  AssertEquals('SEEK_ABSOLUTE', 'absolute', SEEK_ABSOLUTE);
end;

procedure TTestMPVConst.Test_SEEK_ABSOLUTE_PERCENT;
begin
  AssertEquals('SEEK_ABSOLUTE_PERCENT', 'absolute-percent', SEEK_ABSOLUTE_PERCENT);
end;

procedure TTestMPVConst.Test_SEEK_RELATIVE_PERCENT;
begin
  AssertEquals('SEEK_RELATIVE_PERCENT', 'relative-percent', SEEK_RELATIVE_PERCENT);
end;

procedure TTestMPVConst.Test_SEEK_KEYFRAMES;
begin
  AssertEquals('SEEK_KEYFRAMES', 'keyframes', SEEK_KEYFRAMES);
end;

procedure TTestMPVConst.Test_SEEK_EXACT;
begin
  AssertEquals('SEEK_EXACT', 'exact', SEEK_EXACT);
end;

procedure TTestMPVConst.Test_SEEK_SMALL;
begin
  AssertEquals('SEEK_SMALL', 5, SEEK_SMALL);
end;

procedure TTestMPVConst.Test_SEEK_MEDIUM;
begin
  AssertEquals('SEEK_MEDIUM', 30, SEEK_MEDIUM);
end;

procedure TTestMPVConst.Test_SEEK_LARGE;
begin
  AssertEquals('SEEK_LARGE', 60, SEEK_LARGE);
end;

procedure TTestMPVConst.Test_SEEK_Increments_Increasing;
begin
  AssertTrue('SEEK_SMALL < SEEK_MEDIUM', SEEK_SMALL < SEEK_MEDIUM);
  AssertTrue('SEEK_MEDIUM < SEEK_LARGE', SEEK_MEDIUM < SEEK_LARGE);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  DVD Menu Commands Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_DVD_MENU_ROOT;
begin
  AssertTrue('DVD_MENU_ROOT not empty', DVD_MENU_ROOT <> '');
end;

procedure TTestMPVConst.Test_DVD_MENU_TITLE;
begin
  AssertTrue('DVD_MENU_TITLE not empty', DVD_MENU_TITLE <> '');
end;

procedure TTestMPVConst.Test_DVD_MENU_SELECT;
begin
  AssertEquals('DVD_MENU_SELECT', 'select', DVD_MENU_SELECT);
end;

procedure TTestMPVConst.Test_DVD_MENU_UP;
begin
  AssertEquals('DVD_MENU_UP', 'up', DVD_MENU_UP);
end;

procedure TTestMPVConst.Test_DVD_MENU_DOWN;
begin
  AssertEquals('DVD_MENU_DOWN', 'down', DVD_MENU_DOWN);
end;

procedure TTestMPVConst.Test_DVD_MENU_LEFT;
begin
  AssertEquals('DVD_MENU_LEFT', 'left', DVD_MENU_LEFT);
end;

procedure TTestMPVConst.Test_DVD_MENU_RIGHT;
begin
  AssertEquals('DVD_MENU_RIGHT', 'right', DVD_MENU_RIGHT);
end;

procedure TTestMPVConst.Test_DVD_MENU_MOUSE;
begin
  AssertEquals('DVD_MENU_MOUSE', 'mouse', DVD_MENU_MOUSE);
end;

procedure TTestMPVConst.Test_DVD_MENU_PREV;
begin
  AssertEquals('DVD_MENU_PREV', 'prev', DVD_MENU_PREV);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Screenshot Formats Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_SCREENSHOT_JPG;
begin
  AssertEquals('SCREENSHOT_JPG', 'jpg', SCREENSHOT_JPG);
end;

procedure TTestMPVConst.Test_SCREENSHOT_PNG;
begin
  AssertEquals('SCREENSHOT_PNG', 'png', SCREENSHOT_PNG);
end;

procedure TTestMPVConst.Test_SCREENSHOT_WEBP;
begin
  AssertEquals('SCREENSHOT_WEBP', 'webp', SCREENSHOT_WEBP);
end;

procedure TTestMPVConst.Test_SCREENSHOT_DEFAULT;
begin
  AssertEquals('SCREENSHOT_DEFAULT = SCREENSHOT_PNG', SCREENSHOT_PNG, SCREENSHOT_DEFAULT);
end;

procedure TTestMPVConst.Test_SCREENSHOT_VIDEO;
begin
  AssertEquals('SCREENSHOT_VIDEO', 'video', SCREENSHOT_VIDEO);
end;

procedure TTestMPVConst.Test_SCREENSHOT_SUBTITLES;
begin
  AssertEquals('SCREENSHOT_SUBTITLES', 'subtitles', SCREENSHOT_SUBTITLES);
end;

procedure TTestMPVConst.Test_SCREENSHOT_WINDOW;
begin
  AssertEquals('SCREENSHOT_WINDOW', 'window', SCREENSHOT_WINDOW);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Timeout Constants Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_EVENT_TIMEOUT_MS;
begin
  AssertEquals('EVENT_TIMEOUT_MS', 100, EVENT_TIMEOUT_MS);
  AssertTrue('EVENT_TIMEOUT_MS > 0', EVENT_TIMEOUT_MS > 0);
end;

procedure TTestMPVConst.Test_NETWORK_TIMEOUT_SEC;
begin
  AssertEquals('NETWORK_TIMEOUT_SEC', 30, NETWORK_TIMEOUT_SEC);
  AssertTrue('NETWORK_TIMEOUT_SEC > 0', NETWORK_TIMEOUT_SEC > 0);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Radio URLs Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_ICECAST_DIR_URL;
begin
  AssertTrue('ICECAST_DIR_URL starts with http://',
    Copy(ICECAST_DIR_URL, 1, 7) = 'http://');
  AssertTrue('ICECAST_DIR_URL contains yp.xml',
    Pos('yp.xml', ICECAST_DIR_URL) > 0);
end;

procedure TTestMPVConst.Test_RADIO_BROWSER_URL;
begin
  AssertTrue('RADIO_BROWSER_URL starts with https://',
    Copy(RADIO_BROWSER_URL, 1, 8) = 'https://');
  AssertTrue('RADIO_BROWSER_URL contains radio-browser',
    Pos('radio-browser', RADIO_BROWSER_URL) > 0);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Config Filenames Tests
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestMPVConst.Test_CONFIG_FILENAME;
begin
  AssertEquals('CONFIG_FILENAME', 'config.ini', CONFIG_FILENAME);
end;

procedure TTestMPVConst.Test_HISTORY_FILENAME;
begin
  AssertEquals('HISTORY_FILENAME', 'history.ini', HISTORY_FILENAME);
end;

procedure TTestMPVConst.Test_BOOKMARKS_FILENAME;
begin
  AssertEquals('BOOKMARKS_FILENAME', 'bookmarks.ini', BOOKMARKS_FILENAME);
end;

procedure TTestMPVConst.Test_FAVORITES_FILENAME;
begin
  AssertEquals('FAVORITES_FILENAME', 'favorites.ini', FAVORITES_FILENAME);
end;

procedure TTestMPVConst.Test_SHORTCUTS_FILENAME;
begin
  AssertEquals('SHORTCUTS_FILENAME', 'shortcuts.ini', SHORTCUTS_FILENAME);
end;

procedure TTestMPVConst.Test_ConfigFilenames_EndWith_INI;
begin
  AssertTrue('CONFIG_FILENAME ends with .ini',
    Copy(CONFIG_FILENAME, Length(CONFIG_FILENAME) - 3, 4) = '.ini');
  AssertTrue('HISTORY_FILENAME ends with .ini',
    Copy(HISTORY_FILENAME, Length(HISTORY_FILENAME) - 3, 4) = '.ini');
  AssertTrue('BOOKMARKS_FILENAME ends with .ini',
    Copy(BOOKMARKS_FILENAME, Length(BOOKMARKS_FILENAME) - 3, 4) = '.ini');
  AssertTrue('FAVORITES_FILENAME ends with .ini',
    Copy(FAVORITES_FILENAME, Length(FAVORITES_FILENAME) - 3, 4) = '.ini');
  AssertTrue('SHORTCUTS_FILENAME ends with .ini',
    Copy(SHORTCUTS_FILENAME, Length(SHORTCUTS_FILENAME) - 3, 4) = '.ini');
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  Test Registration
  ═══════════════════════════════════════════════════════════════════════════════ }

initialization
  RegisterTest(TTestMPVConst);

end.
