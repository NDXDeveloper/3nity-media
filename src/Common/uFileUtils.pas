{ ═══════════════════════════════════════════════════════════════════════════════
  uFileUtils.pas - File Utility Functions

  Part of 3nity Media - Lazarus Edition

  This unit provides common file utility functions:
  - File size retrieval
  - Path manipulation
  - String case selection

  Phase 25: Log Window & File Utilities

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{ Get file size in bytes (64-bit) }
function GetFileSize64(const FileName: string): Int64;

{ Extract file extension without the leading dot }
function ExtractFileExtNoDot(const FileName: string): string;

{ Extract file path ensuring it ends with a path delimiter }
function ExtractFilePathWithDelimiter(const FileName: string): string;

{ Case-insensitive string selection from array, returns -1 if not found }
function StringToCaseSelect(const Selector: string; const CaseList: array of string): Integer;

{ Check if a file is a media file based on extension }
function IsMediaFileExt(const FileName: string): Boolean;

{ Check if a file is an audio file based on extension }
function IsAudioFileExt(const FileName: string): Boolean;

{ Check if a file is a video file based on extension }
function IsVideoFileExt(const FileName: string): Boolean;

{ Check if a file is a playlist file based on extension }
function IsPlaylistFileExt(const FileName: string): Boolean;

{ Check if a file is a subtitle file based on extension }
function IsSubtitleFileExt(const FileName: string): Boolean;

{ Format file size to human readable string }
function FormatFileSize(Size: Int64): string;

{ Get a unique temporary filename }
function GetTempFileName(const Prefix: string = 'tmp'): string;

implementation

uses
  StrUtils;

const
  { Audio file extensions }
  AUDIO_EXTENSIONS: array[0..18] of string = (
    'mp3', 'wav', 'ogg', 'flac', 'aac', 'm4a', 'wma', 'opus',
    'ape', 'mka', 'ac3', 'dts', 'aiff', 'au', 'mid', 'midi',
    'mod', 'xm', 'it'
  );

  { Video file extensions }
  VIDEO_EXTENSIONS: array[0..20] of string = (
    'mp4', 'mkv', 'avi', 'wmv', 'mov', 'flv', 'webm', 'mpg',
    'mpeg', 'm4v', '3gp', 'ogv', 'ts', 'm2ts', 'mts', 'vob',
    'divx', 'xvid', 'rm', 'rmvb', 'asf'
  );

  { Playlist file extensions }
  PLAYLIST_EXTENSIONS: array[0..4] of string = (
    'm3u', 'm3u8', 'pls', 'xspf', 'asx'
  );

  { Subtitle file extensions }
  SUBTITLE_EXTENSIONS: array[0..7] of string = (
    'srt', 'ass', 'ssa', 'sub', 'vtt', 'idx', 'sup', 'smi'
  );

function GetFileSize64(const FileName: string): Int64;
var
  Stream: TFileStream;
begin
  Result := -1;
  if not FileExists(FileName) then Exit;

  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      Result := Stream.Size;
    finally
      Stream.Free;
    end;
  except
    Result := -1;
  end;
end;

function ExtractFileExtNoDot(const FileName: string): string;
begin
  Result := ExtractFileExt(FileName);
  if (Result <> '') and (Result[1] = '.') then
    Result := Copy(Result, 2, Length(Result) - 1);
end;

function ExtractFilePathWithDelimiter(const FileName: string): string;
begin
  Result := ExtractFilePath(FileName);
  if (Result <> '') and (Result[Length(Result)] <> PathDelim) then
    Result := Result + PathDelim;
end;

function StringToCaseSelect(const Selector: string; const CaseList: array of string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(CaseList) to High(CaseList) do
  begin
    if CompareText(Selector, CaseList[I]) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function IsAudioFileExt(const FileName: string): Boolean;
var
  Ext: string;
  I: Integer;
begin
  Result := False;
  Ext := LowerCase(ExtractFileExtNoDot(FileName));
  for I := Low(AUDIO_EXTENSIONS) to High(AUDIO_EXTENSIONS) do
  begin
    if Ext = AUDIO_EXTENSIONS[I] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function IsVideoFileExt(const FileName: string): Boolean;
var
  Ext: string;
  I: Integer;
begin
  Result := False;
  Ext := LowerCase(ExtractFileExtNoDot(FileName));
  for I := Low(VIDEO_EXTENSIONS) to High(VIDEO_EXTENSIONS) do
  begin
    if Ext = VIDEO_EXTENSIONS[I] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function IsPlaylistFileExt(const FileName: string): Boolean;
var
  Ext: string;
  I: Integer;
begin
  Result := False;
  Ext := LowerCase(ExtractFileExtNoDot(FileName));
  for I := Low(PLAYLIST_EXTENSIONS) to High(PLAYLIST_EXTENSIONS) do
  begin
    if Ext = PLAYLIST_EXTENSIONS[I] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function IsSubtitleFileExt(const FileName: string): Boolean;
var
  Ext: string;
  I: Integer;
begin
  Result := False;
  Ext := LowerCase(ExtractFileExtNoDot(FileName));
  for I := Low(SUBTITLE_EXTENSIONS) to High(SUBTITLE_EXTENSIONS) do
  begin
    if Ext = SUBTITLE_EXTENSIONS[I] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function IsMediaFileExt(const FileName: string): Boolean;
begin
  Result := IsAudioFileExt(FileName) or IsVideoFileExt(FileName);
end;

function FormatFileSize(Size: Int64): string;
const
  KB = 1024;
  MB = KB * 1024;
  GB = MB * 1024;
begin
  if Size < 0 then
    Result := '?'
  else if Size < KB then
    Result := Format('%d B', [Size])
  else if Size < MB then
    Result := Format('%.1f KB', [Size / KB])
  else if Size < GB then
    Result := Format('%.1f MB', [Size / MB])
  else
    Result := Format('%.2f GB', [Size / GB]);
end;

function GetTempFileName(const Prefix: string): string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir) +
            Prefix + '_' + FormatDateTime('yyyymmdd_hhnnsszzz', Now) + '.tmp';
end;

end.
