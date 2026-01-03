{ ===============================================================================
  uMediaInfo.pas - Media Information Dialog

  Part of 3nity Media - Lazarus Edition

  This dialog displays detailed information about the currently playing media:
  - File information (name, path, size, format)
  - Video information (codec, resolution, FPS, bitrate)
  - Audio information (codec, channels, sample rate, bitrate)
  - Metadata (title, artist, album, etc.)

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uMediaInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons, Math, Process,
  uMPVEngine, uLocale;

type
  { TfrmMediaInfo }
  TfrmMediaInfo = class(TForm)
    PageControl: TPageControl;
    tsFile: TTabSheet;
    tsVideo: TTabSheet;
    tsAudio: TTabSheet;
    tsMetadata: TTabSheet;

    pnlBottom: TPanel;
    btnClose: TButton;
    btnCopyAll: TButton;

    { File tab }
    lblFileName: TLabel;
    edtFileName: TEdit;
    lblFilePath: TLabel;
    edtFilePath: TEdit;
    lblFileSize: TLabel;
    edtFileSize: TEdit;
    lblDuration: TLabel;
    edtDuration: TEdit;
    lblFormat: TLabel;
    edtFormat: TEdit;
    lblBitrate: TLabel;
    edtBitrate: TEdit;

    { Video tab }
    lblVideoCodec: TLabel;
    edtVideoCodec: TEdit;
    lblResolution: TLabel;
    edtResolution: TEdit;
    lblFPS: TLabel;
    edtFPS: TEdit;
    lblVideoBitrate: TLabel;
    edtVideoBitrate: TEdit;
    lblPixelFormat: TLabel;
    edtPixelFormat: TEdit;
    lblAspectRatio: TLabel;
    edtAspectRatio: TEdit;

    { Audio tab }
    lblAudioCodec: TLabel;
    edtAudioCodec: TEdit;
    lblChannels: TLabel;
    edtChannels: TEdit;
    lblSampleRate: TLabel;
    edtSampleRate: TEdit;
    lblAudioBitrate: TLabel;
    edtAudioBitrate: TEdit;
    lblAudioLang: TLabel;
    edtAudioLang: TEdit;

    { Metadata tab }
    lvMetadata: TListView;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyAllClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);

  private
    FMPVEngine: TMPVEngine;
    FFileName: string;

    procedure LoadFileInfo;
    procedure LoadVideoInfo;
    procedure LoadAudioInfo;
    procedure LoadMetadata;
    procedure RefreshCurrentTab;
    function FormatFileSize(Size: Int64): string;
    function FormatDuration(Seconds: Double): string;
    function FormatBitrate(Bitrate: Integer): string;

  public
    procedure ApplyLocale;
    procedure LoadMediaInfo(AEngine: TMPVEngine; const AFileName: string);
  end;

var
  frmMediaInfo: TfrmMediaInfo;

implementation

{$R *.lfm}

uses
  Clipbrd;

{ ===============================================================================
  FORM EVENTS
  =============================================================================== }

procedure TfrmMediaInfo.FormCreate(Sender: TObject);
begin
  FMPVEngine := nil;
  FFileName := '';
  PageControl.ActivePageIndex := 0;
  ApplyLocale;
end;

procedure TfrmMediaInfo.ApplyLocale;
begin
  Caption := _T('MediaInfo', 'Title', 'Media Information');

  { Tab sheets }
  tsFile.Caption := _T('MediaInfo', 'FileTab', 'File');
  tsVideo.Caption := _T('MediaInfo', 'VideoTab', 'Video');
  tsAudio.Caption := _T('MediaInfo', 'AudioTab', 'Audio');
  tsMetadata.Caption := _T('MediaInfo', 'MetadataTab', 'Metadata');

  { File tab labels }
  lblFileName.Caption := _T('MediaInfo', 'FileName', 'File Name:');
  lblFilePath.Caption := _T('MediaInfo', 'FilePath', 'Path:');
  lblFileSize.Caption := _T('MediaInfo', 'FileSize', 'Size:');
  lblDuration.Caption := _T('MediaInfo', 'Duration', 'Duration:');
  lblFormat.Caption := _T('MediaInfo', 'Format', 'Format:');
  lblBitrate.Caption := _T('MediaInfo', 'Bitrate', 'Bitrate:');

  { Video tab labels }
  lblVideoCodec.Caption := _T('MediaInfo', 'Codec', 'Codec:');
  lblResolution.Caption := _T('MediaInfo', 'Resolution', 'Resolution:');
  lblFPS.Caption := _T('MediaInfo', 'FrameRate', 'Frame Rate:');
  lblVideoBitrate.Caption := _T('MediaInfo', 'Bitrate', 'Bitrate:');
  lblPixelFormat.Caption := _T('MediaInfo', 'PixelFormat', 'Pixel Format:');
  lblAspectRatio.Caption := _T('MediaInfo', 'AspectRatio', 'Aspect Ratio:');

  { Audio tab labels }
  lblAudioCodec.Caption := _T('MediaInfo', 'Codec', 'Codec:');
  lblChannels.Caption := _T('MediaInfo', 'Channels', 'Channels:');
  lblSampleRate.Caption := _T('MediaInfo', 'SampleRate', 'Sample Rate:');
  lblAudioBitrate.Caption := _T('MediaInfo', 'Bitrate', 'Bitrate:');
  lblAudioLang.Caption := _T('MediaInfo', 'Language', 'Language:');

  { Metadata list columns }
  if lvMetadata.Columns.Count >= 2 then
  begin
    lvMetadata.Columns[0].Caption := _T('MediaInfo', 'Property', 'Property');
    lvMetadata.Columns[1].Caption := _T('MediaInfo', 'Value', 'Value');
  end;

  { Buttons }
  btnClose.Caption := _T('Button', 'Close', 'Close');
  btnCopyAll.Caption := _T('MediaInfo', 'CopyAll', 'Copy All');
end;

procedure TfrmMediaInfo.FormShow(Sender: TObject);
begin
  ApplyLocale;
  { Info is loaded via LoadMediaInfo }
end;

procedure TfrmMediaInfo.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMediaInfo.btnCopyAllClick(Sender: TObject);
var
  Info: TStringList;
  I: Integer;
begin
  Info := TStringList.Create;
  try
    Info.Add('=== File Information ===');
    Info.Add('File Name: ' + edtFileName.Text);
    Info.Add('Path: ' + edtFilePath.Text);
    Info.Add('Size: ' + edtFileSize.Text);
    Info.Add('Duration: ' + edtDuration.Text);
    Info.Add('Format: ' + edtFormat.Text);
    Info.Add('Bitrate: ' + edtBitrate.Text);
    Info.Add('');

    Info.Add('=== Video Information ===');
    Info.Add('Codec: ' + edtVideoCodec.Text);
    Info.Add('Resolution: ' + edtResolution.Text);
    Info.Add('Frame Rate: ' + edtFPS.Text);
    Info.Add('Bitrate: ' + edtVideoBitrate.Text);
    Info.Add('Pixel Format: ' + edtPixelFormat.Text);
    Info.Add('Aspect Ratio: ' + edtAspectRatio.Text);
    Info.Add('');

    Info.Add('=== Audio Information ===');
    Info.Add('Codec: ' + edtAudioCodec.Text);
    Info.Add('Channels: ' + edtChannels.Text);
    Info.Add('Sample Rate: ' + edtSampleRate.Text);
    Info.Add('Bitrate: ' + edtAudioBitrate.Text);
    Info.Add('Language: ' + edtAudioLang.Text);
    Info.Add('');

    Info.Add('=== Metadata ===');
    for I := 0 to lvMetadata.Items.Count - 1 do
      Info.Add(lvMetadata.Items[I].Caption + ': ' + lvMetadata.Items[I].SubItems[0]);

    Clipboard.AsText := Info.Text;
    ShowMessage(_T('MediaInfo', 'CopiedToClipboard', 'Media information copied to clipboard.'));
  finally
    Info.Free;
  end;
end;

procedure TfrmMediaInfo.PageControlChange(Sender: TObject);
begin
  RefreshCurrentTab;
end;

procedure TfrmMediaInfo.RefreshCurrentTab;
begin
  case PageControl.ActivePageIndex of
    0: LoadFileInfo;
    1: LoadVideoInfo;
    2: LoadAudioInfo;
    3: LoadMetadata;
  end;
end;

{ ===============================================================================
  PUBLIC METHODS
  =============================================================================== }

procedure TfrmMediaInfo.LoadMediaInfo(AEngine: TMPVEngine; const AFileName: string);
begin
  FMPVEngine := AEngine;
  FFileName := AFileName;

  LoadFileInfo;
  LoadVideoInfo;
  LoadAudioInfo;
  LoadMetadata;
end;

{ ===============================================================================
  PRIVATE METHODS
  =============================================================================== }

procedure TfrmMediaInfo.LoadFileInfo;
var
  FileInfo: TSearchRec;
  FileSize: Int64;
begin
  edtFileName.Text := ExtractFileName(FFileName);
  edtFilePath.Text := ExtractFilePath(FFileName);

  { Get file size }
  FileSize := 0;
  if FileExists(FFileName) then
  begin
    if FindFirst(FFileName, faAnyFile, FileInfo) = 0 then
    begin
      FileSize := FileInfo.Size;
      FindClose(FileInfo);
    end;
  end;
  edtFileSize.Text := FormatFileSize(FileSize);

  { Duration and format from MPV }
  if FMPVEngine <> nil then
  begin
    edtDuration.Text := FormatDuration(FMPVEngine.Duration);
    edtFormat.Text := FMPVEngine.GetPropertyString('file-format');
    edtBitrate.Text := FormatBitrate(FMPVEngine.GetPropertyInt('video-bitrate') +
                                      FMPVEngine.GetPropertyInt('audio-bitrate'));
  end
  else
  begin
    edtDuration.Text := _T('MediaInfo', 'NA', 'N/A');
    edtFormat.Text := _T('MediaInfo', 'NA', 'N/A');
    edtBitrate.Text := _T('MediaInfo', 'NA', 'N/A');
  end;
end;

procedure TfrmMediaInfo.LoadVideoInfo;
var
  W, H: Integer;
  FPS: Double;
begin
  if FMPVEngine = nil then
  begin
    edtVideoCodec.Text := _T('MediaInfo', 'NA', 'N/A');
    edtResolution.Text := _T('MediaInfo', 'NA', 'N/A');
    edtFPS.Text := _T('MediaInfo', 'NA', 'N/A');
    edtVideoBitrate.Text := _T('MediaInfo', 'NA', 'N/A');
    edtPixelFormat.Text := _T('MediaInfo', 'NA', 'N/A');
    edtAspectRatio.Text := _T('MediaInfo', 'NA', 'N/A');
    Exit;
  end;

  edtVideoCodec.Text := FMPVEngine.GetPropertyString('video-codec');
  if edtVideoCodec.Text = '' then
    edtVideoCodec.Text := FMPVEngine.GetPropertyString('video-format');

  W := FMPVEngine.GetPropertyInt('width');
  H := FMPVEngine.GetPropertyInt('height');
  if (W > 0) and (H > 0) then
    edtResolution.Text := Format('%d x %d', [W, H])
  else
    edtResolution.Text := _T('MediaInfo', 'NA', 'N/A');

  FPS := FMPVEngine.GetPropertyDouble('container-fps');
  if FPS > 0 then
    edtFPS.Text := Format('%.3f fps', [FPS])
  else
    edtFPS.Text := _T('MediaInfo', 'NA', 'N/A');

  edtVideoBitrate.Text := FormatBitrate(FMPVEngine.GetPropertyInt('video-bitrate'));

  edtPixelFormat.Text := FMPVEngine.GetPropertyString('video-params/pixelformat');
  if edtPixelFormat.Text = '' then
    edtPixelFormat.Text := _T('MediaInfo', 'NA', 'N/A');

  edtAspectRatio.Text := FMPVEngine.GetPropertyString('video-params/aspect');
  if edtAspectRatio.Text = '' then
  begin
    if (W > 0) and (H > 0) then
      edtAspectRatio.Text := Format('%.2f:1', [W / H])
    else
      edtAspectRatio.Text := _T('MediaInfo', 'NA', 'N/A');
  end;
end;

procedure TfrmMediaInfo.LoadAudioInfo;
var
  Channels, SampleRate: Integer;
begin
  if FMPVEngine = nil then
  begin
    edtAudioCodec.Text := _T('MediaInfo', 'NA', 'N/A');
    edtChannels.Text := _T('MediaInfo', 'NA', 'N/A');
    edtSampleRate.Text := _T('MediaInfo', 'NA', 'N/A');
    edtAudioBitrate.Text := _T('MediaInfo', 'NA', 'N/A');
    edtAudioLang.Text := _T('MediaInfo', 'NA', 'N/A');
    Exit;
  end;

  edtAudioCodec.Text := FMPVEngine.GetPropertyString('audio-codec');
  if edtAudioCodec.Text = '' then
    edtAudioCodec.Text := FMPVEngine.GetPropertyString('audio-codec-name');

  Channels := FMPVEngine.GetPropertyInt('audio-params/channel-count');
  if Channels > 0 then
  begin
    case Channels of
      1: edtChannels.Text := _T('MediaInfo', 'ChannelMono', '1 (Mono)');
      2: edtChannels.Text := _T('MediaInfo', 'ChannelStereo', '2 (Stereo)');
      6: edtChannels.Text := _T('MediaInfo', 'Channel51', '6 (5.1 Surround)');
      8: edtChannels.Text := _T('MediaInfo', 'Channel71', '8 (7.1 Surround)');
      else edtChannels.Text := IntToStr(Channels);
    end;
  end
  else
    edtChannels.Text := _T('MediaInfo', 'NA', 'N/A');

  SampleRate := FMPVEngine.GetPropertyInt('audio-params/samplerate');
  if SampleRate > 0 then
    edtSampleRate.Text := Format('%d Hz', [SampleRate])
  else
    edtSampleRate.Text := _T('MediaInfo', 'NA', 'N/A');

  edtAudioBitrate.Text := FormatBitrate(FMPVEngine.GetPropertyInt('audio-bitrate'));

  edtAudioLang.Text := FMPVEngine.GetPropertyString('current-tracks/audio/lang');
  if edtAudioLang.Text = '' then
    edtAudioLang.Text := _T('MediaInfo', 'Unknown', 'Unknown');
end;

procedure TfrmMediaInfo.LoadMetadata;
var
  Item: TListItem;
  AProcess: TProcess;
  Output: TStringList;
  AddedKeys: TStringList;
  Line, Key, Value, TagPrefix: string;
  I, P: Integer;
  FoundMetadata: Boolean;

  procedure AddMetadataItem(const AKey, AValue: string);
  var
    DisplayKey, LowerKey: string;
  begin
    if AValue = '' then Exit;

    { Check for duplicates (case-insensitive) }
    LowerKey := LowerCase(AKey);
    if AddedKeys.IndexOf(LowerKey) >= 0 then Exit;
    AddedKeys.Add(LowerKey);

    { Format the key for display (capitalize, replace underscores) }
    DisplayKey := AKey;
    if Length(DisplayKey) > 0 then
    begin
      DisplayKey[1] := UpCase(DisplayKey[1]);
      DisplayKey := StringReplace(DisplayKey, '_', ' ', [rfReplaceAll]);
    end;

    Item := lvMetadata.Items.Add;
    Item.Caption := DisplayKey;
    Item.SubItems.Add(AValue);
  end;

begin
  lvMetadata.Items.Clear;
  FoundMetadata := False;
  AddedKeys := TStringList.Create;
  try

  { First try using ffprobe - it handles all container formats well }
  if FileExists(FFileName) and (Pos('://', FFileName) = 0) then
  begin
    AProcess := TProcess.Create(nil);
    Output := TStringList.Create;
    try
      AProcess.Executable := 'ffprobe';
      AProcess.Parameters.Add('-v');
      AProcess.Parameters.Add('quiet');
      AProcess.Parameters.Add('-show_entries');
      { Get both format tags and stream tags (MKV often has metadata in streams) }
      AProcess.Parameters.Add('format_tags:stream_tags:format=duration,bit_rate:stream=codec_name,codec_type,width,height,sample_rate,channels');
      AProcess.Parameters.Add('-of');
      AProcess.Parameters.Add('flat');
      AProcess.Parameters.Add(FFileName);
      AProcess.Options := [poUsePipes, poNoConsole, poWaitOnExit];

      try
        AProcess.Execute;
        Output.LoadFromStream(AProcess.Output);

        { Parse output - handles both format.tags.key and streams.stream.N.tags.key }
        for I := 0 to Output.Count - 1 do
        begin
          Line := Output[I];
          P := Pos('=', Line);
          if P > 0 then
          begin
            Key := Copy(Line, 1, P - 1);
            Value := Copy(Line, P + 1, Length(Line) - P);
            { Remove quotes }
            if (Length(Value) >= 2) and (Value[1] = '"') and (Value[Length(Value)] = '"') then
              Value := Copy(Value, 2, Length(Value) - 2);

            if Value = '' then Continue;

            { Parse format.tags.* }
            TagPrefix := 'format.tags.';
            if Pos(TagPrefix, Key) = 1 then
            begin
              Key := Copy(Key, Length(TagPrefix) + 1, Length(Key));
              AddMetadataItem(Key, Value);
              FoundMetadata := True;
            end
            { Parse streams.stream.N.tags.* - extract the tag name }
            else if Pos('streams.stream.', Key) = 1 then
            begin
              { Look for .tags. in the key }
              if Pos('.tags.', Key) > 0 then
              begin
                Key := Copy(Key, Pos('.tags.', Key) + 6, Length(Key));
                { Avoid duplicates - check if already added }
                AddMetadataItem(Key, Value);
                FoundMetadata := True;
              end;
            end;
          end;
        end;
      except
        { ffprobe failed, will try MPV fallback }
      end;
    finally
      Output.Free;
      AProcess.Free;
    end;
  end;

  { Fallback to MPV metadata if ffprobe didn't find anything }
  if (not FoundMetadata) and (FMPVEngine <> nil) then
  begin
    { Try common metadata keys via MPV }
    Value := FMPVEngine.GetPropertyString('metadata/by-key/title');
    if Value <> '' then begin AddMetadataItem('Title', Value); FoundMetadata := True; end;

    Value := FMPVEngine.GetPropertyString('metadata/by-key/artist');
    if Value <> '' then begin AddMetadataItem('Artist', Value); FoundMetadata := True; end;

    Value := FMPVEngine.GetPropertyString('metadata/by-key/album');
    if Value <> '' then begin AddMetadataItem('Album', Value); FoundMetadata := True; end;

    Value := FMPVEngine.GetPropertyString('metadata/by-key/album_artist');
    if Value <> '' then begin AddMetadataItem('Album Artist', Value); FoundMetadata := True; end;

    Value := FMPVEngine.GetPropertyString('metadata/by-key/date');
    if Value <> '' then begin AddMetadataItem('Date', Value); FoundMetadata := True; end;

    Value := FMPVEngine.GetPropertyString('metadata/by-key/track');
    if Value <> '' then begin AddMetadataItem('Track', Value); FoundMetadata := True; end;

    Value := FMPVEngine.GetPropertyString('metadata/by-key/genre');
    if Value <> '' then begin AddMetadataItem('Genre', Value); FoundMetadata := True; end;

    Value := FMPVEngine.GetPropertyString('metadata/by-key/comment');
    if Value <> '' then begin AddMetadataItem('Comment', Value); FoundMetadata := True; end;

    Value := FMPVEngine.GetPropertyString('metadata/by-key/composer');
    if Value <> '' then begin AddMetadataItem('Composer', Value); FoundMetadata := True; end;

    Value := FMPVEngine.GetPropertyString('metadata/by-key/performer');
    if Value <> '' then begin AddMetadataItem('Performer', Value); FoundMetadata := True; end;

    Value := FMPVEngine.GetPropertyString('metadata/by-key/copyright');
    if Value <> '' then begin AddMetadataItem('Copyright', Value); FoundMetadata := True; end;

    Value := FMPVEngine.GetPropertyString('metadata/by-key/description');
    if Value <> '' then begin AddMetadataItem('Description', Value); FoundMetadata := True; end;
  end;

  { If still no metadata found }
  if not FoundMetadata then
  begin
    Item := lvMetadata.Items.Add;
    Item.Caption := _T('MediaInfo', 'NoMetadata', '(No metadata)');
    Item.SubItems.Add('');
  end;

  finally
    AddedKeys.Free;
  end;
end;

function TfrmMediaInfo.FormatFileSize(Size: Int64): string;
const
  KB = 1024;
  MB = KB * 1024;
  GB = MB * 1024;
begin
  if Size = 0 then
    Result := _T('MediaInfo', 'NAStream', 'N/A (stream)')
  else if Size >= GB then
    Result := Format('%.2f GB', [Size / GB])
  else if Size >= MB then
    Result := Format('%.2f MB', [Size / MB])
  else if Size >= KB then
    Result := Format('%.2f KB', [Size / KB])
  else
    Result := Format('%d bytes', [Size]);
end;

function TfrmMediaInfo.FormatDuration(Seconds: Double): string;
var
  H, M, S: Integer;
begin
  if (Seconds <= 0) or IsNan(Seconds) or IsInfinite(Seconds) then
  begin
    Result := _T('MediaInfo', 'NALive', 'N/A (live)');
    Exit;
  end;

  H := Trunc(Seconds) div 3600;
  M := (Trunc(Seconds) mod 3600) div 60;
  S := Trunc(Seconds) mod 60;

  if H > 0 then
    Result := Format('%d:%02d:%02d', [H, M, S])
  else
    Result := Format('%d:%02d', [M, S]);
end;

function TfrmMediaInfo.FormatBitrate(Bitrate: Integer): string;
begin
  if Bitrate <= 0 then
    Result := _T('MediaInfo', 'NA', 'N/A')
  else if Bitrate >= 1000000 then
    Result := Format('%.2f Mbps', [Bitrate / 1000000])
  else if Bitrate >= 1000 then
    Result := Format('%d kbps', [Bitrate div 1000])
  else
    Result := Format('%d bps', [Bitrate]);
end;

end.
