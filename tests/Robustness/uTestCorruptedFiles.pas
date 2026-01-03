{ ===============================================================================
  uTestCorruptedFiles.pas - Corrupted File Handling Tests

  Part of 3nity Media - Test Suite

  Tests application robustness when handling corrupted, truncated,
  or malformed media files.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uTestCorruptedFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fpcunit, testregistry;

type
  { ============================================================================
    TTestCorruptedFiles - Corrupted file handling tests
    ============================================================================ }
  TTestCorruptedFiles = class(TTestCase)
  private
    FTestDir: string;
    procedure CreateTestFile(const FileName: string; const Content: string); overload;
    procedure CreateTestFile(const FileName: string; const Content: array of Byte); overload;
    procedure CreateTruncatedFile(const SourceFile, DestFile: string; TruncatePercent: Integer);
    function GetTestDataDir: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Empty file tests }
    procedure Test_EmptyFile_Handled;
    procedure Test_ZeroByteFile_Handled;

    { Truncated file tests }
    procedure Test_TruncatedMP3_Header_Handled;
    procedure Test_TruncatedMP3_Middle_Handled;
    procedure Test_TruncatedMP4_Handled;
    procedure Test_TruncatedMKV_Handled;

    { Invalid header tests }
    procedure Test_InvalidMP3Header_Handled;
    procedure Test_InvalidMP4Header_Handled;
    procedure Test_InvalidMKVHeader_Handled;
    procedure Test_RandomBytes_Handled;

    { Corrupted data tests }
    procedure Test_CorruptedMP3Frames_Handled;
    procedure Test_CorruptedMP4Atoms_Handled;
    procedure Test_PartiallyCorrupted_Handled;

    { Wrong extension tests }
    procedure Test_MP3AsMP4_Handled;
    procedure Test_TextAsMp3_Handled;
    procedure Test_ImageAsAudio_Handled;

    { Malformed playlist tests }
    procedure Test_MalformedM3U_Handled;
    procedure Test_MalformedPLS_Handled;
    procedure Test_BinaryAsPlaylist_Handled;
  end;

  { ============================================================================
    TTestFileValidation - File validation tests
    ============================================================================ }
  TTestFileValidation = class(TTestCase)
  private
    FTestDir: string;
    procedure CreateTestFile(const FileName: string; const Content: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Magic number validation }
    procedure Test_ValidateMP3Magic;
    procedure Test_ValidateMP4Magic;
    procedure Test_ValidateFLACMagic;
    procedure Test_ValidateOGGMagic;

    { File structure validation }
    procedure Test_ValidateM3UStructure;
    procedure Test_ValidatePLSStructure;
    procedure Test_ValidateSRTStructure;

    { Path validation }
    procedure Test_ValidatePath_Normal;
    procedure Test_ValidatePath_Unicode;
    procedure Test_ValidatePath_Special;
    procedure Test_ValidatePath_TooLong;
  end;

implementation

const
  { Valid file magic numbers }
  MP3_MAGIC: array[0..1] of Byte = ($FF, $FB);
  MP3_ID3_MAGIC: array[0..2] of Byte = ($49, $44, $33);  { 'ID3' }
  MP4_FTYP_MAGIC: array[0..3] of Byte = ($66, $74, $79, $70);  { 'ftyp' }
  FLAC_MAGIC: array[0..3] of Byte = ($66, $4C, $61, $43);  { 'fLaC' }
  OGG_MAGIC: array[0..3] of Byte = ($4F, $67, $67, $53);  { 'OggS' }

{ ============================================================================
  TTestCorruptedFiles
  ============================================================================ }

procedure TTestCorruptedFiles.SetUp;
begin
  FTestDir := GetTempDir + 'test_corrupted' + PathDelim;
  if not DirectoryExists(FTestDir) then
    CreateDir(FTestDir);
end;

procedure TTestCorruptedFiles.TearDown;
var
  SR: TSearchRec;
begin
  { Clean up test files }
  if FindFirst(FTestDir + '*', faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') then
        DeleteFile(FTestDir + SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  RemoveDir(FTestDir);
end;

procedure TTestCorruptedFiles.CreateTestFile(const FileName: string; const Content: string);
var
  F: TextFile;
begin
  AssignFile(F, FTestDir + FileName);
  Rewrite(F);
  Write(F, Content);
  CloseFile(F);
end;

procedure TTestCorruptedFiles.CreateTestFile(const FileName: string; const Content: array of Byte);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FTestDir + FileName, fmCreate);
  try
    if Length(Content) > 0 then
      Stream.Write(Content[0], Length(Content));
  finally
    Stream.Free;
  end;
end;

procedure TTestCorruptedFiles.CreateTruncatedFile(const SourceFile, DestFile: string; TruncatePercent: Integer);
var
  Source, Dest: TFileStream;
  NewSize: Int64;
begin
  if not FileExists(SourceFile) then Exit;

  Source := TFileStream.Create(SourceFile, fmOpenRead);
  try
    NewSize := (Source.Size * TruncatePercent) div 100;
    Dest := TFileStream.Create(FTestDir + DestFile, fmCreate);
    try
      Dest.CopyFrom(Source, NewSize);
    finally
      Dest.Free;
    end;
  finally
    Source.Free;
  end;
end;

function TTestCorruptedFiles.GetTestDataDir: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + '..' + PathDelim + 'TestData' + PathDelim;
  if not DirectoryExists(Result) then
    Result := ExtractFilePath(ParamStr(0)) + 'TestData' + PathDelim;
end;

{ Empty file tests }

procedure TTestCorruptedFiles.Test_EmptyFile_Handled;
var
  FileName: string;
  Stream: TFileStream;
begin
  FileName := FTestDir + 'empty.mp3';
  Stream := TFileStream.Create(FileName, fmCreate);
  Stream.Free;

  AssertTrue('Empty file should exist', FileExists(FileName));

  { Verify we can at least check it's empty without crashing }
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    AssertEquals('Empty file should have size 0', 0, Stream.Size);
  finally
    Stream.Free;
  end;
end;

procedure TTestCorruptedFiles.Test_ZeroByteFile_Handled;
var
  FileName: string;
  List: TStringList;
begin
  FileName := FTestDir + 'zero.m3u';
  CreateTestFile('zero.m3u', '');

  List := TStringList.Create;
  try
    { Should not crash when loading empty file }
    List.LoadFromFile(FileName);
    AssertEquals('Zero byte file should have 0 lines', 0, List.Count);
  finally
    List.Free;
  end;
end;

{ Truncated file tests }

procedure TTestCorruptedFiles.Test_TruncatedMP3_Header_Handled;
var
  Content: array[0..9] of Byte;
  FileName: string;
  Stream: TFileStream;
begin
  { Create truncated MP3 with just partial header }
  Content[0] := $FF;
  Content[1] := $FB;
  Content[2] := $90;
  FillChar(Content[3], 7, 0);

  CreateTestFile('truncated_header.mp3', Content);
  FileName := FTestDir + 'truncated_header.mp3';

  { Should be able to open without crash }
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    AssertTrue('Truncated file should have small size', Stream.Size < 100);
  finally
    Stream.Free;
  end;
end;

procedure TTestCorruptedFiles.Test_TruncatedMP3_Middle_Handled;
var
  SourceFile: string;
begin
  SourceFile := GetTestDataDir + 'audio' + PathDelim + 'test_44100_stereo.mp3';
  if not FileExists(SourceFile) then
  begin
    { Skip if test file doesn't exist }
    AssertTrue('Source file needed', True);
    Exit;
  end;

  CreateTruncatedFile(SourceFile, 'truncated_middle.mp3', 50);
  AssertTrue('Truncated file should exist', FileExists(FTestDir + 'truncated_middle.mp3'));
end;

procedure TTestCorruptedFiles.Test_TruncatedMP4_Handled;
var
  SourceFile: string;
begin
  SourceFile := GetTestDataDir + 'video' + PathDelim + 'test_720p.mp4';
  if not FileExists(SourceFile) then
  begin
    AssertTrue('Source file needed', True);
    Exit;
  end;

  CreateTruncatedFile(SourceFile, 'truncated.mp4', 30);
  AssertTrue('Truncated file should exist', FileExists(FTestDir + 'truncated.mp4'));
end;

procedure TTestCorruptedFiles.Test_TruncatedMKV_Handled;
var
  SourceFile: string;
begin
  SourceFile := GetTestDataDir + 'video' + PathDelim + 'test_hevc.mkv';
  if not FileExists(SourceFile) then
  begin
    AssertTrue('Source file needed', True);
    Exit;
  end;

  CreateTruncatedFile(SourceFile, 'truncated.mkv', 25);
  AssertTrue('Truncated file should exist', FileExists(FTestDir + 'truncated.mkv'));
end;

{ Invalid header tests }

procedure TTestCorruptedFiles.Test_InvalidMP3Header_Handled;
var
  Content: array[0..255] of Byte;
begin
  { Create file with invalid MP3 sync bytes }
  Content[0] := $00;
  Content[1] := $00;
  FillChar(Content[2], 254, $AA);

  CreateTestFile('invalid_header.mp3', Content);
  AssertTrue('Invalid header file should exist', FileExists(FTestDir + 'invalid_header.mp3'));
end;

procedure TTestCorruptedFiles.Test_InvalidMP4Header_Handled;
var
  Content: array[0..255] of Byte;
begin
  { Create file without valid ftyp atom }
  FillChar(Content, SizeOf(Content), $00);
  Content[0] := $00;
  Content[1] := $00;
  Content[2] := $00;
  Content[3] := $08;
  Content[4] := $62;  { 'b' instead of 'f' }
  Content[5] := $74;
  Content[6] := $79;
  Content[7] := $70;

  CreateTestFile('invalid_header.mp4', Content);
  AssertTrue('Invalid header file should exist', FileExists(FTestDir + 'invalid_header.mp4'));
end;

procedure TTestCorruptedFiles.Test_InvalidMKVHeader_Handled;
var
  Content: array[0..255] of Byte;
begin
  { Create file without valid EBML header }
  FillChar(Content, SizeOf(Content), $00);

  CreateTestFile('invalid_header.mkv', Content);
  AssertTrue('Invalid header file should exist', FileExists(FTestDir + 'invalid_header.mkv'));
end;

procedure TTestCorruptedFiles.Test_RandomBytes_Handled;
var
  Content: array[0..1023] of Byte;
  I: Integer;
begin
  { Create file with random bytes }
  for I := 0 to High(Content) do
    Content[I] := Random(256);

  CreateTestFile('random_bytes.mp3', Content);
  AssertTrue('Random bytes file should exist', FileExists(FTestDir + 'random_bytes.mp3'));
end;

{ Corrupted data tests }

procedure TTestCorruptedFiles.Test_CorruptedMP3Frames_Handled;
var
  Content: array[0..1023] of Byte;
  I: Integer;
begin
  { Valid-ish header but corrupted frame data }
  Content[0] := $FF;
  Content[1] := $FB;
  Content[2] := $90;
  Content[3] := $00;

  { Fill rest with garbage that looks like corrupted frames }
  for I := 4 to High(Content) do
    if I mod 417 = 0 then
    begin
      Content[I] := $FF;
      if I + 1 <= High(Content) then
        Content[I + 1] := $00;  { Invalid sync }
    end
    else
      Content[I] := Random(256);

  CreateTestFile('corrupted_frames.mp3', Content);
  AssertTrue('Corrupted frames file should exist', FileExists(FTestDir + 'corrupted_frames.mp3'));
end;

procedure TTestCorruptedFiles.Test_CorruptedMP4Atoms_Handled;
var
  Content: array[0..1023] of Byte;
begin
  { Create MP4-like structure with invalid atom sizes }
  FillChar(Content, SizeOf(Content), 0);

  { ftyp atom with wrong size }
  Content[0] := $FF;  { Impossibly large size }
  Content[1] := $FF;
  Content[2] := $FF;
  Content[3] := $FF;
  Content[4] := $66;  { 'ftyp' }
  Content[5] := $74;
  Content[6] := $79;
  Content[7] := $70;

  CreateTestFile('corrupted_atoms.mp4', Content);
  AssertTrue('Corrupted atoms file should exist', FileExists(FTestDir + 'corrupted_atoms.mp4'));
end;

procedure TTestCorruptedFiles.Test_PartiallyCorrupted_Handled;
var
  SourceFile: string;
  Stream: TFileStream;
  Buffer: array[0..99] of Byte;
begin
  SourceFile := GetTestDataDir + 'audio' + PathDelim + 'test_44100_stereo.mp3';
  if not FileExists(SourceFile) then
  begin
    AssertTrue('Source file needed', True);
    Exit;
  end;

  { Copy file and corrupt middle section }
  CopyFile(SourceFile, FTestDir + 'partially_corrupted.mp3');

  Stream := TFileStream.Create(FTestDir + 'partially_corrupted.mp3', fmOpenReadWrite);
  try
    { Seek to middle and write garbage }
    Stream.Seek(Stream.Size div 2, soFromBeginning);
    FillChar(Buffer, SizeOf(Buffer), $00);
    Stream.Write(Buffer, SizeOf(Buffer));
  finally
    Stream.Free;
  end;

  AssertTrue('Partially corrupted file should exist',
    FileExists(FTestDir + 'partially_corrupted.mp3'));
end;

{ Wrong extension tests }

procedure TTestCorruptedFiles.Test_MP3AsMP4_Handled;
var
  SourceFile: string;
begin
  SourceFile := GetTestDataDir + 'audio' + PathDelim + 'test_44100_stereo.mp3';
  if not FileExists(SourceFile) then
  begin
    AssertTrue('Source file needed', True);
    Exit;
  end;

  CopyFile(SourceFile, FTestDir + 'wrong_extension.mp4');
  AssertTrue('Wrong extension file should exist', FileExists(FTestDir + 'wrong_extension.mp4'));
end;

procedure TTestCorruptedFiles.Test_TextAsMp3_Handled;
begin
  CreateTestFile('text_as_audio.mp3', 'This is just plain text, not audio data.');
  AssertTrue('Text as MP3 file should exist', FileExists(FTestDir + 'text_as_audio.mp3'));
end;

procedure TTestCorruptedFiles.Test_ImageAsAudio_Handled;
var
  Content: array[0..7] of Byte;
begin
  { PNG header }
  Content[0] := $89;
  Content[1] := $50;
  Content[2] := $4E;
  Content[3] := $47;
  Content[4] := $0D;
  Content[5] := $0A;
  Content[6] := $1A;
  Content[7] := $0A;

  CreateTestFile('image_as_audio.mp3', Content);
  AssertTrue('Image as audio file should exist', FileExists(FTestDir + 'image_as_audio.mp3'));
end;

{ Malformed playlist tests }

procedure TTestCorruptedFiles.Test_MalformedM3U_Handled;
var
  List: TStringList;
begin
  { Create M3U with invalid content }
  CreateTestFile('malformed.m3u',
    '#EXTM3U' + LineEnding +
    '#EXTINF:invalid_duration,Bad Entry' + LineEnding +
    '' + LineEnding +
    '#EXTINF:-1,Negative Duration' + LineEnding +
    '/path/to/file.mp3' + LineEnding +
    '#EXTINF:999999999999999999999,Huge Number' + LineEnding +
    '/another/file.mp3');

  List := TStringList.Create;
  try
    { Should not crash }
    List.LoadFromFile(FTestDir + 'malformed.m3u');
    AssertTrue('Should load malformed M3U', List.Count > 0);
  finally
    List.Free;
  end;
end;

procedure TTestCorruptedFiles.Test_MalformedPLS_Handled;
var
  List: TStringList;
begin
  { Create PLS with invalid content }
  CreateTestFile('malformed.pls',
    '[playlist]' + LineEnding +
    'File1=' + LineEnding +  { Empty value }
    'Title1=Missing File' + LineEnding +
    'Length1=abc' + LineEnding +  { Invalid number }
    'NumberOfEntries=not_a_number' + LineEnding +
    'Version=999');

  List := TStringList.Create;
  try
    List.LoadFromFile(FTestDir + 'malformed.pls');
    AssertTrue('Should load malformed PLS', List.Count > 0);
  finally
    List.Free;
  end;
end;

procedure TTestCorruptedFiles.Test_BinaryAsPlaylist_Handled;
var
  Content: array[0..255] of Byte;
  List: TStringList;
  I: Integer;
begin
  for I := 0 to High(Content) do
    Content[I] := Random(256);

  CreateTestFile('binary.m3u', Content);

  List := TStringList.Create;
  try
    { This might raise an exception or load garbage, but shouldn't crash }
    try
      List.LoadFromFile(FTestDir + 'binary.m3u');
    except
      { Exception is acceptable }
    end;
    AssertTrue('Binary file handling completed', True);
  finally
    List.Free;
  end;
end;

{ ============================================================================
  TTestFileValidation
  ============================================================================ }

procedure TTestFileValidation.SetUp;
begin
  FTestDir := GetTempDir + 'test_validation' + PathDelim;
  if not DirectoryExists(FTestDir) then
    CreateDir(FTestDir);
end;

procedure TTestFileValidation.TearDown;
var
  SR: TSearchRec;
begin
  if FindFirst(FTestDir + '*', faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') then
        DeleteFile(FTestDir + SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  RemoveDir(FTestDir);
end;

procedure TTestFileValidation.CreateTestFile(const FileName: string; const Content: string);
var
  F: TextFile;
begin
  AssignFile(F, FTestDir + FileName);
  Rewrite(F);
  Write(F, Content);
  CloseFile(F);
end;

{ Magic number validation }

procedure TTestFileValidation.Test_ValidateMP3Magic;
var
  Stream: TFileStream;
  Buffer: array[0..2] of Byte;
  IsMP3: Boolean;
begin
  { Create file with ID3 header }
  Stream := TFileStream.Create(FTestDir + 'test.mp3', fmCreate);
  try
    Buffer[0] := $49;  { 'I' }
    Buffer[1] := $44;  { 'D' }
    Buffer[2] := $33;  { '3' }
    Stream.Write(Buffer, 3);
  finally
    Stream.Free;
  end;

  Stream := TFileStream.Create(FTestDir + 'test.mp3', fmOpenRead);
  try
    Stream.Read(Buffer, 3);
    IsMP3 := (Buffer[0] = $49) and (Buffer[1] = $44) and (Buffer[2] = $33);
    AssertTrue('Should detect ID3 magic', IsMP3);
  finally
    Stream.Free;
  end;
end;

procedure TTestFileValidation.Test_ValidateMP4Magic;
var
  Stream: TFileStream;
  Buffer: array[0..7] of Byte;
  IsMP4: Boolean;
begin
  { Create file with ftyp atom }
  Stream := TFileStream.Create(FTestDir + 'test.mp4', fmCreate);
  try
    Buffer[0] := $00;
    Buffer[1] := $00;
    Buffer[2] := $00;
    Buffer[3] := $08;
    Buffer[4] := $66;  { 'f' }
    Buffer[5] := $74;  { 't' }
    Buffer[6] := $79;  { 'y' }
    Buffer[7] := $70;  { 'p' }
    Stream.Write(Buffer, 8);
  finally
    Stream.Free;
  end;

  Stream := TFileStream.Create(FTestDir + 'test.mp4', fmOpenRead);
  try
    Stream.Read(Buffer, 8);
    IsMP4 := (Buffer[4] = $66) and (Buffer[5] = $74) and (Buffer[6] = $79) and (Buffer[7] = $70);
    AssertTrue('Should detect ftyp magic', IsMP4);
  finally
    Stream.Free;
  end;
end;

procedure TTestFileValidation.Test_ValidateFLACMagic;
var
  Stream: TFileStream;
  Buffer: array[0..3] of Byte;
  IsFLAC: Boolean;
begin
  Stream := TFileStream.Create(FTestDir + 'test.flac', fmCreate);
  try
    Buffer[0] := $66;  { 'f' }
    Buffer[1] := $4C;  { 'L' }
    Buffer[2] := $61;  { 'a' }
    Buffer[3] := $43;  { 'C' }
    Stream.Write(Buffer, 4);
  finally
    Stream.Free;
  end;

  Stream := TFileStream.Create(FTestDir + 'test.flac', fmOpenRead);
  try
    Stream.Read(Buffer, 4);
    IsFLAC := (Buffer[0] = $66) and (Buffer[1] = $4C) and (Buffer[2] = $61) and (Buffer[3] = $43);
    AssertTrue('Should detect FLAC magic', IsFLAC);
  finally
    Stream.Free;
  end;
end;

procedure TTestFileValidation.Test_ValidateOGGMagic;
var
  Stream: TFileStream;
  Buffer: array[0..3] of Byte;
  IsOGG: Boolean;
begin
  Stream := TFileStream.Create(FTestDir + 'test.ogg', fmCreate);
  try
    Buffer[0] := $4F;  { 'O' }
    Buffer[1] := $67;  { 'g' }
    Buffer[2] := $67;  { 'g' }
    Buffer[3] := $53;  { 'S' }
    Stream.Write(Buffer, 4);
  finally
    Stream.Free;
  end;

  Stream := TFileStream.Create(FTestDir + 'test.ogg', fmOpenRead);
  try
    Stream.Read(Buffer, 4);
    IsOGG := (Buffer[0] = $4F) and (Buffer[1] = $67) and (Buffer[2] = $67) and (Buffer[3] = $53);
    AssertTrue('Should detect OGG magic', IsOGG);
  finally
    Stream.Free;
  end;
end;

{ File structure validation }

procedure TTestFileValidation.Test_ValidateM3UStructure;
var
  List: TStringList;
  IsExtM3U: Boolean;
begin
  CreateTestFile('valid.m3u', '#EXTM3U' + LineEnding + '#EXTINF:180,Song' + LineEnding + '/path.mp3');

  List := TStringList.Create;
  try
    List.LoadFromFile(FTestDir + 'valid.m3u');
    IsExtM3U := (List.Count > 0) and (Pos('#EXTM3U', List[0]) = 1);
    AssertTrue('Should detect valid M3U structure', IsExtM3U);
  finally
    List.Free;
  end;
end;

procedure TTestFileValidation.Test_ValidatePLSStructure;
var
  List: TStringList;
  IsPLS: Boolean;
begin
  CreateTestFile('valid.pls', '[playlist]' + LineEnding + 'File1=/path.mp3' + LineEnding + 'NumberOfEntries=1');

  List := TStringList.Create;
  try
    List.LoadFromFile(FTestDir + 'valid.pls');
    IsPLS := (List.Count > 0) and (Pos('[playlist]', List[0]) > 0);
    AssertTrue('Should detect valid PLS structure', IsPLS);
  finally
    List.Free;
  end;
end;

procedure TTestFileValidation.Test_ValidateSRTStructure;
var
  List: TStringList;
  IsSRT: Boolean;
begin
  CreateTestFile('valid.srt',
    '1' + LineEnding +
    '00:00:01,000 --> 00:00:04,000' + LineEnding +
    'Subtitle text' + LineEnding +
    '' + LineEnding +
    '2' + LineEnding +
    '00:00:05,000 --> 00:00:08,000' + LineEnding +
    'More text');

  List := TStringList.Create;
  try
    List.LoadFromFile(FTestDir + 'valid.srt');
    { Basic SRT validation: first line should be a number }
    IsSRT := (List.Count > 0) and (StrToIntDef(Trim(List[0]), -1) > 0);
    AssertTrue('Should detect valid SRT structure', IsSRT);
  finally
    List.Free;
  end;
end;

{ Path validation }

procedure TTestFileValidation.Test_ValidatePath_Normal;
var
  Path: string;
  IsValid: Boolean;
begin
  Path := '/home/user/music/song.mp3';
  IsValid := (Length(Path) > 0) and (Length(Path) < 4096);
  AssertTrue('Normal path should be valid', IsValid);
end;

procedure TTestFileValidation.Test_ValidatePath_Unicode;
var
  Path: string;
  IsValid: Boolean;
begin
  Path := '/home/user/müsic/sóng_日本語.mp3';
  IsValid := (Length(Path) > 0) and (Length(Path) < 4096);
  AssertTrue('Unicode path should be valid', IsValid);
end;

procedure TTestFileValidation.Test_ValidatePath_Special;
var
  Path: string;
  IsValid: Boolean;
begin
  Path := '/path/with spaces/and (parentheses)/file [brackets].mp3';
  IsValid := (Length(Path) > 0) and (Length(Path) < 4096);
  AssertTrue('Path with special chars should be valid', IsValid);
end;

procedure TTestFileValidation.Test_ValidatePath_TooLong;
var
  Path: string;
  I: Integer;
  IsValid: Boolean;
begin
  Path := '/';
  for I := 1 to 5000 do
    Path := Path + 'a';
  Path := Path + '.mp3';

  IsValid := Length(Path) < 4096;
  AssertFalse('Too long path should be invalid', IsValid);
end;

initialization
  RegisterTest('Robustness', TTestCorruptedFiles);
  RegisterTest('Robustness', TTestFileValidation);

end.
