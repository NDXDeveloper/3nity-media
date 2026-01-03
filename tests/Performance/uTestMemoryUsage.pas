{ ===============================================================================
  uTestMemoryUsage.pas - Memory Usage Performance Tests

  Part of 3nity Media - Test Suite

  Tests memory consumption patterns during various operations.
  Monitors memory allocation and deallocation efficiency.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uTestMemoryUsage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type
  { ============================================================================
    TTestMemoryUsage - Memory usage tests
    ============================================================================ }
  TTestMemoryUsage = class(TTestCase)
  private
    function GetMemoryUsed: PtrUInt;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Basic memory allocation tests }
    procedure Test_StringList_MemoryUsage;
    procedure Test_StringList_Large_MemoryUsage;
    procedure Test_MemoryStream_MemoryUsage;
    procedure Test_DynamicArray_MemoryUsage;

    { Playlist memory tests }
    procedure Test_Playlist_100Items_Memory;
    procedure Test_Playlist_1000Items_Memory;
    procedure Test_Playlist_10000Items_Memory;
    procedure Test_Playlist_Clear_FreesMemory;

    { String handling memory tests }
    procedure Test_LongStrings_Memory;
    procedure Test_UnicodeStrings_Memory;
    procedure Test_StringConcatenation_Memory;

    { Cache simulation tests }
    procedure Test_MetadataCache_Memory;
    procedure Test_ThumbnailCache_Memory;
    procedure Test_CacheEviction_FreesMemory;

    { Memory leak detection patterns }
    procedure Test_CreateDestroy_NoLeak;
    procedure Test_ExceptionSafety_NoLeak;
    procedure Test_RepeatedOperations_StableMemory;
  end;

  { ============================================================================
    TTestMemoryPatterns - Memory allocation patterns
    ============================================================================ }
  TTestMemoryPatterns = class(TTestCase)
  private
    function GetMemoryUsed: PtrUInt;
  published
    { Allocation patterns }
    procedure Test_SmallAllocations_Pattern;
    procedure Test_LargeAllocations_Pattern;
    procedure Test_MixedAllocations_Pattern;

    { Deallocation patterns }
    procedure Test_LIFO_Deallocation;
    procedure Test_FIFO_Deallocation;
    procedure Test_Random_Deallocation;

    { Memory fragmentation }
    procedure Test_FragmentationResistance;
    procedure Test_ReallocationEfficiency;
  end;

implementation

{ ============================================================================
  TTestMemoryUsage
  ============================================================================ }

procedure TTestMemoryUsage.SetUp;
begin
  { Nothing to set up }
end;

procedure TTestMemoryUsage.TearDown;
begin
  { Nothing to clean up }
end;

function TTestMemoryUsage.GetMemoryUsed: PtrUInt;
var
  Status: TFPCHeapStatus;
begin
  Status := GetFPCHeapStatus;
  Result := Status.CurrHeapUsed;
end;

{ Basic memory allocation tests }

procedure TTestMemoryUsage.Test_StringList_MemoryUsage;
var
  MemBefore, MemAfter, MemDiff: PtrUInt;
  List: TStringList;
begin
  MemBefore := GetMemoryUsed;

  List := TStringList.Create;
  try
    MemAfter := GetMemoryUsed;
    MemDiff := MemAfter - MemBefore;

    AssertTrue('Empty TStringList should use < 1KB', MemDiff < 1024);
  finally
    List.Free;
  end;
end;

procedure TTestMemoryUsage.Test_StringList_Large_MemoryUsage;
var
  MemBefore, MemAfter, MemDiff: PtrUInt;
  List: TStringList;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;

  List := TStringList.Create;
  try
    for I := 1 to 10000 do
      List.Add('String item number ' + IntToStr(I) + ' with some extra text');

    MemAfter := GetMemoryUsed;
    MemDiff := MemAfter - MemBefore;

    { 10000 strings of ~40 chars each = ~400KB + overhead + heap fragmentation }
    AssertTrue('10000 strings should use < 4MB, used: ' + IntToStr(MemDiff div 1024) + 'KB',
      MemDiff < 4 * 1024 * 1024);
  finally
    List.Free;
  end;

  { Verify memory was freed }
  MemAfter := GetMemoryUsed;
  AssertTrue('Memory should be mostly freed after destroy',
    MemAfter < MemBefore + 10240);  { Allow 10KB variance }
end;

procedure TTestMemoryUsage.Test_MemoryStream_MemoryUsage;
var
  MemBefore, MemAfter: PtrUInt;
  Stream: TMemoryStream;
  Buffer: array[0..1023] of Byte;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  MemBefore := GetMemoryUsed;

  Stream := TMemoryStream.Create;
  try
    Stream.Write(Buffer, SizeOf(Buffer));
    MemAfter := GetMemoryUsed;

    { Stream has internal buffer allocation overhead }
    AssertTrue('1KB stream should use < 16KB', (MemAfter - MemBefore) < 16384);
  finally
    Stream.Free;
  end;
end;

procedure TTestMemoryUsage.Test_DynamicArray_MemoryUsage;
var
  MemBefore, MemAfter: PtrUInt;
  Arr: array of Integer;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;

  SetLength(Arr, 10000);
  for I := 0 to High(Arr) do
    Arr[I] := I;

  MemAfter := GetMemoryUsed;

  { 10000 integers = 40KB (4 bytes each) }
  AssertTrue('10000 integers should use ~40-50KB',
    (MemAfter - MemBefore) < 60 * 1024);

  SetLength(Arr, 0);
end;

{ Playlist memory tests }

procedure TTestMemoryUsage.Test_Playlist_100Items_Memory;
var
  MemBefore, MemAfter, MemDiff: PtrUInt;
  List: TStringList;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;

  List := TStringList.Create;
  try
    for I := 1 to 100 do
      List.Add('/path/to/music/artist/album/song' + IntToStr(I) + '.mp3');

    MemAfter := GetMemoryUsed;
    MemDiff := MemAfter - MemBefore;

    AssertTrue('100 playlist items should use < 50KB, used: ' +
      IntToStr(MemDiff div 1024) + 'KB', MemDiff < 50 * 1024);
  finally
    List.Free;
  end;
end;

procedure TTestMemoryUsage.Test_Playlist_1000Items_Memory;
var
  MemBefore, MemAfter, MemDiff: PtrUInt;
  List: TStringList;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;

  List := TStringList.Create;
  try
    for I := 1 to 1000 do
      List.Add('/path/to/music/artist/album/song' + IntToStr(I) + '.mp3');

    MemAfter := GetMemoryUsed;
    MemDiff := MemAfter - MemBefore;

    AssertTrue('1000 playlist items should use < 500KB, used: ' +
      IntToStr(MemDiff div 1024) + 'KB', MemDiff < 500 * 1024);
  finally
    List.Free;
  end;
end;

procedure TTestMemoryUsage.Test_Playlist_10000Items_Memory;
var
  MemBefore, MemAfter, MemDiff: PtrUInt;
  List: TStringList;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;

  List := TStringList.Create;
  try
    for I := 1 to 10000 do
      List.Add('/path/to/music/artist/album/song' + IntToStr(I) + '.mp3');

    MemAfter := GetMemoryUsed;
    MemDiff := MemAfter - MemBefore;

    AssertTrue('10000 playlist items should use < 4MB, used: ' +
      IntToStr(MemDiff div 1024) + 'KB', MemDiff < 4 * 1024 * 1024);
  finally
    List.Free;
  end;
end;

procedure TTestMemoryUsage.Test_Playlist_Clear_FreesMemory;
var
  MemBefore, MemAfter, MemCleared: PtrUInt;
  List: TStringList;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;

  List := TStringList.Create;
  try
    for I := 1 to 1000 do
      List.Add('/path/to/music/song' + IntToStr(I) + '.mp3');

    MemAfter := GetMemoryUsed;
    AssertTrue('Memory should increase after adding items', MemAfter > MemBefore);

    List.Clear;
    MemCleared := GetMemoryUsed;

    { Memory should decrease after clear, though maybe not to original }
    AssertTrue('Memory should decrease after Clear, was: ' +
      IntToStr((MemAfter - MemBefore) div 1024) + 'KB, now: ' +
      IntToStr((MemCleared - MemBefore) div 1024) + 'KB',
      MemCleared < MemAfter);
  finally
    List.Free;
  end;
end;

{ String handling memory tests }

procedure TTestMemoryUsage.Test_LongStrings_Memory;
var
  MemBefore, MemAfter: PtrUInt;
  S: string;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;

  S := '';
  for I := 1 to 1000 do
    S := S + 'This is a long string that keeps growing. ';

  MemAfter := GetMemoryUsed;

  { ~43KB of text }
  AssertTrue('Long string should use reasonable memory',
    (MemAfter - MemBefore) < 100 * 1024);

  S := '';  { Free the string }
end;

procedure TTestMemoryUsage.Test_UnicodeStrings_Memory;
var
  MemBefore, MemAfter: PtrUInt;
  List: TStringList;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;

  List := TStringList.Create;
  try
    for I := 1 to 100 do
      List.Add('Ménü Ïtem ' + IntToStr(I) + ' with émojis: 🎵🎶🎧');

    MemAfter := GetMemoryUsed;

    { Unicode strings may use more memory }
    AssertTrue('100 unicode strings should use < 50KB',
      (MemAfter - MemBefore) < 50 * 1024);
  finally
    List.Free;
  end;
end;

procedure TTestMemoryUsage.Test_StringConcatenation_Memory;
var
  MemBefore, MemPeak, MemAfter: PtrUInt;
  S: string;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;
  MemPeak := MemBefore;

  S := '';
  for I := 1 to 100 do
  begin
    S := S + IntToStr(I) + ', ';
    if GetMemoryUsed > MemPeak then
      MemPeak := GetMemoryUsed;
  end;

  MemAfter := GetMemoryUsed;

  { String concatenation shouldn't cause excessive memory growth }
  AssertTrue('Peak memory during concatenation should be reasonable',
    (MemPeak - MemBefore) < 50 * 1024);

  S := '';
end;

{ Cache simulation tests }

procedure TTestMemoryUsage.Test_MetadataCache_Memory;
var
  MemBefore, MemAfter: PtrUInt;
  Cache: TStringList;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;

  Cache := TStringList.Create;
  try
    { Simulate caching metadata for 500 files }
    for I := 1 to 500 do
      Cache.Add('/path/to/file' + IntToStr(I) + '.mp3=Artist ' + IntToStr(I) +
        '|Title ' + IntToStr(I) + '|Album ' + IntToStr(I) + '|180');

    MemAfter := GetMemoryUsed;

    AssertTrue('500 cached metadata entries should use < 500KB',
      (MemAfter - MemBefore) < 500 * 1024);
  finally
    Cache.Free;
  end;
end;

procedure TTestMemoryUsage.Test_ThumbnailCache_Memory;
var
  MemBefore, MemAfter: PtrUInt;
  Cache: array of TMemoryStream;
  I: Integer;
  Buffer: array[0..4095] of Byte;  { Simulate 4KB thumbnails }
begin
  FillChar(Buffer, SizeOf(Buffer), $FF);
  MemBefore := GetMemoryUsed;

  SetLength(Cache, 100);
  for I := 0 to High(Cache) do
  begin
    Cache[I] := TMemoryStream.Create;
    Cache[I].Write(Buffer, SizeOf(Buffer));
  end;

  MemAfter := GetMemoryUsed;

  { 100 * 4KB = 400KB + overhead }
  AssertTrue('100 thumbnail cache (4KB each) should use < 600KB',
    (MemAfter - MemBefore) < 600 * 1024);

  { Clean up }
  for I := 0 to High(Cache) do
    Cache[I].Free;
  SetLength(Cache, 0);
end;

procedure TTestMemoryUsage.Test_CacheEviction_FreesMemory;
var
  MemBefore, MemFull, MemEvicted: PtrUInt;
  Cache: TStringList;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;

  Cache := TStringList.Create;
  try
    { Fill cache }
    for I := 1 to 1000 do
      Cache.Add('Key' + IntToStr(I) + '=Value' + IntToStr(I));

    MemFull := GetMemoryUsed;

    { Evict half the cache }
    for I := 999 downto 500 do
      Cache.Delete(I);

    MemEvicted := GetMemoryUsed;

    AssertTrue('Memory should decrease after eviction',
      MemEvicted < MemFull);
  finally
    Cache.Free;
  end;
end;

{ Memory leak detection patterns }

procedure TTestMemoryUsage.Test_CreateDestroy_NoLeak;
var
  MemBefore, MemAfter: PtrUInt;
  List: TStringList;
  I, J: Integer;
begin
  MemBefore := GetMemoryUsed;

  { Create and destroy 100 objects }
  for I := 1 to 100 do
  begin
    List := TStringList.Create;
    for J := 1 to 10 do
      List.Add('Item ' + IntToStr(J));
    List.Free;
  end;

  MemAfter := GetMemoryUsed;

  { Memory should return close to original }
  AssertTrue('No leak after 100 create/destroy cycles, diff: ' +
    IntToStr(Int64(MemAfter) - Int64(MemBefore)) + ' bytes',
    Abs(Int64(MemAfter) - Int64(MemBefore)) < 10240);  { 10KB tolerance }
end;

procedure TTestMemoryUsage.Test_ExceptionSafety_NoLeak;
var
  MemBefore, MemAfter: PtrUInt;
  List: TStringList;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;

  for I := 1 to 50 do
  begin
    List := TStringList.Create;
    try
      List.Add('Item 1');
      List.Add('Item 2');
      if I = 25 then
        raise Exception.Create('Test exception');
    except
      { Exception handled }
    end;
    List.Free;
  end;

  MemAfter := GetMemoryUsed;

  AssertTrue('No leak after exception handling',
    Abs(Int64(MemAfter) - Int64(MemBefore)) < 10240);
end;

procedure TTestMemoryUsage.Test_RepeatedOperations_StableMemory;
var
  MemStart, MemEnd: PtrUInt;
  List: TStringList;
  I, J: Integer;
begin
  List := TStringList.Create;
  try
    { Warm up }
    for J := 1 to 100 do
      List.Add('Warmup ' + IntToStr(J));
    List.Clear;

    MemStart := GetMemoryUsed;

    { Repeated add/clear cycles }
    for I := 1 to 100 do
    begin
      for J := 1 to 100 do
        List.Add('Item ' + IntToStr(J));
      List.Clear;
    end;

    MemEnd := GetMemoryUsed;

    { Memory should remain stable }
    AssertTrue('Memory stable after 100 add/clear cycles, diff: ' +
      IntToStr(Int64(MemEnd) - Int64(MemStart)) + ' bytes',
      Abs(Int64(MemEnd) - Int64(MemStart)) < 20480);  { 20KB tolerance }
  finally
    List.Free;
  end;
end;

{ ============================================================================
  TTestMemoryPatterns
  ============================================================================ }

function TTestMemoryPatterns.GetMemoryUsed: PtrUInt;
var
  Status: TFPCHeapStatus;
begin
  Status := GetFPCHeapStatus;
  Result := Status.CurrHeapUsed;
end;

procedure TTestMemoryPatterns.Test_SmallAllocations_Pattern;
var
  MemBefore, MemAfter: PtrUInt;
  Lists: array[0..999] of TStringList;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;

  { Many small allocations }
  for I := 0 to High(Lists) do
  begin
    Lists[I] := TStringList.Create;
    Lists[I].Add('Small item');
  end;

  MemAfter := GetMemoryUsed;
  AssertTrue('1000 small allocations should use < 1MB',
    (MemAfter - MemBefore) < 1024 * 1024);

  for I := 0 to High(Lists) do
    Lists[I].Free;
end;

procedure TTestMemoryPatterns.Test_LargeAllocations_Pattern;
var
  MemBefore, MemAfter: PtrUInt;
  Streams: array[0..9] of TMemoryStream;
  Buffer: array[0..65535] of Byte;  { 64KB }
  I: Integer;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  MemBefore := GetMemoryUsed;

  { Few large allocations }
  for I := 0 to High(Streams) do
  begin
    Streams[I] := TMemoryStream.Create;
    Streams[I].Write(Buffer, SizeOf(Buffer));
  end;

  MemAfter := GetMemoryUsed;
  { 10 * 64KB = 640KB + overhead }
  AssertTrue('10 large allocations (64KB each) should use < 1MB',
    (MemAfter - MemBefore) < 1024 * 1024);

  for I := 0 to High(Streams) do
    Streams[I].Free;
end;

procedure TTestMemoryPatterns.Test_MixedAllocations_Pattern;
var
  MemBefore, MemAfter: PtrUInt;
  SmallLists: array[0..99] of TStringList;
  LargeStream: TMemoryStream;
  Buffer: array[0..65535] of Byte;
  I: Integer;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  MemBefore := GetMemoryUsed;

  { Mix of small and large allocations }
  for I := 0 to High(SmallLists) do
  begin
    SmallLists[I] := TStringList.Create;
    SmallLists[I].Add('Item ' + IntToStr(I));
  end;

  LargeStream := TMemoryStream.Create;
  LargeStream.Write(Buffer, SizeOf(Buffer));

  MemAfter := GetMemoryUsed;
  AssertTrue('Mixed allocations should use < 200KB',
    (MemAfter - MemBefore) < 200 * 1024);

  LargeStream.Free;
  for I := 0 to High(SmallLists) do
    SmallLists[I].Free;
end;

procedure TTestMemoryPatterns.Test_LIFO_Deallocation;
var
  MemBefore, MemAfter: PtrUInt;
  Lists: array[0..99] of TStringList;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;

  { Allocate }
  for I := 0 to High(Lists) do
    Lists[I] := TStringList.Create;

  { Deallocate LIFO (last in, first out) }
  for I := High(Lists) downto 0 do
    Lists[I].Free;

  MemAfter := GetMemoryUsed;
  AssertTrue('LIFO deallocation should return memory',
    Abs(Int64(MemAfter) - Int64(MemBefore)) < 10240);
end;

procedure TTestMemoryPatterns.Test_FIFO_Deallocation;
var
  MemBefore, MemAfter: PtrUInt;
  Lists: array[0..99] of TStringList;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;

  { Allocate }
  for I := 0 to High(Lists) do
    Lists[I] := TStringList.Create;

  { Deallocate FIFO (first in, first out) }
  for I := 0 to High(Lists) do
    Lists[I].Free;

  MemAfter := GetMemoryUsed;
  AssertTrue('FIFO deallocation should return memory',
    Abs(Int64(MemAfter) - Int64(MemBefore)) < 10240);
end;

procedure TTestMemoryPatterns.Test_Random_Deallocation;
var
  MemBefore, MemAfter: PtrUInt;
  Lists: array[0..99] of TStringList;
  Order: array[0..99] of Integer;
  I, J, Temp: Integer;
begin
  MemBefore := GetMemoryUsed;

  { Allocate }
  for I := 0 to High(Lists) do
    Lists[I] := TStringList.Create;

  { Create random order }
  for I := 0 to High(Order) do
    Order[I] := I;

  { Shuffle }
  for I := High(Order) downto 1 do
  begin
    J := Random(I + 1);
    Temp := Order[I];
    Order[I] := Order[J];
    Order[J] := Temp;
  end;

  { Deallocate in random order }
  for I := 0 to High(Order) do
    Lists[Order[I]].Free;

  MemAfter := GetMemoryUsed;
  AssertTrue('Random deallocation should return memory',
    Abs(Int64(MemAfter) - Int64(MemBefore)) < 20480);  { Slightly more tolerance }
end;

procedure TTestMemoryPatterns.Test_FragmentationResistance;
var
  MemBefore, MemAfter: PtrUInt;
  Lists: array[0..199] of TStringList;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;

  { Allocate all }
  for I := 0 to High(Lists) do
    Lists[I] := TStringList.Create;

  { Free every other one (creates fragmentation) }
  for I := 0 to High(Lists) do
    if I mod 2 = 0 then
    begin
      Lists[I].Free;
      Lists[I] := nil;
    end;

  { Reallocate in the gaps }
  for I := 0 to High(Lists) do
    if Lists[I] = nil then
      Lists[I] := TStringList.Create;

  { Free all }
  for I := 0 to High(Lists) do
    Lists[I].Free;

  MemAfter := GetMemoryUsed;
  AssertTrue('Memory should recover after fragmentation pattern',
    Abs(Int64(MemAfter) - Int64(MemBefore)) < 20480);
end;

procedure TTestMemoryPatterns.Test_ReallocationEfficiency;
var
  MemBefore, MemPeak, MemAfter: PtrUInt;
  List: TStringList;
  I: Integer;
begin
  MemBefore := GetMemoryUsed;
  MemPeak := MemBefore;

  List := TStringList.Create;
  try
    { Grow the list repeatedly (causes reallocations) }
    for I := 1 to 10000 do
    begin
      List.Add('Item ' + IntToStr(I));
      if GetMemoryUsed > MemPeak then
        MemPeak := GetMemoryUsed;
    end;

    MemAfter := GetMemoryUsed;

    { Peak shouldn't be much higher than final (good reallocation strategy) }
    AssertTrue('Peak memory should be < 2x final memory',
      MemPeak < MemAfter * 2);
  finally
    List.Free;
  end;
end;

initialization
  RegisterTest('Performance', TTestMemoryUsage);
  RegisterTest('Performance', TTestMemoryPatterns);

end.
