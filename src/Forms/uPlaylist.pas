{ ═══════════════════════════════════════════════════════════════════════════════
  uPlaylist.pas - Playlist Window

  Part of 3nity Media - Lazarus Edition

  This unit implements the playlist window with:
  - List view with columns (Title, Duration, Artist, Album)
  - Drag and drop reordering
  - Context menu operations
  - Toolbar for common actions
  - Status bar with statistics

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uPlaylist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ExtCtrls, StdCtrls, Buttons, ActnList, LCLType, StrUtils,
  uTypes, uConstants, uPlaylistManager, uLocale, uConfig;

type
  { TfrmPlaylist }
  TfrmPlaylist = class(TForm)
    { ═══════════════════════════════════════════════════════════════════════════
      COMPONENTS
      ═══════════════════════════════════════════════════════════════════════════ }
    lvPlaylist: TListView;
    StatusBar: TStatusBar;

    { Phase 22: Search Panel }
    pnlSearch: TPanel;
    lblSearch: TLabel;
    edtSearch: TEdit;
    btnClearSearch: TButton;
    lblFilterCount: TLabel;

    ToolBar: TToolBar;
    tbAdd: TToolButton;
    tbRemove: TToolButton;
    tbSep1: TToolButton;
    tbMoveUp: TToolButton;
    tbMoveDown: TToolButton;
    tbSep2: TToolButton;
    tbClear: TToolButton;
    tbSep3: TToolButton;
    tbShuffle: TToolButton;
    tbRepeat: TToolButton;
    tbSep4: TToolButton;
    tbLoad: TToolButton;
    tbSave: TToolButton;

    plPopupMenu: TPopupMenu;
    mnuPlay: TMenuItem;
    mnuSep1: TMenuItem;
    mnuAdd: TMenuItem;
    mnuRemove: TMenuItem;
    mnuClear: TMenuItem;
    mnuSep2: TMenuItem;
    mnuMoveUp: TMenuItem;
    mnuMoveDown: TMenuItem;
    mnuSep3: TMenuItem;
    mnuSelectAll: TMenuItem;
    mnuSelectNone: TMenuItem;
    mnuSelectInvert: TMenuItem;
    mnuSep4: TMenuItem;
    mnuSort: TMenuItem;
    mnuSortByName: TMenuItem;
    mnuSortByTitle: TMenuItem;
    mnuSortByArtist: TMenuItem;
    mnuSortByDuration: TMenuItem;
    mnuSortSep: TMenuItem;
    mnuSortReverse: TMenuItem;
    mnuSortRandom: TMenuItem;

    { Phase 22: Additional menu items }
    mnuSep5: TMenuItem;
    mnuRemoveDuplicates: TMenuItem;
    mnuSep6: TMenuItem;
    mnuProperties: TMenuItem;

    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;

    { ═══════════════════════════════════════════════════════════════════════════
      EVENT HANDLERS
      ═══════════════════════════════════════════════════════════════════════════ }
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure lvPlaylistDblClick(Sender: TObject);
    procedure lvPlaylistMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lvPlaylistKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lvPlaylistSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvPlaylistDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lvPlaylistDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tbAddClick(Sender: TObject);
    procedure tbRemoveClick(Sender: TObject);
    procedure tbMoveUpClick(Sender: TObject);
    procedure tbMoveDownClick(Sender: TObject);
    procedure tbClearClick(Sender: TObject);
    procedure tbShuffleClick(Sender: TObject);
    procedure tbRepeatClick(Sender: TObject);
    procedure tbLoadClick(Sender: TObject);
    procedure tbSaveClick(Sender: TObject);

    procedure mnuPlayClick(Sender: TObject);
    procedure mnuAddClick(Sender: TObject);
    procedure mnuRemoveClick(Sender: TObject);
    procedure mnuClearClick(Sender: TObject);
    procedure mnuMoveUpClick(Sender: TObject);
    procedure mnuMoveDownClick(Sender: TObject);
    procedure mnuSelectAllClick(Sender: TObject);
    procedure mnuSelectNoneClick(Sender: TObject);
    procedure mnuSelectInvertClick(Sender: TObject);
    procedure mnuSortByNameClick(Sender: TObject);
    procedure mnuSortByTitleClick(Sender: TObject);
    procedure mnuSortByArtistClick(Sender: TObject);
    procedure mnuSortByDurationClick(Sender: TObject);
    procedure mnuSortReverseClick(Sender: TObject);
    procedure mnuSortRandomClick(Sender: TObject);

    { Phase 22: Search and new features }
    procedure edtSearchChange(Sender: TObject);
    procedure btnClearSearchClick(Sender: TObject);
    procedure mnuRemoveDuplicatesClick(Sender: TObject);
    procedure mnuPropertiesClick(Sender: TObject);

  private
    FPlaylistManager: TPlaylistManager;
    FOnPlayRequest: TPlaylistPlayEvent;
    FFilterText: string;
    FFilteredIndices: array of Integer;
    FLastClickedIndex: Integer;  { Qt5 workaround: store index at MouseDown }
    FWindowStateRestored: Boolean;  { Track if window geometry was restored }
    FSavedBounds: TRect;  { Store actual bounds for Qt5 workaround }

    procedure SetPlaylistManager(Value: TPlaylistManager);
    procedure FormChangeBounds(Sender: TObject);
    procedure SaveWindowGeometry;
    procedure UpdateList;
    procedure UpdateFilteredList;
    procedure UpdateStatusBar;
    procedure HighlightCurrentItem;
    function ItemMatchesFilter(const Item: TPlaylistItem; const Filter: string): Boolean;
    function GetRealIndex(ListIndex: Integer): Integer;
    procedure ShowPropertiesDialog;

    { Playlist manager event handlers }
    procedure OnPlaylistChange(Sender: TObject);
    procedure OnPlaylistItemAdded(Sender: TObject; Index: Integer);
    procedure OnPlaylistItemRemoved(Sender: TObject; Index: Integer);
    procedure OnPlaylistCurrentChange(Sender: TObject; Index: Integer);
    procedure OnPlaylistClear(Sender: TObject);

  public
    property PlaylistManager: TPlaylistManager read FPlaylistManager write SetPlaylistManager;
    property OnPlayRequest: TPlaylistPlayEvent read FOnPlayRequest write FOnPlayRequest;

    procedure ApplyLocale;
    procedure RefreshList;
    procedure ScrollToCurrentItem;
    procedure UpdateToolbarState;
    procedure AddFiles(const Files: TStrings);
  end;

var
  frmPlaylist: TfrmPlaylist;

implementation

{$R *.lfm}

uses
  Math, DateUtils;

const
  COL_NUMBER   = 0;
  COL_TITLE    = 1;
  COL_DURATION = 2;
  COL_ARTIST   = 3;
  COL_ALBUM    = 4;

{ ═══════════════════════════════════════════════════════════════════════════════
  FORM EVENTS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmPlaylist.FormCreate(Sender: TObject);
begin
  FPlaylistManager := nil;
  FFilterText := '';
  SetLength(FFilteredIndices, 0);
  FLastClickedIndex := -1;
  FWindowStateRestored := False;
  FSavedBounds := Rect(Left, Top, Left + Width, Top + Height);

  { Qt5 workaround: track actual window position via OnChangeBounds }
  OnChangeBounds := @FormChangeBounds;

  { Setup open dialog }
  OpenDialog.Filter := 'All Supported Files|*.mp3;*.m4a;*.aac;*.ogg;*.opus;*.flac;*.wav;*.wma;' +
    '*.mp4;*.mkv;*.avi;*.wmv;*.mov;*.flv;*.webm;*.m3u;*.m3u8;*.pls|' +
    'Audio Files|*.mp3;*.m4a;*.aac;*.ogg;*.opus;*.flac;*.wav;*.wma|' +
    'Video Files|*.mp4;*.mkv;*.avi;*.wmv;*.mov;*.flv;*.webm|' +
    'Playlists|*.m3u;*.m3u8;*.pls|' +
    'All Files|*.*';
  OpenDialog.Options := OpenDialog.Options + [ofAllowMultiSelect];

  { Setup save dialog }
  SaveDialog.Filter := _T('Playlist', 'PlaylistFilter', 'M3U Playlist|*.m3u|M3U8 Playlist|*.m3u8|PLS Playlist|*.pls');
  SaveDialog.DefaultExt := '.m3u';

  ApplyLocale;
end;

procedure TfrmPlaylist.ApplyLocale;
begin
  Caption := _T('Playlist', 'Title', 'Playlist');

  { Search panel }
  lblSearch.Caption := _T('Playlist', 'Search', 'Search:');
  edtSearch.TextHint := _T('Playlist', 'FilterPlaceholder', 'Filter playlist...');
  btnClearSearch.Caption := _T('Playlist', 'ClearSearch', 'Clear');

  { Toolbar buttons }
  tbAdd.Caption := _T('Playlist', 'Add', 'Add');
  tbRemove.Caption := _T('Playlist', 'Remove', 'Remove');
  tbMoveUp.Caption := _T('Playlist', 'MoveUp', 'Move Up');
  tbMoveDown.Caption := _T('Playlist', 'MoveDown', 'Move Down');
  tbClear.Caption := _T('Playlist', 'Clear', 'Clear');
  tbShuffle.Caption := _T('Playlist', 'Shuffle', 'Shuffle');
  tbRepeat.Caption := _T('Playlist', 'Repeat', 'Repeat');
  tbLoad.Caption := _T('Playlist', 'LoadPlaylist', 'Load');
  tbSave.Caption := _T('Playlist', 'SavePlaylist', 'Save');

  { Context menu }
  mnuPlay.Caption := _T('Playlist', 'Play', 'Play');
  mnuAdd.Caption := _T('Playlist', 'Add', 'Add');
  mnuRemove.Caption := _T('Playlist', 'Remove', 'Remove');
  mnuClear.Caption := _T('Playlist', 'Clear', 'Clear');
  mnuMoveUp.Caption := _T('Playlist', 'MoveUp', 'Move Up');
  mnuMoveDown.Caption := _T('Playlist', 'MoveDown', 'Move Down');
  mnuSelectAll.Caption := _T('Playlist', 'SelectAll', 'Select All');
  mnuSelectNone.Caption := _T('Playlist', 'SelectNone', 'Select None');
  mnuSelectInvert.Caption := _T('Playlist', 'SelectInvert', 'Invert Selection');
  mnuSort.Caption := _T('Playlist', 'Sort', 'Sort');
  mnuSortByName.Caption := _T('Playlist', 'SortByName', 'By Name');
  mnuSortByTitle.Caption := _T('Playlist', 'SortByTitle', 'By Title');
  mnuSortByArtist.Caption := _T('Playlist', 'SortByArtist', 'By Artist');
  mnuSortByDuration.Caption := _T('Playlist', 'SortByDuration', 'By Duration');
  mnuSortReverse.Caption := _T('Playlist', 'SortReverse', 'Reverse');
  mnuSortRandom.Caption := _T('Playlist', 'SortRandom', 'Randomize');
  mnuRemoveDuplicates.Caption := _T('Playlist', 'RemoveDuplicates', 'Remove Duplicates');
  mnuProperties.Caption := _T('Playlist', 'Properties', 'Properties...');

  { ListView columns }
  if lvPlaylist.Columns.Count >= 5 then
  begin
    lvPlaylist.Columns[0].Caption := '#';
    lvPlaylist.Columns[1].Caption := _T('Playlist', 'ColTitle', 'Title');
    lvPlaylist.Columns[2].Caption := _T('Playlist', 'ColDuration', 'Duration');
    lvPlaylist.Columns[3].Caption := _T('Playlist', 'ColArtist', 'Artist');
    lvPlaylist.Columns[4].Caption := _T('Playlist', 'ColAlbum', 'Album');
  end;
end;

procedure TfrmPlaylist.SaveWindowGeometry;
begin
  if Config <> nil then
  begin
    Config.SaveWindowStateBounds('PlaylistWindow', FSavedBounds, WindowState);
    Config.Save;
  end;
end;

procedure TfrmPlaylist.FormDestroy(Sender: TObject);
begin
  SaveWindowGeometry;

  { Disconnect from playlist manager }
  if FPlaylistManager <> nil then
  begin
    FPlaylistManager.OnChange := nil;
    FPlaylistManager.OnItemAdded := nil;
    FPlaylistManager.OnItemRemoved := nil;
    FPlaylistManager.OnCurrentChange := nil;
    FPlaylistManager.OnClear := nil;
  end;
end;

procedure TfrmPlaylist.FormShow(Sender: TObject);
begin
  { Restore window geometry on first show }
  FWindowStateRestored := True;
  if (Config <> nil) and Config.HasWindowState('PlaylistWindow') then
    Config.LoadWindowState('PlaylistWindow', Self);

  ApplyLocale;
  UpdateList;
  UpdateStatusBar;
  UpdateToolbarState;
  ScrollToCurrentItem;
end;

procedure TfrmPlaylist.FormChangeBounds(Sender: TObject);
begin
  { Qt5 workaround: capture actual window bounds when moved/resized }
  FSavedBounds := BoundsRect;
end;

procedure TfrmPlaylist.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveWindowGeometry;
  CloseAction := caHide;
end;

procedure TfrmPlaylist.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;
    VK_DELETE:
      if ssCtrl in Shift then
        tbClearClick(nil)
      else
        tbRemoveClick(nil);
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  LISTVIEW EVENTS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmPlaylist.lvPlaylistMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Item: TListItem;
begin
  { Capture clicked item index for double-click handling }
  Item := lvPlaylist.GetItemAt(X, Y);
  if Item <> nil then
    FLastClickedIndex := Item.Index
  else
    FLastClickedIndex := -1;
end;

procedure TfrmPlaylist.lvPlaylistDblClick(Sender: TObject);
var
  RealIndex: Integer;
begin
  if FPlaylistManager = nil then
    Exit;

  { Use index captured at MouseDown for reliability }
  if (FLastClickedIndex >= 0) and (FLastClickedIndex < lvPlaylist.Items.Count) then
  begin
    RealIndex := GetRealIndex(FLastClickedIndex);
    FPlaylistManager.PlayIndex(RealIndex);
  end
  else if lvPlaylist.Selected <> nil then
  begin
    RealIndex := GetRealIndex(lvPlaylist.Selected.Index);
    FPlaylistManager.PlayIndex(RealIndex);
  end;
end;

procedure TfrmPlaylist.lvPlaylistKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      mnuPlayClick(nil);
    VK_DELETE:
      tbRemoveClick(nil);
    VK_UP:
      if ssCtrl in Shift then
        tbMoveUpClick(nil);
    VK_DOWN:
      if ssCtrl in Shift then
        tbMoveDownClick(nil);
  end;
end;

procedure TfrmPlaylist.lvPlaylistSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  UpdateToolbarState;
end;

procedure TfrmPlaylist.lvPlaylistDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = lvPlaylist) and (lvPlaylist.Selected <> nil);
end;

{ ═══════════════════════════════════════════════════════════════════════════
  lvPlaylistDragDrop - Handle item reordering via drag and drop

  Purpose: Allows users to reorder playlist items by dragging them to a
           new position within the list.

  Parameters:
    - Sender: The ListView receiving the drop
    - Source: The drag source (must be lvPlaylist itself)
    - X, Y: Drop coordinates within the ListView

  Qt5 Workaround:
    The Lazarus Qt5 widgetset has a known issue where GetItemAt sometimes
    returns nil for valid positions. This implementation includes a fallback
    that manually calculates the target index:
    1. Try GetItemAt(X, Y) first
    2. If nil, calculate: ToIndex = TopItem.Index + (Y / ItemHeight)
    3. ItemHeight is estimated from first two items or defaults to 20px

  Notes:
    - Only accepts drops from the same ListView (internal reordering)
    - Delegates actual movement to FPlaylistManager.Move
    - Bounds-checked to prevent out-of-range indices
    - No-op if FromIndex equals ToIndex (same position)
  ═══════════════════════════════════════════════════════════════════════════ }
procedure TfrmPlaylist.lvPlaylistDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  TargetItem: TListItem;
  FromIndex, ToIndex: Integer;
  ItemHeight, TopIndex: Integer;
begin
  if (Source <> lvPlaylist) or (lvPlaylist.Selected = nil) then Exit;
  if FPlaylistManager = nil then Exit;
  if lvPlaylist.Items.Count = 0 then Exit;

  FromIndex := lvPlaylist.Selected.Index;

  { Qt5 workaround: GetItemAt can be unreliable, calculate index manually }
  TargetItem := lvPlaylist.GetItemAt(X, Y);
  if TargetItem <> nil then
    ToIndex := TargetItem.Index
  else
  begin
    { Fallback: calculate based on item height }
    if lvPlaylist.TopItem <> nil then
    begin
      TopIndex := lvPlaylist.TopItem.Index;
      { Estimate item height from first two items or use default }
      if lvPlaylist.Items.Count > 1 then
        ItemHeight := lvPlaylist.Items[1].Top - lvPlaylist.Items[0].Top
      else
        ItemHeight := 20; { Default row height }
      if ItemHeight <= 0 then ItemHeight := 20;
      ToIndex := TopIndex + (Y div ItemHeight);
      if ToIndex < 0 then ToIndex := 0;
      if ToIndex >= lvPlaylist.Items.Count then ToIndex := lvPlaylist.Items.Count - 1;
    end
    else
      Exit;
  end;

  if (FromIndex <> ToIndex) and (ToIndex >= 0) and (ToIndex < lvPlaylist.Items.Count) then
    FPlaylistManager.Move(FromIndex, ToIndex);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TOOLBAR EVENTS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmPlaylist.tbAddClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    AddFiles(OpenDialog.Files);
end;

procedure TfrmPlaylist.tbRemoveClick(Sender: TObject);
var
  I: Integer;
  Indices: array of Integer;
begin
  if FPlaylistManager = nil then Exit;
  if lvPlaylist.SelCount = 0 then Exit;

  { Collect selected indices }
  SetLength(Indices, lvPlaylist.SelCount);
  I := 0;
  while I < lvPlaylist.Items.Count do
  begin
    if lvPlaylist.Items[I].Selected then
    begin
      SetLength(Indices, Length(Indices) + 1);
      Indices[High(Indices)] := I;
    end;
    Inc(I);
  end;

  { Delete from end to preserve indices }
  for I := High(Indices) downto 0 do
    FPlaylistManager.Delete(Indices[I]);
end;

procedure TfrmPlaylist.tbMoveUpClick(Sender: TObject);
begin
  if FPlaylistManager = nil then Exit;
  if lvPlaylist.Selected = nil then Exit;

  FPlaylistManager.MoveUp(lvPlaylist.Selected.Index);
end;

procedure TfrmPlaylist.tbMoveDownClick(Sender: TObject);
begin
  if FPlaylistManager = nil then Exit;
  if lvPlaylist.Selected = nil then Exit;

  FPlaylistManager.MoveDown(lvPlaylist.Selected.Index);
end;

procedure TfrmPlaylist.tbClearClick(Sender: TObject);
begin
  if FPlaylistManager = nil then Exit;

  if MessageDlg(_T('Playlist', 'ClearTitle', 'Clear Playlist'),
     _T('Playlist', 'ClearConfirm', 'Are you sure you want to clear the playlist?'),
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    FPlaylistManager.Clear;
end;

procedure TfrmPlaylist.tbShuffleClick(Sender: TObject);
begin
  if FPlaylistManager = nil then Exit;

  if FPlaylistManager.PlaybackMode in [pmShuffle, pmShuffleRepeat] then
  begin
    if FPlaylistManager.PlaybackMode = pmShuffleRepeat then
      FPlaylistManager.PlaybackMode := pmRepeatAll
    else
      FPlaylistManager.PlaybackMode := pmNormal;
  end
  else
  begin
    if FPlaylistManager.PlaybackMode = pmRepeatAll then
      FPlaylistManager.PlaybackMode := pmShuffleRepeat
    else
      FPlaylistManager.PlaybackMode := pmShuffle;
  end;

  UpdateToolbarState;
end;

procedure TfrmPlaylist.tbRepeatClick(Sender: TObject);
begin
  if FPlaylistManager = nil then Exit;

  case FPlaylistManager.PlaybackMode of
    pmNormal:
      FPlaylistManager.PlaybackMode := pmRepeatAll;
    pmRepeatAll:
      FPlaylistManager.PlaybackMode := pmRepeatOne;
    pmRepeatOne:
      FPlaylistManager.PlaybackMode := pmNormal;
    pmShuffle:
      FPlaylistManager.PlaybackMode := pmShuffleRepeat;
    pmShuffleRepeat:
      FPlaylistManager.PlaybackMode := pmShuffle;
  end;

  UpdateToolbarState;
end;

procedure TfrmPlaylist.tbLoadClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    if IsPlaylistFile(OpenDialog.FileName) then
    begin
      if FPlaylistManager <> nil then
      begin
        FPlaylistManager.Clear;
        FPlaylistManager.LoadFromFile(OpenDialog.FileName);
      end;
    end
    else
      AddFiles(OpenDialog.Files);
  end;
end;

procedure TfrmPlaylist.tbSaveClick(Sender: TObject);
begin
  if FPlaylistManager = nil then Exit;

  if SaveDialog.Execute then
    FPlaylistManager.SaveToFile(SaveDialog.FileName);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  MENU EVENTS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmPlaylist.mnuPlayClick(Sender: TObject);
var
  RealIndex: Integer;
begin
  if FPlaylistManager = nil then Exit;
  if lvPlaylist.Selected = nil then Exit;

  { Get the real playlist index (may differ when filtering) }
  RealIndex := GetRealIndex(lvPlaylist.Selected.Index);

  { PlayIndex sets CurrentIndex and fires OnPlay which triggers playback }
  FPlaylistManager.PlayIndex(RealIndex);
end;

procedure TfrmPlaylist.mnuAddClick(Sender: TObject);
begin
  tbAddClick(nil);
end;

procedure TfrmPlaylist.mnuRemoveClick(Sender: TObject);
begin
  tbRemoveClick(nil);
end;

procedure TfrmPlaylist.mnuClearClick(Sender: TObject);
begin
  tbClearClick(nil);
end;

procedure TfrmPlaylist.mnuMoveUpClick(Sender: TObject);
begin
  tbMoveUpClick(nil);
end;

procedure TfrmPlaylist.mnuMoveDownClick(Sender: TObject);
begin
  tbMoveDownClick(nil);
end;

procedure TfrmPlaylist.mnuSelectAllClick(Sender: TObject);
begin
  lvPlaylist.SelectAll;
end;

procedure TfrmPlaylist.mnuSelectNoneClick(Sender: TObject);
begin
  lvPlaylist.ClearSelection;
end;

procedure TfrmPlaylist.mnuSelectInvertClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to lvPlaylist.Items.Count - 1 do
    lvPlaylist.Items[I].Selected := not lvPlaylist.Items[I].Selected;
end;

procedure TfrmPlaylist.mnuSortByNameClick(Sender: TObject);
begin
  if FPlaylistManager <> nil then
    FPlaylistManager.Sort(True);
end;

procedure TfrmPlaylist.mnuSortByTitleClick(Sender: TObject);
begin
  if FPlaylistManager <> nil then
    FPlaylistManager.SortByTitle(True);
end;

procedure TfrmPlaylist.mnuSortByArtistClick(Sender: TObject);
begin
  if FPlaylistManager <> nil then
    FPlaylistManager.SortByArtist(True);
end;

procedure TfrmPlaylist.mnuSortByDurationClick(Sender: TObject);
begin
  if FPlaylistManager <> nil then
    FPlaylistManager.SortByDuration(True);
end;

procedure TfrmPlaylist.mnuSortReverseClick(Sender: TObject);
begin
  if FPlaylistManager <> nil then
    FPlaylistManager.Reverse;
end;

procedure TfrmPlaylist.mnuSortRandomClick(Sender: TObject);
begin
  if FPlaylistManager <> nil then
    FPlaylistManager.Randomize;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PRIVATE METHODS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmPlaylist.SetPlaylistManager(Value: TPlaylistManager);
begin
  { Disconnect from old manager }
  if FPlaylistManager <> nil then
  begin
    FPlaylistManager.OnChange := nil;
    FPlaylistManager.OnItemAdded := nil;
    FPlaylistManager.OnItemRemoved := nil;
    FPlaylistManager.OnCurrentChange := nil;
    FPlaylistManager.OnClear := nil;
  end;

  FPlaylistManager := Value;

  { Connect to new manager }
  if FPlaylistManager <> nil then
  begin
    FPlaylistManager.OnChange := @OnPlaylistChange;
    FPlaylistManager.OnItemAdded := @OnPlaylistItemAdded;
    FPlaylistManager.OnItemRemoved := @OnPlaylistItemRemoved;
    FPlaylistManager.OnCurrentChange := @OnPlaylistCurrentChange;
    FPlaylistManager.OnClear := @OnPlaylistClear;
  end;

  UpdateList;
  UpdateStatusBar;
  UpdateToolbarState;
end;

procedure TfrmPlaylist.UpdateList;
var
  I: Integer;
  Item: TListItem;
  PlaylistItem: TPlaylistItem;
begin
  { If we have a filter, use the filtered list instead }
  if FFilterText <> '' then
  begin
    UpdateFilteredList;
    Exit;
  end;

  lvPlaylist.Items.BeginUpdate;
  try
    lvPlaylist.Items.Clear;
    SetLength(FFilteredIndices, 0);

    if FPlaylistManager = nil then Exit;

    for I := 0 to FPlaylistManager.Count - 1 do
    begin
      PlaylistItem := FPlaylistManager[I];

      Item := lvPlaylist.Items.Add;
      Item.Caption := '   ' + IntToStr(I + 1);
      Item.SubItems.Add(PlaylistItem.Title);
      Item.SubItems.Add(PlaylistItem.DurationString);
      Item.SubItems.Add(PlaylistItem.Artist);
      Item.SubItems.Add(PlaylistItem.Album);
      Item.Data := Pointer(PtrInt(I));
    end;
    lblFilterCount.Caption := '';
  finally
    lvPlaylist.Items.EndUpdate;
  end;
  HighlightCurrentItem;
  ScrollToCurrentItem;
end;

procedure TfrmPlaylist.UpdateStatusBar;
var
  TotalDuration: string;
begin
  if FPlaylistManager = nil then
  begin
    StatusBar.SimpleText := _T('Playlist', 'NoPlaylist', 'No playlist');
    Exit;
  end;

  TotalDuration := FPlaylistManager.GetTotalDurationString;

  StatusBar.SimpleText := Format(_T('Playlist', 'StatusFormat', '%d items | Total: %s'), [
    FPlaylistManager.Count,
    TotalDuration
  ]);
end;

procedure TfrmPlaylist.UpdateToolbarState;
var
  HasItems, HasSelection: Boolean;
begin
  if FPlaylistManager = nil then
  begin
    tbRemove.Enabled := False;
    tbMoveUp.Enabled := False;
    tbMoveDown.Enabled := False;
    tbClear.Enabled := False;
    tbSave.Enabled := False;
    tbShuffle.Down := False;
    tbRepeat.Down := False;
    Exit;
  end;

  HasItems := FPlaylistManager.Count > 0;
  HasSelection := lvPlaylist.SelCount > 0;

  tbRemove.Enabled := HasSelection;
  tbMoveUp.Enabled := HasSelection and (lvPlaylist.Selected <> nil) and
                      (lvPlaylist.Selected.Index > 0);
  tbMoveDown.Enabled := HasSelection and (lvPlaylist.Selected <> nil) and
                        (lvPlaylist.Selected.Index < lvPlaylist.Items.Count - 1);
  tbClear.Enabled := HasItems;
  tbSave.Enabled := HasItems;

  { Update shuffle/repeat button states }
  tbShuffle.Down := FPlaylistManager.PlaybackMode in [pmShuffle, pmShuffleRepeat];
  tbRepeat.Down := FPlaylistManager.PlaybackMode in [pmRepeatOne, pmRepeatAll, pmShuffleRepeat];

  { Update button captions and hints based on mode }
  case FPlaylistManager.PlaybackMode of
    pmNormal:
      begin
        tbShuffle.Hint := _T('Playlist', 'ShuffleOff', 'Shuffle: Off');
        tbRepeat.Caption := _T('Playlist', 'Repeat', 'Repeat');
        tbRepeat.Hint := _T('Playlist', 'HintRepeatEnable', 'Click to enable Repeat All');
      end;
    pmRepeatOne:
      begin
        tbShuffle.Hint := _T('Playlist', 'ShuffleOff', 'Shuffle: Off');
        tbRepeat.Caption := _T('Playlist', 'RepeatOne', 'Rep. 1');
        tbRepeat.Hint := _T('Playlist', 'HintRepeatOne', 'Repeat One - Click to disable');
      end;
    pmRepeatAll:
      begin
        tbShuffle.Hint := _T('Playlist', 'ShuffleOff', 'Shuffle: Off');
        tbRepeat.Caption := _T('Playlist', 'RepeatAll', 'Rep. All');
        tbRepeat.Hint := _T('Playlist', 'HintRepeatAll', 'Repeat All - Click for Repeat One');
      end;
    pmShuffle:
      begin
        tbShuffle.Hint := _T('Playlist', 'ShuffleOn', 'Shuffle: On');
        tbRepeat.Caption := _T('Playlist', 'Repeat', 'Repeat');
        tbRepeat.Hint := _T('Playlist', 'HintRepeatEnable', 'Click to enable Repeat All');
      end;
    pmShuffleRepeat:
      begin
        tbShuffle.Hint := _T('Playlist', 'ShuffleOn', 'Shuffle: On');
        tbRepeat.Caption := _T('Playlist', 'RepeatAll', 'Rep. All');
        tbRepeat.Hint := _T('Playlist', 'HintShuffleRepeat', 'Shuffle + Repeat All - Click to disable repeat');
      end;
  end;
end;

procedure TfrmPlaylist.HighlightCurrentItem;
var
  I: Integer;
begin
  { Mark currently playing item with * prefix, align others with spaces }
  for I := 0 to lvPlaylist.Items.Count - 1 do
  begin
    if (FPlaylistManager <> nil) and (I = FPlaylistManager.CurrentIndex) then
      lvPlaylist.Items[I].Caption := '* ' + IntToStr(I + 1)
    else
      lvPlaylist.Items[I].Caption := '   ' + IntToStr(I + 1);
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PLAYLIST MANAGER EVENT HANDLERS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmPlaylist.OnPlaylistChange(Sender: TObject);
begin
  UpdateList;
  UpdateStatusBar;
  UpdateToolbarState;
end;

procedure TfrmPlaylist.OnPlaylistItemAdded(Sender: TObject; Index: Integer);
begin
  UpdateList;
  UpdateStatusBar;
end;

procedure TfrmPlaylist.OnPlaylistItemRemoved(Sender: TObject; Index: Integer);
begin
  UpdateList;
  UpdateStatusBar;
end;

procedure TfrmPlaylist.OnPlaylistCurrentChange(Sender: TObject; Index: Integer);
begin
  HighlightCurrentItem;
  ScrollToCurrentItem;
end;

procedure TfrmPlaylist.OnPlaylistClear(Sender: TObject);
begin
  lvPlaylist.Items.Clear;
  UpdateStatusBar;
  UpdateToolbarState;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PUBLIC METHODS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmPlaylist.RefreshList;
begin
  UpdateList;
  UpdateStatusBar;
  UpdateToolbarState;
end;

procedure TfrmPlaylist.ScrollToCurrentItem;
var
  CurrentIdx, LastIdx: Integer;
begin
  if FPlaylistManager = nil then Exit;

  CurrentIdx := FPlaylistManager.CurrentIndex;
  if (CurrentIdx < 0) or (CurrentIdx >= lvPlaylist.Items.Count) then Exit;

  { First scroll to end of list, then back to current item }
  { This positions current item near the top }
  LastIdx := lvPlaylist.Items.Count - 1;
  if LastIdx > CurrentIdx then
    lvPlaylist.Items[LastIdx].MakeVisible(False);
  lvPlaylist.Items[CurrentIdx].MakeVisible(False);
end;

procedure TfrmPlaylist.AddFiles(const Files: TStrings);
begin
  if FPlaylistManager = nil then Exit;

  FPlaylistManager.AddFiles(Files);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PHASE 22: SEARCH AND FILTER
  ═══════════════════════════════════════════════════════════════════════════════ }

{ ═══════════════════════════════════════════════════════════════════════════
  ItemMatchesFilter - Check if playlist item matches search text

  Purpose: Determines whether a playlist item contains the search text in
           any of its searchable fields.

  Parameters:
    - Item: The playlist item to check
    - Filter: The search text to look for

  Returns: True if any field contains the filter text, or if filter is empty

  Fields searched:
    - Title
    - Artist
    - Album
    - FileName (full path)

  Notes:
    - Case-insensitive matching using LowerCase
    - Uses substring matching (Pos > 0), not exact match
    - Empty filter matches all items
  ═══════════════════════════════════════════════════════════════════════════ }
function TfrmPlaylist.ItemMatchesFilter(const Item: TPlaylistItem; const Filter: string): Boolean;
var
  LowerFilter: string;
begin
  if Filter = '' then
  begin
    Result := True;
    Exit;
  end;

  LowerFilter := LowerCase(Filter);
  Result := (Pos(LowerFilter, LowerCase(Item.Title)) > 0) or
            (Pos(LowerFilter, LowerCase(Item.Artist)) > 0) or
            (Pos(LowerFilter, LowerCase(Item.Album)) > 0) or
            (Pos(LowerFilter, LowerCase(Item.FileName)) > 0);
end;

{ ═══════════════════════════════════════════════════════════════════════════
  GetRealIndex - Map filtered list index to playlist manager index

  Purpose: Translates a visual index from the filtered ListView to the
           actual index in the playlist manager.

  Parameters:
    - ListIndex: Index in the visible (filtered) ListView

  Returns: Corresponding index in FPlaylistManager

  Notes:
    - When no filter is active (FFilterText = ''), returns ListIndex unchanged
    - FFilteredIndices array maintains the mapping: FFilteredIndices[visual] = real
    - Critical for operations like play, delete, move when filter is active
    - Returns ListIndex for out-of-bounds values (fail-safe)
  ═══════════════════════════════════════════════════════════════════════════ }
function TfrmPlaylist.GetRealIndex(ListIndex: Integer): Integer;
begin
  if (FFilterText = '') or (ListIndex < 0) or (ListIndex >= Length(FFilteredIndices)) then
    Result := ListIndex
  else
    Result := FFilteredIndices[ListIndex];
end;

{ ═══════════════════════════════════════════════════════════════════════════
  UpdateFilteredList - Rebuild ListView with filtered items

  Purpose: Clears and repopulates the ListView with only items matching
           the current search filter (FFilterText).

  Algorithm:
    1. Clear ListView and FFilteredIndices
    2. Iterate all playlist items
    3. For each matching item:
       - Store real index in FFilteredIndices[visualIndex]
       - Add visible row to ListView with item data
       - Store real index in Item.Data for quick lookup
    4. Highlight current track if visible
    5. Update filter count label (e.g., "15/42")

  Notes:
    - Uses BeginUpdate/EndUpdate for performance
    - FFilteredIndices grows dynamically as matches are found
    - Item.Data stores PtrInt(realIndex) for event handlers
    - Preserves original playlist order in filtered view
  ═══════════════════════════════════════════════════════════════════════════ }
procedure TfrmPlaylist.UpdateFilteredList;
var
  I, Count: Integer;
  PlaylistItem: TPlaylistItem;
  Item: TListItem;
begin
  lvPlaylist.Items.BeginUpdate;
  try
    lvPlaylist.Items.Clear;
    SetLength(FFilteredIndices, 0);

    if FPlaylistManager = nil then Exit;

    Count := 0;
    for I := 0 to FPlaylistManager.Count - 1 do
    begin
      PlaylistItem := FPlaylistManager[I];

      if ItemMatchesFilter(PlaylistItem, FFilterText) then
      begin
        { Add to filtered indices }
        SetLength(FFilteredIndices, Count + 1);
        FFilteredIndices[Count] := I;
        Inc(Count);

        { Add to list view }
        Item := lvPlaylist.Items.Add;
        Item.Caption := '   ' + IntToStr(I + 1);
        Item.SubItems.Add(PlaylistItem.Title);
        Item.SubItems.Add(PlaylistItem.DurationString);
        Item.SubItems.Add(PlaylistItem.Artist);
        Item.SubItems.Add(PlaylistItem.Album);
        Item.Data := Pointer(PtrInt(I));
      end;
    end;

    HighlightCurrentItem;

    { Update filter count label }
    if FFilterText <> '' then
      lblFilterCount.Caption := Format('%d/%d', [Count, FPlaylistManager.Count])
    else
      lblFilterCount.Caption := '';

  finally
    lvPlaylist.Items.EndUpdate;
  end;
end;

procedure TfrmPlaylist.edtSearchChange(Sender: TObject);
begin
  FFilterText := Trim(edtSearch.Text);
  UpdateFilteredList;
  UpdateStatusBar;
end;

procedure TfrmPlaylist.btnClearSearchClick(Sender: TObject);
begin
  edtSearch.Text := '';
  FFilterText := '';
  UpdateFilteredList;
  UpdateStatusBar;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PHASE 22: REMOVE DUPLICATES
  ═══════════════════════════════════════════════════════════════════════════════ }

{ ═══════════════════════════════════════════════════════════════════════════
  mnuRemoveDuplicatesClick - Find and remove duplicate playlist entries

  Purpose: Scans the playlist for files with identical paths and offers
           to remove all duplicates, keeping only the first occurrence.

  Algorithm:
    1. Use TStringList with Sorted=True for O(log n) duplicate detection
    2. First pass: Collect indices of all duplicate entries
    3. Ask user for confirmation with count of duplicates found
    4. Second pass: Remove in reverse order to preserve valid indices

  Key implementation details:
    - SeenFiles.Sorted := True enables binary search via IndexOf
    - SeenFiles.Duplicates := dupIgnore prevents TStringList errors
    - Case-insensitive matching using LowerCase on filenames
    - Reverse deletion order: for J := High downto 0 maintains indices

  User interaction:
    - Shows confirmation dialog with duplicate count
    - Shows "No duplicates found" message if playlist is clean
    - Requires at least 2 items in playlist to check
  ═══════════════════════════════════════════════════════════════════════════ }
procedure TfrmPlaylist.mnuRemoveDuplicatesClick(Sender: TObject);
var
  I, J, RemovedCount: Integer;
  SeenFiles: TStringList;
  FileName: string;
  IndicesToRemove: array of Integer;
begin
  if FPlaylistManager = nil then Exit;
  if FPlaylistManager.Count < 2 then Exit;

  SeenFiles := TStringList.Create;
  try
    SeenFiles.Sorted := True;
    SeenFiles.Duplicates := dupIgnore;
    SetLength(IndicesToRemove, 0);

    { Find duplicates (keep first occurrence) }
    for I := 0 to FPlaylistManager.Count - 1 do
    begin
      FileName := LowerCase(FPlaylistManager[I].FileName);
      if SeenFiles.IndexOf(FileName) >= 0 then
      begin
        { This is a duplicate }
        SetLength(IndicesToRemove, Length(IndicesToRemove) + 1);
        IndicesToRemove[High(IndicesToRemove)] := I;
      end
      else
        SeenFiles.Add(FileName);
    end;

    RemovedCount := Length(IndicesToRemove);

    if RemovedCount > 0 then
    begin
      if MessageDlg(_T('Playlist', 'RemoveDuplicatesTitle', 'Remove Duplicates'),
         Format(_T('Playlist', 'DuplicatesFound', 'Found %d duplicate(s). Remove them?'), [RemovedCount]),
         mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        { Remove from end to preserve indices }
        for J := High(IndicesToRemove) downto 0 do
          FPlaylistManager.Delete(IndicesToRemove[J]);

        UpdateList;
        UpdateStatusBar;
      end;
    end
    else
      ShowMessage(_T('Playlist', 'NoDuplicates', 'No duplicates found.'));

  finally
    SeenFiles.Free;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PHASE 22: PLAYLIST PROPERTIES DIALOG
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmPlaylist.ShowPropertiesDialog;
var
  Msg: string;
  TotalDuration, PlayedCount, UnplayedCount: Integer;
  I: Integer;
  TotalSizeBytes: Int64;
  FilePath: string;
  SR: TSearchRec;
begin
  if FPlaylistManager = nil then Exit;

  { Calculate statistics }
  TotalDuration := 0;
  PlayedCount := 0;
  UnplayedCount := 0;
  TotalSizeBytes := 0;

  for I := 0 to FPlaylistManager.Count - 1 do
  begin
    TotalDuration := TotalDuration + Round(FPlaylistManager[I].Duration);

    if FPlaylistManager[I].Played then
      Inc(PlayedCount)
    else
      Inc(UnplayedCount);

    { Try to get file size }
    FilePath := FPlaylistManager[I].FileName;
    if FileExists(FilePath) then
    begin
      if FindFirst(FilePath, faAnyFile, SR) = 0 then
      begin
        TotalSizeBytes := TotalSizeBytes + SR.Size;
        FindClose(SR);
      end;
    end;
  end;

  { Build message }
  Msg := _T('Playlist', 'PropertiesTitle', 'Playlist Properties') + LineEnding +
         '────────────────────────────────' + LineEnding +
         LineEnding +
         Format(_T('Playlist', 'PropTotalItems', 'Total items: %d'), [FPlaylistManager.Count]) + LineEnding +
         Format(_T('Playlist', 'PropTotalDuration', 'Total duration: %s'), [FPlaylistManager.GetTotalDurationString]) + LineEnding +
         LineEnding +
         Format(_T('Playlist', 'PropPlayed', 'Played: %d'), [PlayedCount]) + LineEnding +
         Format(_T('Playlist', 'PropUnplayed', 'Unplayed: %d'), [UnplayedCount]) + LineEnding +
         LineEnding +
         Format(_T('Playlist', 'PropTotalSize', 'Total size: %.2f MB'), [TotalSizeBytes / (1024 * 1024)]) + LineEnding +
         LineEnding +
         Format(_T('Playlist', 'PropShuffle', 'Shuffle: %s'), [BoolToStr(FPlaylistManager.PlaybackMode in [pmShuffle, pmShuffleRepeat],
           _T('Playlist', 'On', 'On'), _T('Playlist', 'Off', 'Off'))]) + LineEnding +
         Format(_T('Playlist', 'PropRepeat', 'Repeat: %s'), [BoolToStr(FPlaylistManager.PlaybackMode in [pmRepeatOne, pmRepeatAll, pmShuffleRepeat],
           _T('Playlist', 'On', 'On'), _T('Playlist', 'Off', 'Off'))]);

  ShowMessage(Msg);
end;

procedure TfrmPlaylist.mnuPropertiesClick(Sender: TObject);
begin
  ShowPropertiesDialog;
end;

end.
