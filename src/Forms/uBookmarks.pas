{ ═══════════════════════════════════════════════════════════════════════════════
  uBookmarks.pas - Bookmarks Window

  Part of 3nity Media - Lazarus Edition

  This window displays and manages bookmarks for media files, allowing
  the user to quickly navigate to saved positions.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uBookmarks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ExtCtrls, StdCtrls, Buttons,
  uTypes, uConstants, uConfig, uLocale;

type
  TBookmarkGotoEvent = procedure(Sender: TObject; const FileName: string; BookmarkPos: Double) of object;

  { TfrmBookmarks }
  TfrmBookmarks = class(TForm)
    lvBookmarks: TListView;
    ToolBar: TToolBar;
    tbGoto: TToolButton;
    tbSep1: TToolButton;
    tbRemove: TToolButton;
    tbClear: TToolButton;
    tbSep2: TToolButton;
    tbRefresh: TToolButton;
    StatusBar: TStatusBar;

    BookmarksPopup: TPopupMenu;
    mnuGoto: TMenuItem;
    mnuSep1: TMenuItem;
    mnuRemove: TMenuItem;
    mnuClear: TMenuItem;
    mnuSep2: TMenuItem;
    mnuOpenFolder: TMenuItem;
    mnuCopyPath: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure lvBookmarksDblClick(Sender: TObject);
    procedure lvBookmarksSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);

    procedure tbGotoClick(Sender: TObject);
    procedure tbRemoveClick(Sender: TObject);
    procedure tbClearClick(Sender: TObject);
    procedure tbRefreshClick(Sender: TObject);

    procedure mnuGotoClick(Sender: TObject);
    procedure mnuRemoveClick(Sender: TObject);
    procedure mnuClearClick(Sender: TObject);
    procedure mnuOpenFolderClick(Sender: TObject);
    procedure mnuCopyPathClick(Sender: TObject);

  private
    FOnGoto: TBookmarkGotoEvent;
    FCurrentFile: string;
    FShowAllFiles: Boolean;
    procedure ApplyLocale;
    procedure LoadBookmarks;
    procedure UpdateStatusBar;
    procedure GotoSelected;
    function FormatTime(Seconds: Double): string;

  public
    property OnGoto: TBookmarkGotoEvent read FOnGoto write FOnGoto;
    property CurrentFile: string read FCurrentFile write FCurrentFile;
    property ShowAllFiles: Boolean read FShowAllFiles write FShowAllFiles;
    procedure RefreshBookmarks;
  end;

var
  frmBookmarks: TfrmBookmarks;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, Clipbrd;

{ TfrmBookmarks }

procedure TfrmBookmarks.FormCreate(Sender: TObject);
begin
  FShowAllFiles := True;
  FCurrentFile := '';

  { Set up list view columns }
  lvBookmarks.Columns.Clear;

  with lvBookmarks.Columns.Add do
  begin
    Caption := 'Name';
    Width := 200;
  end;

  with lvBookmarks.Columns.Add do
  begin
    Caption := 'Position';
    Width := 80;
  end;

  with lvBookmarks.Columns.Add do
  begin
    Caption := 'File';
    Width := 300;
  end;

  with lvBookmarks.Columns.Add do
  begin
    Caption := 'Created';
    Width := 120;
  end;

  ApplyLocale;
end;

procedure TfrmBookmarks.FormShow(Sender: TObject);
begin
  ApplyLocale;
  LoadBookmarks;
end;

procedure TfrmBookmarks.ApplyLocale;
begin
  Caption := _T('Bookmarks', 'Title', 'Bookmarks');

  { Toolbar buttons }
  tbGoto.Caption := _T('Bookmarks', 'Goto', 'Go to');
  tbRemove.Caption := _T('Bookmarks', 'Remove', 'Remove');
  tbClear.Caption := _T('Bookmarks', 'Clear', 'Clear All');
  tbRefresh.Caption := _T('Bookmarks', 'Refresh', 'Refresh');

  { Context menu }
  mnuGoto.Caption := _T('Bookmarks', 'Goto', 'Go to');
  mnuRemove.Caption := _T('Bookmarks', 'Remove', 'Remove');
  mnuClear.Caption := _T('Bookmarks', 'Clear', 'Clear All');
  mnuOpenFolder.Caption := _T('Bookmarks', 'OpenFolder', 'Open Folder');
  mnuCopyPath.Caption := _T('Bookmarks', 'CopyPath', 'Copy Path');

  { List view columns }
  if lvBookmarks.Columns.Count >= 4 then
  begin
    lvBookmarks.Columns[0].Caption := _T('Bookmarks', 'ColName', 'Name');
    lvBookmarks.Columns[1].Caption := _T('Bookmarks', 'ColPosition', 'Position');
    lvBookmarks.Columns[2].Caption := _T('Bookmarks', 'ColFile', 'File');
    lvBookmarks.Columns[3].Caption := _T('Bookmarks', 'ColCreated', 'Created');
  end;
end;

procedure TfrmBookmarks.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TfrmBookmarks.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;

    VK_RETURN:
      GotoSelected;

    VK_DELETE:
      tbRemoveClick(nil);

    VK_F5:
      RefreshBookmarks;
  end;
end;

procedure TfrmBookmarks.lvBookmarksDblClick(Sender: TObject);
begin
  GotoSelected;
end;

procedure TfrmBookmarks.lvBookmarksSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  tbGoto.Enabled := lvBookmarks.Selected <> nil;
  tbRemove.Enabled := lvBookmarks.Selected <> nil;
  mnuGoto.Enabled := lvBookmarks.Selected <> nil;
  mnuRemove.Enabled := lvBookmarks.Selected <> nil;
  mnuOpenFolder.Enabled := lvBookmarks.Selected <> nil;
  mnuCopyPath.Enabled := lvBookmarks.Selected <> nil;
end;

procedure TfrmBookmarks.tbGotoClick(Sender: TObject);
begin
  GotoSelected;
end;

procedure TfrmBookmarks.tbRemoveClick(Sender: TObject);
var
  FileName: string;
  BookmarkPos: Double;
begin
  if lvBookmarks.Selected = nil then Exit;

  { Get bookmark info from selected item }
  FileName := lvBookmarks.Selected.SubItems[1]; { File path in column 2 }
  BookmarkPos := PDouble(lvBookmarks.Selected.Data)^;

  { Remove from config }
  Config.RemoveBookmark(FileName, BookmarkPos);

  { Remove from list }
  lvBookmarks.Items.Delete(lvBookmarks.Selected.Index);

  UpdateStatusBar;
end;

procedure TfrmBookmarks.tbClearClick(Sender: TObject);
begin
  if MessageDlg(_T('Bookmarks', 'ClearTitle', 'Clear Bookmarks'),
    _T('Bookmarks', 'ClearConfirm', 'Are you sure you want to clear all bookmarks?'),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if FShowAllFiles then
      Config.ClearBookmarks
    else
      Config.ClearBookmarksForFile(FCurrentFile);
    lvBookmarks.Items.Clear;
    UpdateStatusBar;
  end;
end;

procedure TfrmBookmarks.tbRefreshClick(Sender: TObject);
begin
  RefreshBookmarks;
end;

procedure TfrmBookmarks.mnuGotoClick(Sender: TObject);
begin
  GotoSelected;
end;

procedure TfrmBookmarks.mnuRemoveClick(Sender: TObject);
begin
  tbRemoveClick(nil);
end;

procedure TfrmBookmarks.mnuClearClick(Sender: TObject);
begin
  tbClearClick(nil);
end;

procedure TfrmBookmarks.mnuOpenFolderClick(Sender: TObject);
var
  FolderPath: string;
begin
  if lvBookmarks.Selected = nil then Exit;

  FolderPath := ExtractFilePath(lvBookmarks.Selected.SubItems[1]);
  if DirectoryExists(FolderPath) then
    OpenDocument(FolderPath);
end;

procedure TfrmBookmarks.mnuCopyPathClick(Sender: TObject);
begin
  if lvBookmarks.Selected = nil then Exit;

  Clipboard.AsText := lvBookmarks.Selected.SubItems[1];
end;

procedure TfrmBookmarks.LoadBookmarks;
var
  I: Integer;
  Item: TListItem;
  Bookmarks: TBookmarkItems;
  PosPtr: PDouble;
begin
  lvBookmarks.Items.BeginUpdate;
  try
    lvBookmarks.Items.Clear;

    { Get bookmarks from config }
    if FShowAllFiles then
      Bookmarks := Config.GetBookmarks
    else
      Bookmarks := Config.GetBookmarksForFile(FCurrentFile);

    for I := 0 to High(Bookmarks) do
    begin
      Item := lvBookmarks.Items.Add;
      Item.Caption := Bookmarks[I].Name;
      Item.SubItems.Add(FormatTime(Bookmarks[I].Position));
      Item.SubItems.Add(Bookmarks[I].FileName);
      Item.SubItems.Add(DateTimeToStr(Bookmarks[I].CreatedAt));

      { Store position as data pointer }
      New(PosPtr);
      PosPtr^ := Bookmarks[I].Position;
      Item.Data := PosPtr;
    end;
  finally
    lvBookmarks.Items.EndUpdate;
  end;

  UpdateStatusBar;
end;

procedure TfrmBookmarks.UpdateStatusBar;
begin
  StatusBar.SimpleText := Format(_T('Bookmarks', 'Count', '%d bookmark(s)'), [lvBookmarks.Items.Count]);
end;

procedure TfrmBookmarks.GotoSelected;
var
  FileName: string;
  BookmarkPos: Double;
begin
  if lvBookmarks.Selected = nil then Exit;

  FileName := lvBookmarks.Selected.SubItems[1];
  BookmarkPos := PDouble(lvBookmarks.Selected.Data)^;

  if Assigned(FOnGoto) then
    FOnGoto(Self, FileName, BookmarkPos);
end;

function TfrmBookmarks.FormatTime(Seconds: Double): string;
var
  H, M, S: Integer;
begin
  if Seconds < 0 then
    Seconds := 0;

  H := Trunc(Seconds) div 3600;
  M := (Trunc(Seconds) mod 3600) div 60;
  S := Trunc(Seconds) mod 60;

  if H > 0 then
    Result := Format('%d:%02d:%02d', [H, M, S])
  else
    Result := Format('%d:%02d', [M, S]);
end;

procedure TfrmBookmarks.RefreshBookmarks;
begin
  LoadBookmarks;
end;

end.
