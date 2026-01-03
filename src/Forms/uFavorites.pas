{ ═══════════════════════════════════════════════════════════════════════════════
  uFavorites.pas - Favorites Window

  Part of 3nity Media - Lazarus Edition

  This window displays and manages favorite files and streams, allowing
  quick access to frequently used media.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uFavorites;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ExtCtrls, StdCtrls, Buttons,
  uTypes, uConstants, uConfig, uLocale;

type
  TFavoritePlayEvent = procedure(Sender: TObject; const Path: string) of object;

  { TfrmFavorites }
  TfrmFavorites = class(TForm)
    lvFavorites: TListView;
    ToolBar: TToolBar;
    tbPlay: TToolButton;
    tbSep1: TToolButton;
    tbRemove: TToolButton;
    tbClear: TToolButton;
    tbSep2: TToolButton;
    tbRefresh: TToolButton;
    StatusBar: TStatusBar;

    FavoritesPopup: TPopupMenu;
    mnuPlay: TMenuItem;
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

    procedure lvFavoritesDblClick(Sender: TObject);
    procedure lvFavoritesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);

    procedure tbPlayClick(Sender: TObject);
    procedure tbRemoveClick(Sender: TObject);
    procedure tbClearClick(Sender: TObject);
    procedure tbRefreshClick(Sender: TObject);

    procedure mnuPlayClick(Sender: TObject);
    procedure mnuRemoveClick(Sender: TObject);
    procedure mnuClearClick(Sender: TObject);
    procedure mnuOpenFolderClick(Sender: TObject);
    procedure mnuCopyPathClick(Sender: TObject);

  private
    FOnPlay: TFavoritePlayEvent;
    procedure ApplyLocale;
    procedure LoadFavorites;
    procedure UpdateStatusBar;
    procedure PlaySelected;
    function FavoriteTypeToString(FavType: TFavoriteType): string;

  public
    property OnPlay: TFavoritePlayEvent read FOnPlay write FOnPlay;
    procedure RefreshFavorites;
  end;

var
  frmFavorites: TfrmFavorites;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, Clipbrd;

{ TfrmFavorites }

procedure TfrmFavorites.FormCreate(Sender: TObject);
begin
  { Set up list view columns }
  lvFavorites.Columns.Clear;

  with lvFavorites.Columns.Add do
  begin
    Caption := 'Name';
    Width := 200;
  end;

  with lvFavorites.Columns.Add do
  begin
    Caption := 'Type';
    Width := 80;
  end;

  with lvFavorites.Columns.Add do
  begin
    Caption := 'Path';
    Width := 300;
  end;

  ApplyLocale;
end;

procedure TfrmFavorites.FormShow(Sender: TObject);
begin
  ApplyLocale;
  LoadFavorites;
end;

procedure TfrmFavorites.ApplyLocale;
begin
  Caption := _T('Favorites', 'Title', 'Favorites');

  { Toolbar buttons }
  tbPlay.Caption := _T('Favorites', 'Play', 'Play');
  tbRemove.Caption := _T('Favorites', 'Remove', 'Remove');
  tbClear.Caption := _T('Favorites', 'Clear', 'Clear All');
  tbRefresh.Caption := _T('Favorites', 'Refresh', 'Refresh');

  { Context menu }
  mnuPlay.Caption := _T('Favorites', 'Play', 'Play');
  mnuRemove.Caption := _T('Favorites', 'Remove', 'Remove');
  mnuClear.Caption := _T('Favorites', 'Clear', 'Clear All');
  mnuOpenFolder.Caption := _T('Favorites', 'OpenFolder', 'Open Folder');
  mnuCopyPath.Caption := _T('Favorites', 'CopyPath', 'Copy Path');

  { List view columns }
  if lvFavorites.Columns.Count >= 3 then
  begin
    lvFavorites.Columns[0].Caption := _T('Favorites', 'ColName', 'Name');
    lvFavorites.Columns[1].Caption := _T('Favorites', 'ColType', 'Type');
    lvFavorites.Columns[2].Caption := _T('Favorites', 'ColPath', 'Path');
  end;
end;

procedure TfrmFavorites.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TfrmFavorites.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;

    VK_RETURN:
      PlaySelected;

    VK_DELETE:
      tbRemoveClick(nil);

    VK_F5:
      RefreshFavorites;
  end;
end;

procedure TfrmFavorites.lvFavoritesDblClick(Sender: TObject);
begin
  PlaySelected;
end;

procedure TfrmFavorites.lvFavoritesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  HasSelection: Boolean;
  FavPath: string;
begin
  HasSelection := lvFavorites.Selected <> nil;

  tbPlay.Enabled := HasSelection;
  tbRemove.Enabled := HasSelection;
  mnuPlay.Enabled := HasSelection;
  mnuRemove.Enabled := HasSelection;
  mnuCopyPath.Enabled := HasSelection;

  { Only enable Open Folder for local files }
  if HasSelection then
  begin
    FavPath := lvFavorites.Selected.SubItems[1];
    mnuOpenFolder.Enabled := FileExists(FavPath) or DirectoryExists(ExtractFilePath(FavPath));
  end
  else
    mnuOpenFolder.Enabled := False;
end;

procedure TfrmFavorites.tbPlayClick(Sender: TObject);
begin
  PlaySelected;
end;

procedure TfrmFavorites.tbRemoveClick(Sender: TObject);
var
  Path: string;
begin
  if lvFavorites.Selected = nil then Exit;

  { Get path from SubItems }
  Path := lvFavorites.Selected.SubItems[1];

  { Remove from config }
  Config.RemoveFavorite(Path);

  { Remove from list }
  lvFavorites.Items.Delete(lvFavorites.Selected.Index);

  UpdateStatusBar;
end;

procedure TfrmFavorites.tbClearClick(Sender: TObject);
begin
  if MessageDlg(_T('Favorites', 'ClearTitle', 'Clear Favorites'),
    _T('Favorites', 'ClearConfirm', 'Are you sure you want to clear all favorites?'),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Config.ClearFavorites;
    lvFavorites.Items.Clear;
    UpdateStatusBar;
  end;
end;

procedure TfrmFavorites.tbRefreshClick(Sender: TObject);
begin
  RefreshFavorites;
end;

procedure TfrmFavorites.mnuPlayClick(Sender: TObject);
begin
  PlaySelected;
end;

procedure TfrmFavorites.mnuRemoveClick(Sender: TObject);
begin
  tbRemoveClick(nil);
end;

procedure TfrmFavorites.mnuClearClick(Sender: TObject);
begin
  tbClearClick(nil);
end;

procedure TfrmFavorites.mnuOpenFolderClick(Sender: TObject);
var
  FolderPath: string;
begin
  if lvFavorites.Selected = nil then Exit;

  FolderPath := ExtractFilePath(lvFavorites.Selected.SubItems[1]);
  if DirectoryExists(FolderPath) then
    OpenDocument(FolderPath);
end;

procedure TfrmFavorites.mnuCopyPathClick(Sender: TObject);
begin
  if lvFavorites.Selected = nil then Exit;

  Clipboard.AsText := lvFavorites.Selected.SubItems[1];
end;

procedure TfrmFavorites.LoadFavorites;
var
  I: Integer;
  Item: TListItem;
  Favorites: TFavoriteItems;
begin
  lvFavorites.Items.BeginUpdate;
  try
    lvFavorites.Items.Clear;

    { Get favorites from config }
    Favorites := Config.GetFavorites;

    for I := 0 to High(Favorites) do
    begin
      Item := lvFavorites.Items.Add;
      Item.Caption := Favorites[I].Name;
      Item.SubItems.Add(FavoriteTypeToString(Favorites[I].FavoriteType));
      Item.SubItems.Add(Favorites[I].Path);
    end;
  finally
    lvFavorites.Items.EndUpdate;
  end;

  UpdateStatusBar;
end;

procedure TfrmFavorites.UpdateStatusBar;
begin
  StatusBar.SimpleText := Format(_T('Favorites', 'Count', '%d favorite(s)'), [lvFavorites.Items.Count]);
end;

procedure TfrmFavorites.PlaySelected;
var
  Path: string;
begin
  if lvFavorites.Selected = nil then Exit;

  Path := lvFavorites.Selected.SubItems[1];

  { Update last played }
  Config.UpdateFavoriteLastPlayed(Path);

  if Assigned(FOnPlay) then
    FOnPlay(Self, Path);
end;

function TfrmFavorites.FavoriteTypeToString(FavType: TFavoriteType): string;
begin
  case FavType of
    ftFile: Result := _T('Favorites', 'TypeFile', 'File');
    ftURL: Result := _T('Favorites', 'TypeURL', 'URL');
    ftRadio: Result := _T('Favorites', 'TypeRadio', 'Radio');
    ftDVD: Result := _T('Favorites', 'TypeDVD', 'DVD');
    ftBluray: Result := _T('Favorites', 'TypeBluray', 'Blu-ray');
  else
    Result := _T('Favorites', 'TypeUnknown', 'Unknown');
  end;
end;

procedure TfrmFavorites.RefreshFavorites;
begin
  LoadFavorites;
end;

end.
