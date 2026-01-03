{ ═══════════════════════════════════════════════════════════════════════════════
  uHistory.pas - Playback History Window

  Part of 3nity Media - Lazarus Edition

  This window displays the history of recently played files and allows
  the user to play them again or manage the history.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ExtCtrls, StdCtrls, Buttons,
  uTypes, uConstants, uConfig, uLocale;

type
  THistoryPlayEvent = procedure(Sender: TObject; const FileName: string) of object;

  { TfrmHistory }
  TfrmHistory = class(TForm)
    lvHistory: TListView;
    ToolBar: TToolBar;
    tbPlay: TToolButton;
    tbSep1: TToolButton;
    tbRemove: TToolButton;
    tbClear: TToolButton;
    tbSep2: TToolButton;
    tbRefresh: TToolButton;
    StatusBar: TStatusBar;

    HistoryPopup: TPopupMenu;
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

    procedure lvHistoryDblClick(Sender: TObject);
    procedure lvHistorySelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);

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
    FOnPlay: THistoryPlayEvent;
    procedure ApplyLocale;
    procedure LoadHistory;
    procedure UpdateStatusBar;
    procedure PlaySelected;

  public
    property OnPlay: THistoryPlayEvent read FOnPlay write FOnPlay;
    procedure RefreshHistory;
  end;

var
  frmHistory: TfrmHistory;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, Clipbrd;

{ TfrmHistory }

procedure TfrmHistory.FormCreate(Sender: TObject);
begin
  { Set up list view columns }
  lvHistory.Columns.Clear;

  with lvHistory.Columns.Add do
  begin
    Caption := 'File Name';
    Width := 300;
  end;

  with lvHistory.Columns.Add do
  begin
    Caption := 'Path';
    Width := 400;
  end;

  ApplyLocale;
end;

procedure TfrmHistory.FormShow(Sender: TObject);
begin
  ApplyLocale;
  LoadHistory;
end;

procedure TfrmHistory.ApplyLocale;
begin
  Caption := _T('History', 'Title', 'History');

  { Toolbar buttons }
  tbPlay.Caption := _T('History', 'Play', 'Play');
  tbRemove.Caption := _T('History', 'Remove', 'Remove');
  tbClear.Caption := _T('History', 'Clear', 'Clear All');
  tbRefresh.Caption := _T('History', 'Refresh', 'Refresh');

  { Context menu }
  mnuPlay.Caption := _T('History', 'Play', 'Play');
  mnuRemove.Caption := _T('History', 'Remove', 'Remove');
  mnuClear.Caption := _T('History', 'Clear', 'Clear All');
  mnuOpenFolder.Caption := _T('History', 'OpenFolder', 'Open Folder');
  mnuCopyPath.Caption := _T('History', 'CopyPath', 'Copy Path');

  { List view columns }
  if lvHistory.Columns.Count >= 2 then
  begin
    lvHistory.Columns[0].Caption := _T('History', 'ColFileName', 'File Name');
    lvHistory.Columns[1].Caption := _T('History', 'ColPath', 'Path');
  end;
end;

procedure TfrmHistory.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TfrmHistory.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;

    VK_RETURN:
      PlaySelected;

    VK_DELETE:
      tbRemoveClick(nil);

    VK_F5:
      RefreshHistory;
  end;
end;

procedure TfrmHistory.lvHistoryDblClick(Sender: TObject);
begin
  PlaySelected;
end;

procedure TfrmHistory.lvHistorySelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  tbPlay.Enabled := lvHistory.Selected <> nil;
  tbRemove.Enabled := lvHistory.Selected <> nil;
  mnuPlay.Enabled := lvHistory.Selected <> nil;
  mnuRemove.Enabled := lvHistory.Selected <> nil;
  mnuOpenFolder.Enabled := lvHistory.Selected <> nil;
  mnuCopyPath.Enabled := lvHistory.Selected <> nil;
end;

procedure TfrmHistory.tbPlayClick(Sender: TObject);
begin
  PlaySelected;
end;

procedure TfrmHistory.tbRemoveClick(Sender: TObject);
var
  I: Integer;
  FileName: string;
begin
  if lvHistory.Selected = nil then Exit;

  { Get the full path from SubItems }
  FileName := lvHistory.Selected.SubItems[0];

  { Remove from config }
  Config.RemoveRecentFile(FileName);

  { Remove from list }
  I := lvHistory.Selected.Index;
  lvHistory.Items.Delete(I);

  UpdateStatusBar;
end;

procedure TfrmHistory.tbClearClick(Sender: TObject);
begin
  if MessageDlg(_T('History', 'ClearTitle', 'Clear History'),
    _T('History', 'ClearConfirm', 'Are you sure you want to clear all history?'),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Config.ClearRecentFiles;
    lvHistory.Items.Clear;
    UpdateStatusBar;
  end;
end;

procedure TfrmHistory.tbRefreshClick(Sender: TObject);
begin
  RefreshHistory;
end;

procedure TfrmHistory.mnuPlayClick(Sender: TObject);
begin
  PlaySelected;
end;

procedure TfrmHistory.mnuRemoveClick(Sender: TObject);
begin
  tbRemoveClick(nil);
end;

procedure TfrmHistory.mnuClearClick(Sender: TObject);
begin
  tbClearClick(nil);
end;

procedure TfrmHistory.mnuOpenFolderClick(Sender: TObject);
var
  FolderPath: string;
begin
  if lvHistory.Selected = nil then Exit;

  FolderPath := ExtractFilePath(lvHistory.Selected.SubItems[0]);
  if DirectoryExists(FolderPath) then
    OpenDocument(FolderPath);
end;

procedure TfrmHistory.mnuCopyPathClick(Sender: TObject);
begin
  if lvHistory.Selected = nil then Exit;

  Clipboard.AsText := lvHistory.Selected.SubItems[0];
end;

procedure TfrmHistory.LoadHistory;
var
  I: Integer;
  Item: TListItem;
  FilePath: string;
  RecentFiles: TStringList;
begin
  lvHistory.Items.BeginUpdate;
  try
    lvHistory.Items.Clear;

    { Get recent files from config - returns new TStringList that caller owns }
    RecentFiles := Config.GetRecentFiles;
    try
      for I := 0 to RecentFiles.Count - 1 do
      begin
        FilePath := RecentFiles[I];

        Item := lvHistory.Items.Add;
        Item.Caption := ExtractFileName(FilePath);
        Item.SubItems.Add(FilePath);
      end;
    finally
      RecentFiles.Free;
    end;
  finally
    lvHistory.Items.EndUpdate;
  end;

  UpdateStatusBar;
end;

procedure TfrmHistory.UpdateStatusBar;
begin
  StatusBar.SimpleText := Format(_T('History', 'Count', '%d item(s) in history'), [lvHistory.Items.Count]);
end;

procedure TfrmHistory.PlaySelected;
var
  FileName: string;
begin
  if lvHistory.Selected = nil then Exit;

  FileName := lvHistory.Selected.SubItems[0];

  if Assigned(FOnPlay) then
    FOnPlay(Self, FileName);
end;

procedure TfrmHistory.RefreshHistory;
begin
  LoadHistory;
end;

end.
