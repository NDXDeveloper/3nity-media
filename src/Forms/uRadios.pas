{ ═══════════════════════════════════════════════════════════════════════════════
  uRadios.pas - Online Radio Stations Form

  Part of 3nity Media - Lazarus Edition

  This form provides the user interface for browsing and playing online radio
  stations. Supports Icecast directory, custom stations, and favorites.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uRadios;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Menus, LCLType, uRadioManager, uTypes, uLocale;

type
  { TfrmRadios }
  TfrmRadios = class(TForm)
    { Toolbar }
    ToolBar: TToolBar;
    tbRefresh: TToolButton;
    tbSep1: TToolButton;
    tbAddCustom: TToolButton;
    tbEditCustom: TToolButton;
    tbDeleteCustom: TToolButton;
    tbSep2: TToolButton;
    tbFavorite: TToolButton;
    tbShowFavorites: TToolButton;
    tbSep3: TToolButton;
    tbShowAll: TToolButton;

    { Filter panel }
    pnlFilter: TPanel;
    lblGenre: TLabel;
    cmbGenre: TComboBox;
    lblSearch: TLabel;
    edtSearch: TEdit;
    btnSearch: TButton;
    btnClearSearch: TButton;

    { Station list }
    lvStations: TListView;

    { Status bar }
    StatusBar: TStatusBar;

    { Popup menu }
    pmStations: TPopupMenu;
    mnuPlay: TMenuItem;
    mnuSep1: TMenuItem;
    mnuAddFavorite: TMenuItem;
    mnuRemoveFavorite: TMenuItem;
    mnuSep2: TMenuItem;
    mnuAddCustom: TMenuItem;
    mnuEditCustom: TMenuItem;
    mnuDeleteCustom: TMenuItem;
    mnuSep3: TMenuItem;
    mnuCopyURL: TMenuItem;
    mnuOpenWebsite: TMenuItem;

    { Form events }
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    { Toolbar events }
    procedure tbRefreshClick(Sender: TObject);
    procedure tbAddCustomClick(Sender: TObject);
    procedure tbEditCustomClick(Sender: TObject);
    procedure tbDeleteCustomClick(Sender: TObject);
    procedure tbFavoriteClick(Sender: TObject);
    procedure tbShowFavoritesClick(Sender: TObject);
    procedure tbShowAllClick(Sender: TObject);

    { Filter events }
    procedure cmbGenreChange(Sender: TObject);
    procedure edtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnSearchClick(Sender: TObject);
    procedure btnClearSearchClick(Sender: TObject);

    { ListView events }
    procedure lvStationsDblClick(Sender: TObject);
    procedure lvStationsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lvStationsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);

    { Menu events }
    procedure mnuPlayClick(Sender: TObject);
    procedure mnuAddFavoriteClick(Sender: TObject);
    procedure mnuRemoveFavoriteClick(Sender: TObject);
    procedure mnuAddCustomClick(Sender: TObject);
    procedure mnuEditCustomClick(Sender: TObject);
    procedure mnuDeleteCustomClick(Sender: TObject);
    procedure mnuCopyURLClick(Sender: TObject);
    procedure mnuOpenWebsiteClick(Sender: TObject);

  private
    FRadioManager: TRadioManager;
    FOnPlayRequest: TRadioPlayEvent;
    FShowingFavorites: Boolean;

    procedure RefreshList;
    procedure UpdateGenreCombo;
    procedure UpdateStatusBar;
    procedure PlaySelected;

    { Radio manager event handlers }
    procedure OnRadioLoad(Sender: TObject; ACount: Integer);
    procedure OnRadioError(Sender: TObject; const ErrorMsg: string);
    procedure OnRadioChange(Sender: TObject);

  public
    property RadioManager: TRadioManager read FRadioManager write FRadioManager;
    property OnPlayRequest: TRadioPlayEvent read FOnPlayRequest write FOnPlayRequest;

    procedure ApplyLocale;
  end;

var
  frmRadios: TfrmRadios;

implementation

{$R *.lfm}

uses
  Clipbrd, LCLIntf;

{ ═══════════════════════════════════════════════════════════════════════════════
  FORM EVENTS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmRadios.FormCreate(Sender: TObject);
begin
  FShowingFavorites := False;

  { Initialize ListView columns }
  lvStations.ViewStyle := vsReport;
  lvStations.RowSelect := True;
  lvStations.ReadOnly := True;

  { Add columns }
  with lvStations.Columns.Add do
  begin
    Caption := 'Name';
    Width := 200;
  end;
  with lvStations.Columns.Add do
  begin
    Caption := 'Genre';
    Width := 120;
  end;
  with lvStations.Columns.Add do
  begin
    Caption := 'Bitrate';
    Width := 70;
    Alignment := taRightJustify;
  end;
  with lvStations.Columns.Add do
  begin
    Caption := 'Country';
    Width := 100;
  end;
  with lvStations.Columns.Add do
  begin
    Caption := 'Codec';
    Width := 80;
  end;

  { Initialize genre combo - will be properly translated in ApplyLocale }
  cmbGenre.Items.Clear;
  cmbGenre.Items.Add('(All Genres)');
  cmbGenre.ItemIndex := 0;

  ApplyLocale;
end;

procedure TfrmRadios.ApplyLocale;
begin
  Caption := _T('Radios', 'Title', 'Online Radio Stations');

  { Toolbar }
  tbRefresh.Caption := _T('Radios', 'Refresh', 'Refresh');
  tbAddCustom.Caption := _T('Radios', 'Add', 'Add');
  tbEditCustom.Caption := _T('Radios', 'Edit', 'Edit');
  tbDeleteCustom.Caption := _T('Radios', 'Delete', 'Delete');
  tbFavorite.Caption := _T('Radios', 'Favorite', 'Favorite');
  tbShowFavorites.Caption := _T('Radios', 'ShowFavorites', 'Favorites');
  tbShowAll.Caption := _T('Radios', 'ShowAll', 'All');

  { Filter panel }
  lblGenre.Caption := _T('Radios', 'Genre', 'Genre:');
  lblSearch.Caption := _T('Radios', 'Search', 'Search:');
  btnSearch.Caption := _T('Radios', 'SearchBtn', 'Search');
  btnClearSearch.Caption := _T('Radios', 'ClearSearch', 'Clear');

  { Context menu }
  mnuPlay.Caption := _T('Radios', 'Play', 'Play');
  mnuAddFavorite.Caption := _T('Radios', 'AddFavorite', 'Add to Favorites');
  mnuRemoveFavorite.Caption := _T('Radios', 'RemoveFavorite', 'Remove from Favorites');
  mnuAddCustom.Caption := _T('Radios', 'AddCustom', 'Add Custom Station');
  mnuEditCustom.Caption := _T('Radios', 'EditCustom', 'Edit Station');
  mnuDeleteCustom.Caption := _T('Radios', 'DeleteCustom', 'Delete Station');
  mnuCopyURL.Caption := _T('Radios', 'CopyURL', 'Copy URL');
  mnuOpenWebsite.Caption := _T('Radios', 'OpenWebsite', 'Open Website');

  { Columns }
  if lvStations.Columns.Count >= 5 then
  begin
    lvStations.Columns[0].Caption := _T('Radios', 'ColName', 'Name');
    lvStations.Columns[1].Caption := _T('Radios', 'ColGenre', 'Genre');
    lvStations.Columns[2].Caption := _T('Radios', 'ColBitrate', 'Bitrate');
    lvStations.Columns[3].Caption := _T('Radios', 'ColCountry', 'Country');
    lvStations.Columns[4].Caption := _T('Radios', 'ColCodec', 'Codec');
  end;

  { Update genre combo first item }
  if cmbGenre.Items.Count > 0 then
    cmbGenre.Items[0] := _T('Radios', 'AllGenres', '(All Genres)');
end;

procedure TfrmRadios.FormDestroy(Sender: TObject);
begin
  { Radio manager is owned by main form }
end;

procedure TfrmRadios.FormShow(Sender: TObject);
begin
  ApplyLocale;

  if FRadioManager <> nil then
  begin
    { Connect events }
    FRadioManager.OnLoad := @OnRadioLoad;
    FRadioManager.OnError := @OnRadioError;
    FRadioManager.OnChange := @OnRadioChange;

    { Load default stations if empty }
    if FRadioManager.Count = 0 then
      FRadioManager.LoadDefaultStations;

    UpdateGenreCombo;
    RefreshList;
  end;
end;

procedure TfrmRadios.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  TOOLBAR EVENTS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmRadios.tbRefreshClick(Sender: TObject);
begin
  if FRadioManager <> nil then
  begin
    StatusBar.SimpleText := _T('Radios', 'LoadingIcecast', 'Loading stations from Icecast directory...');
    Application.ProcessMessages;

    if not FRadioManager.LoadFromIcecast then
    begin
      { Fall back to default stations }
      FRadioManager.LoadDefaultStations;
      ShowMessage(_T('Radios', 'IcecastError', 'Could not load Icecast directory. Using default stations.'));
    end;
  end;
end;

procedure TfrmRadios.tbAddCustomClick(Sender: TObject);
var
  StationName, StationURL, StationGenre: string;
  DlgTitle: string;
begin
  if FRadioManager = nil then Exit;

  StationName := '';
  StationURL := '';
  StationGenre := '';
  DlgTitle := _T('Radios', 'AddTitle', 'Add Custom Station');

  if InputQuery(DlgTitle, _T('Radios', 'StationName', 'Station Name:'), StationName) then
    if InputQuery(DlgTitle, _T('Radios', 'StreamURL', 'Stream URL:'), StationURL) then
      if InputQuery(DlgTitle, _T('Radios', 'GenrePrompt', 'Genre:'), StationGenre) then
      begin
        if (StationName <> '') and (StationURL <> '') then
        begin
          FRadioManager.AddCustomStation(StationName, StationURL, StationGenre);
          RefreshList;
        end
        else
          ShowMessage(_T('Radios', 'NameURLRequired', 'Name and URL are required.'));
      end;
end;

procedure TfrmRadios.tbEditCustomClick(Sender: TObject);
var
  Station: TRadioStation;
  StationName, StationURL, StationGenre: string;
  Index: Integer;
  DlgTitle: string;
begin
  if (FRadioManager = nil) or (lvStations.Selected = nil) then Exit;

  Index := lvStations.Selected.Index;
  if Index < 0 then Exit;

  Station := FRadioManager.FilteredStations[Index];

  StationName := Station.Name;
  StationURL := Station.URL;
  StationGenre := Station.Genre;
  DlgTitle := _T('Radios', 'EditTitle', 'Edit Station');

  if InputQuery(DlgTitle, _T('Radios', 'StationName', 'Station Name:'), StationName) then
    if InputQuery(DlgTitle, _T('Radios', 'StreamURL', 'Stream URL:'), StationURL) then
      if InputQuery(DlgTitle, _T('Radios', 'GenrePrompt', 'Genre:'), StationGenre) then
      begin
        { Note: This only works for custom stations }
        FRadioManager.EditCustomStation(Index - FRadioManager.Count,
          StationName, StationURL, StationGenre, Station.Bitrate);
        RefreshList;
      end;
end;

procedure TfrmRadios.tbDeleteCustomClick(Sender: TObject);
var
  Index: Integer;
begin
  if (FRadioManager = nil) or (lvStations.Selected = nil) then Exit;

  Index := lvStations.Selected.Index;
  if Index < 0 then Exit;

  if MessageDlg(_T('Radios', 'DeleteTitle', 'Delete Station'),
    _T('Radios', 'DeleteConfirm', 'Are you sure you want to delete this station?'),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FRadioManager.DeleteCustomStation(Index - FRadioManager.Count);
    RefreshList;
  end;
end;

procedure TfrmRadios.tbFavoriteClick(Sender: TObject);
var
  Station: TRadioStation;
  Index: Integer;
begin
  if (FRadioManager = nil) or (lvStations.Selected = nil) then Exit;

  Index := lvStations.Selected.Index;
  if Index < 0 then Exit;

  Station := FRadioManager.FilteredStations[Index];
  FRadioManager.ToggleFavorite(Station);

  if FShowingFavorites then
    RefreshList;
end;

procedure TfrmRadios.tbShowFavoritesClick(Sender: TObject);
begin
  if FRadioManager = nil then Exit;

  FShowingFavorites := True;
  tbShowFavorites.Down := True;
  tbShowAll.Down := False;

  FRadioManager.FilterFavorites;
  RefreshList;
end;

procedure TfrmRadios.tbShowAllClick(Sender: TObject);
begin
  if FRadioManager = nil then Exit;

  FShowingFavorites := False;
  tbShowAll.Down := True;
  tbShowFavorites.Down := False;

  FRadioManager.ClearFilter;
  cmbGenre.ItemIndex := 0;
  edtSearch.Text := '';
  RefreshList;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  FILTER EVENTS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmRadios.cmbGenreChange(Sender: TObject);
begin
  if FRadioManager = nil then Exit;

  FShowingFavorites := False;
  tbShowAll.Down := True;
  tbShowFavorites.Down := False;

  if cmbGenre.ItemIndex <= 0 then
    FRadioManager.ClearFilter
  else
    FRadioManager.FilterByGenre(cmbGenre.Text);

  RefreshList;
end;

procedure TfrmRadios.edtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    btnSearchClick(nil);
end;

procedure TfrmRadios.btnSearchClick(Sender: TObject);
begin
  if FRadioManager = nil then Exit;

  FShowingFavorites := False;
  tbShowAll.Down := True;
  tbShowFavorites.Down := False;

  if edtSearch.Text = '' then
    FRadioManager.ClearFilter
  else
    FRadioManager.FilterByName(edtSearch.Text);

  cmbGenre.ItemIndex := 0;
  RefreshList;
end;

procedure TfrmRadios.btnClearSearchClick(Sender: TObject);
begin
  edtSearch.Text := '';
  if FRadioManager <> nil then
  begin
    FRadioManager.ClearFilter;
    cmbGenre.ItemIndex := 0;
    RefreshList;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  LISTVIEW EVENTS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmRadios.lvStationsDblClick(Sender: TObject);
begin
  PlaySelected;
end;

procedure TfrmRadios.lvStationsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      PlaySelected;
    VK_DELETE:
      tbDeleteCustomClick(nil);
    VK_F:
      if ssCtrl in Shift then
        edtSearch.SetFocus;
  end;
end;

procedure TfrmRadios.lvStationsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  UpdateStatusBar;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  MENU EVENTS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmRadios.mnuPlayClick(Sender: TObject);
begin
  PlaySelected;
end;

procedure TfrmRadios.mnuAddFavoriteClick(Sender: TObject);
begin
  tbFavoriteClick(nil);
end;

procedure TfrmRadios.mnuRemoveFavoriteClick(Sender: TObject);
begin
  tbFavoriteClick(nil);
end;

procedure TfrmRadios.mnuAddCustomClick(Sender: TObject);
begin
  tbAddCustomClick(nil);
end;

procedure TfrmRadios.mnuEditCustomClick(Sender: TObject);
begin
  tbEditCustomClick(nil);
end;

procedure TfrmRadios.mnuDeleteCustomClick(Sender: TObject);
begin
  tbDeleteCustomClick(nil);
end;

procedure TfrmRadios.mnuCopyURLClick(Sender: TObject);
var
  Station: TRadioStation;
  Index: Integer;
begin
  if (FRadioManager = nil) or (lvStations.Selected = nil) then Exit;

  Index := lvStations.Selected.Index;
  if Index >= 0 then
  begin
    Station := FRadioManager.FilteredStations[Index];
    Clipboard.AsText := Station.URL;
  end;
end;

procedure TfrmRadios.mnuOpenWebsiteClick(Sender: TObject);
var
  Station: TRadioStation;
  Index: Integer;
begin
  if (FRadioManager = nil) or (lvStations.Selected = nil) then Exit;

  Index := lvStations.Selected.Index;
  if Index >= 0 then
  begin
    Station := FRadioManager.FilteredStations[Index];
    if Station.Website <> '' then
      OpenURL(Station.Website);
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PRIVATE METHODS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmRadios.RefreshList;
var
  I: Integer;
  Station: TRadioStation;
  Item: TListItem;
begin
  if FRadioManager = nil then Exit;

  lvStations.Items.BeginUpdate;
  try
    lvStations.Items.Clear;

    for I := 0 to FRadioManager.FilteredCount - 1 do
    begin
      Station := FRadioManager.FilteredStations[I];

      Item := lvStations.Items.Add;
      Item.Caption := Station.Name;
      Item.SubItems.Add(Station.Genre);

      if Station.Bitrate > 0 then
        Item.SubItems.Add(IntToStr(Station.Bitrate) + ' kbps')
      else
        Item.SubItems.Add('');

      Item.SubItems.Add(Station.Country);
      Item.SubItems.Add(Station.Codec);

      { Mark favorites }
      if FRadioManager.IsFavorite(Station.URL) then
        Item.Caption := '* ' + Item.Caption;
    end;
  finally
    lvStations.Items.EndUpdate;
  end;

  UpdateStatusBar;
end;

procedure TfrmRadios.UpdateGenreCombo;
var
  I: Integer;
  SavedIndex: Integer;
begin
  if FRadioManager = nil then Exit;

  SavedIndex := cmbGenre.ItemIndex;

  cmbGenre.Items.BeginUpdate;
  try
    cmbGenre.Items.Clear;
    cmbGenre.Items.Add(_T('Radios', 'AllGenres', '(All Genres)'));

    for I := 0 to FRadioManager.Genres.Count - 1 do
      cmbGenre.Items.Add(FRadioManager.Genres[I]);
  finally
    cmbGenre.Items.EndUpdate;
  end;

  if (SavedIndex >= 0) and (SavedIndex < cmbGenre.Items.Count) then
    cmbGenre.ItemIndex := SavedIndex
  else
    cmbGenre.ItemIndex := 0;
end;

procedure TfrmRadios.UpdateStatusBar;
var
  Station: TRadioStation;
  Index: Integer;
begin
  if FRadioManager = nil then
  begin
    StatusBar.SimpleText := _T('Radios', 'NoStationsLoaded', 'No stations loaded');
    Exit;
  end;

  if lvStations.Selected <> nil then
  begin
    Index := lvStations.Selected.Index;
    Station := FRadioManager.FilteredStations[Index];
    StatusBar.SimpleText := Station.URL;
  end
  else
    StatusBar.SimpleText := Format(_T('Radios', 'StationCount', '%d stations'), [FRadioManager.FilteredCount]);
end;

procedure TfrmRadios.PlaySelected;
var
  Station: TRadioStation;
  Index: Integer;
begin
  if (FRadioManager = nil) or (lvStations.Selected = nil) then Exit;

  Index := lvStations.Selected.Index;
  if Index < 0 then Exit;

  Station := FRadioManager.FilteredStations[Index];

  if Assigned(FOnPlayRequest) then
    FOnPlayRequest(Self, Station);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  RADIO MANAGER EVENT HANDLERS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmRadios.OnRadioLoad(Sender: TObject; ACount: Integer);
begin
  UpdateGenreCombo;
  RefreshList;
  StatusBar.SimpleText := Format(_T('Radios', 'LoadedStations', 'Loaded %d stations'), [ACount]);
end;

procedure TfrmRadios.OnRadioError(Sender: TObject; const ErrorMsg: string);
begin
  StatusBar.SimpleText := _T('Radios', 'ErrorPrefix', 'Error:') + ' ' + ErrorMsg;
end;

procedure TfrmRadios.OnRadioChange(Sender: TObject);
begin
  RefreshList;
end;

end.
