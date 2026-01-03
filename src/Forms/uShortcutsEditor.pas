{ ═══════════════════════════════════════════════════════════════════════════════
  uShortcutsEditor.pas - Keyboard Shortcuts Editor Dialog

  Part of 3nity Media - Lazarus Edition

  This dialog allows users to customize keyboard shortcuts for all actions.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uShortcutsEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons, LCLType, uShortcuts;

type
  { TfrmShortcutsEditor }
  TfrmShortcutsEditor = class(TForm)
    lblTitle: TLabel;
    lblSearch: TLabel;
    edtSearch: TEdit;
    lvShortcuts: TListView;
    pnlEdit: TPanel;
    lblAction: TLabel;
    lblCurrentShortcut: TLabel;
    edtShortcut: TEdit;
    btnAssign: TButton;
    btnClear: TButton;
    btnReset: TButton;
    pnlButtons: TPanel;
    btnResetAll: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    lblConflict: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtSearchChange(Sender: TObject);
    procedure lvShortcutsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure edtShortcutKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnAssignClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnResetAllClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);

  private
    FModified: Boolean;
    FCapturedKey: Word;
    FCapturedShift: TShiftState;
    procedure ApplyLocale;
    procedure PopulateList;
    procedure UpdateEditPanel;
    procedure CheckConflict;
    function GetSelectedAction: TShortcutAction;
    function GetActionDisplayName(AAction: TShortcutAction): string;

  public
    property Modified: Boolean read FModified;
  end;

var
  frmShortcutsEditor: TfrmShortcutsEditor;

implementation

{$R *.lfm}

uses
  uLocale;

{ TfrmShortcutsEditor }

procedure TfrmShortcutsEditor.FormCreate(Sender: TObject);
begin
  FModified := False;
  FCapturedKey := 0;
  FCapturedShift := [];

  { Set up list view columns }
  lvShortcuts.Columns.Clear;
  with lvShortcuts.Columns.Add do
    Width := 100;
  with lvShortcuts.Columns.Add do
    Width := 180;
  with lvShortcuts.Columns.Add do
    Width := 150;

  ApplyLocale;
  PopulateList;
  UpdateEditPanel;
end;

procedure TfrmShortcutsEditor.FormShow(Sender: TObject);
begin
  { Apply current translations (in case language changed) }
  ApplyLocale;
  PopulateList;
end;

procedure TfrmShortcutsEditor.ApplyLocale;
begin
  { Update column headers }
  if lvShortcuts.Columns.Count >= 3 then
  begin
    lvShortcuts.Columns[0].Caption := Locale.GetString('Shortcuts', 'ColCategory', 'Category');
    lvShortcuts.Columns[1].Caption := Locale.GetString('Shortcuts', 'ColAction', 'Action');
    lvShortcuts.Columns[2].Caption := Locale.GetString('Shortcuts', 'ColShortcut', 'Shortcut');
  end;

  { Update button captions }
  btnAssign.Caption := Locale.GetString('Shortcuts', 'Assign', 'Assign');
  btnClear.Caption := Locale.GetString('Shortcuts', 'Clear', 'Clear');
  btnReset.Caption := Locale.GetString('Shortcuts', 'Reset', 'Reset');
  btnResetAll.Caption := Locale.GetString('Shortcuts', 'ResetAll', 'Reset All');
  btnOK.Caption := Locale.GetString('Button', 'OK', 'OK');
  btnCancel.Caption := Locale.GetString('Button', 'Cancel', 'Cancel');

  { Update labels }
  Caption := Locale.GetString('Shortcuts', 'Title', 'Keyboard Shortcuts');
  lblTitle.Caption := Locale.GetString('Shortcuts', 'Description', 'Configure keyboard shortcuts for actions');
  lblSearch.Caption := Locale.GetString('Shortcuts', 'Search', 'Search:');
  edtShortcut.TextHint := Locale.GetString('Shortcuts', 'PressKey', 'Press a key...');
end;

procedure TfrmShortcutsEditor.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    if edtShortcut.Focused then
    begin
      lvShortcuts.SetFocus;
      Key := 0;
    end;
  end;
end;

procedure TfrmShortcutsEditor.edtSearchChange(Sender: TObject);
begin
  PopulateList;
end;

procedure TfrmShortcutsEditor.PopulateList;
var
  A: TShortcutAction;
  Item: TListItem;
  SearchText: string;
  ActionName, CategoryName: string;
begin
  lvShortcuts.Items.BeginUpdate;
  try
    lvShortcuts.Items.Clear;
    SearchText := LowerCase(Trim(edtSearch.Text));

    for A := Low(TShortcutAction) to High(TShortcutAction) do
    begin
      ActionName := GetActionDisplayName(A);
      CategoryName := ShortcutManager.GetActionCategory(A);

      { Filter by search text }
      if (SearchText <> '') and
         (Pos(SearchText, LowerCase(ActionName)) = 0) and
         (Pos(SearchText, LowerCase(CategoryName)) = 0) and
         (Pos(SearchText, LowerCase(ShortcutManager.ShortcutToText(A))) = 0) then
        Continue;

      Item := lvShortcuts.Items.Add;
      Item.Caption := CategoryName;
      Item.SubItems.Add(ActionName);
      Item.SubItems.Add(ShortcutManager.ShortcutToText(A));
      Item.Data := Pointer(PtrUInt(Ord(A)));
    end;
  finally
    lvShortcuts.Items.EndUpdate;
  end;

  if lvShortcuts.Items.Count > 0 then
    lvShortcuts.Items[0].Selected := True;
end;

procedure TfrmShortcutsEditor.lvShortcutsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected then
    UpdateEditPanel;
end;

procedure TfrmShortcutsEditor.UpdateEditPanel;
var
  SelAction: TShortcutAction;
  HasSelection: Boolean;
begin
  HasSelection := (lvShortcuts.Selected <> nil);

  lblAction.Enabled := HasSelection;
  lblCurrentShortcut.Enabled := HasSelection;
  edtShortcut.Enabled := HasSelection;
  btnAssign.Enabled := HasSelection;
  btnClear.Enabled := HasSelection;
  btnReset.Enabled := HasSelection;

  if HasSelection then
  begin
    SelAction := GetSelectedAction;
    lblAction.Caption := GetActionDisplayName(SelAction);
    lblCurrentShortcut.Caption := Format(Locale.GetString('Shortcuts', 'CurrentShortcut', 'Current: %s'),
      [ShortcutManager.ShortcutToText(SelAction)]);
    edtShortcut.Text := '';
    FCapturedKey := 0;
    FCapturedShift := [];
    lblConflict.Caption := '';
    lblConflict.Visible := False;
  end
  else
  begin
    lblAction.Caption := '';
    lblCurrentShortcut.Caption := '';
    edtShortcut.Text := '';
  end;
end;

procedure TfrmShortcutsEditor.edtShortcutKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { Capture the key combination }
  if Key in [VK_SHIFT, VK_CONTROL, VK_MENU, VK_LSHIFT, VK_RSHIFT,
             VK_LCONTROL, VK_RCONTROL, VK_LMENU, VK_RMENU] then
    Exit;

  { Don't capture Tab as it's used for navigation }
  if Key = VK_TAB then Exit;

  FCapturedKey := Key;
  FCapturedShift := Shift * [ssCtrl, ssAlt, ssShift]; { Only keep modifier keys }

  edtShortcut.Text := KeyToText(FCapturedKey, FCapturedShift);
  CheckConflict;

  Key := 0; { Consume the key }
end;

procedure TfrmShortcutsEditor.CheckConflict;
var
  SelAction: TShortcutAction;
  ConflictAction: TShortcutAction;
begin
  if lvShortcuts.Selected = nil then Exit;
  if FCapturedKey = 0 then
  begin
    lblConflict.Visible := False;
    Exit;
  end;

  SelAction := GetSelectedAction;

  if ShortcutManager.ActionExists(FCapturedKey, FCapturedShift, SelAction) then
  begin
    { Find the conflicting action }
    ConflictAction := ShortcutManager.FindAction(FCapturedKey, FCapturedShift);
    lblConflict.Caption := Format(Locale.GetString('Shortcuts', 'Conflict',
      'Conflict: Already assigned to "%s"'), [GetActionDisplayName(ConflictAction)]);
    lblConflict.Visible := True;
    lblConflict.Font.Color := clRed;
  end
  else
  begin
    lblConflict.Visible := False;
  end;
end;

procedure TfrmShortcutsEditor.btnAssignClick(Sender: TObject);
var
  SelAction: TShortcutAction;
  ConflictAction: TShortcutAction;
  I: Integer;
begin
  if lvShortcuts.Selected = nil then Exit;
  if FCapturedKey = 0 then
  begin
    ShowMessage(Locale.GetString('Shortcuts', 'NoKeyPressed', 'Please press a key combination first.'));
    Exit;
  end;

  SelAction := GetSelectedAction;

  { Check for conflict }
  if ShortcutManager.ActionExists(FCapturedKey, FCapturedShift, SelAction) then
  begin
    ConflictAction := ShortcutManager.FindAction(FCapturedKey, FCapturedShift);
    if MessageDlg(Locale.GetString('Shortcuts', 'ConfirmReplace', 'Replace Shortcut'),
      Format(Locale.GetString('Shortcuts', 'ReplaceMessage',
        'This shortcut is already assigned to "%s". Do you want to replace it?'),
        [GetActionDisplayName(ConflictAction)]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      { Clear the conflicting shortcut }
      ShortcutManager.ClearShortcut(ConflictAction);
    end
    else
      Exit;
  end;

  { Assign the new shortcut }
  ShortcutManager.AssignShortcut(SelAction, FCapturedKey, FCapturedShift);
  FModified := True;

  { Update the list }
  PopulateList;

  { Re-select the item }
  for I := 0 to lvShortcuts.Items.Count - 1 do
  begin
    if TShortcutAction(PtrUInt(lvShortcuts.Items[I].Data)) = SelAction then
    begin
      lvShortcuts.Items[I].Selected := True;
      lvShortcuts.Items[I].MakeVisible(False);
      Break;
    end;
  end;

  UpdateEditPanel;
end;

procedure TfrmShortcutsEditor.btnClearClick(Sender: TObject);
var
  SelAction: TShortcutAction;
begin
  if lvShortcuts.Selected = nil then Exit;

  SelAction := GetSelectedAction;
  ShortcutManager.ClearShortcut(SelAction);
  FModified := True;

  { Update display }
  lvShortcuts.Selected.SubItems[1] := ShortcutManager.ShortcutToText(SelAction);
  UpdateEditPanel;
end;

procedure TfrmShortcutsEditor.btnResetClick(Sender: TObject);
var
  SelAction: TShortcutAction;
begin
  if lvShortcuts.Selected = nil then Exit;

  SelAction := GetSelectedAction;
  ShortcutManager.ResetAction(SelAction);
  FModified := True;

  { Update display }
  lvShortcuts.Selected.SubItems[1] := ShortcutManager.ShortcutToText(SelAction);
  UpdateEditPanel;
end;

procedure TfrmShortcutsEditor.btnResetAllClick(Sender: TObject);
begin
  if MessageDlg(Locale.GetString('Shortcuts', 'ConfirmResetAll', 'Reset All Shortcuts'),
    Locale.GetString('Shortcuts', 'ResetAllMessage',
      'Are you sure you want to reset all shortcuts to their defaults?'),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ShortcutManager.ResetToDefaults;
    FModified := True;
    PopulateList;
    UpdateEditPanel;
  end;
end;

procedure TfrmShortcutsEditor.btnOKClick(Sender: TObject);
begin
  if FModified then
    ShortcutManager.Save;
  ModalResult := mrOK;
end;

procedure TfrmShortcutsEditor.btnCancelClick(Sender: TObject);
begin
  if FModified then
  begin
    { Reload to discard changes }
    ShortcutManager.Load;
  end;
  ModalResult := mrCancel;
end;

function TfrmShortcutsEditor.GetSelectedAction: TShortcutAction;
begin
  if lvShortcuts.Selected <> nil then
    Result := TShortcutAction(PtrUInt(lvShortcuts.Selected.Data))
  else
    Result := saPlayPause;
end;

function TfrmShortcutsEditor.GetActionDisplayName(AAction: TShortcutAction): string;
begin
  case AAction of
    saPlayPause: Result := Locale.GetString('Shortcuts', 'ActPlayPause', 'Play / Pause');
    saStop: Result := Locale.GetString('Shortcuts', 'ActStop', 'Stop');
    saPrevious: Result := Locale.GetString('Shortcuts', 'ActPrevious', 'Previous');
    saNext: Result := Locale.GetString('Shortcuts', 'ActNext', 'Next');
    saSeekForward: Result := Locale.GetString('Shortcuts', 'ActSeekForward', 'Seek Forward (10s)');
    saSeekBackward: Result := Locale.GetString('Shortcuts', 'ActSeekBackward', 'Seek Backward (10s)');
    saSeekForward5: Result := Locale.GetString('Shortcuts', 'ActSeekForward5', 'Seek Forward (1 min)');
    saSeekBackward5: Result := Locale.GetString('Shortcuts', 'ActSeekBackward5', 'Seek Backward (1 min)');
    saSpeedUp: Result := Locale.GetString('Shortcuts', 'ActSpeedUp', 'Speed Up');
    saSpeedDown: Result := Locale.GetString('Shortcuts', 'ActSpeedDown', 'Speed Down');
    saSpeedReset: Result := Locale.GetString('Shortcuts', 'ActSpeedReset', 'Reset Speed');
    saSetLoopA: Result := Locale.GetString('Shortcuts', 'ActSetLoopA', 'Set Loop Point A');
    saSetLoopB: Result := Locale.GetString('Shortcuts', 'ActSetLoopB', 'Set Loop Point B');
    saClearLoop: Result := Locale.GetString('Shortcuts', 'ActClearLoop', 'Clear A-B Loop');
    saFrameForward: Result := Locale.GetString('Shortcuts', 'ActFrameForward', 'Frame Forward');
    saFrameBackward: Result := Locale.GetString('Shortcuts', 'ActFrameBackward', 'Frame Backward');
    saPrevChapter: Result := Locale.GetString('Shortcuts', 'ActPrevChapter', 'Previous Chapter');
    saNextChapter: Result := Locale.GetString('Shortcuts', 'ActNextChapter', 'Next Chapter');
    saGotoTime: Result := Locale.GetString('Shortcuts', 'ActGotoTime', 'Go to Time');
    saVolumeUp: Result := Locale.GetString('Shortcuts', 'ActVolumeUp', 'Volume Up');
    saVolumeDown: Result := Locale.GetString('Shortcuts', 'ActVolumeDown', 'Volume Down');
    saMute: Result := Locale.GetString('Shortcuts', 'ActMute', 'Mute / Unmute');
    saAudioDelayPlus: Result := Locale.GetString('Shortcuts', 'ActAudioDelayPlus', 'Audio Delay +');
    saAudioDelayMinus: Result := Locale.GetString('Shortcuts', 'ActAudioDelayMinus', 'Audio Delay -');
    saAudioDelayReset: Result := Locale.GetString('Shortcuts', 'ActAudioDelayReset', 'Reset Audio Delay');
    saFullscreen: Result := Locale.GetString('Shortcuts', 'ActFullscreen', 'Fullscreen');
    saRotate: Result := Locale.GetString('Shortcuts', 'ActRotate', 'Rotate Video');
    saDeinterlace: Result := Locale.GetString('Shortcuts', 'ActDeinterlace', 'Deinterlace');
    saZoomIn: Result := Locale.GetString('Shortcuts', 'ActZoomIn', 'Zoom In');
    saZoomOut: Result := Locale.GetString('Shortcuts', 'ActZoomOut', 'Zoom Out');
    saZoomReset: Result := Locale.GetString('Shortcuts', 'ActZoomReset', 'Reset Zoom');
    saAlwaysOnTop: Result := Locale.GetString('Shortcuts', 'ActAlwaysOnTop', 'Always on Top');
    saFitToVideo: Result := Locale.GetString('Shortcuts', 'ActFitToVideo', 'Fit to Video');
    saScreenshot: Result := Locale.GetString('Shortcuts', 'ActScreenshot', 'Screenshot');
    saVisNextMode: Result := Locale.GetString('Shortcuts', 'ActVisNextMode', 'Next Visualization Mode');
    saVisNextColor: Result := Locale.GetString('Shortcuts', 'ActVisNextColor', 'Next Color Scheme');
    saSubtitleDelayPlus: Result := Locale.GetString('Shortcuts', 'ActSubDelayPlus', 'Subtitle Delay +');
    saSubtitleDelayMinus: Result := Locale.GetString('Shortcuts', 'ActSubDelayMinus', 'Subtitle Delay -');
    saSubtitleDelayReset: Result := Locale.GetString('Shortcuts', 'ActSubDelayReset', 'Reset Subtitle Delay');
    saDVDMenu: Result := Locale.GetString('Shortcuts', 'ActDVDMenu', 'DVD Menu');
    saDVDUp: Result := Locale.GetString('Shortcuts', 'ActDVDUp', 'DVD Navigate Up');
    saDVDDown: Result := Locale.GetString('Shortcuts', 'ActDVDDown', 'DVD Navigate Down');
    saDVDLeft: Result := Locale.GetString('Shortcuts', 'ActDVDLeft', 'DVD Navigate Left');
    saDVDRight: Result := Locale.GetString('Shortcuts', 'ActDVDRight', 'DVD Navigate Right');
    saDVDSelect: Result := Locale.GetString('Shortcuts', 'ActDVDSelect', 'DVD Select');
    saOpenFile: Result := Locale.GetString('Shortcuts', 'ActOpenFile', 'Open File');
    saOpenURL: Result := Locale.GetString('Shortcuts', 'ActOpenURL', 'Open URL');
    saPlaylist: Result := Locale.GetString('Shortcuts', 'ActPlaylist', 'Show Playlist');
    saQuit: Result := Locale.GetString('Shortcuts', 'ActQuit', 'Quit');
  else
    Result := ShortcutManager.GetActionName(AAction);
  end;
end;

end.
