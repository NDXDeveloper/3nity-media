{ ═══════════════════════════════════════════════════════════════════════════════
  uLog.pas - MPV Log/Debug Window

  Part of 3nity Media - Lazarus Edition

  This unit implements the log window with:
  - Real-time MPV log display
  - Command input with history
  - Log clearing and export

  Phase 25: Log Window & File Utilities

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, LCLType,
  uLocale;

var
  { Global file logging }
  LogFileName: string = '';
  LogFileHandle: TextFile;
  LogFileOpen: Boolean = False;

procedure InitFileLog(const FileName: string);
procedure CloseFileLog;
procedure WriteToFileLog(const Line: string);

type
  { TfrmLog }
  TfrmLog = class(TForm)
    memoLog: TMemo;
    pnlBottom: TPanel;
    lblCommand: TLabel;
    edtCommand: TEdit;
    btnClose: TButton;
    btnClear: TButton;
    btnSave: TButton;
    pmLog: TPopupMenu;
    mnuCopy: TMenuItem;
    mnuSelectAll: TMenuItem;
    mnuSeparator: TMenuItem;
    mnuClear: TMenuItem;
    mnuSave: TMenuItem;
    SaveDialog: TSaveDialog;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtCommandKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtCommandKeyPress(Sender: TObject; var Key: Char);
    procedure btnCloseClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure mnuSelectAllClick(Sender: TObject);
    procedure mnuClearClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);

  private
    FHistory: TStringList;
    FHistoryPos: Integer;
    FOnSendCommand: TNotifyEvent;
    FLastCommand: string;

    procedure ScrollToEnd;

  public
    procedure ApplyLocale;
    procedure AddLine(const Line: string);
    procedure AddLines(const Lines: TStrings);
    procedure ClearLog;
    procedure SaveLog(const FileName: string);

    property LastCommand: string read FLastCommand;
    property OnSendCommand: TNotifyEvent read FOnSendCommand write FOnSendCommand;
  end;

var
  frmLog: TfrmLog;

implementation

{$R *.lfm}

{ ═══════════════════════════════════════════════════════════════════════════════
  GLOBAL FILE LOGGING
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure InitFileLog(const FileName: string);
begin
  if LogFileOpen then
    CloseFileLog;

  LogFileName := FileName;
  try
    AssignFile(LogFileHandle, LogFileName);
    if FileExists(LogFileName) then
      Append(LogFileHandle)
    else
      Rewrite(LogFileHandle);
    LogFileOpen := True;
    WriteLn(LogFileHandle, '=== Log started at ' + DateTimeToStr(Now) + ' ===');
    Flush(LogFileHandle);
  except
    LogFileOpen := False;
  end;
end;

procedure CloseFileLog;
begin
  if LogFileOpen then
  begin
    try
      WriteLn(LogFileHandle, '=== Log ended at ' + DateTimeToStr(Now) + ' ===');
      CloseFile(LogFileHandle);
    except
    end;
    LogFileOpen := False;
  end;
end;

procedure WriteToFileLog(const Line: string);
begin
  { Always write to stderr for debugging }
  WriteLn(StdErr, Line);

  { Write to file if open }
  if LogFileOpen then
  begin
    try
      WriteLn(LogFileHandle, FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + Line);
      Flush(LogFileHandle);
    except
    end;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  FORM EVENTS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmLog.FormCreate(Sender: TObject);
begin
  FHistory := TStringList.Create;
  FHistoryPos := 0;
  FLastCommand := '';

  ApplyLocale;
end;

procedure TfrmLog.FormDestroy(Sender: TObject);
begin
  FHistory.Free;
end;

procedure TfrmLog.FormShow(Sender: TObject);
begin
  { Apply current translations (in case language changed) }
  ApplyLocale;

  ScrollToEnd;
  edtCommand.SetFocus;
end;

procedure TfrmLog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TfrmLog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Close;
    Key := 0;
  end;
end;

procedure TfrmLog.edtCommandKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
      begin
        if (FHistoryPos > 0) then
        begin
          Dec(FHistoryPos);
          edtCommand.Text := FHistory[FHistoryPos];
          edtCommand.SelStart := Length(edtCommand.Text);
        end;
        Key := 0;
      end;

    VK_DOWN:
      begin
        if (FHistoryPos < FHistory.Count - 1) then
        begin
          Inc(FHistoryPos);
          edtCommand.Text := FHistory[FHistoryPos];
          edtCommand.SelStart := Length(edtCommand.Text);
        end
        else if FHistoryPos = FHistory.Count - 1 then
        begin
          Inc(FHistoryPos);
          edtCommand.Text := '';
        end;
        Key := 0;
      end;
  end;
end;

procedure TfrmLog.edtCommandKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then  { Enter }
  begin
    if Trim(edtCommand.Text) <> '' then
    begin
      { Add to log }
      AddLine('> ' + edtCommand.Text);

      { Store command }
      FLastCommand := edtCommand.Text;

      { Add to history }
      FHistory.Add(edtCommand.Text);
      FHistoryPos := FHistory.Count;

      { Trigger event }
      if Assigned(FOnSendCommand) then
        FOnSendCommand(Self);

      { Clear input }
      edtCommand.Text := '';
    end;
    Key := #0;
  end;
end;

procedure TfrmLog.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmLog.btnClearClick(Sender: TObject);
begin
  ClearLog;
end;

procedure TfrmLog.btnSaveClick(Sender: TObject);
begin
  SaveDialog.FileName := 'mpv_log_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.txt';
  if SaveDialog.Execute then
    SaveLog(SaveDialog.FileName);
end;

procedure TfrmLog.mnuCopyClick(Sender: TObject);
begin
  memoLog.CopyToClipboard;
end;

procedure TfrmLog.mnuSelectAllClick(Sender: TObject);
begin
  memoLog.SelectAll;
end;

procedure TfrmLog.mnuClearClick(Sender: TObject);
begin
  ClearLog;
end;

procedure TfrmLog.mnuSaveClick(Sender: TObject);
begin
  btnSaveClick(Sender);
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PRIVATE METHODS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmLog.ApplyLocale;

  function _T(const Section, Key, Default: string): string;
  begin
    Result := Locale.GetString(Section, Key, Default);
  end;

begin
  Caption := _T('Log', 'Title', 'MPV Log');
  lblCommand.Caption := _T('Log', 'Command', 'Command:');
  btnClose.Caption := _T('Button', 'Close', 'Close');
  btnClear.Caption := _T('Log', 'Clear', 'Clear');
  btnSave.Caption := _T('Log', 'Save', 'Save...');

  mnuCopy.Caption := _T('Log', 'Copy', '&Copy');
  mnuSelectAll.Caption := _T('Log', 'SelectAll', 'Select &All');
  mnuClear.Caption := _T('Log', 'Clear', '&Clear');
  mnuSave.Caption := _T('Log', 'Save', '&Save...');

  SaveDialog.Title := _T('Log', 'SaveTitle', 'Save Log');
  SaveDialog.Filter := _T('Log', 'SaveFilter', 'Text files (*.txt)|*.txt|All files (*.*)|*.*');
end;

procedure TfrmLog.ScrollToEnd;
begin
  if memoLog.Lines.Count > 0 then
  begin
    memoLog.SelStart := Length(memoLog.Text);
    memoLog.SelLength := 0;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════════
  PUBLIC METHODS
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TfrmLog.AddLine(const Line: string);
begin
  { Write to file/stderr first (always works even if UI is frozen) }
  WriteToFileLog(Line);

  memoLog.Lines.Add(Line);

  { Auto-scroll if visible }
  if Visible then
    ScrollToEnd;

  { Limit log size to prevent memory issues }
  while memoLog.Lines.Count > 10000 do
    memoLog.Lines.Delete(0);
end;

procedure TfrmLog.AddLines(const Lines: TStrings);
var
  I: Integer;
begin
  memoLog.Lines.BeginUpdate;
  try
    for I := 0 to Lines.Count - 1 do
      memoLog.Lines.Add(Lines[I]);
  finally
    memoLog.Lines.EndUpdate;
  end;

  if Visible then
    ScrollToEnd;
end;

procedure TfrmLog.ClearLog;
begin
  memoLog.Clear;
end;

procedure TfrmLog.SaveLog(const FileName: string);
begin
  try
    memoLog.Lines.SaveToFile(FileName);
  except
    on E: Exception do
      MessageDlg(_T('Log', 'ErrorTitle', 'Error'),
                 Format(_T('Log', 'SaveError', 'Failed to save log: %s'), [E.Message]), mtError, [mbOK], 0);
  end;
end;

initialization
  { Auto-initialize file logging to temp directory }
  InitFileLog(GetTempDir + 'trinity3player_debug.log');

finalization
  CloseFileLog;

end.
