{ ═══════════════════════════════════════════════════════════════════════════════
  uGotoTime.pas - Go to Time Dialog

  Part of 3nity Media - Lazarus Edition

  This dialog allows the user to jump to a specific time position in the
  current media file.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uGotoTime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, Buttons,
  uLocale;

type
  { TfrmGotoTime }
  TfrmGotoTime = class(TForm)
    lblTitle: TLabel;
    pnlTime: TPanel;
    lblHours: TLabel;
    seHours: TSpinEdit;
    lblColon1: TLabel;
    lblMinutes: TLabel;
    seMinutes: TSpinEdit;
    lblColon2: TLabel;
    lblSeconds: TLabel;
    seSeconds: TSpinEdit;
    lblCurrentTime: TLabel;
    lblDuration: TLabel;
    pnlButtons: TPanel;
    btnOK: TButton;
    btnCancel: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure seHoursChange(Sender: TObject);
    procedure seMinutesChange(Sender: TObject);
    procedure seSecondsChange(Sender: TObject);

  private
    FCurrentPosition: Double;
    FDuration: Double;
    FTargetPosition: Double;
    procedure ApplyLocale;
    procedure UpdateTimeDisplay;
    procedure ValidateTime;

  public
    property CurrentPosition: Double read FCurrentPosition write FCurrentPosition;
    property Duration: Double read FDuration write FDuration;
    property TargetPosition: Double read FTargetPosition;
  end;

var
  frmGotoTime: TfrmGotoTime;

implementation

{$R *.lfm}

{ TfrmGotoTime }

procedure TfrmGotoTime.FormCreate(Sender: TObject);
begin
  FCurrentPosition := 0;
  FDuration := 0;
  FTargetPosition := 0;
  ApplyLocale;
end;

procedure TfrmGotoTime.ApplyLocale;
begin
  Caption := _T('GotoTime', 'Title', 'Go to Time');
  lblTitle.Caption := _T('GotoTime', 'EnterTime', 'Enter time to jump to:');
  lblHours.Caption := _T('GotoTime', 'Hours', 'H');
  lblMinutes.Caption := _T('GotoTime', 'Minutes', 'M');
  lblSeconds.Caption := _T('GotoTime', 'Seconds', 'S');
  btnOK.Caption := _T('Button', 'OK', 'OK');
  btnCancel.Caption := _T('Button', 'Cancel', 'Cancel');
end;

procedure TfrmGotoTime.FormShow(Sender: TObject);
var
  H, M, S: Integer;
begin
  { Set current position in spin edits }
  H := Trunc(FCurrentPosition / 3600);
  M := Trunc((FCurrentPosition - H * 3600) / 60);
  S := Trunc(FCurrentPosition - H * 3600 - M * 60);

  seHours.Value := H;
  seMinutes.Value := M;
  seSeconds.Value := S;

  { Set max values based on duration }
  if FDuration > 0 then
  begin
    seHours.MaxValue := Trunc(FDuration / 3600) + 1;
  end
  else
  begin
    seHours.MaxValue := 99;
  end;

  UpdateTimeDisplay;
  seHours.SetFocus;
end;

procedure TfrmGotoTime.btnOKClick(Sender: TObject);
begin
  ValidateTime;
  ModalResult := mrOK;
end;

procedure TfrmGotoTime.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmGotoTime.seHoursChange(Sender: TObject);
begin
  ValidateTime;
end;

procedure TfrmGotoTime.seMinutesChange(Sender: TObject);
begin
  ValidateTime;
end;

procedure TfrmGotoTime.seSecondsChange(Sender: TObject);
begin
  ValidateTime;
end;

procedure TfrmGotoTime.UpdateTimeDisplay;
begin
  lblCurrentTime.Caption := Format(_T('GotoTime', 'Current', 'Current:') + ' %02d:%02d:%02d',
    [Trunc(FCurrentPosition / 3600),
     Trunc((FCurrentPosition - Trunc(FCurrentPosition / 3600) * 3600) / 60),
     Trunc(FCurrentPosition) mod 60]);

  if FDuration > 0 then
    lblDuration.Caption := Format(_T('GotoTime', 'Duration', 'Duration:') + ' %02d:%02d:%02d',
      [Trunc(FDuration / 3600),
       Trunc((FDuration - Trunc(FDuration / 3600) * 3600) / 60),
       Trunc(FDuration) mod 60])
  else
    lblDuration.Caption := _T('GotoTime', 'Duration', 'Duration:') + ' --:--:--';
end;

procedure TfrmGotoTime.ValidateTime;
begin
  FTargetPosition := seHours.Value * 3600 + seMinutes.Value * 60 + seSeconds.Value;

  { Clamp to duration if available }
  if (FDuration > 0) and (FTargetPosition > FDuration) then
    FTargetPosition := FDuration;

  { Enable OK button only if target is valid }
  btnOK.Enabled := (FTargetPosition >= 0) and
                   ((FDuration = 0) or (FTargetPosition <= FDuration));
end;

end.
