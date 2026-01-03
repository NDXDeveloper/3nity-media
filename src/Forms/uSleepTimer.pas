{ ═══════════════════════════════════════════════════════════════════════════════
  uSleepTimer.pas - Sleep Timer Dialog

  Part of 3nity Media - Lazarus Edition

  This dialog allows users to set a timer to stop playback, close the
  application, or shutdown the computer after a specified time.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uSleepTimer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, Buttons,
  uLocale;

type
  TSleepTimerAction = (
    staStopPlayback,    { Stop playback when timer expires }
    staCloseApp,        { Close the application }
    staShutdown         { Shutdown the computer }
  );

  { TfrmSleepTimer }
  TfrmSleepTimer = class(TForm)
    lblTitle: TLabel;
    lblHours: TLabel;
    lblMinutes: TLabel;
    seHours: TSpinEdit;
    seMinutes: TSpinEdit;
    gbAction: TGroupBox;
    rbStopPlayback: TRadioButton;
    rbCloseApp: TRadioButton;
    rbShutdown: TRadioButton;
    pnlQuickSet: TPanel;
    btn15Min: TButton;
    btn30Min: TButton;
    btn1Hour: TButton;
    btn2Hours: TButton;
    btnOK: TButton;
    btnCancel: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btn15MinClick(Sender: TObject);
    procedure btn30MinClick(Sender: TObject);
    procedure btn1HourClick(Sender: TObject);
    procedure btn2HoursClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure seHoursChange(Sender: TObject);
    procedure seMinutesChange(Sender: TObject);

  private
    FTimerMinutes: Integer;
    FTimerAction: TSleepTimerAction;
    procedure ApplyLocale;
    procedure UpdateTimerMinutes;

  public
    property TimerMinutes: Integer read FTimerMinutes;
    property TimerAction: TSleepTimerAction read FTimerAction;
  end;

var
  frmSleepTimer: TfrmSleepTimer;

implementation

{$R *.lfm}

{ TfrmSleepTimer }

procedure TfrmSleepTimer.FormCreate(Sender: TObject);
begin
  FTimerMinutes := 30;
  FTimerAction := staStopPlayback;

  seHours.Value := 0;
  seMinutes.Value := 30;
  rbStopPlayback.Checked := True;

  ApplyLocale;
end;

procedure TfrmSleepTimer.ApplyLocale;
begin
  Caption := _T('SleepTimer', 'Title', 'Sleep Timer');
  lblTitle.Caption := _T('SleepTimer', 'Description', 'Set a timer to automatically stop playback');
  lblHours.Caption := _T('SleepTimer', 'Hours', 'Hours:');
  lblMinutes.Caption := _T('SleepTimer', 'Minutes', 'Minutes:');

  gbAction.Caption := _T('SleepTimer', 'Action', 'When timer expires:');
  rbStopPlayback.Caption := _T('SleepTimer', 'StopPlayback', 'Stop playback');
  rbCloseApp.Caption := _T('SleepTimer', 'CloseApp', 'Close application');
  rbShutdown.Caption := _T('SleepTimer', 'Shutdown', 'Shutdown computer');

  pnlQuickSet.Caption := _T('SleepTimer', 'QuickSet', 'Quick set:');
  btn15Min.Caption := _T('SleepTimer', '15Min', '15 min');
  btn30Min.Caption := _T('SleepTimer', '30Min', '30 min');
  btn1Hour.Caption := _T('SleepTimer', '1Hour', '1 hour');
  btn2Hours.Caption := _T('SleepTimer', '2Hours', '2 hours');

  btnOK.Caption := _T('Button', 'OK', 'OK');
  btnCancel.Caption := _T('Button', 'Cancel', 'Cancel');
end;

procedure TfrmSleepTimer.btn15MinClick(Sender: TObject);
begin
  seHours.Value := 0;
  seMinutes.Value := 15;
end;

procedure TfrmSleepTimer.btn30MinClick(Sender: TObject);
begin
  seHours.Value := 0;
  seMinutes.Value := 30;
end;

procedure TfrmSleepTimer.btn1HourClick(Sender: TObject);
begin
  seHours.Value := 1;
  seMinutes.Value := 0;
end;

procedure TfrmSleepTimer.btn2HoursClick(Sender: TObject);
begin
  seHours.Value := 2;
  seMinutes.Value := 0;
end;

procedure TfrmSleepTimer.seHoursChange(Sender: TObject);
begin
  UpdateTimerMinutes;
end;

procedure TfrmSleepTimer.seMinutesChange(Sender: TObject);
begin
  UpdateTimerMinutes;
end;

procedure TfrmSleepTimer.UpdateTimerMinutes;
begin
  FTimerMinutes := seHours.Value * 60 + seMinutes.Value;
  btnOK.Enabled := FTimerMinutes > 0;
end;

procedure TfrmSleepTimer.btnOKClick(Sender: TObject);
begin
  UpdateTimerMinutes;

  if rbStopPlayback.Checked then
    FTimerAction := staStopPlayback
  else if rbCloseApp.Checked then
    FTimerAction := staCloseApp
  else if rbShutdown.Checked then
    FTimerAction := staShutdown;

  ModalResult := mrOK;
end;

procedure TfrmSleepTimer.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
