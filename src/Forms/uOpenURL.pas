{ ═══════════════════════════════════════════════════════════════════════════════
  uOpenURL.pas - Open URL Dialog

  Part of 3nity Media - Lazarus Edition

  This dialog allows the user to enter a media URL to play.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uOpenURL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, uLocale;

type
  { TfrmOpenURL }
  TfrmOpenURL = class(TForm)
    lblPrompt: TLabel;
    edtURL: TEdit;
    pnlButtons: TPanel;
    btnOK: TButton;
    btnCancel: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtURLChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);

  private
    procedure ApplyLocale;

  public
    function GetURL: string;
  end;

var
  frmOpenURL: TfrmOpenURL;

{ Helper function to show the dialog and get URL }
function ShowOpenURLDialog: string;

implementation

{$R *.lfm}

function ShowOpenURLDialog: string;
var
  Dlg: TfrmOpenURL;
begin
  Result := '';
  Dlg := TfrmOpenURL.Create(Application);
  try
    if Dlg.ShowModal = mrOK then
      Result := Dlg.GetURL;
  finally
    Dlg.Free;
  end;
end;

{ TfrmOpenURL }

procedure TfrmOpenURL.FormCreate(Sender: TObject);
begin
  ApplyLocale;
end;

procedure TfrmOpenURL.FormShow(Sender: TObject);
begin
  ApplyLocale;
  edtURL.Text := '';
  edtURL.SetFocus;
  btnOK.Enabled := False;
end;

procedure TfrmOpenURL.ApplyLocale;
begin
  Caption := _T('OpenURL', 'Title', 'Open URL');
  lblPrompt.Caption := _T('OpenURL', 'Prompt', 'Enter media URL:');
  btnOK.Caption := _T('Button', 'OK', 'OK');
  btnCancel.Caption := _T('Button', 'Cancel', 'Cancel');
end;

procedure TfrmOpenURL.edtURLChange(Sender: TObject);
begin
  btnOK.Enabled := Trim(edtURL.Text) <> '';
end;

procedure TfrmOpenURL.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmOpenURL.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TfrmOpenURL.GetURL: string;
begin
  Result := Trim(edtURL.Text);
end;

end.
