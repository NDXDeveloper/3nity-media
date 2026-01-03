{ ===============================================================================
  uAbout.pas - About Dialog

  Part of 3nity Media - Lazarus Edition

  This dialog displays information about the application, credits,
  and system information.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, LCLIntf, LResources,
  uConstants, uLocale, uAppVersion;

type
  { TfrmAbout }
  TfrmAbout = class(TForm)
    pnlHeader: TPanel;
    imgLogo: TImage;
    lblAppName: TLabel;
    lblVersion: TLabel;
    lblCopyright: TLabel;

    pnlTech: TPanel;
    lblTechTitle: TLabel;
    lblFPC: TLabel;
    lblLazarus: TLabel;
    lblMPV: TLabel;

    pnlInfo: TPanel;
    pnlBuild: TPanel;
    lblBuildTitle: TLabel;
    lblBuildDate: TLabel;
    lblTarget: TLabel;
    lblCompiler: TLabel;
    pnlSystem: TPanel;
    lblSystemTitle: TLabel;
    lblPlatform: TLabel;
    lblKernel: TLabel;
    lblMPVApi: TLabel;

    pnlCredits: TPanel;
    lblCreditsTitle: TLabel;
    lblDeveloper: TLabel;
    lblDescription: TLabel;

    pnlBottom: TPanel;
    btnGitHub: TSpeedButton;
    btnLicense: TSpeedButton;
    btnClose: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnGitHubClick(Sender: TObject);
    procedure btnLicenseClick(Sender: TObject);
    procedure lblFPCClick(Sender: TObject);
    procedure lblLazarusClick(Sender: TObject);
    procedure lblMPVClick(Sender: TObject);

  private
    procedure LoadSystemInfo;
    procedure SetupClickableLabels;
    procedure LoadLogo;

  public
    procedure ApplyLocale;
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  Unix, BaseUnix,
  {$ENDIF}
  uLibMPV;

{ ===============================================================================
  FORM EVENTS
  =============================================================================== }

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  { Set application info using uAppVersion }
  lblAppName.Caption := GetAppName;
  lblVersion.Caption := _T('About', 'Version', 'Version') + ' ' + GetFullVersionInfo;
  lblCopyright.Caption := APP_COPYRIGHT_BASE + FormatDateTime('yyyy', Now) + APP_AUTHOR;

  { Set technology versions with version numbers }
  lblFPC.Caption := 'Free Pascal' + LineEnding + {$I %FPCVERSION%};
  lblLazarus.Caption := 'Lazarus IDE' + LineEnding + '3.6';
  lblMPV.Caption := 'libmpv' + LineEnding + _T('About', 'Engine', 'Engine');

  SetupClickableLabels;
  LoadLogo;
  ApplyLocale;
end;

procedure TfrmAbout.ApplyLocale;
begin
  Caption := _T('About', 'Title', 'About');

  { Section titles }
  lblTechTitle.Caption := _T('About', 'Technology', 'Technology');
  lblBuildTitle.Caption := _T('About', 'BuildInfo', 'Build Information');
  lblSystemTitle.Caption := _T('About', 'SystemInfo', 'System');
  lblCreditsTitle.Caption := _T('About', 'Credits', 'Credits');

  { Credits section }
  lblDeveloper.Caption := _T('About', 'Developer', 'Developer:') + ' Nicolas DEOUX';
  lblDescription.Caption := _T('About', 'Description', 'A modern, lightweight media player');

  { Buttons }
  btnGitHub.Caption := _T('About', 'GitHub', 'GitHub');
  btnLicense.Caption := _T('About', 'License', 'License');
  btnClose.Caption := _T('Button', 'Close', 'Close');
end;

procedure TfrmAbout.FormShow(Sender: TObject);
begin
  ApplyLocale;
  LoadSystemInfo;
end;

procedure TfrmAbout.LoadLogo;
var
  ResStream: TLazarusResourceStream;
begin
  try
    ResStream := TLazarusResourceStream.Create('logo', nil);
    try
      imgLogo.Picture.LoadFromStream(ResStream);
    finally
      ResStream.Free;
    end;
  except
    { Resource not found or error loading - ignore }
  end;
end;

procedure TfrmAbout.SetupClickableLabels;
begin
  { Make technology labels clickable with hover effect }
  lblFPC.Cursor := crHandPoint;
  lblFPC.OnClick := @lblFPCClick;

  lblLazarus.Cursor := crHandPoint;
  lblLazarus.OnClick := @lblLazarusClick;

  lblMPV.Cursor := crHandPoint;
  lblMPV.OnClick := @lblMPVClick;
end;

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.btnGitHubClick(Sender: TObject);
begin
  OpenURL(APP_GITHUB);
end;

procedure TfrmAbout.btnLicenseClick(Sender: TObject);
begin
  ShowMessage(_T('About', 'LicenseText',
              '3nity Media is licensed under GPL-2.0' + LineEnding + LineEnding +
              'This means you are free to:' + LineEnding +
              '  • Use the software for any purpose' + LineEnding +
              '  • Study and modify the source code' + LineEnding +
              '  • Distribute copies of the software' + LineEnding +
              '  • Distribute your modifications' + LineEnding + LineEnding +
              'Under the condition that any distributed modifications' + LineEnding +
              'are also licensed under GPL-2.0.'));
end;

procedure TfrmAbout.lblFPCClick(Sender: TObject);
begin
  OpenURL('https://www.freepascal.org/');
end;

procedure TfrmAbout.lblLazarusClick(Sender: TObject);
begin
  OpenURL('https://www.lazarus-ide.org/');
end;

procedure TfrmAbout.lblMPVClick(Sender: TObject);
begin
  OpenURL('https://mpv.io/');
end;

{ ===============================================================================
  PRIVATE METHODS
  =============================================================================== }

procedure TfrmAbout.LoadSystemInfo;
var
  MPVVersion: string;
  {$IFDEF UNIX}
  UN: TUtsName;
  {$ENDIF}
begin
  { Build info from uAppVersion }
  lblBuildDate.Caption := _T('About', 'BuildDate', 'Date:') + ' ' + GetBuildDateTime;
  lblTarget.Caption := _T('About', 'Target', 'Target:') + ' ' + {$I %FPCTARGETCPU%} + '-' + {$I %FPCTARGETOS%};
  lblCompiler.Caption := 'FPC: ' + {$I %FPCVERSION%} + ' | ' + GetGitCommit;

  { System info }
  {$IFDEF WINDOWS}
  lblPlatform.Caption := 'Windows';
  lblKernel.Caption := '';
  {$ENDIF}
  {$IFDEF LINUX}
  lblPlatform.Caption := 'Linux';
  if fpUname(UN) = 0 then
    lblKernel.Caption := _T('About', 'Kernel', 'Kernel:') + ' ' + UN.Release
  else
    lblKernel.Caption := '';
  {$ENDIF}
  {$IFDEF DARWIN}
  lblPlatform.Caption := 'macOS';
  lblKernel.Caption := '';
  {$ENDIF}

  { libmpv info }
  if Assigned(mpv_client_api_version) then
  begin
    MPVVersion := Format('%d.%d',
      [mpv_client_api_version() shr 16,
       mpv_client_api_version() and $FFFF]);
    lblMPVApi.Caption := 'libmpv: ' + MPVVersion;
  end
  else
    lblMPVApi.Caption := 'libmpv: N/A';
end;

initialization
  {$I logo.lrs}

end.
