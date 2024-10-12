unit maOptionsFormUnit;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TmaOptionsForm }

  TmaOptionsForm = class(TForm)
    bbtOk: TBitBtn;
    bbtCancel: TBitBtn;
    cbxShowPassword: TCheckBox;
    edIMAP4Hostname: TEdit;
    edUsername: TEdit;
    edSMTPHostname: TEdit;
    edPassword: TEdit;
    lblPassword: TLabel;
    lblUsername: TLabel;
    lblSMTPHostname: TLabel;
    lblIMAPHostname: TLabel;
    procedure cbxShowPasswordChange(Sender: TObject);
  private

  public
    procedure ShowOptions(var AIMAP, ASMTP, AUser, APass: String);
  end;

var
  maOptionsForm: TmaOptionsForm;

implementation

{$R *.lfm}

{ TmaOptionsForm }

procedure TmaOptionsForm.cbxShowPasswordChange(Sender: TObject);
begin
  if not cbxShowPassword.Checked then edPassword.PasswordChar := '*'
                                 else edPassword.PasswordChar := #0;
end;

procedure TmaOptionsForm.ShowOptions(var AIMAP, ASMTP, AUser, APass: String);
begin
  edIMAP4Hostname.Text    := AIMAP;
  edSMTPHostname.Text     := ASMTP;
  edUsername.Text         := AUser;
  edPassword.Text         := APass;
  edPassword.PasswordChar := '*';
  cbxShowPassword.Checked := False;
  if ShowModal = mrOK then begin
    AIMAP                 := edIMAP4Hostname.Text;
    ASMTP                 := edSMTPHostname.Text;
    AUser                 := edUsername.Text;
    APass                 := edPassword.Text;
  end;
end;

end.

