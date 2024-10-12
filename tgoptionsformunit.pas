unit tgOptionsFormUnit;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TtgOptionsForm }

  TtgOptionsForm = class(TForm)
    bbtOk: TBitBtn;
    bbtCancel: TBitBtn;
    cbxShowToken: TCheckBox;
    edToken: TEdit;
    lblTgBotToken: TLabel;
    procedure cbxShowTokenChange(Sender: TObject);
  private

  public
    procedure ShowOptions(var AToken: String);
  end;

var
  tgOptionsForm: TtgOptionsForm;

implementation

{$R *.lfm}

{ TtgOptionsForm }

procedure TtgOptionsForm.cbxShowTokenChange(Sender: TObject);
begin
  if not cbxShowToken.Checked then edToken.PasswordChar := '*'
                              else edToken.PasswordChar := #0;
end;

procedure TtgOptionsForm.ShowOptions(var AToken: String);
begin
  edToken.Text         := AToken;
  edToken.PasswordChar := '*';
  cbxShowToken.Checked := False;
  if ShowModal = mrOk then
    AToken             := edToken.Text;
end;

end.

