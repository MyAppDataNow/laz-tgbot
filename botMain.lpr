program botMain;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainFormUnit, TelegramBotThreadUnit, MailAgentThreadUnit, 
maOptionsFormUnit, maFilterFormUnit,
  tgOptionsFormUnit, tgFilterFormUnit, AboutFormUnit, tgFilterFormTypes
  { you can add units after this };

{$R *.res}

begin
  Randomize;
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TmaOptionsForm, maOptionsForm);
  Application.CreateForm(TmaFilterForm, maFilterForm);
  Application.CreateForm(TtgOptionsForm, tgOptionsForm);
  Application.CreateForm(TtgFilterForm, tgFilterForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.

