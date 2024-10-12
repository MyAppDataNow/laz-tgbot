unit MainFormUnit;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, TelegramBotThreadUnit, MailAgentThreadUnit, LazUTF8, tgFilterFormTypes,
  MyFunc;

const
  sMainConfigFileName = 'CommonConfig.txt';

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    memLog: TMemo;
    mmBotSettings: TMenuItem;
    mmBotFilter: TMenuItem;
    mmActivateBot: TMenuItem;
    Separator3: TMenuItem;
    mmAgentSettings: TMenuItem;
    mmAgentFilter: TMenuItem;
    mmActivateAgent: TMenuItem;
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    mmAgent: TMenuItem;
    mmBot: TMenuItem;
    mmHelp: TMenuItem;
    mmAbout: TMenuItem;
    mmQuit: TMenuItem;
    mmProgram: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mmAboutClick(Sender: TObject);
    procedure mmActivateAgentClick(Sender: TObject);
    procedure mmActivateBotClick(Sender: TObject);
    procedure mmAgentFilterClick(Sender: TObject);
    procedure mmAgentSettingsClick(Sender: TObject);
    procedure mmBotFilterClick(Sender: TObject);
    procedure mmBotSettingsClick(Sender: TObject);
    procedure mmQuitClick(Sender: TObject);
    procedure GetNewMail(const AMail: TMailAgentMessages;
                            var Handled: Boolean);
    procedure GetNewMessage(const AMessage: TTelegramBotMessage;
                            var Handled: Boolean);
  private
    FHostSMTP: String;
    FHostIMAP: String;
    FUsername: String;
    FPassword: String;
    FToken: String;
    FMailAgent: TMailAgent;
    FTelegramBot: TTelegramBot;
    FTelegramMsg: TTelegramBotMessage;
    procedure CreateMailAgent;
    procedure DeleteMailAgent;
    procedure CreateTelegramBot;
    procedure DeleteTelegramBot;
  public
    procedure SendMailTo(const aMailTo, aSubject: String; aBody: TStrings);
    procedure SendTelegramText(const aText: String);
  end;

var
  MainForm: TMainForm;

procedure Scripts_Run(aScript: PtvCommand);
function Scripts_Dispose(aCommand: PtvCommand): PtvCommand;

implementation

uses
  maOptionsFormUnit, maFilterFormUnit, tgOptionsFormUnit, tgFilterFormUnit,
  AboutFormUnit;

{$R *.lfm}

{ Scripts procedures }

procedure Script_RandomSelect(anArgs: String; aSelf: PtvCommand); // Тут выбираем случайный вариант действия, если такое понадобится
var
  aHead, aTail: PtvCommand;
  aMax, aCount: LongInt;
begin
  aCount        := 0;
  aHead         := aSelf^.next;
  aTail         := aHead;
  aMax          := StrToIntDef(anArgs, 1);
  while (aMax <> 0) and Assigned(aSelf^.next) do begin
    aTail       := aSelf^.next;
    aSelf^.next := aTail^.next;
    inc(aCount);
    dec(aMax);
  end;
  aTail^.next   := nil;
  aCount        := Random(aCount);
  while aCount > 0 do begin
    aTail       := aHead;
    aHead       := Scripts_Dispose(aTail);
    dec(aCount);
  end;
  aTail         := aHead^.next;
  aHead^.next   := nil;
  Scripts_Run(aHead);
  while Assigned(aTail) do
    aTail       := Scripts_Dispose(aTail);
end;

procedure Script_TelegramAnswer(anArg: String; anArgs: PtvCommandArgs); // И тут
begin
  MainForm.SendTelegramText(anArg);
end;

procedure Script_SendMailTo(aMailBoxTo: String; anArgs: PtvCommandArgs); // И тут
const
  aSubjectPrefix = 'subject:';
  aMessagePrefix = 'message:';
var
  aNext: PtvCommandArgs;
  aSubject: String;
  aMessage: TStringList;
begin       // Сейчас он параметры письма просто добавляет в лог
            // Осталось просто отправить письмо по этим параметрам
  aNext := anArgs;
  aMessage := TStringList.Create;
  while Assigned(aNext) do with aNext^ do begin
    if Pos(aSubjectPrefix, text) = 1
      then aSubject := Trim(Copy(text, Length(aSubjectPrefix) + 1,
                       Length(text) - Length(aSubjectPrefix)))
      else if Pos(aMessagePrefix, text) = 1
      then aMessage.Add(Trim(Copy(text, Length(aMessagePrefix) + 1,
                        Length(text) - Length(aMessagePrefix))));
    aNext := aNext^.next;
  end;
  MainForm.SendMailTo(aMailBoxTo, aSubject, aMessage);
  aMessage.Free;
end;

procedure Script_DefaultAction(aText: String; anArgs: PtvCommandArgs); // А тут их исполнение
begin
  // А это для обработки ошибок в скриптах
  // ShowMessage('Не могу исполнить "' + aText + '"');
end;

procedure Scripts_Run(aScript: PtvCommand); // Вот тут выбор команды
const
  sCommandRandom = 'select random:';
  sCommandAnswer = 'answer:';
  sCommandMailTo = 'mailto:';
var
  aNext: PtvCommand;
begin
  aNext := aScript;
  while Assigned(aNext) do with aNext^ do begin
    if Pos(sCommandRandom, text) = 1
      then Script_RandomSelect(Trim(Copy(text, Length(sCommandRandom) + 1,
                               Length(text) - Length(sCommandRandom))), aNext)
      else if Pos(sCommandAnswer, text) = 1
      then Script_TelegramAnswer(Trim(Copy(text, Length(sCommandAnswer) + 1,
                               Length(text) - Length(sCommandAnswer))), args)
      else if Pos(sCommandMailTo, text) = 1
      then Script_SendMailTo(Trim(Copy(text, Length(sCommandMailTo) + 1,
                               Length(text) - Length(sCommandMailTo))), args)
      else Script_DefaultAction(text, args);
    aNext := Scripts_Dispose(aNext);
  end;
end;

function Scripts_Dispose(aCommand: PtvCommand): PtvCommand;
  function DisposeArg(anArg: PtvCommandArgs): PtvCommandArgs;
  begin
    if Assigned(anArg) then begin
      Result := anArg^.next;
      Dispose(anArg);
    end;
  end;

var
  anArg: PtvCommandArgs;
begin
  if Assigned(aCommand) then with aCommand^ do begin
    Result := next;
    anArg  := args;
    while Assigned(anArg) do anArg := DisposeArg(anArg);
    Dispose(aCommand);
  end;
end;

{ TMainForm events }

procedure TMainForm.FormCreate(Sender: TObject);
var
  S: TStringList;
begin
  if FileExists(sMainConfigFileName) then begin
    S := TStringList.Create;
    S.LoadFromFile(sMainConfigFileName);
    if S.Count = 5 then begin
      FHostIMAP := S[0];
      FHostSMTP := S[1];
      FUsername := S[2];
      FPassword := S[3];
      FToken    := S[4];
    end;
    FreeAndNil(S);
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  S: TStringList;
begin
  DeleteMailAgent;
  DeleteTelegramBot;
  S := TStringList.Create;
  S.Add(FHostIMAP);
  S.Add(FHostSMTP);
  S.Add(FUsername);
  S.Add(FPassword);
  S.Add(FToken);
  S.SaveToFile(sMainConfigFileName);
  FreeAndNil(S);
end;

procedure TMainForm.mmActivateAgentClick(Sender: TObject);
begin
  if mmActivateAgent.Checked then CreateMailAgent else DeleteMailAgent;
end;

procedure TMainForm.mmActivateBotClick(Sender: TObject);
begin
  if mmActivateBot.Checked then CreateTelegramBot else DeleteTelegramBot;
end;

procedure TMainForm.mmAgentSettingsClick(Sender: TObject);
var
  sIMAP, sSMTP, sUser, sPass: String;
  bUpdate: Boolean;
begin
  sIMAP       := FHostIMAP;
  sSMTP       := FHostSMTP;
  sUser       := FUsername;
  sPass       := FPassword;
  maOptionsForm.ShowOptions(sIMAP, sSMTP, sUser, sPass);
  bUpdate     := False;
  if sIMAP <> FHostIMAP then begin
    bUpdate   := True;
    FHostIMAP := sIMAP;
  end;
  if sSMTP <> FHostSMTP then begin
    bUpdate   := True;
    FHostSMTP := sSMTP;
  end;
  if sUser <> FUsername then begin
    bUpdate   := True;
    FUsername := sUser;
  end;
  if sPass <> FPassword then begin
    bUpdate   := True;
    FPassword := sPass;
  end;
  if bUpdate then begin
    DeleteMailAgent;
    if mmActivateAgent.Checked then CreateMailAgent;
  end;
end;

procedure TMainForm.mmBotSettingsClick(Sender: TObject);
var
  sToken: String;
begin
  sToken := FToken;
  tgOptionsForm.ShowOptions(sToken);
  if sToken <> FToken then begin
    FToken := sToken;
    DeleteTelegramBot;
    if mmActivateBot.Checked then CreateTelegramBot;
  end;
end;

procedure TMainForm.mmAgentFilterClick(Sender: TObject);
begin
  maFilterForm.Show;
end;

procedure TMainForm.mmBotFilterClick(Sender: TObject);
begin
  tgFilterForm.Show;
end;

procedure TMainForm.mmQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.mmAboutClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.GetNewMail(const AMail: TMailAgentMessages;
  var Handled: Boolean);
begin
  memLog.Lines.Add('Почта: "' + AMail.Subj + '"' + #13 + #10 + AMail.Body);
  Handled := True;
end;

procedure TMainForm.GetNewMessage(const AMessage: TTelegramBotMessage;
  var Handled: Boolean);
  function CutStartString(var AText: String; AStart: String): Boolean;
  begin
    Result := UTF8Pos(AStart, AText) = 1;
    if Result then AText := UTF8Trim(Copy(AText, Length(AStart) + 1,
                                     Length(AText) - Length(AStart)));
  end;

  function GetSubText(AList: TStringList; const AStart, ADefault: String): String;
  var
    sText: String;
    i: LongInt;
  begin
    Result := ADefault;
    for i := 0 to AList.Count - 1 do begin
      sText := AList[i];
      if CutStartString(sText, AStart) then begin
        Result := sText;
        break;
      end;
    end;
  end;

const
  sDefault = 'Я не разговариваю с незнакомыми людьми!';
  sDefaultAnswer = 'Не знаю как ответить!';
  sHtmlMessage = 'html:';
  sMailToCmnd  = 'mailto:';
  aSubjectCmnd = 'subject:';
  sBodyCommand = 'message:';
  aDefSubject  = 'Telegram bot email';
  fmtMailTo    = 'Бот: email -> <%s>: "%s" {' + Eoln + '%s' + Eoln + '}';
var
  sName, sSubj, sBody, sAnswer: String;
  aTemp: TMemoryStream;
  aScripts, aNext: PtvCommand;
begin
  memLog.Lines.BeginUpdate;
  sName := GetMessageUsername(AMessage);
  memLog.Lines.Add(Format('Бот: <%s> "%s"', [sName, GetMessageText(AMessage)]));
  sAnswer := sDefault; // Этот ответ, если фильтр по пользователю или команду не пройден т.е. в ТГ написал кто-то чей ник не в фильтре или написали текст не из фильтра
  if not tgFilterForm.IsAcceptedCommand(AMessage, aScripts) then
    sAnswer := sDefaultAnswer; // Этот ответ, если ошибка в параметрах команды т.е. в ТГ написал ник из фильтра и команда найдена, но параметр к команду записан не правильно
  // Далее мы получили список выполняемых скриптов для данной команды
  // Выполняем все скрипты записанные в команде
  aNext := aScripts;
  FTelegramMsg := AMessage;
  if not Assigned(aScripts) then begin
    FTelegramBot.SendResponse(AMessage, sDefaultAnswer); // Не знаем как ответить т.к. ни одного скрипта не прописано для команды
    memLog.Lines.Add(Format('Бот: ответ -> <%s> "%s"', [sName, sAnswer]));
  end else Scripts_Run(aNext);// А это уже цикл по всем скриптам, чтобы можно было выполнять все прописанные скрипты
{ Это все тестовый обработчик команд

  aButtons := TStringList.Create;
  sSubj := 'Files = ' + IntToStr(GetMessageFilesCount(AMessage));
  if sSubj <> 'Files = 0' then begin
    memLog.Lines.Add('Бот: ' + sSubj);
    aTemp := TMemoryStream.Create;
    memLog.Lines.Add('Downloading: ' +
               DownloadFileFromMessage(aTemp, AMessage));
    aTemp.SaveToFile('/tmp/tempfile.bin');
    FreeAndNil(aTemp);
  end;
  if not tgFilterForm.IsAcceptedCommand(AMessage, aScripts)
    then sAnswer := sDefault;
  if CutStartString(sAnswer, sMailToCmnd) then begin
    sSubj := GetSubText(aButtons, aSubjectCmnd, aDefSubject);
    sBody := GetSubText(aButtons, sBodyCommand, sName);
    // FMailAgent.SendEmail(sAnswer, sSubj, sBody);
    memLog.Lines.Add(Format(fmtMailTo, [sAnswer, sSubj, sBody]));
  end else if CutStartString(sAnswer, sHtmlMessage) then begin
    if aButtons.Count > 0 then begin
      FTelegramBot.SendResponse(AMessage, sAnswer, 0, pmHTML, aButtons);
      memLog.Lines.Add(Format('Бот: ответ -> <%s> <html>%s</html>', [sName, sAnswer]));
      for sName in aButtons do memLog.Lines.Add('Кнопка: "' + sName + '"');
    end else begin
      FTelegramBot.SendResponse(AMessage, sAnswer, 0, pmHTML);
      memLog.Lines.Add(Format('Бот: ответ -> <%s> <html>%s</html>', [sName, sAnswer]));
    end;
  end else if aButtons.Count > 0 then begin
    FTelegramBot.SendResponse(AMessage, sAnswer, 0, pmDefault, aButtons);
    memLog.Lines.Add(Format('Бот: ответ -> <%s> "%s"', [sName, sAnswer]));
    for sName in aButtons do memLog.Lines.Add('Кнопка: "' + sName + '"');
  end else begin
    FTelegramBot.SendResponse(AMessage, sAnswer);
    memLog.Lines.Add(Format('Бот: ответ -> <%s> "%s"', [sName, sAnswer]));
  end;
  FreeAndNil(aButtons);}
  memLog.Lines.EndUpdate;
  Handled := True;
end;

{ TMainForm private }

procedure TMainForm.CreateMailAgent;
begin
  if (trim(FHostIMAP) = EmptyStr) and (trim(FHostSMTP) = EmptyStr)
    and (trim(FUsername) = EmptyStr) and (trim(FPassword) = EmptyStr) then Exit;
  if Assigned(FMailAgent)
    then begin
      FMailAgent.SetSMTPHost(FHostSMTP);
      FMailAgent.SetOptions(FHostIMAP, FUsername, FPassword)
    end else begin
      FMailAgent := TMailAgent.Create(FHostIMAP, FUsername, FPassword);
      FMailAgent.SetSMTPHost(FHostSMTP);
      FMailAgent.OnMail := GetNewMail;
      if FMailAgent.Connected then FMailAgent.Start;
    end;
end;

procedure TMainForm.DeleteMailAgent;
begin
  if not Assigned(FMailAgent) then Exit;
  FMailAgent.Terminate;
  FMailAgent.WaitFor;
  FreeAndNil(FMailAgent);
end;

procedure TMainForm.CreateTelegramBot;
begin
  if trim(FToken) = EmptyStr then Exit;
  if Assigned(FTelegramBot)
    then FTelegramBot.Token := FToken
    else begin
      FTelegramBot := TTelegramBot.Create(FToken);
      FTelegramBot.OnMessage := GetNewMessage;
      if FTelegramBot.Connected then
        FTelegramBot.Start;
    end;
end;

procedure TMainForm.DeleteTelegramBot;
begin
  if not Assigned(FTelegramBot) then Exit;
  FTelegramBot.Terminate;
  FTelegramBot.WaitFor;
  FreeAndNil(FTelegramBot);
end;

procedure TMainForm.SendMailTo(const AMailTo, aSubject: String; aBody: TStrings
  );
begin
  if Assigned(FMailAgent) then with memLog.Lines do begin
    Add(Format('Агент: отправка -> <%s> "%s" {',
                            [aMailTo, aSubject]));
    AddStrings(aBody);
    Add('}');
    FMailAgent.SendMail(aMailTo, aSubject, ABody);
  end else memLog.Lines.Add('Агент: мертв!'); // Если не запущен агент
end;

procedure TMainForm.SendTelegramText(const aText: String);
begin
  if Assigned(FTelegramBot) then begin
    memLog.Lines.Add(Format('Бот: ответ -> <%s> "%s"', [
                            GetMessageUsername(FTelegramMsg), AText]));
    FTelegramBot.SendResponse(FTelegramMsg, AText);
  end else memLog.Lines.Add('Бот: мертв!'); // Если не запущен бот
end;

end.

