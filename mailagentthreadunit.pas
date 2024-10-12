unit MailAgentThreadUnit;

{$mode Delphi}

interface

uses
  Classes, SysUtils, imapsend, smtpsend, ssl_openssl, synautil;

type

  TMailAgentMessageType = (mamtVoid);

  // mamtVoid
  PMailAgentMessageData = ^TMailAgentMessageData;
  TMailAgentMessageData = packed record
    What: TMailAgentMessageType;
    Next: PMailAgentMessageData;
  end;

  PMailAgentMessages = ^TMailAgentMessages;
  TMailAgentMessages = packed record
    Next: PMailAgentMessages;
    Subj: String;
    Body: String;
    More: PMailAgentMessageData;
  end;

  TMailAgentMessageEvent = procedure (const AMail: TMailAgentMessages;
                                      var Handled: Boolean) of object;
  TMailAgentUnhandledMessageEvent = procedure (const AMail: TMailAgentMessages) of object;

  { TMailAgent }

  TMailAgent = class(TThread)
  private
    FAPI: TIMAPSend;
    FDelay: LongInt;
    FLogin: Boolean;
    FPortnumb: Word;
    FSendHost: String;
    FRecvHost: String;
    FUsername: String;
    FPassword: String;
    FRecv: PMailAgentMessages;
    FSend: PMailAgentMessages;
    FSync: TMailAgentMessages;
    FOnMail: TMailAgentMessageEvent;
    FOnUnhandledMail: TMailAgentUnhandledMessageEvent;
    procedure CreateAPI;
    procedure DeleteAPI;
    procedure SendMails;
    procedure RecvMails;
    procedure SyncRecvMail;
    function GetConnectedStatus: Boolean;
    function NewMessage(const ASubject: String; const ABody: String): PMailAgentMessages;
    procedure PushToRecv(AMessage: PMailAgentMessages);
    procedure PushToSend(AMessage: PMailAgentMessages);
    function DisposeMessage(AMessage: PMailAgentMessages): PMailAgentMessages;
    procedure SetHostname(AValue: String);
    procedure SetPassword(AValue: String);
    procedure SetUsername(AValue: String);
  public
    constructor Create(const AHostname: String; const AUsername: String;
                       const APassword: String; ADelay: Cardinal = 1000);
    destructor Destroy; override;
    property Connected: Boolean read GetConnectedStatus;
    property Hostname: String read FHostname write SetHostname;
    property Username: String read FUsername write SetUsername;
    property Password: String read FPassword write SetPassword;
    property OnMail: TMailAgentMessageEvent read FOnMail write FOnMail;
    property OnUnhandledMail: TMailAgentUnhandledMessageEvent
                         read FOnUnhandledMail
                        write FOnUnhandledMail;
    procedure SetSMTPHost(const AHostname: String);
    procedure SetOptions(const AHostname, AUsername, APassword: String);
    procedure SendMail(const AMailTo, ASubject: String; const ABody: TStrings;
                       const Attach: TStrings); overload;
    function SendMail(const AMailTo, ASubject: String;
                       const ABody: TStrings): Boolean; overload;
  protected
    procedure Execute; override;
  end;

implementation

{ TMailAgent }

constructor TMailAgent.Create(const AHostname: String; const AUsername: String;
  const APassword: String; ADelay: Cardinal);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  if (ADelay = 0) or (ADelay > High(FDelay))
    then FDelay := 1000
    else FDelay := ADelay;
  if FDelay < 300 then FDelay := FDelay * 1000;
  FRecvHost := AHostname;
  FUsername := AUsername;
  FPassword := APassword;

  FAPI: TIMAPSend;
  FLogin: Boolean;
  FPortnumb: Word;
  FSendHost: String;
  FRecv: PMailAgentMessages;
  FSend: PMailAgentMessages;
  FSync: TMailAgentMessages;
  FOnMail: TMailAgentMessageEvent;
  FOnUnhandledMail: TMailAgentUnhandledMessageEvent;

  CreateAPI;
end;

destructor TMailAgent.Destroy;
begin
  DeleteAPI;
  inherited Destroy;
end;

procedure TMailAgent.CreateAPI;
begin
  if not Assigned(FAPI) then begin
    FAPI           := TIMAPSend.Create;
    s              := Trim(SeparateRight(FRecvHost, ':'));
    with FAPI do begin
      TargetHost   := Trim(SeparateLeft(FRecvHost, ':'));
      if (s <> '') and (s <> FRecvHost) then
        TargetPort := s;
      Username     := FUsername;
      Password     := FPassword;
      AutoTLS      := True;
      FullSSL      := True;
      FLogon       := Login;
    end;
  end;
end;

procedure TMailAgent.DeleteAPI;
begin

end;

procedure TMailAgent.Execute;
begin
  while not Terminated do if Assigned(FAPI) then begin
    RecvMails;
    SendMails;
    Sleep(FDelay);
  end else Sleep(FDelay);
end;

procedure TMailAgent.SetSMTPHost(const AHostname: String);
begin
  FSendHost := AHostname;
end;

procedure TMailAgent.SetOptions(const AHostname, AUsername, APassword: String);
  function GetHostname(const AHostname: String; out aPort: Word;
    aDefault: Word = 143): String;
  var
    i: LongInt;
  begin
    i := Pos(':', AHostname);
    if i <> 0 then begin
      Result := Copy(AHostname, 1, i - 1);
      aPort  := StrToIntDef(Copy(AHostname, i + 1, Length(AHostname) - i), aDefault);
    end else begin
      Result := AHostname;
      aPort  := aDefault;
    end;
  end;

var
  bUpdateHost, bUpdatePort, bUpdateUser, bUpdatePass: Boolean;
  ARealHost: String;
  aRealPort: Word;
begin
  aRealHost := GetHostname(AHostname, aRealPort, 993); // 143 - без SSL; 993 - SSL
  bUpdateHost := FRecvHost <> aRealHost;
  bUpdateUser := FUsername <> AUsername;
  bUpdatePass := FPassword <> APassword;
  if bUpdateHost or bUpdateUser or bUpdatePass then begin
    if Assigned(FAPI) then DeleteAPI;
    if bUpdateHost then FHostname := aRealHost;
    if bUpdateUser then FUsername := AUsername;
    if bUpdatePass then FPassword := APassword;
    CreateAPI;
  end;
end;

procedure TMailAgent.SetHostname(AValue: String);
begin
  SetOptions(AValue, FUsername, FPassword);
end;

procedure TMailAgent.SetUsername(AValue: String);
begin
  SetOptions(FRecvHost, AValue, FPassword);
end;

procedure TMailAgent.SetPassword(AValue: String);
begin
  SetOptions(FRecvHost, FUsername, AValue);
end;

procedure TMailAgent.SyncRecvMail;
var
  Handled: Boolean;
begin
  Handled := False;
  if Assigned(FOnMail) then FOnMail(FSync, Handled);
  if (not Handled) and (Assigned(FOnUnhandledMail)) then FOnUnhandledMail(FSync);
end;

procedure TMailAgent.RecvMails;
  procedure DoRecvMail(AMail: PMailAgentMessages);
  begin
    with AMail^ do begin
      if Assigned(Next) then DoRecvMail(Next);
      Next  := nil;
      FSync := AMail^;
      Synchronize(SyncRecvMail);
      DisposeMessage(AMail);
    end;
  end;

var
  Data: PMailAgentMessages;
begin
  Data  := FRecv;
  FRecv := nil;
  if not Assigned(Data) then DoRecvMail(Data);
end;

procedure TMailAgent.SendMails;
  procedure DoSendMail(AMail: PMailAgentMessages);
  begin
    with AMail^ do begin
      if Assigned(Next) then DoSendMail(Next);
      Next  := nil;
// !!!Тут отправляем письмо!!!
      DisposeMessage(AMail);
    end;
  end;

var
  Data: PMailAgentMessages;
begin
  Data  := FSend;
  FSend := nil;
  if Assigned(Data) then DoSendMail(Data);
end;

function TMailAgent.GetConnectedStatus: Boolean;
begin
  Result := False;
  if not Assigned(FAPI) then Exit;
end;

function TMailAgent.NewMessage(const ASubject: String; const ABody: String
  ): PMailAgentMessages;
begin
  New(Result);
  with Result^ do begin
    Next := nil;
    Subj := ASubject;
    Body := ABody;
    More := nil;
  end;
end;

procedure TMailAgent.PushToRecv(AMessage: PMailAgentMessages);
var
  Data: PMailAgentMessages;
begin
  if Assigned(AMessage) then begin
    Data       := AMessage;
    while Assigned(Data^.Next) do
      Data     := Data^.Next;
    Data^.Next := FRecv;
    FRecv      := AMessage;
  end;
end;

procedure TMailAgent.PushToSend(AMessage: PMailAgentMessages);
var
  Data: PMailAgentMessages;
begin
  if Assigned(AMessage) then begin
    Data       := AMessage;
    while Assigned(Data^.Next) do
      Data     := Data^.Next;
    Data^.Next := FSend;
    FSend      := AMessage;
  end;
end;

function TMailAgent.DisposeMessage(AMessage: PMailAgentMessages
  ): PMailAgentMessages;
var
  Data, Temp: PMailAgentMessageData;
begin
  if not Assigned(AMessage) then Exit;
  with AMessage^ do begin
    Data   := More;
    while Assigned(Data) do begin
      Temp := Data;
      Data := Data^.Next;
      with Temp^ do case What of
      mamtVoid: Dispose(Temp);
      end;
    end;
    Result := Next;
  end;
  Dispose(AMessage);
end;

procedure TMailAgent.SendMail(const AMailTo, ASubject: String;
  const ABody: TStrings; const Attach: TStrings);
begin

end;

function TMailAgent.SendMail(const AMailTo, ASubject: String;
  const ABody: TStrings): Boolean;
var
  Data: TStringList;
  s, t: String;
begin
  Result          := False;
  Data        := TStringList.Create;
  with Data do begin
    Add('From: ' + FUsername);
    Add('To: ' + AMailTo);
    Add('Date: ' + Rfc822DateTime(now));
    Add('Subject: ' + ASubject);
    Add('X-mailer: Synapse - Delphi & Kylix TCP/IP library');
    Add('');
    AddStrings(ABody);
  end;
  with TSMTPSend.Create do try
    AutoTLS       := True;
    FullSSL       := True;
    TargetHost    := Trim(SeparateLeft(FSendHost, ':'));
    s             := Trim(SeparateRight(FSendHost, ':'));
    if (s <> '') and (s <> FSendHost) then
      TargetPort  := s;
    Username      := FUsername;
    Password      := FPassword;
    if Login then
    begin
      if MailFrom(GetEmailAddr(FUsername), Length(Data.Text)) then
      begin
        s := AMailTo;
        repeat
          t := GetEmailAddr(Trim(FetchEx(s, ',', '"')));
          if t <> '' then Result := MailTo(t);
        until (s = '') or (not Result);
        if Result then Result := MailData(Data);
      end;
      Logout;
    end;
  finally
    Free;
  end;
end;

end.

