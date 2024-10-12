unit TelegramBotThreadUnit;

{$mode Delphi}

interface

uses
  Classes, SysUtils, tgtypes, tgsendertypes, eventlog, lazutf8, fphttpclient;

const
  sDefUnhandledMessage = 'Я вас не понимаю! Задайте свой вопрос по другому.';
  sDefaultUsername     = '<не задан>';

type
  TTelegramBotMessage = Pointer;
  TTelegramBotMessageFile = Pointer;
  TTelegramBotParseMode = (pmDefault, pmMarkdown, pmHTML);
  TTelegramBotMessageType = (tbmtVoid, tbmtUser, tbmtQuote, tbmtButtons,
                             tbmtFile, tbmtMode);
  TTelegramBotMessageFileType = (tbftVoid, tbftAudio, tbftDocument, tbftPhoto,
                                 tbftVideo, tbftVoice);

  // tbmtVoid
  PTelegramBotMessageData = ^TTelegramBotMessageData;
  TTelegramBotMessageData = packed record
    What: TTelegramBotMessageType;
    Next: PTelegramBotMessageData;
  end;

  // tbmtUser
  PTelegramBotMessageDataUser = ^TTelegramBotMessageDataUser;
  TTelegramBotMessageDataUser = packed record
    Base: TTelegramBotMessageData;
    User: String;
    Name: String;
    Last: String;
    isBot: Boolean;
  end;

  // tbmtQuote
  PTelegramBotMessageDataQuote = ^TTelegramBotMessageDataQuote;
  TTelegramBotMessageDataQuote = packed record
    Base: TTelegramBotMessageData;
    MsgId: LongInt;
  end;

  // tbmtButtons
  PTelegramBotMessageDataBtns = ^TTelegramBotMessageDataBtns;
  TTelegramBotMessageDataBtns = packed record
    Base: TTelegramBotMessageData;
    Buttons: array of String;
    DefButn: LongInt;
  end;

  // tbmtFile
  PTelegramBotMessageDataFile = ^TTelegramBotMessageDataFile;
  TTelegramBotMessageDataFile = packed record
    Base: TTelegramBotMessageData;
    What: TTelegramBotMessageFileType;
    Size: Int64;
    Mime: String;
    Spec: String;
    FileId: String;
    URL: String;
  end;

  // tbmtMode
  PTelegramBotMessageDataMode = ^TTelegramBotMessageDataMode;
  TTelegramBotMessageDataMode = packed record
    Base: TTelegramBotMessageData;
    Mode: TTelegramBotParseMode;
  end;

  PTelegramBotMessages = ^TTelegramBotMessages;
  TTelegramBotMessages = packed record
    Next: PTelegramBotMessages;
    Chat: Int64;
    Count: LongInt;
    Text: String;
    More: PTelegramBotMessageData;
  end;

  TTelegramBotMessageEvent = procedure (const AMessage: TTelegramBotMessage;
                                        var Handled: Boolean) of object;
  TTelegramBotUnhandledMessageEvent =
                     procedure (const AMessage: TTelegramBotMessage) of object;

  { TTelegramBot }

  TTelegramBot = class(TThread)
    procedure RecvNewMessage(ASender: TObject; AMessage: TTelegramMessageObj);
  private
    FAPI: TTelegramSender;
    FDebugLog: Boolean;
    FToken: String;
    FDelay: LongInt;
    FNew: PTelegramBotMessages;
    FRecv: PTelegramBotMessages;
    FSend: PTelegramBotMessages;
    FSync: TTelegramBotMessages;
    FOnMessage: TTelegramBotMessageEvent;
    FOnUnhandledMessage: TTelegramBotUnhandledMessageEvent;
    procedure SetDebugLog(AValue: Boolean);
    procedure SetToken(const AToken: String);
    procedure CreateAPI;
    procedure DeleteAPI;
    procedure CreateLog;
    procedure DeleteLog;
    procedure SendMessages;
    procedure RecvMessages;
    procedure SyncRecvMessage;
    function GetConnectedStatus: Boolean;
    function NewMessage(AChatID: Int64; const AText: String): PTelegramBotMessages;
    procedure AddMessageMode(AMessage: PTelegramBotMessages;
                             AMode: TTelegramBotParseMode);
    procedure AddMessageQuote(AMessage: PTelegramBotMessages; AMsgId: LongInt);
    procedure AddMessageUser(AMessage: PTelegramBotMessages;
                             const AUsername: String;
                             const AFirstName: String = '';
                             const ALastName: String = '';
                             IsABot: Boolean = False);
    procedure AddMessageButtons(AMessage: PTelegramBotMessages;
                                AButtons: array of const); overload;
    procedure AddMessageButtons(AMessage: PTelegramBotMessages;
                                ADefButton: LongInt;
                                AButtons: array of const); overload;
    procedure AddMessageButtons(AMessage: PTelegramBotMessages;
                                AButtons: TStringList); overload;
    procedure AddMessageButtons(AMessage: PTelegramBotMessages;
                                ADefButton: LongInt;
                                AButtons: TStringList); overload;
    procedure AddMessageFile(AMessage: PTelegramBotMessages;
                             AType: TTelegramBotMessageFileType; ASize: Int64;
                             const AMimeType, AFileId, AFileSpec: String);
    procedure PushToSend(AMessage: PTelegramBotMessages);
    procedure PushToRecv(AMessage: PTelegramBotMessages);
    function DisposeMessage(AMessage: PTelegramBotMessages): PTelegramBotMessages;
    function GetMessageUser(const AMessage: PTelegramBotMessages;
                             out AUser: PTelegramBotMessageDataUser): Boolean;
    function GetMessageQuote(const AMessage: PTelegramBotMessages;
                             out AQuote: PTelegramBotMessageDataQuote): Boolean;
    function GetMessageParse(const AMessage: PTelegramBotMessages;
                             out AMode: PTelegramBotMessageDataMode): Boolean;
    function GetButtonsCount(const AMessage: PTelegramBotMessages;
                             out AButtons: PTelegramBotMessageDataBtns): LongInt;
    procedure SyncSendMessage;
  public
    constructor Create(const AToken: String; ADelay: Cardinal = 1);
    destructor Destroy; override;
    property Debug: Boolean read FDebugLog write SetDebugLog;
    property OnMessage: TTelegramBotMessageEvent read FOnMessage write FOnMessage;
    property OnUnhandled: TTelegramBotUnhandledMessageEvent read FOnUnhandledMessage
                                                           write FOnUnhandledMessage;
    property Connected: Boolean read GetConnectedStatus;
    property Token: String read FToken write SetToken;
    procedure SendResponse(const AMessage: TTelegramBotMessage;
                           const AText: String; AMsgQuote: LongInt = 0;
                           AParser: TTelegramBotParseMode = pmDefault;
                           AButtons: array of const;
                           ADefBtn: LongInt = -1); overload;
    procedure SendResponse(const AMessage: TTelegramBotMessage;
                           const AText: String; AMsgQuote: LongInt = 0;
                           AParser: TTelegramBotParseMode = pmDefault;
                           AButtons: TStringList = nil;
                           ADefBtn: LongInt = -1); overload;
  protected
    procedure Execute; override;
  end;

function GetMessageText(const AMessage: TTelegramBotMessage): String;
function QuoteMessageId(const AMessage: TTelegramBotMessage): LongInt;
function IsMessageFromBot(const AMessage: TTelegramBotMessage): Boolean;
function GetMessageUsername(const AMessage: TTelegramBotMessage): String;
function GetMessageUserFirstname(const AMessage: TTelegramBotMessage): String;
function GetMessageUserLastname(const AMessage: TTelegramBotMessage): String;
function GetMessageDefButton(const AMessage: TTelegramBotMessage): String;
function GetMessageButton(const AMessage: TTelegramBotMessage;
                          AIndex: LongInt = 0): String;
function GetMessageFilesCount(const AMessage: TTelegramBotMessage): LongInt;
function GetMessageFileByIndex(const AMessage: TTelegramBotMessage;
                               AIndex: LongInt = 0): TTelegramBotMessageFile;
function DownloadMessageFile(AFileData: TStream;
                        const AFile: TTelegramBotMessageFile): String; overload;
function DownloadFileFromMessage(AFileData: TStream;
                                 const AMessage: TTelegramBotMessage;
                                 AIndex: LongInt = 0): String; overload;
function ReplaceWildcard(const AFormat: String;
                         AMessage: PTelegramBotMessages): String;

implementation

{ TTelegramBot }

constructor TTelegramBot.Create(const AToken: String; ADelay: Cardinal);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  if (ADelay = 0) or (ADelay > High(FDelay))
    then FDelay   := 1
    else FDelay   := ADelay;
  FToken          := AToken;
  FDebugLog       := False;
  CreateAPI;
end;

destructor TTelegramBot.Destroy;
begin
  DeleteAPI;
  inherited Destroy;
end;

procedure TTelegramBot.CreateAPI;
begin
  if Assigned(FAPI) then Exit;
  FAPI := TTelegramSender.Create(FToken);
  FAPI.APIEndPoint := TelegramAPI_URL;
  if FDebugLog then CreateLog;
  FAPI.OnReceiveMessage := RecvNewMessage;
end;

procedure TTelegramBot.DeleteAPI;
begin
  if not Assigned(FAPI) then Exit;
  DeleteLog;
  FreeAndNil(FAPI);
end;

function TTelegramBot.GetConnectedStatus: Boolean;
begin
  Result := False;
  if not Assigned(FAPI) then Exit;
  Result := True;
end;

procedure TTelegramBot.CreateLog;
begin
  if not Assigned(FAPI) then Exit;
  if Assigned(FAPI.Logger) then Exit;
  FAPI.Logger         := TEventLog.Create(nil);
  FAPI.Logger.LogType := ltFile;
  FAPI.LogDebug       := True;
end;

procedure TTelegramBot.DeleteLog;
begin
  if not Assigned(FAPI) then Exit;
  if not Assigned(FAPI.Logger) then Exit;
  FAPI.LogDebug  := False;
  FAPI.Logger.Free;
  FAPI.Logger := nil;
end;

function TTelegramBot.NewMessage(AChatID: Int64; const AText: String
  ): PTelegramBotMessages;
begin
  New(Result);
  with Result^ do begin
     Next := nil;
    Count := 0;
     Chat := AChatID;
     Text := AText;
     More := nil;
  end;
end;

procedure TTelegramBot.AddMessageMode(AMessage: PTelegramBotMessages;
  AMode: TTelegramBotParseMode);
var
  Data: PTelegramBotMessageDataMode;
begin
  if GetMessageParse(AMessage, Data) then Data^.Mode := AMode else begin
    New(Data);
    with Data^ do begin
         Base.What := tbmtMode;
         Base.Next := AMessage^.More;
              Mode := AMode;
    end;
    AMessage^.More := PTelegramBotMessageData(Data);
  end;
end;

procedure TTelegramBot.AddMessageQuote(AMessage: PTelegramBotMessages;
  AMsgId: LongInt);
var
  Data: PTelegramBotMessageDataQuote;
begin
  if GetMessageQuote(AMessage, Data) then Data^.MsgId := AMsgId else begin
    New(Data);
    with Data^ do begin
        Base.What  := tbmtQuote;
        Base.Next  := AMessage^.More;
             MsgId := AMsgId;
    end;
    AMessage^.More := PTelegramBotMessageData(Data);
  end;
end;

procedure TTelegramBot.AddMessageUser(AMessage: PTelegramBotMessages;
  const AUsername: String; const AFirstName: String; const ALastName: String;
  IsABot: Boolean);
var
  Data: PTelegramBotMessageDataUser;
begin
  if GetMessageUser(AMessage, Data) then with Data^ do begin
             User  := AUsername;
             Name  := AFirstName;
             Last  := ALastName;
             isBot := IsABot;
  end else begin
    New(Data);
    with Data^ do begin
        Base.What  := tbmtUser;
        Base.Next  := AMessage^.More;
             User  := AUsername;
             Name  := AFirstName;
             Last  := ALastName;
             isBot := IsABot;
    end;
    AMessage^.More := PTelegramBotMessageData(Data);
  end;
end;

procedure TTelegramBot.AddMessageButtons(AMessage: PTelegramBotMessages;
  AButtons: array of const);
begin
  AddMessageButtons(AMessage, -1, AButtons);
end;

procedure TTelegramBot.AddMessageButtons(AMessage: PTelegramBotMessages;
  ADefButton: LongInt; AButtons: array of const);
const
  sFormatStr = '%s';
  sFormatInt = '%d';
  sFormatFlt = '%f';
  sFormatPtr = '0x%016X';
  procedure AddMessageButton(AData: PTelegramBotMessageDataBtns;
                             ASrc, ADst: LongInt);
  begin
    with AData^ do case AButtons[ASrc].VType of
      vtBoolean:
        Buttons[ADst] := BoolToStr(AButtons[ASrc].VBoolean, True);
      vtChar:
        Buttons[ADst] := Format(sFormatStr, [AButtons[ASrc].VChar]);
      vtWideChar:
        Buttons[ADst] := Format(sFormatStr, [AButtons[ASrc].VWideChar]);
      vtString:
        Buttons[ADst] := Format(sFormatStr, [AButtons[ASrc].VString]);
      vtWideString:
        Buttons[ADst] := Format(sFormatStr, [AButtons[ASrc].VWideString]);
      vtPChar:
        Buttons[ADst] := Format(sFormatStr, [AButtons[ASrc].VPChar]);
      vtPWideChar:
        Buttons[ADst] := Format(sFormatStr, [AButtons[ASrc].VPWideChar]);
      vtAnsiString:
        Buttons[ADst] := Format(sFormatStr, [AButtons[ASrc].VAnsiString]);
      vtUnicodeString:
        Buttons[ADst] := Format(sFormatStr, [AButtons[ASrc].VUnicodeString]);
      vtInteger:
        Buttons[ADst] := Format(sFormatInt, [AButtons[ASrc].VInteger]);
      vtInt64:
        Buttons[ADst] := Format(sFormatInt, [AButtons[ASrc].VInt64]);
      vtQWord:
        Buttons[ADst] := Format(sFormatInt, [AButtons[ASrc].VQWord]);
      vtExtended:
        Buttons[ADst] := Format(sFormatFlt, [AButtons[ASrc].VExtended]);
      vtCurrency:
        Buttons[ADst] := Format(sFormatFlt, [AButtons[ASrc].VCurrency]);
      vtPointer:
        Buttons[ADst] := Format(sFormatPtr, [AButtons[ASrc].VPointer]);
      vtObject:
        Buttons[ADst] := Format(sFormatPtr, [AButtons[ASrc].VObject]);
      vtClass:
        Buttons[ADst] := Format(sFormatPtr, [AButtons[ASrc].VClass]);
      vtInterface:
        Buttons[ADst] := Format(sFormatPtr, [AButtons[ASrc].VInterface]);
    end;
  end;

var
  Data: PTelegramBotMessageDataBtns;
  i, l: LongInt;
begin
  if GetButtonsCount(AMessage, Data) = 0 then begin
    New(Data);
    with Data^ do begin
         Base.What := tbmtButtons;
         Base.Next := AMessage^.More;
      SetLength(Buttons, Length(AButtons));
      for i := Low(AButtons) to High(AButtons) do
        AddMessageButton(Data, i, i);
           DefButn := ADefButton;
    end;
    AMessage^.More := PTelegramBotMessageData(Data);
  end else with Data^ do begin
                 l := High(Buttons) + 1;
    SetLength(Buttons, l + Length(AButtons));
    for i := Low(AButtons) to High(AButtons) do
      AddMessageButton(Data, i, l + i);
           DefButn := ADefButton;
  end;
end;

procedure TTelegramBot.AddMessageButtons(AMessage: PTelegramBotMessages;
  AButtons: TStringList);
begin
  AddMessageButtons(AMessage, -1, AButtons);
end;

procedure TTelegramBot.AddMessageButtons(AMessage: PTelegramBotMessages;
  ADefButton: LongInt; AButtons: TStringList);
var
  Data: PTelegramBotMessageDataBtns;
  i, l: LongInt;
begin
  if GetButtonsCount(AMessage, Data) = 0 then begin
    New(Data);
    with Data^ do begin
         Base.What     := tbmtButtons;
         Base.Next     := AMessage^.More;
      SetLength(Buttons, AButtons.Count);
      for i := Low(Buttons) to High(Buttons) do
        Buttons[i]     := AButtons[i];
           DefButn     := ADefButton;
    end;
    AMessage^.More     := PTelegramBotMessageData(Data);
  end else with Data^ do begin
                     l := High(Buttons) + 1;
      SetLength(Buttons, l + AButtons.Count);
      for i := 0 to AButtons.Count - 1 do
        Buttons[l + i] := AButtons[i];
               DefButn := ADefButton;
  end;
end;

procedure TTelegramBot.AddMessageFile(AMessage: PTelegramBotMessages;
                  AType: TTelegramBotMessageFileType; ASize: Int64;
                  const AMimeType, AFileId, AFileSpec: String);
var
  Data: PTelegramBotMessageDataFile;
begin
  if not Assigned(FAPI) then Exit;
  New(Data);
  FAPI.getFile(AFileId);
  with Data^ do begin
       Base.What := tbmtFile;
       Base.Next := AMessage^.More;
            Size := ASize;
            Mime := AMimeType;
            Spec := AFileSpec;
          FileId := AFileId;
             URL := FAPI.FileObj.DownloadLink(FAPI.Token);
  end;
  AMessage^.More := PTelegramBotMessageData(Data);
  inc(AMessage^.Count);
end;

procedure TTelegramBot.PushToSend(AMessage: PTelegramBotMessages);
var
  Data: PTelegramBotMessages;
begin
  if Assigned(AMessage) then begin
    Data       := AMessage;
    while Assigned(Data^.Next) do
      Data     := Data^.Next;
    Data^.Next := FSend;
    FSend      := AMessage;
  end;
end;

procedure TTelegramBot.PushToRecv(AMessage: PTelegramBotMessages);
var
  Data: PTelegramBotMessages;
begin
  if Assigned(AMessage) then begin
    Data       := AMessage;
    while Assigned(Data^.Next) do
      Data     := Data^.Next;
    Data^.Next := FRecv;
    FRecv      := AMessage;
  end;
end;

function TTelegramBot.DisposeMessage(AMessage: PTelegramBotMessages
  ): PTelegramBotMessages;
var
  Data, Temp: PTelegramBotMessageData;
begin
  if not Assigned(AMessage) then Exit;
  with AMessage^ do begin
    Data   := More;
    while Assigned(Data) do begin
      Temp := Data;
      Data := Data^.Next;
      with Temp^ do case What of
        tbmtUser:  Dispose(PTelegramBotMessageDataUser(Temp));
        tbmtQuote: Dispose(PTelegramBotMessageDataQuote(Temp));
        tbmtButtons: with (PTelegramBotMessageDataBtns(Temp)^) do begin
          SetLength(Buttons, 0);
          Buttons := nil;
          Dispose(PTelegramBotMessageDataBtns(Temp));
        end;
      else
        Dispose(Temp);
      end;
    end;
    Result := Next;
  end;
  Dispose(AMessage);
end;

function TTelegramBot.GetMessageUser(const AMessage: PTelegramBotMessages; out
  AUser: PTelegramBotMessageDataUser): Boolean;
var
  Data: PTelegramBotMessageData;
begin
  Result     := False;
  if not Assigned(AMessage) then Exit;
  Data       := AMessage^.More;
  while Assigned(Data) do with Data^ do begin
    if What = tbmtUser then begin
      AUser  := PTelegramBotMessageDataUser(Data);
      Result := True;
      break;
    end;
    Data     := Next;
  end;
end;

function TTelegramBot.GetMessageQuote(const AMessage: PTelegramBotMessages; out
  AQuote: PTelegramBotMessageDataQuote): Boolean;
var
  Data: PTelegramBotMessageData;
begin
  Result     := False;
  if not Assigned(AMessage) then Exit;
  Data       := AMessage^.More;
  while Assigned(Data) do with Data^ do begin
    if What = tbmtQuote then begin
      AQuote := PTelegramBotMessageDataQuote(Data);
      Result := True;
      break;
    end;
    Data     := Next;
  end;
end;

function TTelegramBot.GetMessageParse(const AMessage: PTelegramBotMessages; out
  AMode: PTelegramBotMessageDataMode): Boolean;
var
  Data: PTelegramBotMessageData;
begin
  Result     := False;
  if not Assigned(AMessage) then Exit;
  Data       := AMessage^.More;
  while Assigned(Data) do with Data^ do begin
    if What = tbmtUser then begin
      AMode  := PTelegramBotMessageDataMode(Data);
      Result := True;
      break;
    end;
    Data     := Next;
  end;
end;

function TTelegramBot.GetButtonsCount(const AMessage: PTelegramBotMessages;
  out AButtons: PTelegramBotMessageDataBtns): LongInt;
var
  Data: PTelegramBotMessageData;
begin
  Result       := 0;
  if not Assigned(AMessage) then Exit;
  Data         := AMessage.More;
  while Assigned(Data) do with Data^ do begin
    if What = tbmtButtons then begin
      AButtons := PTelegramBotMessageDataBtns(Data);
      Result   := Length(AButtons^.Buttons);
      break;
    end;
    Data       := Next;
  end;
end;

procedure TTelegramBot.SyncSendMessage;
begin
  PushToSend(FNew);
  FNew := nil;
end;

procedure TTelegramBot.Execute;
begin
  while not Terminated do if Assigned(FAPI) then begin
    FAPI.getUpdatesEx(0, FDelay);
    RecvMessages;
    SendMessages;
  end else Sleep(FDelay);
end;

procedure TTelegramBot.SetToken(const AToken: String);
begin
  if AToken = FToken then Exit;
  DeleteAPI;
  FToken := AToken;
  CreateAPI;
end;

procedure TTelegramBot.RecvNewMessage(ASender: TObject;
  AMessage: TTelegramMessageObj);
  function GetMessageText: String;
  const
    sCaption = '"caption"';
  var
    i, j, l: LongInt;
    bMask, bbMask: Boolean;
    AMessageString: String;
  begin
    if AMessage.Text <> EmptyStr then Result := AMessage.Text else begin
      AMessageString := AMessage.AsString;
      l := Length(AMessageString);
      i := Pos(sCaption, AMessageString);
      if i <> 0 then begin
        j := 1;
        bMask := False;
        inc(i, Length(sCaption) + 1);
        while (i < l) and (AMessageString[i] <> '"') do inc(i);
        while (i + j < l) and ((AMessageString[i + j] <> '"') or bMask) do begin
          bbMask := not bMask;
          bMask := bbMask and (AMessageString[i + j] = '\');
          inc(j);
        end;
        if i + j < l then begin
          AMessageString := Copy(AMessageString, i + 1, j - 1);
          j := 1;
          for i := 1 to Length(AMessageString) do begin
            if bMask then bMask := False else bMask := AMessageString[i] = '\';
            if not bMask then begin
              AMessageString[j] := AMessageString[i];
              inc(j);
            end;
          end;
          Result := Copy(AMessageString, 1, j - 1);
        end else Result := EmptyStr;
      end else Result := EmptyStr;
    end;
  end;

const
  sFormatSpecAudio    = '{Duration: %d; Title: "%s", Performer: "%s"}';
  sFormatSpecDocument = '{Name: "%s"}';
  sFormatSpecPhoto    = '{Width: %d; Height: %d}';
  sFormatSpecVideo    = '{Duration: %d; Width: %d; Height: %d}';
  sFormatSpecVoice    = '{Duration: %d}';
var
  Data: PTelegramBotMessages;
  i: LongInt;
begin
  Data := NewMessage(AMessage.ChatId, GetMessageText);
  if Assigned(AMessage.Voice) then with AMessage.Voice do
    AddMessageFile(Data, tbftVoice, FileSize, MimeType, FileID,
                        Format(sFormatSpecVoice, [Duration]));
  if Assigned(AMessage.Video) then with AMessage.Video do
    AddMessageFile(Data, tbftVideo, FileSize, MimeType, FileID,
                        Format(sFormatSpecVideo, [Duration, Width, Height]));
  if Assigned(AMessage.Photo) then
    for i := 0 to AMessage.Photo.Count - 1 do with AMessage.Photo.Items[i] do
      AddMessageFile(Data, tbftPhoto, FileSize, '', FileID,
                        Format(sFormatSpecPhoto, [Width, Height]));
  if Assigned(AMessage.Document) then with AMessage.Document do
    AddMessageFile(Data, tbftDocument, FileSize, MimeType, FileID,
                        Format(sFormatSpecDocument, [FileName]));
  if Assigned(AMessage.Audio) then with AMessage.Audio do
    AddMessageFile(Data, tbftAudio, FileSize, MimeType, FileID,
                        Format(sFormatSpecAudio, [Duration, Title, Performer]));
  AddMessageUser(Data, AMessage.From.Username, AMessage.From.First_name,
                 AMessage.From.Last_name, AMessage.From.Is_bot);
  PushToRecv(Data);
end;

procedure TTelegramBot.SetDebugLog(AValue: Boolean);
begin
  if FDebugLog=AValue then Exit;
  FDebugLog := AValue;
  if FDebugLog then CreateLog else DeleteLog;
end;

procedure TTelegramBot.SyncRecvMessage;
var
  Handled: Boolean;
begin
  Handled := False;
  if Assigned(FOnMessage) then FOnMessage(@FSync, Handled);
  if not Handled then begin
    if Assigned(FOnUnhandledMessage)
      then FOnUnhandledMessage(@FSync)
      else if Assigned(FAPI) then
        FAPI.sendMessage(FSync.Chat, sDefUnhandledMessage);
  end;
end;

procedure TTelegramBot.RecvMessages;
  procedure DoRecvMessage(AMessage: PTelegramBotMessages);
  begin
    with AMessage^ do begin
      if Assigned(Next) then DoRecvMessage(Next);
      Next  := nil;
      FSync := AMessage^;
      Synchronize(SyncRecvMessage);
      DisposeMessage(AMessage);
    end;
  end;

var
  Data: PTelegramBotMessages;
begin
  Data  := FRecv;
  FRecv := nil;
  if not Assigned(Data) then Exit;
  DoRecvMessage(Data);
end;

procedure TTelegramBot.SendMessages;
  procedure DoSendMessage(AMessage: PTelegramBotMessages);
  var
    aButtons: PTelegramBotMessageDataBtns;
    aReplyMarkup: TReplyMarkup;
    aText: String;
  begin
    with AMessage^ do begin
      if Assigned(Next) then DoSendMessage(Next);
      aReplyMarkup := nil;
      if GetButtonsCount(AMessage, aButtons) > 0 then begin
        aReplyMarkup := TReplyMarkup.Create;
        aReplyMarkup.OneTimeKeyboard := True;
        aReplyMarkup.ReplyKeyboardMarkup := TKeybordButtonArray.Create;
        for aText in aButtons^.Buttons do
          aReplyMarkup.ReplyKeyboardMarkup.Add.AddButton(aText);
      end;
      FAPI.sendMessage(Chat, Text, TParseMode(pmDefault), False,
                       aReplyMarkup, QuoteMessageId(AMessage));
      DisposeMessage(AMessage);
    end;
  end;

var
  Send: PTelegramBotMessages;
begin
  Send  := FSend;
  FSend := nil;
  if not Assigned(Send) then Exit;
  DoSendMessage(Send);
end;

procedure TTelegramBot.SendResponse(const AMessage: TTelegramBotMessage;
  const AText: String; AMsgQuote: LongInt; AParser: TTelegramBotParseMode;
  AButtons: array of const; ADefBtn: LongInt);
var
  AMsg: PTelegramBotMessages;
begin
  AMsg := NewMessage(PTelegramBotMessages(AMessage)^.Chat, AText);
  if Length(AButtons) > 0 then AddMessageButtons(AMsg, ADefBtn, AButtons);
  if AMsgQuote <> 0 then AddMessageQuote(AMsg, AMsgQuote);
  if AParser <> pmDefault then AddMessageMode(AMsg, AParser);
  AMsg^.Next := FNew;
  FNew := AMsg;
  Synchronize(SyncSendMessage);
end;

procedure TTelegramBot.SendResponse(const AMessage: TTelegramBotMessage;
  const AText: String; AMsgQuote: LongInt; AParser: TTelegramBotParseMode;
  AButtons: TStringList; ADefBtn: LongInt);
var
  AMsg: PTelegramBotMessages;
begin
  AMsg := NewMessage(PTelegramBotMessages(AMessage)^.Chat, AText);
  if Assigned(AButtons) and (AButtons.Count > 0) then
    AddMessageButtons(AMsg, ADefBtn, AButtons);
  if AMsgQuote <> 0 then AddMessageQuote(AMsg, AMsgQuote);
  if AParser <> pmDefault then AddMessageMode(AMsg, AParser);
  AMsg^.Next := FNew;
  FNew := AMsg;
  Synchronize(SyncSendMessage);
end;

function FindBlockByTypeAndIndex(const AMessage: TTelegramBotMessage;
  AType: TTelegramBotMessageType = tbmtVoid;
  AIndex: Cardinal = 0): PTelegramBotMessageData;
begin
  Result := nil;
  if not Assigned(AMessage) then Exit;
  Result := PTelegramBotMessages(AMessage)^.More;
  while Assigned(Result) do with Result^ do begin
    if (What = AType) and (AIndex = 0) then break;
    Result := Next;
  end;
end;

function GetMessageText(const AMessage: TTelegramBotMessage): String;
begin
  if Assigned(AMessage)
    then Result := PTelegramBotMessages(AMessage)^.Text
    else Result := EmptyStr;
end;

function QuoteMessageId(const AMessage: TTelegramBotMessage): LongInt;
var
  Data: PTelegramBotMessageData;
begin
  Result := 0;
  Data := FindBlockByTypeAndIndex(AMessage, tbmtQuote);
  if Assigned(Data) then
    Result := PTelegramBotMessageDataQuote(Data)^.MsgId;
end;

function IsMessageFromBot(const AMessage: TTelegramBotMessage): Boolean;
var
  Data: PTelegramBotMessageData;
begin
  Result := False;
  Data := FindBlockByTypeAndIndex(AMessage, tbmtUser);
  if Assigned(Data) then
    Result := PTelegramBotMessageDataUser(Data)^.isBot;
end;

function GetMessageUsername(const AMessage: TTelegramBotMessage): String;
var
  Data: PTelegramBotMessageData;
begin
  Result := sDefaultUsername;
  Data := FindBlockByTypeAndIndex(AMessage, tbmtUser);
  if Assigned(AMessage) then
    Result := PTelegramBotMessageDataUser(Data)^.User;
end;

function GetMessageUserFirstname(const AMessage: TTelegramBotMessage): String;
var
  Data: PTelegramBotMessageData;
begin
  Result := sDefaultUsername;
  Data := FindBlockByTypeAndIndex(AMessage, tbmtUser);
  if Assigned(AMessage) then
    Result := PTelegramBotMessageDataUser(Data)^.Name;
end;

function GetMessageUserLastname(const AMessage: TTelegramBotMessage): String;
var
  Data: PTelegramBotMessageData;
begin
  Result := sDefaultUsername;
  Data := FindBlockByTypeAndIndex(AMessage, tbmtUser);
  if Assigned(AMessage) then
    Result := PTelegramBotMessageDataUser(Data)^.Last;
end;

function GetMessageDefButton(const AMessage: TTelegramBotMessage): String;
var
  Data: PTelegramBotMessageData;
begin
  Result := EmptyStr;
  Data := FindBlockByTypeAndIndex(AMessage, tbmtButtons);
  if Assigned(AMessage) then with PTelegramBotMessageDataBtns(Data)^ do
    if (Low(Buttons) <= DefButn) and (DefButn <= High(Buttons)) then
      Result := Buttons[DefButn];
end;

function GetMessageButton(const AMessage: TTelegramBotMessage;
  AIndex: LongInt): String;
var
  Data: PTelegramBotMessageData;
begin
  Result := EmptyStr;
  Data := FindBlockByTypeAndIndex(AMessage, tbmtButtons);
  if Assigned(AMessage) then with PTelegramBotMessageDataBtns(Data)^ do
    if (Low(Buttons) <= AIndex) and (AIndex <= High(Buttons)) then
      Result := Buttons[AIndex];
end;

function GetMessageFilesCount(const AMessage: TTelegramBotMessage): LongInt;
begin
  if Assigned(AMessage)
    then Result := PTelegramBotMessages(AMessage)^.Count
    else Result := 0;
end;

function GetMessageFileByIndex(const AMessage: TTelegramBotMessage;
  AIndex: LongInt): TTelegramBotMessageFile;
begin
  Result := FindBlockByTypeAndIndex(AMessage, tbmtFile, AIndex);
end;

const
  fmtText = '%d: %s' + #13 + #10 + 'GET %s';

function DownloadMessageFile(AFileData: TStream;
  const AFile: TTelegramBotMessageFile): String;
begin
  Result := '';
  if not Assigned(AFile) then Exit;
  AFileData.Seek(0, soBeginning);
  with TFPHTTPClient.Create(nil) do begin
    Result := PTelegramBotMessageDataFile(AFile)^.URL;
    Get(Result, AFileData);
    Result := Format(fmtText, [ResponseStatusCode, ResponseStatusText, Result]);
    Free;
  end;
end;

function DownloadFileFromMessage(AFileData: TStream;
  const AMessage: TTelegramBotMessage; AIndex: LongInt): String;
var
  AFile: TTelegramBotMessageFile;
begin
  Result := '';
  AFile := FindBlockByTypeAndIndex(AMessage, tbmtFile, AIndex);
  if not Assigned(AFile) then Exit;
  AFileData.Seek(0, soBeginning);
  with TFPHTTPClient.Create(nil) do begin
    Result := PTelegramBotMessageDataFile(AFile)^.URL;
    Get(Result, AFileData);
    Result := Format(fmtText, [ResponseStatusCode, ResponseStatusText, Result]);
    Free;
  end;
end;

function ReplaceWildcard(const AFormat: String; AMessage: PTelegramBotMessages
  ): String;
var
  sTemp: String;
begin
  Result   := UTF8StringReplace(AFormat, '%text%', AMessage^.Text,
                                [rfReplaceAll, rfIgnoreCase]);
  sTemp    := GetMessageUserFirstname(AMessage);
  Result   := UTF8StringReplace(Result,  '%firstname%', sTemp,
                                [rfReplaceAll, rfIgnoreCase]);
  if Length(sTemp) > 0 then
    Result := UTF8StringReplace(Result,  '%fname%', UTF8Copy(sTemp, 1, 1),
                                [rfReplaceAll, rfIgnoreCase]);
  sTemp    := GetMessageUserLastname(AMessage);
  Result   := UTF8StringReplace(Result,  '%lastname%', sTemp,
                                [rfReplaceAll, rfIgnoreCase]);
  if Length(sTemp) > 0 then
    Result := UTF8StringReplace(Result,  '%lname%', UTF8Copy(sTemp, 1, 1),
                                [rfReplaceAll, rfIgnoreCase]);
  Result   := UTF8StringReplace(Result,  '%username%', GetMessageUsername(AMessage),
                                [rfReplaceAll, rfIgnoreCase]);
  Result   := UTF8StringReplace(Result,  '%isbot%',
                                BoolToStr(IsMessageFromBot(AMessage), True),
                                [rfReplaceAll, rfIgnoreCase]);
end;

end.

