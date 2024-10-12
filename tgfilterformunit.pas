unit tgFilterFormUnit;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  TelegramBotThreadUnit, LazUTF8, tgFilterFormTypes;

const
  Eoln  = #13 + #10;
  sTreeViewFilterFileName = 'TelegramBotFilter.txt';

  sCaptionMUAdd  = 'Добавление пользователя';
  sPromptMUAdd   = 'Введите никнейм';
  sDefaultMUAdd  = '<без имени>';

  sCaptionMUEdit = 'Редактирование имени';
  sPromptMUEdit  = 'Введите новый никнейм';

  sCaptionMCAdd  = 'Добавление команды';
  sPromptMCAdd   = 'Введите команду';
  sDefaultMCAdd  = '/help';

  sCaptionMCEdit = 'Редактирование команды';
  sPromptMCEdit  = 'Введите новую команду';

  sCaptionMOAdd  = 'Добавление опции для команды';
  sPromptMOAdd   = 'Введите опцию';
  sDefaultMOAdd  = '<empty>';

  sCaptionMOEdit = 'Редактирование опций';
  sPromptMOEdit  = 'Введите новую строку опции';

  sCaptionMAAdd  = 'Добавление ответа';
  sPromptMAAdd   = 'Введите ответ на команду';
  sDefaultMAAdd  = '<empty>';

  sCaptionMAEdit = 'Редактирование ответа';
  sPromptMAEdit  = 'Введите ответ';

  sCaptionMBAdd  = 'Добавление кнопки к ответу';
  sPromptMBAdd   = 'Введите текст кнопки';
  sDefaultMBAdd  = '<empty>';

  sCaptionMBEdit = 'Редактирование кнопки';
  sPromptMBEdit  = 'Введите новый текст кнопки';

  sHelpWildcard  = 'можно использовать шаблоны подстановки:' + Eoln
        + '  %cmnd% - команда;' + Eoln
        + '  %args% - параметры команды;' + Eoln
        + '  %text% - полный текст сообщения от пользователя;' + Eoln
        + '  %firstname% - Имя пользователя телеграмм;' + Eoln
        + '  %fname% - Первая буква имени пользователя телеграмм;' + Eoln
        + '  %lastname% - Фамилия пользователя телеграмм;' + Eoln
        + '  %lname% - Первая буква фамилии пользователя телеграмм;' + Eoln
        + '  %username% - Никнейм пользователя телеграмм;' + Eoln
        + '  %isbot% - Сообщение написал другой бот (True или False).' + Eoln;

  sHelpAnswer    = 'При добавлении ответа ' + sHelpWildcard + Eoln
        + ' К ответному сообщению можно добавить набор из кнопок выбора' + Eoln
        + 'вариантов ответа.' + Eoln
        + Eoln
        + ' Если ответ начинается с "mailto: <почта>", тогда на эту почту' + Eoln
        + 'будет отправлено письмо. Уточняющие параметры письма задаются' + Eoln
        + 'при добавлении кнопок в ответ.';

  sHelpButtons   = 'При добавлении кнопок ' + sHelpWildcard + Eoln
        + ' При использовании ответа, отправляемого на почту,' + Eoln
        + 'вместо кнопок можно добавлять параметры сообщения,' + Eoln
        + 'где тоже можно использовать подстановки:' + Eoln
        + '  subject: <Заголовок письма>' + Eoln
        + '  message: <Текст письма>' + Eoln
        + '  to: <дополнительный адрес почты>';

type

  { TtgFilterForm }

  TtgFilterForm = class(TForm)
    pmmbAdd: TMenuItem;
    pmmbEdit: TMenuItem;
    pmmbRem: TMenuItem;
    Separator6: TMenuItem;
    pmmbHelp: TMenuItem;
    Separator5: TMenuItem;
    pmmaNew: TMenuItem;
    pmManageButtons: TPopupMenu;
    Separator4: TMenuItem;
    pmmaHelp: TMenuItem;
    pmmaAdd: TMenuItem;
    pmmaEdit: TMenuItem;
    pmmaRem: TMenuItem;
    pmmoAdd: TMenuItem;
    pmmoEdit: TMenuItem;
    pmmoRem: TMenuItem;
    pmManageAnswers: TPopupMenu;
    Separator3: TMenuItem;
    pmmoNew: TMenuItem;
    pmmcAdd: TMenuItem;
    pmmcEdit: TMenuItem;
    pmmcRem: TMenuItem;
    Separator2: TMenuItem;
    pmmcNew: TMenuItem;
    pmmuAdd: TMenuItem;
    pmmuEdit: TMenuItem;
    pmmuRem: TMenuItem;
    Separator1: TMenuItem;
    pmmuNew: TMenuItem;
    pmManageUsers: TPopupMenu;
    pmManageCommands: TPopupMenu;
    pmManageOptions: TPopupMenu;
    tvFilter: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pmmaAddClick(Sender: TObject);
    procedure pmmaEditClick(Sender: TObject);
    procedure pmmaHelpClick(Sender: TObject);
    procedure pmmaNewClick(Sender: TObject);
    procedure pmmaRemClick(Sender: TObject);
    procedure pmmbAddClick(Sender: TObject);
    procedure pmmbEditClick(Sender: TObject);
    procedure pmmbHelpClick(Sender: TObject);
    procedure pmmbRemClick(Sender: TObject);
    procedure pmmcAddClick(Sender: TObject);
    procedure pmmcEditClick(Sender: TObject);
    procedure pmmcNewClick(Sender: TObject);
    procedure pmmcRemClick(Sender: TObject);
    procedure pmmoAddClick(Sender: TObject);
    procedure pmmoEditClick(Sender: TObject);
    procedure pmmoNewClick(Sender: TObject);
    procedure pmmoRemClick(Sender: TObject);
    procedure pmmuAddClick(Sender: TObject);
    procedure pmmuEditClick(Sender: TObject);
    procedure pmmuNewClick(Sender: TObject);
    procedure pmmuRemClick(Sender: TObject);
    procedure tvFilterSelectionChanged(Sender: TObject);
  private
    function IsTreeViewReady(ALevel: LongInt): Boolean;
    procedure AddTreeNode(const sCaption, sPrompt, sDefault, sChecked: String;
                              aLevel: LongInt; aRoot: TTreeNode);
    procedure AddParentNode(const sCaption, sPrompt, sDefault, sChecked: String;
                              aLevel: LongInt; aRoot: TTreeNode);
    procedure EditSelectedNode(const sCaption, sPrompt: String; aLevel: LongInt);
  public
    function IsAcceptedCommand(const AMessage: TTelegramBotMessage;
                               out aScripts: PtvCommand): Boolean;
  end;

var
  tgFilterForm: TtgFilterForm;

implementation

{$R *.lfm}

function IsCorrectName(aRoot: TTreeNode; const sName, sDefault: String): Boolean;
var
  n: TTreeNode;
begin
  if Assigned(aRoot) then n := aRoot.GetFirstChild else n := nil;
  Result := (trim(sName) <> EmptyStr) and (sName <> sDefaultMUAdd);
  while Result and Assigned(n) do begin
    Result := Result and (sName <> n.Text);
    n := n.GetNextSibling;
  end;
end;

{ TtgFilterForm events }

procedure TtgFilterForm.FormCreate(Sender: TObject);
begin
  if FileExists(sTreeViewFilterFileName) then
    tvFilter.LoadFromFile(sTreeViewFilterFileName);
end;

procedure TtgFilterForm.FormDestroy(Sender: TObject);
begin
  tvFilter.SaveToFile(sTreeViewFilterFileName);
end;

procedure TtgFilterForm.tvFilterSelectionChanged(Sender: TObject);
begin
  tvFilter.PopupMenu := pmManageUsers;
  pmmuEdit.Enabled   := False;
  pmmuRem.Enabled    := False;
  pmmuNew.Enabled    := False;
  if tvFilter.SelectionCount > 0 then begin
    if tvFilter.Selected.Level = 1 then begin
      pmmuEdit.Enabled   := True;
      pmmuRem.Enabled    := True;
      pmmuNew.Enabled    := True;
    end;
    if tvFilter.Selected.Level = 2 then tvFilter.PopupMenu := pmManageCommands;
    if tvFilter.Selected.Level = 3 then tvFilter.PopupMenu := pmManageOptions;
    if tvFilter.Selected.Level = 4 then tvFilter.PopupMenu := pmManageAnswers;
    if tvFilter.Selected.Level = 5 then tvFilter.PopupMenu := pmManageButtons;
  end;
end;

{ TtgFilterForm pmManageUsers }

procedure TtgFilterForm.pmmuAddClick(Sender: TObject);
begin
  AddTreeNode(sCaptionMUAdd, sPromptMUAdd, sDefaultMUAdd, sDefaultMUAdd, -1,
              tvFilter.Items.GetFirstNode);
end;

procedure TtgFilterForm.pmmuEditClick(Sender: TObject);
begin
  EditSelectedNode(sCaptionMUEdit, sPromptMUEdit, 1);
end;

procedure TtgFilterForm.pmmuRemClick(Sender: TObject);
begin
  if not IsTreeViewReady(1) then Exit;
  tvFilter.Selected.Delete;
end;

procedure TtgFilterForm.pmmuNewClick(Sender: TObject);
begin
  AddTreeNode(sCaptionMCAdd, sPromptMCAdd, sDefaultMCAdd, sDefaultMCAdd, 1,
                tvFilter.Selected);
end;

{ TtgFilterForm pmManageCommands }

procedure TtgFilterForm.pmmcAddClick(Sender: TObject);
begin
  AddParentNode(sCaptionMCAdd, sPromptMCAdd, sDefaultMCAdd, sDefaultMCAdd, 2,
                tvFilter.Selected);
end;

procedure TtgFilterForm.pmmcEditClick(Sender: TObject);
begin
  EditSelectedNode(sCaptionMCEdit, sPromptMCEdit, 2);
end;

procedure TtgFilterForm.pmmcRemClick(Sender: TObject);
begin
  if not IsTreeViewReady(2) then Exit;
  tvFilter.Selected.Delete;
end;

procedure TtgFilterForm.pmmcNewClick(Sender: TObject);
begin
  AddTreeNode(sCaptionMOAdd, sPromptMOAdd, sDefaultMOAdd, EmptyStr, 2,
                tvFilter.Selected);
end;

{ TtgFilterForm pmManageOptions }

procedure TtgFilterForm.pmmoAddClick(Sender: TObject);
begin
  AddParentNode(sCaptionMOAdd, sPromptMOAdd, sDefaultMOAdd, EmptyStr, 3,
                tvFilter.Selected);
end;

procedure TtgFilterForm.pmmoEditClick(Sender: TObject);
begin
  EditSelectedNode(sCaptionMOEdit, sPromptMOEdit, 3);
end;

procedure TtgFilterForm.pmmoRemClick(Sender: TObject);
begin
  if not IsTreeViewReady(3) then Exit;
  tvFilter.Selected.Delete;
end;

procedure TtgFilterForm.pmmoNewClick(Sender: TObject);
begin
  AddTreeNode(sCaptionMAAdd, sPromptMAAdd, sDefaultMAAdd, sDefaultMAAdd, 3,
                tvFilter.Selected);
end;

{ TtgFilterForm pmManageAnswers }

procedure TtgFilterForm.pmmaAddClick(Sender: TObject);
begin
  AddParentNode(sCaptionMAAdd, sPromptMAAdd, sDefaultMAAdd, sDefaultMAAdd, 4,
                tvFilter.Selected);
end;

procedure TtgFilterForm.pmmaEditClick(Sender: TObject);
begin
  EditSelectedNode(sCaptionMAEdit, sPromptMAEdit, 4);
end;

procedure TtgFilterForm.pmmaRemClick(Sender: TObject);
begin
  if not IsTreeViewReady(4) then Exit;
  tvFilter.Selected.Delete;
end;

procedure TtgFilterForm.pmmaNewClick(Sender: TObject);
begin
  AddTreeNode(sCaptionMBAdd, sPromptMBAdd, sDefaultMBAdd, sDefaultMBAdd, 4,
              tvFilter.Selected);
end;

procedure TtgFilterForm.pmmaHelpClick(Sender: TObject);
begin
  ShowMessage(sHelpAnswer);
end;

{ TtgFilterForm pmManageButtons }

procedure TtgFilterForm.pmmbAddClick(Sender: TObject);
begin
  AddParentNode(sCaptionMBAdd, sPromptMBAdd, sDefaultMBAdd, sDefaultMBAdd, 5,
                tvFilter.Selected);
end;

procedure TtgFilterForm.pmmbEditClick(Sender: TObject);
begin
  EditSelectedNode(sCaptionMBEdit, sPromptMBEdit, 5);
end;

procedure TtgFilterForm.pmmbRemClick(Sender: TObject);
begin
  if not IsTreeViewReady(5) then Exit;
  tvFilter.Selected.Delete;
end;

procedure TtgFilterForm.pmmbHelpClick(Sender: TObject);
begin
  ShowMessage(sHelpButtons);
end;

{ TtgFilterForm tvFilter private }

function TtgFilterForm.IsTreeViewReady(ALevel: LongInt): Boolean;
begin
  Result := ALevel < 0;
  if tvFilter.SelectionCount = 0 then Exit;
  Result := Result or (tvFilter.Selected.Level = ALevel);
end;

procedure TtgFilterForm.AddTreeNode(const sCaption, sPrompt, sDefault,
  sChecked: String; aLevel: LongInt; aRoot: TTreeNode);
var
  sName: String;
begin
  if not IsTreeViewReady(aLevel) then Exit;
  sName := InputBox(sCaption, sPrompt, sDefault);
  if IsCorrectName(aRoot, sName, sChecked) then begin
    tvFilter.Items.AddChild(aRoot, sName);
    if Assigned(aRoot) then aRoot.Expand(False);
  end;
end;

procedure TtgFilterForm.AddParentNode(const sCaption, sPrompt, sDefault,
  sChecked: String; aLevel: LongInt; aRoot: TTreeNode);
begin
  if Assigned(aRoot) then
    AddTreeNode(sCaption, sPrompt, sDefault, sChecked, aLevel, aRoot.Parent);
end;

procedure TtgFilterForm.EditSelectedNode(const sCaption, sPrompt: String;
  aLevel: LongInt);
var
  sName: String;
begin
  if not IsTreeViewReady(ALevel) and not Assigned(tvFilter.Selected) then Exit;
  sName := InputBox(sCaption, sPrompt, tvFilter.Selected.Text);
  if IsCorrectName(tvFilter.Selected.Parent, sName, tvFilter.Selected.Text) then
    tvFilter.Selected.Text := sName;
end;

{ TtgFilterForm tvFilter public }

function TtgFilterForm.IsAcceptedCommand(const AMessage: TTelegramBotMessage;
                                         out aScripts: PtvCommand): Boolean;
var
  Answer: TStringList;
  aNode, aItem: TTreeNode;
  aBody, aText, aCmnd, aArgs, aUser: String;
  lBody, Index: LongInt;
  aScript: PtvCommand;
  aScriptArg: PtvCommandArgs;
  aScriptNew: TtvCommand;
begin
  Result   := False;                     { Вот функция обработки этих списков фильтрации }
  aScripts := nil;
  aNode := tvFilter.Items.GetFirstNode; // Получаем первый элемент в тривью "Фильтр для бота тг"
  if not Assigned(aNode) then Exit;     // Элемент найден?
  aNode := aNode.GetFirstChild;         // Цикл перечисления всех элементов дочернего списка: в данном случае одного: Имя пользователя ТГ соотвественно это все списки
  aBody := GetMessageText(AMessage);    // Получение из команды - текста присланного в ТГ
  aUser := GetMessageUsername(AMessage);// Получение никнейма пользователя приславшего текст в ТГ
  lBody := UTF8Length(aBody);           // Длина команды, символов
  while Assigned(aNode) do begin        // Пока в списке есть элементы
    if aNode.Text = aUser then begin    // Никнейм совпал с элементом списка
      aNode := aNode.GetFirstChild;     // Получение первого элемента дочернего списка - т.е Команда вводимая в ТГ
      while Assigned(aNode) do begin    // Пока в списке есть элементы
        if UTF8Pos(aNode.Text, aBody) = 1 then begin // Проверяем, что текст команды строго в начале сообщения от пользователя
          aCmnd := aNode.Text;          // Сохраняем текст команды
          aText := UTF8Trim(UTF8Copy(aBody, UTF8Length(aNode.Text) + 1,
                            lBody - UTF8Length(aNode.Text))); // Удаляем текст команды из строки и оставляем только параметры
          aNode := aNode.GetFirstChild; // Получение первого элемента дочернего списка - т.е. Параметры команды вводимые в ТГ
          while Assigned(aNode) do begin// Пока в списке есть элементы
            if ((aNode.Text = sDefaultMOAdd) and (aText = EmptyStr)) // Параметров нету
            or (aNode.Text = aText) then begin // или они совпали
              if (aNode.Text = sDefaultMOAdd) and (aText = EmptyStr)
                then aArgs := EmptyStr else aArgs := aNode.Text; // Сохраняем текст параметров
              aNode    := aNode.GetFirstChild; // Получение первого элемента дочернего списка - т.е. Имя процедуры в программе для исполнения: пока answer и mailto
              aScript         := @aScriptNew;
              while Assigned(aNode) do begin
                New(aScript^.next);
                aScript       := aScript^.next;
                aScript^.next := nil;
                aScript^.args := nil;
                aText         := ReplaceWildcard(aNode.Text, AMessage); // Замена в тексте автоподстановок
                aText         := UTF8StringReplace(aText, '%cmnd%', aCmnd, // Дополнительные автоподстановки
                                            [rfReplaceAll, rfIgnoreCase]);
                aScript^.text := UTF8StringReplace(aText, '%args%', aArgs,
                                            [rfReplaceAll, rfIgnoreCase]);
                aItem         := aNode.GetFirstChild;
                aNode         := aNode.GetNextSibling;
                aScriptArg    := PtvCommandArgs(@aScript^.args);
                while Assigned(aItem) do begin
                  New(aScriptArg^.next);
                  aScriptArg  := aScriptArg^.next;
                  aScriptArg^.next := nil;
                  aText       := UTF8StringReplace(aItem.Text, '%cmnd%', aCmnd, // Замена дополнительных автоподстановок в параметрах
                                            [rfReplaceAll, rfIgnoreCase]);
                  aScriptArg^.text := UTF8StringReplace(aText, '%args%', aArgs,
                                            [rfReplaceAll, rfIgnoreCase]);
                  aItem       := aItem.GetNextSibling;
                end;
              end;
              aScripts        := aScriptNew.next;
              break;
            end;
            aNode := aNode.GetNextSibling; // Переходим к следующему элементы списка параметров вводимой в ТГ команды
          end;
          Result := True; // Фильтр успешно пройден
          break;
        end;
        aNode := aNode.GetNextSibling; // Переходим к следующему элементы списка команд вводимых в ТГ
      end;
      break;
    end;
    aNode := aNode.GetNextSibling; // Переходим к следующему элементы списка пользователей
  end;
end;

end.

