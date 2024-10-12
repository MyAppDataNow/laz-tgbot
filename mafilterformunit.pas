unit maFilterFormUnit;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus;

const
  sListViewFilterFileName = 'MailAgentFilter.txt';

type

  { TmaFilterForm }

  TmaFilterForm = class(TForm)
    lvFilter: TListView;
    pmListView: TPopupMenu;
    pmListViewItem: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvFilterSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private

  public

  end;

var
  maFilterForm: TmaFilterForm;

implementation

{$R *.lfm}

{ TmaFilterForm }

procedure TmaFilterForm.FormCreate(Sender: TObject);
var
  S: TStringList;
  i: LongInt;
begin
  if FileExists(sListViewFilterFileName) then begin
    S := TStringList.Create;
    S.LoadFromFile(sListViewFilterFileName);
    for i := 0 to S.Count - 1 do
      with lvFilter.Items.Add do
        Caption := S[i];
    FreeAndNil(S);
  end;
end;

procedure TmaFilterForm.FormDestroy(Sender: TObject);
var
  S: TStringList;
  i: LongInt;
begin
  S := TStringList.Create;
  for i := 0 to lvFilter.Items.Count - 1 do
    S.Add(lvFilter.Items[i].Caption);
  S.SaveToFile(sListViewFilterFileName);
  FreeAndNil(S);
end;

procedure TmaFilterForm.lvFilterSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if lvFilter.SelCount = 0 then PopupMenu := pmListView
                           else PopupMenu := pmListViewItem;
end;

end.

