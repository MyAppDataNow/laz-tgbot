unit tgFilterFormTypes;

{$mode Delphi}

interface

uses
  Classes, SysUtils;

type
  PtvCommandArgs = ^TtvCommandArgs;
  TtvCommandArgs = packed record
    next: PtvCommandArgs;
    text: String;
  end;

  PtvCommand = ^TtvCommand;
  TtvCommand = packed record
    next: PtvCommand;
    text: String;
    args: PtvCommandArgs;
  end;

implementation

end.

