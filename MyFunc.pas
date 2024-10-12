unit MyFunc;

interface

uses
  SysUtils, Dialogs, Windows, Masks, Registry, uSMBIOS, ActiveX, ComObj,
  Classes, Variants, Forms;

function GetComputerNetName: string;
function ServiceRunning(sMachine, sService: PChar): Boolean;
function GetDeviceInfo: String;

implementation

function GetComputerNetName: string;
var
  buffer: array[0..255] of char;
  size: dword;
begin
  size := 256;
  if GetComputerName(buffer, size) then
    Result := buffer
  else
    Result := ''
end;

function ServiceGetStatus(sMachine, sService: PChar): DWORD;
var
  SCManHandle, SvcHandle: SC_Handle;
  SS: TServiceStatus;
  dwStat: DWORD;
begin
  dwStat := 0;
  // Open service manager handle.
  SCManHandle := OpenSCManager(sMachine, nil, SC_MANAGER_CONNECT);
  if (SCManHandle > 0) then
  begin
    SvcHandle := OpenService(SCManHandle, sService, SERVICE_QUERY_STATUS);
    // if Service installed
    if (SvcHandle > 0) then
    begin
      // SS structure holds the service status (TServiceStatus);
      if (QueryServiceStatus(SvcHandle, SS)) then
        dwStat := ss.dwCurrentState;
      CloseServiceHandle(SvcHandle);
    end;
    CloseServiceHandle(SCManHandle);
  end;
  Result := dwStat;
end;

function ServiceRunning(sMachine, sService: PChar): Boolean;
begin
  Result := SERVICE_RUNNING = ServiceGetStatus(sMachine, sService);
end;


procedure RunProgram(cmdStr:string; Wait:Boolean; ShowWindow:Word);
var si:TStartupInfo;
  pi:TProcessInformation;
  s:string;
begin
  FillChar(si, SizeOf(si), 0);
  si.cb := SizeOf(si);
  si.dwFlags:=STARTF_USESHOWWINDOW;
  si.wShowWindow := ShowWindow;
  s:=cmdStr;
  UniqueString(s);
  getlasterror;
  if not CreateProcess(nil, PChar(S), NIL, NIL, False, 0, NIL, NIL, si, pi) then
    showMessageFmt('Error %d. %s',[getlasterror,SysErrorMessage(getlasterror)]);

  if wait then
    WaitForSingleObject(pi.hProcess, INFINITE);
  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);
end;


function DownLoadALLFileFTP(Ftp_folder: String; out_Folder: String; UserName: String = 'anonymous';  Password: String = 'anonymous'): Boolean;
const
 ps1_FILENAME = 'PowerShell.ps1';
var
 CMD_list: TStringList;
begin

 try

      CMD_list := TStringList.Create;
      CMD_list.Clear;
      CMD_list.add('chcp 1251');
      CMD_list.add('del ' + GetTempDir + ps1_FILENAME);

      CMD_list.add('New-Item -ItemType Directory -Force -Path "' + out_Folder + '"');
      CMD_list.add('');
      CMD_list.add('function DownloadFtpDirectory($url, $credentials, $localPath)');
      CMD_list.add('{');
      CMD_list.add('    $listRequest = [Net.WebRequest]::Create($url)');
      CMD_list.add('    $listRequest.Method =');
      CMD_list.add('        [System.Net.WebRequestMethods+Ftp]::ListDirectoryDetails');
      CMD_list.add('    $listRequest.Credentials = $credentials');
      CMD_list.add('');
      CMD_list.add('    $lines = New-Object System.Collections.ArrayList');
      CMD_list.add('');
      CMD_list.add('    $listResponse = $listRequest.GetResponse()');
      CMD_list.add('    $listStream = $listResponse.GetResponseStream()');
      CMD_list.add('    $listReader = New-Object System.IO.StreamReader($listStream)');
      CMD_list.add('    while (!$listReader.EndOfStream)');
      CMD_list.add('    {');
      CMD_list.add('        $line = $listReader.ReadLine()');
      CMD_list.add('        $lines.Add($line) | Out-Null');
      CMD_list.add('    }');
      CMD_list.add('    $listReader.Dispose()');
      CMD_list.add('    $listStream.Dispose()');
      CMD_list.add('    $listResponse.Dispose()');
      CMD_list.add('');
      CMD_list.add('    foreach ($line in $lines)');
      CMD_list.add('    {');
      CMD_list.add('        $tokens = $line.Split(" ", 9, [StringSplitOptions]::RemoveEmptyEntries)');
      CMD_list.add('        $name = $tokens[8]');
      CMD_list.add('        $permissions = $tokens[0]');
      CMD_list.add('');
      CMD_list.add('        $localFilePath = Join-Path $localPath $name');
      CMD_list.add('        $fileUrl = ($url + $name)');
      CMD_list.add('');
      CMD_list.add('        if ($permissions[0] -eq ' + #39 + 'd' + #39 + ')');
      CMD_list.add('        {');
      CMD_list.add('            if (!(Test-Path $localFilePath -PathType container))');
      CMD_list.add('            {');
      CMD_list.add('                Write-Host "Creating directory $localFilePath"');
      CMD_list.add('                New-Item $localFilePath -Type directory | Out-Null');
      CMD_list.add('            }');
      CMD_list.add('');
      CMD_list.add('            DownloadFtpDirectory ($fileUrl + "/") $credentials $localFilePath');
      CMD_list.add('        }');
      CMD_list.add('        else');
      CMD_list.add('        {');
      CMD_list.add('            Write-Host "Downloading $fileUrl to $localFilePath"');
      CMD_list.add('');
      CMD_list.add('            $downloadRequest = [Net.WebRequest]::Create($fileUrl)');
      CMD_list.add('            $downloadRequest.Method =');
      CMD_list.add('                [System.Net.WebRequestMethods+Ftp]::DownloadFile');
      CMD_list.add('            $downloadRequest.Credentials = $credentials');
      CMD_list.add('');
      CMD_list.add('            $downloadResponse = $downloadRequest.GetResponse()');
      CMD_list.add('            $sourceStream = $downloadResponse.GetResponseStream()');
      CMD_list.add('            $targetStream = [System.IO.File]::Create($localFilePath)');
      CMD_list.add('            $buffer = New-Object byte[] 10240');
      CMD_list.add('            while (($read = $sourceStream.Read($buffer, 0, $buffer.Length)) -gt 0)');
      CMD_list.add('            {');
      CMD_list.add('                $targetStream.Write($buffer, 0, $read);');
      CMD_list.add('            }');
      CMD_list.add('            $targetStream.Dispose()');
      CMD_list.add('            $sourceStream.Dispose()');
      CMD_list.add('            $downloadResponse.Dispose()');
      CMD_list.add('        }');
      CMD_list.add('    }');
      CMD_list.add('}');








      CMD_list.add('$credentials = New-Object System.Net.NetworkCredential("' + UserName + '", "' + Password + '") ');
      CMD_list.add('$url = "' + 'ftp://sps-holding.ru' + Ftp_folder + '"');
      CMD_list.add('DownloadFtpDirectory $url $credentials "' + out_Folder + '"');



      CMD_list.SaveToFile(GetTempDir + ps1_FILENAME);
      CMD_list.Free;
      RunProgram('PowerShell.exe -ExecutionPolicy Bypass -File "' + GetTempDir + ps1_FILENAME + '"', True, SW_HIDE);

  except
  end;

end;


function DownLoadFolderFileFTP(Ftp_folder: String; fileExtension: String; out_Folder: String; UserName: String = 'anonymous';  Password: String = 'anonymous'): Boolean;
const
 ps1_FILENAME = 'PowerShell.ps1';
var
 CMD_list: TStringList;
begin

 try


      CMD_list := TStringList.Create;
      CMD_list.Clear;
      CMD_list.add('del ' + GetTempDir + ps1_FILENAME);
      CMD_list.add('');
      CMD_list.add('New-Item -ItemType Directory -Force -Path "' + out_Folder + '"');
      CMD_list.add('');
      CMD_list.add('    #FTP Server Information - SET VARIABLES');
      CMD_list.add('    $ftp = "ftp://sps-holding.ru"');
      CMD_list.add('    $user = ' + #39 + UserName + '' + #39);
      CMD_list.add('    $pass = ' + #39 + Password + '' + #39);
      CMD_list.add('    $folder = ' + #39 + Ftp_folder + '' + #39);
      CMD_list.add('    $target = "' + out_Folder + '"');
      CMD_list.add('');
      CMD_list.add('    #SET CREDENTIALS');
      CMD_list.add('    $credentials = new-object System.Net.NetworkCredential($user, $pass)');
      CMD_list.add('');
      CMD_list.add('    function Get-FtpDir ($url,$credentials) {');
      CMD_list.add('        $request = [Net.WebRequest]::Create($url)');
      CMD_list.add('        $request.Method = [System.Net.WebRequestMethods+FTP]::ListDirectory');
      CMD_list.add('        if ($credentials) { $request.Credentials = $credentials }');
      CMD_list.add('        $response = $request.GetResponse()');
      CMD_list.add('        $reader = New-Object IO.StreamReader $response.GetResponseStream() ');
      CMD_list.add('        $reader.ReadToEnd()');
      CMD_list.add('        $reader.Close()');
      CMD_list.add('        $response.Close()');
      CMD_list.add('    }');
      CMD_list.add('');
      CMD_list.add('    #SET FOLDER PATH');
      CMD_list.add('    $folderPath= $ftp + "/" + $folder + "/"');
      CMD_list.add('');
      CMD_list.add('    $Allfiles=Get-FTPDir -url $folderPath -credentials $credentials');
      CMD_list.add('    $files = ($Allfiles -split "`r`n")');
      CMD_list.add('');
      CMD_list.add('    $files ');
      CMD_list.add('');
      CMD_list.add('    $webclient = New-Object System.Net.WebClient ');
      CMD_list.add('    $webclient.Credentials = New-Object System.Net.NetworkCredential($user,$pass)');
      CMD_list.add('    $counter = 0');
      CMD_list.add('    foreach ($file in ($files | where {$_ -like "' + fileExtension + '"})){');
      CMD_list.add('        $source=$folderPath + $file  ');
      CMD_list.add('        $destination = $target + $file ');
      CMD_list.add('        $webclient.DownloadFile($source, $target+$file)');
      CMD_list.add('');
      CMD_list.add('        #PRINT FILE NAME AND COUNTER');
      CMD_list.add('        $counter++');
      CMD_list.add('        $counter');
      CMD_list.add('        $source');
      CMD_list.add('    }');

      CMD_list.SaveToFile(GetTempDir + ps1_FILENAME);
      CMD_list.Free;
      RunProgram('PowerShell.exe -ExecutionPolicy Bypass -File "' + GetTempDir + ps1_FILENAME + '"', True, SW_HIDE);

  except
  end;

end;



type
  PFileListEntry = ^TFileListEntry;
  TFileListEntry = packed record
    next: PFileListEntry;
    name: String;
  end;
  PFileResults = ^TFileResults;
  TFileResults = packed record
    head, next, tail: PFileListEntry;
  end;



const
  aForm1Caption = '(aDir.Count = %d, Result.Count = %d)';

function GetFindFilesMask(aResult: PFileResults): String;
begin
  Result      := aResult^.head^.name;
end;

function GetFirstFileName(aResult: PFileResults; var aFileName: String): Boolean;
begin
  aFileName     := '';
  Result        := False;
  if aResult^.head^.next = nil then Exit;
  aResult^.next := aResult^.head^.next;
  aFileName     := aResult^.next^.name;
  Result        := True;
end;

function GetNextFileName(aResult: PFileResults; var aFileName: String): Boolean;
begin
  aFileName     := '';
  Result        := False;
  if aResult^.next^.next = nil then Exit;
  aResult^.next := aResult^.next^.next;
  aFileName     := aResult^.next^.name;
  Result        := True;
end;

procedure FreeResults(aResult: PFileResults);
begin
  with aResult^ do while head <> nil do begin
    next := head;
    head := head^.next;
    Dispose(next);
  end;
end;

function FindFiles(const aMask: String; aRecursive: Boolean = True; aCount: Integer = 1000): TFileResults;
const
  PathDelimiter = '\';
  AnyFileName   = '*';
var
  Last, Head, Tail, Temp: PFileListEntry;
  Path, Curr: String;
  Name: TMask;
  sRec: TSearchRec;
begin
  New(Result.head);
  New(Head);
  Path              := ExpandFileName(aMask);
  Name              := TMask.Create(ExtractFileName(Path));
  Path              := ExtractFileDir(Path);
  Last              := Result.head;
  Last^.next        := nil;
  Last^.name        := aMask;
  Result.next       := Last;
  Head.next         := nil;
  if Path[Length(Path)] = PathDelimiter
    then Head^.name := ''
    else Head^.name := PathDelimiter;
  Tail := Head;
  while Head <> nil do begin
    Curr := Path + Head^.name;
    if (FindFirst(Curr + AnyFileName, faAnyFile, sRec) = 0) then repeat
      if sRec.Attr and faDirectory = 0 then begin
        if Name.Matches(sRec.Name) then begin
          New(Last^.next);
          Last       := Last^.next;
          Last^.next := nil;
          Last^.name := Curr + sRec.Name;
// ---------------------------------------
        end;
      end else if aRecursive and (sRec.Name <> '.') and (sRec.Name <> '..') then begin
        New(Tail^.next);
        Tail       := Tail^.next;
        Tail^.next := nil;
        Tail^.name := Head^.name + sRec.Name + PathDelimiter;
// ---------------------------------------
      end;
    until FindNext(sRec) <> 0;
//    FindClose(sRec);
    Temp := Head;
    Head := Temp^.next;
    Dispose(Temp);

    Application.ProcessMessages;
// ---------------------------------------
  end;
  FreeAndNil(Name);
  Result.tail := Last;
end;

procedure FindFilesToDel(const aPath, aMask: String; anExclude: TStringList; aRecursive: Boolean = True);
const
  PathDelimiter = '\';
  AnyFileName   = '*';
var
  Path, Name, Temp, UNam: String;
  List: TFileResults;
  Full, Rela, Fnam: TStringList;
  i: Integer;
  flag: Boolean;
  any: TStringList;
begin

  Path := aMask;
  if Path = '' then Path := AnyFileName;
  if aPath[Length(aPath)] = PathDelimiter
    then Path := aPath + Path
    else Path := aPath + PathDelimiter + Path;
  List := FindFiles(Path, aRecursive);
  Full := TStringList.Create;
  Rela := TStringList.Create;
  Fnam := TStringList.Create;
  any := TStringList.Create; // добавляем еще один список строк
  for i := 0 to anExclude.Count-1 do begin
    Name := anExclude[i];
    if (Length(Name) > 2) and (Name[Length(Name)] = '*') and (Name[Length(Name) - 1] = '\') then begin { еще один критерий
      отбора строк (заканчивающиеся на '\*').
      Такие строки буду исключать все файла в этих поддиректориях,
      но адреса надо обязательно указывать абсолютные }
      any.Add(ExtractFileDir(Name)); // добавляем их в новый список
      continue; // и пропускаем дальнейшие проверка
    end;
    if Length(anExclude[i]) > 1 then if Name[2] = ':' then begin
      Full.Add(Name);
      continue;
    end;
    if Pos('\', Name) > 0 then begin
      Rela.Add(Name);
    end else begin
      Fnam.Add(Name);
    end;
  end;
  Path := ExtractFilePath(ExpandFileName(aPath));
// ---------------------------------------
  if GetFirstFileName(@List, Name) then repeat
    Unam := UpperCase(Name);
    Flag := False; // сбрасываем флаг пропуска имени файла
    for i := 0 to any.Count - 1 do begin // цикл проверки в строках с исключениями
      if Pos(any[i], UNam) = 1 then begin // если начало строки совпало с исключением
        Flag := True; // устанавливаем флаг ...
        break;        // ... и прерываем вложенный цикл
      end;
    end;
    if Flag then continue; // при установленном флаге цикл проверки результатов тоже пропускает элемент списка
    Temp := ExtractFileName(Name);
    if Full.IndexOf(Name) >= 0 then Continue; // Условие поиска в списке полных путей
    if Fnam.IndexOf(Temp) >= 0 then Continue; // Условие поиска в списке имен (Хотя внутри метода цикл)
    if Rela.IndexOf(UpperCase(Copy(Name, Length(Path) + 1, Length(Name) - Length(Path)))) >= 0 then continue; // Условие поиска в списке относительных путей и имен

       DeleteFile(PChar(Name));


// ---------------------------------------
    Application.ProcessMessages;
// ---------------------------------------
  until GetNextFileName(@List, Name) = False;
  FreeResults(@List);
// ---------------------------------------
  FreeAndNil(Fnam);
  FreeAndNil(Rela);
  FreeAndNil(Full);
end;


{
procedure ThreadDelFilesProcesses.Execute;
var
  List: TStringList;
begin

   try


      List := TStringList.Create;

      List.Add(UpperCase('C:\Base\*'));

      FindFilesToDel('D:\RK7\win\', 'fpSHTR*.log', List, True);
      FindFilesToDel('D:\RK7\win\', 'SB_PILOT_1*', List, True);
      FindFilesToDel('D:\RK7\win\', 'UniFR1.tx*', List, True);
      FindFilesToDel('D:\UCS\OstLauncher\screen\', '*.*', List, True);
      FindFilesToDel('C:\RK7\win\BACKUPS\', '*.*', List, True);
      FindFilesToDel('D:\RK7\win\MIDBASE_db\', '*.*', List, True);
      FindFilesToDel('D:\RK7\win\ShiftSender\logs\', '*.*', List, True);
      FindFilesToDel('D:\RK7\Rk7ReportServerAgent\Logs\', '*.*', List, True);


      FreeAndNil(List);

       except

   end;

end;
}


function GetMotherBoardSerial: string;
var
  objWMIService: OLEVariant;
  colItems: OLEVariant;
  colItem: OLEVariant;
  oEnum: IEnumvariant;
  iValue: Longword;
  function GetWMIObject(const objectName: string): IDispatch;
  var
    chEaten: Pulong;
    BindCtx: IBindCtx;
    Moniker: IMoniker;
  begin
    OleCheck(CreateBindCtx(0, bindCtx));
    OleCheck(MkParseDisplayName(BindCtx, StringToOleStr(objectName),
      chEaten, Moniker));
    OleCheck(Moniker.BindToObject(BindCtx, nil, IDispatch, Result));
  end;
begin
  Result := '';
  objWMIService := GetWMIObject('winmgmts:\\localhost\root\cimv2');
  colItems :=
    objWMIService.ExecQuery('SELECT Product FROM Win32_BaseBoard', 'WQL', 0);
  oEnum := IUnknown(colItems._NewEnum) as IEnumVariant;
  if oEnum.Next(1, colItem,
    iValue) = 0 then
    Result := VarToStr(colItem.Product);
end;


function FormatByteSize(A:int64): string;
var
  A1, A2, A3: double;
begin
  A1 := A / 1024;
  A2 := A1 / 1024;
  A3 := A2 / 1024;
  if A1 < 1 then Result := floattostrf(A, ffNumber, 15, 0) + ' байта'
  else if A1 < 10 then Result := floattostrf(A1, ffNumber, 15, 2) + ' КБ'
  else if A1 < 100 then Result := floattostrf(A1, ffNumber, 15, 1) + ' КБ'
  else if A2 < 1 then Result := floattostrf(A1, ffNumber, 15, 0) + ' МБ'
  else if A2 < 10 then Result := floattostrf(A2, ffNumber, 15, 2) + ' МБ'
  else if A2 < 100 then Result := floattostrf(A2, ffNumber, 15, 1) + ' МБ'
  else if A3 < 1 then Result := floattostrf(A2, ffNumber, 15, 0) + ' МБ'
  else if A3 < 10 then Result := floattostrf(A3, ffNumber, 15, 2) + ' ГБ'
  else if A3 < 100 then Result := floattostrf(A3, ffNumber, 15, 1) + ' ГБ'
  else Result := floattostrf(A3, ffNumber, 15, 0) + ' ГБ';
  Result := Result;
end;

type
  MEMORYSTATUSEX = record
     dwLength : DWORD;
     dwMemoryLoad : DWORD;
     ullTotalPhys : uint64;
     ullAvailPhys : uint64;
     ullTotalPageFile : uint64;
     ullAvailPageFile : uint64;
     ullTotalVirtual : uint64;
     ullAvailVirtual : uint64;
     ullAvailExtendedVirtual : uint64;
  end;






function GlobalMemoryStatusEx(var Buffer: MEMORYSTATUSEX): BOOL; stdcall; external 'kernel32' name 'GlobalMemoryStatusEx';

function GetSystemMem: string;
VAR
  MS_Ex : MemoryStatusEx;
  strTotalPhysMem, strTotalPhysAvail, strTotalFreeMem : string;
begin
 FillChar (MS_Ex, SizeOf(MemoryStatusEx), #0);
 MS_Ex.dwLength := SizeOf(MemoryStatusEx);
 GlobalMemoryStatusEx(MS_Ex);
 strTotalPhysMem := FormatByteSize(MS_Ex.ullTotalPhys);
 strTotalFreeMem := FormatByteSize(MS_Ex.ullAvailVirtual);
 strTotalPhysAvail := FormatByteSize(MS_Ex.ullAvailPhys);
 Result := strTotalPhysMem;
end;


function GetFileSizeStr(const sz: Int64): string;
begin
  if sz < 1024
    then Result:= IntToStr(sz)
    else if sz < 1048576
           then Result:= FloatToStrF(sz / 1024, ffFixed, 15, 0) + ' ГБ'
           else Result:= FloatToStrF(sz / 1048576, ffFixed, 15, 0) + ' ТБ';
end;

function GetDeviceInfo: String;
var
    Mes: TStringList;
    R: TRegistry;
    SMBios: TSMBios;
    LMemoryDevice: TMemoryDeviceInformation;
    LPhysicalMemArr: TPhysicalMemoryArrayInformation;
begin
    Mes := TStringList.Create;
    SMBios := TSMBios.Create;
    try
     {
     if GetComputerNetName <> '' then begin
            Mes.Add('' + 'Имя компьютера' + '');
            Mes.Add(GetComputerNetName);
            Mes.Add(#13);
     end;
     }
            {
            Mes.Add('' + 'Материнская плата' + '');
            Mes.Add(GetMotherBoardSerial);
            Mes.Add(#13);
            }

            Mes.Add('' + 'Установленная память (ОЗУ)' + '');
            Mes.Add('('+ GetSystemMem + ' доступно)');
            Mes.Add(#13);


             R := TRegistry.Create;
             R.Rootkey := HKEY_LOCAL_MACHINE;
             R.OpenKeyReadOnly('\HARDWARE\DESCRIPTION\System\CentralProcessor\0');

                  if (R.ReadString('ProcessorNameString') <> '') then begin
                         Mes.Add('' + 'Процессор' + '');
                         Mes.Add(R.ReadString('ProcessorNameString'));
                  end;   Mes.Add(#13);

             R.Free;



      if SMBios.HasMemoryDeviceInfo
      then
        for LMemoryDevice in SMBios.MemoryDeviceInfo do
        begin

              if not (LMemoryDevice.RAWMemoryDeviceInfo.Speed = 0) then begin


              if (LMemoryDevice.ManufacturerStr = 'Unknown') and not (LMemoryDevice.GetMemoryTypeStr = 'Unknown') then begin

                  Mes.Add('' + 'Модуль памяти' + '');
                  Mes.Add(LMemoryDevice.GetMemoryTypeStr + ' ' + GetFileSizeStr(LMemoryDevice.GetSize) + ' ' + IntToStr(LMemoryDevice.RAWMemoryDeviceInfo.Speed) + ' MHz');
                  Mes.Add(#13);
              end else
              if not (LMemoryDevice.ManufacturerStr = 'Unknown') and (LMemoryDevice.GetMemoryTypeStr = 'Unknown') then begin
                  Mes.Add('' + 'Модуль памяти' + '');
                  Mes.Add(LMemoryDevice.ManufacturerStr + ' ' + GetFileSizeStr(LMemoryDevice.GetSize) + ' ' + IntToStr(LMemoryDevice.RAWMemoryDeviceInfo.Speed) + ' MHz');
                  Mes.Add(#13);
              end else
              if (LMemoryDevice.ManufacturerStr = 'Unknown') and (LMemoryDevice.GetMemoryTypeStr = 'Unknown') then begin

                  Mes.Add('' + 'Модуль памяти' + '');
                  Mes.Add(GetFileSizeStr(LMemoryDevice.GetSize) + ' ' + IntToStr(LMemoryDevice.RAWMemoryDeviceInfo.Speed) + ' MHz');
                  Mes.Add(#13);
              end else begin

                  Mes.Add('' + 'Модуль памяти' + '');
                  Mes.Add(LMemoryDevice.ManufacturerStr + ' ' + LMemoryDevice.GetMemoryTypeStr + ' ' + GetFileSizeStr(LMemoryDevice.GetSize) + ' ' + IntToStr(LMemoryDevice.RAWMemoryDeviceInfo.Speed) + ' MHz');
                  Mes.Add(#13);
              end;

              end;

        end;

              for LPhysicalMemArr in SMBios.PhysicalMemoryArrayInfo do
              begin
                    Mes.Add('' + 'Количество слотов памяти' + '');
                    Mes.Add(IntToStr(LPhysicalMemArr.RAWPhysicalMemoryArrayInformation.NumberofMemoryDevices));
                    Mes.Add(#13);
              end;



        Result := Mes.Text;

    finally
      Mes.Free;
      SMBios.Free;
    end;
end;


end.
