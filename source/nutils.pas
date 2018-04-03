unit nutils;

{$mode objfpc}{$H+}

{$DEFINE debug}

interface

uses
  Classes, SysUtils, Dialogs, Process, WinDirs, CheckLst, ComCtrls, LazFileUtils,
  futils;


function NANDGetLastFolderNumber:String;
function NANDCreateFaveLinks(GamesList: TCheckListBox; ProgressBar: TProgressBar; StatusBar: TStatusBar; var FolderSuff: String):Boolean;
function CloverShellPath:String;
function CloverFileExists(FileName: String; IsDir: Boolean = False):Boolean;

var
  snesregion : String;
  hakchidir : String;

implementation

procedure debuglog(Text: String);
var
  tfOut: Textfile;
  FPath: String;
begin
  FPath := ExtractFilePath(ParamStr(0));
  if (not FileExists(FPath+'debug.log')) then
    begin
      AssignFile(tfOut,FPath+'debug.log');
      rewrite(tfOut);
      close(tfOut);
    end;
  AssignFile(tfOut,FPath+'debug.log');
  Append(tfOut);
  WriteLn(tfOut,Text);
  close(tfOut);
end;

function CloverShellPush(Local: String; Remote: String):Boolean;
var
  Process: TProcess;
  FPath,Win32: String;
begin
  FPath := ExtractFilePath(ParamStr(0));
  CloverShellPush := False;
  Process := TProcess.Create(nil);
  try
    Win32 := GetWindowsSpecialDir(CSIDL_SYSTEM);
    Process.Executable := Win32+'cmd.exe';
    Process.Parameters.Add('/c '+FPath+'tools\clovershell.exe push '+Local+' '+Remote);
    Process.Options := Process.Options + [poWaitOnExit];
    // when working + [poNoConsole]
{$IFDEF DEBUG}
    debuglog('Push: '+Win32+'cmd.exe /c '+FPath+'tools\clovershell.exe push '+Local+' '+Remote);
{$ELSE}
    Process.Execute;
{$ENDIF}
  finally
    Process.Free;
  end;
  CloverShellPush := True;
end;

function CloverShellExec(Params: String; Outfile: String = ''):Boolean;
var
  Process: TProcess;
  FPath,Win32: String;
begin
  FPath := ExtractFilePath(ParamStr(0));
  CloverShellExec := False;
  if (not FileExists(FPath+'tools\clovershell.exe')) then exit;
  Process := TProcess.Create(nil);
  try
    Win32 := GetWindowsSpecialDir(CSIDL_SYSTEM);
    Process.Executable := Win32+'cmd.exe';
    if (Outfile <> '') then
      Process.Parameters.Add('/c '+FPath+'tools\clovershell.exe exec "'+Params+'" > '+Outfile)
    else
      Process.Parameters.Add('/c '+FPath+'tools\clovershell.exe exec "'+Params+'"');
    Process.Options := Process.Options + [poWaitOnExit];
    // when working + [poNoConsole]
{$IFDEF DEBUG}
    debuglog('Exec: '+Win32+'cmd.exe /c '+FPath+'tools\clovershell.exe exec "'+Params+'"');
{$ENDIF}
    Process.Execute;
  finally
    Process.Free;
  end;
  CloverShellExec := True;
end;

function CloverShellPath:String;
var
  FStringList: TStringList;
  FPath: String;
begin
  FPath := ExtractFilePath(ParamStr(0));
  CloverShellPath := '';
  if (snesregion <> '') then
    begin
      CloverShellPath := snesregion;
{$IFDEF DEBUG}
      debuglog('Path: snesregion already set to '+snesregion);
{$ENDIF}
      exit;
    end;
  if (snesregion = '') then snesregion := 'usa';
  CloverShellExec('cat /etc/clover/REGION',FPath+'csr.log');
  if (not FileExists(FPath+'csr.log')) then
    begin
      snesregion := '';
      CloverShellPath := '';
{$IFDEF DEBUG}
      debuglog('Path: csr.log file not found.');
{$ENDIF}
      exit;
    end;
  FStringList := TStringList.Create;
  try
    FStringList.LoadFromFile(FPath+'csr.log');
    if (FStringList.Count > 0) then
      begin
{$IFDEF DEBUG}
        debuglog('Path: snesregion:'+FStringList[0]);
{$ENDIF}
        if (FStringList[0] = 'JPN') then snesregion := 'jpn'
        else if (FStringList[0] = 'EUR') then snesregion := 'eur';
      end;
  finally
    FStringList.Free;
  end;
  hakchidir := '/var/lib/hakchi/games/snes-'+snesregion+'/';
{$IFDEF DEBUG}
  debuglog('Path: '+hakchidir);
{$ENDIF}
  DeleteFile(FPath+'csr.log');
  CloverShellPath := snesregion;
end;

function CloverShellFind(Path: String; FileName: String):String;
var
  i: integer;
  FStringList: TStringList;
  FPath: String;
begin
  FPath := ExtractFilePath(ParamStr(0));
  CloverShellFind := '';
  CloverShellExec('find '+Path+' -name '+FileName,FPath+'csf.log');
  if (FileExists(FPath+'csf.log')) then
    begin
      FStringList := TStringList.Create;
      try
        FStringList.LoadFromFile(FPath+'csf.log');
        for i := 0 to FStringList.Count-1 do
          begin
{$IFDEF DEBUG}
            debuglog('Find ('+Path+', '+Filename+'): '+FStringList[i]);
{$ENDIF}
            if (Pos(FileName,FStringList[i]) > 0) then
              begin
{$IFDEF DEBUG}
                debuglog('Find ('+Path+', '+Filename+'): Found at '+inttostr(i));
{$ENDIF}
                CloverShellFind := FStringList[i];
                break;
              end;
          end;
      finally
        FStringList.Free;
      end;
    end;
  DeleteFile(FPath+'csf.log');
end;

function CloverFileExists(FileName: String; IsDir: Boolean = False):Boolean;
var
  i: integer;
  FStringList: TStringList;
  FPath,flag: String;
begin
  FPath := ExtractFilePath(ParamStr(0));
  CloverFileExists := False;
  if (IsDir) then
    flag := '-d'
  else
    flag := '-f';
  CloverShellExec('if [ '+flag+' '+FileName+' ]; then echo "found"; else echo "not found"; fi',FPath+'cfe.log');
  if (FileExists(FPath+'cfe.log')) then
    begin
      FStringList := TStringList.Create;
      try
        FStringList.LoadFromFile(FPath+'cfe.log');
        for i := 0 to FStringList.Count-1 do
          begin
{$IFDEF DEBUG}
            debuglog('FileExists ('+FileName+', '+flag+'): '+inttostr(i)+' '+FStringList[i]);
{$ENDIF}
            if (FStringList[i] = 'found') then
              begin
                CloverFileExists := True;
                break;
              end
            else if (FStringList[i] = 'not found') then
              begin
                CloverFileExists := False;
                break;
              end;
          end;
      finally
        FStringList.Free;
      end;
    end;
{$IFDEF DEBUG}
    debuglog('FileExists: '+BoolToStr(CloverFileExists));
{$ENDIF}
    DeleteFile(FPath+'cfe.log');
end;

function NANDGetLastFolderNumber:String;
var
  i: integer;
  FStringList: TStringList;
  FPath: String;
begin
  FPath := ExtractFilePath(ParamStr(0));
  CloverShellPath;
  NANDGetLastFolderNumber := '';
  if (CloverShellExec('ls '+hakchidir,FPath+'nanddir.log')) then
    begin
      if (FileExists(FPath+'nanddir.log')) then
        begin
          FStringList := TStringList.Create;
          try
            FStringList.LoadFromFile(FPath+'nanddir.log');
            for i := 0 to FStringList.Count-1 do
              begin
{$IFDEF DEBUG}
                debuglog('LastFolder: '+inttostr(i)+' '+FStringList[i]);
{$ENDIF}
                if (Trim(FStringList[i]) <> '') then
                  NANDGetLastFolderNumber := Trim(FStringList[i]);
              end;
          finally
            FStringList.Free;
          end;
          DeleteFile(FPath+'nanddir.log');
        end;
    end;
end;

function NANDCreateFaveLinks(GamesList: TCheckListBox; ProgressBar: TProgressBar; StatusBar: TStatusBar; var FolderSuff: String):Boolean;
var
  DeskFile,EmptyFile: TStringList;
  StatusPanel: TStatusPanel;
  FindLink,FPath,g,t: String;
  j: Integer;
  Res: Boolean;
begin
  FPath := ExtractFilePath(ParamStr(0));
  DeskFile := TStringList.Create;
  EmptyFile := TStringList.Create;
  ProgressBar.Visible := True;
  DeskFile.LineBreak := #10; // Hakchi kernel will give c8 errors for Dos LineFeeds

  StatusPanel := StatusBar.Panels.Items[1];
  ProgressBar.Max := GamesList.Count+3;
  ProgressBar.Position:=0;
  StatusPanel.Text := Inttostr(Round(ProgressBar.Position/ProgressBar.Max*100))+'%';
  StatusBar.Update;


//  if (DirectoryExists(ConsolePath+FolderSuff)) then
//    begin
      if (CloverFileExists(hakchidir+FolderSuff+'/.fav')) then
        begin
          if (CloverFileExists(hakchidir+'000/CLV-S-00'+FolderSuff,True)) then
            begin
{$IFDEF DEBUG}
              debuglog('CreateLinks: FE: '+hakchidir+'000/CLV-S-00'+FolderSuff);
{$ELSE}
              CloverShellExec('rm -rf '+hakchidir+'000/CLV-S-00'+FolderSuff);
{$ENDIF}
            end;
{$IFNDEF DEBUG}
          CloverShellExec('rm -rf '+hakchidir+FolderSuff);
{$ELSE}
          debuglog('CreateLinks: FE-Fav: '+hakchidir+FolderSuff+'/.fav');
{$ENDIF}
        end
      else
        begin
          t := FolderSuff;
          Repeat
            t := GenerateFolderName(t);
          until (not CloverFileExists(hakchidir+t,True));
{$IFDEF DEBUG}
          debuglog('CreateLinks: GenFoldName='+t);
{$ENDIF}
          FolderSuff := t;
        end;
//    end;
  ProgressBar.Position:=ProgressBar.Position+1;
  StatusPanel.Text := Inttostr(Round(ProgressBar.Position/ProgressBar.Max*100))+'%';
  StatusBar.Update;

{$IFNDEF DEBUG}
  CloverShellExec('mkdir '+hakchidir+'000/CLV-S-00'+FolderSuff);
  CloverShellExec('mkdir '+hakchidir+FolderSuff);
{$ENDIF}
  EmptyFile.LineBreak := #10;
  EmptyFile.Add('');
  EmptyFile.SaveToFile(FPath+'.fav');
{$IFNDEF DEBUG}
  CloverShellPush(FPath+'.fav',hakchidir+FolderSuff+'/.fav');
{$ENDIF}
  DeleteFile(FPath+'.fav');

  DeskFile.Add('[Desktop Entry]');
  DeskFile.Add('Type=Application');
  DeskFile.Add('Exec=/bin/chmenu '+FolderSuff+' ');
  DeskFile.Add('Path=/var/saves/FOLDER');
  DeskFile.Add('Name=Favorites');
  DeskFile.Add('Icon=/var/games/CLV-S-00'+FolderSuff+'/CLV-S-00'+FolderSuff+'.png');
  DeskFile.Add('');
  DeskFile.Add('[X-CLOVER Game]');
  DeskFile.Add('Code=CLV-S-00'+FolderSuff);
  DeskFile.Add('TestID=777');
  DeskFile.Add('ID=0');
  DeskFile.Add('Players=1');
  DeskFile.Add('Simultaneous=0');
  DeskFile.Add('ReleaseDate=7777-77-77');
  DeskFile.Add('SaveCount=0');
  DeskFile.Add('SortRawTitle=Эfavorites');
  DeskFile.Add('SortRawPublisher=ZZZZZZZZZX');
  DeskFile.Add('Copyright=hakchi2 ©2017 Alexey ''Cluster'' Avdyukhin');

  DeskFile.SaveToFile(FPath+'tmp.desktop');
{$IFNDEF DEBUG}
  CloverShellPush(FPath+'tmp.desktop',hakchidir+'000/CLV-S-00'+FolderSuff+'/CLV-S-00'+FolderSuff+'.desktop');
{$ENDIF}
  DeleteFile(FPath+'tmp.desktop');

  ProgressBar.Position:=ProgressBar.Position+1;
  StatusPanel.Text := Inttostr(Round(ProgressBar.Position/ProgressBar.Max*100))+'%';
  StatusBar.Update;

{$IFNDEF DEBUG}
  if (FileExists(FPath+'favorites.png')) then
    begin
      CloverShellPush(FPath+'favorites.png',hakchidir+'000/CLV-S-00'+FolderSuff+'/CLV-S-00'+FolderSuff+'.png');
    end;
  if (FileExists(FPath+'favorites_small.png')) then
    begin
      CloverShellPush(FPath+'favorites_small.png',hakchidir+'000/CLV-S-00'+FolderSuff+'/CLV-S-00'+FolderSuff+'_small.png');
    end;

  CloverShellExec('mkdir '+hakchidir+FolderSuff+'/CLV-S-00000');
  CloverShellExec('cp '+hakchidir+'001/CLV-S-00000/CLV-S-00000.desktop '+hakchidir+FolderSuff+'/CLV-S-00000/CLV-S-00000.desktop');
  CloverShellExec('cp '+hakchidir+'001/CLV-S-00000/CLV-S-00000.png '+hakchidir+FolderSuff+'/CLV-S-00000/CLV-S-00000.png');
  CloverShellExec('cp '+hakchidir+'001/CLV-S-00000/CLV-S-00000_small.png '+hakchidir+FolderSuff+'/CLV-S-00000/CLV-S-00000_small.png');
{$ENDIF}
  ProgressBar.Position:=ProgressBar.Position+1;
  StatusPanel.Text := Inttostr(Round(ProgressBar.Position/ProgressBar.Max*100))+'%';
  StatusBar.Update;

  try
    for j := 0 to GamesList.Count-1 do
      begin
        g := GamesList.Items[j];
        if (g[1] <> '-') and (g[1] <> '=') and (GamesList.Checked[j]) then
          begin
            if (Length(GameCodes[j]) > 0) then
              begin
                FindLink := CloverShellFind(hakchidir,GameCodes[j]+'.desktop');
{$IFDEF DEBUG}
                debuglog('CreateLinks: Find: '+hakchidir+' '+GameCodes[j]+'.desktop'+ 'Res='+FindLink);
{$ENDIF}
{$IFNDEF DEBUG}
                if (FindLink <> '') then
                  begin
                    CloverShellExec('mkdir '+hakchidir+FolderSuff+'/'+GameCodes[j]);
                    CloverShellExec('cp '+FindLink+' '+hakchidir+FolderSuff+'/'+GameCodes[j]+'/'+GameCodes[j]+'.desktop');
                  end;
{$ENDIF}
              end;
          end;
        ProgressBar.Position:=ProgressBar.Position+1;
        StatusPanel.Text := Inttostr(Round(ProgressBar.Position/ProgressBar.Max*100))+'%';
        StatusBar.Update;
      end;
  finally
    DeskFile.Free;
    EmptyFile.Free;
  end;
  NANDCreateFaveLinks := True;
end;

end.

