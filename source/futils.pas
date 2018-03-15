{ futils.pas
  Description: Handling of file routines for application


  This file is part of SNESC Favorites.

  SNESC Favorites is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  SNESC Favorites is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with SNESC Favorites.  If not, see <http://www.gnu.org/licenses/>.
}

unit futils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, CheckLst, Dialogs, FileUtil;

function LoadGamesList(CheckListBox: TCheckListBox; Filename: String):Boolean;
function GetLastFolderNumber(Path: String):String;
function GenerateFolderName(Number: String):String;
function CreateFaveLinks(ConsolePath: String; GamesList: TCheckListBox; var FolderSuff: String):Boolean;
function UpdateXML(Filename: String; GamesList: TCheckListBox):Boolean;

var
  GameCodes : Array of String;

implementation


function LoadGamesList(CheckListBox: TCheckListBox; Filename: String):Boolean;
var
  FStringList: TStringList;
  i,j: integer;
  s: String;
  RegexObj: TRegExpr;
begin
  if not FileExists(Filename) then
    begin
      LoadGamesList := False;
      exit;
    end;

  FStringList := TStringList.Create;
  j := 0;

  try
    FStringList.LoadFromFile(Filename);
    for i:=0 to FStringList.Count-1 do
      begin
        s := StringReplace(FStringList[i],'&amp;','&',[rfReplaceAll, rfIgnoreCase]);
        if (Pos('  <Folder',s) = 1) then // and (Pos('folder_',FStringList[i]) <= 0) then
          begin
            RegExObj := TRegExpr.Create('^  <Folder name=\"(.*?)\" icon');
            if RegExObj.Exec(s) then
              CheckListBox.Items.Add('-'+RegExObj.Match[1]);
            RegExObj.Free;
            inc(j);
            SetLength(GameCodes,j);
            GameCodes[j-1] := '000';
          end
        else if (Pos('Game code',s) > 0) then
          begin
            RegExObj := TRegExpr.Create('Game code="(.*?)" name="(.*?)"');
            if RegExObj.Exec(s) then
              begin
                CheckListBox.Items.Add(RegExObj.Match[2]);
                inc(j);
                SetLength(GameCodes,j);
                GameCodes[j-1] := RegExObj.Match[1];
              end;
            RegExObj.Free;
          end;
      end;
  finally
    if Assigned(FStringList) then
      FreeAndNil(FStringList);
  end;
  LoadGamesList := True;
end;

function GetLastFolderNumber(Path: String):String;
var
  FD: TSearchRec;
  SL: TStringList;
begin
  GetLastFolderNumber := '';
  If (FindFirst(Path+'*',faDirectory,FD)=0) then
    begin
      SL := TStringList.Create;
      repeat
        SL.Add(FD.Name);
      until (FindNext(FD) <> 0);
      SL.Sort;
      GetLastFolderNumber := SL[SL.Count-1];
    end;
end;

function GenerateFolderName(Number: String):String;
var
  num: integer;
begin
  if not TryStrToInt(Number,num) then
    begin
      ShowMessage('Error: String not a valid number.');
      exit;
    end;
  num := num+1;
  GenerateFolderName := Format('%.3d',[num]);
end;

function CreateFaveLinks(ConsolePath: String; GamesList: TCheckListBox; var FolderSuff: String):Boolean;
var
  DeskFile,EmptyFile,FindLink: TStringList;
  FPath,g,t: String;
  j: Integer;
  Res: Boolean;
begin

  FPath := ExtractFilePath(ParamStr(0));
  DeskFile := TStringList.Create;
  EmptyFile := TStringList.Create;
  DeskFile.LineBreak := #10; // Hakchi kernel will give c8 errors for Dos LineFeeds

  if (DirectoryExists(ConsolePath+FolderSuff)) then
    begin
      if (FileExists(ConsolePath+FolderSuff+'\.fav')) then
        begin
          if (DirectoryExists(ConsolePath+'000\CLV-S-00'+FolderSuff)) then
            begin
              Res := DeleteDirectory(ConsolePath+'000\CLV-S-00'+FolderSuff,True);
              if Res then
                Res := RemoveDir(ConsolePath+'000\CLV-S-00'+FolderSuff);
            end;
          Res := DeleteDirectory(ConsolePath+FolderSuff,True);
          if Res then
            Res := RemoveDir(ConsolePath+FolderSuff);
        end
      else
        begin
          t := FolderSuff;
          Repeat
            ShowMessage('t='+t);
            t := GenerateFolderName(t);
            ShowMessage('t='+t);
          until (not DirectoryExists(ConsolePath+t));
          FolderSuff := t;
        end;
    end;
  CreateDir(ConsolePath+'000\CLV-S-00'+FolderSuff);
  CreateDir(ConsolePath+FolderSuff);

  EmptyFile.LineBreak := #10;
  EmptyFile.Add('');
  EmptyFile.SaveToFile(ConsolePath+FolderSuff+'\.fav');

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

  DeskFile.SaveToFile(ConsolePath+'000\CLV-S-00'+FolderSuff+'\CLV-S-00'+FolderSuff+'.desktop');

  if (FileExists(FPath+'\favorites.png')) then
    begin
      CopyFile(FPath+'\favorites.png',ConsolePath+'000\CLV-S-00'+FolderSuff+'\CLV-S-00'+FolderSuff+'.png');
    end;
  if (FileExists(FPath+'\favorites_small.png')) then
    begin
      CopyFile(FPath+'\favorites_small.png',ConsolePath+'000\CLV-S-00'+FolderSuff+'\CLV-S-00'+FolderSuff+'_small.png');
    end;

  CreateDir(ConsolePath+FolderSuff+'\CLV-S-00000');
  CopyFile(ConsolePath+'001\CLV-S-00000\CLV-S-00000.desktop',ConsolePath+FolderSuff+'\CLV-S-00000\CLV-S-00000.desktop');
  CopyFile(ConsolePath+'001\CLV-S-00000\CLV-S-00000.png',ConsolePath+FolderSuff+'\CLV-S-00000\CLV-S-00000.png');
  CopyFile(ConsolePath+'001\CLV-S-00000\CLV-S-00000_small.png',ConsolePath+FolderSuff+'\CLV-S-00000\CLV-S-00000_small.png');

  try
    for j := 0 to GamesList.Count-1 do
      begin
        g := GamesList.Items[j];
        if (g[1] <> '-') and (GamesList.Checked[j]) then
          begin
            if (Length(GameCodes[j]) > 0) then
              begin
                try
                  FindLink := TStringList.Create;
                  FindAllFiles(FindLink,ConsolePath,GameCodes[j]+'.desktop',True);
                  if (FindLink.Count > 0) then
                    begin
                      CreateDir(ConsolePath+FolderSuff+'\'+GameCodes[j]);
                      CopyFile(FindLink[0],ConsolePath+FolderSuff+'\'+GameCodes[j]+'\'+GameCodes[j]+'.desktop');
                    end;
                finally
                  FindLink.Free;
                end;
              end;
          end;
      end;
  finally
    DeskFile.Free;
    EmptyFile.Free;
  end;

  CreateFaveLinks := True;
end;

// Updates XML but Hakchi gui overrides it.  Method does not work.
function UpdateXML(Filename: String; GamesList: TCheckListBox):Boolean;
var
  FStringList,NewStringList: TStringList;
  FavMatch: Boolean;
  g,s,t : String;
  i,j : integer;
begin
  if not FileExists(Filename) then
    begin
      UpdateXML := False;
      exit;
    end;

  FavMatch := False;
  FStringList := TStringList.Create;
  NewStringList := TStringList.Create;

  try
    FStringList.LoadFromFile(Filename);
    for i:=0 to FStringList.Count-1 do
      begin
        s := FStringList[i];
        if (Pos('  <Folder name="Favorites"',s) = 1) then
          begin
            FavMatch := True;
          end
        else if (Pos('</Tree>',s) = 1) then
          begin
            NewStringList.Add('  <Folder name="Favorites" icon="favorites" position="3">');
            for j := 0 to GamesList.Count-1 do
              begin
                g := GamesList.Items[j];
                if (g[1] <> '-') and (GamesList.Checked[j]) then
                  begin
                    if (Length(GameCodes[j]) > 0) then
                      begin
                        t := StringReplace(g,'&','&amp;',[rfReplaceAll, rfIgnoreCase]);
                        NewStringList.Add('    <Game code="'+GameCodes[j]+'" name="'+t+'" />');
                      end;
                  end;
              end;
            NewStringList.Add('  </Folder>');
            NewStringList.Add('</Tree>');
          end
        else if (FavMatch) then
          begin
            if (Pos('  </Folder>',s) = 1) then
              begin
                FavMatch := False;
              end;
          end
        else
          begin
            NewStringList.Add(s);
          end;
      end;

    NewStringList.SaveToFile('test.xml');
  finally
    if Assigned(FStringList) then FreeAndNil(FStringList);
  end;

  UpdateXML := True;
end;

end.

