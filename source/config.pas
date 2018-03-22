{ config.pas
  Description: Routines for loading/saving application configuration.


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

unit config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Inifiles, CheckLst, EditBtn, DateUtils,
  futils;

function SaveConfig(XMLEdit:String; GamesEdit:String; LastFavFolder:String; ViewStyle:Integer; ViewSelected:Boolean; CheckListBox:TCheckListBox):Boolean;
function LoadConfig(XMLEdit:TFilenameEdit; GamesEdit:TDirectoryEdit; CheckListBox:TCheckListBox; var ViewStyle:Integer; var ViewSelected:Boolean; var LastFavFolder:String):Boolean;
function GetSavedChecked(XMLEdit:String):String;

implementation

function GetSavedChecked(XMLEdit:String):String;
var
  AppPath,Res: String;
  Ini: TIniFile;
  i,j: Integer;
begin
  AppPath := ExtractFilePath(ParamStr(0));
  Ini := TIniFile.Create(AppPath+'config.ini');
  Res := '';
  try
    for i := 0 to 2 do
      begin
        j := i+1;
        if (XMLEdit = Ini.ReadString('games','path'+inttostr(j),'')) then
          begin
            Res := Ini.ReadString('games', 'checked'+inttostr(j),'');
            break;
          end;
      end;
  finally
    Ini.Free;
  end;
  GetSavedChecked := Res;
end;

function SaveConfig(XMLEdit:String; GamesEdit:String; LastFavFolder:String; ViewStyle:Integer; ViewSelected:Boolean; CheckListBox:TCheckListBox):Boolean;
var
  Ini: TIniFile;
  AppPath,CheckedGames: String;
  savedPath: Array [0..2] of String;
  savedGames: Array [0..2] of String;
  savedStamp: Array [0..2] of Int64;
  i,j,thispath,emptypath,oldestPath: integer;
  oldestStamp: Int64;
begin
  AppPath := ExtractFilePath(ParamStr(0));
  thispath := High(savedPath)+1;
  emptypath := thispath;
  oldestPath := thispath;
  oldestStamp := DateTimeToUnix(Now);
  Ini := TIniFile.Create(AppPath+'config.ini');
  try
    CheckedGames := '';
    for i := 0 to High(savedPath) do
      begin
        j := i+1;
        savedPath[i] := Ini.ReadString('games','path'+inttostr(j),'');
        savedGames[i] := Ini.ReadString('games','checked'+inttostr(j),'');
        savedStamp[i] := Ini.ReadInt64('games','timestamp'+inttostr(j),DateTimeToUnix(Now));
        if (savedPath[i] = XMLEdit) then thispath := i;
        if (savedPath[i] = '') and (i < emptypath) then emptypath := i;
        if (savedStamp[i] < oldestStamp) then oldestPath := i;
      end;
    Ini.WriteString('config','XMLFile',XMLEdit);
    Ini.WriteString('config','gamesfolder',GamesEdit);
    Ini.WriteString('config','lastfav',LastFavFolder);
    Ini.WriteInteger('config','view',ViewStyle);
    Ini.WriteBool('config','viewselect',ViewSelected);
    for i := 0 to (CheckListBox.Count-1) do
      begin
        if (CheckListBox.Checked[i]) and (GameCodes[i] <> '') then CheckedGames := CheckedGames+' '+GameCodes[i];
      end;
    CheckedGames := StringReplace(Trim(CheckedGames),' ',',',[rfReplaceAll]);
    Ini.WriteString('config','checked',CheckedGames);
    if (thispath < High(savedPath)) then
      begin
        savedGames[thispath] := CheckedGames;
        savedStamp[thispath] := DateTimeToUnix(Now);
      end
    else if (emptypath < High(savedPath)) then
      begin
        savedPath[emptypath] := XMLEdit;
        savedGames[emptypath] := CheckedGames;
        savedStamp[emptypath] := DateTimeToUnix(Now);
      end
    else
      begin
        savedPath[oldestpath] := XMLEdit;
        savedGames[oldestpath] := CheckedGames;
        savedStamp[oldestpath] := DateTimeToUnix(Now);
      end;
    for i := 0 to High(savedPath) do
      begin
        j := i+1;
        Ini.WriteString('games','path'+inttostr(j),savedPath[i]);
        Ini.WriteString('games','checked'+inttostr(j),savedGames[i]);
        Ini.WriteInt64('games','timestamp'+inttostr(j),savedStamp[i]);
      end;
  finally
    Ini.Free;
  end;
  SaveConfig := True;
end;

function LoadConfig(XMLEdit:TFilenameEdit; GamesEdit:TDirectoryEdit; CheckListBox:TCheckListBox; var ViewStyle:Integer; var ViewSelected:Boolean; var LastFavFolder:String):Boolean;
var
  Ini: TIniFile;
  ChkStringList: TStringList;
  AppPath,savedPath,CheckedGames: String;
  i,j: integer;
begin
  AppPath := ExtractFilePath(ParamStr(0));
  Ini := TIniFile.Create(AppPath+'config.ini');
  ChkStringList := TStringList.Create;
  try
    CheckedGames := '';
    XMLEdit.Caption := Ini.ReadString('config','XMLFile','');
    GamesEdit.Caption := Ini.ReadString('config','gamesfolder','');
    LastFavFolder := Ini.ReadString('config','lastfav','');
    CheckedGames := Ini.ReadString('config','checked','');
    ViewStyle := Ini.ReadInteger('config','view',1);
    ViewSelected := Ini.ReadBool('config','viewselect',False);
    for i := 0 to 2 do
      begin
        j := i+1;
        savedPath := Ini.ReadString('games','path'+inttostr(j),'');
        if (savedPath = XMLEdit.Caption) then
          begin
            CheckedGames := Ini.ReadString('games','checked'+inttostr(j),'');
            break;
          end;
      end;
    if (FileExists(XMLEdit.Caption)) then LoadGamesList(CheckListBox,XMLEdit.Caption,ViewStyle,ViewSelected,CheckedGames);
    ChkStringList.Clear;
    ChkStringList.Delimiter := ',';
    ChkStringList.StrictDelimiter := True;
    ChkStringList.DelimitedText := CheckedGames;
    for i := 0 to (ChkStringList.Count-1) do
      begin
        for j := 0 to (Length(GameCodes)-1) do
          begin
            if (GameCodes[j] = ChkStringList[i]) then
              begin
                CheckListBox.Checked[j] := True;
                break;
              end;
          end;
      end;
  finally
    Ini.Free;
    ChkStringList.Free;
  end;
  LoadConfig := True;
end;

end.

