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
  Classes, SysUtils, Inifiles, CheckLst, EditBtn, DateUtils, StdCtrls, Menus,
  futils;

function SaveConfig(XMLEdit:String; GamesEdit:String; LastFavFolder:String; ViewStyle:Integer; ViewSelected:Boolean; CheckListBox:TCheckListBox):Boolean;
function LoadConfig(XMLEdit:TFilenameEdit; GamesEdit:TDirectoryEdit; CheckListBox:TCheckListBox; var ViewStyle:Integer; var ViewSelected:Boolean; var LastFavFolder:String):Boolean;
function SaveSlot(Slot: Integer; CheckListBox:TCheckListBox; SlotMenu:TMenuItem; NameOnly:Boolean = False):Boolean;
function LoadSlot(Slot: Integer; CheckListBox:TCheckListBox; SlotMenu:TMenuItem; NameOnly:Boolean = False):Boolean;
function GetSavedChecked(Slot: Integer):String;

implementation

function GetSavedChecked(Slot: Integer):String;
var
  AppPath,Res: String;
  Ini: TIniFile;
begin
  AppPath := ExtractFilePath(ParamStr(0));
  Ini := TIniFile.Create(AppPath+'config.ini');
  Res := '';
  try
    if (Slot > 0) then
      Res := Ini.ReadString('games', 'checked'+inttostr(Slot),'')
    else
      Res := Ini.ReadString('config', 'checked', '');
  finally
    Ini.Free;
  end;
  GetSavedChecked := Res;
end;

function LoadSlot(Slot:Integer; CheckListBox:TCheckListBox; SlotMenu:TMenuItem; NameOnly:Boolean = False):Boolean;
var
  Ini: TIniFile;
  AppPath,CheckedGames: String;
  ChkStringList: TStringList;
  i,j: Integer;
begin
  AppPath := ExtractFilePath(ParamStr(0));
  Ini := TIniFile.Create(AppPath+'config.ini');
  ChkStringList := TStringList.Create;
  try
    SlotMenu.Caption := Ini.ReadString('games','name'+inttostr(Slot),'Slot '+inttostr(Slot));
    if (not NameOnly) then
      begin
        CheckedGames := Ini.ReadString('games','checked'+inttostr(Slot),'');
        ChkStringList.Clear;
        ChkStringList.Delimiter := ',';
        ChkStringList.StrictDelimiter := True;
        ChkStringList.DelimitedText := CheckedGames;
        CheckListBox.CheckAll(cbUnchecked,False,False);
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
      end;
  finally
    Ini.Free;
    ChkStringList.Free;
  end;
  LoadSlot := True;
end;

function SaveSlot(Slot:Integer; CheckListBox:TCheckListBox; SlotMenu:TMenuItem; NameOnly:Boolean = False):Boolean;
var
  Ini: TIniFile;
  AppPath,CheckedGames: String;
  i: Integer;
begin
  AppPath := ExtractFilePath(ParamStr(0));
  Ini := TIniFile.Create(AppPath+'config.ini');

  try
    CheckedGames := '';
    Ini.WriteString('games','name'+inttostr(Slot),SlotMenu.Caption);
    if (not NameOnly) then
      begin
        for i := 0 to (CheckListBox.Count-1) do
          begin
            if (CheckListBox.Checked[i]) and (GameCodes[i] <> '') then CheckedGames := CheckedGames+' '+GameCodes[i];
          end;
        CheckedGames := StringReplace(Trim(CheckedGames),' ',',',[rfReplaceAll]);
        Ini.WriteString('games','checked'+inttostr(Slot),CheckedGames);
      end;
  finally
    Ini.Free;
  end;
  SaveSlot := True;
end;

function SaveConfig(XMLEdit:String; GamesEdit:String; LastFavFolder:String; ViewStyle:Integer; ViewSelected:Boolean; CheckListBox:TCheckListBox):Boolean;
var
  Ini: TIniFile;
  AppPath,CheckedGames: String;
  i: integer;
begin
  AppPath := ExtractFilePath(ParamStr(0));
  Ini := TIniFile.Create(AppPath+'config.ini');
  try
    CheckedGames := '';
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
  finally
    Ini.Free;
  end;
  SaveConfig := True;
end;

function LoadConfig(XMLEdit:TFilenameEdit; GamesEdit:TDirectoryEdit; CheckListBox:TCheckListBox; var ViewStyle:Integer; var ViewSelected:Boolean; var LastFavFolder:String):Boolean;
var
  Ini: TIniFile;
  ChkStringList: TStringList;
  AppPath,CheckedGames: String;
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

