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
  Classes, SysUtils, Inifiles, ComCtrls, EditBtn, DateUtils, StdCtrls, Menus,
  ui_utils;

function SaveConfig(XMLEdit:String; GamesEdit:String; NANDChecked:Boolean; LastFavFolder:String; ViewStyle:Integer; ViewSelected:Boolean; ViewGameInfo:Boolean; HomeName:String; Tree:TTreeView):Boolean;
function LoadConfig(XMLEdit:TFilenameEdit; GamesEdit:TDirectoryEdit; GameTree: TTreeView; TreeView:TTreeView; var NANDCheckBox:TCheckBox; var ViewStyle:Integer; var ViewSelected:Boolean; var ViewGameInfo:Boolean; var LastFavFolder:String; var HomeName:String):Boolean;
function SaveSlot(Slot: Integer; Tree:TTreeView; SlotMenu:TMenuItem; NameOnly:Boolean = False):Boolean;
function LoadSlot(Slot: Integer; Tree:TTreeView; SlotMenu:TMenuItem; NameOnly:Boolean = False):Boolean;
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

function LoadSlot(Slot:Integer; Tree:TTreeView; SlotMenu:TMenuItem; NameOnly:Boolean = False):Boolean;
var
  Ini: TIniFile;
  AppPath,CheckedGames,CheckedHashes: String;
  ChkStringList: TStringList;
begin
  AppPath := ExtractFilePath(ParamStr(0));
  Ini := TIniFile.Create(AppPath+'config.ini');
  ChkStringList := TStringList.Create;
  try
    SlotMenu.Caption := Ini.ReadString('games','name'+inttostr(Slot),'Slot '+inttostr(Slot));
    if (not NameOnly) then
      begin
        CheckedGames := Ini.ReadString('games','checked'+inttostr(Slot),'');
        CheckedHashes := Ini.ReadString('games','shortcuts'+inttostr(Slot),'');
        ChkStringList.Clear;
        ChkStringList.Delimiter := ',';
        ChkStringList.StrictDelimiter := True;
        ChkStringList.DelimitedText := CheckedGames+','+CheckedHashes;
        ChkStringList.Sort;
        ChkStringList.Sorted := True;
        CheckNodesList(Tree,ChkStringList);
      end;
  finally
    Ini.Free;
    ChkStringList.Free;
  end;
  LoadSlot := True;
end;

function SaveSlot(Slot:Integer; Tree:TTreeView; SlotMenu:TMenuItem; NameOnly:Boolean = False):Boolean;
var
  Ini: TIniFile;
  AppPath,CheckedGames,CheckedHashes: String;
  i: Integer;
  GL: TStringList;
begin
  AppPath := ExtractFilePath(ParamStr(0));
  Ini := TIniFile.Create(AppPath+'config.ini');
  GL := TStringList.Create;
  try
    CheckedGames := '';
    Ini.WriteString('games','name'+inttostr(Slot),SlotMenu.Caption);
    if (not NameOnly) then
      begin
        GetCheckedCodes(Tree,GL);
        for i := 0 to (GL.Count-1) do
          begin
            CheckedGames := CheckedGames+' '+GL[i];
          end;
        CheckedGames := StringReplace(Trim(CheckedGames),' ',',',[rfReplaceAll]);
        GetCheckedHashes(Tree,GL);
        for i := 0 to (GL.Count-1) do
          begin
            CheckedHashes := CheckedHashes+' '+GL[i];
          end;
        CheckedHashes := StringReplace(Trim(CheckedHashes),' ',',',[rfReplaceAll]);
        Ini.WriteString('games','checked'+inttostr(Slot),CheckedGames);
        Ini.WriteString('games','shortcuts'+inttostr(Slot),CheckedHashes);
      end;
  finally
    Ini.Free;
    GL.Free;
  end;
  SaveSlot := True;
end;

function SaveConfig(XMLEdit:String; GamesEdit:String; NANDChecked: Boolean; LastFavFolder:String; ViewStyle:Integer; ViewSelected:Boolean; ViewGameInfo:Boolean; HomeName:String; Tree:TTreeView):Boolean;
var
  Ini: TIniFile;
  GL: TStringList;
  AppPath,CheckedGames,CheckedHashes: String;
  i: integer;
begin
  AppPath := ExtractFilePath(ParamStr(0));
  Ini := TIniFile.Create(AppPath+'config.ini');
  try
    CheckedGames := '';
    Ini.WriteString('config','XMLFile',XMLEdit);
    Ini.WriteString('config','gamesfolder',GamesEdit);
    Ini.WriteBool('config','nand',NANDChecked);
    Ini.WriteString('config','lastfav',LastFavFolder);
    Ini.WriteInteger('config','view',ViewStyle);
    Ini.WriteBool('config','viewselect',ViewSelected);
    Ini.WriteBool('config','viewgameinfo',ViewGameInfo);
    Ini.WriteString('config','home',HomeName);
    GL := TStringList.Create;
    GetCheckedCodes(Tree,GL);
    for i := 0 to (GL.Count-1) do
      begin
        CheckedGames := CheckedGames+' '+GL[i];
      end;
    CheckedGames := StringReplace(Trim(CheckedGames),' ',',',[rfReplaceAll]);
    GetCheckedHashes(Tree,GL);
    for i := 0 to (GL.Count-1) do
      begin
        CheckedHashes := CheckedHashes+' '+GL[i];
      end;
    CheckedHashes := StringReplace(Trim(CheckedHashes),' ',',',[rfReplaceAll]);
    Ini.WriteString('config','checked',CheckedGames);
    Ini.WriteString('config','shortcuts',CheckedHashes);
  finally
    Ini.Free;
    GL.Free;
  end;
  SaveConfig := True;
end;

function LoadConfig(XMLEdit:TFilenameEdit; GamesEdit:TDirectoryEdit; GameTree: TTreeView; TreeView:TTreeView; var NANDCheckBox:TCheckBox; var ViewStyle:Integer; var ViewSelected:Boolean; var ViewGameInfo:Boolean; var LastFavFolder:String; var HomeName:String):Boolean;
var
  Ini: TIniFile;
  ChkStringList,SlotList: TStringList;
  AppPath,CheckedGames,CheckedHashes,SlotName: String;
  i: integer;
  tm,tm2,tm3: TMenuItem;
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
    CheckedHashes := Ini.ReadString('config','shortcuts','');
    ViewStyle := Ini.ReadInteger('config','view',1);
    ViewSelected := Ini.ReadBool('config','viewselect',False);
    ViewGameInfo := Ini.ReadBool('config','viewgameinfo',True);
    HomeName := Ini.ReadString('config','home','HOME');

    NANDCheckBox.Checked := Ini.ReadBool('config','nand',False);
    if (NANDCheckBox.Checked) then
      GamesEdit.Enabled:=False
    else
      GamesEdit.Enabled:=True;
    if (FileExists(XMLEdit.Caption)) then
      begin
        XML2Tree(TreeView,XMLEdit.Caption,False,HomeName);
        XML2Tree(GameTree,XMLEdit.Caption,True,HomeName);
      end;
    ChkStringList.Clear;
    ChkStringList.Delimiter := ',';
    ChkStringList.StrictDelimiter := True;
    ChkStringList.DelimitedText := CheckedGames+','+CheckedHashes;
    ChkStringList.Sort;
    ChkStringList.Sorted := True;
    PopulatePathData(GameTree,GamesEdit.Caption);
    PopulateFolderData(GameTree,GamesEdit.Caption);
    PopulateFolderData(TreeView,GamesEdit.Caption);
    PopulateShortcuts(GameTree,GamesEdit.Caption);
    CheckNodesList(GameTree,ChkStringList);
  finally
    Ini.Free;
    ChkStringList.Free;
  end;
  LoadConfig := True;
end;

end.

