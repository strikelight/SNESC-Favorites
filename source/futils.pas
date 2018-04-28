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
  Classes, SysUtils, RegExpr, Dialogs, FileUtil, ComCtrls, Forms, VirtualTrees,
  ui_utils;

function GetLastFolderNumber(Path: String; SubFolder: Boolean = False):String;
function GenerateFolderName(Number: String; SubFolder: Boolean = False):String;
function CreateFaveLinks(ConsolePath: String; GamesList: TVirtualStringTree; ProgressBar: TProgressBar; StatusBar: TStatusBar; var FolderSuff: String):Boolean;
function IsGameChecked(CheckedGames:String; GameCode:String):Boolean;
function SaveShortcuts(Tree: TVirtualStringTree; Path: String; ProgressBar: TProgressBar; StatusBar: TStatusBar):Boolean;
function FindTextInFiles(TxtMatch: String; FileMatch: String; Path: String; var ResList: TStringList):Boolean;
procedure DeleteShortcuts(Tree: TVirtualStringTree; SelectedOnly: Boolean = False);
procedure VTreeToXML(Tree: TVirtualStringTree; Filename: String);

implementation

procedure VTreeToXML(Tree: TVirtualStringTree; Filename: String);
var
  lNode,lNode2: PVirtualNode;
  Data: TStringList;

  procedure nProcessNode(Node: PVirtualNode);
  var
    GData: PTreeData;
    cNode,cNode2: PVirtualNode;
    FName,indent: String;
    i: integer;
  begin
    if (Node = nil) then exit;
    indent := '';
    for i := 0 to Tree.GetNodeLevel(Node) do
      indent := indent + '  ';
    GData := Tree.GetNodeData(Node);
    FName := StringReplace(GData^.Name,'&','&amp;',[rfReplaceAll]);
    if (GData^.FType = 'Folder') and (Node <> Tree.GetFirst()) then
      Data.Add(indent+'<Folder name="'+FName+'" icon="'+GData^.Icon+'" position="3">');

    if (GData^.FType = 'Game') then
      Data.Add(indent+'<Game code="'+GData^.Code+'" name="'+FName+'" />');

    // Goes to the child node
    cNode := Tree.GetFirstChild(Node);

    // Processes all child nodes
    while cNode <> nil do
      begin
        cNode2 := Tree.GetNextSibling(cNode);
        nProcessNode(cNode);
        cNode := cNode2;
      end;

    if (GData^.FType = 'Folder') and (Node <> Tree.GetFirst()) then
      Data.Add(indent+'</Folder>');
  end;

begin
  lNode := Tree.GetFirst();
  if (lNode = nil) then exit;
  Data := TStringList.Create;
  try
    Data.Add('<?xml version="1.0" encoding="utf-16"?>');
    Data.Add('<Tree>');
    while (lNode <> nil) do
      begin
        lNode2 := Tree.GetNextSibling(lNode);
        nProcessNode(lNode);
        lNode := lNode2;
      end;
    Data.Add('</Tree>');
    Data.SaveToFile(Filename);
  finally
    Data.Free;
  end;
end;

procedure DeleteShortcuts(Tree: TVirtualStringTree; SelectedOnly: Boolean = False);
var
  lNode,lNode2: PVirtualNode;

  procedure nProcessNode(Node: PVirtualNode);
  var
    GData: PTreeData;
    cNode,cNode2: PVirtualNode;
    Res,Process: Boolean;
  begin
    if (Node = nil) then exit;
    Process := False;
    GData := Tree.GetNodeData(Node);

    if (GData^.FType = 'Shortcut') and (not SelectedOnly) then
      Process := True;
    if (GData^.FType = 'Shortcut') and (SelectedOnly) and (VNodeChecked(Tree,Node)) then
      Process := True;

    if Process then
      begin
        if DirectoryExists(GData^.FilePath) then
          begin
            Res := DeleteDirectory(GData^.FilePath,True);
            if Res then Res := RemoveDir(GData^.FilePath)
          end;
        cNode := Tree.GetFirstChild(Node);
        Tree.DeleteNode(Node);
      end
    else
      cNode := Tree.GetFirstChild(Node);

    // Processes all child nodes
    while cNode <> nil do
      begin
        cNode2 := Tree.GetNextSibling(cNode);
        nProcessNode(cNode);
        cNode := cNode2;
      end;
  end;

begin
  lNode := Tree.GetFirst();
  if (lNode = nil) then exit;
  while (lNode <> nil) do
    begin
      lNode2 := Tree.GetNextSibling(lNode);
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

function FindTextInFiles(TxtMatch: String; FileMatch: String; Path: String; var ResList: TStringList):Boolean;
var
  FList,FData: TStringList;
  i: integer;
begin
  FindTextInFiles := False;
  FList := TStringList.Create;
  try
    FindAllFiles(FList,Path,FileMatch,True,faDirectory);
    for i := 0 to FList.Count-1 do
      begin
        Application.ProcessMessages;
        FData := TStringList.Create;
        FData.LineBreak:=#10;
        try
          FData.LoadFromFile(FList[i]);
          if (Pos(TxtMatch,FData.Text) <> 0) then
            begin
              FindTextInFiles := True;
              ResList.Add(FList[i]);
            end;
        finally
          FData.Free;
        end;
      end;
  finally
    FList.Free;
  end;
end;

function SaveShortcuts(Tree: TVirtualStringTree; Path: String; ProgressBar: TProgressBar; StatusBar: TStatusBar):Boolean;
var
  lNode,lNode2: PVirtualNode;
  StatusPanel: TStatusPanel;

  procedure nProcessNode(Node: PVirtualNode);
  var
    GData,FData: PTreeData;
    pNode,cNode,cNode2: PVirtualNode;
    TmList,TmList2,sList,EmptyFile: TStringList;
    h,a: integer;
    nPath,sPath,Suffix: String;
    err: Boolean;
  begin
    if (Node = nil) then exit;
  // find all games in the tree
    GData := Tree.GetNodeData(Node);
    if (GData^.FType = 'Game') then // and (VNodeChecked(Tree,Node)) then
      begin
        pNode := Node;
        sList := TStringList.Create;
        try
      // get the source path
          sPath := '';
          FindAllFiles(sList,Path,GData^.Code+'.desktop',True,faDirectory);
          if (sList <> nil) and (sList.Count > 0) then
            begin
              sPath := ExtractFilePath(sList[0]);
            end;
        finally
          sList.Free;
        end;
        TmList := TStringList.Create;
        try
      // create a list of folder name's that lead to path of the game
          while (Assigned(Tree.NodeParent[pNode])) do
            begin
              pNode := Tree.NodeParent[pNode];
              FData := Tree.GetNodeData(pNode);
              if (FData^.FType = 'Folder') and (FData^.Code = 'HOME') then
                TmList.Insert(0,'---HOME---')
              else
                TmList.Insert(0,FData^.Name);
            end;
      // parse the list to find the actual file path that will be used to save shortcut
          if (TmList <> nil) and (TmList.Count > 0) then
            begin
              nPath := Path;
              Suffix := '';
              err := False;
              for h := 0 to TmList.Count-1 do
                begin
                  TmList2 := TStringList.Create;
                  try
                    if (TmList[h] = '---HOME---') then
                      begin
                        Suffix := '000';
                        nPath := Path+'\'+Suffix;
                      end
                    else
                      begin
                        if (not FindTextInFiles('Name='+TmList[h]+#10,'*.desktop',nPath,TmList2)) then
                          err := True;
                        if (TmList2 <> nil) and (TmList2.Count > 0) then
                          begin
                            a := Pos('CLV-S-00',TmList2[0])+8;
                            Suffix := Copy(TmList2[0],a,3);
                            nPath := Path+'\'+Suffix;
                          end;
                      end;
                  finally
                    TmList2.Free;
                  end;
                  if err then break;
                end;
              if (not DirectoryExists(nPath+'\'+GData^.Code)) then
                begin
                  CreateDir(nPath+'\'+GData^.Code);
                  Copyfile(sPath+GData^.Code+'.desktop',nPath+'\'+GData^.Code+'\'+GData^.Code+'.desktop');
                  EmptyFile := TStringList.Create;
                  try
                    EmptyFile.LineBreak := #10;
                    EmptyFile.Add('');
                    EmptyFile.SaveToFile(nPath+'\'+GData^.Code+'\'+'.sht');
                  finally
                    EmptyFile.Free;
                  end;
                end;
            end;
        finally
          TmList.Free;
        end;
        ProgressBar.Position:=ProgressBar.Position+1;
        StatusPanel.Text := Inttostr(Round(ProgressBar.Position/ProgressBar.Max*100))+'%';
        StatusBar.Update;
      end;

    // Goes to the child node
    cNode := Tree.GetFirstChild(Node);

    // Processes all child nodes
    while cNode <> nil do
      begin
        cNode2 := Tree.GetNextSibling(cNode);
        nProcessNode(cNode);
        cNode := cNode2;
      end;
  end;

begin
  SaveShortcuts := False;
  lNode := Tree.GetFirst();
  if (lNode = nil) then exit;

  ProgressBar.Visible := True;
  StatusPanel := StatusBar.Panels.Items[1];
  ProgressBar.Max := VGameCount(Tree);
  ProgressBar.Position:=0;

  while (lNode <> nil) do
    begin
      lNode2 := Tree.GetNextSibling(lNode);
      nProcessNode(lNode);
      lNode := lNode2;
    end;
  SaveShortcuts := True;
end;

function IsGameChecked(CheckedGames:String; GameCode:String):Boolean;
var
  ChkStringList: TStringList;
  i: integer;
  Res: Boolean;
begin
  Res := False;
  if (CheckedGames <> '') then
    begin
      ChkStringList := TStringList.Create;
      try
        ChkStringList.Clear;
        ChkStringList.Delimiter := ',';
        ChkStringList.StrictDelimiter := True;
        ChkStringList.DelimitedText := CheckedGames;
        for i := 0 to (ChkStringList.Count-1) do
          begin
            if (ChkStringList[i] = GameCode) then
              begin
                Res := True;
                break;
              end;
          end;
      finally
        ChkStringList.Free;
      end;
    end;
  IsGameChecked := Res;
end;

function GetLastFolderNumber(Path: String; SubFolder: Boolean = False):String;
var
  FD: TSearchRec;
  SL: TStringList;
  patt: String;
begin
  GetLastFolderNumber := '';
  if (SubFolder) then patt := 'CLV-S-*'
  else patt := '*';
  if (FindFirst(Path+patt,faDirectory,FD)=0) then
    begin
      SL := TStringList.Create;
      try
        repeat
          SL.Add(FD.Name);
        until (FindNext(FD) <> 0);
        SL.Sort;
        GetLastFolderNumber := SL[SL.Count-1];
        if (SubFolder) then
          begin
            GetLastFolderNumber := Copy(GetLastFolderNumber,7,5);
          end;
      finally
        SL.Free;
      end;
    end;
end;

function GenerateFolderName(Number: String; SubFolder: Boolean = False):String;
var
  num: integer;
begin
  if not TryStrToInt(Number,num) then
    begin
      GenerateFolderName := '';
      exit;
//      ShowMessage('Error: String ('+Number+') is not a valid number.');
    end;
  num := num+1;
  if (not SubFolder) then
    GenerateFolderName := Format('%.3d',[num])
  else
    GenerateFolderName := 'CLV-S-'+Format('%.5d',[num]);
end;

function CreateFaveLinks(ConsolePath: String; GamesList: TVirtualStringTree; ProgressBar: TProgressBar; StatusBar: TStatusBar; var FolderSuff: String):Boolean;
var
  GL,DeskFile,EmptyFile,FindLink: TStringList;
  StatusPanel: TStatusPanel;
  FPath,t: String;
  j: Integer;
  Res: Boolean;
begin

  t := GenerateFolderName(FolderSuff);
  if (t = '') then exit;

  FPath := ExtractFilePath(ParamStr(0));
  DeskFile := TStringList.Create;
  EmptyFile := TStringList.Create;
  ProgressBar.Visible := True;
  DeskFile.LineBreak := #10; // Hakchi kernel will give c8 errors for Dos LineFeeds

  StatusPanel := StatusBar.Panels.Items[1];
  try
    GL := TStringList.Create;
    VGetCheckedCodes(GamesList,GL);
    ProgressBar.Max := GL.Count+3;
    ProgressBar.Position:=0;
  finally
  end;
  StatusPanel.Text := Inttostr(Round(ProgressBar.Position/ProgressBar.Max*100))+'%';
  StatusBar.Update;
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
            t := GenerateFolderName(t);
          until (not DirectoryExists(ConsolePath+t));
          FolderSuff := t;
        end;
    end;
  ProgressBar.Position:=ProgressBar.Position+1;
  StatusPanel.Text := Inttostr(Round(ProgressBar.Position/ProgressBar.Max*100))+'%';
  StatusBar.Update;

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
  ProgressBar.Position:=ProgressBar.Position+1;
  StatusPanel.Text := Inttostr(Round(ProgressBar.Position/ProgressBar.Max*100))+'%';
  StatusBar.Update;

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
  ProgressBar.Position:=ProgressBar.Position+1;
  StatusPanel.Text := Inttostr(Round(ProgressBar.Position/ProgressBar.Max*100))+'%';
  StatusBar.Update;

  try
    for j := 0 to GL.Count-1 do
      begin
        try
          FindLink := TStringList.Create;
          FindAllFiles(FindLink,ConsolePath,GL[j]+'.desktop',True);
          if (FindLink.Count > 0) then
            begin
              CreateDir(ConsolePath+FolderSuff+'\'+GL[j]);
              CopyFile(FindLink[0],ConsolePath+FolderSuff+'\'+GL[j]+'\'+GL[j]+'.desktop');
            end;
        finally
          FindLink.Free;
        end;
        ProgressBar.Position:=ProgressBar.Position+1;
        StatusPanel.Text := Inttostr(Round(ProgressBar.Position/ProgressBar.Max*100))+'%';
        StatusBar.Update;
      end;
  finally
    GL.Free;
    DeskFile.Free;
    EmptyFile.Free;
  end;
  CreateFaveLinks := True;
end;

end.

