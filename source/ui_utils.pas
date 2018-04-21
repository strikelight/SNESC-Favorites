{ ui_utils.pas
  Description: Routines for manipulating the
               User Interface of the applicaation


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

unit ui_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLtype, LCLIntf, Graphics, ComCtrls,
  laz2_xmlread, laz2_dom, Themes, Types, Dialogs, FileUtil,
  crc, RegExpr, VirtualTrees;

type
  PTreeData = ^TTreeData;
  TTreeData = record
    Name: String;
    FType: String;
    FilePath: String;
    Icon: String;
    Hash: String;
    Code: String;
    BelongsTo: PVirtualNode;
    TopParent: PVirtualNode;
  end;


procedure VTreeViewDrawItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
function VGetCheckCount(Tree: TVirtualStringTree; StatusPanel: TStatusPanel; Update:Boolean):Integer;
procedure XML2VTree(Tree: TVirtualStringTree; Filename: String; UseGames: Boolean = False; HomeName: String = 'HOME';
                   ViewStyle: Integer = 2);
function SetNodeData(Tree: TVirtualStringTree; Node: PVirtualNode; FName: String; FType: String; FilePath: String;
                      Code: String; FIcon: String; BelongsTo: PVirtualNode):PTreeData;
function FindVNodeData(Tree: TVirtualStringTree; PNode: PVirtualNode; Search: String; Field: Integer; Children: Boolean = True):PVirtualNode;
procedure CheckVNode(Tree: TVirtualStringTree; Node: PVirtualNode; Checked:boolean);
procedure ToggleVTreeViewCheckBoxes(Tree: TVirtualStringTree; Node: PVirtualNode);
function VNodeChecked(Tree: TVirtualStringTree; ANode:PVirtualNode): Boolean;
procedure VMoveChildrenToTopParent(TreeView: TVirtualStringTree);
procedure VMoveChildrenToParent(TreeView: TVirtualStringTree);
procedure VMoveChildrenToTop(TreeView: TVirtualStringTree);
procedure VSelectionView(Tree: TVirtualStringTree; nSelected: Boolean);
procedure VShortcutsView(Tree: TVirtualStringTree; nSelected: Boolean);
procedure VClearSelections(Tree: TVirtualStringTree);
procedure VGetCheckedCodes(Tree: TVirtualStringTree; var List: TStringList);
procedure VGetCheckedHashes(Tree: TVirtualStringTree; var List: TStringList);
procedure CheckVNodesList(Tree: TVirtualStringTree; ChkList: TStringList);
procedure VAddShortcutSelections(Source: TVirtualStringTree; Dest: TVirtualStringTree);
procedure VPopulateShortcuts(Tree: TVirtualStringTree; Path: String);
procedure VPopulatePathData(Tree: TVirtualStringTree; Path: String);
procedure VPopulateFolderData(Tree: TVirtualStringTree; Path: String);
procedure VClearShortcuts(Tree: TVirtualStringTree);
//function FindNextNodeMatch(c: Char; Tree: TTreeView; StartNode: TTreeNode):TTreeNode;
//function VFindNextNodeMatch(c: Char; Tree: TTreeView; StartNode: TTreeNode):TTreeNode;
procedure VUpdateFolderPathData(Tree: TVirtualStringTree; Path: String; ChildFolder: String; ParentFolder: String);
function VGameCount(Tree: TVirtualStringTree):Integer;
function VGetHash(Tree: TVirtualStringTree; GData: PTreeData):String;
function HashToVNode(Tree: TVirtualStringTree; Hash: String):PVirtualNode;
procedure debuglog(Text: String);

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

procedure VAddShortcutSelections(Source: TVirtualStringTree; Dest: TVirtualStringTree);
var
  lNode,lNode2: PVirtualNode;

  procedure nProcessNode(Node: PVirtualNode);
  var
    GData: PTreeData;
    nNode,sNode,cNode,cNode2: PVirtualNode;
  begin
    if (Node = nil) then exit;

    GData := Source.GetNodeData(Node);
    sNode := Dest.GetFirstSelected();
    if (GData^.FType = 'Game') and VNodeChecked(Source,Node) and
    (FindVNodeData(Dest,sNode,'Folder',2,False) = sNode) and
    (FindVNodeData(Dest,sNode,GData^.Code,1,False) = nil) then
      begin
        Dest.Expanded[sNode] := True;
        nNode := Dest.AddChild(sNode);
        SetNodeData(Dest,nNode,GData^.Name,'Game',GData^.FilePath,GData^.Code,GData^.Icon,sNode);
      end;

    // Goes to the child node
    cNode := Source.GetFirstChild(Node);

    // Processes all child nodes
    while cNode <> nil do
      begin
        cNode2 := Source.GetNextSibling(cNode);
        nProcessNode(cNode);
        cNode := cNode2;
      end;
  end;

begin
  if (Source.GetFirst() = nil) or (Dest.GetFirstSelected() = nil) then exit;
  lNode := Source.GetFirst();
  while (lNode <> nil) do
    begin
      lNode2 := Source.GetNextSibling(lNode);
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

procedure CheckVNodesList(Tree:TVirtualStringTree; ChkList: TStringList);
var
  lNode,lNode2: PVirtualNode;

  procedure nProcessNode(Node: PVirtualNode);
  var
    GData: PTreeData;
    cNode,cNode2: PVirtualNode;
  begin
    if (Node = nil) then exit;

    GData := Tree.GetNodeData(Node);
    if (GData^.FType = 'Game') and (ChkList.IndexOf(GData^.Code) > -1) then
      begin
        CheckVNode(Tree, node, true);
//        CheckNode(node, true);
      end
    else if (GData^.FType = 'Shortcut') and (ChkList.IndexOf(GData^.Hash) > -1) then
      begin
        CheckVNode(Tree, node, true);
//        CheckNode(node, true);
      end
    else
      begin
        CheckVNode(Tree, node, false);
//        CheckNode(node, false);
      end;

    // Goes to the child node
    cNode := Tree.GetFirstChild(Node);
//    cNode := Node.GetFirstChild;

    // Processes all child nodes
    while cNode <> nil do
      begin
        cNode2 := Tree.GetNextSibling(cNode);
        nProcessNode(cNode);
        cNode := cNode2;
      end;
  end;

begin
//  if (Tree.Items.Count = 0) then exit;
  lNode := Tree.GetFirst();
  ChkList.Sorted:=True;
  while (Assigned(lNode)) do
    begin
      lNode2 := Tree.GetNextSibling(lNode);
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

function VGameCount(Tree: TVirtualStringTree):Integer;
var
  lNode,lNode2: PVirtualNode;
  total: Integer;

  procedure nProcessNode(Node: PVirtualNode);
  var
    GData: PTreeData;
    cNode,cNode2: PVirtualNode;
  begin
    if (Node = nil) then exit;

    GData := Tree.GetNodeData(Node);
    if (GData^.FType = 'Game') then inc(Total);

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
  total := 0;
  if (Tree.GetFirst() = nil) then
    begin
      VGameCount := total;
      exit;
    end;
  lNode := Tree.GetFirst();
  while (lNode <> nil) do
    begin
      lNode2 := Tree.GetNextSibling(lNode);
      nProcessNode(lNode);
      lNode := lNode2;
    end;
  VGameCount := total;
end;

procedure VGetCheckedCodes(Tree: TVirtualStringTree; var List: TStringList);
var
  lNode,lNode2: PVirtualNode;

  procedure nProcessNode(Node: PVirtualNode);
  var
    GData: PTreeData;
    cNode,cNode2: PVirtualNode;
  begin
    if (Node = nil) then exit;

    GData := Tree.GetNodeData(Node);
    if (GData^.FType = 'Game') and (VNodeChecked(Tree,Node)) then
      List.Add(GData^.Code);

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
  List.Clear;
  if (Tree.GetFirst() = nil) then exit;
  lNode := Tree.GetFirst();
  while (lNode <> nil) do
    begin
      lNode2 := Tree.GetNextSibling(lNode);
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

procedure VGetCheckedHashes(Tree: TVirtualStringTree; var List: TStringList);
var
  lNode,lNode2: PVirtualNode;

  procedure nProcessNode(Node: PVirtualNode);
  var
    GData: PTreeData;
    cNode,cNode2: PVirtualNode;
  begin
    if (Node = nil) then exit;

    GData := Tree.GetNodeData(Node);
    if (GData^.FType = 'Shortcut') and (VNodeChecked(Tree,Node)) then
      List.Add(GData^.Hash);

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
  List.Clear;
  if (Tree.GetFirst() = nil) then exit;
  lNode := Tree.GetFirst();
  while (lNode <> nil) do
    begin
      lNode2 := Tree.GetNextSibling(lNode);
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

{ incremental search built into vtree

function FindNextNodeMatch(c: Char; Tree: TTreeView; StartNode: TTreeNode):TTreeNode;
var
  rNode,lNode,lNode2: TTreeNode;
  Start,Wrap: Boolean;

  procedure nProcessNode(Node: TTreeNode);
  var
    cNode,cNode2: TTreeNode;
    GData: TGameData;
  begin
    if (Node = nil) or (rNode <> nil) or ((Wrap) and (Node = StartNode)) then exit;
    if (Node = StartNode) then Start := True;

    GData := TGameData(Node.Data);
    if (Start) and (GData.FType = 'Game') and (LowerCase(Node.Text[1]) = LowerCase(c)) and (Node <> StartNode) then
      begin
        rNode := Node;
        exit;
      end;

    // Goes to the child node
    cNode := Node.GetFirstChild;

    // Processes all child nodes
    while cNode <> nil do
      begin
        cNode2 := cNode.GetNextSibling;
        nProcessNode(cNode);
        cNode := cNode2;
      end;
  end;

begin
  FindNextNodeMatch := nil;
  rNode := nil;
  Wrap := False;
  if (Tree.Items.Count = 0) or (StartNode = nil) then exit;
  Start := False;
  Tree.AlphaSort;
  lNode := Tree.Items[0];
  while (lNode <> nil) and (rNode = nil) do
    begin
      lNode2 := lNode.GetNextSibling;
      nProcessNode(lNode);
      lNode := lNode2;
    end;
  if (rNode = nil) then
    begin
      Wrap := True;
      lNode := Tree.Items[0];
      while (lNode <> nil) and (rNode = nil) do
        begin
          lNode2 := lNode.GetNextSibling;
          nProcessNode(lNode);
          lNode := lNode2;
        end;
    end;
  FindNextNodeMatch := rNode;
end;
}

procedure VClearShortcuts(Tree: TVirtualStringTree);
var
  lNode,lNode2: PVirtualNode;

  procedure nProcessNode(Node: PVirtualNode);
  var
    GData: PTreeData;
    cNode,cNode2: PVirtualNode;
  begin
    if (Node = nil) then exit;

    // Goes to the child node
    cNode := Tree.GetFirstChild(Node);

    GData := Tree.GetNodeData(Node);
    if (GData^.FType = 'Game') then
      Tree.DeleteNode(Node);

    // Processes all child nodes
    while cNode <> nil do
      begin
        cNode2 := Tree.GetNextSibling(cNode);
        nProcessNode(cNode);
        cNode := cNode2;
      end;
  end;

begin
  if (Tree.GetFirst() = nil) then exit;
  lNode := Tree.GetFirst();
  while (lNode <> nil) do
    begin
      lNode2 := Tree.GetNextSibling(lNode);
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

procedure VClearSelections(Tree: TVirtualStringTree);
var
  lNode,lNode2: PVirtualNode;

  procedure nProcessNode(Node: PVirtualNode);
  var
    GData: PTreeData;
    cNode,cNode2: PVirtualNode;
  begin
    if (Node = nil) then exit;

    GData := Tree.GetNodeData(Node);
    if (GData^.FType <> 'Folder') and (VNodeChecked(Tree,Node)) then
      ToggleVTreeViewCheckBoxes(Tree,Node);

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
  if (Tree.GetFirst() = nil) then exit;
  lNode := Tree.GetFirst();
  while (lNode <> nil) do
    begin
      lNode2 := Tree.GetNextSibling(lNode);
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

procedure VSelectionView(Tree: TVirtualStringTree; nSelected: Boolean);
var
  lNode,lNode2: PVirtualNode;

  procedure nProcessNode(Node: PVirtualNode);
  var
    GData: PTreeData;
    cNode,cNode2: PVirtualNode;
  begin
    if (Node = nil) then exit;

    GData := Tree.GetNodeData(Node);
    if (GData^.FType = 'Game') or (GData^.FType = 'Shortcut') then
      begin
        if (not VNodeChecked(Tree,Node)) and (nSelected) then
          Tree.IsVisible[Node] := False
        else if (VNodeChecked(Tree,Node)) and (nSelected) then
          Tree.IsVisible[Node] := True
        else if (not VNodeChecked(Tree,Node)) and (not nSelected) then
          Tree.IsVisible[Node] := True;
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
  if (Tree.GetFirst() = nil) then exit;
  lNode := Tree.GetFirst();
  while (lNode <> nil) do
    begin
      lNode2 := Tree.GetNextSibling(lNode);
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

procedure VShortcutsView(Tree: TVirtualStringTree; nSelected: Boolean);
var
  lNode,lNode2: PVirtualNode;

  procedure nProcessNode(Node: PVirtualNode);
  var
    GData: PTreeData;
    cNode,cNode2: PVirtualNode;
  begin
    if (Node = nil) then exit;

    GData := Tree.GetNodeData(Node);
    if (nSelected) and (GData^.FType = 'Shortcut') then
      begin
        Tree.IsVisible[Node] := True;
      end
    else if (nSelected) and (GData^.FType <> 'Shortcut') and (GData^.FType <> 'Folder') then
      begin
        Tree.IsVisible[Node] := False;
      end
    else if (not nSelected) and (GData^.FType <> 'Folder') then
      begin
        Tree.IsVisible[Node] := True;
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
  if (Tree.GetFirst() = nil) then exit;
  lNode := Tree.GetFirst();
  while (lNode <> nil) do
    begin
      lNode2 := Tree.GetNextSibling(lNode);
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

procedure CheckVNode(Tree: TVirtualStringTree; Node: PVirtualNode; Checked:boolean);
begin
  if Assigned(Node) then
    if Checked then
      begin
        Tree.CheckState[Node] := csCheckedNormal;
      end
    else
      begin
        Tree.CheckState[Node] := csUncheckedNormal;
      end;
end;

procedure ToggleVTreeViewCheckBoxes(Tree: TVirtualStringTree; Node: PVirtualNode);
begin
  if Assigned(Node) then
    begin
      if Tree.CheckState[Node] = csUncheckedNormal then
        Tree.CheckState[Node] := csCheckedNormal
      else if Tree.CheckState[Node] = csCheckedNormal then
        Tree.CheckState[Node] := csUncheckedNormal;
    end;
end;

function VNodeChecked(Tree:TVirtualStringTree; ANode:PVirtualNode): Boolean;
begin
  if (Tree.CheckState[ANode] = csUncheckedNormal) then result := False
  else result := True;
end;

function VGetHash(Tree: TVirtualStringTree; GData: PTreeData):String;
var
  crcval: longword;
  HString: String;
  dData: PTreeData;
begin
  crcval := crc32(0,nil,0);
  HString := GData^.Name+GData^.FType+GData^.Code+GData^.FilePath;
  if (GData^.BelongsTo <> nil) then
    begin
      dData := Tree.GetNodeData(GData^.BelongsTo);
      HString := HString+dData^.Name;
    end;
  if (GData^.TopParent <> nil) then
    begin
      dData := Tree.GetNodeData(GData^.TopParent);
      HString := HString+dData^.Name;
    end;
  Result := inttohex(crc32(crcval,@HString[1],length(HString)),8);
end;

function SetNodeData(Tree: TVirtualStringTree; Node: PVirtualNode; FName: String; FType: String; FilePath: String;
                      Code: String; FIcon: String;
                      BelongsTo: PVirtualNode):PTreeData;
var
  Data: PTreeData;
  rNode: PVirtualNode;
begin
  Data := Tree.GetNodeData(Node);
  Data^.Name:=FName;
  Data^.FType:=FType;
  Data^.Code:=Code;
  Data^.Icon:=FIcon;
  Data^.FilePath:=FilePath;
  if (BelongsTo = nil) then BelongsTo := Node;
  rNode := BelongsTo;
  while (rNode <> nil) and (Tree.NodeParent[rNode] <> nil) do
    rNode := Tree.NodeParent[rNode];
  Data^.BelongsTo:=BelongsTo;
  Data^.TopParent:=rNode;
  Data^.Hash := VGetHash(Tree,Data);
  Result := Data;
end;

procedure VPopulateShortcuts(Tree: TVirtualStringTree; Path: String);
var
  FList: TStringList;
  lNode,lNode2,mNode,mNode2,fNode,fNode2,nNode: PVirtualNode;
  i: Integer;
  fpath,GameCode: String;
  GData2: PTreeData;
begin
  if (Trim(Path) = '') or (Tree.GetFirst() = nil) then exit;
  FList := TStringList.Create;
  try
    FindAllFiles(FList,Path+'\','.sht',True,faDirectory);
    for i := 0 to FList.Count-1 do
      begin
        fpath := ExtractFilePath(FList[i]);
        fpath := Copy(FList[i],Length(Path)+2,3);
        if (fpath[length(fpath)] = '\') then
          fpath := Copy(fpath,1,length(fpath)-1);
        fpath := Path+'\'+fpath;
        GameCode := Copy(FList[i],Length(fpath)+2,11);
        lNode := Tree.GetFirst();
        while (lNode <> nil) do
          begin
            lNode2 := Tree.GetNextSibling(lNode);
            fNode := nil;
            fNode := FindVNodeData(Tree,lNode,fpath,4);
            if (fNode <> nil) then
              begin
                fNode2 := nil;
                mNode := Tree.GetFirst();
                while (mNode <> nil) do
                  begin
                    mNode2 := Tree.GetNextSibling(mNode);
                    fNode2 := FindVNodeData(Tree,mNode,GameCode,1);
                    if (fNode2 <> nil) then mNode := nil
                    else mNode := mNode2;
                  end;
                if (fNode2 <> nil) and (FindVNodeData(Tree,fNode,GameCode,1,False) = nil) then
                  begin
                    lNode2 := nil;
                    nNode := Tree.AddChild(fNode);
                    GData2 := Tree.GetNodeData(fNode2);
                    SetNodeData(Tree,nNode,GData2^.Name,'Shortcut',
                             ExtractFilePath(FList[i]),GameCode,'',fNode);
                    CheckVNode(Tree,nNode,False);
                  end;
              end;
            lNode := lNode2;
          end;
      end;

  finally
    FList.Free;
  end;
end;

procedure VPopulateFolderData(Tree: TVirtualStringTree; Path: String);
var
  parent,children,foldername: TStringList;
  fname,ele,folder,subfolder: String;
  i,j: integer;
  re1: TRegExpr;
  FList,FList2,FData,TList: TStringList;

  procedure nProcess(folder: String);
  var
    k: integer;
    cList: TStringList;
    nfolder: String;
    GData: PTreeData;
  begin
    cList := TStringList.Create;
    try
      cList.Delimiter:=' ';
      cList.DelimitedText:=Trim(children.Values[folder]);
      if (folder = '000') then
        begin
          GData := Tree.GetNodeData(Tree.GetFirst());
          GData^.FilePath := Path+'\000';
          GData^.TopParent := Tree.GetFirst();
          GData^.Hash := VGetHash(Tree,GData);
        end;
      for k := 0 to cList.Count-1 do
        begin
          nfolder := cList[k];
          VUpdateFolderPathData(Tree,Path+'\'+nfolder,foldername.Values[nfolder],foldername.Values[parent.Values[nfolder]]);
          nProcess(nfolder);
        end;
    finally
      cList.Free;
    end;
  end;

begin
  if ((Path = '') or (not DirectoryExists(Path)) or (Tree.GetFirst() = nil)) then exit;

  FList := TStringList.Create;
  parent := TStringList.Create;
  children := TStringList.Create;
  foldername := TStringList.Create;
  try
    FindAllFiles(FList,Path+'\','CLV-S-*.desktop',True,faDirectory);
    if (FList.Count > 0) then
      for i := 0 to FList.Count-1 do
        begin
          folder := Copy(FList[i],Length(Path)+2,3);
          FList2 := TStringList.Create;
          try
            FindAllFiles(FList2,ExtractFilePath(FList[i]),'CLV-S-*.desktop',False,faDirectory);
            if (FList2.Count > 0) then
              for j := 0 to FList2.Count-1 do
                begin
                  TList := TStringList.Create;
                  try
                    TList.Delimiter:='\';
                    TList.DelimitedText:=FList2[j];
                    subfolder := TList[TList.Count-1];
                  finally
                    TList.Free;
                  end;
                  FData := TStringList.Create;
                  try
                    FData.LineBreak := #10;
                    FData.LoadFromFile(FList2[j]);
                    re1 := TRegExpr.Create('Name=(.[^'+#10+']*)');
                    if re1.Exec(FData.Text) then
                      begin
                        fname := re1.Match[1];
                        if (Pos('Back',fname) > 0) or (Pos('Home',fname) > 0) then continue;
                        re1.Expression := 'CLV-S-00(.*).desktop';
                        re1.Exec(subfolder);
                        ele := re1.Match[1];
                        parent.Values[ele] := folder;
                        children.Values[folder] := children.Values[folder]+' '+ele;
                        foldername.Values[ele] := fname;
                      end;
                  finally
                    FData.Free;
                  end;
                end;
          finally
            re1.Free;
            FList2.Free;
          end;
        end;
    nProcess('000');
  finally
    parent.Free;
    children.Free;
    foldername.Free;
    FList.Free;
  end;
end;

procedure VPopulatePathData(Tree: TVirtualStringTree; Path: String);
var
  FList: TStringList;
  i,a: integer;
  GData: PTreeData;
  lNode,lNode2,fNode: PVirtualNode;
  CodeString: String;
begin
  if ((Path = '') or (not DirectoryExists(Path)) or (Tree.GetFirst() = nil)) then exit;

  FList := TStringList.Create;
  try
    FindAllFiles(FList,Path,'*.desktop',True,faDirectory);
    if (FList.Count > 0) then
      for i := 0 to FList.Count-1 do
        begin
          a := Pos('CLV-',FList[i]);
          if (a > 0) then
            begin
              CodeString := Copy(FList[i],a,11);
              lNode := Tree.GetFirst();
              while (lNode <> nil) do
                begin
                  lNode2 := Tree.GetNextSibling(lNode);
                  fNode := nil;
                  fNode := FindVNodeData(Tree,lNode,CodeString,1);
                  if (fNode <> nil) then
                    begin
                      GData := Tree.GetNodeData(fNode);
                      if (GData^.FilePath = '') then
                        begin
                          GData^.FilePath := ExtractFilePath(FList[i]);
                          GData^.Hash := VGetHash(Tree,GData);
                        end;
                      break;
                    end;
                  lNode := lNode2;
                end;
            end;
        end;
  finally
    FList.Free;
  end;
end;

procedure VMoveChildrenToTop(TreeView: TVirtualStringTree);
var
  lNode,lNode2: PVirtualNode;

  procedure nProcessNode(Node:PVirtualNode);
  var
    rNode,cNode,cNode2: PVirtualNode;
    GData: PTreeData;
    AttachMode: TVTNodeAttachMode;
  begin
    if (Node = nil) then exit;
    GData := TreeView.GetNodeData(Node);
    if (GData^.FType = 'Folder') then
      begin
        TreeView.IsVisible[Node] := False;
      end;
    if (TreeView.GetNodeLevel(Node) > 0) and (GData^.FType <> 'Folder') then
      begin
        AttachMode := amAddChildLast;
        rNode := nil;
        TreeView.MoveTo(Node,rNode,AttachMode,False);
      end;

    // Goes to the child node
    cNode := TreeView.GetFirstChild(Node);

    // Processes all child nodes
    while cNode <> nil do
      begin
        cNode2 := TreeView.GetNextSibling(cNode);
        nProcessNode(cNode);
        cNode := cNode2;
      end;
  end;

begin
  if (TreeView.GetFirst() = nil) then exit;
  lNode := TreeView.GetFirst();
  while (lNode <> nil) do
    begin
      lNode2 := TreeView.GetNextSibling(lNode);
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

procedure VMoveChildrenToParent(TreeView: TVirtualStringTree);
var
  lNode,lNode2: PVirtualNode;

  procedure nProcessNode(Node: PVirtualNode);
  var
    rNode,cNode,cNode2: PVirtualNode;
    GData: PTreeData;
    AttachMode: TVTNodeAttachMode;
    t: Boolean;
  begin
    t := False;
    if (Node = nil) then exit;

    GData := TreeView.GetNodeData(Node);
    if (GData^.FType = 'Folder') then
      TreeView.IsVisible[Node] := True;
    if (TreeView.NodeParent[Node] <> nil) and (GData^.BelongsTo = TreeView.NodeParent[Node]) then t := True;
    if (not t) then
      begin
        if (TreeView.GetNodeLevel(Node) <= 1) and (GData^.FType <> 'Folder') then
          begin
            AttachMode := amAddChildLast; // First?
            rNode := GData^.BelongsTo;
            if (TreeView.IsVisible[GData^.BelongsTo] = False) then
              TreeView.IsVisible[GData^.BelongsTo] := True;
            TreeView.MoveTo(Node,rNode,AttachMode,False); // False?
//            Node.MoveTo(rNode,AttachMode);
          end;

    // Goes to the child node
        cNode := TreeView.GetFirstChild(Node);

    // Processes all child nodes
        while cNode <> nil do
        begin
          cNode2 := TreeView.GetNextSibling(cNode);
          nProcessNode(cNode);
          cNode := cNode2;
        end;
      end;
  end;

begin
  if (TreeView.GetFirst() = nil) then exit;
  lNode := TreeView.GetFirst();
  while (lNode <> nil) do
    begin
      lNode2 := TreeView.GetNextSibling(lNode);
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

procedure VMoveChildrenToTopParent(TreeView: TVirtualStringTree);
var
  lNode,lNode2: PVirtualNode;

  procedure nProcessNode(Node: PVirtualNode);
  var
    rNode,cNode,cNode2: PVirtualNode;
    GData: PTreeData;
    AttachMode: TVTNodeAttachMode;
    t: Boolean;
  begin
    t := False;
    if (Node = nil) then exit;

    GData := TreeView.GetNodeData(Node);
    if (GData^.FType = 'Folder') and (TreeView.GetNodeLevel(Node) = 0) then
      TreeView.IsVisible[Node] := True
    else if (GData^.FType = 'Folder') and (TreeView.GetNodeLevel(Node) > 0) then
      TreeView.IsVisible[Node] := False;
    if (Node^.Parent <> nil) and (GData^.TopParent = Node^.Parent) then t := True;
    if (Node = GData^.TopParent) then t := True;
    if (not t) then
      begin
        if (GData^.FType <> 'Folder') then
          begin
            rNode := GData^.TopParent;
            if (rNode = nil) then AttachMode := amAddChildFirst
            else AttachMode := amAddChildLast;
            TreeView.MoveTo(Node,rNode, AttachMode, False);
//            Node.MoveTo(rNode,AttachMode);
          end;
      end;
    // Goes to the child node
      cNode := TreeView.GetFirstChild(Node);

    // Processes all child nodes
      while cNode <> nil do
        begin
          cNode2 := TreeView.GetNextSibling(cNode);
          nProcessNode(cNode);
          cNode := cNode2;
        end;
  end;

begin
  if (TreeView.GetFirst() = nil) then exit;
  lNode := TreeView.GetFirst();
  while (lNode <> nil) do
    begin
      lNode2 := TreeView.GetNextSibling(lNode);
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

procedure VUpdateFolderPathData(Tree: TVirtualStringTree; Path: String; ChildFolder: String; ParentFolder: String);
var
  lNode,lNode2: PVirtualNode;

  procedure nProcessNode(Node: PVirtualNode);
  var
    cNode,cNode2: PVirtualNode;
    GData,GData2: PTreeData;
  begin
    if (Node = nil) then exit;
    ChildFolder := Trim(ChildFolder);
    ParentFolder := Trim(ParentFolder);

    GData := Tree.GetNodeData(Node);
    if (GData^.BelongsTo <> nil) then
      GData2 := Tree.GetNodeData(GData^.BelongsTo);
    if (GData^.FType = 'Folder') and (GData^.Name = ChildFolder) then
      begin
      if ((GData^.BelongsTo = nil) and (ParentFolder = '')) or
         ((GData2^.Name = GData^.Name) and (ParentFolder = '')) then
        begin
          GData^.FilePath := Path;
          GData^.Hash := VGetHash(Tree,GData);
          lNode2 := nil;
          exit;
        end
      else if (GData2^.Name = ParentFolder) and (GData^.FilePath = '') then
        begin
          GData^.FilePath := Path;
          GData^.Hash := VGetHash(Tree,GData);
          lNode2 := nil;
          exit;
        end;
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
  if (Tree.GetFirst() = nil) then exit;
  lNode := Tree.GetFirst();
  while (lNode <> nil) do
    begin
      lNode2 := Tree.GetNextSibling(lNode);
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

function HashToVNode(Tree: TVirtualStringTree; Hash: String):PVirtualNode;
var
  lNode,lNode2,rNode: PVirtualNode;

  procedure nProcessNode(Node: PVirtualNode);
  var
    cNode,cNode2: PVirtualNode;
    GData: PTreeData;
  begin
    if (Node = nil) then exit;

    GData := Tree.GetNodeData(Node);
    if (GData^.Hash = Hash) then
      begin
        rNode := Node;
        lNode2 := nil;
        exit;
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
  if (Tree.GetFirst() = nil) then exit;
  lNode := Tree.GetFirst();
  rNode := nil;
  while (lNode <> nil) and (rNode = nil) do
    begin
      lNode2 := Tree.GetNextSibling(lNode);
      nProcessNode(lNode);
      lNode := lNode2;
    end;
  Result := rNode;
end;

function FindVNodeData(Tree: TVirtualStringTree; PNode: PVirtualNode; Search: String; Field: Integer; Children: Boolean = True):PVirtualNode;
var
  rNode,cNode: PVirtualNode;
  GData,GData2: PTreeData;
begin
  // 0 = Name, 1 = Code, 2 = Game/Folder, 3 = Parent Node Text, 4 = FilePath, 5 = Hash
  FindVNodeData := nil;
  if (Tree.GetFirst() = nil) then exit;
  if (PNode = nil) then exit;

  GData := Tree.GetNodeData(PNode);
  if (Field = 0) then
    begin
      if GData^.Name = Search then
        begin
          FindVNodeData := PNode;
          exit;
        end;
    end
  else if (Field = 1) then
    begin
      if (GData^.Code <> '') and (GData^.Code = Search) then
        begin
          FindVNodeData := PNode;
          exit;
        end;
    end
  else if (Field = 2) then
    begin
      if GData^.FType = Search then
        begin
          FindVNodeData := PNode;
          exit;
        end;
    end
  else if (Field = 3) then
    begin
      if (GData^.BelongsTo <> nil) then
        begin
          GData2 := Tree.GetNodeData(GData^.BelongsTo);
          if (GData2^.Name = Search) then
            begin
              FindVNodeData := PNode;
              exit;
            end;
        end;
    end
  else if (Field = 4) then
    begin
      if (Trim(Lowercase(GData^.FilePath)) = Trim(Lowercase(Search))) then
        begin
          FindVNodeData := PNode;
          exit;
        end;
    end
  else if (Field = 5) then
    begin
      if (GData^.Hash = Search) then
        begin
          FindVNodeData := PNode;
          exit;
        end;
    end;

  // Goes to the child node
  cNode := Tree.GetFirstChild(PNode);

  // Processes all child nodes
  while cNode <> nil do
    begin
      GData := Tree.GetNodeData(cNode);
      if (Children or (GData^.FType <> 'Folder')) then
        begin
          rNode := FindVNodeData(Tree,cNode,Search,Field);
          if (rNode <> nil) then
            begin
              FindVNodeData := rNode;
              exit;
            end;
        end;
      cNode := Tree.GetNextSibling(cNode);
    end;
end;

procedure XML2VTree(Tree: TVirtualStringTree; Filename: String; UseGames: Boolean = False; HomeName: String = 'HOME';
                   ViewStyle: Integer = 2);
var
  XMLDoc: TXMLDocument;
  iNode: TDOMNode;
  FPath,hicon: String;
  FData: TStringList;
  GData: PTreeData;
  TreeNode: PVirtualNode;

    procedure nProcessNode(Node: TDOMNode; TreeNode: PVirtualNode);
    var
      cNode: TDOMNode;
      s,t,c,d: string;
      GDataT,GDataU: PTreeData;
      tNode,tNode2: PVirtualNode;
    begin
      if Node = nil then Exit; // Stops if reached a leaf

      // Adds a node to the tree
      if (Node.NodeName = 'Folder') then
        begin
          if Node.HasAttributes and (Node.Attributes.Length>0) then
            s := Node.Attributes[0].NodeValue
          else
            s := '';
          if Node.HasAttributes and (Node.Attributes.Length>1) then
            t := Node.Attributes[1].NodeValue
          else
            t := '';
          tNode2 := Tree.AddChild(TreeNode,nil);
          GData := SetNodeData(Tree,tNode2,s,'Folder','','',t,TreeNode);

          if (GData^.TopParent = nil) then GData^.TopParent := TreeNode;

          Tree.CheckState[tNode2] := csUncheckedNormal;

          if (ViewStyle = 1) and (Tree.GetNodeLevel(tNode2) > 0) then
            Tree.Expanded[tNode2] := False
          else
            Tree.Expanded[tNode2] := True;
//            TreeNode.Collapse(False)
//          else TreeNode.Expand(False);
        end
      else if UseGames then
        begin
          if Node.HasAttributes and (Node.Attributes.Length>1) then
            s := Node.Attributes[1].NodeValue
          else
            s := '';
          if Node.HasAttributes and (Node.Attributes.Length>0) then
            c := Node.Attributes[0].NodeValue
          else
            c := '';
          if (TreeNode = nil) then tNode := Tree.GetFirst()
          else tNode := TreeNode;

          tNode2 := Tree.AddChild(tNode,nil);
          GData := SetNodeData(Tree,tNode2,s,'Game','',c,'',tNode);

          Tree.CheckType[tNode2] := ctCheckbox;
          Tree.CheckState[tNode2] := csUncheckedNormal;
          if (GData^.Name = 'RetroArch') then
            begin
              if (GData^.BelongsTo = nil) then d := 'BelongsTo: nil'
              else
                begin
                  GDataT := Tree.GetNodeData(GData^.BelongsTo);
                  d := 'BelongsTo: '+GDataT^.Name;
                end;
              if (GData^.TopParent = nil) then d := d+' TopParent: nil'
              else
                begin
                  GDataU := Tree.GetNodeData(GData^.TopParent);
                  d := d+' TopParent: '+GDataU^.Name;
                end;
//              ShowMessage(d);
            end;

          Tree.IsVisible[tNode2] := True;
        end;

      // Goes to the child node
      cNode := Node.FirstChild;

      // Processes all child nodes
      while cNode <> nil do
      begin
        nProcessNode(cNode, tNode2);
        cNode := cNode.NextSibling;
      end;
    end;
begin
  if (not FileExists(Filename)) then exit;
  FPath := ExtractFilePath(ParamStr(0));
  FData := TStringList.Create;
  try
    // xmldocument only likes utf-8 encoding, we'll cheat and say it is.
    FData.LoadFromFile(Filename);
    FData.Text := StringReplace(FData.Text, 'utf-16', 'utf-8', [rfReplaceAll]);
    FData.SaveToFile(FPath+ExtractFileName(FileName)+'.tmp');
  finally
    FData.Free;
  end;
  ReadXMLFile(XMLDoc, FPath+ExtractFileName(Filename)+'.tmp');
  DeleteFile(FPath+ExtractFileName(Filename)+'.tmp');
  Tree.Clear;
  TreeNode := Tree.AddChild(nil);
  if (FileExists(FPath+'folder_home.png')) then
    hicon := 'home/folder_home'
  else
    hicon := '';
  SetNodeData(Tree,TreeNode,HomeName,'Folder','','HOME',hicon,nil);
  Tree.CheckState[TreeNode] := csUncheckedNormal;
  iNode := XMLDoc.DocumentElement.FirstChild;
  while iNode <> nil do
  begin
    nProcessNode(iNode, nil); // nil Recursive
    iNode := iNode.NextSibling;
  end;
  XMLDoc.Free;
end;

procedure VTreeViewDrawItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
var
  NodeRect,TmRect,BRect: TRect;
  s: String;
  OldBrushStyle: TBrushStyle;
  OldTextStyle: TTextStyle;
  NewTextStyle: TTextStyle;
  OldStyle: TFontStyles;
  b,c : Boolean;
  clOld: TColor;
  GData: PTreeData;
  w,ww,i,j : Integer;
const
  MARGIN = 0;
  ColorSet: Array[0..3] of TColor = ($FDDE54, $2DAEF6, $8DFC5E, $37276B);
begin
  NodeRect := ItemRect;
  ww := GetSystemMetrics(SM_CXVSCROLL);
  TVirtualStringTree(Sender).Indent:=0;
  TVirtualStringTree(Sender).DoubleBuffered:=True;
//  TVirtualStringTree(Sender).BeginUpdate;  -- causes flickering
  b := TVirtualStringTree(Sender).Selected[Node];
  c := (Node = TVirtualStringTree(Sender).FocusedNode);
  GData := TVirtualStringTree(Sender).GetNodeData(Node);

  s := GData^.Name;
  with TargetCanvas do
  begin
    clOld := Font.Color;
    OldBrushStyle := Brush.Style;
    OldStyle := Font.Style;
    Font.Style := [];
    OldTextStyle := TextStyle;
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    TmRect := NodeRect;
    TmRect.Left:=0;
    TmRect.Right:=TVirtualStringTree(Sender).Width-ww-5;
    TmRect.Top:=TmRect.Top; // -2
    TmRect.Bottom:=TmRect.Bottom; // +2
    Pen.Style := psClear;
    Brush.Style := bsClear;
    Pen.Color := clWhite;
//    if (c) then DrawFocusRect(NodeRect);
    Brush.Color := Brush.Color;
    Brush.Style := bsClear;
//    If b then Font.Style := [fsBold];

    if (s <> '') and (GData^.FType = 'Folder') and (TVirtualStringTree(Sender).GetNodeLevel(Node) = 0) then
      begin
        Brush.Style := bsSolid;
        Brush.Color := clWhite;
        FillRect(NodeRect);
        FillRect(Rect(TmRect.Left,(NodeRect.Top+NodeRect.Bottom-TextHeight('Tg')) div 2,NodeRect.Right,NodeRect.Bottom)); // ?
        Brush.Color := Brush.Color;
        Brush.Style := bsClear;

        Brush.Style := bsSolid;
        Brush.Color := clWhite;

        Font.Style := [fsBold];
        Pen.Style := psSolid;
        Pen.Color := $FFA855;
        Font.Color := $FFA855;
        Line(
          TmRect.Left + MARGIN,
          (TmRect.Top + TmRect.Bottom) div 2,
          TmRect.Right - MARGIN,
          (TmRect.Top + TmRect.Bottom) div 2
        );
        s := ' ' + s + ' ';
        w := TextWidth(s);
        TextOut(
          (TmRect.Left + TmRect.Right - w) div 2,
          ((TmRect.Top + TmRect.Bottom - TextHeight('Tg')) div 2),
          s
        );
        Pen.Style := psClear;
        Brush.Style := bsClear;
        Pen.Color := clWhite;
        if (c) then DrawFocusRect(TmRect);
        Font.Style:=[];
        Font.Color := clOld;
        Brush.Style := OldBrushStyle;
        TextStyle := OldTextStyle;
        Font.Style := OldStyle - [fsBold];
        exit;
      end
    else if (s <> '') and (GData^.FType = 'Folder') and (TVirtualStringTree(Sender).GetNodeLevel(Node) > 0) then
     begin
       Brush.Style := bsSolid;
       Brush.Color := clWhite;
       Font.Style := Font.Style - [fsBold];
       Font.Style := [];
       FillRect(NodeRect);
       Pen.Color := clWhite;
       Pen.Style := psSolid;
       i := 0;
       j := 0;
       for j := 0 to TVirtualStringTree(Sender).GetNodeLevel(Node)-1 do
         begin
           Inc(i);
           if (i > High(ColorSet)) then
             i := High(ColorSet);
         end;

       Brush.Style := bsSolid;
       Brush.Color := clWhite;
//////       FillRect(Rect(TmRect.Left,(NodeRect.Top+NodeRect.Bottom-TextHeight('Tg')) div 2,NodeRect.Right,NodeRect.Bottom)); // ?
//       Brush.Color := Brush.Color;
//       Brush.Style := bsClear;
//////       FillRect(TmRect.Left,TmRect.Top,TmRect.Right,TmRect.Bottom);
       Pen.Color := ColorSet[i];
       Font.Color := ColorSet[i];
       Pen.Width := 1;
       Line(
         TmRect.Left + MARGIN,
         (TmRect.Top + TmRect.Bottom) div 2,
         TmRect.Right - MARGIN,
         (TmRect.Top + TmRect.Bottom) div 2
       );
       s := ' ' + s + ' ';
       w := TextWidth(s);
       TextOut(
         (TmRect.Left + TmRect.Right - w) div 2,
         ((TmRect.Top + TmRect.Bottom - TextHeight('Tg')) div 2),
         s
       );
       Pen.Style := psClear;
       Brush.Style := bsClear;
       Pen.Color := clWhite;
       if (c) then DrawFocusRect(TmRect);
       Font.Color := clOld;
       Brush.Style := OldBrushStyle;
       TextStyle := OldTextStyle;
       Font.Style := OldStyle - [fsBold];
       exit;
     end;

    BRect.Left := TmRect.Left + 1;
    BRect.Top := TmRect.Top;
    BRect.Bottom := TmRect.Bottom - 0;
    BRect.Right := BRect.Left + (BRect.Bottom - BRect.Top) - 8;

    BRect.Left := NodeRect.Left;
    BRect.Top := NodeRect.Top+2;
    BRect.Bottom := NodeRect.Bottom-1;
    BRect.Right := NodeRect.Left + (NodeRect.Bottom - NodeRect.Top);
    BRect.Right := NodeRect.Left+(NodeRect.Bottom - NodeRect.Top);

    Brush.Style := bsSolid;
    Brush.Color := clWhite;

    Brush.Color := Brush.Color;
    Brush.Style := bsClear;

    TVirtualStringTree(Sender).CheckType[Node] := ctCheckbox;

    if (TVirtualStringTree(Sender).CheckState[Node] = csCheckedNormal) then
      begin
        if (GData^.FType = 'Shortcut') then
          Font.Color := clRed
        else
          Font.Color := clBlue;
      end
    else
      begin
        if (GData^.FType = 'Shortcut') then
          Font.Color := clRed
        else
          Font.Color := clBlack;
      end;
    If b Then Font.Style := [fsBold];
    NewTextStyle := OldTextStyle;
    NewTextStyle.Layout := tlCenter;
    TextStyle := NewTextStyle;
    TmRect.Left := BRect.Right+3;
    TmRect.Bottom := TmRect.Bottom;

    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    FillRect(Rect(TmRect.Left,NodeRect.Top,NodeRect.Right,NodeRect.Bottom));

    Brush.Color := Brush.Color;
    Brush.Style := bsClear;

    TextOut(
      TmRect.Left,
      ((NodeRect.Top + NodeRect.Bottom - TextHeight('Tg')) div 2),
      s
    );

    Font.Color := clOld;
    Brush.Style := OldBrushStyle;
    TextStyle := OldTextStyle;
    Font.Style := OldStyle - [fsBold];
  end;
end;

function VGetCheckCount(Tree: TVirtualStringTree; StatusPanel: TStatusPanel; Update:Boolean):Integer;
var
  j,k,totalg,totalsh: Integer;
  lNode,lNode2: PVirtualNode;

  procedure nProcessNode(Node: PVirtualNode);
  var
    GData: PTreeData;
    cNode,cNode2: PVirtualNode;
  begin
    if (Node = nil) then exit;

    GData := Tree.GetNodeData(Node);
    if (GData^.FType = 'Game') then inc(totalg);
    if (GData^.FType = 'Shortcut') then inc(totalsh);

    if (GData^.FType = 'Game') and VNodeChecked(Tree,Node) then inc(j);
    if (GData^.FType = 'Shortcut') and VNodeChecked(Tree,Node) then inc(k);

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
  j := 0;
  k := 0;
  totalg := 0;
  totalsh := 0;
  if (Tree.GetFirst() <> nil) then
    begin
      lNode := Tree.GetFirst();
      while (lNode <> nil) do
        begin
          lNode2 := Tree.GetNextSibling(lNode);
          nProcessNode(lNode);
          lNode := lNode2;
        end;
    end;
  if Update then
    begin
      StatusPanel.text := ' [ '+inttostr(j)+'/'+inttostr(totalg)+' games selected ][ '+inttostr(k)+'/'+inttostr(totalsh)+' shortcuts selected ]';
    end;
  VGetCheckCount := j;
end;

end.

