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
  laz2_xmlread, laz2_dom, ImgList, Themes, Types, Dialogs, FileUtil,
  crc, RegExpr;

type
  TGameData = class
    Name: String;
    Code: String;
    FType: String;
    FilePath: String;
    Icon: String;
    BelongsTo: TTreeNode;
    TopParent: TTreeNode;
    Hash: String;
  end;

procedure TreeViewDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; ImageList: TImageList; var DefaultDraw: Boolean);
function GetCheckCount(Tree: TTreeView; StatusPanel: TStatusPanel; Update:Boolean):Integer;
procedure XML2Tree(Tree: TTreeView; Filename: String; UseGames: Boolean = False; HomeName: String = 'HOME';
                   ViewStyle: Integer = 2);
function CreateGameData(Name:String;FType:String;BelongsTo:TTreeNode;Icon:String = '';Code:String = ''):TGameData;
function FindNodeData(Tree: TTreeView; PNode: TTreeNode; Search: String; Field: Integer; Children: Boolean = True):TTreeNode;
procedure CheckNode(Node: TTreeNode; Checked:boolean);
procedure ToggleTreeViewCheckBoxes(Node: TTreeNode);
function NodeChecked(ANode:TTreeNode): Boolean;
procedure FillImageListWithChecks(ImageList: TCustomImageList);
procedure InitChecks(ImageList: TCustomImageList);
procedure MoveChildrenToTopParent(TreeView: TTreeView);
procedure MoveChildrenToParent(TreeView: TTreeView);
procedure MoveChildrenToTop(TreeView: TTreeView);
procedure SelectionView(Tree: TTreeView; nSelected: Boolean);
procedure ClearSelections(Tree: TTreeView);
procedure GetCheckedCodes(Tree: TTreeView; var List: TStringList);
procedure GetCheckedHashes(Tree: TTreeView; var List: TStringList);
procedure CheckNodesList(Tree: TTreeView; ChkList: TStringList);
procedure AddShortcutSelections(Source: TTreeView; Dest: TTreeView);
procedure PopulateShortcuts(Tree: TTreeView; Path: String);
procedure PopulatePathData(Tree: TTreeView; Path: String);
procedure PopulateFolderData(Tree: TTreeView; Path: String);
procedure ClearShortcuts(Tree: TTreeView);
function FindNextNodeMatch(c: Char; Tree: TTreeView; StartNode: TTreeNode):TTreeNode;
procedure UpdateFolderPathData(Tree: TTreeView; Path: String; ChildFolder: String; ParentFolder: String);
function GameCount(Tree: TTreeView):Integer;
function GetHash(GData: TGameData):String;
function HashToNode(Tree: TTreeView; Hash: String):TTreeNode;


procedure debuglog(Text: String);

const
  ImgIndexChecked = 0;   // Image index of checked icon
  ImgIndexUnchecked = 1;  // Image index of unchecked icon

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

procedure AddShortcutSelections(Source: TTreeView; Dest: TTreeView);
var
  lNode,lNode2: TTreeNode;

  procedure nProcessNode(Node: TTreeNode);
  var
    GData: TGameData;
    nNode,cNode,cNode2: TTreeNode;
  begin
    if (Node = nil) then exit;

    GData := TGameData(Node.Data);
    if (GData.FType = 'Game') and NodeChecked(Node)  and
    (FindNodeData(Dest,Dest.Selected,'Folder',2,False) = Dest.Selected) and
    (FindNodeData(Dest,Dest.Selected,GData.Code,1,False) = nil) then
      begin
        Dest.Selected.Expand(False);
        nNode := Dest.Items.AddChild(Dest.Selected,Node.Text);
        nNode.Data := CreateGameData(GData.Name,'Game',Dest.Selected,'',GData.Code);
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
  if (Source.Items.Count = 0) or (Dest.Selected = nil) then exit;
  lNode := Source.Items[0];
  while (lNode <> nil) do
    begin
      lNode2 := lNode.GetNextSibling;
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

procedure CheckNodesList(Tree:TTreeView; ChkList: TStringList);
var
  lNode,lNode2: TTreeNode;

  procedure nProcessNode(Node: TTreeNode);
  var
    GData: TGameData;
    cNode,cNode2: TTreeNode;
  begin
    if (Node = nil) then exit;

    GData := TGameData(Node.Data);
    if (GData.FType = 'Game') and (ChkList.IndexOf(GData.Code) > -1) then
      begin
        CheckNode(node, true);
      end
    else if (GData.FType = 'Shortcut') and (ChkList.IndexOf(GData.Hash) > -1) then
      begin
        CheckNode(node, true);
      end
    else
      begin
        CheckNode(node, false);
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
  if (Tree.Items.Count = 0) then exit;
  lNode := Tree.Items[0];
  ChkList.Sorted:=True;
  while (lNode <> nil) do
    begin
      lNode2 := lNode.GetNextSibling;
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

function GameCount(Tree: TTreeView):Integer;
var
  lNode,lNode2: TTreeNode;
  total: Integer;

  procedure nProcessNode(Node: TTreeNode);
  var
    GData: TGameData;
    cNode,cNode2: TTreeNode;
  begin
    if (Node = nil) then exit;

    GData := TGameData(Node.Data);
    if (GData.FType = 'Game') then inc(Total);

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
  total := 0;
  if (Tree.Items.Count = 0) then
    begin
      GameCount := total;
      exit;
    end;
  lNode := Tree.Items[0];
  while (lNode <> nil) do
    begin
      lNode2 := lNode.GetNextSibling;
      nProcessNode(lNode);
      lNode := lNode2;
    end;
  Result := total;
end;

procedure GetCheckedCodes(Tree: TTreeView; var List: TStringList);
var
  lNode,lNode2: TTreeNode;

  procedure nProcessNode(Node: TTreeNode);
  var
    GData: TGameData;
    cNode,cNode2: TTreeNode;
  begin
    if (Node = nil) then exit;

    GData := TGameData(Node.Data);
    if (GData.FType = 'Game') and (NodeChecked(Node)) then
      List.Add(GData.Code);

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
  List.Clear;
  if (Tree.Items.Count = 0) then exit;
  lNode := Tree.Items[0];
  while (lNode <> nil) do
    begin
      lNode2 := lNode.GetNextSibling;
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

procedure GetCheckedHashes(Tree: TTreeView; var List: TStringList);
var
  lNode,lNode2: TTreeNode;

  procedure nProcessNode(Node: TTreeNode);
  var
    GData: TGameData;
    cNode,cNode2: TTreeNode;
  begin
    if (Node = nil) then exit;

    GData := TGameData(Node.Data);
    if (GData.FType = 'Shortcut') and (NodeChecked(Node)) then
      List.Add(GData.Hash);

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
  List.Clear;
  if (Tree.Items.Count = 0) then exit;
  lNode := Tree.Items[0];
  while (lNode <> nil) do
    begin
      lNode2 := lNode.GetNextSibling;
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;


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

procedure ClearShortcuts(Tree: TTreeView);
var
  lNode,lNode2: TTreeNode;

  procedure nProcessNode(Node: TTreeNode);
  var
    GData: TGameData;
    cNode,cNode2: TTreeNode;
  begin
    if (Node = nil) then exit;

    // Goes to the child node
    cNode := Node.GetFirstChild;

    GData := TGameData(Node.Data);
    if (GData.FType = 'Game') then
      Node.Delete;

    // Processes all child nodes
    while cNode <> nil do
      begin
        cNode2 := cNode.GetNextSibling;
        nProcessNode(cNode);
        cNode := cNode2;
      end;
  end;

begin
  if (Tree.Items.Count = 0) then exit;
  lNode := Tree.Items[0];
  while (lNode <> nil) do
    begin
      lNode2 := lNode.GetNextSibling;
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

procedure ClearSelections(Tree: TTreeView);
var
  lNode,lNode2: TTreeNode;

  procedure nProcessNode(Node: TTreeNode);
  var
    GData: TGameData;
    cNode,cNode2: TTreeNode;
  begin
    if (Node = nil) then exit;

    GData := TGameData(Node.Data);
    if (GData.FType <> 'Folder') and (NodeChecked(Node)) then
      ToggleTreeViewCheckBoxes(Node);

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
  if (Tree.Items.Count = 0) then exit;
  lNode := Tree.Items[0];
  while (lNode <> nil) do
    begin
      lNode2 := lNode.GetNextSibling;
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;


procedure SelectionView(Tree: TTreeView; nSelected: Boolean);
var
  lNode,lNode2: TTreeNode;

  procedure nProcessNode(Node: TTreeNode);
  var
    GData: TGameData;
    cNode,cNode2: TTreeNode;
  begin
    if (Node = nil) then exit;

    GData := TGameData(Node.Data);
    if (GData.FType = 'Game') or (GData.FType = 'Shortcut') then
      begin
        if (not NodeChecked(Node)) and (nSelected) then
          Node.Visible:=False
        else if (NodeChecked(Node)) and (nSelected) then
          Node.Visible:=True
        else if (not NodeChecked(Node)) and (not nSelected) then
          Node.Visible:=True;
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
  if (Tree.Items.Count = 0) then exit;
  lNode := Tree.Items[0];
  while (lNode <> nil) do
    begin
      lNode2 := lNode.GetNextSibling;
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

procedure InitChecks(ImageList: TCustomImageList);
var aSize: TSize;
    aBMP: TBitmap;
    aDetails: TThemedElementDetails;
    aRect: TRect;
begin
  aDetails:=ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal);
  aSize:=ThemeServices.GetDetailSize(aDetails);
  ImageList.Width:=aSize.cx;
  ImageList.Height:=aSize.cy;
  aBMP:=TBitmap.Create;
  with aBMP do
    begin
      SetSize(aSize.cx, aSize.cy);
      Transparent:=True;
      TransparentColor:=clForm;
      Canvas.Brush.Color:=TransparentColor;
      Canvas.FillRect(0,0, Width,Height);
    end;
  aRect:=Rect(0, 0, aSize.cx, aSize.cy);
  ThemeServices.DrawElement(aBMP.Canvas.Handle, aDetails, aRect, nil);
  ImageList.Add(aBMP, nil);
  aBMP.Canvas.FillRect(0,0, aBMP.Width,aBMP.Height);;
  aDetails:=ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
  ThemeServices.DrawElement(aBMP.Canvas.Handle, aDetails, aRect, nil);
  ImageList.Add(aBMP, nil);
  FreeAndNil(aBMP);
end;

procedure FillImageListWithChecks(ImageList: TCustomImageList);

  procedure DrawCheck(Canvas: TCanvas; Checked: Boolean; const Rect: TRect);
  const
    CCheckedButtons: array [Boolean] of TThemedButton =
      (tbCheckBoxUncheckedNormal, tbCheckBoxCheckedNormal);
  begin
    Canvas.FillRect(Rect);
    ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(
      CCheckedButtons[Checked]), Rect);
  end;

var
  Checks: TBitmap;
  W, H: Integer;
  OneCheckRect: TRect;
  Canvas: TCanvas;
begin
  if ImageList = nil then
    Exit;
  H := GetSystemMetrics(SM_CYMENUCHECK);
  W := GetSystemMetrics(SM_CXMENUCHECK);
  ImageList.Height := H;
  ImageList.Width := W;
  Checks := TBitmap.Create;
  try
    Checks.Height := H;
    Checks.Width := 2 * W;
    OneCheckRect := Rect(0, 0, W, H);
    Canvas := Checks.Canvas;
    DrawCheck(Canvas, False, OneCheckRect);
    OffsetRect(OneCheckRect, W, 0);
    DrawCheck(Canvas, True, OneCheckRect);
    ImageList.AddMasked(Checks, Canvas.Brush.Color);
  finally
    Checks.Free;
  end;
end;

procedure CheckNode(Node: TTreeNode; Checked:boolean);
begin
  if Assigned(Node) then
    if Checked then
      begin
        Node.StateIndex := ImgIndexChecked;
      end
    else
      begin
        Node.StateIndex := ImgIndexUnchecked;
      end;
end;

procedure ToggleTreeViewCheckBoxes(Node: TTreeNode);
begin
  if Assigned(Node) then begin
    if Node.StateIndex = ImgIndexUnchecked then
      Node.StateIndex := ImgIndexChecked
    else
    if Node.StateIndex = ImgIndexChecked then
      Node.StateIndex := ImgIndexUnchecked
  end;
end;

function NodeChecked(ANode:TTreeNode): Boolean;
begin
  if (ANode.StateIndex = ImgIndexUnchecked) then result := False
  else result := True;
end;

function GetHash(GData: TGameData):String;
var
  crcval: longword;
  HString: String;
begin
  crcval := crc32(0,nil,0);
  HString := GData.Name+GData.FType+GData.Code+GData.FilePath;
  if (GData.BelongsTo <> nil) then HString := HString+GData.BelongsTo.Text;
  if (GData.TopParent <> nil) then HString := HString+GData.TopParent.Text;
  Result := inttohex(crc32(crcval,@HString[1],length(HString)),8);
end;

function CreateGameData(Name:String;FType:String;BelongsTo:TTreeNode;Icon:String = '';Code:String = ''):TGameData;
var
  rNode: TTreeNode;
begin
  Result := TGameData.Create;
  Result.Name := Name;
  Result.FType := Ftype;
  Result.Code := Code;
  Result.FilePath := '';
  Result.Icon := Icon;
  rNode := BelongsTo;
  while (rNode <> nil) and Assigned(rNode.Parent) do
    rNode := rNode.Parent;
  Result.TopParent := rNode;
  Result.BelongsTo := BelongsTo;
  Result.Hash := GetHash(Result);
end;

procedure PopulateShortcuts(Tree: TTreeView; Path: String);
var
  FList: TStringList;
  lNode,lNode2,mNode,mNode2,fNode,fNode2,nNode: TTreeNode;
  i: Integer;
  fpath,GameCode: String;
  GData: TGameData;
begin
  if (Trim(Path) = '') or (Tree.Items.Count = 0) then exit;
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
        lNode := Tree.Items[0];
        while (lNode <> nil) do
          begin
            lNode2 := lNode.GetNextSibling;
            fNode := nil;
            fNode := FindNodeData(Tree,lNode,fpath,4);
            if (fNode <> nil) then
              begin
                fNode2 := nil;
                mNode := Tree.Items[0];
                while (mNode <> nil) do
                  begin
                    mNode2 := mNode.GetNextSibling;
                    fNode2 := FindNodeData(Tree,mNode,GameCode,1);
                    if (fNode2 <> nil) then mNode := nil
                    else mNode := mNode2;
                  end;
                if (fNode2 <> nil) and (FindNodeData(Tree,fNode,GameCode,1,False) = nil) then
                  begin
                    lNode2 := nil;
                    nNode := Tree.Items.AddChild(fNode,fNode2.Text);
                    GData := CreateGameData(fNode2.Text,'Shortcut',fNode,'',GameCode);
                    GData.TopParent := TGameData(fNode.Data).TopParent;
                    GData.FilePath := ExtractFilePath(FList[i]);
                    nNode.data := GData;
                    CheckNode(nNode,False);
                  end;
              end;
            lNode := lNode2;
          end;
      end;

  finally
    FList.Free;
  end;
end;

procedure PopulateFolderData(Tree: TTreeView; Path: String);
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
    GData: TGameData;
  begin
    cList := TStringList.Create;
    try
      cList.Delimiter:=' ';
      cList.DelimitedText:=Trim(children.Values[folder]);
      if (folder = '000') then
        begin
          GData := TGameData(Tree.Items[0].Data);
          GData.FilePath := Path+'\000';
          GData.TopParent := Tree.Items[0];
          GData.Hash := GetHash(GData);
        end;
      for k := 0 to cList.Count-1 do
        begin
          nfolder := cList[k];
          UpdateFolderPathData(Tree,Path+'\'+nfolder,foldername.Values[nfolder],foldername.Values[parent.Values[nfolder]]);
          nProcess(nfolder);
        end;
    finally
      cList.Free;
    end;
  end;

begin
  if ((Path = '') or (not DirectoryExists(Path)) or (Tree.Items.Count = 0)) then exit;

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

procedure PopulatePathData(Tree: TTreeView; Path: String);
var
  FList: TStringList;
  i,a: integer;
  GData: TGameData;
  lNode,lNode2,fNode: TTreeNode;
  CodeString: String;
begin
  if ((Path = '') or (not DirectoryExists(Path)) or (Tree.Items.Count = 0)) then exit;

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
              lNode := Tree.Items[0];
              while (lNode <> nil) do
                begin
                  lNode2 := lNode.GetNextSibling;
                  fNode := nil;
                  fNode := FindNodeData(Tree,lNode,CodeString,1);
                  if (fNode <> nil) then
                    begin
                      GData := TGameData(fNode.Data);
                      if (GData.FilePath = '') then
                        begin
                          GData.FilePath := ExtractFilePath(FList[i]);
                          GData.Hash := GetHash(GData);
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

procedure MoveChildrenToTop(TreeView: TTreeView);
var
  lNode,lNode2: TTreeNode;

  procedure nProcessNode(Node:TTreeNode);
  var
    rNode,cNode,cNode2: TTreeNode;
    GData: TGameData;
    AttachMode: TNodeAttachMode;
  begin
    if (Node = nil) then exit;
    GData := TGameData(Node.Data);
    if (GData.FType = 'Folder') then
      begin
        Node.Visible := False;
      end;
    if (Node.Level > 0) and (GData.FType <> 'Folder') then
      begin
        AttachMode := naAdd;
        rNode := nil;
        Node.MoveTo(rNode,AttachMode);
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
  if (TreeView.Items.Count = 0) then exit;
  lNode := TreeView.Items[0];
  while (lNode <> nil) do
    begin
      lNode2 := lNode.GetNextSibling;
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

procedure MoveChildrenToParent(TreeView: TTreeView);
var
  lNode,lNode2: TTreeNode;

  procedure nProcessNode(Node: TTreeNode);
  var
    rNode,cNode,cNode2: TTreeNode;
    GData: TGameData;
    AttachMode: TNodeAttachMode;
    t: Boolean;
  begin
    t := False;
    if (Node = nil) then exit;

    GData := TGameData(Node.Data);
    if (GData.FType = 'Folder') then
      Node.Visible := True;
    if (Node.Parent <> nil) and (GData.BelongsTo = Node.Parent) then t := True;
    if (not t) then
      begin
        if (Node.Level <= 1) and (GData.FType <> 'Folder') then
          begin
            AttachMode := naAddChild;
            rNode := GData.BelongsTo;
            Node.MoveTo(rNode,AttachMode);
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
  end;

begin
  if (TreeView.Items.Count = 0) then exit;
  lNode := TreeView.Items[0];
  while (lNode <> nil) do
    begin
      lNode2 := lNode.GetNextSibling;
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

procedure MoveChildrenToTopParent(TreeView: TTreeView);
var
  lNode,lNode2: TTreeNode;

  procedure nProcessNode(Node: TTreeNode);
  var
    rNode,cNode,cNode2: TTreeNode;
    GData: TGameData;
    AttachMode: TNodeAttachMode;
    t: Boolean;
  begin
    t := False;
    if (Node = nil) then exit;

    GData := TGameData(Node.Data);
    if (GData.FType = 'Folder') and (Node.Level = 0) then
      Node.Visible := True
    else if (GData.FType = 'Folder') and (Node.Level > 0) then
      Node.Visible := False;
    if (Node.Parent <> nil) and (GData.TopParent = Node.Parent) then t := True;
    if (Node = GData.TopParent) then t := True;
    if (not t) then
      begin
        if (GData.FType <> 'Folder') then
          begin
            rNode := GData.TopParent;
            if (rNode = nil) then AttachMode := naAdd
            else AttachMode := naAddChild;
            Node.MoveTo(rNode,AttachMode);
          end;
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
  if (TreeView.Items.Count = 0) then exit;
  lNode := TreeView.Items[0];
  while (lNode <> nil) do
    begin
      lNode2 := lNode.GetNextSibling;
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

procedure UpdateFolderPathData(Tree: TTreeView; Path: String; ChildFolder: String; ParentFolder: String);
var
  lNode,lNode2: TTreeNode;

  procedure nProcessNode(Node: TTreeNode);
  var
    cNode,cNode2: TTreeNode;
    GData: TGameData;
  begin
    if (Node = nil) then exit;
    ChildFolder := Trim(ChildFolder);
    ParentFolder := Trim(ParentFolder);

    GData := TGameData(Node.Data);
    if (GData.FType = 'Folder') and (Node.Text = ChildFolder) then
      begin
      if (GData.BelongsTo = nil) and (ParentFolder = '') then
        begin
          GData.FilePath := Path;
          GData.Hash := GetHash(GData);
          lNode2 := nil;
          exit;
        end
      else if (GData.BelongsTo.Text = ParentFolder) and (GData.FilePath = '') then
        begin
          GData.FilePath := Path;
          GData.Hash := GetHash(GData);
          lNode2 := nil;
          exit;
        end;
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
  if (Tree.Items.Count = 0) then exit;
  lNode := Tree.Items[0];
  while (lNode <> nil) do
    begin
      lNode2 := lNode.GetNextSibling;
      nProcessNode(lNode);
      lNode := lNode2;
    end;
end;

function HashToNode(Tree: TTreeView; Hash: String):TTreeNode;
var
  lNode,lNode2,rNode: TTreeNode;

  procedure nProcessNode(Node: TTreeNode);
  var
    cNode,cNode2: TTreeNode;
    GData: TGameData;
  begin
    if (Node = nil) then exit;

    GData := TGameData(Node.Data);
    if (GData.Hash = Hash) then
      begin
        rNode := Node;
        lNode2 := nil;
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
  if (Tree.Items.Count = 0) then exit;
  lNode := Tree.Items[0];
  rNode := nil;
  while (lNode <> nil) and (rNode = nil) do
    begin
      lNode2 := lNode.GetNextSibling;
      nProcessNode(lNode);
      lNode := lNode2;
    end;
  Result := rNode;
end;

function FindNodeData(Tree: TTreeView; PNode: TTreeNode; Search: String; Field: Integer; Children: Boolean = True):TTreeNode;
var
  rNode,cNode: TTreeNode;
  GData: TGameData;
begin
  // 0 = Name, 1 = Code, 2 = Game/Folder, 3 = Parent Node Text, 4 = FilePath, 5 = Hash
  FindNodeData := nil;
  if (Tree.Items.Count = 0) then exit;
  if (PNode = nil) then exit;

  GData := TGameData(PNode.Data);
  if (Field = 0) then
    begin
      if GData.Name = Search then
        begin
          FindNodeData := PNode;
          exit;
        end;
    end
  else if (Field = 1) then
    begin
      if (GData.Code <> '') and (GData.Code = Search) then
        begin
          FindNodeData := PNode;
          exit;
        end;
    end
  else if (Field = 2) then
    begin
      if GData.FType = Search then
        begin
          FindNodeData := PNode;
          exit;
        end;
    end
  else if (Field = 3) then
    begin
      if (GData.BelongsTo <> nil) and (GData.BelongsTo.Text = Search) then
        begin
          FindNodeData := PNode;
          exit;
        end;
    end
  else if (Field = 4) then
    begin
      if (Trim(Lowercase(GData.FilePath)) = Trim(Lowercase(Search))) then
        begin
          FindNodeData := PNode;
          exit;
        end;
    end
  else if (Field = 5) then
    begin
      if (GData.Hash = Search) then
        begin
          FindNodeData := PNode;
          exit;
        end;
    end;

  // Goes to the child node
  cNode := PNode.GetFirstChild;

  // Processes all child nodes
  while cNode <> nil do
    begin
      GData := TGameData(cNode.Data);
      if (Children or (GData.FType <> 'Folder')) then
        begin
          rNode := FindNodeData(Tree,cNode,Search,Field);
          if (rNode <> nil) then
            begin
              FindNodeData := rNode;
              exit;
            end;
        end;
      cNode := cNode.GetNextSibling;
    end;
end;

procedure XML2Tree(Tree: TTreeView; Filename: String; UseGames: Boolean = False; HomeName: String = 'HOME';
                   ViewStyle: Integer = 2);
var
  XMLDoc: TXMLDocument;
  iNode: TDOMNode;
  FPath: String;
  FData: TStringList;
  GData: TGameData;
  TreeNode: TTreeNode;

    procedure nProcessNode(Node: TDOMNode; TreeNode: TTreeNode);
    var
      cNode: TDOMNode;
      s,t,c: string;
      GData: TGameData;
      tNode: TTreeNode;
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

          GData := CreateGameData(s,'Folder',TreeNode,t,'');
          TreeNode := tree.Items.AddChild(TreeNode, s);
          if (GData.TopParent = nil) then GData.TopParent := TreeNode;
          TreeNode.Data := GData;
          TreeNode.StateIndex:=ImgIndexUnchecked;
          if (ViewStyle = 1) and (TreeNode.Level > 0) then
            TreeNode.Collapse(False)
          else TreeNode.Expand(False);
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
          if (TreeNode = nil) then tNode := Tree.Items[0]
          else tNode := TreeNode;
          GData := CreateGameData(s,'Game',tNode,'',c);
          TreeNode := tree.Items.AddChild(tNode, s);
          if (GData.TopParent = nil) then GData.TopParent := TreeNode;
          TreeNode.Data := GData;
          TreeNode.StateIndex:=ImgIndexUnchecked;
          TreeNode.Visible:=True;
        end;

      // Goes to the child node
      cNode := Node.FirstChild;

      // Processes all child nodes
      while cNode <> nil do
      begin
        nProcessNode(cNode, TreeNode);
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
  Tree.Items.Clear;
  GData := CreateGameData(HomeName,'Folder',nil,'','HOME');
  TreeNode := tree.Items.AddChild(nil, HomeName);
  TreeNode.Data := GData;
  TreeNode.StateIndex:=ImgIndexUnchecked;
  iNode := XMLDoc.DocumentElement.FirstChild;
  while iNode <> nil do
  begin
    nProcessNode(iNode, nil); // nil Recursive
    iNode := iNode.NextSibling;
  end;
  XMLDoc.Free;
end;

procedure TreeViewDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; ImageList: TImageList; var DefaultDraw: Boolean);
var
    NodeRect,TmRect,BRect: TRect;
    s: String;
    OldBrushStyle: TBrushStyle;
    OldTextStyle: TTextStyle;
    NewTextStyle: TTextStyle;
    OldStyle: TFontStyles;
    b,c : Boolean;
    clOld: TColor;
    GData: TGameData;
    w,i,j : Integer;
const
   MARGIN = 0;
//   IsChecked : array[Boolean] of Integer = (DFCS_BUTTONCHECK, DFCS_BUTTONCHECK or DFCS_CHECKED) ;
   ColorSet: Array[0..3] of TColor = ($FDDE54, $2DAEF6, $8DFC5E, $37276B);
begin
  DefaultDraw := False;

  TTreeView(Sender).BeginUpdate;
  Node.Height:=TTreeView(Sender).Canvas.TextHeight('Tg');
  b := cdsSelected In State;
  c := cdsFocused In State;
  GData := TGameData(Node.Data);
  NodeRect := Node.DisplayRect(False);

  s := GData.Name;
  with TTreeView(Sender).Canvas do
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
    TmRect.Right:=TTreeView(Sender).Width;
    TmRect.Top:=TmRect.Top; // -2
    TmRect.Bottom:=TmRect.Bottom; // +2
    FillRect(NodeRect);
    FillRect(TmRect);
    Pen.Style := psClear;
    Brush.Style := bsClear;
    Pen.Color := clWhite;
//    if (c) then DrawFocusRect(NodeRect);
    Brush.Color := Brush.Color;
// //    FillRect(TmRect);
    Brush.Style := bsClear;
//    If b then Font.Style := [fsBold];

    if (s <> '') and (GData.FType = 'Folder') and (Node.Level = 0) then
      begin
        Brush.Style := bsSolid;
        Brush.Color := clWhite;
        FillRect(Rect(TmRect.Left,(NodeRect.Top+NodeRect.Bottom-TextHeight('Tg')) div 2,NodeRect.Right,NodeRect.Bottom)); // ?
        Brush.Color := Brush.Color;
        Brush.Style := bsClear;

        Brush.Style := bsSolid;
        Brush.Color := clWhite;

        Font.Style := [fsBold];
//        FillRect(TmRect); // ?
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
//        TTreeView(Sender).SetFocus;
        TTreeView(Sender).EndUpdate;
        exit;
      end
    else if (s <> '') and (GData.FType = 'Folder') and (Node.Level > 0) then
     begin
       Brush.Style := bsSolid;
       Brush.Color := clWhite;
       Font.Style := Font.Style - [fsBold];
       Font.Style := [];
//       FillRect(TmRect);  // .
       Pen.Color := clWhite;
       Pen.Style := psSolid;
       i := 0;
       j := 0;
       for j := 0 to Node.Level-1 do
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
//       Font.Style:=[];
       Font.Color := clOld;
       Brush.Style := OldBrushStyle;
       TextStyle := OldTextStyle;
       Font.Style := OldStyle - [fsBold];
//       TTreeView(Sender).SetFocus;
       TTreeView(Sender).EndUpdate;
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
    FillRect(BRect); // ?

    Brush.Color := Brush.Color;
    Brush.Style := bsClear;

//    BRect.Right := TmRect.Left + (TmRect.Bottom - TmRect.Top) - 2;
//    BRect.Right := ImageList.Width;
//    Node.ImageIndex:=0;
//    Node.SelectedIndex:=0;
//    ImageList.Draw(TTreeView(Sender).Canvas,BRect.Left,BRect.Top+4,0,dsNormal,itImage);
//    DrawFrameControl(Handle, BRect, DFC_BUTTON, IsChecked[NodeChecked(Node)]);

    If NodeChecked(Node) then
    Begin
      ImageList.Draw(TTreeView(Sender).Canvas,BRect.Left,BRect.Top,1,dsNormal,itImage);
      if (GData.FType = 'Shortcut') then
        Font.Color := clRed
      else
        Font.Color := clBlue;
    end Else
    begin
      if (GData.FType = 'Shortcut') then
        Font.Color := clRed
      else
        Font.Color := Font.Color;
////      DrawFrameControl(Handle, BRect, DFC_BUTTON, IsChecked[False]);
      ImageList.Draw(TTreeView(Sender).Canvas,BRect.Left,BRect.Top,0,dsNormal,itImage);
    end;
    If b Then Font.Style := [fsBold];
    NewTextStyle := OldTextStyle;
    NewTextStyle.Layout := tlCenter;
    TextStyle := NewTextStyle;
    TmRect.Left := BRect.Right+3;
    TmRect.Bottom := TmRect.Bottom;

    Brush.Style := bsSolid;
    Brush.Color := clWhite;
//////    FillRect(Rect(TmRect.Left,(NodeRect.Top+NodeRect.Bottom-TextHeight('Tg')) div 2,NodeRect.Right,NodeRect.Bottom)); // ?
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
//  TTreeView(Sender).SetFocus;
  TTreeView(Sender).EndUpdate;
end;

function GetCheckCount(Tree: TTreeView; StatusPanel: TStatusPanel; Update:Boolean):Integer;
var
  j,k,totalg,totalsh: Integer;
  lNode,lNode2: TTreeNode;

  procedure nProcessNode(Node: TTreeNode);
  var
    GData: TGameData;
    cNode,cNode2: TTreeNode;
  begin
    if (Node = nil) then exit;

    GData := TGameData(Node.Data);
    if (GData.FType = 'Game') then inc(totalg);
    if (GData.FType = 'Shortcut') then inc(totalsh);

    if (GData.FType = 'Game') and NodeChecked(Node) then inc(j);
    if (GData.FType = 'Shortcut') and NodeChecked(Node) then inc(k);

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
  j := 0;
  k := 0;
  totalg := 0;
  totalsh := 0;
  if (Tree.Items.Count > 0) then
    begin
      lNode := Tree.Items[0];
      while (lNode <> nil) do
        begin
          lNode2 := lNode.GetNextSibling;
          nProcessNode(lNode);
          lNode := lNode2;
        end;
    end;
  if Update then
    begin
      StatusPanel.text := ' [ '+inttostr(j)+'/'+inttostr(totalg)+' games selected ][ '+inttostr(k)+'/'+inttostr(totalsh)+' shortcuts selected ]';
    end;
  GetCheckCount := j;
end;

end.

