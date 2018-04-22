{ main.pas
  Description: Main unit handling form creation
  and processing of events.


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

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  IPHtml, ComCtrls, Types, StdCtrls, lcltype, lclintf, Math, ExtCtrls, EditBtn,
  Inifiles, Buttons, MouseAndKeyInput, Iphttpbroker, VirtualTrees, ui_utils,
  futils, nutils, config, help, about, tools, popupex;

type
  TSimpleIpHtml = class(TIpHtml)
  public
    property OnGetImageX;
  end;

  { TForm1 }
  TForm1 = class(TForm)
    Bevel1: TBevel;
    ExplorerOpen1: TMenuItem;
    DeletePresetOption: TMenuItem;
    DP1: TIpHttpDataProvider;
    MenuItem10: TMenuItem;
    AddPrefixesOption: TMenuItem;
    MenuItem11: TMenuItem;
    DelPrefixesOption: TMenuItem;
    MenuItem12: TMenuItem;
    ShortcutsOption: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    CreateFolderMnu: TMenuItem;
    MenuItem20: TMenuItem;
    AddPresetOption: TMenuItem;
    MenuItem9: TMenuItem;
    PngSelectDlg: TOpenDialog;
    PopupMenu1: TPopupMenu;
    SaveShcBtn: TButton;
    ClearShcBtn: TButton;
    AddShortCutsBtn: TButton;
    Label4: TLabel;
    GameInfoOption: TMenuItem;
    NANDCheckBox: TCheckBox;
    LoadBtn: TButton;
    GamesEdit: TDirectoryEdit;
    Label3: TLabel;
    PresetsOption: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    PopupMenu2: TPopupMenu;
    RenameHome1: TMenuItem;
    RestoreXMLDlg: TOpenDialog;
    BackupXMLDlg: TSaveDialog;
    HakchiConfigDlg: TSelectDirectoryDialog;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    FlatOption: TMenuItem;
    MenuItem8: TMenuItem;
    SelectedOption: TMenuItem;
    ParentChildOption: TMenuItem;
    ParentOption: TMenuItem;
    StatusBar1: TStatusBar;
    ViewMenu: TMenuItem;
    PopupNotifier1: TPopupEx;
    SVST: TVirtualStringTree;
    VST: TVirtualStringTree;
    XMLEdit: TFileNameEdit;
    Label2: TLabel;
    SaveBtn: TButton;
    ClearBtn: TButton;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    procedure AddPrefixesOptionClick(Sender: TObject);
    procedure AddPresetOptionClick(Sender: TObject);
    procedure AddShortCutsBtnClick(Sender: TObject);
    procedure CreateFolderMnuClick(Sender: TObject);
    procedure DelPrefixesOptionClick(Sender: TObject);
    procedure ExplorerOpen1Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure SaveShcBtnClick(Sender: TObject);
    procedure ClearShcBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure FlatOptionClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure GameInfoOptionClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure NANDCheckBoxChange(Sender: TObject);
    procedure ParentChildOptionClick(Sender: TObject);
    procedure ParentOptionClick(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure RenameHomeClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure SelectedOptionClick(Sender: TObject);
    procedure ShortcutsOptionClick(Sender: TObject);
    procedure SVSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure SVSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure SVSTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LoadSlotClick(Sender: TObject);
    procedure SaveSlotClick(Sender: TObject);
    procedure RenameSlotClick(Sender: TObject);
    procedure DeleteSlotClick(Sender: TObject);
    procedure DataProviderGetImage(Sender: TIpHtmlNode; const URL: string;
          var Picture: TPicture);
    procedure SVSTNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure VSTAfterItemPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
    procedure VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTIncrementalSearch(Sender: TBaseVirtualTree;
      Node: PVirtualNode; const SearchText: String; var Result: Integer);
    procedure VSTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VSTMouseLeave(Sender: TObject);
    procedure VSTMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure VSTNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
  private
    { private declarations }
    progressBar: TProgressBar;
    statusPanel0: TStatusPanel;
    statusPanel1: TStatusPanel;
  public
    { public declarations }
    procedure InitSlots;
    function GetKeyPressed(const VKeyCode: Integer): Boolean;
  end;


var
  Form1: TForm1;
  LastFavFolder: String;
  ViewStyle: Integer;
  ViewSelected: Boolean;
  ViewShortcuts: Boolean;
  ViewGameInfo: Boolean;
  LastUsedXML: String;
  HomeName: String;
  LastSVSTNode: PVirtualNode;
  LastVSTNode: PVirtualNode;


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  r: TRect;
begin
  Form1.Caption := GetProductName+' - '+GetProductVersion;
  VST.DoubleBuffered:=True;
  LastSVSTNode := nil;
  LastVSTNode := nil;
  PopupNotifier1 := TPopupEx.Create(Self);
  PopupNotifier1.Color:=clSkyBlue;
  PopupNotifier1.Title:='Game Information';
  PopupNotifier1.vNotifierForm.ButtonSize:=0;
  DP1.OnGetImage:=@DataProviderGetImage;
  PopupNotifier1.vNotifierForm.htmlPnl.DataProvider:=DP1;
  r.Left:=VST.ClientOrigin.X+5;
  r.Right:=VST.ClientOrigin.X+(Form1.Width - (VST.ClientOrigin.X+VST.Width))-10;
  r.Top:=VST.ClientOrigin.Y;
  r.Bottom:=Bevel1.ClientOrigin.Y-10;
  PopupNotifier1.vNotifierForm.HintRect:=r;
  PopupNotifier1.vNotifierForm.Width:=r.Right-r.Left;
  PopupNotifier1.vNotifierForm.Height:=r.Bottom-r.Top + 10;
  statusPanel0:=StatusBar1.Panels.Items[0];
  statusPanel0.Text:='StatusPanel0';
  statusPanel1:=StatusBar1.Panels.Items[1];
  progressBar:=TProgressBar.Create(StatusBar1);
  progressBar.SetInitialBounds(StatusBar1.Left, StatusBar1.Top,  350, StatusBar1.Height);
  progressBar.Parent:=Self;
  progressBar.Left:=statusBar1.Left;
  progressBar.Top:=statusBar1.Top;
  progressBar.Min:=0;
  progressBar.Position:=0;
  progressBar.Visible:=False;
  LastFavFolder := '';
  HomeName := 'HOME';
  FlatOption.Checked:=False;
  ParentOption.Checked:=False;
  ParentChildOption.Checked:=False;
  LoadConfig(XMLEdit,GamesEdit,VST,SVST,NANDCheckBox,ViewStyle,ViewSelected,ViewShortcuts,ViewGameInfo,LastFavFolder,HomeName);
  InitSlots;
  LastUsedXML:=XMLEdit.Caption;
  case ViewStyle of
       0: FlatOption.Checked:=True;
       1: ParentOption.Checked:=True;
       2: ParentChildOption.Checked:=True;
  end;
  GameInfoOption.Checked:=ViewGameInfo;
  SelectedOption.Checked:=ViewSelected;
  ShortcutsOption.Checked:=ViewShortcuts;
  VST.FullExpand();
  if (VST.GetFirst() <> nil) then
    case ViewStyle of
       0: Form1.FlatOptionClick(Self);
       1: Form1.ParentOptionClick(Self);
       2: Form1.ParentChildOptionClick(Self);
    end;
  if (ViewShortcuts) then
    VShortcutsView(VST,ViewShortcuts)
  else if ViewSelected then
    VSelectionView(VST,ViewSelected);
  VGetCheckCount(VST, StatusBar1.Panels.Items[0], True);
end;

procedure TForm1.DataProviderGetImage(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
  MPath,Filename: String;
  PicCreated: Boolean;
begin
  MPath := ExtractFileDir(XMLEdit.Caption);
  MPath := ExtractFilePath(MPath);
  MPath := MPath+'folder_images\';
  Filename := MPath+URL;
  if (Pos('home/',URL) > 0) then
    begin
      MPath := ExtractFilePath(ParamStr(0));
      FileName := MPath + Copy(URL,6,length(URL));
    end;
  if not FileExists(Filename) then
    begin
      exit;
    end;
  PicCreated := False;
  try
    if Picture=nil then
      begin
        Picture:=TPicture.Create;
        PicCreated := True;
      end;
    Picture.LoadFromFile(Filename);
  except
    if PicCreated then
      Picture.Free;
    Picture := nil;
  end;
end;

procedure TForm1.SVSTNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo
  );
var
  Node: PVirtualNode;
begin
  Node := HitInfo.HitNode;
  if (SVST.Selected[Node] and (LastSVSTNode = Node) and (GetKeyPressed(VK_LCONTROL) or GetKeyPressed(VK_RCONTROL))) then
    begin
      SVST.ClearSelection;
      LastSVSTNode := nil;
    end
  else
    LastSVSTNode := Node;
end;

procedure TForm1.VSTAfterItemPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
begin
  VTreeViewDrawItem(Sender,TargetCanvas,Node,ItemRect);
end;

procedure TForm1.VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  checked: integer;
begin
  checked := VGetCheckCount(VST, StatusPanel0,False);
  if VNodeChecked(VST,Node) then
    begin
      if (checked > 30) then
        begin
          CheckVNode(VST,Node,False);
          Dec(checked);
          MessageDlg('Error','Maximum of 30 favorites allowed.',mtError,[mbOk],0);
        end;
    end
  else if ViewSelected then
    begin
      SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewShortcuts,ViewGameInfo,HomeName,VST);
    end;
  checked := VGetCheckCount(VST, StatusPanel0, True);
end;

procedure TForm1.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  GData1,GData2: PTreeData;
begin
  with Sender as TVirtualStringTree do
    begin
      Result := 0;
      GData1 := GetNodeData(Node1);
      GData2 := GetNodeData(Node2);
      if ((GData1^.FType = 'Game') or (GData1^.FType = 'Shortcut')) and (GData2^.FType = 'Folder') then
        Result := -1
      else if (GData1^.FType = 'Folder') and ((GData2^.FType = 'Game') or (GData2^.FType = 'Shortcut')) then
        Result := 1
      else if ((GData1^.FType = 'Game') or (GData1^.FType = 'Shortcut')) and ((GData2^.Ftype = 'Game') or (GData2^.FType = 'Shortcut')) then
        Result := CompareText(GData1^.Name,GData2^.Name);
    end;
end;

procedure TForm1.VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var
  GData,BData,TData: PTreeData;
  X1,Y1: Integer;
  OControl: TWinControl;
  DisplayText,MPath: String;
  Flush: Boolean;
begin
  if (VST.GetFirst() = nil) then exit;
  Flush := False;
  if (node = nil) then
   begin
     PopupNotifier1.Text:='';
     PopupNotifier1.Hide;
     exit;
   end;
  GData := VST.GetNodeData(node);
  BData := VST.GetNodeData(GData^.BelongsTo);
  TData := VST.GetNodeData(GData^.TopParent);
  Y1 := VST.ClientOrigin.y;
  X1 := VST.ClientOrigin.x+VST.Width+5;
  OControl := Form1.ActiveControl;
  if (ViewGameInfo) and (GData^.FType = 'Game') and (GData^.BelongsTo <> nil) then
    begin
      if (PopupNotifier1.vNotifierForm.Color <> clSkyBlue) then
        Flush := True;
      PopupNotifier1.vNotifierForm.Color := clSkyBlue;
      PopupNotifier1.vNotifierForm.htmlColor := clSkyBlue;
      PopupNotifier1.vNotifierForm.htmlhexColor := TColorToHex(clSkyBlue);
      if (not PopupNotifier1.Visible) then
        PopupNotifier1.ShowAtPos(X1,Y1);
      DisplayText:='<table width="93%"><tr><td align="left" width="40%"><b>Game:</b></td><td align="left">'+GData^.Name+'</td><tr><td align="left">'+'<b>Game Code:</b></td><td align="left">'+GData^.Code+'</td></tr><tr><td align="left">'+'<b>Parent Folder:</b></td><td align="left">'+BData^.Name+'</td></tr>';
      if (GData^.TopParent <> nil) then
       DisplayText:=DisplayText+'<tr><td align="left"><b>Top Folder:</b></td><td align="left">'+TData^.Name+'</td></tr>';
      DisplayText:=DisplayText+'</table>';
      if (Flush) then
       begin
         PopupNotifier1.Text:='';
         PopupNotifier1.vNotifierForm.Repaint;
       end;
      PopupNotifier1.Text:=DisplayText;
      OControl.SetFocus;
    end
  else if (ViewGameInfo) and (GData^.FType = 'Shortcut') and (GData^.BelongsTo <> nil) then
    begin
      if (PopupNotifier1.vNotifierForm.Color <> clFuchsia) then
        Flush := True;
      PopupNotifier1.vNotifierForm.Color := clFuchsia;
      PopupNotifier1.vNotifierForm.htmlColor := clFuchsia;
      PopupNotifier1.vNotifierForm.htmlhexColor := TColorToHex(clFuchsia);
      if (not PopupNotifier1.Visible) then
        PopupNotifier1.ShowAtPos(X1,Y1);
      DisplayText:='<table width="93%"><tr><td align="left" width="40%"><b>Game:</b></td><td align="left">'+GData^.Name+'</td><tr><td align="left">'+'<b>Game Code:</b></td><td align="left">'+GData^.Code+'</td></tr><tr><td align="left">'+'<b>Parent Folder:</b></td><td align="left">'+BData^.Name+'</td></tr>';
      if (GData^.TopParent <> nil) then
       DisplayText:=DisplayText+'<tr><td align="left"><b>Top Folder:</b></td><td align="left">'+TData^.Name+'</td></tr>';
      DisplayText:=DisplayText+'</table>';
      if (Flush) then
       begin
         PopupNotifier1.Text:='';
         PopupNotifier1.vNotifierForm.Repaint;
       end;
      PopupNotifier1.Text:=DisplayText;
      OControl.SetFocus;
    end
  else if (ViewGameInfo) and (GData^.FType = 'Folder') then
    begin
      MPath := ExtractFileDir(XMLEdit.Caption);
      MPath := ExtractFilePath(MPath);
      MPath := MPath+'folder_images';
      if (PopupNotifier1.vNotifierForm.Color <> clSkyBlue) then
        Flush := True;
      PopupNotifier1.vNotifierForm.Color := clSkyBlue;
      PopupNotifier1.vNotifierForm.htmlColor := clSkyBlue;
      PopupNotifier1.vNotifierForm.htmlhexColor := TColorToHex(clSkyBlue);
      if (not PopupNotifier1.Visible) then
        PopupNotifier1.ShowAtPos(X1,Y1);
      DisplayText:='<table width="93%"><tr><td align="left" width="40%"><b>Folder:</b></td><td align="left">'+GData^.Name+'</td>';
      if (GData^.BelongsTo <> nil) and (GData^.BelongsTo <> node) then
        DisplayText:=DisplayText+'<tr><td align="left">'+'<b>Parent Folder:</b></td><td align="left">'+BData^.Name+'</td></tr>';
      if (GData^.TopParent <> nil) and (GData^.TopParent <> node) then
        DisplayText:=DisplayText+'<tr><td align="left"><b>Top Folder:</b></td><td align="left">'+TData^.Name+'</td></tr>';
      if (GData^.Icon <> '') then
        begin
          DisplayText:=DisplayText+'<tr><td align="center" colspan="2"><br/><img height="50" width="50" src="'+GData^.Icon+'.png" /></td></tr>';
        end;
      DisplayText:=DisplayText+'</table>';
      if (Flush) then
       begin
         PopupNotifier1.Text:='';
         PopupNotifier1.vNotifierForm.Repaint;
       end;
      PopupNotifier1.Text:=DisplayText;
      OControl.SetFocus;
    end;
end;

procedure TForm1.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PTreeData;
begin
  Data := TVirtualStringTree(Sender).GetNodeData(Node);
  if Assigned(Data) then
    begin
      Data^.BelongsTo:=nil;
      Data^.TopParent:=nil;
      Data^.Code:='';
      Data^.FilePath:='';
      Data^.FType:='';
      Data^.Name:='';
      Data^.Hash:='';
      Data^.Icon:='';
    end;
end;

procedure TForm1.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TForm1.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  Data: PTreeData;
begin
  Data := TVirtualStringTree(Sender).GetNodeData(Node);
  CellText := Data^.Name;
end;

procedure TForm1.VSTIncrementalSearch(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: String; var Result: Integer);
var
  S,PText: String;
  GData: PTreeData;
begin
  S := SearchText;
  GData := TVirtualStringTree(Sender).GetNodeData(Node);
  PText := GData^.Name;
  Result := StrLIComp(PChar(S), PChar(PText), Min(Length(S), Length(PText)));
end;

procedure TForm1.VSTMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) then
   MouseInput.Click(mbLeft,[]);
end;

procedure TForm1.VSTMouseLeave(Sender: TObject);
begin
  VST.Repaint;
  if PopupNotifier1.Visible then
   begin
     PopupNotifier1.Text:='';
     PopupNotifier1.Hide;
   end;
end;

procedure TForm1.VSTMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
  );
var
  node: PVirtualNode;
  GData,BData,TData: PTreeData;
  X1,Y1: Integer;
  OControl: TWinControl;
  DisplayText,MPath: String;
  Flush: Boolean;
begin
  if (VST.GetFirst() = nil) then exit;
  Flush := False;
  node := VST.GetNodeAt(X, Y);
  if (node = nil) then
   begin
     PopupNotifier1.Text:='';
     PopupNotifier1.Hide;
     exit;
   end;
  GData := VST.GetNodeData(node);
  BData := VST.GetNodeData(GData^.BelongsTo);
  TData := VST.GetNodeData(GData^.TopParent);
  Y1 := VST.ClientOrigin.y;
  X1 := VST.ClientOrigin.x+VST.Width+5;
  OControl := Form1.ActiveControl;
  if (ViewGameInfo) and (GData^.FType = 'Game') and (GData^.BelongsTo <> nil) then
    begin
      if (PopupNotifier1.vNotifierForm.Color <> clSkyBlue) then
        Flush := True;
      PopupNotifier1.vNotifierForm.Color := clSkyBlue;
      PopupNotifier1.vNotifierForm.htmlColor := clSkyBlue;
      PopupNotifier1.vNotifierForm.htmlhexColor := TColorToHex(clSkyBlue);
      if (not PopupNotifier1.Visible) then
        PopupNotifier1.ShowAtPos(X1,Y1);
      DisplayText:='<table width="93%"><tr><td align="left" width="40%"><b>Game:</b></td><td align="left">'+GData^.Name+'</td><tr><td align="left">'+'<b>Game Code:</b></td><td align="left">'+GData^.Code+'</td></tr><tr><td align="left">'+'<b>Parent Folder:</b></td><td align="left">'+BData^.Name+'</td></tr>';
      if (GData^.TopParent <> nil) then
       DisplayText:=DisplayText+'<tr><td align="left"><b>Top Folder:</b></td><td align="left">'+TData^.Name+'</td></tr>';
      DisplayText:=DisplayText+'</table>';
      if (Flush) then
       begin
         PopupNotifier1.Text:='';
         PopupNotifier1.vNotifierForm.Repaint;
       end;
      PopupNotifier1.Text:=DisplayText;
      OControl.SetFocus;
    end
  else if (ViewGameInfo) and (GData^.FType = 'Shortcut') and (GData^.BelongsTo <> nil) then
    begin
      if (PopupNotifier1.vNotifierForm.Color <> clFuchsia) then
        Flush := True;
      PopupNotifier1.vNotifierForm.Color := clFuchsia;
      PopupNotifier1.vNotifierForm.htmlColor := clFuchsia;
      PopupNotifier1.vNotifierForm.htmlhexColor := TColorToHex(clFuchsia);
      if (not PopupNotifier1.Visible) then
        PopupNotifier1.ShowAtPos(X1,Y1);
      DisplayText:='<table width="93%"><tr><td align="left" width="40%"><b>Game:</b></td><td align="left">'+GData^.Name+'</td><tr><td align="left">'+'<b>Game Code:</b></td><td align="left">'+GData^.Code+'</td></tr><tr><td align="left">'+'<b>Parent Folder:</b></td><td align="left">'+BData^.Name+'</td></tr>';
      if (GData^.TopParent <> nil) then
       DisplayText:=DisplayText+'<tr><td align="left"><b>Top Folder:</b></td><td align="left">'+TData^.Name+'</td></tr>';
      DisplayText:=DisplayText+'</table>';
      if (Flush) then
       begin
         PopupNotifier1.Text:='';
         PopupNotifier1.vNotifierForm.Repaint;
       end;
      PopupNotifier1.Text:=DisplayText;
      OControl.SetFocus;
    end
  else if (ViewGameInfo) and (GData^.FType = 'Folder') then
    begin
      MPath := ExtractFileDir(XMLEdit.Caption);
      MPath := ExtractFilePath(MPath);
      MPath := MPath+'folder_images';
      if (PopupNotifier1.vNotifierForm.Color <> clSkyBlue) then
        Flush := True;
      PopupNotifier1.vNotifierForm.Color := clSkyBlue;
      PopupNotifier1.vNotifierForm.htmlColor := clSkyBlue;
      PopupNotifier1.vNotifierForm.htmlhexColor := TColorToHex(clSkyBlue);
      if (not PopupNotifier1.Visible) then
        PopupNotifier1.ShowAtPos(X1,Y1);
      DisplayText:='<table width="93%"><tr><td align="left" width="40%"><b>Folder:</b></td><td align="left">'+GData^.Name+'</td>';
      if (GData^.BelongsTo <> nil) and (GData^.BelongsTo <> node) then
        DisplayText:=DisplayText+'<tr><td align="left">'+'<b>Parent Folder:</b></td><td align="left">'+BData^.Name+'</td></tr>';
      if (GData^.TopParent <> nil) and (GData^.TopParent <> node) then
        DisplayText:=DisplayText+'<tr><td align="left"><b>Top Folder:</b></td><td align="left">'+TData^.Name+'</td></tr>';
      if (GData^.Icon <> '') then
        begin
          DisplayText:=DisplayText+'<tr><td align="center" colspan="2"><br/><img height="50" width="50" src="'+GData^.Icon+'.png" /></td></tr>';
        end;
      DisplayText:=DisplayText+'</table>';
      if (Flush) then
       begin
         PopupNotifier1.Text:='';
         PopupNotifier1.vNotifierForm.Repaint;
       end;
      PopupNotifier1.Text:=DisplayText;
      OControl.SetFocus;
    end;
end;

procedure TForm1.VSTNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo
  );
var
  Node: PVirtualNode;
begin
  Node := HitInfo.HitNode;
  if (SVST.Selected[Node] and (LastVSTNode = Node) and (GetKeyPressed(VK_LCONTROL) or GetKeyPressed(VK_RCONTROL))) then
    begin
      VST.ClearSelection;
      LastVSTNode := nil;
    end
  else
    LastVSTNode := Node;
end;

procedure TForm1.InitSlots;
var
  Ini: TIniFile;
  AppPath: String;
  SlotList: TStringList;
  tm,tm2: TMenuItem;
  i: Integer;
begin
  AppPath := ExtractFilePath(ParamStr(0));
  Ini := TIniFile.Create(AppPath+'config.ini');
  SlotList := TStringList.Create;
  try
    Ini.ReadSectionValues('presets',SlotList);
    for i := 0 to SlotList.Count-1 do
      begin
        tm := TMenuItem.Create(MainMenu1);
        tm.Caption := SlotList.Names[i];
        tm.Tag := i;
        PresetsOption.Insert(i,tm);
        tm2 := TMenuItem.Create(tm);
        tm2.Caption := 'Load';
        tm2.Tag := i;
        tm2.OnClick:=@LoadSlotClick;
        tm.Add(tm2);
        tm2 := TMenuItem.Create(tm);
        tm2.Caption := 'Save';
        tm2.Tag := i;
        tm2.OnClick:=@SaveSlotClick;
        tm.Add(tm2);
        tm2 := TMenuItem.Create(tm);
        tm2.Caption := 'Rename';
        tm2.Tag := i;
        tm2.OnClick:=@RenameSlotClick;
        tm.Add(tm2);
        tm := TMenuItem.Create(MainMenu1);
        tm.Caption := SlotList.Names[i];
        tm.Tag := i;
        tm.OnClick:=@DeleteSlotClick;
        DeletePresetOption.Add(tm);
      end;
  finally
    SlotList.Free;
    Ini.Free;
  end;
end;

procedure TForm1.GameInfoOptionClick(Sender: TObject);
begin
  if (ViewGameInfo) then ViewGameInfo := False
  else ViewGameInfo := True;
  if (not ViewGameInfo) then PopupNotifier1.Visible:=False;
  GameInfoOption.Checked := ViewGameInfo;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewShortcuts,ViewGameInfo,HomeName,VST);
end;

procedure TForm1.LoadBtnClick(Sender: TObject);
var
  CheckedGames: String;
  ChkStringList: TStringList;
begin
  if (not LoadBtn.Focused) and (not GetKeyPressed(VK_MENU)) then exit;
  if (XMLEdit.Caption = '') or (not FileExists(XMLEdit.Caption)) then
    begin
      ShowMessage('Error: Supplied XML filename invalid.');
      exit;
    end;
  LastUsedXML := XMLEdit.Caption;
  VST.Clear;
  SVST.Clear;
  CheckedGames := GetSavedChecked(0);
  XML2VTree(SVST,XMLEdit.Caption,False,HomeName);
  XML2VTree(VST,XMLEdit.Caption,True,HomeName);
  try
    ChkStringList := TStringList.Create;
    ChkStringList.Clear;
    ChkStringList.Delimiter := ',';
    ChkStringList.StrictDelimiter := True;
    ChkStringList.DelimitedText := CheckedGames;
    ChkStringList.Sort;
    CheckVNodesList(VST,ChkStringList);
  finally
    ChkStringList.Free;
  end;
  VST.FullExpand();
  VPopulatePathData(VST,GamesEdit.Caption);
  VPopulateFolderData(VST,GamesEdit.Caption);
  VPopulateFolderData(SVST,GamesEdit.Caption);
  VPopulateShortcuts(VST,GamesEdit.Caption);
  if (VST.GetFirst() <> nil) then
    case ViewStyle of
       0: Form1.FlatOptionClick(Self);
       1: Form1.ParentOptionClick(Self);
       2: Form1.ParentChildOptionClick(Self);
    end;
  if (ViewShortcuts) then
    VShortcutsView(VST,ViewShortcuts)
  else if ViewSelected then
    VSelectionView(VST,ViewSelected);
  VGetCheckCount(VST, StatusBar1.Panels.Items[0], True);
end;

procedure TForm1.MenuItem15Click(Sender: TObject);
begin
  AddHomeIcons(GamesEdit.Caption,ProgressBar,StatusBar1);
  if (MessageDlg('Update','Completed.',mtInformation,[mbOk],0) = mrOk) then
    begin
      ProgressBar.Position:=0;
      ProgressBar.Visible:= False;
      statusPanel1.Text:='';
      VGetCheckCount(VST, StatusBar1.Panels.Items[0], True);
    end;
end;

procedure TForm1.MenuItem16Click(Sender: TObject);
begin
  BackupXML(XMLEdit.Caption,BackupXMLDlg);
end;

procedure TForm1.MenuItem17Click(Sender: TObject);
begin
  RestoreXML(XMLEdit.Caption,RestoreXMLDlg,HakchiConfigDlg);
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  HelpForm.Visible:=True;
  HelpForm.Memo1.Lines.LoadFromFile(ExtractFilePath(ParamStr(0))+'\README.txt');
  HelpForm.BringToFront;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  SaveBtn.Click;
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
  Form1.Close;
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
begin
  UpdateAboutInfo;
  AboutForm.ShowModal;
end;

procedure TForm1.NANDCheckBoxChange(Sender: TObject);
begin
  if (NANDCheckBox.Checked) then
    GamesEdit.Enabled := False
  else
    GamesEdit.Enabled := True;
end;

function TForm1.GetKeyPressed(const VKeyCode: Integer): Boolean;
begin
  Result := GetKeyState(VKeyCode) and $80 <> 0;
end;

procedure TForm1.SaveBtnClick(Sender: TObject);
var
  fname,LastFolder,NandPath: string;
begin
  if (not SaveBtn.Focused) and (not GetKeyPressed(VK_MENU)) then exit;
  if (LastUsedXML <> XMLEdit.Caption) then
    begin
      ShowMessage('Warning: New XML filename entered, please click load button first.');
      exit;
    end;
  if (NANDCheckBox.Checked) then
    begin
      NandPath := CloverShellPath;
      if (NandPath = '') then
        begin
          ShowMessage('Error Detecting SNES-Mini.  Please make sure it is plugged in and try again.');
          exit;
        end;
      if (LastFavFolder <> '') then
        begin
          if (not CloverFileExists(fname+LastFavFolder+'\.fav')) then
            LastFavFolder := '';
        end;
      LastFolder := NANDGetLastFolderNumber;
      NandPath := '/var/lib/hakchi/games/snes-'+NandPath+'/';
      if (CloverFileExists(NandPath+'.fav')) then
        LastFavFolder := LastFolder
      else
        LastFavFolder := GenerateFolderName(LastFolder);
      statusPanel0.Text:='';
    // To Do: Update NANDCreateFaveLinks for VST 4/21/2018
    //  NANDCreateFaveLinks(TreeView2,ProgressBar,StatusBar1,LastFavFolder);
    end
  else
    begin
      fname := GamesEdit.Caption;
      fname := fname + '\';
      if (fname = '') or (not DirectoryExists(fname)) then
        begin
          ShowMessage('Error: Supplied games folder path does not exist.');
          exit;
        end;
      if (LastFavFolder <> '') then
        begin
          if (not FileExists(fname+LastFavFolder+'\.fav')) then
            LastFavFolder := '';
        end;
      if (LastFavFolder = '') then
        begin
          LastFolder := GetLastFolderNumber(fname);
          if (FileExists(fname+LastFolder+'\.fav')) then
            LastFavFolder := LastFolder
          else
            LastFavFolder := GenerateFolderName(GetLastFolderNumber(fname));
        end;
      statusPanel0.Text:='';
      CreateFaveLinks(fname,VST,ProgressBar,StatusBar1,LastFavFolder);
    end;
  if (MessageDlg('Save','Completed.',mtInformation,[mbOk],0) = mrOk) then
    begin
      ProgressBar.Position:=0;
      ProgressBar.Visible:= False;
      statusPanel1.Text:='';
      VGetCheckCount(VST, StatusBar1.Panels.Items[0], True);
      SaveConfig(XMLEdit.Caption,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewShortcuts,ViewGameInfo,HomeName,VST);
    end;
end;

procedure TForm1.SelectedOptionClick(Sender: TObject);
begin
  if (ViewSelected) then ViewSelected := False
  else
    begin
      ViewShortcuts := False;
      ViewSelected := True;
      ShortcutsOption.Checked := False;
    end;
  SelectedOption.Checked := ViewSelected;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewShortcuts,ViewGameInfo,HomeName,VST);
  VST.BeginUpdate;
  VSelectionView(VST,ViewSelected);
  VST.EndUpdate;
  if (VST.GetFirstSelected() <> nil) and ((not ViewSelected) or (ViewSelected and VNodeChecked(VST,VST.GetFirstSelected()))) then
    VST.ScrollIntoView(VST.GetFirstSelected,False)
  else if (VST.GetFirst <> nil) then
    VST.ScrollIntoView(VST.GetFirstVisible(),False);
end;

procedure TForm1.ShortcutsOptionClick(Sender: TObject);
begin
  if (ViewShortcuts) then ViewShortcuts := False
  else
    begin
      ViewShortcuts := True;
      ViewSelected := False;
      SelectedOption.Checked := False;
    end;
  ShortcutsOption.Checked := ViewShortcuts;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewShortcuts,ViewGameInfo,HomeName,VST);
  VST.BeginUpdate;
  VShortcutsView(VST,ViewShortcuts);
  VST.EndUpdate;
  if (VST.GetFirstSelected() <> nil) and ((not ViewSelected) or (ViewSelected and VNodeChecked(VST,VST.GetFirstSelected()))) then
    VST.ScrollIntoView(VST.GetFirstSelected,False)
  else if (VST.GetFirst <> nil) then
    VST.ScrollIntoView(VST.GetFirstVisible(),False);
end;

procedure TForm1.SVSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  GData1,GData2: PTreeData;
begin
  Result := 0;
  GData1 := SVST.GetNodeData(Node1);
  GData2 := SVST.GetNodeData(Node2);
  if ((GData1^.FType = 'Game') or (GData1^.FType = 'Shortcut')) and (GData2^.FType = 'Folder') then
    Result := -1
  else if (GData1^.FType = 'Folder') and ((GData2^.FType = 'Game') or (GData2^.FType = 'Shortcut')) then
    Result := 1
  else if ((GData1^.FType = 'Game') or (GData1^.FType = 'Shortcut')) and ((GData2^.Ftype = 'Game') or (GData2^.FType = 'Shortcut')) then
    Result := CompareText(GData1^.Name,GData2^.Name);
end;

procedure TForm1.SVSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
  const CellRect: TRect; var DefaultDraw: Boolean);
var
  NodeRect,TmRect: TRect;
  DisplayText: String;
  OldBrushStyle: TBrushStyle;
  OldStyle: TFontStyles;
  b,c : Boolean;
  clOld: TColor;
  GData: PTreeData;
begin
  with TVirtualStringTree(Sender) do
    begin
      b := Selected[Node];
      c := (Node = FocusedNode);

      GData := GetNodeData(Node);
      if (GData^.FType = 'Folder') then
        begin
          DefaultDraw := True;
          exit;
        end;

      with TargetCanvas do
      begin
        DefaultDraw := False;
        NodeRect := CellRect;
        clOld := Font.Color;
        OldBrushStyle := Brush.Style;
        OldStyle := Font.Style;

        Brush.Style := bsSolid;
        Brush.Color := clWhite;
        TmRect := NodeRect;
        TmRect.Left:=0;
        TmRect.Right:=TVirtualStringTree(Sender).Width;
        TmRect.Top:=TmRect.Top-2;
        TmRect.Bottom:=TmRect.Bottom+2;
        FillRect(TmRect);

        Pen.Style := psClear;
        Brush.Style := bsClear;
        Pen.Color := clWhite;
        if (c) then DrawFocusRect(NodeRect);

        Brush.Color := Brush.Color;
        FillRect(NodeRect);
        Brush.Style := bsClear;
        If b then Font.Style := [fsBold];
        Font.Color := clBlue;

        DisplayText := CellText;
        TextOut(NodeRect.Left, NodeRect.Top, DisplayText);
    //    TextRect(NodeRect, NodeRect.Left, NodeRect.Top, DisplayText);
        Font.Color := clOld;
        Brush.Style := OldBrushStyle;
        Font.Style := OldStyle;
      end;
    end;
end;

procedure TForm1.SVSTMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
begin
  if (Button = mbRight) then
    MouseInput.Click(mbLeft,[]);
  Node := SVST.GetNodeAt(X,Y);
  if (Node = nil) then SVST.ClearSelection;
end;

procedure TForm1.LoadSlotClick(Sender: TObject);
var
  AppPath,CheckedGames: String;
  Ini: TIniFile;
  ChkStringList: TStringList;
  TagNum: Integer;
begin
  AppPath := ExtractFilePath(ParamStr(0));
  Ini := TIniFile.Create(AppPath+'config.ini');
  try
    TagNum := TMenuItem(Sender).Tag;
    CheckedGames := Ini.ReadString('presets',PresetsOption.Items[TagNum].Caption,'');
    ChkStringList := TStringList.Create;
    try
      ChkStringList.Clear;
      ChkStringList.Delimiter := ',';
      ChkStringList.StrictDelimiter := True;
      ChkStringList.DelimitedText := CheckedGames;
      ChkStringList.Sort;
      ChkStringList.Sorted := True;
      CheckVNodesList(VST,ChkStringList);
    finally
      ChkStringList.Free;
    end;
  finally
    Ini.Free;
  end;
  VShortcutsView(VST,ViewShortcuts);
  VSelectionView(VST,ViewSelected);
  VGetCheckCount(VST, StatusPanel0, True);
end;

procedure TForm1.DeleteSlotClick(Sender: TObject);
var
  AppPath: String;
  Ini: TIniFile;
  TagNum: Integer;
  i: integer;
begin
  AppPath := ExtractFilePath(ParamStr(0));
  TagNum := TMenuItem(Sender).Tag;
  if (MessageDlg('Confirm','Do you wish to delete preset "'+
      PresetsOption.Items[TagNum].Caption+'"?',mtConfirmation,[mbYes,mbNo],0)=mrYes) then
    begin
      Ini := TIniFile.Create(AppPath+'config.ini');
      try
        Ini.DeleteKey('presets',PresetsOption.Items[TagNum].Caption);
      finally
        Ini.Free;
      end;
      PresetsOption.Delete(TagNum);
      for i := 0 to PresetsOption.Count-4 do
        PresetsOption.Items[i].Tag:=i;
      DeletePresetOption.Delete(TagNum);
      for i := 0 to DeletePresetOption.Count-1 do
        DeletePresetOption.Items[i].Tag:=i;
    end;
end;

procedure TForm1.RenameSlotClick(Sender: TObject);
var
  SlotName,AppPath,CheckedGames: String;
  Ini: TIniFile;
  TagNum: Integer;
begin
  SlotName := Trim(InputBox('Slot Name','Please enter a name for this preset',''));
  if (SlotName = '') then exit;
  if (length(SlotName) > 15) then SlotName := Copy(SlotName, 1, 15);
  AppPath := ExtractFilePath(ParamStr(0));
  Ini := TIniFile.Create(AppPath+'config.ini');
  try
    TagNum := TMenuItem(Sender).Tag;
    CheckedGames := Ini.ReadString('presets',PresetsOption.Items[TagNum].Caption,'');
    Ini.DeleteKey('presets',PresetsOption.Items[TagNum].Caption);
    Ini.WriteString('presets',SlotName,CheckedGames);
    PresetsOption.Items[TagNum].Caption:=SlotName;
    DeletePresetOption.Items[TagNum].Caption:=SlotName;
  finally
    Ini.Free;
  end;
end;

procedure TForm1.SaveSlotClick(Sender: TObject);
var
  AppPath,CheckedGames,CheckedHashes: String;
  Ini: TIniFile;
  GL: TStringList;
  TagNum: Integer;
  i: integer;
begin
  AppPath := ExtractFilePath(ParamStr(0));
  Ini := TIniFile.Create(AppPath+'config.ini');
  try
    TagNum := TMenuItem(Sender).Tag;
    GL := TStringList.Create;
    try
      CheckedGames := '';
      VGetCheckedCodes(VST,GL);
      for i := 0 to (GL.Count-1) do
        begin
          CheckedGames := CheckedGames+' '+GL[i];
        end;
      CheckedGames := StringReplace(Trim(CheckedGames),' ',',',[rfReplaceAll]);
      VGetCheckedHashes(VST,GL);
      for i := 0 to (GL.Count-1) do
        begin
          CheckedHashes := CheckedHashes+' '+GL[i];
        end;
      CheckedHashes := StringReplace(Trim(CheckedHashes),' ',',',[rfReplaceAll]);
      CheckedGames := CheckedGames+','+CheckedHashes;
      Ini.WriteString('presets',PresetsOption.Items[TagNum].Caption,CheckedGames);
    finally
      GL.Free;
    end;
  finally
    Ini.Free;
  end;
  VShortcutsView(VST,ViewShortcuts);
  VSelectionView(VST,ViewSelected);
  VGetCheckCount(VST, StatusPanel0, True);
end;

{ procedure TForm1.TreeView2KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  CurNode,fNode: TTreeNode;
begin
  if (Key = VK_SPACE) then
    begin
      CurNode := TreeView2.Selected;
      ToggleCheck(CurNode);
    end
  else if not (ssAlt in Shift) then
    begin
      CurNode := TreeView2.Selected;
      if (CurNode = nil) then exit;
      fNode := FindNextNodeMatch(chr(Key),TreeView2,CurNode);
      if (fNode = nil) then exit;
      TreeView2.Selected := fNode;
      fNode.MakeVisible;
      Key := 0;
    end;
end; }

procedure TForm1.ClearShcBtnClick(Sender: TObject);
begin
  if (not ClearShcBtn.Focused) and (not GetKeyPressed(VK_MENU)) then exit;
  VClearShortcuts(SVST);
  SVST.Refresh;
end;

procedure TForm1.AddShortCutsBtnClick(Sender: TObject);
begin
  VAddShortcutSelections(VST,SVST);
end;

procedure TForm1.AddPresetOptionClick(Sender: TObject);
var
  AppPath,NewName: String;
  Ini: TIniFile;
  i,TagNum: Integer;
  tm,tm2: TMenuItem;
begin
  NewName := Trim(InputBox('Add Preset','Please enter a name for this preset',''));
  if (NewName = '') then exit;
  if (Length(NewName) > 15) then NewName := Copy(NewName,1,15);
  TagNum := -1;
  for i := 0 to PresetsOption.Count-4 do
    begin
      TagNum := PresetsOption.Items[i].Tag;
    end;
  TagNum := TagNum + 1;
  tm := TMenuItem.Create(MainMenu1);
  tm.Caption := NewName;
  tm.Tag := TagNum;
  PresetsOption.Insert(TagNum,tm);
  tm2 := TMenuItem.Create(tm);
  tm2.Caption := 'Load';
  tm2.Tag := TagNum;
  tm2.OnClick:=@LoadSlotClick;
  tm.Add(tm2);
  tm2 := TMenuItem.Create(tm);
  tm2.Caption := 'Save';
  tm2.Tag := TagNum;
  tm2.OnClick:=@SaveSlotClick;
  tm.Add(tm2);
  tm2 := TMenuItem.Create(tm);
  tm2.Caption := 'Rename';
  tm2.Tag := TagNum;
  tm2.OnClick:=@RenameSlotClick;
  tm.Add(tm2);
  tm := TMenuItem.Create(MainMenu1);
  tm.Caption := NewName;
  tm.Tag := TagNum;
  tm.OnClick:=@DeleteSlotClick;
  DeletePresetOption.Add(tm);
  AppPath := ExtractFilePath(ParamStr(0));
  Ini := TIniFile.Create(AppPath+'config.ini');
  try
    Ini.WriteString('presets',NewName,'');
  finally
    Ini.Free;
  end;
end;

procedure TForm1.AddPrefixesOptionClick(Sender: TObject);
var
  MPath: String;
begin
  MPath := ExtractFileDir(XMLEdit.Caption);
  MPath := ExtractFilePath(MPath);
  MPath := MPath+'games_snes';
  if (not DirectoryExists(MPath)) then exit;
  if (MessageDlg('Confirm','This feature will add console prefixes to the game titles in Hakchi.  Do you wish to proceed?',
      mtConfirmation,[mbYes,mbNo],0)=mrYes) then
    begin
      statusPanel0.Text:='';
      AddGamePrefixes(MPath,ProgressBar,StatusBar1);
      if (MessageDlg('Operation','Completed operation.',mtInformation,[mbOk],0) = mrOk) then
      begin
        ProgressBar.Position:=0;
        ProgressBar.Visible:= False;
        statusPanel1.Text:='';
        VGetCheckCount(VST, StatusBar1.Panels.Items[0], True);
      end;
    end;
end;

procedure TForm1.CreateFolderMnuClick(Sender: TObject);
var
  NewName,FName,LFName,FPath,MPath,ConsolePath: String;
  Node,nNode,nNode2,pNode,fNode,lNode,lNode2: PVirtualNode;
  GData,GData2,GData3: PTreeData;
  DeskFile: TStringList;
begin
  Node := SVST.GetFirstSelected();
  if (Node = nil) then exit;
  ConsolePath := GamesEdit.Caption;
  FPath := ExtractFilePath(ParamStr(0));
  GData := SVST.GetNodeData(Node);
  FName := GetLastFolderNumber(GData^.FilePath+'\',True);
  ShowMessage('FName for '+Gdata^.FilePath+' is '+FName);
  if (FName = '') then exit;
  Fname := GenerateFolderName(FName,True);
  if (ConsolePath[Length(ConsolePath)] <> '\') then
    ConsolePath := ConsolePath+'\';
  NewName := Trim(InputBox('Create Folder','Please enter a name for this folder',''));
  if (NewName = '') then exit;
  if (Length(NewName) > 20) then NewName := Copy(NewName,1,20);

  PngSelectDlg.InitialDir := FPath;
  PngSelectDlg.Filter:='Portable Network Graphics|*.png';
  PngSelectDlg.FileName := FPath+'default.png';
  if (PngSelectDlg.Execute) then
    begin
      if (FileExists(PngSelectDlg.FileName)) then
        begin
          if (Node = SVST.GetFirst()) then pNode := nil
          else pNode := Node;
          nNode := SVST.AddChild(pNode);
          LFName := GetLastFolderNumber(ConsolePath);
          LFName := GenerateFolderName(LFName);
          GData2 := SetNodeData(SVST,nNode,NewName,'Folder',ConsolePath+LFName,'','',pNode);
          GData2^.Icon := GData2^.Hash;
          if (GData2^.TopParent = nil) then GData2^.TopParent := nNode;
          CreateDir(GData2^.FilePath);
          CreateDir(GData^.FilePath+'\CLV-S-00'+LFName);

        // LFName = 00x
        // FName = CLV-S-00xxx of subfolder

          DeskFile := TStringList.Create;
          try
            DeskFile.LineBreak := #10;
            DeskFile.Add('[Desktop Entry]');
            DeskFile.Add('Type=Application');
            DeskFile.Add('Exec=/bin/chmenu '+LFName+' ');
            DeskFile.Add('Path=/var/saves/FOLDER');
            DeskFile.Add('Name='+NewName);
            DeskFile.Add('Icon=/var/games/CLV-S-00'+LFName+'/CLV-S-00'+LFName+'.png');
            DeskFile.Add('');
            DeskFile.Add('[X-CLOVER Game]');
            DeskFile.Add('Code=CLV-S-00'+LFName);
            DeskFile.Add('TestID=777');
            DeskFile.Add('ID=0');
            DeskFile.Add('Players=1');
            DeskFile.Add('Simultaneous=0');
            DeskFile.Add('ReleaseDate=7777-77-77');
            DeskFile.Add('SaveCount=0');
            DeskFile.Add('SortRawTitle='+LowerCase(NewName));
            DeskFile.Add('SortRawPublisher=ZZZZZZZZZX');
            DeskFile.Add('Copyright=hakchi2 ©2017 Alexey ''Cluster'' Avdyukhin');

            DeskFile.SaveToFile(GData^.FilePath+'\CLV-S-00'+LFName+'\CLV-S-00'+LFName+'.desktop');
          finally
            DeskFile.Free;
          end;

          CreateDir(GData2^.FilePath+'\CLV-S-00000');
          CopyFile(ConsolePath+'001\CLV-S-00000\CLV-S-00000.desktop',GData2^.FilePath+'\CLV-S-00000\CLV-S-00000.desktop');
          CopyFile(ConsolePath+'001\CLV-S-00000\CLV-S-00000.png',GData2^.FilePath+'\CLV-S-00000\CLV-S-00000.png');
          CopyFile(ConsolePath+'001\CLV-S-00000\CLV-S-00000_small.png',GData2^.FilePath+'\CLV-S-00000\CLV-S-00000_small.png');
          SVST.ScrollIntoView(nNode,False);

          lNode := VST.GetFirst();
          while (lNode <> nil) do
            begin
              lNode2 := VST.GetNextSibling(lNode);
              fNode := nil;
              fNode := FindVNodeData(VST,lNode,GData^.Hash,5);
              if (fNode <> nil) then
                begin
                  lNode2 := nil;
                  if (fNode = VST.GetFirst()) then pNode := nil
                  else pNode := fNode;

                  nNode2 := VST.AddChild(pNode);
                  GData3 := SetNodeData(VST,nNode2,GData2^.Name,GData2^.FType,
                            ConsolePath+LFName,GData2^.Code,GData2^.Icon,pNode);
                  if (GData3^.TopParent = nil) then GData3^.TopParent := nNode2;
                  VST.ScrollIntoView(nNode2,False);
                end;
              lNode := lNode2;
            end;
          VST.Refresh;
          SVST.Refresh;

          ResizeImage(PngSelectDlg.FileName,FPath+GData2^.Hash+'.png', 204, 204);
          ResizeImage(PngSelectDlg.FileName,FPath+GData2^.Hash+'_small.png', 40, 40);
          CopyFile(FPath+GData2^.Hash+'.png',GData^.FilePath+'\CLV-S-00'+LFName+'\CLV-S-00'+LFName+'.png');
          CopyFile(FPath+GData2^.Hash+'_small.png',GData^.FilePath+'\CLV-S-00'+LFName+'\CLV-S-00'+LFName+'_small.png');

          MPath := ExtractFileDir(XMLEdit.Caption);
          MPath := ExtractFilePath(MPath);
          MPath := MPath+'folder_images';
          if (DirectoryExists(MPath)) then
            CopyFile(FPath+GData2^.Hash+'.png',MPath+'\'+GData2^.Hash+'.png');
          DeleteFile(FPath+GData2^.Hash+'.png');
          DeleteFile(FPath+GData2^.Hash+'_small.png');
          if (FileExists(XMLEdit.Caption)) then
            begin
              MPath := ExtractFilePath(XMLEdit.Caption);
              if (FileExists(MPath+'folders_snes.bak')) then
                DeleteFile(MPath+'folders_snes.bak');
              CopyFile(XMLEdit.Caption,MPath+'folders_snes.bak');
            end;
          VTreeToXML(VST,XMLEdit.Caption);
        end;
    end;
end;

procedure TForm1.DelPrefixesOptionClick(Sender: TObject);
var
  MPath: String;
begin
  MPath := ExtractFileDir(XMLEdit.Caption);
  MPath := ExtractFilePath(MPath);
  MPath := MPath+'games_snes';
  if (not DirectoryExists(MPath)) then exit;
  if (MessageDlg('Confirm','This feature will remove console prefixes previously added from game titles in Hakchi.  Do you wish to proceed?',
      mtConfirmation,[mbYes,mbNo],0)=mrYes) then
    begin
      statusPanel0.Text:='';
      AddGamePrefixes(MPath,ProgressBar,StatusBar1,True);
      if (MessageDlg('Operation','Completed operation.',mtInformation,[mbOk],0) = mrOk) then
      begin
        ProgressBar.Position:=0;
        ProgressBar.Visible:= False;
        statusPanel1.Text:='';
        VGetCheckCount(VST, StatusBar1.Panels.Items[0], True);
      end;
    end;
end;

procedure TForm1.ExplorerOpen1Click(Sender: TObject);
var
  Node: PVirtualNode;
  GData: PTreeData;
begin
  Node := VST.GetFirstSelected();
  GData := VST.GetNodeData(Node);
  if (GData^.FilePath <> '') then
    SysUtils.ExecuteProcess('explorer.exe', GData^.FilePath, []);
end;

procedure TForm1.MenuItem18Click(Sender: TObject);
begin
  if (MessageDlg('Confirm','Do you wish to delete all shortcuts in your list?',
      mtConfirmation,[mbYes,mbNo],0)=mrYes) then
    begin
      DeleteShortcuts(VST);
      if (MessageDlg('Operation','Completed operation.',mtInformation,[mbOk],0) = mrOk) then
        begin
          VGetCheckCount(VST, StatusBar1.Panels.Items[0], True);
        end;
    end;
end;

procedure TForm1.MenuItem19Click(Sender: TObject);
begin
  if (MessageDlg('Confirm','Do you wish to delete the selected shortcuts in your list?',
      mtConfirmation,[mbYes,mbNo],0)=mrYes) then
    begin
      DeleteShortcuts(VST, True);
      if (MessageDlg('Operation','Completed operation.',mtInformation,[mbOk],0) = mrOk) then
        begin
          VGetCheckCount(VST, StatusBar1.Panels.Items[0], True);
        end;
    end;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
var
  CurNode: PVirtualNode;
begin
  CurNode := SVST.GetFirstSelected();
  if (CurNode = nil) then abort;
end;

procedure TForm1.SaveShcBtnClick(Sender: TObject);
var
  fname: String;
begin
  if (not SaveShcBtn.Focused) and (not GetKeyPressed(VK_MENU)) then exit;
  if (LastUsedXML <> XMLEdit.Caption) then
    begin
      ShowMessage('Warning: New XML filename entered, please click load button first.');
      exit;
    end;
  fname := GamesEdit.Caption;
  fname := fname + '\';
  if (fname = '') or (not DirectoryExists(fname)) then
    begin
      ShowMessage('Error: Supplied games folder path does not exist.');
      exit;
    end;
  statusPanel0.Text:='';
  SaveShortcuts(SVST,GamesEdit.Caption,ProgressBar,StatusBar1);
  if (MessageDlg('Save','Completed.'+#13#10+#13#10+'To remove shortcuts, you will need to re-export your games from hakchi, or choose "Delete all shortcuts" from the Tools menu.',mtInformation,[mbOk],0) = mrOk) then
  begin
    ProgressBar.Position:=0;
    ProgressBar.Visible:= False;
    statusPanel1.Text:='';
    VPopulateShortcuts(VST,GamesEdit.Caption);
    if (VST.GetFirst() <> nil) and (ViewStyle = 0) then
      VMoveChildrenToTop(VST)
    else if (VST.GetFirst() <> nil) and (ViewStyle = 1) then
      VMoveChildrenToTopParent(VST)
    else if (VST.GetFirst() <> nil) and (ViewStyle = 2) then
      VMoveChildrenToParent(VST);
    VST.FullExpand();
    if (VST.GetFirstSelected() <> nil) and ((not ViewSelected) or (ViewSelected and VNodeChecked(VST,VST.GetFirstSelected()))) then
      VST.ScrollIntoView(VST.GetFirstSelected,False)
    else if (VST.GetFirst <> nil) then
      VST.ScrollIntoView(VST.GetFirstVisible(),False);
    VGetCheckCount(VST, StatusBar1.Panels.Items[0], True);
  end;
end;

procedure TForm1.ClearBtnClick(Sender: TObject);
begin
  if (not ClearBtn.Focused) and (not GetKeyPressed(VK_MENU)) then exit;
  VST.ClearChecked;
  VGetCheckCount(VST, StatusPanel0, True);
end;

procedure TForm1.FlatOptionClick(Sender: TObject);
begin
  ViewStyle := 0;
  FlatOption.Checked:=True;
  ParentOption.Checked:=False;
  ParentChildOption.Checked:=False;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewShortcuts,ViewGameInfo,HomeName,VST);
  if (VST.GetFirst() <> nil) then
    begin
      VST.BeginUpdate;
      VMoveChildrenToTop(VST);
      VST.FullCollapse();
      VST.FullExpand();
      VST.EndUpdate;
      if (VST.GetFirstSelected() <> nil) and ((not ViewSelected) or (ViewSelected and VNodeChecked(VST,VST.GetFirstSelected()))) then
        VST.ScrollIntoView(VST.GetFirstSelected,False)
      else if (VST.GetFirst <> nil) then
        VST.ScrollIntoView(VST.GetFirstVisible(),False);
      VST.Refresh;
    end;
end;

procedure TForm1.ParentOptionClick(Sender: TObject);
begin
  ViewStyle := 1;
  FlatOption.Checked:=False;
  ParentOption.Checked:=True;
  ParentChildOption.Checked:=False;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewShortcuts,ViewGameInfo,HomeName,VST);
  if (VST.GetFirst() <> nil) then
    begin
      VST.BeginUpdate;
      VMoveChildrenToTopParent(VST);
      VST.FullExpand();
      VST.FullCollapse();
      VST.FullExpand();
      VST.EndUpdate;
      if (VST.GetFirstSelected() <> nil) and ((not ViewSelected) or (ViewSelected and VNodeChecked(VST,VST.GetFirstSelected()))) then
        VST.ScrollIntoView(VST.GetFirstSelected,False)
      else if (VST.GetFirst <> nil) then
        VST.ScrollIntoView(VST.GetFirstVisible(),False);
      VST.Refresh;
    end;
end;

procedure TForm1.ParentChildOptionClick(Sender: TObject);
begin
  ViewStyle := 2;
  FlatOption.Checked:=False;
  ParentOption.Checked:=False;
  ParentChildOption.Checked:=True;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewShortcuts,ViewGameInfo,HomeName,VST);
  if (VST.GetFirst() <> nil) then
    begin
      VST.BeginUpdate;
      VMoveChildrenToParent(VST);
      VST.FullExpand();
      VST.FullCollapse();
      VST.FullExpand();
      VST.EndUpdate;
      if (VST.GetFirstSelected() <> nil) and ((not ViewSelected) or (ViewSelected and VNodeChecked(VST,VST.GetFirstSelected()))) then
        VST.ScrollIntoView(VST.GetFirstSelected,False)
      else if (VST.GetFirst <> nil) then
        VST.ScrollIntoView(VST.GetFirstVisible(),False);
      VST.Refresh;
    end;
end;

procedure TForm1.PopupMenu2Popup(Sender: TObject);
var
  Code: String;
  GData: PTreeData;
  CurNode: PVirtualNode;
begin
  CurNode := VST.GetFirstSelected();
  GData := VST.GetNodeData(CurNode);
  Code := GData^.Code;
  if (Code <> 'HOME') and (GData^.FilePath = '') then abort;
  if (GData^.FilePath <> '') then ExplorerOpen1.Visible := True
  else ExplorerOpen1.Visible := False;
  if (Code = 'HOME') then RenameHome1.Visible := True
  else RenameHome1.Visible := False;
end;

procedure TForm1.RenameHomeClick(Sender: TObject);
var
  NewName: String;
  GData: PTreeData;
begin
  NewName := Trim(InputBox('Rename Home Folder','Please enter a name for this folder',''));
  if (NewName = '') then exit;
  if (Length(NewName) > 15) then NewName := Copy(NewName,1,15);
  HomeName := NewName;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewShortcuts,ViewGameInfo,HomeName,VST);
  if (VST.GetFirst() <> nil) then
    begin
      GData := VST.GetNodeData(VST.GetFirst());
      GData^.Name := HomeName;
      VST.Refresh;
    end;
  if (SVST.GetFirst() <> nil) then
    begin
      GData := SVST.GetNodeData(SVST.GetFirst());
      GData^.Name := HomeName;
      SVST.Refresh;
    end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewShortcuts,ViewGameInfo,HomeName,VST);
end;

end.

