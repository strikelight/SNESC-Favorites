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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, IPHtml,
  ComCtrls, Types, StdCtrls, lcltype, lclintf, ExtCtrls, EditBtn, Inifiles,
  Buttons, MouseAndKeyInput, Iphttpbroker, ui_utils, futils, nutils, config,
  help, about, tools, popupex;

type
  TSimpleIpHtml = class(TIpHtml)
  public
    property OnGetImageX;
  end;

  { TForm1 }
  TMyTreeView = class(TTreeView)
  private
//    procedure WMVScroll(var Msg :TLMessage); message WM_VSCROLL;
  public
  end;

  TForm1 = class(TForm)
    Bevel1: TBevel;
    ExplorerOpen1: TMenuItem;
    DeletePresetOption: TMenuItem;
    DP1: TIpHttpDataProvider;
    MenuItem10: TMenuItem;
    AddPrefixesOption: TMenuItem;
    MenuItem11: TMenuItem;
    DelPrefixesOption: TMenuItem;
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
    ImageList1: TImageList;
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
    TreeView1: TTreeView;
    TreeView2: TTreeView;
    ViewMenu: TMenuItem;
    PopupNotifier1: TPopupEx;
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
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
    procedure TreeView1Compare(Sender: TObject; Node1, Node2: TTreeNode;
      var Compare: Integer);
    procedure TreeView1CustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeView1Deletion(Sender: TObject; Node: TTreeNode);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView2Click(Sender: TObject);
    procedure TreeView2CustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeView2KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeView2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView2MouseLeave(Sender: TObject);
    procedure TreeView2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TreeView2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView2SelectionChanged(Sender: TObject);
    procedure LoadSlotClick(Sender: TObject);
    procedure SaveSlotClick(Sender: TObject);
    procedure RenameSlotClick(Sender: TObject);
    procedure DeleteSlotClick(Sender: TObject);
    procedure DataProviderGetImage(Sender: TIpHtmlNode; const URL: string;
          var Picture: TPicture);
  private
    { private declarations }
    progressBar: TProgressBar;
    statusPanel0: TStatusPanel;
    statusPanel1: TStatusPanel;
  public
    { public declarations }
    procedure ToggleCheck(node: TTreeNode);
    procedure InitSlots;
  end;


var
  Form1: TForm1;
  LastFavFolder: String;
  ViewStyle: Integer;
  ViewSelected: Boolean;
  ViewGameInfo: Boolean;
  LastUsedXML: String;
  HomeName: String;


implementation

{$R *.lfm}

{procedure WMVScroll(var Msg :TLMessage);
begin
  inherited;
  Form1.TreeView2.Update;
end;
 }
{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  r: TRect;
begin
  Form1.Caption := GetProductName+' - '+GetProductVersion;
  TreeView2.DoubleBuffered:=True;
  PopupNotifier1 := TPopupEx.Create(Self);
  PopupNotifier1.Color:=clSkyBlue;
  PopupNotifier1.Title:='Game Information';
  PopupNotifier1.vNotifierForm.ButtonSize:=0;
  DP1.OnGetImage:=@DataProviderGetImage;
  PopupNotifier1.vNotifierForm.htmlPnl.DataProvider:=DP1;
  r.Left:=TreeView2.ClientOrigin.X+5;
  r.Right:=TreeView2.ClientOrigin.X+(Form1.Width - (TreeView2.ClientOrigin.X+TreeView2.Width))-10;
  r.Top:=TreeView2.ClientOrigin.Y;
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
  InitChecks(ImageList1);
  FlatOption.Checked:=False;
  ParentOption.Checked:=False;
  ParentChildOption.Checked:=False;
  LoadConfig(XMLEdit,GamesEdit,TreeView2,TreeView1,NANDCheckBox,ViewStyle,ViewSelected,ViewGameInfo,LastFavFolder,HomeName);
  InitSlots;
  LastUsedXML:=XMLEdit.Caption;
  case ViewStyle of
       0: FlatOption.Checked:=True;
       1: ParentOption.Checked:=True;
       2: ParentChildOption.Checked:=True;
  end;
  GameInfoOption.Checked:=ViewGameInfo;
  SelectedOption.Checked:=ViewSelected;
  SelectionView(TreeView2,ViewSelected);
  TreeView2.FullExpand;
  TreeView2.AlphaSort;
  if (TreeView2.Items.Count > 0) then
    case ViewStyle of
       0: Form1.FlatOptionClick(Self);
       1: Form1.ParentOptionClick(Self);
       2: Form1.ParentChildOptionClick(Self);
    end;
  GetCheckCount(TreeView2, StatusBar1.Panels.Items[0], True);
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
  Filename:=MPath+URL;
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

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TreeView2.Update;
end;

procedure TForm1.GameInfoOptionClick(Sender: TObject);
begin
  if (ViewGameInfo) then ViewGameInfo := False
  else ViewGameInfo := True;
  if (not ViewGameInfo) then PopupNotifier1.Visible:=False;
  GameInfoOption.Checked := ViewGameInfo;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewGameInfo,HomeName,TreeView2);
end;

procedure TForm1.LoadBtnClick(Sender: TObject);
var
  CheckedGames: String;
  ChkStringList: TStringList;
begin
  if (XMLEdit.Caption = '') or (not FileExists(XMLEdit.Caption)) then
    begin
      ShowMessage('Error: Supplied XML filename invalid.');
      exit;
    end;
  LastUsedXML := XMLEdit.Caption;
  TreeView1.Items.Clear;
  TreeView2.Items.Clear;
  CheckedGames := GetSavedChecked(0);
  XML2Tree(TreeView1,XMLEdit.Caption,False,HomeName);
  XML2Tree(TreeView2,XMLEdit.Caption,True,HomeName);
  try
    ChkStringList := TStringList.Create;
    ChkStringList.Clear;
    ChkStringList.Delimiter := ',';
    ChkStringList.StrictDelimiter := True;
    ChkStringList.DelimitedText := CheckedGames;
    ChkStringList.Sort;
    CheckNodesList(TreeView2,ChkStringList);
  finally
    ChkStringList.Free;
  end;
  SelectionView(TreeView2,ViewSelected);
  TreeView2.FullExpand;
  PopulatePathData(TreeView2,GamesEdit.Caption);
  PopulateFolderData(TreeView2,GamesEdit.Caption);
  PopulateFolderData(TreeView1,GamesEdit.Caption);
  PopulateShortcuts(TreeView2,GamesEdit.Caption);
  TreeView2.AlphaSort;
  if (TreeView2.Items.Count > 0) then
    case ViewStyle of
       0: Form1.FlatOptionClick(Self);
       1: Form1.ParentOptionClick(Self);
       2: Form1.ParentChildOptionClick(Self);
    end;
  GetCheckCount(TreeView2, StatusBar1.Panels.Items[0], True);
end;

procedure TForm1.MenuItem15Click(Sender: TObject);
begin
  AddHomeIcons(GamesEdit.Caption,ProgressBar,StatusBar1);
  if (MessageDlg('Update','Completed.',mtInformation,[mbOk],0) = mrOk) then
    begin
      ProgressBar.Position:=0;
      ProgressBar.Visible:= False;
      statusPanel1.Text:='';
      GetCheckCount(TreeView2, StatusBar1.Panels.Items[0], True);
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

procedure TForm1.SaveBtnClick(Sender: TObject);
var
  fname,LastFolder,NandPath: string;
begin
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
      NANDCreateFaveLinks(TreeView2,ProgressBar,StatusBar1,LastFavFolder);
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
      CreateFaveLinks(fname,TreeView2,ProgressBar,StatusBar1,LastFavFolder);
    end;
  if (MessageDlg('Save','Completed.',mtInformation,[mbOk],0) = mrOk) then
    begin
      ProgressBar.Position:=0;
      ProgressBar.Visible:= False;
      statusPanel1.Text:='';
      GetCheckCount(TreeView2, StatusBar1.Panels.Items[0], True);
      SaveConfig(XMLEdit.Caption,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewGameInfo,HomeName,TreeView2);
    end;
end;

procedure TForm1.SelectedOptionClick(Sender: TObject);
begin
  if (ViewSelected) then ViewSelected := False
  else ViewSelected := True;
  SelectedOption.Checked := ViewSelected;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewGameInfo,HomeName,TreeView2);
  SelectionView(TreeView2,ViewSelected);
  TreeView2.AlphaSort;
  if (TreeView2.Selected <> nil) and ((not ViewSelected) or (ViewSelected and NodeChecked(TreeView2.Selected))) then
    TreeView2.Selected.MakeVisible
  else if (TreeView2.Items.Count > 0) then
    TreeView2.Items[0].MakeVisible;
end;

procedure TForm1.TreeView1Compare(Sender: TObject; Node1, Node2: TTreeNode;
  var Compare: Integer);
var
  GData1,GData2: TGameData;
begin
  Compare := 0;
  GData1 := TGameData(Node1.Data);
  GData2 := TGameData(Node2.Data);
  if ((GData1.FType = 'Game') or (GData1.FType = 'Shortcut')) and (GData2.FType = 'Folder') then
    Compare := -1
  else if (GData1.FType = 'Folder') and ((GData2.FType = 'Game') or (GData2.FType = 'Shortcut')) then
    Compare := 1
  else if ((GData1.FType = 'Game') or (GData1.FType = 'Shortcut')) and ((GData2.Ftype = 'Game') or (GData2.FType = 'Shortcut')) then
    Compare := CompareText(GData1.Name,GData2.Name);
end;

procedure TForm1.TreeView1CustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  NodeRect,TmRect: TRect;
  DisplayText: String;
  OldBrushStyle: TBrushStyle;
  OldStyle: TFontStyles;
  b,c : Boolean;
  clOld: TColor;
  GData: TGameData;
begin
  b := cdsSelected In State;
  c := cdsFocused In State;
  GData := TGameData(Node.Data);
  if (GData.FType = 'Folder') then
    begin
      DefaultDraw := True;
      exit;
    end;

  with TTreeView(Sender).Canvas do
  begin
    DefaultDraw := False;
    NodeRect := Node.DisplayRect(True);
    clOld := Font.Color;
    OldBrushStyle := Brush.Style;
    OldStyle := Font.Style;

    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    TmRect := NodeRect;
    TmRect.Left:=0;
    TmRect.Right:=TTreeView(Sender).Width;
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

    DisplayText := Node.Text;
    TextOut(NodeRect.Left, NodeRect.Top, DisplayText);
//    TextRect(NodeRect, NodeRect.Left, NodeRect.Top, DisplayText);
    Font.Color := clOld;
    Brush.Style := OldBrushStyle;
    Font.Style := OldStyle;
  end;
end;

procedure TForm1.TreeView1Deletion(Sender: TObject; Node: TTreeNode);
begin
// this will free the information even when clearing the treeview
  if Node.Data <> nil then
    TObject(Node.Data).Free;  // enough to cast to TObject because of virtual destructor
end;

procedure TForm1.TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) then
    MouseInput.Click(mbLeft,[]);
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
      CheckNodesList(TreeView2,ChkStringList);
    finally
      ChkStringList.Free;
    end;
  finally
    Ini.Free;
  end;
  SelectionView(TreeView2,ViewSelected);
  GetCheckCount(TreeView2,StatusPanel0,True);
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
      GetCheckedCodes(TreeView2,GL);
      for i := 0 to (GL.Count-1) do
        begin
          CheckedGames := CheckedGames+' '+GL[i];
        end;
      CheckedGames := StringReplace(Trim(CheckedGames),' ',',',[rfReplaceAll]);
      GetCheckedHashes(TreeView2,GL);
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
  SelectionView(TreeView2,ViewSelected);
  GetCheckCount(TreeView2,StatusPanel0,True);
end;

procedure TForm1.ToggleCheck(node: TTreeNode);
var
  checked: integer;
begin
  ToggleTreeViewCheckBoxes(node);

  checked := GetCheckCount(TreeView2,StatusPanel0,False);
  if NodeChecked(node) then
    begin
      if (checked > 30) then
        begin
          ToggleTreeViewCheckBoxes(node);
          Dec(checked);
          MessageDlg('Error','Maximum of 30 favorites allowed.',mtError,[mbOk],0);
        end;
    end
  else if ViewSelected then
    begin
      SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewGameInfo,HomeName,TreeView2);
    end;
  checked := GetCheckCount(TreeView2,StatusPanel0,True);
end;

procedure TForm1.TreeView2Click(Sender: TObject);
var
  P: TPoint;
  node: TTreeNode;
  ht: THitTests;
begin
  P := TPoint.Create(0,0);
  GetCursorPos(P);
  P := TreeView2.ScreenToClient(P);
  ht := TreeView2.GetHitTestInfoAt(P.X, P.Y);
  if (htOnStateIcon in ht) then
    begin
      node := TreeView2.GetNodeAt(P.X, P.Y);
      ToggleCheck(node);
    end;
end;

procedure TForm1.TreeView2CustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  TreeViewDrawItem(Sender,Node,State,ImageList1,DefaultDraw);
end;

procedure TForm1.TreeView2KeyDown(Sender: TObject; var Key: Word;
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
end;

procedure TForm1.TreeView2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) then
   MouseInput.Click(mbLeft,[]);
end;

procedure TForm1.TreeView2MouseLeave(Sender: TObject);
begin
  if PopupNotifier1.Visible then
   begin
     PopupNotifier1.Text:='';
     PopupNotifier1.Hide;
   end;
end;

procedure TForm1.TreeView2MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  node: TTreeNode;
  GData: TGameData;
  X1,Y1: Integer;
  OControl: TWinControl;
  DisplayText,MPath: String;
  Flush: Boolean;
begin
  if (TreeView2.Items.Count = 0) then exit;
  Flush := False;
  node := TreeView2.GetNodeAt(X, Y);
  if (node = nil) then
   begin
     PopupNotifier1.Text:='';
     PopupNotifier1.Hide;
     exit;
   end;
  GData := TGameData(node.Data);
  if (ViewGameInfo) and (GData.FType = 'Game') and (GData.BelongsTo <> nil) then
    begin
      OControl := Form1.ActiveControl;
      Y1 := TreeView2.ClientOrigin.y;
      X1 := TreeView2.ClientOrigin.x+TreeView2.Width+5;
      if (PopupNotifier1.vNotifierForm.Color <> clSkyBlue) then
        Flush := True;
      PopupNotifier1.vNotifierForm.Color := clSkyBlue;
      PopupNotifier1.vNotifierForm.htmlColor := clSkyBlue;
      PopupNotifier1.vNotifierForm.htmlhexColor := TColorToHex(clSkyBlue);
      if (not PopupNotifier1.Visible) then
        PopupNotifier1.ShowAtPos(X1,Y1);
      DisplayText:='<table width="93%"><tr><td align="left" width="40%"><b>Game:</b></td><td align="left">'+node.text+'</td><tr><td align="left">'+'<b>Game Code:</b></td><td align="left">'+GData.Code+'</td></tr><tr><td align="left">'+'<b>Parent Folder:</b></td><td align="left">'+GData.BelongsTo.Text+'</td></tr>';
      if (GData.TopParent <> nil) then
       DisplayText:=DisplayText+'<tr><td align="left"><b>Top Folder:</b></td><td align="left">'+GData.TopParent.Text+'</td></tr>';
      DisplayText:=DisplayText+'</table>';
      if (Flush) then
       begin
         PopupNotifier1.Text:='';
         PopupNotifier1.vNotifierForm.Repaint;
       end;
      PopupNotifier1.Text:=DisplayText;
      OControl.SetFocus;
    end
  else if (ViewGameInfo) and (GData.FType = 'Shortcut') and (GData.BelongsTo <> nil) then
    begin
      OControl := Form1.ActiveControl;
      Y1 := TreeView2.ClientOrigin.y;
      X1 := TreeView2.ClientOrigin.x+TreeView2.Width+5;
      if (PopupNotifier1.vNotifierForm.Color <> clFuchsia) then
        Flush := True;
      PopupNotifier1.vNotifierForm.Color := clFuchsia;
      PopupNotifier1.vNotifierForm.htmlColor := clFuchsia;
      PopupNotifier1.vNotifierForm.htmlhexColor := TColorToHex(clFuchsia);
      if (not PopupNotifier1.Visible) then
        PopupNotifier1.ShowAtPos(X1,Y1);
      DisplayText:='<table width="93%"><tr><td align="left" width="40%"><b>Game:</b></td><td align="left">'+node.text+'</td><tr><td align="left">'+'<b>Game Code:</b></td><td align="left">'+GData.Code+'</td></tr><tr><td align="left">'+'<b>Parent Folder:</b></td><td align="left">'+GData.BelongsTo.Text+'</td></tr>';
      if (GData.TopParent <> nil) then
       DisplayText:=DisplayText+'<tr><td align="left"><b>Top Folder:</b></td><td align="left">'+GData.TopParent.Text+'</td></tr>';
      DisplayText:=DisplayText+'</table>';
      if (Flush) then
       begin
         PopupNotifier1.Text:='';
         PopupNotifier1.vNotifierForm.Repaint;
       end;
      PopupNotifier1.Text:=DisplayText;
      OControl.SetFocus;
    end
  else if (ViewGameInfo) and (GData.FType = 'Folder') then
    begin
      MPath := ExtractFileDir(XMLEdit.Caption);
      MPath := ExtractFilePath(MPath);
      MPath := MPath+'folder_images';
      OControl := Form1.ActiveControl;
      Y1 := TreeView2.ClientOrigin.y;
      X1 := TreeView2.ClientOrigin.x+TreeView2.Width+5;
      if (PopupNotifier1.vNotifierForm.Color <> clSkyBlue) then
        Flush := True;
      PopupNotifier1.vNotifierForm.Color := clSkyBlue;
      PopupNotifier1.vNotifierForm.htmlColor := clSkyBlue;
      PopupNotifier1.vNotifierForm.htmlhexColor := TColorToHex(clSkyBlue);
      if (not PopupNotifier1.Visible) then
        PopupNotifier1.ShowAtPos(X1,Y1);
      DisplayText:='<table width="93%"><tr><td align="left" width="40%"><b>Folder:</b></td><td align="left">'+node.text+'</td>';
      if (GData.BelongsTo <> nil) then
        DisplayText:=DisplayText+'<tr><td align="left">'+'<b>Parent Folder:</b></td><td align="left">'+GData.BelongsTo.Text+'</td></tr>';
      if (GData.TopParent <> nil) then
        DisplayText:=DisplayText+'<tr><td align="left"><b>Top Folder:</b></td><td align="left">'+GData.TopParent.Text+'</td></tr>';
      if (GData.Icon <> '') and (FileExists(MPath+'\'+GData.Icon+'.png')) then
        begin
          DisplayText:=DisplayText+'<tr><td align="center" colspan="2"><br/><img height="50" width="50" src="'+GData.Icon+'.png" /></td></tr>';
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

procedure TForm1.TreeView2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TreeView2.Repaint;
end;

procedure TForm1.TreeView2SelectionChanged(Sender: TObject);
var
  node: TTreeNode;
  GData: TGameData;
  X1,Y1: Integer;
  OControl: TWinControl;
  MPath,DisplayText: String;
  Flush: Boolean;
begin
  node := TreeView2.Selected;
  if (node = nil) then
    begin
      PopupNotifier1.Text:='';
      PopupNotifier1.Hide;
      exit;
    end;
  Flush := False;
  GData := TGameData(node.Data);
  if (ViewGameInfo) and (GData.FType = 'Game') and (GData.BelongsTo <> nil) then
    begin
      OControl := Form1.ActiveControl;
      Y1 := TreeView2.ClientOrigin.y;
      X1 := TreeView2.ClientOrigin.x+TreeView2.Width+5;
      if (PopupNotifier1.vNotifierForm.Color <> clSkyBlue) then
        Flush := True;
      PopupNotifier1.vNotifierForm.Color := clSkyBlue;
      PopupNotifier1.vNotifierForm.htmlColor := clSkyBlue;
      PopupNotifier1.vNotifierForm.htmlhexColor := TColorToHex(clSkyBlue);
      if (not PopupNotifier1.Visible) then
        PopupNotifier1.ShowAtPos(X1,Y1);
      DisplayText:='<table width="93%"><tr><td align="left" width="40%"><b>Game:</b></td><td align="left">'+node.text+'</td><tr><td align="left">'+'<b>Game Code:</b></td><td align="left">'+GData.Code+'</td></tr><tr><td align="left">'+'<b>Parent Folder:</b></td><td align="left">'+GData.BelongsTo.Text+'</td></tr>';
      if (GData.TopParent <> nil) then
       DisplayText:=DisplayText+'<tr><td align="left"><b>Top Folder:</b></td><td align="left">'+GData.TopParent.Text+'</td></tr>';
      DisplayText:=DisplayText+'</table>';
      if (Flush) then
       begin
         PopupNotifier1.Text:='';
         PopupNotifier1.vNotifierForm.Repaint;
       end;
      PopupNotifier1.Text:=DisplayText;
      OControl.SetFocus;
    end
  else if (ViewGameInfo) and (GData.FType = 'Shortcut') and (GData.BelongsTo <> nil) then
    begin
      OControl := Form1.ActiveControl;
      Y1 := TreeView2.ClientOrigin.y;
      X1 := TreeView2.ClientOrigin.x+TreeView2.Width+5;
      if (PopupNotifier1.vNotifierForm.Color <> clFuchsia) then
        Flush := True;
      PopupNotifier1.vNotifierForm.Color := clFuchsia;
      PopupNotifier1.vNotifierForm.htmlColor := clFuchsia;
      PopupNotifier1.vNotifierForm.htmlhexColor := TColorToHex(clFuchsia);
      if (not PopupNotifier1.Visible) then
        PopupNotifier1.ShowAtPos(X1,Y1);
      DisplayText:='<table width="93%"><tr><td align="left" width="40%"><b>Game:</b></td><td align="left">'+node.text+'</td><tr><td align="left">'+'<b>Game Code:</b></td><td align="left">'+GData.Code+'</td></tr><tr><td align="left">'+'<b>Parent Folder:</b></td><td align="left">'+GData.BelongsTo.Text+'</td></tr>';
      if (GData.TopParent <> nil) then
       DisplayText:=DisplayText+'<tr><td align="left"><b>Top Folder:</b></td><td align="left">'+GData.TopParent.Text+'</td></tr>';
      DisplayText:=DisplayText+'</table>';
      if (Flush) then
       begin
         PopupNotifier1.Text:='';
         PopupNotifier1.vNotifierForm.Repaint;
       end;
      PopupNotifier1.Text:=DisplayText;
      OControl.SetFocus;
    end
  else if (ViewGameInfo) and (GData.FType = 'Folder') then
    begin
      MPath := ExtractFileDir(XMLEdit.Caption);
      MPath := ExtractFilePath(MPath);
      MPath := MPath+'folder_images';
      OControl := Form1.ActiveControl;
      Y1 := TreeView2.ClientOrigin.y;
      X1 := TreeView2.ClientOrigin.x+TreeView2.Width+5;
      if (PopupNotifier1.vNotifierForm.Color <> clSkyBlue) then
        Flush := True;
      PopupNotifier1.vNotifierForm.Color := clSkyBlue;
      PopupNotifier1.vNotifierForm.htmlColor := clSkyBlue;
      PopupNotifier1.vNotifierForm.htmlhexColor := TColorToHex(clSkyBlue);
      if (not PopupNotifier1.Visible) then
        PopupNotifier1.ShowAtPos(X1,Y1);
      DisplayText:='<table width="93%"><tr><td align="left" width="40%"><b>Folder:</b></td><td align="left">'+node.text+'</td>';
      if (GData.BelongsTo <> nil) then
        DisplayText:=DisplayText+'<tr><td align="left">'+'<b>Parent Folder:</b></td><td align="left">'+GData.BelongsTo.Text+'</td></tr>';
      if (GData.TopParent <> nil) then
       DisplayText:=DisplayText+'<tr><td align="left"><b>Top Folder:</b></td><td align="left">'+GData.TopParent.Text+'</td></tr>';
      if (GData.Icon <> '') and (FileExists(MPath+'\'+GData.Icon+'.png')) then
        begin
          DisplayText:=DisplayText+'<tr><td align="center" colspan="2"><br/><img height="50" width="50" src="'+GData.Icon+'.png" /></td></tr>';
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
  TreeView2.SetFocus;
end;

procedure TForm1.ClearShcBtnClick(Sender: TObject);
begin
  ClearShortcuts(TreeView1);
  TreeView1.Refresh;
end;

procedure TForm1.AddShortCutsBtnClick(Sender: TObject);
begin
  AddShortcutSelections(TreeView2,TreeView1);
  TreeView1.AlphaSort;
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
        GetCheckCount(TreeView2, StatusBar1.Panels.Items[0], True);
      end;
    end;
end;

procedure TForm1.CreateFolderMnuClick(Sender: TObject);
var
  NewName,FName,LFName,FPath,MPath,ConsolePath: String;
  Node,nNode,nNode2,pNode,fNode,lNode,lNode2: TTreeNode;
  GData,GData2,GData3: TGameData;
  DeskFile: TStringList;
begin
  Node := TreeView1.Selected;
  if (Node = nil) then exit;
  ConsolePath := GamesEdit.Caption;
  FPath := ExtractFilePath(ParamStr(0));
  GData := TGameData(Node.Data);
  FName := GetLastFolderNumber(GData.FilePath+'\',True);
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
          if (Node = TreeView1.Items[0]) then
            GData2 := CreateGameData(NewName,'Folder',nil,'','')
          else
            GData2 := CreateGameData(NewName,'Folder',Node,'','');
          GData2.Icon := GData2.Hash;
          if (Node = TreeView1.Items[0]) then pNode := nil
          else pNode := Node;
          nNode := TreeView1.Items.AddChild(pNode,NewName);
          if (GData2.TopParent = nil) then GData2.TopParent := nNode;
          LFName := GetLastFolderNumber(ConsolePath);
          LFName := GenerateFolderName(LFName);
          GData2.FilePath := ConsolePath+LFName;
          nNode.Data := GData2;
          CreateDir(GData2.FilePath);
          CreateDir(GData.FilePath+'\CLV-S-00'+LFName);

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

            DeskFile.SaveToFile(GData.FilePath+'\CLV-S-00'+LFName+'\CLV-S-00'+LFName+'.desktop');
          finally
            DeskFile.Free;
          end;

          CreateDir(GData2.FilePath+'\CLV-S-00000');
          CopyFile(ConsolePath+'001\CLV-S-00000\CLV-S-00000.desktop',GData2.FilePath+'\CLV-S-00000\CLV-S-00000.desktop');
          CopyFile(ConsolePath+'001\CLV-S-00000\CLV-S-00000.png',GData2.FilePath+'\CLV-S-00000\CLV-S-00000.png');
          CopyFile(ConsolePath+'001\CLV-S-00000\CLV-S-00000_small.png',GData2.FilePath+'\CLV-S-00000\CLV-S-00000_small.png');

          TreeView1.AlphaSort;
          nNode.MakeVisible;

          lNode := TreeView2.Items[0];
          while (lNode <> nil) do
            begin
              lNode2 := lNode.GetNextSibling;
              fNode := nil;
              fNode := FindNodeData(TreeView2,lNode,GData.Hash,5);
              if (fNode <> nil) then
                begin
                  lNode2 := nil;
                  if (fNode = TreeView2.Items[0]) then pNode := nil
                  else pNode := fNode;

                  GData3 := CreateGameData(GData2.Name,GData2.FType,pNode,
                            GData2.Icon,GData2.Code);
                  nNode2 := TreeView2.Items.AddChild(pNode,NewName);
                  nNode2.Data := GData3;
                  GData3.FilePath := ConsolePath+LFName;
                  if (GData3.TopParent = nil) then GData3.TopParent := nNode2;

                  TreeView2.AlphaSort;
                  nNode2.MakeVisible;
                end;
              lNode := lNode2;
            end;
          TreeView2.Invalidate;
          Form1.Invalidate;


          ResizeImage(PngSelectDlg.FileName,FPath+GData2.Hash+'.png', 204, 204);
          ResizeImage(PngSelectDlg.FileName,FPath+GData2.Hash+'_small.png', 40, 40);
          CopyFile(FPath+GData2.Hash+'.png',GData.FilePath+'\CLV-S-00'+LFName+'\CLV-S-00'+LFName+'.png');
          CopyFile(FPath+GData2.Hash+'_small.png',GData.FilePath+'\CLV-S-00'+LFName+'\CLV-S-00'+LFName+'_small.png');

          MPath := ExtractFileDir(XMLEdit.Caption);
          MPath := ExtractFilePath(MPath);
          MPath := MPath+'folder_images';
          if (DirectoryExists(MPath)) then
            CopyFile(FPath+GData2.Hash+'.png',MPath+'\'+GData2.Hash+'.png');
          DeleteFile(FPath+GData2.Hash+'.png');
          DeleteFile(FPath+GData2.Hash+'_small.png');
          if (FileExists(XMLEdit.Caption)) then
            begin
              MPath := ExtractFilePath(XMLEdit.Caption);
              if (FileExists(MPath+'folders_snes.bak')) then
                DeleteFile(MPath+'folders_snes.bak');
              CopyFile(XMLEdit.Caption,MPath+'folders_snes.bak');
            end;
          TreeToXML(TreeView2,XMLEdit.Caption);
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
        GetCheckCount(TreeView2, StatusBar1.Panels.Items[0], True);
      end;
    end;
end;

procedure TForm1.ExplorerOpen1Click(Sender: TObject);
var
  Node: TTreeNode;
  GData: TGameData;
begin
  Node := TreeView2.Selected;
  GData := TGameData(Node.Data);
  if (GData.FilePath <> '') then
    SysUtils.ExecuteProcess('explorer.exe', GData.FilePath, []);
end;

procedure TForm1.MenuItem18Click(Sender: TObject);
begin
  DeleteShortcuts(TreeView2);
  GetCheckCount(TreeView2, StatusBar1.Panels.Items[0], True);
end;

procedure TForm1.MenuItem19Click(Sender: TObject);
begin
  DeleteShortcuts(TreeView2, True);
  GetCheckCount(TreeView2, StatusBar1.Panels.Items[0], True);
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
var
  CurNode: TTreeNode;
begin
  CurNode := TreeView1.Selected;
  if (CurNode = nil) then abort;
end;

procedure TForm1.SaveShcBtnClick(Sender: TObject);
var
  fname: String;
begin
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
  SaveShortcuts(Treeview1,GamesEdit.Caption,ProgressBar,StatusBar1);

  if (MessageDlg('Save','Completed.'+#13#10+#13#10+'To remove shortcuts, you will need to re-export your games from hakchi, or choose "Delete all shortcuts" from the Tools menu.',mtInformation,[mbOk],0) = mrOk) then
  begin
    ProgressBar.Position:=0;
    ProgressBar.Visible:= False;
    statusPanel1.Text:='';
    PopulateShortcuts(TreeView2,GamesEdit.Caption);
    TreeView2.BeginUpdate;
    if (TreeView2.Items.Count > 0) and (ViewStyle = 0) then
      MoveChildrenToTop(TreeView2)
    else if (TreeView2.Items.Count > 0) and (ViewStyle = 1) then
      MoveChildrenToTopParent(TreeView2)
    else if (TreeView2.Items.Count > 0) and (ViewStyle = 2) then
      MoveChildrenToParent(TreeView2);
    TreeView2.FullExpand;
    TreeView2.AlphaSort;
    if (TreeView2.Selected <> nil) and ((not ViewSelected) or (ViewSelected and NodeChecked(TreeView2.Selected))) then
     TreeView2.Selected.MakeVisible
    else if (TreeView2.Items.Count > 0) then
     TreeView2.Items[0].MakeVisible;
    TreeView2.EndUpdate;
    GetCheckCount(TreeView2, StatusBar1.Panels.Items[0], True);
  end;
end;

procedure TForm1.ClearBtnClick(Sender: TObject);
begin
  ClearSelections(TreeView2);
  GetCheckCount(TreeView2, StatusPanel0, True);
end;

procedure TForm1.FlatOptionClick(Sender: TObject);
begin
  ViewStyle := 0;
  FlatOption.Checked:=True;
  ParentOption.Checked:=False;
  ParentChildOption.Checked:=False;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewGameInfo,HomeName,TreeView2);
  if (TreeView2.Items.Count > 0) then
    begin
      TreeView2.BeginUpdate;
      MoveChildrenToTop(TreeView2);
      TreeView2.AlphaSort;
      if (TreeView2.Selected <> nil) and ((not ViewSelected) or (ViewSelected and NodeChecked(TreeView2.Selected))) then
       TreeView2.Selected.MakeVisible
      else if (TreeView2.Items.Count > 0) then
       TreeView2.Items[0].MakeVisible;
      TreeView2.EndUpdate;
    end;
end;

procedure TForm1.ParentOptionClick(Sender: TObject);
begin
  ViewStyle := 1;
  FlatOption.Checked:=False;
  ParentOption.Checked:=True;
  ParentChildOption.Checked:=False;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewGameInfo,HomeName,TreeView2);
  if (TreeView2.Items.Count > 0) then
    begin
      TreeView2.BeginUpdate;
      MoveChildrenToTopParent(TreeView2);
      TreeView2.FullExpand;
      if (TreeView2.Selected <> nil) and ((not ViewSelected) or (ViewSelected and NodeChecked(TreeView2.Selected))) then
       TreeView2.Selected.MakeVisible
      else if (TreeView2.Items.Count > 0) then
       TreeView2.Items[0].MakeVisible;
      TreeView2.EndUpdate;
    end;
end;

procedure TForm1.ParentChildOptionClick(Sender: TObject);
begin
  ViewStyle := 2;
  FlatOption.Checked:=False;
  ParentOption.Checked:=False;
  ParentChildOption.Checked:=True;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewGameInfo,HomeName,TreeView2);
  if (TreeView2.Items.Count > 0) then
    begin
      TreeView2.BeginUpdate;
      MoveChildrenToParent(TreeView2);
      TreeView2.FullExpand;
      if (TreeView2.Selected <> nil) and ((not ViewSelected) or (ViewSelected and NodeChecked(TreeView2.Selected))) then
       TreeView2.Selected.MakeVisible
      else if (TreeView2.Items.Count > 0) then
       TreeView2.Items[0].MakeVisible;
      TreeView2.EndUpdate;
    end;
end;

procedure TForm1.PopupMenu2Popup(Sender: TObject);
var
  Code: String;
  GData: TGameData;
  CurNode: TTreeNode;
begin
  CurNode := TreeView2.Selected;
  GData := TGameData(CurNode.Data);
  Code := GData.Code;
  if (Code <> 'HOME') and (GData.FilePath = '') then abort;
  if (GData.FilePath <> '') then ExplorerOpen1.Visible := True
  else ExplorerOpen1.Visible := False;
  if (Code = 'HOME') then RenameHome1.Visible := True
  else RenameHome1.Visible := False;
end;

procedure TForm1.RenameHomeClick(Sender: TObject);
var
  NewName: String;
  GData: TGameData;
begin
  NewName := Trim(InputBox('Rename Home Folder','Please enter a name for this folder',''));
  if (NewName = '') then exit;
  if (Length(NewName) > 15) then NewName := Copy(NewName,1,15);
  HomeName := NewName;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewGameInfo,HomeName,TreeView2);
  if (TreeView2.Items.Count > 0) then
    begin
      GData := TGameData(TreeView2.Items[0].Data);
      GData.Name := HomeName;
      TreeView2.Items[0].Data := GData;
      TreeView2.Items[0].Text := HomeName;
    end;
  if (TreeView1.Items.Count > 0) then
    begin
      GData := TGameData(TreeView1.Items[0].Data);
      GData.Name := HomeName;
      TreeView1.Items[0].Data := GData;
      TreeView1.Items[0].Text := HomeName;
    end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,ViewGameInfo,HomeName,TreeView2);
end;

end.

