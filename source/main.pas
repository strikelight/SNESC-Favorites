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
  ComCtrls, Types, StdCtrls, lcltype, lclintf, ExtCtrls, CheckLst, EditBtn,
  MouseAndKeyInput, ui_utils, futils, nutils, config, help, about, tools;

type

  { TForm1 }

  TForm1 = class(TForm)
    NANDCheckBox: TCheckBox;
    LoadBtn: TButton;
    GamesEdit: TDirectoryEdit;
    Label3: TLabel;
    MenuItem10: TMenuItem;
    LoadSlot1: TMenuItem;
    LoadSlot2: TMenuItem;
    LoadSlot3: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    RestoreXMLDlg: TOpenDialog;
    RenameHome: TMenuItem;
    PopupMenu1: TPopupMenu;
    BackupXMLDlg: TSaveDialog;
    HakchiConfigDlg: TSelectDirectoryDialog;
    SlotName3: TMenuItem;
    SlotName2: TMenuItem;
    SlotName1: TMenuItem;
    SaveSlot3: TMenuItem;
    SaveSlot2: TMenuItem;
    SaveSlot1: TMenuItem;
    PSlot3: TMenuItem;
    PSlot2: TMenuItem;
    PSlot1: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    FlatOption: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    SelectedOption: TMenuItem;
    ParentChildOption: TMenuItem;
    ParentOption: TMenuItem;
    StatusBar1: TStatusBar;
    ViewMenu: TMenuItem;
    XMLEdit: TFileNameEdit;
    Label2: TLabel;
    SaveBtn: TButton;
    ClearBtn: TButton;
    CheckListBox1: TCheckListBox;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    procedure CheckListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure CheckListBox1ItemClick(Sender: TObject; Index: integer);
    procedure CheckListBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ClearBtnClick(Sender: TObject);
    procedure FlatOptionClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure LoadSlot1Click(Sender: TObject);
    procedure LoadSlot2Click(Sender: TObject);
    procedure LoadSlot3Click(Sender: TObject);
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
    procedure PopupMenu1Popup(Sender: TObject);
    procedure RenameHomeClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure SaveSlot1Click(Sender: TObject);
    procedure SaveSlot2Click(Sender: TObject);
    procedure SaveSlot3Click(Sender: TObject);
    procedure SelectedOptionClick(Sender: TObject);
    procedure SlotName1Click(Sender: TObject);
    procedure SlotName2Click(Sender: TObject);
    procedure SlotName3Click(Sender: TObject);
  private
    { private declarations }
    progressBar: TProgressBar;
    statusPanel0: TStatusPanel;
    statusPanel1: TStatusPanel;
  public
    { public declarations }
  end;


var
  Form1: TForm1;
  LastFavFolder: String;
  ViewStyle: Integer;
  ViewSelected: Boolean;
  LastUsedXML: String;
  HomeName: String;


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Form1.Caption := GetProductName+' - '+GetProductVersion;
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
  LoadConfig(XMLEdit,GamesEdit,CheckListBox1,NANDCheckBox,ViewStyle,ViewSelected,LastFavFolder,HomeName);
  LoadSlot(1,CheckListBox1,PSlot1,True);
  LoadSlot(2,CheckListBox1,PSlot2,True);
  LoadSlot(3,CheckListBox1,PSlot3,True);
  LastUsedXML:=XMLEdit.Caption;
  GetCheckCount(CheckListBox1, StatusBar1.Panels.Items[0], True);
  FlatOption.Checked:=False;
  ParentOption.Checked:=False;
  ParentChildOption.Checked:=False;
  SelectedOption.Checked:=ViewSelected;
  case ViewStyle of
       0: FlatOption.Checked:=True;
       1: ParentOption.Checked:=True;
       2: ParentChildOption.Checked:=True;
  end;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  try
    GameCodes := nil;
  finally
  end;
end;

procedure TForm1.LoadBtnClick(Sender: TObject);
var
  CheckedGames: String;
begin
  if (XMLEdit.Caption = '') or (not FileExists(XMLEdit.Caption)) then
    begin
      ShowMessage('Error: Supplied XML filename invalid.');
      exit;
    end;
  LastUsedXML := XMLEdit.Caption;
  CheckListBox1.Clear;
  SetLength(GameCodes,0);
  PSlot1.Checked := False;
  PSlot2.Checked := False;
  PSlot3.Checked := False;
  CheckedGames := GetSavedChecked(0);
  LoadGamesList(CheckListBox1,XMLEdit.Caption,ViewStyle,ViewSelected,HomeName,CheckedGames);
  GetCheckCount(CheckListBox1,StatusPanel0,True);
end;

procedure TForm1.LoadSlot1Click(Sender: TObject);
var
  oViewSelected: Boolean;
begin
  PSlot1.Checked := True;
  PSlot2.Checked := False;
  PSlot3.Checked := False;
  oViewSelected := ViewSelected;
  if ViewSelected then
    begin
      ViewSelected := False;
      SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
      LoadConfig(XMLEdit,GamesEdit,CheckListBox1,NANDCheckBox,ViewStyle,ViewSelected,LastFavFolder,HomeName);
    end
  else
    SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
  LoadSlot(1,CheckListBox1,PSlot1);
  if oViewSelected then
    begin
      ViewSelected := oViewSelected;
      SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
      LoadConfig(XMLEdit,GamesEdit,CheckListBox1,NANDCheckBox,ViewStyle,ViewSelected,LastFavFolder,HomeName);
    end;
  GetCheckCount(CheckListBox1,StatusPanel0,True);
end;

procedure TForm1.LoadSlot2Click(Sender: TObject);
var
  oViewSelected: Boolean;
begin
  PSlot1.Checked := False;
  PSlot2.Checked := True;
  PSlot3.Checked := False;
  oViewSelected := ViewSelected;
  if ViewSelected then
    begin
      ViewSelected := False;
      SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
      LoadConfig(XMLEdit,GamesEdit,CheckListBox1,NANDCheckBox,ViewStyle,ViewSelected,LastFavFolder,HomeName);
    end
  else
    SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
  LoadSlot(2,CheckListBox1,PSlot2);
  if oViewSelected then
    begin
      ViewSelected := oViewSelected;
      SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
      LoadConfig(XMLEdit,GamesEdit,CheckListBox1,NANDCheckBox,ViewStyle,ViewSelected,LastFavFolder,HomeName);
    end;
  GetCheckCount(CheckListBox1,StatusPanel0,True);
end;

procedure TForm1.LoadSlot3Click(Sender: TObject);
var
  oViewSelected: Boolean;
begin
  PSlot1.Checked := False;
  PSlot2.Checked := False;
  PSlot3.Checked := True;
  oViewSelected := ViewSelected;
  if ViewSelected then
    begin
      ViewSelected := False;
      SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
      LoadConfig(XMLEdit,GamesEdit,CheckListBox1,NANDCheckBox,ViewStyle,ViewSelected,LastFavFolder,HomeName);
    end
  else
    SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
  LoadSlot(3,CheckListBox1,PSlot3);
  if oViewSelected then
    begin
      ViewSelected := oViewSelected;
      SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
      LoadConfig(XMLEdit,GamesEdit,CheckListBox1,NANDCheckBox,ViewStyle,ViewSelected,LastFavFolder,HomeName);
    end;
  GetCheckCount(CheckListBox1,StatusPanel0,True);
end;

procedure TForm1.MenuItem15Click(Sender: TObject);
begin
  AddHomeIcons(GamesEdit.Caption,ProgressBar,StatusBar1);
  if (MessageDlg('Update','Completed.',mtInformation,[mbOk],0) = mrOk) then
    begin
      ProgressBar.Position:=0;
      ProgressBar.Visible:= False;
      statusPanel1.Text:='';
      GetCheckCount(CheckListBox1, StatusBar1.Panels.Items[0], True);
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
  Res: Boolean;
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
      Res := NANDCreateFaveLinks(CheckListBox1,ProgressBar,StatusBar1,LastFavFolder);
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
      Res := CreateFaveLinks(fname,CheckListBox1,ProgressBar,StatusBar1,LastFavFolder);
    end;
  if (MessageDlg('Save','Completed.',mtInformation,[mbOk],0) = mrOk) then
    begin
      ProgressBar.Position:=0;
      ProgressBar.Visible:= False;
      statusPanel1.Text:='';
      GetCheckCount(CheckListBox1, StatusBar1.Panels.Items[0], True);
      SaveConfig(XMLEdit.Caption,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
    end;
end;

procedure TForm1.SaveSlot1Click(Sender: TObject);
begin
  if (SaveSlot(1,CheckListBox1,PSlot1)) then
    begin
      ShowMessage('Successfully saved selected favorites to preset slot 1');
      PSlot1.Checked := True;
      PSlot2.Checked := False;
      PSlot3.Checked := False;
    end;
end;

procedure TForm1.SaveSlot2Click(Sender: TObject);
begin
  if (SaveSlot(2,CheckListBox1,PSlot2)) then
    begin
      ShowMessage('Successfully saved selected favorites to preset slot 2');
      PSlot1.Checked := False;
      PSlot2.Checked := True;
      PSlot3.Checked := False;
    end;
end;

procedure TForm1.SaveSlot3Click(Sender: TObject);
begin
  if (SaveSlot(3,CheckListBox1,PSlot3)) then
    begin
      ShowMessage('Successfully saved selected favorites to preset slot 3');
      PSlot1.Checked := False;
      PSlot2.Checked := False;
      PSlot3.Checked := True;
    end;
end;

procedure TForm1.SelectedOptionClick(Sender: TObject);
begin
  if (ViewSelected) then ViewSelected := False
  else ViewSelected := True;
  SelectedOption.Checked := ViewSelected;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
  LoadConfig(XMLEdit,GamesEdit,CheckListBox1,NANDCheckBox,ViewStyle,ViewSelected,LastFavFolder,HomeName);
end;

procedure TForm1.SlotName1Click(Sender: TObject);
var
  SlotName: String;
begin
  SlotName := Trim(InputBox('Slot Name','Please enter a name for this preset',''));
  if (SlotName = '') then exit;
  if (length(SlotName) > 15) then SlotName := Copy(SlotName, 1, 15);
  PSlot1.Caption := SlotName;
  SaveSlot(1,CheckListBox1,PSlot1,True);
end;

procedure TForm1.SlotName2Click(Sender: TObject);
var
  SlotName: String;
begin
  SlotName := Trim(InputBox('Slot Name','Please enter a name for this preset',''));
  if (SlotName = '') then exit;
  if (length(SlotName) > 15) then SlotName := Copy(SlotName, 1, 15);
  PSlot2.Caption := SlotName;
  SaveSlot(2,CheckListBox1,PSlot2,True);
end;

procedure TForm1.SlotName3Click(Sender: TObject);
var
  SlotName: String;
begin
  SlotName := Trim(InputBox('Slot Name','Please enter a name for this preset',''));
  if (SlotName = '') then exit;
  if (length(SlotName) > 15) then SlotName := Copy(SlotName, 1, 15);
  PSlot3.Caption := SlotName;
  SaveSlot(3,CheckListBox1,PSlot3,True);
end;

procedure TForm1.CheckListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  CheckListBoxDrawItem(Control,Index,ARect,State);
end;

procedure TForm1.CheckListBox1ItemClick(Sender: TObject; Index: integer);
var
  checked : integer;
begin
  checked := GetCheckCount(CheckListBox1,StatusPanel0,False);
  if CheckListBox1.Checked[Index] then
    begin
      if (checked > 30) then
        begin
          CheckListBox1.Checked[Index] := False;
          Dec(checked);
          MessageDlg('Error','Maximum of 30 favorites allowed.',mtError,[mbOk],0);
        end;
    end
  else if ViewSelected then
    begin
      SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
      LoadConfig(XMLEdit,GamesEdit,CheckListBox1,NANDCheckBox,ViewStyle,ViewSelected,LastFavFolder,HomeName);
    end;
  if (checked = 1) then
    StatusPanel0.text := ' 1 game selected.'
  else
    StatusPanel0.text := ' '+inttostr(checked)+' games selected.';
end;

procedure TForm1.CheckListBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) then
   MouseInput.Click(mbLeft,[]);
end;

procedure TForm1.ClearBtnClick(Sender: TObject);
begin
  CheckListBox1.CheckAll(cbUnchecked,False,False);
  GetCheckCount(CheckListBox1, StatusPanel0, True);
end;

procedure TForm1.FlatOptionClick(Sender: TObject);
begin
  ViewStyle := 0;
  FlatOption.Checked:=True;
  ParentOption.Checked:=False;
  ParentChildOption.Checked:=False;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
  LoadConfig(XMLEdit,GamesEdit,CheckListBox1,NANDCheckBox,ViewStyle,ViewSelected,LastFavFolder,HomeName);
end;

procedure TForm1.ParentOptionClick(Sender: TObject);
begin
  ViewStyle := 1;
  FlatOption.Checked:=False;
  ParentOption.Checked:=True;
  ParentChildOption.Checked:=False;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
  LoadConfig(XMLEdit,GamesEdit,CheckListBox1,NANDCheckBox,ViewStyle,ViewSelected,LastFavFolder,HomeName);
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
var
  Code: String;
begin
  Code := GetCode(CheckListBox1.GetSelectedText,CheckListBox1);
  if (Code <> 'HOME') then abort;
end;

procedure TForm1.RenameHomeClick(Sender: TObject);
var
  NewName: String;
begin
  NewName := Trim(InputBox('Rename Home Folder','Please enter a name for this folder',''));
  if (NewName = '') then exit;
  if (Length(NewName) > 15) then NewName := Copy(NewName,1,15);
  HomeName := NewName;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
  LoadConfig(XMLEdit,GamesEdit,CheckListBox1,NANDCheckBox,ViewStyle,ViewSelected,LastFavFolder,HomeName);
end;

procedure TForm1.ParentChildOptionClick(Sender: TObject);
begin
  ViewStyle := 2;
  FlatOption.Checked:=False;
  ParentOption.Checked:=False;
  ParentChildOption.Checked:=True;
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
  LoadConfig(XMLEdit,GamesEdit,CheckListBox1,NANDCheckBox,ViewStyle,ViewSelected,LastFavFolder,HomeName);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig(LastUsedXML,GamesEdit.Caption,NANDCheckBox.Checked,LastFavFolder,ViewStyle,ViewSelected,HomeName,CheckListBox1);
end;

end.

