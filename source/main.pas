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
  ui_utils, futils, config, help;

type

  { TForm1 }

  TForm1 = class(TForm)
    LoadBtn: TButton;
    GamesEdit: TDirectoryEdit;
    Label3: TLabel;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    FlatOption: TMenuItem;
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
    procedure ClearBtnClick(Sender: TObject);
    procedure FlatOptionClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure ParentChildOptionClick(Sender: TObject);
    procedure ParentOptionClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
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

const
  pName = 'SNESC Favorites';
  pVersion = '1.0.0';

implementation

{$R *.lfm}

{ TForm1 }



procedure TForm1.FormCreate(Sender: TObject);
begin
  Form1.Caption := pName+' - '+pVersion;
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

  LastFavFolder := '';
  LoadConfig(XMLEdit,GamesEdit,CheckListBox1,ViewStyle,LastFavFolder);
  FlatOption.Checked:=False;
  ParentOption.Checked:=False;
  ParentChildOption.Checked:=False;
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
begin
  if (XMLEdit.Caption = '') or (not FileExists(XMLEdit.Caption)) then exit;
  CheckListBox1.Clear;
  SetLength(GameCodes,0);
  LoadGamesList(CheckListBox1,XMLEdit.Caption,ViewStyle);
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
  MessageDlg('About',pName+' '+pVersion+' by StrikeLight.'+#10#13+#10#13+'Released under the GNU Public License.',mtInformation,[mbOk],0);
end;

procedure TForm1.SaveBtnClick(Sender: TObject);
var
  fname: string;
  Res: Boolean;
  LastFolder: String;
begin
  fname := GamesEdit.Caption;
  fname := fname + '\';
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
  if (fname = '') or (not DirectoryExists(fname)) then exit;
  Res := CreateFaveLinks(fname,CheckListBox1,ProgressBar,StatusBar1,LastFavFolder);
  if (MessageDlg('Save','Completed.',mtInformation,[mbOk],0) = mrOk) then
    begin
      ProgressBar.Position:=0;
      statusPanel1.Text:='';
    end;
end;

procedure TForm1.CheckListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  CheckListBoxDrawItem(Control,Index,ARect,State);
end;

procedure TForm1.CheckListBox1ItemClick(Sender: TObject; Index: integer);
var
  i,checked : integer;
begin
  if (Sender as TCheckListBox).Checked[Index] then
    begin
      checked := 0;
      for i := 0 to CheckListBox1.Count-1 do
        if (CheckListBox1.Checked[i]) then inc(checked);
      if (checked > 30) then
        begin
          CheckListBox1.Checked[Index] := False;
          MessageDlg('Error','Maximum of 30 favorites allowed.',mtError,[mbOk],0);
        end;
    end;
end;

procedure TForm1.ClearBtnClick(Sender: TObject);
begin
  CheckListBox1.CheckAll(cbUnchecked,False,False);
end;

procedure TForm1.FlatOptionClick(Sender: TObject);
begin
  ViewStyle := 0;
  FlatOption.Checked:=True;
  ParentOption.Checked:=False;
  ParentChildOption.Checked:=False;
  SaveConfig(XMLEdit.Caption,GamesEdit.Caption,LastFavFolder,ViewStyle,CheckListBox1);
  LoadConfig(XMLEdit,GamesEdit,CheckListBox1,ViewStyle,LastFavFolder);
//  LoadGamesList(CheckListBox1,XMLEdit.Caption,ViewStyle);
end;

procedure TForm1.ParentOptionClick(Sender: TObject);
begin
  ViewStyle := 1;
  FlatOption.Checked:=False;
  ParentOption.Checked:=True;
  ParentChildOption.Checked:=False;
  SaveConfig(XMLEdit.Caption,GamesEdit.Caption,LastFavFolder,ViewStyle,CheckListBox1);
  LoadConfig(XMLEdit,GamesEdit,CheckListBox1,ViewStyle,LastFavFolder);
//  LoadGamesList(CheckListBox1,XMLEdit.Caption,ViewStyle);
end;

procedure TForm1.ParentChildOptionClick(Sender: TObject);
begin
  ViewStyle := 2;
  FlatOption.Checked:=False;
  ParentOption.Checked:=False;
  ParentChildOption.Checked:=True;
  SaveConfig(XMLEdit.Caption,GamesEdit.Caption,LastFavFolder,ViewStyle,CheckListBox1);
  LoadConfig(XMLEdit,GamesEdit,CheckListBox1,ViewStyle,LastFavFolder);
//  LoadGamesList(CheckListBox1,XMLEdit.Caption,ViewStyle);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig(XMLEdit.Caption,GamesEdit.Caption,LastFavFolder,ViewStyle,CheckListBox1);
end;

end.

