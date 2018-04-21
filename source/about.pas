unit about;

{ about.pas
  Description: Unit for displaying about box and
               providing version information.

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


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LCLIntf, LCLVersion, fileinfo, winpeimagereader;

procedure UpdateAboutInfo;
function GetProductVersion:String;
function GetProductName:String;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PwrLabel: TLabel;
    RDLabel: TLabel;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    VerLabel: TLabel;
    PrgInfoLabel: TLabel;
    StaticText1: TStaticText;
    procedure Button1Click(Sender: TObject);
    procedure StaticText1Click(Sender: TObject);
    procedure StaticText1MouseEnter(Sender: TObject);
    procedure StaticText1MouseLeave(Sender: TObject);
    procedure StaticText2Click(Sender: TObject);
    procedure StaticText2MouseEnter(Sender: TObject);
    procedure StaticText2MouseLeave(Sender: TObject);
    procedure StaticText3Click(Sender: TObject);
    procedure StaticText3MouseEnter(Sender: TObject);
    procedure StaticText3MouseLeave(Sender: TObject);
  private
  public
  end;

var
  AboutForm: TAboutForm;


implementation

{$R *.lfm}

{ TAboutForm }

function GetProductName:String;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    GetProductName := FileVerInfo.VersionStrings.Values['ProductName'];
  finally
    FileVerInfo.Free;
  end;
end;


function GetProductVersion:String;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    GetProductVersion := FileVerInfo.VersionStrings.Values['ProductVersion'];
  finally
    FileVerInfo.Free;
  end;
end;

procedure UpdateAboutInfo;
var
  MDate,MYear,MMonth,MDay: String;
begin
  AboutForm.PrgInfoLabel.Caption:=GetProductName;
  AboutForm.VerLabel.Caption:=GetProductVersion;
  MDate := {$I %DATE%};
  MYear := Copy(MDate, 1, 4);
  MDate := Copy(MDate, 6, length(MDate));
  MMonth := Copy(MDate, 1, pos('/',MDate)-1);
  MDay := Copy(MDate,pos('/',MDate)+1,length(MDate));
  AboutForm.RDLabel.Caption:=MDay+'/'+MMonth+'/'+MYear;
  AboutForm.PwrLabel.Caption:='Powered by Lazarus '+lcl_version+', FPC '+{$I %FPCVERSION%};
//  AboutForm.RDLabel.Caption:=RDate;
  AboutForm.Update;
end;

procedure TAboutForm.StaticText1Click(Sender: TObject);
begin
  OpenUrl('https://github.com/strikelight/SNESC-Favorites/releases');
end;

procedure TAboutForm.Button1Click(Sender: TObject);
begin
  AboutForm.Close;
end;

procedure TAboutForm.StaticText1MouseEnter(Sender: TObject);
begin
  StaticText1.Cursor := crHandPoint;
  StaticText1.Font.Color := clBlue;
  StaticText1.Font.Style := [fsUnderline];
end;

procedure TAboutForm.StaticText1MouseLeave(Sender: TObject);
begin
  StaticText1.Font.Color := clNavy;
  StaticText1.Font.Style := [];
end;

procedure TAboutForm.StaticText2Click(Sender: TObject);
begin
  OpenUrl('https://discord.gg/QMsazaS');
end;

procedure TAboutForm.StaticText2MouseEnter(Sender: TObject);
begin
  StaticText2.Cursor := crHandPoint;
  StaticText2.Font.Color := clBlue;
  StaticText2.Font.Style := [fsUnderline];
end;

procedure TAboutForm.StaticText2MouseLeave(Sender: TObject);
begin
  StaticText2.Font.Color := clNavy;
  StaticText2.Font.Style := [];
end;

procedure TAboutForm.StaticText3Click(Sender: TObject);
begin
  OpenUrl('https://www.reddit.com/r/miniSNESmods/');
end;

procedure TAboutForm.StaticText3MouseEnter(Sender: TObject);
begin
  StaticText3.Cursor := crHandPoint;
  StaticText3.Font.Color := clBlue;
  StaticText3.Font.Style := [fsUnderline];
end;

procedure TAboutForm.StaticText3MouseLeave(Sender: TObject);
begin
  StaticText3.Font.Color := clNavy;
  StaticText3.Font.Style := [];
end;

end.

