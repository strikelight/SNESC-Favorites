unit tools;

{ tools.pas
  Description: Unit for other tasks the application can perform.

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
  Classes, SysUtils, FileUtil, IniFiles, Dialogs, ComCtrls;

function AddHomeIcons(Path: String; ProgressBar: TProgressBar; StatusBar: TStatusBar):Boolean;

implementation

function AddHomeIcons(Path: String; ProgressBar: TProgressBar; StatusBar: TStatusBar):Boolean;
var
 FD,FD2: TSearchRec;
 FPath,sName,sExec: String;
 Ini: TIniFile;
 DeskFile: TStringList;
 Res,fb: Boolean;
 StatusPanel: TStatusPanel;
 fCount: Integer;
begin
  AddHomeIcons := False;
  FPath := ExtractFilePath(ParamStr(0));
  if (not DirectoryExists(Path)) then exit;
  ProgressBar.Visible:=True;
  StatusPanel := StatusBar.Panels.Items[1];
  Path := Path + '\';
  fCount := 0;
  if (FindFirst(Path+'*',faDirectory,FD)=0) then
    repeat
      Inc(fCount);
    until (FindNext(FD) <> 0);
  ProgressBar.Max := fCount;
  ProgressBar.Position:=0;
  StatusPanel.Text := Inttostr(Round(ProgressBar.Position/ProgressBar.Max*100))+'%';
  StatusBar.Update;

  if (FindFirst(Path+'*',faDirectory,FD)=0) then
    begin
      repeat
        if (FindFirst(Path+FD.Name+'\CLV-S-*',faDirectory,FD2)=0) then
          begin
            fb := False;
            repeat
              Ini := TIniFile.Create(Path+FD.Name+'\'+FD2.Name+'\'+FD2.Name+'.desktop');
              try
                sName := Ini.ReadString('Desktop Entry','Name','');
                if (sName = 'Back') then
                  begin
                    sExec := Ini.ReadString('Desktop Entry','Exec','');
                    if (Pos('chmenu',sExec) > 0) and (Pos('chmenu 000',sExec) = 0) then
                      begin
                        if (DirectoryExists(Path+FD.Name+'\CLV-S-99001')) then
                          begin
                            Res := DeleteDirectory(Path+FD.Name+'\CLV-S-99001',True);
                            if Res then
                              Res := RemoveDir(Path+FD.Name+'\CLV-S-99001');
                          end;
                        if (not DirectoryExists(Path+FD.Name+'\CLV-S-99001')) then
                          begin
                            CreateDir(Path+FD.Name+'\CLV-S-99001');
                            DeskFile := TStringList.Create;
                            try
                              DeskFile.LineBreak := #10;
                              DeskFile.Add('[Desktop Entry]');
                              DeskFile.Add('Type=Application');
                              DeskFile.Add('Exec=/bin/chmenu 000 ');
                              DeskFile.Add('Path=/var/saves/FOLDER');
                              DeskFile.Add('Name=Home');
                              DeskFile.Add('Icon=/var/games/CLV-S-99001/CLV-S-99001.png');
                              DeskFile.Add('');
                              DeskFile.Add('[X-CLOVER Game]');
                              DeskFile.Add('Code=CLV-S-99001');
                              DeskFile.Add('TestID=777');
                              DeskFile.Add('ID=0');
                              DeskFile.Add('Players=1');
                              DeskFile.Add('Simultaneous=0');
                              DeskFile.Add('ReleaseDate=9999-99-99');
                              DeskFile.Add('SaveCount=0');
                              DeskFile.Add('SortRawTitle=Яhome');
                              DeskFile.Add('SortRawPublisher=ZZZZZZZZZZ');
                              DeskFile.Add('Copyright=hakchi2 ©2017 Alexey ''Cluster'' Avdyukhin');

                              DeskFile.SaveToFile(Path+FD.Name+'\CLV-S-99001\CLV-S-99001.desktop');
                              if (FileExists(FPath+'\folder_home.png')) then
                                begin
                                  CopyFile(FPath+'\folder_home.png',Path+FD.Name+'\CLV-S-99001\CLV-S-99001.png');
                                end;
                              if (FileExists(FPath+'\folder_home_small.png')) then
                                begin
                                  CopyFile(FPath+'\folder_home_small.png',Path+FD.Name+'\CLV-S-99001\CLV-S-99001_small.png');
                                end;
                            finally
                              DeskFile.Free;
                            end;
                          end;
                        fb := True;
                      end;
                  end;
              finally
                Ini.Free;
              end;
            until ((FindNext(FD2) <> 0) or fb);
          end;
        ProgressBar.Position:=ProgressBar.Position+1;
        StatusPanel.Text := Inttostr(Round(ProgressBar.Position/ProgressBar.Max*100))+'%';
        StatusBar.Update;
      until (FindNext(FD) <> 0);
    end;
  AddHomeIcons := True;
end;

end.

