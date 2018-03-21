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
  Classes, SysUtils, Controls, LCLtype, LCLIntf, Graphics, Checklst, ComCtrls;

procedure CheckListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
function GetCheckCount(CheckListBox: TCheckListBox; StatusPanel: TStatusPanel; Update:Boolean):Integer;

implementation

procedure CheckListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
Var clOld: TColor;
    w,i : Integer;
    OldBrushStyle: TBrushStyle;
    OldTextStyle: TTextStyle;
    NewTextStyle: TTextStyle;
    BRect: TRect;
    b,c: Boolean;
    OldStyle: TFontStyles;
    s: string;
const
   MARGIN = 2;
   IsChecked : array[Boolean] of Integer = (DFCS_BUTTONCHECK, DFCS_BUTTONCHECK or DFCS_CHECKED) ;
   ColorSet: Array[0..3] of TColor = ($FDDE54, $2DAEF6, $8DFC5E, $37276B);
begin
   b := odSelected In State;
   c := odFocused In State;
   With TCheckListBox(Control) Do
   Begin
      clOld := Canvas.Font.Color;
      OldBrushStyle := Canvas.Brush.Style;
      OldTextStyle := Canvas.TextStyle;
      OldStyle := Canvas.Font.Style;
      s := Items[Index];
     if (s <> '') and (s[1] = '-') then
       begin
         Canvas.Brush.Style := bsSolid;
         Canvas.Brush.Color := clWhite;

         Canvas.Font.Style := [fsBold];
         Canvas.FillRect(ARect);
         Canvas.Pen.Style := psSolid;
         Canvas.Pen.Color := $FFA855;
         Canvas.Font.Color := $FFA855;
         Canvas.Line(
           ARect.Left + MARGIN,
           (ARect.Top + ARect.Bottom) div 2,
           ARect.Right - MARGIN,
           (ARect.Top + ARect.Bottom) div 2
         );
         Delete(s, 1, 1);
         if s <> '' then
           begin
             s := ' ' + s + ' ';
             w := Canvas.TextWidth(s);
             Canvas.TextOut(
               (ARect.Left + ARect.Right - w) div 2,
               (ARect.Top + ARect.Bottom - Canvas.TextHeight('Tg')) div 2,
               s
             );
           end;
         Canvas.Pen.Style := psClear;
         Canvas.Brush.Style := bsClear;
         Canvas.Pen.Color := clWhite;
         if (c) then Canvas.DrawFocusRect(ARect);
         Canvas.Font.Style:=[];
         Canvas.Font.Color := clOld;
         Canvas.Brush.Style := OldBrushStyle;
         Canvas.TextStyle := OldTextStyle;
         Canvas.Font.Style := OldStyle;
         exit;
       end
      else if (s <> '') and (s[1] = '=') then
       begin
         Canvas.Brush.Style := bsSolid;
         Canvas.Brush.Color := clWhite;
         Canvas.Font.Style := [];
         Canvas.FillRect(ARect);
         Canvas.Pen.Style := psSolid;
         i := 0;
         while (s[1] = '=') do
           begin
             Delete(s, 1, 1);
             Inc(i);
             if (i > High(ColorSet)) then
               i := High(ColorSet);
           end;
         Canvas.Pen.Color := ColorSet[i];
         Canvas.Font.Color := ColorSet[i];
         Canvas.Line(
           ARect.Left + MARGIN,
           (ARect.Top + ARect.Bottom) div 2,
           ARect.Right - MARGIN,
           (ARect.Top + ARect.Bottom) div 2
         );
         if s <> '' then
           begin
             s := ' ' + s + ' ';
             w := Canvas.TextWidth(s);
             Canvas.TextOut(
               (ARect.Left + ARect.Right - w) div 2,
               (ARect.Top + ARect.Bottom - Canvas.TextHeight('Tg')) div 2,
               s
             );
           end;
         Canvas.Pen.Style := psClear;
         Canvas.Brush.Style := bsClear;
         Canvas.Pen.Color := clWhite;
         if (c) then Canvas.DrawFocusRect(ARect);
         Canvas.Font.Style:=[];
         Canvas.Font.Color := clOld;
         Canvas.Brush.Style := OldBrushStyle;
         Canvas.TextStyle := OldTextStyle;
         Canvas.Font.Style := OldStyle;
         exit;
       end;

      s := Items[Index];
      if (s <> '') and ((s[1] = '-') or (s[1] = '=')) then exit;
      Canvas.Brush.Color := Brush.Color;
      Canvas.FillRect(ARect);
      Canvas.Brush.Style := bsClear;
      BRect.Left := ARect.Left + 1;
      BRect.Top := ARect.Top + 1;
      BRect.Bottom := ARect.Bottom+ 2;
      BRect.Right := ARect.Left + (ARect.Bottom - ARect.Top) - 2;

      DrawFrameControl(Canvas.Handle, BRect, DFC_BUTTON, IsChecked[Checked[Index]]);

      If b Then
         Canvas.Font.Style := [fsBold];

      If Checked[Index] Then
      Begin
        Canvas.Font.Color := clBlue;
      end Else Canvas.Font.Color := Font.Color;
      NewTextStyle := OldTextStyle;
      NewTextStyle.Layout := tlCenter;
      Canvas.TextStyle := NewTextStyle;
      ARect.Left := BRect.Right;
      Canvas.TextRect(ARect, ARect.Left+2, ARect.Top, Items[Index]);

      Canvas.Font.Color := clOld;
      Canvas.Brush.Style := OldBrushStyle;
      Canvas.TextStyle := OldTextStyle;
      Canvas.Font.Style := OldStyle;
   end;
end;

function GetCheckCount(CheckListBox: TCheckListBox; StatusPanel: TStatusPanel; Update:Boolean):Integer;
var
  i,j: Integer;
begin
  j := 0;
  for i := 0 to CheckListBox.Count-1 do
    if CheckListBox.Checked[i] then
      Inc(j);
  if Update then
    begin
      if (j = 1) then
        StatusPanel.text := ' 1 game selected.'
      else
        StatusPanel.text := ' '+inttostr(j)+' games selected.';
    end;
  GetCheckCount := j;
end;

end.

