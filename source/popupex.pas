{ popupex.pas
  Description: Provides a class for a modified PopupNotifier
               based on code by A. J. Venter and Felipe Monteiro de Carvalho

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


unit PopupEx;

interface

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, IpHtml, ClipBrd;
  { Note: Be careful that ExtCtrls depend on popupnotifier, so
    it should have only a minimal amount of dependencies to avoid circular
    references. Preferably only units that ExtCtrls already has }

type
  { TNotifierXButton }

  { To avoid dependency on Buttons }
  TNotifierExXButtonButtonState =
  (
    nbsUp,       // button is up
    nbsDown,     // button is down
    nbsHot       // button is under mouse
  );

  TNotifierExXButton = class(TCustomControl)
  private
    FState: TNotifierExXButtonButtonState;
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  end;

  { TNotifierExForm }

  TNotifierExForm = class(THintWindow)
  private
    FButtonSize: Integer; // Introduce new X Button Size property
    lblTitle: TLabel;
    lblText: TLabel;
//    htmlPnl: TIpHtmlPanel;
    FhtmlColor: TColor;
    FhtmlhexColor: String;
    imgIcon: TPicture;
    btnX: TNotifierExXButton;
    procedure HideForm(Sender: TObject);
    procedure HandleResize(Sender: TObject);
    procedure SetFButtonSize(AValue: integer);
  public
    htmlPnl: TIpHtmlPanel;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property ButtonSize: integer read FButtonSize write SetFButtonSize;
    property htmlColor: TColor read FhtmlColor write FhtmlColor;
    property htmlhexColor: String read FhtmlhexColor write FhtmlhexColor;
  end;

  { TPopupNotifier }

  TPopupEx = class(TComponent)
  private
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetIcon: TPicture;
    procedure SetIcon(const Value: TPicture);
    function GetText: string;
    procedure SetText(Value: string);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    procedure SetOnClose(const Value: TCloseEvent);
    function  GetOnClose:TCloseEvent;
  public
    vNotifierForm: TNotifierExForm;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Hide;
    procedure Show;
    procedure ShowAtPos(x: Integer; y: Integer);
  published
    property Color: TColor  read GetColor write SetColor;
    property Icon: TPicture read GetIcon write SetIcon;
    property Text: string read GetText write SetText;
    property Title: string read GetTitle write SetTitle;
    property Visible: Boolean read GetVisible write SetVisible;
    property OnClose: TCloseEvent  read GetOnClose write SetOnClose;
  end;

const
  BGDrawn: Boolean = False;

implementation

const
  INT_NOTIFIER_FORM_WIDTH  = 325;
  INT_NOTIFIER_FORM_HEIGHT = 110;
  INT_NOTIFIER_SCREEN_SPACING = 10;
  INT_NOTIFIER_SPACING = 5;
  INT_NOTIFIER_BUTTON_SIZE = 20;


{$ifndef fpc}
  {$R *.DFM}
{$endif}

{ TNotifierExXButton }

procedure TNotifierExXButton.HandleMouseDown(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin
    FState := nbsDown;
    Self.Invalidate;
  end;
end;

procedure TNotifierExXButton.HandleMouseUp(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FState := nbsUp;
  Self.Invalidate;
end;

constructor TNotifierExXButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FState := nbsUp;

  OnMouseUp := HandleMouseUp;
  OnMouseDown := HandleMouseDown;
end;

destructor TNotifierExXButton.Destroy;
begin

  inherited Destroy;
end;

procedure TNotifierExXButton.Paint;
begin
  Canvas.Pen.Color := cl3DDKShadow;
  Canvas.Pen.Width := 1;

  Canvas.Brush.Color := Color;
  Canvas.FillRect(0, 0, Width, Height);

  if FState = nbsUp then
    Canvas.Brush.Color := clBtnFace
  else
    Canvas.Brush.Color := clHotLight;

  Canvas.RoundRect(0, 0, Width, Height, 4, 4);

  Canvas.Pen.EndCap:=pecSquare;
  Canvas.Pen.Width := 2;

  Canvas.MoveTo(7, 7);
  Canvas.LineTo(Width - 7, Height - 7);

  Canvas.MoveTo(Width - 7, 7);
  Canvas.LineTo(7, Height - 7);

  inherited Paint;
end;

{ TNotifierExForm }

{*******************************************************************
*  TNotifierExForm.Create ()
*
*  Creates the notifier form
*******************************************************************}
constructor TNotifierExForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BorderStyle := bsNone;
  DoubleBuffered := True;

  Width := INT_NOTIFIER_FORM_WIDTH;
  Height := INT_NOTIFIER_FORM_HEIGHT;

  if Screen.Width - INT_NOTIFIER_SCREEN_SPACING < Width then
    Width := Screen.Width - INT_NOTIFIER_SCREEN_SPACING;

  ImgIcon := TPicture.Create;

  lblTitle := TLabel.Create(Self);
  lblTitle.Parent := Self;
  lblTitle.AutoSize := False;
  lblTitle.Transparent := True;
  lblTitle.Font.Style := [FsBold];
  lblTitle.Caption := 'Caption';
  lblTitle.ParentColor := True;
  lblTitle.OnClick := HideForm;

  // Modification: Introduce Html panel for display instead of label
  htmlPnl := TIpHtmlPanel.Create(Self);
  htmlPnl.Parent := Self;
  htmlPnl.AutoSize := False;
  htmlPnl.OnClick := HideForm;
  htmlPnl.BorderStyle:=bsNone;
  if (FhtmlColor = clNone) then
    begin
      htmlPnl.BgColor:=clSkyBlue;
      htmlPnl.Color:=clSkyBlue;
    end
  else
    begin
      htmlPnl.BgColor:=FhtmlColor;
      htmlPnl.Color:=FhtmlColor;
    end;
  htmlPnl.MarginHeight:=1;
  htmlPnl.MarginWidth:=1;
  htmlPnl.Visible:=False;

  // Keep label for dimension calculations
  lblText := TLabel.Create(Self);
  lblText.Parent := Self;
  lblText.AutoSize := False;
  lblText.Transparent := True;
  lblText.Caption := 'Text';
  lblText.WordWrap := True;
  lblText.ParentColor := True;
  lblText.OnClick := HideForm;
  lblText.Visible:= False;

  FButtonSize := INT_NOTIFIER_BUTTON_SIZE;
  BtnX := TNotifierExXButton.Create(Self);
  BtnX.Parent := Self;
  BtnX.Color :=  Color;
  btnX.OnClick := HideForm;

  HandleResize(Self);

  Color := $DCFFFF; // Doesn't work on Gtk

  // Connects the methods to events
  OnClick := HideForm;
  OnShow := HandleResize;
end;

{*******************************************************************
*  TNotifierExForm.Destroy ()
*
*  Releases associated resources of the notifier form
*******************************************************************}
destructor TNotifierExForm.Destroy;

begin
  ImgIcon.Free;
  lblTitle.Free;
  lblText.Free;
  htmlPnl.Free;
  BtnX.Free;
  inherited Destroy;
end;

procedure TNotifierExForm.SetFButtonSize(AValue: integer);
begin
  if (FButtonSize=AValue) then exit;
  FButtonSize:=AValue;
  HandleResize(Self);
end;

procedure TNotifierExForm.Paint;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0,0,width,height));

  { Paints the icon. We can't use a TImage because it's on ExtCtrls }
  if Assigned(imgIcon.Bitmap) then Canvas.Draw(5, 5, imgIcon.Bitmap);
end;

{*******************************************************************
*  TNotifierExForm.HideForm ()
*
*  Utilized for events that hide the form, such as clicking on it
*******************************************************************}
procedure TNotifierExForm.HideForm(Sender: TObject);
Var NoValue :TCloseAction;
begin
if Assigned(OnClose) then
   OnClose(Self,NoValue);
  Hide;
end;

{*******************************************************************
*  TNotifierExForm.HandleResize ()
*
*  Handles OnResize events of the form
*******************************************************************}
procedure TNotifierExForm.HandleResize(Sender: TObject);
var
  IconAdjust: Integer;
begin
  if (ImgIcon.Bitmap <> nil) then
    IconAdjust := INT_NOTIFIER_SPACING + imgIcon.Bitmap.Width
  else
    IconAdjust := 0;

  if (lblTitle <> nil) then
  begin
    lblTitle.Left := IconAdjust + INT_NOTIFIER_SPACING;
    lblTitle.Top := INT_NOTIFIER_SPACING;
    lblTitle.Width := Width - (lblTitle.Left + INT_NOTIFIER_SPACING);
    lblTitle.Height := 20;
  end;

  if (htmlPnl <> nil) then
  begin
    htmlPnl.Left := IconAdjust + 12;
    htmlPnl.Top := LblTitle.Top + LblTitle.Height + INT_NOTIFIER_SPACING-5;
    htmlPnl.Width := Width - (htmlPnl.Left + INT_NOTIFIER_SPACING)+5;
    htmlPnl.Height := Height - (htmlPnl.Top + INT_NOTIFIER_SPACING);
  end;

  if (lblText <> nil) then
  begin
    lblText.Left := IconAdjust + 20;
    lblText.Top := LblTitle.Top + LblTitle.Height + INT_NOTIFIER_SPACING;
    lblText.Width := Width - (lblText.Left + INT_NOTIFIER_SPACING);
    lblText.Height := Height - (lblText.Top + INT_NOTIFIER_SPACING);
  end;

  if (BtnX <> nil) then
  begin
    BtnX.Left := Width - (FButtonSize + 5);
    BtnX.Top := INT_NOTIFIER_SPACING;
    BtnX.Width := FButtonSize;
    BtnX.Height := FButtonSize;
  end;
end;

{ TPopupEx }

{*******************************************************************
*  Methods associated to properties
*******************************************************************}

function TPopupEx.GetTitle: string;
begin
  Result := vNotifierForm.lblTitle.Caption;
end;

procedure TPopupEx.SetTitle(const Value: string);
begin
  vNotifierForm.lblTitle.Caption := Value;
end;

procedure TPopupEx.SetOnClose(const Value: TCloseEvent);
begin
  VNotifierForm.Onclose := Value;
end;

function TPopupEx.GetOnClose:TCloseEvent;
begin
  Result := VNotifierForm.Onclose;
end;


function TPopupEx.GetVisible: Boolean;
begin
  Result := vNotifierForm.Visible;
end;

procedure TPopupEx.SetVisible(const Value: Boolean);
begin
  vNotifierForm.Visible := Value;
end;

function TPopupEx.GetText: string;
begin
  Result := vNotifierForm.lblText.Caption;
end;

procedure TPopupEx.SetText(Value: string);
var
  tHTML,Col: String;
  tmpPnl: TIPHtmlPanel;
  dRect: TRect;
begin
  if (vNotifierForm.lblText.Caption = Value) and Visible then exit;
  if (vNotifierForm.FhtmlColor = clNone) then
    begin
      vNotifierForm.htmlPnl.BgColor:=clSkyBlue;
      vNotifierForm.htmlPnl.Color:=clSkyBlue;
    end
  else
    begin
      vNotifierForm.htmlPnl.BgColor:=vNotifierForm.FhtmlColor;
      vNotifierForm.htmlPnl.Color:=vNotifierForm.FhtmlColor;
    end;
  vNotifierForm.lblText.Caption:=Value;
  vNotifierForm.BeginUpdateBounds;
  vNotifierForm.DoubleBuffered:=True;
  vNotifierForm.htmlPnl.Visible:=False;

  tmpPnl := TIpHtmlPanel.Create(Self);
  tmpPnl.AutoSize := False;
  tmpPnl.BorderStyle:=bsNone;
  tmpPnl.BgColor:=vNotifierForm.htmlPnl.BgColor;
  tmpPnl.Color:=vNotifierForm.htmlPnl.Color;
  tmpPnl.MarginHeight:=1;
  tmpPnl.MarginWidth:=1;
  tmpPnl.Visible:=True;
  tmpPnl.DoubleBuffered:=True;
  if (Assigned(vNotifierForm.htmlPnl.DataProvider)) then
    tmpPnl.DataProvider:=vNotifierForm.htmlPnl.DataProvider;
  if (tmpPnl <> nil) then
  begin
    tmpPnl.Left := vNotifierForm.ClientOrigin.x+vNotifierForm.Width+100;
    tmpPnl.Top := vNotifierForm.ClientOrigin.y+vNotifierForm.Height+100;
    tmpPnl.Width := vNotifierForm.htmlPnl.Width+15;
    tmpPnl.Height := vNotifierForm.htmlPnl.Height+20;
  end;

  if (vNotifierForm.htmlhexColor = '') then
    Col := '#A6CAF0'
  else
    Col := vNotifierForm.htmlhexColor;
  tHTML := '<html><head><title>test</title></head><body bgcolor="'+Col+'"><font size="6px">'+Value+'</font></body></html>';
  tHTML := StringReplace(tHTML,#13#10,'<br/>',[rfReplaceAll]);
  try
    tmpPnl.SetHtmlFromStr(tHTML);
    tmpPnl.Parent := vNotifierForm;
    dRect := Rect(vNotifierForm.htmlPnl.Left,
                   vNotifierForm.htmlPnl.Top,
                   vNotifierForm.htmlPnl.Left+vNotifierForm.htmlPnl.Width,
                   vNotifierForm.htmlPnl.Top+vNotifierForm.htmlPnl.Height);
    tmpPnl.PaintTo(vNotifierForm.Canvas,dRect.Left,dRect.Top);
  finally
    tmpPnl.Free;
  end;
  vNotifierForm.EndUpdateBounds;
end;

function TPopupEx.GetIcon: TPicture;
begin
  Result := vNotifierForm.imgIcon;
end;

procedure TPopupEx.SetIcon(const Value: TPicture);
begin
  vNotifierForm.imgIcon.Assign(Value);
end;

function TPopupEx.GetColor: TColor;
begin
  Result := vNotifierForm.Color;
end;

procedure TPopupEx.SetColor(const Value: TColor);
begin
  vNotifierForm.Color := Value;
end;

{*******************************************************************
*  TPopupEx.Create ()
*******************************************************************}
constructor TPopupEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  vNotifierForm := TNotifierExForm.Create(nil);
  vNotifierForm.Visible := False;
end;

{*******************************************************************
*  TPopupEx.Destroy ()
*******************************************************************}
destructor TPopupEx.Destroy;
begin
  vNotifierForm.Hide;

  // The following line needs to be removed if we have
  // vNotifierForm := TNotifierExForm.Create(Application);
  vNotifierForm.Free;

  inherited Destroy;
end;

{*******************************************************************
*  TPopupEx.Hide ()
*******************************************************************}
procedure TPopupEx.Hide;
begin
  vNotifierForm.Hide;
end;

{*******************************************************************
*  TPopupEx.Show ()
*******************************************************************}
procedure TPopupEx.Show;
begin
  vNotifierForm.Show;
end;

{*******************************************************************
*  TPopupEx.ShowAtPos ()
*
*  Shows the notifier at a specific position
*
*  The position is corrected to fit the screen, similarly to how
*  a popup menu would have it's position corrected
*
*******************************************************************}
procedure TPopupEx.ShowAtPos(x: Integer; y: Integer);
var
  tmp: String;
begin
  if x + vNotifierForm.Width > Screen.Width then
  begin
    vNotifierForm.left := x - vNotifierForm.Width;
    if vNotifierForm.Left < 0 then vNotifierForm.Left := 0;
  end
  else
    vNotifierForm.left := x;

  if y + vNotifierForm.Height > Screen.Height then
  begin
    vNotifierForm.top := y - vNotifierForm.Height;
    if vNotifierForm.top < 0 then vNotifierForm.top := 0;
  end
  else
    vNotifierForm.top := y;

  vNotifierForm.Show;
  vNotifierForm.Update;
  tmp := vNotifierForm.lblText.Caption;
  vNotifierForm.lblText.Caption := '';
  SetText(tmp);
end;

end.
