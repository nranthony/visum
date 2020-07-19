unit ImageControl;

interface

uses
     Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, ExtCtrls, StdCtrls, Qt,
//  ******************************************************************
     TypesUnit, ImageWindow, StageControl;
//  ******************************************************************

type
  TImageCtrlFrm = class(TForm)
    Image: TImage;
    ControlsPanel: TPanel;
    MoveToButton: TButton;
    ChannelRadioGroup: TRadioGroup;
    PointCoOrdsLabel: TLabel;
    FromCenter2Label: TLabel;
    FromCenterLabel: TLabel;
    CoOrdsLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ChannelRadioGroupClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MoveToButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    MoveToX,
    MoveToY: Single;
    CrossPen: TPen;
    LinePen: TPen;
    CrossPenHandle: HPen;
  public
    ImageDC: HDC;
    DispChA,
    DispChB,
    DispChC: Boolean;
    DeltaX, DeltaY: Single;
    procedure DrawCenterCross();
  end;

var
  ImageCtrlFrm: TImageCtrlFrm;

implementation

uses Main;
{$R *.dfm}

procedure TImageCtrlFrm.FormCreate(Sender: TObject);
begin
Self.WindowState := wsMinimized;
Self.Color := PaletteRGB(25,25,30);
Self.ControlsPanel.Color := PaletteRGB(25,25,30);
Self.Width := 521;
Self.Height := 676;
ImageDC := CreateCompatibleDC(0);
DispChA := True;
Image.Canvas.Pen.Color := clYellow;
Image.Canvas.Pen.Width := 2;
CrossPen := TPen.Create;

CrossPenHandle := CreatePen(PS_DASH,1,clSilver);
SetBkMode(Image.Canvas.Handle,TRANSPARENT);
CrossPen.Handle := CrossPenHandle;

LinePen := TPen.Create;
LinePen.Width := 2;
LinePen.Style := psSolid;
LinePen.Color := clYellow;
end;

procedure TImageCtrlFrm.FormShow(Sender: TObject);
begin
Self.Top := 132;
Self.Left := Screen.DesktopWidth - Self.Width;
end;

procedure TImageCtrlFrm.ChannelRadioGroupClick(Sender: TObject);
begin
case ChannelRadioGroup.ItemIndex of
 0: begin
     DispChA := True; DispChB := False; DispChC := False;
    end;
 1: begin
     DispChA := False; DispChB := True; DispChC := False;
    end;
 2: begin
     DispChA := False; DispChB := False; DispChC := True;
    end;
end;

end;

procedure TImageCtrlFrm.MoveToButtonClick(Sender: TObject);
begin
MoveStageForm.XMoveEdit.Text := FormatFloat('0.######',MoveToX/1000);
MoveStageForm.YMoveEdit.Text := FormatFloat('0.######',MoveToY/1000);
MoveStageForm.Top := ImageCtrlFrm.Top;
MoveStageForm.Left := ImageCtrlFrm.Left - MoveStageForm.Width;
MoveStageForm.Show;
end;

procedure TImageCtrlFrm.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
DeltaX := ((X/Image.Width)-0.5)*SettingsInUse.Image.ImagePhysWidth;
DeltaY := -((Y/Image.Height)-0.5)*SettingsInUse.Image.ImagePhysLength;
CoOrdsLabel.Caption := '(' + FormatFloat('0.###',DeltaX) + Chr(Key_mu) +'m' +
                       ',' + FormatFloat('0.###',DeltaY) + Chr(Key_mu) +'m' + ')';
end;

procedure TImageCtrlFrm.ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
if Acquiring then Exit;
if DispChA then StretchBlt(Image.Canvas.Handle, 0, 0, Image.Width, Image.Height,
                           ImageForm.ChanInfoA.OffScreen, 0, 0, SettingsInUse.Image.ImageWidth,
                           SettingsInUse.Image.ImageLength, SRCCOPY);
if DispChB then StretchBlt(Image.Canvas.Handle, 0, 0, Image.Width, Image.Height,
                           ImageForm.ChanInfoB.OffScreen, 0, 0, SettingsInUse.Image.ImageWidth,
                           SettingsInUse.Image.ImageLength, SRCCOPY);
if DispChC then StretchBlt(Image.Canvas.Handle, 0, 0, Image.Width, Image.Height,
                           ImageForm.ChanInfoC.OffScreen, 0, 0, SettingsInUse.Image.ImageWidth,
                           SettingsInUse.Image.ImageLength, SRCCOPY);
MoveToEx(Image.Canvas.Handle, Image.Width div 2, Image.Height div 2, nil);
LineTo(Image.Canvas.Handle, X, Y);
MoveToEx(Image.Canvas.Handle, X-7, Y, nil);
LineTo(Image.Canvas.Handle, X+7, Y);
MoveToEx(Image.Canvas.Handle, X, Y-7, nil);
LineTo(Image.Canvas.Handle, X, Y+7);
DrawCenterCross;
Image.Invalidate;
MoveToX := DeltaX;
MoveToY := DeltaY;
PointCoOrdsLabel.Caption := '(' + FormatFloat('0.###',DeltaX) + Chr(Key_mu) +'m' +
                            ',' + FormatFloat('0.###',DeltaY) + Chr(Key_mu) +'m' + ')';
end;

procedure TImageCtrlFrm.DrawCenterCross;
begin
if CrossPen <> nil then Image.Canvas.Pen := CrossPen;
MoveToEx(Image.Canvas.Handle, Image.Width div 2, (Image.Height div 2)-Round(Image.Height*0.1), nil);
LineTo(Image.Canvas.Handle, Image.Width div 2, (Image.Height div 2)+Round(Image.Height*0.1));
MoveToEx(Image.Canvas.Handle, (Image.Width div 2)-Round(Image.Width*0.1), Image.Height div 2, nil);
LineTo(Image.Canvas.Handle, (Image.Width div 2)+Round(Image.Width*0.1), Image.Height div 2);
Image.Invalidate;
if LinePen <> nil then Image.Canvas.Pen := LinePen;
end;


  
procedure TImageCtrlFrm.FormDestroy(Sender: TObject);
begin
DeleteDC(ImageDC);
CrossPen.Free;
LinePen.Free;
end;




procedure TImageCtrlFrm.FormResize(Sender: TObject);
begin
if (Self.ClientWidth) < (Self.ClientHeight-132) then
 begin
  Image.Width := Self.ClientWidth+8;
  Image.Height := Self.ClientWidth+8;
  Image.Picture.Bitmap.Width := Self.ClientWidth+8;
  Image.Picture.Bitmap.Height := Self.ClientWidth+8;
  ControlsPanel.Top := Self.ClientWidth+8;
  ControlsPanel.Width := Self.ClientWidth+8;
  if DispChA then StretchBlt(Image.Canvas.Handle, 0, 0, Image.Width, Image.Height,
                             ImageForm.ChanInfoA.OffScreen, 0, 0, SettingsInUse.Image.ImageWidth,
                             SettingsInUse.Image.ImageLength, SRCCOPY);
  if DispChB then StretchBlt(Image.Canvas.Handle, 0, 0, Image.Width, Image.Height,
                             ImageForm.ChanInfoB.OffScreen, 0, 0, SettingsInUse.Image.ImageWidth,
                             SettingsInUse.Image.ImageLength, SRCCOPY);
  if DispChC then StretchBlt(Image.Canvas.Handle, 0, 0, Image.Width, Image.Height,
                             ImageForm.ChanInfoC.OffScreen, 0, 0, SettingsInUse.Image.ImageWidth,
                             SettingsInUse.Image.ImageLength, SRCCOPY);
  DrawCenterCross;
  Image.Invalidate;
 end
else
 begin
  Image.Width := Self.ClientHeight-132;
  Image.Height := Self.ClientHeight-132;
  Image.Picture.Bitmap.Width := Self.ClientHeight-132;
  Image.Picture.Bitmap.Height := Self.ClientHeight-132;
  ControlsPanel.Top := Self.ClientHeight-132;
  ControlsPanel.Width := Self.ClientWidth+8;

  if DispChA then StretchBlt(Image.Canvas.Handle, 0, 0, Image.Width, Image.Height,
                             ImageForm.ChanInfoA.OffScreen, 0, 0, SettingsInUse.Image.ImageWidth,
                             SettingsInUse.Image.ImageLength, SRCCOPY);
  if DispChB then StretchBlt(Image.Canvas.Handle, 0, 0, Image.Width, Image.Height,
                             ImageForm.ChanInfoB.OffScreen, 0, 0, SettingsInUse.Image.ImageWidth,
                             SettingsInUse.Image.ImageLength, SRCCOPY);
  if DispChC then StretchBlt(Image.Canvas.Handle, 0, 0, Image.Width, Image.Height,
                             ImageForm.ChanInfoC.OffScreen, 0, 0, SettingsInUse.Image.ImageWidth,
                             SettingsInUse.Image.ImageLength, SRCCOPY);
  Image.Invalidate;
 end;

end;


end.
