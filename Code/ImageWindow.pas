unit ImageWindow;

interface

uses
      Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
      Dialogs, ExtCtrls, Qt, StdCtrls,
//  ******************************************************************
      TypesUnit, Preferences;
//  ******************************************************************
type


  TChannelInfo = class
    private
     BMPPtr: PRGB32Array;
     QPos,
     QTemp,
     QStat: Array[1..3] of Word;
     TempStat,
     LineStat: TDataArray;
     Image: TImage;
     Chan: string;
     procedure PrepBitmaps();
    public
     ImageArray: TDataArray;
     BMP: TBitmap;
     OffScreen: HDC;
     MaxVal,
     TempMaxVal: Word;
     ColorBuffer: TRGB32Array;
     procedure PerformStatsFull();
     procedure PerformStatsCurrent(LineNumber: Word);
     procedure SetQValues();
     procedure Display(LineNumber, IndexPoint: integer);
     procedure DisplayFull();
    published
     procedure ResetImages();
   end;


  TImageForm = class(TForm)
    ChannelAInfoLabel: TLabel;
    ChannelBInfoLabel: TLabel;
    ChannelCInfoLabel: TLabel;
    ChAImage: TImage;
    ChBImage: TImage;
    ChCImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DetailsMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
  private
    InitQT,
    TempQT: Double;
    BlendFunc: TBlendFunction;
    procedure PrepBitmaps();
    procedure Display(var Msg: TMessage); message WM_DISPLAY_IMAGE;
    procedure FrameComplete(var Msg: TMessage); message WM_FRM_COMPLT;
    procedure GetQuartiles(const SortedX: array of Word; var Q1, Q3: Word);
    function GetMedian(const SortedX: array of Word): Word;
    function ArraySort(const Numbers: array of Word; Array_Size: Cardinal; MaxVal: Word): TDataArray;
  public
    AutoScaleRequested: Boolean;
    ChanInfoA,
    ChanInfoB,
    ChanInfoC: TChannelInfo;
    Ras_DisplayCode: Byte; //  code for the choice of channels to display
    procedure ResetImages();
    procedure GuessMaxVal();
    procedure SetWindows(ChA, ChB, ChC: Boolean);
  end;


var
  ImageForm: TImageForm;

implementation
//  ***********************************************************************************************************
uses HardwareFunctions, Main, FilmStripForm, ImageControl, MiscFunctions;
//  ***********************************************************************************************************
{$R *.dfm}

procedure TImageForm.FormCreate(Sender: TObject);
var
i: integer;
Temp: Word;
clTextFile: TextFile;
ReadStr: string;
TempArray: array of Extended;
begin
SetLength(TempArray,3);

AssignFile(clTextFile,'HardwareData\Blue.txt');
Reset(clTextFile);
for i := 0 to 255 do
 begin
  ReadLn(clTextFile,ReadStr);
  _GetValue(TempArray,ReadStr,3);
  BlueRGBArray[i].R := Round(TempArray[0]);
  BlueRGBArray[i].G := Round(TempArray[1]);
  BlueRGBArray[i].B := Round(TempArray[2]);
 end;
CloseFile(clTextFile);
AssignFile(clTextFile,'HardwareData\Cyan.txt');
Reset(clTextFile);
for i := 0 to 255 do
 begin
  ReadLn(clTextFile,ReadStr);
  _GetValue(TempArray,ReadStr,3);
  CyanRGBArray[i].R := Round(TempArray[0]);
  CyanRGBArray[i].G := Round(TempArray[1]);
  CyanRGBArray[i].B := Round(TempArray[2]);
 end;
CloseFile(clTextFile);
AssignFile(clTextFile,'HardwareData\Green.txt');
Reset(clTextFile);
for i := 0 to 255 do
 begin
  ReadLn(clTextFile,ReadStr);
  _GetValue(TempArray,ReadStr,3);
  GreenRGBArray[i].R := Round(TempArray[0]);
  GreenRGBArray[i].G := Round(TempArray[1]);
  GreenRGBArray[i].B := Round(TempArray[2]);
 end;
CloseFile(clTextFile);
AssignFile(clTextFile,'HardwareData\Red.txt');
Reset(clTextFile);
for i := 0 to 255 do
 begin
  ReadLn(clTextFile,ReadStr);
  _GetValue(TempArray,ReadStr,3);
  RedRGBArray[i].R := Round(TempArray[0]);
  RedRGBArray[i].G := Round(TempArray[1]);
  RedRGBArray[i].B := Round(TempArray[2]);
 end;
CloseFile(clTextFile);
AssignFile(clTextFile,'HardwareData\Yellow.txt');
Reset(clTextFile);
for i := 0 to 255 do
 begin
  ReadLn(clTextFile,ReadStr);
  _GetValue(TempArray,ReadStr,3);
  YellowRGBArray[i].R := Round(TempArray[0]);
  YellowRGBArray[i].G := Round(TempArray[1]);
  YellowRGBArray[i].B := Round(TempArray[2]);
 end;
CloseFile(clTextFile);
AssignFile(clTextFile,'HardwareData\YellowHot.txt');
Reset(clTextFile);
for i := 0 to 255 do
 begin
  ReadLn(clTextFile,ReadStr);
  _GetValue(TempArray,ReadStr,3);
  YellowHotRGBArray[i].R := Round(TempArray[0]);
  YellowHotRGBArray[i].G := Round(TempArray[1]);
  YellowRGBArray[i].B := Round(TempArray[2]);
 end;
CloseFile(clTextFile);
Finalize(TempArray);

Self.WindowState := wsMinimized;
Self.Left := 385;
Self.Top := 135;
Self.Color := PaletteRGB(25,25,30);
ChannelAInfoLabel.Color := PaletteRGB(25,25,30);
ChannelBInfoLabel.Color := PaletteRGB(25,25,30);
ChannelCInfoLabel.Color := PaletteRGB(25,25,30);

ChanInfoA := TChannelInfo.Create;
ChanInfoB := TChannelInfo.Create;
ChanInfoC := TChannelInfo.Create;

ChanInfoA.Chan := 'A';
ChanInfoB.Chan := 'B';
ChanInfoC.Chan := 'C';

ChanInfoA.TempMaxVal := 0;
ChanInfoB.TempMaxVal := 0;
ChanInfoC.TempMaxVal := 0;

ChanInfoA.OffScreen := CreateCompatibleDC(0);
ChanInfoB.OffScreen := CreateCompatibleDC(0);
ChanInfoC.OffScreen := CreateCompatibleDC(0);

New(ChanInfoA.BMPPtr);
New(ChanInfoB.BMPPtr);
New(ChanInfoC.BMPPtr);

HandleNeeded;

Self.WindowState := wsNormal;
end;



procedure TImageForm.ResetImages;
begin
ImageCtrlFrm.ChannelRadioGroup.Enabled := True;

ChanInfoA.BMP := TBitmap.Create;
ChanInfoB.BMP := TBitmap.Create;
ChanInfoC.BMP := TBitmap.Create;

ChanInfoA.BMP.PixelFormat := pf32bit;
ChanInfoB.BMP.PixelFormat := pf32bit;
ChanInfoC.BMP.PixelFormat := pf32bit;

ChanInfoA.BMP.Width := SettingsInUse.Image.ImageWidth;
ChanInfoB.BMP.Width := SettingsInUse.Image.ImageWidth;
ChanInfoC.BMP.Width := SettingsInUse.Image.ImageWidth;

ChanInfoA.BMP.Height := SettingsInUse.Image.ImageLength+3;
ChanInfoB.BMP.Height := SettingsInUse.Image.ImageLength+3;
ChanInfoC.BMP.Height := SettingsInUse.Image.ImageLength+3;

SetLength(ChanInfoA.TempStat,SettingsInUse.PhotonBufCount);
SetLength(ChanInfoB.TempStat,SettingsInUse.PhotonBufCount);
SetLength(ChanInfoC.TempStat,SettingsInUse.PhotonBufCount);

SetLength(ChanInfoA.LineStat,SettingsInUse.PhotonBufCount);
SetLength(ChanInfoB.LineStat,SettingsInUse.PhotonBufCount);
SetLength(ChanInfoC.LineStat,SettingsInUse.PhotonBufCount);

SetLength(ChanInfoA.ImageArray,SettingsInUse.PhotonBufCount);
SetLength(ChanInfoB.ImageArray,SettingsInUse.PhotonBufCount);
SetLength(ChanInfoC.ImageArray,SettingsInUse.PhotonBufCount);


SetStretchBltMode(ChAImage.Canvas.Handle, PrefFrm.StretchBltMode);
SetStretchBltMode(ChBImage.Canvas.Handle, PrefFrm.StretchBltMode);
SetStretchBltMode(ChCImage.Canvas.Handle, PrefFrm.StretchBltMode);
if PrefFrm.StretchBltMode = 4 then
 begin
  SetBrushOrgEx(ChAImage.Canvas.Handle,0,0,nil);
  SetBrushOrgEx(ChBImage.Canvas.Handle,0,0,nil);
  SetBrushOrgEx(ChCImage.Canvas.Handle,0,0,nil);
 end;



PrepBitmaps;
SelectObject(ChanInfoB.OffScreen, ChanInfoB.BMP.Handle);
StretchBlt(ChBImage.Canvas.Handle, 0, 0, ChBImage.Width, ChBImage.Height, ChanInfoB.OffScreen, 0, 0
           , SettingsInUse.Image.ImageWidth, SettingsInUse.Image.ImageLength, SRCCOPY);
SelectObject(ChanInfoB.OffScreen, ChanInfoB.BMP.Handle);
StretchBlt(ChBImage.Canvas.Handle, 0, 0, ChBImage.Width, ChBImage.Height, ChanInfoB.OffScreen, 0, 0
           , SettingsInUse.Image.ImageWidth, SettingsInUse.Image.ImageLength, SRCCOPY);
SelectObject(ChanInfoC.OffScreen, ChanInfoC.BMP.Handle);
StretchBlt(ChCImage.Canvas.Handle, 0, 0, ChCImage.Width, ChCImage.Height, ChanInfoC.OffScreen, 0, 0
           , SettingsInUse.Image.ImageWidth, SettingsInUse.Image.ImageLength, SRCCOPY);
ChAImage.Invalidate;
ChBImage.Invalidate;
ChCImage.Invalidate;
end;


procedure TImageForm.PrepBitmaps;
var i,j:integer;
begin
for i := 0 to SettingsInUse.Image.ImageLength - 1 do
 begin
  ChanInfoA.BMPPtr := ChanInfoA.BMP.ScanLine[i];
  ChanInfoB.BMPPtr := ChanInfoB.BMP.ScanLine[i];
  ChanInfoC.BMPPtr := ChanInfoC.BMP.ScanLine[i];
   for j := 0 to SettingsInUse.Image.ImageWidth - 1 do
    begin
     ChanInfoA.BMPPtr^[j].R := 0;
     ChanInfoA.BMPPtr^[j].G := 0;
     ChanInfoA.BMPPtr^[j].B := 0;
     ChanInfoA.BMPPtr^[j].A := 0;
     ChanInfoB.BMPPtr^[j].R := 0;
     ChanInfoB.BMPPtr^[j].G := 0;
     ChanInfoB.BMPPtr^[j].B := 0;
     ChanInfoB.BMPPtr^[j].A := 0;
     ChanInfoC.BMPPtr^[j].R := 0;
     ChanInfoC.BMPPtr^[j].G := 0;
     ChanInfoC.BMPPtr^[j].B := 0;
     ChanInfoC.BMPPtr^[j].A := 0;
    end;
 end;
end;



procedure TImageForm.SetWindows(ChA, ChB, ChC: Boolean);
var
i, j: integer;
TempAPtr, TempBPtr, TempCPtr: PRGB32Array;
begin
if (ChA = False) AND (ChB = False) AND (ChC = False) then Ras_DisplayCode := 0;  // None
if (ChA = True) AND (ChB = False) AND (ChC = False)  then Ras_DisplayCode := 1;  // Just A
if (ChA = False) AND (ChB = True) AND (ChC = False)  then Ras_DisplayCode := 2;  // Just B
if (ChA = False) AND (ChB = False) AND (ChC = True)  then Ras_DisplayCode := 3;  // Just C
if (ChA = True) AND (ChB = True) AND (ChC = False)   then Ras_DisplayCode := 4;  // A & B
if (ChA = True) AND (ChB = False) AND (ChC = True)   then Ras_DisplayCode := 5;  // A & C
if (ChA = False) AND (ChB = True) AND (ChC = True)   then Ras_DisplayCode := 6;  // B & C
if (ChA = True) AND (ChB = True) AND (ChC = True)    then Ras_DisplayCode := 7;  // All

case Ras_DisplayCode of
 0: begin
     ChannelAInfoLabel.Hide;  ChannelBInfoLabel.Hide;  ChannelCInfoLabel.Hide;
     ChAImage.Hide;  ChBImage.Hide;   ChCImage.Hide;
     Self.Width := 300;                   Self.Height := 300;
     ChAImage.Picture := nil;
     ChBImage.Picture := nil;
     ChCImage.Picture := nil;
    end;
 1: begin
     ChannelAInfoLabel.Show;  ChannelBInfoLabel.Hide;  ChannelCInfoLabel.Hide;
     ChannelAInfoLabel.Top := 8;         ChannelAInfoLabel.Left := 8;
     ChAImage.Show;  ChBImage.Hide;   ChCImage.Hide;
     ChannelAInfoLabel.Top := 8;
     ChannelAInfoLabel.Left := 8;
     ChAImage.Left := 8;
     ChAImage.Top := 32;
     ChAImage.Width := Ras_OneChannelDimension;
     ChAImage.Height := Ras_OneChannelDimension;
     Self.Height := Ras_OneChannelDimension + 88;
     Self.Width := Ras_OneChannelDimension + 28;
     ChAImage.Picture := nil;
     ChBImage.Picture := nil;
     ChCImage.Picture := nil;
    end;
 2: begin
     ChannelAInfoLabel.Hide;  ChannelBInfoLabel.Show;  ChannelCInfoLabel.Hide;
     ChannelBInfoLabel.Top := 8;         ChannelBInfoLabel.Left := 8;
     ChAImage.Hide;  ChBImage.Show;   ChCImage.Hide;
     ChannelBInfoLabel.Top := 8;
     ChannelBInfoLabel.Left := 8;
     ChBImage.Left := 8;
     ChBImage.Top := 32;
     ChBImage.Width := Ras_OneChannelDimension;
     ChBImage.Height := Ras_OneChannelDimension;
     Self.Height := Ras_OneChannelDimension + 88;
     Self.Width := Ras_OneChannelDimension + 28;
     ChAImage.Picture := nil;
     ChBImage.Picture := nil;
     ChCImage.Picture := nil;
    end;
 3: begin
     ChannelAInfoLabel.Hide;  ChannelBInfoLabel.Hide;  ChannelCInfoLabel.Show;
     ChannelCInfoLabel.Top := 8;         ChannelCInfoLabel.Left := 8;
     ChAImage.Hide;  ChBImage.Hide;   ChCImage.Show;
     ChannelCInfoLabel.Top := 8;
     ChannelCInfoLabel.Left := 8;
     ChCImage.Left := 8;
     ChCImage.Top := 32;
     ChCImage.Width := Ras_OneChannelDimension;
     ChCImage.Height := Ras_OneChannelDimension;
     Self.Height := Ras_OneChannelDimension + 88;
     Self.Width := Ras_OneChannelDimension + 28;
     ChAImage.Picture := nil;
     ChBImage.Picture := nil;
     ChCImage.Picture := nil;
    end;
 4: begin
     ChannelAInfoLabel.Show;  ChannelBInfoLabel.Show;  ChannelCInfoLabel.Hide;
     ChannelAInfoLabel.Top := 8;         ChannelAInfoLabel.Left := 8;
     ChannelBInfoLabel.Top := 8;         ChannelBInfoLabel.Left := Ras_TwoChannelDimension + 16;
     ChAImage.Show;  ChBImage.Show;   ChCImage.Hide;
     ChannelAInfoLabel.Top := 8;
     ChannelAInfoLabel.Left := 8;
     ChannelBInfoLabel.Top := 8;
     ChannelBInfoLabel.Left := Ras_TwoChannelDimension + 16;
     ChAImage.Left := 8;
     ChAImage.Top := 32;
     ChBImage.Left := Ras_TwoChannelDimension + 16;
     ChBImage.Top := 32;
     ChAImage.Width := Ras_TwoChannelDimension;
     ChAImage.Height := Ras_TwoChannelDimension;
     ChBImage.Width := Ras_TwoChannelDimension;
     ChBImage.Height := Ras_TwoChannelDimension;
     Self.Height := Ras_TwoChannelDimension+100;
     Self.Width := 2*Ras_TwoChannelDimension+32;
     ChAImage.Picture := nil;
     ChBImage.Picture := nil;
     ChCImage.Picture := nil;
    end;
 5: begin
     ChannelAInfoLabel.Show;  ChannelBInfoLabel.Hide;  ChannelCInfoLabel.Show;
     ChannelAInfoLabel.Top := 8;         ChannelAInfoLabel.Left := 8;
     ChannelCInfoLabel.Top := 8;         ChannelCInfoLabel.Left := Ras_TwoChannelDimension + 16;
     ChAImage.Show;  ChBImage.Hide;   ChCImage.Show;
     ChannelAInfoLabel.Top := 8;
     ChannelAInfoLabel.Left := 8;
     ChannelCInfoLabel.Top := 8;
     ChannelCInfoLabel.Left := Ras_TwoChannelDimension + 16;
     ChAImage.Left := 8;
     ChAImage.Top := 32;
     ChCImage.Left := Ras_TwoChannelDimension + 16;
     ChCImage.Top := 32;
     ChAImage.Width := Ras_TwoChannelDimension;
     ChAImage.Height := Ras_TwoChannelDimension;
     ChCImage.Width := Ras_TwoChannelDimension;
     ChCImage.Height := Ras_TwoChannelDimension;
     Self.Height := Ras_TwoChannelDimension+100;
     Self.Width := 2*Ras_TwoChannelDimension+32;
     ChAImage.Picture := nil;
     ChBImage.Picture := nil;
     ChCImage.Picture := nil;
   end;
 6: begin
     ChannelAInfoLabel.Hide;  ChannelBInfoLabel.Show;  ChannelCInfoLabel.Show;
     ChannelBInfoLabel.Top := 8;         ChannelBInfoLabel.Left := 8;
     ChannelCInfoLabel.Top := 8;         ChannelCInfoLabel.Left := Ras_TwoChannelDimension + 16;
     ChAImage.Hide;  ChBImage.Show;   ChCImage.Show;
     ChannelBInfoLabel.Top := 8;
     ChannelBInfoLabel.Left := 8;
     ChannelCInfoLabel.Top := 8;
     ChannelCInfoLabel.Left := Ras_TwoChannelDimension + 16;
     ChBImage.Left := 8;
     ChBImage.Top := 32;
     ChCImage.Left := Ras_TwoChannelDimension + 16;
     ChCImage.Top := 32;
     ChBImage.Width := Ras_TwoChannelDimension;
     ChBImage.Height := Ras_TwoChannelDimension;
     ChCImage.Width := Ras_TwoChannelDimension;
     ChCImage.Height := Ras_TwoChannelDimension;
     Self.Height := Ras_TwoChannelDimension+100;
     Self.Width := 2*Ras_TwoChannelDimension+32;
     ChAImage.Picture := nil;
     ChBImage.Picture := nil;
     ChCImage.Picture := nil;
    end;
 7: begin
     ChannelAInfoLabel.Show;  ChannelBInfoLabel.Show;  ChannelCInfoLabel.Show;
     ChannelAInfoLabel.Top := 8;         ChannelAInfoLabel.Left := 8;
     ChannelBInfoLabel.Top := 8;         ChannelBInfoLabel.Left := Ras_ThreeChannelDimension + 8;
     ChannelCInfoLabel.Top := 8;         ChannelCInfoLabel.Left := 2*Ras_ThreeChannelDimension + 16;
     ChAImage.Show;  ChBImage.Show;   ChCImage.Show;
     ChannelAInfoLabel.Top := 8;
     ChannelAInfoLabel.Left := 8;
     ChannelBInfoLabel.Top := 8;
     ChannelBInfoLabel.Left := Ras_ThreeChannelDimension + 16;
     ChannelCInfoLabel.Top := 8;
     ChannelCInfoLabel.Left := 2*Ras_ThreeChannelDimension + 24;
     ChAImage.Left := 8;
     ChAImage.Top := 32;
     ChBImage.Left := Ras_ThreeChannelDimension + 16;
     ChBImage.Top := 32;
     ChCImage.Left := 2*Ras_ThreeChannelDimension + 24;
     ChCImage.Top := 32;
     ChAImage.Width := Ras_ThreeChannelDimension;
     ChAImage.Height := Ras_ThreeChannelDimension;
     ChBImage.Width := Ras_ThreeChannelDimension;
     ChBImage.Height := Ras_ThreeChannelDimension;
     ChCImage.Width := Ras_ThreeChannelDimension;
     ChCImage.Height := Ras_ThreeChannelDimension;
     Self.Height := Ras_ThreeChannelDimension + 100;
     Self.Width := 3*Ras_ThreeChannelDimension + 39;
     ChAImage.Picture := nil;
     ChBImage.Picture := nil;
     ChCImage.Picture := nil;
   end;
end;
ChAImage.Invalidate;
ChBImage.Invalidate;
ChCImage.Invalidate;
end;



procedure TImageForm.FormResize(Sender: TObject);
begin
//DisplayCode  0;  // None
//DisplayCode  1;  // Just A
//DisplayCode  2;  // Just B
//DisplayCode  3;  // Just C
//DisplayCode  4;  // A & B
//DisplayCode  5;  // A & C
//DisplayCode  6;  // B & C
//DisplayCode  7;  // All
if NOT Ran then Exit;
case Ras_DisplayCode of
 0: ;//
 1: begin
     if (ClientWidth -8) < (ClientHeight-32) then
      begin
       ChAImage.Width := ClientWidth-16;
       ChAImage.Height := ClientWidth-16;
       ChAImage.Picture.Bitmap.Width := ClientWidth-16;
       ChAImage.Picture.Bitmap.Height := ClientWidth-16;
       StretchBlt(ChAImage.Canvas.Handle,0,0,ChAImage.Width,ChAImage.Height,
           ChanInfoA.OffScreen,0,0,ChanInfoA.BMP.Width,ChanInfoA.BMP.Height-3,SRCCOPY);
       ChAImage.Invalidate;
      end
     else
      begin
       ChAImage.Width := ClientHeight-40;
       ChAImage.Height := ClientHeight-40;
       ChAImage.Picture.Bitmap.Width := ClientHeight-40;
       ChAImage.Picture.Bitmap.Height := ClientHeight-40;
       StretchBlt(ChAImage.Canvas.Handle,0,0,ChAImage.Width,ChAImage.Height,
           ChanInfoA.OffScreen,0,0,ChanInfoA.BMP.Width,ChanInfoA.BMP.Height-3,SRCCOPY);
       ChAImage.Invalidate;
      end;
    end;
 2: begin
     if (ClientWidth -8) < (ClientHeight-32) then
      begin
       ChBImage.Width := ClientWidth-16;
       ChBImage.Height := ClientWidth-16;
       ChBImage.Picture.Bitmap.Width := ClientWidth-16;
       ChBImage.Picture.Bitmap.Height := ClientWidth-16;
       StretchBlt(ChBImage.Canvas.Handle,0,0,ChBImage.Width,ChBImage.Height,
           ChanInfoB.OffScreen,0,0,ChanInfoB.BMP.Width,ChanInfoB.BMP.Height-3,SRCCOPY);
       ChBImage.Invalidate;
      end
     else
      begin
       ChBImage.Width := ClientHeight-40;
       ChBImage.Height := ClientHeight-40;
       ChBImage.Picture.Bitmap.Width := ClientHeight-40;
       ChBImage.Picture.Bitmap.Height := ClientHeight-40;
       StretchBlt(ChBImage.Canvas.Handle,0,0,ChBImage.Width,ChBImage.Height,
           ChanInfoB.OffScreen,0,0,ChanInfoB.BMP.Width,ChanInfoB.BMP.Height-3,SRCCOPY);
       ChBImage.Invalidate;
      end;
    end;
 3: begin
     if (ClientWidth -8) < (ClientHeight-32) then
      begin
       ChCImage.Width := ClientWidth-16;
       ChCImage.Height := ClientWidth-16;
       ChCImage.Picture.Bitmap.Width := ClientWidth-16;
       ChCImage.Picture.Bitmap.Height := ClientWidth-16;
       StretchBlt(ChCImage.Canvas.Handle,0,0,ChCImage.Width,ChCImage.Height,
           ChanInfoC.OffScreen,0,0,ChanInfoC.BMP.Width,ChanInfoC.BMP.Height-3,SRCCOPY);
       ChCImage.Invalidate;
      end
     else
      begin
       ChCImage.Width := ClientHeight-40;
       ChCImage.Height := ClientHeight-40;
       ChCImage.Picture.Bitmap.Width := ClientHeight-40;
       ChCImage.Picture.Bitmap.Height := ClientHeight-40;
       StretchBlt(ChCImage.Canvas.Handle,0,0,ChCImage.Width,ChCImage.Height,
           ChanInfoC.OffScreen,0,0,ChanInfoC.BMP.Width,ChanInfoC.BMP.Height-3,SRCCOPY);
       ChCImage.Invalidate;
      end;
    end;
 end;
end;




procedure TImageForm.GuessMaxVal;
var
Temp, n: integer;
begin
Temp := Round(SettingsInUse.Image.PixelPulseTimeMicroSec * 0.00001 * EstimatePhotonFreq);
ChanInfoA.MaxVal := Temp;
ChanInfoB.MaxVal := Temp;
ChanInfoC.MaxVal := Temp;
ChanInfoA.TempMaxVal := Temp;
ChanInfoB.TempMaxVal := Temp;
ChanInfoC.TempMaxVal := Temp;
for n := 1 to 3 do
 begin
  ChanInfoA.QPos[n] := Round(n * Temp * 0.25);
  ChanInfoB.QPos[n] := Round(n * Temp * 0.25);
  ChanInfoC.QPos[n] := Round(n * Temp * 0.25);
 end;
MainForm.DetailsMemo.Lines.Add('EstimatedMax: ChA-' + IntToStr(ChanInfoA.MaxVal)
                                 + ' ChB-' + IntToStr(ChanInfoB.MaxVal)
                                 + ' ChC-' + IntToStr(ChanInfoC.MaxVal));
end;


 
procedure TImageForm.Display(var Msg: TMessage);
var
DistanceDown,
IndexPoint, x ,y: integer;
begin
//beep;
DistanceDown := Msg.WParam;
IndexPoint := Msg.LParam;
// ************************************************************************************************   /\
// ************************************************************************************************  /__\
// ************************************************************************************************ /    \
if SettingsInUse.Image.ChannelASelected then
 begin
  if AutoScaleRequested then
   begin
    ChanInfoA.PerformStatsCurrent(DistanceDown);
    ChanInfoA.SetQValues;
    ChannelAInfoLabel.Caption := 'Channel A  MaxCountRate: ' +
           FormatFloat('0.###',(ChanInfoA.TempMaxVal/SettingsInUse.Image.PixelPulseTimeMicroSec)*1000) + 'kHz';
    if PrefFrm.ReScaleFullImg then ChanInfoA.DisplayFull;
   end;
  ChanInfoA.Display(DistanceDown,IndexPoint);
  SelectObject(ChanInfoA.OffScreen, ChanInfoA.BMP.Handle);
  StretchBlt(ChAImage.Canvas.Handle, 0, 0, ChAImage.Width, ChAImage.Height, ChanInfoA.OffScreen, 0, 0
             , SettingsInUse.Image.ImageWidth, SettingsInUse.Image.ImageLength, SRCCOPY);
 end;
// *************************************************************************************************  |'''\
// *************************************************************************************************  |---/
// *************************************************************************************************  |,,,\
if SettingsInUse.Image.ChannelBSelected then
 begin
  if AutoScaleRequested then
   begin
    ChanInfoB.PerformStatsCurrent(DistanceDown);
    ChanInfoB.SetQValues;
    ChannelBInfoLabel.Caption := 'Channel B  MaxCountRate: ' +
           FormatFloat('0.###',(ChanInfoB.TempMaxVal/SettingsInUse.Image.PixelPulseTimeMicroSec)*1000) + 'kHz';
    if PrefFrm.ReScaleFullImg then ChanInfoB.DisplayFull;
   end;
  ChanInfoB.Display(DistanceDown,IndexPoint);
  SelectObject(ChanInfoB.OffScreen, ChanInfoB.BMP.Handle);
  StretchBlt(ChBImage.Canvas.Handle, 0, 0, ChBImage.Width, ChBImage.Height, ChanInfoB.OffScreen, 0, 0
             , SettingsInUse.Image.ImageWidth, SettingsInUse.Image.ImageLength, SRCCOPY);
 end;
// *************************************************************************************************  |'''
// *************************************************************************************************  |
// *************************************************************************************************  |,,,
if SettingsInUse.Image.ChannelCSelected then
 begin
  if AutoScaleRequested then
   begin
    ChanInfoC.PerformStatsCurrent(DistanceDown);
    ChanInfoC.SetQValues;
    ChannelCInfoLabel.Caption := 'Channel C  MaxCountRate: ' +
           FormatFloat('0.###',(ChanInfoC.TempMaxVal/SettingsInUse.Image.PixelPulseTimeMicroSec)*1000) + 'kHz';
    if PrefFrm.ReScaleFullImg then ChanInfoB.DisplayFull;
   end;
  ChanInfoC.Display(DistanceDown,IndexPoint);
  SelectObject(ChanInfoC.OffScreen, ChanInfoC.BMP.Handle);
  StretchBlt(ChCImage.Canvas.Handle, 0, 0, ChCImage.Width, ChCImage.Height, ChanInfoC.OffScreen, 0, 0
             , SettingsInUse.Image.ImageWidth, SettingsInUse.Image.ImageLength, SRCCOPY);
 end;
// *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
AutoScaleRequested := False;
if SettingsInUse.Image.ChannelASelected then ChAImage.Invalidate;
if SettingsInUse.Image.ChannelBSelected then ChBImage.Invalidate;
if SettingsInUse.Image.ChannelCSelected then ChCImage.Invalidate;

end;


procedure TImageForm.FrameComplete(var Msg: TMessage);
var
DimensionStr: string;
ImageNumber: Word;
begin
DimensionStr := FormatFloat('###.##',SettingsInUse.Image.ImagePhysWidth) + Chr(Key_mu) +  'm x ' +
                FormatFloat('###.##',SettingsInUse.Image.ImagePhysLength) + Chr(Key_mu) + 'm';
// ************************************************************************************************   /\
// ************************************************************************************************  /__\
// ************************************************************************************************ /    \
if SettingsInUse.Image.ChannelASelected then
 begin
  ChanInfoA.PerformStatsFull;
  ChanInfoA.SetQValues;
  ChannelAInfoLabel.Caption := 'Channel A  MaxCountRate: ' +
        FormatFloat('0.###',(ChanInfoA.TempMaxVal/SettingsInUse.Image.PixelPulseTimeMicroSec)*1000) + 'kHz';
  SelectObject(ChanInfoA.OffScreen, ChanInfoA.BMP.Handle);
  ImageNumber := FilmStrForm.NewImage(False,'A', SettingsInUse.Image.HintString + 'ChA', DimensionStr);
  StretchBlt(FilmStrForm.ThumbArray[ImageNumber].Canvas.Handle, 0, 0, 89, 89, ChanInfoA.OffScreen, 0, 0
             , ChanInfoA.BMP.Width, SettingsInUse.Image.ImageLength, SRCCOPY);
  if ImageCtrlFrm.DispChA then
  StretchBlt(ImageCtrlFrm.Image.Canvas.Handle, 0, 0, ImageCtrlFrm.Image.Width, ImageCtrlFrm.Image.Height,
             ChanInfoA.OffScreen, 0, 0, ChanInfoA.BMP.Width, SettingsInUse.Image.ImageLength, SRCCOPY);
  ImageCtrlFrm.Image.Invalidate;
  FilmStrForm.ThumbArray[ImageNumber].Invalidate;
 end;
// *************************************************************************************************  |'''\
// *************************************************************************************************  |---/
// *************************************************************************************************  |,,,\
if SettingsInUse.Image.ChannelBSelected then
 begin
  ChanInfoB.PerformStatsFull;
  ChanInfoB.SetQValues;
  ChannelBInfoLabel.Caption := 'Channel B  MaxCountRate: ' +
        FormatFloat('0.###',(ChanInfoB.TempMaxVal/SettingsInUse.Image.PixelPulseTimeMicroSec)*1000) + 'kHz';
  SelectObject(ChanInfoB.OffScreen, ChanInfoB.BMP.Handle);
  ImageNumber := FilmStrForm.NewImage(False,'B', SettingsInUse.Image.HintString + 'ChB', DimensionStr);
  StretchBlt(FilmStrForm.ThumbArray[ImageNumber].Canvas.Handle, 0, 0, 89, 89, ChanInfoB.OffScreen, 0, 0
             , ChanInfoB.BMP.Width, SettingsInUse.Image.ImageLength, SRCCOPY);
  if ImageCtrlFrm.DispChB then
  StretchBlt(ImageCtrlFrm.Image.Canvas.Handle, 0, 0, ImageCtrlFrm.Image.Width, ImageCtrlFrm.Image.Height,
             ChanInfoB.OffScreen, 0, 0, ChanInfoB.BMP.Width, SettingsInUse.Image.ImageLength, SRCCOPY);
  FilmStrForm.ThumbArray[ImageNumber].Invalidate;
 end;
// *************************************************************************************************  |'''
// *************************************************************************************************  |
// *************************************************************************************************  |,,,
if SettingsInUse.Image.ChannelCSelected then
 begin
  ChanInfoC.PerformStatsFull;
  ChanInfoC.SetQValues;
  ChannelCInfoLabel.Caption := 'Channel C  MaxCountRate: ' +
        FormatFloat('0.###',(ChanInfoC.TempMaxVal/SettingsInUse.Image.PixelPulseTimeMicroSec)*1000) + 'kHz';
  SelectObject(ChanInfoC.OffScreen, ChanInfoC.BMP.Handle);
  ImageNumber := FilmStrForm.NewImage(False,'C', SettingsInUse.Image.HintString + 'ChC', DimensionStr);
  StretchBlt(FilmStrForm.ThumbArray[ImageNumber].Canvas.Handle, 0, 0, 89, 89, ChanInfoC.OffScreen, 0, 0
             , ChanInfoC.BMP.Width, SettingsInUse.Image.ImageLength, SRCCOPY);
  if ImageCtrlFrm.DispChC then
  StretchBlt(ImageCtrlFrm.Image.Canvas.Handle, 0, 0, ImageCtrlFrm.Image.Width, ImageCtrlFrm.Image.Height,
             ChanInfoC.OffScreen, 0, 0, ChanInfoC.BMP.Width, SettingsInUse.Image.ImageLength, SRCCOPY);
  FilmStrForm.ThumbArray[ImageNumber].Invalidate;
 end;
//GuessMaxVal;
 // *******************************************************************************************************
// *******************************************************************************************************
// *******************************************************************************************************
ImageCtrlFrm.DrawCenterCross;
end;

procedure TImageForm.DetailsMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
if (Key = VK_ESCAPE) then CancelAcq := True;
if (Key = VK_SPACE) then AutoScaleRequested := True;
end;

procedure TImageForm.FormDestroy(Sender: TObject);
begin
ChanInfoA.BMPPtr := nil;
ChanInfoB.BMPPtr := nil;
ChanInfoC.BMPPtr := nil;

ChanInfoA.BMP.Free;
ChanInfoB.BMP.Free;
ChanInfoC.BMP.Free;

DeleteDC(ChanInfoA.OffScreen);
DeleteDC(ChanInfoB.OffScreen);
DeleteDC(ChanInfoC.OffScreen);

ChanInfoA.Free;
ChanInfoB.Free;
ChanInfoC.Free;
end;


procedure TImageForm.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
if Acquiring then
 begin
  Resize := False;
  Exit;
 end;
end;
// **************************************************************************************************************************
// **************************************************************************************************************************
// **************************************************************************************************************************
// **************************************************************************************************************************
// **************************************************************************************************************************
// **************************************************************************************************************************
// **************************************************************************************************************************
// **************************************************************************************************************************
// ***************************                                             Statistics



function TImageForm.ArraySort(const Numbers: array of Word;
                             Array_Size: Cardinal; MaxVal: Word): TDataArray;
var
i, j, index: Cardinal;
TempArray: array of Word;
begin
if MaxVal = 0 then MaxVal := 1;
index := 0;
SetLength(Result,Array_Size);
SetLength(TempArray,MaxVal);
for i := 0 to Array_Size-1 do
 begin
  if Numbers[i] > (MaxVal-1) then
  Inc(TempArray[MaxVal-1])
  else
  Inc(TempArray[Numbers[i]]);
 end;
for i := 0 to MaxVal-1 do
 begin
  if TempArray[i] <> 0 then
   begin
    for j := 1 to TempArray[i] do
     begin
      Result[index] := i;
      inc(index);
     end;
   end;
 end;
end;


function TImageForm.GetMedian(const SortedX: array of Word): Word;
var
N: Word;
begin
N := High(SortedX);
Result := SortedX[N div 2];
end;

procedure TImageForm.GetQuartiles(const SortedX: array of Word; var Q1, Q3: Word);
var
N: Word;
begin
N := High(SortedX);
Q1 := SortedX[(N div 4)];
Q3 := SortedX[((3 * N) div 4)];
end;


// **************************************************************************************************************************
// **************************************************************************************************************************
// **************************************************************************************************************************
// **************************************************************************************************************************
// **************************************************************************************************************************
// **************************************************************************************************************************
// **************************************************************************************************************************
// **************************************************************************************************************************
// *****************************************                       { TChannelInfo }


procedure TChannelInfo.Display(LineNumber, IndexPoint: integer);
var
x, y: integer;
ArrayValue, Temp: Word;
begin
if QPos[1] = 0 then QPos[1] := 1;
if (QPos[2]-QPos[1]) = 0 then inc(QPos[2],1);
if (QPos[3]-QPos[2]) = 0 then inc(QPos[3],1);
if (((MaxVal-QPos[3]) = 0) AND (MaxVal < 65535)) then inc(MaxVal,1);
if (((MaxVal-QPos[3]) = 0) AND (MaxVal = 65535)) then dec(MaxVal,1);

try
BMPPtr := BMP.ScanLine[LineNumber];
for x := 0 to SettingsInUse.Image.ImageWidth - 1 do
 begin
  ArrayValue := ImageArray[x + IndexPoint];
  if (ArrayValue <= QPos[1]) then Temp := Round(((ArrayValue/QPos[1])*QTemp[1]));
  if ( (ArrayValue > QPos[1]) and (ArrayValue <= QPos[2]) ) then
    Temp := Round(( ( ((ArrayValue-QPos[1])/(QPos[2]-QPos[1])) * (QTemp[2]-QTemp[1]) ) + QTemp[1]));
  if ( (ArrayValue > QPos[2]) and (ArrayValue <= QPos[3]) ) then
    Temp := Round(( ( ((ArrayValue-QPos[2])/(QPos[3]-QPos[2])) * (QTemp[3]-QTemp[2]) ) + QTemp[2]));
  if ( (ArrayValue > QPos[3]) and (ArrayValue <= MaxVal) ) then
    Temp := Round(( ( ((ArrayValue-QPos[3])/(MaxVal-QPos[3])) *(255-QTemp[3]) ) + QTemp[3]));
  //BMPPtr^[x].G := Temp; BMPPtr^[x].R := 0; BMPPtr^[x].B := 0;
  BMPPtr^[x].G := Self.ColorBuffer[Temp].G;
  BMPPtr^[x].R := Self.ColorBuffer[Temp].R;
  BMPPtr^[x].B := Self.ColorBuffer[Temp].B;
 end; // end for x loop
BMPPtr := BMP.ScanLine[LineNumber+1];
for x := 0 to SettingsInUse.Image.ImageWidth - 1 do
 begin
  BMPPtr^[x].B := 255; BMPPtr^[x].G := 255; BMPPtr^[x].R := 160;
 end;
except
MainForm.DetailsMemo.Lines.Add(
        'DisplayError on Channel A: y=' + IntToStr(LineNumber) + ' x=' + IntToStr(x));
end;
end;


procedure TChannelInfo.DisplayFull;
var
x, y: integer;
ArrayValue, Temp: Word;
begin
if QPos[1] = 0 then QPos[1] := 1;
if (QPos[2]-QPos[1]) = 0 then inc(QPos[2],1);
if (QPos[3]-QPos[2]) = 0 then inc(QPos[3],1);
if (((MaxVal-QPos[3]) = 0) AND (MaxVal < 65535)) then inc(MaxVal,1);
if (((MaxVal-QPos[3]) = 0) AND (MaxVal = 65535)) then dec(MaxVal,1);

try
for y := 0 to SettingsInUse.Image.ImageLength - 1 do
 begin
  BMPPtr := BMP.ScanLine[y];
  for x := 0 to SettingsInUse.Image.ImageWidth - 1 do
   begin
    ArrayValue := ImageArray[x + ( y * SettingsInUse.Image.ImageWidth)];
    if (ArrayValue <= QPos[1]) then Temp := Round(((ArrayValue/QPos[1])*QTemp[1]));
    if ( (ArrayValue > QPos[1]) and (ArrayValue <= QPos[2]) ) then
        Temp := Round(( ( ((ArrayValue-QPos[1])/(QPos[2]-QPos[1])) * (QTemp[2]-QTemp[1]) ) + QTemp[1]));
    if ( (ArrayValue > QPos[2]) and (ArrayValue <= QPos[3]) ) then
        Temp := Round(( ( ((ArrayValue-QPos[2])/(QPos[3]-QPos[2])) * (QTemp[3]-QTemp[2]) ) + QTemp[2]));
    if ( (ArrayValue > QPos[3]) and (ArrayValue <= MaxVal) ) then
        Temp := Round(( ( ((ArrayValue-QPos[3])/(MaxVal-QPos[3])) *(255-QTemp[3]) ) + QTemp[3]));
    //BMPPtr^[x].G := Temp; BMPPtr^[x].R := 0; BMPPtr^[x].B := 0;
    BMPPtr^[x].G := Self.ColorBuffer[Temp].G;
    BMPPtr^[x].R := Self.ColorBuffer[Temp].R;
    BMPPtr^[x].B := Self.ColorBuffer[Temp].B;
   end; // end for x loop
 end;
except
MainForm.DetailsMemo.Lines.Add(
        'DisplayError on Channel A: y=' + IntToStr(y) + ' x=' + IntToStr(x));
end;
end;


procedure TChannelInfo.PerformStatsCurrent(LineNumber: Word);
var
i, lstart, lfinish: integer;
begin
if LineNumber = 0 then Exit;
lstart := 0;
lfinish := (LineNumber*SettingsInUse.Image.ImageWidth)-1;
for i := lstart to lfinish do LineStat[i-lstart] := ImageArray[i];
//TempStat := ImageForm.ArraySort(LineStat,(LineNumber+1)*SettingsInUse.Image.ImageWidth,Self.TempMaxVal);
TempStat := ImageForm.ArraySort(LineStat,(LineNumber+1)*SettingsInUse.Image.ImageWidth,Length(TempStat)-1);
QStat[2] := ImageForm.GetMedian(TempStat);
ImageForm.GetQuartiles(TempStat,QStat[1],QStat[3]);
MaxVal := TempStat[Length(TempStat)-1];
//MaxVal := TempStat[Self.TempMaxVal-1];
MainForm.DetailsMemo.Lines.Add('MaxVal' + Self.Chan + ': ' + IntToStr(MaxVal) + ' (Q1,Q2,Q3) = ('
                   + IntToStr(QStat[1]) + ',' + IntToStr(QStat[2]) + ',' + IntToStr(QStat[3]) + ')');
end;

procedure TChannelInfo.PerformStatsFull;
var
arrsize: cardinal;
begin
//TempStat := ImageForm.ArraySort(ImageArray,SettingsInUse.PhotonBufCount,Self.TempMaxVal);
TempStat := ImageForm.ArraySort(ImageArray,SettingsInUse.PhotonBufCount,Length(TempStat)-1);
QStat[2] := ImageForm.GetMedian(TempStat);
ImageForm.GetQuartiles(TempStat,QStat[1],QStat[3]);
//MaxVal := TempStat[Self.TempMaxVal-1];
MaxVal := TempStat[Length(TempStat)-1];
MainForm.DetailsMemo.Lines.Add('MaxVal' + Self.Chan + ': ' + IntToStr(MaxVal) + ' (Q1,Q2,Q3) = ('
                   + IntToStr(QStat[1]) + ',' + IntToStr(QStat[2]) + ',' + IntToStr(QStat[3]) + ')');
end;


procedure TChannelInfo.SetQValues;
var
i :word;
QuartileConst: array[1..3] of Single;
begin
if MaxVal = 0 then MaxVal := 1;
for i := 1 to 3 do
 begin
  QuartileConst[i] := ( 1 - (QStat[i]/(i*MaxVal*0.25)) );
  QTemp[i] := (64*i) + Round((SettingsInUse.Image.QSettings[i]*QuartileConst[i]));
  QPos[i] := Round((i*MaxVal*0.25) - (SettingsInUse.Image.QShiftMagnitude*MagnAdjConst*QuartileConst[i]));
 end;
end;



procedure TChannelInfo.PrepBitmaps;
var i,j:integer;
begin
for i := 0 to SettingsInUse.Image.ImageLength - 1 do
 begin
  BMPPtr := Self.BMP.ScanLine[i];
   for j := 0 to SettingsInUse.Image.ImageWidth - 1 do
    begin
     BMPPtr^[j].R := 0;
     BMPPtr^[j].G := 0;
     BMPPtr^[j].B := 0;
     BMPPtr^[j].A := 0;
    end;
 end;
end;



procedure TChannelInfo.ResetImages;
begin
ImageCtrlFrm.ChannelRadioGroup.Enabled := True;
ImageForm.SetWindows(SettingsInUse.Image.ChannelASelected,SettingsInUse.Image.ChannelBSelected,SettingsInUse.Image.ChannelCSelected);

BMP := TBitmap.Create;
BMP.PixelFormat := pf32bit;
BMP.Width := SettingsInUse.Image.ImageWidth;
Image.Picture.Bitmap.Width := SettingsInUse.Image.ImageWidth;
BMP.Height := SettingsInUse.Image.ImageLength+3;
Image.Picture.Bitmap.Height := SettingsInUse.Image.ImageLength;
SetLength(TempStat,SettingsInUse.PhotonBufCount);
SetLength(LineStat,SettingsInUse.PhotonBufCount);
SetLength(ImageArray,SettingsInUse.PhotonBufCount);
SetStretchBltMode(Image.Canvas.Handle, PrefFrm.StretchBltMode);
SetBrushOrgEx(Image.Canvas.Handle,0,0,nil);

PrepBitmaps;
SelectObject(OffScreen, BMP.Handle);
StretchBlt(Image.Canvas.Handle, 0, 0, Image.Width, Image.Height, OffScreen, 0, 0
           , SettingsInUse.Image.ImageWidth, SettingsInUse.Image.ImageLength, SRCCOPY);
Image.Invalidate;

end;




end.
