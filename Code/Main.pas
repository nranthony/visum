unit main;
       
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdActns, ActnList, StdCtrls, ToolWin, ActnMan, ActnCtrls,
  ComCtrls, ImgList, jpeg, ExtCtrls, Inifiles, Qt, Math,
// ******************************************************************************
  TIFFControl, TypesUnit, ImageWindow, StatusCodes, Preferences, CountDispWindow;
// ******************************************************************************
type

  TAcquireThread = class(TThread)
  private
  protected
    procedure Execute; override;
  end;


  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    PrintSetup1: TMenuItem;
    Print1: TMenuItem;
    N2: TMenuItem;
    SaveAs1: TMenuItem;
    Save1: TMenuItem;
    Open1: TMenuItem;
    New1: TMenuItem;
    ActionList1: TActionList;
    WindowCascade1: TWindowCascade;
    WindowTileHorizontal1: TWindowTileHorizontal;
    WindowTileVertical1: TWindowTileVertical;
    WindowMinimizeAll1: TWindowMinimizeAll;
    WindowArrange1: TWindowArrange;
    FileSaveAs1: TFileSaveAs;
    FileOpen1: TFileOpen;
    RasterImage1: TMenuItem;
    FCS1: TMenuItem;
    ScanningFCS1: TMenuItem;
    Spectrum1: TMenuItem;
    PCH1: TMenuItem;
    Window1: TMenuItem;
    ActiveWindowHandleLabel: TLabel;
    WindowCounterLabel: TLabel;
    ToolBarImageList: TImageList;
    ToolBar1: TToolBar;
    StartRasterToolButton: TToolButton;
    StopToolButton: TToolButton;
    ToolBarSpacer1: TToolButton;
    ToolBarSpacer2: TToolButton;
    SettingsToolButton: TToolButton;
    Edit1: TMenuItem;
    Preferences1: TMenuItem;
    Counters1: TMenuItem;
    StartFCSToolButton: TToolButton;
    PreviousImages1: TMenuItem;
    ImageControl1: TMenuItem;
    Settings1: TMenuItem;
    ImageDisplay1: TMenuItem;
    StageControl1: TMenuItem;
    ToolBarSpacer4: TToolButton;
    ToolBarSpacer3: TToolButton;
    BkCountToolButton: TToolButton;
    SFCSWindow1: TMenuItem;
    DetailsMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SettingsToolButtonClick(Sender: TObject);
    procedure StartRasterToolButtonClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure StopToolButtonClick(Sender: TObject);
    procedure Preferences1Click(Sender: TObject);
    procedure Counters1Click(Sender: TObject);
    procedure PreviousImages1Click(Sender: TObject);
    procedure Settings1Click(Sender: TObject);
    procedure ImageControl1Click(Sender: TObject);
    procedure StageControl1Click(Sender: TObject);
    procedure ImageDisplay1Click(Sender: TObject);
    procedure BkCountToolButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure StartSFCSToolButtonClick(Sender: TObject);
    procedure SFCSWindow1Click(Sender: TObject);
  private
    LittleINI: TIniFile;
    WindowFroozen: Boolean;
    myFile: File of SmallInt;
    CountA, CountB, CountC: Single;
    LogSteps: array[0..20] of Single;
    //procedure WMWindowPosChanging(var msg: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure OnActivateTimer(Sender: TObject);
    function GetCurrentUserName(var CurrentUserName: string): Boolean;
  public
    TodaysFolder: string;
  end;


var
  MainForm: TMainForm;
  AcquireThread: TAcquireThread;
  Acquiring,
  CancelAcq,
  Ran: Boolean;
  iCount: integer;
  CurrentUser: string;
  Matrix: array[0..511,0..511] of Word;
  Timer: TTimer;

implementation

uses
// ************************************************************************
    HardwareFunctions, HardwareSettingsForm, SettingsForm, FilmStripForm,
    ImageControl, StageControl, Spalsh, SFCSWindow, PowerFunctions;
// ************************************************************************
{$R *.dfm}
{$Define HARDWAREPRESENT}

procedure TMainForm.FormCreate(Sender: TObject);
var
i,j:integer;
Number: SmallInt;
DirString: string;
FormatSettings: TFormatSettings;
begin
//  fill the log step array with the required values - 4 points per decade
LogSteps[0] := 0.00125;
LogSteps[1] := 0.001768;
LogSteps[2] := 0.0025;
LogSteps[3] := 0.003536;
LogSteps[4] := 0.005;
LogSteps[5] := 0.007071;
LogSteps[6] := 0.01;
LogSteps[7]  := LogSteps[0]*10;
LogSteps[8]  := LogSteps[1]*10;
LogSteps[9]  := LogSteps[2]*10;
LogSteps[10] := LogSteps[3]*10;
LogSteps[11] := LogSteps[4]*10;
LogSteps[12] := LogSteps[5]*10;
LogSteps[13] := LogSteps[6]*10;
LogSteps[14] := LogSteps[0]*100;
LogSteps[15] := LogSteps[1]*100;
LogSteps[16] := LogSteps[2]*100;
LogSteps[17] := LogSteps[3]*100;
LogSteps[18] := LogSteps[4]*100;
LogSteps[19] := LogSteps[5]*100;
LogSteps[20] := LogSteps[6]*100;

//  New pointers for the color arrays
New(BlueRGBArray);
New(CyanRGBArray);
New(GreenRGBArray);
New(RedRGBArray);
New(YellowRGBArray);
New(YellowHotRGBArray);

Self.Color := PaletteRGB(25,25,30);
Self.Caption := 'Vis' + Chr(Key_mu) +'m';
Application.Title := 'Vis' + Chr(Key_mu) +'m';
HardwareControl := THardwareControl.Create;
TIFFFunc := TTIFFFunc.Create;

GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);
FormatSettings.DateSeparator := '-';
TodaysFolder := DateTimeToStr(Date,FormatSettings);
GetCurrentUserName(CurrentUser);
DirString := GetCurrentDir + '\TemporaryData\' + TodaysFolder;
CreateDir(DirString);

//Self.Width := Screen.WorkAreaWidth+8;
//Self.Height := Screen.WorkAreaHeight-20;
//Self.Left := Screen.WorkAreaLeft-4;
//Self.Top := Screen.WorkAreaTop-4;

//Self.Width := Screen.DesktopWidth+8;
//Self.Height := Screen.DesktopHeight-20;
//Self.Left := Screen.DesktopLeft-4;
//Self.Top := Screen.DesktopTop-4;
//WindowFroozen := True;

//DetailsMemo.Width := Screen.WorkAreaWidth - 18;
//DetailsMemo.Top := Screen.WorkAreaHeight - DetailsMemo.Height - 62;
//DetailsMemo.Left := Screen.DesktopWidth - Screen.WorkAreaWidth;

LittleINI := TIniFile.Create('HardwareData\ImageCount.ini');
iCount := LittleINI.ReadInteger('raster','count',0);

Timer := TTimer.Create(Self);
Timer.Enabled := False;

DetailsMemo.Top := ClientHeight-DetailsMemo.Height;
DetailsMemo.Visible := False;
DetailsMemo.HandleNeeded;

HardwareControl.AllocMem;
//HardwareControl.Config;

ToolBar1.Buttons[6].Width := 200;

{$IfDef HARDWAREPRESENT}
{$Else}
AssignFile(myFile,'NellyPledge9540ChA.tif');
//AssignFile(myFile,'D:\My Documents\Emory\ProgramData\R1722S00A1Word.ibw');
//AssignFile(myFile,'D:\My Documents\Emory\ProgramData\CA517A1.ibw');
FileMode := fmOpenRead;
Reset(myFile);
Seek(myFile,8);
for j := 0 to 511 do
 begin
 // Seek(myFile,192+(j*1024));
  for i := 0  to 511 do
   begin
    Read(myFile, Number);
    Matrix[j,i] := Number;
   end;
 end;
CloseFile(myFile);
{$EndIf}

end;

function TMainForm.GetCurrentUserName(var CurrentUserName: string): Boolean;
var
  BufferSize: DWORD;
  pUser: PChar;
begin
  BufferSize := 0;
  GetUserName(nil, BufferSize);
  pUser := StrAlloc(BufferSize);
  try
    Result := GetUserName(pUser, BufferSize);
    CurrentUserName := StrPas(pUser);
  finally
    StrDispose(pUser);
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
//SettingsFrm.PowerCtrl.Cancel := True;
LittleINI.WriteInteger('raster','count',iCount);
LittleINI.Free;
Timer.Free;
HardwareControl.FreeMem;
HardwareControl.Free;
TIFFFunc.Free;

BlueRGBArray := nil;
CyanRGBArray := nil;
Dispose(BlueRGBArray);
Dispose(CyanRGBArray);
Dispose(GreenRGBArray);
Dispose(RedRGBArray);
Dispose(YellowRGBArray);
Dispose(YellowHotRGBArray);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
if Acquiring then CanClose := False;
end;

(*procedure TMainForm.WMWindowPosChanging(var msg: TWMWindowPosChanging);
begin
if WindowFroozen then msg.WindowPos.Flags := msg.WindowPos.flags or swp_NoMove;
end; *)


// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ****************************                                              Menu Items

procedure TMainForm.Counters1Click(Sender: TObject);
begin
HardwareSettingsFrm.WindowState := wsNormal;
HardwareSettingsFrm.ShowModal;
end;

procedure TMainForm.Preferences1Click(Sender: TObject);
begin
PrefFrm.WindowState := wsNormal;
if Acquiring then
 begin
  if MessageDlg('Adjusting Preferences during acquisition may disrupt data!' + #10 +
                                'Are you sure you want to continue?',mtWarning,[mbYes,mbCancel],0) = mrYes then PrefFrm.ShowModal;
  Exit;
 end
else PrefFrm.ShowModal;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
MainForm.Close;
end;

procedure TMainForm.PreviousImages1Click(Sender: TObject);
begin
FilmStrForm.Show;
end;

procedure TMainForm.Settings1Click(Sender: TObject);
begin
SettingsFrm.WindowState := wsNormal;
SettingsFrm.Left := 0;
SettingsFrm.Top := 96;
SettingsFrm.Show;
SettingsToolButton.Tag := 1;
end;

procedure TMainForm.ImageDisplay1Click(Sender: TObject);
begin
ImageForm.Show;
ImageForm.WindowState := wsNormal;
end;

procedure TMainForm.ImageControl1Click(Sender: TObject);
begin
ImageCtrlFrm.Show;
ImageCtrlFrm.WindowState := wsNormal;
end;

procedure TMainForm.StageControl1Click(Sender: TObject);
begin
MoveStageForm.XMoveEdit.Text := IntToStr(0);
MoveStageForm.YMoveEdit.Text := IntToStr(0);
MoveStageForm.ZMoveEdit.Text := IntToStr(0);
MoveStageForm.Show;
end;

procedure TMainForm.SFCSWindow1Click(Sender: TObject);
begin
SFCSForm.WindowState := wsNormal;
SFCSForm.Show;
end;

// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ****************************                                              ToolBar Buttons

procedure TMainForm.SettingsToolButtonClick(Sender: TObject);
begin
SettingsFrm.WindowState := wsNormal;
//  Uses the button tag as a boolean value to toggle the settings window when requested.
if SettingsToolButton.Tag = 0 then
 begin
  SettingsFrm.Left := 0;
  SettingsFrm.Top := 96;
  SettingsFrm.Show;
  SettingsToolButton.Tag := 1;
  Exit;
 end;
if SettingsToolButton.Tag = 1 then
 begin
  SettingsFrm.Hide;
  SettingsToolButton.Tag := 0;
  Exit;
 end;
end;



procedure TMainForm.StartRasterToolButtonClick(Sender: TObject);
begin
if Acquiring then Exit;
if   (NOT SettingsFrm.Ras_ChannelAChBx.Checked
  AND NOT SettingsFrm.Ras_ChannelBChBx.Checked
  AND NOT SettingsFrm.Ras_ChannelCChBx.Checked ) then Exit;
Acquiring := True;
CancelAcq := False;
//  Takes all the details for settingsinuse and sets some relevant parameters
try
//  PreStart does all the last StrToInt, StrToFloat conversions from the settings form.
//  On conversion errors, you are asked to recheck and the acquistion ends here.
SettingsFrm.Ras_PreStart;
except on EConvertError do
  begin
   ShowMessage('Please check settings and try again.');
   Acquiring := False;
   Exit;
  end;
end;
//  Checks for odd values of image size.  Only even values are allowed.
if Odd(SettingsFrm.Settings.Image.ImageWidth) OR Odd(SettingsFrm.Settings.Image.ImageLength) then
 begin
  ShowMessage('Please ensure image dimensions are even values.');
  if SettingsToolButton.Tag = 0 then SettingsToolButton.Tag := 1;
  SettingsFrm.Show;
  SettingsFrm.ActiveControl := SettingsFrm.Ras_UserPixelWidthEdit;
  Acquiring := False;
  Exit;
 end;

//  Sets the scale factors associated with the voltage binary values and the physical size of images.
SettingsFrm.SetScaleFactors;
//  copies settings from form onto SettingsInUse for acquisition
SettingsInUse := SettingsFrm.Settings;
with SettingsInUse do
 begin
  ID := 1; //  id number denoting raster scan type for future functions
  ScanRangeCount := (2*Image.ImageWidth*Image.ImageLength)+2;//Scan Buffer Length
  PhotonBufCount := (Image.ImageWidth*Image.ImageLength);// Photon Buffer Length
 end;
// Flags indicating changes in size, autoscale, residence times, and more.....
// Sets the size of the bitmaps and a few other relevant details from settings
// data if pixel size has changed from previous image.
if SettingsFrm.Settings.Image.NewSize then ImageForm.ResetImages;
// Sets an estimation of the maximum value depending on a change of pixel residence time
if SettingsFrm.Settings.Image.NewTime then ImageForm.GuessMaxVal;
// Resets the Qvalues for auto-scale display
if SettingsInUse.Image.ScaleChanged then

with SaveData do                    //  Fills out the save data to match settingsinuse
 begin
  umPerPixelx := SettingsInUse.MicroMPerPixelx;
  umPerPixely := SettingsInUse.MicroMPerPixely;
  StartingPhase := 0.0;
  Radius := 0.0;
  XOffset := 0.0;
  YOffset := 0.0;
  ImagePixelWidth := SettingsInUse.Image.ImageWidth;
  ImagePixelHeight := SettingsInUse.Image.ImageLength;
  PixelTimes := SettingsInUse.Image.PixelTimes;
  LineTimes := SettingsInUse.Image.LineTimes;
  FrameTimes := SettingsInUse.Image.FrameTimes;
  SetTimeHigh := SettingsInUse.SetTimeHigh;
  SetTimeLow := SettingsInUse.SetTimeLow;
  PosVector := SettingsInUse.PosVector;
  Wavelength := SettingsInUse.Wavelength;
  ObjectiveCode := SettingsInUse.ObjectiveCode;
  ID := SettingsInUse.ID;
  LaserPower := SettingsFrm.PowerCtrl.PowerNow;
 end;

Ran := True;
//  Create and run thread.
AcquireThread := TAcquireThread.Create(False);
end;



procedure TMainForm.StopToolButtonClick(Sender: TObject);
begin
CancelAcq := True;
end;




procedure TMainForm.BkCountToolButtonClick(Sender: TObject);
begin
if Acquiring then Exit;
CountDispFrm := TCountDispFrm.Create(Application);

Acquiring := True;
CancelAcq := False;
SettingsInUse.ID := 9; //  id number used for background counts
SettingsInUse.PhotonBufCount := 1048576;
with SettingsInUse.Image do
 begin
  PixelTimes.Delay := 10;
  PixelTimes.Pulse := Round(PrefFrm.Duration/PrefFrm.CountsPerDur)-10;
  ChannelASelected := True;
  ChannelBSelected := True;
  ChannelCSelected := True;
 end;
AcquireThread := TAcquireThread.Create(False);
CountDispFrm.ShowModal;
AcquireThread.WaitFor;
CountDispFrm.Free;
end;





procedure TMainForm.StartSFCSToolButtonClick(Sender: TObject);
var
i: Byte;
OutVolts: Single;
begin
if Acquiring then Exit;
if   (NOT SettingsFrm.SFCS_ChannelAChBx.Checked
  AND NOT SettingsFrm.SFCS_ChannelBChBx.Checked
  AND NOT SettingsFrm.SFCS_ChannelCChBx.Checked ) then Exit;
Acquiring := True;
CancelAcq := False;

try
SettingsFrm.SFCS_PreStart;
except on EConvertError do
  begin
   ShowMessage('Please check settings and try again.');
   Acquiring := False;
   Exit;
  end;
end;

SettingsInUse := SettingsFrm.Settings;

with SettingsInUse do
 begin
  ID := 3; // id number used for scanning fcs
  PhotonBufCount := 1048576;
  ScanRangeCount := (ScanFCS.NumberOfPts*2)+2;
  MicroMPerPixelx := (2*Pi*SettingsInUse.ScanFCS.Radius)/SettingsInUse.ScanFCS.NumberOfPts;
 end;
with SettingsInUse.ScanFCS do
 begin
  SaturationVal := Round(5.0E06*PixelPulseTimeMicroSec*1.0E-06);
  invSatVal := 1/SaturationVal;
  if PrefFrm.SFCS_PulsesPerGate then
   begin
    if DurationTime then RequestedLength := Round( DurTimeVal/( (PixelTimes.Pulse+PixelTimes.Delay)*12.5E-09*NumberOfPts ) );
    if DurationCount then RequestedLength := DurCyclesVal;
    MaxSafeLength := Trunc( (4294967296-(2*SizeOf(SaveData)))/(NumberOfPts*2) );
   end;
  if PrefFrm.SFCS_SeparOfPulses then
   begin
    if DurationTime then TimerInterval := Round(DurTimeVal*1000);
    if DurationCount then TimerInterval := Round((NumberOfPts*(PixelPulseTimeMicroSec+PixelDelayTimeMicroSec))*DurCyclesVal*1000);
    MaxSafeLength := 4294967296-(2*SizeOf(SaveData));
    Timer.OnTimer := OnActivateTimer;
    Timer.Interval := TimerInterval;
   end;
 end;

SFCSForm.ResetStacks;

with SaveData do                    //  Fills out the save data to match settingsinuse
 begin
  umPerPixelx := SettingsInUse.MicroMPerPixelx;
  umPerPixely := 0.0;
  StartingPhase := SettingsInUse.ScanFCS.StartingPhase;
  Radius := SettingsInUse.ScanFCS.Radius;
  XOffset := SettingsInUse.ScanFCS.XOffset;
  YOffset := SettingsInUse.ScanFCS.YOffSet;
  ImagePixelWidth := SettingsInUse.ScanFCS.NumberOfPts;
  ImagePixelHeight := 0;
  PixelTimes := SettingsInUse.ScanFCS.PixelTimes;
  LineTimes.Pulse := 0;
  LineTimes.Delay := 0;
  FrameTimes.Pulse := 0;
  FrameTimes.Delay := 0;
  SetTimeHigh := SettingsInUse.SetTimeHigh;
  SetTimeLow := SettingsInUse.SetTimeLow;
  PosVector := SettingsInUse.PosVector;
  Wavelength := SettingsInUse.Wavelength;
  ObjectiveCode := SettingsInUse.ObjectiveCode;
  ID := SettingsInUse.ID;
  LaserPower := SettingsFrm.PowerCtrl.PowerNow;
 end;

if SettingsInUse.ScanFCS.PowerSweep then
 begin
  for i := 0 to 20 do
   begin
    Acquiring := True;
    OutVolts := (SettingsFrm.PowerCtrl.DynRange*ArcCos(1-2*LogSteps[i]))/Pi;
    SettingsFrm.PowerCtrl.SetPockels(OutVolts);
    SaveData.LaserPower := SettingsFrm.PowerCtrl.PowerNow;
    AcquireThread := TAcquireThread.Create(False);
     repeat
      Application.ProcessMessages;
      sleep(25);
      if CancelAcq then Exit;
     until NOT Acquiring;
   end;
 end
else AcquireThread := TAcquireThread.Create(False);
end;



// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************

// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ************************************************************************************************************************************
// ****************************                                              { TImageThread }


procedure TAcquireThread.Execute;
var
ErrorList, TempErrArray: TErrorArray;
CallCount, i: integer;
begin
  inherited;

CallCount := 0;
Finalize(ErrorList);

//  HARDWARE CONFIG
TempErrArray := HardwareControl.Config;
SetLength(ErrorList,Length(TempErrArray)+CallCount);
for i := 0 to (Length(TempErrArray)-1) do ErrorList[i+CallCount] := TempErrArray[i];
CallCount := CallCount + Length(TempErrArray);
//  SET SCANNER
TempErrArray := HardwareControl.SetScanner;
SetLength(ErrorList,Length(TempErrArray)+CallCount);
for i := 0 to (Length(TempErrArray)-1) do ErrorList[i+CallCount] := TempErrArray[i];
CallCount := CallCount + Length(TempErrArray);
//  SET DATA COUNTERS
TempErrArray := HardwareControl.SetDataCounters;
SetLength(ErrorList,Length(TempErrArray)+CallCount);
for i := 0 to (Length(TempErrArray)-1) do ErrorList[i+CallCount] := TempErrArray[i];
CallCount := CallCount + Length(TempErrArray);
//  SET CLOCKS
TempErrArray := HardwareControl.SetClocks;
SetLength(ErrorList,Length(TempErrArray)+CallCount);
for i := 0 to (Length(TempErrArray)-1) do ErrorList[i+CallCount] := TempErrArray[i];
CallCount := CallCount + Length(TempErrArray);
//  START ACQUISITION
TempErrArray := HardwareControl.StartDaq;
SetLength(ErrorList,Length(TempErrArray)+CallCount);
for i := 0 to (Length(TempErrArray)-1) do ErrorList[i+CallCount] := TempErrArray[i];
CallCount := CallCount + Length(TempErrArray);
//  ACQUIRE
TempErrArray := HardwareControl.Acquire;
SetLength(ErrorList,Length(TempErrArray)+CallCount);
for i := 0 to (Length(TempErrArray)-1) do ErrorList[i+CallCount] := TempErrArray[i];
CallCount := CallCount + Length(TempErrArray);
//  STOP
TempErrArray := HardwareControl.Stop;
SetLength(ErrorList,Length(TempErrArray)+CallCount);
for i := 0 to (Length(TempErrArray)-1) do ErrorList[i+CallCount] := TempErrArray[i];
CallCount := CallCount + Length(TempErrArray);
//  Display Errors
{$IfDef HARDWAREPRESENT}
for i := 0 to Length(ErrorList)-1 do
 begin
  if ErrorList[i].StatusCode < 0 then MainForm.DetailsMemo.Lines.Add('Error '
    + IntToStr(ErrorList[i].StatusCode) + ' ' + ErrorList[i].FuncCalled + ' - '
    + GetCodeMeaning(ErrorList[i].StatusCode));
 end;
{$Else}
{$EndIf}
MainForm.DetailsMemo.Lines.Add(DateTimeToStr(Now));

Acquiring := False;
end;





procedure TMainForm.OnActivateTimer(Sender: TObject);
begin
CancelAcq := True;
beep;
Timer.Enabled := False;
end;



end.
