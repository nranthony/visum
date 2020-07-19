{
External functions for accessing the NI-DAQ boards.
Each function returns an array of smallint containing the error codes returned.
Negative -- Function did not execute because of an error
Zero -- Function completed successfully
Positive -- Function executed but with a potentially serious side effect
}
 
unit HardwareFunctions;

interface

uses nidaq, nidaqcns, windows, dialogs, SysUtils, Classes, Forms, Graphics,
     Math,
// ************************************************************************
     TypesUnit, ImageWindow, SettingsForm, Preferences;
// ************************************************************************

type

   THardwareControl = class
    private
      StartX, StartY: SmallInt;
      PointsRequested: Cardinal;
      PointsRetrieved: Cardinal;
      PointsAvailableA: Cardinal;
      PointsAvailableB: Cardinal;
      PointsAvailableC: Cardinal;
      function _MoveToScanPoint(): TErrorArray;
      function _CentralPosition(): TErrorArray;
      procedure _SetRasterScanner();
      procedure _SetSFCSScanner();
      function _AcquireRaster(): TErrorArray;
      function _AcquireBkCount(): TErrorArray;
      function _AcquireSFCS1(): TErrorArray;
      function _AcquireSFCS2(): TErrorArray;
      function _SetAOutBuf(): TErrorArray;
      function _SetPhotonCounterA(const Source, Gate, AppType: Cardinal): TErrorArray;
      function _SetPhotonCounterB(const Source, Gate, AppType: Cardinal): TErrorArray;
      function _SetPhotonCounterC(const Source, Gate, AppType: Cardinal): TErrorArray;
      function _SetPixelCounter(const Pulse, Delay: Cardinal): TErrorArray;
      function _SetLineCounter(const Pulse, Delay: Cardinal): TErrorArray;
      function _SetFrameCounter(const Pulse, Delay: Cardinal): TErrorArray;
      function _SetLineFrameHigh(): TErrorArray;
      function _SetTrigger(): TErrorArray;
      function _ThrowTrigger(): TErrorArray;
    public
      AOutsDevNum: SmallInt;
      CountersDevNum: SmallInt;
      AOutTrigLine: Cardinal;
      AOutTrigLinePolarity: Cardinal;
      CountersTrigLine: Cardinal;
      CountersTrigLinePolarity: Cardinal;
      SetTrigDIGState: SmallInt;
      GODIGState: Smallint;
      PixelClockHardwareCtr: Cardinal;
      PixelClockSource: Cardinal;
      PixelClockSourcePolarity: Cardinal;
      PixelClockOutputSignal: Cardinal;
      PixelClockOutputSource: Cardinal;
      PixelClockOutputPolarity: Cardinal;
      LineClockHardwareCtr: Cardinal;
      LineClockSource: Cardinal;
      LineClockSourcePolarity: Cardinal;
      LineClockOutputSignal: Cardinal;
      LineClockPFILine: Byte;
      LineClockOutputSource: Cardinal;
      LineClockOutputPolarity: Cardinal;
      FrameClockHardwareCtr: Cardinal;
      FrameClockSource: Cardinal;
      FrameClockSourcePolarity: Cardinal;
      FrameClockOutputSignal: Cardinal;
      FrameClockPFILine: Byte;
      FrameClockOutputSource: Cardinal;
      FrameClockOutputPolarity: Cardinal;
      FrameClockGate: Cardinal;
      FrameClockGatePolarity: Cardinal;
      PhCtrAHardwareCtr: Cardinal;
      PhCtrASource: Cardinal;
      PhCtrAGate: Cardinal;
      PhCtrBHardwareCtr: Cardinal;
      PhCtrBSource: Cardinal;
      PhCtrBGate: Cardinal;
      PhCtrCHardwareCtr: Cardinal;
      PhCtrCSource: Cardinal;
      PhCtrCGate: Cardinal;
      PhCtrDHardwareCtr: Cardinal;
      PhCtrDSource: Cardinal;
      PhCtrDGate: Cardinal;
      PhCtrEHardwareCtr: Cardinal;
      PhCtrESource: Cardinal;
      PhCtrEGate: Cardinal;
      CountArrayA,
      CountArrayB,
      CountArrayC: array of Cardinal;
      function Config(): TErrorArray;
      procedure AllocMem();
      procedure FreeMem();
      function SetClocks(): TErrorArray;
      function SetDataCounters(): TErrorArray;
      function SetScanner(): TErrorArray;
      function StartDaq(): TErrorArray;
      function Acquire(): TErrorArray;
      function Stop(): TErrorArray;
      function Reset(): Boolean;
    published
     end;

var
 HardwareControl: THardwareControl;
 pScanStartPoint: pi16;
 SaveData: TSaveDat;
 myTIFFA,
 myTIFFB,
 myTIFFC: TFileStream;

implementation

// ************************************************************************
uses Main, TIFFControl, CountDispWindow, HardwareSettingsForm, SFCSWindow;
// ************************************************************************

{$Define HARDWAREPRESENT}
{$Define RECORDADDR}

var

 OpStart: SmallInt = 1; //  Start waveform operation
 OpClear: SmallInt = 0; //  Clear waveform operation
 Chan0: SmallInt = 0; //  Analog output channel one
 Chan1: SmallInt = 1; //  Analog output channel two
 NumChans: SmallInt = 2; //  Number of channels in analog output group "Group"
 Group: SmallInt = 1; //  Group number for channels in out output group
 Iterations: Cardinal = 0; //  Number of times waveform generation steps through buffer.
                           //  zero means waveform generation proceeds indefinitely
 FiFoMode: SmallInt = 0; //  first in first out mode - fifo mode waveform generation
 WhichClock: SmallInt = 0; //  Selects default update clock
 UpdateTB: SmallInt = 0; //  with whichclock set to 0 this uses external clock set by Selct_Signal
 UpdateInt: Cardinal = 0; //  UpdateTB divisor - irrelevant as external clock used
 DelayMode: SmallInt = 0; //  Enables delay clock - irrelevant due to fifo waveform generation
 ChanVect: array[0..1] of byte;

 hPhotonBufferA: THandle;
 hPhotonBufferB: THandle;
 hPhotonBufferC: THandle;
 pPhotonBufferA: PU32Array;
 pPhotonBufferB: PU32Array;
 pPhotonBufferC: PU32Array;
 hRetrieveBufferA: THandle;
 hRetrieveBufferB: THandle;
 hRetrieveBufferC: THandle;
 pRetrieveBufferA: PU32Array;
 pRetrieveBufferB: PU32Array;
 pRetrieveBufferC: PU32Array;
 hScanRange: THandle;
 pScanRange: PI16Array;

{ THardwareControl }

function THardwareControl._MoveToScanPoint: TErrorArray;
begin
SetLength(Result,2);
Result[0].StatusCode := AO_Write(AOutsDevNum,Chan0,StartX);
Result[0].FuncCalled := '_MoveToScanPoint_AO_Write';
Result[1].StatusCode := AO_Write(AOutsDevNum,Chan1,StartY);
Result[1].FuncCalled := '_MoveToScanPoint_AO_Write';
end;


function THardwareControl._CentralPosition: TErrorArray;
begin
SetLength(Result,2);
Result[0].StatusCode := AO_Write(AOutsDevNum,Chan0,0);
Result[0].FuncCalled := '_MoveToScanPoint_AO_Write';
Result[1].StatusCode := AO_Write(AOutsDevNum,Chan1,0);
Result[1].FuncCalled := '_MoveToScanPoint_AO_Write';
end;

procedure THardwareControl._SetRasterScanner;
var
ScanBufIndex: Integer;
x, y: SmallInt;
TempBinValX, TempBinValY, BinValX, BinValY: Integer;  //temporary values of x and y binary value votlage positions
SlowVari, FastVari, SlowVariStPnt, FastVariStPnt: integer;
{$IfDef RECORDADDR}
AddressFile: TextFile;
TempStr: string;
{$EndIf}
begin
//  API function take size in bytes.  A SmallInt is 2 bytes long.
hScanRange := GlobalAlloc(GHND, 4194308);
pScanRange := PI16Array(GlobalLock(hScanRange));
//  opens file for writing the created scanner ranges to.
{$IfDef RECORDADDR}
AssignFile(AddressFile,'TemporaryData\RasAddresses' + IntToStr(iCount) + '.txt');
FileMode := fmOpenWrite;
ReWrite(AddressFile);
{$EndIf}
//  Top left postion is first into the scanbuffer.  From neg to pos accross and down.
//  This assumes a central postion of 0,0 and a default (+-10V) bipolar analog ouput signal.
//  The half a binary value ensures the the image is central, as array values must be even.
FastVariStPnt := PrefFrm.ReflectAlongYAxisConst *
               ((-(SettingsInUse.Image.ImageWidth * SettingsInUse.BinValPerPixelx) Div 2)
               + Round(0.5*SettingsInUse.BinValPerPixelx));
SlowVariStPnt := PrefFrm.ReflectAlongXAxisConst *
               ((-(SettingsInUse.Image.ImageLength * SettingsInUse.BinValPerPixely) Div 2)
               + Round(0.5*SettingsInUse.BinValPerPixely));

ScanBufIndex := 0;
//  todo -oNeil: Check order of analog group - xy or yx for addresses buffer
//  nested loop puts binary values for each position on the raster image into a
//  continueous array to be sent to the galvenometers via the AFM controller.

if PrefFrm.SwapChannels then
 begin
  for x := 0 to SettingsInUse.Image.ImageWidth-1 do
   begin
    FastVari := FastVariStPnt + PrefFrm.ReflectAlongYAxisConst
               * (x*SettingsInUse.BinValPerPixelx);
    {$IfDef RECORDADDR}
    TempStr := '';
    {$EndIf}
    for y := 0 to SettingsInUse.Image.ImageLength-1 do
     begin
      SlowVari := SlowVariStPnt + PrefFrm.ReflectAlongXAxisConst
                 * (y*SettingsInUse.BinValPerPixely);
      pScanRange^[ScanBufIndex] := FastVari;
      inc(ScanBufIndex);
      pScanRange^[ScanBufIndex] := SlowVari;
      inc(ScanBufIndex);
      {$IfDef RECORDADDR}
      TempStr := TempStr + IntToStr(pScanRange^[ScanBufIndex-2])
                  + ', ' + IntToStr(pScanRange^[ScanBufIndex-1])+ ', ';
      {$EndIf}
     end;
    {$IfDef RECORDADDR}
    WriteLn(AddressFile,TempStr);
    {$EndIf}
   end;
 end
else
 begin
  for y := 0 to SettingsInUse.Image.ImageLength-1 do
   begin
    SlowVari :=  SlowVariStPnt + PrefFrm.ReflectAlongXAxisConst
                * (y*SettingsInUse.BinValPerPixely);
    {$IfDef RECORDADDR}
    TempStr := '';
    {$EndIf}
    for x := 0 to SettingsInUse.Image.ImageWidth-1 do
     begin
      FastVari := FastVariStPnt + PrefFrm.ReflectAlongYAxisConst
                * (x*SettingsInUse.BinValPerPixelx);
      pScanRange^[ScanBufIndex] := SlowVari;
      inc(ScanBufIndex);
      pScanRange^[ScanBufIndex] := FastVari;
      inc(ScanBufIndex);
      {$IfDef RECORDADDR}
      TempStr := TempStr + IntToStr(pScanRange^[ScanBufIndex-2])
                  + ', ' + IntToStr(pScanRange^[ScanBufIndex-1])+ ', ';
      {$EndIf}
     end;
    {$IfDef RECORDADDR}
    WriteLn(AddressFile,TempStr);
    {$EndIf}
   end;
 end;
//  a copy of the first two go last and the buffer indicies sent to the analog outputs is offset
pScanRange^[ScanBufIndex] := pScanRange^[0];
inc(ScanBufIndex);
pScanRange^[ScanBufIndex] := pScanRange^[1];
//  The start point passed to the NI card is the second point of the acquistion. The first point of
//  the acquistion has been copied to the end.
pScanStartPoint := @pScanRange^[2];

{$IfDef RECORDADDR}
CloseFile(AddressFile);
{$EndIf}
end;


procedure THardwareControl._SetSFCSScanner;
var
StepSize, XMicroM, YMicroM, BinValPerMicroM: Single;
t, n, ScanBufIndex: integer;
XBinVal, YBinVal: SmallInt;
{$IfDef RECORDADDR}
AddressFile: TextFile;
TempStr: string;
{$EndIf}
begin
//  API function take size in bytes.  A SmallInt is 2 bytes long.
hScanRange := GlobalAlloc(GHND, 4194308);
pScanRange := PI16Array(GlobalLock(hScanRange));
{$IfDef RECORDADDR}
AssignFile(AddressFile,'TemporaryData\SFCSAddresses' + IntToStr(iCount) + '.txt');
FileMode := fmOpenWrite;
ReWrite(AddressFile);
{$EndIf}
ScanBufIndex := 0;
BinValPerMicroM := 1/SettingsInUse.MicroMPerBinVal;
n := SettingsInUse.ScanFCS.NumberOfPts;
StepSize := (2*Pi)/n;

for t := 0 to n-1 do
 begin
  XMicroM := SettingsInUse.ScanFCS.XOffset + XCalibrationFactor * SettingsInUse.ScanFCS.Radius
  * Cos( (0.01745329 * SettingsInUse.ScanFCS.StartingPhase) + (StepSize * t) );  //  0.01745329 =  2Pi/360
  XBinVal := Round(XMicroM*BinValPerMicroM);
  YMicroM := SettingsInUse.ScanFCS.YOffset + YCalibrationFactor * SettingsInUse.ScanFCS.Radius
  * Sin( (0.01745329 * SettingsInUse.ScanFCS.StartingPhase) + (StepSize * t) );
  YBinVal := Round(YMicroM*BinValPerMicroM);
  pScanRange^[ScanBufIndex] := XBinVal;
  inc(ScanBufIndex);
  pScanRange^[ScanBufIndex] := YBinVal;
  inc(ScanBufIndex);
  {$IfDef RECORDADDR}
  TempStr := IntToStr(XBinVal) + ', ' + IntToStr(YBinVal);
  WriteLn(AddressFile,TempStr);
  {$EndIf}
 end;
pScanStartPoint := @pScanRange^[0];
{$IfDef RECORDADDR}
CloseFile(AddressFile);
{$EndIf}
end;

function THardwareControl._SetAOutBuf: TErrorArray;
var
temp: SmallInt;
begin
SetLength(Result,6);
//  todo -oNeil: Check aoutput channel order - AO0 & AO1 xy or yx?
ChanVect[1] := 0; ChanVect[0] := 1;
Result[0].StatusCode := WFM_Group_Setup(AOutsDevNum, NumChans, @ChanVect[0], Group);
Result[0].FuncCalled := '_SetAOutBuf_WFM_Group_Setup';
Result[1].StatusCode := AO_Change_Parameter(AOutsDevNum, Chan0, ND_DATA_TRANSFER_CONDITION, ND_FIFO_EMPTY);
Result[1].FuncCalled := '_SetAOutBuf_AO_Change_Parameter';
Result[2].StatusCode := AO_Change_Parameter(AOutsDevNum, Chan1, ND_DATA_TRANSFER_CONDITION, ND_FIFO_EMPTY);
Result[2].FuncCalled := '_SetAOutBuf_AO_Change_Parameter';
Result[3].StatusCode := WFM_Load(AOutsDevNum, NumChans, @ChanVect[0], pi16(pScanStartPoint),
                      SettingsInUse.ScanRangeCount-2, Iterations, FiFoMode);
temp := PSmallInt(pScanStartPoint)^;
Result[3].FuncCalled := '_SetAOutBuf_WFM_Load';
Result[4].StatusCode := WFM_ClockRate(AOutsDevNum, Group, WhichClock, UpdateTB, UpdateInt, DelayMode);
Result[4].FuncCalled := '_SetAOutBuf_WFM_ClockRate';
Result[5].StatusCode := Select_Signal(AOutsDevNum, ND_OUT_UPDATE, AOutTrigLine, AOutTrigLinePolarity);
Result[5].FuncCalled := '_SetAOutBuf_Select_Signal';
end;

function THardwareControl._SetPixelCounter(const Pulse, Delay: Cardinal): TErrorArray;
begin
SetLength(Result,9);
Result[0].StatusCode := GPCTR_Control(CountersDevNum, PixelClockHardwareCtr, ND_RESET);
Result[0].FuncCalled := '_SetPixelCounter_GPCTR_Control';
Result[1].StatusCode := GPCTR_Set_Application(CountersDevNum, PixelClockHardwareCtr, ND_PULSE_TRAIN_GNR);
Result[1].FuncCalled := '_SetPixelCounter_GPCTR_Set_Application';
Result[2].StatusCode := GPCTR_Change_Parameter(CountersDevNum, PixelClockHardwareCtr, ND_START_TRIGGER, ND_ENABLED);
Result[2].FuncCalled := '_SetPixelCounter_GPCTR_Change_Parameter';
Result[3].StatusCode := GPCTR_Change_Parameter(CountersDevNum, PixelClockHardwareCtr, ND_SOURCE, PixelClockSource);
Result[3].FuncCalled := '_SetPixelCounter_GPCTR_Change_Parameter';
if PixelClockSourcePolarity <> 0 then  //  value of 0 corresponds to N/A
Result[4].StatusCode := GPCTR_Change_Parameter(CountersDevNum, PixelClockHardwareCtr, ND_SOURCE_POLARITY, PixelClockSourcePolarity);
Result[4].FuncCalled := '_SetPixelCounter_GPCTR_Change_Parameter';
Result[5].StatusCode := GPCTR_Change_Parameter(CountersDevNum, PixelClockHardwareCtr, ND_COUNT_1, Delay);
Result[5].FuncCalled := '_SetPixelCounter_GPCTR_Change_Parameter';
Result[6].StatusCode := GPCTR_Change_Parameter(CountersDevNum, PixelClockHardwareCtr, ND_COUNT_2, Pulse);
Result[6].FuncCalled := '_SetPixelCounter_GPCTR_Change_Parameter';
Result[7].StatusCode := Select_Signal(CountersDevNum, PixelClockOutputSignal, PixelClockOutputSource, PixelClockOutputPolarity);
Result[7].FuncCalled := '_SetPixelCounter_Select_Signal';
Result[8].StatusCode := GPCTR_Control(CountersDevNum, PixelClockHardwareCtr, ND_PROGRAM);
Result[8].FuncCalled := '_SetPixelCounter_GPCTR_Control';
end;

function THardwareControl._SetLineCounter(const Pulse, Delay: Cardinal): TErrorArray;
begin
SetLength(Result,9);
Result[0].StatusCode := GPCTR_Control(CountersDevNum, LineClockHardwareCtr, ND_RESET);
Result[0].FuncCalled := '_SetLineCounter_GPCTR_Control';
Result[1].StatusCode := GPCTR_Set_Application(CountersDevNum, LineClockHardwareCtr, ND_PULSE_TRAIN_GNR);
Result[1].FuncCalled := '_SetLineCounter_GPCTR_Set_Application';
Result[2].StatusCode := GPCTR_Change_Parameter(CountersDevNum, LineClockHardwareCtr, ND_START_TRIGGER, ND_ENABLED);
Result[2].FuncCalled := '_SetLineCounter_GPCTR_Change_Parameter';
Result[3].StatusCode := GPCTR_Change_Parameter(CountersDevNum, LineClockHardwareCtr, ND_SOURCE, LineClockSource);
Result[3].FuncCalled := '_SetLineCounter_GPCTR_Change_Parameter';
if LineClockSourcePolarity <> 0 then  //  value of 0 corresponds to N/A
Result[4].StatusCode := GPCTR_Change_Parameter(CountersDevNum, LineClockHardwareCtr, ND_SOURCE_POLARITY, LineClockSourcePolarity);
Result[4].FuncCalled := '_SetLineCounter_GPCTR_Change_Parameter';
Result[5].StatusCode := GPCTR_Change_Parameter(CountersDevNum, LineClockHardwareCtr, ND_COUNT_1, Delay);
Result[5].FuncCalled := '_SetLineCounter_GPCTR_Change_Parameter';
Result[6].StatusCode := GPCTR_Change_Parameter(CountersDevNum, LineClockHardwareCtr, ND_COUNT_2, Pulse);
Result[6].FuncCalled := '_SetLineCounter_GPCTR_Change_Parameter';
Result[7].StatusCode := Select_Signal(CountersDevNum, LineClockOutputSignal, LineClockOutputSource, LineClockOutputPolarity);
Result[7].FuncCalled := '_SetLineCounter_Select_Signal';
Result[8].StatusCode := GPCTR_Control(CountersDevNum, LineClockHardwareCtr, ND_PROGRAM);
Result[8].FuncCalled := '_SetLineCounter_GPCTR_Control';
end;

function THardwareControl._SetFrameCounter(const Pulse, Delay: Cardinal): TErrorArray;
begin
SetLength(Result,12);
Result[0].StatusCode := GPCTR_Control(CountersDevNum, FrameClockHardwareCtr, ND_RESET);
Result[0].FuncCalled := '_SetFrameCounter_GPCTR_Control';
Result[1].StatusCode := GPCTR_Set_Application(CountersDevNum, FrameClockHardwareCtr, ND_PULSE_TRAIN_GNR);
Result[1].FuncCalled := '_SetFrameCounter_GPCTR_Set_Application';
Result[2].StatusCode := GPCTR_Change_Parameter(CountersDevNum, FrameClockHardwareCtr, ND_START_TRIGGER, ND_ENABLED);
Result[2].FuncCalled := '_SetFrameCounter_GPCTR_Change_Parameter';
Result[3].StatusCode := GPCTR_Change_Parameter(CountersDevNum, FrameClockHardwareCtr, ND_SOURCE, FrameClockSource);
Result[3].FuncCalled := '_SetFrameCounter_GPCTR_Change_Parameter';
if FrameClockSourcePolarity <> 0 then  //  value of 0 corresponds to N/A
Result[4].StatusCode := GPCTR_Change_Parameter(CountersDevNum, FrameClockHardwareCtr, ND_SOURCE_POLARITY, FrameClockSourcePolarity);
Result[4].FuncCalled := '_SetFrameCounter_GPCTR_Change_Parameter';
Result[5].StatusCode := 0;//GPCTR_Change_Parameter(CountersDevNum, FrameClockHardwareCtr, ND_GATE, FrameClockGate);
Result[5].FuncCalled := '_SetFrameCounter_GPCTR_Change_Parameter';
Result[6].StatusCode := 0;//GPCTR_Change_Parameter(CountersDevNum, FrameClockHardwareCtr, ND_GATE_POLARITY, FrameClockGatePolarity);
Result[6].FuncCalled := '_SetFrameCounter_GPCTR_Change_Parameter';
Result[7].StatusCode := GPCTR_Change_Parameter(CountersDevNum, FrameClockHardwareCtr, ND_OUTPUT_POLARITY, FrameClockOutputPolarity);
Result[7].FuncCalled := '_SetFrameCounter_GPCTR_Change_Parameter';
Result[8].StatusCode := GPCTR_Change_Parameter(CountersDevNum, FrameClockHardwareCtr, ND_COUNT_1, Delay);
Result[8].FuncCalled := '_SetFrameCounter_GPCTR_Change_Parameter';
Result[9].StatusCode := GPCTR_Change_Parameter(CountersDevNum, FrameClockHardwareCtr, ND_COUNT_2, Pulse);
Result[9].FuncCalled := '_SetFrameCounter_GPCTR_Change_Parameter';
Result[10].StatusCode := Select_Signal(CountersDevNum, FrameClockOutputSignal, FrameClockOutputSource, FrameClockOutputPolarity);
Result[10].FuncCalled := '_SetFrameCounter_Select_Signal';
Result[11].StatusCode := GPCTR_Control(CountersDevNum, FrameClockHardwareCtr, ND_PROGRAM);
Result[11].FuncCalled := '_SetFrameCounter_GPCTR_Control';
end;


function THardwareControl._SetPhotonCounterA(const Source, Gate, AppType: Cardinal): TErrorArray;
begin
SetLength(Result,10);
Result[0].StatusCode := GPCTR_Control(CountersDevNum,  PhCtrAHardwareCtr, ND_RESET);
Result[0].FuncCalled := '_SetPhotonCounterA_GPCTR_Control';
Result[1].StatusCode := GPCTR_Set_Application(CountersDevNum,  PhCtrAHardwareCtr, AppType);
Result[1].FuncCalled := '_SetPhotonCounterA_GPCTR_Set_Application';
Result[8].StatusCode := GPCTR_Config_Buffer(CountersDevNum,  PhCtrAHardwareCtr, 0, SettingsInUse.PhotonBufCount, pu32(pPhotonBufferA));
Result[8].FuncCalled := '_SetPhotonCounterA_GPCTR_Config_Buffer';
Result[2].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrAHardwareCtr, ND_SOURCE, Source);
Result[2].FuncCalled := '_SetPhotonCounterA_GPCTR_Change_Parameter';
Result[3].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrAHardwareCtr, ND_GATE, Gate);
Result[3].FuncCalled := '_SetPhotonCounterA_GPCTR_Change_Parameter';
Result[4].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrAHardwareCtr, ND_INITIAL_COUNT, 0);
Result[4].FuncCalled := '_SetPhotonCounterA_GPCTR_Change_Parameter';
Result[5].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrAHardwareCtr, ND_BUFFER_MODE, ND_CONTINUOUS);
Result[5].FuncCalled := '_SetPhotonCounterA_GPCTR_Change_Parameter';
Result[6].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrAHardwareCtr, ND_START_TRIGGER, ND_ENABLED);
Result[6].FuncCalled := '_SetPhotonCounterA_GPCTR_Change_Parameter';
Result[7].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrAHardwareCtr, ND_COUNTING_SYNCHRONOUS, ND_YES);
Result[7].FuncCalled := '_SetPhotonCounterA_GPCTR_Change_Parameter';
Result[9].StatusCode := GPCTR_Control(CountersDevNum,  PhCtrAHardwareCtr, ND_PROGRAM);
Result[9].FuncCalled := '_SetPhotonCounterA_GPCTR_Control';
end;

function THardwareControl._SetPhotonCounterB(const Source, Gate, AppType: Cardinal): TErrorArray;
begin
SetLength(Result,10);
Result[0].StatusCode := GPCTR_Control(CountersDevNum,  PhCtrBHardwareCtr, ND_RESET);
Result[0].FuncCalled := '_SetPhotonCounterB_GPCTR_Control';
Result[1].StatusCode := GPCTR_Set_Application(CountersDevNum,  PhCtrBHardwareCtr, AppType);
Result[1].FuncCalled := '_SetPhotonCounterB_GPCTR_Set_Application';
Result[8].StatusCode := GPCTR_Config_Buffer(CountersDevNum,  PhCtrBHardwareCtr, 0, SettingsInUse.PhotonBufCount, pu32(pPhotonBufferB));
Result[8].FuncCalled := '_SetPhotonCounterB_GPCTR_Config_Buffer';
Result[2].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrBHardwareCtr, ND_SOURCE, Source);
Result[2].FuncCalled := '_SetPhotonCounterB_GPCTR_Change_Parameter';
Result[3].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrBHardwareCtr, ND_GATE, Gate);
Result[3].FuncCalled := '_SetPhotonCounterB_GPCTR_Change_Parameter';
Result[4].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrBHardwareCtr, ND_INITIAL_COUNT, 0);
Result[4].FuncCalled := '_SetPhotonCounterB_GPCTR_Change_Parameter';
Result[5].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrBHardwareCtr, ND_BUFFER_MODE, ND_CONTINUOUS);
Result[5].FuncCalled := '_SetPhotonCounterB_GPCTR_Change_Parameter';
Result[6].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrBHardwareCtr, ND_START_TRIGGER, ND_ENABLED);
Result[6].FuncCalled := '_SetPhotonCounterB_GPCTR_Change_Parameter';
Result[7].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrBHardwareCtr, ND_COUNTING_SYNCHRONOUS, ND_YES);
Result[7].FuncCalled := '_SetPhotonCounterB_GPCTR_Change_Parameter';
Result[9].StatusCode := GPCTR_Control(CountersDevNum,  PhCtrBHardwareCtr, ND_PROGRAM);
Result[9].FuncCalled := '_SetPhotonCounterB_GPCTR_Control';
end;

function THardwareControl._SetPhotonCounterC(const Source, Gate, AppType: Cardinal): TErrorArray;
begin
SetLength(Result,10);
Result[0].StatusCode := GPCTR_Control(CountersDevNum,  PhCtrCHardwareCtr, ND_RESET);
Result[0].FuncCalled := '_SetPhotonCounterC_GPCTR_Control';
Result[1].StatusCode := GPCTR_Set_Application(CountersDevNum,  PhCtrCHardwareCtr, AppType);
Result[1].FuncCalled := '_SetPhotonCounterC_GPCTR_Set_Application';
Result[8].StatusCode := GPCTR_Config_Buffer(CountersDevNum,  PhCtrCHardwareCtr, 0, SettingsInUse.PhotonBufCount, pu32(pPhotonBufferC));
Result[8].FuncCalled := '_SetPhotonCounterC_GPCTR_Config_Buffer';
Result[2].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrCHardwareCtr, ND_SOURCE, Source);
Result[2].FuncCalled := '_SetPhotonCounterC_GPCTR_Change_Parameter';
Result[3].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrCHardwareCtr, ND_GATE, Gate);
Result[3].FuncCalled := '_SetPhotonCounterC_GPCTR_Change_Parameter';
Result[4].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrCHardwareCtr, ND_INITIAL_COUNT, 0);
Result[4].FuncCalled := '_SetPhotonCounterC_GPCTR_Change_Parameter';
Result[5].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrCHardwareCtr, ND_BUFFER_MODE, ND_CONTINUOUS);
Result[5].FuncCalled := '_SetPhotonCounterC_GPCTR_Change_Parameter';
Result[6].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrCHardwareCtr, ND_START_TRIGGER, ND_ENABLED);
Result[6].FuncCalled := '_SetPhotonCounterC_GPCTR_Change_Parameter';
Result[7].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrCHardwareCtr, ND_COUNTING_SYNCHRONOUS, ND_YES);
Result[7].FuncCalled := '_SetPhotonCounterC_GPCTR_Change_Parameter';
Result[9].StatusCode := GPCTR_Control(CountersDevNum,  PhCtrCHardwareCtr, ND_PROGRAM);
Result[9].FuncCalled := '_SetPhotonCounterC_GPCTR_Control';
end;


function THardwareControl._SetLineFrameHigh: TErrorArray;
begin
SetLength(Result,2);
//  Set digital output line high - This line is wired to PFI_7 as default.
Result[0].StatusCode := DIG_Out_Line(CountersDevNum, {port} 0, {line} FrameClockPFILine, {state} 1);
//Result[0].StatusCode := DIG_Out_Line(CountersDevNum, {port} 0, {line} 3, {state} 1);
Result[0].FuncCalled := '_SetLineFrameHigh_DIG_Out_Line';
Result[1].StatusCode := DIG_Out_Line(CountersDevNum, {port} 0, {line} LineClockPFILine, {state} 1);
//Result[1].StatusCode := DIG_Out_Line(CountersDevNum, {port} 0, {line} 4, {state} 1);
Result[1].FuncCalled := '_SetLineFrameHigh_DIG_Out_Line';
end;

function THardwareControl._SetTrigger: TErrorArray;
begin
SetLength(Result,1);
//  Set digital output line high - This line is wired to PFI_7 as default.
Result[0].StatusCode := DIG_Out_Line(CountersDevNum, {port} 0, {line} 1, {state} SetTrigDIGState);
Result[0].FuncCalled := '_SetTrigger_DIG_Out_Line';
end;

function THardwareControl._ThrowTrigger: TErrorArray;
begin
SetLength(Result,2);
// Start waveform generation at analog outputs
Result[0].StatusCode := WFM_Group_Control(AOutsDevNum, Group, OpSTART);
Result[0].FuncCalled := '_GO_WFM_Group_Control';
// Set Digital Line 1 of Port 0 to Low to trigger the counters
Result[1].StatusCode := DIG_Out_Line(CountersDevNum, {Port} 0, {Line} 1, {State} GODIGState);
Result[1].FuncCalled := '_GO_DIG_Out_Line';
end;

function THardwareControl._AcquireRaster: TErrorArray;
var
j, k, IndexPoint: integer;
CheckChA, CheckChB, CheckChC: Boolean;
begin
//beep;
Setlength(Result,7);
PointsRequested := SettingsInUse.Image.ImageWidth;
Result[0].StatusCode := 0;
Result[1].StatusCode := 0;
Result[2].StatusCode := 0;
PointsRetrieved := 0;
Result[1].FuncCalled := '_GetArrayLineA_GPCTR_Watch';
Result[2].FuncCalled := '_GetArrayLineB_GPCTR_Watch';
Result[3].FuncCalled := '_GetArrayLineC_GPCTR_Watch';
Result[4].FuncCalled := '_GetArrayLineA_GPCTR_Read_Buffer';
Result[5].FuncCalled := '_GetArrayLineB_GPCTR_Read_Buffer';
Result[6].FuncCalled := '_GetArrayLineC_GPCTR_Read_Buffer';
 
for j := 0 to SettingsInUse.Image.ImageLength-1 do
 begin
  if SettingsInUse.Image.ChannelASelected then CheckChA := True else CheckChA := False;
  if SettingsInUse.Image.ChannelBSelected then CheckChB := True else CheckChB := False;
  if SettingsInUse.Image.ChannelCSelected then CheckChC := True else CheckChC := False;
  PointsAvailableA := 0;
  PointsAvailableB := 0;
  PointsAvailableC := 0;
  IndexPoint := j * SettingsInUse.Image.ImageWidth;
  // -10920 gpctrDataLossError
  // One or more data points may have been lost during buffered
  // GPCTR operations due to speed limitations of your system.
  while (CheckChA OR CheckChB OR CheckChC) do
   begin
    if SettingsInUse.Image.ChannelASelected then
     begin
      //beep;
      Result[1].StatusCode := GPCTR_Watch(CountersDevNum, PhCtrAHardwareCtr, ND_AVAILABLE_POINTS, @PointsAvailableA);
      if PointsAvailableA >= PointsRequested then CheckChA := False;
      //ImageForm.DetailsMemo.Lines.Add('PointsAvail ChA: ' + IntToStr(PointsAvailableA));
      if ((Result[1].StatusCode < 0) AND (Result[1].StatusCode <> -10920)) then CheckChA := False;
     end;
    if SettingsInUse.Image.ChannelBSelected then
     begin
      Result[2].StatusCode := GPCTR_Watch(CountersDevNum, PhCtrBHardwareCtr, ND_AVAILABLE_POINTS, @PointsAvailableB);
      if PointsAvailableB >= PointsRequested then CheckChB := False;
      if ((Result[2].StatusCode < 0) AND (Result[2].StatusCode <> -10920)) then CheckChB := False;
     end;
    if SettingsInUse.Image.ChannelCSelected then
     begin
      Result[3].StatusCode := GPCTR_Watch(CountersDevNum, PhCtrCHardwareCtr, ND_AVAILABLE_POINTS, @PointsAvailableC);
      if PointsAvailableC >= PointsRequested then CheckChC := False;
      if ((Result[3].StatusCode < 0) AND (Result[3].StatusCode <> -10920)) then CheckChC := False;
     end;
    if CancelAcq then Exit;
   end;

  if SettingsInUse.Image.ChannelASelected then
   begin
    Result[4].StatusCode := GPCTR_Read_Buffer(CountersDevNum, PhCtrAHardwareCtr, ND_READ_MARK,
                              {ReadOffset} 0, PointsRequested, {TimeOut} 1.0,
                                        @PointsRetrieved, pu32(pRetrieveBufferA));
    for k := 0 to SettingsInUse.Image.ImageWidth-1 do
     begin
      {$IfDef HARDWAREPRESENT}
      ImageForm.ChanInfoA.ImageArray[IndexPoint + k] := pRetrieveBufferA^[k];
      //if pRetrieveBufferA^[k] > ImageForm.ChanInfoA.TempMaxVal then
      //   ImageForm.ChanInfoA.TempMaxVal := pRetrieveBufferA^[k];
      myTIFFA.Write(pRetrieveBufferA^[k],2);
      {$Else}
      ImageForm.ChanInfoA.ImageArray[IndexPoint + k] := Matrix[j,k];
      {$EndIf}
     end;
   end;  // end if A section.
  if SettingsInUse.Image.ChannelBSelected then
   begin
    Result[5].StatusCode := GPCTR_Read_Buffer(CountersDevNum, PhCtrBHardwareCtr, ND_READ_MARK,
                              {ReadOffset} 0, PointsRequested, {TimeOut} 1.0,
                                        @PointsRetrieved, pu32(pRetrieveBufferB));
    for k := 0 to SettingsInUse.Image.ImageWidth-1 do
     begin
      {$IfDef HARDWAREPRESENT}
      ImageForm.ChanInfoB.ImageArray[IndexPoint + k] := pRetrieveBufferB^[k];
      //if pRetrieveBufferB^[k] > ImageForm.ChanInfoB.TempMaxVal then
      //   ImageForm.ChanInfoB.TempMaxVal := pRetrieveBufferB^[k];
      myTIFFB.Write(pRetrieveBufferB^[k],2);
      {$Else}
      ImageForm.ChanInfoB.ImageArray[IndexPoint + k] := Matrix[j,k];
      {$EndIf}
     end;
   end;  // end if B section.
  if SettingsInUse.Image.ChannelCSelected then
   begin
    Result[6].StatusCode := GPCTR_Read_Buffer(CountersDevNum, PhCtrCHardwareCtr, ND_READ_MARK,
                              {ReadOffset} 0, PointsRequested, {TimeOut} 1.0,
                                        @PointsRetrieved, pu32(pRetrieveBufferC));
    for k := 0 to SettingsInUse.Image.ImageWidth-1 do
     begin
      {$IfDef HARDWAREPRESENT}
      ImageForm.ChanInfoC.ImageArray[IndexPoint + k] := pRetrieveBufferC^[k];
      //if pRetrieveBufferC^[k] > ImageForm.ChanInfoC.TempMaxVal then
      //   ImageForm.ChanInfoC.TempMaxVal := pRetrieveBufferC^[k];
      myTIFFC.Write(pRetrieveBufferC^[k],2);
      {$Else}
      ImageForm.ChanInfoC.ImageArray[IndexPoint + k] := Matrix[j,k];
      {$EndIf}
     end;
   end;  // end if C section.
  if CancelAcq then Exit;
  PostMessage(ImageForm.Handle,WM_DISPLAY_IMAGE,j,IndexPoint);
 end; //  end from top to bottom loop.
end;





function THardwareControl._AcquireBkCount: TErrorArray;
var
k: integer;
CheckChA, CheckChB, CheckChC: Boolean;
TempA, TempB, TempC, i: Cardinal;
CountA, CountB, CountC: Single;
CountAStr, CountBStr, CountCStr: string;
RetrievedValues: string;
PtrA,PtrB,PtrC: Pointer;
begin
Setlength(Result,7);
SetLength(CountArrayA,PrefFrm.CountsPerDur);
SetLength(CountArrayB,PrefFrm.CountsPerDur);
SetLength(CountArrayC,PrefFrm.CountsPerDur);
Result[0].StatusCode := 0;
Result[1].StatusCode := 0;
Result[2].StatusCode := 0;
PointsRetrieved := 0;
Result[1].FuncCalled := '_GetArrayLineA_GPCTR_Watch';
Result[2].FuncCalled := '_GetArrayLineB_GPCTR_Watch';
Result[3].FuncCalled := '_GetArrayLineC_GPCTR_Watch';
Result[4].FuncCalled := '_GetArrayLineA_GPCTR_Read_Buffer';
Result[5].FuncCalled := '_GetArrayLineB_GPCTR_Read_Buffer';
Result[6].FuncCalled := '_GetArrayLineC_GPCTR_Read_Buffer';
PointsRequested := PrefFrm.CountsPerDur;
New(PtrA);
New(PtrB);
New(PtrC);

repeat
CheckChA := True;
CheckChB := True;
CheckChC := True;
while (CheckChA OR CheckChB OR CheckChC) do
 begin
  Result[1].StatusCode := GPCTR_Watch(CountersDevNum, PhCtrAHardwareCtr, ND_AVAILABLE_POINTS, @PointsAvailableA);
  if PointsAvailableA >= PointsRequested then CheckChA := False;
  if ((Result[1].StatusCode < 0) AND (Result[1].StatusCode <> -10920)) then CheckChA := False;
  Result[2].StatusCode := GPCTR_Watch(CountersDevNum, PhCtrBHardwareCtr, ND_AVAILABLE_POINTS, @PointsAvailableB);
  if PointsAvailableB >= PointsRequested then CheckChB := False;
  if ((Result[2].StatusCode < 0) AND (Result[2].StatusCode <> -10920)) then CheckChB := False;
  Result[3].StatusCode := GPCTR_Watch(CountersDevNum, PhCtrCHardwareCtr, ND_AVAILABLE_POINTS, @PointsAvailableC);
  if PointsAvailableC >= PointsRequested then CheckChC := False;
  if ((Result[3].StatusCode < 0) AND (Result[3].StatusCode <> -10920)) then CheckChC := False;
  if CancelAcq then Exit;
  sleep(25);
 end;

Result[4].StatusCode := GPCTR_Read_Buffer(CountersDevNum, PhCtrAHardwareCtr, ND_READ_MARK,
                           {ReadOffset} 0, PrefFrm.CountsPerDur, {TimeOut} 1.0, @PointsRetrieved, pu32(pRetrieveBufferA));
for k := 0 to PrefFrm.CountsPerDur-1 do CountArrayA[k] := pRetrieveBufferA^[k];

RetrievedValues := IntToStr(PointsRetrieved);

Result[5].StatusCode := GPCTR_Read_Buffer(CountersDevNum, PhCtrBHardwareCtr, ND_READ_MARK,
                           {ReadOffset} 0, PrefFrm.CountsPerDur, {TimeOut} 1.0, @PointsRetrieved, pu32(pRetrieveBufferB));
for k := 0 to PrefFrm.CountsPerDur-1 do CountArrayB[k] := pRetrieveBufferB^[k];

RetrievedValues := RetrievedValues + IntToStr(PointsRetrieved);

Result[6].StatusCode := GPCTR_Read_Buffer(CountersDevNum, PhCtrCHardwareCtr, ND_READ_MARK,
                           {ReadOffset} 0, PrefFrm.CountsPerDur, {TimeOut} 1.0, @PointsRetrieved, pu32(pRetrieveBufferC));
for k := 0 to PrefFrm.CountsPerDur-1 do CountArrayC[k] := pRetrieveBufferC^[k];

RetrievedValues := RetrievedValues + IntToStr(PointsRetrieved);
if PrefFrm.BkCntRet then MainForm.DetailsMemo.Lines.Add(RetrievedValues);

RetrievedValues := '';
TempA := 0;
TempB := 0;
TempC := 0;
for i := 0 to PrefFrm.CountsPerDur-1 do
 begin
  TempA := TempA + HardwareControl.CountArrayA[i];
  TempB := TempB + HardwareControl.CountArrayB[i];
  TempC := TempC + HardwareControl.CountArrayC[i];
  //RetrievedValues := RetrievedValues + IntToStr(HardwareControl.CountArrayA[i]) + ', '
  //                                   + IntToStr(HardwareControl.CountArrayB[i]) + ', '
  //                                   + IntToStr(HardwareControl.CountArrayC[i]) + ', ';
  //beep;
 end;
//ImageForm.DetailsMemo.Lines.Add(RetrievedValues);
CountA := 0.001 * ((TempA/PrefFrm.CountsPerDur)/(SettingsInUse.Image.PixelTimes.Pulse*(12.5E-09)));
CountB := 0.001 * ((TempB/PrefFrm.CountsPerDur)/(SettingsInUse.Image.PixelTimes.Pulse*(12.5E-09)));
CountC := 0.001 * ((TempC/PrefFrm.CountsPerDur)/(SettingsInUse.Image.PixelTimes.Pulse*(12.5E-09)));

PtrA := @CountA;
PtrB := @CountB;
PtrC := @CountC;

SendMessage(CountDispFrm.Handle,WM_DISP_A_CNT,integer(PtrA),0);
SendMessage(CountDispFrm.Handle,WM_DISP_B_CNT,integer(PtrB),0);
SendMessage(CountDispFrm.Handle,WM_DISP_C_CNT,integer(PtrC),0);

//beep;
Application.ProcessMessages;
until CancelAcq;
PtrA := nil;
PtrB := nil;
PtrC := nil;
end;




function THardwareControl._AcquireSFCS1: TErrorArray;
var
k, LineCount: Cardinal;
CheckChA, CheckChB, CheckChC: Boolean;
begin
LineCount := 0;
Setlength(Result,7);
PointsRequested := SettingsInUse.ScanFCS.NumberOfPts;
Result[0].StatusCode := 0;
Result[1].StatusCode := 0;
Result[2].StatusCode := 0;
PointsRetrieved := 0;
Result[1].FuncCalled := '_GetArrayLineA_GPCTR_Watch';
Result[2].FuncCalled := '_GetArrayLineB_GPCTR_Watch';
Result[3].FuncCalled := '_GetArrayLineC_GPCTR_Watch';
Result[4].FuncCalled := '_GetArrayLineA_GPCTR_Read_Buffer';
Result[5].FuncCalled := '_GetArrayLineB_GPCTR_Read_Buffer';
Result[6].FuncCalled := '_GetArrayLineC_GPCTR_Read_Buffer';

repeat
CheckChA := True;
CheckChB := True;
CheckChC := True;
while (CheckChA OR CheckChB OR CheckChC) do
 begin
  Result[1].StatusCode := GPCTR_Watch(CountersDevNum, PhCtrAHardwareCtr, ND_AVAILABLE_POINTS, @PointsAvailableA);
  if PointsAvailableA >= PointsRequested then CheckChA := False;
  if ((Result[1].StatusCode < 0) AND (Result[1].StatusCode <> -10920)) then CheckChA := False;
  Result[2].StatusCode := GPCTR_Watch(CountersDevNum, PhCtrBHardwareCtr, ND_AVAILABLE_POINTS, @PointsAvailableB);
  if PointsAvailableB >= PointsRequested then CheckChB := False;
  if ((Result[2].StatusCode < 0) AND (Result[2].StatusCode <> -10920)) then CheckChB := False;
  Result[3].StatusCode := GPCTR_Watch(CountersDevNum, PhCtrCHardwareCtr, ND_AVAILABLE_POINTS, @PointsAvailableC);
  if PointsAvailableC >= PointsRequested then CheckChC := False;
  if ((Result[3].StatusCode < 0) AND (Result[3].StatusCode <> -10920)) then CheckChC := False;
  if CancelAcq then Exit;
 end;

if SettingsInUse.ScanFCS.ChannelASelected then
 begin
  Result[4].StatusCode := GPCTR_Read_Buffer(CountersDevNum, PhCtrAHardwareCtr, ND_READ_MARK,
                             {ReadOffset} 0, PointsRequested, {TimeOut} 1.0, @PointsRetrieved, pu32(pRetrieveBufferA));
  {$IfDef HARDWAREPRESENT}
  {$Else}
  for k := 0 to PointsRequested-1 do
  pRetrieveBufferA^[k] := Round(Random(500) * Abs(Sin(0.0632*k)));
  {$EndIf}
  for k := 0 to PointsRequested-1 do myTIFFA.Write(pRetrieveBufferA^[k],2);
  PostMessage(SFCSForm.Handle,WM_DISPLAY_SFCSA,integer(pRetrieveBufferA),LineCount);
 end;
if SettingsInUse.ScanFCS.ChannelBSelected then
 begin
  Result[5].StatusCode := GPCTR_Read_Buffer(CountersDevNum, PhCtrBHardwareCtr, ND_READ_MARK,
                             {ReadOffset} 0, PointsRequested, {TimeOut} 1.0, @PointsRetrieved, pu32(pRetrieveBufferB));
  {$IfDef HARDWAREPRESENT}
  {$Else}
  for k := 0 to PointsRequested-1 do
  pRetrieveBufferB^[k] := Round(Random(500) * Abs(Sin(0.0632*k)));
  {$EndIf}
  for k := 0 to PointsRequested-1 do myTIFFB.Write(pRetrieveBufferB^[k],2);
  PostMessage(SFCSForm.Handle,WM_DISPLAY_SFCSB,integer(pRetrieveBufferB),LineCount);
 end;
if SettingsInUse.ScanFCS.ChannelCSelected then
 begin
  Result[6].StatusCode := GPCTR_Read_Buffer(CountersDevNum, PhCtrCHardwareCtr, ND_READ_MARK,
                             {ReadOffset} 0, PointsRequested, {TimeOut} 1.0, @PointsRetrieved, pu32(pRetrieveBufferC));
  {$IfDef HARDWAREPRESENT}
  {$Else}
  for k := 0 to PointsRequested-1 do
  pRetrieveBufferC^[k] := Round(Random(500) * Abs(Sin(0.0632*k)));
  {$EndIf}
  for k := 0 to PointsRequested-1 do myTIFFC.Write(pRetrieveBufferC^[k],2);
  PostMessage(SFCSForm.Handle,WM_DISPLAY_SFCSC,integer(pRetrieveBufferC),LineCount);
 end;

inc(LineCount);

until CancelAcq OR (LineCount >= SettingsInUse.ScanFCS.RequestedLength) OR (LineCount >= SettingsInUse.ScanFCS.MaxSafeLength);
SettingsInUse.ScanFCS.TIFFImageLengthA := LineCount;
SettingsInUse.ScanFCS.TIFFImageLengthB := LineCount;
SettingsInUse.ScanFCS.TIFFImageLengthC := LineCount;
end;





function THardwareControl._AcquireSFCS2: TErrorArray;
var
k, RunningTotalCount: integer;
TiffSizeA, TiffSizeB, TiffSizeC, LineCount: Cardinal;
CheckChA, CheckChB, CheckChC: Boolean;
begin
Setlength(Result,7);
LineCount := 0;
PointsRequested := 512;
Result[0].StatusCode := 0;
Result[1].StatusCode := 0;
Result[2].StatusCode := 0;
PointsRetrieved := 0;
Result[1].FuncCalled := '_GetArrayLineA_GPCTR_Watch';
Result[2].FuncCalled := '_GetArrayLineB_GPCTR_Watch';
Result[3].FuncCalled := '_GetArrayLineC_GPCTR_Watch';
Result[4].FuncCalled := '_GetArrayLineA_GPCTR_Read_Buffer';
Result[5].FuncCalled := '_GetArrayLineB_GPCTR_Read_Buffer';
Result[6].FuncCalled := '_GetArrayLineC_GPCTR_Read_Buffer';

repeat
CheckChA := True;
CheckChB := True;
CheckChC := True;
while (CheckChA OR CheckChB OR CheckChC) do
 begin
  Result[1].StatusCode := GPCTR_Watch(CountersDevNum, PhCtrAHardwareCtr, ND_AVAILABLE_POINTS, @PointsAvailableA);
  if PointsAvailableA >= PointsRequested then CheckChA := False;
  if ((Result[1].StatusCode < 0) AND (Result[1].StatusCode <> -10920)) then CheckChA := False;
  Result[2].StatusCode := GPCTR_Watch(CountersDevNum, PhCtrBHardwareCtr, ND_AVAILABLE_POINTS, @PointsAvailableB);
  if PointsAvailableB >= PointsRequested then CheckChB := False;
  if ((Result[2].StatusCode < 0) AND (Result[2].StatusCode <> -10920)) then CheckChB := False;
  Result[3].StatusCode := GPCTR_Watch(CountersDevNum, PhCtrCHardwareCtr, ND_AVAILABLE_POINTS, @PointsAvailableC);
  if PointsAvailableC >= PointsRequested then CheckChC := False;
  if ((Result[3].StatusCode < 0) AND (Result[3].StatusCode <> -10920)) then CheckChC := False;
  if CancelAcq then Exit;
 end;

if SettingsInUse.ScanFCS.ChannelASelected then
 begin
  Result[4].StatusCode := GPCTR_Read_Buffer(CountersDevNum, PhCtrAHardwareCtr, ND_READ_MARK,
                             {ReadOffset} 0, PointsRequested, {TimeOut} 1.0, @PointsRetrieved, pu32(pRetrieveBufferA));
  for k := 0 to PointsRequested-1 do myTIFFA.Write(pRetrieveBufferA^[k],2);
  TiffSizeA := myTIFFA.Position;
  PostMessage(SFCSForm.Handle,WM_DISPLAY_SFCSA,integer(pRetrieveBufferA),LineCount);
 end;
if SettingsInUse.ScanFCS.ChannelBSelected then
 begin
  Result[5].StatusCode := GPCTR_Read_Buffer(CountersDevNum, PhCtrBHardwareCtr, ND_READ_MARK,
                             {ReadOffset} 0, PointsRequested, {TimeOut} 1.0, @PointsRetrieved, pu32(pRetrieveBufferB));
  for k := 0 to PointsRequested-1 do myTIFFB.Write(pRetrieveBufferB^[k],2);
  TiffSizeB := myTIFFB.Position;
  PostMessage(SFCSForm.Handle,WM_DISPLAY_SFCSB,integer(pRetrieveBufferB),LineCount);
 end;
if SettingsInUse.ScanFCS.ChannelCSelected then
 begin
  Result[6].StatusCode := GPCTR_Read_Buffer(CountersDevNum, PhCtrCHardwareCtr, ND_READ_MARK,
                             {ReadOffset} 0, PointsRequested, {TimeOut} 1.0, @PointsRetrieved, pu32(pRetrieveBufferC));
  for k := 0 to PointsRequested-1 do myTIFFC.Write(pRetrieveBufferC^[k],2);
  TiffSizeC := myTIFFC.Position;
  PostMessage(SFCSForm.Handle,WM_DISPLAY_SFCSC,integer(pRetrieveBufferC),LineCount);
 end;

inc(LineCount);
until CancelAcq OR (TiffSizeA >= SettingsInUse.ScanFCS.MaxSafeLength)
                OR (TiffSizeB >= SettingsInUse.ScanFCS.MaxSafeLength)
                OR (TiffSizeC >= SettingsInUse.ScanFCS.MaxSafeLength);
SettingsInUse.ScanFCS.TIFFImageLengthA := Trunc((TiffSizeA-8)/SettingsInUse.ScanFCS.NumberOfPts)+1;
SettingsInUse.ScanFCS.TIFFImageLengthB := Trunc((TiffSizeB-8)/SettingsInUse.ScanFCS.NumberOfPts)+1;
SettingsInUse.ScanFCS.TIFFImageLengthC := Trunc((TiffSizeC-8)/SettingsInUse.ScanFCS.NumberOfPts)+1;
end;




// **************************************************************************************************************************************************
// **************************************************************************************************************************************************
// **************************************************************************************************************************************************
// **************************************************************************************************************************************************
// **************************************************************************************************************************************************
// **************************************************************************************************************************************************
// **************************************************************************************************************************************************
// **************************************************************************************************************************************************
// **************************************************************************************************************************************************
// **************************************************************************************************************************************************
// **************************************************************************************************************************************************
// **************************************************************************************************************************************************
// **************************************************************************************************************************************************
// **************************************************************************************************************************************************
// **************************************                                           Public Functions






procedure THardwareControl.AllocMem;
var
i: byte;
begin
//  The Handle for the memory allocated in GlobalAlloc is passed to GlobalLock and the
//  pointer returned is cast to a PU32Array for type compatibility.
hPhotonBufferA := GlobalAlloc(GHND, 4194304);
pPhotonBufferA := PU32Array(GlobalLock(hPhotonBufferA));
hPhotonBufferB := GlobalAlloc(GHND, 4194304);
pPhotonBufferB := PU32Array(GlobalLock(hPhotonBufferB));
hPhotonBufferC := GlobalAlloc(GHND, 4194304);
pPhotonBufferC := PU32Array(GlobalLock(hPhotonBufferC));

hRetrieveBufferA := GlobalAlloc(GHND, 4194304);
pRetrieveBufferA := PU32Array(GlobalLock(hRetrieveBufferA));
hRetrieveBufferB := GlobalAlloc(GHND, 4194304);
pRetrieveBufferB := PU32Array(GlobalLock(hRetrieveBufferB));
hRetrieveBufferC := GlobalAlloc(GHND, 4194304);
pRetrieveBufferC := PU32Array(GlobalLock(hRetrieveBufferC));
end;



function THardwareControl.Config: TErrorArray;
begin
SetLength(Result,5);
//  Set digital output line config
Result[0].StatusCode := DIG_Line_Config(CountersDevNum, {port} 0, {line} 1, {direction} 1);
Result[0].FuncCalled := 'HardwareCONFIG_DIG_Line_Config';
//  Select signal PFI and point on which to trigger
Result[1].StatusCode := Select_Signal(CountersDevNum, ND_START_TRIGGER, CountersTrigLine, CountersTrigLinePolarity);
Result[1].FuncCalled := 'HardwareCONFIG_Select_Signal';
//   Synchronses signal using the ND_INTERNAL_MAX_TIMEBASE
Result[2].StatusCode := Line_Change_Attribute(CountersDevNum, CountersTrigLine, ND_LINE_FILTER, ND_SYNCHRONIZATION);
Result[2].FuncCalled := 'HardwareCONFIG_Line_Change_Attribute';
Result[3].StatusCode := 0;
Result[4].StatusCode := 0;
//  configs the frame and line clocks for digital output, must be done before all other counter calls
case SettingsInUse.ID of
 9,3 : begin
   Result[3].StatusCode := DIG_Line_Config(CountersDevNum, {port} 0, {line} FrameClockPFILine, {direction} 0);
   Result[3].StatusCode := DIG_Line_Config(CountersDevNum, {port} 0, {line} FrameClockPFILine, {direction} 1);
   //Result[3].StatusCode := DIG_Line_Config(CountersDevNum, {port} 0, {line} 3, {direction} 1);
   Result[3].FuncCalled := '_SetLineFrameHigh_DIG_Line_Config';
   Result[4].StatusCode := DIG_Line_Config(CountersDevNum, {port} 0, {line} LineClockPFILine, {direction} 0);
   Result[4].StatusCode := DIG_Line_Config(CountersDevNum, {port} 0, {line} LineClockPFILine, {direction} 1);
   //Result[4].StatusCode := DIG_Line_Config(CountersDevNum, {port} 0, {line} 4, {direction} 1);
   Result[4].FuncCalled := '_SetLineFrameHigh_DIG_Line_Config';
  end;
 end;
end;



function THardwareControl.Reset: Boolean;
begin

end;



function THardwareControl.SetClocks: TErrorArray;
var
TempErrs: TErrorArray;
CallCount, i: Byte;
begin
CallCount := 0;
TempErrs := _SetTrigger;
SetLength(Result,Length(TempErrs)+CallCount);
for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
CallCount := CallCount + Length(TempErrs);

case SettingsInUse.ID of
 0: begin                //  No Scan ************************
    end;
 1: begin                //  Raster Scan Type ***************
     TempErrs := _SetPixelCounter(SettingsInUse.Image.PixelTimes.Pulse,SettingsInUse.Image.PixelTimes.Delay);
     SetLength(Result,Length(TempErrs)+CallCount);
     for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
     CallCount := CallCount + Length(TempErrs);
     TempErrs := _SetLineCounter(SettingsInUse.Image.LineTimes.Pulse,SettingsInUse.Image.LineTimes.Delay);
     SetLength(Result,Length(TempErrs)+CallCount);
     for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
     CallCount := CallCount + Length(TempErrs);
     TempErrs := _SetFrameCounter(SettingsInUse.Image.FrameTimes.Pulse,SettingsInUse.Image.FrameTimes.Delay);
     SetLength(Result,Length(TempErrs)+CallCount);
     for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
     CallCount := CallCount + Length(TempErrs);
    end;
 2: begin                //  FCS Scan Type ******************
     //  FCS stuff
    end;

 3: begin
     TempErrs := _SetLineFrameHigh;
     SetLength(Result,Length(TempErrs)+CallCount);
     for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
     CallCount := CallCount + Length(TempErrs);
     TempErrs := _SetPixelCounter(SettingsInUse.ScanFCS.PixelTimes.Pulse,SettingsInUse.ScanFCS.PixelTimes.Delay);
     SetLength(Result,Length(TempErrs)+CallCount);
     for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
     CallCount := CallCount + Length(TempErrs);
    end;
 9: begin
     TempErrs := _SetLineFrameHigh;
     SetLength(Result,Length(TempErrs)+CallCount);
     for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
     CallCount := CallCount + Length(TempErrs);
     TempErrs := _SetPixelCounter(SettingsInUse.Image.PixelTimes.Pulse,SettingsInUse.Image.PixelTimes.Delay);
     SetLength(Result,Length(TempErrs)+CallCount);
     for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
     CallCount := CallCount + Length(TempErrs);
    end;
end;
Finalize(TempErrs);
end;



function THardwareControl.SetScanner: TErrorArray;
var
TempErrs: TErrorArray;
CallCount, i: Byte;
begin
CallCount := 0;

case SettingsInUse.ID of
 0: begin                //  No Scan ************************
    end;
 1: begin                //  Raster Scan Type ***************
     _SetRasterScanner;
     TempErrs := _SetAOutBuf;
     SetLength(Result,Length(TempErrs)+CallCount);
     for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
     CallCount := CallCount + Length(TempErrs);
    end;
 2: begin                //  FCS Scan Type ******************
     //  FCS stuff
    end;
 3: begin                //  Scanning FCS Scan Type ***************
     _SetSFCSScanner;
     TempErrs := _SetAOutBuf;
     SetLength(Result,Length(TempErrs)+CallCount);
     for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
     CallCount := CallCount + Length(TempErrs);
    end;
 9: begin
     SetLength(Result,1);
     Result[0].StatusCode := 0;
     Result[0].FuncCalled := '';
    end;
end;
Finalize(TempErrs);
end;



function THardwareControl.SetDataCounters: TErrorArray;
var
TempErrs: TErrorArray;
CallCount, i: Byte;
begin
CallCount := 0;

case SettingsInUse.ID of
 0: begin                //  No Scan ************************
    end;
 1,9: begin                //  Raster Scan Type ***************
     if SettingsInUse.Image.ChannelASelected then
      begin
       TempErrs := _SetPhotonCounterA(PhCtrASource, PhCtrAGate, HardwareSettingsFrm.NI_DAQ_CNS_Value('ND_BUFFERED_PULSE_WIDTH_MSR'));
       SetLength(Result,Length(TempErrs)+CallCount);
       for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
       CallCount := CallCount + Length(TempErrs);
      end;
     if SettingsInUse.Image.ChannelBSelected then
      begin
       TempErrs := _SetPhotonCounterB(PhCtrBSource, PhCtrBGate, HardwareSettingsFrm.NI_DAQ_CNS_Value('ND_BUFFERED_PULSE_WIDTH_MSR'));
       SetLength(Result,Length(TempErrs)+CallCount);
       for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
       CallCount := CallCount + Length(TempErrs);
      end;
     if SettingsInUse.Image.ChannelCSelected then
      begin
       TempErrs := _SetPhotonCounterC(PhCtrCSource, PhCtrCGate, HardwareSettingsFrm.NI_DAQ_CNS_Value('ND_BUFFERED_PULSE_WIDTH_MSR'));
       SetLength(Result,Length(TempErrs)+CallCount);
       for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
       CallCount := CallCount + Length(TempErrs);
      end;
    end;
 2: begin                //  FCS Scan Type ******************
     // FCS Stuff
    end;
 3: begin                //  ScanningFCS Scan Type ******************
     if SettingsInUse.ScanFCS.ChannelASelected then
      begin
       if PrefFrm.SFCS_PulsesPerGate then TempErrs := _SetPhotonCounterA(PhCtrASource,PhCtrAGate,
                                                               HardwareSettingsFrm.NI_DAQ_CNS_Value('ND_BUFFERED_PULSE_WIDTH_MSR'));
       if PrefFrm.SFCS_SeparOfPulses then TempErrs := _SetPhotonCounterA(HardwareSettingsFrm.NI_DAQ_CNS_Value('ND_INTERNAL_MAX_TIMEBASE'),PhCtrAGate,
                                                               HardwareSettingsFrm.NI_DAQ_CNS_Value('ND_BUFFERED_PERIOD_MSR'));
       SetLength(Result,Length(TempErrs)+CallCount);
       for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
       CallCount := CallCount + Length(TempErrs);
      end;
     if SettingsInUse.ScanFCS.ChannelBSelected then
      begin
       if PrefFrm.SFCS_PulsesPerGate then TempErrs := _SetPhotonCounterB(PhCtrBSource,PhCtrBGate,
                                                               HardwareSettingsFrm.NI_DAQ_CNS_Value('ND_BUFFERED_PULSE_WIDTH_MSR'));
       if PrefFrm.SFCS_SeparOfPulses then TempErrs := _SetPhotonCounterB(HardwareSettingsFrm.NI_DAQ_CNS_Value('ND_INTERNAL_MAX_TIMEBASE'),PhCtrBGate,
                                                               HardwareSettingsFrm.NI_DAQ_CNS_Value('ND_BUFFERED_PERIOD_MSR'));
       SetLength(Result,Length(TempErrs)+CallCount);
       for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
       CallCount := CallCount + Length(TempErrs);
      end;
     if SettingsInUse.ScanFCS.ChannelCSelected then
      begin
       if PrefFrm.SFCS_PulsesPerGate then TempErrs := _SetPhotonCounterC(PhCtrCSource,PhCtrCGate,
                                                               HardwareSettingsFrm.NI_DAQ_CNS_Value('ND_BUFFERED_PULSE_WIDTH_MSR'));
       if PrefFrm.SFCS_SeparOfPulses then TempErrs := _SetPhotonCounterC(HardwareSettingsFrm.NI_DAQ_CNS_Value('ND_INTERNAL_MAX_TIMEBASE'),PhCtrCGate,
                                                               HardwareSettingsFrm.NI_DAQ_CNS_Value('ND_BUFFERED_PERIOD_MSR'));
       SetLength(Result,Length(TempErrs)+CallCount);
       for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
       CallCount := CallCount + Length(TempErrs);
      end;
    end;
end;
end;



function THardwareControl.StartDaq: TErrorArray;
var
TempErrs: TErrorArray;
CallCount, i: Byte;
begin
CallCount := 0;

case SettingsInUse.ID of
 0: begin                //  No Scan ************************
    end;
 1: begin                //  Raster Scan Type ***************
     TempErrs := _MoveToScanPoint;
     SetLength(Result,Length(TempErrs)+CallCount);
     for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
     CallCount := CallCount + Length(TempErrs);
     TempErrs := _ThrowTrigger;
     SetLength(Result,Length(TempErrs)+CallCount);
     for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
     CallCount := CallCount + Length(TempErrs);
    end;
 2: begin                //  FCS Scan Type ******************
     // FCS Stuff
    end;
 9: begin
     SetLength(Result,1);
     // Set Digital Line 1 of Port 0 to Low to trigger the counters
     Result[0].StatusCode := DIG_Out_Line(CountersDevNum, {Port} 0, {Line} 1, {State} GODIGState);
     Result[0].FuncCalled := '_GO_DIG_Out_Line';
    end;
 3: begin                //  Scannings ze FCS Scan Type ******************
     TempErrs := _CentralPosition;
     SetLength(Result,Length(TempErrs)+CallCount);
     for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
     CallCount := CallCount + Length(TempErrs);
     TempErrs := _ThrowTrigger;
     SetLength(Result,Length(TempErrs)+CallCount);
     for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
     CallCount := CallCount + Length(TempErrs);
     if PrefFrm.SFCS_SeparOfPulses then Timer.Enabled := True;
    end;
end;
end;



function THardwareControl.Acquire: TErrorArray;
var
TempErrs: TErrorArray;
CallCount, i: Byte;
begin
CallCount := 0;

case SettingsInUse.ID of
 0: begin                //  No Scan ************************
    end;
 1: begin                //  Raster Scan Type ***************
     repeat
      TIFFFunc.SetResFractions;
      if SettingsInUse.Image.UserSaveDirectory then
      SettingsInUse.Image.HintString := SettingsInUse.Image.FileName +
                                      FormatFloat('000', SettingsInUse.Image.StartingVal*1.0)
      else
      SettingsInUse.Image.HintString := MainForm.TodaysFolder + '\' + CurrentUser + IntToStr(iCount);
      if SettingsInUse.Image.ChannelASelected then      // ***********************************************   A
       begin
        if SettingsInUse.Image.UserSaveDirectory then
        myTIFFA := TFileStream.Create(SettingsInUse.Image.SaveDir + '\' + SettingsInUse.Image.FileName +
                                      FormatFloat('000', SettingsInUse.Image.StartingVal*1.0) + 'ChA.tif', fmCreate)
        else
        myTIFFA := TFileStream.Create('TemporaryData\' + SettingsInUse.Image.HintString + 'ChA.tif', fmCreate);
        TIFFFunc.PrepTIFF(myTIFFA,SettingsInUse.Image.ImageWidth,SettingsInUse.Image.ImageLength);
       end;
      if SettingsInUse.Image.ChannelBSelected then      // ***********************************************   B
       begin
        if SettingsInUse.Image.UserSaveDirectory then
        myTIFFA := TFileStream.Create(SettingsInUse.Image.SaveDir + '\' + SettingsInUse.Image.FileName +
                                      FormatFloat('000', SettingsInUse.Image.StartingVal*1.0) + 'ChB.tif', fmCreate)
        else
        myTIFFB := TFileStream.Create('TemporaryData\' + SettingsInUse.Image.HintString + 'ChB.tif', fmCreate);
        TIFFFunc.PrepTIFF(myTIFFB,SettingsInUse.Image.ImageWidth,SettingsInUse.Image.ImageLength);
       end;
      if SettingsInUse.Image.ChannelCSelected then      // ***********************************************   C
       begin
        if SettingsInUse.Image.UserSaveDirectory then
        myTIFFA := TFileStream.Create(SettingsInUse.Image.SaveDir + '\' + SettingsInUse.Image.FileName +
                                      FormatFloat('000', SettingsInUse.Image.StartingVal*1.0) + 'ChC.tif', fmCreate)
        else
        myTIFFC := TFileStream.Create('TemporaryData\' + SettingsInUse.Image.HintString + 'ChC.tif', fmCreate);
        TIFFFunc.PrepTIFF(myTIFFC,SettingsInUse.Image.ImageWidth,SettingsInUse.Image.ImageLength);
       end;

      TempErrs := _AcquireRaster;
      SetLength(Result,Length(TempErrs)+CallCount);
      for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
      CallCount := CallCount + Length(TempErrs);
      inc(iCount);

      if SettingsInUse.Image.ChannelASelected then myTIFFA.Free;
      if SettingsInUse.Image.ChannelBSelected then myTIFFB.Free;
      if SettingsInUse.Image.ChannelCSelected then myTIFFC.Free;

      PostMessage(ImageForm.Handle,WM_FRM_COMPLT,0,0);
      if SettingsInUse.Image.UserSaveDirectory then
       begin
        inc(SettingsInUse.Image.StartingVal);
        SettingsFrm.Ras_FNStartValEdit.Text := IntToStr(SettingsInUse.Image.StartingVal);
       end;
     until ((CancelAcq) OR (NOT SettingsInUse.Continuous));

    end;
 2: begin                //  FCS Scan Type ******************
     // FCS Stuff
    end;
 3: begin                //  Scanning da FCS Scan Type ******************
     TIFFFunc.SetSFCSResFractions;
     if SettingsInUse.ScanFCS.UserSaveDirectory then
      SettingsInUse.ScanFCS.HintString := SettingsInUse.ScanFCS.FileName + FormatFloat('000', SettingsInUse.ScanFCS.StartingVal*1.0)
     else
      SettingsInUse.ScanFCS.HintString := MainForm.TodaysFolder + '\' + CurrentUser + 'SFCS' + IntToStr(iCount);
     if SettingsInUse.ScanFCS.ChannelASelected then      // ***********************************************   A
      begin
       if SettingsInUse.ScanFCS.UserSaveDirectory then
        myTIFFA := TFileStream.Create(SettingsInUse.ScanFCS.SaveDir + '\' + SettingsInUse.ScanFCS.FileName +
                                     FormatFloat('000', SettingsInUse.ScanFCS.StartingVal*1.0) + 'ChA.tif', fmCreate)
       else
        myTIFFA := TFileStream.Create('TemporaryData\' + SettingsInUse.ScanFCS.HintString + 'ChA.tif', fmCreate);
       myTIFFA.Position := 8;
      end;
     if SettingsInUse.ScanFCS.ChannelBSelected then      // ***********************************************   B
      begin
       if SettingsInUse.ScanFCS.UserSaveDirectory then
        myTIFFB := TFileStream.Create(SettingsInUse.ScanFCS.SaveDir + '\' + SettingsInUse.ScanFCS.FileName +
                                     FormatFloat('000', SettingsInUse.ScanFCS.StartingVal*1.0) + 'ChB.tif', fmCreate)
       else
        myTIFFB := TFileStream.Create('TemporaryData\' + SettingsInUse.ScanFCS.HintString + 'ChB.tif', fmCreate);
       myTIFFB.Position := 8;
      end;
     if SettingsInUse.ScanFCS.ChannelCSelected then      // ***********************************************   C
      begin
       if SettingsInUse.ScanFCS.UserSaveDirectory then
        myTIFFC := TFileStream.Create(SettingsInUse.ScanFCS.SaveDir + '\' + SettingsInUse.ScanFCS.FileName +
                                     FormatFloat('000', SettingsInUse.ScanFCS.StartingVal*1.0) + 'ChC.tif', fmCreate)
       else
        myTIFFC := TFileStream.Create('TemporaryData\' + SettingsInUse.ScanFCS.HintString + 'ChC.tif', fmCreate);
       myTIFFC.Position := 8;
      end;

     if PrefFrm.SFCS_SeparOfPulses then TempErrs := _AcquireSFCS2;
     if PrefFrm.SFCS_PulsesPerGate then TempErrs := _AcquireSFCS1;
     SetLength(Result,Length(TempErrs)+CallCount);
     for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
     CallCount := CallCount + Length(TempErrs);

     if SettingsInUse.ScanFCS.ChannelASelected then
      begin
       TIFFFunc.PrepFCSTiff(myTIFFA,SettingsInUse.ScanFCS.NumberOfPts,SettingsInUse.ScanFCS.TIFFImageLengthA);
       myTIFFA.Free;
      end;
     if SettingsInUse.ScanFCS.ChannelBSelected then
      begin
       TIFFFunc.PrepFCSTiff(myTIFFB,SettingsInUse.ScanFCS.NumberOfPts,SettingsInUse.ScanFCS.TIFFImageLengthB);
       myTIFFB.Free;
      end;
     if SettingsInUse.ScanFCS.ChannelCSelected then
      begin
       TIFFFunc.PrepFCSTiff(myTIFFC,SettingsInUse.ScanFCS.NumberOfPts,SettingsInUse.ScanFCS.TIFFImageLengthC);
       myTIFFC.Free;
      end;
      if SettingsInUse.ScanFCS.UserSaveDirectory then
       begin
        inc(SettingsInUse.ScanFCS.StartingVal);
        SettingsFrm.SFCS_FNStartValEdit.Text := IntToStr(SettingsInUse.ScanFCS.StartingVal);
       end;
    end;
 9: begin
     TempErrs := _AcquireBkCount;
     SetLength(Result,Length(TempErrs)+CallCount);
     for i := 0 to (Length(TempErrs)-1) do Result[i+CallCount] := TempErrs[i];
     CallCount := CallCount + Length(TempErrs);
    end;
end;
end;




function THardwareControl.Stop: TErrorArray;
begin
SetLength(Result,9);
//  Clears the waveform generation
Result[0].StatusCode := WFM_Group_Control(AOutsDevNum,1,OpClear);
Result[0].FuncCalled := '_STOP_WFM_Group_Control';
//  Disarm counters
//  NOTE: Counters with a buffer assigned for data must repeat complete setup process.
//        Pulse generation counters can simply ND_PREPARE and ND_ARM if settings are the same.

Result[1].StatusCode := GPCTR_Control(CountersDevNum,PhCtrAHardwareCtr,ND_RESET);
Result[1].FuncCalled := '_STOP_GPCTR0_Control';
Result[2].StatusCode := GPCTR_Control(CountersDevNum,PhCtrBHardwareCtr,ND_RESET);
Result[2].FuncCalled := '_STOP_GPCTR1_Control';
Result[3].StatusCode := GPCTR_Control(CountersDevNum,PhCtrCHardwareCtr,ND_RESET);
Result[3].FuncCalled := '_STOP_GPCTR2_Control';
Result[4].StatusCode := GPCTR_Control(CountersDevNum,PixelClockHardwareCtr,ND_RESET);
Result[4].FuncCalled := '_STOP_GPCTR3_Control';
Result[5].StatusCode := GPCTR_Control(CountersDevNum,LineClockHardwareCtr,ND_RESET);
Result[5].FuncCalled := '_STOP_GPCTR4_Control';
Result[6].StatusCode := GPCTR_Control(CountersDevNum,FrameClockHardwareCtr,ND_RESET);
Result[6].FuncCalled := '_STOP_GPCTR5_Control';
Result[7].StatusCode := GPCTR_Control(CountersDevNum,ND_COUNTER_6,ND_RESET);
Result[7].FuncCalled := '_STOP_GPCTR6_Control';
Result[8].StatusCode := GPCTR_Control(CountersDevNum,ND_COUNTER_7,ND_RESET);
Result[8].FuncCalled := '_STOP_GPCTR7_Control';

GlobalUnlock(hScanRange);
GlobalFree(hScanRange);

if (PrefFrm.CenterLaser AND (SettingsInUse.ID = 1)) then _CentralPosition;
end;




procedure THardwareControl.FreeMem;
var
i: byte;
begin
GlobalUnlock(hPhotonBufferA);
GlobalUnlock(hPhotonBufferB);
GlobalUnlock(hPhotonBufferC);
GlobalUnlock(hRetrieveBufferA);
GlobalUnlock(hRetrieveBufferB);
GlobalUnlock(hRetrieveBufferC);
GlobalUnlock(hScanRange);

GlobalFree(hPhotonBufferA);
GlobalFree(hPhotonBufferB);
GlobalFree(hPhotonBufferC);
GlobalFree(hRetrieveBufferA);
GlobalFree(hRetrieveBufferB);
GlobalFree(hRetrieveBufferC);
GlobalFree(hScanRange);
end;






end.
