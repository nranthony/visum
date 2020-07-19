unit Main6;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, nidaq, nidaqcns, StdCtrls, Qt;

type

  //  Array of 16bit integer error codes returned from NI-DAQ calls
  TErrDetails = packed record
    StatusCode: SmallInt;
    FuncCalled: String;
    end;
  TErrorArray = array[0..15] of TErrDetails;
  PErrorArray = ^TErrorArray;
  TErrors = array[0..6] of TErrorArray;

  TU32Array = array[0..100000] of Cardinal;
  PU32Array = ^TU32Array;

  TForm1 = class(TForm)
    StartButton: TButton;
    SrcLowEdit: TEdit;
    SrcLowLabel: TLabel;
    SrcHighLabel: TLabel;
    SrcHighEdit: TEdit;
    Memo1: TMemo;
    CancelButton: TButton;
    WidthVarEdit: TEdit;
    WidthVarLabel: TLabel;
    usLabel1: TLabel;
    usLabel2: TLabel;
    ActSrcLowLabel: TLabel;
    ActSrcHighLabel: TLabel;
    ActGateLowLabel: TLabel;
    ActGateHighLabel: TLabel;
    SrcsPerGateLabel: TLabel;
    SrcsPerGateEdit: TEdit;
    ActSrcLowPulsesLabel: TLabel;
    ActSrcHighPulsesLabel: TLabel;
    ActGateLowPulsesLabel: TLabel;
    ActGateHighPulsesLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  procedure ShowErr();

    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  hMainBuffer: THandle;
  hMainBuffer2: THandle;
  hMainBuffer3: THandle;
  pMainBuffer: PU32Array;
  pMainBuffer2: PU32Array;
  pMainBuffer3: PU32Array;
  hRetBuffer: THandle;
  pRetBuffer: PU32Array;
  MainBufFile, RetBufFile: TextFile;
  FileString, FileString2: string;
  Start,
  Stop,
  Freq: Int64;
  Ptr: pi16;

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

ChanVect: array[0..1] of SmallInt; //  array containing channel numbers for group


Result: TErrorArray;

MainBufferCount: Cardinal;
Count: integer;
CancelAcq: Boolean;

 AOutsDevNum: SmallInt;
 CountersDevNum: SmallInt;
 AOutTrigLine: Cardinal;
 AOutTrigLinePolarity: Cardinal;
 CountersTrigLine: Cardinal;
 CountersTrigLinePolarity: Cardinal;
 SetTrigDIGState: SmallInt;
 GODIGState: Smallint;

 GenSourceHardwareCtr: Cardinal;
 GenSourceSource: Cardinal;
 GenSourceSourcePolarity: Cardinal;
 GenSourceOutputSignal: Cardinal;
 GenSourceOutputSource: Cardinal;
 GenSourceOutputPolarity: Cardinal;

 PixelHardwareCtr: Cardinal;
 PixelSource: Cardinal;
 PixelSourcePolarity: Cardinal;
 PixelOutputSignal: Cardinal;
 PixelOutputSource: Cardinal;
 PixelOutputPolarity: Cardinal;

 FrameHardwareCtr: Cardinal;
 FrameSource: Cardinal;
 FrameSourcePolarity: Cardinal;
 FrameOutputSignal: Cardinal;
 FrameOutputSource: Cardinal;
 FrameOutputPolarity: Cardinal;

 LineHardwareCtr: Cardinal;
 LineSource: Cardinal;
 LineSourcePolarity: Cardinal;
 LineOutputSignal: Cardinal;
 LineOutputSource: Cardinal;
 LineOutputPolarity: Cardinal;

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


 CntSrcLow,
 CntSrcHigh,
 CntTotalGateHigh,
 CntGateLow,
 CntGateHigh: Cardinal;
 WidthVar: integer;

 MainBufArray: array of Cardinal;
 pMainBufArray: PCardinal;
 MainBufArray2: array of Cardinal;
 pMainBufArray2: PCardinal;
 MainBufArray3: array of Cardinal;
 pMainBufArray3: PCardinal;
 RetBufArray: array of Cardinal;
 pRetBufArray: PCardinal;

 SimpleCountValue: Cardinal;
 pSimpleCountValue: PCardinal;
 PointsRequested: Cardinal;
 PointsRetrieved: Cardinal;
 PointsAvailable: Cardinal;
 PointsAvailable2: Cardinal;
 PointsAvailable3: Cardinal;


implementation

{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
var
Str: string;
begin


QueryPerformanceFrequency(Freq);

AOutsDevNum:=3;

CountersDevNum:=2;
CountersTrigLine:=ND_PFI_7;
CountersTrigLinePolarity:=ND_HIGH_TO_LOW;

GenSourceHardwareCtr:=ND_COUNTER_1;
GenSourceSource:=ND_INTERNAL_MAX_TIMEBASE;
GenSourceOutputSignal:=ND_PFI_32;
GenSourceOutputSource:=ND_GPCTR1_OUTPUT;
GenSourceOutputPolarity:=ND_LOW_TO_HIGH;

PixelHardwareCtr:=ND_COUNTER_0;
PixelSource:=ND_INTERNAL_MAX_TIMEBASE;
PixelOutputSignal:=ND_PFI_36;
PixelOutputSource:=ND_GPCTR0_OUTPUT;
PixelOutputPolarity:=ND_LOW_TO_HIGH;

FrameHardwareCtr:=ND_COUNTER_7;
FrameSource:=ND_INTERNAL_MAX_TIMEBASE;
FrameOutputSignal:=ND_PFI_8;
FrameOutputSource:=ND_GPCTR7_OUTPUT;
FrameOutputPolarity:=ND_NEGATIVE;

PhCtrAHardwareCtr:=ND_COUNTER_2;
PhCtrASource:=ND_PFI_31;
PhCtrAGate:=ND_PFI_30;

PhCtrBHardwareCtr:=ND_COUNTER_3;
PhCtrBSource:=ND_PFI_27;
PhCtrBGate:=ND_PFI_26;

PhCtrCHardwareCtr := ND_COUNTER_4;
PhCtrCSource := ND_PFI_23;
PhCtrCGate := ND_PFI_22;

PhCtrDHardwareCtr := ND_COUNTER_5;
PhCtrDSource := ND_PFI_19;
PhCtrDGate := ND_PFI_18;

PhCtrEHardwareCtr := ND_COUNTER_6;
PhCtrESource := ND_PFI_15;
PhCtrEGate := ND_PFI_14;

CancelAcq := False;
AssignFile(MainBufFile,'Count.txt');
Reset(MainBufFile);
ReadLn(MainBufFile,Str);
Count := StrToInt(Str);
CloseFile(MainBufFile);

usLabel1.Caption := Chr(Key_mu) + 's';
usLabel2.Caption := Chr(Key_mu) + 's';
end;

procedure TForm1.ShowErr;
var
i,j: integer;
begin
for i := 0 to 15 do
  if Result[i].StatusCode < 0 then
  Memo1.Lines.Add(Result[i].FuncCalled + ' ' + IntTostr(Result[i].StatusCode));
end;




procedure TForm1.StartButtonClick(Sender: TObject);
var
i,j: integer;
begin
inc(Count);
Memo1.Lines.Add('');
WidthVar := StrToInt(WidthVarEdit.Text);
CancelAcq := False;

AssignFile(RetBufFile, 'RetBufFile'+ IntTostr(Count) + '.txt');
AssignFile(MainBufFile, 'MainBufFile'+ IntTostr(Count) + '.txt');
ReWrite(MainBufFile);
ReWrite(RetBufFile);

SimpleCountValue := 0;
pSimpleCountValue := @SimpleCountValue;

MainBufferCount := WidthVar*WidthVar;

SetLength(RetBufArray,WidthVar);
SetLength(MainBufArray,MainBufferCount);
SetLength(MainBufArray2,MainBufferCount);
SetLength(MainBufArray3,MainBufferCount);

New(pMainBufArray);
New(pMainBufArray2);
New(pMainBufArray3);
New(pRetBufArray);
pMainBufArray := @MainBufArray[0];
pMainBufArray2 := @MainBufArray2[0];
pMainBufArray3 := @MainBufArray3[0];
pRetBufArray := @RetBufArray[0];

hMainBuffer := GlobalAlloc(GHND, 4*MainBufferCount);
pMainBuffer := PU32Array(GlobalLock(hMainBuffer));
hMainBuffer2 := GlobalAlloc(GHND, 4*MainBufferCount);
pMainBuffer2 := PU32Array(GlobalLock(hMainBuffer2));
hMainBuffer3 := GlobalAlloc(GHND, 4*MainBufferCount);
pMainBuffer3 := PU32Array(GlobalLock(hMainBuffer3));
hRetBuffer := GlobalAlloc(GHND, 4*WidthVar);
pRetBuffer := PU32Array(GlobalLock(hRetBuffer));

try
CntSrcLow := Round(80*StrToFloat(SrcLowEdit.Text));
ActSrcLowPulsesLabel.Caption := 'Actual pulses: ' + IntToStr(CntSrcLow);
ActSrcLowLabel.Caption := 'Actual time: ' + FloatToStr(0.0125*CntSrcLow) + Chr(Key_mu) + 's';
CntSrcHigh := Round(80*StrToFloat(SrcHighEdit.Text));
ActSrcHighLabel.Caption := 'Actual: ' + FloatToStr(0.0125*CntSrcHigh) + Chr(Key_mu) + 's';
ActSrcHighPulsesLabel.Caption := 'Actual pulses: ' + IntToStr(CntSrcHigh);

CntGateHigh := (CntSrcLow+CntSrcHigh)*StrToInt(SrcsPerGateEdit.Text);
ActGateHighLabel.Caption := 'Actual Gate High: ' + FloatToStr(0.0125*CntGateHigh) + Chr(Key_mu) + 's';
ActGateHighPulsesLabel.Caption := 'Actual pulses: ' + IntToStr(CntGateHigh);
CntGateLow := CntSrcLow+CntSrcHigh;
ActGateLowLabel.Caption := 'Actual Gate Low: ' + FloatToStr(0.0125*CntGateLow) + Chr(Key_mu) + 's';
ActGateLowPulsesLabel.Caption := 'Actual pulses: ' + IntToStr(CntGateLow);
except
Showmessage('Recheck values!');
Exit;
end;

CntTotalGateHigh := MainBufferCount*(CntGateLow+CntGateHigh);

// NOTE: All set up of hardware lines should have been done before setting up counters !!!
//  HARDWARE SETUP STUFF
Result[0].StatusCode := DIG_Line_Config(CountersDevNum, {port} 0, {line} 1, {direction} 1);
Result[0].FuncCalled := 'HardwareCONFIG_DIG_Line_Config';
//  Select signal PFI and point on which to trigger
Result[1].StatusCode := Select_Signal(CountersDevNum, ND_START_TRIGGER, CountersTrigLine, CountersTrigLinePolarity);
Result[1].FuncCalled := 'HardwareCONFIG_Select_Signal';
//   Synchronses signal using the ND_INTERNAL_MAX_TIMEBASE
Result[2].StatusCode := Line_Change_Attribute(CountersDevNum, CountersTrigLine, ND_LINE_FILTER, ND_SYNCHRONIZATION);
Result[2].FuncCalled := 'HardwareCONFIG_Line_Change_Attribute';
i := 0;
for i := 3 to 15 do Result[i].StatusCode := 0;
for i := 3 to 15 do Result[i].FuncCalled := '';
ShowErr;

//  Set Line Line High
Result[0].StatusCode := DIG_Line_Config(CountersDevNum,0,0,1);
Result[0].FuncCalled := 'LineCONFIG_DIG_Line_Config';
Result[1].StatusCode := DIG_Out_Line(CountersDevNum,0,0,1);
Result[1].FuncCalled := 'LineDIG_Out_Line';
i := 0;
for i := 2 to 15 do Result[i].StatusCode := 0;
for i := 2 to 15 do Result[i].FuncCalled := '';
ShowErr;

//  SET TRIGGER STUFF
//  Set digital output line high - This line is wired to PFI_7 as default.
//  Setting this line low triggers the counter start trigger selected next.
Result[0].StatusCode := DIG_Out_Line(CountersDevNum, {port} 0, {line} 1, {state} 1);
Result[0].FuncCalled := '_SetTrigger_DIG_Out_Line';
i := 0;
for i := 1 to 15 do Result[i].StatusCode := 0;
for i := 1 to 15 do Result[i].FuncCalled := '';
ShowErr;


//  MAIN COUNTER SETUP
// Returns counter to power on state without resetting the polarity
Result[0].StatusCode := GPCTR_Control(CountersDevNum,  PhCtrAHardwareCtr, ND_RESET);
Result[0].FuncCalled := '_SetPhotonCounterA_GPCTR_Control';
// Sets a pulse train generation on Counter
Result[1].StatusCode := GPCTR_Set_Application(CountersDevNum,  PhCtrAHardwareCtr, ND_BUFFERED_PULSE_WIDTH_MSR);
Result[1].FuncCalled := '_SetPhotonCounterA_GPCTR_Set_Application';
// Assign buffer for count information - third input is reserved and set to zero
Result[2].StatusCode := GPCTR_Config_Buffer(CountersDevNum,  PhCtrAHardwareCtr, 0, MainBufferCount, pu32(pMainBufArray));
//Result[2].StatusCode := GPCTR_Config_Buffer(CountersDevNum,  PhCtrAHardwareCtr, 0, MainBufferCount, pu32(pMainBuffer));
Result[2].FuncCalled := '_SetPhotonCounterA_GPCTR_Config_Buffer';
// Set starting value of counter - counts from this value
Result[3].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrAHardwareCtr, ND_INITIAL_COUNT, 0);
Result[3].FuncCalled := '_SetPhotonCounterA_GPCTR_Change_Parameter';
// Sets buffer mode
Result[4].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrAHardwareCtr, ND_BUFFER_MODE, ND_CONTINUOUS);
Result[4].FuncCalled := '_SetPhotonCounterA_GPCTR_Change_Parameter';
// Sets hardware triggering
Result[5].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrAHardwareCtr, ND_START_TRIGGER, ND_ENABLED);
Result[5].FuncCalled := '_SetPhotonCounterA_GPCTR_Change_Parameter';
// Synchronous counting
Result[6].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrAHardwareCtr, ND_COUNTING_SYNCHRONOUS, ND_YES);
Result[6].FuncCalled := '_SetPhotonCounterA_GPCTR_Change_Parameter';
// Arms counter
Result[7].StatusCode := GPCTR_Control(CountersDevNum,  PhCtrAHardwareCtr, ND_PROGRAM);
Result[7].FuncCalled := '_SetPhotonCounterA_GPCTR_Control';
i := 0;
for i := 8 to 15 do Result[i].StatusCode := 0;
for i := 8 to 15 do Result[i].FuncCalled := '';
ShowErr;

//  MAIN COUNTER 2 SETUP
// Returns counter to power on state without resetting the polarity
Result[0].StatusCode := GPCTR_Control(CountersDevNum,  PhCtrBHardwareCtr, ND_RESET);
Result[0].FuncCalled := '_SetPhotonCounterB_GPCTR_Control';
// Sets a pulse train generation on Counter
Result[1].StatusCode := GPCTR_Set_Application(CountersDevNum,  PhCtrBHardwareCtr, ND_BUFFERED_PULSE_WIDTH_MSR);
Result[1].FuncCalled := '_SetPhotonCounterB_GPCTR_Set_Application';
// Assign buffer for count information - third input is reserved and set to zero
Result[2].StatusCode := GPCTR_Config_Buffer(CountersDevNum,  PhCtrBHardwareCtr, 0, MainBufferCount, pu32(pMainBufArray2));
//Result[2].StatusCode := GPCTR_Config_Buffer(CountersDevNum,  PhCtrBHardwareCtr, 0, MainBufferCount, pu32(pMainBuffer2));
Result[2].FuncCalled := '_SetPhotonCounterB_GPCTR_Config_Buffer';
// Set starting value of counter - counts from this value
Result[3].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrBHardwareCtr, ND_INITIAL_COUNT, 0);
Result[3].FuncCalled := '_SetPhotonCounterB_GPCTR_Change_Parameter';
// Sets buffer mode
Result[4].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrBHardwareCtr, ND_BUFFER_MODE, ND_CONTINUOUS);
Result[4].FuncCalled := '_SetPhotonCounterB_GPCTR_Change_Parameter';
// Sets hardware triggering
Result[5].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrBHardwareCtr, ND_START_TRIGGER, ND_ENABLED);
Result[5].FuncCalled := '_SetPhotonCounterB_GPCTR_Change_Parameter';
// Synchronous counting
Result[6].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrBHardwareCtr, ND_COUNTING_SYNCHRONOUS, ND_YES);
Result[6].FuncCalled := '_SetPhotonCounterB_GPCTR_Change_Parameter';
// Arms counter
Result[7].StatusCode := GPCTR_Control(CountersDevNum,  PhCtrBHardwareCtr, ND_PROGRAM);
Result[7].FuncCalled := '_SetPhotonCounterB_GPCTR_Control';
i := 0;
for i := 8 to 15 do Result[i].StatusCode := 0;
for i := 8 to 15 do Result[i].FuncCalled := '';
ShowErr;

//  MAIN COUNTER 3 SETUP
// Returns counter to power on state without resetting the polarity
Result[0].StatusCode := GPCTR_Control(CountersDevNum,  PhCtrCHardwareCtr, ND_RESET);
Result[0].FuncCalled := '_SetPhotonCounterB_GPCTR_Control';
// Sets a pulse train generation on Counter
Result[1].StatusCode := GPCTR_Set_Application(CountersDevNum,  PhCtrCHardwareCtr, ND_BUFFERED_PULSE_WIDTH_MSR);
Result[1].FuncCalled := '_SetPhotonCounterB_GPCTR_Set_Application';
// Assign buffer for count information - third input is reserved and set to zero
Result[2].StatusCode := GPCTR_Config_Buffer(CountersDevNum,  PhCtrCHardwareCtr, 0, MainBufferCount, pu32(pMainBufArray3));
//Result[2].StatusCode := GPCTR_Config_Buffer(CountersDevNum,  PhCtrCHardwareCtr, 0, MainBufferCount, pu32(pMainBuffer3));
Result[2].FuncCalled := '_SetPhotonCounterB_GPCTR_Config_Buffer';
// Set starting value of counter - counts from this value
Result[3].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrCHardwareCtr, ND_INITIAL_COUNT, 0);
Result[3].FuncCalled := '_SetPhotonCounterB_GPCTR_Change_Parameter';
// Sets buffer mode
Result[4].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrCHardwareCtr, ND_BUFFER_MODE, ND_CONTINUOUS);
Result[4].FuncCalled := '_SetPhotonCounterB_GPCTR_Change_Parameter';
// Sets hardware triggering
Result[5].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrCHardwareCtr, ND_START_TRIGGER, ND_ENABLED);
Result[5].FuncCalled := '_SetPhotonCounterB_GPCTR_Change_Parameter';
// Synchronous counting
Result[6].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrCHardwareCtr, ND_COUNTING_SYNCHRONOUS, ND_YES);
Result[6].FuncCalled := '_SetPhotonCounterB_GPCTR_Change_Parameter';
// Arms counter
Result[7].StatusCode := GPCTR_Control(CountersDevNum,  PhCtrCHardwareCtr, ND_PROGRAM);
Result[7].FuncCalled := '_SetPhotonCounterB_GPCTR_Control';
i := 0;
for i := 8 to 15 do Result[i].StatusCode := 0;
for i := 8 to 15 do Result[i].FuncCalled := '';
ShowErr;

//  COUNT COUNTER SETUP
// Returns counter to power on state without resetting the polarity
Result[0].StatusCode := GPCTR_Control(CountersDevNum,  PhCtrEHardwareCtr, ND_RESET);
Result[0].FuncCalled := '_SetPhotonCounterA_GPCTR_Control';
//  Sets simple event counting
Result[1].StatusCode := GPCTR_Set_Application(CountersDevNum,  PhCtrEHardwareCtr, ND_SIMPLE_EVENT_CNT);
Result[1].FuncCalled := '_SetPhotonCounterA_GPCTR_Set_Application';
// Set starting value of counter - counts from this value
Result[2].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrEHardwareCtr, ND_INITIAL_COUNT, 0);
Result[2].FuncCalled := '_SetPhotonCounterA_GPCTR_Change_Parameter';
// Sets hardware triggering
Result[3].StatusCode := GPCTR_Change_Parameter(CountersDevNum,  PhCtrEHardwareCtr, ND_START_TRIGGER, ND_ENABLED);
Result[3].FuncCalled := '_SetPhotonCounterA_GPCTR_Change_Parameter';
// Arms counter
Result[4].StatusCode := GPCTR_Control(CountersDevNum,  PhCtrEHardwareCtr, ND_PROGRAM);
Result[4].FuncCalled := '_SetPhotonCounterA_GPCTR_Control';
i := 0;
for i := 5 to 15 do Result[i].StatusCode := 0;
for i := 5 to 15 do Result[i].FuncCalled := '';
ShowErr;

// CNT SOURCE SETUP
// Returns counter to power on state without resetting the polarity
Result[0].StatusCode := GPCTR_Control(CountersDevNum, GenSourceHardwareCtr, ND_RESET);
Result[0].FuncCalled := '_SetSourceCounter_GPCTR_Control';
//  Pulse train generation
Result[1].StatusCode := GPCTR_Set_Application(CountersDevNum, GenSourceHardwareCtr, ND_PULSE_TRAIN_GNR);
Result[1].FuncCalled := '_SetSourceCounter_GPCTR_Set_Application';
// Sets hardware triggering
Result[2].StatusCode := GPCTR_Change_Parameter(CountersDevNum, GenSourceHardwareCtr, ND_START_TRIGGER, ND_ENABLED);
Result[2].FuncCalled := '_SetSourceCounter_GPCTR_Change_Parameter';
// Sets the source of the generation to the max timebase - 80MHz for this pci card
Result[3].StatusCode := GPCTR_Change_Parameter(CountersDevNum, GenSourceHardwareCtr, ND_SOURCE, ND_INTERNAL_MAX_TIMEBASE);
Result[3].FuncCalled := '_SetSourceCounter_GPCTR_Change_Parameter';
//  How many max timebase 'clicks' for the Low part of wave
Result[4].StatusCode := GPCTR_Change_Parameter(CountersDevNum, GenSourceHardwareCtr, ND_COUNT_1, CntSrcLow);
Result[4].FuncCalled := '_SetSourceCounter_GPCTR_Change_Parameter';
//  How many max timebase 'clicks' for the High part of wave
Result[5].StatusCode := GPCTR_Change_Parameter(CountersDevNum, GenSourceHardwareCtr, ND_COUNT_2, CntSrcHigh);
Result[5].FuncCalled := '_SetSourceCounter_GPCTR_Change_Parameter';
//  Slect signal and point on signal for triggering
Result[6].StatusCode := Select_Signal(CountersDevNum, GenSourceOutputSignal,
                           GenSourceOutputSource, GenSourceOutputPolarity);
Result[6].FuncCalled := '_SetSourceCounter_Select_Signal';
// PROGRAM
Result[7].StatusCode := GPCTR_Control(CountersDevNum, GenSourceHardwareCtr, ND_PROGRAM);
Result[7].FuncCalled := '_SetSourceCounter_GPCTR_Control';
i := 0;
for i := 8 to 15 do Result[i].StatusCode := 0;
for i := 8 to 15 do Result[i].FuncCalled := '';
ShowErr;


//  PIXEL LINE
// Returns counter to power on state without resetting the polarity
Result[0].StatusCode := GPCTR_Control(CountersDevNum, PixelHardwareCtr, ND_RESET);
Result[0].FuncCalled := '_SetPixelCounter_GPCTR_Control';
//  Pulse train generation
Result[1].StatusCode := GPCTR_Set_Application(CountersDevNum, PixelHardwareCtr, ND_PULSE_TRAIN_GNR);
Result[1].FuncCalled := '_SetPixelCounter_GPCTR_Set_Application';
// Sets hardware triggering
Result[2].StatusCode := GPCTR_Change_Parameter(CountersDevNum, PixelHardwareCtr, ND_START_TRIGGER, ND_ENABLED);
Result[2].FuncCalled := '_SetPixelCounter_GPCTR_Change_Parameter';
// Sets the source of the generation to the max timebase - 80MHz for this pci card
Result[3].StatusCode := GPCTR_Change_Parameter(CountersDevNum, PixelHardwareCtr, ND_SOURCE, ND_INTERNAL_MAX_TIMEBASE);
Result[3].FuncCalled := '_SetPixelCounter_GPCTR_Change_Parameter';
//  How many max timebase 'clicks'
Result[4].StatusCode := GPCTR_Change_Parameter(CountersDevNum, PixelHardwareCtr, ND_COUNT_1, CntGateLow);
Result[4].FuncCalled := '_SetPixelCounter_GPCTR_Change_Parameter';
//  How many max timebase 'clicks'
Result[5].StatusCode := GPCTR_Change_Parameter(CountersDevNum, PixelHardwareCtr, ND_COUNT_2, CntGateHigh);
Result[5].FuncCalled := '_SetPixelCounter_GPCTR_Change_Parameter';
//  Slect signal and point on signal for triggering
Result[6].StatusCode := Select_Signal(CountersDevNum, PixelOutputSignal,
                         PixelOutputSource, PixelOutputPolarity);
Result[6].FuncCalled := '_SetPixelCounter_Select_Signal';
//  ND_PROGRAM
Result[7].StatusCode := GPCTR_Control(CountersDevNum, PixelHardwareCtr, ND_PROGRAM);
Result[7].FuncCalled := '_SetPixelCounter_GPCTR_Control';
i := 0;
for i := 8 to 15 do Result[i].StatusCode := 0;
for i := 8 to 15 do Result[i].FuncCalled := '';
ShowErr;


//  Frame LINE
// Returns counter to power on state without resetting the polarity
Result[0].StatusCode := GPCTR_Control(CountersDevNum, FrameHardwareCtr, ND_RESET);
Result[0].FuncCalled := '_SetFrameCounter_GPCTR_Control';
//  Pulse train generation
Result[1].StatusCode := GPCTR_Set_Application(CountersDevNum, FrameHardwareCtr, ND_PULSE_TRAIN_GNR);
Result[1].FuncCalled := '_SetFrameCounter_GPCTR_Set_Application';
// Sets hardware triggering
Result[2].StatusCode := GPCTR_Change_Parameter(CountersDevNum, FrameHardwareCtr, ND_START_TRIGGER, ND_ENABLED);
Result[2].FuncCalled := '_SetFrameCounter_GPCTR_Change_Parameter';
// Sets the source of the generation to the max timebase - 80MHz for this pci card
Result[3].StatusCode := GPCTR_Change_Parameter(CountersDevNum, FrameHardwareCtr, ND_SOURCE, ND_INTERNAL_MAX_TIMEBASE);
Result[3].FuncCalled := '_SetFrameCounter_GPCTR_Change_Parameter';
//  How many max timebase 'clicks'
Result[4].StatusCode := GPCTR_Change_Parameter(CountersDevNum, FrameHardwareCtr, ND_COUNT_1, CntTotalGateHigh);
Result[4].FuncCalled := '_SetFrameCounter_GPCTR_Change_Parameter';
//  How many max timebase 'clicks'
Result[5].StatusCode := GPCTR_Change_Parameter(CountersDevNum, FrameHardwareCtr, ND_COUNT_2, 4294967290);
Result[5].FuncCalled := '_SetFrameCounter_GPCTR_Change_Parameter';
Result[6].StatusCode := GPCTR_Change_Parameter(CountersDevNum, FrameHardwareCtr,
                                              ND_OUTPUT_POLARITY, FrameOutputPolarity);
Result[6].FuncCalled := '_SetFrameCounter_GPCTR_Change_Parameter';
//  Slect signal and point on signal for triggering
Result[7].StatusCode := Select_Signal(CountersDevNum, FrameOutputSignal,
                          FrameOutputSource, FrameOutputPolarity);
Result[7].FuncCalled := '_SetFrameCounter_Select_Signal';
//  ND_PROGRAM
Result[8].StatusCode := GPCTR_Control(CountersDevNum, FrameHardwareCtr, ND_PROGRAM);
Result[8].FuncCalled := '_SetFrameCounter_GPCTR_Control';
i := 0;
for i := 9 to 15 do Result[i].StatusCode := 0;
for i := 9 to 15 do Result[i].FuncCalled := '';
ShowErr;


//  GO STUFF
// Start waveform generation at analog outputs
Result[0].StatusCode := 0;//WFM_Group_Control(AOutsDevNum, Group, OpSTART);
Result[0].FuncCalled := '_GO_WFM_Group_Control';
// Set Digital Line 1 of Port 0 to Low to trigger the counters
Result[1].StatusCode := DIG_Out_Line(CountersDevNum, {Port} 0, {Line} 1, {State} 0);
Result[1].FuncCalled := '_GO_DIG_Out_Line';
i := 0;
for i := 2 to 15 do Result[i].StatusCode := 0;
for i := 2 to 15 do Result[i].FuncCalled := '';
ShowErr;


GPCTR_Watch(CountersDevNum,PhCtrAHardwareCtr,ND_BUFFER_SIZE,@PointsAvailable);
Memo1.Lines.Add('Buffer Size Available: ' + IntToStr(PointsAvailable));
GPCTR_Watch(CountersDevNum,PhCtrAHardwareCtr,ND_ELEMENT_SIZE,@PointsAvailable);
Memo1.Lines.Add('Element Size: ' + IntToStr(PointsAvailable) + 'bytes.');

GPCTR_Watch(CountersDevNum,PhCtrBHardwareCtr,ND_BUFFER_SIZE,@PointsAvailable);
Memo1.Lines.Add('Buffer2 Size Available: ' + IntToStr(PointsAvailable));
GPCTR_Watch(CountersDevNum,PhCtrBHardwareCtr,ND_ELEMENT_SIZE,@PointsAvailable);
Memo1.Lines.Add('Element2 Size: ' + IntToStr(PointsAvailable) + 'bytes.');

GPCTR_Watch(CountersDevNum,PhCtrCHardwareCtr,ND_BUFFER_SIZE,@PointsAvailable);
Memo1.Lines.Add('Buffer3 Size Available: ' + IntToStr(PointsAvailable));
GPCTR_Watch(CountersDevNum,PhCtrCHardwareCtr,ND_ELEMENT_SIZE,@PointsAvailable);
Memo1.Lines.Add('Element3 Size: ' + IntToStr(PointsAvailable) + 'bytes.');

FileString := '';
Result[1].FuncCalled := '_GetArrayLineA_GPCTR_Watch';
Result[0].FuncCalled := '_GetArrayLineA';

  QueryPerformanceCounter(Start);

for j := 0 to WidthVar-1 do
 begin
  PointsRequested := WidthVar;
  PointsRetrieved := 0;
  PointsAvailable := 0;
  PointsAvailable2 := 0;
  PointsAvailable3 := 0;
  Result[0].StatusCode := 0;
  Result[1].StatusCode := 0;
  Result[2].StatusCode := 0;

  // -10920 gpctrDataLossError
  // One or more data points may have been lost during buffered
  // GPCTR operations due to speed limitations of your system.
  while
   ({OR2}   ({OR1}   ((*1*)(PointsAvailable < PointsRequested)  AND
                     ((Result[1].StatusCode=0)OR(Result[1].StatusCode=-10920))(*1*))
                   OR
                     ((*2*)(PointsAvailable2 < PointsRequested) AND
                     ((Result[2].StatusCode=0)OR(Result[2].StatusCode=-10920))(*2*))   {OR1})
                  OR
                     ((*3*)(PointsAvailable3 < PointsRequested) AND
                     ((Result[3].StatusCode=0)OR(Result[3].StatusCode=-10920))(*3*))            {OR2} )
          do

         begin
          Result[1].StatusCode := GPCTR_Watch(CountersDevNum, PhCtrAHardwareCtr,
                                              ND_AVAILABLE_POINTS, @PointsAvailable);
          Result[2].StatusCode := GPCTR_Watch(CountersDevNum, PhCtrBHardwareCtr,
                                              ND_AVAILABLE_POINTS, @PointsAvailable2);
          Result[3].StatusCode := GPCTR_Watch(CountersDevNum, PhCtrCHardwareCtr,
                                              ND_AVAILABLE_POINTS, @PointsAvailable3);
          Memo1.Lines.Add('PtsAvail: ' + IntToStr(PointsAvailable) +
                          'PtsAvail2: ' + IntToStr(PointsAvailable2) +
                          'PtsAvail3: ' + IntToStr(PointsAvailable3) + ', Sweep: ' + IntToStr(j));
          Application.ProcessMessages;
          if CancelAcq then Break;
         end;

  if Result[1].StatusCode < 0 then Memo1.Lines.Add(IntToStr(Result[1].StatusCode));
  if Result[2].StatusCode < 0 then Memo1.Lines.Add(IntToStr(Result[2].StatusCode));
  if Result[3].StatusCode < 0 then Memo1.Lines.Add(IntToStr(Result[3].StatusCode));

  Result[4].FuncCalled := '_GetArrayLineA_GPCTR_Read_Buffer';
  Result[4].StatusCode := GPCTR_Read_Buffer(CountersDevNum, PhCtrAHardwareCtr, ND_READ_MARK,
                              {ReadOffset} 0, PointsRequested, {TimeOut} 1.0,
                                        @PointsRetrieved, pu32(pRetBufArray));
  if Result[4].StatusCode < 0 then Memo1.Lines.Add(IntToStr(Result[4].StatusCode));

  for i := 0 to WidthVar-1 do FileString := FileString + ', ' + IntToStr(RetBufArray[i]);
  //for i := 0 to WidthVar-1 do FileString := FileString + ', ' + IntToStr(pRetBuffer^[i]);
  FileString := FileString + ' RetBuf1';
  WriteLn(RetBufFile,FileString);
  FileString := '';

  Result[5].FuncCalled := '_GetArrayLineB_GPCTR_Read_Buffer';
  Result[5].StatusCode := GPCTR_Read_Buffer(CountersDevNum, PhCtrBHardwareCtr, ND_READ_MARK,
                              {ReadOffset} 0, PointsRequested, {TimeOut} 1.0,
                                        @PointsRetrieved, pu32(pRetBufArray));
  if Result[5].StatusCode < 0 then Memo1.Lines.Add(IntToStr(Result[5].StatusCode));

  for i := 0 to WidthVar-1 do FileString := FileString + ', ' + IntToStr(RetBufArray[i]);
  //for i := 0 to WidthVar-1 do FileString := FileString + ', ' + IntToStr(pRetBuffer^[i]);
  FileString := FileString + ' RetBuf2';
  WriteLn(RetBufFile,FileString);
  FileString := '';

  Result[6].FuncCalled := '_GetArrayLineC_GPCTR_Read_Buffer';
  Result[6].StatusCode := GPCTR_Read_Buffer(CountersDevNum, PhCtrCHardwareCtr, ND_READ_MARK,
                              {ReadOffset} 0, PointsRequested, {TimeOut} 1.0,
                                        @PointsRetrieved, pu32(pRetBufArray));
  if Result[6].StatusCode < 0 then Memo1.Lines.Add(IntToStr(Result[6].StatusCode));

  for i := 0 to WidthVar-1 do FileString := FileString + ', ' + IntToStr(RetBufArray[i]);
  //for i := 0 to WidthVar-1 do FileString := FileString + ', ' + IntToStr(pRetBuffer^[i]);
  FileString := FileString + ' RetBuf3';
  WriteLn(RetBufFile,FileString);
  FileString := '';

  Application.ProcessMessages;
  if CancelAcq then Break;
  i := 0;
  for i := 2 to 15 do Result[i].StatusCode := 0;
  for i := 2 to 15 do Result[i].FuncCalled := '';
  ShowErr;

 end;

 Result[0].StatusCode := GPCTR_Watch(CountersDevNum,PhCtrEHardwareCtr,ND_COUNT,pu32(pSimpleCountValue));
 if Result[0].StatusCode < 0 then Memo1.Lines.Add(IntToStr(Result[0].StatusCode));
 Memo1.Lines.Add('Gate pulses: ' + IntToStr(SimpleCountValue));

   QueryPerformanceCounter(Stop);
  Memo1.Lines.Add('Time: ' + FloatToStr((Stop-Start)/Freq));




//  STOP STUFF
//  Clears the waveform generation
Result[0].StatusCode := 0;//WFM_Group_Control(AOutsDevNum,1,OpClear);
Result[0].FuncCalled := '_STOP_WFM_Group_Control';
//  Disarm counters
//  NOTE: Counters with a buffer assigned for data must repeat complete setup process.
//        Pulse generation counters can simply ND_PROGRAM and ND_ARM if settings are the same.
// ND_COUNTER_0 is PhotonCounterA
Result[1].StatusCode := GPCTR_Control(CountersDevNum,ND_COUNTER_0,ND_DISARM);
Result[1].FuncCalled := '_STOP_GPCTR_Control';
// ND_COUNTER_1 is Pixel Counter
Result[2].StatusCode := GPCTR_Control(CountersDevNum,ND_COUNTER_1,ND_DISARM);
Result[2].FuncCalled := '_STOP_GPCTR_Control';
// ND_COUNTER_2 is Line Counter
Result[3].StatusCode := GPCTR_Control(CountersDevNum,ND_COUNTER_2,ND_DISARM);
Result[3].FuncCalled := '_STOP_GPCTR_Control';
// ND_COUNTER_3 is Frame Counter
Result[4].StatusCode := GPCTR_Control(CountersDevNum,ND_COUNTER_3,ND_DISARM);
Result[4].FuncCalled := '_STOP_GPCTR_Control';
// ND_COUNTER_4 is PhotonCounterB
Result[5].StatusCode := GPCTR_Control(CountersDevNum,ND_COUNTER_4,ND_DISARM);
Result[5].FuncCalled := '_STOP_GPCTR_Control';
// ND_COUNTER_5 is Check Source test counter
Result[6].StatusCode := GPCTR_Control(CountersDevNum,ND_COUNTER_5,ND_DISARM);
Result[6].FuncCalled := '_STOP_GPCTR_Control';
// Clears all remaining counters
Result[7].StatusCode := GPCTR_Control(CountersDevNum,ND_COUNTER_6,ND_DISARM);
Result[7].FuncCalled := '_STOP_GPCTR_Control';
Result[8].StatusCode := GPCTR_Control(CountersDevNum,ND_COUNTER_7,ND_DISARM);
Result[8].FuncCalled := '_STOP_GPCTR_Control';
i := 0;
for i := 9 to 15 do Result[i].StatusCode := 0;
for i := 9 to 15 do Result[i].FuncCalled := '';
ShowErr;


FileString := 'Main1   Main2   Main3';
WriteLn(MainBufFile,FileString);

for i := 0 to MainBufferCount-1 do
 begin
  FileString := IntToStr(MainBufArray[i]) + '      '
              + IntToStr(MainBufArray2[i]) + '      '
              + IntToStr(MainBufArray3[i]);
  WriteLn(MainBufFile,FileString);
 end;

(*
for i := 0 to MainBufferCount-1 do
 begin
  FileString := IntToStr(pMainBuffer[i]) + '      '
              + IntToStr(pMainBuffer2[i]) + '      '
              + IntToStr(pMainBuffer3[i]);
  WriteLn(MainBufFile,FileString);
 end;
*)

GlobalUnlock(hMainBuffer);
GlobalUnlock(hMainBuffer2);
GlobalUnlock(hMainBuffer3);
GlobalUnlock(hRetBuffer);

GlobalFree(hMainBuffer);
GlobalFree(hMainBuffer2);
GlobalFree(hMainBuffer3);
GlobalFree(hRetBuffer);

CloseFile(MainBufFile);
CloseFile(RetBufFile);

pMainBufArray := nil;
pMainBufArray2 := nil;
pMainBufArray3 := nil;
pRetBufArray := nil;

Finalize(RetBufArray);
Finalize(MainBufArray);
Finalize(MainBufArray2);
Finalize(MainBufArray3);

Memo1.Lines.add('Complete' + TimeToStr(Now));
end;


procedure TForm1.CancelButtonClick(Sender: TObject);
begin
CancelAcq := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
AssignFile(MainBufFile,'Count.txt');
Rewrite(MainBufFile);
WriteLn(MainBufFile,IntToStr(Count));
CloseFile(MainBufFile);
end;



end.
