unit LabJackIO;

//Bill Waslo, Liberty Instruments Inc.  11/03

//This provides access to the Labjack DLL (via dynamic loading of the DLL,
// so programs including this can still run even if Labjack software is not installed).
//
//The DLL calls are wrapped in Delphi functions, which return a "Labjack not
//found" error if  the driver DLL is not found.  The wrapper functions also 
//allow the functions to be exposed to user-written scripts, when using
//scripting engines such as "Dream Scripter" (which cannot call DLLs themselves).
//
//**************************
//Also included is a TSimpleLabJack object ("LJ"), which allows extremely simple
//access to a single Labjack device for common needs, through use of properties.
//
//  Some example usages of the LJ object:
//
//    v := LJ.InVoltage[1];
//    LJ.OutVoltage[0] := 1.234;
//    n := LJ.Counter;
//    ReadSomeDataLines := LJ.InByte;
//
//***************************

interface

Uses Dialogs,Windows,Sysutils,Classes;

Type
   //parameter types, script users should be provided these definitions
   t4Integers = array[0..3] of Integer;
   tErrorString = array[0..49] of Char;
   tVoltages = array[0..3] of Single;
   tVBuffer = array[0..4095] of tVoltages;
   tIOBuffer = array[0..4095] of Integer;
   tCalMatrix = array[0..19] of Integer;
   tCalMatrixList = array[0..126] of tCalMatrix;
   t127Integers = array[0..126] of Integer;

  TSimpleLabJack = class(TPersistent)
  //to make properties available for ONE Labjack, and to greatly simplify user programming
  private
    //Get/Set private functions to support the properties
    function GetAnalogGain(line: integer): integer;
    function GetInAO(line: integer): single; //line:0-7 single ended, 8-11:differential
    function GetInByte: integer;
    function GetInIO(line: integer): integer;
    procedure SetAnalogGain(line: integer; const Value: integer);
    procedure SetOutAO(line: integer; const Value: single);
    procedure SetOutByte(const Value: integer);
    procedure SetOutIO(line: integer; const Value: integer);
    function GetCounter: integer;
    function GetSimpleLJID: integer;
    procedure SetSimpleLJID(const Value: integer);//legal values=1,2,4,5,8,10,16,20
  protected
  public
    property OutByte:integer write SetOutByte; //data lines 0-7
    property InByte:integer read GetInByte;  //data lines 8-15
    Property OutIO[line:integer]:integer write SetOutIO;
       //sets pin to an output and writes it (0,or nonzero);
    Property InIO[line:integer]:integer read GetInIO;//makes line be an input, reads it (1 or 0)
    Property OutVoltage[line:integer]:single write SetOutAO;//sets voltage, 2 channels
    Property InVoltage[line:integer]:single read GetInAO;
      //line:0-7 single ended, 8-11:differential
    Property AnalogGain[line:integer]:integer read GetAnalogGain write SetAnalogGain;
      //only work for differential, else gain=1
    Property Counter:integer read GetCounter;
    procedure ResetCounter;
    Property SimpleLJID: integer read GetSimpleLJID write SetSimpleLJID;
  end;


//These are the Labjack function type definitions 
//(these types are not intended for the user, but are
//  declared to allow the DLL to be loaded dynamically!):

   TljAISample=function (var idnum: Integer; demo: Integer; var stateIO: Integer;
                        updateIO: Integer; ledOn: Integer; numChannels: Integer;
                        var channels: t4Integers; var gains: t4Integers;
                        disableCal: Integer;  var overVoltage: Integer;
                        var voltages: tVoltages): Integer stdcall;

   TljAIBurst= function (var idnum: Integer; demo: Integer; stateIOin: Integer;
                        updateIO: Integer; ledOn: Integer; numChannels: Integer;
                        var channels: t4Integers; var gains: t4Integers;
                        var scanRate: single; disableCal: Integer;
                        triggerIO: Integer; triggerState: Integer;
                        numScans: Integer; timeout: Integer;
                        var voltages: tVoltages;
                        var stateIOout: Integer; var overVoltage: Integer;
                        transferMode: Integer): Integer stdcall;

   TljAIStreamStart= function (var idnum: Integer; demo: Integer; stateIOin: Integer;
                        updateIO: Integer; ledOn: Integer; numChannels: Integer;
                        var channels: t4Integers; var gains: t4Integers;
                        var scanRate: Single; disableCal: Integer;
                        reserved1: Integer; reserved2: Integer): Integer stdcall;

   TljAIStreamRead= function (localID: Integer; numScans: Integer; timeout: Integer;
	     	        var voltages: tVBuffer; var stateIOout: tIOBuffer;
                        var reserved: Integer;  var ljScanBacklog: Integer;
                        var overVoltage: Integer): Integer stdcall;

   TljAIStreamClear = function (localID: Integer): Integer stdcall;

   TljAOUpdate= function (var idnum:integer; demo: Integer; trisD: Integer; trisIO: Integer;
	   	        var stateD: Integer; var stateIO: Integer;
	   	        updateDigital: Integer; resetCounter: Integer;
	   	        var count: LongWord; analogOut0: Single;
                        analogOut1: Single): Integer stdcall;

   TljBitsToVolts= function (chnum: Integer; chgain: Integer; bits: Integer;
                        volts: Single): Integer stdcall;

   TljVoltsToBits= function (chnum: Integer; chgain: Integer; volts: Single;
		        var bits: Integer): Integer stdcall;

   TljCounter= function (var pl_idnum:integer; demo: Integer; var stateD: Integer;
                        var stateIO: Integer;resetCounter: Integer;
                        EnableStb:integer; var count: LongWord): Integer stdcall;

   TljDigitalIO= function (var pl_idnum:integer; demo: Integer;	var trisD: Integer;
                        trisIO: Integer; var stateD: Integer;
                        var stateIO: Integer; updateDigital: Integer;
                        var outputD: Integer): Integer stdcall;

   TljGetDriverVersion= function (): Single stdcall;

   TljGetErrorString= procedure (errorcode: Integer; var errorString: tErrorString) stdcall;

   TljGetFirmwareVersion= function (var idnum: Integer): Single stdcall;

   TljListAll= function (var pl_productIDList: Integer; var serialnumList: t127Integers;
	       	        var localIDList: t127Integers;var powerList: t127Integers;
                        var calMatrixList: tCalMatrixList; var numberFound: Integer;
	    	        var fcddMaxSize: Integer; var hvcMaxSize: Integer): Integer stdcall;

   TljLocalID= function (var idnum: Integer; localID: Integer): Integer stdcall;

   TljReEnum= function (var idnum: Integer): Integer stdcall;

   TljReset= function (var idnum: Integer): Integer stdcall;

   TljWatchdog= function (var idnum: Integer; demo: Integer; active: Integer;
                        timeout: Integer; reset: Integer; activeD0: Integer;
                        activeD1: Integer; activeD8: Integer;
                        stateD0: Integer;stateD1: Integer;
                        stateD8: Integer): Integer stdcall;

   TljReadMem= function (var idnum: Integer; address: Integer; var data3: Integer;
			var data2: Integer; var data1: Integer;
		        var data0: Integer): Integer stdcall;

   TljWriteMem= function (var idnum: Integer; unlocked: Integer; address: Integer;
		       data3: Integer; data2: Integer; data1: Integer;
		       data0: Integer): Integer stdcall;

   TljEDigitalIn= function (var idnum:integer; demo:integer; channel:integer;
                         readD:integer; var state:integer):integer stdcall;

   TljEDigitalOut= function (var idnum:integer; demo:integer; channel:integer;
                         writeD:integer; state:integer):integer stdcall;

   TljEAnalogIn= function (var idnum:integer; demo:integer; channel:integer;
                         gain:integer; var overvoltage:integer;
                         var voltage:single):integer stdcall;

   TljEAnalogOut= function(var idnum:integer; demo:integer; analogOut0:single;
                         analogOut1:single):integer stdcall;

   TljECount= function(var idnum:integer; demo:integer; resetCounter:integer;
                         var count:double; var ms:double):integer stdcall;


var
  LJ: TSimpleLabJack;
  LabjackDLLFound:boolean = false;
  LJ_LibHandle: THandle=0;
  LastLJNPtime: TDateTime=0;
  LJGains: t4Integers; //1,2,4,5,8,10,16,20
  fSimpleLJID: integer=-1; //default is "find first"
  ljTrisIO: integer=0;
  ljStateIO: integer=0;
  LJanalogOut0: single=0;
  LJanalogOut1: single=0;

  //support background functions
  Function LJ_DLLloaded:boolean;
  Function LJGetProcAddress(var ProcPtr:Pointer;ProcName:string):Integer;

  //exposed Labjack functions:
  function LJ_EDigitalOut(var idnum:integer; demo:integer; channel:integer;
                         writeD:integer; state:integer):integer;

  function LJ_EDigitalIn(var idnum:integer; demo:integer; channel:integer;
                         writeD:integer; var state:integer):integer;

  function LJ_EAnalogIn(var idnum:integer; demo:integer; channel:integer;
                         gain:integer; var overvoltage:integer;
                         var voltage:single):integer;

  function LJ_EAnalogOut(var idnum:integer; demo:integer; analogOut0:single;
                         analogOut1:single):integer;

  function LJ_ECount(var idnum:integer; demo:integer; resetCounter:integer;
                         var count:double; var ms:double):integer;

  function LJ_AISample(var idnum: Integer; demo: Integer; stateIO: Integer;
                        updateIO: Integer; ledOn: Integer; numChannels: Integer;
                        channels: t4Integers; gains: t4Integers;
                        disableCal: Integer;  var overVoltage: Integer;
                        var voltages: tVoltages): Integer;

  function LJ_AIBurst(var idnum: Integer; demo: Integer; stateIOin: Integer;
                        updateIO: Integer; ledOn: Integer; numChannels: Integer;
                        channels: t4Integers; gains: t4Integers;
                        var scanRate: single; disableCal: Integer;
                        triggerIO: Integer; triggerState: Integer;
                        numScans: Integer; timeout: Integer;
                        var voltages: tVoltages;
                        var stateIOout: Integer; var overVoltage: Integer;
                        transferMode: Integer): Integer;

  function LJ_AIStreamStart(var idnum: Integer; demo: Integer; stateIOin: Integer;
                        updateIO: Integer; ledOn: Integer; numChannels: Integer;
                        channels: t4Integers; gains: t4Integers;
                        var scanRate: Single; disableCal: Integer;
                        reserved1: Integer; reserved2: Integer): Integer;

  function LJ_AIStreamRead(localID: Integer; numScans: Integer; timeout: Integer;
	     	        var voltages: tVBuffer; var stateIOout: tIOBuffer;
                        var reserved: Integer;  var ljScanBacklog: Integer;
                        var overVoltage: Integer): Integer;

  function LJ_AIStreamClear(localID: Integer): Integer;

  function LJ_AOUpdate(var idnum:integer; demo: Integer; trisD: Integer; trisIO: Integer;
	   	        var stateD: Integer; var stateIO: Integer;
	   	        updateDigital: Integer; resetCounter: Integer;
	   	        var count: LongWord; analogOut0: Single;
                        analogOut1: Single): Integer;

  function LJ_BitsToVolts(chnum: Integer; chgain: Integer; bits: Integer;
                        volts: Single): Integer;

  function LJ_VoltsToBits(chnum: Integer; chgain: Integer; volts: Single;
		        var bits: Integer): Integer;

  function LJ_Counter(var pl_idnum:integer; demo: Integer; var stateD: Integer;
                        var stateIO: Integer;resetCounter: Integer;
                        EnableStb:integer; var count: LongWord): Integer;

  function LJ_DigitalIO(var pl_idnum:integer; demo: Integer; var trisD: Integer;
                        trisIO: Integer; var stateD: Integer;
                        var stateIO: Integer; updateDigital: Integer;
                        var outputD: Integer): Integer;

  function LJ_GetDriverVersion: Single;

  procedure LJ_GetErrorString(errorcode: Integer; var errorString: tErrorString);

  function LJ_GetFirmwareVersion(var idnum: Integer): Single;

  function LJ_ListAll(var pl_productIDList: Integer; var serialnumList: t127Integers;
	       	        var localIDList: t127Integers;var powerList: t127Integers;
                        var calMatrixList: tCalMatrixList; var numberFound: Integer;
	    	        var fcddMaxSize: Integer; var hvcMaxSize: Integer): Integer;


  function LJ_LocalID(var idnum: Integer; localID: Integer): Integer;

  function LJ_ReEnum(var idnum: Integer): Integer;

  function LJ_Reset(var idnum: Integer): Integer;

  function LJ_Watchdog(var idnum: Integer; demo: Integer; active: Integer;
                        timeout: Integer; reset: Integer; activeD0: Integer;
                        activeD1: Integer; activeD8: Integer;
                        stateD0: Integer;stateD1: Integer;
                        stateD8: Integer): Integer;

  function LJ_ReadMem(var idnum: Integer; address: Integer; var data3: Integer;
			var data2: Integer; var data1: Integer;
		        var data0: Integer): Integer;

  function LJ_WriteMem(var idnum: Integer; unlocked: Integer; address: Integer;
		       data3: Integer; data2: Integer; data1: Integer;
		       data0: Integer): Integer;

implementation

//DLL functions buffered and exposed to scripts----------------------------------------

function LJ_EDigitalOut(var idnum:integer; demo:integer; channel:integer;
                         writeD:integer; state:integer):integer;
var ljEDigitalOut:TljEDigitalOut;
begin
  Result:=LJGetProcAddress(@ljEDigitalOut,'EDigitalOut');
  If Result=0 then Result:=
    ljEDigitalOut(idnum,demo,channel,writeD,state);
end;

function LJ_EDigitalIn(var idnum:integer; demo:integer; channel:integer;
                         writeD:integer; var state:integer):integer;
var ljEDigitalIn:TljEDigitalIn;
begin
  Result:=LJGetProcAddress(@ljEDigitalIn,'EDigitalIn');
  If Result=0 then Result:=
    ljEDigitalIn(idnum,demo,channel,writeD,state);
end;

function LJ_EAnalogIn(var idnum:integer; demo:integer; channel:integer;
                         gain:integer; var overvoltage:integer;
                         var voltage:single):integer;
var ljEAnalogIn:TljEAnalogIn;
begin
  Result:=LJGetProcAddress(@ljEAnalogIn,'EAnalogIn');
  If Result=0 then Result:=
    ljEAnalogIn(idnum,demo,channel,gain,overvoltage,voltage);
end;

function LJ_EAnalogOut(var idnum:integer; demo:integer; analogOut0:single;
                         analogOut1:single):integer;
var ljEAnalogOut:TljEAnalogOut;
begin
  Result:=LJGetProcAddress(@ljEAnalogOut,'EAnalogOut');
  If Result=0 then Result:=
    ljEAnalogOut(idnum,demo,analogOut0,analogOut1);
end;

function LJ_ECount(var idnum:integer; demo:integer; resetCounter:integer;
                         var count:double; var ms:double):integer;
var ljECount:TljECount;
begin
  Result:=LJGetProcAddress(@ljECount,'ECount');
  If Result=0 then Result:=
    ljECount(idnum,demo,resetCounter,count,ms);
end;

function LJ_AISample(var idnum: Integer; demo: Integer; stateIO: Integer;
                        updateIO: Integer; ledOn: Integer; numChannels: Integer;
                        channels: t4Integers; gains: t4Integers;
                        disableCal: Integer;  var overVoltage: Integer;
                        var voltages: tVoltages): Integer;
var ljAISample:TljAISample;
begin
  Result:=LJGetProcAddress(@ljAISample,'AISample');
  If Result=0 then Result:=
    ljAISample(idnum,demo,stateIO,updateIO,ledOn,numChannels,channels,gains,
                          disableCal,overVoltage,voltages);
end;

function LJ_AIBurst(var idnum: Integer; demo: Integer; stateIOin: Integer;
                        updateIO: Integer; ledOn: Integer; numChannels: Integer;
                        channels: t4Integers; gains: t4Integers;
                        var scanRate: single; disableCal: Integer;
                        triggerIO: Integer; triggerState: Integer;
                        numScans: Integer; timeout: Integer;
                        var voltages: tVoltages;
                        var stateIOout: Integer; var overVoltage: Integer;
                        transferMode: Integer): Integer;
var ljAIBurst:TljAIBurst;
begin
  Result:=LJGetProcAddress(@ljAIBurst,'AIBurst');
  If Result=0 then Result:=
    ljAIBurst(idnum,demo,stateIOin,updateIO,ledOn,numChannels,channels,gains,
                        scanRate,disableCal,triggerIO,triggerState,
                        numScans,timeout,voltages,stateIOout,overVoltage,transferMode);
end;

function LJ_AIStreamStart(var idnum: Integer; demo: Integer; stateIOin: Integer;
                        updateIO: Integer; ledOn: Integer; numChannels: Integer;
                        channels: t4Integers; gains: t4Integers;
                        var scanRate: Single; disableCal: Integer;
                        reserved1: Integer; reserved2: Integer): Integer;
var ljAIStreamStart:TljAIStreamStart;
begin
  Result:=LJGetProcAddress(@ljAIStreamStart,'AIStreamStart');
  If Result=0 then Result:=
    ljAIStreamStart(idnum,demo,stateIOin,updateIO,ledOn,numChannels,channels,gains,
                        scanRate,disableCal,reserved1,reserved2);
end;

function LJ_AIStreamRead(localID: Integer; numScans: Integer; timeout: Integer;
	     	        var voltages: tVBuffer; var stateIOout: tIOBuffer;
                        var reserved: Integer;  var ljScanBacklog: Integer;
                        var overVoltage: Integer): Integer;
var ljAIStreamRead:TljAIStreamRead;
begin
  Result:=LJGetProcAddress(@ljAIStreamRead,'AIStreamRead');
  If Result=0 then Result:=
    ljAIStreamRead(localID,numScans,timeout,voltages,stateIOout,reserved,
                        ljScanBacklog,overVoltage);
end;

function LJ_AIStreamClear(localID: Integer): Integer;
var ljAIStreamClear:TljAIStreamClear;
begin
  Result:=LJGetProcAddress(@ljAIStreamClear,'AIStreamClear');
  If Result=0 then Result:=
    ljAIStreamClear(localID);
end;

function LJ_AOUpdate(var idnum:integer; demo: Integer; trisD: Integer; trisIO: Integer;
	   	        var stateD: Integer; var stateIO: Integer;
	   	        updateDigital: Integer; resetCounter: Integer;
	   	        var count: LongWord; analogOut0: Single;
                        analogOut1: Single): Integer;
var ljAOUpdate:TljAOUpdate;
begin
  Result:=LJGetProcAddress(@ljAOUpdate,'AOUpdate');
  If Result=0 then Result:=
    ljAOUpdate(idnum,demo,trisD,trisIO,stateD,stateIO,updateDigital,
                        resetCounter,count,analogOut0,analogOut1);
end;

function LJ_BitsToVolts(chnum: Integer; chgain: Integer; bits: Integer;
                        volts: Single): Integer;
var ljBitsToVolts:TljBitsToVolts;
begin
  Result:=LJGetProcAddress(@ljBitsToVolts,'BitsToVolts');
  If Result=0 then Result:=
    ljBitsToVolts(chnum,chgain,bits,volts);
end;

function LJ_VoltsToBits(chnum: Integer; chgain: Integer; volts: Single;
		        var bits: Integer): Integer;
var ljVoltsToBits:TljVoltsToBits;
begin
  Result:=LJGetProcAddress(@ljVoltsToBits,'VoltsToBits');
  If Result=0 then Result:=
    ljVoltsToBits(chnum,chgain,volts,bits);
end;

function LJ_Counter(var pl_idnum:integer; demo: Integer; var stateD: Integer;
                        var stateIO: Integer;resetCounter: Integer;
                        EnableStb:integer; var count: LongWord): Integer;
var ljCounter:TljCounter;
begin
  Result:=LJGetProcAddress(@ljCounter,'Counter');
  If Result=0 then Result:=
    ljCounter(pl_idnum,demo,stateD,stateIO,resetCounter,EnableStb,count);
end;

function LJ_DigitalIO(var pl_idnum:integer; demo: Integer; var trisD: Integer;
                        trisIO: Integer; var stateD: Integer;
                        var stateIO: Integer; updateDigital: Integer;
                        var outputD: Integer): Integer;
var ljDigitalIO:TljDigitalIO;
begin
  Result:=LJGetProcAddress(@ljDigitalIO,'DigitalIO');
  If Result=0 then Result:=
    ljDigitalIO(pl_idnum,demo,trisD,trisIO,stateD,stateIO,updateDigital,outputD);
end;

function LJ_GetDriverVersion: Single;
var ljGetDriverVersion:TljGetDriverVersion;
begin
  Result:=LJGetProcAddress(@ljGetDriverVersion,'GetDriverVersion');
  If Result=0 then Result:= ljGetDriverVersion;
end;

procedure LJ_GetErrorString(errorcode: Integer; var errorString: tErrorString);
var ljGetErrorString:TljGetErrorString; lResult:integer;
begin
  lResult:=LJGetProcAddress(@ljGetErrorString,'GetErrorString');
  If lResult=0 then ljGetErrorString(errorcode,errorString);
end;

function LJ_GetFirmwareVersion(var idnum: Integer): Single;
var ljGetFirmwareVersion:TljGetFirmwareVersion;
begin
  Result:=LJGetProcAddress(@ljGetFirmwareVersion,'GetFirmwareVersion');
  If Result=0 then Result:= ljGetFirmwareVersion(idnum);
end;

function LJ_ListAll(var pl_productIDList: Integer; var serialnumList: t127Integers;
	       	        var localIDList: t127Integers;var powerList: t127Integers;
                        var calMatrixList: tCalMatrixList; var numberFound: Integer;
	    	        var fcddMaxSize: Integer; var hvcMaxSize: Integer): Integer;
var ljListAll:TljListAll;
begin
  Result:=LJGetProcAddress(@ljListAll,'ListAll');
  If Result=0 then Result:=
    ljListAll(pl_productIDList,serialnumList,localIDList,powerList,calMatrixList,
                        numberFound,fcddMaxSize,hvcMaxSize);
end;

function LJ_LocalID(var idnum: Integer; localID: Integer): Integer;
var ljLocalID:TljLocalID;
begin
  Result:=LJGetProcAddress(@ljLocalID,'LocalID');
  If Result=0 then Result:= ljLocalID(idnum,localID);
end;

function LJ_ReEnum(var idnum: Integer): Integer;
var ljReEnum:TljReEnum;
begin
  Result:=LJGetProcAddress(@ljReEnum,'ReEnum');
  If Result=0 then Result:= ljReEnum(idnum);
end;

function LJ_Reset(var idnum: Integer): Integer;
var ljReset:TljReset;
begin
  Result:=LJGetProcAddress(@ljReset,'Reset');
  If Result=0 then Result:= ljReset(idnum);
end;

function LJ_Watchdog(var idnum: Integer; demo: Integer; active: Integer;
                        timeout: Integer; reset: Integer; activeD0: Integer;
                        activeD1: Integer; activeD8: Integer;
                        stateD0: Integer;stateD1: Integer;
                        stateD8: Integer): Integer;
var ljWatchdog:TljWatchdog;
begin
  Result:=LJGetProcAddress(@ljWatchdog,'Watchdog');
  If Result=0 then Result:= ljWatchdog(idnum,demo,active,timeout,reset,activeD0,
                        activeD1,activeD8,stateD0,stateD1,stateD8);
end;

function LJ_ReadMem(var idnum: Integer; address: Integer; var data3: Integer;
			var data2: Integer; var data1: Integer;
		        var data0: Integer): Integer;
var ljReadMem:TljReadMem;
begin
  Result:=LJGetProcAddress(@ljReadMem,'ReadMem');
  If Result=0 then Result:= ljReadMem(idnum,address,data3,data2,data1,data0);
end;

function LJ_WriteMem(var idnum: Integer; unlocked: Integer; address: Integer;
		       data3: Integer; data2: Integer; data1: Integer;
		       data0: Integer): Integer;
var ljWriteMem:TljWriteMem;
begin
  Result:=LJGetProcAddress(@ljWriteMem,'WriteMem');
  If Result=0 then Result:= ljWriteMem(idnum,unlocked,address,data3,data2,data1,data0);
end;



//-------------------------------------------------------
// --- background support functions ---

Function FindLabJack:boolean; //call this at startup
begin
  LJ_LibHandle:=LoadLibrary('ljackuw.dll');
  Result:=(LJ_LibHandle<>0);
end;

Function LJ_DLLloaded:boolean;
begin
  Result:=LabjackDLLFound;
  If not Result then
    If (Now-LastLJNPtime)>(1/24/60) then //more than a minute since error message shown
    begin
      LastLJNPtime:=Now;
      ShowMessage('The LabJack Drivers are not installed!');
    end;
end;

Function LJGetProcAddress(var ProcPtr:Pointer;ProcName:string):Integer;
//does checking for DLL, etc.
begin
  Result:=2; //default= "LabJack not present"
  If LJ_DLLloaded then
  begin
    ProcPtr:=GetProcAddress(LJ_LibHandle,PChar(ProcName));
    If (ProcPtr<>nil) then result:=0;
  end;
end;

//----------------------------------------

{ TSimpleLabJack Object property methods}

function TSimpleLabJack.GetAnalogGain(line: integer): integer;
begin
  if (line<12) and (line>7) then
    Result:=LJGains[line-8]
  else Result:=0;
end;

function TSimpleLabJack.GetCounter: integer;
var errcode:integer; CountV,ms:double;
begin
  errcode:=LJ_ECount(fSimpleLJID,0,0,CountV,ms);
  If errcode=0 then result:=round(CountV)
  else result:=-1;
end;

function TSimpleLabJack.GetInAO(line: integer): single;
var errcode,gain:integer; overvoltage:integer; voltage:single;
begin
  If (line<12) and (line>=0) then
  begin
    If line>7 then
    begin
      case LJGains[line-7] of
      1: gain:=0;
      2: gain:=1;
      4: gain:=2;
      5: gain:=3;
      8: gain:=4;
      10: gain:=5;
      16: gain:=6;
      20: gain:=7;
      else gain:=0;
      end;
    end
    else gain:=0;
    errcode:=LJ_EAnalogIn(fSimpleLJID,0,line,gain,overvoltage,voltage);
    if errcode=0 then result:=voltage else result:=0;
  end
  else
    Result:=0;
end;

function TSimpleLabJack.GetInByte: integer;
var errcode,trisD,trisIO,stateD,stateIO,UpdateDigital,OutputD:integer;
begin
  trisD:=127; //0=input,1=output; bits 0-7 are outputs
  trisIO:=ljTrisIO;
  updateDigital:=0; //just read the values
  errcode:=LJ_DigitalIO(fSimpleLJID,0,trisD,trisIO,stateD,stateIO,updateDigital,outputD);
  if errcode=0 then
    result:=integer((outputD div 256) AND 255)
  else result:=0;
end;

function TSimpleLabJack.GetInIO(line: integer): integer;
var errcode,state:integer;
begin
  if (line>=0) and (line<4) then
  begin
    errcode:=LJ_EDigitalIn(fSimpleLJID,0,line,0,state);
    if errcode=0 then
      if state=0 then result:=0 else result:=1
    else result:=0;
  end
  else result:=0;
end;

function TSimpleLabJack.GetSimpleLJID: integer;
begin
  result:=fSimpleLJID;
end;

procedure TSimpleLabJack.ResetCounter;
var countv,ms:double;
begin
  LJ_ECount(fSimpleLJID,0,1,countv,ms);
end;

procedure TSimpleLabJack.SetAnalogGain(line: integer;
  const Value: integer);
var v2:integer;
begin
  v2:=value;
  if (line<12) and (line>7) then
  begin
    //choose next LOWER value of gain (avoid overload)
    If v2>=20 then v2:=20
    else if v2>=16 then v2:=16
    else if v2>=10 then v2:=10
    else if v2>=8 then v2:=8
    else if v2>=5 then v2:=5
    else if v2>=4 then v2:=4
    else if v2>=2 then v2:=2
    else v2:=1;
    LJGains[line-8]:=v2;
  end
end;

procedure TSimpleLabJack.SetOutAO(line: integer; const Value: single);
begin
  If (line=0) then
  begin
    LJ_EAnalogOut(fSimpleLJID,0,value,LJanalogOut1);
    LJanalogOut0:=value;
  end
  else if (line=1) then
  begin
    LJ_EAnalogOut(fSimpleLJID,0,LJanalogOut0,value);
    LJanalogOut1:=value;
  end;
end;

procedure TSimpleLabJack.SetOutByte(const Value: integer);
var trisD,trisIO,stateD,stateIO,UpdateDigital,OutputD:integer;
begin
  trisD:=127; //0=input,1=output; bits 8-15 are INputs
  trisIO:=ljTrisIO;
  StateIO:=ljStateIO;
  updateDigital:=1; //write the values
  stateD:=integer(Value and 255);
  LJ_DigitalIO(fSimpleLJID,0,trisD,trisIO,stateD,stateIO,updateDigital,outputD);
end;

procedure TSimpleLabJack.SetOutIO(line: integer; const Value: integer);
begin
  if (line>=0) and (line<4) then
  begin
    LJ_EDigitalOut(fSimpleLJID,0,line,0,value);
  end;
end;

procedure TSimpleLabJack.SetSimpleLJID(const Value: integer);
begin
  fSimpleLJID:=value;
end;

//--------------------

initialization

  LabjackDLLFound:=FindLabJack;
  LJ:= TSimpleLabJack.Create;
  LJGains[0]:=1; LJGains[1]:=1; LJGains[2]:=1; LJGains[3]:=1;

finalization

  LJ.Free;
  If LabjackDLLFound then
  begin
     FreeLibrary(LJ_LibHandle);
  end;

end.

