(*
This unit allows for user alterations of hardware settings.  The defaults and saved settings
are stored in an .ini file in the hardware settings folder.  Also, in the hardware folder, are
the .txt files that list the Counters, PFI lines, Source Lines and the default combinations of
Source, Gate, and Outputs for each counter.
This allows for users to update the .txt and .ini files to change for advanced setups that suit
their needs.
NOTE: for completeness users should update the default strings of each ReadString call to the .ini
file in case of file reading errors.  These strings occur in the ReadIniFile procdure, in all
of the OnChange events,
// todo -oNeil: Check places that need to be updated on changes and double check validity of this
//              header desciption.


The global variables in the ExtFunctionsTakeTwo are the ones which this unit changes.
ExtFunctionsTakeTwo uses them to call the hardware calls.
*)

unit HardwareSettingsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TypesUnit, ImgList, ComCtrls, ExtCtrls, StdCtrls, Inifiles,
  Spin, jpeg;

type

  THardwareSettingsFrm = class(TForm)
    SaveSettingsButton: TButton;
    CancelButton: TButton;
    ClockPageControl: TPageControl;
    PixelClockTabSheet: TTabSheet;
    LineClockTabSheet: TTabSheet;
    FrameClockTabSheet: TTabSheet;
    PixelClockDevNumberLabel: TLabel;
    PixelClockHardwareCtrLabel: TLabel;
    PixelClockSourceLabel: TLabel;
    PixelClockOutputSignalLabel: TLabel;
    AOutsGroupBox: TGroupBox;
    AOutsDevNumberLabel: TLabel;
    AOutsTriggerLineLabel: TLabel;
    AOutsTrigLineComboBox: TComboBox;
    AOutsTrigLinePolarityComboBox: TComboBox;
    AOutsTrigPolarityLabel: TLabel;
    AOutsDevNumSpinEdit: TSpinEdit;
    PhotonCtrsPageControl: TPageControl;
    PhotonCounterATabSheet: TTabSheet;
    PhotonCounterBTabSheet: TTabSheet;
    PhotonCounterCTabSheet: TTabSheet;
    PhotonCounterDTabSheet: TTabSheet;
    PhotonCounterETabSheet: TTabSheet;
    LineClockDevNumberLabel: TLabel;
    LineClockHardwareCtrLabel: TLabel;
    LineClockSourceLabel: TLabel;
    LineClockOutputSignalLabel: TLabel;
    FrameClockDevNumberLabel: TLabel;
    FrameClockHardwareCtrLabel: TLabel;
    FrameClockSourceLabel: TLabel;
    FrameClockOutputSignalLabel: TLabel;
    PixelClockDevNumSpinEdit: TSpinEdit;
    PixelClockHardwareCtrComboBox: TComboBox;
    PixelClockSourceComboBox: TComboBox;
    PixelClockOutputSignalComboBox: TComboBox;
    PixelClockChangeLinesChBx: TCheckBox;
    LineClockDevNumSpinEdit: TSpinEdit;
    LineClockHardwareCtrComboBox: TComboBox;
    LineClockChangeLinesChBx: TCheckBox;
    LineClockSourceComboBox: TComboBox;
    LineClockOutputSignalComboBox: TComboBox;
    FrameClockDevNumSpinEdit: TSpinEdit;
    FrameClockHardwareCtrComboBox: TComboBox;
    FrameClockChangeLinesChBx: TCheckBox;
    FrameClockSourceComboBox: TComboBox;
    FrameClockOutputSignalComboBox: TComboBox;
    PhCtrADevNumLabel: TLabel;
    PhCtrADevNumSpinEdit: TSpinEdit;
    PhCtrAHardwareCtrLabel: TLabel;
    PhCtrAHardwareCtrComboBox: TComboBox;
    PhCtrAChangeLinesChBx: TCheckBox;
    PhCtrASourceLabel: TLabel;
    PhCtrASourceComboBox: TComboBox;
    PhCtrAGateLabel: TLabel;
    PhCtrAGateComboBox: TComboBox;
    PhCtrBGateComboBox: TComboBox;
    PhCtrBGateLabel: TLabel;
    PhCtrBSourceComboBox: TComboBox;
    PhCtrBSourceLabel: TLabel;
    PhCtrBChangeLinesChBx: TCheckBox;
    PhCtrBDevNumSpinEdit: TSpinEdit;
    PhCtrBDevNumLabel: TLabel;
    PhCtrBHardwareCtrLabel: TLabel;
    PhCtrBHardwareCtrComboBox: TComboBox;
    PixelImage: TImage;
    LineImage: TImage;
    FrameImage: TImage;
    PhCtrCDevNumLabel: TLabel;
    PhCtrCDevNumSpinEdit: TSpinEdit;
    PhCtrCChangeLinesChBx: TCheckBox;
    PhCtrCSourceComboBox: TComboBox;
    PhCtrCSourceLabel: TLabel;
    PhCtrCHardwareCtrLabel: TLabel;
    PhCtrCHardwareCtrComboBox: TComboBox;
    PhCtrCGateLabel: TLabel;
    PhCtrCGateComboBox: TComboBox;
    PhCtrDDevNumLabel: TLabel;
    PhCtrDDevNumSpinEdit: TSpinEdit;
    PhCtrDChangeLinesChBx: TCheckBox;
    PhCtrDHardwareCtrLabel: TLabel;
    PhCtrDHardwareCtrComboBox: TComboBox;
    PhCtrDSourceLabel: TLabel;
    PhCtrDSourceComboBox: TComboBox;
    PhCtrDGateLabel: TLabel;
    PhCtrDGateComboBox: TComboBox;
    PhCtrEDevNumLabel: TLabel;
    PhCtrEDevNumSpinEdit: TSpinEdit;
    PhCtrEChangeLinesChBx: TCheckBox;
    PhCtrEHardwareCtrLabel: TLabel;
    PhCtrEHardwareCtrComboBox: TComboBox;
    PhCtrESourceLabel: TLabel;
    PhCtrESourceComboBox: TComboBox;
    PhCtrEGateLabel: TLabel;
    PhCtrEGateComboBox: TComboBox;
    InitBrdsMemo: TMemo;
    InitBrdsLabel: TLabel;
    CountersGroupBox: TGroupBox;
    CountersTrigLinePolarityLabel: TLabel;
    CountersTrigLinePolarityComboBox: TComboBox;
    CountersTrigLineComboBox: TComboBox;
    CountersTrigLineLabel: TLabel;
    CountersDevNumLabel: TLabel;
    CountersDevNumSpinEdit: TSpinEdit;
    PixelClockOutputSourceLabel: TLabel;
    PixelClockOutputSourceComboBox: TComboBox;
    LineClockOutputSourceLabel: TLabel;
    LineClockOutputSourceComboBox: TComboBox;
    FrameClockOutputSourceLabel: TLabel;
    FrameClockOutputSourceComboBox: TComboBox;
    RestoreDefaultsButton: TButton;
    PixelClockSourcePolarityLabel: TLabel;
    PixelClockSourcePolarityComboBox: TComboBox;
    LineClockSourcePolarityComboBox: TComboBox;
    LineClockSourcePolarityLabel: TLabel;
    FrameClockSourcePolarityLabel: TLabel;
    FrameClockSourcePolarityComboBox: TComboBox;
    PixelClockOutputPolarityLabel: TLabel;
    PixelClockOutputPolarityComboBox: TComboBox;
    LineClockOutputPolarityLabel: TLabel;
    LineClockOutputPolarityComboBox: TComboBox;
    FrameClockOutputPolarityLabel: TLabel;
    FrameClockOutputPolarityComboBox: TComboBox;
    FrameClockGateLabel: TLabel;
    FrameClockGateComboBox: TComboBox;
    FrameClockGatePolarityLabel: TLabel;
    FrameClockGatePolarityComboBox: TComboBox;
    StatusBar1: TStatusBar;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure AOutsDevNumSpinEditChange(Sender: TObject);
    procedure AOutsTrigLineComboBoxChange(Sender: TObject);
    procedure AOutsTrigLinePolarityComboBoxChange(Sender: TObject);
    procedure CountersDevNumSpinEditChange(Sender: TObject);
    procedure CountersTrigLineComboBoxChange(Sender: TObject);
    procedure CountersTrigLinePolarityComboBoxChange(Sender: TObject);
    procedure PixelClockHardwareCtrComboBoxChange(Sender: TObject);
    procedure PixelClockSourceComboBoxChange(Sender: TObject);
    procedure PixelClockSourcePolarityComboBoxChange(Sender: TObject);
    procedure PixelClockOutputSignalComboBoxChange(Sender: TObject);
    procedure PixelClockOutputSourceComboBoxChange(Sender: TObject);
    procedure PixelClockOutputPolarityComboBoxChange(Sender: TObject);
    procedure LineClockHardwareCtrComboBoxChange(Sender: TObject);
    procedure LineClockSourceComboBoxChange(Sender: TObject);
    procedure LineClockSourcePolarityComboBoxChange(Sender: TObject);
    procedure LineClockOutputSignalComboBoxChange(Sender: TObject);
    procedure LineClockOutputSourceComboBoxChange(Sender: TObject);
    procedure LineClockOutputPolarityComboBoxChange(Sender: TObject);
    procedure FrameClockHardwareCtrComboBoxChange(Sender: TObject);
    procedure FrameClockSourceComboBoxChange(Sender: TObject);
    procedure FrameClockSourcePolarityComboBoxChange(Sender: TObject);
    procedure FrameClockOutputSignalComboBoxChange(Sender: TObject);
    procedure FrameClockOutputSourceComboBoxChange(Sender: TObject);
    procedure FrameClockOutputPolarityComboBoxChange(Sender: TObject);
    procedure FrameClockGateComboBoxChange(Sender: TObject);
    procedure FrameClockGatePolarityComboBoxChange(Sender: TObject);
    procedure PhCtrAHardwareCtrComboBoxChange(Sender: TObject);
    procedure PhCtrASourceComboBoxChange(Sender: TObject);
    procedure PhCtrAGateComboBoxChange(Sender: TObject);
    procedure PhCtrBHardwareCtrComboBoxChange(Sender: TObject);
    procedure PhCtrBSourceComboBoxChange(Sender: TObject);
    procedure PhCtrBGateComboBoxChange(Sender: TObject);
    procedure PhCtrCHardwareCtrComboBoxChange(Sender: TObject);
    procedure PhCtrCSourceComboBoxChange(Sender: TObject);
    procedure PhCtrCGateComboBoxChange(Sender: TObject);
    procedure PhCtrDHardwareCtrComboBoxChange(Sender: TObject);
    procedure PhCtrDSourceComboBoxChange(Sender: TObject);
    procedure PhCtrDGateComboBoxChange(Sender: TObject);
    procedure PhCtrEHardwareCtrComboBoxChange(Sender: TObject);
    procedure PhCtrESourceComboBoxChange(Sender: TObject);
    procedure PhCtrEGateComboBoxChange(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure RestoreDefaultsButtonClick(Sender: TObject);
    procedure SaveSettingsButtonClick(Sender: TObject);
  private
   HardwareSettings: TIniFile;
   Boards: array of SmallInt;
   BoardStrings: TStringList;
   PFILines: TStringList;
   CtrNumbers: TStringList;
   SourceLines: TStringList;
   OutputSourceLines: TStringList;
   GateLines: TStringList;
   Section: string;
   sAppDir: string;
   SGOLines: array[0..7] of TStringList;
   Modified: Boolean;
   DuplicateLine: Boolean;
   DuplicateDevice: Boolean;
   function PCIBoard(const DeviseNumberCode: SmallInt): string;
   function RedundantCtr(const NewCtr: string): string;
   procedure ReadIniFile();
   procedure SaveIniFile();
   procedure CheckSettings();
   procedure Compare();
  public
   PCISlots: SmallInt;
   function LinePFI(): Byte;
   function FramePFI(): Byte;
   function NI_DAQ_CNS_Value(const ConstString: string): Cardinal;
  published

  end;

var
  HardwareSettingsFrm: THardwareSettingsFrm;

implementation

{$R *.dfm}

uses nidaq, nidaqcns, HardwareFunctions;

{ TAdvCntSetFrm }


procedure THardwareSettingsFrm.FormCreate(Sender: TObject);
var
i: SmallInt;
pPCICheck: pi16;
hPCICheck: THandle;

begin
Self.WindowState := wsMinimized;
Modified := False;
//  creates and opens string lists for the drop down menus of the combo boxes
sAppDir := ExtractFilePath(Application.ExeName);
for i := 0 to 7 do SGOLines[i] := TStringList.Create;
PFILines := TStringList.Create;
CtrNumbers := TStringList.Create;
BoardStrings := TStringList.Create;
SourceLines := TStringList.Create;
OutputSourceLines := TStringList.Create;
GateLines := TStringList.Create;
try
for i := 0 to 7 do SGOLines[i].LoadFromFile('HardwareData\Ctr' + IntToStr(i) + 'SGO.txt');
PFILines.LoadFromFile('HardwareData\PFILines.txt');
CtrNumbers.LoadFromFile('HardwareData\CtrNumbers.txt');
SourceLines.LoadFromFile('HardwareData\SourceLines.txt');
OutputSourceLines.LoadFromFile('HardwareData\OutputSourceLines.txt');
GateLines.LoadFromFile('HardwareData\GateLines.txt');
HardwareSettings := TIniFile.Create(sAppDir + 'HardwareData\L&THardwareSettings.ini');
except
  on E: Exception do MessageDlg('One or more source files are missing or corrupt' + #10 +
                                'See documentation for details.', mtWarning, [mbOK],0);
end;

//  check the device number and reset all NI PCI boards available
PCISlots := 6;
Setlength(Boards,PCISlots);
hPCICheck := GlobalAlloc(GHND,2*PCISlots);
pPCICheck := pi16(GlobalLock(hPCICheck));
for i := 0 to PCISlots-1 do
 begin
  Init_DA_Brds(i,pPCICheck);
  Boards[i] := pPCICheck^;
  BoardStrings.Add(PCIBoard(Boards[i]));
  InitBrdsMemo.Lines.Add(IntToStr(i) + ': ' + BoardStrings[i]);
 end;
GlobalUnlock(hPCICheck);
GlobalFree(hPCICheck);

//  Assigns the stringlists, loaded in from the files in HardwareData folder, to the
//  relevent components on the form.
// Trigger Lines
AOutsTrigLineComboBox.Items := PFILines;
CountersTrigLineComboBox.Items := PFILines;
// Pixel clock settings
PixelClockSourceComboBox.Items := SourceLines;
PixelClockOutputSignalComboBox.Items := PFILines;
PixelClockOutputSourceComboBox.Items := OutputSourceLines;
PixelClockHardwareCtrComboBox.Items := CtrNumbers;
// Line clock settings
LineClockSourceComboBox.Items := SourceLines;
LineClockOutputSignalComboBox.Items := PFILines;
LineClockOutputSourceComboBox.Items := OutputSourceLines;
LineClockHardwareCtrComboBox.Items := CtrNumbers;
// Frame clock settings
FrameClockSourceComboBox.Items := SourceLines;
FrameClockOutputSignalComboBox.Items := PFILines;
FrameClockOutputSourceComboBox.Items := OutputSourceLines;
FrameClockGateComboBox.Items := GateLines;
FrameClockHardwareCtrComboBox.Items := CtrNumbers;
//  Photon counter A settings
PhCtrAHardwareCtrComboBox.Items := CtrNumbers;
PhCtrASourceComboBox.Items := PFILines;
PhCtrAGateComboBox.Items := PFILines;
//  Photon counter B settings
PhCtrBHardwareCtrComboBox.Items := CtrNumbers;
PhCtrBSourceComboBox.Items := PFILines;
PhCtrBGateComboBox.Items := PFILines;
//  Photon counter C settings
PhCtrCHardwareCtrComboBox.Items := CtrNumbers;
PhCtrCSourceComboBox.Items := PFILines;
PhCtrCGateComboBox.Items := PFILines;
//  Photon counter D settings
PhCtrDHardwareCtrComboBox.Items := CtrNumbers;
PhCtrDSourceComboBox.Items := PFILines;
PhCtrDGateComboBox.Items := PFILines;
//  Photon counter E settings
PhCtrEHardwareCtrComboBox.Items := CtrNumbers;
PhCtrESourceComboBox.Items := PFILines;
PhCtrEGateComboBox.Items := PFILines;

ReadIniFile;

HardwareControl.FrameClockPFILine := FramePFI;
HardwareControl.LineClockPFILine := LinePFI;
end;


procedure THardwareSettingsFrm.RestoreDefaultsButtonClick(Sender: TObject);
begin
//  changes usersaved to false causing the ReadIniFile procedure to restore values to default
HardwareSettings.WriteString('defaults','usersaved','false');
ReadIniFile;
DuplicateLine := False;
DuplicateDevice := False;
StatusBar1.SimpleText := 'Defaults restored.';
HardwareControl.FrameClockPFILine := FramePFI;
HardwareControl.LineClockPFILine := LinePFI;
end;

procedure THardwareSettingsFrm.CancelButtonClick(Sender: TObject);
begin
//  reads ini file to set values as they where when the form was first opened
ReadIniFile;
Self.Close;
end;

procedure THardwareSettingsFrm.SaveSettingsButtonClick(Sender: TObject);
begin
if Modified then Compare;
if (NOT DuplicateLine) AND (NOT DuplicateDevice) then
 begin
  //  changes usersaved to true causing the ReadIniFile procedure to restore saved values
  HardwareSettings.WriteString('defaults','usersaved','true');
  StatusBar1.SimpleText := 'Saving settings .';
  SaveIniFile;
  Self.Close;
  HardwareControl.FrameClockPFILine := FramePFI;
  HardwareControl.LineClockPFILine := LinePFI;
 end;

end;


procedure THardwareSettingsFrm.FormDestroy(Sender: TObject);
var i: integer;
begin
BoardStrings.Destroy;
CtrNumbers.Destroy;
PFILines.Destroy;
SourceLines.Destroy;
OutputSourceLines.Destroy;
GateLines.Destroy;
for i := 0 to 7 do SGOLines[i].Destroy;
HardwareSettings.Free;
end;


procedure THardwareSettingsFrm.ReadIniFile;
var TempStr: string;
begin
// todo - oNeil: verify all these readsettings default strings are the same as L&THardwareSettings
//  Checks for user value within .ini file and sets section to either default or user
//  See L&THardwareSettings.ini
TempStr := HardwareSettings.ReadString('defaults','usersaved','');
if TempStr = '' then
MessageDlg('Error reading settings file.  Please ensure the .ini file is in the correct directory and intact.  See Documentation for details.'
            ,mtError,[mbOK],0);
if TempStr = 'false' then Section := 'defaults'
else Section := 'user';
//  The remainder of this procedure reads in the string from the .ini file and then converts
//  it to the corresponding Cardinal value needed for the NI board and then assigns that
//  value to the global variables declared at the top of the ExtFunctionsTakeTwo unit.
//  Reads in the analog output settings
AOutsDevNumSpinEdit.Text := HardwareSettings.ReadString(Section,'AOutsDevNum','1');
HardwareControl.AOutsDevNum := StrToInt(AOutsDevNumSpinEdit.Text);
AOutsTrigLineComboBox.Text := HardwareSettings.ReadString(Section,'AOutsTrigLine','ND_PFI_3');
HardwareControl.AOutTrigLine := NI_DAQ_CNS_Value(AOutsTrigLineComboBox.Text);
AOutsTrigLinePolarityComboBox.Text := HardwareSettings.ReadString(Section,'AOutsTrigLinePolarity','ND_HIGH_TO_LOW');
HardwareControl.AOutTrigLinePolarity := NI_DAQ_CNS_Value(AOutsTrigLinePolarityComboBox.Text);
//  reads in the counter trigger settings and sets polarity of trigger
CountersDevNumSpinEdit.Text := HardwareSettings.ReadString(Section,'CountersDevNum','2');
HardwareControl.CountersDevNum := StrToInt(CountersDevNumSpinEdit.Text);
CountersTrigLineComboBox.Text := HardwareSettings.ReadString(Section,'CountersTrigLine','ND_PFI_7');
HardwareControl.CountersTrigLine := NI_DAQ_CNS_Value(CountersTrigLineComboBox.Text);
CountersTrigLinePolarityComboBox.Text := HardwareSettings.ReadString(Section,'CountersTrigLinePolarity','ND_HIGH_TO_LOW');
HardwareControl.CountersTrigLinePolarity := NI_DAQ_CNS_Value(CountersTrigLinePolarityComboBox.Text);
if HardwareControl.CountersTrigLinePolarity = 18100 {ND_HIGH_TO_LOW}then
 begin
  HardwareControl.SetTrigDIGState := 1;  //  Set digital out line high
  HardwareControl.GODIGState := 0;       //  and then low to go
 end
else
 begin
  HardwareControl.SetTrigDIGState := 0;   //  Set digital out line low
  HardwareControl.GODIGState := 1;        //  and then high to go
 end;
//  reads in pixel clock settings
PixelClockHardwareCtrComboBox.Text := HardwareSettings.ReadString(Section,'PixelClockHardwareCtr','ND_COUNTER_1');
HardwareControl.PixelClockHardwareCtr := NI_DAQ_CNS_Value(PixelClockHardwareCtrComboBox.Text);
PixelClockSourceComboBox.Text := HardwareSettings.ReadString(Section,'PixelClockSource','ND_INTERNAL_MAX_TIMEBASE');
HardwareControl.PixelClockSource := NI_DAQ_CNS_Value(PixelClockSourceComboBox.Text);
PixelClockSourcePolarityComboBox.Text := HardwareSettings.ReadString(Section,'PixelClockSourcePolarity','N/A');
HardwareControl.PixelClockSourcePolarity := NI_DAQ_CNS_Value(PixelClockSourcePolarityComboBox.Text);
PixelClockOutputSignalComboBox.Text := HardwareSettings.ReadString(Section,'PixelClockOutputSignal','ND_PFI_32');
HardwareControl.PixelClockOutputSignal := NI_DAQ_CNS_Value(PixelClockOutputSignalComboBox.Text);
PixelClockOutputSourceComboBox.Text := HardwareSettings.ReadString(Section,'PixelClockOutputSource','ND_GPCTR1_OUTPUT');
HardwareControl.PixelClockOutputSource := NI_DAQ_CNS_Value(PixelClockOutputSourceComboBox.Text);
PixelClockOutputPolarityComboBox.Text := HardwareSettings.ReadString(Section,'PixelClockOutputPolarity','ND_POSITIVE');
HardwareControl.PixelClockOutputPolarity := NI_DAQ_CNS_Value(PixelClockOutputPolarityComboBox.Text);
//  reads in line clock settings
LineClockHardwareCtrComboBox.Text := HardwareSettings.ReadString(Section,'LineClockHardwareCtr','ND_COUNTER_2');
HardwareControl.LineClockHardwareCtr := NI_DAQ_CNS_Value(LineClockHardwareCtrComboBox.Text);
LineClockSourceComboBox.Text := HardwareSettings.ReadString(Section,'LineClockSource','ND_INTERNAL_MAX_TIMEBASE');
HardwareControl.LineClockSource := NI_DAQ_CNS_Value(LineClockSourceComboBox.Text);
LineClockSourcePolarityComboBox.Text := HardwareSettings.ReadString(Section,'LineClockSourcePolarity','N/A');
HardwareControl.LineClockSourcePolarity := NI_DAQ_CNS_Value(LineClockSourcePolarityComboBox.Text);
LineClockOutputSignalComboBox.Text := HardwareSettings.ReadString(Section,'LineClockOutputSignal','ND_PFI_28');
HardwareControl.LineClockOutputSignal := NI_DAQ_CNS_Value(LineClockOutputSignalComboBox.Text);
LineClockOutputSourceComboBox.Text := HardwareSettings.ReadString(Section,'LineClockOutputSource','ND_GPCTR2_OUTPUT');
HardwareControl.LineClockOutputSource := NI_DAQ_CNS_Value(LineClockOutputSourceComboBox.Text);
LineClockOutputPolarityComboBox.Text := HardwareSettings.ReadString(Section,'LineClockOutputPolarity','ND_POSITIVE');
HardwareControl.LineClockOutputPolarity := NI_DAQ_CNS_Value(LineClockOutputPolarityComboBox.Text);
// reads in frame clock settings
FrameClockHardwareCtrComboBox.Text := HardwareSettings.ReadString(Section,'FrameClockHardwareCtr','ND_COUNTER_0');
HardwareControl.FrameClockHardwareCtr := NI_DAQ_CNS_Value(FrameClockHardwareCtrComboBox.Text);
FrameClockSourceComboBox.Text := HardwareSettings.ReadString(Section,'FrameClockSource','ND_PFI_39');
HardwareControl.FrameClockSource := NI_DAQ_CNS_Value(FrameClockSourceComboBox.Text);
FrameClockSourcePolarityComboBox.Text := HardwareSettings.ReadString(Section,'FrameClockSourcePolarity','ND_LOW_TO_HIGH');
HardwareControl.FrameClockSourcePolarity := NI_DAQ_CNS_Value(FrameClockSourcePolarityComboBox.Text);
FrameClockOutputSignalComboBox.Text := HardwareSettings.ReadString(Section,'FrameClockOutputSignal','ND_PFI_36');
HardwareControl.FrameClockOutputSignal := NI_DAQ_CNS_Value(FrameClockOutputSignalComboBox.Text);
FrameClockOutputSourceComboBox.Text := HardwareSettings.ReadString(Section,'FrameClockOutputSource','ND_GPCTR3_OUTPUT');
HardwareControl.FrameClockOutputSource := NI_DAQ_CNS_Value(FrameClockOutputSourceComboBox.Text);
FrameClockOutputPolarityComboBox.Text := HardwareSettings.ReadString(Section,'FrameClockOutputPolarity','');
HardwareControl.FrameClockOutputPolarity := NI_DAQ_CNS_Value(FrameClockOutputPolarityComboBox.Text);
FrameClockGateComboBox.Text := HardwareSettings.ReadString(Section,'FrameClockGate','ND_PFI_38');
HardwareControl.FrameClockGate := NI_DAQ_CNS_Value(FrameClockGateComboBox.Text);
FrameClockGatePolarityComboBox.Text := HardwareSettings.ReadString(Section,'FrameClockGatePolarity','ND_LOW_TO_HIGH');
HardwareControl.FrameClockGatePolarity := NI_DAQ_CNS_Value(FrameClockGatePolarityComboBox.Text);
//  reads in the setting for the photon counters
PhCtrAHardwareCtrComboBox.Text := HardwareSettings.ReadString(Section,'PhCtrAHardwareCtr','ND_COUNTER_3');
HardwareControl.PhCtrAHardwareCtr := NI_DAQ_CNS_Value(PhCtrAHardwareCtrComboBox.Text);
PhCtrASourceComboBox.Text := HardwareSettings.ReadString(Section,'PhCtrASource','ND_PFI_27');
HardwareControl.PhCtrASource := NI_DAQ_CNS_Value(PhCtrASourceComboBox.Text);
PhCtrAGateComboBox.Text := HardwareSettings.ReadString(Section,'PhCtrAGate','ND_PFI_26');
HardwareControl.PhCtrAGate := NI_DAQ_CNS_Value(PhCtrAGateComboBox.Text);

PhCtrBHardwareCtrComboBox.Text := HardwareSettings.ReadString(Section,'PhCtrBHardwareCtr','ND_COUNTER_4');
HardwareControl.PhCtrBHardwareCtr := NI_DAQ_CNS_Value(PhCtrBHardwareCtrComboBox.Text);
PhCtrBSourceComboBox.Text := HardwareSettings.ReadString(Section,'PhCtrBSource','ND_PFI_23');
HardwareControl.PhCtrBSource := NI_DAQ_CNS_Value(PhCtrBSourceComboBox.Text);
PhCtrBGateComboBox.Text := HardwareSettings.ReadString(Section,'PhCtrBGate','ND_PFI_22');
HardwareControl.PhCtrBGate := NI_DAQ_CNS_Value(PhCtrBGateComboBox.Text);

PhCtrCHardwareCtrComboBox.Text := HardwareSettings.ReadString(Section,'PhCtrCHardwareCtr','ND_COUNTER_5');
HardwareControl.PhCtrCHardwareCtr := NI_DAQ_CNS_Value(PhCtrCHardwareCtrComboBox.Text);
PhCtrCSourceComboBox.Text := HardwareSettings.ReadString(Section,'PhCtrCSource','ND_PFI_19');
HardwareControl.PhCtrCSource := NI_DAQ_CNS_Value(PhCtrCSourceComboBox.Text);
PhCtrCGateComboBox.Text := HardwareSettings.ReadString(Section,'PhCtrCGate','ND_PFI_18');
HardwareControl.PhCtrCGate := NI_DAQ_CNS_Value(PhCtrCGateComboBox.Text);

PhCtrDHardwareCtrComboBox.Text := HardwareSettings.ReadString(Section,'PhCtrDHardwareCtr','ND_COUNTER_6');
HardwareControl.PhCtrDHardwareCtr := NI_DAQ_CNS_Value(PhCtrDHardwareCtrComboBox.Text);
PhCtrDSourceComboBox.Text := HardwareSettings.ReadString(Section,'PhCtrDSource','ND_PFI_15');
HardwareControl.PhCtrDSource := NI_DAQ_CNS_Value(PhCtrDSourceComboBox.Text);
PhCtrDGateComboBox.Text := HardwareSettings.ReadString(Section,'PhCtrDGate','ND_PFI_14');
HardwareControl.PhCtrDGate := NI_DAQ_CNS_Value(PhCtrDGateComboBox.Text);

PhCtrEHardwareCtrComboBox.Text := HardwareSettings.ReadString(Section,'PhCtrEHardwareCtr','ND_COUNTER_7');
HardwareControl.PhCtrEHardwareCtr := NI_DAQ_CNS_Value(PhCtrEHardwareCtrComboBox.Text);
PhCtrESourceComboBox.Text := HardwareSettings.ReadString(Section,'PhCtrESource','ND_PFI_11');
HardwareControl.PhCtrESource := NI_DAQ_CNS_Value(PhCtrESourceComboBox.Text);
PhCtrEGateComboBox.Text := HardwareSettings.ReadString(Section,'PhCtrEGate','ND_PFI_10');
HardwareControl.PhCtrEGate := NI_DAQ_CNS_Value(PhCtrEGateComboBox.Text);

end;


procedure THardwareSettingsFrm.SaveIniFile;
begin
HardwareSettings.WriteString('user','AOutsDevNum',AOutsDevNumSpinEdit.Text);
StatusBar1.SimpleText := 'Saving settings /';
HardwareSettings.WriteString('user','AOutsTrigLine',AOutsTrigLineComboBox.Text);
HardwareSettings.WriteString('user','AOutsTrigLinePolarity',AOutsTrigLinePolarityComboBox.Text);
StatusBar1.SimpleText := 'Saving settings -';
HardwareSettings.WriteString('user','CountersDevNum',CountersDevNumSpinEdit.Text);
HardwareSettings.WriteString('user','CountersTrigLine',CountersTrigLineComboBox.Text);
StatusBar1.SimpleText := 'Saving settings \';
HardwareSettings.WriteString('user','CountersTrigLinePolarity',CountersTrigLinePolarityComboBox.Text);
HardwareSettings.WriteString('user','PixelClockHardwareCtr',PixelClockHardwareCtrComboBox.Text);
StatusBar1.SimpleText := 'Saving settings |';
HardwareSettings.WriteString('user','PixelClockSource',PixelClockSourceComboBox.Text);
HardwareSettings.WriteString('user','PixelClockSourcePolarity',PixelClockSourcePolarityComboBox.Text);
StatusBar1.SimpleText := 'Saving settings /';
HardwareSettings.WriteString('user','PixelClockOutputSignal',PixelClockOutputSignalComboBox.Text);
HardwareSettings.WriteString('user','PixelClockOutputSource',PixelClockOutputSourceComboBox.Text);
StatusBar1.SimpleText := 'Saving settings -';
HardwareSettings.WriteString('user','PixelClockOutputPolarity',PixelClockOutputPolarityComboBox.Text);
HardwareSettings.WriteString('user','LineClockHardwareCtr',LineClockHardwareCtrComboBox.Text);
StatusBar1.SimpleText := 'Saving settings \';
HardwareSettings.WriteString('user','LineClockSource',LineClockSourceComboBox.Text);
HardwareSettings.WriteString('user','LineClockSourcePolarity',LineClockSourcePolarityComboBox.Text);
StatusBar1.SimpleText := 'Saving settings |';
HardwareSettings.WriteString('user','LineClockOutputSignal',LineClockOutputSignalComboBox.Text);
HardwareSettings.WriteString('user','LineClockOutputSource',LineClockOutputSourceComboBox.Text);
StatusBar1.SimpleText := 'Saving settings /';
HardwareSettings.WriteString('user','LineClockOutputPolarity',LineClockOutputPolarityComboBox.Text);
HardwareSettings.WriteString('user','FrameClockHardwareCtr',FrameClockHardwareCtrComboBox.Text);
StatusBar1.SimpleText := 'Saving settings -';
HardwareSettings.WriteString('user','FrameClockSource',FrameClockSourceComboBox.Text);
HardwareSettings.WriteString('user','FrameClockSourcePolarity',FrameClockSourcePolarityComboBox.Text);
StatusBar1.SimpleText := 'Saving settings \';
HardwareSettings.WriteString('user','FrameClockOutputSignal',FrameClockOutputSignalComboBox.Text);
HardwareSettings.WriteString('user','FrameClockOutputSource',FrameClockOutputSourceComboBox.Text);
StatusBar1.SimpleText := 'Saving settings |';
HardwareSettings.WriteString('user','FrameClockOutputPolarity',FrameClockOutputPolarityComboBox.Text);
HardwareSettings.WriteString('user','FrameClockGate',FrameClockGateComboBox.Text);
StatusBar1.SimpleText := 'Saving settings /';
HardwareSettings.WriteString('user','FrameClockGatePolarity',FrameClockGatePolarityComboBox.Text);
HardwareSettings.WriteString('user','PhCtrAHardwareCtr',PhCtrAHardwareCtrComboBox.Text);
StatusBar1.SimpleText := 'Saving settings -';
HardwareSettings.WriteString('user','PhCtrASource',PhCtrASourceComboBox.Text);
HardwareSettings.WriteString('user','PhCtrAGate',PhCtrAGateComboBox.Text);
StatusBar1.SimpleText := 'Saving settings \';
HardwareSettings.WriteString('user','PhCtrBHardwareCtr',PhCtrBHardwareCtrComboBox.Text);
HardwareSettings.WriteString('user','PhCtrBSource',PhCtrBSourceComboBox.Text);
StatusBar1.SimpleText := 'Saving settings |';
HardwareSettings.WriteString('user','PhCtrBGate',PhCtrBGateComboBox.Text);
HardwareSettings.WriteString('user','PhCtrCHardwareCtr',PhCtrCHardwareCtrComboBox.Text);
StatusBar1.SimpleText := 'Saving settings /';
HardwareSettings.WriteString('user','PhCtrCSource',PhCtrCSourceComboBox.Text);
HardwareSettings.WriteString('user','PhCtrCGate',PhCtrCGateComboBox.Text);
StatusBar1.SimpleText := 'Saving settings -';
HardwareSettings.WriteString('user','PhCtrDHardwareCtr',PhCtrDHardwareCtrComboBox.Text);
HardwareSettings.WriteString('user','PhCtrDSource',PhCtrDSourceComboBox.Text);
StatusBar1.SimpleText := 'Saving settings \';
HardwareSettings.WriteString('user','PhCtrDGate',PhCtrDGateComboBox.Text);
HardwareSettings.WriteString('user','PhCtrEHardwareCtr',PhCtrEHardwareCtrComboBox.Text);
StatusBar1.SimpleText := 'Saving settings |';
HardwareSettings.WriteString('user','PhCtrESource',PhCtrESourceComboBox.Text);
HardwareSettings.WriteString('user','PhCtrEGate',PhCtrEGateComboBox.Text);
StatusBar1.SimpleText := 'Settings saved.';
Sleep(200);
end;


// **********************************************************************************
// **********************************************************************************
// **********************************************************************************
// **********************************************************************************
// ******************************************                        String Functions


{  This function converts the strings used in the settings .ini file, the combo boxes, and the .txt
 data files into the Cardinal value used in the hardware calls. See nidaqcns.
   The variables that these values are assigned to are in ExtFunctionsTakeTwo.  }
function THardwareSettingsFrm.NI_DAQ_CNS_Value(const ConstString: string): Cardinal;
begin
if ConstString = 'N/A' then Result := 0;
if ConstString = 'ND_BUFFERED_PERIOD_MSR' then Result := 12300;
if ConstString = 'ND_BUFFERED_PULSE_WIDTH_MSR' then Result := 12400;
if ConstString = 'ND_NONE' then Result := 26300;
if ConstString = 'ND_DONT_CARE' then Result := 15900;
if ConstString = 'ND_OTHER_GPCTR_GATE' then Result := 50590;
if ConstString = 'ND_OTHER_GPCTR_OUTPUT' then Result := 27300;
if ConstString = 'ND_INTERNAL_MAX_TIMEBASE' then Result := 50660;
if ConstString = 'ND_DEFAULT_PFI_LINE' then Result := 51580;
if ConstString = 'ND_INTERNAL_20_MHZ' then Result := 19400;
if ConstString = 'ND_INTERNAL_100_KHZ' then Result := 19200;
if ConstString = 'ND_HIGH_TO_LOW' then Result := 18100;
if ConstString = 'ND_LOW_TO_HIGH' then Result := 24100;
if ConstString = 'ND_NEGATIVE' then Result := 26100;
if ConstString = 'ND_POSITIVE' then Result := 29100;
if ConstString = 'ND_PFI_0' then Result := 28100;
if ConstString = 'ND_PFI_1' then Result := 28200;
if ConstString = 'ND_PFI_2' then Result := 28300;
if ConstString = 'ND_PFI_3' then Result := 28400;
if ConstString = 'ND_PFI_4' then Result := 28500;
if ConstString = 'ND_PFI_5' then Result := 28600;
if ConstString = 'ND_PFI_6' then Result := 28700;
if ConstString = 'ND_PFI_7' then Result := 28800;
if ConstString = 'ND_PFI_8' then Result := 28900;
if ConstString = 'ND_PFI_9' then Result := 29000;
if ConstString = 'ND_PFI_10' then Result := 50280;
if ConstString = 'ND_PFI_11' then Result := 50290;
if ConstString = 'ND_PFI_12' then Result := 50300;
if ConstString = 'ND_PFI_13' then Result := 50310;
if ConstString = 'ND_PFI_14' then Result := 50320;
if ConstString = 'ND_PFI_15' then Result := 50330;
if ConstString = 'ND_PFI_16' then Result := 50340;
if ConstString = 'ND_PFI_17' then Result := 50350;
if ConstString = 'ND_PFI_18' then Result := 50360;
if ConstString = 'ND_PFI_19' then Result := 50370;
if ConstString = 'ND_PFI_20' then Result := 50380;
if ConstString = 'ND_PFI_21' then Result := 50390;
if ConstString = 'ND_PFI_22' then Result := 50400;
if ConstString = 'ND_PFI_23' then Result := 50410;
if ConstString = 'ND_PFI_24' then Result := 50420;
if ConstString = 'ND_PFI_25' then Result := 50430;
if ConstString = 'ND_PFI_26' then Result := 50440;
if ConstString = 'ND_PFI_27' then Result := 50450;
if ConstString = 'ND_PFI_28' then Result := 50460;
if ConstString = 'ND_PFI_29' then Result := 50470;
if ConstString = 'ND_PFI_30' then Result := 50480;
if ConstString = 'ND_PFI_31' then Result := 50490;
if ConstString = 'ND_PFI_32' then Result := 50500;
if ConstString = 'ND_PFI_33' then Result := 50510;
if ConstString = 'ND_PFI_34' then Result := 50520;
if ConstString = 'ND_PFI_35' then Result := 50530;
if ConstString = 'ND_PFI_36' then Result := 50540;
if ConstString = 'ND_PFI_37' then Result := 50550;
if ConstString = 'ND_PFI_38' then Result := 50560;
if ConstString = 'ND_PFI_39' then Result := 50570;
if ConstString = 'ND_RTSI_0' then Result := 31400;
if ConstString = 'ND_RTSI_1' then Result := 31500;
if ConstString = 'ND_RTSI_2' then Result := 31600;
if ConstString = 'ND_RTSI_3' then Result := 31700;
if ConstString = 'ND_RTSI_4' then Result := 31800;
if ConstString = 'ND_RTSI_5' then Result := 31900;
if ConstString = 'ND_RTSI_6' then Result := 32000;
if ConstString = 'ND_COUNTER_0' then Result := 13300;
if ConstString = 'ND_COUNTER_1' then Result := 13400;
if ConstString = 'ND_COUNTER_2' then Result := 13310;
if ConstString = 'ND_COUNTER_3' then Result := 13320;
if ConstString = 'ND_COUNTER_4' then Result := 13330;
if ConstString = 'ND_COUNTER_5' then Result := 13340;
if ConstString = 'ND_COUNTER_6' then Result := 13350;
if ConstString = 'ND_COUNTER_7' then Result := 13360;
if ConstString = 'ND_GPCTR0_GATE' then Result := 17300;
if ConstString = 'ND_GPCTR0_OUTPUT' then Result := 17400;
if ConstString = 'ND_GPCTR0_SOURCE' then Result := 17500;
if ConstString = 'ND_GPCTR1_GATE' then Result := 17600;
if ConstString = 'ND_GPCTR1_OUTPUT' then Result := 17700;
if ConstString = 'ND_GPCTR1_SOURCE' then Result := 17800;
if ConstString = 'ND_GPCTR2_GATE' then Result := 17320;
if ConstString = 'ND_GPCTR2_OUTPUT' then Result := 17420;
if ConstString = 'ND_GPCTR2_SOURCE' then Result := 17520;
if ConstString = 'ND_GPCTR3_GATE' then Result := 17330;
if ConstString = 'ND_GPCTR3_OUTPUT' then Result := 17430;
if ConstString = 'ND_GPCTR3_SOURCE' then Result := 17530;
if ConstString = 'ND_GPCTR4_GATE' then Result := 17340;
if ConstString = 'ND_GPCTR4_OUTPUT' then Result := 17440;
if ConstString = 'ND_GPCTR4_SOURCE' then Result := 17540;
if ConstString = 'ND_GPCTR5_GATE' then Result := 17350;
if ConstString = 'ND_GPCTR5_OUTPUT' then Result := 17450;
if ConstString = 'ND_GPCTR5_SOURCE' then Result := 17550;
if ConstString = 'ND_GPCTR6_GATE' then Result := 17360;
if ConstString = 'ND_GPCTR6_OUTPUT' then Result := 17460;
if ConstString = 'ND_GPCTR6_SOURCE' then Result := 17660;
if ConstString = 'ND_GPCTR7_GATE' then Result := 17370;
if ConstString = 'ND_GPCTR7_OUTPUT' then Result := 17470;
if ConstString = 'ND_GPCTR7_SOURCE' then Result := 17570;
if ConstString = 'ND_INTERNAL_LINE_0' then Result := 50710;
if ConstString = 'ND_INTERNAL_LINE_8' then Result := 50790;
end;


{ This function converts the SmallInt number returned from the InitBrds call
into a string of the corresponding hardware card.
  NOTE:- The devise numbers and other useful features can be found in the National
  Instruments Measurement and Automation Explorer. }
function THardwareSettingsFrm.PCIBoard(const DeviseNumberCode: SmallInt): string;
begin
case DeviseNumberCode of
1: Result := 'Not a National Instruments DAQ device';
7: Result := 'PC-DIO-24';
8: Result := 'AT-DIO-32F';
12: Result := 'PC-DIO-96';
13: Result := 'PC-LPM-16';
15: Result := 'AT-AO-6';
25: Result := 'AT-MIO-16E-2';
26: Result := 'AT-AO-10';
32: Result := 'NEC-MIO-16E-4';
35: Result := 'DAQCard DIO-24';
36: Result := 'AT-MIO-16E-10';
37: Result := 'AT-MIO-16DE-10';
38: Result := 'AT-MIO-64E-3';
39: Result := 'AT-MIO-16XE-50';
40: Result := 'NEC-AI-16E-4';
41: Result := 'NEC-MIO-16XE-50';
42: Result := 'NEC-AI-16XE-50';
44: Result := 'AT-MIO-16E-1';
50: Result := 'AT-MIO-16XE-10';
51: Result := 'AT-AI-16XE-10';
52: Result := 'DAQCard-AI-16XE-50';
53: Result := 'DAQCard-AI-16E-4';
65: Result := 'PC-DIO-24/PnP';
66: Result := 'PC-DIO-96/PnP';
67: Result := 'AT-DIO-32HS';
68: Result := 'DAQCard-6533';
// todo -oNeil: check for typo error with NI
//75: Result := 'DAQPad-6507';
75: Result := 'DAQPad-6508';
76: Result := 'DAQPad-6020E for USB';
88: Result := 'DAQCard-6062E';
89: Result := 'DAQCard-6715';
90: Result := 'DAQCard-6023E';
91: Result := 'DAQCard-6024E';
200:  Result := 'PCI-DIO-96';
201:  Result := 'PCI-';
202:  Result := 'PCI-MIO-16XE-50';
203:  Result := 'PCI-5102';
204:  Result := 'PCI-MIO-16XE-10';
205:  Result := 'PCI-MIO-16E-1';
206:  Result := 'PCI-MIO-16E-4';
207:  Result := 'PXI-6070E';
208:  Result := 'PXI-6040E';
209:  Result := 'PXI-6030E';
210:  Result := 'PXI-6011E';
211:  Result := 'PCI-DIO-32HS';
215:  Result := 'PXI-6533';
216:  Result := 'PCI-6534';
218:  Result := 'PXI-6534';
220:  Result := 'PCI-6031E (MIO-64XE-10)';
221:  Result := 'PCI-6032E (AI-16XE-10)';
222:  Result := 'PCI-6033E (AI-64XE-10)';
223:  Result := 'PCI-6071E (MIO-64E-1)';
232:  Result := 'PCI-6602';
233:  Result := 'NI 4451 for PCI';
234:  Result := 'NI 4452 for PCI';
235:  Result := 'NI 4551 for PCI';
236:  Result := 'NI 4552 for PCI';
237:  Result := 'PXI-6602';
240:  Result := 'PXI-6508';
241:  Result := 'PCI-6110';
244:  Result := 'PCI-6111';
256:  Result := 'PCI-6503';
257:  Result := 'PXI-6503';
258:  Result := 'PXI-6071E';
259:  Result := 'PXI-6031E';
261:  Result := 'PCI-6711';
262:  Result := 'PCI-6711';
263:  Result := 'PCI-6713';
264:  Result := 'PXI-6713';
265:  Result := 'PCI-6704';
266:  Result := 'PXI-6704';
267:  Result := 'PCI-6023E';
268:  Result := 'PXI-6023E';
269:  Result := 'PCI-6024E';
270:  Result := 'PXI-6024E';
271:  Result := 'PCI-6025E';
272:  Result := 'PXI-6025E';
273:  Result := 'PCI-6052E';
274:  Result := 'PXI-6052E';
275:  Result := 'DAQPad-6070E (for 1394)';
276:  Result := 'DAQPad-6052E for 1394 (mass termination)';
285:  Result := 'PCI-6527';
286:  Result := 'PXI-6527';
308:  Result := 'PCI-6601';
311:  Result := 'PCI-6703';
314:  Result := 'PCI-6034E';
315:  Result := 'PXI-6034E';
316:  Result := 'PCI-6035E';
317:  Result := 'PXI-6035E';
318:  Result := 'PXI-6703';
319:  Result := 'PXI-6608';
321:  Result := 'NI 4454 for PCI';
327:  Result := 'PCI-6608';
329:  Result := 'NI 6222 for PCI';
330:  Result := 'NI 6222 for PXI';
331:  Result := 'NI 6224 for Ethernet';
332:  Result := 'DAQPad-6052E for USB';
335:  Result := 'NI 4472 for PXI/CompactPCI';
338:  Result := 'PCI-6115';
339:  Result := 'PXI-6115';
340:  Result := 'PCI-6120';
341:  Result := 'PXI-6120';
342:  Result := 'NI 4472 for PCI';
348:  Result := 'DAQCard-6036E';
// todo -oNeil: check for typo error with NI
//348:  Result := 'NI 6036E for PCI';
349:  Result := 'NI 6731 for PCI';
350:  Result := 'NI 6733 for PCI';
351:  Result := 'NI 6731 for PXI/Compact PCI';
352:  Result := 'NI 6733 for PXI/Compact PCI';
353:  Result := 'PCI-4474';
354:  Result := 'PXI-4474';
361:  Result := 'DAQPad-6052E for 1394 (BNC)';
366:  Result := 'PCI-6013';
367:  Result := 'PCI-6014';
else
Result := ' Devise Number Case Invalid';
end;
end;

// this function returns the now redundant counter which has just been entered into
// another counter - á la 'NewStr'
function THardwareSettingsFrm.RedundantCtr(const NewCtr: string): string;
var
TempStr: string;
begin
TempStr := PixelClockHardwareCtrComboBox.Text;
if NewCtr = TempStr then Result := 'Pixel Clock';
TempStr := LineClockHardwareCtrComboBox.Text;
if NewCtr = TempStr then Result := 'Line Clock';
TempStr := FrameClockHardwareCtrComboBox.Text;
if NewCtr = TempStr then Result := 'Frame Clock';
TempStr := PhCtrAHardwareCtrComboBox.Text;
if NewCtr = TempStr then Result := 'Photon Counter A';
TempStr := PhCtrBHardwareCtrComboBox.Text;
if NewCtr = TempStr then Result := 'Photon Counter B';
TempStr := PhCtrCHardwareCtrComboBox.Text;
if NewCtr = TempStr then Result := 'Photon Counter C';
TempStr := PhCtrDHardwareCtrComboBox.Text;
if NewCtr = TempStr then Result := 'Photon Counter D';
TempStr := PhCtrEHardwareCtrComboBox.Text;
if NewCtr = TempStr then Result := 'Photon Counter E';
end;


procedure THardwareSettingsFrm.Compare;
begin
if (HardwareControl.AOutsDevNum = HardwareControl.CountersDevNum) then DuplicateDevice := True
else DuplicateDevice := False;
if ( (HardwareControl.LineClockHardwareCtr = HardwareControl.PixelClockHardwareCtr) OR
   (HardwareControl.LineClockHardwareCtr = HardwareControl.FrameClockHardwareCtr) OR
   (HardwareControl.LineClockHardwareCtr = HardwareControl.PhCtrAHardwareCtr) OR
   (HardwareControl.LineClockHardwareCtr = HardwareControl.PhCtrBHardwareCtr) OR
   (HardwareControl.LineClockHardwareCtr = HardwareControl.PhCtrCHardwareCtr) OR
   (HardwareControl.LineClockHardwareCtr = HardwareControl.PhCtrDHardwareCtr) OR
   (HardwareControl.LineClockHardwareCtr = HardwareControl.PhCtrEHardwareCtr) OR
   (HardwareControl.PixelClockHardwareCtr = HardwareControl.FrameClockHardwareCtr) OR
   (HardwareControl.PixelClockHardwareCtr = HardwareControl.PhCtrAHardwareCtr) OR
   (HardwareControl.PixelClockHardwareCtr = HardwareControl.PhCtrBHardwareCtr) OR
   (HardwareControl.PixelClockHardwareCtr = HardwareControl.PhCtrCHardwareCtr) OR
   (HardwareControl.PixelClockHardwareCtr = HardwareControl.PhCtrDHardwareCtr) OR
   (HardwareControl.PixelClockHardwareCtr = HardwareControl.PhCtrEHardwareCtr) OR
   (HardwareControl.FrameClockHardwareCtr = HardwareControl.PhCtrAHardwareCtr) OR
   (HardwareControl.FrameClockHardwareCtr = HardwareControl.PhCtrBHardwareCtr) OR
   (HardwareControl.FrameClockHardwareCtr = HardwareControl.PhCtrCHardwareCtr) OR
   (HardwareControl.FrameClockHardwareCtr = HardwareControl.PhCtrDHardwareCtr) OR
   (HardwareControl.FrameClockHardwareCtr = HardwareControl.PhCtrEHardwareCtr) OR
   (HardwareControl.PhCtrAHardwareCtr = HardwareControl.PhCtrBHardwareCtr) OR
   (HardwareControl.PhCtrAHardwareCtr = HardwareControl.PhCtrCHardwareCtr) OR
   (HardwareControl.PhCtrAHardwareCtr = HardwareControl.PhCtrDHardwareCtr) OR
   (HardwareControl.PhCtrAHardwareCtr = HardwareControl.PhCtrEHardwareCtr) OR
   (HardwareControl.PhCtrBHardwareCtr = HardwareControl.PhCtrCHardwareCtr) OR
   (HardwareControl.PhCtrBHardwareCtr = HardwareControl.PhCtrDHardwareCtr) OR
   (HardwareControl.PhCtrBHardwareCtr = HardwareControl.PhCtrEHardwareCtr) OR
   (HardwareControl.PhCtrCHardwareCtr = HardwareControl.PhCtrDHardwareCtr) OR
   (HardwareControl.PhCtrCHardwareCtr = HardwareControl.PhCtrEHardwareCtr) OR
   (HardwareControl.PhCtrDHardwareCtr = HardwareControl.PhCtrEHardwareCtr) ) then DuplicateLine := True
else DuplicateLine := False;

if DuplicateLine then MessageDlg('One or more counters have been doubley allocated' + #10 +
                                 'Please re-check and try again.',mtWarning,[mbOK],0);
if DuplicateDevice then MessageDlg('The device numbers shouldn' + #39 + 't be the same.' + #10 +
                                 'Please re-check and try again.',mtWarning,[mbOK],0);

end;


function THardwareSettingsFrm.FramePFI: Byte;
var
TempStr: string;
begin
TempStr := FrameClockOutputSignalComboBox.Text[8];
if Length(FrameClockOutputSignalComboBox.Text) > 8 then TempStr := TempStr + FrameClockOutputSignalComboBox.Text[9];
Result := StrToInt(TempStr);
end;

function THardwareSettingsFrm.LinePFI: Byte;
var
TempStr: string;
begin
TempStr := LineClockOutputSignalComboBox.Text[8];
if Length(LineClockOutputSignalComboBox.Text) > 8 then TempStr := TempStr + LineClockOutputSignalComboBox.Text[9];
Result := StrToInt(TempStr);
end;


// **********************************************************************************
// **********************************************************************************
// **********************************************************************************
// **********************************************************************************
// *************************************************                  OnChange Events

//  The OnChange events below change the cardinal values declared in ExtFunctionsTakeTwo,
//  sets the Modified boolean variable to true if it isn't the same as the default value, and
//  takes care of any knock on counters settings and warnings
procedure THardwareSettingsFrm.AOutsDevNumSpinEditChange(Sender: TObject);
begin
HardwareControl.AOutsDevNum := StrToInt(AOutsDevNumSpinEdit.Text);
Modified := True;
if HardwareControl.AOutsDevNum = StrToInt(HardwareSettings.ReadString(Section,'AOutsDevNum','3'))
then Modified := False;
end;

procedure THardwareSettingsFrm.AOutsTrigLineComboBoxChange(Sender: TObject);
begin
HardwareControl.AOutTrigLine := NI_DAQ_CNS_Value(AOutsTrigLineComboBox.Text);
Modified := True;
if AOutsTrigLineComboBox.Text = HardwareSettings.ReadString(Section,'AOutsTrigLine','ND_PFI_3')
then Modified := False;
end;

procedure THardwareSettingsFrm.AOutsTrigLinePolarityComboBoxChange(Sender: TObject);
begin
HardwareControl.AOutTrigLinePolarity := NI_DAQ_CNS_Value(AOutsTrigLinePolarityComboBox.Text);
Modified := True;
if AOutsTrigLinePolarityComboBox.Text = HardwareSettings.ReadString(Section,'AOutsTrigLinePolarity','ND_HIGH_TO_LOW')
then Modified := False;
end;

procedure THardwareSettingsFrm.CountersDevNumSpinEditChange(Sender: TObject);
begin
HardwareControl.CountersDevNum := StrToInt(CountersDevNumSpinEdit.Text);
PixelClockDevNumSpinEdit.Text := CountersDevNumSpinEdit.Text;
LineClockDevNumSpinEdit.Text := CountersDevNumSpinEdit.Text;
FrameClockDevNumSpinEdit.Text := CountersDevNumSpinEdit.Text;
PhCtrADevNumSpinEdit.Text := CountersDevNumSpinEdit.Text;
PhCtrBDevNumSpinEdit.Text := CountersDevNumSpinEdit.Text;
PhCtrCDevNumSpinEdit.Text := CountersDevNumSpinEdit.Text;
PhCtrDDevNumSpinEdit.Text := CountersDevNumSpinEdit.Text;
PhCtrEDevNumSpinEdit.Text := CountersDevNumSpinEdit.Text;
Modified := True;
if HardwareControl.CountersDevNum = StrToInt(HardwareSettings.ReadString(Section,'CountersDevNum','2'))
then Modified := False;
end;

procedure THardwareSettingsFrm.CountersTrigLineComboBoxChange(Sender: TObject);
begin
HardwareControl.CountersTrigLine := NI_DAQ_CNS_Value(CountersTrigLineComboBox.Text);
Modified := True;
if CountersTrigLineComboBox.Text = HardwareSettings.ReadString(Section,'CountersTrigLine','ND_PFI_7')
then Modified := False;
end;

procedure THardwareSettingsFrm.CountersTrigLinePolarityComboBoxChange(Sender: TObject);
begin
HardwareControl.CountersTrigLinePolarity := NI_DAQ_CNS_Value(CountersTrigLinePolarityComboBox.Text);
Modified := True;
if CountersTrigLinePolarityComboBox.Text = HardwareSettings.ReadString(Section,'CountersLinePolarity','ND_HIGH_TO_LOW')
then Modified := False;
end;

// **********************************************************************************
// ************************************************                       Pixel Clock

procedure THardwareSettingsFrm.PixelClockHardwareCtrComboBoxChange(Sender: TObject);
var
TempStr: string;
RedundantStr: string;
TempInt: integer;
begin
TempStr := PixelClockHardwareCtrComboBox.Text;
RedundantStr := RedundantCtr(TempStr);
StatusBar1.SimpleText := 'Please check ' + RedundantStr + ' for redundancy.';
HardwareControl.PixelClockHardwareCtr := NI_DAQ_CNS_Value(TempStr);
Modified := True;
if TempStr = HardwareSettings.ReadString(Section,'PixelClockHardwareCtr','ND_COUNTER_1')
then Modified := False;
//  Sets the corresponding lines to defaults in string lists
TempInt := StrToInt(TempStr[12]);
PixelClockOutputSignalComboBox.Text := SGOLines[TempInt].Strings[2];
PixelClockOutputSignalComboBoxChange(Self);
PixelClockOutputSourceComboBox.Text := SGOLines[TempInt].Strings[3];
PixelClockOutputSourceComboBoxChange(Self);
end;

procedure THardwareSettingsFrm.PixelClockSourceComboBoxChange(Sender: TObject);
begin
HardwareControl.PixelClockSource := NI_DAQ_CNS_Value(PixelClockSourceComboBox.Text);
Modified := True;
if PixelClockSourceComboBox.Text = HardwareSettings.ReadString(Section,'PixelClockSource','ND_INTERNAL_MAX_TIMEBASE')
then Modified := False;
end;

procedure THardwareSettingsFrm.PixelClockSourcePolarityComboBoxChange(Sender: TObject);
begin
HardwareControl.PixelClockSourcePolarity := NI_DAQ_CNS_Value(PixelClockSourcePolarityComboBox.Text);
Modified := True;
if PixelClockSourcePolarityComboBox.Text = HardwareSettings.ReadString(Section,'PixelClockSourcePolarity','N/A')
then Modified := False;
end;

procedure THardwareSettingsFrm.PixelClockOutputSignalComboBoxChange(Sender: TObject);
begin
HardwareControl.PixelClockOutputSignal := NI_DAQ_CNS_Value(PixelClockOutputSignalComboBox.Text);
Modified := True;
if PixelClockOutputSignalComboBox.Text = HardwareSettings.ReadString(Section,'PixelClockOutputSignal','ND_PFI_32')
then Modified := False;
end;

procedure THardwareSettingsFrm.PixelClockOutputSourceComboBoxChange(Sender: TObject);
begin
HardwareControl.PixelClockOutputSource := NI_DAQ_CNS_Value(PixelClockOutputSourceComboBox.Text);
Modified := True;
if PixelClockOutputSourceComboBox.Text = HardwareSettings.ReadString(Section,'PixelClockOutputSource','ND_GPCTR1_OUTPUT')
then Modified := False;
end;

procedure THardwareSettingsFrm.PixelClockOutputPolarityComboBoxChange(Sender: TObject);
begin
HardwareControl.PixelClockSourcePolarity := NI_DAQ_CNS_Value(PixelClockOutputPolarityComboBox.Text);
Modified := True;
if PixelClockOutputPolarityComboBox.Text = HardwareSettings.ReadString(Section,'PixelClockOutputPolarity','ND_LOW_TO_HIGH')
then Modified := False;
end;


// **********************************************************************************
// ************************************************                        Line Clock

procedure THardwareSettingsFrm.LineClockHardwareCtrComboBoxChange(Sender: TObject);
var
TempStr: string;
RedundantStr: string;
TempInt: integer;
begin
TempStr := LineClockHardwareCtrComboBox.Text;
RedundantStr := RedundantCtr(TempStr);
StatusBar1.SimpleText := 'Please check ' + RedundantStr + ' for redundancy.';
HardwareControl.LineClockHardwareCtr := NI_DAQ_CNS_Value(TempStr);
Modified := True;
if TempStr = HardwareSettings.ReadString(Section,'LineClockHardwareCtr','ND_COUNTER_2')
then Modified := False;
//  Sets the corresponding lines to defaults in string lists
TempInt := StrToInt(TempStr[12]);
LineClockOutputSignalComboBox.Text := SGOLines[TempInt].Strings[2];
LineClockOutputSignalComboBoxChange(Self);
LineClockOutputSourceComboBox.Text := SGOLines[TempInt].Strings[3];
LineClockOutputSourceComboBoxChange(Self);
end;

procedure THardwareSettingsFrm.LineClockSourceComboBoxChange(Sender: TObject);
begin
HardwareControl.LineClockSource := NI_DAQ_CNS_Value(LineClockSourceComboBox.Text);
Modified := True;
if LineClockSourceComboBox.Text = HardwareSettings.ReadString(Section,'LineClockSource','ND_INTERNAL_MAX_TIMEBASE')
then Modified := False;
end;

procedure THardwareSettingsFrm.LineClockSourcePolarityComboBoxChange(Sender: TObject);
begin
HardwareControl.LineClockSourcePolarity := NI_DAQ_CNS_Value(LineClockSourcePolarityComboBox.Text);
Modified := True;
if LineClockSourcePolarityComboBox.Text = HardwareSettings.ReadString(Section,'LineClockSourcePolarity','N/A')
then Modified := False;
end;

procedure THardwareSettingsFrm.LineClockOutputSignalComboBoxChange(Sender: TObject);
begin
HardwareControl.LineClockOutputSignal := NI_DAQ_CNS_Value(LineClockOutputSignalComboBox.Text);
Modified := True;
if LineClockOutputSignalComboBox.Text = HardwareSettings.ReadString(Section,'LineClockOutputSignal','ND_PFI_28')
then Modified := False;
end;

procedure THardwareSettingsFrm.LineClockOutputSourceComboBoxChange(Sender: TObject);
begin
HardwareControl.LineClockOutputSource := NI_DAQ_CNS_Value(LineClockOutputSourceComboBox.Text);
Modified := True;
if LineClockOutputSourceComboBox.Text = HardwareSettings.ReadString(Section,'LineClockOutputSource','ND_GPCTR2_OUTPUT')
then Modified := False;
end;

procedure THardwareSettingsFrm.LineClockOutputPolarityComboBoxChange(Sender: TObject);
begin
HardwareControl.LineClockSourcePolarity := NI_DAQ_CNS_Value(LineClockOutputPolarityComboBox.Text);
Modified := True;
if LineClockOutputPolarityComboBox.Text = HardwareSettings.ReadString(Section,'LineClockOutputPolarity','ND_LOW_TO_HIGH')
then Modified := False;
end;


// **********************************************************************************
// ************************************************                       Frame Clock

procedure THardwareSettingsFrm.FrameClockHardwareCtrComboBoxChange(Sender: TObject);
var
TempStr: string;
RedundantStr: string;
TempInt: integer;
begin
TempStr := FrameClockHardwareCtrComboBox.Text;
RedundantStr := RedundantCtr(TempStr);
StatusBar1.SimpleText := 'Please check ' + RedundantStr + ' for redundancy.';
HardwareControl.FrameClockHardwareCtr := NI_DAQ_CNS_Value(TempStr);
Modified := True;
if TempStr = HardwareSettings.ReadString(Section,'FrameClockHardwareCtr','ND_COUNTER_0')
then Modified := False;
//  Sets the corresponding Frames to defaults in string lists
TempInt := StrToInt(TempStr[12]);
FrameClockOutputSignalComboBox.Text := SGOLines[TempInt].Strings[2];
FrameClockOutputSignalComboBoxChange(Self);
FrameClockOutputSourceComboBox.Text := SGOLines[TempInt].Strings[3];
FrameClockOutputSourceComboBoxChange(Self);
end;

procedure THardwareSettingsFrm.FrameClockSourceComboBoxChange(Sender: TObject);
begin
HardwareControl.FrameClockSource := NI_DAQ_CNS_Value(FrameClockSourceComboBox.Text);
Modified := True;
if FrameClockSourceComboBox.Text = HardwareSettings.ReadString(Section,'FrameClockSource','ND_PFI_39')
then Modified := False;
end;

procedure THardwareSettingsFrm.FrameClockSourcePolarityComboBoxChange(Sender: TObject);
begin
HardwareControl.FrameClockSourcePolarity := NI_DAQ_CNS_Value(FrameClockSourcePolarityComboBox.Text);
Modified := True;
if FrameClockSourcePolarityComboBox.Text = HardwareSettings.ReadString(Section,'FrameClockSourcePolarity','ND_HIGH_TO_LOW')
then Modified := False;
end;

procedure THardwareSettingsFrm.FrameClockOutputSignalComboBoxChange(Sender: TObject);
begin
HardwareControl.FrameClockOutputSignal := NI_DAQ_CNS_Value(FrameClockOutputSignalComboBox.Text);
Modified := True;
if FrameClockOutputSignalComboBox.Text = HardwareSettings.ReadString(Section,'FrameClockOutputSignal','ND_PFI_36')
then Modified := False;
end;

procedure THardwareSettingsFrm.FrameClockOutputSourceComboBoxChange(Sender: TObject);
begin
HardwareControl.FrameClockOutputSource := NI_DAQ_CNS_Value(FrameClockOutputSourceComboBox.Text);
Modified := True;
if FrameClockOutputSourceComboBox.Text = HardwareSettings.ReadString(Section,'FrameClockOutputSource','ND_GPCTR0_OUTPUT')
then Modified := False;
end;

procedure THardwareSettingsFrm.FrameClockOutputPolarityComboBoxChange(Sender: TObject);
begin
HardwareControl.FrameClockSourcePolarity := NI_DAQ_CNS_Value(FrameClockOutputPolarityComboBox.Text);
Modified := True;
if FrameClockOutputPolarityComboBox.Text = HardwareSettings.ReadString(Section,'FrameClockOutputPolarity','ND_NEGATIVE')
then Modified := False;
end;

procedure THardwareSettingsFrm.FrameClockGateComboBoxChange(Sender: TObject);
begin
HardwareControl.FrameClockGate := NI_DAQ_CNS_Value(FrameClockGateComboBox.Text);
Modified := True;
if FrameClockGateComboBox.Text = HardwareSettings.ReadString(Section,'FrameClockGate','ND_PFI_38')
then
Modified := False;
end;

procedure THardwareSettingsFrm.FrameClockGatePolarityComboBoxChange(Sender: TObject);
begin
HardwareControl.FrameClockGatePolarity := NI_DAQ_CNS_Value(FrameClockGatePolarityComboBox.Text);
Modified := True;
if FrameClockGatePolarityComboBox.Text = HardwareSettings.ReadString(Section,'FrameClockGatePolarity','ND_LOW_TO_HIGH')
then Modified := False;
end;

// **********************************************************************************
// ************************************************                  Photon Counter A

procedure THardwareSettingsFrm.PhCtrAHardwareCtrComboBoxChange(Sender: TObject);
var
TempStr: string;
RedundantStr: string;
TempInt: integer;
begin
TempStr := PhCtrAHardwareCtrComboBox.Text;
RedundantStr := RedundantCtr(TempStr);
StatusBar1.SimpleText := 'Please check ' + RedundantStr + ' for redundancy.';
HardwareControl.PhCtrAHardwareCtr := NI_DAQ_CNS_Value(TempStr);
Modified := True;
if TempStr = HardwareSettings.ReadString(Section,'PhCtrAHardwareCtr','ND_COUNTER_3')
then Modified := False;
//  Sets the corresponding Frames to defaults in string lists
TempInt := StrToInt(TempStr[12]);
PhCtrASourceComboBox.Text := SGOLines[TempInt].Strings[0];
PhCtrASourceComboBoxChange(Self);
PhCtrAGateComboBox.Text := SGOLines[TempInt].Strings[1];
PhCtrAGateComboBoxChange(Self);
end;

procedure THardwareSettingsFrm.PhCtrASourceComboBoxChange(Sender: TObject);
begin
HardwareControl.PhCtrASource := NI_DAQ_CNS_Value(PhCtrASourceComboBox.Text);
Modified := True;
if PhCtrASourceComboBox.Text = HardwareSettings.ReadString(Section,'PhCtrASource','ND_PFI_27')
then Modified := False;
end;

procedure THardwareSettingsFrm.PhCtrAGateComboBoxChange(Sender: TObject);
begin
HardwareControl.PhCtrAGate := NI_DAQ_CNS_Value(PhCtrAGateComboBox.Text);
Modified := True;
if PhCtrAGateComboBox.Text = HardwareSettings.ReadString(Section,'PhCtrAGate','ND_PFI_26')
then Modified := False;
end;

// **********************************************************************************
// ************************************************                  Photon Counter B

procedure THardwareSettingsFrm.PhCtrBHardwareCtrComboBoxChange(Sender: TObject);
var
TempStr: string;
RedundantStr: string;
TempInt: integer;
begin
TempStr := PhCtrBHardwareCtrComboBox.Text;
RedundantStr := RedundantCtr(TempStr);
StatusBar1.SimpleText := 'Please check ' + RedundantStr + ' for redundancy.';
HardwareControl.PhCtrBHardwareCtr := NI_DAQ_CNS_Value(TempStr);
Modified := True;
if TempStr = HardwareSettings.ReadString(Section,'PhCtrBHardwareCtr','ND_COUNTER_4')
then Modified := False;
//  Sets the corresponding Frames to defaults in string lists
TempInt := StrToInt(TempStr[12]);
PhCtrBSourceComboBox.Text := SGOLines[TempInt].Strings[0];
PhCtrBSourceComboBoxChange(Self);
PhCtrBGateComboBox.Text := SGOLines[TempInt].Strings[1];
PhCtrBGateComboBoxChange(Self);
end;

procedure THardwareSettingsFrm.PhCtrBSourceComboBoxChange(Sender: TObject);
begin
HardwareControl.PhCtrBSource := NI_DAQ_CNS_Value(PhCtrBSourceComboBox.Text);
Modified := True;
if PhCtrBSourceComboBox.Text = HardwareSettings.ReadString(Section,'PhCtrBSource','ND_PFI_23')
then Modified := False;
end;

procedure THardwareSettingsFrm.PhCtrBGateComboBoxChange(Sender: TObject);
begin
HardwareControl.PhCtrBGate := NI_DAQ_CNS_Value(PhCtrBGateComboBox.Text);
Modified := True;
if PhCtrBGateComboBox.Text = HardwareSettings.ReadString(Section,'PhCtrBGate','ND_PFI_22')
then Modified := False;
end;

// **********************************************************************************
// ************************************************                  Photon Counter C

procedure THardwareSettingsFrm.PhCtrCHardwareCtrComboBoxChange(Sender: TObject);
var
TempStr: string;
RedundantStr: string;
TempInt: integer;
begin
TempStr := PhCtrCHardwareCtrComboBox.Text;
RedundantStr := RedundantCtr(TempStr);
StatusBar1.SimpleText := 'Please check ' + RedundantStr + ' for redundancy.';
HardwareControl.PhCtrCHardwareCtr := NI_DAQ_CNS_Value(TempStr);
Modified := True;
if TempStr = HardwareSettings.ReadString(Section,'PhCtrCHardwareCtr','ND_COUNTER_5')
then Modified := False;
//  Sets the corresponding Frames to defaults in string lists
TempInt := StrToInt(TempStr[12]);
PhCtrCSourceComboBox.Text := SGOLines[TempInt].Strings[0];
PhCtrCSourceComboBoxChange(Self);
PhCtrCGateComboBox.Text := SGOLines[TempInt].Strings[1];
PhCtrCGateComboBoxChange(Self);
end;

procedure THardwareSettingsFrm.PhCtrCSourceComboBoxChange(Sender: TObject);
begin
HardwareControl.PhCtrCSource := NI_DAQ_CNS_Value(PhCtrCSourceComboBox.Text);
Modified := True;
if PhCtrCSourceComboBox.Text = HardwareSettings.ReadString(Section,'PhCtrCSource','ND_PFI_19')
then Modified := False;
end;

procedure THardwareSettingsFrm.PhCtrCGateComboBoxChange(Sender: TObject);
begin
HardwareControl.PhCtrCGate := NI_DAQ_CNS_Value(PhCtrCGateComboBox.Text);
Modified := True;
if PhCtrCGateComboBox.Text = HardwareSettings.ReadString(Section,'PhCtrCGate','ND_PFI_18')
then Modified := False;
end;

// **********************************************************************************
// ************************************************                  Photon Counter D

procedure THardwareSettingsFrm.PhCtrDHardwareCtrComboBoxChange(Sender: TObject);
var
TempStr: string;
RedundantStr: string;
TempInt: integer;
begin
TempStr := PhCtrDHardwareCtrComboBox.Text;
RedundantStr := RedundantCtr(TempStr);
StatusBar1.SimpleText := 'Please check ' + RedundantStr + ' for redundancy.';
HardwareControl.PhCtrDHardwareCtr := NI_DAQ_CNS_Value(TempStr);
Modified := True;
if TempStr = HardwareSettings.ReadString(Section,'PhCtrDHardwareCtr','ND_COUNTER_6')
then Modified := False;
//  Sets the corresponding Frames to defaults in string lists
TempInt := StrToInt(TempStr[12]);
PhCtrDSourceComboBox.Text := SGOLines[TempInt].Strings[0];
PhCtrDSourceComboBoxChange(Self);
PhCtrDGateComboBox.Text := SGOLines[TempInt].Strings[1];
PhCtrDGateComboBoxChange(Self);
end;

procedure THardwareSettingsFrm.PhCtrDSourceComboBoxChange(Sender: TObject);
begin
HardwareControl.PhCtrDSource := NI_DAQ_CNS_Value(PhCtrDSourceComboBox.Text);
Modified := True;
if PhCtrDSourceComboBox.Text = HardwareSettings.ReadString(Section,'PhCtrDSource','ND_PFI_15')
then Modified := False;
end;

procedure THardwareSettingsFrm.PhCtrDGateComboBoxChange(Sender: TObject);
begin
HardwareControl.PhCtrDGate := NI_DAQ_CNS_Value(PhCtrDGateComboBox.Text);
Modified := True;
if PhCtrDGateComboBox.Text = HardwareSettings.ReadString(Section,'PhCtrDGate','ND_PFI_14')
then Modified := False;
end;

// **********************************************************************************
// ************************************************                  Photon Counter E

procedure THardwareSettingsFrm.PhCtrEHardwareCtrComboBoxChange(Sender: TObject);
var
TempStr: string;
RedundantStr: string;
TempInt: integer;
begin
TempStr := PhCtrEHardwareCtrComboBox.Text;
RedundantStr := RedundantCtr(TempStr);
StatusBar1.SimpleText := 'Please check ' + RedundantStr + ' for redundancy.';
HardwareControl.PhCtrEHardwareCtr := NI_DAQ_CNS_Value(TempStr);
Modified := True;
if TempStr = HardwareSettings.ReadString(Section,'PhCtrEHardwareCtr','ND_COUNTER_7')
then Modified := False;
//  Sets the corresponding Frames to defaults in string lists
TempInt := StrToInt(TempStr[12]);
PhCtrESourceComboBox.Text := SGOLines[TempInt].Strings[0];
PhCtrESourceComboBoxChange(Self);
PhCtrEGateComboBox.Text := SGOLines[TempInt].Strings[1];
PhCtrEGateComboBoxChange(Self);
end;

procedure THardwareSettingsFrm.PhCtrESourceComboBoxChange(Sender: TObject);
begin
HardwareControl.PhCtrESource := NI_DAQ_CNS_Value(PhCtrESourceComboBox.Text);
Modified := True;
if PhCtrESourceComboBox.Text = HardwareSettings.ReadString(Section,'PhCtrESource','ND_PFI_11')
then Modified := False;
end;

procedure THardwareSettingsFrm.PhCtrEGateComboBoxChange(Sender: TObject);
begin
HardwareControl.PhCtrEGate := NI_DAQ_CNS_Value(PhCtrEGateComboBox.Text);
Modified := True;
if PhCtrEGateComboBox.Text = HardwareSettings.ReadString(Section,'PhCtrEGate','ND_PFI_10')
then Modified := False;
end;
//                                                             End of OnChange Events
// **********************************************************************************
// **********************************************************************************
// **********************************************************************************




procedure THardwareSettingsFrm.CheckSettings;
begin

end;

procedure THardwareSettingsFrm.Image1DblClick(Sender: TObject);
var
State :TKeyboardState;
begin
GetKeyboardState(State);
if ((State[vk_Control] And 128) <> 0) then
 begin
  PixelClockChangeLinesChBx.Checked := NOT PixelClockChangeLinesChBx.Checked;
  PixelClockSourceLabel.Enabled := NOT PixelClockSourceLabel.Enabled;
  PixelClockSourceComboBox.Enabled := NOT PixelClockSourceComboBox.Enabled;
  PixelClockSourcePolarityLabel.Enabled := NOT PixelClockSourcePolarityLabel.Enabled;
  PixelClockSourcePolarityComboBox.Enabled := NOT PixelClockSourcePolarityComboBox.Enabled;
  PixelClockOutputSignalLabel.Enabled := NOT PixelClockOutputSignalLabel.Enabled;
  PixelClockOutputSignalComboBox.Enabled := NOT PixelClockOutputSignalComboBox.Enabled;
  PixelClockOutputSourceLabel.Enabled := NOT PixelClockOutputSourceLabel.Enabled;
  PixelClockOutputSourceComboBox.Enabled :=NOT PixelClockOutputSourceComboBox.Enabled;
  PixelClockOutputPolarityLabel.Enabled := NOT PixelClockOutputPolarityLabel.Enabled;
  PixelClockOutputPolarityComboBox.Enabled := NOT PixelClockOutputPolarityComboBox.Enabled;

  LineClockChangeLinesChBx.Checked := NOT LineClockChangeLinesChBx.Checked;
  LineClockSourceLabel.Enabled := NOT LineClockSourceLabel.Enabled;
  LineClockSourceComboBox.Enabled := NOT LineClockSourceComboBox.Enabled;
  LineClockSourcePolarityLabel.Enabled := NOT LineClockSourcePolarityLabel.Enabled;
  LineClockSourcePolarityComboBox.Enabled := NOT LineClockSourcePolarityComboBox.Enabled;
  LineClockOutputSignalLabel.Enabled := NOT LineClockOutputSignalLabel.Enabled;
  LineClockOutputSignalComboBox.Enabled := NOT LineClockOutputSignalComboBox.Enabled;
  LineClockOutputSourceLabel.Enabled := NOT LineClockOutputSourceLabel.Enabled;
  LineClockOutputSourceComboBox.Enabled :=NOT LineClockOutputSourceComboBox.Enabled;
  LineClockOutputPolarityLabel.Enabled := NOT LineClockOutputPolarityLabel.Enabled;
  LineClockOutputPolarityComboBox.Enabled := NOT LineClockOutputPolarityComboBox.Enabled;

  FrameClockChangeLinesChBx.Checked := NOT FrameClockChangeLinesChBx.Checked;
  FrameClockSourceLabel.Enabled := NOT FrameClockSourceLabel.Enabled;
  FrameClockSourceComboBox.Enabled := NOT FrameClockSourceComboBox.Enabled;
  FrameClockSourcePolarityLabel.Enabled := NOT FrameClockSourcePolarityLabel.Enabled;
  FrameClockSourcePolarityComboBox.Enabled := NOT FrameClockSourcePolarityComboBox.Enabled;
  FrameClockOutputSignalLabel.Enabled := NOT FrameClockOutputSignalLabel.Enabled;
  FrameClockOutputSignalComboBox.Enabled := NOT FrameClockOutputSignalComboBox.Enabled;
  FrameClockOutputSourceLabel.Enabled := NOT FrameClockOutputSourceLabel.Enabled;
  FrameClockOutputSourceComboBox.Enabled :=NOT FrameClockOutputSourceComboBox.Enabled;
  FrameClockOutputPolarityLabel.Enabled := NOT FrameClockOutputPolarityLabel.Enabled;
  FrameClockOutputPolarityComboBox.Enabled := NOT FrameClockOutputPolarityComboBox.Enabled;
  FrameClockGateLabel.Enabled := NOT FrameClockGateLabel.Enabled;
  FrameClockGateComboBox.Enabled := NOT FrameClockGateComboBox.Enabled;
  FrameClockGatePolarityLabel.Enabled := NOT FrameClockGatePolarityLabel.Enabled;
  FrameClockGatePolarityComboBox.Enabled := NOT FrameClockGatePolarityComboBox.Enabled;

  PhCtrAChangeLinesChBx.Checked := NOT PhCtrAChangeLinesChBx.Checked;
  PhCtrASourceLabel.Enabled := NOT PhCtrASourceLabel.Enabled;
  PhCtrASourceComboBox.Enabled := NOT PhCtrASourceComboBox.Enabled;
  PhCtrAGateLabel.Enabled := NOT PhCtrAGateLabel.Enabled;
  PhCtrAGateComboBox.Enabled := NOT PhCtrAGateComboBox.Enabled;

  PhCtrBChangeLinesChBx.Checked := NOT PhCtrBChangeLinesChBx.Checked;
  PhCtrBSourceLabel.Enabled := NOT PhCtrBSourceLabel.Enabled;
  PhCtrBSourceComboBox.Enabled := NOT PhCtrBSourceComboBox.Enabled;
  PhCtrBGateLabel.Enabled := NOT PhCtrBGateLabel.Enabled;
  PhCtrBGateComboBox.Enabled := NOT PhCtrBGateComboBox.Enabled;

  PhCtrCChangeLinesChBx.Checked := NOT PhCtrCChangeLinesChBx.Checked;
  PhCtrCSourceLabel.Enabled := NOT PhCtrCSourceLabel.Enabled;
  PhCtrCSourceComboBox.Enabled := NOT PhCtrCSourceComboBox.Enabled;
  PhCtrCGateLabel.Enabled := NOT PhCtrCGateLabel.Enabled;
  PhCtrCGateComboBox.Enabled := NOT PhCtrCGateComboBox.Enabled;

  PhCtrDChangeLinesChBx.Checked := NOT PhCtrDChangeLinesChBx.Checked;
  PhCtrDSourceLabel.Enabled := NOT PhCtrDSourceLabel.Enabled;
  PhCtrDSourceComboBox.Enabled := NOT PhCtrDSourceComboBox.Enabled;
  PhCtrDGateLabel.Enabled := NOT PhCtrDGateLabel.Enabled;
  PhCtrDGateComboBox.Enabled := NOT PhCtrDGateComboBox.Enabled;

  PhCtrEChangeLinesChBx.Checked := NOT PhCtrEChangeLinesChBx.Checked;
  PhCtrESourceLabel.Enabled := NOT PhCtrESourceLabel.Enabled;
  PhCtrESourceComboBox.Enabled := NOT PhCtrESourceComboBox.Enabled;
  PhCtrEGateLabel.Enabled := NOT PhCtrEGateLabel.Enabled;
  PhCtrEGateComboBox.Enabled := NOT PhCtrEGateComboBox.Enabled;
 end;
end;


end.
