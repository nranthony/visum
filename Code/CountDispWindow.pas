unit CountDispWindow;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
// ******************************************************************************
  TypesUnit, Preferences;
// ******************************************************************************
type
  TCountDispFrm = class(TForm)
    ChannelALabel: TLabel;
    ChannelBLabel: TLabel;
    ChannelCLabel: TLabel;
    CountDurationLabel: TLabel;
    CountsPerDurLabel: TLabel;
    CloseButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure DisplayA(var Msg: TMessage); message WM_DISP_A_CNT;
    procedure DisplayB(var Msg: TMessage); message WM_DISP_B_CNT;
    procedure DisplayC(var Msg: TMessage); message WM_DISP_C_CNT;
    { Public declarations }
  end;

var
  CountDispFrm: TCountDispFrm;

implementation

uses Main;

{$R *.dfm}

procedure TCountDispFrm.FormCreate(Sender: TObject);
begin
Self.Color := PaletteRGB(25,25,30);
Self.BorderStyle := bsToolWindow;
Self.Top := 89;
Self.Left := 150;
Self.Width := 940;
Self.Height := 560;
CountDurationLabel.Caption := 'Count Duration: ' + FormatFloat('0.###',PrefFrm.Duration*12.5E-09) + 's';
CountsPerDurLabel.Caption := 'Counts Per Duration: ' + IntToStr(PrefFrm.CountsPerDur);
end;

procedure TCountDispFrm.CloseButtonClick(Sender: TObject);
begin
CancelAcq := True;
Self.Close;

end;

procedure TCountDispFrm.DisplayA(var Msg: TMessage);
var
Ptr: PSingle;
TempSingle: Single;
begin
New(Ptr);
Ptr := PSingle(Msg.WParam);
TempSingle := Ptr^;
Self.ChannelALabel.Caption := 'Channel A: ' + FormatFloat('0.###',TempSingle) + 'kHz ';
Ptr := nil;
end;

procedure TCountDispFrm.DisplayB(var Msg: TMessage);
var
Ptr: PSingle;
TempSingle: Single;
begin
New(Ptr);
Ptr := PSingle(Msg.WParam);
TempSingle := Ptr^;

Self.ChannelBLabel.Caption := 'Channel B: ' + FormatFloat('0.###',TempSingle) + 'kHz ';
Ptr := nil;
end;

procedure TCountDispFrm.DisplayC(var Msg: TMessage);
var
Ptr: PSingle;
TempSingle: Single;
begin
New(Ptr);
Ptr := PSingle(Msg.WParam);
TempSingle := Ptr^;
Self.ChannelCLabel.Caption := 'Channel C: ' + FormatFloat('0.###',TempSingle) + 'kHz ';
Ptr := nil;
end;

end.
