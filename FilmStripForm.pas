unit FilmStripForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ImgList,
// ************************************************************************
  Preferences;
// ************************************************************************
type
  TFilmStrForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
  private
    ThumbCount,
    RowNumber,
    ColumnNumber: Word;
    { Private declarations }
  public
    ThumbArray: array of TImage;
    function NewImage(Link2Last: Boolean; Channel: Char; NameHint, DimensionsHint: string): Word;

    { Public declarations }
  end;

var
  FilmStrForm: TFilmStrForm;


implementation

uses Main;

{$R *.dfm}

procedure TFilmStrForm.FormCreate(Sender: TObject);
begin

ThumbCount := 0;
RowNumber := 0;
ColumnNumber := 0;
Self.Color := PaletteRGB(25,25,30);

end;

function TFilmStrForm.NewImage(Link2Last: Boolean; Channel: Char;
                               NameHint, DimensionsHint: string): Word;
var
ChannelLabel: TLabel;
begin
SetLength(ThumbArray,ThumbCount+1);
ThumbArray[ThumbCount] := TImage.Create(Self);
if ColumnNumber > 12 then
 begin
  ColumnNumber := 0;
  inc(RowNumber);
 end;
with ThumbArray[ThumbCount] do
 begin
  Parent := Self;
  Visible := True;
  Top := 8 + (RowNumber*96) - Self.VertScrollBar.ScrollPos;
  Left := 8 + (ColumnNumber*96);
  Width := 89;
  Height := 89;
 end;
if PrefFrm.UseStretchBlt then
 begin
  SetStretchBltMode(ThumbArray[ThumbCount].Canvas.Handle, PrefFrm.StretchBltMode);
  if PrefFrm.StretchBltMode = 4 then SetBrushOrgEx(ThumbArray[ThumbCount].Canvas.Handle,0,0,nil);
 end;
if PrefFrm.UseGraphicEx then
 begin
  //
 end;
    
Result := ThumbCount;
ChannelLabel := Tlabel.Create(Self);
with ChannelLabel do
 begin
  Parent := Self;
  Caption := Channel;
  Font.Color := clSilver;
  Top := 9 + (RowNumber*96) - Self.VertScrollBar.ScrollPos;
  Left := 9 + (ColumnNumber*96);
  Visible := True;
  BringToFront;
 end;

ThumbArray[ThumbCount].Hint := NameHint + #10 + DimensionsHint;
ThumbArray[ThumbCount].ShowHint := True;
Self.VertScrollBar.Position := High(Self.VertScrollBar.Range);
inc(ColumnNumber);
inc(ThumbCount);
end;

  
procedure TFilmStrForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
Self.VertScrollBar.Position := Self.VertScrollBar.Position - WheelDelta;
end;

procedure TFilmStrForm.FormShow(Sender: TObject);
begin
Self.Width := MainForm.Width - 8;
Self.Height := 200;
Self.Top := Screen.WorkAreaHeight - Self.Height + 32;
Self.Left := 1280;
end;

end.
