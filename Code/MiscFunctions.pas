unit MiscFunctions;

interface
uses SysUtils, Math,
//   ************************************************************************
     TypesUnit;
//   ************************************************************************

  function _Compare(NeedStrC, MeasStrC: string): Extended;
  function _GetExtrapo1(KnownVal, Grad: Extended; ExtrapoSet: TExtMatrix): Extended;
  function _GetExtrapo2(KnownVal, Grad: Extended; ExtrapoSet: TExtMatrix): Extended;
  procedure _GetValue(var ReturnArray: array of Extended; InputStr: string; HowMany: Integer);

implementation

function _Compare(NeedStrC, MeasStrC: string): Extended;
var
ORDif: integer;// difference between orders of magnitude.
TempExt: Extended;
begin
try
 ORDif := StrToInt(NeedStrC[7]+NeedStrC[8])//Need string exponent value.
        - StrToInt(MeasStrC[7]+MeasStrC[8]);//Meas string exponent vlaue.
 TempExt := StrToFloat(MeasStrC[1]+MeasStrC[2]+MeasStrC[3]+MeasStrC[4]) * IntPower(10,ORDif)
           //Measured decimal value, times ten to the power of ORDif.
          - StrToFloat(NeedStrC[1]+NeedStrC[2]+NeedStrC[3]+NeedStrC[4]);
          //minus needed decimal number.
 Result := TempExt * IntPower(10,-(StrToInt(NeedStrC[7]+NeedStrC[8])));
except
 //StatusBar1.Panels[1].Text := 'Error executing compare alogorithm.';
end;
end;


function _GetExtrapo1(KnownVal, Grad: Extended; ExtrapoSet: TExtMatrix): Extended;
//  Known value is in the first column and the returned value is extrapolated
//  in the second column.  ExtrapoSet must be of setlength [n,2].  The known value
//  is in [n,0] and the returned is in [n,1].  n > 1. Only works for numerically
//  assending within the known val column.
var
i, len: Integer;
pcthru, along, more: Extended;
begin
len := Length(ExtrapoSet);
if KnownVal < ExtrapoSet[0,0] then
 begin
  Result := ExtrapoSet[0,1];
  Grad := ExtrapoSet[1,1] - ExtrapoSet[0,1] / ExtrapoSet[1,0] - ExtrapoSet[0,0];
 end;
if KnownVal > ExtrapoSet[(len - 1),0] then
 begin
  Result := ExtrapoSet[(len - 1),1];
  Grad := ExtrapoSet[(len-1),1] - ExtrapoSet[(len-2),1] / ExtrapoSet[(len-1),0] - ExtrapoSet[(len-2),0];
 end;
//  Length command returns SetLength value given.  Some arrays start at 1 and others
//  start at 0 ! whats that all about then me ol china ?
for i := 0 to (len - 2) do
 begin
  if KnownVal = ExtrapoSet[i,0] then
  begin
   Result := ExtrapoSet[i,1];
   Grad := ExtrapoSet[(i+1),1] - ExtrapoSet[i,1]
             / ExtrapoSet[(i+1),0] - ExtrapoSet[i,0];
  end;
  if ((KnownVal > ExtrapoSet[i,0]) AND (KnownVal < ExtrapoSet[i+1,0])) then
   begin
    pcthru := (ExtrapoSet[i+1,0] - ExtrapoSet[i,0]) / (KnownVal - ExtrapoSet[i,0]);
    along := ExtrapoSet[i+1,1] - ExtrapoSet[i,1];
    more := ExtrapoSet[i,1];
    Result := (pcthru * along) + more;
    Grad := ExtrapoSet[(i+1),1] - ExtrapoSet[i,1] / ExtrapoSet[(i+1),0] - ExtrapoSet[i,0];
   end;
  if KnownVal = ExtrapoSet[i+1,0] then
  begin
   Result := ExtrapoSet[i+1,1];
   Grad := ExtrapoSet[(len-1),1] - ExtrapoSet[(len-2),1] / ExtrapoSet[(len-1),0] - ExtrapoSet[(len-2),0];
  end;
 end;
end;


function _GetExtrapo2(KnownVal, Grad: Extended; ExtrapoSet: TExtMatrix): Extended;
//  Known value is in the second column and the returned value is extrapolated
//  in the first column.  ExtrapoSet must be of setlength [n,2].  The known value
//  is in [n,1] and the returned is in [n,0].  n > 1.  Only works for numerically
//  assending within the known val column.
var
i, len: Integer;
pcthru, along, more: Extended;
begin
len := Length(ExtrapoSet);
if KnownVal < ExtrapoSet[0,1] then
 begin
  Result := ExtrapoSet[0,0];
  Grad := ExtrapoSet[1,0] - ExtrapoSet[0,0] / ExtrapoSet[1,1] - ExtrapoSet[0,1];
 end;
if KnownVal > ExtrapoSet[(len - 1),1] then
 begin
  Result := ExtrapoSet[(len - 1),0];
  Grad := ExtrapoSet[(len-1),0] - ExtrapoSet[(len-2),0] / ExtrapoSet[(len-1),1] - ExtrapoSet[(len-2),1];
 end;
//  Length command returns SetLength value given.  Some arrays start at 1 and others
//  start at 0 ! whats that all about then me ol china ?
for i := 0 to (Length(ExtrapoSet) - 2) do
 begin
  if KnownVal = ExtrapoSet[i,1] then
  begin
   Result := ExtrapoSet[i,0];
   Grad := ExtrapoSet[(i+1),0] - ExtrapoSet[i,0] / ExtrapoSet[(i+1),1] - ExtrapoSet[i,1];
  end;
  if ((KnownVal > ExtrapoSet[i,1]) AND (KnownVal < ExtrapoSet[i+1,1])) then
   begin
    pcthru := (KnownVal - ExtrapoSet[i,1]) / (ExtrapoSet[i+1,1] - ExtrapoSet[i,1]);
    along := ExtrapoSet[i+1,0] - ExtrapoSet[i,0];
    more := ExtrapoSet[i,0];
    Grad := (ExtrapoSet[(i+1),0] - ExtrapoSet[i,0]) / (ExtrapoSet[(i+1),1] - ExtrapoSet[i,1]);
    Result := (pcthru * along) + more;
   end;
  if KnownVal = ExtrapoSet[i+1,1] then
  begin
   Result := ExtrapoSet[i+1,0];
   Grad := ExtrapoSet[(len-1),0] - ExtrapoSet[(len-2),0] / ExtrapoSet[(len-1),1] - ExtrapoSet[(len-2),1];
  end;
 end;
end;


procedure _GetValue(var ReturnArray: array of Extended; InputStr: string; HowMany: Integer);
var
 i, x, di, tad, ta, p: Integer;
 EscChar, Little, DecPlaces: Boolean;
 Exp1, Exp2: Char;
 TempArray: Array[1..20] of Extended;
 TempArrayDec: Array[1..20] of Extended;
 ExpFactor, Exponent, Temp: Extended;

begin
if InputStr = '' then Exit;
x := 0;
p := 1;
repeat
 i := 0; di := 0;
 ExpFactor := 1;                     //  resets all the variables for next chunk of data
 EscChar := False;                   //  if more than one value exists in string
 DecPlaces := False;
 While not EscChar do
  begin
   if  InputStr[p] = #10 then inc(p);
   case InputStr[p] of
     // if it is a space, comma, tab, carrage return or null it sets EscChar to end current while loop
    #13: EscChar := True;
    #44: EscChar := True;
    #32: EscChar := True;
    #9:  EscChar := True;
    #0:  EscChar := True;
     // if it is an e or E it sets up an exponent factor to by multiplied before stored
    #69,#101 : begin
                inc(p);                    // negative sign
                if InputStr[p] = #45 then
                 begin
                  inc(p);
                  Exp1 := InputStr[p];
                  inc(p);
                  Exp2 := InputStr[p];
                  Little := True;
                 end
                else
                 begin// postive sign
                  if InputStr[p] = #43 then inc(p);
                  Exp1 := InputStr[p];
                  inc(p);
                  Exp2 := InputStr[p];
                 end;
                Exponent := StrToInt( Exp1 + Exp2 );
                if Little = true then Exponent := -Exponent;
                ExpFactor := Power(10,Exponent);
               end;
    else    // else it is a normal digit
     begin     // or it could be a decimal point
     if InputStr[p] = #46 then
      begin
       DecPlaces := True;
       inc(p);
      end;
     if DecPlaces = True then
      begin
       inc (di);
       TempArrayDec[di] := StrToFloat(InputStr[p]);
      end
     else
      begin
       inc (i);
       TempArray[i] := StrToFloat(InputStr[p]);
      end;
     end; //end else clause after cases
   end;  // end of case loop
  inc(p);
 end;  // end of while loop
 // Temp holds the value to go into the final value taking into account the
 // relative magnitudes for each value at each position.
 Temp := 0;
  for ta := 1 to i do
   begin
    Temp := Temp + (TempArray[ta] * IntPower(10,(i-ta)));
    if DecPlaces = True then
      for tad := 1 to di do Temp := Temp + (TempArrayDec[tad] * (IntPower(10,-tad)));
   end;
  ReturnArray[x] := Temp * ExpFactor;
  inc(x);
until x = HowMany;
end;


end.
