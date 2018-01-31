unit uFunction;

interface

procedure OpenForm(AMsg: string = '');

implementation

uses System.SyncObjs, Unit1, Vcl.Forms, uJSWindowBindingWithFunction;

var
  Critical: TCriticalSection;
  Count: Integer;


procedure OpenForm(AMsg: string);
begin
  //Critical.Enter;
  //TInterlocked.Increment(count);
  try
    //JSWindowBindingWithFunctionFrm.Enabled := False;
    with TForm1.Create(nil) do
    try
      Caption := AMsg;
      ShowModal;
    finally
      Free;
    end;
  finally
    //TInterlocked.Decrement(Count);
    if Count <= 0 then
    begin
      Count := 0;
      //JSWindowBindingWithFunctionFrm.Enabled := True;
    end;
    //Critical.Leave;
  end;
end;


initialization
Critical := TCriticalSection.Create;

finalization
Critical.Free;

end.
