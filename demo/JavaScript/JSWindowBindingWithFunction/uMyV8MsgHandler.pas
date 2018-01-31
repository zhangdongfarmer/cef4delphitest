unit uMyV8MsgHandler;

interface

uses uCEFInterfaces, uCEFTypes, Winapi.Messages;

type
  TMyV8MsgHandler = class
  public
    Handle: THandle;
    procedure HandleHostMsg(var AMsg: TMessage);
    procedure OnHandleProcessMsg(Sender: TObject; const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
  end;

implementation

uses Winapi.Windows, uCommon, uFunction, uCEFProcessMessage;

{ TMyV8MsgHandler }

procedure TMyV8MsgHandler.HandleHostMsg(var AMsg: TMessage);
begin
  openform(GlobalVar.GetParam(AMsg.WParam));
end;

procedure TMyV8MsgHandler.OnHandleProcessMsg(Sender: TObject;
  const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
var
  LParams: string;
  LMsg: ICefProcessMessage;
  LIdx: Integer;
begin
  if message.Name = 'open_form' then
  begin
    LParams := message.ArgumentList.GetString(0);

    LIdx := GlobalVar.AddParam(LParams);
    PostMessage(Handle, MSG_OPEN_FORM, LIdx, 0);

    Result := True;
  end;
end;

end.
