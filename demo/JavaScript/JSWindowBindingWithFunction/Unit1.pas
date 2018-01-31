unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, uCEFWindowParent,
  uCEFChromiumWindow, uCEFChromium, uCEFInterfaces, uCommon, uMyV8MsgHandler;

type
  TForm1 = class(TForm)
    tmr1: TTimer;
    chrm1: TChromium;
    cfwndwprnt1: TCEFWindowParent;
    procedure tmr1Timer(Sender: TObject);
    procedure chrm1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure MsgOpenForm(var AMsg: TMessage); message MSG_OPEN_FORM;
    procedure MsgLoadUrl(var AMsg: TMessage); message MSG_LOAD_URL;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MyV8MsgHandler: TMyV8MsgHandler;
  end;

var
  Form1: TForm1;

implementation

uses uFunction;

{$R *.dfm}

procedure TForm1.chrm1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  //chrm1.LoadURL('http://baidu.com');
  PostMessage(Handle, MSG_LOAD_URL, 0, 0);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MyV8MsgHandler := TMyV8MsgHandler.Create;
  MyV8MsgHandler.Handle := Handle;
  chrm1.OnProcessMessageReceived := MyV8MsgHandler.OnHandleProcessMsg;
  tmr1.Enabled := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  MyV8MsgHandler.Free;
end;

procedure TForm1.MsgLoadUrl(var AMsg: TMessage);
begin
  chrm1.LoadURL('file:///d:/dev/delphi/cef4delphitest/html/js/JSWindowBindingWithFunction.html');
end;

procedure TForm1.MsgOpenForm(var AMsg: TMessage);
begin
  MyV8MsgHandler.HandleHostMsg(AMsg);
end;

procedure TForm1.tmr1Timer(Sender: TObject);
begin
  tmr1.Enabled := False;
  if (not chrm1.CreateBrowser(cfwndwprnt1, '')) and (not chrm1.Initialized) then
    tmr1.Enabled := True
end;

end.
