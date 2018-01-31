unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uCEFWindowParent, uCEFChromiumWindow,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, uCEFChromium, uCEFInterfaces, uCEFTypes;

const
  MSG_AFTER = WM_USER + 1090;
type
  TForm2 = class(TForm)
    btn1: TBitBtn;
    tmr1: TTimer;
    cfwndwprnt1: TCEFWindowParent;
    chrm1: TChromium;
    procedure tmr1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure chrm1ProcessMessageReceived(Sender: TObject;
      const browser: ICefBrowser; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure chrm1AfterCreated(Sender: TObject; const browser: ICefBrowser);

    procedure MSGAFTER(var msg: TMessage); message MSG_AFTER;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses uJSWindowBindingWithFunction;


{$R *.dfm}


procedure TForm2.btn1Click(Sender: TObject);
begin
  //JSWindowBindingWithFunctionFrm.btn1.Click;
end;

procedure TForm2.chrm1AfterCreated(Sender: TObject; const browser: ICefBrowser);
var
  Lfile: string;
begin
  PostMessage(Handle, MSG_AFTER, 0, 0);
end;

procedure TForm2.chrm1ProcessMessageReceived(Sender: TObject;
  const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
begin
  ShowMessage('ok');
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  tmr1.Enabled := True;
end;

procedure TForm2.MSGAFTER(var msg: TMessage);
var
  Lfile: string;
begin
  Lfile := 'file:///form2.html';
  chrm1.LoadURL(Lfile);
end;

procedure TForm2.tmr1Timer(Sender: TObject);
begin
  tmr1.Enabled := False;
  if (not chrm1.Initialized) and (not chrm1.CreateBrowser(cfwndwprnt1, ''))  then
    tmr1.Enabled := True;
end;

end.
