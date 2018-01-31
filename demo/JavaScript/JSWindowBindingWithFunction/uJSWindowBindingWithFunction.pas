// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright ?2017 Salvador Díaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uJSWindowBindingWithFunction;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCommon, uCEFInterfaces, uMyV8MsgHandler, uCEFApplication, uCEFTypes, uCEFConstants;

type
  TJSWindowBindingWithFunctionFrm = class(TForm)
    NavControlPnl: TPanel;
    Edit1: TEdit;
    GoBtn: TButton;
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Timer1Timer(Sender: TObject);
    procedure Chromium1ProcessMessageReceived(Sender: TObject;
      const browser: ICefBrowser; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Edit1DblClick(Sender: TObject);
  protected
    procedure MSGOpenForm(var aMessage : TMessage); message MSG_OPEN_FORM;
    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

     procedure WMEnterSizeMove(var Message: TMessage) ; message WM_ENTERSIZEMOVE;
     //procedure WMExitSizeMove(var Message: TMessage) ; message WM_EXITSIZEMOVE;
  public
    { Public declarations }
    V8MsgHandler: TMyV8MsgHandler;
  end;

var
  JSWindowBindingWithFunctionFrm: TJSWindowBindingWithFunctionFrm;

procedure GlobalCEFApp_OnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
procedure GlobalCEFApp_OnWebkitInitialized;

implementation

{$R *.dfm}

uses
  uCEFv8Value, uMyV8Handler, uFunction, uCEFMiscFunctions;

// The CEF3 document describing JavaScript integration is here :
// https://bitbucket.org/chromiumembedded/cef/wiki/JavaScriptIntegration.md

// The HTML file in this demo has a button that shows the result of 'window.myfunc()'
// which was set in the GlobalCEFApp.OnContextCreated event.

procedure GlobalCEFApp_OnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
var
  LH: TMyV8Handler;
  TempHandler  : ICefv8Handler;
  TempFunction : ICefv8Value;
begin
  // This is the JS Window Binding example with a function in the "JavaScript Integration" wiki page at
  // https://bitbucket.org/chromiumembedded/cef/wiki/JavaScriptIntegration.md
  LH := TMyV8Handler.Create;
  LH.context := context;
  TempHandler  := LH;
  TempFunction := TCefv8ValueRef.NewFunction('test_form', TempHandler);

  context.Global.SetValueByKey('test_form', TempFunction, V8_PROPERTY_ATTRIBUTE_NONE);
end;


procedure GlobalCEFApp_OnWebkitInitialized;
var
  TempExtensionCode : string;
  TempHandler       : ICefv8Handler;
begin
  // This is the JS extension example with a function in the "JavaScript Integration" wiki page at
  // https://bitbucket.org/chromiumembedded/cef/wiki/JavaScriptIntegration.md

//  TempExtensionCode := 'var test;' +
//                       'if (!test)' +
//                       '  test = {};' +
//                       '(function() {' +
//                       '  test.myfunc = function() {' +
//                       '    native function myfunc();' +
//                       '    return myfunc();' +
//                       '  };' +
//                       '})();';
//
//  TempHandler := TMyV8Handler.Create;

  //CefRegisterExtension('v8/test', TempExtensionCode, TempHandler);
end;


procedure TJSWindowBindingWithFunctionFrm.GoBtnClick(Sender: TObject);
begin

  Chromium1.LoadURL(Edit1.Text);
end;

procedure TJSWindowBindingWithFunctionFrm.MSGOpenForm(var aMessage: TMessage);
begin
  OpenForm(GlobalVar.GetParam(aMessage.WParam));
end;

procedure TJSWindowBindingWithFunctionFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TJSWindowBindingWithFunctionFrm.Chromium1ProcessMessageReceived(
  Sender: TObject; const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
begin
  if message.Name = 'open_form' then
  begin
    //OpenForm('');
    PostMessage(Handle, MSG_OPEN_FORM, 0, 0);
    Result := True;
  end;
end;

procedure TJSWindowBindingWithFunctionFrm.FormCreate(Sender: TObject);
begin
  V8MsgHandler := TMyV8MsgHandler.Create;
  V8MsgHandler.Handle := Handle;
  Chromium1.OnProcessMessageReceived := V8MsgHandler.OnHandleProcessMsg;
end;

procedure TJSWindowBindingWithFunctionFrm.FormDestroy(Sender: TObject);
begin
  V8MsgHandler.Free;
end;

procedure TJSWindowBindingWithFunctionFrm.FormShow(Sender: TObject);
begin
  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) then Timer1.Enabled := True;
end;

procedure TJSWindowBindingWithFunctionFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TJSWindowBindingWithFunctionFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TJSWindowBindingWithFunctionFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TJSWindowBindingWithFunctionFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  Caption := 'JSWindowBindingWithFunction';
  CEFWindowParent1.UpdateSize;
  NavControlPnl.Enabled := True;
  GoBtn.Click;
end;

procedure TJSWindowBindingWithFunctionFrm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;
  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TJSWindowBindingWithFunctionFrm.WMEnterSizeMove(
  var Message: TMessage);
begin
  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TJSWindowBindingWithFunctionFrm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;
  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;


procedure TJSWindowBindingWithFunctionFrm.Edit1DblClick(Sender: TObject);
begin
  edit1.Text := 'http://huarui.org/mall/agency/?dis=main/table';
end;

end.
