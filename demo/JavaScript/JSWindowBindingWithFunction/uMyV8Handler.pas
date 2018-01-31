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
//        Copyright ?2017 Salvador D韆z Fau. All rights reserved.
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

unit uMyV8Handler;

{$I cef.inc}

interface

uses
  uCEFTypes, uCEFInterfaces, uCEFv8Value, uCEFv8Handler;

type
  TMyV8Handler = class(TCefv8HandlerOwn)
    protected
      function Execute(const name: ustring; const obj: ICefv8Value; const arguments: TCefv8ValueArray; var retval: ICefv8Value; var exception: ustring): Boolean; override;

    public
      context: ICefv8Context;
  end;

implementation

uses Winapi.Windows, uCommon, Vcl.Dialogs, System.SysUtils, Vcl.Forms, uCEFProcessMessage;

function TMyV8Handler.Execute(const name      : ustring;
                              const obj       : ICefv8Value;
                              const arguments : TCefv8ValueArray;
                              var   retval    : ICefv8Value;
                              var   exception : ustring): Boolean;
var
  LMsg: ICefProcessMessage;
begin
  if (name = 'myfunc') then
  begin
    retval := TCefv8ValueRef.NewString('My Func!');
    Result := True;
  end
  else if (name = 'test_form') then
    begin
      LMsg := TCefProcessMessageRef.New('open_form');
      LMsg.ArgumentList.SetString(0, arguments[0].GetStringValue);
      context.Browser.SendProcessMessage(PID_BROWSER, LMsg);
      //PostMessage(Application.MainForm.Handle, MSG_OPEN_FORM, 0, 0);

      //可以传递回调函数和回调时的上下文给到进程中

      retval := TCefv8ValueRef.NewString('My Form!');
      Result := True;
    end
   else
    Result := False;
end;


end.
