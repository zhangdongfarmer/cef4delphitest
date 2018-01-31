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

program JSWindowBindingWithFunction;

{$I cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  Forms,
  windows,
  {$ELSE}
  Forms,
  Windows,
  {$ENDIF }
  uCEFApplication,
  uJSWindowBindingWithFunction in 'uJSWindowBindingWithFunction.pas' {JSWindowBindingWithFunctionFrm},
  uMyV8Handler in 'uMyV8Handler.pas',
  uCommon in 'uCommon.pas',
  Unit1 in 'Unit1.pas' {Form1},
  uFunction in 'uFunction.pas',
  uMyV8MsgHandler in 'uMyV8MsgHandler.pas',
  uGlobalVar in 'uGlobalVar.pas';

{$R *.res}

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

begin
  ReportMemoryLeaksOnShutdown := True;

  GlobalCEFApp                  := TCefApplication.Create;
  GlobalCEFApp.OnContextCreated := GlobalCEFApp_OnContextCreated;
  GlobalCEFApp.OnWebKitInitialized := GlobalCEFApp_OnWebkitInitialized;;
  GlobalCEFApp.SingleProcess := False;

  if GlobalCEFApp.StartMainProcess then
    begin

      Application.Initialize;
      GlobalVar := TGlobalVar.Create;

      {$IFDEF DELPHI11_UP}
      Application.MainFormOnTaskbar := True;
      {$ENDIF}
      Application.CreateForm(TJSWindowBindingWithFunctionFrm, JSWindowBindingWithFunctionFrm);
      Application.Run;

      GlobalVar.Free;
    end;

  GlobalCEFApp.Free;
end.
