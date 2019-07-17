{$IFDEF FPC}
  {$mode DELPHI}
{$IFEND}
unit MiniREST.Server.Intf;

interface

uses SysUtils, MiniREST.Controller.Intf, {MiniREST.Controller.Base,}
  MiniREST.Controller.Security.Intf, MiniREST.Intf;

type
  IMiniRESTServer = interface
  ['{91170691-E5BF-4C74-9B7F-052908DDA8E7}']
    procedure AddController(AController : TClass); overload;
    procedure AddController(AControllerFactory : IMiniRESTControllerFactory); overload;
    procedure SetControllerOtherwise(AController : TClass);
    {$IFNDEF FPC}
    procedure SetSecurityController(AController : TFunc<IMiniRESTSecurityController>);
    {$IFEND}
    function GetLogger : IMiniRESTLogger;
    procedure SetLogger(ALogger : IMiniRESTLogger);
    procedure AddMiddleware(AMiddleware : IMiniRESTMiddleware); { TODO : Mudar para class ou factory ?}
    function GetPort : Integer;
    procedure SetPort(APort : Integer);
    function Start : Boolean;
    function Stop : Boolean;
  end;

implementation

end.
