unit MiniRest.Server.Intf;

interface

uses SysUtils, MiniRest.Controller.Intf, {MiniRest.Controller.Base,}
  MiniRest.Controller.Security.Intf, MiniRest.Intf;

type
  IMiniRestServer = interface
  ['{91170691-E5BF-4C74-9B7F-052908DDA8E7}']
    procedure AddController(AController : TClass); overload;
    procedure AddController(AControllerFactory : IMiniRestControllerFactory); overload;
    procedure SetControllerOtherwise(AController : TClass);
    procedure SetSecurityController(AController : TFunc<IMiniRestSecurityController>);
    function GetLogger : IMiniRestLogger;
    procedure SetLogger(ALogger : IMiniRestLogger);
    procedure AddMiddleware(AMiddleware : IMiniRestMiddleware); { TODO : Mudar para class ou factory ?}
    function GetPort : Integer;
    procedure SetPort(APort : Integer);
    function Start : Boolean;
    function Stop : Boolean;
  end;

implementation

end.
