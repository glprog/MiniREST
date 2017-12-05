unit MiniREST.Controller.Security.Intf;

interface

uses MiniREST.Intf;

type
  IMiniRESTSecurityResponse = interface
  ['{891AC301-28E4-4136-BDA3-9F05BE896857}']
    function GetHasPermission : Boolean;
    function GetPermissionErrorMessage : string;
    property HasPermission : Boolean read GetHasPermission;
    property PermissionErrorMessage : string read GetPermissionErrorMessage;
  end;

  IMiniRESTSecurityController = interface
  ['{1A6EDC33-F636-45DD-B492-C6611C2BE04A}']
    //function HasPermission(AContext : IMiniRESTActionContext) : Boolean;
    function HasPermission(AContext : IMiniRESTActionContext) : IMiniRESTSecurityResponse;
//    function GetClientToken(AContext : IMiniRESTActionContext) : string;
//    function Login(AuthInfo : string) : string;
//    function IsValidToken(AToken : string) : Boolean; overload;
//    function IsValidToken(AContext : IMiniRESTActionContext) : Boolean; overload;
//    function GetLoginPath : string;
//    procedure AddExcludedPath(APath : string);
//    function IsExcludedPath(APath : string) : Boolean;
  end;

implementation

end.
