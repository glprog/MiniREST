unit MiniRest.Controller.Security.Intf;

interface

uses MiniRest.Intf;

type
  IMiniRestSecurityResponse = interface
  ['{891AC301-28E4-4136-BDA3-9F05BE896857}']
    function GetHasPermission : Boolean;
    function GetPermissionErrorMessage : string;
    property HasPermission : Boolean read GetHasPermission;
    property PermissionErrorMessage : string read GetPermissionErrorMessage;
  end;

  IMiniRestSecurityController = interface
  ['{1A6EDC33-F636-45DD-B492-C6611C2BE04A}']
    //function HasPermission(AContext : IMiniRestActionContext) : Boolean;
    function HasPermission(AContext : IMiniRestActionContext) : IMiniRestSecurityResponse;
//    function GetClientToken(AContext : IMiniRestActionContext) : string;
//    function Login(AuthInfo : string) : string;
//    function IsValidToken(AToken : string) : Boolean; overload;
//    function IsValidToken(AContext : IMiniRestActionContext) : Boolean; overload;
//    function GetLoginPath : string;
    procedure AddExcludedPath(APath : string);
    function IsExcludedPath(APath : string) : Boolean;
  end;

implementation

end.
