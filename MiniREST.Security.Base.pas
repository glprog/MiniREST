unit MiniREST.Security.Base;

interface

uses MiniRest.Controller.Security.Intf;

type
  TMiniRESTSecurityResponse = class(TInterfacedObject, IMiniRESTSecurityResponse)
  private
    FHasPermission: Boolean;
    FPermissionErrorMessage: string;
  public
    constructor Create(const AHasPermission: Boolean; const APermissionErrorMessage: string = '');
    function GetHasPermission: Boolean;
    function GetPermissionErrorMessage: string;
  end;

implementation

{ TMiniRESTSecurityResponse }

constructor TMiniRESTSecurityResponse.Create(const AHasPermission: Boolean;
  const APermissionErrorMessage: string);
begin
  FHasPermission:= AHasPermission;
  FPermissionErrorMessage:= APermissionErrorMessage;
end;

function TMiniRESTSecurityResponse.GetHasPermission: Boolean;
begin
  Result:= FHasPermission;
end;

function TMiniRESTSecurityResponse.GetPermissionErrorMessage: string;
begin
  Result:= FPermissionErrorMessage;
end;

end.
