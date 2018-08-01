unit MiniREST.SQL.Base;

interface

uses MiniREST.SQL.Intf, SyncObjs, Generics.Collections;

type
  TMiniRESTSQLConnectionFactoryBase = class abstract(TInterfacedObject, IMiniRESTSQLConnectionFactory)
  protected
    FSemaphore: TLightweightSemaphore;
    FCriticalSection: TCriticalSection;
    FStack: TStack<IMiniRESTSQLConnection>;
    procedure ReleaseConnection(AConnection : IMiniRESTSQLConnection);
    function InternalGetconnection: IMiniRESTSQLConnection; virtual; abstract;
    constructor Create(AConnections : Integer);
  public
    destructor Destroy; override;
    function GetConnection: IMiniRESTSQLConnection;
  end;

  TMiniRESTSQLConnectionBase = class abstract(TInterfacedObject, IMiniRESTSQLConnection)
  protected
    FOwner : IMiniRESTSQLConnectionFactory;
    function _Release: Integer; stdcall;
    constructor Create(AOwner : IMiniRESTSQLConnectionFactory);
  public
    function GetQuery(ASQL: string): IMiniRESTSQLQuery; overload; virtual; abstract;
    function GetQuery(ASQL: string; AParams : array of Variant): IMiniRESTSQLQuery; overload; virtual; abstract;
    function Execute(ACommand: string) : IMiniRESTSQLQuery; overload; virtual; abstract;
    function Execute(ACommand: string; AParams : array of Variant) : IMiniRESTSQLQuery; overload; virtual; abstract;
    procedure StartTransaction; virtual; abstract;
    procedure Commit; virtual; abstract;
    procedure Rollback; virtual; abstract;
  end;

implementation

{ TMiniRESTSQLConnectionFactoryBase }

constructor TMiniRESTSQLConnectionFactoryBase.Create(AConnections: Integer);
var I : Integer;
begin
  FSemaphore := TLightweightSemaphore.Create(AConnections, AConnections);
  FCriticalSection := TCriticalSection.Create;
  FStack := TStack<IMiniRESTSQLConnection>.Create;
  for I := 1 to AConnections do
  begin
    FStack.Push(InternalGetconnection);
  end;
end;

destructor TMiniRESTSQLConnectionFactoryBase.Destroy;
begin
  FSemaphore.Free;
  FStack.Free;
  FCriticalSection.Free;
  inherited;
end;

function TMiniRESTSQLConnectionFactoryBase.GetConnection: IMiniRESTSQLConnection;
begin
  FSemaphore.Acquire;
  FCriticalSection.Enter;
  try
    Result := FStack.Pop;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TMiniRESTSQLConnectionFactoryBase.ReleaseConnection(
  AConnection: IMiniRESTSQLConnection);
begin
  FCriticalSection.Enter;
  try
    FStack.Push(AConnection);
  finally
    FCriticalSection.Leave;
  end;
  FSemaphore.Release(1);
end;

{ TMiniRESTSQLConnectionBase }

constructor TMiniRESTSQLConnectionBase.Create(
  AOwner: IMiniRESTSQLConnectionFactory);
begin
  FOwner := AOwner;
end;

function TMiniRESTSQLConnectionBase._Release: Integer;
begin
  if (FRefCount = 1) and (FOwner <> nil) then
    FOwner.ReleaseConnection(Self);
  Result := inherited;
end;

end.
