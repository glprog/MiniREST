unit MiniREST.SQL.Base;

interface

uses MiniREST.SQL.Intf, MiniREST.SQL.Common, SyncObjs, Generics.Collections,
  SysUtils;

type
  TMiniRESTSQLConnectionFactoryBase = class abstract(TInterfacedObject, IMiniRESTSQLConnectionFactory)
  protected
    FSemaphore: TLightweightSemaphore;
    FCriticalSection: TCriticalSection;
    FQueue: TQueue<IMiniRESTSQLConnection>;
    FConnectionCounter: Integer;
    FConnectionsCount: Integer;
    procedure ReleaseConnection(AConnection : IMiniRESTSQLConnection);
    function InternalGetconnection: IMiniRESTSQLConnection; virtual; abstract;
    constructor Create(const AConnectionCount: Integer);
  public
    destructor Destroy; override;
    function GetConnection: IMiniRESTSQLConnection;
  end;

  TMiniRESTSQLConnectionBase = class abstract(TInterfacedObject, IMiniRESTSQLConnection)
  protected
    FOwner : Pointer;
    FName: string;
    FEstaNoPool: Boolean;
    function _Release: Integer; stdcall;
    function GetObject: TObject; virtual; abstract;
    constructor Create(AOwner : IMiniRESTSQLConnectionFactory);
  public
    procedure StartTransaction; virtual; abstract;
    procedure Commit; virtual; abstract;
    procedure Rollback; virtual; abstract;
    procedure Connect; virtual; abstract;
    function GetQuery: IMiniRESTSQLQuery; overload; virtual; abstract;
    function GetQuery(const ASQL: string): IMiniRESTSQLQuery; overload; virtual; abstract;
    function GetQuery(const ASQL: string; AParams : array of IMiniRESTSQLParam): IMiniRESTSQLQuery; overload; virtual; abstract;
    function GetName: string;
    function SetName(const AName: string): IMiniRESTSQLConnection;
    function Execute(const ACommand: string; AParams: array of IMiniRESTSQLParam): Integer; virtual; abstract;
  end;

implementation

{ TMiniRESTSQLConnectionFactoryBase }

constructor TMiniRESTSQLConnectionFactoryBase.Create(const AConnectionCount: Integer);
begin
  FConnectionsCount := AConnectionCount;
  FSemaphore := TLightweightSemaphore.Create(AConnectionCount, AConnectionCount);
  FCriticalSection := TCriticalSection.Create;
  FQueue := TQueue<IMiniRESTSQLConnection>.Create;
end;

destructor TMiniRESTSQLConnectionFactoryBase.Destroy;
begin
  FSemaphore.Free;
  FQueue.Free;
  FCriticalSection.Free;
  inherited;
end;

function TMiniRESTSQLConnectionFactoryBase.GetConnection: IMiniRESTSQLConnection;
var
  LConnection: IMiniRESTSQLConnection;
begin
  FSemaphore.Acquire;
  FCriticalSection.Enter;
  try
    if FQueue.Count = 0 then
    begin
      LConnection := InternalGetconnection.SetName('Connection' + IntToStr(FConnectionCounter));
      Inc(FConnectionCounter);
      Result := LConnection;
    end
    else
    begin
      Result := FQueue.Dequeue;
    end;
    TMiniRESTSQLConnectionBase(Result).FEstaNoPool := False;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TMiniRESTSQLConnectionFactoryBase.ReleaseConnection(
  AConnection: IMiniRESTSQLConnection);
begin
  FCriticalSection.Enter;
  try
    FQueue.Enqueue(AConnection);
    TMiniRESTSQLConnectionBase(AConnection).FEstaNoPool := True;
    FSemaphore.Release(1);
  finally
    FCriticalSection.Leave;
  end;
end;

{ TMiniRESTSQLConnectionBase }

constructor TMiniRESTSQLConnectionBase.Create(
  AOwner: IMiniRESTSQLConnectionFactory);
begin
  FOwner := Pointer(AOwner);
end;

function TMiniRESTSQLConnectionBase.GetName: string;
begin
  Result := FName;
end;

function TMiniRESTSQLConnectionBase.SetName(
  const AName: string): IMiniRESTSQLConnection;
begin
  FName := AName;
  Result := Self;
end;

function TMiniRESTSQLConnectionBase._Release: Integer;
begin
  if (FRefCount = 1) and (FOwner <> nil) and (not FEstaNoPool) then
    IMiniRESTSQLConnectionFactory(FOwner).ReleaseConnection(Self);
  Result := inherited;
end;

end.
