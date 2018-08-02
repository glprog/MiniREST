unit MiniREST.SQL.Base;

interface

uses MiniREST.SQL.Intf, MiniREST.SQL.Common, SyncObjs, Generics.Collections,
  SysUtils;

type
  TMiniRESTSQLConnectionFactoryBase = class abstract(TInterfacedObject, IMiniRESTSQLConnectionFactory)
  protected
    FSemaphore: TLightweightSemaphore;
    FCriticalSection: TCriticalSection;
    FStack: TStack<IMiniRESTSQLConnection>;
    FConnectionsCount: Integer;
    procedure ReleaseConnection(AConnection : IMiniRESTSQLConnection);
    function InternalGetconnection: IMiniRESTSQLConnection; virtual; abstract;
    procedure GenerateConnections; virtual;
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
    function GetQuery(ASQL: string): IMiniRESTSQLQuery; overload; virtual; abstract;
    function GetQuery(ASQL: string; AParams : array of IMiniRESTSQLParam): IMiniRESTSQLQuery; overload; virtual; abstract;
    function GetName: string;
    function SetName(const AName: string): IMiniRESTSQLConnection;
  end;

implementation

{ TMiniRESTSQLConnectionFactoryBase }

destructor TMiniRESTSQLConnectionFactoryBase.Destroy;
begin
  FSemaphore.Free;
  FStack.Free;
  FCriticalSection.Free;
  inherited;
end;

procedure TMiniRESTSQLConnectionFactoryBase.GenerateConnections;
var I : Integer;
  LConnection: IMiniRESTSQLConnection;
begin
  FSemaphore := TLightweightSemaphore.Create(FConnectionsCount, FConnectionsCount);
  FCriticalSection := TCriticalSection.Create;
  FStack := TStack<IMiniRESTSQLConnection>.Create;
  for I := 1 to FConnectionsCount do
  begin
    LConnection := InternalGetconnection.SetName('Connection' + IntToStr(I));
    TMiniRESTSQLConnectionBase(LConnection).FEstaNoPool := True;
    FStack.Push(LConnection);
  end;
end;

function TMiniRESTSQLConnectionFactoryBase.GetConnection: IMiniRESTSQLConnection;
begin
  FSemaphore.Acquire;
  FCriticalSection.Enter;
  try
    Result := FStack.Pop;
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
    FStack.Push(AConnection);
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
  FOwner := AOwner;
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
