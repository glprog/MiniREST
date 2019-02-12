{$mode DELPHI}
unit MiniREST.SQL.Base;

interface

uses MiniREST.SQL.Intf, MiniREST.SQL.Common, SyncObjs, {$IFNDEF FPC}Generics.Collections, 
{$ELSE} Contnrs, lazCollections,{$IFEND} SysUtils, Classes;

type
  TMiniRESTSQLConnectionFactoryBase = class abstract(TInterfacedObject, IMiniRESTSQLConnectionFactory)
  protected
    {$IFNDEF FPC}
    FSemaphore: TLightweightSemaphore;
    FQueue: TQueue<IMiniRESTSQLConnection>;    
    {$ELSE}
    FConnectionGetEvent: PRTLEvent;
    FConnectionReleaseEvent: PRTLEvent;
    FQueue: TArray<IMiniRESTSQLConnection>;
    FQueuePosition: Integer;
    FAvailableConnections: Integer;    
    {$IFEND}
    FCriticalSection: TCriticalSection;
    FConnectionCounter: Integer;
    FConnectionsCount: Integer;
    FConnectionsToNotifyFree: TList;
    procedure ReleaseConnection(AConnection : IMiniRESTSQLConnection);
    function InternalGetconnection: IMiniRESTSQLConnection; virtual; abstract;
    constructor Create(const AConnectionCount: Integer);
  public
    destructor Destroy; override;
    function GetConnection: IMiniRESTSQLConnection;
  end;

  TMiniRESTSQLConnectionBase = class abstract(TInterfacedObject, IMiniRESTSQLConnection)
  strict private
    FOwner : Pointer;
  protected
    FName: string;
    FEstaNoPool: Boolean;
    function _Release: Integer; stdcall;
    function GetObject: TObject; virtual; abstract;
    procedure SetOwner(AOwner: Pointer);
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
    function GetDatabaseInfo: IMiniRESTSQLDatabaseInfo; virtual; abstract;
  end;

  TMiniRESTSQLPrimaryKeyInfo = class(TInterfacedObject, IMiniRESTSQLPrimaryKeyInfo)
  private
    FFields: TArray<string>;
    FName: string;
  public
    function GetFields: TArray<System.string>;
    procedure SetFields(const AFields: TArray<System.string>);
    function GetName: string;
    procedure SetName(const AName: string);
  end;

  TMiniRESTSQLForeignKeyInfo = class(TInterfacedObject, IMiniRESTSQLForeignKeyInfo)
  private
    FFKFields: TArray<string>;
    FFKTableName: string;
    FFields: TArray<string>;
    FName: string;
  public
    function GetFKFields: TArray<System.string>;
    procedure SetFKFields(const AFields: TArray<System.string>);
    function GetFKTableName: string;
    procedure SetFKTableName(const AName: string);
    function GetFields: TArray<System.string>;
    procedure SetFields(const AFields: TArray<System.string>);
    function GetName: string;
    procedure SetName(const AName: string);
  end;

  TMiniRESTSQLColumnInfo = class(TInterfacedObject, IMiniRESTSQLColumnInfo)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    function GetName: string;
  end;

implementation

{ TMiniRESTSQLConnectionFactoryBase }

constructor TMiniRESTSQLConnectionFactoryBase.Create(const AConnectionCount: Integer);
begin
  FConnectionsCount := AConnectionCount;
  {$IFNDEF FPC}
  FSemaphore := TLightweightSemaphore.Create(AConnectionCount, AConnectionCount);
  FQueue := TQueue<IMiniRESTSQLConnection>.Create;  
  {$ELSE}
  FAvailableConnections := AConnectionCount;
  SetLength(FQueue, AConnectionCount);
  FConnectionGetEvent := RTLEventCreate;
  FQueuePosition := 0;
  {$ENDIF}
  FCriticalSection := TCriticalSection.Create;  
  FConnectionsToNotifyFree := TList.Create;
end;

destructor TMiniRESTSQLConnectionFactoryBase.Destroy;
var
  I: Integer;
begin
  for I := 0 to FConnectionsToNotifyFree.Count - 1 do
  begin
    TMiniRESTSQLConnectionBase(FConnectionsToNotifyFree.Items[I]).SetOwner(nil);
  end;
  {$IFNDEF FPC}
  FSemaphore.Free;
  FQueue.Free;
  {$ELSE}
  RTLeventdestroy(FConnectionGetEvent);
  {$ENDIF}  
  FCriticalSection.Free;
  FConnectionsToNotifyFree.Free;
  inherited;
end;

function TMiniRESTSQLConnectionFactoryBase.GetConnection: IMiniRESTSQLConnection;
var
  LConnection: IMiniRESTSQLConnection;
begin
  {$IFNDEF FPC}
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
  {$IFEND}
  {$IFDEF FPC}
  if InterlockedDecrement(FAvailableConnections) < 0 then    
    RTLeventWaitFor(FConnectionReleaseEvent);
  RTLeventWaitFor(FConnectionGetEvent);
  try
    if (FConnectionCounter < FConnectionsCount) then
    begin
      LConnection := InternalGetconnection.SetName('Connection' + IntToStr(FConnectionCounter));
      Inc(FConnectionCounter);
      Result := LConnection;
    end
    else
    begin
      Result := FQueue[FQueuePosition];
    end;
    Inc(FQueuePosition);
    if FConnectionsCount = FQueuePosition then
      FQueuePosition := 0;  
    TMiniRESTSQLConnectionBase(Result).FEstaNoPool := False;
    Inc(FAvailableConnections); 
  finally
    RTLeventSetEvent(FConnectionGetEvent);
  end;
  {$IFEND}
end;

procedure TMiniRESTSQLConnectionFactoryBase.ReleaseConnection(
  AConnection: IMiniRESTSQLConnection);
begin
  {$IFNDEF FPC}  
  FCriticalSection.Enter;
  try    
    FQueue.Enqueue(AConnection);    
    FSemaphore.Release(1);    
    TMiniRESTSQLConnectionBase(AConnection).FEstaNoPool := True;
    FConnectionsToNotifyFree.Remove(Pointer(AConnection));
  finally
    FCriticalSection.Leave;
  end;
  {$ELSE}
  RTLeventWaitFor(FConnectionGetEvent);
  try
    FQueue[FQueuePosition - 1] := AConnection;
    TMiniRESTSQLConnectionBase(AConnection).FEstaNoPool := True;
    FConnectionsToNotifyFree.Remove(Pointer(AConnection));
    Dec(FAvailableConnections);
  finally
    RTLeventSetEvent(FConnectionGetEvent);
  end;    
  {$IFEND}
end;

{ TMiniRESTSQLConnectionBase }

constructor TMiniRESTSQLConnectionBase.Create(
  AOwner: IMiniRESTSQLConnectionFactory);
begin
  FOwner := Pointer(AOwner);
  //TMiniRESTSQLConnectionFactoryBase(AOwner)
  //.FConnectionsToNotifyFree.Add(Pointer(Self));
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

procedure TMiniRESTSQLConnectionBase.SetOwner(AOwner: Pointer);
begin
  FOwner := AOwner;
end;

function TMiniRESTSQLConnectionBase._Release: Integer;
begin
  if (FRefCount = 1) and (FOwner <> nil) and (not FEstaNoPool) then
    IMiniRESTSQLConnectionFactory(FOwner).ReleaseConnection(Self);
  Result := inherited;
end;

{ TMiniRESTSQLPrimaryKeyInfo }

function TMiniRESTSQLPrimaryKeyInfo.GetFields: TArray<System.string>;
begin
  Result := FFields;
end;

function TMiniRESTSQLPrimaryKeyInfo.GetName: string;
begin
  Result := FName;
end;

procedure TMiniRESTSQLPrimaryKeyInfo.SetFields(
  const AFields: TArray<System.string>);
begin
  FFields := AFields;
end;

procedure TMiniRESTSQLPrimaryKeyInfo.SetName(const AName: string);
begin
  FName := AName;
end;

{ TMiniRESTSQLForeignKeyInfo }

function TMiniRESTSQLForeignKeyInfo.GetFields: TArray<System.string>;
begin
  Result := FFields;
end;

function TMiniRESTSQLForeignKeyInfo.GetFKFields: TArray<System.string>;
begin
  Result := FFKFields;
end;

function TMiniRESTSQLForeignKeyInfo.GetFKTableName: string;
begin
  Result := FFKTableName;
end;

function TMiniRESTSQLForeignKeyInfo.GetName: string;
begin
  Result := FName;
end;

procedure TMiniRESTSQLForeignKeyInfo.SetFields(
  const AFields: TArray<System.string>);
begin
  FFields := AFields;
end;

procedure TMiniRESTSQLForeignKeyInfo.SetFKFields(
  const AFields: TArray<System.string>);
begin
  FFKFields := AFields;
end;

procedure TMiniRESTSQLForeignKeyInfo.SetFKTableName(const AName: string);
begin
  FFKTableName := AName;
end;

procedure TMiniRESTSQLForeignKeyInfo.SetName(const AName: string);
begin
  FName := AName;
end;

{ TMiniRESTSQLColumnInfo }

constructor TMiniRESTSQLColumnInfo.Create(const AName: string);
begin
  FName := AName;
end;

function TMiniRESTSQLColumnInfo.GetName: string;
begin
  Result := FName;
end;

end.
