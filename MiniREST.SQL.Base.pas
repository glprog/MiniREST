{$IFDEF FPC}
  {$mode DELPHI}
{$IFEND}
unit MiniREST.SQL.Base;

interface

uses MiniREST.SQL.Intf, MiniREST.SQL.Common, SyncObjs, {$IFNDEF FPC}Generics.Collections, 
{$ELSE}fgl,{$IFEND} SysUtils, Classes;

type

  TMiniRESTSQLConnectionBase = class;

  { TMiniRESTSQLConnectionFactoryBase }

  TMiniRESTSQLConnectionFactoryBase = class abstract(TInterfacedObject, IMiniRESTSQLConnectionFactory)
  strict private
    FConnectionsToNotifyFree: TList;
  protected
    FConnectionFactoryEventLogger: IMiniRESTSQLConnectionFactoryEventLogger;
    {$IFNDEF FPC}
    FSemaphore: TLightweightSemaphore;
    FQueue: TQueue<IMiniRESTSQLConnection>;    
    {$ELSE}
    FConnectionGetEvent: PRTLEvent;
    FConnectionReleaseEvent: PRTLEvent;    
    FQueue: TFPGInterfacedObjectList<IMiniRESTSQLConnection>;
    //FQueuePosition: Integer;
    FAvailableConnections: Integer;    
    {$IFEND}
    FCriticalSection: TCriticalSection;
    FConnectionCounter: Integer;
    FConnectionsCount: Integer;
    FSingletonConnection: IMiniRESTSQLConnection;
    procedure AddConnectionToNotifyFree(AConnection: IMiniRESTSQLConnection);
    procedure RemoveConnectionToNotifyFree(AConnection: IMiniRESTSQLConnection);
    procedure ReleaseConnection(AConnection: IMiniRESTSQLConnection); virtual;
    function InternalGetconnection: IMiniRESTSQLConnection; virtual; abstract;
    procedure LogConnectionPoolEvent(const AMessage: string);
  public
    constructor Create(const AConnectionCount: Integer); overload;
    constructor Create(AParams: IMiniRESTSQLConnectionFactoryParams); overload;
    destructor Destroy; override;
    function GetConnection: IMiniRESTSQLConnection; overload;
    function GetObject: TObject;
    function GetConnectionsCount: Integer; virtual; abstract;
    function GetQueueCount: Integer; virtual; abstract;
    function GetConnection(const AIdentifier: string): IMiniRESTSQLConnection; overload;
    function GetSingletonConnection: IMiniRESTSQLConnection;
  end;

  { TMiniRESTSQLConnectionBase }

  TMiniRESTSQLConnectionBase = class abstract(TInterfacedObject, IMiniRESTSQLConnection)
  strict private
    FOwner: TObject;
    FConnectionID: Integer;
  protected
    FName: string;
    FEstaNoPool: Boolean;
    function _Release: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function GetObject: TObject; virtual; abstract;
    procedure SetOwner(AOwner: Pointer);
  public
    constructor Create(AOwner: IMiniRESTSQLConnectionFactory); overload;
    constructor Create(AParams: IMiniRESTSQLConnectionParams); overload;
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
    function InTransaction: Boolean; virtual; abstract;
    function GetConnectionID: Integer;
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

  TMiniRESTSQLConnectionParams = class(TInterfacedObject, IMiniRESTSQLConnectionParams)
  private
    FConnectionFactory: IMiniRESTSQLConnectionFactory;
    FConnectionID: Integer;
  public
    //class function New: IMiniRESTSQLConnectionParams;
    function GetConnectionFactory: IMiniRESTSQLConnectionFactory;
    procedure SetConnectionFactory(AConnectionFactory: IMiniRESTSQLConnectionFactory);
    function GetConnectionID: Integer;
    procedure SetConnectionID(const AID: Integer);
  end;

  TMiniRESTSQLConnectionFactoryParams = class(TInterfacedObject, IMiniRESTSQLConnectionFactoryParams)
  private
    FConnectionCount: Integer;
    FConnectionFactoryEventLogger: IMiniRESTSQLConnectionFactoryEventLogger;
  public
    function GetConnectionsCount: Integer;
    procedure SetConnectionsCount(const ACount: Integer);
    function GetObject: TObject;
    function GetConnectionFactoryEventLogger: IMiniRESTSQLConnectionFactoryEventLogger;
    procedure SetConnectionFactoryEventLogger(ALogger: IMiniRESTSQLConnectionFactoryEventLogger);
  end;

implementation

var
  gConnectionIDCounter: Integer;

{ TMiniRESTSQLConnectionFactoryBase }

constructor TMiniRESTSQLConnectionFactoryBase.Create(const AConnectionCount: Integer);
var
  LParams: IMiniRESTSQLConnectionFactoryParams;
begin
  LParams := TMiniRESTSQLConnectionFactoryParams.Create;
  LParams.SetConnectionsCount(AConnectionCount);
  Create(LParams);
end;

destructor TMiniRESTSQLConnectionFactoryBase.Destroy;
var
  I: Integer;
  LItemToNotifyFree: TMiniRESTSQLConnectionBase;
begin
  for I := 0 to (FConnectionsToNotifyFree.Count - 1) do
  begin    
    LItemToNotifyFree := TMiniRESTSQLConnectionBase(FConnectionsToNotifyFree.Items[I]);
    LItemToNotifyFree.SetOwner(nil);
  end;
  {$IFNDEF FPC}
  FSemaphore.Free;
  FQueue.Free;
  {$ELSE}
  RTLeventdestroy(FConnectionReleaseEvent);
  RTLeventdestroy(FConnectionGetEvent);
  FQueue.Free;
  {$ENDIF}  
  FCriticalSection.Free;
  FConnectionsToNotifyFree.Free;
  inherited;
end;

function TMiniRESTSQLConnectionFactoryBase.GetConnection: IMiniRESTSQLConnection;
begin
  Result := GetConnection('');
end;

function TMiniRESTSQLConnectionFactoryBase.GetObject: TObject;
begin
  Result := Self;
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
  {$IFEND}  
end;

{ TMiniRESTSQLConnectionBase }

constructor TMiniRESTSQLConnectionBase.Create(
  AOwner: IMiniRESTSQLConnectionFactory);
var
  LID: Integer;
  LParams: IMiniRESTSQLConnectionParams;
begin
  LID := InterLockedIncrement(gConnectionIDCounter);
  LParams := TMiniRESTSQLConnectionParams.Create;
  LParams.SetConnectionFactory(AOwner);
  LParams.SetConnectionID(LID);
  Create(LParams);
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

function TMiniRESTSQLConnectionBase._Release: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if (FRefCount = 1) and (FOwner <> nil) and (not FEstaNoPool) then
    TMiniRESTSQLConnectionFactoryBase(FOwner).ReleaseConnection(Self);
  Result := inherited _Release;
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

procedure TMiniRESTSQLConnectionFactoryBase.AddConnectionToNotifyFree(AConnection: IMiniRESTSQLConnection);
begin
  FConnectionsToNotifyFree.Add(AConnection.GetObject);
end;

procedure TMiniRESTSQLConnectionFactoryBase.RemoveConnectionToNotifyFree(AConnection: IMiniRESTSQLConnection);
begin
  FConnectionsToNotifyFree.Remove(AConnection.GetObject);
end;

procedure TMiniRESTSQLConnectionFactoryBase.LogConnectionPoolEvent(const AMessage: string);
begin
  if FConnectionFactoryEventLogger = nil then
    Exit;
  FConnectionFactoryEventLogger.LogPoolEvent(AMessage);
end;

function TMiniRESTSQLConnectionFactoryBase.GetConnection(const AIdentifier: string): IMiniRESTSQLConnection;
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
    FSemaphore.Release(1);
    FCriticalSection.Leave;
  end;
  {$IFEND}
  {$IFDEF FPC}
  if InterlockedDecrement(FAvailableConnections) < 0 then
  begin
    RTLeventResetEvent(FConnectionReleaseEvent);
    RTLeventWaitFor(FConnectionReleaseEvent);
  end;
  RTLeventWaitFor(FConnectionGetEvent);
  try
    if (FConnectionCounter < FConnectionsCount) then
    begin
      LConnection := InternalGetconnection.SetName('Connection' + IntToStr(FConnectionCounter) + ' ' + AIdentifier);
      Inc(FConnectionCounter);
      Result := LConnection;
    end
    else
    begin      
      LConnection := FQueue.Last;
      Result := FQueue.Extract(LConnection);      
    end;

    TMiniRESTSQLConnectionBase(Result.GetObject).FEstaNoPool := False;
    LogConnectionPoolEvent(Format('GET CONNECTION %d - %s', [Result.GetConnectionID, Result.GetName]));
  finally
    RTLeventSetEvent(FConnectionGetEvent);
  end;
  {$IFEND}  
end;

function TMiniRESTSQLConnectionBase.GetConnectionID: Integer;
begin
  Result := FConnectionID;  
end;

procedure TMiniRESTSQLConnectionParams.SetConnectionFactory(AConnectionFactory: IMiniRESTSQLConnectionFactory);
begin
  FConnectionFactory := AConnectionFactory;
end;

function TMiniRESTSQLConnectionParams.GetConnectionFactory: IMiniRESTSQLConnectionFactory;
begin
  Result := FConnectionFactory;
end;

function TMiniRESTSQLConnectionParams.GetConnectionID: Integer;
begin
  Result := FConnectionID;
end;

procedure TMiniRESTSQLConnectionParams.SetConnectionID(const AID: Integer);
begin
  FConnectionID := AID;
end;

constructor TMiniRESTSQLConnectionBase.Create(AParams: IMiniRESTSQLConnectionParams);
begin
  FOwner := nil;
  FOwner := AParams.GetConnectionFactory.GetObject;
  FConnectionID := AParams.GetConnectionID;
  
  TMiniRESTSQLConnectionFactoryBase(FOwner).AddConnectionToNotifyFree(Self);  
end;

function TMiniRESTSQLConnectionFactoryParams.GetConnectionsCount: Integer;
begin
  Result := FConnectionCount;
end;

procedure TMiniRESTSQLConnectionFactoryParams.SetConnectionsCount(const ACount: Integer);
begin
  FConnectionCount := ACount;
end;

// class function TMiniRESTSQLConnectionFactoryParams.New: IMiniRESTSQLConnectionFactoryParams;
// begin
//   Result := Create;  
// end;

constructor TMiniRESTSQLConnectionFactoryBase.Create(AParams: IMiniRESTSQLConnectionFactoryParams);
begin
  FConnectionsCount := AParams.GetConnectionsCount;
  {$IFNDEF FPC}
  FSemaphore := TLightweightSemaphore.Create(FConnectionsCount, FConnectionsCount);
  FQueue := TQueue<IMiniRESTSQLConnection>.Create;  
  {$ELSE}
  FAvailableConnections := FConnectionsCount;  
  FQueue := TFPGInterfacedObjectList<IMiniRESTSQLConnection>.Create;
  FConnectionGetEvent := RTLEventCreate;
  FConnectionReleaseEvent := RTLEventCreate;
  RTLeventSetEvent(FConnectionGetEvent);
  RTLeventSetEvent(FConnectionReleaseEvent);  
  {$ENDIF}
  FCriticalSection := TCriticalSection.Create;  
  FConnectionsToNotifyFree := TList.Create;
  FConnectionFactoryEventLogger := AParams.GetConnectionFactoryEventLogger;
end;

function TMiniRESTSQLConnectionFactoryParams.GetObject: TObject;
begin
  Result := Self;
end;

function TMiniRESTSQLConnectionFactoryParams.GetConnectionFactoryEventLogger: IMiniRESTSQLConnectionFactoryEventLogger;
begin
  Result := FConnectionFactoryEventLogger;  
end;

procedure TMiniRESTSQLConnectionFactoryParams.SetConnectionFactoryEventLogger(ALogger: IMiniRESTSQLConnectionFactoryEventLogger);
begin
  FConnectionFactoryEventLogger := ALogger;
end;

function TMiniRESTSQLConnectionFactoryBase.GetSingletonConnection: IMiniRESTSQLConnection;
begin
  if not Assigned(FSingletonConnection) then
    FSingletonConnection := GetConnection('Singleton Connetion');
  Result := FSingletonConnection;
end;

initialization
  gConnectionIDCounter := 0;

end.
