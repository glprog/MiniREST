{$IFDEF FPC}
  {$mode DELPHI}
{$IFEND}
unit MiniREST.SQL.Base;

interface

uses MiniREST.SQL.Intf, MiniREST.SQL.Common, SyncObjs, {$IFNDEF FPC}Generics.Collections, 
{$ELSE} Contnrs, fgl,{$IFEND} SysUtils, Classes;

type

  TMiniRESTSQLConnectionBase = class;

  { TMiniRESTSQLConnectionFactoryBase }

  TMiniRESTSQLConnectionFactoryBase = class abstract(TInterfacedObject, IMiniRESTSQLConnectionFactory)
  strict private
    FConnectionsToNotifyFree: TList;
  protected
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
    procedure AddConnectionToNotifyFree(AConnection: IMiniRESTSQLConnection);
    procedure RemoveConnectionToNotifyFree(AConnection: IMiniRESTSQLConnection);
    procedure ReleaseConnection(AConnection: IMiniRESTSQLConnection); virtual;
    function InternalGetconnection: IMiniRESTSQLConnection; virtual; abstract;
    constructor Create(const AConnectionCount: Integer);
  public
    destructor Destroy; override;
    function GetConnection: IMiniRESTSQLConnection;
    function GetObject: TObject;
  end;

  { TMiniRESTSQLConnectionBase }

  TMiniRESTSQLConnectionBase = class abstract(TInterfacedObject, IMiniRESTSQLConnection)
  strict private
    FOwner : TObject;
  protected
    FName: string;
    FEstaNoPool: Boolean;
    function _Release: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
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
var
  I: Integer;
begin
  FConnectionsCount := AConnectionCount;
  {$IFNDEF FPC}
  FSemaphore := TLightweightSemaphore.Create(AConnectionCount, AConnectionCount);
  FQueue := TQueue<IMiniRESTSQLConnection>.Create;  
  {$ELSE}
  FAvailableConnections := AConnectionCount;  
  FQueue := TFPGInterfacedObjectList<IMiniRESTSQLConnection>.Create;
  FConnectionGetEvent := RTLEventCreate;
  FConnectionReleaseEvent := RTLEventCreate;
  RTLeventSetEvent(FConnectionGetEvent);
  RTLeventSetEvent(FConnectionReleaseEvent);  
  {$ENDIF}
  FCriticalSection := TCriticalSection.Create;  
  FConnectionsToNotifyFree := TList.Create;
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
  begin
    RTLeventResetEvent(FConnectionReleaseEvent);
    RTLeventWaitFor(FConnectionReleaseEvent);
  end;
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
      LConnection := FQueue.Last;
      Result := FQueue.Extract(LConnection);      
    end;

    TMiniRESTSQLConnectionBase(Result.GetObject).FEstaNoPool := False;
  finally
    RTLeventSetEvent(FConnectionGetEvent);
  end;
  {$IFEND}
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
begin
  FOwner := nil;
  FOwner := AOwner.GetObject;
  //TMiniRESTSQLConnectionFactoryBase(AOwner)
  //.FConnectionsToNotifyFree.Add(Self);
  TMiniRESTSQLConnectionFactoryBase(AOwner.GetObject).AddConnectionToNotifyFree(Self);
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

procedure TMiniRESTSQLConnectionFactoryBase.AddConnectionToNotifyFree(AConnection: IMiniRESTSQLConnection);
begin
  FConnectionsToNotifyFree.Add(AConnection.GetObject);
end;

procedure TMiniRESTSQLConnectionFactoryBase.RemoveConnectionToNotifyFree(AConnection: IMiniRESTSQLConnection);
begin
  FConnectionsToNotifyFree.Remove(AConnection.GetObject);
end;

end.
