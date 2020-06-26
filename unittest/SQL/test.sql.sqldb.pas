unit Test.SQL.SQLDb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, Test.SQL.Default, MiniREST.SQL.SQLDb,
  MiniREST.SQL.Intf, MiniREST.SQL.Common, sqldb;

type
  { TMiniRESTSQLTestSQLDbFPC }

  TMiniRESTSQLTestSQLDbFPC= class(TMiniRESTSQLTest)
  protected
    procedure SetUpOnce; override;
    function GetConnectionFactory: IMiniRESTSQLConnectionFactory; override; overload;
    function GetConnectionFactory(AParams: IMiniRESTSQLConnectionFactoryParams): IMiniRESTSQLConnectionFactory; override; overload;
    procedure LogMessage(const AMessage: string); override;
  published
    procedure TestSQLDB1;
  end;

  procedure LogEvent(Sender : TSQLConnection; EventType : TDBEventType; Const Msg : String); overload;
  procedure LogEvent(Const Msg : String); overload;

implementation

var
  gLog: TStringList;
  gRTLEvent: PRTLEvent;

procedure LogEvent(Sender : TSQLConnection; EventType : TDBEventType; Const Msg : String); overload;
begin
  if not gLogHabilitado then
    Exit;
  RTLeventWaitFor(gRTLEvent);
  try
    gLog.Add(Msg);
  finally
    RTLeventSetEvent(gRTLEvent);
  end;
end;

procedure LogEvent(Const Msg : String); overload;
begin
  if not gLogHabilitado then
    Exit;
  RTLeventWaitFor(gRTLEvent);
  try
    gLog.Add(Msg);
  finally
    RTLeventSetEvent(gRTLEvent);
  end;
end;

procedure TMiniRESTSQLTestSQLDbFPC.SetUpOnce;
begin
  SetupFixture;
end;

function TMiniRESTSQLTestSQLDbFPC.GetConnectionFactory: IMiniRESTSQLConnectionFactory;
begin
  Result := GetConnectionFactory(GetConnectionFactoryParams);
end;

procedure TMiniRESTSQLTestSQLDbFPC.TestSQLDB1;
var
  LConn1: IMiniRESTSQLConnection;
begin  
  LConn1 := FConnectionFactory.GetConnection;
  Check(Assigned(LConn1));
end;

procedure TMiniRESTSQLTestSQLDbFPC.LogMessage(const AMessage: string);
begin
  LogEvent(AMessage);
end;

function TMiniRESTSQLTestSQLDbFPC.GetConnectionFactory(AParams: IMiniRESTSQLConnectionFactoryParams): IMiniRESTSQLConnectionFactory;
var
  LParams: IMiniRESTSQLConnectionFactoryParamsSQLDb;
begin
  if not AParams.GetObject.GetInterface(IMiniRESTSQLConnectionFactoryParamsSQLDb, LParams) then
    raise Exception.Create('AParams não implementa a interface IMiniRESTSQLConnectionFactoryParamsSQLDb');
  Result := TMiniRESTSQLConnectionFactorySQLDb.Create(LParams);
end;

initialization  
  if FileExists('log.txt') then
    DeleteFile('log.txt');
  gLog := TStringList.Create;
  gRTLEvent := RTLEventCreate;
  RTLeventSetEvent(gRTLEvent);
finalization
  gLog.SaveToFile('log.txt');
  gLog.Free;
  RTLEventDestroy(gRTLEvent);
end.

