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
    function GetConnectionFactory: IMiniRESTSQLConnectionFactory; override;
    procedure LogMessage(const AMessage: string); override;
  published
    procedure TestSQLDB1;
  end;

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
var
  LConnectionInfo: TStringList;
  LDBFilePath: string;
  LPathAux: string;
begin
  LConnectionInfo := TStringList.Create;
  try
    //LConnectionInfo.LoadFromFile('..\..\dbxcon.txt');
    LPathAux := ExtractFilePath(ParamStr(0)) + '..\TEST.FDB';
    LDBFilePath := ExpandFileName(LPathAux);
    LConnectionInfo.Values['DatabaseName'] := LDBFilePath;
    LConnectionInfo.Values['Server'] := 'localhost';
    Result := TMiniRESTSQLConnectionFactorySQLDb.Create(
      TMiniRESTSQLConnectionParamsSQLDb.New
      .SetConnectionsCount(3)
      .SetConnectionString(LConnectionInfo.Text)
      .SetDatabseType(dbtFirebird)
      .SetDatabaseName(LDBFilePath)
      .SetUserName('SYSDBA')
      .SetPassword('masterkey')
      .SetLogEvent(@LogEvent)
    );
  finally
    LConnectionInfo.Free;
  end;
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

initialization
  RegisterTest(TMiniRESTSQLTestSQLDbFPC.Suite);
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

