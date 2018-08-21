unit uTest;

interface
uses
  Classes, DUnitX.TestFramework, MiniREST.Intf, MiniREST.Server.Intf;

type

  [TestFixture]
  TMiniRESTTest = class(TObject)
  private
    FServer: IMiniRESTServer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Test1;
    [Test]
    [TestCase('TestA','1,2')]
    [TestCase('TestB','3,4')]
    procedure Test2(const AValue1 : Integer;const AValue2 : Integer);
    [Test]
    procedure TestHello;
  end;

implementation

uses MiniREST.Indy, HttpConnection, HttpConnectionIndy;

procedure TMiniRESTTest.Setup;
begin
  FServer := TMiniRESTServerIndy.Create;
  FServer.SetPort(8099);
  FServer.Start;
end;

procedure TMiniRESTTest.TearDown;
begin
  FServer := nil;
end;

procedure TMiniRESTTest.Test1;
begin
end;

procedure TMiniRESTTest.Test2(const AValue1 : Integer;const AValue2 : Integer);
begin
end;

procedure TMiniRESTTest.TestHello;
var
  LConnection: IHttpConnection;
  LStream: TStringStream;
begin
  LConnection := THttpConnectionIndy.Create;
  LStream := TStringStream.Create;
  try
    LConnection.Get('http://localhost:8099/hello', LStream);
    WriteLn(LStream.ToString);
  finally
    LStream.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMiniRESTTest);
end.
