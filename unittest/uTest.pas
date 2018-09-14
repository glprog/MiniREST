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
    [SetupFixture]
    procedure Setup;
    [TearDownFixture]
    procedure TearDown;
    [Test]
    procedure TestHello;
    [Test]
    procedure TestHelloWithName;
    [Test]
    procedure TestHeader;
  end;

implementation

uses MiniREST.Indy, HttpConnection, HttpConnectionIndy, Hello.Controller;

procedure TMiniRESTTest.Setup;
begin
  FServer := TMiniRESTServerIndy.Create;
  FServer.AddController(THelloController);
  FServer.SetPort(8099);
  FServer.Start;
end;

procedure TMiniRESTTest.TearDown;
begin
  FServer := nil;
end;

procedure TMiniRESTTest.TestHeader;
begin

end;

procedure TMiniRESTTest.TestHello;
var
  LConnection: IHttpConnection;
  LStream: TStringStream;
begin
  LConnection := THttpConnectionIndy.Create;
  LStream := TStringStream.Create;
  LStream.Position := 0;
  try
    LConnection.Get('http://localhost:8099/hello', LStream);
    Assert.AreEqual('{"msg":"hello"}', LStream.DataString);
  finally
    LStream.Free;
  end;
end;

procedure TMiniRESTTest.TestHelloWithName;
var
  LConnection: IHttpConnection;
  LStream: TStringStream;
begin
  LConnection := THttpConnectionIndy.Create;
  LStream := TStringStream.Create;
  LStream.Position := 0;
  try
    LConnection.Get('http://localhost:8099/hello/hueBR', LStream);
    Assert.AreEqual('{"msg":"hello hueBR"}', LStream.DataString);
  finally
    LStream.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMiniRESTTest);
end.
