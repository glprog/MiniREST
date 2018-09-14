unit uTest.mORMot;

interface
uses
  Classes, DUnitX.TestFramework, MiniREST.Intf, MiniREST.Server.Intf;

type

  [TestFixture]
  TMiniRESTTestmORMot = class(TObject)
  private
    FServer: IMiniRESTServer;
    FServermORMot: IMiniRESTServer;
  public
    [SetupFixture]
    procedure Setup;
    [TearDownFixture]
    procedure TearDown;
    [Test]
    procedure TestHello;
    [Test]
    procedure TestHelloWithName;
  end;

implementation

uses MiniREST.mORMot, HttpConnection, HttpConnectionIndy,
  Hello.Controller;

procedure TMiniRESTTestmORMot.Setup;
begin
  if FServermORMot = nil then
  begin
    FServermORMot := TMiniRESTServermORMot.Create;
    FServermORMot.AddController(THelloController);
    FServermORMot.SetPort(8087);
    FServermORMot.Start;
  end;
end;

procedure TMiniRESTTestmORMot.TearDown;
begin
  FServermORMot := nil;
end;

procedure TMiniRESTTestmORMot.TestHello;
var
  LConnection: IHttpConnection;
  LStream: TStringStream;
begin
  LConnection := THttpConnectionIndy.Create;
  LStream := TStringStream.Create;
  LStream.Position := 0;
  try
    LConnection.Get('http://localhost:8087/hello', LStream);
    Assert.AreEqual('{"msg":"hello"}', LStream.DataString);
  finally
    LStream.Free;
  end;
end;

procedure TMiniRESTTestmORMot.TestHelloWithName;
var
  LConnection: IHttpConnection;
  LStream: TStringStream;
begin
  LConnection := THttpConnectionIndy.Create;
  LStream := TStringStream.Create;
  LStream.Position := 0;
  try
    LConnection.Get('http://localhost:8087/hello/hueBR', LStream);
    Assert.AreEqual('{"msg":"hello hueBR"}', LStream.DataString);
  finally
    LStream.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMiniRESTTestmORMot);
end.
