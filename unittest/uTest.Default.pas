unit uTest.Default;

interface

uses SysUtils, Classes, DUnitX.TestFramework, MiniREST.Intf, MiniREST.Server.Intf;

type
  TMiniRESTTestdefault = class
  protected
    FServer: IMiniRESTServer;
    FPorta: Integer;
  public
    procedure Setup; virtual;
    [Test]
    procedure TestHello;
    [Test]
    procedure TestHelloWithName;
    [Test]
    procedure TestRequestHeader;
    [Test]
    procedure TestResponseHeader;
    [Test]
    procedure TestAppendHeader;
  end;

implementation

uses IdHTTP, {HttpConnection, HttpConnectionIndy,} Hello.Controller;

{ TMiniRESTTestdefault }

procedure TMiniRESTTestdefault.Setup;
begin
  FServer.AddController(THelloController);
  FServer.SetPort(FPorta);
  FServer.Start;
end;

procedure TMiniRESTTestdefault.TestRequestHeader;
var
  LConnection: TIdHTTP;
  LStream: TStringStream;
begin
  LConnection := TIdHTTP.Create(nil);
  LStream := TStringStream.Create;
  LStream.Position := 0;
  try
    LConnection.Request.CustomHeaders.AddValue('NomeNoHeader', 'Bob');
    LConnection.Get('http://localhost:' + IntToStr(FPorta) + '/helloHeader', LStream);
    Assert.AreEqual('{"msg": "hello Bob"}', LStream.DataString);
  finally
    LConnection.Free;
    LStream.Free;
  end;
end;

procedure TMiniRESTTestdefault.TestResponseHeader;
var
  LConnection: TIdHTTP;
  LStream: TStringStream;
  LResponseHeader: string;
begin
  LConnection := TIdHTTP.Create(nil);
  LStream := TStringStream.Create;
  LStream.Position := 0;
  try
    LConnection.Get('http://localhost:' + IntToStr(FPorta) + '/helloHeader', LStream);
    LResponseHeader := LConnection.Response.RawHeaders.Values['TesteHeader'];
    Assert.AreEqual('321', LResponseHeader);
  finally
    LConnection.Free;
    LStream.Free;
  end;
end;

procedure TMiniRESTTestdefault.TestHello;
var
  LConnection: TIdHTTP;
  LStream: TStringStream;
begin
  LConnection := TIdHTTP.Create(nil);
  LStream := TStringStream.Create;
  LStream.Position := 0;
  try
    LConnection.Get('http://localhost:' + IntToStr(FPorta) + '/hello', LStream);
    Assert.AreEqual('{"msg":"hello"}', LStream.DataString);
  finally
    LConnection.Free;
    LStream.Free;
  end;
end;

procedure TMiniRESTTestdefault.TestHelloWithName;
var
  LConnection: TIdHTTP;
  LStream: TStringStream;
begin
  LConnection := TIdHTTP.Create;
  LStream := TStringStream.Create;
  LStream.Position := 0;
  try
    LConnection.Get('http://localhost:' + IntToStr(FPorta) + '/hello/hueBR', LStream);
    Assert.AreEqual('{"msg":"hello hueBR"}', LStream.DataString);
  finally
    LConnection.Free;
    LStream.Free;
  end;
end;

procedure TMiniRESTTestdefault.TestAppendHeader;
var
  LConnection: TIdHTTP;
  LStream: TStringStream;
  LResponseHeader: String;
begin
  LConnection := TIdHTTP.Create;
  LStream := TStringStream.Create;
  LStream.Position := 0;
  try
    LConnection.Response.RawHeaders.FoldLines := True;
    LConnection.Response.RawHeaders.UnFoldLines := False;
    LConnection.Get('http://localhost:' + IntToStr(FPorta) + '/helloAppendHeader', LStream);
    LResponseHeader := LConnection.Response.RawHeaders.Values['TestAppendHeader'];
    Assert.AreEqual('{"msg":"hello hueBR"}', LResponseHeader);
  finally
    LConnection.Free;
    LStream.Free;
  end;  
end;

end.
