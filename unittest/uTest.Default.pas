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
    [Test]
    procedure TestQueryParam;
    [Test]
    procedure TestGetRequestContentAsString;
    [Test]
    procedure TestGet;
    [Test]
    procedure TestPut;
    [Test]
    procedure TestPost;
    [Test]
    procedure TestDelete;    
    [Test]
    procedure TestOptions;
    [Test]
    procedure TestGet2;
    [Test]
    procedure TestPost2;
  end;

implementation

uses IdHTTP, IdHeaderList, {HttpConnection, HttpConnectionIndy,} Hello.Controller;

{ TMiniRESTTestdefault }

type
  TIdHeaderListHack = class(TIdHeaderList);

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
  LResponseHeader, LTemp: string;
  LHeaders: TIdHeaderList;
  I: Integer;
begin
  LConnection := TIdHTTP.Create;
  LStream := TStringStream.Create;
  LStream.Position := 0;
  try
    LConnection.Get('http://localhost:' + IntToStr(FPorta) + '/helloAppendHeader', LStream);
    LHeaders := LConnection.Response.RawHeaders;
    I := 0;
    while I < LHeaders.Count do
    begin
      if LHeaders.Names[I] = 'TestAppendHeader' then
      begin
        LTemp := TIdHeaderListHack(LHeaders).GetValueFromLine(I);
        if LResponseHeader = '' then
          LResponseHeader := LTemp
        else
          LResponseHeader := LResponseHeader + ' ' + LTemp;
      end
      else
        TIdHeaderListHack(LHeaders).SkipValueAtLine(I);
    end;
    Assert.AreEqual('Hello World !', LResponseHeader);
  finally
    LConnection.Free;
    LStream.Free;
  end;  
end;

procedure TMiniRESTTestdefault.TestQueryParam;
var
  LConnection: TIdHTTP;
  LStream: TStringStream;
begin
  LConnection := TIdHTTP.Create;
  LStream := TStringStream.Create;
  LStream.Position := 0;
  try
    LConnection.Get('http://localhost:' + IntToStr(FPorta) + '/queryParam?param1=%25Hue123+hn'+
    '&param2=hhn6%26+%26&param3=kkLP', LStream);
    //%Hue123 hn
    //hhn6& &
    //kkLP
    Assert.AreEqual('{"param1": "%Hue123 hn", "param2": "hhn6& &", "param3": "kkLP"}', LStream.DataString);
  finally
    LConnection.Free;
    LStream.Free;
  end;  
end;

procedure TMiniRESTTestdefault.TestGetRequestContentAsString;
var
  LConnection: TIdHTTP;
  LRequest, LResponse: TStringStream;
begin
  LConnection := TIdHTTP.Create;
  LRequest := TStringStream.Create;
  LRequest.Position := 0;
  LResponse := TStringStream.Create;
  LResponse.Position := 0;
  try
    LRequest.WriteString('{"param1": "%Hue123 hn", "param2": "hhn6& &", "param3": "kkLP"}');
    LConnection.Post('http://localhost:' + IntToStr(FPorta) + '/getRequestContentAsString', LRequest, LResponse);
    //%Hue123 hn
    //hhn6& &
    //kkLP
    Assert.AreEqual(LRequest.DataString, LResponse.DataString);
  finally
    LConnection.Free;
    LResponse.Free;
    LRequest.Free;
  end;  
end;

procedure TMiniRESTTestdefault.TestGet;
var
  LConnection: TIdHTTP;
  LResponse: TStringStream;  
begin
  LConnection := TIdHTTP.Create;
  LResponse := TStringStream.Create;
  try    
    LConnection.Get('http://localhost:' + IntToStr(FPorta) + '/helloGet', LResponse);
    Assert.AreEqual('{"msg": "helloGet"}', LResponse.DataString);
  finally
    LConnection.Free;
    LResponse.Free;
  end; 
end;

procedure TMiniRESTTestdefault.TestPut;
var
  LConnection: TIdHTTP;
  LResponse, LRequest: TStringStream;  
begin
  LConnection := TIdHTTP.Create;
  LResponse := TStringStream.Create;
  LRequest := TStringStream.Create;
  try    
    LConnection.Put('http://localhost:' + IntToStr(FPorta) + '/helloPut', LRequest, LResponse);
    Assert.AreEqual('{"msg": "helloPut"}', LResponse.DataString);
  finally
    LConnection.Free;
    LResponse.Free;
    LRequest.Free;
  end; 
end;

procedure TMiniRESTTestdefault.TestPost;
var
  LConnection: TIdHTTP;
  LResponse, LRequest: TStringStream;  
begin
  LConnection := TIdHTTP.Create;
  LResponse := TStringStream.Create;
  LRequest := TStringStream.Create;
  try    
    LConnection.Post('http://localhost:' + IntToStr(FPorta) + '/helloPost', LRequest, LResponse);
    Assert.AreEqual('{"msg": "helloPost"}', LResponse.DataString);
  finally
    LConnection.Free;
    LResponse.Free;
    LRequest.Free;
  end; 
end;

procedure TMiniRESTTestdefault.TestDelete;
var
  LConnection: TIdHTTP;
  LResponse: TStringStream;  
begin
  LConnection := TIdHTTP.Create;
  LResponse := TStringStream.Create;
  try    
    LConnection.Delete('http://localhost:' + IntToStr(FPorta) + '/helloDelete', LResponse);
    Assert.AreEqual('{"msg": "helloDelete"}', LResponse.DataString);
  finally
    LConnection.Free;
    LResponse.Free;
  end;
end;

procedure TMiniRESTTestdefault.TestOptions;
var
  LConnection: TIdHTTP;
  LResponse: TStringStream;  
begin
  LConnection := TIdHTTP.Create;
  LResponse := TStringStream.Create;
  try    
    LConnection.Options('http://localhost:' + IntToStr(FPorta) + '/helloOptions', LResponse);
    Assert.AreEqual('{"msg": "helloOptions"}', LResponse.DataString);
  finally
    LConnection.Free;
    LResponse.Free;
  end;
end;

procedure TMiniRESTTestdefault.TestGet2;
var
  LConnection: TIdHTTP;
  LResponse: TStringStream;  
begin
  LConnection := TIdHTTP.Create;
  LResponse := TStringStream.Create;
  try    
    LConnection.Get('http://localhost:' + IntToStr(FPorta) + '/helloVerb', LResponse);
    Assert.AreEqual('{"msg": "helloGet2"}', LResponse.DataString);
  finally
    LConnection.Free;
    LResponse.Free;
  end;  
end;

procedure TMiniRESTTestdefault.TestPost2;
var
  LConnection: TIdHTTP;
  LResponse, LRequest: TStringStream;  
begin
  LConnection := TIdHTTP.Create;
  LResponse := TStringStream.Create;
  LRequest := TStringStream.Create;
  try    
    LConnection.Post('http://localhost:' + IntToStr(FPorta) + '/helloVerb', LRequest, LResponse);
    Assert.AreEqual('{"msg": "helloPost2"}', LResponse.DataString);
  finally
    LConnection.Free;
    LResponse.Free;
    LRequest.Free;
  end;   
end;

end.
