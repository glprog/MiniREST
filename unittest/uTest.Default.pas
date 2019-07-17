{$IFDEF FPC}
  {$mode DELPHI}
{$IFEND}
unit uTest.Default;

interface

uses SysUtils, Classes, {$IFNDEF FPC}DUnitX.TestFramework, {$ELSE} TestFramework, {$IFEND} MiniREST.Intf, MiniREST.Server.Intf{$IFNDEF FPC}, IdHTTP {$ELSE}, fphttpclient {$IFEND};

type
  TMiniRESTTestdefault = class({$IFDEF FPC}TTestCase{$ELSE}TObject{$IFEND})
  private
    function MD5(const fileName : string) : string; overload;
    function MD5(AStream: TStream): string; overload;
  protected
    FServer: IMiniRESTServer;
    FPorta: Integer;
    {$IFNDEF FPC}
    procedure OnRedirectTestRedirect(Sender: TObject; var dest: string; var NumRedirect: Integer; var Handled: boolean; var VMethod: TIdHTTPMethod);
    {$IFEND}
    procedure Get(const AURL: string; AHeaders: TStrings; AStream: TStream); overload;
    procedure Get(const AURL: string; AStream: TStream); overload;
    procedure Post(const AURL: string; ARequestStream, AResponseStream: TStream);
    procedure Put(const AURL: string; ARequestStream, AResponseStream: TStream);
    procedure Delete(const AURL: string; AStream: TStream);
    procedure Options(const AURL: string; AStream: TStream);
  public
    procedure Setup; virtual;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestHello;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestHelloWithName;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestRequestHeader;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestResponseHeader;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestAppendHeader;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestQueryParam;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestGetRequestContentAsString;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestGet;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestPut;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestPost;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestDelete;    
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestOptions;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestGet2;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestPost2;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestMRestToken;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestContentType;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestContentTypeJson;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure TestRedirect;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure HelloSendFile;
    {$IFNDEF FPC}
    [Test]
    {$IFEND}
    procedure HelloSendStream;
  end;

implementation

uses Hello.Controller, {$IFNDEF FPC} IdHeaderList, IdHashMessageDigest, idHash, {$IFEND} Types;

{ TMiniRESTTestdefault }

{$IFNDEF FPC}
type
  TIdHeaderListHack = class(TIdHeaderList);
  TIdHashMessageDigest5Hack = class(TIdHashMessageDigest5);
{$IFEND}

procedure TMiniRESTTestdefault.Setup;
begin
  FServer.AddController(THelloController);
  FServer.SetPort(FPorta);
  FServer.Start;
end;

procedure TMiniRESTTestdefault.TestRequestHeader;
var  
  LStream: TStringStream;
  LHeaders: TStringList;
begin
  LHeaders := TStringList.Create;
  LStream := TStringStream.Create('');
  LStream.Position := 0;
  try
    LHeaders.Values['NomeNoHeader'] := 'Bob';    
    Get('http://localhost:' + IntToStr(FPorta) + '/helloHeader', LHeaders, LStream);
    {$IFNDEF FPC}
    Assert.AreEqual('{"msg": "hello Bob"}', LStream.DataString);
    {$ELSE}
    CheckEquals('{"msg": "hello Bob"}', LStream.DataString);
    {$IFEND}
  finally    
    LStream.Free;
  end;
end;

procedure TMiniRESTTestdefault.TestResponseHeader;
var  
  LStream: TStringStream;
  LResponseHeader: string;  
begin  
  LStream := TStringStream.Create('');
  LStream.Position := 0;
  try
    Get('http://localhost:' + IntToStr(FPorta) + '/helloHeader', LStream);
    // LResponseHeader := LConnection.Response.RawHeaders.Values['TesteHeader'];
    {$IFNDEF FPC}
    Assert.AreEqual('321', LResponseHeader);
    {$ELSE}
    CheckEquals('321', LResponseHeader);
    {$IFEND}
  finally    
    LStream.Free;
  end;
end;

procedure TMiniRESTTestdefault.TestHello;
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create('');
  LStream.Position := 0;
  try
    Get('http://localhost:' + IntToStr(FPorta) + '/hello', LStream);
    {$IFNDEF FPC}
    Assert.AreEqual('{"msg":"hello"}', LStream.DataString);
    {$ELSE}
    CheckEquals('{"msg":"hello"}', LStream.DataString);
    {$IFEND}
  finally   
    LStream.Free;
  end;
end;

procedure TMiniRESTTestdefault.TestHelloWithName;
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create('');
  LStream.Position := 0;
  try
    Get('http://localhost:' + IntToStr(FPorta) + '/hello/hueBR', LStream);
    {$IFNDEF FPC}
    Assert.AreEqual('{"msg":"hello hueBR"}', LStream.DataString);
    {$ELSE}
    CheckEquals('{"msg":"hello hueBR"}', LStream.DataString);
    {$IFEND}
  finally    
    LStream.Free;
  end;
end;

procedure TMiniRESTTestdefault.TestAppendHeader;
var
  LStream: TStringStream;
  LResponseHeader, LTemp: string;
  // LHeaders: TIdHeaderList;
  I: Integer;
begin
  LStream := TStringStream.Create('');
  LStream.Position := 0;
  try
    Get('http://localhost:' + IntToStr(FPorta) + '/helloAppendHeader', LStream);
    {LHeaders := LConnection.Response.RawHeaders;
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
    end;}
    {$IFNDEF FPC}
    Assert.AreEqual('Hello World !', LResponseHeader);
    {$ELSE}
    CheckEquals('Hello World !', LResponseHeader);
    {$IFEND}
  finally    
    LStream.Free;
  end;  
end;

procedure TMiniRESTTestdefault.TestQueryParam;
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create('');
  LStream.Position := 0;
  try
    Get('http://localhost:' + IntToStr(FPorta) + '/queryParam?param1=%25Hue123+hn'+
    '&param2=hhn6%26+%26&param3=kkLP', LStream);
    //%Hue123 hn
    //hhn6& &
    //kkLP
    {$IFNDEF FPC}
    Assert.AreEqual('{"param1": "%Hue123 hn", "param2": "hhn6& &", "param3": "kkLP"}', LStream.DataString);
    {$ELSE}
    CheckEquals('{"param1": "%Hue123 hn", "param2": "hhn6& &", "param3": "kkLP"}', LStream.DataString);
    {$IFEND}
  finally    
    LStream.Free;
  end;  
end;

procedure TMiniRESTTestdefault.TestGetRequestContentAsString;
var
  LRequest, LResponse: TStringStream;
begin
  LRequest := TStringStream.Create('');
  LRequest.Position := 0;
  LResponse := TStringStream.Create('');
  LResponse.Position := 0;
  try
    LRequest.WriteString('{"param1": "%Hue123 hn", "param2": "hhn6& &", "param3": "kkLP"}');
    Post('http://localhost:' + IntToStr(FPorta) + '/getRequestContentAsString', LRequest, LResponse);
    //%Hue123 hn
    //hhn6& &
    //kkLP
    {$IFNDEF FPC}
    Assert.AreEqual(LRequest.DataString, LResponse.DataString);
    {$ELSE}
    CheckEquals(LRequest.DataString, LResponse.DataString);
    {$IFEND}
  finally
    LResponse.Free;
    LRequest.Free;
  end;  
end;

procedure TMiniRESTTestdefault.TestGet;
var
  LResponse: TStringStream;  
begin
  LResponse := TStringStream.Create('');
  try    
    Get('http://localhost:' + IntToStr(FPorta) + '/helloGet', LResponse);
    {$IFNDEF FPC}
    Assert.AreEqual('{"msg": "helloGet"}', LResponse.DataString);
    {$ELSE}
    CheckEquals('{"msg": "helloGet"}', LResponse.DataString);
    {$IFEND}
  finally
    LResponse.Free;
  end; 
end;

procedure TMiniRESTTestdefault.TestPut;
var
  LResponse, LRequest: TStringStream;  
begin
  LResponse := TStringStream.Create('');
  LRequest := TStringStream.Create('');
  try    
    Put('http://localhost:' + IntToStr(FPorta) + '/helloPut', LRequest, LResponse);
    {$IFNDEF FPC}
    Assert.AreEqual('{"msg": "helloPut"}', LResponse.DataString);
    {$ELSE}
    CheckEquals('{"msg": "helloPut"}', LResponse.DataString);
    {$IFEND}
  finally    
    LResponse.Free;
    LRequest.Free;
  end; 
end;

procedure TMiniRESTTestdefault.TestPost;
var
  LResponse, LRequest: TStringStream;  
begin
  LResponse := TStringStream.Create('');
  LRequest := TStringStream.Create('');
  try    
    Post('http://localhost:' + IntToStr(FPorta) + '/helloPost', LRequest, LResponse);
    {$IFNDEF FPC}
    Assert.AreEqual('{"msg": "helloPost"}', LResponse.DataString);
    {$ELSE}
    CheckEquals('{"msg": "helloPost"}', LResponse.DataString);
    {$IFEND}
  finally
    LResponse.Free;
    LRequest.Free;
  end; 
end;

procedure TMiniRESTTestdefault.TestDelete;
var
  LResponse: TStringStream;  
begin
  LResponse := TStringStream.Create('');
  try    
    Delete('http://localhost:' + IntToStr(FPorta) + '/helloDelete', LResponse);
    {$IFNDEF FPC}
    Assert.AreEqual('{"msg": "helloDelete"}', LResponse.DataString);
    {$ELSE}
    CheckEquals('{"msg": "helloDelete"}', LResponse.DataString);
    {$IFEND}
  finally
    LResponse.Free;
  end;
end;

procedure TMiniRESTTestdefault.TestOptions;
var
  LResponse: TStringStream;  
begin
  LResponse := TStringStream.Create('');
  try    
    Options('http://localhost:' + IntToStr(FPorta) + '/helloOptions', LResponse);
    {$IFNDEF FPC}
    Assert.AreEqual('{"msg": "helloOptions"}', LResponse.DataString);
    {$ELSE}
    CheckEquals('{"msg": "helloOptions"}', LResponse.DataString);
    {$IFEND}
  finally
    LResponse.Free;
  end;
end;

procedure TMiniRESTTestdefault.TestGet2;
var
  LResponse: TStringStream;  
begin
  LResponse := TStringStream.Create('');
  try    
    Get('http://localhost:' + IntToStr(FPorta) + '/helloVerb', LResponse);
    {$IFNDEF FPC}
    Assert.AreEqual('{"msg": "helloGet2"}', LResponse.DataString);
    {$ELSE}
    CheckEquals('{"msg": "helloGet2"}', LResponse.DataString);
    {$IFEND}
  finally
    LResponse.Free;
  end;  
end;

procedure TMiniRESTTestdefault.TestPost2;
var
  LResponse, LRequest: TStringStream;  
begin
  LResponse := TStringStream.Create('');
  LRequest := TStringStream.Create('');
  try    
    Post('http://localhost:' + IntToStr(FPorta) + '/helloVerb', LRequest, LResponse);
    {$IFNDEF FPC}
    Assert.AreEqual('{"msg": "helloPost2"}', LResponse.DataString);
    {$ELSE}
    CheckEquals('{"msg": "helloPost2"}', LResponse.DataString);
    {$IFEND}
  finally
    LResponse.Free;
    LRequest.Free;
  end;   
end;

procedure TMiniRESTTestdefault.TestMRestToken;
var
  LResponse: TStringStream;  
begin
  LResponse := TStringStream.Create('');
  try
    // LConnection.Request.CustomHeaders.AddValue('MRestToken','121314');    
    Get('http://localhost:' + IntToStr(FPorta) + '/helloMRestToken', LResponse);
    {$IFNDEF FPC}
    Assert.AreEqual('{"msg": "121314"}', LResponse.DataString);
    {$ELSE}
    CheckEquals('{"msg": "121314"}', LResponse.DataString);
    {$IFEND}
  finally
    LResponse.Free;
  end;   
end;

procedure TMiniRESTTestdefault.TestContentType;
var
  LResponse: TStringStream;
  LContentType: string;  
begin
  LResponse := TStringStream.Create('');
  try    
    Get('http://localhost:' + IntToStr(FPorta) + '/helloContentType', LResponse);        
    // LContentType := LConnection.Response.RawHeaders.Values['Content-Type'];
    {$IFNDEF FPC}
    Assert.AreEqual('<h1>Test</h1>', LResponse.DataString);
    Assert.AreEqual('text/html; charset=utf-8', LContentType);
    {$ELSE}
    CheckEquals('<h1>Test</h1>', LResponse.DataString);
    CheckEquals('text/html; charset=utf-8', LContentType);
    {$IFEND}
  finally
    LResponse.Free;
  end;   
end;

procedure TMiniRESTTestdefault.TestContentTypeJson;
var
  LResponse: TStringStream;
  LContentType: string;  
begin
  LResponse := TStringStream.Create('');
  try    
    Get('http://localhost:' + IntToStr(FPorta) + '/helloContentTypeJson', LResponse);        
    //LContentType := LConnection.Response.RawHeaders.Values['Content-Type'];
    {$IFNDEF FPC}
    Assert.AreEqual('<h1>Test</h1>', LResponse.DataString);
    Assert.AreEqual('application/json; charset=utf-8', LContentType);
    {$ELSE}
    CheckEquals('<h1>Test</h1>', LResponse.DataString);
    CheckEquals('application/json; charset=utf-8', LContentType);
    {$IFEND}
  finally
    LResponse.Free;
  end;  
end;

procedure TMiniRESTTestdefault.TestRedirect;
var
  LResponse: TStringStream;  
  LLocation: string;
begin
  LResponse := TStringStream.Create('');
  try  
    Get('http://localhost:' + IntToStr(FPorta) + '/helloRedirect', LResponse);
    // LLocation := LConnection.Response.Location;
    {$IFNDEF FPC}
    Assert.AreEqual('http://www.hue.com', LLocation);
    {$ELSE}
    CheckEquals('http://www.hue.com', LLocation);
    {$IFEND}
  finally
    LResponse.Free;
  end;  
end;

{$IFNDEF FPC}
procedure TMiniRESTTestdefault.OnRedirectTestRedirect(Sender: TObject; var dest: string; var NumRedirect: Integer; var Handled: boolean; var VMethod: TIdHTTPMethod);
begin
  Assert.AreEqual('http://www.hue.com', dest);
  Handled := True;  
end;
{$IFEND}

procedure TMiniRESTTestdefault.HelloSendFile;
var
  LResponse: TMemoryStream;
  LFiles: TStringDynArray;
  LHash, LHashResponse: string;
  i: Integer;  
begin
  LResponse := TMemoryStream.Create;
  SetLength(LFiles, 3);
  LFiles[0] := 'test1.txt';
  LFiles[1] := 'test2.html';
  LFiles[2] := 'someImg.jpg';  
  try
    for i := 0 to Length(LFiles) - 1 do
    begin
      LResponse.Position := 0;
      LResponse.Size := 0;
      LHash := MD5(ExtractFilePath(ParamStr(0)) + LFiles[i]);
      Get('http://localhost:' + IntToStr(FPorta) + '/helloSendFile?file=' + LFiles[i], LResponse);
      if (LResponse.Size > 0) then
      begin
        LResponse.SaveToFile(ExtractFilePath(ParamStr(0)) + LFiles[i] + 'return');
        LHashResponse := MD5(ExtractFilePath(ParamStr(0)) + LFiles[i] + 'return');
        {$IFNDEF FPC}
        Assert.AreEqual(LHash, LHashResponse, 'The hash is not equal ' + LFiles[i]);
        {$ELSE}
        CheckEquals(LHash, LHashResponse, 'The hash is not equal ' + LFiles[i]);
        {$IFEND}
      end
      else
      begin
        {$IFNDEF FPC}
        Assert.Fail('Did not return files', @TMiniRESTTestdefault.HelloSendFile);
        {$ELSE}
        Fail('Did not return files', @TMiniRESTTestdefault.HelloSendFile);
        {$IFEND}
      end;            
    end;    
  finally
    LResponse.Free;
  end;   
end;

function TMiniRESTTestdefault.MD5(const fileName: string): string;
var  
  fs : TFileStream;  
begin
  fs := TFileStream.Create(fileName, fmOpenRead OR fmShareDenyWrite) ;
  try
    result := MD5(fs);
  finally
    fs.Free;
  end;  
end;

function TMiniRESTTestdefault.MD5(AStream: TStream): string;
{$IFNDEF FPC}
var
  idmd5 : TIdHashMessageDigest5;  
  hash : T4x4LongWordRecord;
{$ELSE}
{$IFEND}
begin
  {$IFNDEF FPC}
  idmd5 := TIdHashMessageDigest5.Create;  
  try        
    result := TIdHashMessageDigest5Hack(idmd5).HashToHex(idmd5.HashStream(AStream));
  finally    
    idmd5.Free;
  end;
  {$ELSE}
  raise Exception.Create('Implement');
  {$IFEND}
end;

procedure TMiniRESTTestdefault.HelloSendStream;
var
  LResponse: TStringStream;  
  LResult: string;
begin
  LResponse := TStringStream.Create('');
  try    
    Get('http://localhost:' + IntToStr(FPorta) + '/helloSendStream', LResponse);
    // LResult := LResponse.DataString + LConnection.Response.RawHeaders.Text;
    {$IFNDEF FPC}
    Assert.AreEqual('{}', LResult);
    {$ELSE}
    CheckEquals('{}', LResult);
    {$IFEND}
  finally
    LResponse.Free;
  end;    
end;

procedure TMiniRESTTestdefault.Get(const AURL: string; AHeaders: TStrings; AStream: TStream);
{$IFNDEF FPC}
var
  LConnection: TIdHTTP;  
{$ELSE}
{$IFEND}
begin
  {$IFNDEF FPC}
  LConnection := TIdHTTP.Create(nil);
  {$ELSE}
  {$IFEND}
end;

procedure TMiniRESTTestdefault.Get(const AURL: string; AStream: TStream);
var
  LHeaders: TStringList;
begin
  LHeaders := TStringList.Create;
  try
    Get(AURL, LHeaders, AStream);
  finally
    LHeaders.Free;
  end;
end;

procedure TMiniRESTTestdefault.Post(const AURL: string; ARequestStream: TStream; AResponseStream: TStream);
begin
  
end;

procedure TMiniRESTTestdefault.Put(const AURL: string; ARequestStream: TStream; AResponseStream: TStream);
begin
  
end;

procedure TMiniRESTTestdefault.Delete(const AURL: string; AStream: TStream);
begin
  
end;

procedure TMiniRESTTestdefault.Options(const AURL: string; AStream: TStream);
begin
  
end;

end.
