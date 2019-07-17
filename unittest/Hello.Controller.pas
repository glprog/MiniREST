unit Hello.Controller;

interface

uses Classes, SysUtils, MiniREST.Controller.Base, {$IFNDEF FPC} MiniREST.Attribute, {$IFEND} MiniREST.Common;

type
  THelloController = class(TMiniRESTControllerBase)
  public
    {$IFNDEF FPC}
    [RequestMapping('/hello')]
    {$IFEND}
    procedure Hello;
    {$IFNDEF FPC}
    [RequestMapping('/hello/{name}')]
    {$IFEND}
    procedure HelloWithName;
    {$IFNDEF FPC}
    [RequestMapping('/helloHeader')]
    {$IFEND}
    procedure HelloHeader;
    {$IFNDEF FPC}
    [RequestMapping('/helloAppendHeader')] 
    {$IFEND}
    procedure HelloAppendHeader;
    {$IFNDEF FPC}
    [RequestMapping('/queryParam')]
    {$IFEND}
    procedure HelloQueryParam;
    {$IFNDEF FPC}
    [RequestMapping('/getRequestContentAsString', rmPost)]
    {$IFEND}
    procedure HelloGetRequestContentAsString;
    {$IFNDEF FPC}
    [RequestMapping('/helloGet', rmGet)]
    {$IFEND}
    procedure HelloGet;
    {$IFNDEF FPC}
    [RequestMapping('/helloPut', rmPut)]
    {$IFEND}
    procedure HelloPut;       
    {$IFNDEF FPC}
    [RequestMapping('/helloPost', rmPost)]
    {$IFEND}
    procedure HelloPost;
    {$IFNDEF FPC}
    [RequestMapping('/helloDelete', rmDelete)]
    {$IFEND}
    procedure HelloDelete;
    {$IFNDEF FPC}
    [RequestMapping('/helloOptions', rmOptions)]
    {$IFEND}
    procedure HelloOptions;
    {$IFNDEF FPC}
    [RequestMapping('/helloVerb')]
    {$IFEND}
    procedure HelloGet2;
    {$IFNDEF FPC}
    [RequestMapping('/helloVerb', rmPost)]
    {$IFEND}
    procedure HelloPost2;
    {$IFNDEF FPC}
    [RequestMapping('/helloMRestToken')]
    {$IFEND}
    procedure HelloMRestToken;
    {$IFNDEF FPC}
    [RequestMapping('/helloContentType')]
    {$IFEND}
    procedure HelloContentType;
    {$IFNDEF FPC}
    [RequestMapping('/helloContentTypeJson')]
    {$IFEND}
    procedure HelloContentTypeJson;
    {$IFNDEF FPC}
    [RequestMapping('/helloRedirect')]
    {$IFEND}
    procedure HelloRedirect;
    {$IFNDEF FPC}
    [RequestMapping('/helloSendFile')]    
    {$IFEND}
    procedure HelloSendFile;
    {$IFNDEF FPC}
    [RequestMapping('/helloSendStream')]
    {$IFEND}
    procedure HelloSendStream;
  end;

implementation

{ THelloController }

procedure THelloController.Hello;
begin
  ResponseJson('{"msg":"hello"}');
end;

procedure THelloController.HelloWithName;
begin
  ResponseJson('{"msg":"hello ' +  PathVariable('name') + '"}');
end;

procedure THelloController.HelloHeader;
var
  LNomeNoHeader: string;
begin
  LNomeNoHeader := GetActionContext.GetHeader('NomeNoHeader');
  GetActionContext.SetHeader('TesteHeader', '321');
  ResponseJson('{"msg": "hello ' + LNomeNoHeader + '"}');  
end;

procedure THelloController.HelloAppendHeader;
begin
  GetActionContext.AppendHeader('TestAppendHeader', 'Hello');
  GetActionContext.AppendHeader('TestAppendHeader', 'World');
  GetActionContext.AppendHeader('TestAppendHeader', '!');
  ResponseJson('{}');  
end;

procedure THelloController.HelloQueryParam;
var
  LParam1, LParam2, LParam3, LJson: string;
begin
  LJson := '{"param1": "%s", "param2": "%s", "param3": "%s"}';
  LParam1 := QueryParam('param1');
  LParam2 := QueryParam('param2');
  LParam3 := QueryParam('param3');
  LJson := Format(LJson, [LParam1, LParam2, LParam3]);
  ResponseJson(LJson);
end;

procedure THelloController.HelloGetRequestContentAsString;
begin
  ResponseJson(GetActionContext.GetRequestContentAsString);  
end;

procedure THelloController.HelloGet;
begin  
  ResponseJson('{"msg": "helloGet"}');
end;

procedure THelloController.HelloPut;
begin
  ResponseJson('{"msg": "helloPut"}');
end;

procedure THelloController.HelloPost;
begin
  ResponseJson('{"msg": "helloPost"}');
end;

procedure THelloController.HelloDelete;
begin
  ResponseJson('{"msg": "helloDelete"}');
end;

procedure THelloController.HelloOptions;
begin
  ResponseJson('{"msg": "helloOptions"}');
end;

procedure THelloController.HelloGet2;
begin
  ResponseJson('{"msg": "helloGet2"}')  
end;

procedure THelloController.HelloPost2;
begin
  ResponseJson('{"msg": "helloPost2"}')  
end;

procedure THelloController.HelloMRestToken;
begin
  ResponseJson(Format('{"msg": "%s"}', [GetActionContext.GetHeader('MRestToken')]))  
end;

procedure THelloController.HelloContentType;
begin
  Response('<h1>Test</h1>', rtTextHtml, 200);
end;

procedure THelloController.HelloContentTypeJson;
begin
  Response('<h1>Test</h1>', rtApplicationJson, 200);  
end;

procedure THelloController.HelloRedirect;
begin
  GetActionContext.SendRedirect('http://www.hue.com');  
end;

procedure THelloController.HelloSendFile;
var
  LFile: string;
begin
  LFile := QueryParam('file');
  GetActionContext.ServeFile(LFile);
end;

procedure THelloController.HelloSendStream;
var
  LStringStream: TStringStream; 
begin
  LStringStream := TStringStream.Create{$IFDEF FPC}(''){$IFEND};
  try
    LStringStream.WriteString('teste');
    LStringStream.Position := 0;
    GetActionContext.SetResponseStream(LStringStream);  
  finally
    LStringStream.Free;
  end;
end;

end.
