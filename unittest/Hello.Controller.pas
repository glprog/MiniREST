unit Hello.Controller;

interface

uses SysUtils, MiniREST.Controller.Base, MiniREST.Attribute, MiniREST.Common;

type
  THelloController = class(TMiniRESTControllerBase)
  public
    [RequestMapping('/hello')]
    procedure Hello;
    [RequestMapping('/hello/{name}')]
    procedure HelloWithName;
    [RequestMapping('/helloHeader')]
    procedure HelloHeader;
    [RequestMapping('/helloAppendHeader')] 
    procedure HelloAppendHeader;
    [RequestMapping('/queryParam')]
    procedure HelloQueryParam;
    [RequestMapping('/getRequestContentAsString', rmPost)]
    procedure HelloGetRequestContentAsString;
    [RequestMapping('/helloGet', rmGet)]
    procedure HelloGet;
    [RequestMapping('/helloPut', rmPut)]
    procedure HelloPut;       
    [RequestMapping('/helloPost', rmPost)]
    procedure HelloPost;
    [RequestMapping('/helloDelete', rmDelete)]
    procedure HelloDelete;
    [RequestMapping('/helloOptions', rmOptions)]
    procedure HelloOptions;
    [RequestMapping('/helloVerb')]
    procedure HelloGet2;
    [RequestMapping('/helloVerb', rmPost)]
    procedure HelloPost2;
    [RequestMapping('/helloMRestToken')]
    procedure HelloMRestToken;
    [RequestMapping('/helloContentType')]
    procedure HelloContentType;
    [RequestMapping('/helloContentTypeJson')]
    procedure HelloContentTypeJson;
    [RequestMapping('/helloRedirect')]
    procedure HelloRedirect;    
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

end.
