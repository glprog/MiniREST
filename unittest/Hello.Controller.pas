unit Hello.Controller;

interface

uses SysUtils, MiniREST.Controller.Base, MiniREST.Attribute;

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

end.
