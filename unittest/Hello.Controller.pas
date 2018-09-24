unit Hello.Controller;

interface

uses MiniREST.Controller.Base, MiniREST.Attribute;

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
  ResponseJson('{}');  
end;

end.
