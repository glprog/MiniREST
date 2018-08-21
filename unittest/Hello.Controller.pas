unit Hello.Controller;

interface

uses MiniREST.Controller.Base, MiniREST.Attribute;

type
  THelloController = class(TMiniRESTControllerBase)
  public
    [RequestMapping('/hello')]
    procedure Hello;
  end;

implementation

{ THelloController }

procedure THelloController.Hello;
begin
  ResponseJson('{"msg":"hello"}');
end;

end.
