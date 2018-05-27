# MiniREST
Create a simple REST server using Delphi

## Example

```delphi
THelloWorldController = class(TMiniRESTControllerBase)
public
  [RequestMapping('/hello')]
  procedure Hello;
  [RequestMapping('/hello/{name}')]
  procedure HelloWithPathVariable;
end;

procedure THelloWorldController.Hello;
begin
  Response('Hello World!');
end;

procedure THelloWorldController.HelloWithPathVariable;
begin
  Response('Hello ' + PathVariable('name'));
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  FMiniRESTServer := TMiniRESTServerIndy.Create;
  FMiniRESTServer.AddController(THelloWorldController);
  FMiniRESTServer.SetPort(StrToIntDef(edtPort.Text, 8080));
  FMiniRESTServer.Start;
end;
```
