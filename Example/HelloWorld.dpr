program HelloWorld;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  MiniREST.ActionContext.Intf in '..\MiniREST.ActionContext.Intf.pas',
  MiniREST.Attribute in '..\MiniREST.Attribute.pas',
  MiniREST.Common in '..\MiniREST.Common.pas',
  MiniREST.Controller.Base in '..\MiniREST.Controller.Base.pas',
  MiniREST.Controller.Intf in '..\MiniREST.Controller.Intf.pas',
  MiniREST.Controller.Security.Intf in '..\MiniREST.Controller.Security.Intf.pas',
  MiniREST.ControllerOtherwise.Intf in '..\MiniREST.ControllerOtherwise.Intf.pas',
  MiniREST.Indy in '..\MiniREST.Indy.pas',
  MiniREST.Intf in '..\MiniREST.Intf.pas',
  MiniREST.JSON in '..\MiniREST.JSON.pas',
  MiniREST.RequestInfo in '..\MiniREST.RequestInfo.pas',
  MiniREST.Security.Base in '..\MiniREST.Security.Base.pas',
  MiniREST.Server.Base in '..\MiniREST.Server.Base.pas',
  MiniREST.Server.Intf in '..\MiniREST.Server.Intf.pas',
  MiniREST.Util in '..\MiniREST.Util.pas',
  JsonDataObjects in '..\JsonDataObjects\Source\JsonDataObjects.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
