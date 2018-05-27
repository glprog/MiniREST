program HelloWorld;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  MiniREST.Controller.Base in '..\MiniREST.Controller.Base.pas',
  MiniREST.Indy in '..\MiniREST.Indy.pas',
  MiniREST.Server.Intf in '..\MiniREST.Server.Intf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
