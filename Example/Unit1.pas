unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, MiniREST.Controller.Base,
  MiniREST.Server.Intf, MiniREST.Attribute, MiniREST.Indy;

type
  TForm1 = class(TForm)
    btnStart: TButton;
    edtPort: TEdit;
    Label1: TLabel;
    procedure btnStartClick(Sender: TObject);
  private
    { Private declarations }
    FMiniRESTServer: IMiniRESTServer;
  public
    { Public declarations }
  end;

  THelloWorldController = class(TMiniRESTControllerBase)
  public
    [RequestMapping('/hello')]
    procedure Hello;
    [RequestMapping('/hello/{name}')]
    procedure HelloWithPathVariable;
  end;



var
  Form1: TForm1;

implementation

{$R *.dfm}

{ THelloWorldController }

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

end.
