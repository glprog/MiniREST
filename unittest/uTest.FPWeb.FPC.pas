unit uTest.FPWeb.FPC;

interface
uses
  uTest.Default;

type
  
  TMiniRESTTestFPWebFPC = class(TMiniRESTTestdefault)
  public
    procedure Setup; overload;    
    procedure TearDown;
  end;

implementation

uses MiniREST.FPWeb;

procedure TMiniRESTTestFPWebFPC.Setup;
begin
  FServer := TMiniRESTServerFPWeb.Create;
  FPorta := 8099;
  inherited;
end;

procedure TMiniRESTTestFPWebFPC.TearDown;
begin
  //FServer := nil;
end;

initialization
  RegisterTest(TMiniRESTTestFPWebFPC.Suite);
end.
