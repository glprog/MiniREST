unit uTest.Indy.FPC;

interface
uses
  uTest.Default;

type
  
  TMiniRESTTestIndyFPC = class(TMiniRESTTestdefault)
  public
    procedure Setup; overload;    
    procedure TearDown;
  end;

implementation

uses MiniREST.Indy;

procedure TMiniRESTTestIndyFPC.Setup;
begin
  FServer := TMiniRESTServerIndy.Create;
  FPorta := 8099;
  inherited;
end;

procedure TMiniRESTTestIndyFPC.TearDown;
begin
  //FServer := nil;
end;

initialization
  RegisterTest(TMiniRESTTestIndyFPC.Suite);
end.
