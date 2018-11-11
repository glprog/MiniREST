unit uTest;

interface
uses
  DUnitX.TestFramework, uTest.Default;

type

  [TestFixture]
  TMiniRESTTest = class(TMiniRESTTestdefault)
  public
    [SetupFixture]
    procedure Setup; overload;
    [TearDownFixture]
    procedure TearDown;
  end;

implementation

uses MiniREST.Indy;

procedure TMiniRESTTest.Setup;
begin
  FServer := TMiniRESTServerIndy.Create;
  FPorta := 8099;
  inherited;
end;

procedure TMiniRESTTest.TearDown;
begin
  //FServer := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TMiniRESTTest);
end.
