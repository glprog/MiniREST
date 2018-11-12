unit uTest.mORMot;

interface
uses
  DUnitX.TestFramework, uTest.Default;

type

  [TestFixture]
  TMiniRESTTestmORMot = class(TMiniRESTTestdefault)
  public
    [SetupFixture]
    procedure Setup; overload;
    [TearDownFixture]
    procedure TearDown;
  end;

implementation

uses MiniREST.mORMot;

procedure TMiniRESTTestmORMot.Setup;
begin
  FServer := TMiniRESTServermORMot.Create;
  FPorta := 8087;
  inherited;
end;

procedure TMiniRESTTestmORMot.TearDown;
begin
  FServer := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TMiniRESTTestmORMot);
end.
