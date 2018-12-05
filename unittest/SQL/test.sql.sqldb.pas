unit Test.SQL.SQLDb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, Test.SQL.Default, MiniREST.SQL.SQLDb;

type

  TMiniRESTSQLTestSQLDbFPC= class(TMiniRESTSQLTest)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

procedure TMiniRESTSQLTestSQLDbFPC.SetUp;
begin

end;

procedure TMiniRESTSQLTestSQLDbFPC.TearDown;
begin

end;

initialization
  RegisterTest(TMiniRESTSQLTestSQLDbFPC.Suite);
end.

