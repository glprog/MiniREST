program MiniRESTORMTest;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  uTestORM in 'uTestORM.pas',
  MiniREST.ORM.Attribute in '..\..\MiniREST.ORM.Attribute.pas',
  MiniREST.ORM.Column in '..\..\MiniREST.ORM.Column.pas',
  MiniREST.ORM.Common in '..\..\MiniREST.ORM.Common.pas',
  MiniREST.ORM.EntityManager in '..\..\MiniREST.ORM.EntityManager.pas',
  MiniREST.ORM.Intf in '..\..\MiniREST.ORM.Intf.pas',
  MiniREST.ORM.Mapper in '..\..\MiniREST.ORM.Mapper.pas',
  MiniREST.ORM.Table in '..\..\MiniREST.ORM.Table.pas',
  MiniREST.ORM.Utils in '..\..\MiniREST.ORM.Utils.pas',
  MiniREST.SQL.Base in '..\..\MiniREST.SQL.Base.pas',
  MiniREST.SQL.Common in '..\..\MiniREST.SQL.Common.pas',
  MiniREST.SQL.DBX in '..\..\MiniREST.SQL.DBX.pas',
  MiniREST.SQL.Intf in '..\..\MiniREST.SQL.Intf.pas',
  MiniREST.JSON in '..\..\MiniREST.JSON.pas',
  JsonDataObjects in '..\..\JsonDataObjects\Source\JsonDataObjects.pas',
  MiniREST.Util in '..\..\MiniREST.Util.pas',
  MiniREST.Intf in '..\..\MiniREST.Intf.pas',
  MiniREST.Common in '..\..\MiniREST.Common.pas',
  MiniREST.SQL.Firebird in '..\..\MiniREST.SQL.Firebird.pas',
  MiniREST.ORM.SQL in '..\..\MiniREST.ORM.SQL.pas',
  MiniREST.ORM.Firebird in '..\..\MiniREST.ORM.Firebird.pas',
  MiniREST.ORM.Schema in '..\..\MiniREST.ORM.Schema.pas',
  Pessoa.Model in 'Pessoa.Model.pas',
  MiniREST.Nullable in '..\..\MiniREST.Nullable.pas',
  MiniREST.Common.Utils in '..\..\MiniREST.Common.Utils.pas',
  DocumentModel in 'Model\DocumentModel.pas',
  Documento.Model.Intf in 'Documento.Model.Intf.pas',
  Pessoa.Model.Intf in 'Pessoa.Model.Intf.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  try
    if not (DebugHook > 0) then
    begin
      TDUnitX.Options.ExitBehavior := TDUnitXExitBehavior.Continue;
    end;

    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
