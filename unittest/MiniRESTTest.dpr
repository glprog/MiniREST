program MiniRESTTest;

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
  uTest in 'uTest.pas',
  JsonDataObjects in '..\JsonDataObjects\Source\JsonDataObjects.pas',
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
  MiniREST.SQL.Base in '..\MiniREST.SQL.Base.pas',
  MiniREST.SQL.Common in '..\MiniREST.SQL.Common.pas',
  MiniREST.SQL.DBX in '..\MiniREST.SQL.DBX.pas',
  MiniREST.SQL.Intf in '..\MiniREST.SQL.Intf.pas',
  MiniREST.Util in '..\MiniREST.Util.pas',
  Hello.Controller in 'Hello.Controller.pas',
  HttpConnection in '..\delphi-rest-client-api\src\HttpConnection.pas',
  HttpConnectionIndy in '..\delphi-rest-client-api\src\HttpConnectionIndy.pas',
  RestUtils in '..\delphi-rest-client-api\src\RestUtils.pas',
  RestException in '..\delphi-rest-client-api\src\RestException.pas',
  ProxyUtils in '..\delphi-rest-client-api\src\ProxyUtils.pas',
  MiniREST.mORMot in '..\MiniREST.mORMot.pas',
  SynCrtSock in '..\mORMot\SynCrtSock.pas',
  SynWinSock in '..\mORMot\SynWinSock.pas',
  uTest.mORMot in 'uTest.mORMot.pas',
  uTest.Default in 'uTest.Default.pas',
  MiniREST.SQL.Firebird in '..\MiniREST.SQL.Firebird.pas',
  MiniREST.Common.Utils in '..\MiniREST.Common.Utils.pas';

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
    TDUnitX.Options.ExitBehavior := TDUnitXExitBehavior.Continue;
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(True);
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
