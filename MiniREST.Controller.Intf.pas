unit MiniREST.Controller.Intf;

interface

uses MiniREST.Intf, MiniREST.Common;

type
  IMiniRESTController = interface
  ['{16EA18CD-7AF3-4851-A85B-B2D9E0B22EB5}']
    function GetActionContext : IMiniRESTActionContext;
    procedure SetActionContext(AContext : IMiniRESTActionContext);
    procedure Response(AContent : string; AContentType : TMiniRESTResponseType = rtTextHtml; AStatusCode : Integer = 200);
    procedure ResponseErro(AMsgErro : string; AStatusCode : Integer = 500);
    procedure ResponseJson(AJson : string; AStatusCode : Integer = 200);
    function GetLogger : IMiniRESTLogger;
    procedure SetLogger(ALogger: IMiniRESTLogger);
  end;

implementation

end.
