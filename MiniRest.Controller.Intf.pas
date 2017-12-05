unit MiniRest.Controller.Intf;

interface

uses MiniRest.Intf, MiniRest.Common;

type
  IMiniRestController = interface
  ['{16EA18CD-7AF3-4851-A85B-B2D9E0B22EB5}']
    function GetActionContext : IMiniRestActionContext;
    procedure SetActionContext(AContext : IMiniRestActionContext);
    procedure Response(AContent : string; AContentType : TMiniRestResponseType = rtTextHtml; AStatusCode : Integer = 200);
    procedure ResponseErro(AMsgErro : string; AStatusCode : Integer = 500);
    procedure ResponseJson(AJson : string; AStatusCode : Integer = 200);
  end;

implementation

end.
