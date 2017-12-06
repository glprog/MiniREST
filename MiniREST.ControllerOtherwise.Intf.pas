unit MiniREST.ControllerOtherwise.Intf;

interface

uses MiniREST.Intf;

type

  IMiniRESTControllerOtherwise = interface
  ['{E4F3F2A1-2F92-403B-9B11-10BD43A98BB0}']
    procedure Action(AContext : IMiniRESTActionContext);
  end;

implementation

end.
