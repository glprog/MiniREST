unit MiniRest.ControllerOtherwise.Intf;

interface

uses MiniRest.Intf;

type

  IMiniRestControllerOtherwise = interface
  ['{E4F3F2A1-2F92-403B-9B11-10BD43A98BB0}']
    procedure Action(AContext : IMiniRestActionContext);
  end;

implementation

end.
