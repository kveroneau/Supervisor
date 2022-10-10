unit httpsrv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpapp, httproute;

type

  { TServerThread }

  TServerThread = class(TThread)
  private
    function GetPort: word;
  protected
    procedure Execute; override;
  public
    property Port: word read GetPort;
    constructor Create(const APort: word);
    procedure StopServer;
  end;

implementation

{ TServerThread }

function TServerThread.GetPort: word;
begin
  Result:=Application.Port;
end;

procedure TServerThread.Execute;
begin
  Application.Initialize;
  Application.Run;
end;

constructor TServerThread.Create(const APort: word);
begin
  inherited Create(True);
  Application.Title:='Supervisor';
  Application.Port:=APort;
  Application.Threaded:=True;
end;

procedure TServerThread.StopServer;
begin
  Application.Terminate;
end;

end.

