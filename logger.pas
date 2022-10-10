unit logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, eventlog;

procedure LogInfo(const msg: string);
procedure LogWarning(const msg: string);
procedure LogError(const msg: string);

implementation

var
  log: TEventLog;

procedure SetupLog;
begin
  log:=TEventLog.Create(Nil);
  log.LogType:=ltFile;
  log.FileName:='Supervisor.log';
  log.AppendContent:=True;
  log.Active:=True;
  log.Info('Supervisor starting...');
end;

procedure LogInfo(const msg: string);
begin
  log.Info(msg);
end;

procedure LogWarning(const msg: string);
begin
  log.Warning(msg);
end;

procedure LogError(const msg: string);
begin
  log.Error(msg);
end;

initialization
  SetupLog;
finalization
  log.Free;
end.

