program Supervisor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, BaseUnix,
  {$ENDIF}
  Classes, SysUtils, CustApp, pscmd, uPSComponent, dos, psrt, process, StrUtils,
  httpsrv, httproute, HTTPDefs, logger;

const
  {$IFDEF DEBUG}
  SIGNAL_FILE = 'stop.sig';
  {$ELSE}
  SIGNAL_FILE = '/tmp/stop.sig';
  {$ENDIF}

type

  { TSupervisorService }

  TSupervisorService = class(TComponent)
  private
    FProcList: TComponent;
    FSvcList: TComponent;
    FParameters: string;
    procedure SetParameters(const Value: string);
    {$IFDEF UNIX}
    procedure SetupProcess(Sender: TObject);
    {$ENDIF}
  public
    Executable, CurrentDirectory: string;
    RunUID, RunGID: cuint32;
    AlwaysRestart: Boolean;
    property ProcList: TComponent write FProcList;
    property SvcList: TComponent write FSvcList;
    property Parameters: string read FParameters write SetParameters;
    function GetProcess: TProcess;
  end;

  { TSupervisor }

  TSupervisor = class(TCustomApplication)
  private
    FCmdLine: TPSShell;
    FCmdList: TStringList;
    FProcList: TComponent;
    FSvcList: TComponent;
    FParams: string;
    FHTTPServer: TServerThread;
    FStartShell: Boolean;
    FPath: string;
    FUID, FGID: cuint32;
    FStopSig: Boolean;
    procedure WriteLn(const s: string);
    procedure AddMethod(Sender: TPSScript; Ptr: Pointer; const Decl: string);
    procedure PSCompile(Sender: TPSScript);
    procedure PSExecFuncs(Sender: TPSRuntime);
    procedure PSWriteLn(const s: string);
    procedure SetPrompt(const s: string);
    procedure RunSource(const AFileName: string);
    procedure CompileBC(const AFileName: string; const AOutFile: string);
    procedure RunBC(const AFileName: string);
    procedure PSExec(const cmd, parms: string);
    procedure RunService(const SName, cmd, parms: string);
    procedure StopService(const SName: string);
    procedure Logs(const SName: string);
    procedure ListServices;
    procedure StartHTTPServer(const APort: word);
    procedure StopHTTPServer;
    procedure ConfigHTTPRoutes;
    procedure StartShell;
    procedure SetPath(const s: string);
    function GetPath: string;
    procedure SetUser(const uid, gid: integer);
    procedure SavePID(const fname: string);
    procedure PSAlwaysRestart(const SName: string; value: boolean);
    function CheckStopSignal: Boolean;
    procedure WaitOnProcs;
    procedure MonitorServices;
    function GetParams: string;
    procedure EnterDir;
    procedure PSHalt;
    procedure CommandList;
    Procedure IndexRoute(ARequest: TRequest; AResponse: TResponse);
    procedure AlwaysRestart(ARequest: TRequest; AResponse: TResponse);
    procedure StopSvcRoute(ARequest: TRequest; AResponse: TResponse);
    procedure InfoRoute(ARequest: TRequest; AResponse: TResponse);
    procedure StopHTTPRoute(ARequest: TRequest; AResponse: TResponse);
    procedure StopServerRoute(ARequest: TRequest; AResponse: TResponse);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure StopAllServices;
  end;

{ TSupervisorService }

procedure TSupervisorService.SetParameters(const Value: string);
begin
  FParameters:=ReplaceStr(Value, ' ', ',');
end;

{$IFDEF UNIX}
procedure TSupervisorService.SetupProcess(Sender: TObject);
var
  p: TProcess;
  s: TSupervisorService;
begin
  p:=TProcess(Sender);
  s:=TSupervisorService(FSvcList.FindComponent(p.Name));
  if s.RunGID > 0 then
    FpSetgid(s.RunGID);
  if s.RunUID > 0 then
    FpSetuid(s.RunUID);
end;
{$ENDIF}

function TSupervisorService.GetProcess: TProcess;
begin
  Result:=TProcess.Create(FProcList);
  Result.Name:=Name;
  Result.Options:=[poUsePipes, poStderrToOutPut];
  Result.Executable:=Executable;
  Result.Parameters.CommaText:=Parameters;
  Result.CurrentDirectory:=CurrentDirectory;
  {$IFDEF UNIX}
  Result.OnForkEvent:=@SetupProcess;
  {$ENDIF}
  { TODO : Not sure of proper cross-platform API, most likely OS specific }
end;

{ TSupervisor }

procedure TSupervisor.WriteLn(const s: string);
begin
  if Assigned(FCmdLine) then
    System.WriteLn(s);
  LogInfo(s);
end;

procedure TSupervisor.AddMethod(Sender: TPSScript; Ptr: Pointer;
  const Decl: string);
begin
  if Sender.AddMethod(Self, Ptr, Decl) then
    FCmdList.Add(Decl);
end;

procedure TSupervisor.PSCompile(Sender: TPSScript);
begin
  FCmdList.Clear;
  AddMethod(Sender, @TSupervisor.PSWriteLn, 'procedure WriteLn(const s: string)');
  AddMethod(Sender, @TSupervisor.PSHalt, 'procedure Halt');
  AddMethod(Sender, @TSupervisor.SetPrompt, 'procedure SetPrompt(const s: string)');
  AddMethod(Sender, @TSupervisor.RunSource, 'procedure RunSource(const AFileName: string)');
  AddMethod(Sender, @TSupervisor.CompileBC, 'procedure Compile(const AFileName: string; const AOutFile: string)');
  AddMethod(Sender, @TSupervisor.RunBC, 'procedure Run(const AFileName: string)');
  AddMethod(Sender, @TSupervisor.PSExec, 'procedure Exec(const cmd, params: string)');
  AddMethod(Sender, @TSupervisor.RunService, 'procedure RunService(const SName, cmd, params: string)');
  AddMethod(Sender, @TSupervisor.StopService, 'procedure StopService(const SName: string)');
  AddMethod(Sender, @TSupervisor.ListServices, 'procedure ListServices');
  AddMethod(Sender, @TSupervisor.Logs, 'procedure Logs(const SName: string)');
  AddMethod(Sender, @TSupervisor.StartHTTPServer, 'procedure StartHTTPServer(const APort: word)');
  AddMethod(Sender, @TSupervisor.StopHTTPServer, 'procedure StopHTTPServer');
  AddMethod(Sender, @TSupervisor.StartShell, 'procedure StartShell');
  AddMethod(Sender, @TSupervisor.SetPath, 'procedure SetPath(const s: string)');
  AddMethod(Sender, @TSupervisor.GetPath, 'function GetPath: string');
  AddMethod(Sender, @TSupervisor.SetUser, 'procedure SetUser(const uid, gid: integer)');
  AddMethod(Sender, @TSupervisor.SavePID, 'procedure SavePID(const fname: string)');
  AddMethod(Sender, @TSupervisor.PSAlwaysRestart, 'procedure AlwaysRestart(const SName: string; value: boolean)');
  AddMethod(Sender, @TSupervisor.MonitorServices, 'procedure MonitorServices');
  AddMethod(Sender, @TSupervisor.GetParams, 'function GetParams: string');
  AddMethod(Sender, @TSupervisor.EnterDir, 'procedure EnterDir;');
  AddMethod(Sender, @TSupervisor.CommandList, 'procedure Help');
end;

procedure TSupervisor.PSExecFuncs(Sender: TPSRuntime);
begin
  Sender.RegisterDelphiMethod(Self, @TSupervisor.PSWriteLn, 'WRITELN', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.PSHalt, 'HALT', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.SetPrompt, 'SETPROMPT', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.RunSource, 'RUNSOURCE', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.CompileBC, 'COMPILE', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.RunBC, 'RUN', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.PSExec, 'EXEC', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.RunService, 'RUNSERVICE', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.StopService, 'STOPSERVICE', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.ListServices, 'LISTSERVICES', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.Logs, 'LOGS', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.StartHTTPServer, 'STARTHTTPSERVER', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.StopHTTPServer, 'STOPHTTPSERVER', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.StartShell, 'STARTSHELL', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.SetPath, 'SETPATH', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.GetPath, 'GETPATH', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.SetUser, 'SETUSER', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.SavePID, 'SAVEPID', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.PSAlwaysRestart, 'ALWAYSRESTART', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.MonitorServices, 'MONITORSERVICES', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.GetParams, 'GETPARAMS', cdRegister);
  Sender.RegisterDelphiMethod(Self, @TSupervisor.EnterDir, 'ENTERDIR', cdRegister);
end;

procedure TSupervisor.PSWriteLn(const s: string);
begin
  WriteLn(s);
end;

procedure TSupervisor.SetPrompt(const s: string);
begin
  FCmdLine.Prompt:=s;
end;

procedure TSupervisor.RunSource(const AFileName: string);
begin
  with TPSShell.Create(Nil) do
    try
      OnCompile:=@PSCompile;
      LoadSourceFile(AFileName);
      Execute;
    finally
      Free;
    end;
end;

procedure TSupervisor.CompileBC(const AFileName: string; const AOutFile: string
  );
begin
  with TPSShell.Create(Nil) do
    try
      OnCompile:=@PSCompile;
      LoadSourceFile(AFileName);
      Compile(AOutFile);
    finally
      Free;
    end;
end;

procedure TSupervisor.RunBC(const AFileName: string);
begin
  with TPSShell.Create(Nil) do
    try
      OnCompile:=@PSCompile;
      OnExec:=@PSExecFuncs;
      RunBinary(AFileName);
    finally
      Free;
    end;
end;

procedure TSupervisor.PSExec(const cmd, parms: string);
var
  path: string;
begin
  path:=GetCurrentDir;
  SetCurrentDir(FPath);
  Exec(cmd, parms);
  SetCurrentDir(path);
end;

procedure TSupervisor.RunService(const SName, cmd, parms: string);
var
  s: TSupervisorService;
  p: TProcess;
begin
  s:=TSupervisorService.Create(FSvcList);
  try
    s.ProcList:=FProcList;
    s.SvcList:=FSvcList;
    s.Name:=SName;
    s.Executable:=cmd;
    s.Parameters:=parms;
    s.CurrentDirectory:=FPath;
    s.AlwaysRestart:=False;
    s.RunUID:=FUID;
    s.RunGID:=FGID;
  except
    WriteLn('Service with this name already exists!');
  end;
  try
    p:=s.GetProcess;
    p.Execute;
  except
    p.Free;
    WriteLn('Unable to RunService, perhaps one is already running?');
  end;
end;

procedure TSupervisor.StopService(const SName: string);
var
  p: TProcess;
  s: TSupervisorService;
begin
  p:=TProcess(FProcList.FindComponent(SName));
  if not Assigned(p) then
    Exit;
  if p.Terminate(1) then
    WriteLn('Service Stopped.');
  if not p.Active then
  begin
    FProcList.RemoveComponent(p);
    p.Free;
    s:=TSupervisorService(FSvcList.FindComponent(SName));
    FSvcList.RemoveComponent(s);
    s.Free;
  end;
end;

procedure TSupervisor.Logs(const SName: string);
var
  p: TProcess;
  b: integer;
  s: string;
begin
  p:=TProcess(FProcList.FindComponent(SName));
  b:=p.Output.NumBytesAvailable;
  if b > 0 then
  begin
    SetLength(s, b);
    p.Output.Read(s[1], b);
    WriteLn(s);
  end;
end;

procedure TSupervisor.ListServices;
var
  i: Integer;
  p: TProcess;
  s: TSupervisorService;
begin
  for i:=0 to FProcList.ComponentCount-1 do
  begin
    p:=TProcess(FProcList.Components[i]);
    if p.Active then
      WriteLn(IntToStr(i)+': '+p.Name+' - '+p.Executable)
    else
    begin
      FProcList.RemoveComponent(p);
      p.Free;
      s:=TSupervisorService(FSvcList.Components[i]);
      FSvcList.RemoveComponent(s);
      s.Free;
    end;
  end;
  WriteLn(' - Total Services: '+IntToStr(FProcList.ComponentCount));
  if Assigned(FHTTPServer) then
    if FHTTPServer.Finished then
      FreeAndNil(FHTTPServer)
    else
      WriteLn('HTTP Server running on port: '+IntToStr(FHTTPServer.Port));
end;

procedure TSupervisor.StartHTTPServer(const APort: word);
begin
  if Assigned(FHTTPServer) then
    Exit;
  FHTTPServer:=TServerThread.Create(APort);
  FHTTPServer.Start;
  WriteLn('HTTP Server started on port '+IntToStr(APort));
end;

procedure TSupervisor.StopHTTPServer;
begin
  if not Assigned(FHTTPServer) then
    Exit;
  if FHTTPServer.Finished then
    FreeAndNil(FHTTPServer)
  else
  begin
    FHTTPServer.StopServer;
    WriteLn('Waiting for server to stop...');
    repeat
      Sleep(1000);
    until FHTTPServer.Finished;
    FreeAndNil(FHTTPServer);
  end;
end;

procedure TSupervisor.ConfigHTTPRoutes;
begin
  HTTPRouter.RegisterRoute('/', @IndexRoute);
  HTTPRouter.RegisterRoute('/AlwaysRestart', @AlwaysRestart);
  HTTPRouter.RegisterRoute('/StopService', @StopSvcRoute);
  HTTPRouter.RegisterRoute('/Info', @InfoRoute);
  HTTPRouter.RegisterRoute('/StopHTTP', @StopHTTPRoute);
  HTTPRouter.RegisterRoute('/StopServer', @StopServerRoute);
end;

procedure TSupervisor.StartShell;
begin
  FStartShell:=True;
end;

procedure TSupervisor.SetPath(const s: string);
begin
  FPath:=s;
end;

function TSupervisor.GetPath: string;
begin
  Result:=FPath;
end;

procedure TSupervisor.SetUser(const uid, gid: integer);
begin
  FUID:=uid;
  FGID:=gid;
end;

procedure TSupervisor.SavePID(const fname: string);
var
  pid: string;
begin
  pid:=IntToStr(GetProcessID);
  with TFileStream.Create(fname, fmCreate) do
    try
      Write(pid[1], Length(pid));
    finally
      Free;
    end;
end;

procedure TSupervisor.PSAlwaysRestart(const SName: string; value: boolean);
var
  s: TSupervisorService;
begin
  TSupervisorService(FSvcList.FindComponent(SName)).AlwaysRestart:=value;
end;

function TSupervisor.CheckStopSignal: Boolean;
var
  i: integer;
begin
  Result:=FStopSig;
  if Result then
    Exit;
  if FileExists(SIGNAL_FILE) then
  begin
    Result:=True;
    WriteLn('Supervisor Stop signal detected, closing down...');
    DeleteFile(SIGNAL_FILE);
    for i:=0 to FProcList.ComponentCount-1 do
      TProcess(FProcList.Components[i]).Terminate(1);
  end;
end;

procedure TSupervisor.WaitOnProcs;
var
  i: integer;
  p: TProcess;
begin
  repeat
    CheckStopSignal;
    for i:=0 to FProcList.ComponentCount-1 do
    begin
      p:=TProcess(FProcList.Components[i]);
      if not p.Active then
      begin
        FProcList.RemoveComponent(p);
        p.Free;
      end;
    end;
    Sleep(1000);
  until FProcList.ComponentCount = 0;
end;

procedure TSupervisor.MonitorServices;
var
  i: integer;
  p: TProcess;
  s: TSupervisorService;
  running: Boolean;
begin
  running:=True;
  repeat
    Sleep(1000);
    for i:=0 to FProcList.ComponentCount-1 do
    begin
      p:=TProcess(FProcList.Components[i]);
      if not p.Active then
      begin
        s:=TSupervisorService(FSvcList.FindComponent(p.Name));
        if (not FStopSig) and s.AlwaysRestart then
        begin
          WriteLn('Restarting stopped service: '+p.Name);
          FProcList.RemoveComponent(p);
          p.Free;
          p:=s.GetProcess;
          p.Execute;
        end
        else
        begin
          WriteLn('Service not set to restart: '+p.Name);
          FProcList.RemoveComponent(p);
          p.Free;
          FSvcList.RemoveComponent(s);
          s.Free;
          if FProcList.ComponentCount = 0 then
            running:=False;
        end;
      end;
    end;
    if CheckStopSignal then
      running:=False;
    if FProcList.ComponentCount = 0 then
      running:=False;
  until not running;
end;

function TSupervisor.GetParams: string;
begin
  Result:=FParams;
end;

procedure TSupervisor.EnterDir;
begin
  SetCurrentDir(FPath);
end;

procedure TSupervisor.PSHalt;
begin
  FCmdLine.StopLoop;
end;

procedure TSupervisor.CommandList;
var
  i: integer;
begin
  WriteLn('Full list of procedures: ');
  for i:=0 to FCmdList.Count-1 do
    WriteLn(FCmdList.Strings[i]);
end;

procedure TSupervisor.IndexRoute(ARequest: TRequest; AResponse: TResponse);
var
  i: integer;
  pers: string;
  SName: string;
begin
  LogInfo('Index Requested by '+ARequest.RemoteAddr);
  with AResponse.Contents do
    try
      Add('<html><head><title>Supervisor</title></head><body>');
      Add('<table width="800">');
      Add('<tr><th>Service Name</th><th>Persistent?</th><th>Actions</th></tr>');
      for i:=0 to FSvcList.ComponentCount-1 do
      begin
        SName:=FSvcList.Components[i].Name;
        if TSupervisorService(FSvcList.Components[i]).AlwaysRestart then
          pers:='Yes'
        else
          pers:='No';
        Add('<tr><td><a href="/Info?SName='+SName+'">'+SName+'</a></td>');
        Add('<td><a href="/AlwaysRestart?SName='+SName+'">'+pers+'</a></td>');
        Add('<td><a href="/StopService?SName='+SName+'">Stop</a></td></tr>');
      end;
      Add('</table></body></html>');
    except
      AResponse.Code:=500;
      AResponse.Content:='An error occured!';
    end;
end;

procedure TSupervisor.AlwaysRestart(ARequest: TRequest; AResponse: TResponse);
var
  SName: string;
  s: TSupervisorService;
begin
  SName:=ARequest.QueryFields.Values['SName'];
  if SName = '' then
  begin
    AResponse.Code:=404;
    AResponse.Content:='<h2>Service not found</h2>';
    Exit;
  end;
  s:=TSupervisorService(FSvcList.FindComponent(SName));
  if not Assigned(s) then
  begin
    AResponse.Code:=404;
    AResponse.Content:='<h2>Service not found</h2>';
    Exit;
  end;
  LogInfo('AlwaysRestart Requested by '+ARequest.RemoteAddr);
  if s.AlwaysRestart then
    s.AlwaysRestart:=False
  else
    s.AlwaysRestart:=True;
  AResponse.Code:=302;
  AResponse.Location:='/';
end;

procedure TSupervisor.StopSvcRoute(ARequest: TRequest; AResponse: TResponse);
var
  SName: string;
begin
  SName:=ARequest.QueryFields.Values['SName'];
  if SName = '' then
  begin
    AResponse.Code:=404;
    AResponse.Content:='<h2>Service not found</h2>';
    Exit;
  end;
  LogInfo('StopService Requested by '+ARequest.RemoteAddr);
  StopService(SName);
  AResponse.Code:=302;
  AResponse.Location:='/';
end;

procedure TSupervisor.InfoRoute(ARequest: TRequest; AResponse: TResponse);
var
  SName: string;
  s: TSupervisorService;
  p: TProcess;
  b: integer;
  buf: string;
begin
  SName:=ARequest.QueryFields.Values['SName'];
  if SName = '' then
  begin
    AResponse.Code:=404;
    AResponse.Content:='<h2>Service not found</h2>';
    Exit;
  end;
  s:=TSupervisorService(FSvcList.FindComponent(SName));
  p:=TProcess(FProcList.FindComponent(SName));
  if (not Assigned(s)) or (not Assigned(p)) then
  begin
    AResponse.Code:=404;
    AResponse.Content:='<h2>Service not found</h2>';
    Exit;
  end;
  with AResponse.Contents do
    try
      Add('<html><head><title>Supervisor - '+SName+'</title></head><body>');
      Add('<table width="800">');
      Add('<tr><th>Name:</th><td>'+SName+'</td></tr>');
      Add('<tr><th>Command:</th><td>'+s.Executable+'</td></tr>');
      Add('<tr><th>Parameters:</th><td>'+s.Parameters+'</td></tr>');
      Add('<tr><th>Directory:</th><td>'+s.CurrentDirectory+'</td></tr>');
      Add('<tr><th>PID:</th><td>'+IntToStr(p.ProcessID)+'</td></tr>');
      Add('<tr><th>Always Restart?</th><td><a href="/AlwaysRestart?SName='+SName+'">');
      if s.AlwaysRestart then
        Add('Yes')
      else
        Add('No');
      Add('</a></td></tr>');
      Add('</table>');
      b:=p.Output.NumBytesAvailable;
      if b > 0 then
      begin
        SetLength(buf, b);
        p.Output.Read(buf[1], b);
        Add('<pre>'+buf+'</pre>');
      end;
      Add('</body></html>');
    except
      AResponse.Code:=500;
      AResponse.Content:='Server Error!';
    end;
end;

procedure TSupervisor.StopHTTPRoute(ARequest: TRequest; AResponse: TResponse);
begin
  StopHTTPServer;
  AResponse.Content:='Done.';
end;

procedure TSupervisor.StopServerRoute(ARequest: TRequest; AResponse: TResponse);
begin
  StopAllServices;
  Terminate(3);
end;

procedure TSupervisor.DoRun;
var
  ErrorMsg: String;
  i: Integer;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hr:p:', 'help run params');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('p', 'params') then
    FParams:=GetOptionValue('p', 'params')
  else
    FParams:='';

  FProcList:=TComponent.Create(Self);
  FSvcList:=TComponent.Create(Self);
  ConfigHTTPRoutes;

  if HasOption('r', 'run') then
  begin
    LogInfo('Running configuration: '+GetOptionValue('r','run'));
    RunBC(GetOptionValue('r','run'));
    if FStopSig then
      Exit;
    LogInfo('Configuration Complete!');
    if not FStartShell then
    begin
      LogInfo('Waiting on services to complete...');
      WaitOnProcs;
      LogInfo('Preparing to shutdown Supervisor...');
      Terminate;
      Exit;
    end;
  end;

  LogInfo('Starting PascalScript Supervisor Shell...');
  FCmdLine:=TPSShell.Create(Self);
  FCmdLine.OnCompile:=@PSCompile;
  FCmdLine.Prompt:='Supervisor>';
  FCmdLine.ShellProfile:='profile.txt';
  FCmdLine.CmdLoop;

  LogInfo('Shell exited, terminating running services...');
  StopAllServices;

  // stop program loop
  Terminate;
end;

constructor TSupervisor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FCmdLine:=Nil;
  FCmdList:=TStringList.Create;
  FHTTPServer:=Nil;
  FStartShell:=False;
  FPath:=GetCurrentDir;
  FUID:=0;
  FGID:=0;
  FStopSig:=False;
end;

destructor TSupervisor.Destroy;
begin
  LogInfo('Supervisor stopping...');
  FCmdList.Free;
  StopHTTPServer;
  LogInfo('Supervisor stopped.');
  inherited Destroy;
end;

procedure TSupervisor.WriteHelp;
begin
  { add your help code here }
  System.writeln('Usage: ', ExeName, ' -h');
end;

procedure TSupervisor.StopAllServices;
var
  i: integer;
begin
  FStopSig:=True;
  for i:=0 to FProcList.ComponentCount-1 do
    TProcess(FProcList.Components[i]).Terminate(1);
  if Assigned(FCmdLine) then
    FCmdLine.StopLoop;
end;

var
  Application: TSupervisor;
{$IFDEF UNIX}
  oa,na: psigactionrec;

procedure HandleSignal(sig: cint); cdecl;
begin
  if (sig = SIGTERM) or (sig = SIGINT) then
  begin
    Application.StopAllServices;
    Application.Terminate(2);
  end;
end;

procedure HookSignals;
begin
  New(oa);
  New(na);
  na^.sa_handler:=sigactionhandler(@HandleSignal);
  FillChar(na^.sa_mask, SizeOf(na^.sa_mask), #0);
  na^.sa_flags:=0;
  {$ifdef Linux}
  na^.sa_restorer:=Nil;
  {$endif}
  if FPSigaction(SIGINT, na, oa) <> 0 then
  begin
    WriteLn('Unable to trap signal: ',SIGINT);
    Halt(1);
  end;
  if FPSigaction(SIGTERM, na, oa) <> 0 then
  begin
    WriteLn('Unable to trap signal: ',SIGINT);
    Halt(1);
  end;
end;
{$ENDIF}

begin
  {$IFDEF UNIX}HookSignals;{$ENDIF}
  Application:=TSupervisor.Create(nil);
  Application.Title:='Supervisor';
  Application.Run;
  Application.Free;
  {$IFDEF UNIX}
  Dispose(na);
  Dispose(oa);
  {$ENDIF}
end.

