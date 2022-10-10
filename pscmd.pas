unit pscmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPSComponent, uPSRuntime, psrt;

type

  TRuntimeEvent = procedure(Sender: TPSRuntime) of object;

  { TPSShell }

  TPSShell = class(TComponent)
  private
    FPScript: TPSScript;
    FRunning: Boolean;
    FPrompt: string;
    FOnExec: TRuntimeEvent;
    FShellProfile: string;
    procedure SetOnCompile(Value: TPSEvent);
  public
    property OnCompile: TPSEvent write SetOnCompile;
    property Prompt: string read FPrompt write FPrompt;
    property OnExec: TRuntimeEvent read FOnExec write FOnExec;
    property ShellProfile: string read FShellProfile write FShellProfile;
    constructor Create(AOwner: TComponent); override;
    procedure LoadSourceFile(const AFileName: string);
    procedure Execute;
    procedure RunLine(const line: string);
    procedure Compile(const AOutFile: string);
    procedure RunBinary(const AFileName: string);
    procedure CmdLoop;
    procedure StopLoop;
  end;

implementation

{ TPSShell }

procedure TPSShell.SetOnCompile(Value: TPSEvent);
begin
  FPScript.OnCompile:=Value;
end;

constructor TPSShell.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPScript:=TPSScript.Create(Self);
end;

procedure TPSShell.LoadSourceFile(const AFileName: string);
begin
  FPScript.Script.LoadFromFile(AFileName);
end;

procedure TPSShell.Execute;
begin
  if not FPScript.Compile then
    WriteLn('Compile error: '+FPScript.CompilerErrorToStr(0))
  else if not FPScript.Execute then
    WriteLn('Runtime Error: '+FPScript.ExecErrorToString);
end;

procedure TPSShell.RunLine(const line: string);
begin
  FPScript.Script.Clear;
  if FileExists(FShellProfile) then
  begin
    FPScript.Script.LoadFromFile(FShellProfile);
    FPScript.Script.Add('');
  end;
  FPScript.Script.Add('begin');
  FPScript.Script.Add(line);
  FPScript.Script.Add('end.');
  Execute;
end;

procedure TPSShell.Compile(const AOutFile: string);
var
  bc: string;
begin
  if not FPScript.Compile then
    WriteLn('Compile error: '+FPScript.CompilerErrorToStr(0))
  else
  begin
    FPScript.Comp.GetOutput(bc);
    with TFileStream.Create(AOutFile, fmCreate) do
      try
        Write(bc[1], Length(bc));
      finally
        Free;
      end;
  end;
end;

procedure TPSShell.RunBinary(const AFileName: string);
var
  rt: TPSRuntime;
begin
  rt:=TPSRuntime.Create(Nil);
  try
    if Assigned(FOnExec) then
      FOnExec(rt)
    else
      WriteLn('OnExec not assigned!');
    if not rt.LoadFromFile(AFileName) then
      WriteLn('Load Error: ',rt.ExecErrorString)
    else if not rt.RunProgram then
      WriteLn('Runtime Error: ',rt.ExecErrorString);
  finally
    rt.Free;
  end;
end;

procedure TPSShell.CmdLoop;
var
  line: string;
begin
  FRunning:=True;
  repeat
    { TODO : Update these to call events instead to allow for more than just terminal }
    Write(FPrompt);
    ReadLn(line);
    RunLine(line);
  until not FRunning;
end;

procedure TPSShell.StopLoop;
begin
  FRunning:=False;
  if FPScript.Running then
    FPScript.Stop;
end;

end.

