unit psrt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPSRuntime, uPSUtils;

type

  { TPSRuntime }

  TPSRuntime = class(TComponent)
  private
    FExec: TPSExec;
    function GetExecErrorString: tbtstring;
  public
    property ExecErrorString: tbtstring read GetExecErrorString;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadFromFile(const AFilename: string): boolean;
    function LoadFromStream(s: TStream): boolean;
    function RunProgram: boolean;
    function ExecFunction(const Params: array of Variant; const ProcName: tbtstring): Variant;
    procedure RegisterDelphiFunction(ProcPtr: Pointer; const ProcName: tbtstring; CC: TPSCallingConvention);
    procedure RegisterDelphiMethod(Slf, ProcPtr: Pointer; const ProcName: tbtstring; CC: TPSCallingConvention);
  end;

implementation

{ TPSRuntime }

function TPSRuntime.GetExecErrorString: tbtstring;
begin
  Result:=TIFErrorToString(FExec.ExceptionCode, FExec.ExceptionString);
end;

constructor TPSRuntime.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExec:=TPSExec.Create;
end;

destructor TPSRuntime.Destroy;
begin
  FExec.Free;
  inherited Destroy;
end;

function TPSRuntime.LoadFromFile(const AFilename: string): boolean;
var
  bc: string;
  f: TFileStream;
begin
  Result:=False;
  f:=TFileStream.Create(AFilename, fmOpenRead);
  try
    SetLength(bc, f.Size);
    f.Read(bc[1], f.Size);
    if FExec.LoadData(bc) then
      Result:=True;
  finally
    f.Free;
  end;
end;

function TPSRuntime.LoadFromStream(s: TStream): boolean;
begin
  Result:=False;
  if FExec.LoadData(s.ReadAnsiString) then
    Result:=True;
end;

function TPSRuntime.RunProgram: boolean;
begin
  Result:=FExec.RunScript;
end;

function TPSRuntime.ExecFunction(const Params: array of Variant;
  const ProcName: tbtstring): Variant;
begin
  Result:=FExec.RunProcPN(Params, ProcName);
end;

procedure TPSRuntime.RegisterDelphiFunction(ProcPtr: Pointer;
  const ProcName: tbtstring; CC: TPSCallingConvention);
begin
  FExec.RegisterDelphiFunction(ProcPtr, ProcName, CC);
end;

procedure TPSRuntime.RegisterDelphiMethod(Slf, ProcPtr: Pointer;
  const ProcName: tbtstring; CC: TPSCallingConvention);
begin
  FExec.RegisterDelphiMethod(Slf, ProcPtr, ProcName, CC);
end;

end.

