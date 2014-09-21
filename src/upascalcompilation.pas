unit upascalcompilation;

{$mode objfpc}{$H+}{$CODEPAGE UTF8}

interface

uses
  Classes, SysUtils, process, FileUtil, LazUTF8Classes,
  umessagemanager, ucompileprepare;

type

  TCustomCompilationMessage = class(TTestPreparedMessage)
  private
    fOutput: String;
    fReturnCode: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Output: String read fOutput write fOutput;
    property ReturnCode: Integer read fReturnCode write fReturnCode;
  end;

  TPascalCompilationDoneMessage   = class(TCustomCompilationMessage);
  TPascalCompilationFailedMessage = class(TCustomCompilationMessage);


  { TLogSavedMessage }

  TLogSavedMessage = class(TCustomCompilationMessage)
  private
    fLogFileName: UnicodeString;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property LogFileName: UnicodeString read fLogFileName write fLogFileName;
  end;

  { TPascalCompilation }

  TPascalCompilation = class(TQueuedThread)
  protected
    procedure ProcessMessage(aMessage: TMessage); override;
  end;

  { TPascalLogSaver }

  TPascalLogSaver = class(TQueuedThread)
  protected
    procedure ProcessMessage(aMessage: TMessage); override;
  end;

var
  PascalCompiliationManager: TRoundRobinThreadPool;
  PascalLogSaver           : TPascalLogSaver      ;

implementation

{ TLogSavedMessage }

procedure TLogSavedMessage.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is Self.ClassType then
  begin
    TLogSavedMessage(Dest).fLogFileName := Self.fLogFileName;
  end;
end;

{ TPascalLogSaver }

procedure TPascalLogSaver.ProcessMessage(aMessage: TMessage);
var
  cd: TCustomCompilationMessage absolute aMessage;
  fs: TFileStreamUTF8;
  lsm: TLogSavedMessage;

begin
  Assert(aMessage is TCustomCompilationMessage, 'Invalid Message type');
  lsm := TLogSavedMessage.Create;
  lsm.Assign(aMessage);
  lsm.LogFileName := lsm.ProjectDirectory+'fpc.log';
  try
    fs := TFileStreamUTF8.Create(UTF8Encode(lsm.LogFileName), fmCreate);
    try
      if Length(lsm.Output) > 0 then
        fs.WriteBuffer(lsm.Output[1], Length(lsm.Output));
    finally
      fs.Destroy;
    end;
    DispatchMessage(lsm);
  except
    lsm.Destroy;
    raise;
  end;
end;

{ TCustomCompilationMessage }

procedure TCustomCompilationMessage.AssignTo(Dest: TPersistent);
var
  pcd: TCustomCompilationMessage absolute Dest;
begin
  inherited AssignTo(Dest);
  if Dest is TCustomCompilationMessage then
  begin
    pcd.fOutput     := Self.fOutput;
    pcd.fReturnCode := Self.fReturnCode;
  end;
end;

{ TPascalCompilation }

procedure TPascalCompilation.ProcessMessage(aMessage: TMessage);
var
  PM: TTestPreparedMessage absolute aMessage;
  FPC_Out: AnsiString;
  FPC_ExitStatus: Integer;
  rc: Integer;
  pcd: TCustomCompilationMessage;
begin
  Assert(aMessage is TTestPreparedMessage, 'Invalid message type.');
  FPC_Out := '';
  FPC_ExitStatus := 0;

  rc := RunCommandInDir(
    UTF8ToSys(UTF8Encode(PM.ProjectDirectory)),
    'C:\Lazarus\lazbuild.exe',
    ['-B', UTF8ToSys(UTF8Encode(PM.ProjectFile))],
    FPC_Out,
    FPC_ExitStatus
  );

  if FPC_ExitStatus = 0 then  // everything okay
    pcd := TPascalCompilationDoneMessage.Create
  else            // exception
    pcd := TPascalCompilationFailedMessage.Create;

  pcd.Assign(aMessage);
  pcd.Output := FPC_Out;
  pcd.ReturnCode := FPC_ExitStatus;

  DispatchMessage(pcd);
end;

initialization
  PascalCompiliationManager := TRoundRobinThreadPool.Create(True, DefaultStackSize);
  PascalCompiliationManager.FreeOnTerminate := True;
  PascalCompiliationManager.WorkerClass := TPascalCompilation;
  PascalCompiliationManager.PoolSize := 6;
  PascalCompiliationManager.Start;
  MessageManager.RegisterForMessage(TTestPreparedMessage, PascalCompiliationManager);

  PascalLogSaver := TPascalLogSaver.Create(False, DefaultStackSize);
  PascalLogSaver.FreeOnTerminate := True;
  MessageManager.RegisterForMessage(TPascalCompilationDoneMessage  , PascalLogSaver);
  MessageManager.RegisterForMessage(TPascalCompilationFailedMessage, PascalLogSaver);
finalization
  PascalCompiliationManager.Terminate;
  PascalLogSaver.Terminate;
end.

