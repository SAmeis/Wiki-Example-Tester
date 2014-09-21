{ Message Manager

  Copyright (C) 2014 Simon Ameis <simon.ameis@web.de>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:
  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.
  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.
  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit umessagemanager;

{$mode objfpc}{$H+}{$Interfaces CORBA}

interface

uses
  Classes, SysUtils;

{ TMessage              - Class type indicates what to do
                          Must not be modified after dispatching (may be passed
                          to other threads without copying in the future)
                          Dispatched to a thread wich registered itself to this
                          class type
                          Lifecycle:
                          - Created and forwarded to handling thread
                          - Destroyed by handling thread after processing
                          - Each thread gets it's own copy of this message
                            (TMessage.Copy)
                          - Each thread destroys message after processing
                            (see above)
  TThreadMessageHandler - Defines which thread will be notified for which
                          message class
  TQueuedThread         - Worker thread base class
  TMessageManager       - Dispatcher Thread

  EMessage              - Base exception class for message related exceptions
  EMessageNotSupported  - Message type is not supported by this thread
}

const
  IID_EXCEPTIONMESSAGE = '{65ABAC45-7AF2-4C87-B6F6-CC80BEBB2F49}';
  IID_HASEXCEPTIONMESSAGE = '{045DA6D7-5694-44EC-85D5-16EF4BCD32BB}';
type

  EShutDown = class(EThread);
  ETerminated = class(EThread);

  { TMessage }

  TMessage = class(TPersistent)
  strict private
    fUUID: TGuid;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    function Clone: TMessage; virtual;
    property UUID: TGuid read fUUID;  // identifys a message an all following messages
  end;

  { TExceptionMessage }

  TExceptionMessage = class(TMessage)
  strict private
    fExceptionClass: TClass;
    fExceptionMessage: String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property ExceptionClass: TClass read fExceptionClass write fExceptionClass;
    property ExceptionMessage: String read fExceptionMessage write fExceptionMessage;
  end;

  { IHasExceptionMessage }

  IHasExceptionMessage = interface [IID_HASEXCEPTIONMESSAGE]
  function GetExceptionMessage: TExceptionMessage;
    property ExceptionMessage: TExceptionMessage read GetExceptionMessage;
  end;

  { IExceptionMessage }

  IExceptionMessage = interface [IID_EXCEPTIONMESSAGE]
    function GetExceptionClass: TClass;
    function GetExceptionMessage: String;
    procedure SetExceptionClass(aValue: TClass);
    procedure SetExceptionMessage(aValue: String);
    property ExceptionClass: TClass read GetExceptionClass write SetExceptionClass;
    property ExceptionMessage: String read GetExceptionMessage write SetExceptionMessage;
  end;

  TMessageClass = class of TMessage;
  TMessageEvent = procedure(aMessage: TMessage) of object;

  TQueuedThread = class;

  EMessage = class(Exception);
  EMessageNotSupported = class(EMessage)
    constructor Create(aMessage: TMessage; aThread: TQueuedThread);
  end;

  { TThreadMessageHandler }

  TThreadMessageHandler = class(TObject)
  strict private
    fMessageClass: TMessageClass;
    fThread: TQueuedThread;
  public
    property MessageClass: TMessageClass read fMessageClass write fMessageClass;
    property Thread: TQueuedThread read fThread write fThread;
  end;

  { TQueuedThread }

  TQueuedThread = class(TThread, IFPObserver)
  strict private
    fShutdown: Boolean;
    fDelayBetweenMessages: Integer;
    fMessageQueue: TThreadList;
    fWakeUpEvent: PRTLEvent;
    fWakeUpTimeout: Integer;
    Procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; {%H-}Data : Pointer);
  strict protected
    procedure FreeAndNilThreadListWithObjects(var aTL: TThreadList);
    procedure ProcessMessage(aMessage: TMessage; var FreeMessage: Boolean); virtual;
    procedure ProcessMessage({%H-}aMessage: TMessage); virtual;
    class procedure DispatchMessage(aMessage: TMessage); static; virtual;
    property  DelayBetweenMessages: Integer read fDelayBetweenMessages write fDelayBetweenMessages;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt =
       DefaultStackSize); reintroduce; virtual;
    destructor Destroy; override;
    procedure AddMessage(aMessage: TMessage); virtual;  // may restrict some message classes
    procedure Terminate; reintroduce; virtual;
    procedure Shutdown;  // process all queued messages and terminate
    property WakeUpTimeout: Integer read fWakeUpTimeout write fWakeUpTimeout;
  end;

  TQueuedThreadClass = class of TQueuedThread;

  { TMessageManager }

  TMessageManager = class(TQueuedThread)
  strict private
    fHandler: TThreadList;
    fMainThreadForwarder: TQueuedThread;
  strict protected
    procedure ProcessMessage(aMessage: TMessage); override;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt =
      DefaultStackSize); override;
    destructor Destroy; override;
    procedure RegisterForMessage(aMessageClass: TMessageClass);
    procedure RegisterForMessage(aMessageClass: TMessageClass; aThread: TQueuedThread);
    procedure UnregisterForMessage(aMessageClass: TMessageClass);
    procedure UnregisterForMessage(aMessageClass: TMessageClass; aThread: TQueuedThread);
    property MainThreadForwarder: TQueuedThread read fMainThreadForwarder write fMainThreadForwarder;
  end;

  { TAbstractThreadPool }

  TAbstractThreadPool = class(TQueuedThread)
  public
    type TOnThreadCreated = procedure(aThread: TQueuedThread) of object; // called after workerthread.create in this thread
  strict private
    type TThreadPool = array of TQueuedThread;
  strict private
    fFadeOut    : Boolean;
    fOnThreadCreated: TOnThreadCreated;
    fWorkerClass: TQueuedThreadClass;
    fThreadPool : TThreadPool;
    function GetPoolSize: SizeInt;
    function GetWorkerClass: TQueuedThreadClass;
    procedure SetPoolSize(aValue: SizeInt);
    procedure SetWorkerClass(aValue: TQueuedThreadClass);
  strict protected
    fCSThreadPools: TRTLCriticalSection;
    property ThreadPool   : TThreadPool read fThreadPool;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt =
       DefaultStackSize); override;
    destructor Destroy; override;
    property PoolSize: SizeInt read GetPoolSize write SetPoolSize;
    property WorkerClass: TQueuedThreadClass read GetWorkerClass write SetWorkerClass;
    property FadeOut: Boolean read fFadeOut write fFadeOut default False;  // true => workers finish current queue before termination
    property OnThreadCreated: TOnThreadCreated read fOnThreadCreated write fOnThreadCreated;
  end;

  { TRoundRobinThreadPool }

  TRoundRobinThreadPool = class(TAbstractThreadPool)
  strict private
    fNextWorkerIndex: SizeInt;
  strict protected
    procedure ProcessMessage(aMessage: TMessage; var FreeMessage: Boolean); override;
  end;

var
  MessageManager: TMessageManager;

implementation

{ TRoundRobinThreadPool }

procedure TRoundRobinThreadPool.ProcessMessage(aMessage: TMessage;
  var FreeMessage: Boolean);
var
  w: TQueuedThread;
begin
  EnterCriticalsection(fCSThreadPools);
  try
   Assert(Length(ThreadPool) > 0, 'Thread pool not populated');

   if fNextWorkerIndex > high(ThreadPool) then
     fNextWorkerIndex := 0;
   w := ThreadPool[fNextWorkerIndex];

   w.AddMessage(aMessage);
   FreeMessage := False;

   inc(fNextWorkerIndex);
  finally
    LeaveCriticalsection(fCSThreadPools);
  end;
end;

{ TAbstractThreadPool }

function TAbstractThreadPool.GetPoolSize: SizeInt;
begin
  EnterCriticalsection(fCSThreadPools);
  Result := Length(fThreadPool);
  LeaveCriticalsection(fCSThreadPools);
end;

function TAbstractThreadPool.GetWorkerClass: TQueuedThreadClass;
begin
  EnterCriticalsection(fCSThreadPools);
  Result := fWorkerClass;
  LeaveCriticalsection(fCSThreadPools);
end;

procedure TAbstractThreadPool.SetPoolSize(aValue: SizeInt);
var
  OldLength: Integer;
  c: TQueuedThreadClass;
  i: SizeInt;
  o: TOnThreadCreated;
  f: Boolean;
begin
  if aValue < 0 then
    aValue := 0;

  EnterCriticalsection(fCSThreadPools);
  try
    OldLength := Length(fThreadPool);
    if OldLength = aValue then
      exit
    else if aValue > OldLength then  // grow
    begin
      SetLength(fThreadPool, aValue);
      // get values, so they may not change while using them
      c := fWorkerClass;
      o := fOnThreadCreated;
      f := FadeOut;

      for i := OldLength to High(fThreadPool) do
      begin
        if Assigned(c) then
        begin
          fThreadPool[i] := c.Create(True, DefaultStackSize);
          fThreadPool[i].FreeOnTerminate := True;
          if Assigned(o) then
            o(fThreadPool[i]);
          fThreadPool[i].Start;
        end;
      end;
    end else  // shrink
    begin
      for i := high(fThreadPool) downto aValue do
      begin
        if f then
          fThreadPool[i].Shutdown
        else
          fThreadPool[i].Terminate;
      end;
      SetLength(fThreadPool, aValue);
    end;

  finally
    LeaveCriticalsection(fCSThreadPools);
  end;
end;

procedure TAbstractThreadPool.SetWorkerClass(aValue: TQueuedThreadClass);
var
  OldPoolSize: SizeInt;
begin
  EnterCriticalsection(fCSThreadPools);
  try
    if fWorkerClass = aValue then Exit;
    OldPoolSize := PoolSize;
    // remove all threads
    SetPoolSize(0);
    fWorkerClass := aValue;
    // recreate with new threads
    SetPoolSize(OldPoolSize);
  finally
    LeaveCriticalsection(fCSThreadPools);
  end;
end;

constructor TAbstractThreadPool.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(True, StackSize);
  InitCriticalSection(fCSThreadPools);

  if CreateSuspended then
    Self.Start;
end;

destructor TAbstractThreadPool.Destroy;
begin
  SetPoolSize(0);
  DoneCriticalsection(fCSThreadPools);
  inherited Destroy;
end;

{ TExceptionMessage }

procedure TExceptionMessage.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TExceptionMessage then
  begin
    TExceptionMessage(Dest).fExceptionClass := Self.fExceptionClass;
    TExceptionMessage(Dest).fExceptionMessage := Self.fExceptionMessage;
  end;
end;

{ EMessageNotSupported }

constructor EMessageNotSupported.Create(aMessage: TMessage;
  aThread: TQueuedThread);
begin
  inherited CreateFmt(
    'Message class %s is not supported by thread %d of class %s.',
    [aMessage.ClassName, aThread.ThreadID, aThread.ClassName]);
end;

{ TMessage }

procedure TMessage.AssignTo(Dest: TPersistent);
begin
  if Dest is TMessage then
    TMessage(Dest).fUUID := fUUID
  else
    inherited AssignTo(Dest);
end;

constructor TMessage.Create;
begin
  inherited Create;
  if CreateGUID(fUUID) <> 0 then
  begin
    PInt64(@fUUID)[0] := Random(High(Int64));
    PInt64(@fUUID)[1] := Random(High(Int64));
  end;
end;

function TMessage.Clone: TMessage;
begin
  if not Assigned(Self) then Exit(Nil);

  Result := TMessageClass(Self.ClassType).Create;
  Result.Assign(Self);
end;

{ TMessageManager }

procedure TMessageManager.ProcessMessage(aMessage: TMessage);
var
  l: TList;
  i: Integer;
  handler: TThreadMessageHandler;
  CopiedMessage: TMessage;
begin
  l := fHandler.LockList;
  try
    i := l.Count - 1;
    while i >= 0 do
    begin
      handler := TThreadMessageHandler(l.Items[i]);
      // handler.MessageClass => all messages accepted; then AddMessage() may not raise any exception
      if (aMessage.ClassType = handler.MessageClass) or not Assigned(handler.MessageClass) then
        try
          if not Assigned(handler.Thread) then
          begin
            if Assigned(fMainThreadForwarder) then
            begin
              CopiedMessage := aMessage.Clone;
              fMainThreadForwarder.AddMessage(CopiedMessage);
            end;
            // ignore message if no forwarder for main thread defined
          end else
          begin
            CopiedMessage := aMessage.Clone;
            handler.Thread.AddMessage(CopiedMessage);
          end;
        except
          // message is not supported by this thread (invalid handler)
          on EMessageNotSupported do
            l.Delete(i);
        end;

      dec(i);
    end;
  finally
    fHandler.UnlockList;
  end;
end;

constructor TMessageManager.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(True, StackSize);
  fHandler := TThreadList.Create;

  if not CreateSuspended then Start;
end;

destructor TMessageManager.Destroy;
begin
  FreeAndNilThreadListWithObjects(fHandler);
  inherited Destroy;
end;

procedure TMessageManager.RegisterForMessage(aMessageClass: TMessageClass);
var
  aThread: TQueuedThread;
begin
  if MainThreadID <> GetCurrentThreadId then
    aThread := TQueuedThread(TThread.CurrentThread)
  else if Assigned(fMainThreadForwarder) then
    aThread := fMainThreadForwarder
  else
    raise Exception.CreateFmt('No instance of %s for main thread provided in MainThreadForwarder property. ', [TQueuedThread.ClassName]);
  RegisterForMessage(aMessageClass, aThread);
end;

procedure TMessageManager.RegisterForMessage(aMessageClass: TMessageClass;
  aThread: TQueuedThread);
var
  tm: TThreadMessageHandler;
begin
  tm := TThreadMessageHandler.Create;
  tm.MessageClass := aMessageClass;
  tm.Thread := aThread;
  fHandler.Add(tm);
end;

procedure TMessageManager.UnregisterForMessage(aMessageClass: TMessageClass);
var
  callingThread: TQueuedThread;
begin
  if MainThreadID <> GetCurrentThreadId then
    callingThread := TQueuedThread(TThread.CurrentThread)
  else
    callingThread := nil;

  UnregisterForMessage(aMessageClass, callingThread);
end;

procedure TMessageManager.UnregisterForMessage(aMessageClass: TMessageClass;
  aThread: TQueuedThread);
var
  l: TList;
  handler: TThreadMessageHandler;
  i: Integer;
begin
  l := fHandler.LockList;

  handler := Nil;
  i := l.Count - 1;
  while i > 0 do
  begin
    handler := TThreadMessageHandler(l.Items[i]);
    if  (handler.Thread = aThread)
    and (handler.MessageClass = aMessageClass) then
    begin
      l.Delete(i);
      FreeAndNil(handler);
    end;

    dec(i);
  end;

  fHandler.UnlockList;
end;

{ TQueuedThread }

procedure TQueuedThread.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if (ASender = fMessageQueue) and (Operation = ooChange) then
  begin
    RTLeventSetEvent(fWakeUpEvent);
  end;
end;

procedure TQueuedThread.Execute;
  function GetNextMessage(out aQueuedCount: Integer): TMessage;
  var
    l: TList;
  begin
    l := fMessageQueue.LockList;
    try
      if l.Count >= 1 then
      begin
        Result := TMessage(l.Items[0]);
        l.Delete(0);
        aQueuedCount := l.Count;
      end else
      begin
        Result := Nil;
        aQueuedCount := 0;
      end;
    finally
      fMessageQueue.UnlockList;
    end;
  end;

var
  message: TMessage;
  QueuedCount: Integer;
  vFreeMessage: Boolean;
begin
  repeat
    message := GetNextMessage(QueuedCount);

    if Assigned(message) then
    try
      vFreeMessage := True;
      ProcessMessage(message, vFreeMessage);
    finally
      if vFreeMessage then
      begin
        message.Destroy;
        message := Nil;
      end;
    end;

    if not Terminated then
    begin
      if QueuedCount = 0 then
        if fShutdown then
          Terminate
        else
          RTLeventWaitFor(fWakeUpEvent, fWakeUpTimeout)
      else if (fDelayBetweenMessages <> 0) then
        RTLeventWaitFor(fWakeUpEvent, fDelayBetweenMessages);
    end;
  until Terminated;
end;

class procedure TQueuedThread.DispatchMessage(aMessage: TMessage);
begin
  Assert(Assigned(aMessage), 'Message is nil');
  MessageManager.AddMessage(aMessage);
end;

constructor TQueuedThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
var
  l: TList;
begin
  inherited Create(False, StackSize);
  fMessageQueue := TThreadList.Create;
  l := fMessageQueue.LockList;
  l.FPOAttachObserver(Self);
  fMessageQueue.UnlockList;

  fWakeUpEvent := RTLEventCreate;
  fWakeUpTimeout := 100;
  if not CreateSuspended then Start;
end;

destructor TQueuedThread.Destroy;
begin
  FreeAndNilThreadListWithObjects(fMessageQueue);
  inherited Destroy;
end;

procedure TQueuedThread.AddMessage(aMessage: TMessage);
begin
  if fShutdown then
    raise EShutDown.CreateFmt('Thread %d already shut down.', [Self.ThreadID]);
  if Terminated then
    raise ETerminated.CreateFmt('Thread %d already terminated.', [Self.ThreadID]);

  Assert(Assigned(aMessage), 'Message is nil');
  fMessageQueue.Add(aMessage);
end;

procedure TQueuedThread.FreeAndNilThreadListWithObjects(var aTL: TThreadList);
var
  l: TList;
  i: Integer;
  tl: TThreadList;
begin
  if Assigned(aTL) then
  begin
    tl := aTL;
    aTL := nil;

    l := tl.LockList;
    try
      for i := 0 to l.Count - 1 do
        TObject(l.Items[i]).Free;
    finally
      tl.UnlockList;
    end;
    tl.Destroy;
  end;
end;

procedure TQueuedThread.ProcessMessage(aMessage: TMessage;
  var FreeMessage: Boolean);
begin
  ProcessMessage(aMessage);
  FreeMessage := True;
end;

procedure TQueuedThread.ProcessMessage(aMessage: TMessage);
begin
  // dummy
end;

procedure TQueuedThread.Terminate;
begin
  inherited Terminate;
  RTLeventSetEvent(fWakeUpEvent);
end;

procedure TQueuedThread.Shutdown;
begin
  fShutdown := True;
  RTLeventSetEvent(fWakeUpEvent);
end;

initialization
  MessageManager := TMessageManager.Create(False, DefaultStackSize);
  MessageManager.FreeOnTerminate := True;
finalization
  MessageManager.Terminate;
end.

