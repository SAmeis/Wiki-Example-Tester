unit umessagemanager;

{$mode objfpc}{$H+}

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

type
  { TMessage }

  TMessage = class(TPersistent)
  public
    constructor Create; virtual;
    function Clone: TMessage; virtual;
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
  private
    fMessageClass: TMessageClass;
    fThread: TQueuedThread;
  public
    property MessageClass: TMessageClass read fMessageClass write fMessageClass;
    property Thread: TQueuedThread read fThread write fThread;
  end;

  { TQueuedThread }

  TQueuedThread = class(TThread, IFPObserver)
  private
    fDelayBetweenMessages: Integer;
    fMessageQueue: TThreadList;
    fWakeUpEvent: PRTLEvent;
    fWakeUpTimeout: Integer;
    Procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; {%H-}Data : Pointer);
  protected
    procedure FreeAndNilThreadListWithObjects(var aTL: TThreadList);
    procedure ProcessMessage(aMessage: TMessage); virtual; abstract;
    procedure Execute; override;
    procedure DispatchMessage(aMessage: TMessage); virtual;
    property  DelayBetweenMessages: Integer read fDelayBetweenMessages write fDelayBetweenMessages;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt =
       DefaultStackSize); reintroduce; virtual;
    destructor Destroy; override;
    procedure AddMessage(aMessage: TMessage); virtual;  // may restrict some message classes
    procedure Terminate; reintroduce;
    property WakeUpTimeout: Integer read fWakeUpTimeout write fWakeUpTimeout;
  end;

  { TMessageManager }

  TMessageManager = class(TQueuedThread)
  private
    fHandler: TThreadList;
    fMainThreadForwarder: TQueuedThread;
  protected
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

var
  MessageManager: TMessageManager;

implementation

{ EMessageNotSupported }

constructor EMessageNotSupported.Create(aMessage: TMessage;
  aThread: TQueuedThread);
begin
  inherited CreateFmt(
    'Message class %s is not supported by thread %d of class %s.',
    [aMessage.ClassName, aThread.ThreadID, aThread.ClassName]);
end;

{ TMessage }

constructor TMessage.Create;
begin
  inherited Create;
end;

function TMessage.Clone: TMessage;
begin
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
      if aMessage.ClassType = handler.MessageClass then
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
begin
  repeat
    message := GetNextMessage(QueuedCount);

    if Assigned(message) then
    try
      ProcessMessage(message);
    finally
      message.Destroy;
      message := Nil;
    end;

    if not Terminated then
    begin
      if QueuedCount = 0 then
        RTLeventWaitFor(fWakeUpEvent, fWakeUpTimeout)
      else if (fDelayBetweenMessages <> 0) then
        RTLeventWaitFor(fWakeUpEvent, fDelayBetweenMessages);
    end;
  until Terminated;
end;

procedure TQueuedThread.DispatchMessage(aMessage: TMessage);
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
        TObject(l.Items[i]).Destroy;
    finally
      tl.UnlockList;
    end;
    tl.Destroy;
  end;
end;

procedure TQueuedThread.Terminate;
begin
  inherited Terminate;
  RTLeventSetEvent(fWakeUpEvent);
end;

initialization
  MessageManager := TMessageManager.Create(False, DefaultStackSize);
  MessageManager.FreeOnTerminate := True;
finalization
  MessageManager.Terminate;
end.

