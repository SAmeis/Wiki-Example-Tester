unit uapplicationforwarder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umessagemanager, forms;

{ TApplicationForwarder - Forwards message to TApplication.QueueAsyncCall()
}
type
  { TApplicationForwarder }

  TApplicationForwarder = class(TQueuedThread)
  private
    fOnMessage: TDataEvent;
    fRegisteredMessages: TThreadList;
    function GetRegisteredMessageCount: Integer;
    function GetRegisteredMessages(index: Integer): TMessageClass;
  protected
    procedure ProcessMessage(aMessage: TMessage); override;
    property RegisteredMessagesList: TThreadList read fRegisteredMessages;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt =
       DefaultStackSize); override;
    destructor Destroy; override;
    procedure RegisterMessage(aMessageClass: TMessageClass);
    procedure UnregisterMessage(aMessageClass: TMessageClass);
    property OnMessage: TDataEvent read fOnMessage write fOnMessage;
    property RegisteredMessages[index: Integer]: TMessageClass read GetRegisteredMessages;
    property RegisteredMessageCount: Integer read GetRegisteredMessageCount;
  end;

implementation

 { TApplicationForwarder }

function TApplicationForwarder.GetRegisteredMessageCount: Integer;
var
  l: TList;
begin
  l := RegisteredMessagesList.LockList;
  Result := l.Count;
  RegisteredMessagesList.LockList;
end;

function TApplicationForwarder.GetRegisteredMessages(index: Integer
  ): TMessageClass;
var
  l: TList;
begin
  l := RegisteredMessagesList.LockList;
  try
    Result := TMessageClass(l.Items[index]);
  finally
    RegisteredMessagesList.UnlockList;
  end;
end;

procedure TApplicationForwarder.ProcessMessage(aMessage: TMessage);
var
  AsyncMethod: Forms.TDataEvent;
begin
  AsyncMethod := fOnMessage;
  if Assigned(AsyncMethod) then
    Application.QueueAsyncCall(AsyncMethod, {%H-}Ptrint(aMessage.Clone));
end;

constructor TApplicationForwarder.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(True, StackSize);
  fRegisteredMessages := TThreadList.Create;

  if CreateSuspended then Start;
end;

destructor TApplicationForwarder.Destroy;
begin
  fOnMessage := Nil;
  FreeAndNil(fRegisteredMessages);
  inherited Destroy;
end;

procedure TApplicationForwarder.RegisterMessage(aMessageClass: TMessageClass);
begin
  RegisteredMessagesList.Add(Pointer(aMessageClass));
  MessageManager.RegisterForMessage(aMessageClass, Self);
end;

procedure TApplicationForwarder.UnregisterMessage(aMessageClass: TMessageClass);
var
  l: TList;
begin
  MessageManager.UnregisterForMessage(aMessageClass);
  l := RegisteredMessagesList.LockList;
  try
    l.Remove(Pointer(aMessageClass));
  finally
    RegisteredMessagesList.UnlockList;
  end;
end;

end.

