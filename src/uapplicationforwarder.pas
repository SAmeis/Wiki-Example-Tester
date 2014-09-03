{ Application forwarder

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

