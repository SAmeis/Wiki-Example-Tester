{ Wiki Page Content Retriever

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
unit upagecontentretriever;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umessagemanager;

{ TGetPageContentMessage       - Request page content
  TPageContentRetrievedMessage - Page content fetched from Wiki
  TPageContentRetriever        - Thread fetching content
}

type
  { TGetPageContentMessage }

  TGetPageContentMessage = class(TMessage)
  private
    fPageName: String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property PageName: String read fPageName write fPageName;
  end;

  { TPageContentRetrievedMessage }

  TPageContentRetrievedMessage = class(TGetPageContentMessage)
  private
    fContent: TStrings;
    fURL: String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Content: TStrings read fContent;
    property URL: String read fURL write fURL;
  end;

  { TPageContentRetriever }

  TPageContentRetriever = class(TQueuedThread)
  protected
    procedure ProcessMessage(aMessage: TMessage); override;
  public
    procedure AddMessage(aMessage: TMessage); override;
    property DelayBetweenMessages;
  end;

var
  PageContentRetriever: TPageContentRetriever;

implementation

uses fphttpclient;

{ TGetPageContentMessage }

procedure TGetPageContentMessage.AssignTo(Dest: TPersistent);
begin
  if Dest is TGetPageContentMessage then
    TGetPageContentMessage(Dest).PageName := Self.PageName
  else
    inherited AssignTo(Dest);
end;

{ TPageContentRetriever }

procedure TPageContentRetriever.ProcessMessage(aMessage: TMessage);
const
  BASE_URL='http://wiki.freepascal.org/';
  INDEX_PARAMS='index.php?title=%s&action=raw';
var
  m: TGetPageContentMessage absolute aMessage;
  url: String;
  encodedName: String;
  RetrievedMessage: TPageContentRetrievedMessage;
begin
  Assert(aMessage is TGetPageContentMessage, 'Invlaid message class');

  url := BASE_URL + INDEX_PARAMS;
  encodedName := EncodeURLElement(m.PageName);
  url := Format(url, [encodedName]);
  try
    RetrievedMessage := TPageContentRetrievedMessage.Create;
    RetrievedMessage.PageName := m.PageName;
    RetrievedMessage.URL := url;
    TFPHTTPClient.SimpleGet(url, RetrievedMessage.Content);
  except
    on e: Exception do
    begin
      RetrievedMessage.Destroy;
      raise e;
    end;
  end;
  MessageManager.AddMessage(RetrievedMessage);
end;

procedure TPageContentRetriever.AddMessage(aMessage: TMessage);
begin
  if not (aMessage is TGetPageContentMessage) then
    raise EMessageNotSupported.Create(aMessage, Self);

  inherited AddMessage(aMessage);
end;

{ TPageContentRetrievedMessage }

procedure TPageContentRetrievedMessage.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TPageContentRetrievedMessage then
  begin
    TPageContentRetrievedMessage(Dest).URL := Self.URL;
    TPageContentRetrievedMessage(Dest).Content.Assign(Self.Content);
  end;
end;

constructor TPageContentRetrievedMessage.Create;
begin
  inherited Create;
  fContent := TStringList.Create;
end;

destructor TPageContentRetrievedMessage.Destroy;
begin
  fContent.Free;
  inherited Destroy;
end;

initialization
  PageContentRetriever := TPageContentRetriever.Create(False, DefaultStackSize);
  PageContentRetriever.FreeOnTerminate := True;
  PageContentRetriever.DelayBetweenMessages := 500;
  MessageManager.RegisterForMessage(TGetPageContentMessage, PageContentRetriever);
finalization
  PageContentRetriever.Terminate;
end.

