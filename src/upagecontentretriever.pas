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

