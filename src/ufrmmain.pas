unit ufrmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, umessagemanager, uapplicationforwarder, upagecontentretriever,
  upagecontentparser;

type
  { TPageContent }

  TPageContent = class
  private
    fCode: TStrings;
    fContent: TStrings;
    fLanguage: String;
    fPackages: TStrings;
    fOperatingSystem: String;
    fPageName: String;
    fPreset: String;
  public
    const DEFAULT_LANGUAGE = 'delphi';
  public
    constructor Create;
    destructor Destroy; override;
    class function CreateFromPageContent(const aPageName: String; aContent: TStrings): TPageContent;
    property PageName: String read fPageName write fPageName;
    property Content: TStrings read fContent;
    property Code: TStrings read fCode;
    property OperatingSystem: String read fOperatingSystem write fOperatingSystem;
    property Packages: TStrings read fPackages;
    property Preset: String read fPreset write fPreset;
    property Language: String read fLanguage write fLanguage;
  end;

  { TFrmMain }

  TFrmMain = class(TForm)
    BtnGetCode: TButton;
    BtnDumpContent: TButton;
    EURL: TEdit;
    Label1: TLabel;
    MeSiteContent: TMemo;
    MeContentDump: TMemo;
    MePages: TMemo;
    procedure BtnDumpContentClick(Sender: TObject);
    procedure BtnGetCodeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fApplicationForwarder: TApplicationForwarder;
    procedure ProcessMessageFromThread(Data: PtrInt);
    procedure OnContentRetrieved(aMessage: TMessage);
    procedure OnContentParsed(aMessage: TMessage);
    procedure OnContentParseError(aMessage: TMessage);
    procedure GetPageContentAsync(const aPage: String);
  protected
    property ApplicationForwarder: TApplicationForwarder read fApplicationForwarder;
  end;

var
  FrmMain: TFrmMain;

implementation

uses XMLRead, dom;
{$R *.lfm}

{ TPageContent }

constructor TPageContent.Create;
begin
  inherited Create;
  fCode     := TStringList.Create;
  fContent  := TStringList.Create;
  fPackages := TStringList.Create;
  fPackages.Delimiter := ',';
  fLanguage := DEFAULT_LANGUAGE;
end;

destructor TPageContent.Destroy;
begin
  fCode.Free;
  fContent.Free;
  fPackages.Free;
  inherited Destroy;
end;

class function TPageContent.CreateFromPageContent(const aPageName: String;
  aContent: TStrings): TPageContent;
  function GetTextWithoutComments(aNode: TDOMNode): String;
  var
    i: Integer;
    cn: TDOMNode;
  begin
    Result := '';
    for i := 0 to aNode.ChildNodes.Count - 1 do
    begin
      cn := aNode.ChildNodes[i];
      if cn.NodeType = TEXT_NODE then
        Result += UTF8Encode(cn.TextContent)
      else if cn.NodeType = ELEMENT_NODE then
        Result += GetTextWithoutComments(cn);
      if i < aNode.ChildNodes.Count - 1 then
        Result += LineEnding;
    end;
  end;

  procedure ParseXMLDoc(aDoc: TXMLDocument);
  var
    compClasses: TStringList;
    codeTag: TDOMNode;
    i: Integer;
    n: String;
  begin
    compClasses := TStringList.Create;
    compClasses.Delimiter := ' ';
    compClasses.DelimitedText := UTF8String(aDoc.DocumentElement.AttribStrings['class']);
    if compClasses.IndexOf('compileable') = -1 then
      raise ENotTestable.Create('Page has not compileable attribute.');
    compClasses.NameValueSeparator := '-';
    for i := 0 to compClasses.Count - 1 do
    begin
      n := compClasses.Names[i];
      if n = 'os' then
        Result.OperatingSystem := compClasses.ValueFromIndex[i]
      else if n = 'preset' then
        Result.Preset := compClasses.ValueFromIndex[i]
      else if n = 'packages' then
        Result.Packages.DelimitedText := compClasses.ValueFromIndex[i];
    end;
    codeTag := aDoc.DocumentElement.FindNode('syntaxhighlight');
    if not Assigned(codeTag) then
      codeTag := aDoc.DocumentElement.FindNode('source');
    if Assigned(codeTag) then
      if codeTag.HasChildNodes then
        Result.Code.Text := GetTextWithoutComments(codeTag);
  end;
const
  XML_HEAD='<?xml version="1.0"?>';
var
  ms: TMemoryStream;
  doc: TXMLDocument;
begin
  Assert(aPageName <> '', 'Page title not set');

  if Length(aContent.Text) < 1 then
    raise ENoPageContent.Create('Page content is empty.');

  Result := TPageContent.Create;
  try
    Result.PageName := aPageName;
    Result.Content.Assign(aContent);
    ms := TMemoryStream.Create;
    try
      ms.WriteBuffer(XML_HEAD[1], Length(XML_HEAD));
      ms.WriteBuffer(LineEnding[1], Length(LineEnding));
      ms.WriteBuffer(aContent.Text[1], Length(aContent.Text) );
      ms.Position := 0;
      ReadXMLFile(doc, ms);
      ParseXMLDoc(doc);
    finally
      ms.Destroy;
      doc.Destroy;
    end;
  except
    on e: TObject do
    begin
      Result.Destroy;
      Result := nil;
      raise e;
    end;
  end;
end;

{ TFrmMain }

procedure TFrmMain.BtnGetCodeClick(Sender: TObject);
begin
  if MePages.Lines.Count > 1 then
    if MessageDlg('Mehrere Seiten', 'Es sind mehrere Seiten angegeben; es wird im Test jedoch nur die erste Seite verwendet.', mtWarning, mbOKCancel, 0) = mrCancel then
      exit;
  if MePages.Lines.Count = 0 then
  begin
    MessageDlg('Keine Seiten angegeben', 'Es sind keine Seiten angegeben.', mtError, [mbOK],0);
    exit;
  end;
  MeSiteContent.Lines.Clear;
  GetPageContentAsync(MePages.Lines[0]);
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  fApplicationForwarder := TApplicationForwarder.Create(False, DefaultStackSize);
  fApplicationForwarder.FreeOnTerminate := True;
  fApplicationForwarder.RegisterMessage(TPageContentRetrievedMessage);
  fApplicationForwarder.RegisterMessage(TPageContentParsedMessage);
  fApplicationForwarder.RegisterMessage(TPageContentParseErrorMessage);
  fApplicationForwarder.OnMessage := @ProcessMessageFromThread;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  fApplicationForwarder.Terminate;
end;

procedure TFrmMain.ProcessMessageFromThread(Data: PtrInt);
begin
  Assert(Data <> 0, 'Invalid Data value');
  if TMessage(Data).ClassType = TPageContentRetrievedMessage then
    OnContentRetrieved(TMessage(Data))
  else if TMessage(Data).ClassType = TPageContentParsedMessage then
    OnContentParsed(TMessage(Data))
  else if TMessage(Data).ClassType = TPageContentParseErrorMessage then
    OnContentParseError(TMessage(Data));

  TMessage(Data).Destroy;
end;

procedure TFrmMain.OnContentRetrieved(aMessage: TMessage);
var
  pcr: TPageContentRetrievedMessage absolute aMessage;
begin
  Assert(aMessage.ClassType = TPageContentRetrievedMessage, 'Invalid message type');
  EURL.Caption := pcr.PageName + ' - ' + pcr.URL;
  MeSiteContent.Lines.Assign(pcr.Content);
end;

procedure TFrmMain.OnContentParsed(aMessage: TMessage);
var
  pc: upagecontentparser.TPageContent absolute aMessage;
  package: String;
begin
  Assert(aMessage is TPageContentParsedMessage, 'Invalid message type.');
  MeContentDump.Lines.Clear;
  MeContentDump.Lines.Add('OS='+pc.OperatingSystem);
  MeContentDump.Lines.Add('Preset='+pc.Preset);
  for package in pc.Packages do
    MeContentDump.Lines.Add('Package='+package);
  MeContentDump.Lines.Add('CodeLanguage='+pc.Language);
  MeContentDump.Lines.Add('DefaultLanguage='+Booltostr(pc.DefaultLanguage,True));
  MeContentDump.Lines.Add('===CODE===');
  MeContentDump.Lines.AddStrings(pc.Code);
end;

procedure TFrmMain.OnContentParseError(aMessage: TMessage);
var
  pe: TPageContentParseErrorMessage absolute aMessage;
begin
  Assert(aMessage is TPageContentParseErrorMessage, 'Invalid message type');
  MeContentDump.Lines.Clear;
  if Assigned(pe.ErrorException) then
  begin
    MeContentDump.Lines.Add(pe.ErrorException.ClassName);
    MeContentDump.Lines.Add(pe.ErrorException.Message);
  end;
  MeContentDump.Lines.Add(pe.PageName);
  MeContentDump.Lines.Add('===Content===');
  MeContentDump.Lines.AddStrings(pe.Content);
end;

procedure TFrmMain.BtnDumpContentClick(Sender: TObject);
var
  pc: TPageContent;
  package: String;
begin
  if MeSiteContent.Lines.Count = 0 then exit;
  pc := TPageContent.CreateFromPageContent(MePages.Lines[0], MeSiteContent.Lines);
  MeContentDump.Lines.Clear;
  MeContentDump.Lines.Add('OS='+pc.OperatingSystem);
  MeContentDump.Lines.Add('Preset='+pc.Preset);
  for package in pc.Packages do
    MeContentDump.Lines.Add('Package='+package);
  MeContentDump.Lines.Add('CodeLanguage='+pc.Language);
  MeContentDump.Lines.Add('===CODE===');
  MeContentDump.Lines.AddStrings(pc.Code);
  pc.Free;
end;

procedure TFrmMain.GetPageContentAsync(const aPage: String);
var
  m: TGetPageContentMessage;
begin
  m := TGetPageContentMessage.Create;
  m.PageName := aPage;
  MessageManager.AddMessage(m);
end;

end.

