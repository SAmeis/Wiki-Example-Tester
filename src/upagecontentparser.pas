{ Wiki Page Content Parser

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
unit upagecontentparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umessagemanager, upagecontentretriever, RegExpr;
{ TPageContentParsedMessage - Page content sucessfully parsed
  TPageContentEmptyMessage  - Page content is empty, cannot be parsed
  TPageContent              - Alias for TPageContentParsedMessage if used
                              outside of message handling
  TPageContentParser        - parses page content
}
type

  EPageContent = class(Exception);
  EPageContentClass = class of EPageContent;
  ENoPageContent = class(EPageContent);  // empty page
  ENotTestable = class(EPageContent); // Testable_Begin template missing
  ENoCode = class(EPageContent);     // no <syntaxhighlight> tag or no closing tag
  EMultipleCode = class(EPageContent); // more than one <syntaxhighlight>
  EEmptyCode = class(EPageContent);  // <syntaxhighlight> is present but no code (except whitespace) in this tag


  TPageContentParsedMessage = class(TPageContentRetrievedMessage)
  private
    fCode: TStrings;
    fDefaultLanguage: Boolean;
    fLanguage: String;
    fOperatingSystem: String;
    fPackages: TStrings;
    fPreset: String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    const DEFAULT_LANGUAGE = 'delphi';
  public
    constructor Create; override;
    destructor Destroy; override;
    property Code: TStrings read fCode;
    property SourceCode: TStrings read fCode; // alias for Code
    property OperatingSystem: String read fOperatingSystem write fOperatingSystem;
    property Packages: TStrings read fPackages;
    property Preset: String read fPreset write fPreset;
    property Language: String read fLanguage write fLanguage;
    property DefaultLanguage: Boolean read fDefaultLanguage write fDefaultLanguage;
  end;
  TPageContent = TPageContentParsedMessage;

  { TPageContentParseErrorMessage }

  TPageContentParseErrorMessage = class(TPageContentRetrievedMessage)
  private
    fErrorException: EPageContent;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(aException: EPageContent); overload;
    destructor Destroy; override;
    property ErrorException: EPageContent read fErrorException write fErrorException;
  end;

  TPageContentEmptyMessage = class(TPageContentParseErrorMessage);

  { TPageContentParser }

  TPageContentParser = class(TQueuedThread)
  private
    fRETestable    : TRegExpr;
    fRECode        : TRegExpr;
    fRECodeLanguage: TRegExpr;
  protected const
    TESTABLE_TEMPLATE = 'User:Socke/Testable_Begin';
    REGEX_TESTABLE    = '\{{2}'+TESTABLE_TEMPLATE+'[ \|.]*'
                       +'(os=([^ \|\}]*))?[ \|.]*'
                       +'(preset=([^ \|\}]*))?[ \|.]*'
                       +'(packages=([^ \|\}]*))?.*\}{2}';
    REGEX_CODE        = '<syntaxhighlight[^>]*>(.*)</syntaxhighlight>';
    REGEX_CODE_LANG   = '<syntaxhighlight.*lang="([^"]*)".*>';
  protected
    procedure ProcessMessage(aMessage: TMessage); override;
    procedure ParseTestableParams  (aMessage: TPageContentRetrievedMessage;
      aParsed: TPageContentParsedMessage);
    procedure ParseCode            (aMessage: TPageContentRetrievedMessage;
      aParsed: TPageContentParsedMessage);
    procedure ParseCodeLanguage    (aMessage: TPageContentRetrievedMessage;
      aParsed: TPageContentParsedMessage);
    function ParseContent(aMessage: TPageContentRetrievedMessage): TPageContentParsedMessage;
    property RETestable    : TRegExpr read fRETestable;
    property RECode        : TRegExpr read fRECode;
    property RECodeLanguage: TRegExpr read fRECodeLanguage;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt =
      DefaultStackSize); override;
    destructor Destroy; override;
  end;

var
  PageContentParser: TPageContentParser;

implementation

{ TPageContentParseErrorMessage }

procedure TPageContentParseErrorMessage.AssignTo(Dest: TPersistent);
var
  pe: TPageContentParseErrorMessage absolute Dest;
begin
  inherited AssignTo(Dest);
  if Dest is TPageContentParseErrorMessage then
  begin
    if Assigned(pe.fErrorException) then
      FreeAndNil(pe.fErrorException);
    if Assigned(Self.fErrorException) then
      pe.fErrorException :=
        EPageContentClass(Self.fErrorException.ClassType).CreateHelp(
          Self.fErrorException.Message,
          Self.fErrorException.HelpContext
        );
  end;
end;

constructor TPageContentParseErrorMessage.Create(aException: EPageContent);
begin
  inherited Create;
  fErrorException := aException;
end;

destructor TPageContentParseErrorMessage.Destroy;
begin
  fErrorException.Free;
  inherited Destroy;
end;

{ TPageContentParser }

procedure TPageContentParser.ProcessMessage(aMessage: TMessage);
var
  pc: TPageContentRetrievedMessage absolute aMessage;
  ParsedContent: TPageContentParsedMessage;
  EmptyContent: TPageContentEmptyMessage;
  ErrorMessage: TPageContentParseErrorMessage;
begin
  Assert(aMessage is TPageContentRetrievedMessage, 'Invalid message type');

  if Length(Trim(pc.Content.Text)) = 0 then
  begin
    EmptyContent := TPageContentEmptyMessage.Create;
    EmptyContent.Assign(aMessage);
    DispatchMessage(EmptyContent);
    exit;
  end;
  try
    ParsedContent := ParseContent(pc);
    DispatchMessage(ParsedContent);
  except
    on e: EPageContent do
    begin
      ErrorMessage := TPageContentParseErrorMessage.Create;
      ErrorMessage.Assign(aMessage);
      // original exception is disposed at end of except block => clone
      ErrorMessage.ErrorException := EPageContentClass(e.ClassType).CreateHelp(e.Message, e.HelpContext);
      DispatchMessage(ErrorMessage);
    end;
  end;
end;

procedure TPageContentParser.ParseTestableParams(
  aMessage: TPageContentRetrievedMessage; aParsed: TPageContentParsedMessage);
var
  i: Integer;
begin
  if not RETestable.Exec(aMessage.Content.Text) then
    raise ENotTestable.CreateFmt(
      'Page %s doesnot include testable template %s.',
      [aMessage.PageName, TESTABLE_TEMPLATE]);

  for i := 0 to RETestable.SubExprMatchCount do
  begin
    case i of
      0: ; // full template code
      1: ; // full os=<content>
      2: aParsed.OperatingSystem := RETestable.Match[i]; // os or '' if not present
      3: ; // full preset=<content>
      4: aParsed.Preset := RETestable.Match[i]; // preset or '' if not present
      5: ; // full packages=<content>
      6: aParsed.Packages.DelimitedText := RETestable.Match[i]; // package list
    end;
  end;
end;

procedure TPageContentParser.ParseCode(aMessage: TPageContentRetrievedMessage;
  aParsed: TPageContentParsedMessage);
var
  ExtractedCode: String;
begin
  if not RECode.Exec(aMessage.Content.Text) then
    raise ENoCode.CreateFmt('Page %s doesnot contain any highlighted code.',
      [aMessage.PageName]);

  Assert(RECode.SubExprMatchCount >= 1, 'Unknown code format?');
  if RECode.SubExprMatchCount > 1 then
    raise EMultipleCode.CreateFmt(
      'There are %d code sections in page %s; at maximum one is allowed.',
      [(RECode.SubExprMatchCount div 2)+1, aMessage.PageName]);

  ExtractedCode := RECode.Match[1];
  ExtractedCode := Trim(ExtractedCode);
  if ExtractedCode = '' then
    raise EEmptyCode.CreateFmt('Page %s contains no source code.',
      [aMessage.PageName]);

  aParsed.Code.Text := ExtractedCode;
end;

procedure TPageContentParser.ParseCodeLanguage(
  aMessage: TPageContentRetrievedMessage; aParsed: TPageContentParsedMessage);
var
  defLang: Boolean;  // default language
begin
  defLang := not RECodeLanguage.Exec(aMessage.Content.Text);
  aParsed.DefaultLanguage := defLang;
  if not defLang then
  begin
    Assert(RECodeLanguage.SubExprMatchCount = 1, 'Code Language found but no SubExpr');
    aParsed.Language := RECodeLanguage.Match[1];
  end;
end;

function TPageContentParser.ParseContent(aMessage: TPageContentRetrievedMessage
  ): TPageContentParsedMessage;
begin
  Result := TPageContentParsedMessage.Create;
  Result.Assign(aMessage);
  try
    ParseTestableParams(aMessage, Result);
    ParseCode          (aMessage, Result);
    ParseCodeLanguage  (aMessage, Result);
  except
    on Exception do
    begin
      Result.Destroy;
      Result := nil;
      raise;
    end;
  end;
end;

constructor TPageContentParser.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  fRETestable := TRegExpr.Create;
  fRETestable.ModifierI := True;
  fRETestable.ModifierM := True;
  fRETestable.ModifierS := False;
  fRETestable.Expression := REGEX_TESTABLE;

  fRECode := TRegExpr.Create;
  fRECode.ModifierM := True;
  fRECode.Expression := REGEX_CODE;

  fRECodeLanguage := TRegExpr.Create;
  fRECodeLanguage.ModifierM := True;
  fRECodeLanguage.ModifierS := False;
  fRECodeLanguage.Expression := REGEX_CODE_LANG;

  inherited Create(CreateSuspended, StackSize);
end;

destructor TPageContentParser.Destroy;
begin
  fRETestable .Free;
  fRECode        .Free;
  fRECodeLanguage.Free;
  inherited Destroy;
end;

{ TPageContentParsedMessage }

procedure TPageContentParsedMessage.AssignTo(Dest: TPersistent);
var
  pc: TPageContentParsedMessage absolute Dest;
begin
  inherited AssignTo(Dest);
  if Dest is TPageContentParsedMessage then
  begin
    pc.fCode.Assign(Self.fCode);
    pc.fDefaultLanguage := Self.fDefaultLanguage;
    pc.fLanguage := Self.fLanguage;
    pc.fOperatingSystem := Self.fOperatingSystem;
    pc.fPackages.Assign(Self.fPackages);
    pc.fPreset := Self.fPreset;
  end;
end;

constructor TPageContentParsedMessage.Create;
begin
  inherited Create;
  fCode     := TStringList.Create;
  fPackages := TStringList.Create;
  fPackages.Delimiter := ',';
  fLanguage := DEFAULT_LANGUAGE;
end;

destructor TPageContentParsedMessage.Destroy;
begin
  fCode.Free;
  fPackages.Free;
  inherited Destroy;
end;


initialization
  PageContentParser := TPageContentParser.Create(False, DefaultStackSize);
  PageContentParser.FreeOnTerminate := True;
  MessageManager.RegisterForMessage(TPageContentRetrievedMessage, PageContentParser);
finalization
  PageContentParser.Terminate;
end.

