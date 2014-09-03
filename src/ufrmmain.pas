{ GUI main form

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
unit ufrmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, umessagemanager, uapplicationforwarder, upagecontentretriever,
  upagecontentparser;

type
  { TFrmMain }

  TFrmMain = class(TForm)
    BtnGetCode: TButton;
    EURL: TEdit;
    Label1: TLabel;
    MeSiteContent: TMemo;
    MeContentDump: TMemo;
    MePages: TMemo;
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

{$R *.lfm}

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

procedure TFrmMain.GetPageContentAsync(const aPage: String);
var
  m: TGetPageContentMessage;
begin
  m := TGetPageContentMessage.Create;
  m.PageName := aPage;
  MessageManager.AddMessage(m);
end;

end.

