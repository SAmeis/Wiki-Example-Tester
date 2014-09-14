{ Preparation of example test
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

unit ucompileprepare;

{$mode objfpc}{$H+}{$CODEPAGE UTF8}

interface

uses
  Classes, SysUtils, FileUtil, LazUTF8Classes, {$IFDEF WINDOWS} windows,{$ENDIF}
  umessagemanager, upagecontentparser, upagecontentretriever;

type
  ETestPrepare = class(Exception);
  EProjectDirectory = class(ETestPrepare);

  { TTestPreparedMessage }

  TTestPreparedMessage = class(TPageContentParsedMessage)
  private
    fProgramFile: UnicodeString;
    fProjectDirectory: UnicodeString;
    fProjectFile: UnicodeString;
    fRootDir: UnicodeString;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property RootDir: UnicodeString read fRootDir write fRootDir;
    property ProjectDirectory: UnicodeString read fProjectDirectory write fProjectDirectory;
    property ProjectFile: UnicodeString read fProjectFile write fProjectFile; // lpi
    property ProgramFile: UnicodeString read fProgramFile write fProgramFile; // lpr
  end;

  { TRollbackTestDirectoryMessage }

  TRollbackTestDirectoryMessage = class(TGetPageContentMessage)
  private
    fProjectDirectory: UnicodeString;
    fRootDir: UnicodeString;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property RootDir: UnicodeString read fRootDir write fRootDir;
    property ProjectDirectory: UnicodeString read fProjectDirectory write fProjectDirectory;
  end;

  { TDirectoryCreateErrorMessage }

  TDirectoryCreateErrorMessage = class(TRollbackTestDirectoryMessage, IExceptionMessage)
  private
    fExceptionClass: TClass;
    fExceptionMessage: String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    function GetExceptionClass: TClass;
    function GetExceptionMessage: String;
    procedure SetExceptionClass(aValue: TClass);
    procedure SetExceptionMessage(aValue: String);
    property ExceptionClass: TClass read GetExceptionClass write SetExceptionClass;
    property ExceptionMessage: String read GetExceptionMessage write SetExceptionMessage;
  end;


  { TTestPreparation }

  TTestPreparation = class(TQueuedThread)
  private
    fRootDirectory: UnicodeString;
  protected
    procedure RollbackProjectDirectory(aMessage: TTestPreparedMessage);
    procedure CreateProjectDirectory(aMessage: TTestPreparedMessage);
    procedure ProcessMessage(aMessage: TMessage); override;
    procedure PutFileContents(aMessage: TTestPreparedMessage);
  public
    property RootDirectory: UnicodeString read fRootDirectory write fRootDirectory;
  end;

var
  TestPreparation: TTestPreparation;

implementation

{ TDirectoryCreateErrorMessage }

procedure TDirectoryCreateErrorMessage.AssignTo(Dest: TPersistent);
var
  i: IExceptionMessage;
begin
  inherited AssignTo(Dest);
  if Dest.GetInterface(IID_EXCEPTIONMESSAGE, i) then
  begin
    i.ExceptionClass := Self.fExceptionClass;
    i.ExceptionMessage := Self.fExceptionMessage;
    i := nil;
  end;
end;

function TDirectoryCreateErrorMessage.GetExceptionClass: TClass;
begin
  Result := fExceptionClass;
end;

function TDirectoryCreateErrorMessage.GetExceptionMessage: String;
begin
  Result := fExceptionMessage;
end;

procedure TDirectoryCreateErrorMessage.SetExceptionClass(aValue: TClass);
begin
  fExceptionClass := aValue;
end;

procedure TDirectoryCreateErrorMessage.SetExceptionMessage(aValue: String);
begin
  fExceptionMessage := aValue;
end;

{ TRollbackTestDirectoryMessage }

procedure TRollbackTestDirectoryMessage.AssignTo(Dest: TPersistent);
var
  rtd: TRollbackTestDirectoryMessage absolute Dest;
begin
  inherited AssignTo(Dest);
  if Dest is Self.ClassType then
  begin
    rtd.fRootDir := Self.fRootDir;
    rtd.fProjectDirectory := Self.fProjectDirectory;
  end;
end;

{ TTestPreparation }

procedure TTestPreparation.RollbackProjectDirectory(
  aMessage: TTestPreparedMessage);
var
  rollback: TRollbackTestDirectoryMessage;
begin
  rollback := TRollbackTestDirectoryMessage.Create;
  rollback.Assign(aMessage);
  DispatchMessage(rollback);
end;

procedure TTestPreparation.CreateProjectDirectory(aMessage: TTestPreparedMessage
  );
var
  r: LongInt;
  prjdir: UnicodeString;
begin
  Assert(aMessage.RootDir <> '', 'Message root directory not set');
  Assert(aMessage.RootDir = IncludeTrailingPathDelimiter(aMessage.RootDir), 'Message root directory without trailing path delimiter.');

  // get random directory which doesnot exist yet
  repeat
    r := Random(MaxInt);
    prjdir := {%H-}hexStr(r, SizeOf(r) * 2);
    prjdir := aMessage.RootDir+prjdir;
  until not DirectoryExists(prjdir);

  if not ForceDirectories(prjdir) then
    raise EProjectDirectory.CreateFmt('Couldnot create project direcotry "%s".', [prjdir]);

  aMessage.ProjectDirectory := IncludeTrailingPathDelimiter(prjdir);
end;

procedure TTestPreparation.ProcessMessage(aMessage: TMessage);
var
  pc: TPageContentRetrievedMessage absolute aMessage;
  TestPrepared: TTestPreparedMessage;
  dce: TDirectoryCreateErrorMessage;
begin
  Assert(aMessage is TPageContentParsedMessage, 'Invalid message class');

  TestPrepared := TTestPreparedMessage.Create;
  TestPrepared.Assign(aMessage);
  TestPrepared.RootDir := RootDirectory;
  try
    CreateProjectDirectory(TestPrepared);
    TestPrepared.ProgramFile := TestPrepared.ProjectDirectory+'project1.lpr';
    TestPrepared.ProjectFile := TestPrepared.ProjectDirectory+'project1.lpi';
    PutFileContents(TestPrepared);

    DispatchMessage(TestPrepared);
  except
    on e: EProjectDirectory do
    begin
      RollbackProjectDirectory(TestPrepared);

      // Dispatch Exception message
      dce := TDirectoryCreateErrorMessage.Create;
      dce.Assign(aMessage);
      dce.ExceptionClass := e.ClassType;
      dce.ExceptionMessage := e.Message;
      DispatchMessage(dce);

      TestPrepared.Destroy;
      TestPrepared := Nil;
    end else
      raise;
  end;
end;

procedure TTestPreparation.PutFileContents(aMessage: TTestPreparedMessage);
var
  // use LazUTF8Classes.TFileStreamUTF8 until Classes.TFileStream
  // supports unicode file names
  f: TFileStreamUTF8;
  r: TResourceStream;
begin
  Assert(aMessage.ProgramFile <> '', 'ProgramFile not set.');
  Assert(aMessage.ProjectFile <> '', 'ProjectFile not set.');
  Assert(not FileExists(aMessage.ProgramFile), 'ProgramFile already exists.');
  Assert(not FileExists(aMessage.ProjectFile), 'ProjectFile already exists.');

  f := TFileStreamUTF8.Create(UTF8Encode(aMessage.ProgramFile), fmCreate);
  try
    aMessage.Code.SaveToStream(f);
  finally
    f.Destroy;
    f := nil;
  end;

  f := TFileStreamUTF8.Create(UTF8Encode(aMessage.ProjectFile), fmCreate);
  r := TResourceStream.Create(HINSTANCE, 'PRJ_LPI_TEMPL', RT_RCDATA);
  try
    f.CopyFrom(r, r.Size);
  finally
    r.Destroy;
    f.Destroy;
  end;
end;

{ TTestPreparedMessage }

procedure TTestPreparedMessage.AssignTo(Dest: TPersistent);
var
  cpm: TTestPreparedMessage absolute Dest;
  rt: TRollbackTestDirectoryMessage absolute Dest;
begin
  inherited AssignTo(Dest);
  if Dest is TTestPreparedMessage then
  begin
    cpm.fProgramFile      := Self.fProgramFile;
    cpm.fProjectDirectory := Self.fProjectDirectory;
    cpm.ProjectFile       := Self.fProjectFile;
    cpm.fRootDir          := Self.fRootDir;
  end;
  if Dest is TRollbackTestDirectoryMessage then
  begin
    rt.fProjectDirectory := Self.fProjectDirectory;
    rt.fRootDir := Self.fRootDir;
  end;
end;

initialization
  Randomize;

  TestPreparation := TTestPreparation.Create(True, DefaultStackSize);
  TestPreparation.FreeOnTerminate := True;
  {$IFDEF DEBUG}
  TestPreparation.RootDirectory :=
    IncludeTrailingPathDelimiter(
      ExpandFileName(UTF8Decode('workdir'))
    );
  {$ELSE}
  TestPreparation.RootDirectory :=
    IncludeTrailingPathDelimiter(
      UTF8Decode(
        SysToUTF8(
          GetTempDir(False)+VendorName
        )
      )
    );
  {$ENDIF}
  TestPreparation.Start;
  MessageManager.RegisterForMessage(TPageContentParsedMessage, TestPreparation);
finalization
  TestPreparation.Terminate;
end.

