unit ufilesystemworker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umessagemanager;

type

  { TFileSystemMessage }

  TFileSystemMessage = class(TMessage)
  private
    fPath: UTF8String;
    fSuccessMessage: TMessage;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    destructor Destroy; override;
    property Path: UTF8String read fPath write fPath;
    property SuccessMessage: TMessage read fSuccessMessage write fSuccessMessage;  // dispatched on success; destroyed on failure
  end;

  TCreateDirectorMessage = class(TFileSystemMessage);
  TDirectoryCreatedMessage = class(TFileSystemMessage);

  { TDirectoryCreatedErrorMessage }

  TDirectoryCreatedErrorMessage = class(TCreateDirectorMessage, IHasExceptionMessage)
  private
    fExceptionMessage: TExceptionMessage;
  public
    function GetExceptionMessage: TExceptionMessage;
  end;

  TRemoveDirectoryeMessage = class(TFileSystemMessage);

  TRemoveFileMessage = class(TFileSystemMessage);


  TFileIsDirectoryErrorMessage = class(TFileSystemMessage, IHasExceptionMessage)

  end;

  TDirectoryIsFileErrorMessage = class(TFileSystemMessage, IHasExceptionMessage)

  end;

  TFileSystemWorker = class(TQueuedThread)

  end;

implementation

{ TDirectoryCreatedErrorMessage }

function TDirectoryCreatedErrorMessage.GetExceptionMessage: TExceptionMessage;
begin
  Result := fExceptionMessage;
end;

{ TFileSystemMessage }

procedure TFileSystemMessage.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TFileSystemMessage then
  begin
    TFileSystemMessage(Path).fPath := Self.fPath;
    TFileSystemMessage(Path).fSuccessMessage := fSuccessMessage.Clone;
  end;
end;

destructor TFileSystemMessage.Destroy;
begin
  fSuccessMessage.Free;
  inherited Destroy;
end;

end.

