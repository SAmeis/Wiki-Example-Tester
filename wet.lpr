program wet;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  sysutils,
  Forms, ufrmmain, umessagemanager, uapplicationforwarder,
  upagecontentretriever, upagecontentparser, ucompileprepare, 
upascalcompilation;

{$R *.res}

begin
  {$IFDEF DEBUG}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF}
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.

