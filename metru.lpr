program metru;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainunit, runtimetypeinfocontrols,
  { you can add units after this }
  metru.core   in 'libs\metru.core.pas'
  ;

{$R *.res}

begin
  metru.core.setup(metruApp);
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TmainWidget, mainWidget);
  Application.Run;
end.

