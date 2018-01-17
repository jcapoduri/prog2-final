program metru;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainunit,
  { you can add units after this }
  metru.core   in 'libs\metru.core.pas'
  ;

{$R *.res}

begin
  metru.core.kickoff(metruApp);
  //metru.core.setup(metruApp);
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

