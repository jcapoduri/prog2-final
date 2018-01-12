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
  Application.Title:='metru';
  metru.core.setup(metru);
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormLogin, FormLogin);
  Application.Run;
end.

