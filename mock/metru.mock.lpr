program metru.mock;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainwidget
  { you can add units after this };

{$R *.res}

begin
  Randomize;
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(Tmocker, mocker);
  Application.Run;
end.

