program metru.test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  { you can add units after this }
  lib.tree.lcrs    in 'libs\lib.tree.lcrs.pas',
  lib.hash.open    in 'libs\lib.hash.open.pas',
  lib.hash.close   in 'libs\lib.hash.close.pas',
  lib.tree.avl     in 'libs\lib.tree.avl.pas',
  lib.tree.trinary in 'libs\lib.tree.trinary.pas',
  io.helpers       in 'libs\io.helpers.pas'
  ;

{$R *.res}

begin
end.

