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
  //lib.tree.avl     in 'libs\lib.tree.avl.pas',
  lib.tree.trinary in 'libs\lib.tree.trinary.pas'
  //io.helpers       in 'libs\io.helpers.pas'
  ;

var
  user1, user2, user3 : lib.hash.open.tUser; 
  userDB              : lib.hash.open.tOpenHash;
  found               : boolean;
  userPointer         : lib.hash.open.tHashValue;

begin
  lib.hash.open.loadHash(userDB, 'data/', 'users');

  user1.email := 'jcapoduri@gmail.com';

  lib.hash.open.insert(userDB, user1);  

  user2.email := 'natalia.vallone87@gmail.com';

  lib.hash.open.insert(userDB, user2);

  user3.email := 'cletonko@gmail.com';

  lib.hash.open.insert(userDB, user3);

  user1.email := 'pepito';

  found := lib.hash.open.search(userDB, 'jcapoduri@gmail.com', userPointer);

  if found then
    WriteLn('encontrado')
  else
    WriteLn('nop');

  user1 := lib.hash.open.fetch(userDB, 'jcapoduri@gmail.com');

  Write(user1.email);

end.

