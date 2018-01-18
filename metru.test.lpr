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

procedure dumpCategoryTree();
var
  category            : lib.tree.lcrs.tCategory;
  catDB               : lib.tree.lcrs.tLCRStree;
  idx, maxIdx         : idxRange;
begin
  lib.tree.lcrs.loadTree(catDB, 'data/', 'categories');
  reset(catDB.data);
  maxIdx := filesize(catDB.data);
  close(catDB.data);

  write('pos | ');
  write('name | ');
  write('desc | ');
  write('VAT | ');
  write('parent | ');
  write('leftChild | ');
  write('rightChild | ');
  writeln;
  for idx := 0 to maxIdx do
    begin
      category := lib.tree.lcrs.fetch(catDB, idx);
      write(idx, ' | ');
      write(category.categoryName, ' | ');
      write(category.description, ' | ');
      write(category.VAT, ' | ');
      write(category.parent, ' | ');
      write(category.leftChild, ' | ');
      write(category.rightSibling, ' | ');
      writeln;
    end;
end;

var
  user1, user2, user3 : lib.hash.open.tUser; 
  userDB              : lib.hash.open.tOpenHash;
  found               : boolean;
  userPointer         : lib.hash.open.tHashValue;
  catDB               : lib.tree.lcrs.tLCRStree;
  category            : lib.tree.lcrs.tCategory;
  idx, maxIdx         : idxRange;

begin
  dumpCategoryTree();
  readln;
end.

