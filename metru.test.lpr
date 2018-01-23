program metru.test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
{$IFNDEF FPC}
  Console in 'libs\Console.pas'
{$ENDIF}
{$IFDEF FPC}
  Crt
{$ENDIF}
  { you can add units after this }
  ,lib.tree.lcrs    in 'libs\lib.tree.lcrs.pas'
  ,lib.hash.open    in 'libs\lib.hash.open.pas'
  ,lib.hash.close   in 'libs\lib.hash.close.pas'
  ,lib.tree.avl     in 'libs\lib.tree.avl.pas'
  ,lib.tree.trinary in 'libs\lib.tree.trinary.pas'
  ,io.helpers       in 'libs\io.helpers.pas'
  ;

type
  tMenuFunction = function():integer;
  tActFunction  = procedure(op : integer);

procedure actMenu(menu : tMenuFunction; act : tActFunction);
var
  op : integer;

begin
  op := -1;

  while op <> 0 do
    begin
      op := menu();
      if op > 0 then act(op);
    end;
end;

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

{ user menu block }
procedure dumpUserHash();
var
  user : tUser;
  io   : tOpenHash;
  i    : integer;
begin
  ClrScr;
  write('pos | ');
  write('id | ');
  write('email | ');
  write('password | ');
  write('fullname | ');
  write('address | ');
  write('providence | ');
  write('status | ');
  write('ctimestamp | ');
  write('photoUrl | ');
  write('blocked | ');
  write('utimestamp | ');
  writeln;

  lib.hash.open.loadHash(io, 'data/', 'users');
  reset(io.data);
  for i := 0 to lib.hash.open.MAX do
    begin
      seek(io.data, i);
      read(io.data, user);
      write(i);
      if (user.id > 0) then
        begin
          write(user.id, ' | ');
          write(user.email, ' | ');
          write(user.password, ' | ');
          write(user.fullname, ' | ');
          write(user.address, ' | ');
          write(user.providence, ' | ');
          write(user.status, ' | ');
          write(user.ctimestamp, ' | ');
          write(user.photoUrl, ' | ');
          write(user.blocked, ' | ');
          write(user.utimestamp, ' | ');
        end
      else
        write('VACIO');
      writeln;
    end;
  close(io.data);
  wait('presione una tecla para continuar');
end;

procedure dumpUserControl();
var
  ctrl : lib.hash.open.tControlRecord;
  io   : tOpenHash;
begin
  ClrScr;
  lib.hash.open.loadHash(io, 'data/', 'users');
  reset(io.control);
  seek(io.control, 0);
  read(io.control, ctrl);
  close(io.control);
  writeln('Ultimo ID: ' , ctrl.lastID);
  writeln('Cantidad: ', ctrl.count);
  wait('presione una tecla para continuar');
end;

function usermenu() : integer;
begin
  ClrScr;
  writeln('Menu Usuarios (1-5)');
  writeln('1- Dump de estructura del hash');
  writeln('2- Dump de estructura de control');
  writeln('3- Test de aglomeracion/dispersion');
  writeln('0- Salir');
  usermenu := readValidNumber(0, 3);
end;

procedure usermenuAct(op : integer);
begin
  case op of
    1: dumpUserHash;
    2: dumpUserControl;
  end;
end;


{ end user menu block }

{main menu block}
function mainmenu() : integer;
begin
  ClrScr;
  writeln('Menu (1-5)');
  writeln('1- Usuarios');
  writeln('2- Categorias');
  writeln('3- Publicaciones');
  writeln('4- Mensajes');
  writeln('5- Ventas');
  writeln('0- Salir');
  mainmenu := readValidNumber(0, 5);
end;

procedure mainmenuAct(op : integer);
begin
  case op of
    1: actMenu(@usermenu, @usermenuAct);
  end;
end;
{end main menu block}

begin
  actMenu(@mainmenu, @mainmenuAct);
end.

