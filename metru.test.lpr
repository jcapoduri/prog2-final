program metru.test;

{$mode objfpc}{$H+}

uses
  sysutils,
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
  ,metru.core       in 'libs\metru.core.pas'
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

{ category menu block }

procedure dumpCategoryTree();
var
  category            : lib.tree.lcrs.tCategory;
  catDB               : lib.tree.lcrs.tLCRStree;
  idx, maxIdx         : idxRange;
begin
  lib.tree.lcrs.loadTree(catDB, 'data/', 'categories');
  reset(catDB.data);
  maxIdx := filesize(catDB.data) - 1;
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
  wait('presione una tecla para continuar');
end;

function categorymenu() : integer;
begin
  ClrScr;
  writeln('Menu Categorias (1-2)');
  writeln('1- Dump de estructura de datos');
  writeln('2- Dump de estructura de control');
  writeln('0- Salir');
  categorymenu := readValidNumber(0, 2);
end;

procedure categorymenuAct(op : integer);
begin
  case op of
    1: dumpCategoryTree;
  end;
end;

{ end category menu block }

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
  writeln('Menu Usuarios (1-3)');
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

{ message menu block }

procedure insertMessage();
var
  message     : lib.tree.trinary.tMessage;
  io          : lib.tree.trinary.tTrinaryTree;
  idx, maxIdx : idxRange;
begin
  lib.tree.trinary.newEmptyTree(io, 'data/', 'messages');
  message.question  := 'usted es el rey de los minisupers?';
  message.answer    := 'asi es';
  message.timestamp := Now;
  lib.tree.trinary.insertMessage(io, 1, 1, message);
  lib.tree.trinary.insertMessage(io, 2, 1, message);
  lib.tree.trinary.insertMessage(io, 1, 1, message);
  lib.tree.trinary.insertMessage(io, 1, 2, message);
end;

procedure dumpMessageData();
var
  message     : lib.tree.trinary.tMessage;
  io          : lib.tree.trinary.tTrinaryTree;
  idx, maxIdx : idxRange;
begin
  lib.tree.trinary.loadTree(io, 'data/', 'messages');
  reset(io.data);
  maxIdx := filesize(io.data) - 1;
  close(io.data);
  write('pos | ');
  write('number | ');
  write('question | ');
  write('answer | ');
  write('timestamp | ');
  write('next');
  writeln;
  for idx := 0 to maxIdx do
    begin
      message := lib.tree.trinary.fetchMessage(io, idx);
      write(idx, ' | ');
      write(message.number, ' | ');
      write(message.question, ' | ');
      write(message.answer, ' | ');
      write(message.timestamp, ' | ');
      write(message.next);
      writeln;
    end;
  readln;
end;

procedure dumpMessageControl();
begin
end;

procedure dumpMessageTree();
var
  node        : lib.tree.trinary.tNode;
  io          : lib.tree.trinary.tTrinaryTree;
  idx, maxIdx : idxRange;
begin
  lib.tree.trinary.loadTree(io, 'data/', 'messages');
  reset(io.index);
  maxIdx := filesize(io.index) - 1;
  write('pos | ');
  write('id | ');
  write('idUser | ');
  write('parent | ');
  write('first | ');
  write('last | ');
  write('left | ');
  write('center | ');
  write('right');
  writeln;
  for idx := 0 to maxIdx do
    begin
      seek(io.index, idx);
      read(io.index, node);
      write(idx, ' | ');
      write(node.id, ' | ');
      write(node.idUser, ' | ');
      write(node.parent, ' | ');
      write(node.first, ' | ');
      write(node.last, ' | ');
      write(node.left, ' | ');
      write(node.center, ' | ');
      write(node.right, ' | ');
      writeln;
    end;
  close(io.index);  
  readln;
end;


function messagemenu() : integer;
begin
  ClrScr;
  writeln('Menu Message (1-3)');
  writeln('1- Dump de estructura de datos');
  writeln('2- Dump de estructura de control');
  writeln('3- Dump de estructura de arbol');
  writeln('4- Emular mensajes');
  writeln('0- Salir');
  messagemenu := readValidNumber(0, 4);
end;

procedure messagemenuAct(op : integer);
begin
  case op of
    1: dumpMessageData;
    2: dumpMessageControl;
    3: dumpMessageTree;
    4: insertMessage;
  end;
end;

{ end message menu block }


{ sells menu block }

procedure dumpSellsHash();
var
  sell : tSell;
  io   : tCloseHash;
  i    : integer;
begin
  ClrScr;
  write('pos | ');
  write('idBuyer | ');
  write('idItem | ');
  write('itemName | ');
  write('price | ');
  write('publishDate | ');
  write('sellDate | ');
  write('itemType | ');
  write('calification | ');
  write('tax | ');
  write('alreadyCollected | ');
  write('previous | ');
  write('next | ');
  writeln;

  lib.hash.close.loadHash(io, 'data/', 'sells');
  reset(io.data);
  for i := 0 to (filesize(io.data) - 1) do
    begin
      seek(io.data, i);
      read(io.data, sell);
      write(i);
      //if (sell.id > 0) then
        //begin
          write(i, ' | ');
          write(sell.idBuyer, ' | ');
          write(sell.idItem, ' | ');
          write(sell.itemName, ' | ');
          write(sell.price, ' | ');
          write(sell.publishDate, ' | ');
          write(sell.sellDate, ' | ');
          write(sell.itemType, ' | ');
          write(sell.calification, ' | ');
          write(sell.tax, ' | ');
          write(sell.alreadyCollected, ' | ');
          write(sell.previous, ' | ');
          write(sell.next, ' | ');
          writeln;
        //end
      //else
        //write('VACIO');
      writeln;
    end;
  close(io.data);
  wait('presione una tecla para continuar');
end;

procedure dumpSellsControl();
var
  ctrl : lib.hash.close.tControlRecord;
  io   : tCloseHash;
begin
  ClrScr;
  lib.hash.close.loadHash(io, 'data/', 'sells');
  reset(io.control);
  seek(io.control, 0);
  read(io.control, ctrl);
  close(io.control);
  writeln('Primer borrado: ' , ctrl.erased);
  wait('presione una tecla para continuar');
end;


function sellsmenu() : integer;
begin
  ClrScr;
  writeln('Menu Ventas (1-3)');
  writeln('1- Dump de estructura del hash');
  writeln('2- Dump de estructura de control');
  writeln('3- Test de aglomeracion/dispersion');
  writeln('0- Salir');
  sellsmenu := readValidNumber(0, 3);
end;

procedure sellsmenuAct(op : integer);
begin
  case op of
    1: dumpSellsHash;
    2: dumpSellsControl;
  end;
end;

{ end sells menu block }

{main menu block}
procedure resetApp;
begin
  metru.core.kickoff(metruApp);
  wait('Reset realizado. Presione una tecla para continuar');
end;

function mainmenu() : integer;
begin
  ClrScr;
  writeln('Menu (1-5)');
  writeln('1- Usuarios');
  writeln('2- Categorias');
  writeln('3- Publicaciones');
  writeln('4- Mensajes');
  writeln('5- Ventas');
  writeln('6- RESET GENERAL DEL SISTEMA');
  writeln('0- Salir');
  mainmenu := readValidNumber(0, 6);
end;

procedure mainmenuAct(op : integer);
begin
  case op of
    1: actMenu(@usermenu, @usermenuAct);
    2: actMenu(@categorymenu, @categorymenuAct);
    4: actMenu(@messagemenu, @messagemenuAct);
    5: actMenu(@sellsmenu, @sellsmenuAct);
    6: resetApp;
  end;
end;
{end main menu block }

begin
  actMenu(@mainmenu, @mainmenuAct);
end.

