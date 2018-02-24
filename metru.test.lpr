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
  ,DateUtils
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

procedure dumpCategoryControl();
var
  ctrl : lib.tree.lcrs.tControlRecord;
  io   : tLCRStree;
begin
  ClrScr;
  lib.tree.lcrs.loadTree(io, 'data/', 'categories');
  reset(io.control);
  seek(io.control, 0);
  read(io.control, ctrl);
  close(io.control);
  writeln('Ultimo ID: ' , ctrl.lastID);
  writeln('Raiz: ', ctrl.root);
  writeln('Indice borrado: ', ctrl.erased);
  wait('presione una tecla para continuar');
end;

procedure generateCategories();
var
  cat  : tCategory;
begin
  metru.core.setup(metruApp);

  cat.categoryName := 'Tecnologia';
  cat.description  := 'Tecnologia';
  cat.VAT          := Random(2000) / 100;
  cat.parent       := NULLIDX;
  cat.leftChild    := NULLIDX;
  cat.rightSibling := NULLIDX;
  metru.core.createCateogry(metruApp, cat);

  cat.categoryName := 'Electrodomestico';
  cat.description  := 'Electrodomestico';
  cat.VAT          := Random(2000) / 100;
  cat.parent       := NULLIDX;
  cat.leftChild    := NULLIDX;
  cat.rightSibling := NULLIDX;
  metru.core.createCateogry(metruApp, cat);

  cat.categoryName := 'Computadoras';
  cat.description  := 'Computadoras';
  cat.VAT          := Random(2000) / 100;
  cat.parent       := 1;
  cat.leftChild    := NULLIDX;
  cat.rightSibling := NULLIDX;
  metru.core.createCateogry(metruApp, cat);

  cat.categoryName := 'Aires';
  cat.description  := 'Aires';
  cat.VAT          := Random(2000) / 100;
  cat.parent       := 2;
  cat.leftChild    := NULLIDX;
  cat.rightSibling := NULLIDX;
  metru.core.createCateogry(metruApp, cat);
end;

function categorymenu() : integer;
begin
  ClrScr;
  writeln('Menu Categorias (1-2)');
  writeln('1- Dump de estructura de datos');
  writeln('2- Dump de estructura de control');
  writeln('3- Generar categorias');
  writeln('0- Salir');
  categorymenu := readValidNumber(0, 2);
end;

procedure categorymenuAct(op : integer);
begin
  case op of
    1: dumpCategoryTree;
    2: dumpCategoryControl;
    3: generateCategories;
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
      write(i, ' | ');
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

procedure generateBulkUsers();
var
  user : tUser;
  io   : tOpenHash;
begin
  lib.hash.open.loadHash(io, BASEPATH, USERFILE);

  user.email      := 'jcapoduri@gmail.com';
  user.password   := 'palo_y_a_la_bolsa';
  user.fullname   := 'Jorge Capoduri';
  user.address    := 'Yrigoyen 831';
  user.providence := Random(25);
  user.ctimestamp := Now;
  user.photoUrl   := '';
  user.status     := false;
  user.utimestamp := Now;
  user.blocked    := false;
  lib.hash.open.insert(io, user);

  user.email      := 'alexalv@juan23.edu.ar';
  user.password   := 'palo_y_a_la_bolsa';
  user.fullname   := 'Claudio Alvarez';
  user.address    := 'El Juan 23';
  user.providence := Random(25);
  user.ctimestamp := Now;
  user.photoUrl   := '';
  user.status     := false;
  user.utimestamp := Now;
  user.blocked    := false;
  lib.hash.open.insert(io, user);

  user.email      := 'natalia.vallone87@gmail.com';
  user.password   := 'palo_y_a_la_bolsa';
  user.fullname   := 'Naty  Carulias';
  user.address    := 'Yrigoyen 831';
  user.providence := Random(25);
  user.ctimestamp := Now;
  user.photoUrl   := '';
  user.status     := false;
  user.utimestamp := Now;
  user.blocked    := false;
  lib.hash.open.insert(io, user);

  user.email      := 'donbarredora@gmail.com';
  user.password   := 'palo_y_a_la_bolsa';
  user.fullname   := 'Don barredora';
  user.address    := 'Yrigoyen 831';
  user.providence := Random(25);
  user.ctimestamp := Now;
  user.photoUrl   := '';
  user.status     := false;
  user.utimestamp := Now;
  user.blocked    := false;
  lib.hash.open.insert(io, user);

  user.email      := 'cloetonko@gmail.com';
  user.password   := 'palo_y_a_la_bolsa';
  user.fullname   := 'El terror de los 7 mares';
  user.address    := 'Yrigoyen 831';
  user.providence := Random(25);
  user.ctimestamp := Now;
  user.photoUrl   := '';
  user.status     := false;
  user.utimestamp := Now;
  user.blocked    := false;
  lib.hash.open.insert(io, user);

end;

function usermenu() : integer;
begin
  ClrScr;
  writeln('Menu Usuarios (1-3)');
  writeln('1- Dump de estructura del hash');
  writeln('2- Dump de estructura de control');
  writeln('3- Generar Usuarios de prueba');
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

procedure bulkInsertMessageIntoPublication(pubIdx : tPublishIdx);
var
  totalUser, pubId : integer;
  i, count         : integer;
  user, pubUser    : tUser;
  publication      : tPublish;
  found            : boolean;
  it               : tUserIterator;
begin
  metru.core.dereferencePublication(metruApp, pubIdx, publication);
  found := metru.core.retrieveFirstUser(metruApp, it);
  metru.core.retrieveUser(metruApp, publication.idUser, pubUser);
  while found do
    begin
      metru.core.retrieveUser(metruApp, it, user);
      metru.core.postMessage(metruApp, pubIdx, user,
        'Hola. Me llamo ' + user.fullname + ' - Fecha ' + DateTimeToStr(Now),
        'Gracias. Me llamo ' + pubUser.fullname + ' - Vendo : ' + publication.itemName
      );
      found := metru.core.retrieveNextUser(metruApp, it);
    end;
end;

procedure bulkInsertMessage();
var
  i, count         : integer;
  found            : boolean;
  list             : tPublishIdxList;
begin
  metru.core.setup(metruApp);
  list      := metru.core.retrieveAllPublication(metruApp);
  count     := length(list) - 1;
  found     := false;
  i         := 0;
  for i := 0 to count do
    bulkInsertMessageIntoPublication(list[i]);
  wait('presione una tecla para continuar');
end;

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
  write('idUser | ');
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
      write(message.idUser, ' | ');
      write(message.question, ' | ');
      write(message.answer, ' | ');
      write(message.timestamp, ' | ');
      write(message.next);
      writeln;
    end;
  readln;
end;

procedure dumpMessageControl();
var
  rc : lib.tree.trinary.tControlRecord;
  io : lib.tree.trinary.tTrinaryTree;
  i  : integer;
  lvl : tLevel;
begin
  lib.tree.trinary.loadTree(io, 'data/', 'messages');
  reset(io.control);
  reset(io.levels);
  seek(io.control, 0);
  read(io.control, rc);
  writeln('Raiz: ', rc.root);
  writeln('Indice de nodos eliminados: ', rc.erasedIndexes);
  writeln('Indice de mensajes eliminados: ', rc.erasedMessages);
  writeln('Ultimo nivel: ', rc.lastLevel);
  writeln('Balance threshold: ', rc.balanceThreshold);
  for i:= 0 to rc.lastLevel do
    begin
      seek(io.levels, i);
      read(io.levels, lvl);
      writeln('Nivel ', i, '::nodos = ', lvl.totalNodes);
    end;
  close(io.control);
  close(io.levels);
  readln;
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
  write('id  | ');
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
      write(idx:3, ' | ');
      write(node.id:3, ' | ');
      write(node.idUser:6, ' | ');
      write(node.parent:6, ' | ');
      write(node.first:5, ' | ');
      write(node.last:4, ' | ');
      write(node.left:4, ' | ');
      write(node.center:6, ' | ');
      write(node.right:3, ' | ');
      writeln;
    end;
  close(io.index);  
  readln;
end;

procedure dumpPrimaryMessageTree();
var
  node        : lib.tree.trinary.tNode;
  io          : lib.tree.trinary.tTrinaryTree;
  idx, maxIdx : idxRange;
begin
  lib.tree.trinary.loadTree(io, 'data/', 'messages');
  reset(io.index);
  maxIdx := filesize(io.index) - 1;
  write('pos | ');
  write('id  | ');
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
      if node.center <> -1 then
        begin
          write(idx:3, ' | ');
          write(node.id:3, ' | ');
          write(node.idUser:6, ' | ');
          write(node.parent:6, ' | ');
          write(node.first:5, ' | ');
          write(node.last:4, ' | ');
          write(node.left:4, ' | ');
          write(node.center:6, ' | ');
          write(node.right:3, ' | ');
          writeln;
        end;
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
  writeln('4- Dump de estructura de arbol primario');
  writeln('5- Emular mensajes (un mensaje de cada usuario en cada publicacion)');
  writeln('0- Salir');
  messagemenu := readValidNumber(0, 5);
end;

procedure messagemenuAct(op : integer);
begin
  case op of
    1: dumpMessageData;
    2: dumpMessageControl;
    3: dumpMessageTree;
    4: dumpPrimaryMessageTree;
    5: bulkInsertMessage;
  end;
end;

{ end message menu block }


{ publication menu block }
procedure dumpPublicationData();
var
  publication : lib.tree.avl.tPublish;
  io          : lib.tree.avl.tAVLtree;
  idx, maxIdx : idxRange;
begin
  lib.tree.avl.loadTree(io, 'data/', 'publication');
  reset(io.data);
  maxIdx := filesize(io.data) - 1;
  close(io.data);
  write('pos | ');
  write('id | ');
  write('idCategory | ');
  write('idUser | ');
  write('itemName | ');
  write('details | ');
  write('price | ');
  write('ctimestamp | ');
  write('etimestamp | ');
  write('itemType | ');
  write('status | ');
  writeln;
  for idx := 0 to maxIdx do
    begin
      publication := lib.tree.avl.fetch(io, idx);
      write(idx, ' | ');
      write(publication.id, ' | ');
      write(publication.idCategory, ' | ');
      write(publication.idUser, ' | ');
      write(publication.itemName, ' | ');
      write(publication.details, ' | ');
      write(publication.price, ' | ');
      write(publication.ctimestamp, ' | ');
      write(publication.etimestamp, ' | ');
      write(publication.itemType, ' | ');
      write(publication.status);
      writeln;
    end;
  wait('presione una tecla para continuar');
end;

procedure dumpMessageUserTree();
var
  node        : lib.tree.avl.tNode;
  io          : lib.tree.avl.tAVLtree;
  idx, maxIdx : idxRange;
begin
  lib.tree.avl.loadTree(io, 'data/', 'publication');
  reset(io.idxByUser);
  maxIdx := filesize(io.idxByUser) - 1;
  write('pos | ');
  write('key | ');
  write('index | ');
  write('parent | ');
  write('left | ');
  write('right');
  writeln;
  for idx := 0 to maxIdx do
    begin
      seek(io.idxByUser, idx);
      read(io.idxByUser, node);
      write(idx:3, ' | ');
      write(node.key:3, ' | ');
      write(node.index:3, ' | ');
      write(node.parent:3, ' | ');
      write(node.left:3, ' | ');
      write(node.right:3);
      writeln;
    end;
  close(io.idxByUser);  
  wait('presione una tecla para continuar');
end;

procedure dumpMessageCategoryTree();
var
  node        : lib.tree.avl.tNode;
  io          : lib.tree.avl.tAVLtree;
  idx, maxIdx : idxRange;
begin
  lib.tree.avl.loadTree(io, 'data/', 'publication');
  reset(io.idxByCategory);
  maxIdx := filesize(io.idxByCategory) - 1;
  write('pos | ');
  write('key | ');
  write('index | ');
  write('parent | ');
  write('left | ');
  write('right');
  writeln;
  for idx := 0 to maxIdx do
    begin
      seek(io.idxByCategory, idx);
      read(io.idxByCategory, node);
      write(idx:3, ' | ');
      write(node.key:3, ' | ');
      write(node.index:3, ' | ');
      write(node.parent:3, ' | ');
      write(node.left:3, ' | ');
      write(node.right:3);
      writeln;
    end;
  close(io.idxByCategory);  
  wait('presione una tecla para continuar');
end;

procedure dumpPublicationControl();
var
  rc : lib.tree.avl.tControlRecord;
  io : lib.tree.avl.tAVLtree;
begin
  lib.tree.avl.loadTree(io, 'data/', 'publication');
  reset(io.control);
  seek(io.control, 0);
  read(io.control, rc);
  writeln('Raiz User: ', rc.root1);
  writeln('Raiz Categoria: ', rc.root2);
  writeln('Indice de user eliminados: ', rc.erased1);
  writeln('Indice de categorias eliminados: ', rc.erased2);
  writeln('Ultimo ID: ', rc.lastID);
  close(io.control);
  readln;
end;

procedure generatePublication(); 
var
  user        : tUser;
  it          : tUserIterator;
  keep        : boolean;
  pub         : tPublish;
  list        : tCategoryList;
  cat         : tCategory;
  i, j, count : integer;
begin
  metru.core.setup(metruApp);
  keep  := metru.core.retrieveFirstUser(metruApp, it);
  list  := metru.core.retrieveAllLeafCateogies(metruApp);
  i     := 0;
  count := length(list);
  while keep do
    begin
      metru.core.retrieveUser(metruApp, it, user);
      for j:= 0 to 2 do
        begin
          metru.core.dereferenceCategory(metruApp, list[i], cat);
          pub.id         := 0;
          pub.idCategory := cat.id;
          pub.idUser     := user.id;
          pub.itemName   := 'Item generado al azar ' + user.fullname + ' ' + cat.categoryName;
          pub.details    := 'Item generado al azar';
          pub.price      := Random(100000) / 100;
          pub.ctimestamp := Now;
          pub.etimestamp := IncDay(Now, Random(8));
          if (Random(2) = 0) then
            pub.itemType   := New
          else
            pub.itemType   := Used;
          pub.status     := tStatus.Publish;
          pub.image      := EmptyStr;
          writeln('generando publication para ' + user.fullname + ' en categoria ' + cat.categoryName);
          metru.core.createPublication(metruApp, pub);
        end;
      i := (i + 1) mod count;
      keep := metru.core.retrieveNextUser(metruApp, it);
    end;
  wait('presione una tecla para continuar');
end;

function publicationmenu() : integer;
begin
  ClrScr;
  writeln('Menu Publicaciones (1-5)');
  writeln('1- Dump de estructura de datos');
  writeln('2- Dump de estructura de control');
  writeln('3- Dump de estructura de arbol de usuario');
  writeln('4- Dump de estructura de arbol de categorias');
  writeln('5- Agregar 3 publicaciones por usuario');
  writeln('0- Salir');
  publicationmenu := readValidNumber(0, 5);
end;

procedure publicationmenuAct(op : integer);
begin
  case op of
    1: dumpPublicationData;
    2: dumpPublicationControl;
    3: dumpMessageUserTree;
    4: dumpMessageCategoryTree;
    5: generatePublication;
  end;
end;
{ end publication menu block }


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

procedure resetAppAndMock;
begin
  metru.core.kickoff(metruApp);
  generateCategories;
  generateBulkUsers;
  generatePublication;
  bulkInsertMessage;
  wait('Reset realizado. Datos de prueba cargados. Presione una tecla para continuar');
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
  writeln('7- RESET GENERAL + DATOS DE PRUEBA');
  writeln('0- Salir');
  mainmenu := readValidNumber(0, 7);
end;

procedure mainmenuAct(op : integer);
begin
  case op of
    1: actMenu(@usermenu, @usermenuAct);
    2: actMenu(@categorymenu, @categorymenuAct);
    3: actMenu(@publicationmenu, @publicationmenuAct);
    4: actMenu(@messagemenu, @messagemenuAct);
    5: actMenu(@sellsmenu, @sellsmenuAct);
    6: resetApp;
    7: resetAppAndMock;
  end;
end;
{end main menu block }

begin
  Randomize;

  actMenu(@mainmenu, @mainmenuAct);
end.

