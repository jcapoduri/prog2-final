unit metru.core;

interface

uses
  sysutils
  ,lib.tree.lcrs    in 'libs\lib.tree.lcrs.pas'
  ,lib.tree.avl     in 'libs\lib.tree.avl.pas'
  ,lib.tree.trinary in 'libs\lib.tree.trinary.pas'
  ,lib.hash.open    in 'libs\lib.hash.open.pas'
  ,lib.hash.close   in 'libs\lib.hash.close.pas'
  ;

const
  NULLIDX          = -1;
  BASEPATH         = 'data/';
  USERFILE         = 'users';
  CATEGORYFILE     = 'categories';
  SELLSFILE        = 'sells';
  MESSAGEFILE      = 'messages';
  PUBLICATIONFILE  = 'publication';

type
  { expose data records only }
  tCategoryList = lib.tree.lcrs.tCategoryList;
  tCategory     = lib.tree.lcrs.tCategory;
  tPublish      = lib.tree.avl.tPublish;
  tPublishList  = array of tPublish;
  tItemType     = lib.tree.avl.tItemType;
  tStatus       = lib.tree.avl.tStatus;
  tUser         = lib.hash.open.tUser;
  tMessage      = lib.tree.trinary.tMessage;

  tMetruIO =   record
                 users        : tOpenHash;
                 categories   : tLCRStree;
                 messages     : tTrinaryTree;
                 sells        : tCloseHash;
                 publications : tAVLtree;
               end;

  tMetruCore = record
                 io      : tMetruIO;
                 user    : tUser;
                 isAdmin : boolean;
               end;

  operator + (a, b : tCategoryList) c: tCategoryList;

  { start up functions }
  procedure setup   (var this : tMetruCore);
  procedure kickoff (var this : tMetruCore);

  { User functions }
  function  login      (var this : tMetruCore; email, pass : string; var blocked : boolean) : boolean;
  function  isLogedIn  (var this : tMetruCore) : boolean;
  function  loggedUser (var this : tMetruCore) : tUser;
  procedure logoff     (var this : tMetruCore);
  function  createUser (var this : tMetruCore; user : tUser) : boolean;
  function  updateUser (var this : tMetruCore; user : tUser) : boolean;
  function  banUser    (var this : tMetruCore; user : tUser) : boolean;

  { category functions }
  function  createCateogry         (var this : tMetruCore; category : tCategory) : boolean;
  function  editCateogry           (var this : tMetruCore; category : tCategory) : boolean;
  function  deleteCateogry         (var this : tMetruCore; category : tCategory) : boolean;
  function  retrieveAllLeafCateogies   (var this : tMetruCore) : tCategoryList;
  function  retrieveBaseCateogies  (var this : tMetruCore) : tCategoryList;
  function  retrieveChildCateogies (var this : tMetruCore; category : tCategory) : tCategoryList;
  function  retrieveAllCateogies   (var this : tMetruCore) : tCategoryList;

  { publication functions }
  function createPublication             (var this : tMetruCore; publication : tPublish) : boolean;
  function editPublication               (var this : tMetruCore; publication : tPublish) : boolean;
  function deletePublication             (var this : tMetruCore; publication : tPublish) : boolean;
  function retrievePublicationByCategory (var this : tMetruCore; var category : tCategory) : tPublishList;
  function retrievePublicationByUser     (var this : tMetruCore; var user : tUser) : tPublishList;


var
  metruApp : tMetruCore;

implementation
  operator + (a, b : tCategoryList) c: tCategoryList;
  var
    i :longint;
  begin
    SetLength(c, Length(a) + Length(b));
    for i := 0 to High(a) do
        c[i] := a[i];
    for i := 0 to High(b) do
        c[i + Length(a)] := b[i];
  end;

  procedure setup   (var this : tMetruCore);
  begin
    lib.hash.open.loadHash(this.io.users, BASEPATH, USERFILE);
    lib.tree.lcrs.loadTree(this.io.categories, BASEPATH, CATEGORYFILE);
    lib.tree.trinary.loadTree(this.io.messages, BASEPATH, MESSAGEFILE);
    lib.hash.close.loadHash(this.io.sells, BASEPATH, SELLSFILE);
    lib.tree.avl.loadTree(this.io.publications, BASEPATH, PUBLICATIONFILE);
  end;

  procedure kickoff (var this : tMetruCore);
  var
    user : tUser;
    cat  : tCategory;
  begin
    lib.hash.open.newEmptyHash(this.io.users, BASEPATH, USERFILE);
    lib.tree.lcrs.newEmptyTree(this.io.categories, BASEPATH, CATEGORYFILE);
    lib.tree.trinary.newEmptyTree(this.io.messages, BASEPATH, MESSAGEFILE);
    lib.hash.close.newEmptyHash(this.io.sells, BASEPATH, SELLSFILE);
    lib.tree.avl.newEmptyTree(this.io.publications, BASEPATH, PUBLICATIONFILE);
    { setup users}
    user.email      := 'admistrador@mercatrucho.com';
    user.password   := 'palo_y_a_la_bolsa';
    user.fullname   := 'Administrador';
    user.address    := '';
    user.providence := 0;
    user.ctimestamp := Now;
    user.photoUrl   := '';
    user.status     := false;
    user.utimestamp := Now;
    user.blocked    := false;
    lib.hash.open.insert(this.io.users, user);

    { setup categories }
    cat.categoryName := 'RAIZ';
    cat.description  := 'RAIZ';
    cat.VAT          := 0;
    cat.parent       := NULLIDX;
    cat.leftChild    := NULLIDX;
    cat.rightSibling := NULLIDX;
    lib.tree.lcrs.addSibling(this.io.categories, NULLIDX, cat);


    cat.categoryName := 'Tecnologia';
    cat.description  := 'Tecnologia';
    cat.VAT          := 10;
    cat.parent       := 1;
    cat.leftChild    := NULLIDX;
    cat.rightSibling := NULLIDX;
    createCateogry(metruApp, cat);

    cat.categoryName := 'Electrodomestico';
    cat.description  := 'Electrodomestico';
    cat.VAT          := 10;
    cat.parent       := 1;
    cat.leftChild    := NULLIDX;
    cat.rightSibling := NULLIDX;
    createCateogry(metruApp, cat);

    cat.categoryName := 'Computadoras';
    cat.description  := 'Computadoras';
    cat.VAT          := 10;
    cat.parent       := 2;
    cat.leftChild    := NULLIDX;
    cat.rightSibling := NULLIDX;
    createCateogry(metruApp, cat);

    cat.categoryName := 'Aires';
    cat.description  := 'Aires';
    cat.VAT          := 10;
    cat.parent       := 3;
    cat.leftChild    := NULLIDX;
    cat.rightSibling := NULLIDX;
    createCateogry(metruApp, cat);
  end;

  function  login      (var this : tMetruCore; email, pass : string; var blocked : boolean) : boolean;
  var
    found : boolean;
    pos   : tHashValue;
    user  : tUser;
  begin
    blocked := false;
    found   := lib.hash.open.search(this.io.users, email, pos);
    if (found) then
      begin
        user    := lib.hash.open.fetch(this.io.users, email);
        blocked := user.blocked;
        if (not blocked) and (user.password = pass) then
          begin
            user.status := true;
            lib.hash.open.insert(this.io.users, user);
            this.user   := user;
          end
        else
          found := false;
      end;
    login := found;
  end;

  function  isLogedIn  (var this : tMetruCore) : boolean;
  begin
    isLogedIn := this.user.id > 0;
  end;

  function  loggedUser (var this : tMetruCore) : tUser;
  begin
    loggedUser := this.user;
  end;

  procedure logoff     (var this : tMetruCore);
  var 
    user : tUser;
    pos  : idxRange;
  begin
    this.user.status := false;
    lib.hash.open.insert(this.io.users, this.user);
    this.user.id := 0;
  end;

  function  createUser (var this : tMetruCore; user : tUser) : boolean;
  var 
    pos : tHashValue;
    ok  : boolean;
  begin
    user.id         := 0; 
    user.ctimestamp := Now;
    user.status     := false;
    user.utimestamp := Now;
    user.blocked    := false;
    ok              := not lib.hash.open.search(this.io.users, user.email, pos);
    if ok then
      lib.hash.open.insert(this.io.users, user);        
    createUser := ok;
  end;

  function  updateUser (var this : tMetruCore; user : tUser) : boolean;
  var 
    pos : tHashValue;
    ok  : boolean;
  begin
    ok := lib.hash.open.search(this.io.users, user.email, pos);
    if ok then
      lib.hash.open.insert(this.io.users, user);        
    updateUser := ok;
  end;
  
  function  banUser    (var this : tMetruCore; user : tUser) : boolean;
  var 
    pos : tHashValue;
    ok  : boolean;
  begin
    ok := lib.hash.open.search(this.io.users, user.email, pos);
    if ok then
      {TODO : search if has active publication}
      lib.hash.open.remove(this.io.users, user);
    banUser := ok;
  end;

  function  createCateogry         (var this : tMetruCore; category : tCategory) : boolean;
  var
    pos : idxRange;
  begin
    if (category.parent = NULLIDX) then
      pos := lib.tree.lcrs.root(this.io.categories)
    else
      lib.tree.lcrs.search(this.io.categories, category.parent, pos);
    category.parent := pos;
    lib.tree.lcrs.addChild(this.io.categories, pos, category);
  end;

  function  editCateogry           (var this : tMetruCore; category : tCategory) : boolean;
  var
    pos : idxRange;
  begin
    lib.tree.lcrs.search(this.io.categories, category.id, pos);
    lib.tree.lcrs.update(this.io.categories, pos, category);
  end;

  function  deleteCateogry         (var this : tMetruCore; category : tCategory) : boolean;
  var
    pos : idxRange;
  begin
    lib.tree.lcrs.search(this.io.categories, category.id, pos);
    lib.tree.lcrs.remove(this.io.categories, pos);
  end;

  function  retrieveBaseCateogies  (var this : tMetruCore) : tCategoryList;
  var
    list    : tCategoryList;
    auxList : tCategoryList;
    auxCat  : tCategory;
    i       : integer;
    auxPos  : idxRange;
  begin
    auxPos  := lib.tree.lcrs.root(this.io.categories);
    auxCat  := lib.tree.lcrs.fetch(this.io.categories, auxPos);
    auxPos  := lib.tree.lcrs.firstChild(this.io.categories, auxPos);
    i := 0;
    while (auxPos <> NULLIDX) do
      begin
        auxCat      := lib.tree.lcrs.fetch(this.io.categories, auxPos);
        i           := i + 1;
        SetLength(list, i);
        list[i - 1] := auxCat;
        auxPos      := lib.tree.lcrs.nextSibling(this.io.categories, auxPos);
      end;
    retrieveBaseCateogies := list;
  end;

  function  retrieveChildCateogies (var this : tMetruCore; category : tCategory) : tCategoryList;
  var
    list    : tCategoryList;
    auxList : tCategoryList;
    auxCat  : tCategory;
    i       : integer;
    auxPos  : idxRange;
  begin
    SetLength(list, 0);
    lib.tree.lcrs.search(this.io.categories, category.id, auxPos);
    auxPos := lib.tree.lcrs.firstChild(this.io.categories, auxPos);
    i := 0;
    while (auxPos <> NULLIDX) do
      begin
        auxCat      := lib.tree.lcrs.fetch(this.io.categories, auxPos);
        i           := i + 1;
        SetLength(list, i);
        list[i - 1] := auxCat;
        auxPos      := lib.tree.lcrs.nextSibling(this.io.categories, auxPos);
      end;
    retrieveChildCateogies := list;
  end;

  function  retrieveAllCateogies   (var this : tMetruCore) : tCategoryList;
  var
    list    : tCategoryList;
    auxList : tCategoryList;
    auxCat  : tCategory;
    i       : integer;
    auxPos  : idxRange;
  begin
    SetLength(list, 1);
    auxPos  := lib.tree.lcrs.root(this.io.categories);
    auxCat  := lib.tree.lcrs.fetch(this.io.categories, auxPos); 
    list[0] := auxCat;
    i       := 0;
    while (Length(list) > i) do
      begin
        auxCat  := list[i];
        auxList := retrieveChildCateogies(this, auxCat);
        if (Length(auxList) > 0) then
          list := list + auxList;
        i       := i + 1;
      end;
    retrieveAllCateogies := list;
  end;

  function  _retrieveAllLeafCateogies  (var this : tMetruCore; pos : idxRange) : tCategoryList;
  var
    list    : tCategoryList;
    auxList : tCategoryList;
    auxCat  : tCategory;
    i       : integer;
    auxPos  : idxRange;
  begin
    SetLength(list, 0);
    auxCat  := lib.tree.lcrs.fetch(this.io.categories, pos); 
    i       := 0;
    if (lib.tree.lcrs.isLeaf(this.io.categories, auxCat)) then
      begin
        SetLength(list, 1);
        list[0] := auxCat;
      end
    else
      begin
        auxPos := lib.tree.lcrs.firstChild(this.io.categories, pos);
        if (auxPos <> NULLIDX) then
          begin
            auxList := _retrieveAllLeafCateogies(this, auxPos);
            list    := list + auxList;
          end;
        auxPos := lib.tree.lcrs.nextSibling(this.io.categories, pos);  
        if (auxPos <> NULLIDX) then
          begin
            auxList := _retrieveAllLeafCateogies(this, auxPos);
            list    := list + auxList;
          end;
      end;
    _retrieveAllLeafCateogies := list;
  end;  

  function  retrieveAllLeafCateogies   (var this : tMetruCore) : tCategoryList;
  var
    auxPos  : idxRange;
  begin
    auxPos  := lib.tree.lcrs.root(this.io.categories);
    retrieveAllLeafCateogies := _retrieveAllLeafCateogies(this, auxPos);

  end;

  function createPublication         (var this : tMetruCore; publication : tPublish) : boolean;
  begin
    lib.tree.avl.append(this.io.publications, publication);
  end;

  function editPublication           (var this : tMetruCore; publication : tPublish) : boolean;
  begin
  end;
  
  function deletePublication         (var this : tMetruCore; publication : tPublish) : boolean;
  begin
  end;

  function retrievePublicationByCategory (var this : tMetruCore; var category : tCategory) : tPublishList;
  var
    list : tPublishList;
    item : tPublish;
    idx  : idxRange;
    i    : integer;
  begin
    idx := NULLIDX;
    i   := 0;
    while lib.tree.avl.searchByCategory(this.io.publications, category.id, idx) do
      begin
        item := lib.tree.avl.fetchByCategory(this.io.publications, idx);
        i    := i + 1;
        SetLength(list, i);
        list[i - 1] := item;
      end;
    retrievePublicationByCategory := list;
  end;

  function retrievePublicationByUser (var this : tMetruCore; var user : tUser) : tPublishList;
  var
    list : tPublishList;
    item : tPublish;
    idx  : idxRange;
    i    : integer;
  begin
    idx := NULLIDX;
    i   := 0;
    while lib.tree.avl.searchByUser(this.io.publications, user.id, idx) do
      begin
        item := lib.tree.avl.fetchByUser(this.io.publications, idx);
        i    := i + 1;
        SetLength(list, i);
        list[i - 1] := item;
      end;
    retrievePublicationByUser := list;
  end;
  
end.
