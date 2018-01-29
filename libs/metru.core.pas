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
  tCategory       = lib.tree.lcrs.tCategory;
  tCategoryList   = lib.tree.lcrs.tCategoryList;
  tPublishIdx     = lib.tree.avl.idxRange;
  tPublish        = lib.tree.avl.tPublish;
  tPublishList    = array of tPublish;
  tPublishIdxList = array of tPublishIdx;
  tSell           = lib.hash.close.tSell;
  tSellIdx        = lib.hash.close.idxRange;
  tSellList       = array of tSell;
  tSellIdxList    = array of tSellIdx;
  tCalification   = lib.hash.close.tCalification;
  tItemType       = lib.tree.avl.tItemType;
  tStatus         = lib.tree.avl.tStatus;
  tUser           = lib.hash.open.tUser;
  tMessage        = lib.tree.trinary.tMessage;
  tMessageIdx     = lib.tree.trinary.idxRange;
  tMessageList    = array of tMessageIdx;

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
  function  login        (var this : tMetruCore; email, pass : string; var blocked : boolean) : boolean;
  function  isLogedIn    (var this : tMetruCore) : boolean;
  function  loggedUser   (var this : tMetruCore) : tUser;
  procedure logoff       (var this : tMetruCore);
  function  createUser   (var this : tMetruCore; user : tUser) : boolean;
  function  updateUser   (var this : tMetruCore; user : tUser) : boolean;
  function  banUser      (var this : tMetruCore; user : tUser) : boolean;
  function  retrieveUser (var this : tMetruCore; id : integer; var user : tUser) : boolean;

  { category functions }
  procedure createCateogry           (var this : tMetruCore; category : tCategory);
  procedure editCateogry             (var this : tMetruCore; category : tCategory);
  procedure deleteCateogry           (var this : tMetruCore; category : tCategory);
  function  retrieveAllLeafCateogies (var this : tMetruCore) : tCategoryList;
  function  retrieveBaseCateogies    (var this : tMetruCore) : tCategoryList;
  function  retrieveChildCateogies   (var this : tMetruCore; category : tCategory) : tCategoryList;
  function  retrieveAllCateogies     (var this : tMetruCore) : tCategoryList;

  { publication functions }
  procedure createPublication             (var this : tMetruCore; publication : tPublish);
  procedure editPublication               (var this : tMetruCore; publication : tPublish);
  procedure deletePublication             (var this : tMetruCore; publication : tPublish);
  function  retrieveAllPublication        (var this : tMetruCore) : tPublishIdxList;
  function  retrievePublicationByCategory (var this : tMetruCore; var category : tCategory) : tPublishIdxList;
  function  retrievePublicationByUser     (var this : tMetruCore; var user : tUser) : tPublishIdxList;
  function  dereferencePublication        (var this : tMetruCore; idx : tPublishIdx; var publication : tPublish) : boolean;

  { message related functions }
  function postMessage        (var this : tMetruCore; pubIdx : tPublishIdx; user : tUser; msg : string) : boolean;
  function postResponse       (var this : tMetruCore; var message : tMessageIdx; response : string) : boolean;
  function retrieveMessages   (var this : tMetruCore; pubIdx : tPublishIdx) : tMessageList;
  function dereferenceMessage (var this : tMetruCore; idx : tMessageIdx; var message : tMessage) : boolean;

  { sells }
  procedure doPurchase                  (var this : tMetruCore; pubIdx : tPublishIdx; user : tUser);
  procedure doPayment                   (var this : tMetruCore; sellIdx : tSellIdx);
  procedure doReviewPurchase            (var this : tMetruCore; sellIdx : tSellIdx; review : tCalification);
  function  retrievePurchaseIfAvailable (var this : tMetruCore; pubIdx : tPublishIdx; var purchase : tSellIdx) : boolean;
  function  retrieveAllMyPurchase       (var this : tMetruCore; user : tUser) : tSellIdxList;
  function  dereferenceSell             (var this : tMetruCore; idx : tSellIdx; var sell : tSell) : boolean;

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

  operator + (a, b : tPublishList) c: tPublishList;
  var
    i :longint;
  begin
    SetLength(c, Length(a) + Length(b));
    for i := 0 to High(a) do
        c[i] := a[i];
    for i := 0 to High(b) do
        c[i + Length(a)] := b[i];
  end;

  operator + (a, b : tMessageList) c: tMessageList;
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

     {user.email      := 'jcapoduri@gmail.com';
    user.password   := 'abcd1234';
    user.fullname   := 'Jorge Capoduri';
    user.address    := 'Yrigoyen 831';
    user.providence := 4;
    user.ctimestamp := Now;
    user.photoUrl   := '';
    user.status     := false;
    user.utimestamp := Now;
    user.blocked    := false;
    lib.hash.open.insert(this.io.users, user);}

    { setup categories }
    cat.categoryName := 'RAIZ';
    cat.description  := 'RAIZ';
    cat.VAT          := 0;
    cat.parent       := NULLIDX;
    cat.leftChild    := NULLIDX;
    cat.rightSibling := NULLIDX;
    lib.tree.lcrs.addSibling(this.io.categories, NULLIDX, cat);


    {cat.categoryName := 'Tecnologia';
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
    cat.rightSibling := NULLIDX;}
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
            lib.hash.open.update(this.io.users, user);
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
  begin
    this.user.status := false;
    lib.hash.open.update(this.io.users, this.user);
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

  function  retrieveUser (var this : tMetruCore; id : integer; var user : tUser) : boolean;
  var
    idxUser : lib.hash.open.tHashValue;
    found   : boolean;
  begin
    found := lib.hash.open.searchById(this.io.users, id, idxUser);
    if found then
      user := fetchByIdx(this.io.users, idxUser);
    retrieveUser := found;
  end;

  procedure  createCateogry         (var this : tMetruCore; category : tCategory);
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

  procedure  editCateogry           (var this : tMetruCore; category : tCategory);
  var
    pos : idxRange;
  begin
    lib.tree.lcrs.search(this.io.categories, category.id, pos);
    lib.tree.lcrs.update(this.io.categories, pos, category);
  end;

  procedure  deleteCateogry         (var this : tMetruCore; category : tCategory);
  var
    pos : idxRange;
  begin
    lib.tree.lcrs.search(this.io.categories, category.id, pos);
    lib.tree.lcrs.remove(this.io.categories, pos);
  end;

  function  retrieveBaseCateogies  (var this : tMetruCore) : tCategoryList;
  var
    list    : tCategoryList;
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
    auxPos  : idxRange;
  begin
    SetLength(list, 0);
    auxCat  := lib.tree.lcrs.fetch(this.io.categories, pos); 
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

  procedure createPublication         (var this : tMetruCore; publication : tPublish);
  begin
    lib.tree.avl.append(this.io.publications, publication);
  end;

  procedure editPublication           (var this : tMetruCore; publication : tPublish);
  begin
  end;
  
  procedure deletePublication         (var this : tMetruCore; publication : tPublish);
  begin
  end;

  function retrievePublicationByCategory (var this : tMetruCore; var category : tCategory) : tPublishIdxList;
  var
    list : tPublishIdxList;
    item : tPublishIdx;
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

  function retrievePublicationByUser (var this : tMetruCore; var user : tUser) : tPublishIdxList;
  var
    list : tPublishIdxList;
    item : tPublishIdx;
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

  function  dereferencePublication        (var this : tMetruCore; idx : tPublishIdx; var publication : tPublish) : boolean;
  var
    found : boolean;
  begin
    found       := true;
    publication := lib.tree.avl.fetch(this.io.publications, idx);
    dereferencePublication := found;
  end;

  function retrieveAllPublication        (var this : tMetruCore) : tPublishIdxList;
  var
    list, auxList : tPublishIdxList;
    i             : integer;
    category      : tCategory;
    catList       : tCategoryList;
  begin
    SetLength(list, 0);
    catList := retrieveAllLeafCateogies(this);
    for i := 0 to High(catList) do
      begin
        category := catList[i];
        auxList  := retrievePublicationByCategory(this, category);
        list     := list + auxList;
      end;
    retrieveAllPublication := list;
  end;
  
  function postMessage  (var this : tMetruCore; pubIdx : tPublishIdx; user : tUser; msg : string) : boolean;
  var
    message : tMessage;
  begin

  end;

  function postResponse (var this : tMetruCore; var message : tMessageIdx; response : string) : boolean;
  begin
  end;  

  function retrieveMessages   (var this : tMetruCore; pubIdx : tPublishIdx) : tMessageList;
  begin
  end;

  function dereferenceMessage (var this : tMetruCore; idx : tMessageIdx; var message : tMessage) : boolean;
  var
    found : boolean;
  begin
    found   := true;
    message := lib.tree.trinary.fetchMessage(this.io.messages, idx);
    dereferenceMessage := found;
  end;

  function _retrieveAllMessagesByKeys (var this : tMetruCore; pk, sk : lib.tree.trinary.tKey) : tMessageList;
  var
    msgIdx : lib.tree.trinary.idxRange;
    msg    : lib.tree.trinary.tMessage;
    list   : tMessageList;
    found  : boolean;
    i      : integer;
  begin
    SetLength(list, 0);
    i     := 0;
    found := retrieveFirstMsgIdx(this.io.messages, pk, sk, msgIdx);
    if found then
      repeat
        begin
          i   := i + 1;
          SetLength(list, i);
          list[i] := msgIdx;
        end;
      until not retrieveNextMsgIdx(this.io.messages, pk, sk, msgIdx);
  end;

  function retrieveMessage (var this : tMetruCore; pubIdx : tPublishIdx) : tMessageList;
  var 
    linkedKeys  : tLinkedKeys;
    list        : tMessageList;
    publication : tPublish;
  begin
    dereferencePublication(this, pubIdx, publication);
    linkedKeys := lib.tree.trinary.retrieveAllLinkedKeys(this.io.messages, publication.id);

  end;

  procedure doPurchase            (var this : tMetruCore; pubIdx : tPublishIdx; user : tUser);
  var
    purchase    : tSell;
    category    : tCategory;
    pos         : lib.tree.lcrs.idxRange;
    publication : tPublish;
  begin
    { TODO: mark publication as sold }
    metru.core.dereferencePublication(this, pubIdx, publication);

    lib.tree.lcrs.search(this.io.categories, publication.idCategory, pos);
    category                  := lib.tree.lcrs.fetch(this.io.categories, pos);
    purchase.idBuyer          := user.id;
    purchase.idItem           := publication.id;
    purchase.itemName         := publication.itemName;
    purchase.price            := publication.price;
    purchase.publishDate      := publication.ctimestamp;
    purchase.sellDate         := Now;
    if publication.itemType = lib.tree.avl.New then
      purchase.itemType       := lib.hash.close.New
    else
      purchase.itemType       := lib.hash.close.Used;
    purchase.calification     := lib.hash.close.None;
    purchase.tax              := category.VAT;
    purchase.alreadyCollected := false;

    lib.hash.close.insert(this.io.sells, purchase);

    publication.status        :=  lib.tree.avl.Sold;
    lib.tree.avl.update(this.io.publications, publication);
  end;

  procedure doPayment             (var this : tMetruCore; sellIdx : tSellIdx);
  var
    auxPurchase : tSell;
    idxPurchase : lib.hash.close.idxRange;
  begin
    if dereferenceSell(this, sellIdx, auxPurchase) then
      begin
        auxPurchase.alreadyCollected := true;
        lib.hash.close.update(this.io.sells, idxPurchase, auxPurchase);
      end;
  end;

  procedure doReviewPurchase      (var this : tMetruCore; sellIdx : tSellIdx; review : tCalification);
  var
    auxPurchase : tSell;
    idxPurchase : lib.hash.close.idxRange;

  begin
    if (review > None) and (dereferenceSell(this, sellIdx, auxPurchase)) then
      begin
        auxPurchase.calification := review;
        lib.hash.close.update(this.io.sells, idxPurchase, auxPurchase);
      end;
  end;

  function  retrievePurchaseIfAvailable (var this : tMetruCore; pubIdx : tPublishIdx; var purchase : tSellIdx) : boolean;
  var
    found : boolean;
    idx   : lib.hash.close.idxRange;
    publication : tPublish;
  begin
    dereferencePublication(this, pubIdx, publication);
    found    := lib.hash.close.search(this.io.sells, publication.id, purchase);
    retrievePurchaseIfAvailable := found;  
  end;

  function  retrieveAllMyPurchase (var this : tMetruCore; user : tUser) : tSellIdxList;
  var 
    list        : tSellIdxList;
    purchase    : tSell;
    pubList     : tPublishIdxList;
    i, j        : integer;
    publication : tPublish;
    idxPurchase : lib.hash.close.idxRange;
  begin
    j := 0;
    SetLength(list, 0);
    pubList := retrieveAllPublication(this);
    for i := 0 to high(pubList) do
      begin
        dereferencePublication(this, pubList[i], publication);
        if lib.hash.close.search(this.io.sells, publication.id, idxPurchase) then
          begin
            purchase := lib.hash.close.fetch(this.io.sells, idxPurchase);
            if purchase.idBuyer = user.id then
              begin
                j := j + 1;
                SetLength(list, j);
                list[j - 1] := idxPurchase;
              end;
          end;
      end;

    retrieveAllMyPurchase := list;
  end;

  function  dereferenceSell             (var this : tMetruCore; idx : tSellIdx; var sell : tSell) : boolean;
  var
    found : boolean;
  begin
    found := true;
    sell  := lib.hash.close.fetch(this.io.sells, idx);
    dereferenceSell := found;
  end;
end.
