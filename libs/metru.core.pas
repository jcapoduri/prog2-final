unit metru.core;

interface

uses
  sysutils
  ,lib.tree.lcrs    in 'libs\lib.tree.lcrs.pas'
  ,lib.hash.open    in 'libs\lib.hash.open.pas'
  ,lib.hash.close   in 'libs\lib.hash.close.pas'
  ,lib.tree.trinary in 'libs\lib.tree.trinary.pas'
  ;

const
  NULLIDX      = -1;
  BASEPATH     = 'data/';
  USERFILE     = 'users';
  CATEGORYFILE = 'categories';
  SELLSFILE    = 'sells';
  MESSAGEFILE  = 'messages';

type
  tMetruIO =   record
                 users      : tOpenHash;
                 categories : tLCRStree;
                 messages   : tTrinaryTree;
                 sells      : tCloseHash;
               end;

  tMetruCore = record
                 io      : tMetruIO;
                 user    : tUser;
                 isAdmin : boolean;
               end;

  { start up functions }
  procedure setup   (var this : tMetruCore);
  procedure kickoff (var this : tMetruCore);

  { User functions }
  function  login      (var this : tMetruCore; email, pass : string) : boolean;
  function  isLogedIn  (var this : tMetruCore) : boolean;
  function  loggedUser (var this : tMetruCore) : tUser;
  procedure logoff     (var this : tMetruCore);
  function  createUser (var this : tMetruCore; tUser : user) : boolean;
  function  updateUser (var this : tMetruCore; tUser : user) : boolean;
  function  banUser    (var this : tMetruCore; tUser : user) : boolean;

  { category functions }

var
  metruApp : tMetruCore;

implementation
  procedure setup   (var this : tMetruCore);
  begin
    lib.hash.open.loadHash(this.io.users, BASEPATH, CATEGORYFILE);
    lib.tree.lcrs.loadTree(this.io.categories, BASEPATH, CATEGORYFILE);
    lib.tree.trinary.loadTree(this.io.messages, BASEPATH, MESSAGEFILE);
    lib.hash.close.loadHash(this.io.sells, BASEPATH, SELLSFILE);
  end;

  procedure kickoff (var this : tMetruCore);
  begin
    lib.hash.open.newEmptyHash(this.io.users, BASEPATH, CATEGORYFILE);
    lib.tree.lcrs.newEmptyTree(this.io.categories, BASEPATH, CATEGORYFILE);
    lib.tree.trinary.newEmptyTree(this.io.messages, BASEPATH, MESSAGEFILE);
    lib.hash.close.newEmptyHash(this.io.sells, BASEPATH, SELLSFILE);
  end;

  function  login      (var this : tMetruCore; email, pass : string) : boolean;
  var
    found : boolean;
    pos   : idxRange;
    user  : tUser;
  begin
    found := this.hash.open.search(this.io.users, email, pos);
    if (found) then
      begin
        user := this.hash.open.fetch(this.io.users, pos);
        if (this.user.password = pass) then
          begin
            user.status := true;
            lib.hash.open.insert(this.io.users, user);
            this.user   := user;
          end;
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
    user : tUser
    pos  : idxRange;
  begin
    this.user.status := false;
    lib.hash.open.insert(this.io.users, this.user);
    this.user.id := 0;
  end;

  function  createUser (var this : tMetruCore; tUser : user) : boolean;
  var 
    pos : tHashValue;
    ok  : boolean;
  begin
    ok := not lib.hash.open.search(this.io.users, user.email, pos);
    if ok then
      lib.hash.open.insert(this.io.users, user);        
    createUser := ok;
  end;

  function  updateUser (var this : tMetruCore; tUser : user) : boolean;
  var 
    pos : tHashValue;
    ok  : boolean;
  begin
    ok := lib.hash.open.search(this.io.users, user.email, pos);
    if ok then
      lib.hash.open.insert(this.io.users, user);        
    createUser := ok;
  end;
  
  function  banUser    (var this : tMetruCore; tUser : user) : boolean;
  var 
    pos : tHashValue;
    ok  : boolean;
  begin
    ok := lib.hash.open.search(this.io.users, user.email, pos);
    if ok then
      {TODO : search if has active publication}
      lib.hash.open.remove(this.io.users, user);        
      }
    createUser := ok;
  end;
  
end.
