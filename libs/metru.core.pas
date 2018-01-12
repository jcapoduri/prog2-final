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
  function  login      (var this : tMetruCore, email, pass : string) : boolean;
  function  isLogedIn  (var this : tMetruCore) : boolean;
  function  loggedUser (var this : tMetruCore) : tUser;
  procedure logoff     (var this : tMetruCore);

  { category functions }

var
  metruApp : tMetruCore;

implementation
  procedure setup   (var this : tMetruCore);
  begin
    lib.hash.open.loadHash(this.io.users, BASEPATH, CATEGORYFILE);
    lib.tree.lcrs.loadTree(this.io.categories, BASEPATH, CATEGORYFILE);
    lib.tree.trinary.loadTree(this.io.messages, BASEPATH, MESSAGEFILE);
    lib.tree.close.loadHash(this.io.sells, BASEPATH, SELLSFILE);
  end;

  procedure kickoff (var this : tMetruCore);
  begin
    lib.hash.open.newEmptyHash(this.io.users, BASEPATH, CATEGORYFILE);
    lib.tree.lcrs.newEmptyTree(this.io.categories, BASEPATH, CATEGORYFILE);
    lib.tree.trinary.newEmptyTree(this.io.messages, BASEPATH, MESSAGEFILE);
    lib.tree.close.newEmptyHash(this.io.sells, BASEPATH, SELLSFILE);
  end;

end.