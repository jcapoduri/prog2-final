unit lib.hash.close;

interface

uses
  sysutils;

const
  NULLIDX  = -1;

type
  idxRange       = NULLIDX..MAXINT;
  tKey           = string[3];
  tNode          = record
                     key      : tKey;
                     parent   : idxRange;
                     left     : idxRange;
                     center   : idxRange;
                     right    : idxRange;
                   end;
  tControlRecord = record
                     root   : idxRange;
                     erased : idxRange;
                   end;
  tControl       = file of tControlRecord;
  tData          = file of tNode;
  tTrinaryTree   = record
                     data    : tData;
                     control : tControl;
                   end;

  procedure loadHash         (var this : tTrinaryTree; path, filename : string);
  procedure newEmptyHash     (var this : tTrinaryTree; path, filename : string);
  function  isEmpty          (var this : tTrinaryTree) : boolean;
  function  search           (var this : tTrinaryTree; key : tKey; var pos: idxRange) : boolean;
  procedure insert           (var this : tTrinaryTree; pos: idxRange; key : tKey);
  procedure remove           (var this : tTrinaryTree; pos: idxRange);
  function  fetch            (var this : tTrinaryTree; pos: idxRange) : tNode;

implementation
  { Helpers }
  procedure  _openHash (var this : tTrinaryTree);
  begin
    reset(this.data);
    reset(this.control);
  end;

  procedure  _closeHash (var this : tTrinaryTree);
  begin
    close(this.data);
    close(this.control);
  end;