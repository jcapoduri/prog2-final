unit lib.hash.close;

interface

uses
  sysutils;

const
  NULLIDX  = -1;
  MAX      = 50;

type
  idxRange       = NULLIDX..MAXINT;
  tNode          = record
                     idBuyer      : longint;
                     idItem       : longint;
                     itemName     : string;
                     price        : decimal(10, 2);
                     publishDate  : timestamp;
                     sellDate     : timestamp;
                     itemType     : 1..2;
                     calification : 0..3;
                     tax          : double;
                     previous     : idxRange;
                     next         : idxRange;
                   end;
  tControlRecord = record
                     erased : idxRange;
                   end;
  tHashNode      = record
                     first : idxRange;
                     last  : idxRange;
                     total : int;
                   end;
  tControl       = file of tControlRecord;
  tData          = file of tNode;
  tCloseHash     = record
                     data    : tData;
                     control : tControl;
                   end;

  procedure loadHash         (var this : tCloseHash; path, filename : string);
  procedure newEmptyHash     (var this : tCloseHash; path, filename : string);
  function  isEmpty          (var this : tCloseHash) : boolean;
  function  search           (var this : tCloseHash; key : tKey; var pos: idxRange) : boolean;
  procedure insert           (var this : tCloseHash; pos: idxRange; key : tKey);
  procedure remove           (var this : tCloseHash; pos: idxRange);
  function  fetch            (var this : tCloseHash; pos: idxRange) : tNode;

implementation
  { Helpers }
  procedure  _openHash (var this : tCloseHash);
  begin
    reset(this.data);
    reset(this.control);
  end;

  procedure  _closeHash (var this : tCloseHash);
  begin
    close(this.data);
    close(this.control);
  end;
end.