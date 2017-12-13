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
                     right    : idxRange;
                   end;
  tControlRecord = record
                     root   : idxRange;
                     erased : idxRange;
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