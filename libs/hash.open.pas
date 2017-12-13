unit lib.hash.open;

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
  tOpenHash      = record
                     data    : tData;
                     control : tControl;
                   end;

  procedure loadHash         (var this : tOpenHash; path, filename : string);
  procedure newEmptyHash     (var this : tOpenHash; path, filename : string);
  function  isEmpty          (var this : tOpenHash) : boolean;
  function  search           (var this : tOpenHash; key : tKey; var pos: idxRange) : boolean;
  procedure insert           (var this : tOpenHash; pos: idxRange; key : tKey);
  procedure remove           (var this : tOpenHash; pos: idxRange);
  function  fetch            (var this : tOpenHash; pos: idxRange) : tNode;

implementation
  { Helpers }
  procedure  _openHash (var this : tOpenHash);
  begin
    reset(this.data);
    reset(this.control);
  end;

  procedure  _closeHash (var this : tOpenHash);
  begin
    close(this.data);
    close(this.control);
  end;
end.