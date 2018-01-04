{Sells module}
unit lib.hash.close;

interface

uses
  sysutils;

const
  NULLIDX  = -1;
  MAX      = 50;

type
  tItemType      = (New, Used);
  tCalification  = (None, Good, Neutral, Bad);
  idxRange       = NULLIDX..MAXINT;
  tKey           = longint;
  tNode          = record
                     idBuyer      : longint;
                     idItem       : tKey;
                     itemName     : string[255];
                     price        : Currency;
                     publishDate  : TDateTime;
                     sellDate     : TDateTime;
                     itemType     : tItemType;
                     calification : tCalification;
                     tax          : double;
                     previous     : idxRange;
                     next         : idxRange;
                   end;
  tSell          = tNode;
  tControlRecord = record
                     erased : idxRange;
                   end;
  tHashNode      = record
                     first : idxRange;
                     last  : idxRange;
                     total : integer;
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

  procedure loadHash         (var this : tCloseHash; path, filename : string);
  begin
    {TODO}
  end;

  procedure newEmptyHash     (var this : tCloseHash; path, filename : string);
  begin
    {TODO}
  end;

  function  isEmpty          (var this : tCloseHash) : boolean;
  begin
    {TODO}
  end;

  function  search           (var this : tCloseHash; key : tKey; var pos: idxRange) : boolean;
  begin
    {TODO}
  end;

  procedure insert           (var this : tCloseHash; pos: idxRange; key : tKey);
  begin
    {TODO}
  end;

  procedure remove           (var this : tCloseHash; pos: idxRange);
  begin
    {TODO}
  end;

  function  fetch            (var this : tCloseHash; pos: idxRange) : tNode;
  begin
    {TODO}
  end;
  
end.
