unit lib.hash.open;

interface

uses
  sysutils, md5;

const
  NULLIDX  = -1;
  MAX      = 80;

type
  idxRange       = NULLIDX..MAXINT;
  tHashValue     = 0..MAX;
  tNode          = record
                     id         : longint;
                     email      : string;
                     password   : string;
                     fullname   : string;
                     address    : string;
                     providence : 0..25;
                     ctimestamp : timestamp; {creation timestamp}
                     photoUrl   : string;
                     status     : boolean;
                     utimestamp : timestamp; {login timestamp}
                     next       : idxRange; {next item on the open hash}
                   end;
  tControlRecord = record
                     hash   : array [tHashValue] of idxRange;
                     lastID : longint;
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

  function  _hash(email : string) : tHashValue;
  var
    hashing : string;
  begin
    hashing := MD5String(email);
    _hash   := Hex2Dec(hashing) mod MAX;
  end;
end.