{Users module}
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
  tKey           = string[255];
  tNode          = record
                     id         : longint;
                     email      : tKey;
                     password   : string[64];
                     fullname   : string[255];
                     address    : string[255];
                     providence : 0..25;
                     ctimestamp : TDateTime; {creation timestamp}
                     photoUrl   : string[255];
                     status     : boolean;
                     utimestamp : TDateTime; {login timestamp}
                     next       : idxRange; {next item on the open hash}
                   end;
  tUser          = tNode;
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
    hashing := MD5Print(MD5String(email));
    _hash   := StrToInt64('$' + hashing) mod MAX;
  end;

  procedure loadHash         (var this : tOpenHash; path, filename : string);
  begin
    {TODO}
  end;

  procedure newEmptyHash     (var this : tOpenHash; path, filename : string);
  begin
    {TODO}
  end;

  function  isEmpty          (var this : tOpenHash) : boolean;
  begin
    {TODO}
  end;

  function  search           (var this : tOpenHash; key : tKey; var pos: idxRange) : boolean;
  begin
    {TODO}
  end;

  procedure insert           (var this : tOpenHash; pos: idxRange; key : tKey);
  begin
    {TODO}
  end;

  procedure remove           (var this : tOpenHash; pos: idxRange);
  begin
    {TODO}
  end;

  function  fetch            (var this : tOpenHash; pos: idxRange) : tNode;
  begin
    {TODO}
  end;
  
end.
