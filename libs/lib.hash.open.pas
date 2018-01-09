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
  {function  isEmpty          (var this : tOpenHash) : boolean;}
  function  search           (var this : tOpenHash; key : tKey; var pos: idxRange) : boolean;
  procedure insert           (var this : tOpenHash; node : tNode);
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

  function  _getControl (var this : tOpenHash) : tControlRecord;
  var
    rc : tControlRecord;
  begin
    seek (this.control, 0);
    read (this.control, rc);
    _getControl := rc;
  end;

  procedure _setControl(var this : tOpenHash; RC : tControlRecord);
  begin
    seek(this.control, 0);
    write(this.control, RC);
  end;

  function  _get (var this : tOpenHash; pos : idxRange) : tNode;
  var
    node : tNode;
  begin
    seek(this.data, pos);
    read(this.data, node);
    _get := node;
  end;

  procedure _set (var this : tOpenHash; pos : idxRange; node : tNode);
  begin
    seek(this.data, pos);
    write(this.data, node);
  end;

  function  _hash(email : string) : tHashValue;
  var
    hashing : string;
  begin
    hashing := MD5Print(MD5String(email));
    _hash   := StrToInt64('$' + hashing) mod MAX;
  end;

  function _append (var this : tOpenHash; var item : tNode) : idxRange;
  var
    rc      : tControlRecord;
    pos     : idxRange;
    auxNode : tNode;
  begin
    rc        := _getControl(this);
    rc.lastID := rc.lastID + 1;
    pos := NULLIDX;
    item.next   := NULLIDX;
    item.id     := rc.lastID;

    if Rc.erased = NULLIDX then
      pos         := filesize(this.data)
    else
      begin
        pos         := rc.erased;
        auxNode     := _get(this, pos);
        rc.erased   := auxNode.next;
      end;
    _set(this, pos, item);
    _setControl(this, rc);
    _append := pos;
  end;

  procedure loadHash         (var this : tOpenHash; path, filename : string);
  var
    controlError, dataError : boolean;
    fullFileName : string;
    rc : tControlRecord;
    i : integer;
  begin
    fullFileName := path + filename;
    {$I-}
    //check if data file exists
    assign(this.data, fullFileName + '.dat');
    reset(this.data);
    dataError := IOResult <> 0;

    assign(this.control, fullFileName + '.ctrl');
    reset(this.control);
    controlError := IOResult <> 0;
    {$I+}

    if (controlError and dataError) then
      begin
        rewrite(this.data);
        rewrite(this.control);
        rc.erased := NULLIDX;
        rc.lastID := 0;
        for i := 0 to MAX do
          rc.hash[i] := NULLIDX;
        _setControl(this, rc);
      end;

    _closeHash(this);
  end;

  procedure newEmptyHash     (var this : tOpenHash; path, filename : string);
  var
    fullFileName : string;
    rc : tControlRecord;
    i: integer;
  begin
    fullFileName := path + filename;
    {$I-}
    assign(this.data, fullFileName + '.dat');
    assign(this.control, fullFileName + '.ctrl');
    rewrite(this.data);
    rewrite(this.control);
    rc.lastID := 0;
    rc.erased := NULLIDX;
    for i := 0 to MAX do
          rc.hash[i] := NULLIDX;
    _setControl(this, rc);
    _closeHash(this);
  end;

  function  search           (var this : tOpenHash; key : tKey; var pos: idxRange) : boolean;
  begin
    {TODO}
    _openHash(this);
    _closeHash(this);
  end;

  procedure insert           (var this : tOpenHash; node : tNode);
  var
    key : tHashValue;
    pos : idxRange;
    rc  : tControlRecord;
  begin
    {TODO}
    _openHash(this);
    key := _hash(node.email);
    pos := _append(this, node);
    rc  := _getControl(this);
    if rc.hash[key] = NULLIDX then
      rc.hash[key] := pos
    else
      begin
        node.next    := rc.hash[key];
        rc.hash[key] := pos;
      end;
    _setControl(this, rc);
    _closeHash(this);
  end;

  procedure remove           (var this : tOpenHash; pos: idxRange);
  var
    key    : tHashValue;
    auxPos : idxRange;
    node   : tNode;
    rc     : tControlRecord;
  begin
    {TODO}
    _openHash(this);
    key    := _hash(node.email);
    node   := _get(this, node, pos);
    rc     := _getControl(this);
    auxPos := rc.hash[key];
    if pos = auxPos then
      begin
        rc.hash[key] := node.next
        node.next    := rc.erased;
        rc.erased    := pos;
      end;
    {else}

    _set(this, node, pos);
    _setControl(this, rc);
    _closeHash(this);
  end;

  function  fetch            (var this : tOpenHash; pos: idxRange) : tNode;
  var
    node   : tNode;
  begin
    _openHash(this);
    node := _get(this, pos);
    _closeHash(this);
    fetch := node;
  end;
  
end.
