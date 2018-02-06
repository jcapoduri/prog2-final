{Users module}
unit lib.hash.open;

interface

uses
  sysutils, md5;

const
  NULLIDX     = -1;
  MAX         = 80;
  defaultPass = 'palo_y_a_la_bolsa';

type
  idxRange       = NULLIDX..MAXINT;
  tHashValue     = NULLIDX..MAX;
  tKey           = string[255];
  tNode          = record
                     id         : longint;
                     email      : tKey;
                     password   : string[64];
                     fullname   : string[255];
                     address    : string[255];
                     providence : 0..25;
                     status     : boolean;
                     ctimestamp : TDateTime; {creation timestamp}
                     photoUrl   : string[255];
                     blocked    : boolean;
                     utimestamp : TDateTime; {login timestamp}                     
                   end;
  tUser          = tNode;
  tControlRecord = record
                     lastID : longint;
                     count  : tHashValue;
                   end;
  tControl       = file of tControlRecord;
  tData          = file of tNode;
  tOpenHash      = record
                     data    : tData;
                     control : tControl;
                   end;

  procedure loadHash     (var this : tOpenHash; path, filename : string);
  procedure newEmptyHash (var this : tOpenHash; path, filename : string);
  function  hash         (var this : tOpenHash; email : tKey) : tHashValue;
  function  search       (var this : tOpenHash; email : tKey; var pos: tHashValue) : boolean;
  function  searchById   (var this : tOpenHash; id :longint; var pos: tHashValue) : boolean;
  procedure insert       (var this : tOpenHash; node : tNode);
  procedure update       (var this : tOpenHash; node : tNode);
  procedure remove       (var this : tOpenHash; node : tNode);
  function  fetch        (var this : tOpenHash; email: tKey) : tNode;
  function  fetchByIdx   (var this : tOpenHash; pos: tHashValue) : tNode;
  function  fetchFirst   (var this : tOpenHash; var pos: idxRange) : boolean;
  function  fetchNext    (var this : tOpenHash; var pos: idxRange) : boolean;

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

  procedure _setControl(var this : tOpenHash; rc : tControlRecord);
  begin
    seek(this.control, 0);
    write(this.control, rc);
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

  function  _hash(email : tKey) : tHashValue;
  var
    hashing : string;
  begin
    hashing := MD5Print(MD5String(email));
    _hash   := StrToInt64(LeftStr('$' + hashing, 16)) mod MAX;
  end;

  function  hash(var this : tOpenHash; email : tKey) : tHashValue;
  begin
    hash := _hash(email);
  end;

  function _nextPos(pos : tHashValue) : tHashValue;
  begin
    _nextPos := (pos + 1) mod (MAX + 1);
  end;

  function _getBucket(var this : tOpenHash; email : tKey) : tHashValue;
  var
    pos  : tHashValue;
    node : tNode;
  begin
    pos  := _hash(email);
    node := _get(this, pos);
    while (node.id <> NULLIDX) do
      begin
        pos  := _nextPos(pos);
        node := _get(this, pos);
      end;
    _getBucket := pos;
  end;

  procedure loadHash         (var this : tOpenHash; path, filename : string);
  var
    controlError, dataError : boolean;
    fullFileName            : string;
    rc                      : tControlRecord;
    i                       : integer;
    node                    : tNode;
  begin
    fullFileName := path + filename;
    {$I-}
    //check if data file exists
    assign(this.data, fullFileName + '.dat');
    reset(this.data);
    dataError := IOResult <> 0;

    assign(this.control, fullFileName + '.con');
    reset(this.control);
    controlError := IOResult <> 0;
    {$I+}

    if (controlError and dataError) then
      begin
        rewrite(this.data);
        rewrite(this.control);
        rc.count  := 0;
        rc.lastID := 0;
        node.id   := NULLIDX;
        for i := 0 to MAX do
          _set(this, i, node);
        _setControl(this, rc);
      end;

    _closeHash(this);
  end;

  procedure newEmptyHash     (var this : tOpenHash; path, filename : string);
  var
    fullFileName : string;
    rc           : tControlRecord;
    i            : integer;
    node         : tNode;
  begin
    fullFileName := path + filename;
    {$I-}
    assign(this.data, fullFileName + '.dat');
    assign(this.control, fullFileName + '.con');
    rewrite(this.data);
    rewrite(this.control);
    rc.lastID := 0;
    rc.count  := 0;
    node.id   := NULLIDX;
    for i := 0 to MAX do
      _set(this, i, node);
    _setControl(this, rc);
    _closeHash(this);
  end;

  function  _search          (var this : tOpenHash; email : tKey; var pos: tHashValue) : boolean;
  var
    rc      : tControlRecord;
    found   : boolean;
    auxPos  : tHashValue;
    node    : tNode;
  begin
    rc      := _getControl(this);
    found   := false;
    pos     := NULLIDX;
    if (rc.count > 0) then
      begin
        pos    := _hash(email);
        node   := _get(this, pos);
        if (node.email = email) then
          found := true
        else
          begin
            auxPos := _nextPos(pos);
            while (auxPos <> pos) and (not found) do
              begin
                node := _get(this, auxPos);
                if (node.email = email) then
                  found := true
                else
                  auxPos := _nextPos(auxPos);
              end;
          end;
      end;

    _search := found;
  end;

  function  search           (var this : tOpenHash; email : tKey; var pos: tHashValue) : boolean;
  var
    found   : boolean;
  begin
    _openHash(this);
    found   := _search(this, email, pos);
    _closeHash(this);
    search := found;
  end;

  function  searchById   (var this : tOpenHash; id :longint; var pos: tHashValue) : boolean;
  var
    found   : boolean;
    auxPos  : integer; 
    node    : tNode;   
  begin
    _openHash(this);
    found   := false;
    auxPos  := 0;
    while (auxPos <= MAX) and (not found) do
      begin
        node := _get(this, auxPos);
        if (node.id = id) then
          found := true
        else
          auxPos := auxPos + 1;
      end;

    if found then 
      pos := auxPos;
    _closeHash(this);
    searchById := found;
  end;

  procedure insert           (var this : tOpenHash; node : tNode);
  var
    pos : tHashValue;
    rc  : tControlRecord;
  begin
    _openHash(this);
    pos := _getBucket(this, node.email);
    rc  := _getControl(this);
    rc.count  := rc.count + 1;
    rc.lastID := rc.lastID + 1;
    node.id   := rc.lastID;
    _set(this, pos, node);
    _setControl(this, rc);
    _closeHash(this);
  end;

  procedure update           (var this : tOpenHash; node : tNode);
  var
    pos : tHashValue;
    found : boolean;
  begin
    _openHash(this);
    found := _search(this, node.email, pos);
    if (found) then
      _set(this, pos, node);
    _closeHash(this);
  end;

  procedure remove           (var this : tOpenHash; node: tNode);
  var
    pos   : tHashValue;
    rc    : tControlRecord;
  begin
    _openHash(this);
    search(this, node.email, pos);
    node.id := NULLIDX;
    _set(this, pos, node);
    rc       := _getControl(this);
    rc.count := rc.count - 1;
    _setControl(this, rc);
    _closeHash(this);
  end;

  function  fetch            (var this : tOpenHash; email : tKey) : tNode;
  var
    node : tNode;
    pos  : tHashValue; 
  begin
    search(this, email, pos);
    _openHash(this);
    node := _get(this, pos);
    _closeHash(this);
    fetch := node;
  end;

  function  fetchByIdx   (var this : tOpenHash; pos: tHashValue) : tNode;
  var
    node : tNode;
  begin
    _openHash(this);
    node := _get(this, pos);
    _closeHash(this);
    fetchByIdx := node;
  end;

  function  fetchFirst       (var this : tOpenHash; var pos: idxRange) : boolean;
  var
    found : boolean;
    node  : tNode;
    idx   : idxRange;
  begin
    pos := -1;
    fetchFirst := fetchNext(this, pos);
  end;

  function  fetchNext        (var this : tOpenHash; var pos: idxRange) : boolean;
  var
    found : boolean;
    node  : tNode;
    idx   : idxRange;
  begin
    idx   := pos + 1;
    found := false;
    _openHash(this);
    while (not found) and (idx <= MAX) do
      begin
        node := _get(this, idx);
        if (node.id > NULLIDX) then
          found := true
        else
          idx := idx + 1;
      end;
    _closeHash(this);
    if found then
      pos      := idx;

    fetchNext := found;
  end;
  
end.
