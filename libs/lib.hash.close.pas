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
  tHashValue     = NULLIDX..MAX;
  tKey           = longint;
  tNode          = record
                     idBuyer          : longint;
                     idItem           : tKey;
                     itemName         : string[255];
                     price            : Currency;
                     publishDate      : TDateTime;
                     sellDate         : TDateTime;
                     itemType         : tItemType;
                     tCalification     : tCalification;
                     tax              : double;
                     alreadyCollected : boolean;
                     previous         : idxRange;
                     next             : idxRange;
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
  tHashControl   = file of tHashNode;
  tCloseHash     = record
                     data    : tData;
                     control : tControl;
                     hash    : tHashControl;
                   end;

  procedure loadHash         (var this : tCloseHash; path, filename : string);
  procedure newEmptyHash     (var this : tCloseHash; path, filename : string);
  function  isEmpty          (var this : tCloseHash) : boolean;
  function  search           (var this : tCloseHash; key : tKey; var pos: idxRange) : boolean;
  procedure insert           (var this : tCloseHash; sell : tSell);
  procedure update           (var this : tCloseHash; pos: idxRange; sell : tSell);
  procedure remove           (var this : tCloseHash; pos: idxRange);
  function  fetch            (var this : tCloseHash; pos: idxRange) : tSell;

implementation
  { Helpers }
  procedure  _openHash (var this : tCloseHash);
  begin
    reset(this.data);
    reset(this.control);
    reset(this.hash);
  end;

  procedure  _closeHash (var this : tCloseHash);
  begin
    close(this.data);
    close(this.control);
    close(this.hash);
  end;

  function  _get (var this : tCloseHash; pos : idxRange) : tNode;
  var
    node : tNode;
  begin
    seek(this.data, pos);
    read(this.data, node);
    _get := node;
  end;

  procedure _set (var this : tCloseHash; pos : idxRange; node : tNode);
  begin
    seek(this.data, pos);
    write(this.data, node);
  end;

  function  _getHash (var this : tCloseHash; pos : tHashValue) : tHashNode;
  var
    node : tHashNode;
  begin
    seek(this.hash, pos);
    read(this.hash, node);
    _getHash := node;
  end;

  procedure _setHash (var this : tCloseHash; pos : tHashValue; node : tHashNode);
  begin
    seek(this.hash, pos);
    write(this.hash, node);
  end;

  function  _getControl (var this : tCloseHash) : tControlRecord;
  var
    rc : tControlRecord;
  begin
    seek (this.control, 0);
    read (this.control, rc);
    _getControl := rc;
  end;

  procedure _setControl(var this : tCloseHash; rc : tControlRecord);
  begin
    seek(this.control, 0);
    write(this.control, rc);
  end;  

  function  _hash(id : tKey) : tHashValue;
  begin
    _hash := id mod MAX;
  end;

  function _append (var this : tCloseHash; var item : tNode) : idxRange;
  var
    rc      : tControlRecord;
    pos     : idxRange;
    auxNode : tNode;
  begin
    rc            := _getControl(this);
    pos           := NULLIDX;
    item.next     := NULLIDX;
    item.previous := NULLIDX;

    if rc.erased = NULLIDX then
      pos         := filesize(this.data)
    else
      begin
        pos       := rc.erased;
        auxNode   := _get(this, pos);
        rc.erased := auxNode.next;
      end;
    _set(this, pos, item);
    _setControl(this, rc);
    _append := pos;
  end;

  procedure _detach  (var this : tCloseHash; pos : idxRange);
  var
    rc      : tControlRecord;
    auxNode : tNode;
  begin
    rc               := _getControl(this);
    auxNode          := _get(this, pos);
    auxNode.next     := rc.erased;
    auxNode.previous := NULLIDX;
    rc.erased        := pos;

    _set(this, pos, auxNode);
    _setControl(this, rc);
  end;

  procedure loadHash         (var this : tCloseHash; path, filename : string);
  var
    controlError, dataError : boolean;
    fullFileName            : string;
  begin
    {TODO}
    fullFileName := path + filename;
    {$I-}
    //check if data file exists
    assign(this.data, fullFileName + '.dat');
    reset(this.data);
    dataError := IOResult <> 0;

    assign(this.control, fullFileName + '.con');
    reset(this.control);
    controlError := IOResult <> 0;

    assign(this.hash, fullFileName + '.hash');
    reset(this.hash);
    controlError := IOResult <> 0;
    {$I+}

    if (controlError and dataError) then
      newEmptyHash(this, path, filename);

    _closeHash(this);
  end;

  procedure newEmptyHash     (var this : tCloseHash; path, filename : string);
  var
    fullFileName : string;
    rc           : tControlRecord;
    i            : integer;
    hashNode     : tHashNode;
  begin
    {TODO}
    fullFileName := path + filename;
    {$I-}
    assign(this.data,    fullFileName + '.dat');
    assign(this.control, fullFileName + '.con');
    assign(this.hash,    fullFileName + '.hash');
    rewrite(this.data);
    rewrite(this.control);
    rewrite(this.hash);
    rc.erased := NULLIDX;

    hashNode.first := NULLIDX;
    hashNode.last  := NULLIDX;
    hashNode.total := 0;

    for i := 0 to MAX do
      _setHash(this, i, hashNode);

    _setControl(this, rc);
    _closeHash(this);
  end;

  function  isEmpty          (var this : tCloseHash) : boolean;
  begin
    {TODO}
  end;

  function  search           (var this : tCloseHash; key : tKey; var pos: idxRange) : boolean;
  var
    hashNode : tHashNode;
    node     : tNode;
    found    : boolean;
    hashKey  : tHashValue;
  begin
    {TODO}
    _openHash(this);
    hashKey  := _hash(key);
    pos      := NULLIDX;
    hashNode := _getHash(this, hashKey);
    found    := false;

    if (hashNode.total > 0) then
      begin
        pos := hashNode.first;
        while (pos <> NULLIDX) and (not found) do
          begin
            node := _get(this, pos);
            if (node.idItem = key) then
              found := true
            else
              pos := node.next;
          end;
      end;

    search := found;
  end;

  procedure insert           (var this : tCloseHash; sell : tSell);
  var
    key              : tHashValue;
    pos, previousPos : idxRange;
    hashNode         : tHashNode;
    previousSell     : tSell;
  begin
    _openHash(this);
    key      := _hash(sell.idItem);
    pos      := _append(this, sell);
    hashNode := _getHash(this, key);
    if (hashNode.total = 0) then
      begin
        hashNode.total := 1;
        hashNode.first := pos;
        hashNode.last  := pos;
      end
    else
      begin
        previousPos           := hashNode.first;
        previousSell          := _get(this, previousPos);
        previousSell.previous := pos;
        sell.next             := hashNode.first;
        hashNode.total        := hashNode.total + 1;
        hashNode.first        := pos;
        _set(this, previousPos, previousSell);
        _set(this, pos, sell);
      end;
    _setHash(this, key, hashNode);
    _closeHash(this);
  end;

  procedure update           (var this : tCloseHash; pos: idxRange; sell : tSell);
  begin
    _openHash(this);
    _set(this, pos, sell);
    _closeHash(this);
  end;


  procedure remove           (var this : tCloseHash; pos: idxRange);
  var
    node     : tNode;
    key      : tHashValue;
    auxPos   : idxRange;
    hashNode : tHashNode;
    auxNode  : tNode;
  begin
    {TODO}
     _openHash(this);
    node     := _get(this, pos);
    key      := _hash(node.idItem);
    hashNode := _getHash(this, key);

    //remove element from the hash
    if (hashNode.total = 1) then { it's the only one }
      begin
        hashNode.total := 0;
        hashNode.first := NULLIDX;
        hashNode.last  := NULLIDX;
      end
    else
      begin
        if (pos = hashNode.first) then { first element from the list}
          begin
            auxPos           := node.next;
            auxNode          := _get(this, auxPos);
            auxNode.previous := NULLIDX;
            _set(this, auxPos, auxNode);
            hashNode.first   := auxPos;
          end
        else                            { it's in the middle or last}
          begin
            auxPos       := node.previous;
            auxNode      := _get(this, auxPos);
            auxNode.next := node.next;
            _set(this, auxPos, auxNode); 

            if (pos = hashNode.last) then { last element from the list}
              begin
                auxPos           := node.next;
                auxNode          := _get(this, auxPos);
                auxNode.previous := NULLIDX;
                _set(this, auxPos, auxNode);
                hashNode.last    := auxPos;
              end
            else                            { it's in the middle}
              begin
                auxPos       := node.next;
                auxNode      := _get(this, auxPos);
                auxNode.next := node.next;
                _set(this, auxPos, auxNode); 
              end;
          end;
        hashNode.total := hashNode.total - 1;
      end;

    //mark element as deleted
    _detach(this, pos);

    _closeHash(this);
  end;

  function  fetch            (var this : tCloseHash; pos: idxRange) : tSell;
  var
    node : tSell;
  begin
    _openHash(this);
    node  := _get(this, pos);
    _closeHash(this);
    fetch := node;
  end;
  
end.
