{messages module}
unit lib.tree.trinary;

interface

uses
  sysutils;

const
  NULLIDX          = -1;
  BALANCETHRESHOLD = 80;

type
  idxRange       = NULLIDX..MAXINT;
  tKey           = integer;
  tPercentage    = 0..100;  
  tNode          = record
                     id       : tKey;
                     idUser   : tKey;
                     parent   : idxRange;
                     first    : idxRange;
                     last     : idxRange;
                     left     : idxRange;
                     center   : idxRange;
                     right    : idxRange;
                   end;
  tControlRecord = record
                     root             : idxRange;
                     erasedIndexes    : idxRange;
                     erasedMessages   : idxRange;
                     lastLevel        : integer;
                     balanceThreshold : tPercentage
                   end;
  tMessage       = record
                     number    : integer;
                     question  : string[255];
                     answer    : string[255];
                     timestamp : tDateTime;
                     next      : idxRange;
                   end;
  tLevel         = record
                     totalNodes : integer;
                   end;
  tControl       = file of tControlRecord;
  tData          = file of tMessage;
  tIndex         = file of tNode;
  tLevels        = file of tLevel;
  tTrinaryTree   = record
                     data    : tData;
                     index   : tIndex;
                     control : tControl;
                     levels  : tLevels;
                   end;

  procedure loadTree         (var this : tTrinaryTree; path, filename : string);
  procedure newEmptyTree     (var this : tTrinaryTree; path, filename : string);
  function  isEmpty          (var this : tTrinaryTree) : boolean;

  { check below here}
  function  search           (var this : tTrinaryTree; key : tKey; var pos: idxRange) : boolean;
  procedure insert           (var this : tTrinaryTree; pos: idxRange; key : tKey);
  procedure remove           (var this : tTrinaryTree; pos: idxRange);
  function  fetch            (var this : tTrinaryTree; pos: idxRange) : tNode;
  function  root             (var this : tTrinaryTree) : idxRange;
  function  leftChild        (var this : tTrinaryTree; pos: idxRange) : idxRange;
  function  rightChild       (var this : tTrinaryTree; pos: idxRange) : idxRange;
  function  parent           (var this : tTrinaryTree; pos: idxRange) : idxRange;
  function  nextItem         (var this : tTrinaryTree; node: tNode) : tNode;



implementation
  { Helpers }
  procedure  _openTree (var this : tTrinaryTree);
  begin
    reset(this.data);
    reset(this.index);
    reset(this.control);
    reset(this.levels);
  end;

  procedure  _closeTree (var this : tTrinaryTree);
  begin
    close(this.data);
    close(this.index);
    close(this.control);
    close(this.levels);
  end;

  function  _get (var this : tTrinaryTree; pos : idxRange) : tNode;
  var
    node : tNode;
  begin
    seek(this.index, pos);
    read(this.index, node);
    _get := node;
  end;

  procedure _set (var this : tTrinaryTree; pos : idxRange; node : tNode);
  begin
    seek(this.index, pos);
    write(this.index, node);
  end;

  function  _getMessage (var this : tTrinaryTree; pos : idxRange) : tMessage;
  var
    node : tMessage;
  begin
    seek(this.data, pos);
    read(this.data, node);
    _getMessage := node;
  end;

  procedure _setMessage (var this : tTrinaryTree; pos : idxRange; node : tMessage);
  begin
    seek(this.data, pos);
    write(this.data, node);
  end;

  function  _getControl (var this : tTrinaryTree) : tControlRecord;
  var
    rc : tControlRecord;
  begin
    seek (this.control, 0);
    read (this.control, rc);
    _getControl := rc;
  end;

  procedure _setControl(var this : tTrinaryTree; rc : tControlRecord);
  begin
    seek(this.control, 0);
    write(this.control, rc);
  end;

  function  _getLevel (var this : tTrinaryTree; pos : longint) : tLevel;
  var
    lvl : tLevel;
  begin
    seek (this.levels, pos);
    read (this.levels, lvl);
    _getLevel := lvl;
  end;

  procedure _setLevel(var this : tTrinaryTree; pos : longint; lvl : tLevel);
  begin
    seek(this.levels, pos);
    write(this.levels, lvl);
  end;

  function _max(a, b : integer) : integer;
  begin
    if a > b then
      _max := a
    else
      _max := b;
  end;

  function _height (var this : tTrinaryTree; branchRoot : idxRange) : integer;
  var
    h    : integer;
    node : tNode;
  begin
    if branchRoot = NULLIDX then
      h := 0
    else
      begin
        node := _get(this, branchRoot);
        h := _max(_height(this, node.right), _height(this, node.left)) + 1;
      end;
    _height := h;
  end;

  function _appendData (var this : tTrinaryTree; var message : tMessage) : idxRange;
  var
    rc     : tControlRecord;
    pos    : idxRange;
    auxMsg : tMessage;
  begin
    rc  := _getControl(this);
    pos := NULLIDX;
    message.next  := NULLIDX;
    if rc.erasedMessages = NULLIDX then
      pos           := filesize(this.data)
    else
      begin
        pos               := rc.erasedMessages;
        auxMsg            := _getMessage(this, pos);
        rc.erasedMessages := auxMsg.next;
      end;
    _setControl(this, rc);
    _setMessage(this, pos, message);
    _appendData := pos;
  end;


  function _appendNode (var this : tTrinaryTree; var node : tNode) : idxRange;
  var
    rc      : tControlRecord;
    pos     : idxRange;
    auxNode : tNode;
  begin
    rc          := _getControl(this);
    pos         := NULLIDX;
    node.first  := NULLIDX;
    node.last   := NULLIDX;
    node.right  := NULLIDX;
    node.center := NULLIDX;
    node.left   := NULLIDX;
    if rc.erasedIndexes = NULLIDX then
      pos          := filesize(this.data)
    else
      begin
        pos              := rc.erasedIndexes;
        auxNode          := _get(this, pos);
        rc.erasedIndexes := auxNode.parent;
      end;
    _set(this, pos, node);
    _setControl(this, rc);
    _appendNode := pos;
  end;

  procedure _detachMessages (var this : tTrinaryTree; pos : idxRange);
  var
    rc     : tControlRecord;
    msg    : tMessage;
    auxPos : idxRange;
  begin
    rc     := _getControl(this);
    auxPos := pos;
    msg    := _getMessage(this, auxPos);
    while (msg.next <> NULLIDX) do
      begin
        msg    := _getMessage(this, auxPos);
        auxPos := msg.next;
      end;
    msg.next := rc.erasedMessages;
    _setMessage(this, auxPos, msg);
    rc.erasedMessages := pos;
    _setControl(this, rc);
  end;

  procedure _detachNode (var this : tTrinaryTree; pos : idxRange; var node : tNode);
  var
    rc : tControlRecord;
  begin
    _detachMessages(this, node.first);
    rc               := _getControl(this);
    node.first       := NULLIDX;
    node.last        := NULLIDX;
    node.right       := NULLIDX;
    node.center      := NULLIDX;
    node.left        := NULLIDX;
    node.parent      := rc.erasedIndexes;
    rc.erasedIndexes := pos;
    _set(this, pos, node);
    _setControl(this, rc);
  end;

  function _isLeaf(var node : tNode) : boolean;
  begin
    _isLeaf := (node.left = NULLIDX) and (node.right = NULLIDX);
  end;

  function _parent(var this : tTrinaryTree; var idx : idxRange) : idxRange;
  var
    node : tNode;
  begin
    node    := _get(this, idx);
    _parent := node.parent;
  end;

  procedure _updateParent(var this : tTrinaryTree; parentIdx, old, new : idxRange);
  var
    rc         : tControlRecord;
    parentNode : tNode;
  begin
    if parentIdx = NULLIDX then // pivot was root, update
      begin
        rc      := _getControl(this);
        rc.root := new;
        _setControl(this, rc);
      end
    else
      begin
        parentNode := _get(this, parentIdx);
        if parentNode.left = old then
          parentNode.left  := new
        else
          parentNode.right := new;
        _set(this, parentIdx, parentNode);
      end;
  end;

  function _getBiggerFromBranch (var this : tTrinaryTree; var pivot : idxRange) : tKey;
  var
    node : tNode;
  begin
    node := _get(this, pivot);
    while (node.right <> NULLIDX) do
      begin
        pivot := node.right;
        node  := _get(this, pivot);
      end;
    _getBiggerFromBranch := node.id;
  end;

  function _getSmallerFromBranch (var this : tTrinaryTree; var pivot : idxRange) : tKey;
  var
    node : tNode;
  begin
    node := _get(this, pivot);
    while (node.left <> NULLIDX) do
      begin
        pivot := node.left;
        node  := _get(this, pivot);
      end;
    _getSmallerFromBranch := node.id;
  end;

  { key helpers }
  function keyGt(key1, key2 : tKey) : boolean;
  begin
    keyGt := key1 > key2;
  end;

  function keyLt(key1, key2 : tKey) : boolean;
  begin
    keyLt := key1 < key2;
  end;

  function keyEq(key1, key2 : tKey) : boolean;
  begin
    keyEq := key1 = key2;
  end;

  {Public}
  procedure loadTree         (var this : tTrinaryTree; path, filename : string);
  var
    controlError, dataError : boolean;
    fullFileName : string;
    rc : tControlRecord;
  begin
    fullFileName := path + filename;
    {$I-}
    //check if data file exists
    assign(this.data, fullFileName + '.dat');
    reset(this.data);
    dataError := IOResult <> 0;

    assign(this.index, fullFileName + '.ntx');
    reset(this.index);
    dataError := IOResult <> 0;

    assign(this.control, fullFileName + '.ctrl');
    reset(this.control);
    controlError := IOResult <> 0;

    assign(this.levels, fullFileName + '.niv');
    reset(this.levels);
    dataError := IOResult <> 0;
    {$I+}

    if (controlError and dataError) then
      begin
        rewrite(this.data);
        rewrite(this.control);
        rc.root             := NULLIDX;
        rc.erasedMessages   := NULLIDX;
        rc.erasedIndexes    := NULLIDX;
        rc.lastLevel        := 0;
        rc.balanceThreshold := BALANCETHRESHOLD;
        _setControl(this, rc);
      end;

    _closeTree(this);
  end;

  procedure newEmptyTree     (var this : tTrinaryTree; path, filename : string);
  var
    fullFileName : string;
    rc : tControlRecord;
  begin
    fullFileName := path + filename;
    {$I-}
    assign(this.data, fullFileName + '.dat');
    assign(this.index, fullFileName + '.ntx');
    assign(this.control, fullFileName + '.ctrl');
    assign(this.levels, fullFileName + '.niv');
    rewrite(this.data);
    rewrite(this.index);
    rewrite(this.control);
    rewrite(this.levels);
    rc.root             := NULLIDX;
    rc.erasedMessages   := NULLIDX;
    rc.erasedIndexes    := NULLIDX;
    rc.lastLevel        := 0;
    rc.balanceThreshold := BALANCETHRESHOLD;
    _setControl(this, rc);

    _closeTree(this);
  end;

  function  isEmpty          (var this : tTrinaryTree) : boolean;
  var
    empty : boolean;
    rc    : tControlRecord;
  begin
    _openTree(this);
    rc    := _getControl(this);
    empty := rc.root = NULLIDX;
    _closeTree(this);
    isEmpty := empty;
  end;

  function  search           (var this : tTrinaryTree; key : tKey; var pos: idxRange) : boolean;
  var
    found      : boolean;
    curNodeIdx : idxRange;
    curNode    : tNode;
    rc         : tControlRecord;
  begin
    _openTree(this);
    found      := false;
    rc         := _getControl(this);
    curNodeIdx := rc.root;
    pos        := NULLIDX;
    while (curNodeIdx <> NULLIDX) and (not found) do
      begin
        curNode := _get(this, curNodeIdx);
        if keyEq(curNode.id, key) then
          begin
            found := true;
            pos   := curNodeIdx;
          end
        else
          begin
            pos := curNodeIdx;
            if keyGt(key, curNode.id) then
              curNodeIdx := curNode.right
            else
              curNodeIdx := curNode.left;
          end;
      end;
    _closeTree(this);
    search := found;
  end;

  procedure insert           (var this : tTrinaryTree; pos: idxRange; key : tKey);
  var
    node, parent : tNode;
    rc           : tControlRecord;
    auxIdx       : idxRange;
  begin
    _openTree(this);
    node.id     := key;
    node.parent := pos;
    auxIdx      := _append(this, node);

    if pos = NULLIDX then //empty tree, insert at root
      begin
        rc      := _getControl(this);
        rc.root := auxIdx;
        _setControl(this, rc);
      end
    else
      begin
        parent := _get(this, pos);
        if keyEq(key, parent.id) then
          begin
          end
        else
          if keyGt(key, parent.id) then
            parent.right := auxIdx
          else
            parent.left  := auxIdx;
          _set(this, pos, parent);
      end;

    _closeTree(this);
  end;

  procedure _remove          (var this: tTrinaryTree; pos: idxRange);
  var
    node, parent   : tNode;
    rc             : tControlRecord;
    auxIdx         : idxRange;
    replacementKey : tKey;
  begin
    node        := _get(this, pos);

    if _isLeaf(node) then //easy
      begin
        if node.parent = NULLIDX then // is the root
          begin
            rc      := _getControl(this);
            rc.root := NULLIDX;
            _setControl(this, rc);
          end
        else
          begin
            auxIdx := node.parent;
            parent := _get(this, auxIdx);
            if parent.right = pos then
              parent.right := NULLIDX
            else
              parent.left  := NULLIDX;
            _set(this, auxIdx, parent);
          end;
        _detach(this, pos, node);
      end
    else
      begin
        if node.right = NULLIDX then
          begin
            auxIdx         := node.left;
            replacementKey := _getBiggerFromBranch(this, auxIdx);
          end
        else
          begin
            auxIdx         := node.right;
            replacementKey := _getSmallerFromBranch(this, auxIdx);
          end;
        node.key := replacementKey;
        _set(this, pos, node);
        _remove(this, auxIdx);
      end;
  end;

  procedure remove          (var this: tTrinaryTree; pos: idxRange);
  begin
    _openTree(this);
    _remove(this, pos);
    _closeTree(this);
  end;

  function  fetch            (var this : tTrinaryTree; pos: idxRange) : tNode;
  var
    node : tNode;
  begin
    _openTree(this);
    node := _get(this, pos);
    _closeTree(this);
    fetch := node;
  end;

  function  root             (var this : tTrinaryTree) : idxRange;
  var
    rc : tControlRecord;
  begin
    _openTree(this);
    rc := _getControl(this);
    _closeTree(this);
    root := rc.root;
  end;

  function  leftChild        (var this : tTrinaryTree; pos: idxRange) : idxRange;
  var
    node : tNode;
  begin
    _openTree(this);
    node := _get(this, pos);
    _closeTree(this);
    leftChild := node.left;
  end;

  function  rightChild       (var this : tTrinaryTree; pos: idxRange) : idxRange;
  var
    node : tNode;
  begin
    _openTree(this);
    node := _get(this, pos);
    _closeTree(this);
    rightChild := node.right;
  end;

  function  parent           (var this : tTrinaryTree; pos: idxRange) : idxRange;
  var
    idx : idxRange;
  begin
    _openTree(this);
    idx := _parent(this, pos);
    _closeTree(this);
    parent := idx;
  end;

  function  nextItem         (var this : tTrinaryTree; node: tNode) : tNode;
   var
    auxNode : tNode;
  begin
    _openTree(this);
    auxNode := _get(this, node.center);
    _closeTree(this);
    nextItem := auxNode;
  end;
end.
