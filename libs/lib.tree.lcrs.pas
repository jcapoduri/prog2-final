{Published category}
unit lib.tree.lcrs;

interface

uses
  sysutils;

const
  NULLIDX  = -1;

type
  idxRange       = NULLIDX..MAXINT;
  tKey           = longint;
  tItemType      = (New, Used);
  tStatus        = (Publish, Paused, Sold, Void, Blocked);
  tNode          = record
                     id           : longint;
                     categoryName : string[255];
                     description  : string[255];
                     VAT          : Currency;
                     parent       : idxRange;
                     leftChild    : idxRange;
                     rightSibling : idxRange;
                   end;
  tCategory      = tNode;
  tCategoryList  = array of tCategory;
  tControlRecord = record
                     root   : idxRange;
                     erased : idxRange;
                     lastID : longint;
                   end;
  tControl       = file of tControlRecord;
  tData          = file of tNode;
  tLCRStree      = record
                     data    : tData;
                     control : tControl;
                   end;

  procedure loadTree         (var this : tLCRStree; path, filename : string);
  procedure newEmptyTree     (var this : tLCRStree; path, filename : string);
  function  isEmpty          (var this : tLCRStree) : boolean;
  function  search           (var this : tLCRStree; key : tKey; var pos: idxRange) : boolean;
  procedure addChild         (var this : tLCRStree; pos: idxRange; item : tNode);
  procedure addSibling       (var this : tLCRStree; pos: idxRange; item : tNode);
  procedure update           (var this : tLCRStree; pos: idxRange; item : tNode);
  procedure remove           (var this : tLCRStree; pos: idxRange);
  function  fetch            (var this : tLCRStree; pos: idxRange) : tCategory;
  function  root             (var this : tLCRStree) : idxRange;
  function  firstChild       (var this : tLCRStree; pos: idxRange) : idxRange;
  function  nextSibling      (var this : tLCRStree; pos: idxRange) : idxRange;
  function  parent           (var this : tLCRStree; pos: idxRange) : idxRange;

implementation
  { Helpers }
  procedure  _openTree (var this : tLCRStree);
  begin
    reset(this.data);
    reset(this.control);
  end;

  procedure  _closeTree (var this : tLCRStree);
  begin
    close(this.data);
    close(this.control);
  end;

  function  _get (var this : tLCRStree; pos : idxRange) : tNode;
  var
    node : tNode;
  begin
    seek(this.data, pos);
    read(this.data, node);
    _get := node;
  end;

  procedure _set (var this : tLCRStree; pos : idxRange; node : tNode);
  begin
    seek(this.data, pos);
    write(this.data, node);
  end;

  function  _getControl (var this : tLCRStree) : tControlRecord;
  var
    rc : tControlRecord;
  begin
    seek (this.control, 0);
    read (this.control, rc);
    _getControl := rc;
  end;

  function _isLeaf(var node : tNode) : boolean;
  begin
    _isLeaf := (node.leftChild = NULLIDX) and (node.rightSibling = NULLIDX);
  end;

  function _parent(var this : tLCRStree; var idx : idxRange) : idxRange;
  var
    node : tNode;
  begin
    node    := _get(this, idx);
    _parent := node.parent;
  end;

  procedure _setControl(var this : tLCRStree; RC : tControlRecord);
  begin
    seek(this.control, 0);
    write(this.control, RC);
  end;  

  { tree helpers }
  function _append (var this : tLCRStree; var item : tNode) : idxRange;
  var
    rc      : tControlRecord;
    pos     : idxRange;
    auxNode : tNode;
  begin
    rc        := _getControl(this);
    pos       := NULLIDX;
    rc.lastID := rc.lastID + 1;
    item.id   := rc.lastID;
    if Rc.erased = NULLIDX then
      begin
        pos               := filesize(this.data);
        item.rightSibling := NULLIDX;
        item.leftChild    := NULLIDX;
        _set(this, pos, item);
      end
    else
      begin
        pos               := rc.erased;
        auxNode           := _get(this, pos);
        rc.erased         := auxNode.parent;
        item.rightSibling := NULLIDX;
        item.leftChild    := NULLIDX;
        _set(this, pos, item);
      end;
    _setControl(this, rc);
    _append := pos;
  end;

  procedure _detach (var this : tLCRStree; pos : idxRange; var node : tNode);
  var
    rc : tControlRecord;
  begin
    rc                := _getControl(this);
    node.rightSibling := NULLIDX;
    node.leftChild    := NULLIDX;
    node.parent       := rc.erased;
    rc.erased         := pos;
    _set(this, pos, node);
    _setControl(this, rc);
  end;

  procedure _remove          (var this: tLCRStree; pos: idxRange);
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
            if parent.rightSibling = pos then
              parent.rightSibling := NULLIDX
            else
              parent.leftChild := NULLIDX;
            _set(this, auxIdx, parent);
          end;
        _detach(this, pos, node);
      end
    else
      begin
        if node.rightSibling = NULLIDX then
          begin
            auxIdx         := node.leftChild;
            //FIXME
            //replacementKey := _getBiggerFromBranch(this, auxIdx);
          end
        else
          begin
            auxIdx         := node.rightSibling;
            //FIXME
            //replacementKey := _getSmallerFromBranch(this, auxIdx);
          end;
        node.id := replacementKey;
        _set(this, pos, node);
        _remove(this, auxIdx);
      end;
  end;


  {Public}
  procedure loadTree         (var this : tLCRStree; path, filename : string);
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

    assign(this.control, fullFileName + '.ctrl');
    reset(this.control);
    controlError := IOResult <> 0;
    {$I+}

    if (controlError and dataError) then
      begin
        rewrite(this.data);
        rewrite(this.control);
        rc.root := NULLIDX;
        rc.erased := NULLIDX;
        _setControl(this, rc);
      end;

    _closeTree(this);
  end;

  procedure newEmptyTree     (var this : tLCRStree; path, filename : string);
  var
    fullFileName : string;
    rc : tControlRecord;
  begin
    fullFileName := path + filename;
    {$I-}
    assign(this.data, fullFileName + '.dat');
    assign(this.control, fullFileName + '.ctrl');
    rewrite(this.data);
    rewrite(this.control);
    rc.root   := NULLIDX;
    rc.lastID := 0;
    rc.erased := NULLIDX;
    _setControl(this, rc);

    _closeTree(this);
  end;

  function  isEmpty          (var this : tLCRStree) : boolean;
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

  function  firstChild        (var this : tLCRStree; pos: idxRange) : idxRange;
  var
    node : tNode;
  begin
    _openTree(this);
    node := _get(this, pos);
    _closeTree(this);
    firstChild := node.leftChild;
  end;

  function  nextSibling       (var this : tLCRStree; pos: idxRange) : idxRange;
  var
    node : tNode;
  begin
    _openTree(this);
    node := _get(this, pos);
    _closeTree(this);
    nextSibling := node.rightSibling;
  end;

  function  parent           (var this : tLCRStree; pos: idxRange) : idxRange;
  var
    idx : idxRange;
  begin
    _openTree(this);
    idx := _parent(this, pos);
    _closeTree(this);
    parent := idx;
  end;

  procedure update          (var this : tLCRStree; pos: idxRange; item : tNode);
  begin
    _openTree(this);
    _set(this, pos, item);
    _closeTree(this);
  end;

  procedure remove          (var this: tLCRStree; pos: idxRange);
  begin
    _openTree(this);
    _remove(this, pos);
    _closeTree(this);
  end;

  function  fetch            (var this : tLCRStree; pos: idxRange) : tNode;
  var
    node : tNode;
  begin
    _openTree(this);
    node := _get(this, pos);
    _closeTree(this);
    fetch := node;
  end;

  function  root             (var this : tLCRStree) : idxRange;
  var
    rc : tControlRecord;
  begin
    _openTree(this);
    rc := _getControl(this);
    _closeTree(this);
    root := rc.root;
  end;

  procedure addChild         (var this : tLCRStree; pos: idxRange; item : tNode);
  var
    node, parent : tNode;
    rc           : tControlRecord;
    auxIdx       : idxRange;
  begin
    _openTree(this);
    //item.id     := key;
    item.parent := pos;
    auxIdx      := _append(this, item);

    if pos = NULLIDX then //empty tree, insert at root
      begin
        rc      := _getControl(this);
        rc.root := auxIdx;
        _setControl(this, rc);
      end
    else
      begin
        parent := _get(this, pos);        
        if parent.leftChild = NULLIDX then
          parent.leftChild := auxIdx
        else
          begin
            item.rightSibling := parent.leftChild;
            parent.leftChild  := auxIdx;
            _set(this, auxIdx, item);
          end;
        _set(this, pos, parent);
      end;

    _closeTree(this);
  end;

  procedure addSibling       (var this : tLCRStree; pos: idxRange; item : tNode);
  var
    node, parent : tNode;
    rc           : tControlRecord;
    auxIdx       : idxRange;
  begin
    _openTree(this);
    //item.id    := key;
    item.parent := pos;
    auxIdx      := _append(this, item);

    if pos = NULLIDX then //empty tree, insert at root
      begin
        rc      := _getControl(this);
        rc.root := auxIdx;
        _setControl(this, rc);
      end
    else
      begin
        parent := _get(this, pos);        
        if parent.rightSibling = NULLIDX then
          parent.rightSibling := auxIdx
        else
          begin
            item.rightSibling := parent.rightSibling;
            parent.rightSibling  := auxIdx;
            _set(this, auxIdx, item);
          end;
        _set(this, pos, parent);
      end;

    _closeTree(this);
  end;

  function _search (var this : tLCRStree; key : tKey; var pos: idxRange) : boolean;
  var 
    found      : boolean;
    curNodeIdx : idxRange;
    curNode    : tNode;
  begin
    curNodeIdx := pos;
    found      := false;
    
    if pos <> NULLIDX then
      begin
        curNode    := _get(this, curNodeIdx);
        if curNode.id = key then
          begin
            found := true;
            pos   := curNodeIdx;
          end
        else
          begin
            pos := curNode.rightSibling;
            found := _search(this, key, pos);
            if not found then
              begin
                pos := curNode.leftChild;
                found := _search(this, key, pos);
              end;
          end;
      end;
    _search := found;
  end;

  function  search           (var this : tLCRStree; key : tKey; var pos: idxRange) : boolean;
  var
    found      : boolean;
    curNodeIdx : idxRange;
    curNode    : tNode;
    rc         : tControlRecord;
  begin
    _openTree(this);
    rc         := _getControl(this);
    pos        := rc.root;
    found      := _search(this, key, pos);
    _closeTree(this);
    if not found then
      pos := NULLIDX;
    
    search := found;
  end;

end.
