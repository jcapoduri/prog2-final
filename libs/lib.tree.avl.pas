{Published publications}
unit lib.tree.avl;

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
  tPublish       = record
                     id         : longint;
                     idCategory : longint;
                     idUser     : longint;
                     itemName   : string[255];
                     details    : string[255];
                     price      : Currency;
                     ctimestamp : TDateTime;
                     etimestamp : TDateTime;
                     itemType   : tItemType;
                     status     : tStatus;
                   end;
  tPublishList   = array of tPublish;
  tNode          = record
                     key      : longint;
                     index    : idxRange;
                     parent   : idxRange;
                     left     : idxRange;
                     right    : idxRange;
                   end;
  tControlRecord = record
                     root1   : idxRange; { by user }
                     root2   : idxRange; { by category }
                     erased1 : idxRange; { by user }
                     erased2 : idxRange; { by category }
                     lastID  : longint;
                   end;
  tControl       = file of tControlRecord;
  tIdxFile       = file of tNode;      
  tIdxByUser     = tIdxFile;
  tIdxByCategory = tIdxFile;
  tData          = file of tPublish;
  tAVLtree       = record
                     data          : tData;
                     idxByUser     : tIdxByUser;
                     idxByCategory : tIdxByCategory;
                     control       : tControl;
                   end;

  procedure loadTree           (var this : tAVLtree; path, filename : string);
  procedure newEmptyTree       (var this : tAVLtree; path, filename : string);
  function  isEmpty            (var this : tAVLtree) : boolean;
  function  searchByUser       (var this : tAVLtree; key : tKey; var pos: idxRange) : boolean;
  function  searchByCategory   (var this : tAVLtree; key : tKey; var pos: idxRange) : boolean;
  procedure append             (var this : tAVLtree; publication : tPublish);
  procedure update             (var this : tAVLtree; publication : tPublish);
  procedure remove             (var this : tAVLtree; publication : tPublish);
  function  fetch              (var this : tAVLtree; pos: idxRange) : tPublish;
  function  fetchByUser        (var this : tAVLtree; pos: idxRange) : tPublish;
  function  fetchByCategory    (var this : tAVLtree; pos: idxRange) : tPublish;
  function  rootOfUsers        (var this : tAVLtree) : idxRange;
  function  rootOfCategories   (var this : tAVLtree) : idxRange;
  function  leftUserChild      (var this : tAVLtree; pos: idxRange) : idxRange;
  function  rightUserChild     (var this : tAVLtree; pos: idxRange) : idxRange;
  function  leftCategoryChild  (var this : tAVLtree; pos: idxRange) : idxRange;
  function  rightCategoryChild (var this : tAVLtree; pos: idxRange) : idxRange;
  function  parentByUser       (var this : tAVLtree; pos: idxRange) : idxRange;
  function  parentByCategory   (var this : tAVLtree; pos: idxRange) : idxRange;

implementation
  { Helpers }
  procedure  _openTree (var this : tAVLtree);
  begin
    reset(this.data);
    reset(this.idxByUser);
    reset(this.idxByCategory);
    reset(this.control);
  end;

  procedure  _closeTree (var this : tAVLtree);
  begin
    close(this.data);
    close(this.idxByUser);
    close(this.idxByCategory);
    close(this.control);
  end;

  { getters }
  function _getByReference(var idx: tIdxFile; pos : idxRange) : tNode;
  var
    node : tNode;
  begin
    seek(idx, pos);
    read(idx, node);
    _getByReference := node;
  end;

  function  _getByUser (var this : tAVLtree; pos : idxRange) : tNode;
  begin
    _getByUser := _getByReference(this.idxByUser, pos);
  end;

  function  _getByCategory (var this : tAVLtree; pos : idxRange) : tNode;
  begin
    _getByCategory := _getByReference(this.idxByCategory, pos);
  end;

  function  _getCategoryByNode (var this : tAVLtree; idx : tNode) : tPublish;
  var
    node : tPublish;
  begin
    seek(this.data, idx.index);
    read(this.data, node);
    _getCategoryByNode := node;
  end;

  function  _getCategoryByPos (var this : tAVLtree; pos : idxRange) : tPublish;
  var
    node : tPublish;
  begin
    seek(this.data, pos);
    read(this.data, node);
    _getCategoryByPos := node;
  end;

  { setters }
  procedure _setByReference(var idx: tIdxFile; pos : idxRange; node : tNode);
  begin
    seek(idx, pos);
    write(idx, node);
  end;

  procedure  _setByUser (var this : tAVLtree; pos : idxRange; node : tNode);
  begin
    _setByReference(this.idxByUser, pos, node);
  end;

  procedure  _setByCategory (var this : tAVLtree; pos : idxRange; node : tNode);
  begin
    _setByReference(this.idxByCategory, pos, node);
  end;

  procedure _setPublication (var this : tAVLtree; pos : idxRange; publication : tPublish);
  var
    node : tPublish;
  begin
    seek(this.data, pos);
    write(this.data, publication);
  end;

  { control management }
  function  _getControl (var this : tAVLtree) : tControlRecord;
  var
    rc : tControlRecord;
  begin
    seek (this.control, 0);
    read (this.control, rc);
    _getControl := rc;
  end;

  procedure _setControl(var this : tAVLtree; rc : tControlRecord);
  begin
    seek (this.control, 0);
    write(this.control, rc);
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

  function _max(a, b : integer) : integer;
  begin
    if a > b then
      _max := a
    else
      _max := b;
  end;

  function _heightByReference (var idx : tIdxFile; branchRoot : idxRange) : integer;
  var
    h    : integer;
    node : tNode;
  begin
    if branchRoot = NULLIDX then
      h := 0
    else
      begin
        node := _getByReference(idx, branchRoot);
        h := _max(_heightByReference(idx, node.right), _heightByReference(idx, node.left)) + 1;
      end;
    _heightByReference := h;
  end;

  function _heightByUser (var this : tAVLtree; branchRoot : idxRange) : integer;
  begin
    _heightByUser := _heightByReference(this.idxByUser, branchRoot);
  end;

  function _heightByCategory (var this : tAVLtree; branchRoot : idxRange) : integer;
  begin
    _heightByCategory := _heightByReference(this.idxByCategory, branchRoot);
  end;

  { ---------------------------------------------------------------------------------------------------------------- }
  function _parent(var this : tIdxFile; var idx : idxRange) : idxRange; Overload;
  var
    node : tNode;
  begin
    node    := _getByReference(this, idx);
    _parent := node.parent;
  end;

  procedure _updateParent(var this : tAVLtree; var index : tIdxFile; parentIdx, old, new : idxRange);
  var
    rc         : tControlRecord;
    parentNode : tNode;
  begin
    if parentIdx = NULLIDX then // pivot was root, update
      begin
        rc      := _getControl(this);
        if (rc.root1 = old) then
          rc.root1 := new
        else
          rc.root2 := new;
        _setControl(this, rc);
      end
    else
      begin
        parentNode := _getByReference(index, parentIdx);
        if parentNode.left = old then
          parentNode.left  := new
        else
          parentNode.right := new;
        _setByReference(index, parentIdx, parentNode);
      end;
  end;

  procedure _balanceRight (var this : tAVLtree; var index : tIdxFile; var pivot : idxRange); forward;

  procedure _balanceLeft(var this : tAVLtree; var index : tIdxFile; var pivot : idxRange);
  var
    pivotNode, newBranchRoot, auxNode : tNode;
    newBranchRootIdx, parentIdx          : idxRange;
    rc                                   : tControlRecord;
    hLeft, hRight                        : integer;
  begin
    pivotNode        := _getByReference(index, pivot);
    parentIdx        := pivotNode.parent;
    newBranchRootIdx := pivotNode.right;
    newBranchRoot    := _getByReference(index, newBranchRootIdx);

    {check if need to re-balance}
    hLeft            := _heightByReference(index, newBranchRoot.left);
    hRight           := _heightByReference(index, newBranchRoot.right);
    if (hLeft > hRight) then
      begin
        _balanceRight(this, index, newBranchRootIdx);
        pivotNode        := _getByReference(index, pivot);
        newBranchRootIdx := pivotNode.right;
        newBranchRoot    := _getByReference(index, newBranchRootIdx);
      end;

    pivotNode.right      := newBranchRoot.left;
    newBranchRoot.left   := pivot;
    newBranchRoot.parent := parentIdx;
    pivotNode.parent     := newBranchRootIdx;

    _updateParent(this, index, parentIdx, pivot, newBranchRootIdx);

    if pivotNode.right <> NULLIDX then
      begin
        auxNode          := _getByReference(index, pivotNode.right);
        auxNode.parent   := pivot;
        _setByReference(index, pivotNode.right, auxNode);
      end;

    _setByReference(index, pivot, pivotNode);
    _setByReference(index, newBranchRootIdx, newBranchRoot);

    pivot := newBranchRootIdx;
  end;

  procedure _balanceRight (var this : tAVLtree; var index : tIdxFile; var pivot : idxRange);
  var
    pivotNode, newBranchRoot, auxNode : tNode;
    newBranchRootIdx, parentIdx       : idxRange;
    hLeft, hRight                     : integer;
  begin
    pivotNode        := _getByReference(index, pivot);
    parentIdx        := pivotNode.parent;
    newBranchRootIdx := pivotNode.left;
    newBranchRoot    := _getByReference(index, newBranchRootIdx);

    {check if need to re-balance}
    hLeft            := _heightByReference(index, newBranchRoot.left);
    hRight           := _heightByReference(index, newBranchRoot.right);
    if (hRight > hLeft) then
      begin
        _balanceLeft(this, index, newBranchRootIdx);
        pivotNode        := _getByReference(index, pivot);
        newBranchRootIdx := pivotNode.left;
        newBranchRoot    := _getByReference(index, newBranchRootIdx);
      end;

    pivotNode.left       := newBranchRoot.right;
    newBranchRoot.right  := pivot;
    newBranchRoot.parent := parentIdx;
    pivotNode.parent     := newBranchRootIdx;

    _updateParent(this, index, parentIdx, pivot, newBranchRootIdx);

    if pivotNode.left <> NULLIDX then
      begin
        auxNode          := _getByReference(index, pivotNode.left);
        auxNode.parent   := pivot;
        _setByReference(index, pivotNode.left, auxNode);
      end;

    _setByReference(index, pivot, pivotNode);
    _setByReference(index, newBranchRootIdx, newBranchRoot);

    pivot := newBranchRootIdx;
  end;

  procedure _balanceBranch (var this : tAVLtree; var index : tIdxFile; var pivot : idxRange);
  var
    hLeft, hRight  : integer;
    node           : tNode;
  begin
    node   := _getByReference(index, pivot);
    hLeft  := _heightByReference(index, node.left);
    hRight := _heightByReference(index, node.right);
    if abs(hLeft - hRight) > 1 then
      if hLeft > hRight then
        _balanceRight (this, index, pivot)
      else
        _balanceLeft  (this, index, pivot);
  end;

  procedure _balanceIfNeeded (var this : tAVLtree; var index : tIdxFile; pivot : idxRange);
  var
    currentIdx  : idxRange;
  begin
    currentIdx  := pivot;
    while currentIdx <> NULLIDX do
      begin
        _balanceBranch(this, index, currentIdx);
        currentIdx := _parent(index, currentIdx);
      end;
  end;

  function _searchByUser      (var this : tAVLtree; key : tKey; var pos: idxRange) : boolean;
  var
    found, searchNext : boolean;
    curNodeIdx        : idxRange;
    curNode           : tNode;
    rc                : tControlRecord;
  begin
    found      := false;
    rc         := _getControl(this);
    curNodeIdx := rc.root1;
    searchNext := not (pos = NULLIDX);

    while (curNodeIdx <> NULLIDX) and (not found) do
      begin
        curNode := _getByUser(this, curNodeIdx);
        if keyEq(curNode.key, key) and not searchNext then
          begin
            found := true;
            pos   := curNodeIdx;
          end
        else
          begin
            if searchNext and (pos = curNodeIdx) then
              searchNext := false;
            pos := curNodeIdx;
            if keyGt(key, curNode.key) then
              curNodeIdx := curNode.right
            else
              curNodeIdx := curNode.left;
          end;
      end;
    _searchByUser := found;
  end;

  function searchByUser      (var this : tAVLtree; key : tKey; var pos: idxRange) : boolean;
  var
    found      : boolean;
  begin
    _openTree(this);
    found := _searchByUser(this, key, pos);
    _closeTree(this);
    searchByUser := found;
  end;

  function  _searchByCategory    (var this : tAVLtree; key : tKey; var pos: idxRange) : boolean;
  var
    found, searchNext : boolean;
    curNodeIdx        : idxRange;
    curNode           : tNode;
    rc                : tControlRecord;
  begin
    found      := false;
    rc         := _getControl(this);
    curNodeIdx := rc.root2;
    searchNext := not (pos = NULLIDX);

    while (curNodeIdx <> NULLIDX) and (not found) do
      begin
        curNode := _getByCategory(this, curNodeIdx);
        if keyEq(curNode.key, key) and not searchNext then
          begin
            found := true;
            pos   := curNodeIdx;
          end
        else
          begin
            if searchNext and (pos = curNodeIdx) then
              searchNext := false;
            pos := curNodeIdx;
            if keyGt(key, curNode.key) then
              curNodeIdx := curNode.right
            else
              curNodeIdx := curNode.left;
          end;
      end;
    _searchByCategory := found;
  end;

  function searchByCategory      (var this : tAVLtree; key : tKey; var pos: idxRange) : boolean;
  var
    found      : boolean;
  begin
    _openTree(this);
    found := _searchByCategory(this, key, pos);
    _closeTree(this);
    searchByCategory := found;
  end;

  function _appendData (var this : tAVLtree; var item : tPublish) : idxRange;
  var
    pos     : idxRange;
  begin
    pos        := filesize(this.data);
    _setPublication(this, pos, item);
    _appendData := pos;
  end;

  function _appendByUser (var this : tAVLtree; var item : tNode) : idxRange;
  var
    rc      : tControlRecord;
    pos     : idxRange;
    auxNode : tNode;
  begin
    rc  := _getControl(this);
    pos := NULLIDX;
    if rc.erased1 = NULLIDX then
      begin
        pos        := filesize(this.idxByUser);
        item.right := NULLIDX;
        item.index := NULLIDX;
        item.left  := NULLIDX;
        _setByReference(this.idxByUser, pos, item);
      end
    else
      begin
        pos        := rc.erased1;
        auxNode    := _getByReference(this.idxByUser, pos);
        rc.erased1 := auxNode.parent;
        item.right := NULLIDX;
        item.left  := NULLIDX;
        _setByReference(this.idxByUser, pos, item);
        _setControl(this, rc);
      end;
    _appendByUser := pos;
  end;

  function _appendByCategory (var this : tAVLtree; var item : tNode) : idxRange;
  var
    rc      : tControlRecord;
    pos     : idxRange;
    auxNode : tNode;
  begin
    rc  := _getControl(this);
    pos := NULLIDX;
    if rc.erased2 = NULLIDX then
      begin
        pos        := filesize(this.idxByCategory);
        item.right := NULLIDX;
        item.index := NULLIDX;
        item.left  := NULLIDX;
        _setByReference(this.idxByCategory, pos, item);
      end
    else
      begin
        pos        := rc.erased2;
        auxNode    := _getByReference(this.idxByCategory, pos);
        rc.erased2 := auxNode.parent;
        item.right := NULLIDX;
        item.left  := NULLIDX;
        _setByReference(this.idxByCategory, pos, item);
        _setControl(this, rc);
      end;
    _appendByCategory := pos;
  end;

  procedure _append (var this : tAVLtree; var publication : tPublish);
  var
    nodeUser, nodeCat, parent : tNode;
    rc                        : tControlRecord;
    auxIdxUser, auxIdxCat, posUser, posCat   : idxRange;
  begin
    posUser := NULLIDX;
    posCat  := NULLIDX;
    { add user node }
    _searchByUser(this, publication.idUser, posUser);
    nodeUser.key    := publication.idUser;
    nodeUser.parent := posUser;
    auxIdxUser      := _appendByUser(this, nodeUser);

    { add category node }
    _searchByCategory(this, publication.idCategory, posCat);
    nodeCat.key    := publication.idCategory;
    nodeCat.parent := posCat;
    auxIdxCat      := _appendByCategory(this, nodeCat);

    { add publicatio id }
    rc        := _getControl(this);
    rc.lastID := rc.lastID + 1;
    _setControl(this, rc);
    publication.id := rc.lastID;

    { set index for publication }
    if nodeUser.index = NULLIDX then { we need to create a new publication }
      begin
        nodeUser.index := _appendData(this, publication);
        nodeCat.index  := nodeUser.index;
        _setByUser(this, auxIdxUser, nodeUser);
        _setByCategory(this, auxIdxCat, nodeCat);
      end
    else
      _setPublication(this, nodeUser.index, publication);

    if posUser = NULLIDX then //empty tree, insert at root
      begin
        rc      := _getControl(this);
        rc.root1 := auxIdxUser;
        _setControl(this, rc);
      end
    else
      begin
        parent := _getByUser(this, posUser);
        if keyGt(nodeUser.key, parent.key) then
          parent.right := auxIdxUser
        else
          parent.left  := auxIdxUser;
        _setByUser(this, posUser, parent);
      end;

    if posCat = NULLIDX then //empty tree, insert at root
      begin
        rc      := _getControl(this);
        rc.root2 := auxIdxCat;
        _setControl(this, rc);
      end
    else
      begin
        parent := _getByCategory(this, posCat);
        if keyGt(nodeCat.key, parent.key) then
          parent.right := auxIdxCat
        else
          parent.left  := auxIdxCat;
        _setByCategory(this, posCat, parent);
      end;

    
    _balanceIfNeeded(this, this.idxByUser, posUser);
    _balanceIfNeeded(this, this.idxByCategory, posCat);
  end;

  { todo }
  procedure _detach (var this : tAVLtree; posUser, posCat : idxRange; var nodeUser, nodeCat : tNode);
  var
    rc : tControlRecord;
  begin
    rc              := _getControl(this);
    
    nodeUser.right  := NULLIDX;
    nodeUser.left   := NULLIDX;
    nodeUser.parent := rc.erased1;
    rc.erased1      := posUser;
    _setByReference(this.idxByUser, posUser, nodeUser);
    
    nodeCat.right   := NULLIDX;
    nodeCat.left    := NULLIDX;
    nodeCat.parent  := rc.erased2;
    rc.erased1      := posCat;
    _setByReference(this.idxByUser, posCat, nodeCat);

    _setControl(this, rc);
  end;

  procedure _detachByUser (var this : tAVLtree; pos : idxRange; var node : tNode);
  var
    rc : tControlRecord;
  begin
    rc              := _getControl(this);
    
    node.right  := NULLIDX;
    node.left   := NULLIDX;
    node.parent := rc.erased1;
    rc.erased1  := pos;
    _setByReference(this.idxByUser, pos, node);

    _setControl(this, rc);
  end;

  procedure _detachByCategory (var this : tAVLtree; pos : idxRange; var node : tNode);
  var
    rc : tControlRecord;
  begin
    rc              := _getControl(this);
    
    node.right  := NULLIDX;
    node.left   := NULLIDX;
    node.parent := rc.erased1;
    rc.erased2  := pos;
    _setByReference(this.idxByCategory, pos, node);

    _setControl(this, rc);
  end;

  function _isLeaf(var node : tNode) : boolean;
  begin
    _isLeaf := (node.left = NULLIDX) and (node.right = NULLIDX);
  end;

  function _getBiggerFromBranch (var this : tIdxFile; var pivot : idxRange) : tKey;
  var
    node : tNode;
  begin
    node := _getByReference(this, pivot);
    while (node.right <> NULLIDX) do
      begin
        pivot := node.right;
        node  := _getByReference(this, pivot);
      end;
    _getBiggerFromBranch := node.key;
  end;

  function _getSmallerFromBranch (var this : tIdxFile; var pivot : idxRange) : tKey;
  var
    node : tNode;
  begin
    node := _getByReference(this, pivot);
    while (node.left <> NULLIDX) do
      begin
        pivot := node.left;
        node  := _getByReference(this, pivot);
      end;
    _getSmallerFromBranch := node.key;
  end;

  {Public}
  procedure loadTree         (var this : tAVLtree; path, filename : string);
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

    assign(this.idxByUser, fullFileName + '.ntx_1');
    reset(this.idxByUser);
    dataError := IOResult <> 0;

    assign(this.idxByCategory, fullFileName + '.ntx_2');
    reset(this.idxByCategory);
    dataError := IOResult <> 0;

    assign(this.control, fullFileName + '.ctrl');
    reset(this.control);
    controlError := IOResult <> 0;
    {$I+}

    if (controlError and dataError) then
      begin
        rewrite(this.data);
        rewrite(this.control);
        rc.lastID  := 0;
        rc.root1   := NULLIDX;
        rc.erased1 := NULLIDX;
        rc.root2   := NULLIDX;
        rc.erased2 := NULLIDX;
        _setControl(this, rc);
      end;

    _closeTree(this);
  end;

  procedure newEmptyTree     (var this : tAVLtree; path, filename : string);
  var
    fullFileName : string;
    rc : tControlRecord;
  begin
    fullFileName := path + filename;
    {$I-}
    assign(this.data,          fullFileName + '.dat');
    assign(this.idxByUser,     fullFileName + '.ntx_1');
    assign(this.idxByCategory, fullFileName + '.ntx_2');
    assign(this.control,       fullFileName + '.ctrl');
    rewrite(this.data);
    rewrite(this.idxByUser);
    rewrite(this.idxByCategory);
    rewrite(this.control);

    rc.root1   := NULLIDX;
    rc.erased1 := NULLIDX;
    rc.root2   := NULLIDX;
    rc.erased2 := NULLIDX;
    rc.lastID  := 0;
    _setControl(this, rc);

    _closeTree(this);
    {$I+}
  end;

  function  isEmpty          (var this : tAVLtree) : boolean;
  var
    empty : boolean;
    rc    : tControlRecord;
  begin
    _openTree(this);
    rc    := _getControl(this);
    empty := rc.root1 = NULLIDX;
    _closeTree(this);
    isEmpty := empty;
  end;

  procedure append           (var this : tAVLtree; publication : tPublish);
  begin
    _openTree(this);
    _append(this, publication);
    _closeTree(this);
  end;

  procedure update             (var this : tAVLtree; publication : tPublish);
  var
    pos  : idxRange;
    node : tNode;
  begin
    _openTree(this);
    _searchByUser(this, publication.idUser, pos);
    node := _getByUser(this, pos);
    _setPublication(this, node.index, publication);
    _append(this, publication);
    _closeTree(this);
  end;

  procedure _removeByUser         (var this: tAVLtree; pos: idxRange);
  var
    node, parent   : tNode;
    rc             : tControlRecord;
    auxIdx         : idxRange;
    replacementKey : tKey;
  begin
    node        := _getByReference(this.idxByUser, pos);

    if _isLeaf(node) then //easy
      begin
        if node.parent = NULLIDX then // is the root
          begin
            rc       := _getControl(this);
            rc.root1 := NULLIDX;
            _setControl(this, rc);
          end
        else
          begin
            auxIdx := node.parent;
            parent := _getByReference(this.idxByUser, auxIdx);
            if parent.right = pos then
              parent.right := NULLIDX
            else
              parent.left  := NULLIDX;
            _setByReference(this.idxByUser, auxIdx, parent);
          end;
        _detachByUser(this, pos, node);
      end
    else
      begin
        if node.right = NULLIDX then
          begin
            auxIdx         := node.left;
            replacementKey := _getBiggerFromBranch(this.idxByUser, auxIdx);
          end
        else
          begin
            auxIdx         := node.right;
            replacementKey := _getSmallerFromBranch(this.idxByUser, auxIdx);
          end;
        node.key := replacementKey;
        _setByReference(this.idxByUser, pos, node);
        _removeByUser(this, auxIdx);
      end;
  end;

  procedure _removeByCategory         (var this: tAVLtree; pos: idxRange);
  var
    node, parent   : tNode;
    rc             : tControlRecord;
    auxIdx         : idxRange;
    replacementKey : tKey;
  begin
    node        := _getByReference(this.idxByCategory, pos);

    if _isLeaf(node) then //easy
      begin
        if node.parent = NULLIDX then // is the root
          begin
            rc       := _getControl(this);
            rc.root2 := NULLIDX;
            _setControl(this, rc);
          end
        else
          begin
            auxIdx := node.parent;
            parent := _getByReference(this.idxByCategory, auxIdx);
            if parent.right = pos then
              parent.right := NULLIDX
            else
              parent.left  := NULLIDX;
            _setByReference(this.idxByCategory, auxIdx, parent);
          end;
        _detachByCategory(this, pos, node);
      end
    else
      begin
        if node.right = NULLIDX then
          begin
            auxIdx         := node.left;
            replacementKey := _getBiggerFromBranch(this.idxByCategory, auxIdx);
          end
        else
          begin
            auxIdx         := node.right;
            replacementKey := _getSmallerFromBranch(this.idxByCategory, auxIdx);
          end;
        node.key := replacementKey;
        _setByReference(this.idxByCategory, pos, node);
        _removeByCategory(this, auxIdx);
      end;
  end;

  procedure remove          (var this: tAVLtree; publication: tPublish);
  var
    pos : idxRange;
  begin
    _openTree(this);
    searchByUser(this, publication.idUser, pos);
    _removeByUser(this, pos);
    _balanceIfNeeded(this, this.idxByUser, pos);
    searchByCategory(this, publication.idCategory, pos);
    _removeByCategory(this, pos);
    _balanceIfNeeded(this, this.idxByCategory, pos);
    _closeTree(this);
  end;

  function  fetch            (var this : tAVLtree; pos: idxRange) : tPublish;
  var
    node : tPublish;
  begin
    _openTree(this);
    node := _getCategoryByPos(this, pos);
    _closeTree(this);
    fetch := node;
  end;

  function  fetchByUser        (var this : tAVLtree; pos: idxRange) : tPublish;
  var
    publish : tPublish;
    node    : tNode;
  begin
    _openTree(this);
    node    := _getByUser(this, pos);
    publish := _getCategoryByPos(this, node.index);
    _closeTree(this);
    fetchByUser := publish;
  end;

  function  fetchByCategory    (var this : tAVLtree; pos: idxRange) : tPublish;
  var
    publish : tPublish;
    node    : tNode;
  begin
    _openTree(this);
    node    := _getByCategory(this, pos);
    publish := _getCategoryByPos(this, node.index);
    _closeTree(this);
    fetchByCategory := publish;
  end;

  function  rootOfUsers      (var this : tAVLtree) : idxRange;
  var
    rc : tControlRecord;
  begin
    _openTree(this);
    rc := _getControl(this);
    _closeTree(this);
    rootOfUsers := rc.root1;
  end;

  function  rootOfCategories (var this : tAVLtree) : idxRange;
  var
    rc : tControlRecord;
  begin
    _openTree(this);
    rc := _getControl(this);
    _closeTree(this);
    rootOfCategories := rc.root2;
  end;

  function  _leftChild        (var this : tIdxFile; pos: idxRange) : idxRange;
  var
    node : tNode;
  begin
    node := _getByReference(this, pos);
    _leftChild := node.left;
  end;

  function  _rightChild       (var this : tIdxFile; pos: idxRange) : idxRange;
  var
    node : tNode;
  begin
    node := _getByReference(this, pos);
    _rightChild := node.right;
  end;

  function  leftUserChild      (var this : tAVLtree; pos: idxRange) : idxRange;
  begin
    _openTree(this);
    leftUserChild := _leftChild(this.idxByUser, pos);
    _closeTree(this);
  end;

  function  rightUserChild     (var this : tAVLtree; pos: idxRange) : idxRange;
  begin
    _openTree(this);
    rightUserChild := _rightChild(this.idxByUser, pos);
    _closeTree(this);
  end;

  function  leftCategoryChild  (var this : tAVLtree; pos: idxRange) : idxRange;
  begin
    _openTree(this);
    leftCategoryChild := _leftChild(this.idxByCategory, pos);
    _closeTree(this);
  end;
  function  rightCategoryChild (var this : tAVLtree; pos: idxRange) : idxRange;
  begin
    _openTree(this);
    rightCategoryChild := _rightChild(this.idxByCategory, pos);
    _closeTree(this);
  end;

  function  parentByUser     (var this : tAVLtree; pos: idxRange) : idxRange;
  var
    idx : idxRange;
  begin
    _openTree(this);
    idx := _parent(this.idxByUser, pos);
    _closeTree(this);
    parentByUser := idx;
  end;

  function  parentByCategory (var this : tAVLtree; pos: idxRange) : idxRange;
  var
    idx : idxRange;
  begin
    _openTree(this);
    idx := _parent(this.idxByCategory, pos);
    _closeTree(this);
    parentByCategory := idx;
  end;

end.
