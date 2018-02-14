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
  tKey           = longint;
  tLinkedKeys    = array of tKey;
  tPercentage    = 0..100;  
  tNode          = record
                     id       : tKey;
                     idUser   : tKey;
                     lastID   : longint;
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

  procedure loadTree            (var this : tTrinaryTree; path, filename : string);
  procedure newEmptyTree        (var this : tTrinaryTree; path, filename : string);
  function  isEmpty             (var this : tTrinaryTree) : boolean;
   
  procedure insertMessage       (var this : tTrinaryTree; pk, sk : tKey; var msg : tMessage);
  procedure updateMessage       (var this : tTrinaryTree; pos : idxRange; var msg : tMessage);
  function  fetchMessage        (var this : tTrinaryTree; pos : idxRange) : tMessage;

  function  retrieveFirstMsgIdx   (var this : tTrinaryTree; pk, sk : tKey; var idxMsg : idxRange) : boolean; overload;
  function  retrieveFirstMsgIdx   (var this : tTrinaryTree; pk : tKey; var idxMsg : idxRange) : boolean; overload;
  function  retrieveNextMsgIdx    (var this : tTrinaryTree; pk, sk : tKey; var idxMsg : idxRange) : boolean; overload;
  function  retrieveNextMsgIdx    (var this : tTrinaryTree; pk : tKey; var idxMsg : idxRange) : boolean; overload;
  function  retrieveAllLinkedKeys (var this : tTrinaryTree; pk : tKey) : tLinkedKeys;

  function  getTotalLevels        (var this : tTrinaryTree) : integer;
  function  getThreshold          (var this : tTrinaryTree) : integer;
  function  getTotalNodesPerLevel (var this : tTrinaryTree; lvl : integer) : integer;
  procedure balance               (var this : tTrinaryTree);

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
    lvl.totalNodes := 0;
    seek(this.levels, pos);
    if filesize(this.levels) <= pos then
      begin
        write(this.levels, lvl);
        seek(this.levels, pos);
      end;
    read(this.levels, lvl);
    _getLevel := lvl;
  end;

  procedure _setLevel(var this : tTrinaryTree; pos : longint; lvl : tLevel);
  begin
    seek(this.levels, pos);
    write(this.levels, lvl);
  end;

  function _nodeDistanceToRoot(var this : tTrinaryTree; node: tNode) : integer;
  var 
    auxNode  : tNode;
    distance : integer;
  begin
    distance := 0;
    auxNode := node;
    while auxNode.parent <> NULLIDX do
      begin
        auxNode  := _get(this, auxNode.parent);
        distance := distance + 1;
      end;
    _nodeDistanceToRoot := distance;
  end;

  procedure _increaseLevel(var this : tTrinaryTree; node: tNode);
  var
    level  : tLevel;
    lvlNro : integer;
    rc     : tControlRecord;
  begin
    rc               := _getControl(this);
    lvlNro           := _nodeDistanceToRoot(this, node);
    level            := _getLevel(this, lvlNro);
    level.totalNodes := level.totalNodes + 1;
    _setLevel(this, lvlNro, level);
    if rc.lastLevel < lvlNro then
      rc.lastLevel := lvlNro;
    _setControl(this, rc);
  end;

  procedure _decreaseLevel(var this : tTrinaryTree; node: tNode);
  var
    level  : tLevel;
    lvlNro : integer;
    rc     : tControlRecord;
  begin
    rc               := _getControl(this);
    lvlNro           := _nodeDistanceToRoot(this, node);
    level            := _getLevel(this, lvlNro);
    level.totalNodes := level.totalNodes - 1;
    _setLevel(this, lvlNro, level);
    if (rc.lastLevel = lvlNro) and (level.totalNodes = 0) then
      rc.lastLevel := rc.lastLevel - 1;
    _setControl(this, rc);
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
    node.lastID := 0;
    node.first  := NULLIDX;
    node.last   := NULLIDX;
    node.right  := NULLIDX;
    node.center := NULLIDX;
    node.left   := NULLIDX;
    if rc.erasedIndexes = NULLIDX then
      pos          := filesize(this.index)
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

  { balance function }
  procedure _balanceRight (var this : tTrinaryTree; var pivot : idxRange); forward;

  procedure _balanceLeft(var this : tTrinaryTree; var pivot : idxRange);
  var
    pivotNode, newBranchRoot, auxNode : tNode;
    newBranchRootIdx, parentIdx          : idxRange;
    hLeft, hRight                        : integer;
  begin
    pivotNode        := _get(this, pivot);
    parentIdx        := pivotNode.parent;
    newBranchRootIdx := pivotNode.right;
    newBranchRoot    := _get(this, newBranchRootIdx);

    {check if need to re-balance}
    hLeft            := _height(this, newBranchRoot.left);
    hRight           := _height(this, newBranchRoot.right);
    if (hLeft > hRight) then
      begin
        _balanceRight(this, newBranchRootIdx);
        pivotNode        := _get(this, pivot);
        newBranchRootIdx := pivotNode.right;
        newBranchRoot    := _get(this, newBranchRootIdx);
      end;

    pivotNode.right      := newBranchRoot.left;
    newBranchRoot.left   := pivot;
    newBranchRoot.parent := parentIdx;
    pivotNode.parent     := newBranchRootIdx;

    _updateParent(this, parentIdx, pivot, newBranchRootIdx);

    if pivotNode.right <> NULLIDX then
      begin
        auxNode          := _get(this, pivotNode.right);
        auxNode.parent   := pivot;
        _set(this, pivotNode.right, auxNode);
      end;

    _set(this, pivot, pivotNode);
    _set(this, newBranchRootIdx, newBranchRoot);

    pivot := newBranchRootIdx;
  end;

  procedure _balanceRight (var this : tTrinaryTree; var pivot : idxRange);
  var
    pivotNode, newBranchRoot, auxNode : tNode;
    newBranchRootIdx, parentIdx       : idxRange;
    hLeft, hRight                     : integer;
  begin
    pivotNode        := _get(this, pivot);
    parentIdx        := pivotNode.parent;
    newBranchRootIdx := pivotNode.left;
    newBranchRoot    := _get(this, newBranchRootIdx);

    {check if need to re-balance}
    hLeft            := _height(this, newBranchRoot.left);
    hRight           := _height(this, newBranchRoot.right);
    if (hRight > hLeft) then
      begin
        _balanceLeft(this, newBranchRootIdx);
        pivotNode        := _get(this, pivot);
        newBranchRootIdx := pivotNode.left;
        newBranchRoot    := _get(this, newBranchRootIdx);
      end;

    pivotNode.left       := newBranchRoot.right;
    newBranchRoot.right  := pivot;
    newBranchRoot.parent := parentIdx;
    pivotNode.parent     := newBranchRootIdx;

    _updateParent(this, parentIdx, pivot, newBranchRootIdx);

    if pivotNode.left <> NULLIDX then
      begin
        auxNode          := _get(this, pivotNode.left);
        auxNode.parent   := pivot;
        _set(this, pivotNode.left, auxNode);
      end;

    _set(this, pivot, pivotNode);
    _set(this, newBranchRootIdx, newBranchRoot);

    pivot := newBranchRootIdx;
  end;

  function _balanceBranch (var this : tTrinaryTree; var pivot : idxRange) : boolean;
  var
    hLeft, hRight  : integer;
    node           : tNode;
    balanced       : boolean;
  begin
    node     := _get(this, pivot);
    hLeft    := _height(this, node.left);
    hRight   := _height(this, node.right);
    balanced := true;
    if abs(hLeft - hRight) > 1 then
      begin
        balanced := false;
        if hLeft > hRight then
          _balanceRight (this, pivot)
        else
          _balanceLeft  (this, pivot);
      end;
    _balanceBranch := balanced;
  end;

  function _balance (var this : tTrinaryTree; var pivot : idxRange) : boolean;
  var
    node     : tNode;
    balanced : boolean;
  begin
    if pivot = NULLIDX then
      balanced := true
    else
      begin
        begin
          node     := _get(this, pivot);
          balanced := _balance(this, node.left) and _balance(this, node.right) and _balanceBranch(this, pivot);
        end
      end;
    _balance := balanced;
  end;

  procedure _checkLevel (var this : tTrinaryTree; pivot : idxRange);
  var
    node : tNode;
  begin
    if pivot <> NULLIDX then
      begin
        node := _get(this, pivot);
        _checkLevel(this, node.left);
        _checkLevel(this, node.right);
        _increaseLevel(this, node);
      end;
  end;

  procedure balance (var this : tTrinaryTree);
  var
    rc       : tControlRecord;
    pivot    : idxRange;
    balanced : boolean;
    i        : integer;
    level    : tLevel;
  begin
    _openTree(this);
    repeat
      begin
        rc       := _getControl(this);
        pivot    := rc.root;
        balanced := _balance(this, pivot);
      end
    until balanced;
    
    { recreate levels }
    rc       := _getControl(this);
    { reset levels }
    level.totalNodes := 0;
    for i := 0 to rc.lastLevel do
      _setLevel(this, i, level);
    rc.lastLevel := -1;
    _setControl(this, rc);
    _checkLevel(this, rc.root);

    _closeTree(this);
  end;
  { end balance function }

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

  function _searchNodeByPk(var this : tTrinaryTree; key : tKey; var pos : idxRange) : boolean;
  var
    found      : boolean;
    curNodeIdx : idxRange;
    curNode    : tNode;
    rc         : tControlRecord;
  begin
    found      := false;
    rc         := _getControl(this);
    pos        := NULLIDX;
    curNodeIdx := rc.root;

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
    _searchNodeByPk := found;
  end;

  function _searchNodeBySk(var this : tTrinaryTree; key : tKey; var pos : idxRange) : boolean;
  var
    found      : boolean;
    curNodeIdx : idxRange;
    curNode    : tNode;
  begin
    found      := false;
    curNodeIdx := pos;

    while (curNodeIdx <> NULLIDX) and (not found) do
      begin
        curNode := _get(this, curNodeIdx);
        if keyEq(curNode.idUser, key) then
          begin
            found := true;
            pos   := curNodeIdx;
          end
        else
          begin
            pos := curNodeIdx;
            if keyGt(key, curNode.idUser) then
              curNodeIdx := curNode.right
            else
              curNodeIdx := curNode.left;
          end;
      end;
    _searchNodeBySk := found;
  end;

  function _searchNode(var this : tTrinaryTree; pk, sk : tKey; var pos : idxRange) : boolean;
  var
    found      : boolean;
    curNodeIdx : idxRange;
    curNode    : tNode;
  begin
    pos   := NULLIDX;
    found := _searchNodeByPk(this, pk, curNodeIdx);
    if found then
      begin
        curNode := _get(this, curNodeIdx);
        pos     := curNode.center;
        found   := _searchNodeBySk(this, sk, pos);
      end;
    _searchNode := found;
  end;

  procedure _insertByPk(var this : tTrinaryTree; key : tKey; parentPos, childPos : idxRange);
  var
    node   : tNode;
    rc   : tControlRecord;
  begin
    rc := _getControl(this);
    if (parentPos = NULLIDX) and (rc.root = NULLIDX) then
      begin
        rc.root := childPos;
        _setControl(this, rc);
      end
    else
      begin
        node := _get(this, parentPos);
        if keyGt(key, node.id) then
          node.right := childPos
        else
          node.left := childPos;
        _set(this, parentPos, node)
      end;
  end;

  procedure _insertBySk(var this : tTrinaryTree; key : tKey; parentPos, childPos : idxRange);
  var
    node   : tNode;
  begin
      node := _get(this, parentPos);
      if keyGt(key, node.idUser) then
        node.right := childPos
      else
        node.left := childPos;
      _set(this, parentPos, node);
  end;

  function _retrieveOrCreate(var this : tTrinaryTree; pk, sk : tKey) : idxRange;
  var
    found                  : boolean;
    pos, auxPos, parentPos : idxRange;
    node, auxNode          : tNode;
  begin
    found := _searchNodeByPk(this, pk, pos);
    if not found then
      begin
        {create node for publication}
        node.id     := pk;
        node.idUser := NULLIDX;
        node.parent := pos;
        parentPos   := _appendNode(this, node);
        _insertByPk(this, pk, pos, parentPos);
        _increaseLevel(this, node);
        { create node for messages}
        auxNode.id     := pk;
        auxNode.idUser := sk;
        auxNode.parent := parentPos;
        auxPos         := _appendNode(this, auxNode);
        node.center    := auxPos;
        _set(this, parentPos, node); 
        { return pos }
        pos         := auxPos;
      end
    else
      begin
        node      := _get(this, pos);
        parentPos := node.center;
        found     := _searchNodeBySk(this, sk, parentPos);
        if not found then
          begin
            auxNode.id     := pk;
            auxNode.idUser := sk;
            auxNode.parent := parentPos;
            auxPos         := _appendNode(this, auxNode);
            _insertBySk(this, sk, parentPos, auxPos);
            pos := auxPos;
          end
        else
          pos := parentPos;
      end;
    _retrieveOrCreate := pos;
  end;

{-------------------------------------------------------- Public --------------------------------------------------------}

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
        rc.lastLevel        := -1;
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
    rc.lastLevel        := -1;
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

  procedure insertMessage       (var this : tTrinaryTree; pk, sk : tKey; var msg : tMessage);
  var
    pos    : idxRange;
    node   : tNode;
    auxMsg : tMessage;
    idxMsg : idxRange;
  begin
    _openTree(this);
    msg.next    := NULLIDX;
    pos         := _retrieveOrCreate(this, pk, sk);
    node        := _get(this, pos);
    node.lastID := node.lastID + 1;
    msg.number  := node.lastID;
    idxMsg      := _appendData(this, msg);    
    if node.first = NULLIDX then { first message}
      begin
        node.first := idxMsg;
        node.last  := idxMsg;        
      end
    else
      begin
        auxMsg      := _getMessage(this, node.last);
        auxMsg.next := idxMsg;
        _setMessage(this, node.last, auxMsg);
        node.last   := idxMsg;        
      end;
    _set(this, pos, node);
    _closeTree(this);
  end;

  procedure updateMessage       (var this : tTrinaryTree; pos : idxRange; var msg : tMessage);
  var
    auxMsg : tMessage;
  begin
    _openTree(this);
    auxMsg           := _getMessage(this, pos);
    auxMsg.question  := msg.question;
    auxMsg.answer    := msg.answer;
    auxMsg.timestamp := msg.timestamp;
    _setMessage(this, pos, auxMsg);
    _closeTree(this);
  end;

  function  fetchMessage        (var this : tTrinaryTree; pos : idxRange) : tMessage;
  var
    auxMsg : tMessage;
  begin
    _openTree(this);
    auxMsg       := _getMessage(this, pos);
    _closeTree(this);
    fetchMessage := auxMsg;
  end;

  function  retrieveFirstMsgIdx (var this : tTrinaryTree; pk, sk : tKey; var idxMsg : idxRange) : boolean; overload;
  var
    found  : boolean;
    node   : tNode;
    pos    : idxRange;
  begin
    _openTree(this);
    idxMsg := NULLIDX;
    found  := _searchNodeByPk(this, pk, pos);
    if found then 
      found := _searchNodeBySk(this, sk, pos);

    if found then
      begin
        node   := _get(this, pos);
        idxMsg := node.first;
      end;
    _closeTree(this);
    retrieveFirstMsgIdx := found;
  end;

  function  retrieveNextMsgIdx  (var this : tTrinaryTree; pk, sk : tKey; var idxMsg : idxRange) : boolean; overload;
  var
    found  : boolean;
    node   : tNode;
    auxMsg : tMessage;
    pos    : idxRange;
  begin
    _openTree(this);
    idxMsg := NULLIDX;
    found  := _searchNodeByPk(this, pk, pos);
    if found then 
      begin
        node  := _get(this, pos);
        pos   := node.center;
        found := _searchNodeBySk(this, sk, pos);
      end;

    if found then
      begin
        node   := _get(this, pos);
        if pos = node.last then
          found := false
        else
          begin
            auxMsg := _getMessage(this, pos);
            idxMsg := auxMsg.next;
          end;
      end;
    _closeTree(this);
    retrieveNextMsgIdx := found;
  end;

  function  retrieveFirstMsgIdx   (var this : tTrinaryTree; pk : tKey; var idxMsg : idxRange) : boolean; overload;
  var
    auxNode  : tNode;
    pos      : idxRange;
    list     : tLinkedKeys;
    i, count : integer;
    found    : boolean;
  begin
    _openTree(this);
    idxMsg := NULLIDX;
    list   := retrieveAllLinkedKeys(this, pk);
    count  := length(list);
    found  := false;

    if count > 0 then
      begin
        _searchNode(this, pk, list[i], pos);
        auxNode := _get(this, pos);
        idxMsg  := auxNode.first;
        found   := true;
      end;

    _closeTree(this);
    retrieveFirstMsgIdx := found;
  end;

  function _retrieveNextNodeFromLastMessage (var this : tTrinaryTree; pk : tKey; var idxMsg : idxRange) : boolean;
  var
    auxNode          : tNode;
    pos              : idxRange;
    list             : tLinkedKeys;
    i, count         : integer;
    msgIdxAux        : idxRange;
    found, keepGoing : boolean;
  begin
    list  := retrieveAllLinkedKeys(this, pk);
    i     := 0;
    count := length(list);
    found := false;
    keepGoing := true;

    while (i < count) and (not found) and keepGoing do
      begin
        _searchNode(this, pk, list[i], pos);
        auxNode := _get(this, pos);
        if auxNode.last = idxMsg then
          begin
            i := i + 1;
            if i = count then
              keepGoing := false
            else
              begin
                _searchNode(this, pk, list[i], pos);
                auxNode := _get(this, pos);
                idxMsg  := auxNode.first;
                found   := true;
              end;
          end
        else
          i := i + 1;
      end;
    _retrieveNextNodeFromLastMessage := found;
  end;

  function  retrieveNextMsgIdx    (var this : tTrinaryTree; pk : tKey; var idxMsg : idxRange) : boolean; overload;
  var
    found  : boolean;
    node   : tNode;
    auxMsg : tMessage;
    pos    : idxRange;
  begin
    _openTree(this);
    auxMsg := _getMessage(this, idxMsg);
    if auxMsg.next = NULLIDX then
      begin
        found := _retrieveNextNodeFromLastMessage(this, pk, idxMsg);
      end
    else
      idxMsg := auxMsg.next;
      
    _closeTree(this);
    retrieveNextMsgIdx := found;
    
  end;

  operator + (a, b : tLinkedKeys) c: tLinkedKeys;
  var
    i :longint;
  begin
    SetLength(c, Length(a) + Length(b));
    for i := 0 to High(a) do
        c[i] := a[i];
    for i := 0 to High(b) do
        c[i + Length(a)] := b[i];
  end;

  function _retrieveAllLinkedKeys (var this : tTrinaryTree; pos : idxRange) : tLinkedKeys;
  var
    node          : tNode;
    list, auxList : tLinkedKeys;
  begin
    if pos <> NULLIDX then
      begin
        SetLength(list, 1);
        node    := _get(this, pos);
        list[0] := node.idUser;
        auxList := _retrieveAllLinkedKeys(this, node.left);
        list    := auxList + list;
        auxList := _retrieveAllLinkedKeys(this, node.right);
        list    := list + auxList;
      end;
    _retrieveAllLinkedKeys := list;
  end;

  function  retrieveAllLinkedKeys (var this : tTrinaryTree; pk : tKey) : tLinkedKeys;
  var
    node  : tNode;
    pos   : idxRange;
    list  : tLinkedKeys;
    found : boolean;
  begin
    SetLength(list, 0);
    found := _searchNodeByPk(this, pk, pos);
    if found then
      begin
        node := _get(this, pos);
        list := _retrieveAllLinkedKeys(this, node.center)
      end;
    retrieveAllLinkedKeys := list;
  end;

  function  getTotalLevels        (var this : tTrinaryTree) : integer;
  var
    rc : tControlRecord;
  begin
    _openTree(this);
    rc := _getControl(this);
    _closeTree(this);
    getTotalLevels := rc.lastLevel;
  end;

  function  getThreshold          (var this : tTrinaryTree) : integer;
  var
    rc : tControlRecord;
  begin
    _openTree(this);
    rc := _getControl(this);
    _closeTree(this);
    getThreshold := rc.balanceThreshold;
  end;

  function  getTotalNodesPerLevel (var this : tTrinaryTree; lvl : integer) : integer;
  var
    level : tLevel;
  begin
    _openTree(this);
    level := _getLevel(this, lvl);
    _closeTree(this);
    getTotalNodesPerLevel := level.totalNodes;
  end;

end.
