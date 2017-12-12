unit oca.movements;

interface

uses
  sysutils;

const
  NULLIDX  = -1;

type
  idxRange       = NULLIDX..MAXINT;
  tOcaMovement   = record
                     player : integer;
                     dice   : integer;
                     next   : idxRange;
                   end;
  tControlRecord = record
                     first  : idxRange;
                     last   : idxRange;
                     erased : idxRange;
                     count  : integer;
                   end;
  tControl       = file of tControlRecord;
  tData          = file of tOcaMovement;
  tQueueOcaMvmt  = record
                     data    : tData;
                     control : tControl;
                   end;

//(var this : tQueueOcaMvmt);
  procedure loadQueue     (var this : tQueueOcaMvmt; path, filename : string);
  procedure newEmptyQueue (var this : tQueueOcaMvmt; path, filename : string);
  procedure insert        (var this : tQueueOcaMvmt; item : tOcaMovement);
  procedure deletePos     (var this : tQueueOcaMvmt; pos : idxRange);
  procedure deleteItem    (var this : tQueueOcaMvmt; item : tOcaMovement);

  procedure queue         (var this : tQueueOcaMvmt; item : tOcaMovement);
  function  dequeue       (var this : tQueueOcaMvmt) : tOcaMovement;
  function  peek          (var this : tQueueOcaMvmt) : tOcaMovement;
  function  createMovement(var this : tQueueOcaMvmt; player, movement : integer) : tOcaMovement;

  function  get           (var this : tQueueOcaMvmt; pos : idxRange) : tOcaMovement;
  function  isEmpty       (var this : tQueueOcaMvmt) : boolean;
  function  length        (var this : tQueueOcaMvmt) : integer;
  function  first         (var this : tQueueOcaMvmt) : idxRange;
  function  last          (var this : tQueueOcaMvmt) : idxRange;
  function  next          (var this : tQueueOcaMvmt; pos : idxRange) : idxRange;

  function  isValidPos    (var this : tQueueOcaMvmt; pos : idxRange) : boolean;

implementation

function getControlRecord(var this : tQueueOcaMvmt) : tControlRecord;
var
  Rc : tControlRecord;
begin
  reset(this.control);
  seek (this.control, 0);
  read (this.control, Rc);
  close(this.control);
  getControlRecord := Rc;
end;

procedure setControlRecord(var this : tQueueOcaMvmt; Rc : tControlRecord);
begin
  reset(this.control);
  seek (this.control, 0);
  write(this.control, Rc);
  close(this.control);
end;

procedure loadQueue (var this : tQueueOcaMvmt; path, filename : string);
var
  fullFileName : string;
  Rc           : tControlRecord;
begin
  fullFileName := path + filename;

  //check if data file exists
  assign(this.data, fullFileName + '.dat');
  if not fileexists(fullFileName + '.dat') then
    rewrite(this.data)
  else
    reset(this.data);
  close(this.data);

  //check if data file exists
  assign(this.control, fullFileName + '.ctrl');
  if not fileexists(fullFileName + '.ctrl') then
    begin
      rewrite(this.control);
      Rc.first  := NULLIDX;
      Rc.last   := NULLIDX;
      Rc.erased := NULLIDX;
      Rc.count  := 0;
      seek(this.control, 0);
      write(this.control, Rc);
    end
  else
    reset(this.control);
  close(this.control);
end;

procedure newEmptyQueue (var this : tQueueOcaMvmt; path, filename : string);
var
  fullFileName : string;
  Rc           : tControlRecord;
begin
  fullFileName := path + filename;

  //check if data file exists
  assign(this.data, fullFileName + '.dat');
  rewrite(this.data);
  close(this.data);

  //check if data file exists
  assign(this.control, fullFileName + '.ctrl');
  rewrite(this.control);
  Rc.first  := NULLIDX;
  Rc.last   := NULLIDX;
  Rc.erased := NULLIDX;
  Rc.count  := 0;
  seek(this.control, 0);
  write(this.control, Rc);
  close(this.control);
end;

function  isEmpty (var this : tQueueOcaMvmt) : Boolean;
var
  Rc : tControlRecord;
begin
  Rc      := getControlRecord(this);
  isEmpty := Rc.first = NULLIDX;
end;

function  length (var this : tQueueOcaMvmt) : integer;
var
  Rc : tControlRecord;
begin
  Rc     := getControlRecord(this);
  length := Rc.count;
end;

function  first (var this : tQueueOcaMvmt) : idxRange;
var
  Rc : tControlRecord;
begin
  Rc    := getControlRecord(this);
  first := Rc.first;
end;

function  last (var this : tQueueOcaMvmt) : idxRange;
var
  Rc : tControlRecord;
begin
  Rc   := getControlRecord(this);
  last := Rc.last;
end;

function  get (var this : tQueueOcaMvmt; pos : idxRange) : tOcaMovement;
var
  item: tOcaMovement;
begin
  reset (this.data);
  seek  (this.data, pos);
  read  (this.data, item);
  close (this.data);
  get := item;
end;

procedure update (var this : tQueueOcaMvmt; pos : idxRange; var item : tOcaMovement);
begin
  reset (this.data);
  seek  (this.data, pos);
  write (this.data, item);
  close (this.data);
end;

function append (var this : tQueueOcaMvmt; var item : tOcaMovement) : idxRange;
var
  Rc      : tControlRecord;
  pos     : idxRange;
  auxItem : tOcaMovement;
begin
  Rc := getControlRecord(this);
  if Rc.erased = NULLIDX then
    begin
      reset(this.data);
      pos := FileSize(this.data);
      seek (this.data, pos);
      write(this.data, item);
      close(this.data);
    end
  else
    begin
      pos       := Rc.erased;
      auxItem   := get(this, Rc.erased);
      Rc.erased := auxItem.next;

      update(this, pos, item);

      setControlRecord(this, Rc);
    end;
  append := pos;
end;

function  next (var this : tQueueOcaMvmt; pos : idxRange) : idxRange;
var
  Rc   : tControlRecord;
  Ridx : idxRange;
  item : tOcaMovement;
begin
  if pos = NULLIDX then
    begin
      Rc   := getControlRecord(this);
      next := Rc.first;
    end
  else
    begin
      item := get(this, pos);
      next := item.next;
    end;
end;

procedure insert (var this : tQueueOcaMvmt; item : tOcaMovement);
var
  pos, auxPos : idxRange;
  auxItem     : tOcaMovement;
  Rc          : tControlRecord;
begin
  Rc := getControlRecord(this);
  if Rc.first = Rc.last then
    begin
      auxPos   := append(this, item);
      Rc.first := auxPos;
      Rc.last  := auxPos;
      Rc.count := 1;
      setControlRecord(this, Rc);
    end
  else
    begin
      auxPos  := append(this, item);

      if pos = Rc.last then
        setControlRecord(this, Rc);

      auxItem := get(this, pos);
      pos     := auxItem.next;

      auxItem.next := auxPos;
      update(this, auxPos, auxItem);

      item.next := pos;
      update(this, pos, item);

      Rc.count := Rc.count + 1;
      setControlRecord(this, Rc);
    end;
end;

procedure deletePos (var this : tQueueOcaMvmt; pos : idxRange);
begin

end;

procedure deleteItem (var this : tQueueOcaMvmt; item : tOcaMovement);
begin

end;

function  isValidPos (var this : tQueueOcaMvmt; pos : idxRange) : Boolean;
begin
  isValidPos := pos <> NULLIDX;
end;

procedure queue         (var this : tQueueOcaMvmt; item : tOcaMovement);
var
  Rc      : tControlRecord;
  itemPos : idxRange;
  auxItem : tOcaMovement;
  auxPos  : idxRange;
begin
  itemPos := append(this, item);
  Rc      := getControlRecord(this);
  if Rc.first = NULLIDX then
    begin
      Rc.first := itemPos;
      Rc.last  := itemPos;
    end
  else
    begin
      auxPos  := Rc.last;
      auxItem := get(this, auxPos);
      auxItem.next := itemPos;
      update(this, auxPos, auxItem);
      Rc.last := itemPos;
    end;
  setControlRecord(this, Rc);
end;


function  dequeue (var this : tQueueOcaMvmt) : tOcaMovement;
var
  Rc      : tControlRecord;
  itemPos : idxRange;
  auxItem : tOcaMovement;
  auxPos  : idxRange;
begin
  Rc      := getControlRecord(this);
  auxPos  := Rc.first;
  auxItem := get(this, auxPos);


  Rc.first     := auxItem.next;
  auxItem.next := Rc.erased;
  Rc.erased    := auxPos;


  update(this, auxPos, auxItem);
  setControlRecord(this, Rc);


  auxItem.next := NULLIDX;
  dequeue := auxItem;
end;

function  peek (var this : tQueueOcaMvmt) : tOcaMovement;
var
  Rc      : tControlRecord;
  auxItem : tOcaMovement;
  auxPos  : idxRange;
begin
  Rc      := getControlRecord(this);
  auxPos  := Rc.first;
  auxItem := get(this, auxPos);

  auxItem.next := NULLIDX;
  peek    := auxItem;
end;

function  createMovement(var this : tQueueOcaMvmt; player, movement : integer) : tOcaMovement;
var
  item : tOcaMovement;
begin
  item.player    := player;
  item.dice      := movement;
  item.next      := NULLIDX;
  createMovement := item;
end;

end.
