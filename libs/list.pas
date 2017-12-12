unit oca.space;

interface

uses
  sysutils;

const
  NULLIDX  = -1;

type
  idxRange       = NULLIDX..MAXINT;
  tOcaSpace      = record
                     cell     : integer;
                     next     : idxRange;
                   end;
  tControlRecord = record
                     first  : idxRange;
                     last   : idxRange;
                     erased : idxRange;
                     count  : integer;
                   end;
  tControl       = file of tControlRecord;
  tData          = file of tOcaSpace;
  tListOcaSpace  = record
                     data    : tData;
                     control : tControl;
                   end;

  procedure loadList      (var this : tListOcaSpace; path, filename : string);
  procedure newEmptyList  (var this : tListOcaSpace; path, filename : string);
  procedure insert        (var this : tListOcaSpace; item : tOcaSpace);
  procedure deletePos     (var this : tListOcaSpace; pos : idxRange);
  procedure deleteItem    (var this : tListOcaSpace; item : tOcaSpace);

  function  get           (var this : tListOcaSpace; pos : idxRange) : tOcaSpace;
  function  isEmpty       (var this : tListOcaSpace) : boolean;
  function  length        (var this : tListOcaSpace) : integer;
  function  first         (var this : tListOcaSpace) : idxRange;
  function  last          (var this : tListOcaSpace) : idxRange;
  function  next          (var this : tListOcaSpace; pos : idxRange) : idxRange;
  function  prev          (var this : tListOcaSpace; pos : idxRange) : idxRange;
  function  search        (var this : tListOcaSpace; cell : integer; var pos : idxRange) : boolean;

  function  isValidPos    (var this : tListOcaSpace; pos : idxRange) : boolean;
  function  generateSpace (var this : tListOcaSpace; cell: integer): tOcaSpace;

implementation

function getControlRecord(var this : tListOcaSpace) : tControlRecord;
var
  Rc : tControlRecord;
begin
  reset(this.control);
  seek (this.control, 0);
  read (this.control, Rc);
  close(this.control);
  getControlRecord := Rc;
end;

procedure setControlRecord(var this : tListOcaSpace; Rc : tControlRecord);
begin
  reset(this.control);
  seek (this.control, 0);
  write(this.control, Rc);
  close(this.control);
end;

procedure loadList (var this : tListOcaSpace; path, filename : string);
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

procedure newEmptyList (var this : tListOcaSpace; path, filename : string);
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

function  isEmpty (var this : tListOcaSpace) : Boolean;
var
  Rc : tControlRecord;
begin
  Rc      := getControlRecord(this);
  isEmpty := Rc.count = 0;
end;

function  length (var this : tListOcaSpace) : integer;
var
  Rc : tControlRecord;
begin
  Rc     := getControlRecord(this);
  length := Rc.count;
end;

function  first (var this : tListOcaSpace) : idxRange;
var
  Rc : tControlRecord;
begin
  Rc    := getControlRecord(this);
  first := Rc.first;
end;

function  last (var this : tListOcaSpace) : idxRange;
var
  Rc : tControlRecord;
begin
  Rc   := getControlRecord(this);
  last := Rc.last;
end;

function  get (var this : tListOcaSpace; pos : idxRange) : tOcaSpace;
var
  item: tOcaSpace;
begin
  reset (this.data);
  seek  (this.data, pos);
  read  (this.data, item);
  close (this.data);
  get := item;
end;

procedure update (var this : tListOcaSpace; pos : idxRange; var item : tOcaSpace);
begin
  reset (this.data);
  seek  (this.data, pos);
  write (this.data, item);
  close (this.data);
end;

function append (var this : tListOcaSpace; var item : tOcaSpace) : idxRange;
var
  Rc      : tControlRecord;
  pos     : idxRange;
  auxItem : tOcaSpace;
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

function  next (var this : tListOcaSpace; pos : idxRange) : idxRange;
var
  Rc   : tControlRecord;
  Ridx : idxRange;
  item : tOcaSpace;
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

function  prev (var this : tListOcaSpace; pos : idxRange) : idxRange;
var
  Rc   : tControlRecord;
  idx  : idxRange;
  item : tOcaSpace;
begin
  Rc   := getControlRecord(this);
  if pos = Rc.first then
    begin
      prev := NULLIDX;
    end
  else
    begin
      idx := Rc.first;
      item := get(this, idx);
      while item.next <> pos do
        begin
          idx := item.next;
          item := get(this, idx);
        end;
      prev := idx;
    end;
end;

function search (var this : tListOcaSpace; cell : integer; var pos : idxRange) : boolean;
var
  found : boolean;
  Rc    : tControlRecord;
  Ridx  : idxRange;
  item  : tOcaSpace;
begin
  found := false;
  Rc    := getControlRecord(this);
  pos   := NULLIDX;
  if Rc.first <> NULLIDX then
    begin
      repeat
        Ridx := next(this, pos);
        item := get(this, Ridx);
        if item.cell = cell then
          found := true
        else
          if (item.cell < cell) then
            pos := Ridx;
      until found or (Ridx = Rc.last) or (item.cell > cell);
    end;

  if found then pos := Ridx;

  search := found;
end;

procedure insert (var this : tListOcaSpace; item : tOcaSpace);
var
  pos, itemPos : idxRange;
  auxItem      : tOcaSpace;
  Rc           : tControlRecord;
begin
  Rc := getControlRecord(this);
  if Rc.first = NULLIDX then
    begin
      itemPos  := append(this, item);
      Rc.first := itemPos;
      Rc.last  := itemPos;
      Rc.count := 1;
      setControlRecord(this, Rc);
    end
  else
    if not search(this, item.cell, pos) then
      begin
        itemPos := append(this, item);

        //if pos is null, needs to be added at the beginning
        if pos = NULLIDX then
          begin
            item.next := Rc.first;
            Rc.first  := itemPos;
          end
        else
          begin
            auxItem := get(this, pos);
            item.next := auxItem.next;
            auxItem.next := itemPos;
            update(this, pos, auxItem);
          end;

        update(this, itemPos, item);

        //if pos to add was the last one, update last
        if Rc.last = pos then
          Rc.last := itemPos;

        Rc.count := Rc.count + 1;
        setControlRecord(this, Rc);
      end;
end;

function  generateSpace (var this : tListOcaSpace; cell : integer): tOcaSpace;
var
  item: tOcaSpace;
begin
  item.cell     := cell;
  item.next     := NULLIDX;
  generateSpace := item;
end;

procedure deletePos (var this : tListOcaSpace; pos : idxRange);
begin

end;

procedure deleteItem (var this : tListOcaSpace; item : tOcaSpace);
begin

end;

function  isValidPos (var this : tListOcaSpace; pos : idxRange) : Boolean;
begin
  isValidPos := pos <> NULLIDX;
end;

end.
