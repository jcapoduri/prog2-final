unit oca.modifiers;

interface

uses
  sysutils;

const
  NULLIDX  = -1;

type
  idxRange       = NULLIDX..MAXINT;
  tModifiers     = (None, Goose, Dice, Bridge, Prison, Inn, Pit, Labyrinth, Death);
  tOcaModifier    = record
                     modifier : tModifiers;
                     cell     : integer;
                     next     : idxRange;
                   end;
  tControlRecord = record
                     first  : idxRange;
                     erased : idxRange;
                   end;
  tControl       = file of tControlRecord;
  tData          = file of tOcaModifier;
  tStackOca      = record
                     data    : tData;
                     control : tControl;
                   end;

  procedure loadStack        (var this : tStackOca; path, filename : string);
  procedure newEmptyStack    (var this : tStackOca; path, filename : string);
  procedure push             (var this : tStackOca; item : tOcaModifier);
  function  peek             (var this : tStackOca) : tOcaModifier;
  function  pop              (var this : tStackOca) : tOcaModifier;
  function  isEmpty          (var this : tStackOca) : boolean;
  function  search           (var this : tStackOca; cell : integer; var modifier : tModifiers) : boolean;
  function  nextAfter        (var this : tStackOca; modifier : tOcaModifier; var cellnumber : integer) : boolean;
  function  existsCell       (var this : tStackOca; cell: integer) : boolean;
  function  generateModifier (var this : tStackOca;  modifier : tModifiers; cell: integer) : tOcaModifier;


implementation

function getControlRecord(var this : tStackOca) : tControlRecord;
var
  Rc : tControlRecord;
begin
  reset(this.control);
  seek (this.control, 0);
  read (this.control, Rc);
  close(this.control);
  getControlRecord := Rc;
end;

function quickGetControlRecord(var this : tStackOca) : tControlRecord;
var
  Rc : tControlRecord;
begin
  seek (this.control, 0);
  read (this.control, Rc);
  quickGetControlRecord := Rc;
end;

procedure setControlRecord(var this : tStackOca; Rc : tControlRecord);
begin
  reset(this.control);
  seek (this.control, 0);
  write(this.control, Rc);
  close(this.control);
end;

procedure quickSetControlRecord(var this : tStackOca; Rc : tControlRecord);
begin
  seek (this.control, 0);
  write(this.control, Rc);
end;

procedure loadStack (var this : tStackOca; path, filename : string);
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
      Rc.erased := NULLIDX;
      seek(this.control, 0);
      write(this.control, Rc);
    end
  else
    reset(this.control);
  close(this.control);
end;

procedure newEmptyStack (var this : tStackOca; path, filename : string);
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
  Rc.erased := NULLIDX;
  seek(this.control, 0);
  write(this.control, Rc);
  close(this.control);
end;

function  isEmpty (var this : tStackOca) : Boolean;
var
  Rc : tControlRecord;
begin
  Rc      := getControlRecord(this);
  isEmpty := Rc.first = NULLIDX;
end;

function  quickIsEmpty (var this : tStackOca) : Boolean;
var
  Rc : tControlRecord;
begin
  Rc           := quickGetControlRecord(this);
  quickIsEmpty := Rc.first = NULLIDX;
end;

// inner function
function  get (var this : tStackOca; pos : idxRange) : tOcaModifier;
var
  item: tOcaModifier;
begin
  reset (this.data);
  seek  (this.data, pos);
  read  (this.data, item);
  close (this.data);
  get := item;
end;

function  quickGet (var this : tStackOca; pos : idxRange) : tOcaModifier;
var
  item: tOcaModifier;
begin
  seek  (this.data, pos);
  read  (this.data, item);
  quickGet := item;
end;

procedure update (var this : tStackOca; pos : idxRange; var item : tOcaModifier);
begin
  reset (this.data);
  seek  (this.data, pos);
  write (this.data, item);
  close (this.data);
end;

procedure quickUpdate (var this : tStackOca; pos : idxRange; var item : tOcaModifier);
begin
  seek  (this.data, pos);
  write (this.data, item);
end;

function append (var this : tStackOca; var item : tOcaModifier) : idxRange;
var
  Rc      : tControlRecord;
  pos     : idxRange;
  auxItem : tOcaModifier;
begin
  Rc  := getControlRecord(this);
  pos := NULLIDX;
  if Rc.erased = NULLIDX then
    begin
      reset(this.data);
      pos := filesize(this.data);
      seek(this.data, pos);
      item.next := NULLIDX;
      write(this.data, item);
      close(this.data);
    end
  else
    begin
      pos       := Rc.erased;
      auxItem   := get(this, pos);
      Rc.erased := auxItem.next;
      item.next := NULLIDX;

      update(this, pos, item);
      setControlRecord(this, Rc);
    end;
  append := pos;
end;

function quickAppend (var this : tStackOca; var item : tOcaModifier) : idxRange;
var
  Rc      : tControlRecord;
  pos     : idxRange;
  auxItem : tOcaModifier;
begin
  Rc  := quickGetControlRecord(this);
  pos := NULLIDX;
  if Rc.erased = NULLIDX then
    begin
      pos := filesize(this.data);
      seek(this.data, pos);
      item.next := NULLIDX;
      write(this.data, item);
    end
  else
    begin
      pos       := Rc.erased;
      auxItem   := quickGet(this, pos);
      Rc.erased := auxItem.next;
      item.next := NULLIDX;

      quickUpdate(this, pos, item);
      quickSetControlRecord(this, Rc);
    end;
  quickAppend := pos;
end;

procedure push (var this : tStackOca; item : tOcaModifier);
var
  auxPos : idxRange;
  Rc     : tControlRecord;
begin
  Rc := getControlRecord(this);
  if Rc.first = NULLIDX then
    begin
      auxPos   := append(this, item);
      Rc       := getControlRecord(this);
      Rc.first := auxPos;
    end
  else
    begin
      auxPos      := append(this, item);
      Rc          := getControlRecord(this);
      item.next   := Rc.first;
      Rc.first    := auxPos;
      update(this, auxPos, item);
    end;

  setControlRecord(this, Rc);
end;

procedure quickPush (var this : tStackOca; item : tOcaModifier);
var
  auxPos : idxRange;
  Rc     : tControlRecord;
begin
  Rc := quickGetControlRecord(this);
  if Rc.first = NULLIDX then
    begin
      auxPos   := quickAppend(this, item);
      Rc       := quickGetControlRecord(this);
      Rc.first := auxPos;
    end
  else
    begin
      auxPos      := quickAppend(this, item);
      Rc          := quickGetControlRecord(this);
      item.next   := Rc.first;
      Rc.first    := auxPos;
      quickUpdate(this, auxPos, item);
    end;

  quickSetControlRecord(this, Rc);
end;

function  peek (var this : tStackOca) : tOcaModifier;
var
  pos, auxPos : idxRange;
  auxItem     : tOcaModifier;
  Rc          : tControlRecord;
begin
  Rc := getControlRecord(this);
  if not (Rc.first = NULLIDX) then
    begin
      auxItem := get(this, Rc.first);
    end;
  peek := auxItem;
end;

function  pop (var this : tStackOca) : tOcaModifier;
var
  pos, auxPos : idxRange;
  auxItem     : tOcaModifier;
  Rc          : tControlRecord;
begin
  Rc := getControlRecord(this);
  if not (Rc.first = NULLIDX) then
    begin
      auxItem      := get(this, Rc.first);
      auxPos       := auxItem.next;
      auxItem.next := Rc.erased;
      update(this, Rc.first, auxItem);

      Rc.erased    := Rc.first;
      Rc.first     := auxPos;
      setControlRecord(this, Rc);
      auxItem.next := NULLIDX;
      pop          := auxItem;
    end;
end;

function  quickPop (var this : tStackOca) : tOcaModifier;
var
  pos, auxPos : idxRange;
  auxItem     : tOcaModifier;
  Rc          : tControlRecord;
begin
  Rc := quickGetControlRecord(this);
  if not (Rc.first = NULLIDX) then
    begin
      auxItem      := quickGet(this, Rc.first);
      auxPos       := auxItem.next;
      auxItem.next := Rc.erased;
      quickUpdate(this, Rc.first, auxItem);

      Rc.erased    := Rc.first;
      Rc.first     := auxPos;
      quickSetControlRecord(this, Rc);
      auxItem.next := NULLIDX;
      quickPop     := auxItem;
    end;
end;

function  quickSearch (var this : tStackOca; cell : integer; var modifier : tModifiers) : boolean;
var
  item   : tOcaModifier;
begin
  modifier := None;
  item := quickPop(this);
  if item.cell = cell then
    begin
      modifier := item.modifier;
      quickSearch := true;
    end
  else
    if quickIsEmpty(this) then
      quickSearch := false
    else
      quickSearch := quickSearch(this, cell, modifier);

  quickPush(this, item);
end;

function  search (var this : tStackOca; cell : integer; var modifier : tModifiers) : boolean;
var
  found : boolean;
begin
  reset(this.control);
  reset(this.data);
  found := quickSearch(this, cell, modifier);
  close(this.data);
  close(this.control);
  search := found;
end;

function  innerNextAfter (var this : tStackOca; modifier : tOcaModifier; var cellnumber : integer; alreadyFound : boolean) : boolean;
var
  item : tOcaModifier;
begin
  if isEmpty(this) then
    innerNextAfter := false
  else
    begin
      item := pop(this);
      if alreadyFound then
        if item.modifier = modifier.modifier then
          begin
            cellnumber     := item.cell;
            innerNextAfter := true;
          end
        else
          innerNextAfter := innerNextAfter(this, modifier, cellnumber, alreadyFound)
      else
        begin
          if item.cell = modifier.cell then
            alreadyFound := true;
          innerNextAfter := innerNextAfter(this, modifier, cellnumber, alreadyFound);
        end;
      push(this, item);
    end;
end;

function  nextAfter (var this : tStackOca; modifier : tOcaModifier; var cellnumber : integer) : boolean;
begin
  nextAfter := innerNextAfter(this, modifier, cellnumber, false);
end;

function  existsCell (var this : tStackOca; cell: integer) : boolean;
var
  item: tOcaModifier;
begin
  if isEmpty(this) then
    existsCell := false
  else
    begin
      item := pop(this);
      if (item.cell = cell) then
        existsCell := true
      else
        existsCell := existsCell(this, cell);
      push(this, item);
    end;
end;

function  generateModifier (var this : tStackOca;  modifier : tModifiers; cell: integer) : tOcaModifier;
var
  item: tOcaModifier;
begin
  item.modifier    := modifier;
  item.cell        := cell;
  item.next        := NULLIDX;
  generateModifier := item;
end;

end.
