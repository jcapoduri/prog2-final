unit topNUserReportPublicationForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, Math,
  Buttons, reportform, metru.core;

type
  tTopNUserReportPublicationForm = class(TReportFormWidget)
  	procedure doReport; override;
    private
      myList : tList;
  end;

  tRegister = class
                id         : longint;
                email      : string;
                fullname   : string;
                address    : string;
                sells      : integer;
              end;

implementation

{ tTopNUserReportPublicationForm }

function compareByQuantity(Item1, Item2: Pointer): Integer;
var
  userRecord1, userRecord2 : tRegister;
begin
  userRecord1 := tRegister(Item1);
  userRecord2 := tRegister(Item2);

  if userRecord1.sells < userRecord2.sells then
    Result := 1
  else if userRecord1.sells = userRecord2.sells then
    Result := 0
  else 
    Result := -1;
end;

procedure tTopNUserReportPublicationForm.doReport();
var
  userInput            : string;
  ok, found            : boolean;
  nUserValue, i, count : integer;
  idx                  : integer;
  iterator             : tUserIterator;
  usr                  : tUser;
  sellsList            : tSellIdxList;
  userRecord           : tRegister;
begin
  ok                := InputQuery('Reporte', 'Por favor, ingrese la cantidad de usuarios tope que quiere mostrar', userInput);
  nUserValue        := StrToIntDef(userInput, 0);
  dataGrid.ColCount := 6;

  with dataGrid do
    begin
      Cells[1,0] := 'id User';
      Cells[2,0] := 'Email';
      Cells[3,0] := 'Nombre Completo';
      Cells[4,0] := 'Direccion';
      Cells[5,0] := 'Cantidad de ventas';
    end;

  if ok and (nUserValue > 0) then
    begin
      myList    := tList.Create;
      found     := metru.core.retrieveFirstUser(metruApp, iterator);
      while found do
        begin
          metru.core.retrieveUser(metruApp, iterator, usr);
          sellsList           := metru.core.retrieveAllMyPurchase(metruApp, usr);
          userRecord          := tRegister.Create;
          userRecord.id       := usr.id;
          userRecord.email    := usr.email;
          userRecord.fullname := usr.fullname;
          userRecord.address  := usr.address;
          userRecord.sells    := length(sellsList);
          myList.add(userRecord);
          found := metru.core.retrieveNextUser(metruApp, iterator);
        end;

      myList.Sort(@compareByQuantity);

      count := min(myList.Count, nUserValue);
      dataGrid.RowCount := count + 1;

      for i := 0 to count - 1 do
        begin
          idx                    := i + 1;
          userRecord             := tRegister(myList[i]);
          dataGrid.Cells[0, idx] := IntToStr(idx);
          dataGrid.Cells[1, idx] := IntToStr(userRecord.id);
          dataGrid.Cells[2, idx] := userRecord.email;
          dataGrid.Cells[3, idx] := userRecord.fullname;
          dataGrid.Cells[4, idx] := userRecord.address;
          dataGrid.Cells[5, idx] := IntToStr(userRecord.sells);
        end;
    end;
end;

end.
