unit detailedCategoryReportPublicationForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  Buttons, reportform, metru.core;

type
  tDetailedCategoryPublicationForm = class(TReportFormWidget)
  	procedure doReport; override;
    private
      myList : TList;
  end;

  tRegister = class
                id           : integer;
                itemName     : string;
                totalArt     : integer;
                totalSells   : integer;
                totalPaused  : integer;
                totalBlocked : integer 
              end;

implementation

{ tUserReportPublicationForm }

function compareByQuantity(Item1, Item2: Pointer): Integer;
var
  catRecord1, catRecord2 : tRegister;
begin
  catRecord1 := tRegister(Item1);
  catRecord2 := tRegister(Item2);

  if catRecord1.totalArt < catRecord2.totalArt then
    Result := 1
  else if catRecord1.totalArt = catRecord2.totalArt then
    Result := 0
  else 
    Result := -1;
end;

procedure tDetailedCategoryPublicationForm.doReport();
var
  list          : tCategoryList;
  pubList       : tPublishIdxList;
  publication   : tPublish;
  category      : tCategory;
  i, j, count, idx : longint;
  catRecord     : tRegister;     
begin
  self.myList       := TList.Create;
  dataGrid.ColCount := 7;
  list              := metru.core.retrieveAllCateogies(metruApp);
  count             := length(list);
  dataGrid.RowCount := count + 1;

  { todo }
  with dataGrid do
    begin
      Cells[1,0] := 'id Cateogria';
      Cells[2,0] := 'Nombre';
      Cells[3,0] := 'Cantidad de publicaciones';
      Cells[4,0] := 'Cantidad de ventas';
      Cells[5,0] := 'Cantidad de publicaciones pausadas';
      Cells[6,0] := 'Cantidad de publicaciones bloqueadas';
    end;

  for i := 0 to count - 1 do
    begin
      metru.core.dereferenceCategory(metruApp, list[i], category);
      idx                    := i + 1;
      pubList                := metru.core.retrievePublicationByCategory(metruApp, category);
      catRecord              := tRegister.Create;
      catRecord.id           := category.id;
      catRecord.itemName     := category.categoryName;
      catRecord.totalArt     := length(pubList);
      catRecord.totalSells   := 0;
      catRecord.totalPaused  := 0;
      catRecord.totalBlocked := 0;
      for j := 0 to catRecord.totalArt - 1 do
        begin
          metru.core.dereferencePublication(metruApp, pubList[j], publication);
          if publication.status = tStatus.Sold    then catRecord.totalSells := catRecord.totalSells + 1;
          if publication.status = tStatus.Paused  then catRecord.totalPaused := catRecord.totalPaused + 1;
          if publication.status = tStatus.Blocked then catRecord.totalBlocked := catRecord.totalBlocked + 1;
        end;
      myList.Add(catRecord);
    end;

    myList.Sort(@compareByQuantity);

    for i := 0 to myList.Count-1 do
      begin
        idx                    := i + 1;
        catRecord              := tRegister(myList[i]);
        dataGrid.Cells[0, idx] := IntToStr(idx);
        dataGrid.Cells[1, idx] := IntToStr(catRecord.id);
        dataGrid.Cells[2, idx] := catRecord.itemName;
        dataGrid.Cells[3, idx] := IntToStr(catRecord.totalArt);
        dataGrid.Cells[4, idx] := IntToStr(catRecord.totalSells);
        dataGrid.Cells[5, idx] := IntToStr(catRecord.totalPaused);
        dataGrid.Cells[6, idx] := IntToStr(catRecord.totalBlocked);
      end;
end;

end.
