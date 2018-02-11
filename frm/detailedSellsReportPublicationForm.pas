unit detailedSellsReportPublicationForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  Buttons, daterangedreportform, metru.core;

type
  tDetailedSellsReportPublicationForm = class(TDateRangeReportFormWidget)
  	procedure doReport; override;
    private
      myList : tList;
  end;

  tRegister = class
                idBuyer          : longint;
                idItem           : longint;
                itemName         : string;
                price            : Currency;
                publishDate      : TDateTime;
                sellDate         : TDateTime;
                itemType         : tSellItemType;
                calification     : tCalification;
                tax              : double;
                alreadyCollected : boolean;
              end;

implementation

{ tUserReportPublicationForm }

procedure tDetailedSellsReportPublicationForm.doReport();
var
  fromDate, toDate : tDateTime;
  sellIdx          : tSellIdx;
  sell             : tSell;
  found            : boolean;
  sellRecord       : tRegister;
  idx, i, count    : integer;
  total            : double;
begin
  fromDate := self.fromDateEdit.Date;
  toDate   := self.toDateEdit.Date;
  myList   := tList.Create;

  dataGrid.ColCount := 11;

  with dataGrid do
    begin
      Cells[1,0]  := 'id User comprador';
      Cells[2,0]  := 'id publication';
      Cells[3,0]  := 'Nombre de la publicacion';
      Cells[4,0]  := 'Precio';
      Cells[5,0]  := 'Fecha de publicacion';
      Cells[6,0]  := 'Fecha de venta';
      Cells[7,0]  := 'Tipo';
      Cells[8,0]  := 'Calificacion';
      Cells[9,0]  := 'Impuesto interno';
      Cells[10,0] := 'Cobrado';
    end;
  
  found    := metru.core.retrieveFirstSell(metruApp, sellIdx);
  while found do
    begin
      metru.core.dereferenceSell(metruApp, sellIdx, sell);
      if (fromDate <= sell.sellDate) and (sell.sellDate <= toDate) then
        begin
          sellRecord                  := tRegister.Create;
          sellRecord.idBuyer          := sell.idBuyer;
          sellRecord.idItem           := sell.idItem;
          sellRecord.itemName         := sell.itemName;
          sellRecord.price            := sell.price;
          sellRecord.publishDate      := sell.publishDate;
          sellRecord.sellDate         := sell.sellDate;
          sellRecord.itemType         := sell.itemType;
          sellRecord.calification     := sell.calification;
          sellRecord.tax              := sell.tax;
          sellRecord.alreadyCollected := sell.alreadyCollected;
          myList.Add(sellRecord);
        end;
      found := metru.core.retrieveNextSell(metruApp, sellIdx);
    end;

  count             := myList.Count;
  dataGrid.RowCount := count + 2;
  total             := 0.0;
  idx               := 0;

  for i := 0 to count - 1 do
    begin
      sellRecord              := tRegister(myList[i]);
      idx                     := i + 1;
      dataGrid.Cells[0, idx]  := IntToStr(idx);
      dataGrid.Cells[1, idx]  := IntToStr(sellRecord.idBuyer);
      dataGrid.Cells[2, idx]  := IntToStr(sellRecord.idItem);
      dataGrid.Cells[3, idx]  := sellRecord.itemName;
      dataGrid.Cells[4, idx]  := FloatToStr(sellRecord.price);
      dataGrid.Cells[5, idx]  := DateTimeToStr(sellRecord.publishDate);
      dataGrid.Cells[6, idx]  := DateTimeToStr(sellRecord.sellDate);
      if sellRecord.itemType = tSellItemType.New then
        dataGrid.Cells[7, idx]  := 'Nuevo'
      else
        dataGrid.Cells[7, idx]  := 'Usado';
      case sellRecord.calification of
        tCalification.None    : dataGrid.Cells[8, idx]  := 'Sin calificar';
        tCalification.Good    : dataGrid.Cells[8, idx]  := 'Buena';
        tCalification.Neutral : dataGrid.Cells[8, idx]  := 'Neutral';
        tCalification.Bad     : dataGrid.Cells[8, idx]  := 'Mala';
      end;          
      dataGrid.Cells[9, idx]  := FloatToStr(sellRecord.tax);
      if sellRecord.alreadyCollected then
        begin
          dataGrid.Cells[10, idx] := 'Cobrado';
          total                   := total + (sellRecord.price * (sellRecord.tax / 100));
        end
      else
        dataGrid.Cells[10, idx] := 'Pendiente de cobro';
    end;
  idx                     := idx + 1;
  dataGrid.Cells[9, idx]  := 'TOTAL: ';
  dataGrid.Cells[10, idx] := FloatToStr(total);
end;

end.
