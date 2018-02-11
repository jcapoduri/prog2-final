unit purchasesByUserReportPublicationForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  Buttons, reportform, metru.core;

type
  tPurchasesByUserReportPublicationForm = class(TReportFormWidget)
  	procedure doReport; override;
  end;

implementation

{ tUserReportPublicationForm }

procedure tPurchasesByUserReportPublicationForm.doReport();
var
  userInput        : string;
  ok, found        : boolean;
  userId, i, count : integer;
  idx              : integer;
  usr              : tUser;
  sellsList        : tSellIdxList;
  sellItem         : tSell;
begin
  ok            := InputQuery('Reporte', 'Por favor, ingrese el id del usuario a listar', userInput);
  userId        := StrToIntDef(userInput, 0);
  if ok and (userId > 0) then
    ok := metru.core.retrieveUser(metruApp, userId, usr);
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

  if ok then
    begin
      sellsList := metru.core.retrieveAllMyPurchase(metruApp, usr);
      count     := length(sellsList);
      dataGrid.RowCount := count + 1;
      for i := 0 to count - 1 do
        begin
          metru.core.dereferenceSell(metruApp, sellsList[i], sellItem);
          idx                    := i + 1;
          dataGrid.Cells[0, idx]  := IntToStr(idx);
          dataGrid.Cells[1, idx]  := IntToStr(sellItem.idBuyer);
          dataGrid.Cells[2, idx]  := IntToStr(sellItem.idItem);
          dataGrid.Cells[3, idx]  := sellItem.itemName;
          dataGrid.Cells[4, idx]  := FloatToStr(sellItem.price);
          dataGrid.Cells[5, idx]  := DateTimeToStr(sellItem.publishDate);
          dataGrid.Cells[6, idx]  := DateTimeToStr(sellItem.sellDate);
          if sellItem.itemType = tSellItemType.New then
            dataGrid.Cells[7, idx]  := 'Nuevo'
          else
            dataGrid.Cells[7, idx]  := 'Usado';
          case sellItem.calification of
            tCalification.None    : dataGrid.Cells[8, idx]  := 'Sin calificar';
            tCalification.Good    : dataGrid.Cells[8, idx]  := 'Buena';
            tCalification.Neutral : dataGrid.Cells[8, idx]  := 'Neutral';
            tCalification.Bad     : dataGrid.Cells[8, idx]  := 'Mala';
          end;          
          dataGrid.Cells[9, idx]  := FloatToStr(sellItem.tax);
          if sellItem.alreadyCollected then
            dataGrid.Cells[10, idx] := 'Cobrado'
          else
            dataGrid.Cells[10, idx] := 'Pendiente de cobro';
        end;
    end;
end;

end.
