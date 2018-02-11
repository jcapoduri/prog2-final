unit publicationByUserReportPublicationForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  Buttons, reportform, metru.core;

type
  tPublicationByUserReportPublicationForm = class(TReportFormWidget)
  	procedure doReport; override;
  end;

implementation

{ tPublicationByUserReportPublicationForm }

procedure tPublicationByUserReportPublicationForm.doReport();
var
  userInput        : string;
  ok, found        : boolean;
  userId, i, count : integer;
  idx              : integer;
  usr              : tUser;
  publishList      : tPublishIdxList;
  pubItem          : tPublish;
  pubIdx           : tPublishIdx;
  sellItem         : tSell;
  sellIdxItem      : tSellIdx;
begin
  ok            := InputQuery('Reporte', 'Por favor, ingrese el id del usuario a listar', userInput);
  userId        := StrToIntDef(userInput, 0);
  if ok and (userId > 0) then
    ok := metru.core.retrieveUser(metruApp, userId, usr);
  dataGrid.ColCount := 12;

  with dataGrid do
    begin
      Cells[1,0]  := 'id publication';
      Cells[2,0]  := 'Nombre de la publicacion';
      Cells[3,0]  := 'Precio';
      Cells[4,0]  := 'id User comprador';
      Cells[5,0]  := 'Nombre User comprador';
      Cells[6,0]  := 'Fecha de publicacion';
      Cells[7,0]  := 'Fecha de venta';
      Cells[8,0]  := 'Tipo';
      Cells[9,0]  := 'Calificacion';
      Cells[10,0] := 'Impuesto interno';
      Cells[11,0] := 'Cobrado';
    end;

  if ok then
    begin
      publishList := metru.core.retrievePublicationByUser(metruApp, usr);
      count       := length(publishList);
      dataGrid.RowCount := count + 1;
      for i := 0 to count - 1 do
        begin
          metru.core.dereferencePublication(metruApp, publishList[i], pubItem);
          metru.core.retrieveUser(metruApp, pubItem.idUser, usr);
          idx                     := i + 1;
          dataGrid.Cells[0, idx]  := IntToStr(idx);
          dataGrid.Cells[1, idx]  := IntToStr(pubItem.id);
          dataGrid.Cells[2, idx]  := pubItem.itemName;
          dataGrid.Cells[3, idx]  := FloatToStr(pubItem.price);
          dataGrid.Cells[4, idx]  := IntToStr(pubItem.idUser);
          dataGrid.Cells[5, idx]  := usr.fullname;
          dataGrid.Cells[6, idx]  := DateTimeToStr(pubItem.ctimestamp);
          if metru.core.retrievePurchaseIfAvailable(metruApp, publishList[i], sellIdxItem) then
            begin
              metru.core.dereferenceSell(metruApp, sellIdxItem, sellItem);
              if sellItem.itemType = tSellItemType.New then
                dataGrid.Cells[8, idx]  := 'Nuevo'
              else
                dataGrid.Cells[8, idx]  := 'Usado';
              case sellItem.calification of
                tCalification.None    : dataGrid.Cells[9, idx]  := 'Sin calificar';
                tCalification.Good    : dataGrid.Cells[9, idx]  := 'Buena';
                tCalification.Neutral : dataGrid.Cells[9, idx]  := 'Neutral';
                tCalification.Bad     : dataGrid.Cells[9, idx]  := 'Mala';
              end;          
              dataGrid.Cells[10, idx]  := FloatToStr(sellItem.tax);
              if sellItem.alreadyCollected then
                dataGrid.Cells[11, idx] := 'Cobrado'
              else
                dataGrid.Cells[11, idx] := 'Pendiente de cobro';
            end;
        end;
    end;
end;

end.
