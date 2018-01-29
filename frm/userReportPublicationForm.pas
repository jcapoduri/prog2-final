unit userReportPublicationForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  Buttons, reportform, metru.core;

type
  tUserReportPublicationForm = class(TReportFormWidget)
  	procedure doReport; override;
  end;

implementation

{ tUserReportPublicationForm }

procedure tUserReportPublicationForm.doReport();
var
  publicationList : tPublishIdxList;
  publication     : tPublish;
  user            : tUser;
  purchase        : tSell;
  purchaseIdx     : tSellIdx;
  i, count, idx   : longint;
  found           : boolean;
begin
  dataGrid.ColCount := 10;
  user              := metru.core.loggedUser(metruApp);
  publicationList   := metru.core.retrievePublicationByUser(metruApp, user);
  count             := high(publicationList);
  dataGrid.RowCount := 1 + count + 1;

  with dataGrid do
    begin
      Cells[1,0] := 'id Publicacion';
      Cells[2,0] := 'Fecha publ.';
      Cells[3,0] := 'Estado';
      Cells[4,0] := 'id comprados';
      Cells[5,0] := 'nombre comprador';
      Cells[6,0] := 'email comprador';
      Cells[7,0] := 'fecha de compra';
      Cells[8,0] := 'calificacion';
      Cells[9,0] := 'pagado';
    end;

  for i := low(publicationList) to high(publicationList) do
    begin
      metru.core.dereferencePublication(metruApp, publicationList[i], publication);
      idx                    := i + 1;
      dataGrid.Cells[0, idx] := IntToStr(i + 1);
      dataGrid.Cells[1, idx] := IntToStr(publication.id);
      dataGrid.Cells[2, idx] := DateTimeToStr(publication.ctimestamp);
      dataGrid.Cells[3, idx] := IntToStr(Ord(publication.status));
      if metru.core.retrievePurchaseIfAvailable(metruApp, publicationList[i], purchaseIdx) then
        begin
          metru.core.dereferenceSell(metruApp, purchaseIdx, purchase);
          metru.core.retrieveUser(metruApp, purchase.idBuyer, user);
          dataGrid.Cells[4, idx] := IntToStr(user.id);
          dataGrid.Cells[5, idx] := user.fullname;
          dataGrid.Cells[6, idx] := user.email;
          dataGrid.Cells[7, idx] := DateTimeToStr(purchase.sellDate);
          dataGrid.Cells[8, idx] := IntToStr(Ord(purchase.calification));
          if purchase.alreadyCollected then
            dataGrid.Cells[9, idx] := 'Si'
          else
            dataGrid.Cells[9, idx] := 'No';
        end;
    end;

end;

end.
