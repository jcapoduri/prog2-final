unit buyerUserMessagesReportPublicationForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  Buttons, reportform, metru.core;

type
  tBuyerUserMessagesReportPublicationForm = class(TReportFormWidget)
  	procedure doReport; override;
  end;

implementation

{ tBuyerUserMessagesReportPublicationForm }

procedure tBuyerUserMessagesReportPublicationForm.doReport();
var
  userInput           : string;
  ok, found           : boolean;
  userId, i, count    : integer;
  msgCount, j, count2 : integer;
  usr, usr2           : tUser;
  sellsList           : tSellIdxList;
  sellItem            : tSell;
  pubIdx              : tPublishIdx;
  messageList         : tMessageList;
  message             : tMessage;
begin
  ok            := InputQuery('Reporte', 'Por favor, ingrese el id del usuario a listar', userInput);
  userId        := StrToIntDef(userInput, 0);
  if ok and (userId > 0) then
    ok := metru.core.retrieveUser(metruApp, userId, usr);
  
  dataGrid.ColCount  := 8;
  dataGrid.RowCount  := 1;

  with dataGrid do
    begin
      Cells[1,0] := 'Id publicacion';
      Cells[2,0] := 'Id usuario vend.';
      Cells[3,0] := 'Usuario vendedor';
      Cells[4,0] := 'Fecha';
      Cells[5,0] := 'Mensaje enviado';
      Cells[6,0] := 'Respuesta';
      Cells[7,0] := 'Total';
    end;

  if ok then
    begin
      sellsList := metru.core.retrieveAllMyPurchase(metruApp, usr);
      count     := length(sellsList);
      for i := 0 to count - 1 do
        begin
          metru.core.dereferenceSell(metruApp, sellsList[i], sellItem);
          msgCount := 0;
          metru.core.retrieveUser(metruApp, sellItem.idBuyer, usr2);
          //metru.core.retrievePublicationById(metruApp, sellItem.idBuyer, pubIdx);
          dataGrid.RowCount                    := dataGrid.RowCount + 1;
          dataGrid.Cells[1, dataGrid.RowCount - 1] := IntToStr(sellItem.idITem);
          dataGrid.Cells[2, dataGrid.RowCount - 1] := IntToStr(usr2.id);
          dataGrid.Cells[3, dataGrid.RowCount - 1] := usr2.fullname;
          //

          messageList := metru.core.retrieveMessages(metruApp, pubIdx);
          count2      := length(messageList);
          for j := 0 to count2 - 1 do
            begin
              metru.core.dereferenceMessage(metruApp, messageList[j], message);
              dataGrid.RowCount                    := dataGrid.RowCount + 1;
              dataGrid.Cells[4, dataGrid.RowCount - 1] := DateTimeToStr(message.timestamp);
              dataGrid.Cells[5, dataGrid.RowCount - 1] := message.answer;
              dataGrid.Cells[6, dataGrid.RowCount - 1] := message.question;
              msgCount := msgCount + 1;
            end;
          dataGrid.RowCount := dataGrid.RowCount + 1;
          dataGrid.Cells[7, dataGrid.RowCount - 1] := IntToStr(msgCount);
        end;
    end;
end;

end.
