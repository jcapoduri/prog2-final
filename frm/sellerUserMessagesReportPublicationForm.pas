unit sellerUserMessagesReportPublicationForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  Buttons, reportform, metru.core;

type
  tSellerUserMessagesReportPublicationForm = class(TReportFormWidget)
  	procedure doReport; override;
  end;

implementation

{ tSellerUserMessagesReportPublicationForm }

procedure tSellerUserMessagesReportPublicationForm.doReport();
var
  userInput           : string;
  ok, found           : boolean;
  userId, i, count    : integer;
  msgCount, j, count2 : integer;
  usr, usr2           : tUser;
  publishList         : tPublishIdxList;
  pubItem             : tPublish;
  pubIdx              : tPublishIdx;
  messageList         : tMessageList;
  message             : tMessage;
begin
  ok     := InputQuery('Reporte', 'Por favor, ingrese el id del usuario a listar', userInput);
  userId := StrToIntDef(userInput, 0);
  if ok and (userId > 0) then
    ok := metru.core.retrieveUser(metruApp, userId, usr);
  
  dataGrid.ColCount := 7;
  dataGrid.RowCount := 1;

  with dataGrid do
    begin
      Cells[1,0] := 'Id publicacion';
      Cells[2,0] := 'Fecha';
      Cells[3,0] := 'Usuario';
      Cells[4,0] := 'Mensaje enviado';
      Cells[5,0] := 'Respuesta';
      Cells[6,0] := 'Total';
    end;

  if ok then
    begin
      publishList := metru.core.retrievePublicationByUser(metruApp, usr);
      count       := length(publishList);
      for i := 0 to count - 1 do
        begin
          metru.core.dereferencePublication(metruApp, publishList[i], pubItem);
          msgCount                                 := 0;
          dataGrid.RowCount                        := dataGrid.RowCount + 1;
          dataGrid.Cells[1, dataGrid.RowCount - 1] := IntToStr(pubItem.id);

          messageList := metru.core.retrieveMessages(metruApp, publishList[i]);
          count2      := length(messageList);
          for j := 0 to count2 - 1 do
            begin
              metru.core.dereferenceMessage(metruApp, messageList[j], message);
              if (metru.core.retrieveUser(metruApp, message.idUser, usr2)) then
                dataGrid.Cells[2, dataGrid.RowCount - 1] := usr2.fullname
              else
                dataGrid.Cells[2, dataGrid.RowCount - 1] := '[borrado]';
              dataGrid.RowCount                          := dataGrid.RowCount + 1;
              dataGrid.Cells[3, dataGrid.RowCount - 1]   := DateTimeToStr(message.timestamp);
              dataGrid.Cells[4, dataGrid.RowCount - 1]   := message.answer;
              dataGrid.Cells[5, dataGrid.RowCount - 1]   := message.question;
              msgCount                                   := msgCount + 1;
            end;
          dataGrid.RowCount                        := dataGrid.RowCount + 1;
          dataGrid.Cells[5, dataGrid.RowCount - 1] := IntToStr(msgCount);
        end;
    end;
end;

end.
