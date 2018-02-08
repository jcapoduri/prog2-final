unit blockedUserReportForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  Buttons, reportform, metru.core;

type
  tBlockedUserReportForm = class(TReportFormWidget)
  	procedure doReport; override;
  end;

implementation

{ tBlockedUserReportForm }

procedure tBlockedUserReportForm.doReport();
var
  it    : tUserIterator;
  user  : tUser;
  count : integer;
  found : boolean;
begin
  dataGrid.ColCount := 7;
  count             := 1;
  dataGrid.RowCount := count;

  with dataGrid do
    begin
      Cells[1,0] := 'id Usuario';
      Cells[2,0] := 'Email';
      Cells[3,0] := 'Nombre Completo';
      Cells[4,0] := 'Direccion';
      Cells[5,0] := 'Provincia';
      Cells[6,0] := 'Creado el dia';
    end;

  found := metru.core.retrieveFirstUser(metruApp, it);

  while found do
    begin
      metru.core.retrieveUser(metruApp, it, user);
      if user.blocked then
        begin
          dataGrid.RowCount := count + 1;
          dataGrid.Cells[0, count] := IntToStr(count);
          dataGrid.Cells[1, count] := IntToStr(user.id);
          dataGrid.Cells[2, count] := user.email;
          dataGrid.Cells[3, count] := user.fullname;
          dataGrid.Cells[4, count] := user.address;
          dataGrid.Cells[5, count] := IntToStr(user.providence);
          dataGrid.Cells[6, count] := DateTimeToStr(user.ctimestamp);
          count := count + 1;
        end;
      found := metru.core.retrieveNextUser(metruApp, it);
    end;

end;

end.
