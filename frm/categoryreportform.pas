unit categoryReportForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  Buttons, reportform, metru.core;

type
  tCategoryReportForm = class(TReportFormWidget)
  	procedure doReport; override;
  end;

implementation

{ tCategoryReportForm }

procedure tCategoryReportForm.doReport();
var
  list          : tCategoryList;
  pubList       : tPublishIdxList;
  category      : tCategory;
  i, count, idx : longint;
begin
  dataGrid.ColCount := 5;
  list              := metru.core.retrieveAllCateogies(metruApp);
  count             := length(list);
  dataGrid.RowCount := count + 1;

  with dataGrid do
    begin
      Cells[1,0] := 'id Cateogria';
      Cells[2,0] := 'Nombre';
      Cells[3,0] := 'Descripcion';
      Cells[4,0] := 'Cantidad de publicaciones';
    end;

  for i := 0 to count - 1 do
    begin
      metru.core.dereferenceCategory(metruApp, list[i], category);
      idx                    := i + 1;
      pubList                := metru.core.retrievePublicationByCategory(metruApp, category);
      dataGrid.Cells[0, idx] := IntToStr(i + 1);
      dataGrid.Cells[1, idx] := IntToStr(category.id);
      dataGrid.Cells[2, idx] := category.categoryName;
      dataGrid.Cells[3, idx] := category.description;
      dataGrid.Cells[4, idx] := IntToStr(length(pubList));
    end;
end;

end.
