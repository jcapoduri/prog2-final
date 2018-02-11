unit expiredpublicationsbycategoryreportform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  Buttons, StdCtrls, EditBtn
  ,metru.core         in '..\libs\metru.core.pas'
  ,model.categoryitem in '..\models\model.categoryitem.pas'
  ;

type

  { TExpiredPublicationsByCategoryReportRorm }

  TExpiredPublicationsByCategoryReportRorm = class(TForm)
    categoryComboBox: TComboBox;
    fromDateEdit: TDateEdit;
    Label3: TLabel;
    toDateEdit: TDateEdit;
    doReportButton: TBitBtn;
    exportToCSVButton: TBitBtn;
    closeButton: TBitBtn;
    dataGrid: TStringGrid;
    Label1: TLabel;
    Label2: TLabel;
    procedure closeButtonClick(Sender: TObject);
    procedure doReportButtonClick(Sender: TObject);
    procedure exportToCSVButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure doReport;
  end;

var
  expiredPublicationReportFormWidget: TExpiredPublicationsByCategoryReportRorm;

implementation

{$R *.lfm}

{ TExpiredPublicationsByCategoryReportRorm }

procedure TExpiredPublicationsByCategoryReportRorm.doReport();
var
  fromDate, toDate : tDateTime;
  found            : boolean;
  publishList      : tPublishIdxList;             
  pubItem          : tPublish;
  pubIdx           : tPublishIdx;
  usr              : tUser;
  idx, i, count    : integer;
  item             : TCategoryItem;
begin
  fromDate := self.fromDateEdit.Date;
  toDate   := self.toDateEdit.Date;

  dataGrid.ColCount := 8;

  with dataGrid do
    begin
      Cells[1,0]  := 'id publication';
      Cells[2,0]  := 'Nombre de la publicacion';
      Cells[3,0]  := 'Precio';
      Cells[4,0]  := 'id Usuario';
      Cells[5,0]  := 'Nombre Usuario';
      Cells[6,0]  := 'Fecha de publicacion';
      Cells[7,0]  := 'Fecha limite';
    end;

  item        := TCategoryItem(self.categoryCombobox.Items.Objects[self.categoryCombobox.ItemIndex]);
  publishList := metru.core.retrievePublicationByCategory(metruApp, item.category);
  count       := length(publishList);
  for i := 0 to count - 1 do
    begin
      metru.core.dereferencePublication(metruApp, publishList[i], pubItem);
      if (
           (fromDate <= pubItem.etimestamp) and (pubItem.etimestamp <= toDate) { between range}
           and
           (pubItem.etimestamp >= Now) and (pubItem.status <> tStatus.Sold)  { expired without being sold}
         )
      then
        begin
          metru.core.retrieveUser(metruApp, pubItem.idUser, usr);
          idx                    := i + 1;
          dataGrid.Cells[0, idx]  := IntToStr(idx);
          dataGrid.Cells[1, idx]  := IntToStr(pubItem.id);
          dataGrid.Cells[2, idx]  := pubItem.itemName;
          dataGrid.Cells[3, idx]  := FloatToStr(pubItem.price);
          dataGrid.Cells[4, idx]  := IntToStr(pubItem.idUser);
          dataGrid.Cells[5, idx]  := usr.fullname;
          dataGrid.Cells[6, idx]  := DateTimeToStr(pubItem.ctimestamp);
          dataGrid.Cells[7, idx]  := DateTimeToStr(pubItem.etimestamp);
        end;

    end;
end;

procedure TExpiredPublicationsByCategoryReportRorm.closeButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TExpiredPublicationsByCategoryReportRorm.doReportButtonClick(Sender: TObject);
begin
  doReport;
end;

procedure TExpiredPublicationsByCategoryReportRorm.exportToCSVButtonClick(Sender: TObject);
var
  saveDialog : TSaveDialog;
  i          : integer;
  CSV        : TStrings;
  filename   : String;
begin
  try
    saveDialog := TSaveDialog.Create(Self);
    saveDialog.filter := 'CSV- Valores separados por coma (*.csv)|*.CSV';
    if saveDialog.Execute = True then
      begin
        filename := saveDialog.FileName;
        if Copy(filename,Pos('.',filename),Length(filename)-Pos('.',filename)+1) <> '.csv' then 
          filename := filename + '.csv';
        
        Screen.Cursor := crHourGlass;
        CSV := TStringList.Create;
        try
          for i := 0 To dataGrid.RowCount - 1 Do 
            CSV.Add(dataGrid.Rows[I].CommaText);
          
          CSV.SaveToFile(filename);
        finally
          CSV.Free;
        end;
      end;
  finally
    saveDialog.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TExpiredPublicationsByCategoryReportRorm.FormCreate(Sender: TObject);
var
  list     : tCategoryList;
  i        : integer;
  item     : TCategoryItem;
begin
  list     := metru.core.retrieveAllCateogies(metruApp);
  for i := Low(list) to High(list) do
    begin
      item := TCategoryItem.Create(list[i]);
      self.categoryCombobox.Items.AddObject(item.displayName, item);
    end;
  self.categoryCombobox.ItemIndex := 0;
end;

end.

