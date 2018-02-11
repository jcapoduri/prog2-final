unit daterangedreportform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  Buttons, StdCtrls, EditBtn;

type

  { TDateRangeReportFormWidget }

  TDateRangeReportFormWidget = class(TForm)
    fromDateEdit: TDateEdit;
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
  private

  public
    procedure doReport; Virtual; Abstract;
  end;

var
  datedReportFormWidget: TDateRangeReportFormWidget;

implementation

{$R *.lfm}

{ TDateRangeReportFormWidget }

procedure TDateRangeReportFormWidget.closeButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TDateRangeReportFormWidget.doReportButtonClick(Sender: TObject);
begin
  doReport;
end;

procedure TDateRangeReportFormWidget.exportToCSVButtonClick(Sender: TObject);
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

end.

