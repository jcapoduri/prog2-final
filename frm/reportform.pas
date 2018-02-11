unit reportform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  Buttons;

type

  { TReportFormWidget }

  TReportFormWidget = class(TForm)
    doReportButton: TBitBtn;
    exportToCSVButton: TBitBtn;
    closeButton: TBitBtn;
    dataGrid: TStringGrid;
    procedure closeButtonClick(Sender: TObject);
    procedure doReportButtonClick(Sender: TObject);
    procedure exportToCSVButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure doReport; Virtual; Abstract;
  end;

var
  ReportFormWidget: TReportFormWidget;

implementation

{$R *.lfm}

{ TReportFormWidget }

procedure TReportFormWidget.closeButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TReportFormWidget.doReportButtonClick(Sender: TObject);
begin
  doReport;
end;

procedure TReportFormWidget.exportToCSVButtonClick(Sender: TObject);
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

procedure TReportFormWidget.FormCreate(Sender: TObject);
begin
  doReportButtonClick(Sender);
end;

end.

