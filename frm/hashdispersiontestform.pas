unit hashdispersiontestform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Spin, MD5, metru.core in '..\libs\metru.core.pas';

type

  { THashDispersionTestForm }

  THashDispersionTestForm = class(TForm)
    closeButton: TButton;
    generateButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    userHashRadioButton: TRadioButton;
    sellsHashRadioButton: TRadioButton;
    SpinEdit1: TSpinEdit;
    resultStringGrid: TStringGrid;
    procedure closeButtonClick(Sender: TObject);
    procedure generateButtonClick(Sender: TObject);
  private

  public
    procedure testUserHash;
    procedure testSellsHash;
  end;

var
  fHashDispersionTestForm: THashDispersionTestForm;

implementation

{$R *.lfm}

{ THashDispersionTestForm }

procedure THashDispersionTestForm.closeButtonClick(Sender: TObject);
begin
  Close;
end;

procedure THashDispersionTestForm.generateButtonClick(Sender: TObject);
begin
  self.resultStringGrid.Clean;

  if self.userHashRadioButton.Checked then
    self.testUserHash
  else
    self.testSellsHash;
end;

procedure THashDispersionTestForm.testUserHash;
var
  count, i, max, val, bucket : LongInt;
  strToHash                  : string;
begin
  count := metru.core.getMaxUserBuckets(metruApp);
  self.resultStringGrid.RowCount := count;
  max   := self.SpinEdit1.Value;

  for i := 0 to count - 1 do self.resultStringGrid.Cells[0, i] := IntToStr(i);

  for i := 1 to max do
    begin
      strToHash                         := MD5Print(MD5String(IntToStr(i)));
      bucket                            := metru.core.hashUser(metruApp, strToHash);
      val                               := StrToIntDef(self.resultStringGrid.Cells[1, bucket + 1], 0) + 1;
      resultStringGrid.Cells[1, bucket] := IntToStr(val);
    end;
end;

procedure THashDispersionTestForm.testSellsHash;
var
  count, i, max, val, bucket : LongInt;
begin
  count := metru.core.getMaxSellBuckets(metruApp);
  self.resultStringGrid.RowCount := count;
  max   := self.SpinEdit1.Value;

  for i := 0 to count - 1 do self.resultStringGrid.Cells[0, i] := IntToStr(i);

  for i := 1 to max do
    begin
      bucket                            := metru.core.hashSell(metruApp, i);
      val                               := StrToIntDef(self.resultStringGrid.Cells[1, bucket + 1], 0) + 1;
      resultStringGrid.Cells[1, bucket] := IntToStr(val);
    end;
end;

end.

