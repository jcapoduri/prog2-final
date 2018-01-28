unit selllistform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, sellwidget, metru.core;

type

  { tSellListForm }

  tSellListForm = class(TForm)
    Label1: TLabel;
    purchaseDateLabel: TLabel;
    openButton: TBitBtn;
    ItemNameLabel: TLabel;
    priceLabel: TLabel;
    Label2: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure openButtonClick(Sender: TObject);
  private
    sell : tSell;
  public
    constructor Create(theOwner : tComponent; ownSell : tSell); overload;
  end;

var
  fsellListForm: tSellListForm;

implementation

{$R *.lfm}

{ tSellListForm }

procedure tSellListForm.FormActivate(Sender: TObject);
begin
  self.ItemNameLabel.Caption     := self.sell.itemName;
  self.purchaseDateLabel.Caption := DateToStr(self.sell.sellDate);
  self.priceLabel.Caption        := '$' + FloatToStr(self.sell.price);
end;

procedure tSellListForm.openButtonClick(Sender: TObject);
var
  form : tSellWidget;
begin
  form := tSellWidget.Create(nil, self.sell);
  form.ShowModal;
  form.free;
end;

constructor tSellListForm.Create(theOwner: tComponent; ownSell: tSell); overload;
begin
  self.sell := ownSell;
  Create(theOwner)
end;

end.

