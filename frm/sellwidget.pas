unit sellwidget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, metru.core;

type

  { SellWidgetform }

  { tSellWidget }

  tSellWidget = class(TForm)
    paymentButton: TBitBtn;
    reviewButton: TBitBtn;
    reviewComboBox: TComboBox;
    Label1: TLabel;
    sellDateLabel: TLabel;
    isTaxCollectedLabel: TLabel;
    Label6: TLabel;
    publishDateLabel: TLabel;
    titleLabel: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    priceLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure paymentButtonClick(Sender: TObject);
    procedure reviewButtonClick(Sender: TObject);
  private
     sell    : tSell;
     sellIdx : tSellIdx;
  public
     constructor Create (theOwner : TComponent; _sellIdx : tSellIdx); overload;
  end;

var
  fsellWidget: tSellWidget;

implementation

{$R *.lfm}

{ tSellWidget }

procedure tSellWidget.FormCreate(Sender: TObject);
begin
  self.titleLabel.Caption       := self.sell.itemName;
  self.publishDateLabel.Caption := DateToStr(self.sell.publishDate);
  self.sellDateLabel.Caption    := DateToStr(self.sell.sellDate);
  self.priceLabel.Caption       := '$' + FloatToStr(self.sell.price);

  if self.sell.alreadyCollected then
    begin
      self.isTaxCollectedLabel.Caption := 'MercaTrucho ya cobro comision por esta venta';
      self.paymentButton.Enabled := false;
    end;
  if Ord(self.sell.calification) = 0 then
     self.reviewButton.Enabled := true
  else
    self.reviewComboBox.Enabled := false;
    //  self.isTa
   self.reviewComboBox.ItemIndex := Ord(self.sell.calification);
end;

procedure tSellWidget.paymentButtonClick(Sender: TObject);
begin
  metru.core.doPayment(metruApp, self.sellIdx);
  self.sell.alreadyCollected := true;
  self.paymentButton.Enabled := false;
  MessageDlg('Gracias por su pago! No olvide de puntuar su compra', mtCustom , [mbOK], 0);
  Close;
end;

procedure tSellWidget.reviewButtonClick(Sender: TObject);
begin
  if self.reviewComboBox.ItemIndex > 0 then
    begin
      self.sell.calification := metru.core.tCalification(self.reviewComboBox.ItemIndex);
      metru.core.doReviewPurchase(metruApp, self.sellIdx, metru.core.tCalification(self.reviewComboBox.ItemIndex));
      MessageDlg('Gracias por calificar el producto!', mtCustom , [mbOK], 0);
    end;
end;

constructor tSellWidget.Create(theOwner: TComponent; _sellIdx : tSellIdx); overload;
begin
  self.sellIdx := _sellIdx;
  metru.core.dereferenceSell(metruApp, sellIdx, self.sell);
  Create(theOwner);
end;

end.

