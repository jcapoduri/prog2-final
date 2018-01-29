unit publicationsellwidget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, metru.core;

type

  { tPublicationSellWidget }

  tPublicationSellWidget = class(TForm)
    sendQuestionButton: TBitBtn;
    buyButton: TBitBtn;
    imageDisplay: TImage;
    questionEdit: TMemo;
    titleLabel: TLabel;
    descriptionLabel: TLabel;
    Label3: TLabel;
    priceLabel: TLabel;
    Label5: TLabel;
    messagesContainer: TScrollBox;
    procedure buyButtonClick(Sender: TObject);
    constructor Create(parentComponent : TComponent; var User : tUser; pubIdx : tPublishIdx); overload;
    procedure FormActivate(Sender: TObject);
  private
    user              : tUser;
    publicationIdx    : tPublishIdx;
    publication       : tPublish;
    isSelfPublication : boolean;
  public

  end;

var
  wPublicationSellWidget: tPublicationSellWidget;

implementation

{$R *.lfm}

{ tPublicationSellWidget }

constructor tPublicationSellWidget.Create(parentComponent: TComponent;
  var User: tUser; pubIdx : tPublishIdx);
begin
  inherited Create(parentComponent);
  self.publicationIdx    := pubIdx;
  metru.core.dereferencePublication(metruApp, pubIdx, self.publication);
  self.user              := user;
  self.isSelfPublication := user.id = publication.idUser;
end;

procedure tPublicationSellWidget.buyButtonClick(Sender: TObject);
begin
  metru.core.doPurchase(metruApp, self.publicationIdx, self.user);
  MessageDlg('Gracias por su compra! por favor acceda a ella para poder abonarla y calificar al vendedor', mtCustom , [mbOK], 0);
  self.buyButton.Enabled:=false;
end;

procedure tPublicationSellWidget.FormActivate(Sender: TObject);
begin
  self.titleLabel.Caption := self.publication.itemName;
  self.descriptionLabel.Caption := self.publication.details;
  self.priceLabel.Caption:= FloatToStr(self.publication.price);
  if (publication.idUser = user.id) then {if publication is my own, disable purchase}
     self.buyButton.Enabled := false;
end;

end.

