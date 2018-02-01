unit publicationsellwidget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, metru.core, messagewidget;

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
    procedure FormCreate(Sender: TObject);
    procedure sendQuestionButtonClick(Sender: TObject);
  private
    user              : tUser;
    publicationIdx    : tPublishIdx;
    publication       : tPublish;
    isSelfPublication : boolean;
  public
    procedure populateMessages();
  end;

var
  wPublicationSellWidget: tPublicationSellWidget;

implementation

{$R *.lfm}

{ tPublicationSellWidget }

constructor tPublicationSellWidget.Create(parentComponent: TComponent;
  var User: tUser; pubIdx : tPublishIdx);
begin
  self.publicationIdx    := pubIdx;
  metru.core.dereferencePublication(metruApp, pubIdx, self.publication);
  self.user              := user;
  self.isSelfPublication := user.id = publication.idUser;
  inherited Create(parentComponent);
  FormCreate(Nil);
end;

procedure tPublicationSellWidget.buyButtonClick(Sender: TObject);
begin
  metru.core.doPurchase(metruApp, self.publicationIdx, self.user);
  MessageDlg('Gracias por su compra! por favor acceda a ella para poder abonarla y calificar al vendedor', mtCustom , [mbOK], 0);
  self.buyButton.Enabled:=false;
end;

procedure tPublicationSellWidget.FormCreate(Sender: TObject);
begin
  self.titleLabel.Caption := self.publication.itemName;
  self.descriptionLabel.Caption := self.publication.details;
  self.priceLabel.Caption:= FloatToStr(self.publication.price);
  if (publication.idUser = user.id) then {if publication is my own, disable purchase}
     self.buyButton.Enabled := false;
  populateMessages;
end;

procedure tPublicationSellWidget.populateMessages();
var
  messageList : tMessageList;
  mesIdx      : tMessageIdx;
  i, count    : integer;
  messageItem : tMessageWidget;
begin
  { retrieve messages }
  self.messagesContainer.DestroyComponents;
  messageList := metru.core.retrieveMessages(metruApp, publicationIdx);
  count       := length(messageList) - 1;
  for i := 0 to count do
      begin
        mesIdx := messageList[i];
        messageItem := tMessageWidget.Create(self, self.user, mesIdx);
        messageItem.parent  := self.messagesContainer;
        messageItem.top     := i * 106;
        messageItem.left    := 0;
        messageItem.width   := self.messagesContainer.Width - 20; // avoid double-scroll
        messageItem.Height  := 100;
        messageItem.Visible := true;
      end;
end;

procedure tPublicationSellWidget.sendQuestionButtonClick(Sender: TObject);
begin
  metru.core.postMessage(metruApp, self.publicationIdx, self.user, string(self.questionEdit.Text));
  self.questionEdit.Text := EmptyStr;
  populateMessages;
end;

end.

