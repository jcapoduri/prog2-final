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
    constructor Create(parentComponent : TComponent; var User : tUser; var publication : tPublish); overload;
  private
    user              : tUser;
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
  var User: tUser; var publication : tPublish);
begin
  inherited Create(parentComponent);
  self.publication       := publication;
  self.user              := user;
  self.isSelfPublication := user.id = publication.idUser;
end;

end.

