unit publicationdisplaywidget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, metru.core, publicationSellWidget, publicationForm;

type

  { tPublicationDisplayWidget }

  tPublicationDisplayWidget = class(TForm)
    editButton: TBitBtn;
    descriptionLabel: TLabel;
    imagePreview: TImage;
    openButton: TButton;
    priceLabel: TLabel;
    titleLabel: TLabel;
    constructor Create(var _owner : TComponent; pubIdx: tPublishIdx; isOwn : boolean); overload;
    procedure editButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    function GetPublication() : tPublish;
    procedure openButtonClick(Sender: TObject);
  private
    publication    : tPublish;
    publicationIdx : tPublishIdx;
    isOwn          : boolean;
  public

  end;

var
  pubForm: tPublicationDisplayWidget;

implementation

{$R *.lfm}

{ tPublicationDisplayWidget }

constructor tPublicationDisplayWidget.Create(var _owner: TComponent;
  pubIdx: tPublishIdx; isOwn : boolean); overload;
begin
  Create(_owner);
  self.publicationIdx := pubIdx;
  metru.core.dereferencePublication(metruApp, pubIdx, self.publication);
  self.isOwn       := isOwn;
  FormActivate(nil);
end;

procedure tPublicationDisplayWidget.editButtonClick(Sender: TObject);
var
  form : tPublicationForm;
  user : tUser;
begin
  user := metru.core.loggedUser(metruApp);
  form := tPublicationForm.Create(self, user, self.publicationIdx);
  form.Show;
end;

procedure tPublicationDisplayWidget.FormActivate(Sender: TObject);
begin
  descriptionLabel.Caption := publication.details;
  titleLabel.Caption       := publication.itemName;
  priceLabel.Caption       := FloatToStr(publication.price);
  editButton.Enabled       := self.isOwn;
  editButton.Visible       := self.isOwn;
end;

function tPublicationDisplayWidget.GetPublication(): tPublish;
begin
  GetPublication := self.publication;
end;

procedure tPublicationDisplayWidget.openButtonClick(Sender: TObject);
var
  form : tPublicationSellWidget;
  user : tUser;
begin
  user := metru.core.loggedUser(metruApp);
  form := tPublicationSellWidget.Create(self, user, self.publicationIdx);
  form.Show;
end;

end.

