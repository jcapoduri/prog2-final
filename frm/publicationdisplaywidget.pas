unit publicationdisplaywidget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, metru.core, publicationSellWidget, publicationForm;

type

  { tPublicationDisplayWidget }
  tMyEvent = procedure (const Sender : TObject) of object;

  tPublicationDisplayWidget = class(TForm)
    blockButton: TBitBtn;
    editButton: TBitBtn;
    descriptionLabel: TLabel;
    deleteButton: TBitBtn;
    imagePreview: TImage;
    statusLabel: TLabel;
    openButton: TButton;
    priceLabel: TLabel;
    titleLabel: TLabel;
    procedure blockButtonClick(Sender: TObject);
    constructor Create(var _owner : TComponent; pubIdx: tPublishIdx; isOwn : boolean); overload;
    procedure deleteButtonClick(Sender: TObject);
    procedure editButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function GetPublication() : tPublish;
    procedure openButtonClick(Sender: TObject);
  private
    publication    : tPublish;
    publicationIdx : tPublishIdx;
    isOwn          : boolean;
    isAdmin        : boolean;
  public
    onUpdate       : tMyEvent;
  end;

var
  pubForm: tPublicationDisplayWidget;

implementation

{$R *.lfm}

{ tPublicationDisplayWidget }

constructor tPublicationDisplayWidget.Create(var _owner: TComponent;
  pubIdx: tPublishIdx; isOwn : boolean); overload;
begin
  self.publicationIdx := pubIdx;
  metru.core.dereferencePublication(metruApp, pubIdx, self.publication);
  self.isOwn       := isOwn;
  self.isAdmin     := metru.core.isLogedUserAdmin(metruApp);
  Create(_owner);
end;

procedure tPublicationDisplayWidget.deleteButtonClick(Sender: TObject);
var
  buttonSelected : Integer;
begin
  buttonSelected := MessageDlg('Esta seguro que desea borrar su publicacion?',mtCustom, [mbYes,mbCancel], 0);
  if buttonSelected = mrYes then
    begin
      metru.core.deletePublication(metruApp, self.publicationIdx);
      if Assigned(self.onUpdate) then
        onUpdate(self);
    end;
end;

procedure tPublicationDisplayWidget.blockButtonClick(Sender: TObject);
begin

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

procedure tPublicationDisplayWidget.FormCreate(Sender: TObject);
begin
  descriptionLabel.Caption := publication.details;
  titleLabel.Caption       := publication.itemName;
  priceLabel.Caption       := FloatToStr(publication.price);
  case self.publication.status of
    tStatus.Publish: begin
                       self.statusLabel.Caption  := 'Publicado';
                       self.blockButton.Visible  := self.isAdmin;
                       self.editButton.Visible   := self.isOwn;
                       self.deleteButton.Visible := self.isOwn;
                     end;
    tStatus.Paused:  self.statusLabel.Caption := 'Pausado';
    tStatus.Sold:    self.statusLabel.Caption := 'Vendido';
    tStatus.Void:    self.statusLabel.Caption := 'Anulado';
    tStatus.Blocked: self.statusLabel.Caption := 'Bloqueado';
  end;
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

