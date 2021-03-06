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
    idLabel: TLabel;
    publishByLabel: TLabel;
    statusLabel: TLabel;
    openButton: TButton;
    priceLabel: TLabel;
    titleLabel: TLabel;
    procedure blockButtonClick(Sender: TObject);
    constructor Create(var _owner : TComponent; pubIdx: tPublishIdx; usr : tUser); overload;
    constructor Create(var _owner : TComponent; pubIdx: tPublishIdx); overload;
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

constructor tPublicationDisplayWidget.Create(var _owner: TComponent; pubIdx: tPublishIdx;  usr : tUser); overload;
begin
  self.publicationIdx := pubIdx;
  metru.core.dereferencePublication(metruApp, pubIdx, self.publication);
  self.isOwn       := publication.idUser = usr.id;
  self.isAdmin     := metru.core.isLogedUserAdmin(metruApp);
  Create(_owner);
end;

constructor tPublicationDisplayWidget.Create(var _owner: TComponent; pubIdx: tPublishIdx); overload;
begin
  self.publicationIdx := pubIdx;
  metru.core.dereferencePublication(metruApp, pubIdx, self.publication);
  self.isOwn       := true;
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
var
  buttonSelected : Integer;
  operation      : string;
begin
  if publication.status = tStatus.Blocked then
    operation := 'desbloquear'
  else
    operation := 'bloquear';
  buttonSelected := MessageDlg('Esta seguro que desea' + operation + 'esta publicacion?',mtCustom, [mbYes,mbCancel], 0);
  if buttonSelected = mrYes then
    begin
      if publication.status = tStatus.Blocked then
        publication.status := tStatus.Publish
      else
        publication.status := tStatus.Blocked;
      metru.core.editPublication(metruApp, self.publicationIdx, publication);
      if Assigned(self.onUpdate) then
        onUpdate(self);
    end;
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
var
  usr : tUser;
begin
  metru.core.retrieveUser(metruApp, publication.idUser, usr);
  descriptionLabel.Caption := publication.details;
  titleLabel.Caption       := publication.itemName;
  priceLabel.Caption       := FloatToStr(publication.price);
  idLabel.Caption          := 'ID: ' + IntToStr(publication.id);
  publishByLabel.Caption   := 'Publicado por: ' + usr.fullname;
  if publication.image <> EmptyStr then
    imagePreview.Picture.LoadFromFile(metru.core.retrieveImage(metruApp,  publication.image));
  case self.publication.status of
    tStatus.Publish: begin
                       self.statusLabel.Caption  := 'Publicado';
                       self.editButton.Visible   := self.isOwn;
                       self.deleteButton.Visible := self.isOwn;
                       self.blockButton.Visible  := self.isAdmin;
                     end;
    tStatus.Paused:  self.statusLabel.Caption := 'Pausado';
    tStatus.Sold:    self.statusLabel.Caption := 'Vendido';
    tStatus.Void:    self.statusLabel.Caption := 'Anulado';
    tStatus.Blocked: begin
                       self.blockButton.Visible := self.isAdmin;
                       self.statusLabel.Caption := 'Bloqueado';
                       self.blockButton.Caption := 'Desbloquear';
                     end;
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

