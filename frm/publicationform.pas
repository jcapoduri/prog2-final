unit publicationForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, EditBtn, ExtDlgs, Buttons
  ,metru.core
  ,model.categoryitem
  ;

type

  { tPublicationForm }

  tPublicationForm = class(TForm)
    addPhotoButton: TBitBtn;
    pauseButton: TBitBtn;
    voidButton: TBitBtn;
    errorLabel: TLabel;
    Image1: TImage;
    statusNameLabel: TLabel;
    statusLabel: TLabel;
    saveButton: TBitBtn;
    closeButton: TBitBtn;
    categoryComboBox: TComboBox;
    descriptionEdit: TMemo;
    expireDateEdit: TDateEdit;
    previewWidget: TImage;
    isNewCheckBox: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    pictureDialog: TOpenPictureDialog;
    priceEdit: TFloatSpinEdit;
    titleEdit: TLabeledEdit;
    procedure CloseForm(Sender: TObject);
    constructor Create(parentComponent : TComponent; var User : tUser); overload;
    constructor Create(parentComponent : TComponent; var User : tUser; pubIdx : tPublishIdx); overload;
    procedure addPictureButtonClick(Sender: TObject);
    procedure pauseButtonClick(Sender: TObject);
    procedure saveButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure voidButtonClick(Sender: TObject);
  private
    publication    : tPublish;
    publicationIdx : tPublishIdx;
    user           : tUser;
    procedure assertValues();
  public

  end;

var
  pubForm: tPublicationForm;

implementation

{$R *.lfm}
constructor tPublicationForm.Create(parentComponent : TComponent; var User : tUser); Overload;
begin
  Create(parentComponent);
  self.user := user;
end;

constructor tPublicationForm.Create(parentComponent: TComponent;
  var User: tUser; pubIdx: tPublishIdx);
begin
  self.publicationIdx := pubIdx;
  metru.core.dereferencePublication(metruApp, pubIdx, self.publication);
  Create(parentComponent, user);
  self.statusLabel.visible     := true;
  self.statusNameLabel.visible := true;
  self.voidButton.visible      := true;
  self.pauseButton.visible     := true;

  case self.publication.status of
    tStatus.Publish: begin
                       self.statusLabel.Caption := 'Publicado';
                       self.pauseButton.Enabled := true;
                       self.voidButton.Enabled  := true;
                     end;
    tStatus.Paused:  begin
                       self.statusLabel.Caption := 'Pausado';
                       self.pauseButton.Caption := 'Reanudar';
                       self.pauseButton.Enabled := true;
                       self.voidButton.Enabled  := true;
                     end;
    tStatus.Sold:    self.statusLabel.Caption := 'Vendido';
    tStatus.Void:    self.statusLabel.Caption := 'Anulado';
    tStatus.Blocked: self.statusLabel.Caption := 'Bloqueado';
  end;
end;

procedure tPublicationForm.CloseForm(Sender: TObject);
begin
  Close;
end;

procedure tPublicationForm.FormCreate(Sender: TObject);
var
  list     : tCategoryList;
  i        : integer;
  item     : TCategoryItem;
  selected : integer;
begin
  list     := metru.core.retrieveAllLeafCateogies(metruApp);
  for i := Low(list) to High(list) do
    begin
      item := TCategoryItem.Create(list[i]);
      self.categoryComboBox.Items.AddObject(item.displayName, item);
      if (item.category.id = self.publication.idCategory) then
        selected := i;
    end;
  self.categoryComboBox.ItemIndex := selected;
  if (self.publication.id > 0) then
    begin
      self.categoryComboBox.Enabled := false;
      titleEdit.Text                := publication.itemName;
      descriptionEdit.Text          := publication.details;
      self.priceEdit.Value          := publication.price;
      self.expireDateEdit.Date      := publication.etimestamp;
      self.isNewCheckBox.Checked    := publication.itemType = tItemType.New;
    end;
end;

procedure tPublicationForm.voidButtonClick(Sender: TObject);
begin
  self.publication.status := tStatus.Void;
  metru.core.editPublication(metruApp, publicationIdx, publication);
  Close;
end;

procedure tPublicationForm.assertValues();
begin
  Close;
end;

procedure tPublicationForm.addPictureButtonClick(Sender: TObject);
begin
   if pictureDialog.Execute then
     previewWidget.Picture.LoadFromFile(pictureDialog.FileName);
end;

procedure tPublicationForm.pauseButtonClick(Sender: TObject);
begin
  if self.publication.status = tStatus.Paused then
    self.publication.status := tStatus.Publish
  else
    self.publication.status := tStatus.Paused;
  metru.core.editPublication(metruApp, publicationIdx, publication);
  Close;
end;

procedure tPublicationForm.saveButtonClick(Sender: TObject);
var
  item        : TCategoryItem;
begin
  { assert if valid }
  errorLabel.Caption := '';
  assertValues;
  if errorLabel.Caption <> EmptyStr then exit;

  item                   := TCategoryItem(self.categoryComboBox.Items.Objects[self.categoryComboBox.ItemIndex]);
  publication.id         := 0;
  publication.idCategory := item.category.id;
  publication.idUser     := self.user.id;
  publication.itemName   := LowerCase(titleEdit.Text);
  publication.details    := LowerCase(descriptionEdit.Text);
  publication.price      := self.priceEdit.Value;
  publication.ctimestamp := Now;
  publication.etimestamp := self.expireDateEdit.Date;
  if (self.isNewCheckBox.Checked) then
    publication.itemType := tItemType.New
  else
    publication.itemType := tItemType.Used;
  publication.status     := tStatus.Publish;
  metru.core.createPublication(metruApp, publication);
  Close;
end;

end.

