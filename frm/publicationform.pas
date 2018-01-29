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
    errorLabel: TLabel;
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
    procedure saveButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
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
end;

procedure tPublicationForm.CloseForm(Sender: TObject);
begin
  Close;
end;

procedure tPublicationForm.FormActivate(Sender: TObject);
var
  list     : tCategoryList;
  i        : integer;
  item     : TCategoryItem;
begin
  list     := metru.core.retrieveAllLeafCateogies(metruApp);
  for i := Low(list) to High(list) do
    begin
      item := TCategoryItem.Create(list[i]);
      self.categoryComboBox.Items.AddObject(item.displayName, item);
      {if (item.category.id = self.category.parent) then
        selected := j;}
    end;
  {self.categoryComboBox.ItemIndex := selected;
  if (self.category.id > 0) then
    self.categoryComboBox.Enabled := false;}
end;

procedure tPublicationForm.assertValues();
begin
end;

procedure tPublicationForm.addPictureButtonClick(Sender: TObject);
begin
   if pictureDialog.Execute then
     previewWidget.Picture.LoadFromFile(pictureDialog.FileName);
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

end;

end.

