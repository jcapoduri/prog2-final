unit categoryform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, Buttons
  ,metru.core         in '..\libs\metru.core.pas'
  ,model.categoryitem in '..\models\model.categoryitem.pas'
  ;
type

  { TCategoryForm }
  TCategoryForm = class(TForm)
    descriptionEdit: TLabeledEdit;
    saveButton: TBitBtn;
    cancelButton: TBitBtn;
    categoryCombobox: TComboBox;
    comisionEdit: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    nameEdit: TLabeledEdit;
    procedure cancelButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure saveButtonClick(Sender: TObject);
  private
    category : tCategory;
  public
    constructor CreateWithCategory(TheOwner: TComponent; cat: tCategory);
    constructor Create(TheOwner: TComponent);

  end;

var
  CategoryForm1: TCategoryForm;

implementation

{$R *.lfm}

{ TCategoryForm }

procedure TCategoryForm.saveButtonClick(Sender: TObject);
var
  item : TCategoryItem;
begin
  self.category.categoryName := self.nameEdit.Text;
  self.category.description  := self.descriptionEdit.Text;
  self.category.VAT          := self.comisionEdit.Value;
  if self.categoryCombobox.ItemIndex <> -1 then
    begin
      item                   := TCategoryItem(self.categoryCombobox.Items.Objects[self.categoryCombobox.ItemIndex]);
      self.category.parent   := item.category.id;
    end
  else
    self.category.parent     := -1;

  if self.category.id = 0 then
    metru.core.createCateogry(metruApp, self.category)
  else
    metru.core.editCateogry(metruApp, self.category);
  close;
end;

procedure TCategoryForm.FormActivate(Sender: TObject);
var
  list     : tCategoryList;
  i, j     : integer;
  item     : TCategoryItem;
  selected : longint;
begin
  list     := metru.core.retrieveAllCateogies(metruApp);
  selected := -1;
  for i := Low(list) to High(list) do
    begin
      item := TCategoryItem.Create(list[i]);
      j    := self.categoryCombobox.Items.AddObject(item.displayName, item);
      if (item.category.id = self.category.parent) then
        selected := j;
    end;
  self.categoryCombobox.ItemIndex := selected;
  if (self.category.id > 0) then
    self.categoryCombobox.Enabled := false;
end;

procedure TCategoryForm.cancelButtonClick(Sender: TObject);
begin
  close;
end;

constructor TCategoryForm.CreateWithCategory(TheOwner: TComponent;
  cat: tCategory);
begin
  self.category := cat;
end;

constructor TCategoryForm.Create(TheOwner: TComponent);
begin
  self.Create(TheOwner);
  self.category.id := 0;
end;

end.

