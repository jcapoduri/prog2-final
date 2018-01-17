unit categoryform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, Buttons,
  metru.core in '..\libs\metru.core.pas';

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
begin
  self.category.categoryName := self.nameEdit.Text;
  self.category.description  := self.descriptionEdit.Text;
  self.category.VAT          := self.comisionEdit.Value;
  self.category.parent       := -1;
  if self.category.id = 0 then
    metru.core.createCateogry(metruApp, self.category)
  else
    metru.core.editCateogry(metruApp, self.category);
  close;
end;

procedure TCategoryForm.FormActivate(Sender: TObject);
var
  list : tCategoryList;
  i    : integer;
begin
  list := metru.core.retrieveAllCateogies(metruApp);
  for i := Low(list) to High(list) do
    begin
      self.categoryCombobox.Items.Add(list[i].categoryName);
    end;
end;

procedure TCategoryForm.cancelButtonClick(Sender: TObject);
begin
  close;
end;

constructor TCategoryForm.CreateWithCategory(TheOwner: TComponent;
  cat: tCategory);
begin
  self.Create(TheOwner);
  self.category := cat;
end;

constructor TCategoryForm.Create(TheOwner: TComponent);
begin
  self.Create(TheOwner);
  self.category.id := 0;
end;

end.

