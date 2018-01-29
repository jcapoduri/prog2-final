unit categorybase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ComCtrls, Buttons, ExtCtrls, metru.core, categoryform
  ,model.categoryitem in '..\models\model.category.pas'
  ;

type

  { TCategoryBase }

  TCategoryBase = class(TForm)
    addButton: TBitBtn;
    deleteButton: TBitBtn;
    editButton: TBitBtn;
    Panel1: TPanel;
    treeView: TTreeView;
    procedure addButtonClick(Sender: TObject);
    procedure deleteButtonClick(Sender: TObject);
    procedure editButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    form : TCategoryForm;
    procedure loadBranch(var parentNode : TTreeNode; category : tCategory);
  public

  end;

var
  CategoryBaseForm: TCategoryBase;

implementation

{$R *.lfm}

{ TCategoryBase }

procedure TCategoryBase.FormActivate(Sender: TObject);
var
  list       : tCategoryList;
  i          : integer;
  parentNode : TTreeNode;
  item       : TCategoryItem;
begin
  list := metru.core.retrieveBaseCateogies(metruApp);
  for i := Low(list) to High(list) do
    begin
      item       := TCategoryItem.Create(list[i]);
      parentNode := self.treeView.Items.AddObject(nil, item.displayName, item);
      self.loadBranch(parentNode, list[i]);
    end;
end;

procedure TCategoryBase.addButtonClick(Sender: TObject);
var
  i : integer;
begin
  Application.createForm(TCategoryForm, self.form);
  i := self.form.ShowModal;
end;

procedure TCategoryBase.deleteButtonClick(Sender: TObject);
var
  i    : integer;
  node : TTreeNode;
  item : tCategoryItem;
begin
  node := self.treeView.Selected;
  item := tCategoryItem(node.Data);
//  Application.createForm(TCategoryForm, self.form, item.category);
  self.form := TCategoryForm.CreateWithCategory(self, item.category);
  i := self.form.ShowModal;
end;

procedure TCategoryBase.editButtonClick(Sender: TObject);
var
  i    : integer;
  node : TTreeNode;
  item : tCategoryItem;
begin
  node := self.treeView.Selected;
  item := tCategoryItem(node.Data);
  self.form := TCategoryForm.CreateWithCategory(self, item.category);
  i := self.form.ShowModal;
end;

procedure TCategoryBase.loadBranch(var parentNode: TTreeNode; category: tCategory);
var
  list    : tCategoryList;
  i       : integer;
  auxNode : TTreeNode;
  item    : TCategoryItem;
begin
  list := metru.core.retrieveChildCateogies(metruApp, category);
  for i := Low(list) to High(list) do
    begin
      item    := TCategoryItem.Create(list[i]);
      auxNode := self.treeView.Items.AddChildObject(parentNode, item.displayName, item);
      self.loadBranch(auxNode, list[i]);
    end;
end;

end.

