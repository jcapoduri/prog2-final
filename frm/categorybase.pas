unit categorybase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, metru.core, categoryform;

type

  { TCategoryBase }

  TCategoryBase = class(TForm)
    addButton: TBitBtn;
    treeView: TTreeView;
    procedure addButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    form : TCategoryForm;
    procedure loadBranch(parentNode : TTreeNode; category : tCategory);
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
begin
  list := metru.core.retrieveBaseCateogies(metruApp);
  for i := Low(list) to High(list) do
    begin
      parentNode := self.treeView.Items.Add(nil, list[i].categoryName);
      self.loadBranch(parentNode, list[i]);
    end;
end;

procedure TCategoryBase.addButtonClick(Sender: TObject);
begin
  Application.createForm(TCategoryForm, self.form);
  self.form.ShowModal;
end;

procedure TCategoryBase.loadBranch(parentNode: TTreeNode; category: tCategory);
var
  list : tCategoryList;
  i    : integer;
begin
  list := metru.core.retrieveChildCateogies(metruApp, category);
  for i := Low(list) to High(list) do
    begin
      self.loadBranch(self.treeView.Items.Add(parentNode, list[i].categoryName), list[i]);
    end;
end;

end.

