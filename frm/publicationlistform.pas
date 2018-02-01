unit publicationlistform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, metru.core, model.categoryitem, publicationdisplaywidget, publicationsellwidget
  ;

type

  { tPublicationListForm }

  tPublicationListForm = class(TForm)
    ComboBox1: TComboBox;
    orderByComboBox: TComboBox;
    providenceComboBox: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    searchButton: TButton;
    categoryComboBox: TComboBox;
    searchEdit: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    publicationContainer: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure searchButtonClick(Sender: TObject);
    constructor Create(theOwner : TComponent; user : tUser; isOwn : boolean); overload;
  private
    user  : tUser;
    isOwn : boolean;
    procedure loadPublicationList(list : tPublishIdxList);
    procedure openPublication(Sender : TObject);
  public

  end;

var
  pubListForm: tPublicationListForm;

implementation

{$R *.lfm}

{ tPublicationListForm }

procedure tPublicationListForm.FormCreate(Sender: TObject);
var
  list   : tCategoryList;
  i      : integer;
  item   : TCategoryItem;
  result : tPublishIdxList;
begin
  if self.isOwn then
    begin
      self.GroupBox1.Visible := false;
      result := metru.core.retrievePublicationByUser(metruApp, self.user);
      loadPublicationList(result)
    end
  else
    begin
      list     := metru.core.retrieveAllLeafCateogies(metruApp);
      for i := Low(list) to High(list) do
        begin
          item := TCategoryItem.Create(list[i]);
          self.categoryCombobox.Items.AddObject(item.displayName, item);
        end;
     end;
end;

procedure tPublicationListForm.searchButtonClick(Sender: TObject);
var
  list   : tPublishIdxList;
  item   : tCategoryItem;
begin
  item := TCategoryItem(self.categoryCombobox.Items.Objects[self.categoryCombobox.ItemIndex]);
  list := metru.core.retrievePublicationByCategory(metruApp, item.category);
  loadPublicationList(list);
end;

procedure tPublicationListForm.loadPublicationList(list : tPublishIdxList);
var
  widget : tPublicationDisplayWidget;
  i      : integer;
  par    : TComponent;
begin
  for i := low(list) to high(list) do
    begin
      par := self.publicationContainer as TComponent;
      widget         := tPublicationDisplayWidget.create(par, list[i], true);
      widget.parent  := self.publicationContainer;
      widget.top     := i * 206;
      widget.left    := 0;
      widget.width   := self.publicationContainer.Width;
      widget.Height  := 100;
      widget.Visible := true;
    end;
  self.publicationContainer.Refresh;
end;

procedure tPublicationListForm.openPublication(Sender : TObject);
var
  form        : tPublicationSellWidget;
  publication : tPublish;
  widget      : tPublicationDisplayWidget;
  button      : tButton;
begin
  {button      := Sender as TButton;
  widget      := button.Parent as tPublicationDisplayWidget;
  publication := widget.GetPublication();
  form        := tPublicationSellWidget.Create(self, user, publication);}
  form.Show;
end;

constructor tPublicationListForm.Create(theOwner: TComponent; user: tUser;
  isOwn: boolean); overload;
begin
  Create(theOwner);
  self.user  := user;
  self.isOwn := isOwn;
end;

end.

