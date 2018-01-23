unit publicationlistform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, metru.core, model.categoryitem, publicationdisplaywidget
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
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure searchButtonClick(Sender: TObject);
    constructor Create(theOwner : TComponent; user : tUser; isOwn : boolean); overload;
  private
    user  : tUser;
    isOwn : boolean;
  public

  end;

var
  pubListForm: tPublicationListForm;

implementation

{$R *.lfm}

{ tPublicationListForm }

procedure tPublicationListForm.FormActivate(Sender: TObject);
var
  list : tCategoryList;
  i    : integer;
  item : TCategoryItem;
begin
  list     := metru.core.retrieveAllLeafCateogies(metruApp);
  for i := Low(list) to High(list) do
    begin
      item := TCategoryItem.Create(list[i]);
      self.categoryCombobox.Items.AddObject(item.displayName, item);
    end;
end;

procedure tPublicationListForm.FormCreate(Sender: TObject);
begin

end;

procedure tPublicationListForm.searchButtonClick(Sender: TObject);
var
  list   : tPublishList;
  item   : tCategoryItem;
  widget : tPublicationDisplayWidget;
  i      : integer;
  par    : TComponent;
begin
  item := TCategoryItem(self.categoryCombobox.Items.Objects[self.categoryCombobox.ItemIndex]);
  list := metru.core.retrievePublicationByCategory(metruApp, item.category);
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
end;

constructor tPublicationListForm.Create(theOwner: TComponent; user: tUser;
  isOwn: boolean); overload;
begin
  Create(theOwner);
  self.user  := user;
  self.isOwn := isOwn;
end;

end.

