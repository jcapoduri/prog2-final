unit publicationlistform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, StrUtils,
  Menus, metru.core, model.categoryitem, publicationdisplaywidget, publicationsellwidget
  ;

type

  { tPublicationListForm }

  tPublicationListForm = class(TForm)
    publicationComboBox: TComboBox;
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
    function  weightPublications(var pub : tPublish) : integer;
    function  weightPublications(var pub1, pub2 : tPublish) : integer;
    procedure swapListItem(var pubList : tPublishIdxList; var realPubList : tPublishList; i : integer);
    procedure orderList(var pubList : tPublishIdxList);
  public

  end;

var
  pubListForm: tPublicationListForm;

implementation

{$R *.lfm}

{ tPublicationListForm }

function  tPublicationListForm.weightPublications(var pub : tPublish) : integer;
var
  weight, i   : integer;
  usr        : tUser;
  searchSeed : string;
begin
  weight := 0;
  metru.core.retrieveUser(metruApp, pub.idUser, usr);
  searchSeed := self.searchEdit.Text;

  i := AnsiPos(searchSeed, LowerCase(pub.itemName));
  if AnsiPos(searchSeed, LowerCase(pub.itemName)) <> 0 then
    weight := weight + 1000;

  if AnsiPos(searchSeed, LowerCase(pub.details)) <> 0 then
    weight := weight + 100;

  if (publicationComboBox.ItemIndex = 1) and (pub.itemType = tItemType.New) then
    weight := weight + 10;

  if (publicationComboBox.ItemIndex = 2) and (pub.itemType = tItemType.Used) then
    weight := weight + 10;

  if (providenceComboBox.ItemIndex = usr.providence) then
    weight := weight + 1;

  weightPublications := weight;
end;

procedure tPublicationListForm.swapListItem(var pubList : tPublishIdxList; var realPubList : tPublishList; i : integer);
var
  publication      : tPublish;
  publicationIdx   : tPublishIdx;
begin
  publication        := realPubList[i];
  realPubList[i]     := realPubList[i + 1];
  realPubList[i + 1] := publication;
  publicationIdx     := pubList[i];
  pubList[i]         := pubList[i + 1];
  pubList[i + 1]     := publicationIdx;
end;

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
  self.categoryComboBox.ItemIndex    := 0;
  self.providenceComboBox.ItemIndex  := 0;
  self.publicationComboBox.ItemIndex := 0;
  self.orderByComboBox.ItemIndex     := 0;
end;

procedure tPublicationListForm.searchButtonClick(Sender: TObject);
var
  list   : tPublishIdxList;
  item   : tCategoryItem;
begin
  item := TCategoryItem(self.categoryCombobox.Items.Objects[self.categoryCombobox.ItemIndex]);
  list := metru.core.retrievePublicationByCategory(metruApp, item.category);
  orderList(list);
  loadPublicationList(list);
end;

procedure tPublicationListForm.loadPublicationList(list : tPublishIdxList);
var
  widget : tPublicationDisplayWidget;
  i      : integer;
  par    : TComponent;
  temp   : TControl;
  pubIdx : tPublishIdx;
begin
  self.publicationContainer.DestroyComponents;

  for i := low(list) to high(list) do
    begin
      par            := self.publicationContainer as TComponent;
      pubIdx         := list[i];
      widget         := tPublicationDisplayWidget.Create(par, pubIdx, self.user);
      widget.parent  := self.publicationContainer;
      widget.top     := i * 206;
      widget.left    := 0;
      widget.width   := self.publicationContainer.Width;
      widget.Height  := 120;
      widget.Visible := true;
    end;
  self.publicationContainer.Refresh;
end;

constructor tPublicationListForm.Create(theOwner: TComponent; user: tUser;
  isOwn: boolean); overload;
begin
  Create(theOwner);
  self.user  := user;
  self.isOwn := isOwn;
end;

function  tPublicationListForm.weightPublications(var pub1, pub2 : tPublish) : integer;
var
  weight1, weight2, weight : integer;
begin
  weight1 := weightPublications(pub1);
  weight2 := weightPublications(pub2);
  weight  := 0;
  if (weight1 > weight2) then weight := -1;
  if (weight2 > weight1) then weight := 1;
  weightPublications := weight;
end;

procedure tPublicationListForm.orderList(var pubList : tPublishIdxList);
var
  realPubList      : tPublishList;
  i, count, weight : integer;
  sorted           : boolean;
begin
  count := length(pubList);
  setLength(realPubList, count);
  for i := 0 to count - 1 do
    metru.core.dereferencePublication(metruApp, pubList[i], realPubList[i]);

  repeat
    begin
      sorted := true;
      for i := 0 to count - 2 do
        begin
          weight := self.weightPublications(realPubList[i], realPubList[i + 1]);
          case weight of
              0: begin
                   if (
                        (self.orderByComboBox.ItemIndex = 0) and (realPubList[i].price > realPubList[i + 1].price)
                        or
                        (self.orderByComboBox.ItemIndex = 1) and (realPubList[i].price < realPubList[i + 1].price)
                      ) then
                     begin
                       swapListItem(pubList, realPubList, i);
                       sorted := false;
                     end;
                 end;
              1: begin
                   swapListItem(pubList, realPubList, i);
                   sorted := false;
                 end;
            end;
        end;
    end
  until sorted;
end;

end.

