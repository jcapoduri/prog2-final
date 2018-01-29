unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, publicationlistform, publicationForm,
  PublicationDisplayWidget,
  Contnrs,
  loginform    in 'frm\loginform',
  categorybase in 'frm\cateogorybase.pas',
  selllistform,
  widgetlistform,
  userReportPublicationForm,
  metru.core   in 'libs\metru.core.pas';

type

  { TmainWidget }

  TmainWidget = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    reportByMaxPerCateogryMenuItem: TMenuItem;
    reportByCategoryMenuItem: TMenuItem;
    reportAllPublicationsMenuItem: TMenuItem;
    mySellsBaseMenuItem: TMenuItem;
    userBaseMenuItem: TMenuItem;
    newPublicationMenuItem: TMenuItem;
    publicationBaseMenuItem: TMenuItem;
    categoryMenuItem: TMenuItem;
    categoryCRUDMenuItem: TMenuItem;
    sellsBaseMenuItem: TMenuItem;
    advanceBaseMenuItem: TMenuItem;
    userEditMenuItem: TMenuItem;
    quitMenuItem: TMenuItem;
    publicationAllMenuItem: TMenuItem;
    myPublicationMenuItem: TMenuItem;
    displayPanel: TPanel;
    login : TLoginForm;
    catBase : TCategoryBase;
    procedure FormCreate(Sender: TObject);
    procedure categoryCRUDMenuItemClick(Sender: TObject);
    procedure mySellsBaseMenuItemClick(Sender: TObject);
    procedure myPublicationMenuItemClick(Sender: TObject);
    procedure newPublicationMenuItemClick(Sender: TObject);
    procedure publicationAllMenuItemClick(Sender: TObject);
    procedure quitMenuItemClick(Sender: TObject);
    procedure EmbedForm(ArgParent : TWinControl; ArgForm : TCustomForm);
    procedure reportAllPublicationsMenuItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  mainWidget: TmainWidget;

implementation

{$R *.lfm}

{ TmainWidget }

procedure TmainWidget.EmbedForm(ArgParent: TWinControl; ArgForm: TCustomForm);
begin
  while ArgForm.MDIChildCount > 0 do
    ArgForm.MDIChildren[0].Parent := ArgParent;
  ArgForm.Parent:= ArgParent;
end;

procedure TmainWidget.reportAllPublicationsMenuItemClick(Sender: TObject);
var
  form : tUserReportPublicationForm;
begin
  form := tUserReportPublicationForm.Create(nil);
  form.Show;
end;

procedure TmainWidget.FormCreate(Sender: TObject);
begin
  Application.CreateForm(TLoginForm, login);
  { WARNING : Please delete this 2 lines before prod! }
  login.emailEdit.Text    := 'admistrador@mercatrucho.com';
  login.passwordEdit.Text := 'palo_y_a_la_bolsa';
  login.ShowModal;
end;

procedure TmainWidget.categoryCRUDMenuItemClick(Sender: TObject);
begin
   Application.CreateForm(TCategoryBase, self.catBase);
   self.catBase.Show;
end;

procedure TmainWidget.mySellsBaseMenuItemClick(Sender: TObject);
var
  list         : tComponentList;
  item         : tSellListForm;
  purchaseList : tSellIdxList;
  user         : tUser;
  i            : integer;
  form         : tWidgetListForm;
begin
  user         := metru.core.loggedUser(metruApp);
  purchaseList := metru.core.retrieveAllMyPurchase(metruApp, user);
  list         := tComponentList.Create;
  for i := low(purchaseList) to high(purchaseList) do
    begin
      item :=  tSellListForm.Create(self, purchaseList[i]);
      list.Add(item);
    end;
  form := tWidgetListForm.Create(self, list);
  form.Show;
end;

procedure TmainWidget.myPublicationMenuItemClick(Sender: TObject);
var
  result        : tPublishIdxList;
  componentList : tComponentList;
  _parent       : tComponent;
  item          : tPublicationDisplayWidget;
  user          : tUser;
  i             : integer;
  form          : tWidgetListForm;
begin
  user          := metru.core.loggedUser(metruApp);
  result        := metru.core.retrievePublicationByUser(metruApp, user);
  componentList := tComponentList.Create;
  for i := low(result) to high(result) do
    begin
      _parent := self as TComponent;
      item    :=  tPublicationDisplayWidget.Create(_parent, result[i], true);
      componentList.Add(item);
    end;
  
  form := tWidgetListForm.Create(self, componentList);
  form.Show;
end;

procedure TmainWidget.newPublicationMenuItemClick(Sender: TObject);
var
  form : tPublicationForm;
  user : tUser;
begin
  user := metru.core.loggedUser(metruApp);
  form := tPublicationForm.Create(self, user);
  form.showModal;
end;

procedure TmainWidget.publicationAllMenuItemClick(Sender: TObject);
var
  form : tPublicationListForm;
begin
  form := tPublicationListForm.Create(self, metru.core.loggedUser(metruApp), false);
  form.show;
end;

procedure TmainWidget.quitMenuItemClick(Sender: TObject);
begin
  metru.core.logoff(metruApp);
  login.ShowModal;
end;

end.

