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
  UserForm,
  userblockform,
  userReportPublicationForm,
  metru.core   in 'libs\metru.core.pas';

type

  { TmainWidget }

  TmainWidget = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    deleteUserMenuItem: TMenuItem;
    reportsMenuItem: TMenuItem;
    balanceTrinaryTreeMenuItem: TMenuItem;
    hashDispersionTestMenuItem: TMenuItem;
    lockUserMenuItem: TMenuItem;
    reportByPurchasesMenuItem: TMenuItem;
    reportCategoryMenuItem: TMenuItem;
    reportOfPublicationByMenuItem: TMenuItem;
    reportBySellerMenuItem: TMenuItem;
    reportByBuyerMenuItem: TMenuItem;
    messagesPopulationMenuItem: TMenuItem;
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
    procedure balanceTrinaryTreeMenuItemClick(Sender: TObject);
    procedure deleteUserMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure categoryCRUDMenuItemClick(Sender: TObject);
    procedure hashDispersionTestMenuItemClick(Sender: TObject);
    procedure lockUserMenuItemClick(Sender: TObject);
    procedure messagesPopulationMenuItemClick(Sender: TObject);
    procedure mySellsBaseMenuItemClick(Sender: TObject);
    procedure myPublicationMenuItemClick(Sender: TObject);
    procedure newPublicationMenuItemClick(Sender: TObject);
    procedure publicationAllMenuItemClick(Sender: TObject);
    procedure quitMenuItemClick(Sender: TObject);
    procedure EmbedForm(ArgParent : TWinControl; ArgForm : TCustomForm);
    procedure reportAllPublicationsMenuItemClick(Sender: TObject);
    procedure reportByCategoryMenuItemClick(Sender: TObject);
    procedure reportByMaxPerCateogryMenuItemClick(Sender: TObject);
    procedure userEditMenuItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure cleanPanel;
    procedure setComponentOnPanel(childItem : TForm);
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
  form := tUserReportPublicationForm.Create(self);
  setComponentOnPanel(form);
end;

procedure TmainWidget.reportByCategoryMenuItemClick(Sender: TObject);
begin

end;

procedure TmainWidget.reportByMaxPerCateogryMenuItemClick(Sender: TObject);
begin

end;

procedure TmainWidget.userEditMenuItemClick(Sender: TObject);
var
  userForm : TUserForm;
  usr      : tUser;
begin
  usr      := metru.core.loggedUser(metruApp);
  userForm := TUserForm.Create(self, usr);
  userForm.Show;
end;

procedure TmainWidget.FormCreate(Sender: TObject);
begin
  Application.CreateForm(TLoginForm, login);
  { WARNING : Please delete this 2 lines before prod! }
  login.emailEdit.Text    := 'admistrador@mercatrucho.com';
  login.passwordEdit.Text := 'palo_y_a_la_bolsa';
  login.ShowModal;
end;

procedure TmainWidget.balanceTrinaryTreeMenuItemClick(Sender: TObject);
begin

end;

procedure TmainWidget.deleteUserMenuItemClick(Sender: TObject);
var
  buttonSelected : Integer;
  user           : tUser;
begin
  user           := metru.core.loggedUser(metruApp);
  buttonSelected := MessageDlg('Esta seguro que desea cerrar su cuenta de MercaTrucho',mtCustom, [mbYes,mbCancel], 0);
  if buttonSelected = mrYes then
    if metru.core.banUser(metruApp, user) then
      begin
        MessageDlg('Usuario borrado con exito',mtCustom, [mbOk], 0);
        self.quitMenuItemClick(Sender);
      end
    else
      MessageDlg('No pudo cerrarse la cuenta, por favor cierre todas sus publicaciones e intente de nuevo',mtCustom, [mbOk], 0);
end;

procedure TmainWidget.categoryCRUDMenuItemClick(Sender: TObject);
var
  form : TCategoryBase;
begin
  form := TCategoryBase.Create(self);
  setComponentOnPanel(form);
end;

procedure TmainWidget.hashDispersionTestMenuItemClick(Sender: TObject);
begin

end;

procedure TmainWidget.lockUserMenuItemClick(Sender: TObject);
var
  form : TUserBlockingForm;
begin
  form := TUserBlockingForm.Create(self);
  form.show;
end;

procedure TmainWidget.messagesPopulationMenuItemClick(Sender: TObject);
begin

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
  setComponentOnPanel(form);
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
  setComponentOnPanel(form);
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
  setComponentOnPanel(form);
end;

procedure TmainWidget.quitMenuItemClick(Sender: TObject);
begin
  metru.core.logoff(metruApp);
  cleanPanel;
  login.ShowModal;
end;

procedure TmainWidget.cleanPanel;
var
  i    : integer;
  temp : tControl;
begin
  for i := 0 to self.displayPanel.ControlCount - 1 do
    begin
      temp := self.displayPanel.Controls[i];
      self.displayPanel.RemoveControl(temp);
    end;
end;

procedure TmainWidget.setComponentOnPanel(childItem : TForm);
begin
  cleanPanel;
  childItem.parent  := self.displayPanel;
  childItem.BorderStyle:= bsNone;
  childItem.top     := 0;
  childItem.left    := 0;
  childItem.width   := self.displayPanel.width;
  childItem.height  := self.displayPanel.height;
  childItem.anchors := [akTop, akLeft, akRight, akBottom];
  childItem.visible := true;
end;

end.

