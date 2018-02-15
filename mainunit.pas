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
  messageCheckForm,
  HashDispersionTestForm,
  userReportPublicationForm,
  categoryReportForm,
  purchasesByUserReportPublicationForm,
  publicationByUserReportPublicationForm,
  detailedCategoryReportPublicationForm,
  detailedSellsReportPublicationForm,
  topNUserReportPublicationForm,
  buyerUserMessagesReportPublicationForm,
  sellerUserMessagesReportPublicationForm,
  expiredpublicationsbycategoryreportform,
  sellsReportPublicationForm,
  blockedUserReportForm,
  messagegeneratorform,
  metru.core   in 'libs\metru.core.pas';

type

  { TmainWidget }

  TmainWidget = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    deleteUserMenuItem: TMenuItem;
    blockedUserReportMenuItem: TMenuItem;
    publicationByUserReportMenuItem: TMenuItem;
    purchaseDetailedReportMenuItem: TMenuItem;
    topSellersReportMenuItem: TMenuItem;
    reportsMenuItem: TMenuItem;
    balanceTrinaryTreeMenuItem: TMenuItem;
    hashDispersionTestMenuItem: TMenuItem;
    lockUserMenuItem: TMenuItem;
    reportByPurchasesMenuItem: TMenuItem;
    reportCategoryMenuItem: TMenuItem;
    reportOfPurchaseByMenuItem: TMenuItem;
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
    procedure blockedUserReportMenuItemClick(Sender: TObject);
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
    procedure publicationByUserReportMenuItemClick(Sender: TObject);
    procedure purchaseDetailedReportMenuItemClick(Sender: TObject);
    procedure quitMenuItemClick(Sender: TObject);
    procedure EmbedForm(ArgParent : TWinControl; ArgForm : TCustomForm);
    procedure reportAllPublicationsMenuItemClick(Sender: TObject);
    procedure reportByBuyerMenuItemClick(Sender: TObject);
    procedure reportByCategoryMenuItemClick(Sender: TObject);
    procedure reportByMaxPerCateogryMenuItemClick(Sender: TObject);
    procedure reportByPurchasesMenuItemClick(Sender: TObject);
    procedure reportBySellerMenuItemClick(Sender: TObject);
    procedure reportCategoryMenuItemClick(Sender: TObject);
    procedure reportOfPurchaseByMenuItemClick(Sender: TObject);
    procedure topSellersReportMenuItemClick(Sender: TObject);
    procedure userEditMenuItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure cleanPanel;
    procedure setComponentOnPanel(childItem : TForm);
    procedure setUIasAdmin;
    procedure setUIasUser;
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

procedure TmainWidget.reportByBuyerMenuItemClick(Sender: TObject);
var
  form : tSellerUserMessagesReportPublicationForm;
begin
  form := tSellerUserMessagesReportPublicationForm.Create(self);
  setComponentOnPanel(form);
end;

procedure TmainWidget.reportByCategoryMenuItemClick(Sender: TObject);
var
  form : TCategoryReportForm;
begin
  form := TCategoryReportForm.Create(self);
  setComponentOnPanel(form);
end;

procedure TmainWidget.reportByMaxPerCateogryMenuItemClick(Sender: TObject);
var
  form : tDetailedCategoryPublicationForm;
begin
  form := tDetailedCategoryPublicationForm.Create(self);
  setComponentOnPanel(form);
end;

procedure TmainWidget.reportByPurchasesMenuItemClick(Sender: TObject);
var
  form : tSellsReportPublicationForm;
begin
  form := tSellsReportPublicationForm.Create(self);
  setComponentOnPanel(form);
end;

procedure TmainWidget.reportBySellerMenuItemClick(Sender: TObject);
var
  form : tBuyerUserMessagesReportPublicationForm;
begin
  form := tBuyerUserMessagesReportPublicationForm.Create(self);
  setComponentOnPanel(form);
end;

procedure TmainWidget.reportCategoryMenuItemClick(Sender: TObject);
var
  form : TExpiredPublicationsByCategoryReportRorm;
begin
  form := TExpiredPublicationsByCategoryReportRorm.Create(self);
  setComponentOnPanel(form);
end;

procedure TmainWidget.reportOfPurchaseByMenuItemClick(Sender: TObject);
var
  form : tPurchasesByUserReportPublicationForm;
begin
  form := tPurchasesByUserReportPublicationForm.Create(self);
  setComponentOnPanel(form);
end;

procedure TmainWidget.topSellersReportMenuItemClick(Sender: TObject);
var
  form : tTopNUserReportPublicationForm;
begin
  form := tTopNUserReportPublicationForm.Create(self);
  setComponentOnPanel(form);
end;

procedure TmainWidget.userEditMenuItemClick(Sender: TObject);
var
  userForm : TUserForm;
  usr      : tUser;
begin
  usr      := metru.core.loggedUser(metruApp);
  userForm := TUserForm.Create(self, usr);
  userForm.ShowModal;
  metru.core.reloadUser(metruApp);
end;

procedure TmainWidget.FormCreate(Sender: TObject);
begin
  Application.CreateForm(TLoginForm, login);
  { WARNING : Please delete this 2 lines before prod!
  login.emailEdit.Text    := 'admistrador@mercatrucho.com';
  login.passwordEdit.Text := 'palo_y_a_la_bolsa';}
  login.ShowModal;
  if metru.core.isLogedUserAdmin(metruApp) then
    setUIasAdmin
  else
    setUIasUser;
end;

procedure TmainWidget.balanceTrinaryTreeMenuItemClick(Sender: TObject);
var
  form : TmessageCheckFrm;
begin
  form := TmessageCheckFrm.Create(self);
  setComponentOnPanel(form);
end;

procedure TmainWidget.blockedUserReportMenuItemClick(Sender: TObject);
var
  form : tBlockedUserReportForm;
begin
  form := tBlockedUserReportForm.Create(self);
  setComponentOnPanel(form);
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
var
  form : THashDispersionTestForm;
begin
  form := THashDispersionTestForm.Create(self);
  setComponentOnPanel(form);
end;

procedure TmainWidget.lockUserMenuItemClick(Sender: TObject);
var
  form : TUserBlockingForm;
begin
  form := TUserBlockingForm.Create(self);
  form.show;
end;

procedure TmainWidget.messagesPopulationMenuItemClick(Sender: TObject);
var
  form : TmessageGeneratorForm;
begin
  form := TmessageGeneratorForm.Create(self);
  form.ShowModal;
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
      item    :=  tPublicationDisplayWidget.Create(_parent, result[i]);
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

procedure TmainWidget.publicationByUserReportMenuItemClick(Sender: TObject);
var
  form : tPublicationByUserReportPublicationForm;
begin
  form := tPublicationByUserReportPublicationForm.Create(self);
  setComponentOnPanel(form);
end;

procedure TmainWidget.purchaseDetailedReportMenuItemClick(Sender: TObject);
var
  form : tDetailedSellsReportPublicationForm;
begin
  form := tDetailedSellsReportPublicationForm.Create(self);
  setComponentOnPanel(form);
end;

procedure TmainWidget.quitMenuItemClick(Sender: TObject);
begin
  metru.core.logoff(metruApp);
  cleanPanel;
  login.ShowModal;
  if metru.core.isLogedUserAdmin(metruApp) then
    setUIasAdmin
  else
    setUIasUser;
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

procedure TmainWidget.setUIasAdmin;
begin
  self.advanceBaseMenuItem.Visible             := true;
  self.reportByPurchasesMenuItem.Visible       := true;
  self.blockedUserReportMenuItem.Visible       := true;
  self.reportCategoryMenuItem.Visible          := true;
  self.reportOfPurchaseByMenuItem.Visible      := true;
  self.reportBySellerMenuItem.Visible          := true;
  self.reportByBuyerMenuItem.Visible           := true;
  self.reportByMaxPerCateogryMenuItem.Visible  := true;
  self.reportByCategoryMenuItem.Visible        := true;
  self.reportByMaxPerCateogryMenuItem.Visible  := true;
  self.purchaseDetailedReportMenuItem.Visible  := true;
  self.publicationByUserReportMenuItem.Visible := true;
  self.topSellersReportMenuItem.Visible        := true;
  self.categoryMenuItem.Visible                := true;
  self.deleteUserMenuItem.Visible              := false;
end;

procedure TmainWidget.setUIasUser;
begin
  self.advanceBaseMenuItem.Visible             := false;
  self.reportByPurchasesMenuItem.Visible       := false;
  self.blockedUserReportMenuItem.Visible       := false;
  self.reportCategoryMenuItem.Visible          := false;
  self.reportOfPurchaseByMenuItem.Visible      := false;
  self.reportBySellerMenuItem.Visible          := false;
  self.reportByBuyerMenuItem.Visible           := false;
  self.reportByMaxPerCateogryMenuItem.Visible  := false;
  self.reportByCategoryMenuItem.Visible        := false;
  self.reportByMaxPerCateogryMenuItem.Visible  := false;
  self.purchaseDetailedReportMenuItem.Visible  := false;
  self.publicationByUserReportMenuItem.Visible := false;
  self.topSellersReportMenuItem.Visible        := false;
  self.categoryMenuItem.Visible                := false;
  self.deleteUserMenuItem.Visible              := true;
end;

end.

