unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, publicationlistform, publicationForm,
  loginform    in 'frm\loginform',
  categorybase in 'frm\cateogorybase.pas',
  metru.core   in 'libs\metru.core.pas';

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
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
    procedure myPublicationMenuItemClick(Sender: TObject);
    procedure newPublicationMenuItemClick(Sender: TObject);
    procedure publicationAllMenuItemClick(Sender: TObject);
    procedure quitMenuItemClick(Sender: TObject);
    procedure EmbedForm(ArgParent : TWinControl; ArgForm : TCustomForm);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.EmbedForm(ArgParent: TWinControl; ArgForm: TCustomForm);
begin
  while ArgForm.MDIChildCount > 0 do
    ArgForm.MDIChildren[0].Parent := ArgParent;
  ArgForm.Parent:= ArgParent;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Application.CreateForm(TLoginForm, login);
  { WARNING : Please delete this 2 lines before prod! }
  login.emailEdit.Text    := 'admistrador@mercatrucho.com';
  login.passwordEdit.Text := 'palo_y_a_la_bolsa';
  login.ShowModal;
end;

procedure TForm1.categoryCRUDMenuItemClick(Sender: TObject);
begin
   Application.CreateForm(TCategoryBase, self.catBase);
   self.catBase.Show;
end;

procedure TForm1.myPublicationMenuItemClick(Sender: TObject);
var
  form : tPublicationListForm;
begin
  form := tPublicationListForm.Create(self, metru.core.loggedUser(metruApp), true);
  form.Show;
end;

procedure TForm1.newPublicationMenuItemClick(Sender: TObject);
var
  form : tPublicationForm;
  user : tUser;
begin
  user := metru.core.loggedUser(metruApp);
  form := tPublicationForm.Create(self, user);
  form.showModal;
end;

procedure TForm1.publicationAllMenuItemClick(Sender: TObject);
var
  form : tPublicationListForm;
begin
  form := tPublicationListForm.Create(self, metru.core.loggedUser(metruApp), false);
  form.show;
end;

procedure TForm1.quitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.

