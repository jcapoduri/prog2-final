unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls,
  loginform    in 'frm\loginform', publicationForm,
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
    Panel1: TPanel;
    login : TLoginForm;
    catBase : TCategoryBase;
    procedure FormCreate(Sender: TObject);
    procedure categoryCRUDMenuItemClick(Sender: TObject);
    procedure newPublicationMenuItemClick(Sender: TObject);
    procedure quitMenuItemClick(Sender: TObject);
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

procedure TForm1.newPublicationMenuItemClick(Sender: TObject);
var
  form : tPublicationForm;
  user : tUser;
begin
  user := metru.core.loggedUser(metruApp);
  form := tPublicationForm.Create(self, user);
  form.showModal;
end;

procedure TForm1.quitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.

