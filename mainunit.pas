unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls,
  loginform    in 'frm\loginform',
  categorybase in 'frm\cateogorybase.pas',
  metru.core   in 'libs\metru.core.pas';

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    categoryMenuItem: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    Panel1: TPanel;
    login : TLoginForm;
    catBase : TCategoryBase;
    procedure FormCreate(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure categoryMenuItemClick(Sender: TObject);
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

procedure TForm1.MenuItem1Click(Sender: TObject);
begin

end;

procedure TForm1.categoryMenuItemClick(Sender: TObject);
begin
   Application.CreateForm(TCategoryBase, self.catBase);
   self.catBase.Show;
end;

end.

