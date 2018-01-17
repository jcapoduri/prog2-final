unit loginform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls,
  UserForm,
  metru.core in '../libs/metru.core.pas';

type

  { TLoginForm }

  TLoginForm = class(TForm)
    cancelButton  : TBitBtn;
    emailEdit     : TLabeledEdit;
    errorLabel: TLabel;
    loginButton   : TBitBtn;
    newUserButton : TBitBtn;
    passwordEdit  : TLabeledEdit;
    procedure cancelButtonClick(Sender: TObject);
    procedure loginButtonClick(Sender: TObject);
    procedure newUserButtonClick(Sender: TObject);
  private
    userForm : TUserForm;
  public

  end;
var
  FormLogin: TLoginForm;

implementation

{$R *.lfm}

{ TLoginForm }

procedure TLoginForm.loginButtonClick(Sender: TObject);
var
  ok      : boolean;
  blocked : boolean;
begin
  ok := metru.core.login(metruApp, self.emailEdit.Text, self.passwordEdit.Text, blocked);
  if ok then
     self.Close
  else
    self.errorLabel.Caption := 'Email o Password Invalido';
end;

procedure TLoginForm.newUserButtonClick(Sender: TObject);
begin
  { TODO }
  Application.CreateForm(TUserForm, self.userForm);
  self.userForm.ShowModal;
end;

procedure TLoginForm.cancelButtonClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.

