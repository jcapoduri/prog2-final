unit loginForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons;

type

  { TLoginForm }

  TLoginForm = class(TForm)
    cancelButton: TBitBtn;
    emailEdit: TLabeledEdit;
    loginButton: TBitBtn;
    newUserButton: TBitBtn;
    passwordEdit: TLabeledEdit;
  private

  public

  end;
var
  FormLogin: TLoginForm;

implementation

{$R frm/*.lfm}

end.

