unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls,
  loginform  in 'frm\loginform',
  metru.core in 'libs\metru.core.pas';

type

  { TForm1 }

  TForm1 = class(TForm)
    Panel1: TPanel;
    login : TLoginForm;
    procedure FormCreate(Sender: TObject);
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
  login.ShowModal;
end;

end.

