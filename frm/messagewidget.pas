unit messagewidget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, metru.core;

type

  { TForm3 }

  { tMessageWidget }

  tMessageWidget = class(TForm)
    createResponseButton: TBitBtn;
    doneButton: TBitBtn;
    questionLabel: TLabel;
    responseLabel: TLabel;
    responseEdit: TMemo;
    constructor Create(parentComponent : TComponent; var User : tUser; canEdit : boolean); overload;
  private
    user    : tUser;
    canEdit : boolean;
  public

  end;

var
  Form3: tMessageWidget;

implementation

{$R *.lfm}

{ tMessageWidget }

constructor tMessageWidget.Create(parentComponent: TComponent; var User: tUser;
   canEdit : boolean);
begin
  Create(parentComponent);
  self.user    := user;
  sefl.canEdit := canEdit;
end;

end.

