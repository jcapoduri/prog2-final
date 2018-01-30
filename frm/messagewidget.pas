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
    doneButton: TBitBtn;
    questionLabel: TLabel;
    responseLabel: TLabel;
    responseEdit: TMemo;
    constructor Create(parentComponent : TComponent; User : tUser; msgIdx : tMessageIdx); overload;
    procedure doneButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    user       : tUser;
    canEdit    : boolean;
    messageIdx : tMessageIdx;
    message    : tMessage;
  public

  end;

var
  Form3: tMessageWidget;

implementation

{$R *.lfm}

{ tMessageWidget }

constructor tMessageWidget.Create(parentComponent : TComponent; User : tUser; msgIdx : tMessageIdx);
begin
  self.user       := user;
  self.messageIdx := msgIdx;
  self.canEdit    := true;
  metru.core.dereferenceMessage(metruApp, self.messageIdx, self.message);
  Create(parentComponent);
end;

procedure tMessageWidget.doneButtonClick(Sender: TObject);
begin
  metru.core.postResponse(metruApp, messageIdx, string(self.responseEdit.Caption));
end;

procedure tMessageWidget.FormCreate(Sender: TObject);
begin
  self.questionLabel.Caption := message.question;
  if (EmptyStr <> message.answer) then
     begin
       responseLabel.Caption:= message.answer;
       responseEdit.Visible:=false;
       doneButton.Visible:=false;
     end;
end;

end.

