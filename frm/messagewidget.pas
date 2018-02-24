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
    Label1: TLabel;
    sentByLabel: TLabel;
    questionLabel: TLabel;
    responseLabel: TLabel;
    responseEdit: TMemo;
    constructor Create(parentComponent : TComponent; canResponseQuestion : boolean; msgIdx : tMessageIdx); overload;
    procedure doneButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    canResponse : boolean;
    messageIdx  : tMessageIdx;
    message     : tMessage;
  public

  end;

var
  Form3: tMessageWidget;

implementation

{$R *.lfm}

{ tMessageWidget }

constructor tMessageWidget.Create(parentComponent : TComponent; canResponseQuestion : boolean; msgIdx : tMessageIdx);
begin
  self.messageIdx  := msgIdx;
  self.canResponse := canResponseQuestion;
  metru.core.dereferenceMessage(metruApp, self.messageIdx, self.message);
  Create(parentComponent);
end;

procedure tMessageWidget.doneButtonClick(Sender: TObject);
begin
  self.message.answer := string(self.responseEdit.Caption);
  metru.core.postResponse(metruApp, messageIdx, string(self.responseEdit.Caption));
  FormCreate(nil);
end;

procedure tMessageWidget.FormCreate(Sender: TObject);
var
  usr : tUser;
begin
  if metru.core.retrieveUser(metruApp, message.idUser, usr) then
    sentByLabel.Caption := usr.fullname;
  self.questionLabel.Caption := IntToStr(message.number) + ' - ' + message.question;
  if (EmptyStr <> message.answer) or (not canResponse) then
     begin
       responseLabel.Caption:= message.answer;
       responseEdit.Visible:=false;
       doneButton.Visible:=false;
     end;
end;

end.

