unit userform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, ExtDlgs, regexpr,
  metru.core in '..\libs\metru.core.pas';

type

  { TUserForm }

  TUserForm = class(TForm)
    addPictureButton: TBitBtn;
    errorLabel: TLabel;
    previewWidget: TImage;
    Label2: TLabel;
    pictureDialog: TOpenPictureDialog;
    State: TComboBox;
    Label1: TLabel;
    nameEdit: TLabeledEdit;
    addressEdit: TLabeledEdit;
    saveButton: TBitBtn;
    cancelButton: TBitBtn;
    emailEdit: TLabeledEdit;
    pass1Edit: TLabeledEdit;
    pass2Edit: TLabeledEdit;
    procedure addPictureButtonClick(Sender: TObject);
    procedure cancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure saveButtonClick(Sender: TObject);
    constructor Create(_owner : tComponent; usr : tUser); overload;
  private
    user : tUser;
  public

  end;

var
  Form2: TUserForm;

implementation

{$R *.lfm}

{ TUserForm }


procedure TUserForm.FormCreate(Sender: TObject);
begin
  if user.id > 0 then
    begin
      emailEdit.Text         := user.email;      
      addressEdit.Text       := user.address;
      nameEdit.Text          := user.fullname;
      State.ItemIndex        := user.providence;
      pictureDialog.FileName := user.photoUrl;
    end;
end;

procedure TUserForm.cancelButtonClick(Sender: TObject);
begin
  self.Close;
end;

procedure TUserForm.addPictureButtonClick(Sender: TObject);
begin
   if pictureDialog.Execute then
     previewWidget.Picture.LoadFromFile(pictureDialog.FileName);
end;

procedure TUserForm.saveButtonClick(Sender: TObject);
var
  regularExpression : TRegExpr;
  createdOrUpdated : boolean;
begin
  { assert if valid }
  errorLabel.Caption := '';
  regularExpression  := TRegExpr.Create;
  regularExpression.Expression  := '^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$';

  if EmptyStr = emailEdit.Text                  then errorLabel.Caption := errorLabel.Caption + 'Tiene que ingresar un email.';
  if not regularExpression.Exec(emailEdit.Text) then errorLabel.Caption := errorLabel.Caption + 'Tiene que ingresar un email valido.';
  if (user.id = 0) or (pass1Edit.Text <> EmptyStr) then
    begin
      if Length(pass1Edit.Text) < 8                 then errorLabel.Caption := errorLabel.Caption + 'Tiene que ingresar una contraseña de al menos 8 caracteres.';
      if pass1Edit.Text <> pass2Edit.Text           then errorLabel.Caption := errorLabel.Caption + 'Las contraseña deben coincidir.';
    end;

  if errorLabel.Caption <> EmptyStr then exit;

  user.email      := LowerCase(emailEdit.Text);
  if (user.id = 0) or (pass1Edit.Text <> EmptyStr) then
    user.password   := LowerCase(pass1Edit.Text);
  user.address    := LowerCase(addressEdit.Text);
  user.fullname   := LowerCase(nameEdit.Text);
  user.providence := State.Items.IndexOf(State.Text);
  user.photoUrl   := LowerCase(pictureDialog.FileName);
  if user.id = 0 then
    createdOrUpdated := metru.core.createUser(metruApp, user)
  else
    createdOrUpdated := metru.core.updateUser(metruApp, user);

  if createdOrUpdated then
    close
  else
    errorLabel.Caption := 'El email ya esta registrado';
end;

constructor TUserForm.Create(_owner: tComponent; usr: tUser);
begin
  self.user := usr;
  inherited Create(_owner);
end;

end.

