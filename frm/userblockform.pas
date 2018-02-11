unit userblockform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, metru.core;

type

  { TUserBlockingForm }

  TUserBlockingForm = class(TForm)
    searchButton: TButton;
    closeButton: TBitBtn;
    nameLabel: TLabel;
    Label2: TLabel;
    statusLabel: TLabel;
    toggleBlockButton: TBitBtn;
    emailEdit: TLabeledEdit;
    procedure closeButtonClick(Sender: TObject);
    procedure searchButtonClick(Sender: TObject);
    procedure toggleBlockButtonClick(Sender: TObject);
  private
    user : tUser;
  public

  end;

var
  UserBlockingForm: TUserBlockingForm;

implementation

{$R *.lfm}

{ TUserBlockingForm }

procedure TUserBlockingForm.searchButtonClick(Sender: TObject);
var
  found : boolean;
  usr   : tUser;
begin
  found := metru.core.retrieveUser(metruApp, self.emailEdit.Text, usr);
  if found then
    begin
      self.user := usr;
      self.toggleBlockButton.Enabled:=true;
      self.nameLabel.Caption:= self.user.fullname;
      if user.blocked then
        begin
          self.statusLabel.Caption:= 'BLOQUEADO';
          self.toggleBlockButton.Caption:= 'Desbloquear';
        end
      else
        begin
          self.statusLabel.Caption:= 'HABILITADO';
          self.toggleBlockButton.Caption:= 'Desbloquear';
        end;
    end
  else
    begin
      self.toggleBlockButton.Enabled:=false;
      self.statusLabel.Caption:=EmptyStr;
    end;
end;

procedure TUserBlockingForm.toggleBlockButtonClick(Sender: TObject);
begin
  self.user.blocked := not self.user.blocked;
  metru.core.updateUser(metruApp, self.user);
end;

procedure TUserBlockingForm.closeButtonClick(Sender: TObject);
begin
  Close;
end;

end.

