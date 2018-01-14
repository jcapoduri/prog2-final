unit userform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls,
  metru.core in '..\libs\metru.core.pas';

type

  { TUserForm }

  TUserForm = class(TForm)
    State: TComboBox;
    Label1: TLabel;
    nameEdit: TLabeledEdit;
    addressEdit: TLabeledEdit;
    saveButton: TBitBtn;
    cancelButton: TBitBtn;
    emailEdit: TLabeledEdit;
    pass1Edit: TLabeledEdit;
    pass2Edit: TLabeledEdit;
    procedure cancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure saveButtonClick(Sender: TObject);
  private

  public

  end;

var
  Form2: TUserForm;

implementation

{$R *.lfm}

{ TUserForm }


procedure TUserForm.FormCreate(Sender: TObject);
begin

end;

procedure TUserForm.cancelButtonClick(Sender: TObject);
begin
  self.Close;
end;

procedure TUserForm.saveButtonClick(Sender: TObject);
begin

end;

end.

