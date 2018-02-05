unit messageGeneratorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, Spin;

type

  { TfmessageGeneratorForm }

  TmessageGeneratorForm = class(TForm)
    generateButton: TBitBtn;
    closeButton: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    publicationSpinEditor: TSpinEdit;
    messagesSpinEditor: TSpinEdit;
    usersSpinEditor: TSpinEdit;
    procedure closeButtonClick(Sender: TObject);
    procedure generateButtonClick(Sender: TObject);
  private

  public

  end;

var
  fmessageGeneratorForm: TmessageGeneratorForm;

implementation

{$R *.lfm}

{ TfmessageGeneratorForm }

procedure TmessageGeneratorForm.closeButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TmessageGeneratorForm.generateButtonClick(Sender: TObject);
begin

end;

end.

