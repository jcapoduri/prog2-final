unit messageGeneratorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, Spin, metru.core;

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
var
  totalUser, pubId : integer;
  i, count         : integer;
  user, pubUser    : tUser;
  publication      : tPublish;
  pubIdx           : tPublishIdx;
  found            : boolean;
  list             : tPublishIdxList;
  it               : tUserIterator;
  usr              : tUser;
begin
  totalUser := self.usersSpinEditor.Value;
  pubId     := self.publicationSpinEditor.Value;
  list      := metru.core.retrieveAllPublication(metruApp);
  count     := length(list);
  found     := false;
  i         := 0;
  while (not found) and (i < count) do
    begin
      metru.core.dereferencePublication(metruApp, list[i], publication);
      if publication.id = pubId then
        found := true
      else
        i := i + 1;
    end;
  pubIdx := list[i];
  if found then
    begin
      found := metru.core.retrieveFirstUser(metruApp, it);
      i     := 0;
      metru.core.retrieveUser(metruApp, publication.idUser, pubUser);
      while found and (i < count) do
        begin
          metru.core.retrieveUser(metruApp, it, usr);
          metru.core.postMessage(metruApp, pubIdx, usr,
            'Hola. Me llamo ' + user.fullname + ' - Fecha ' + DateTimeToStr(Now),
            'Gracias. Me llamo ' + pubUser.fullname + ' - Vendo : ' + publication.itemName
          );
          found := metru.core.retrieveNextUser(metruApp, it);
        end;
      MessageDlg('Mensajes', 'Los mensajes han sido generados exitosamente', mtCustom, [mbOK], 0);
    end
  else
    MessageDlg('Advertencia', 'El Id de publicacion seleccionado no es valido', mtInformation, [mbOK], 0);
end;

end.

