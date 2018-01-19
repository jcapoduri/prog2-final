unit publicationForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, EditBtn, ExtDlgs, Buttons;

type

  { tPublicationForm }

  tPublicationForm = class(TForm)
    addPhotoButton: TBitBtn;
    saveButton: TBitBtn;
    closeButton: TBitBtn;
    categoryComboBox: TComboBox;
    descriptionEdit: TMemo;
    expireDateEdit: TDateEdit;
    Image1: TImage;
    isNewCheckBox: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    pictureDialog: TOpenPictureDialog;
    priceEdit: TFloatSpinEdit;
    titleEdit: TLabeledEdit;
  private

  public

  end;

var
  publicationForm: tPublicationForm;

implementation

{$R *.lfm}

end.

