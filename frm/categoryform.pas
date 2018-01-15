unit categoryform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, Buttons;

type

  { TCategoryForm }

  TCategoryForm = class(TForm)
    saveButton: TBitBtn;
    cancelButton: TBitBtn;
    ComboBox1: TComboBox;
    comisionEdit: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    nameEdit: TLabeledEdit;
  private

  public

  end;

var
  CategoryForm: TCategoryForm;

implementation

{$R *.lfm}

{ TCategoryForm }

end.

