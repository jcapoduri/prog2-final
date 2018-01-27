unit sellwidget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { tSellWidget }

  tSellWidget = class(TForm)
    paymentButton: TBitBtn;
    reviewButton: TBitBtn;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label5: TLabel;
    isTaxCollectedLabel: TLabel;
    Label6: TLabel;
    publishDateLabel: TLabel;
    titleLabel: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
  private

  public

  end;

var
  sellWidget: tSellWidget;

implementation

{$R *.lfm}

end.

