unit selllistform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs
  , metru.core;

type

  { tSellListForm }

  tSellListForm = class(TForm)
  private
    sell : tSell;
  public
    constructor Create(theOwner : tComponent; ownSell : tSell); overload;
  end;

var
  fsellListForm: tSellListForm;

implementation

{$R *.lfm}

{ tSellListForm }

constructor tSellListForm.Create(theOwner: tComponent; ownSell: tSell); overload;
begin
  self.sell := ownSell;
  Create(theOwner)
end;

end.

