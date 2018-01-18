unit model.categoryitem;

interface

uses
  Classes, SysUtils
  ,metru.core in '..\libs\metru.core.pas'
  ;

type
  { TCategoryItem }

  TCategoryItem = class(TObject)
                    category    : tCategory;
                    displayName : string;
                    constructor Create(c : tCategory);
                  end;

implementation
	{ TCategoryItem }

  constructor TCategoryItem.Create(c: tCategory);
  begin
    self.category    := c;
    self.displayName := c.categoryName;
  end;
end.
