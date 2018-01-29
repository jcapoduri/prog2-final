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
                    categoryIdx : tCategoryIdx;
                    displayName : string;
                    constructor Create(c : tCategoryIdx);
                  end;

implementation
	{ TCategoryItem }

  constructor TCategoryItem.Create(c: tCategoryIdx);
  begin
    self.categoryIdx := c;
    metru.core.dereferenceCategory(metruApp, c, self.category);
    self.displayName := category.categoryName;
  end;
end.
