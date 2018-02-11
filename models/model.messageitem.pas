unit model.messageitem;

interface

uses
  Classes, SysUtils
  ,metru.core in '..\libs\metru.core.pas'
  ;

type
  { TCategoryItem }

  TMessageItem = class(TObject)
                    message : tMessage;
                    index   : tMessageIdx;
                    constructor Create(: tMessage; idx : tMessageIdx);
                    function getQuestion() : string;
                    function getAnswer() : string;
                    function getMessagePosition() : integer;
                    procedure setQuestion(txt : string);
                    procedure setAnswer(txt : string);
                  end;

implementation
    { TCategoryItem }

  constructor TMessageItem.Create(m: tMessage; idx : tMessageIdx);
  begin
    self.category    := c;
    self.displayName := c.categoryName;
  end;

  function getQuestion() : string;
  begin
    getQuestion := self.message.question;
  end;

  function getAnswer() : string;
  begin
    getAnswer := self.message.answer;
  end;

  function getMessagePosition() : integer;
  begin
    getMessagePosition := self.message.number;
  end;

  procedure setQuestion(txt : string);
  begin
    self.message.question := txt;
  end;

  procedure setAnswer(txt : string);
  begin
    self.message.question := txt;
  end;
end.
