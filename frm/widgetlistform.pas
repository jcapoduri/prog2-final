unit widgetlistform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Contnrs, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { tWidgetListForm }

  tWidgetListForm = class(TForm)
    closeButton: TBitBtn;
    componentListScrollBox: TScrollBox;
    procedure FormActivate(Sender: TObject);
  private
    list : tComponentList;
  public
    constructor Create(theOwner : tComponent; componentsList : tComponentList); overload;
  end;

var
  fwidgetListForm: tWidgetListForm;

implementation

{$R *.lfm}

{ tWidgetListForm }

procedure tWidgetListForm.FormActivate(Sender: TObject);
var
  i      : integer;
  widget : tForm;
  item   : tComponent;
begin
  for i := 0 to list.Count do
    begin
      item           := self.list.Items[i];
      widget         := item as TForm;
      widget.parent  := self.componentListScrollBox;
      widget.top     := i * 106;
      widget.left    := 0;
      widget.width   := self.componentListScrollBox.Width;
      widget.Height  := 100;
      widget.Visible := true;
    end;
  self.componentListScrollBox.Refresh;
end;

constructor tWidgetListForm.Create(theOwner: tComponent;
  componentsList: tComponentList); overload;
begin
  self.list := componentsList;
  Create(theOwner);
  FormActivate(nil);
end;

end.

