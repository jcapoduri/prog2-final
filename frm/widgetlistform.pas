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
    procedure CloseButtonClicked(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

procedure tWidgetListForm.FormCreate(Sender: TObject);
var
  i, count : integer;
  widget   : tForm;
  item     : tComponent;
begin
  count := list.Count - 1;
  for i := 0 to count do
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

procedure tWidgetListForm.CloseButtonClicked(Sender: TObject);
begin
  Close;
end;

constructor tWidgetListForm.Create(theOwner: tComponent;
  componentsList: tComponentList); overload;
begin
  self.list := componentsList;
  Create(theOwner);
  FormCreate(nil);
end;

end.

