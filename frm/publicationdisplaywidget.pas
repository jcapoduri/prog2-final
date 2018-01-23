unit publicationdisplaywidget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, metru.core, publicationsellwidget;

type

  { tPublicationDisplayWidget }

  tPublicationDisplayWidget = class(TForm)
    editButton: TBitBtn;
    descriptionLabel: TLabel;
    imagePreview: TImage;
    openButton: TButton;
    priceLabel: TLabel;
    titleLabel: TLabel;
    constructor Create(var _owner : TComponent; publication: tPublish; isOwn : boolean); overload;
    procedure FormActivate(Sender: TObject);
  private
    publication : tPublish;
    isOwn       : boolean;
  public

  end;

var
  pubForm: tPublicationDisplayWidget;

implementation

{$R *.lfm}

{ tPublicationDisplayWidget }

constructor tPublicationDisplayWidget.Create(var _owner: TComponent;
  publication: tPublish; isOwn : boolean); overload;
begin
  Create(_owner);
  self.publication := publication;
  self.isOwn       := isOwn;
end;

procedure tPublicationDisplayWidget.FormActivate(Sender: TObject);
begin
  descriptionLabel.Caption := publication.details;
  titleLabel.Caption       := publication.itemName;
  priceLabel.Caption       := FloatToStr(publication.price);
end;

end.

