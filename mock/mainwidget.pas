unit mainwidget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  metru.core;

type

  { Tmocker }

  Tmocker = class(TForm)
    doButton: TButton;
    categoryCombo: TComboBox;
    messageComboBox: TComboBox;
    publicationComboBox: TComboBox;
    userComboBox: TComboBox;
    sellsComboBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure doButtonClick(Sender: TObject);
  private
    users        : array of tUser;
    categories   : tCategoryList;
    publications : tPublishIdxList;
    sells        : tSellIdxList;

    names      : TStringList;
    lastnames  : TStringList;
    emailsEnds : TStringList;
    pubNames   : TStringList;
    pubDesc    : TStringList;

    procedure generateCategories(depth, quantity: integer);
    procedure generateUsers (quantity : integer);
    procedure generatePublications (quantity : integer);
    procedure generateMessages(quantity : integer);
    procedure generateSells (quantity : integer);
    
  public
    constructor Create(theOwner : tComponent); override;
  end;

var
  mocker: Tmocker;

implementation

procedure Shuffle(Strings: TStringList);
var
  i: Integer;
begin
  for i := Strings.Count-1 downto 1 do 
    Strings.Exchange(i, Random(i+1));
end;

function RandomPick(Strings: TStringList) : string;
var
  i: Integer;
begin
  RandomPick := Strings[Random(Strings.Count)];
end;

{$R *.lfm}

{ Tmocker }

constructor Tmocker.Create(theOwner : tComponent);
begin
  names      := TStringList.Create;
  lastnames  := TStringList.Create;
  emailsEnds := TStringList.Create;
  pubNames   := TStringList.Create;
  pubDesc    := TStringList.Create;

  names.Add('Daniel');
  names.Add('Lautaro');
  names.Add('Federico');
  names.Add('Jorge');
  names.Add('Milton');
  names.Add('Lucia');
  names.Add('Ana');
  names.Add('Elina');

  lastnames.Add('Ramos');
  lastnames.Add('Medrano');
  lastnames.Add('Capoduri');
  lastnames.Add('Annese');
  lastnames.Add('Oflaherty');
  lastnames.Add('Jimenez');
  lastnames.Add('Pereira');
  lastnames.Add('Basili');

  emailsEnds.Add('gmail.com');
  emailsEnds.Add('mercatrucho.com');
  emailsEnds.Add('yahoo.com.ar');
  emailsEnds.Add('bahiablanca.gob.ar');

  pubNames.Add('plancha');
  pubNames.Add('heladera');
  pubNames.Add('computadora');
  pubNames.Add('auto');
  pubNames.Add('camioneta');
  pubNames.Add('homocinetica de R12');
  pubNames.Add('tenedor');
  pubNames.Add('placa de video');
  pubNames.Add('laptop');
  pubNames.Add('taladro');
  pubNames.Add('joystick');

  pubDesc.Add('joya nunca taxi');
  pubDesc.Add('una bizcoca');
  pubDesc.Add('esta nuevo, lo vendo oprque necesito la plata');
  pubDesc.Add('lo vendo como esta');
  pubDesc.Add('impecable, digno de ver');
  pubDesc.Add('con detalles a la vista, vendo');
  pubDesc.Add('nada, como se ve en la foto');

  inherited Create(theOwner);
end;

procedure Tmocker.doButtonClick(Sender: TObject);
begin
  Shuffle(names);
  Shuffle(lastnames);
  Shuffle(emailsEnds);
  Shuffle(pubNames);
  Shuffle(pubDesc);

  case self.categoryCombo.ItemIndex of
    0: self.generateCategories(0, 5);
    1: self.generateCategories(1, 5);
    2: self.generateCategories(2, 5);
  end;

  case self.userComboBox.ItemIndex of
    0: self.generateUsers(3);
    1: self.generateUsers(5);
    2: self.generateUsers(10);
  end;
end;

procedure Tmocker.generateCategories(depth, quantity: integer);
var
  count, i : integer;
  cat      : tCategory;
begin
  if depth > 0 then
     begin
       count := length(self.categories);
       SetLength(self.categories, count + 3);
       for i := count to count + 3 do
         begin
           cat.categoryName := 'Categoria padre depth ' + IntToStr(depth);
           cat.description  := cat.categoryName;
           cat.VAT          := 0;
           cat.parent       := 1 { fix!! }
           metru.core.createCateogry(metruApp);
           self.categories[i] := cat;
         end;
     end;
end;

procedure Tmocker.generateUsers(quantity: integer);
begin

end;

procedure Tmocker.generatePublications(quantity: integer);
begin

end;

procedure Tmocker.generateMessages(quantity: integer);
begin

end;

procedure Tmocker.generateSells(quantity: integer);
begin

end;

function createRandomUser(count : integer) : tUser;
var
  user  : tUser;
begin
  user.email      := 'jcapoduri@gmail.com';
  user.password   := 'plao_y_a_la_bolsa';
  user.fullname   := 'Jorge Capoduri';
  user.address    := 'Yrigoyen 831';
  user.providence := 4;
  user.ctimestamp := Now;
  user.photoUrl   := '';
  user.status     := false;
  user.utimestamp := Now;
  user.blocked    := false;
end;

end.

