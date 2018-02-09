unit messageCheckForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, metru.core;

type

  { TmessageCheckFrm }

  TmessageCheckFrm = class(TForm)
    balanceButton: TBitBtn;
    limitLevelLabel: TLabel;
    levelsLabel: TLabel;
    percentageLabel: TLabel;
    statusLimitLabel: TLabel;
    resultLabel: TLabel;
    procedure balanceButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  messageCheckFrm: TmessageCheckFrm;

implementation

{$R *.lfm}

{ TmessageCheckFrm }


procedure TmessageCheckFrm.FormCreate(Sender: TObject);
var
  totalNodes, totalLevels, threshold, thresholdLevel, targetNodesAtLevel : LongInt;
begin
  totalLevels        := metru.core.getMessagesTotalLevels(metruApp);
  threshold          := metru.core.getMessagesThreshold(metruApp);
  thresholdLevel     := round(totalLevels  * (threshold / 100));
  targetNodesAtLevel := 1 shl thresholdLevel;
  totalNodes         := metru.core.getMessagesTotalNodesPerLevel(metruApp, thresholdLevel);

  self.levelsLabel.Caption      := self.levelsLabel.Caption + IntToStr(totalLevels);
  self.percentageLabel.Caption  := self.percentageLabel.Caption + IntToStr(threshold) + '%';
  self.limitLevelLabel.Caption  := self.limitLevelLabel.Caption + IntToStr(thresholdLevel);
  self.statusLimitLabel.Caption := self.statusLimitLabel.Caption + IntToStr(totalNodes) + '/' + IntToStr(targetNodesAtLevel);
  if targetNodesAtLevel = totalNodes then
    self.resultLabel.Caption    := self.resultLabel.Caption + 'El arbol se encuentra balanceado, performando correctamente. No es necesario un rebalanceo'
  else
    self.resultLabel.Caption    := self.resultLabel.Caption + 'El arbol se encuentra desbalanceado, volviendolo ineficiente. Es necesario un rebalanceo'
end;

procedure TmessageCheckFrm.balanceButtonClick(Sender: TObject);
begin
  Close;
end;

end.

