program LazarusUnitTests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, SEPATestCase, SEPACommonTests,
  SEPACreditTransferTests, SEPADirectDebitTests, SEPACommon,
  SEPACreditTransfer, SEPADirectDebit;

{$R *.res}

begin
  Application.Title:='Unit tests with Lazarus';
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

