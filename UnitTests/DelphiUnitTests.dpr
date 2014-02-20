// Uncomment the following directive to create a console application
// or leave commented to create a GUI application... 
// {$APPTYPE CONSOLE}

program DelphiUnitTests;

uses
  TestFramework {$IFDEF LINUX},
  QForms,
  QGUITestRunner {$ELSE},
  GUITestRunner {$ENDIF},
  TextTestRunner,
  SEPACommonTests in 'SEPACommonTests.pas',
  SEPACommon in '..\SEPACommon.pas',
  SEPACreditTransfer in '..\SEPACreditTransfer.pas',
  SEPADirectDebit in '..\SEPADirectDebit.pas',
  SEPATestCase in 'SEPATestCase.pas',
  SEPACreditTransferTests in 'SEPACreditTransferTests.pas';

{$R *.RES}

begin

{$IFDEF LINUX}
  QGUITestRunner.RunRegisteredTests;
{$ELSE}
  if System.IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
{$ENDIF}

end.

 