program ExampleProject;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ExampleUnit, SEPADirectDebit, SEPACommon, SEPACreditTransfer
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='SEPA XML file example';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TExampleForm, ExampleForm);
  Application.Run;
end.

