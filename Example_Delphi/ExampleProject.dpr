program ExampleProject;

uses
  Forms,
  ExampleUnit in 'ExampleUnit.pas' {ExampleForm},
  SEPADirectDebit in '../SEPADirectDebit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SEPA XML file example';
  Application.CreateForm(TExampleForm, ExampleForm);
  Application.Run;
end.
