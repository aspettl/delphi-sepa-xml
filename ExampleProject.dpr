program ExampleProject;

uses
  Forms,
  ExampleUnit in 'ExampleUnit.pas' {ExampleForm},
  SEPADirectDebit in 'SEPADirectDebit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TExampleForm, ExampleForm);
  Application.Run;
end.
