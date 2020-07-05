//
//   Console test runner for unit tests
//   (beta version 0.4.0, 2020-07-05)
//
//   Copyright (C) 2013-2018 by Aaron Spettl
//
//   Licensed under the Apache License, Version 2.0, *or* (at your
//   option) the GNU General Public License, version 2 or (at your
//   option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   license text for more details.
//
program FPCConsoleUnitTests;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, SEPACommonTests, SEPACreditTransferTests,
  SEPADirectDebitTests, SEPATestCase, SEPACommon, SEPACreditTransfer,
  SEPADirectDebit;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
