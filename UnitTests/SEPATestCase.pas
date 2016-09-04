//
//   SEPA unit tests base class
//   (beta version 0.2.3, 2016-09-03)
//
//   Copyright (C) 2013-2016 by Aaron Spettl
//
//   This program is free software; you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation; either version 2 of the License, or
//   (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; if not, write to the Free Software
//   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
//   Author:  Aaron Spettl
//            Virchowstr. 26
//            89075 Ulm
//            Germany
//   E-mail:  aaron@spettl.de
//
unit SEPATestCase;

{$IFDEF FPC}                // Lazarus: set compiler mode and file encoding
{%encoding CP1252}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SEPACommon,
  {$IFDEF FPC}
  fpcunit, testutils, testregistry
  {$ELSE}
  TestFrameWork
  {$ENDIF},
  Classes, SysUtils, StrUtils;

type
  {$IFNDEF FPC}
  {$IFNDEF Unicode}
  RawByteString = AnsiString; // define RawByteString as an ANSI string before Delphi 2009
  {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC}
  {$IFNDEF FPC_HAS_CPSTRING}
  RawByteString = AnsiString; // define RawByteString as an ANSI string for FPC < 3.0
  {$ENDIF}
  {$ENDIF}

  TSEPATestCase = class(TTestCase)
  private
    fOld_SEPASupportSpecialChars: Boolean;

    fSaveStream: TMemoryStream;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function FetchAndResetSaveStream: RawByteString;

    procedure CheckValidation(expected: array of String; actual: TStrings; msg: String = '');
    procedure CheckValidationContains(expected: array of String; actual: TStrings; msg: String = '');
    procedure CheckSaveStream(expected: RawByteString; msg: String = '');

    property SaveStream: TMemoryStream read fSaveStream;
  end;

implementation

// TSEPATestCase

function TSEPATestCase.FetchAndResetSaveStream: RawByteString;
begin
  // read written bytes from stream
  fSaveStream.Seek(0, soFromBeginning);
  SetLength(Result, fSaveStream.Size);
  fSaveStream.ReadBuffer(Result[1], fSaveStream.Size);

  // clear stream
  fSaveStream.Clear;
end;

procedure TSEPATestCase.CheckValidation(expected: array of String; actual: TStrings; msg: String = '');
var
  expectedStringList: TStringList;
  i: Integer;
begin
  expectedStringList := TStringList.Create;
  try
    // note: we do not only compare the validation messages, but also their order
    for i := Low(expected) to High(expected) do
    begin
      if expected[i] <> '' then
        expectedStringList.Add(expected[i]);
    end;

    CheckEquals(expectedStringList.Text, actual.Text, msg);
  finally
    expectedStringList.Free;
  end;
end;

procedure TSEPATestCase.CheckValidationContains(expected: array of String; actual: TStrings; msg: String = '');
var
  i: Integer;
begin
  for i := Low(expected) to High(expected) do
  begin
    if expected[i] <> '' then
      Check(actual.IndexOf(expected[i]) >= 0, msg + IfThen(msg<>'', ', ', '') +
            Format('expected "%s" to be contained in "%s"', [expected[i], actual.Text]));
  end;
end;

procedure TSEPATestCase.CheckSaveStream(expected: RawByteString; msg: String = '');

  function RemoveWhitespace(const str: RawByteString): RawByteString;
  begin
    // Just remove line breaks by replacing them with spaces, then
    // replace all remaining spaces between tags. This is not very
    // sophisticated, but for our case it is enough...
    Result := str;
    Result := AnsiReplaceStr(Result, #13, ' ');
    Result := AnsiReplaceStr(Result, #10, ' ');
    while Pos(' <', Result) > 0 do             
      Result := AnsiReplaceStr(Result, ' <', '<');
    while Pos('> ', Result) > 0 do
      Result := AnsiReplaceStr(Result, '> ', '>');
  end;

var
  actual: RawByteString;
begin
  actual := FetchAndResetSaveStream;
  CheckEquals(RemoveWhitespace(expected), RemoveWhitespace(actual), msg);
end;

procedure TSEPATestCase.SetUp;
begin
  // make sure all global variables / constants are in a consistent state;
  // restore them after the tests
  fOld_SEPASupportSpecialChars := SEPASupportSpecialChars;
  SEPASupportSpecialChars := false;

  fSaveStream := TMemoryStream.Create;
end;

procedure TSEPATestCase.TearDown;
begin
  FreeAndNil(fSaveStream);

  SEPASupportSpecialChars := fOld_SEPASupportSpecialChars;
end;

end.
