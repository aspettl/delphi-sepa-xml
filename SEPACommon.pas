//
//   Delphi unit with helper methods etc. for SEPA XML file creation
//   (beta version 0.2.1, 2014-02-25)
//
//   Copyright (C) 2013-2014 by Aaron Spettl
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
unit SEPACommon;

{$IFDEF FPC}                // Lazarus: set compiler mode and file encoding
{%encoding CP1252}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, StrUtils, Math, Classes, DateUtils;

const
  SCHEMA_PAIN_001_002_03    = 'pain.001.002.03';
  SCHEMA_PAIN_001_003_03    = 'pain.001.003.03';
  SCHEMA_PAIN_008_002_02    = 'pain.008.002.02';
  SCHEMA_PAIN_008_003_02    = 'pain.008.003.02';

  SEPA                      = 'SEPA';
  FIN_INSTN_NOTPROVIDED     = 'NOTPROVIDED';
  END_TO_END_ID_NOTPROVIDED = 'NOTPROVIDED';
  CCY_EUR                   = 'EUR';
  PMT_MTD_CREDIT_TRANSFER   = 'TRF';
  PMT_MTD_DIRECT_DEBIT      = 'DD';
  LCL_INSTRM_CD_CORE        = 'CORE';
  LCL_INSTRM_CD_COR1        = 'COR1';
  LCL_INSTRM_CD_B2B         = 'B2B';
  SEQ_TP_FRST               = 'FRST';
  SEQ_TP_RCUR               = 'RCUR';
  SEQ_TP_OOFF               = 'OOFF';
  SEQ_TP_FNAL               = 'FNAL';
  INSTR_PRTY_NORM           = 'NORM';
  INSTR_PRTY_HIGH           = 'HIGH';
  CHRG_BR_SLEV              = 'SLEV';
  ORGNL_DBTR_AGT_SMNDA      = 'SMNDA';
  SEPA_FALSE                = 'false';
  SEPA_TRUE                 = 'true';

  COUNTRY_CODE_DE           = 'DE';

  ID_MAX_LEN                = 35;
  INITG_PTY_NAME_MAX_LEN    = 70;
  CDTR_NM_MAX_LEN           = 70;
  END_TO_END_ID_MAX_LEN     = 35;
  DBTR_NM_MAX_LEN           = 70;
  MNDT_ID_MAX_LEN           = 35;
  RMT_INF_USTRD_MAX_LEN     = 140;

resourcestring
  EMPTY_BIC_OTHR_ID         = 'BIC required (or OthrID = NOTPROVIDED must be given).';
  BOTH_BIC_OTHR_ID          = 'BIC and OthrID must not be both given at the same time.';
  INVALID_BIC               = 'BIC "%s" not valid.';
  INVALID_OTHR_ID           = 'OthrID "%s" not valid (valid: empty or NOTPROVIDED).';
  IBAN_ONLY_NOT_ALLOWED     = 'IBAN-only is not allowed (or not yet allowed, before 2014-02-01) for this schema.';
  EMPTY_IBAN                = 'IBAN required.';
  INVALID_IBAN              = 'IBAN "%s" not valid.';
  EMPTY_AMDMNT_INF_DTLS     = 'Not all fields of mandate amendment information details may be empty at once.';
  INVALID_ORGNL_MNDT_ID     = 'OrgnlMndtId "%s" not valid.';
  INVALID_ORGNL_CRDTR_NM    = 'OrgnlCdtrSchmeIdNm "%s" not valid.';
  INVALID_ORGNL_CRDTR_ID    = 'OrgnlCdtrSchmeIdIdPrvtIdOthrId "%s" not valid.';
  INVALID_ORGNL_CRDTR_PRTRY = 'OrgnlCdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry "%s" not valid (valid: SEPA).';
  INVALID_ORGNL_FIN_INST_ID = 'OrgnlDbtrAgtFinInstIdOthrId "%s" not valid (valid: SMNDA).';
  EMPTY_MNDT_ID             = 'MndtId required.';
  EMPTY_DT_OF_SGNTR         = 'DtOfSgntr required.';
  INVALID_MNDT_ID           = 'MndtId "%s" not valid.';
  INVALID_DT_OF_SGNTR       = 'DtOfSgntr "%s" not valid (may not be in the future).';
  EMPTY_END_TO_END_ID       = 'PmtIdEndToEndId required, set to NOTPROVIDED if necessary.';
  EMPTY_INSTD_AMT_CCY       = 'InstdAmtCcy required.';
  EMPTY_DBTR_NM             = 'DbtrNm required.';
  EMPTY_RMT_INF_USTRD       = 'RmtInfUstrd required.';
  INVALID_END_TO_END_ID     = 'PmtIdEndToEndId "%s" not valid.';
  INVALID_INSTD_AMT         = 'InstdAmt "%s" not valid, positive value with at most 2 decimal places required.';
  INVALID_DBTR_NM           = 'DbtrNm "%s" not valid.';
  INVALID_ULTMT_DBTR_NM     = 'UltmtDbtrNm "%s" not valid.';
  INVALID_RMT_INF_USTRD     = 'RmtInfUstrd "%s" not valid.';
  INVALID_IBAN_NOT_DE       = 'Only German bank accounts are allowed for IBAN-only (no BIC given).';
  EMPTY_PMT_INF_ID          = 'PmtInfId required.';
  EMPTY_CDTR_NM             = 'CdtrNm required.';
  EMPTY_CDTR_ID             = 'CdtrSchmeIdIdPrvtIdOthrId required.';
  INVALID_PMT_INF_ID        = 'PmtInfId "%s" not valid.';
  INVALID_PMT_MTD           = 'PmtMtd "%s" not valid (valid: DD).';
  INVALID_LCL_INSTRM_CD     = 'PmtTpInfLclInstrmCd "%s" not valid (valid: CORE, COR1, B2B).';
  INVALID_LCL_INSTRM_CD_COR1= 'PmtTpInfLclInstrmCd "COR1" only valid with schema "pain.008.003.02".';
  INVALID_SEQ_TP            = 'PmtTpInfSeqTp "%s" not valid (valid: FRST, RCUR, OOFF, FNAL).';
  INVALID_REQD_EXCTN_DT     = 'ReqdExctnDt "%s" too early.';
  INVALID_REQD_COLLTN_DT    = 'ReqdColltnDt "%s" too early.';
  INVALID_SVC_LVL_CD        = 'PmtTpInfSvcLvlCd "%s" not valid (valid: SEPA).';
  INVALID_INSTR_PRTY        = 'PmtTpInfInstrPrty "%s" not valid (valid: NORM, HIGH).';
  INVALID_CHRG_BR           = 'ChrgBr "%s" not valid (valid: SLEV).';
  INVALID_CDTR_NM           = 'CdtrNm "%s" not valid.';
  INVALID_CDTR_ID           = 'CdtrSchmeIdIdPrvtIdOthrId "%s" not valid.';
  INVALID_CDTR_PRTRY        = 'CdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry "%s" not valid (valid: SEPA).';
  INVALID_DBTR_ACCT_NOT_DE  = 'Debtor bank account should be of a German bank.';
  INVALID_CDTR_ACCT_NOT_DE  = 'Creditor bank account should be of a German bank.';
  INVALID_SEQ_TP_FRST_SMNDA1= 'PmtTpInfSeqTp is "FRST", but there is a transaction with mandate amendment details, which is not marked "SMNDA" (same mandate, new debtor agent).';
  INVALID_SEQ_TP_FRST_SMNDA2= 'PmtTpInfSeqTp is not "FRST", but there is a transaction with mandate amendment details marked "SMNDA" (same mandate, new debtor agent), which implies "FRST".';
  UNKNOWN_SCHEMA            = 'ISO schema "%s" not known or invalid for this type of XML file.';
  EMPTY_GRP_HDR_MSG_ID      = 'GrpHdrMsgId required.';
  EMPTY_INITG_PTY_NAME      = 'GrpHdrInitgPtyName required.';
  INVALID_GRP_HDR_MSG_ID    = 'GrpHdrMsgId "%s" not valid.';
  INVALID_INITG_PTY_NAME    = 'GrpHdrInitgPtyName "%s" not valid.';
  INVALID_NB_OF_TXS         = 'No transactions contained.';
  INVALID_PMT_INF_MIXING    = 'One file may only contain payment instructions with the same PmtTpInfLclInstrmCd.';

type
  // TAccountIdentification and TFinancialInstitution are used for IBAN and BIC
  // for identification of debtor/creditor accounts and financial institutions,
  // see respective units for credit transfer and direct debit transactions.
  //
  // Note that all strings in these units are interpreted with respect to the
  // default behavior of the development environment, i.e.,
  // a) for Delphi < 2009:  ANSI strings
  // b) for Delphi >= 2009: Unicode strings
  // c) for Lazarus:        no encoding specified, ANSI is assumed

  TFinancialInstitution = class
  private
    fBIC: String;                                      // financial institution identification: BIC (8 or 11 characters)
    fOthrID: String;                                   // other identification: used for IBAN-only ("NOTPROVIDED")

    procedure SetBIC(const str: String);
  public
    property BIC: String read fBIC write SetBIC;
    property OthrID: String read fOthrID write fOthrID;

    function Validate(const schema: String): TStringList;
    procedure SaveToStream(const stream: TStream; const schema: String);
  end;

  TAccountIdentification = class
  private
    fIBAN: String;                                     // account identification: IBAN

    procedure SetIBAN(const str: String);
  public
    property IBAN: String read fIBAN write SetIBAN;

    function Validate(const schema: String): TStringList;
    procedure SaveToStream(const stream: TStream; const schema: String);
  end;

function SEPAGenerateUUID: String;

function SEPACleanIBANorBICorCI(s: String): String;
function SEPAModulo97(const str: String): Integer;
function SEPACheckIBAN(const iban: String): Boolean;
function SEPACheckBIC(const bic: String): Boolean;
function SEPACheckCI(const ci: String): Boolean;
function SEPAIsGermanIBAN(const iban: String): Boolean;

function SEPACleanString(const s: String; const maxlen: Integer = -1): String;
function SEPACheckString(const s: String; const maxlen: Integer = -1): Boolean;

function SEPACheckRounded(const d: Currency): Boolean;
function SEPAFormatAmount(const d: Currency; const digits: Integer = 2): String;
function SEPAFormatBoolean(const b: Boolean): String;
function SEPAFormatDate(const d: TDateTime): String;
function SEPAFormatDateTime(const d: TDateTime): String;

procedure SEPAWriteLine(const stream: TStream; const line: String);

var
  SEPASupportSpecialChars: Boolean = false; // support for German special characters,
                                            // only allowed in "pain.001.003.03" and
                                            // in "pain.008.003.02"

implementation

// private methods and code for better compiler compatibility

{$IFNDEF FPC}
{$IF CompilerVersion >= 15}
{$IF CompilerVersion <= 21}     // Delphi 7 to 2010:
uses                            // include unit Windows for the constant
  Windows;                      // LOCALE_SYSTEM_DEFAULT
{$IFEND}
{$IFEND}
{$ENDIF}

{$IFDEF FPC}
type UTF8String = String;       // just use the usual strings in Lazarus
{$ELSE}
{$IF CompilerVersion < 11}      // also use the usual strings in Delphi < 2007
type UTF8String = String;       // (same definition as in Delphi 2007)
{$IFEND}
{$ENDIF}

function StringToUTF8(const str: String): UTF8String;
begin
  // note: just use conversion from Unicode to UTF-8 (in Unicode-based
  // Delphi 2009 and higher), or conversion from ANSI to UTF-8 (for older
  // Delphi versions and Lazarus).
  Result := {$IFDEF Unicode}UTF8Encode{$ELSE}AnsiToUtf8{$ENDIF}(str);
end;

{$IFDEF FPC}                    // Lazarus supports TFormatSettings and has
{$DEFINE FormatSettings}        // a method "DefaultFormatSettings"
{$ELSE}
{$IF CompilerVersion >= 15}     // Delphi 7 and higher supports TFormatSettings,
{$DEFINE FormatSettings}        // but DefaultFormatSettings is not known
function DefaultFormatSettings: TFormatSettings;
begin
{$IF CompilerVersion >= 22}     // Delphi XE and later
  Result := TFormatSettings.Create;
{$ELSE}                         // Delphi 2010 and before
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, Result);
{$IFEND}
end;
{$IFEND}
{$ENDIF}

function CharIsInInterval(const c: Char; const i1: Char; const i2: Char): Boolean;
begin
  Result := ((Ord(c) >= Ord(i1)) and (Ord(c) <= Ord(i2)));
end;

function CharIsSEPAWhitelisted(const c: Char): Boolean;
begin
  // note: we do not use "c in [...]" syntax because that's only correct
  //       for strings with single-byte characters
  Result := CharIsInInterval(c, 'A', 'Z') or
            CharIsInInterval(c, 'a', 'z') or
            CharIsInInterval(c, '0', '9') or
            (c = '''') or (c = ':') or (c = '?') or
            (c = ',') or (c = '-') or (c = ' ') or
            (c = '(') or (c = '+') or (c = '.') or
            (c = ')') or (c = '/');
end;

function CharIsGermanSpecialChar(const c: Char): Boolean;
begin
  Result := (c = 'Ä') or (c = 'Ö') or (c = 'Ü') or
            (c = 'ä') or (c = 'ö') or (c = 'ü') or
            (c = 'ß') or (c = '&') or (c = '*') or
            (c = '$') or (c = '%');
end;

function ConvertAlphaToNumber(const s: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    if CharIsInInterval(s[i], '0', '9') then
      Result := Result + s[i]
    else if CharIsInInterval(s[i], 'A', 'Z') then
      Result := Result + IntToStr(10 + Ord(s[i]) - Ord('A'))
    else
      raise Exception.Create('Invalid character!');
  end;
end;

function Modulo97(const n: String): Integer;
begin
  // note: the number stored in "n" may be very large, therefore we cannot
  //       use the standard modulo methods; see also (German):
  //       http://www.pruefziffernberechnung.de/Originaldokumente/IBAN/Prufziffer_07.00.pdf
  if Length(n) > 9 then
    Result := Modulo97(IntToStr(Modulo97(Copy(n, 1, 9))) + Copy(n, 10, Length(n)-9))
  else
    Result := StrToInt(n) mod 97;
end;

function CheckCleanIBAN(const cleanIBAN: String): Boolean;
begin
  // check length
  if Length(cleanIBAN) > 34 then
  begin
    Result := false;
    Exit;
  end;

  // correct check digits?
  try
    Result := (SEPAModulo97(Copy(cleanIBAN, 5, Length(cleanIBAN)) + Copy(cleanIBAN, 1, 4)) = 1);
  except
    // invalid characters detected
    Result := false;
  end;
end;

function CheckCleanBIC(const cleanBIC: String): Boolean;
var
  i: Integer;
begin
  // check length
  Result := (Length(cleanBIC) = 8) or (Length(cleanBIC) = 11);

  // check characters
  if Result then
  begin
    for i := 1 to Length(cleanBIC) do
    begin
      if not CharIsInInterval(cleanBIC[i], '0', '9') and
         not CharIsInInterval(cleanBIC[i], 'A', 'Z') then
      begin
        Result := false;
        Break;
      end;
    end;
  end;
end;

function CheckCleanCI(const cleanCI: String): Boolean;
begin
  // check length
  if Length(cleanCI) > 35 then
  begin
    Result := false;
    Exit;
  end;

  // correct check digits?
  try
    Result := (SEPAModulo97(Copy(cleanCI, 8, Length(cleanCI)) + Copy(cleanCI, 1, 4)) = 1);
  except
    // invalid characters detected
    Result := false;
  end;
end; 

function IsGermanIBAN(const cleanIBAN: String): Boolean;
begin
  Result := (Copy(cleanIBAN, 1, 2) = COUNTRY_CODE_DE);
end;

procedure WriteString(const stream: TStream; const str: String);
var
  utf8: UTF8String;
begin
  utf8 := StringToUTF8(str);
  stream.WriteBuffer(utf8[1], Length(utf8));
end;

// commonly used (public) methods

function SEPAGenerateUUID: String;
var
  uid: TGuid;
  res: HResult;
begin
  res := CreateGuid(Uid);
  if res = S_OK then
  begin
    Result := GuidToString(uid);
    Result := StringReplace(Result, '-', '', [rfReplaceAll]);
    Result := StringReplace(Result, '{', '', [rfReplaceAll]);
    Result := StringReplace(Result, '}', '', [rfReplaceAll]);
  end
  else
    Result := IntToStr(RandomRange(10000, High(Integer)));  // fallback to simple random number
end;

function SEPACleanIBANorBICorCI(s: String): String;
begin
  // note: AnsiUpperCase works on Unicode strings since Delphi 2009
  Result := Trim(StringReplace(AnsiUpperCase(s), ' ', '', [rfReplaceAll]));
end;

function SEPAModulo97(const str: String): Integer;
var
  n: String;
begin
  n := ConvertAlphaToNumber(str);
  if n = '' then
    Result := 0
  else
    Result := Modulo97(n);
end;

function SEPACheckIBAN(const iban: String): Boolean;
begin
  Result := CheckCleanIBAN(SEPACleanIBANorBICorCI(iban));
end;

function SEPACheckBIC(const bic: String): Boolean;
begin
  Result := CheckCleanBIC(SEPACleanIBANorBICorCI(bic));
end;

function SEPACheckCI(const ci: String): Boolean;
begin
  Result := CheckCleanCI(SEPACleanIBANorBICorCI(ci));
end;

function SEPAIsGermanIBAN(const iban: String): Boolean;
begin
  Result := IsGermanIBAN(SEPACleanIBANorBICorCI(iban));
end;

function SEPACleanString(const s: String; const maxlen: Integer = -1): String;
var
  i: Integer;
begin
  Result := s;
  for i := 1 to Length(Result) do
  begin
    if not CharIsSEPAWhitelisted(Result[i]) then
    begin
      if (SEPASupportSpecialChars and CharIsGermanSpecialChar(Result[i])) then
      begin
        // some special characters are allowed in "pain.008.003.02", do not convert
        // them if "SupportGermanSpecialChars" is set
      end
      else
      begin
        // use "EPC Best Practices" to convert characters that were allowed in
        // the old DTAUS files
        if Result[i] = 'Ä' then       Result[i] := 'A'
        else if Result[i] = 'Ö' then  Result[i] := 'O'
        else if Result[i] = 'Ü' then  Result[i] := 'U'
        else if Result[i] = 'ä' then  Result[i] := 'a'
        else if Result[i] = 'ö' then  Result[i] := 'o'
        else if Result[i] = 'ü' then  Result[i] := 'u'
        else if Result[i] = 'ß' then  Result[i] := 's'
        else if Result[i] = '&' then  Result[i] := '+'
        else if Result[i] = '*' then  Result[i] := '.'
        else if Result[i] = '$' then  Result[i] := '.'
        else if Result[i] = '%' then  Result[i] := '.'
        else                          Result[i] := ' ';
      end;
    end;
  end;
  if (maxlen >= 0) and (Length(Result) > maxlen) then
    Result := Copy(Result, 1, maxlen);
end;

function SEPACheckString(const s: String; const maxlen: Integer = -1): Boolean;
begin
  Result := (SEPACleanString(s, maxlen) = s);
end;

function SEPACheckRounded(const d: Currency): Boolean;
begin
  // check that the given value is rounded to two decimal places;
  // currency values have exactly four decimal places, just use a string
  // comparison after formatting (once after rounding) - not exactly
  // elegant, but works
  Result := (CurrToStrF(d, ffFixed, 4) = CurrToStrF(SimpleRoundTo(d, -2), ffFixed, 2)+'00');
end;

function SEPAFormatAmount(const d: Currency; const digits: Integer = 2): String;
{$IFDEF FormatSettings}
var
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  Result := CurrToStrF(d, ffFixed, digits, fs);
end;
{$ELSE}
var
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := DecimalSeparator; // note: not thread-safe
  DecimalSeparator    := '.';
  Result              := CurrToStrF(d, ffFixed, digits);
  DecimalSeparator    := OldDecimalSeparator;
end;
{$ENDIF}

function SEPAFormatBoolean(const b: Boolean): String;
begin
  Result := IfThen(b, SEPA_TRUE, SEPA_FALSE);
end;

function SEPAFormatDate(const d: TDateTime): String;
begin
  Result := FormatDateTime('yyyy"-"mm"-"dd', d);
end;

function SEPAFormatDateTime(const d: TDateTime): String;
begin
  Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"."zzz"Z"', d);
end;

procedure SEPAWriteLine(const stream: TStream; const line: String);
begin
  WriteString(stream, line);
  WriteString(stream, sLineBreak);
end;

// TFinancialInstitution

procedure TFinancialInstitution.SetBIC(const str: String);
begin
  fBIC := SEPACleanIBANorBICorCI(str);
end;

function TFinancialInstitution.Validate(const schema: String): TStringList;
begin
  Result := TStringList.Create;

  if (BIC = '') and (OthrID = '') then
    Result.Append(EMPTY_BIC_OTHR_ID);

  if (BIC <> '') and (OthrID <> '') then
    Result.Append(BOTH_BIC_OTHR_ID);

  if (BIC <> '') and not SEPACheckBIC(BIC) then
    Result.Append(Format(INVALID_BIC, [BIC]));

  if (OthrID <> '') and (OthrID <> FIN_INSTN_NOTPROVIDED) then
    Result.Append(INVALID_OTHR_ID);

  if (schema = SCHEMA_PAIN_001_002_03) or (schema = SCHEMA_PAIN_008_002_02) then
  begin
    // IBAN-only not oallowed:

    if (BIC = '') and (OthrID <> '') then
      Result.Append(IBAN_ONLY_NOT_ALLOWED);
  end;
end;

procedure TFinancialInstitution.SaveToStream(const stream: TStream; const schema: String);
begin
  if (BIC = '') and (OthrID <> '') then
    SEPAWriteLine(stream, '<FinInstnId><Othr><Id>'+SEPACleanString(OthrID)+'</Id></Othr></FinInstnId>')
  else
    SEPAWriteLine(stream, '<FinInstnId><BIC>'+SEPACleanString(BIC)+'</BIC></FinInstnId>');
end;

// TAccountIdentification

procedure TAccountIdentification.SetIBAN(const str: String);
begin
  fIBAN := SEPACleanIBANorBICorCI(str);
end;

function TAccountIdentification.Validate(const schema: String): TStringList;
begin
  Result := TStringList.Create;

  if IBAN = '' then
    Result.Append(EMPTY_IBAN);

  if (IBAN <> '') and not SEPACheckIBAN(IBAN) then
    Result.Append(Format(INVALID_IBAN, [IBAN]));
end;

procedure TAccountIdentification.SaveToStream(const stream: TStream; const schema: String);
begin
  SEPAWriteLine(stream, '<Id><IBAN>'+SEPACleanString(IBAN)+'</IBAN></Id>');
end;

end.
