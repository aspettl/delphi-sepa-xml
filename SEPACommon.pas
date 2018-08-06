//
//   Delphi unit with helper methods etc. for SEPA XML file creation
//   (beta version 0.3.0, 2017-10-01)
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
unit SEPACommon;

{$IFDEF FPC}                // Lazarus: set compiler mode and file encoding
{%encoding CP1252}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, StrUtils, Math, Classes, DateUtils;

const
  SCHEMA_PAIN_001_002_03    = 'pain.001.002.03'; // Credit Transfer Initiation
  SCHEMA_PAIN_001_003_03    = 'pain.001.003.03'; // - valid since 2013-11-04
  SCHEMA_PAIN_001_001_03    = 'pain.001.001.03'; // - valid since 2016-11-20

  SCHEMA_PAIN_008_002_02    = 'pain.008.002.02'; // Direct Debit Initiation
  SCHEMA_PAIN_008_003_02    = 'pain.008.003.02'; // - valid since 2013-11-04
  SCHEMA_PAIN_008_001_02    = 'pain.008.001.02'; // - valid since 2016-11-20

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
  ORGNL_DBTR_ACCT_SMNDA     = 'SMNDA';
  SEPA_FALSE                = 'false';
  SEPA_TRUE                 = 'true';

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
  SCHEMA_AMDMNT_INF_DTLS_26 = 'Mandate amendment information details according to specification in version 2.6 to 2.9 does not apply for schema "%s".';
  SCHEMA_AMDMNT_INF_DTLS_30 = 'Mandate amendment information details according to specification in version 3.0 does not apply for schema "%s".';
  INVALID_ORGNL_MNDT_ID     = 'OrgnlMndtId "%s" not valid.';
  INVALID_ORGNL_CRDTR_NM    = 'OrgnlCdtrSchmeIdNm "%s" not valid.';
  INVALID_ORGNL_CRDTR_ID    = 'OrgnlCdtrSchmeIdIdPrvtIdOthrId "%s" not valid.';
  INVALID_ORGNL_FIN_INST_ID = 'OrgnlDbtrAgtFinInstIdOthrId "%s" not valid (valid: SMNDA).';
  INVALID_SEQ_TP_FRST_SMNDA1= 'Mandate amendment details must be marked "SMNDA" (same mandate, new debtor agent) for sequence type "FRST".';
  INVALID_SEQ_TP_FRST_SMNDA2= 'Mandate amendment details are marked "SMNDA" (same mandate, new debtor agent) but the sequence type is not "FRST".';
  INVALID_ORGNL_DBTR_ACCT   = 'OrgnlDbtrAcct "%s" not valid (valid: SMNDA = same mandate, new debtor account).';
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
  EMPTY_PMT_INF_ID          = 'PmtInfId required.';
  EMPTY_CDTR_NM             = 'CdtrNm required.';
  EMPTY_CDTR_ID             = 'CdtrSchmeIdIdPrvtIdOthrId required.';
  INVALID_PMT_INF_ID        = 'PmtInfId "%s" not valid.';
  INVALID_PMT_MTD           = 'PmtMtd "%s" not valid (valid: DD).';
  INVALID_LCL_INSTRM_CD     = 'PmtTpInfLclInstrmCd "%s" not valid (valid: CORE, B2B and COR1 depending on schema).';
  INVALID_LCL_INSTRM_CD_COR1= 'PmtTpInfLclInstrmCd "COR1" is not supported with schema "pain.008.002.02".';
  INVALID_LCL_INSTRM_CD_COR1_TO_CORE = 'PmtTpInfLclInstrmCd "COR1" must be replaced by "CORE" for schema "pain.008.001.02".';
  INVALID_SEQ_TP            = 'PmtTpInfSeqTp "%s" not valid (valid: FRST, RCUR, OOFF, FNAL).';
  INVALID_REQD_EXCTN_DT     = 'ReqdExctnDt "%s" too early.';
  INVALID_REQD_COLLTN_DT    = 'ReqdColltnDt "%s" too early.';
  INVALID_SVC_LVL_CD        = 'PmtTpInfSvcLvlCd "%s" not valid (valid: SEPA).';
  INVALID_INSTR_PRTY        = 'PmtTpInfInstrPrty "%s" not valid (valid: NORM, HIGH).';
  INVALID_CHRG_BR           = 'ChrgBr "%s" not valid (valid: SLEV).';
  INVALID_CDTR_NM           = 'CdtrNm "%s" not valid.';
  INVALID_CDTR_ID           = 'CdtrSchmeIdIdPrvtIdOthrId "%s" not valid.';
  INVALID_CDTR_PRTRY        = 'CdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry "%s" not valid (valid: SEPA).';
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

    function Validate(const schema: String; const appendTo: TStringList = nil): TStringList;
    procedure SaveToStream(const stream: TStream; const schema: String);
  end;

  TAccountIdentification = class
  private
    fIBAN: String;                                     // account identification: IBAN

    procedure SetIBAN(const str: String);
  public
    property IBAN: String read fIBAN write SetIBAN;

    function Validate(const schema: String; const appendTo: TStringList = nil): TStringList;
    procedure SaveToStream(const stream: TStream; const schema: String);
  end;

function SEPAGenerateUUID: String;

function SEPACleanIBANorBICorCI(s: String): String;
function SEPAModulo97(const str: String): Integer;
function SEPACheckIBAN(const iban: String): Boolean;
function SEPACheckBIC(const bic: String): Boolean;
function SEPACheckCI(const ci: String): Boolean;

function SEPACleanString(const s: String; const maxlen: Integer = -1): String;
function SEPACheckString(const s: String; const maxlen: Integer = -1): Boolean;

function SEPACheckRounded(const d: Currency): Boolean;
function SEPAFormatAmount(const d: Currency; const digits: Integer = 2): String;
function SEPAFormatBoolean(const b: Boolean): String;
function SEPAFormatDate(const d: TDateTime): String;
function SEPAFormatDateTime(const d: TDateTime): String;
function SEPAEarliestCollectionDate(PmtTpInfLclInstrmCd: String; PmtTpInfSeqTp: String; const schema: String; BaseDate: TDateTime = 0): Cardinal;

function SEPABoolean2Xml(const value: Boolean):string;
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

{$IFNDEF FPC}
{$IF CompilerVersion < 11}
type                            // Delphi < 2007:
  UTF8String = String;          // UTF8String: use the usual strings (same definition as in Delphi 2007)
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

{$IFDEF FormatSettings}                // format settings variable with correct
var                                    // decimal separator, initialized in unit
  SEPAFormatSettings: TFormatSettings; // "initialization" block below
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

            (c = ';') or   // We allow the ; char so we can use XML-Escapes like &amp;

            (c = ')') or (c = '/');
end;

function CP1252CharIsGermanSpecialChar(const c: Char): Boolean;
begin
  // note:
  // - this method works only for CP1252-encoded ANSI strings
  // - to avoid problems with file encoding and compiler interpretation of string
  //   literals, we use hardcoded byte values for German special characters

  Result := (c = #196) or // �
            (c = #214) or // �
            (c = #220) or // �
            (c = #228) or // �
            (c = #246) or // �
            (c = #252) or // �
            (c = #223) or // �
            (c = '&') or (c = '*') or (c = '$') or (c = '%');
end;

function ConvertCP1252SpecialChar(const c: Char): Char;
begin
  // use "EPC Best Practices" to convert characters that were allowed in
  // the old DTAUS files
  //
  // note:
  // - this method works only for CP1252-encoded ANSI strings
  // - to avoid problems with file encoding and compiler interpretation of string
  //   literals, we use hardcoded byte values for German special characters

  if c = #196 then      Result := 'A' // �
  else if c = #214 then Result := 'O' // �
  else if c = #220 then Result := 'U' // �
  else if c = #228 then Result := 'a' // �
  else if c = #246 then Result := 'o' // �
  else if c = #252 then Result := 'u' // �
  else if c = #223 then Result := 's' // �
  else if c = '&' then  Result := '+'
  else if c = '*' then  Result := '.'
  else if c = '$' then  Result := '.'
  else if c = '%' then  Result := '.'
  else                  Result := ' ';
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

function SEPACleanString(const s: String; const maxlen: Integer = -1): String;
var
  i: Integer;
begin
  Result := s;

  {$IFDEF FPC_HAS_CPSTRING}
  // FPC 3: in the code below we assume ANSI strings with codepage 1252
  // (hardcoded character comparisons)
  SetCodePage(RawByteString(Result), 1252, true);
  {$ENDIF}

  for i := 1 to Length(Result) do
  begin
    if not CharIsSEPAWhitelisted(Result[i]) then
    begin
      if (SEPASupportSpecialChars and CP1252CharIsGermanSpecialChar(Result[i])) then
      begin
        // some special characters are allowed in "pain.008.003.02", do not convert
        // them if "SupportGermanSpecialChars" is set
      end
      else
      begin
        // use "EPC Best Practices" to convert characters that were allowed in
        // the old DTAUS files
        Result[i] := ConvertCP1252SpecialChar(Result[i]);
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
  // here, currency values are assumed to have exactly four relevant decimal
  // places (remaining decimal places are just ignored)
  Result := (Round(d*10000) = Round(d*100)*100);
end;

function SEPAFormatAmount(const d: Currency; const digits: Integer = 2): String;
{$IFDEF FormatSettings}
begin
  Result := CurrToStrF(d, ffFixed, digits, SEPAFormatSettings);
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

function SEPAEarliestCollectionDate(PmtTpInfLclInstrmCd: String; PmtTpInfSeqTp: String; const schema: String; BaseDate: TDateTime = 0): Cardinal;
var
  add_days, i: Integer;
begin
  // This method returns the date or more precisely a lower bound for the date
  // that is the earliest possible point in time for a direct debit transfer.
  // Note that this method does not take e.g. (bank) holidays into account, so
  // it is not precise).

  // plan to add 1 or 2 or 5 days (seen from the current date), depending on the
  // local instrument code and the sequence type
  // (note that CORE behaves as COR1 with pain.008.001.02)
  if (PmtTpInfLclInstrmCd = LCL_INSTRM_CD_CORE) and ((schema = SCHEMA_PAIN_008_002_02) or (schema = SCHEMA_PAIN_008_003_02)) then
  begin
    if (PmtTpInfSeqTp = SEQ_TP_FRST) or (PmtTpInfSeqTp = SEQ_TP_OOFF) then
      add_days := 5
    else
      add_days := 2;
  end
  else
    add_days := 1;
  // now increment the date accordingly but skip saturdays and sundays
  Result := Trunc(BaseDate);
  if Result = 0 then
    Result := Trunc(Today);
  for i := 1 to add_days do
  begin
    Inc(Result);
    while DayOfTheWeek(Result) > 5 do
      Inc(Result);
  end;
end;

function SEPABoolean2Xml(const value: Boolean):string;
begin
  if value then 
    result:='true' 
  else 
    result:='false';
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

function TFinancialInstitution.Validate(const schema: String; const appendTo: TStringList = nil): TStringList;
begin
  if appendTo <> nil then
    Result := appendTo
  else
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
    // IBAN-only not allowed:

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

function TAccountIdentification.Validate(const schema: String; const appendTo: TStringList = nil): TStringList;
begin
  if appendTo <> nil then
    Result := appendTo
  else
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

{$IFDEF FormatSettings}
initialization
  // initialize format settings variable with correct decimal separator 
  SEPAFormatSettings := DefaultFormatSettings;
  SEPAFormatSettings.DecimalSeparator := '.';
{$ENDIF}

end.
