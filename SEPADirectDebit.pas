//
//   Delphi unit for SEPA direct debit XML file creation
//   (beta version 0.1.3, 2014-01-17)
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
unit SEPADirectDebit;

{%encoding CP1252}  // for Lazarus: This file is CP-1252 encoded (not UTF-8).

interface

uses
  SysUtils, StrUtils, Math, Classes, DateUtils;

const
  SCHEMA_PAIN_008_002_02    = 'pain.008.002.02';
  SCHEMA_PAIN_008_003_02    = 'pain.008.003.02';

  SEPA                      = 'SEPA';
  FIN_INSTN_NOTPROVIDED     = 'NOTPROVIDED';
  END_TO_END_ID_NOTPROVIDED = 'NOTPROVIDED';
  CCY_EUR                   = 'EUR';
  PMT_MTD_DIRECT_DEBIT      = 'DD';
  LCL_INSTRM_CD_CORE        = 'CORE';
  LCL_INSTRM_CD_COR1        = 'COR1';
  LCL_INSTRM_CD_B2B         = 'B2B';
  SEQ_TP_FRST               = 'FRST';
  SEQ_TP_RCUR               = 'RCUR';
  SEQ_TP_OOFF               = 'OOFF';
  SEQ_TP_FNAL               = 'FNAL';
  CHRG_BR_SLEV              = 'SLEV';
  ORGNL_DBTR_AGT_SMNDA      = 'SMNDA';
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
  EMPTY_BIC_OTHR_ID         = 'TFinancialInstitution: BIC required (or OthrID must be given).';
  BOTH_BIC_OTHR_ID          = 'TFinancialInstitution: BIC and OthrID must not be both given.';
  INVALID_BIC               = 'TFinancialInstitution: BIC "%s" not valid.';
  INVALID_OTHR_ID           = 'TFinancialInstitution: OthrID "%s" not valid (valid: empty or NOTPROVIDED).';
  INVALID_OTHR_ID_NONEMPTY  = 'TFinancialInstitution: OthrID has to be empty when a BIC is supplied (or before February 1st, 2014).';
  EMPTY_IBAN                = 'TAccountIdentification: IBAN required.';
  INVALID_IBAN              = 'TAccountIdentification: IBAN "%s" not valid.';
  EMPTY_AMDMNT_INF_DTLS     = 'TAmendmentInformationDetails: not all fields may be empty at once.';
  INVALID_ORGNL_MNDT_ID     = 'TAmendmentInformationDetails: OrgnlMndtId "%s" not valid.';
  INVALID_ORGNL_CRDTR_NM    = 'TAmendmentInformationDetails: OrgnlCdtrSchmeIdNm "%s" not valid.';
  INVALID_ORGNL_CRDTR_ID    = 'TAmendmentInformationDetails: OrgnlCdtrSchmeIdIdPrvtIdOthrId "%s" not valid.';
  INVALID_ORGNL_CRDTR_PRTRY = 'TAmendmentInformationDetails: OrgnlCdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry "%s" not valid (valid: SEPA).';
  INVALID_ORGNL_FIN_INST_ID = 'TAmendmentInformationDetails: OrgnlDbtrAgtFinInstIdOthrId "%s" not valid (valid: SMNDA).';
  EMPTY_MNDT_ID             = 'TMandateRelatedInformation: MndtId required.';
  EMPTY_DT_OF_SGNTR         = 'TMandateRelatedInformation: DtOfSgntr required.';
  INVALID_MNDT_ID           = 'TMandateRelatedInformation: MndtId "%s" not valid.';
  INVALID_DT_OF_SGNTR       = 'TMandateRelatedInformation: DtOfSgntr "%s" not valid (may not be in the future).';
  EMPTY_END_TO_END_ID       = 'TDirectDebitTransactionInformation: PmtIdEndToEndId required, set to NOTPROVIDED if necessary.';
  EMPTY_INSTD_AMT_CCY       = 'TDirectDebitTransactionInformation: InstdAmtCcy required.';
  EMPTY_DBTR_NM             = 'TDirectDebitTransactionInformation: DbtrNm required.';
  EMPTY_RMT_INF_USTRD       = 'TDirectDebitTransactionInformation: RmtInfUstrd required.';
  INVALID_END_TO_END_ID     = 'TDirectDebitTransactionInformation: PmtIdEndToEndId "%s" not valid.';
  INVALID_INSTD_AMT         = 'TDirectDebitTransactionInformation: InstdAmt "%s" not valid, positive value required.';
  INVALID_DBTR_NM           = 'TDirectDebitTransactionInformation: DbtrNm "%s" not valid.';
  INVALID_ULTMT_DBTR_NM     = 'TDirectDebitTransactionInformation: UltmtDbtrNm "%s" not valid.';
  INVALID_RMT_INF_USTRD     = 'TDirectDebitTransactionInformation: RmtInfUstrd "%s" not valid.';
  INVALID_IBAN_NOT_DE       = 'TDirectDebitTransactionInformation: Only German bank accounts are allowed for IBAN-only (no BIC given).';
  EMPTY_PMT_INF_ID          = 'TPaymentInstructionInformation: PmtInfId required.';
  EMPTY_CDTR_NM             = 'TPaymentInstructionInformation: CdtrNm required.';
  EMPTY_CDTR_ID             = 'TPaymentInstructionInformation: CdtrSchmeIdIdPrvtIdOthrId required.';
  INVALID_PMT_INF_ID        = 'TPaymentInstructionInformation: PmtInfId "%s" not valid.';
  INVALID_PMT_MTD           = 'TPaymentInstructionInformation: PmtMtd "%s" not valid (valid: DD).';
  INVALID_LCL_INSTRM_CD     = 'TPaymentInstructionInformation: PmtTpInfLclInstrmCd "%s" not valid (valid: CORE, COR1, B2B).';
  INVALID_LCL_INSTRM_CD_COR1= 'TPaymentInstructionInformation: PmtTpInfLclInstrmCd "COR1" only valid with schema "pain.008.003.02".';
  INVALID_SEQ_TP            = 'TPaymentInstructionInformation: PmtTpInfSeqTp "%s" not valid (valid: FRST, RCUR, OOFF, FNAL).';
  INVALID_REQD_COLLTN_DT    = 'TPaymentInstructionInformation: ReqdColltnDt "%s" too early.';
  INVALID_SVC_LVL_CD        = 'TPaymentInstructionInformation: PmtTpInfSvcLvlCd "%s" not valid (valid: SEPA).';
  INVALID_CHRG_BR           = 'TPaymentInstructionInformation: ChrgBr "%s" not valid (valid: SLEV).';
  INVALID_CDTR_NM           = 'TPaymentInstructionInformation: CdtrNm "%s" not valid.';
  INVALID_CDTR_ID           = 'TPaymentInstructionInformation: CdtrSchmeIdIdPrvtIdOthrId "%s" not valid.';
  INVALID_CDTR_PRTRY        = 'TPaymentInstructionInformation: CdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry "%s" not valid (valid: SEPA).';
  INVALID_CDTR_ACCT_NOT_DE  = 'TPaymentInstructionInformation: Creditor bank account should be of a German bank.';
  INVALID_SEQ_TP_FRST_SMNDA1= 'TPaymentInstructionInformation: PmtTpInfSeqTp is "FRST", but there is a transaction with mandate amendment details, which is not marked "SMNDA" (same mandate, new debtor agent).';
  INVALID_SEQ_TP_FRST_SMNDA2= 'TPaymentInstructionInformation: PmtTpInfSeqTp is not "FRST", but there is a transaction with mandate amendment details marked "SMNDA" (same mandate, new debtor agent), which implies "FRST".';
  UNKNOWN_SCHEMA            = 'TDirectDebitInitiation: ISO schema "%s" not known.';
  EMPTY_GRP_HDR_MSG_ID      = 'TDirectDebitInitiation: GrpHdrMsgId required.';
  EMPTY_INITG_PTY_NAME      = 'TDirectDebitInitiation: GrpHdrInitgPtyName required.';
  INVALID_GRP_HDR_MSG_ID    = 'TDirectDebitInitiation: GrpHdrMsgId "%s" not valid.';
  INVALID_INITG_PTY_NAME    = 'TDirectDebitInitiation: GrpHdrInitgPtyName "%s" not valid.';
  INVALID_NB_OF_TXS         = 'TDirectDebitInitiation: no transactions contained.';
  INVALID_PMT_INF_MIXING    = 'TDirectDebitInitiation: one file may only contain payment instructions with the same PmtTpInfLclInstrmCd.';

type
  // In the following, all necessary classes to create direct debit transactions
  // for a SEPA XML file are introduced. Please have a look at the specification
  // of the XML data format at http://www.ebics.de/index.php?id=77 (section 2.2.2,
  // "Anlage3_Datenformate_V2.7.pdf" by EBICS, Die Deutsche Kreditwirtschaft).
  //
  // Short explanation of XML file:
  //
  // XML tags                      corresponding class
  // ---------------------------------------------------------------------------
  // <Document>                    TDirectDebitInitiation
  //   <CstmrDrctDbtInitn>         TDirectDebitInitiation
  //     <PmtInf>                  TPaymentInstructionInformation
  //       <DrctDbtTxInf>          TDirectDebitTransactionInformation
  //         <MndtRltdInf>         TMandateRelatedInformation
  //           <AmdmntInfDtls>     TAmendmentInformationDetails
  //       <DrctDbtTxInf>          ...
  //         ...
  //     <PmtInf>
  //       ...
  //
  // TAccountIdentification and TFinancialInstitution are used for IBAN and BIC,
  // e.g. in TDirectDebitTransactionInformation and TPaymentInstructionInformation.
  //
  // Note that all strings in this unit are interpreted with respect to the
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

  TAmendmentInformationDetails = class
  private
    fOrgnlMndtId: String;                              // original mandate identification
    fOrgnlCdtrSchmeIdNm: String;                       // original creditor name
    fOrgnlCdtrSchmeIdIdPrvtIdOthrId: String;           // original creditor identifier
    fOrgnlCdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry: String; // (always "SEPA")
    fOrgnlDbtrAcct: TAccountIdentification;            // original debtor account identification
    fOrgnlDbtrAgtFinInstIdOthrId: String;              // "SMNDA" if same mandate + new debtor agent

    procedure SetOrgnlCdtrSchmeIdIdPrvtIdOthrId(const str: String);
  public
    constructor Create;

    property OrgnlMndtId: String read fOrgnlMndtId write fOrgnlMndtId;
    property OrgnlCdtrSchmeIdNm: String read fOrgnlCdtrSchmeIdNm write fOrgnlCdtrSchmeIdNm;
    property OrgnlCdtrSchmeIdIdPrvtIdOthrId: String read fOrgnlCdtrSchmeIdIdPrvtIdOthrId write SetOrgnlCdtrSchmeIdIdPrvtIdOthrId;
    property OrgnlCdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry: String read fOrgnlCdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry write fOrgnlCdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry;
    property OrgnlDbtrAcct: TAccountIdentification read fOrgnlDbtrAcct;
    property OrgnlDbtrAgtFinInstIdOthrId: String read fOrgnlDbtrAgtFinInstIdOthrId write fOrgnlDbtrAgtFinInstIdOthrId;

    function Validate(const schema: String): TStringList;
    procedure SaveToStream(const stream: TStream; const schema: String);
  end;

  TMandateRelatedInformation = class
  private
    fMndtId: String;                                   // mandate identification
    fDtOfSgntr: TDateTime;                             // date of signature
    fAmdmntInd: Boolean;                               // amendment indicator ("false" or "true")
    fAmdmntInfDtls: TAmendmentInformationDetails;
  public
    constructor Create;

    property MndtId: String read fMndtId write fMndtId;
    property DtOfSgntr: TDateTime read fDtOfSgntr write fDtOfSgntr;
    property AmdmntInd: Boolean read fAmdmntInd write fAmdmntInd;
    property AmdmntInfDtls: TAmendmentInformationDetails read fAmdmntInfDtls;

    function Validate(const schema: String): TStringList;
    procedure SaveToStream(const stream: TStream; const schema: String);
  end;

  TDirectDebitTransactionInformation = class
  private
    fPmtIdEndToEndId: String;                          // end-to-end identification of this payment (by default "NOTPROVIDED")
    fInstdAmtCcy: String;                              // instructed amount, currency (always "EUR")
    fInstdAmt: Double;                                 // instructed amount
    fDrctDbtTxMndtRltdInf: TMandateRelatedInformation;
    fDbtrAgt: TFinancialInstitution;                   // debtor agent
    fDbtrNm: String;                                   // debtor name
    fDbtrAcct: TAccountIdentification;                 // debtor account identification
    fUltmtDbtrNm: String;                              // ultimate debtor name (optional)
    fRmtInfUstrd: String;                              // unstructured remittance information

    procedure SetDbtrNm(const str: String);
    procedure SetUltmtDbtrNm(const str: String);
    procedure SetRmtInfUstrd(const str: String);
  public
    constructor Create;

    property PmtIdEndToEndId: String read fPmtIdEndToEndId write fPmtIdEndToEndId;
    property InstdAmtCcy: String read fInstdAmtCcy write fInstdAmtCcy;
    property InstdAmt: Double read fInstdAmt write fInstdAmt;
    property DrctDbtTxMndtRltdInf: TMandateRelatedInformation read fDrctDbtTxMndtRltdInf;
    property DbtrAgt: TFinancialInstitution read fDbtrAgt;
    property DbtrNm: String read fDbtrNm write SetDbtrNm;
    property DbtrAcct: TAccountIdentification read fDbtrAcct;
    property UltmtDbtrNm: String read fUltmtDbtrNm write SetUltmtDbtrNm;
    property RmtInfUstrd: String read fRmtInfUstrd write SetRmtInfUstrd;

    function Validate(const schema: String): TStringList;
    procedure SaveToStream(const stream: TStream; const schema: String);
  end;

  TPaymentInstructionInformation = class
  private
    fPmtInfId: String;                                 // payment information identification
    fPmtMtd: String;                                   // payment method (always "DD")
    fPmtTpInfSvcLvlCd: String;                         // payment type, service level code (always "SEPA")
    fPmtTpInfLclInstrmCd: String;                      // payment type, local instrument code ("CORE" or "B2B")
    fPmtTpInfSeqTp: String;                            // payment type, sequence type ("FRST", "RCUR", "OOFF" or "FNAL")
    fReqdColltnDt: TDateTime;                          // requested collection date
    fCdtrNm: String;                                   // creditor name
    fCdtrAcct: TAccountIdentification;                 // creditor account identification
    fCdtrAgt: TFinancialInstitution;                   // creditor agent
    fChrgBr: String;                                   // charge bearer (always "SLEV")
    fCdtrSchmeIdIdPrvtIdOthrId: String;                // creditor identifier
    fCdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry: String;      // proprietary (always "SEPA")
    fDrctDbtTxInf: array of TDirectDebitTransactionInformation;

    procedure SetCdtrNm(const str: String);
    procedure SetCdtrSchmeIdIdPrvtIdOthrId(const str: String);

    function GetCtrlSum: Double;
    function GetDrctDbtTxInfEntry(const i: Integer): TDirectDebitTransactionInformation;
    function GetDrctDbtTxInfCount: Integer;
  public
    constructor Create;

    property PmtInfId: String read fPmtInfId write fPmtInfId;
    property PmtMtd: String read fPmtMtd write fPmtMtd;
    property NbOfTxs: Integer read GetDrctDbtTxInfCount;
    property CtrlSum: Double read GetCtrlSum;
    property PmtTpInfSvcLvlCd: String read fPmtTpInfSvcLvlCd write fPmtTpInfSvcLvlCd;
    property PmtTpInfLclInstrmCd: String read fPmtTpInfLclInstrmCd write fPmtTpInfLclInstrmCd;
    property PmtTpInfSeqTp: String read fPmtTpInfSeqTp write fPmtTpInfSeqTp;
    property ReqdColltnDt: TDateTime read fReqdColltnDt write fReqdColltnDt;
    property CdtrNm: String read fCdtrNm write SetCdtrNm;
    property CdtrAcct: TAccountIdentification read fCdtrAcct;
    property CdtrAgt: TFinancialInstitution read fCdtrAgt;
    property ChrgBr: String read fChrgBr write fChrgBr;
    property CdtrSchmeIdIdPrvtIdOthrId: String read fCdtrSchmeIdIdPrvtIdOthrId write SetCdtrSchmeIdIdPrvtIdOthrId;
    property CdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry: String read fCdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry write fCdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry;

    procedure AppendDrctDbtTxInfEntry(const transaction: TDirectDebitTransactionInformation);
    property DrctDbtTxInfEntry[const i: Integer]: TDirectDebitTransactionInformation read GetDrctDbtTxInfEntry;
    property DrctDbtTxInfCount: Integer read GetDrctDbtTxInfCount;

    function Validate(const schema: String): TStringList;
    procedure SaveToStream(const stream: TStream; const schema: String);
  end;

  TDirectDebitInitiation = class
  private
    fSchema: String;                                   // ISO schema, e.g. "pain.008.002.02", empty means auto-select based on date and COR1 / IBAN-only
    fGrpHdrMsgId: String;                              // group header: message identification
    fGrpHdrCreDtTm: TDateTime;                         // group header: time of file creation
    fGrpHdrInitgPtyName: String;                       // group header: initiator name
    fPmtInf: array of TPaymentInstructionInformation;

    function GetSchema: String;
    procedure SetGrpHdrInitgPtyName(const str: String);

    function GetGrpHdrNbOfTxs: Integer;
    function GetPmtInfEntry(const i: Integer): TPaymentInstructionInformation;
    function GetPmtInfCount: Integer;
  public
    constructor Create;

    property Schema: String read GetSchema write fSchema;

    property GrpHdrMsgId: String read fGrpHdrMsgId write fGrpHdrMsgId;
    property GrpHdrCreDtTm: TDateTime read fGrpHdrCreDtTm write fGrpHdrCreDtTm;
    property GrpHdrNbOfTxs: Integer read GetGrpHdrNbOfTxs;
    property GrpHdrInitgPtyName: String read fGrpHdrInitgPtyName write SetGrpHdrInitgPtyName;

    procedure AppendPmtInfEntry(const instruction: TPaymentInstructionInformation);
    property PmtInfEntry[const i: Integer]: TPaymentInstructionInformation read GetPmtInfEntry;
    property PmtInfCount: Integer read GetPmtInfCount;

    function Validate: TStringList;
    procedure SaveToStream(const stream: TStream);
    procedure SaveToDisk(const FileName: String);
  end;

// public methods
function SEPACheckIBAN(const iban: String): Boolean;
function SEPACheckBIC(const bic: String): Boolean;
function SEPACheckCI(const ci: String): Boolean;

implementation

// commonly used methods (private)

function GetUUID(): String;
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

function SEPAConvertAlphaToNumber(const s: String): String;
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

function SEPAModulo97(const n: String): Integer;
begin
  if Length(n) > 9 then
    Result := SEPAModulo97(IntToStr(SEPAModulo97(Copy(n, 1, 9))) + Copy(n, 10, Length(n)))
  else
    Result := StrToInt(n) mod 97;
end;

function SEPACleanIBANorBICorCI(s: String): String;
begin
  // note: AnsiUpperCase works on Unicode strings since Delphi 2009
  Result := Trim(StringReplace(AnsiUpperCase(s), ' ', '', [rfReplaceAll]));
end;

function SEPACheckCleanIBAN(const cleanIBAN: String): Boolean;
begin
  // check length
  if Length(cleanIBAN) > 34 then
  begin
    Result := false;
    Exit;
  end;

  // correct check digits?
  try
    Result := (SEPAModulo97(SEPAConvertAlphaToNumber(Copy(cleanIBAN, 5, Length(cleanIBAN)) + Copy(cleanIBAN, 1, 4))) = 1);
  except
    // invalid characters detected
    Result := false;
  end;
end;

function SEPACheckCleanBIC(const cleanBIC: String): Boolean;
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

function SEPACheckCleanCI(const cleanCI: String): Boolean;
begin
  // check length
  if Length(cleanCI) > 35 then
  begin
    Result := false;
    Exit;
  end;

  // correct check digits?
  try
    Result := (SEPAModulo97(SEPAConvertAlphaToNumber(Copy(cleanCI, 8, Length(cleanCI)) + Copy(cleanCI, 1, 4))) = 1);
  except
    // invalid characters detected
    Result := false;
  end;
end;

function SEPAIsGermanIBAN(const cleanIBAN: String): Boolean;
begin
  Result := (Copy(cleanIBAN, 1, 2) = 'DE');
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
  if (maxlen >= 0) and (Length(Result) > maxlen) then
    Result := Copy(Result, 1, maxlen);
end;

function SEPACheckString(const s: String; const maxlen: Integer = -1): Boolean;
begin
  Result := (SEPACleanString(s, maxlen) = s);
end;

function SEPARoundAmount(const d: Double): Double;
begin
  Result := RoundTo(d, -2);
end;

{$IFDEF FPC} // TFormatSettings available in Lazarus
function SEPAFormatAmount(const d: Double): String;
var
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  Result := Format('%.2f', [SEPARoundAmount(d)], fs); // round explicitly, makes sure that SEPARoundAmount and SEPAFormatAmount behaves exactly the same
end;
{$ELSE}
{$IF CompilerVersion >= 15} // TFormatSettings available since Delphi 7 (?)
function SEPAFormatAmount(const d: Double): String;
var
  fs: TFormatSettings;
begin
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
  Result := Format('%.2f', [SEPARoundAmount(d)], fs); // round explicitly, makes sure that SEPARoundAmount and SEPAFormatAmount behaves exactly the same
end;
{$ELSE}
function SEPAFormatAmount(const d: Double): String;
var
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := DecimalSeparator; // note: not thread-safe
  DecimalSeparator    := '.';
  Result              := Format('%.2f', [SEPARoundAmount(d)]); // round explicitly, makes sure that SEPARoundAmount and SEPAFormatAmount behaves exactly the same
  DecimalSeparator    := OldDecimalSeparator;
end;
{$IFEND}
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

procedure WriteString(const stream: TStream; const str: String);
var
  utf8: {$IFDEF Unicode}UTF8String{$ELSE}String{$ENDIF};
begin
  // note: we do not use any special characters, so we really don't
  // care about encodings - just use conversion from Unicode to
  // UTF-8 (in Unicode-based Delphi 2009 and higher), or conversion
  // from ANSI to UTF-8 (for older Delphi versions).
  utf8 := {$IFDEF Unicode}UTF8Encode{$ELSE}AnsiToUtf8{$ENDIF}(str);
  stream.WriteBuffer(utf8[1], Length(utf8));
end;

procedure WriteLine(const stream: TStream; const line: String);
begin
  WriteString(stream, line);
  WriteString(stream, sLineBreak);
end;

// commonly used methods (public)

function SEPACheckIBAN(const iban: String): Boolean;
begin
  Result := SEPACheckCleanIBAN(SEPACleanIBANorBICorCI(iban));
end;

function SEPACheckBIC(const bic: String): Boolean;
begin
  Result := SEPACheckCleanBIC(SEPACleanIBANorBICorCI(bic));
end;

function SEPACheckCI(const ci: String): Boolean;
begin
  Result := SEPACheckCleanCI(SEPACleanIBANorBICorCI(ci));
end;

// TFinancialInstitution

procedure TFinancialInstitution.SetBIC(const str: String);
begin
  fBIC := SEPACleanIBANorBICorCI(str);
end;

function TFinancialInstitution.Validate(const schema: String): TStringList;
begin
  Result := TStringList.Create;

  if (schema = SCHEMA_PAIN_008_002_02) or
     ((schema = SCHEMA_PAIN_008_003_02) and (Now < EncodeDate(2014, 2, 1))) then
  begin
    // IBAN-only not allowed:

    if BIC = '' then
      Result.Append(EMPTY_BIC_OTHR_ID);

    if OthrID <> '' then
      Result.Append(INVALID_OTHR_ID_NONEMPTY);

    if not SEPACheckCleanBIC(BIC) then
      Result.Append(Format(INVALID_BIC, [BIC]));
  end
  else
  begin
    // IBAN-only allowed:

    if (BIC = '') and (OthrID = '') then
      Result.Append(EMPTY_BIC_OTHR_ID);

    if (BIC <> '') and (OthrID <> '') then
      Result.Append(BOTH_BIC_OTHR_ID);

    if not SEPACheckCleanBIC(BIC) then
      Result.Append(Format(INVALID_BIC, [BIC]));

    if (OthrID <> '') and (OthrID <> FIN_INSTN_NOTPROVIDED) then
      Result.Append(INVALID_OTHR_ID);
  end;
end;

procedure TFinancialInstitution.SaveToStream(const stream: TStream; const schema: String);
begin
  if (BIC = '') and (OthrID <> '') then
    WriteLine(stream, '<FinInstnId><Othr><Id>'+SEPACleanString(OthrID)+'</Id></Othr></FinInstnId>')
  else
    WriteLine(stream, '<FinInstnId><BIC>'+SEPACleanString(BIC)+'</BIC></FinInstnId>');
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

  if not SEPACheckCleanIBAN(IBAN) then
    Result.Append(Format(INVALID_IBAN, [IBAN]));
end;

procedure TAccountIdentification.SaveToStream(const stream: TStream; const schema: String);
begin
  WriteLine(stream, '<Id><IBAN>'+SEPACleanString(IBAN)+'</IBAN></Id>');
end;

// TAmendmentInformationDetails

constructor TAmendmentInformationDetails.Create;
begin
  inherited Create;
  fOrgnlCdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry := SEPA;
  fOrgnlDbtrAcct                            := TAccountIdentification.Create;
end;

procedure TAmendmentInformationDetails.SetOrgnlCdtrSchmeIdIdPrvtIdOthrId(const str: String);
begin
  fOrgnlCdtrSchmeIdIdPrvtIdOthrId := SEPACleanIBANorBICorCI(str);
end;

function TAmendmentInformationDetails.Validate(const schema: String): TStringList;
begin
  Result := TStringList.Create;

  // check for empty fields

  if (OrgnlMndtId = '') and (OrgnlCdtrSchmeIdNm = '') and (OrgnlCdtrSchmeIdIdPrvtIdOthrId = '') and
     (OrgnlDbtrAcct.IBAN = '') and (OrgnlDbtrAgtFinInstIdOthrId = '') then
    Result.Append(EMPTY_AMDMNT_INF_DTLS);

  // check for invalid fields

  if not SEPACheckString(OrgnlMndtId, MNDT_ID_MAX_LEN) then
    Result.Append(Format(INVALID_ORGNL_MNDT_ID, [OrgnlMndtId]));

  if not SEPACheckString(OrgnlCdtrSchmeIdNm, CDTR_NM_MAX_LEN) then
    Result.Append(Format(INVALID_ORGNL_CRDTR_NM, [OrgnlCdtrSchmeIdNm]));

  if (OrgnlCdtrSchmeIdIdPrvtIdOthrId <> '') and not SEPACheckCleanCI(OrgnlCdtrSchmeIdIdPrvtIdOthrId) then
    Result.Append(Format(INVALID_ORGNL_CRDTR_ID, [OrgnlCdtrSchmeIdIdPrvtIdOthrId]));

  if OrgnlCdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry <> SEPA then
    Result.Append(Format(INVALID_ORGNL_CRDTR_PRTRY, [OrgnlCdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry]));

  if (OrgnlDbtrAgtFinInstIdOthrId <> '') and (OrgnlDbtrAgtFinInstIdOthrId <> ORGNL_DBTR_AGT_SMNDA) then
    Result.Append(Format(INVALID_ORGNL_FIN_INST_ID, [OrgnlDbtrAgtFinInstIdOthrId]));

  // delegate validations where possible

  if (OrgnlDbtrAcct.IBAN <> '') then
    Result.AddStrings(OrgnlDbtrAcct.Validate(schema));
end;

procedure TAmendmentInformationDetails.SaveToStream(const stream: TStream; const schema: String);
begin
  WriteLine(stream, '<AmdmntInfDtls>');

  if OrgnlMndtId <> '' then
    WriteLine(stream, '<OrgnlMndtId>'+SEPACleanString(OrgnlMndtId)+'</OrgnlMndtId>');

  if (OrgnlCdtrSchmeIdNm <> '') or (OrgnlCdtrSchmeIdIdPrvtIdOthrId <> '') then
  begin
    WriteLine(stream, '<OrgnlCdtrSchmeId>');
    if OrgnlCdtrSchmeIdNm <> '' then
      WriteLine(stream, '<Nm>'+SEPACleanString(OrgnlCdtrSchmeIdNm, CDTR_NM_MAX_LEN)+'</Nm>');
    if OrgnlCdtrSchmeIdIdPrvtIdOthrId <> '' then
      WriteLine(stream, '<Id><PrvtId><Othr>'+
                          '<Id>'+SEPACleanString(OrgnlCdtrSchmeIdIdPrvtIdOthrId)+'</Id>'+
                          '<SchmeNm><Prtry>'+SEPACleanString(OrgnlCdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry)+'</Prtry></SchmeNm>'+
                        '</Othr></PrvtId></Id>');
    WriteLine(stream, '</OrgnlCdtrSchmeId>');
  end;

  if OrgnlDbtrAcct.IBAN <> '' then
  begin
    WriteLine(stream, '<OrgnlDbtrAcct>');
    OrgnlDbtrAcct.SaveToStream(stream, schema);
    WriteLine(stream, '</OrgnlDbtrAcct>');
  end;

  if OrgnlDbtrAgtFinInstIdOthrId <> '' then
    WriteLine(stream, '<OrgnlDbtrAgt><FinInstnId><Othr><Id>'+SEPACleanString(OrgnlDbtrAgtFinInstIdOthrId)+'</Id></Othr></FinInstnId></OrgnlDbtrAgt>');

  WriteLine(stream, '</AmdmntInfDtls>');
end;

// TMandateRelatedInformation

constructor TMandateRelatedInformation.Create;
begin
  inherited Create;
  fAmdmntInfDtls := TAmendmentInformationDetails.Create;
end;

function TMandateRelatedInformation.Validate(const schema: String): TStringList;
begin
  Result := TStringList.Create;

  // check for empty fields

  if MndtId = '' then
    Result.Append(EMPTY_MNDT_ID);

  if Trunc(DtOfSgntr) = 0 then
    Result.Append(EMPTY_DT_OF_SGNTR);

  // check for invalid fields

  if not SEPACheckString(MndtId, MNDT_ID_MAX_LEN) then
    Result.Append(Format(INVALID_MNDT_ID, [MndtId]));

  if Trunc(DtOfSgntr) > Trunc(Today) then
    Result.Append(Format(INVALID_DT_OF_SGNTR, [DateToStr(DtOfSgntr)]));

  // delegate validations where possible

  if AmdmntInd then
    Result.AddStrings(AmdmntInfDtls.Validate(schema));
end;

procedure TMandateRelatedInformation.SaveToStream(const stream: TStream; const schema: String);
begin
  WriteLine(stream, '<MndtRltdInf>');
  WriteLine(stream, '<MndtId>'+SEPACleanString(MndtId, MNDT_ID_MAX_LEN)+'</MndtId>');
  WriteLine(stream, '<DtOfSgntr>'+SEPAFormatDate(DtOfSgntr)+'</DtOfSgntr>');
  WriteLine(stream, '<AmdmntInd>'+SEPAFormatBoolean(AmdmntInd)+'</AmdmntInd>');
  if AmdmntInd then
    AmdmntInfDtls.SaveToStream(stream, schema);
  WriteLine(stream, '</MndtRltdInf>');
end;

// TDirectDebitTransactionInformation

constructor TDirectDebitTransactionInformation.Create;
begin
  inherited Create;
  fPmtIdEndToEndId      := END_TO_END_ID_NOTPROVIDED;
  fInstdAmtCcy          := CCY_EUR;
  fDrctDbtTxMndtRltdInf := TMandateRelatedInformation.Create;
  fDbtrAgt              := TFinancialInstitution.Create;
  fDbtrAcct             := TAccountIdentification.Create;
end;

procedure TDirectDebitTransactionInformation.SetDbtrNm(const str: String);
begin
  fDbtrNm := SEPACleanString(str);
end;

procedure TDirectDebitTransactionInformation.SetUltmtDbtrNm(const str: String);
begin
  fUltmtDbtrNm := SEPACleanString(str);
end;

procedure TDirectDebitTransactionInformation.SetRmtInfUstrd(const str: String);
begin
  fRmtInfUstrd := SEPACleanString(str);
end;

function TDirectDebitTransactionInformation.Validate(const schema: String): TStringList;
begin
  Result := TStringList.Create;

  // check for empty fields

  if PmtIdEndToEndId = '' then
    Result.Append(EMPTY_END_TO_END_ID);

  if InstdAmtCcy = '' then
    Result.Append(EMPTY_INSTD_AMT_CCY);

  if DbtrNm = '' then
    Result.Append(EMPTY_DBTR_NM);

  if RmtInfUstrd = '' then
    Result.Append(EMPTY_RMT_INF_USTRD);

  // check for invalid fields

  if not SEPACheckString(PmtIdEndToEndId, END_TO_END_ID_MAX_LEN) then
    Result.Append(Format(INVALID_END_TO_END_ID, [PmtIdEndToEndId]));

  if InstdAmt <= 0.0 then
    Result.Append(Format(INVALID_INSTD_AMT, [SEPAFormatAmount(InstdAmt)]));

  if not SEPACheckString(DbtrNm, DBTR_NM_MAX_LEN) then
    Result.Append(Format(INVALID_DBTR_NM, [DbtrNm]));

  if not SEPACheckString(UltmtDbtrNm, DBTR_NM_MAX_LEN) then
    Result.Append(Format(INVALID_ULTMT_DBTR_NM, [UltmtDbtrNm]));

  if not SEPACheckString(RmtInfUstrd, RMT_INF_USTRD_MAX_LEN) then
    Result.Append(Format(INVALID_RMT_INF_USTRD, [RmtInfUstrd]));

  // delegate validations where possible

  Result.AddStrings(DbtrAgt.Validate(schema));
  Result.AddStrings(DbtrAcct.Validate(schema));
  Result.AddStrings(DrctDbtTxMndtRltdInf.Validate(schema));

  // plausibility checks

  if (DbtrAgt.OthrID = FIN_INSTN_NOTPROVIDED) and not SEPAIsGermanIBAN(DbtrAcct.IBAN) then
      Result.Append(INVALID_IBAN_NOT_DE);
end;

procedure TDirectDebitTransactionInformation.SaveToStream(const stream: TStream; const schema: String);
begin
  WriteLine(stream, '<DrctDbtTxInf>');

  WriteLine(stream, '<PmtId><EndToEndId>'+SEPACleanString(PmtIdEndToEndId)+'</EndToEndId></PmtId>');
  WriteLine(stream, '<InstdAmt Ccy="'+SEPACleanString(InstdAmtCcy)+'">'+SEPAFormatAmount(InstdAmt)+'</InstdAmt>');

  WriteLine(stream, '<DrctDbtTx>');
  DrctDbtTxMndtRltdInf.SaveToStream(stream, schema);
  WriteLine(stream, '</DrctDbtTx>');

  WriteLine(stream, '<DbtrAgt>');
  DbtrAgt.SaveToStream(stream, schema);
  WriteLine(stream, '</DbtrAgt>');

  WriteLine(stream, '<Dbtr><Nm>'+SEPACleanString(DbtrNm, DBTR_NM_MAX_LEN)+'</Nm></Dbtr>');

  WriteLine(stream, '<DbtrAcct>');
  DbtrAcct.SaveToStream(stream, schema);
  WriteLine(stream, '</DbtrAcct>');

  if UltmtDbtrNm <> '' then
    WriteLine(stream, '<UltmtDbtr><Nm>'+SEPACleanString(UltmtDbtrNm, DBTR_NM_MAX_LEN)+'</Nm></UltmtDbtr>');
  
  WriteLine(stream, '<RmtInf><Ustrd>'+SEPACleanString(RmtInfUstrd, RMT_INF_USTRD_MAX_LEN)+'</Ustrd></RmtInf>');

  WriteLine(stream, '</DrctDbtTxInf>');
end;

// TPaymentInstructionInformation

constructor TPaymentInstructionInformation.Create;
begin
  inherited Create;
  fPmtInfId                            := GetUUID();
  fPmtMtd                              := PMT_MTD_DIRECT_DEBIT;
  fPmtTpInfSvcLvlCd                    := SEPA;
  fChrgBr                              := CHRG_BR_SLEV;
  fCdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry := SEPA;
  fCdtrAcct                            := TAccountIdentification.Create;
  fCdtrAgt                             := TFinancialInstitution.Create;
end;

procedure TPaymentInstructionInformation.SetCdtrNm(const str: String);
begin
  fCdtrNm := SEPACleanString(str);
end;

procedure TPaymentInstructionInformation.SetCdtrSchmeIdIdPrvtIdOthrId(const str: String);
begin
  fCdtrSchmeIdIdPrvtIdOthrId := SEPACleanIBANorBICorCI(str);
end;

function TPaymentInstructionInformation.GetCtrlSum: Double;
var
  i: Integer;
begin
  Result := 0.0;
  for i := 0 to DrctDbtTxInfCount-1 do
    Result := Result + SEPARoundAmount(DrctDbtTxInfEntry[i].InstdAmt);
end;

procedure TPaymentInstructionInformation.AppendDrctDbtTxInfEntry(const transaction: TDirectDebitTransactionInformation);
var
  i: Integer;
begin
  i := Length(fDrctDbtTxInf);
  SetLength(fDrctDbtTxInf, i+1);
  fDrctDbtTxInf[i] := transaction;
end;

function TPaymentInstructionInformation.GetDrctDbtTxInfEntry(const i: Integer): TDirectDebitTransactionInformation;
begin
  Result := fDrctDbtTxInf[i];
end;

function TPaymentInstructionInformation.GetDrctDbtTxInfCount: Integer;
begin
  Result := Length(fDrctDbtTxInf);
end;

function TPaymentInstructionInformation.Validate(const schema: String): TStringList;
var
  possible_reqd_colltn_dt: Cardinal;
  add_days,i: Integer;
begin
  Result := TStringList.Create;

  // check for empty fields

  if PmtInfId = '' then
    Result.Append(EMPTY_PMT_INF_ID);

  if CdtrNm = '' then
    Result.Append(EMPTY_CDTR_NM);

  if CdtrSchmeIdIdPrvtIdOthrId = '' then
    Result.Append(EMPTY_CDTR_ID);

  // check for invalid fields

  if not SEPACheckString(PmtInfId, ID_MAX_LEN) then
    Result.Append(Format(INVALID_PMT_INF_ID, [PmtInfId]));

  if PmtMtd <> PMT_MTD_DIRECT_DEBIT then
    Result.Append(Format(INVALID_PMT_MTD, [PmtMtd]));

  if (PmtTpInfLclInstrmCd <> LCL_INSTRM_CD_CORE) and
     (PmtTpInfLclInstrmCd <> LCL_INSTRM_CD_COR1) and
     (PmtTpInfLclInstrmCd <> LCL_INSTRM_CD_B2B) then
    Result.Append(Format(INVALID_LCL_INSTRM_CD, [PmtTpInfLclInstrmCd]));

  if (PmtTpInfLclInstrmCd = LCL_INSTRM_CD_COR1) and (schema <> SCHEMA_PAIN_008_003_02) then
    Result.Append(INVALID_LCL_INSTRM_CD_COR1);

  if (PmtTpInfSeqTp <> SEQ_TP_FRST) and
     (PmtTpInfSeqTp <> SEQ_TP_RCUR) and
     (PmtTpInfSeqTp <> SEQ_TP_OOFF) and
     (PmtTpInfSeqTp <> SEQ_TP_FNAL) then
    Result.Append(Format(INVALID_SEQ_TP, [PmtTpInfSeqTp]));

  // compute earliest possible date for collection (not precise: e.g. no holidays; always ask your bank for deadlines)
  possible_reqd_colltn_dt := Trunc(Today);
  if PmtTpInfLclInstrmCd = LCL_INSTRM_CD_CORE then
  begin
    if (PmtTpInfSeqTp = SEQ_TP_FRST) or (PmtTpInfSeqTp = SEQ_TP_OOFF) then
      add_days := 5
    else
      add_days := 2;
  end
  else
    add_days := 1;
  for i := 1 to add_days do
  begin
    Inc(possible_reqd_colltn_dt);
    while DayOfTheWeek(possible_reqd_colltn_dt) > 5 do
      Inc(possible_reqd_colltn_dt);
  end;
  if Trunc(ReqdColltnDt) < possible_reqd_colltn_dt then
    Result.Append(Format(INVALID_REQD_COLLTN_DT, [DateToStr(ReqdColltnDt)]));

  if PmtTpInfSvcLvlCd <> SEPA then
    Result.Append(Format(INVALID_SVC_LVL_CD, [PmtTpInfSvcLvlCd]));

  if ChrgBr <> CHRG_BR_SLEV then
    Result.Append(Format(INVALID_CHRG_BR, [ChrgBr]));

  if not SEPACheckString(CdtrNm, CDTR_NM_MAX_LEN) then
    Result.Append(Format(INVALID_CDTR_NM, [CdtrNm]));

  if not SEPACheckCleanCI(CdtrSchmeIdIdPrvtIdOthrId) then
    Result.Append(Format(INVALID_CDTR_ID, [CdtrSchmeIdIdPrvtIdOthrId]));

  if CdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry <> SEPA then
    Result.Append(Format(INVALID_CDTR_PRTRY, [CdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry]));

  // delegate validations where possible

  Result.AddStrings(CdtrAcct.Validate(schema));
  Result.AddStrings(CdtrAgt.Validate(schema));

  for i := 0 to DrctDbtTxInfCount-1 do
    Result.AddStrings(DrctDbtTxInfEntry[i].Validate(schema));

  // plausibility checks

  if not SEPAIsGermanIBAN(CdtrAcct.IBAN) then
    Result.Append(INVALID_CDTR_ACCT_NOT_DE);

  if PmtTpInfSeqTp = SEQ_TP_FRST then
  begin
    for i := 0 to DrctDbtTxInfCount-1 do
    begin
      if DrctDbtTxInfEntry[i].DrctDbtTxMndtRltdInf.AmdmntInd then
      begin
        if DrctDbtTxInfEntry[i].DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlDbtrAgtFinInstIdOthrId <> ORGNL_DBTR_AGT_SMNDA then
          Result.Append(INVALID_SEQ_TP_FRST_SMNDA1);
      end;
    end;
  end
  else
  begin
    for i := 0 to DrctDbtTxInfCount-1 do
    begin
      if DrctDbtTxInfEntry[i].DrctDbtTxMndtRltdInf.AmdmntInd then
      begin
        if DrctDbtTxInfEntry[i].DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlDbtrAgtFinInstIdOthrId = ORGNL_DBTR_AGT_SMNDA then
          Result.Append(INVALID_SEQ_TP_FRST_SMNDA2);
      end;
    end;
  end;

  // note: number of objects in DrctDbtTxInf is not checked - if empty, then this
  // object will be ignored by TDirectDebitInitiation; and TDirectDebitInitiation
  // ensures in its validation that it has some transactions
end;

procedure TPaymentInstructionInformation.SaveToStream(const stream: TStream; const schema: String);
var
  i: Integer;
begin
  WriteLine(stream, '<PmtInf>');

  WriteLine(stream, '<PmtInfId>'+SEPACleanString(PmtInfId)+'</PmtInfId>');
  WriteLine(stream, '<PmtMtd>'+SEPACleanString(PmtMtd)+'</PmtMtd>');
  WriteLine(stream, '<CtrlSum>'+SEPAFormatAmount(CtrlSum)+'</CtrlSum>');

  WriteLine(stream, '<PmtTpInf>');
  WriteLine(stream, '<SvcLvl><Cd>'+SEPACleanString(PmtTpInfSvcLvlCd)+'</Cd></SvcLvl>');
  WriteLine(stream, '<LclInstrm><Cd>'+SEPACleanString(PmtTpInfLclInstrmCd)+'</Cd></LclInstrm>');
  WriteLine(stream, '<SeqTp>'+SEPACleanString(fPmtTpInfSeqTp)+'</SeqTp>');
  WriteLine(stream, '</PmtTpInf>');
  
  WriteLine(stream, '<ReqdColltnDt>'+SEPAFormatDate(ReqdColltnDt)+'</ReqdColltnDt>');
  WriteLine(stream, '<Cdtr><Nm>'+SEPACleanString(CdtrNm, CDTR_NM_MAX_LEN)+'</Nm></Cdtr>');

  WriteLine(stream, '<CdtrAcct>');
  CdtrAcct.SaveToStream(stream, schema);
  WriteLine(stream, '</CdtrAcct>');

  WriteLine(stream, '<CdtrAgt>');
  CdtrAgt.SaveToStream(stream, schema);
  WriteLine(stream, '</CdtrAgt>');

  WriteLine(stream, '<ChrgBr>'+SEPACleanString(ChrgBr)+'</ChrgBr>');

  WriteLine(stream, '<CdtrSchmeId><Id><PrvtId><Othr>');
  WriteLine(stream, '<Id>'+SEPACleanString(CdtrSchmeIdIdPrvtIdOthrId)+'</Id>');
  WriteLine(stream, '<SchmeNm><Prtry>'+SEPACleanString(CdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry)+'</Prtry></SchmeNm>');
  WriteLine(stream, '</Othr></PrvtId></Id></CdtrSchmeId>');

  for i := 0 to DrctDbtTxInfCount-1 do
    DrctDbtTxInfEntry[i].SaveToStream(stream, schema);

  WriteLine(stream, '</PmtInf>');
end;

// TDirectDebitInitiation

constructor TDirectDebitInitiation.Create;
begin
  inherited Create;
  fSchema        := ''; // empty = auto-select
  fGrpHdrMsgId   := GetUUID();
  fGrpHdrCreDtTm := Now;
end;

function TDirectDebitInitiation.GetSchema: String;
var
  i,j: Integer;
  b: Boolean;
begin
  Result := fSchema;

  // Default schema:
  // - always choose pain.008.003.02 after February 1st, 2014
  //   (it is valid since November 4th, 2013 - but it seems several banks don't
  //    accept this new format for now...)
  // - if COR1 or IBAN-only is used, then we will switch to pain.008.003.02
  // - otherwise, always use pain.008.002.02

  if (Result = '') and (Now > EncodeDate(2014, 2, 1)) then
    Result := SCHEMA_PAIN_008_003_02;

  if Result = '' then
  begin
    // detect usage of COR1 or IBAN-only
    b := false;
    for i := 0 to PmtInfCount-1 do
    begin
      if (PmtInfEntry[i].PmtTpInfLclInstrmCd = LCL_INSTRM_CD_COR1) or
         (PmtInfEntry[i].CdtrAgt.OthrID = FIN_INSTN_NOTPROVIDED) then
      begin
        b := true;
        Break;
      end;
      for j := 0 to PmtInfEntry[i].DrctDbtTxInfCount-1 do
      begin
        if PmtInfEntry[i].DrctDbtTxInfEntry[j].DbtrAgt.OthrID = FIN_INSTN_NOTPROVIDED then
        begin
          b := true;
          Break;
        end;
      end;
      if b then
        Break;
    end;
    if b then
      Result := SCHEMA_PAIN_008_003_02;
  end;

  if Result = '' then
    Result := SCHEMA_PAIN_008_002_02;
end;

procedure TDirectDebitInitiation.SetGrpHdrInitgPtyName(const str: String);
begin
  fGrpHdrInitgPtyName := SEPACleanString(str);
end;

function TDirectDebitInitiation.GetGrpHdrNbOfTxs: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to PmtInfCount-1 do
    Inc(Result, PmtInfEntry[i].NbOfTxs);
end;

procedure TDirectDebitInitiation.AppendPmtInfEntry(const instruction: TPaymentInstructionInformation);
var
  i: Integer;
begin
  i := Length(fPmtInf);
  SetLength(fPmtInf, i+1);
  fPmtInf[i] := instruction;
end;

function TDirectDebitInitiation.GetPmtInfEntry(const i: Integer): TPaymentInstructionInformation;
begin
  Result := fPmtInf[i];
end;

function TDirectDebitInitiation.GetPmtInfCount: Integer;
begin
  Result := Length(fPmtInf);   
end;

function TDirectDebitInitiation.Validate: TStringList;
var
  FirstPmtTpInfLclInstrmCd: String;
  i: Integer;
begin
  Result := TStringList.Create;

  // check ISO schema

  if (Schema <> SCHEMA_PAIN_008_002_02) and (Schema <> SCHEMA_PAIN_008_003_02) then
    Result.Append(Format(UNKNOWN_SCHEMA, [Schema]));

  // check for empty fields

  if GrpHdrMsgId = '' then
    Result.Append(EMPTY_GRP_HDR_MSG_ID);

  if GrpHdrInitgPtyName = '' then
    Result.Append(EMPTY_INITG_PTY_NAME);

  // check for invalid fields

  if not SEPACheckString(GrpHdrMsgId, ID_MAX_LEN) then
    Result.Append(Format(INVALID_GRP_HDR_MSG_ID, [GrpHdrMsgId]));

  if not SEPACheckString(GrpHdrInitgPtyName, INITG_PTY_NAME_MAX_LEN) then
    Result.Append(Format(INVALID_INITG_PTY_NAME, [GrpHdrInitgPtyName]));

  // delegate validations where possible

  for i := 0 to PmtInfCount-1 do
    Result.AddStrings(PmtInfEntry[i].Validate(Schema));

  // plausibility checks

  if GrpHdrNbOfTxs = 0 then
    Result.Append(INVALID_NB_OF_TXS);

  if PmtInfCount > 0 then
  begin
    FirstPmtTpInfLclInstrmCd := PmtInfEntry[0].PmtTpInfLclInstrmCd;
    for i := 1 to PmtInfCount-1 do
    begin
      if (PmtInfEntry[i].PmtTpInfLclInstrmCd <> FirstPmtTpInfLclInstrmCd) then
      begin
        Result.Append(INVALID_PMT_INF_MIXING);
        Break;
      end;
    end;
  end;
end;

procedure TDirectDebitInitiation.SaveToStream(const stream: TStream);
var
  i: Integer;
begin
  WriteLine(stream, '<?xml version="1.0" encoding="UTF-8"?>');
  WriteLine(stream, '<Document xmlns="urn:iso:std:iso:20022:tech:xsd:'+Schema+'"'+
                    ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'+
                    ' xsi:schemaLocation="urn:iso:std:iso:20022:tech:xsd:'+Schema+' '+Schema+'.xsd">');
  WriteLine(stream, '<CstmrDrctDbtInitn>');

  WriteLine(stream, '<GrpHdr>');
  WriteLine(stream, '<MsgId>'+SEPACleanString(GrpHdrMsgId)+'</MsgId>');
  WriteLine(stream, '<CreDtTm>'+SEPAFormatDateTime(GrpHdrCreDtTm)+'</CreDtTm>');
  WriteLine(stream, '<NbOfTxs>'+IntToStr(GrpHdrNbOfTxs)+'</NbOfTxs>');
  WriteLine(stream, '<InitgPty><Nm>'+SEPACleanString(GrpHdrInitgPtyName, INITG_PTY_NAME_MAX_LEN)+'</Nm></InitgPty>');
  WriteLine(stream, '</GrpHdr>');

  for i := 0 to PmtInfCount-1 do
    if PmtInfEntry[i].NbOfTxs > 0 then
      PmtInfEntry[i].SaveToStream(stream, Schema);

  WriteLine(stream, '</CstmrDrctDbtInitn>');
  WriteLine(stream, '</Document>');
end;

procedure TDirectDebitInitiation.SaveToDisk(const FileName: String);
var
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  try
    SaveToStream(stream);
    stream.SaveToFile(FileName);
  finally
    stream.Free;
  end;
end;

end.
