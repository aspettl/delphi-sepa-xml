//
//   Delphi unit for SEPA direct debit XML file creation
//   (beta version 0.3.0, 2017-10-01)
//
//   Copyright (C) 2013-2017 by Aaron Spettl
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
//   E-mail:  aaron@spettl.de
//
unit SEPADirectDebit;

{$IFDEF FPC}                // Lazarus: set compiler mode and file encoding
{%encoding CP1252}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, DateUtils, SEPACommon;

type
  // In the following, all necessary classes to create direct debit transactions
  // for SEPA XML files are introduced. Please have a look at the specification
  // of the XML data format at
  //   http://www.ebics.de/index.php?id=77
  // (section 2.2.2, "Anlage3_Datenformate_V2.7.pdf" by EBICS, Die Deutsche Kreditwirtschaft).
  //
  // Short explanation of XML file for direct debit transactions:
  //
  // XML tags                      corresponding class or interface
  // ---------------------------------------------------------------------------
  // <Document>                    TDirectDebitInitiation
  //   <CstmrDrctDbtInitn>         TDirectDebitInitiation
  //     <PmtInf>                  TDirectDebitPaymentInformation
  //       <DrctDbtTxInf>          TDirectDebitTransactionInformation
  //         <MndtRltdInf>         TMandateRelatedInformation
  //           <AmdmntInfDtls>     IAmendmentInformationDetails
  //       <DrctDbtTxInf>          ...
  //         ...
  //     <PmtInf>
  //       ...
  //
  // Note that all strings in these units are interpreted with respect to the
  // default behavior of the development environment, i.e.,
  // a) for Delphi < 2009:        ANSI strings
  // b) for Delphi >= 2009:       Unicode strings
  // c) for Lazarus with FPC 2.6: no encoding specified, ANSI is assumed
  // d) for Lazarus with FPC 3.0: codepage-aware strings

  IAmendmentInformationDetails = interface
    function Validate(const schema: String; const PmtTpInfSeqTp: String; const appendTo: TStringList = nil): TStringList;
    procedure SaveToStream(const stream: TStream; const schema: String);
  end;

  TAmendmentInformationDetails26 = class(TInterfacedObject, IAmendmentInformationDetails)
  private
    fOrgnlMndtId: String;                              // original mandate identification
    fOrgnlCdtrSchmeIdNm: String;                       // original creditor name
    fOrgnlCdtrSchmeIdIdPrvtIdOthrId: String;           // original creditor identifier
    fOrgnlDbtrAcct: TAccountIdentification;            // original debtor account identification
    fOrgnlDbtrAgtFinInstIdOthrId: String;              // "SMNDA" if same mandate + new debtor agent

    procedure SetOrgnlCdtrSchmeIdIdPrvtIdOthrId(const str: String);
  public
    constructor Create;
    destructor Destroy; override;

    property OrgnlMndtId: String read fOrgnlMndtId write fOrgnlMndtId;
    property OrgnlCdtrSchmeIdNm: String read fOrgnlCdtrSchmeIdNm write fOrgnlCdtrSchmeIdNm;
    property OrgnlCdtrSchmeIdIdPrvtIdOthrId: String read fOrgnlCdtrSchmeIdIdPrvtIdOthrId write SetOrgnlCdtrSchmeIdIdPrvtIdOthrId;
    property OrgnlDbtrAcct: TAccountIdentification read fOrgnlDbtrAcct;
    property OrgnlDbtrAgtFinInstIdOthrId: String read fOrgnlDbtrAgtFinInstIdOthrId write fOrgnlDbtrAgtFinInstIdOthrId;

    function Validate(const schema: String; const PmtTpInfSeqTp: String; const appendTo: TStringList = nil): TStringList;
    procedure SaveToStream(const stream: TStream; const schema: String);
  end;

  TAmendmentInformationDetails30 = class(TInterfacedObject, IAmendmentInformationDetails)
  private
    fOrgnlMndtId: String;                              // original mandate identification
    fOrgnlCdtrSchmeIdNm: String;                       // original creditor name
    fOrgnlCdtrSchmeIdIdPrvtIdOthrId: String;           // original creditor identifier
    fOrgnlDbtrAcct: String;                            // original debtor account: "SMNDA" if same mandate + new debtor account
                                                       // original debtor agent: not provided as it is always optional with "SMNDA"

    procedure SetOrgnlCdtrSchmeIdIdPrvtIdOthrId(const str: String);
  public
    property OrgnlMndtId: String read fOrgnlMndtId write fOrgnlMndtId;
    property OrgnlCdtrSchmeIdNm: String read fOrgnlCdtrSchmeIdNm write fOrgnlCdtrSchmeIdNm;
    property OrgnlCdtrSchmeIdIdPrvtIdOthrId: String read fOrgnlCdtrSchmeIdIdPrvtIdOthrId write SetOrgnlCdtrSchmeIdIdPrvtIdOthrId;
    property OrgnlDbtrAcct: String read fOrgnlDbtrAcct write fOrgnlDbtrAcct;

    function Validate(const schema: String; const PmtTpInfSeqTp: String; const appendTo: TStringList = nil): TStringList;
    procedure SaveToStream(const stream: TStream; const schema: String); 
  end;

  TMandateRelatedInformation = class
  private
    fMndtId: String;                                   // mandate identification
    fDtOfSgntr: TDateTime;                             // date of signature
    fAmdmntInd: Boolean;                               // amendment indicator ("false" or "true")
    fAmdmntInfDtls26: TAmendmentInformationDetails26;
    fAmdmntInfDtls30: TAmendmentInformationDetails30;
  public
    constructor Create;
    destructor Destroy; override;

    property MndtId: String read fMndtId write fMndtId;
    property DtOfSgntr: TDateTime read fDtOfSgntr write fDtOfSgntr;
    property AmdmntInd: Boolean read fAmdmntInd write fAmdmntInd;
    // Note:
    // - Depending on the schema, either amendment information details according to the
    //   2.6-2.9 or 3.0 specification will be used.
    // - We provide no property "AmdmntInfDtls" returning the interface because, then,
    //   reference counting interferes with explicit freeing
    property AmdmntInfDtls26: TAmendmentInformationDetails26 read fAmdmntInfDtls26;
    property AmdmntInfDtls30: TAmendmentInformationDetails30 read fAmdmntInfDtls30;

    function Validate(const schema: String; const PmtTpInfSeqTp: String; const appendTo: TStringList = nil): TStringList;
    procedure SaveToStream(const stream: TStream; const schema: String);
  end;

  TDirectDebitTransactionInformation = class
  private
    fPmtIdEndToEndId: String;                          // end-to-end identification of this payment (by default "NOTPROVIDED")
    fInstdAmtCcy: String;                              // instructed amount, currency (always "EUR")
    fInstdAmt: Currency;                               // instructed amount
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
    destructor Destroy; override;

    property PmtIdEndToEndId: String read fPmtIdEndToEndId write fPmtIdEndToEndId;
    property InstdAmtCcy: String read fInstdAmtCcy write fInstdAmtCcy;
    property InstdAmt: Currency read fInstdAmt write fInstdAmt;
    property DrctDbtTxMndtRltdInf: TMandateRelatedInformation read fDrctDbtTxMndtRltdInf;
    property DbtrAgt: TFinancialInstitution read fDbtrAgt;
    property DbtrNm: String read fDbtrNm write SetDbtrNm;
    property DbtrAcct: TAccountIdentification read fDbtrAcct;
    property UltmtDbtrNm: String read fUltmtDbtrNm write SetUltmtDbtrNm;
    property RmtInfUstrd: String read fRmtInfUstrd write SetRmtInfUstrd;

    function Validate(const schema: String; const PmtTpInfSeqTp: String; const appendTo: TStringList = nil): TStringList;
    procedure SaveToStream(const stream: TStream; const schema: String);
  end;

  TDirectDebitPaymentInformation = class
  private
    fPmtInfId: String;                                 // payment information identification
    fPmtMtd: String;                                   // payment method (always "DD")
    fPmtTpInfSvcLvlCd: String;                         // payment type, service level code (always "SEPA")
    fPmtTpInfLclInstrmCd: String;                      // payment type, local instrument code ("CORE", "COR1" or "B2B")
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

    function GetCtrlSum: Currency;
    function GetDrctDbtTxInfEntry(const i: Integer): TDirectDebitTransactionInformation;
    function GetDrctDbtTxInfCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property PmtInfId: String read fPmtInfId write fPmtInfId;
    property PmtMtd: String read fPmtMtd write fPmtMtd;
    property NbOfTxs: Integer read GetDrctDbtTxInfCount;
    property CtrlSum: Currency read GetCtrlSum;
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

    function Validate(const schema: String; const appendTo: TStringList = nil): TStringList;
    procedure SaveToStream(const stream: TStream; const schema: String);
  end;

  TDirectDebitInitiation = class
  private
    fSchema: String;                                   // ISO schema, e.g. "pain.008.002.02", empty means auto-select based on date and COR1
    fGrpHdrMsgId: String;                              // group header: message identification
    fGrpHdrCreDtTm: TDateTime;                         // group header: time of file creation
    fGrpHdrInitgPtyName: String;                       // group header: initiator name
    fPmtInf: array of TDirectDebitPaymentInformation;

    function GetSchema: String;
    procedure SetGrpHdrInitgPtyName(const str: String);

    function GetGrpHdrNbOfTxs: Integer;
    function GetGrpHdrCtrlSum: Currency;
    function GetPmtInfEntry(const i: Integer): TDirectDebitPaymentInformation;
    function GetPmtInfCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property Schema: String read GetSchema write fSchema;

    property GrpHdrMsgId: String read fGrpHdrMsgId write fGrpHdrMsgId;
    property GrpHdrCreDtTm: TDateTime read fGrpHdrCreDtTm write fGrpHdrCreDtTm;
    property GrpHdrNbOfTxs: Integer read GetGrpHdrNbOfTxs; 
    property GrpHdrCtrlSum: Currency read GetGrpHdrCtrlSum;
    property GrpHdrInitgPtyName: String read fGrpHdrInitgPtyName write SetGrpHdrInitgPtyName;

    procedure AppendPmtInfEntry(const instruction: TDirectDebitPaymentInformation);
    property PmtInfEntry[const i: Integer]: TDirectDebitPaymentInformation read GetPmtInfEntry;
    property PmtInfCount: Integer read GetPmtInfCount;

    function Validate(const appendTo: TStringList = nil): TStringList;
    procedure SaveToStream(const stream: TStream);
    procedure SaveToDisk(const FileName: String);
  end;

implementation

// TAmendmentInformationDetails26

constructor TAmendmentInformationDetails26.Create;
begin
  inherited;
  fOrgnlDbtrAcct := TAccountIdentification.Create;
end;

destructor TAmendmentInformationDetails26.Destroy;
begin
  FreeAndNil(fOrgnlDbtrAcct);
  inherited;
end;

procedure TAmendmentInformationDetails26.SetOrgnlCdtrSchmeIdIdPrvtIdOthrId(const str: String);
begin
  fOrgnlCdtrSchmeIdIdPrvtIdOthrId := SEPACleanIBANorBICorCI(str);
end;

function TAmendmentInformationDetails26.Validate(const schema: String; const PmtTpInfSeqTp: String; const appendTo: TStringList = nil): TStringList;
var
  empty: Boolean;
begin
  if appendTo <> nil then
    Result := appendTo
  else
    Result := TStringList.Create;

  // check for empty fields

  empty := (OrgnlMndtId = '') and (OrgnlCdtrSchmeIdNm = '') and (OrgnlCdtrSchmeIdIdPrvtIdOthrId = '') and
           (OrgnlDbtrAcct.IBAN = '') and (OrgnlDbtrAgtFinInstIdOthrId = '');
  if empty then
    Result.Append(EMPTY_AMDMNT_INF_DTLS);

  // check schema

  if (schema <> SCHEMA_PAIN_008_002_02) and (schema <> SCHEMA_PAIN_008_003_02) then
    Result.Append(Format(SCHEMA_AMDMNT_INF_DTLS_26, [schema]));

  // check for invalid fields

  if not SEPACheckString(OrgnlMndtId, MNDT_ID_MAX_LEN) then
    Result.Append(Format(INVALID_ORGNL_MNDT_ID, [OrgnlMndtId]));

  if not SEPACheckString(OrgnlCdtrSchmeIdNm, CDTR_NM_MAX_LEN) then
    Result.Append(Format(INVALID_ORGNL_CRDTR_NM, [OrgnlCdtrSchmeIdNm]));

  if (OrgnlCdtrSchmeIdIdPrvtIdOthrId <> '') and not SEPACheckCI(OrgnlCdtrSchmeIdIdPrvtIdOthrId) then
    Result.Append(Format(INVALID_ORGNL_CRDTR_ID, [OrgnlCdtrSchmeIdIdPrvtIdOthrId]));

  if (OrgnlDbtrAgtFinInstIdOthrId <> '') and (OrgnlDbtrAgtFinInstIdOthrId <> ORGNL_DBTR_AGT_SMNDA) then
    Result.Append(Format(INVALID_ORGNL_FIN_INST_ID, [OrgnlDbtrAgtFinInstIdOthrId]));

  if not empty and (PmtTpInfSeqTp = SEQ_TP_FRST) and (OrgnlDbtrAgtFinInstIdOthrId <> ORGNL_DBTR_AGT_SMNDA) then
    Result.Append(INVALID_SEQ_TP_FRST_SMNDA1);
  if (PmtTpInfSeqTp <> SEQ_TP_FRST) and (OrgnlDbtrAgtFinInstIdOthrId = ORGNL_DBTR_AGT_SMNDA) then
    Result.Append(INVALID_SEQ_TP_FRST_SMNDA2);

  // delegate validations where possible

  if (OrgnlDbtrAcct.IBAN <> '') then
    OrgnlDbtrAcct.Validate(schema, Result);
end;

procedure TAmendmentInformationDetails26.SaveToStream(const stream: TStream; const schema: String);
begin
  SEPAWriteLine(stream, '<AmdmntInfDtls>');

  if OrgnlMndtId <> '' then
    SEPAWriteLine(stream, '<OrgnlMndtId>'+SEPACleanString(OrgnlMndtId)+'</OrgnlMndtId>');

  if (OrgnlCdtrSchmeIdNm <> '') or (OrgnlCdtrSchmeIdIdPrvtIdOthrId <> '') then
  begin
    SEPAWriteLine(stream, '<OrgnlCdtrSchmeId>');
    if OrgnlCdtrSchmeIdNm <> '' then
      SEPAWriteLine(stream, '<Nm>'+SEPACleanString(OrgnlCdtrSchmeIdNm, CDTR_NM_MAX_LEN)+'</Nm>');
    if OrgnlCdtrSchmeIdIdPrvtIdOthrId <> '' then
      SEPAWriteLine(stream, '<Id><PrvtId><Othr>'+
                              '<Id>'+SEPACleanString(OrgnlCdtrSchmeIdIdPrvtIdOthrId)+'</Id>'+
                              '<SchmeNm><Prtry>SEPA</Prtry></SchmeNm>'+
                            '</Othr></PrvtId></Id>');
    SEPAWriteLine(stream, '</OrgnlCdtrSchmeId>');
  end;

  if OrgnlDbtrAcct.IBAN <> '' then
  begin
    SEPAWriteLine(stream, '<OrgnlDbtrAcct>');
    OrgnlDbtrAcct.SaveToStream(stream, schema);
    SEPAWriteLine(stream, '</OrgnlDbtrAcct>');
  end;

  if OrgnlDbtrAgtFinInstIdOthrId <> '' then
    SEPAWriteLine(stream, '<OrgnlDbtrAgt><FinInstnId><Othr><Id>'+SEPACleanString(OrgnlDbtrAgtFinInstIdOthrId)+'</Id></Othr></FinInstnId></OrgnlDbtrAgt>');

  SEPAWriteLine(stream, '</AmdmntInfDtls>');
end;

// TAmendmentInformationDetails30

procedure TAmendmentInformationDetails30.SetOrgnlCdtrSchmeIdIdPrvtIdOthrId(const str: String);
begin
  fOrgnlCdtrSchmeIdIdPrvtIdOthrId := SEPACleanIBANorBICorCI(str);
end;

function TAmendmentInformationDetails30.Validate(const schema: String; const PmtTpInfSeqTp: String; const appendTo: TStringList = nil): TStringList;
begin
  if appendTo <> nil then
    Result := appendTo
  else
    Result := TStringList.Create;

  // check for empty fields

  if (OrgnlMndtId = '') and (OrgnlCdtrSchmeIdNm = '') and (OrgnlCdtrSchmeIdIdPrvtIdOthrId = '') and
     (OrgnlDbtrAcct = '') then
    Result.Append(EMPTY_AMDMNT_INF_DTLS);

  // check schema

  if schema <> SCHEMA_PAIN_008_001_02 then
    Result.Append(Format(SCHEMA_AMDMNT_INF_DTLS_30, [schema]));

  // check for invalid fields

  if not SEPACheckString(OrgnlMndtId, MNDT_ID_MAX_LEN) then
    Result.Append(Format(INVALID_ORGNL_MNDT_ID, [OrgnlMndtId]));

  if not SEPACheckString(OrgnlCdtrSchmeIdNm, CDTR_NM_MAX_LEN) then
    Result.Append(Format(INVALID_ORGNL_CRDTR_NM, [OrgnlCdtrSchmeIdNm]));

  if (OrgnlCdtrSchmeIdIdPrvtIdOthrId <> '') and not SEPACheckCI(OrgnlCdtrSchmeIdIdPrvtIdOthrId) then
    Result.Append(Format(INVALID_ORGNL_CRDTR_ID, [OrgnlCdtrSchmeIdIdPrvtIdOthrId]));

  if (OrgnlDbtrAcct <> '') and (OrgnlDbtrAcct <> ORGNL_DBTR_ACCT_SMNDA) then
    Result.Append(Format(INVALID_ORGNL_DBTR_ACCT, [OrgnlDbtrAcct]));
end;

procedure TAmendmentInformationDetails30.SaveToStream(const stream: TStream; const schema: String);
begin
  SEPAWriteLine(stream, '<AmdmntInfDtls>');

  if OrgnlMndtId <> '' then
    SEPAWriteLine(stream, '<OrgnlMndtId>'+SEPACleanString(OrgnlMndtId)+'</OrgnlMndtId>');

  if (OrgnlCdtrSchmeIdNm <> '') or (OrgnlCdtrSchmeIdIdPrvtIdOthrId <> '') then
  begin
    SEPAWriteLine(stream, '<OrgnlCdtrSchmeId>');
    if OrgnlCdtrSchmeIdNm <> '' then
      SEPAWriteLine(stream, '<Nm>'+SEPACleanString(OrgnlCdtrSchmeIdNm, CDTR_NM_MAX_LEN)+'</Nm>');
    if OrgnlCdtrSchmeIdIdPrvtIdOthrId <> '' then
      SEPAWriteLine(stream, '<Id><PrvtId><Othr>'+
                              '<Id>'+SEPACleanString(OrgnlCdtrSchmeIdIdPrvtIdOthrId)+'</Id>'+
                              '<SchmeNm><Prtry>SEPA</Prtry></SchmeNm>'+
                            '</Othr></PrvtId></Id>');
    SEPAWriteLine(stream, '</OrgnlCdtrSchmeId>');
  end;

  if OrgnlDbtrAcct <> '' then
  begin
    SEPAWriteLine(stream, '<OrgnlDbtrAcct>');
    SEPAWriteLine(stream, '<Id><Othr><Id>'+SEPACleanString(OrgnlDbtrAcct)+'</Id></Othr></Id>');
    SEPAWriteLine(stream, '</OrgnlDbtrAcct>');
  end;

  SEPAWriteLine(stream, '</AmdmntInfDtls>');
end;

// TMandateRelatedInformation

constructor TMandateRelatedInformation.Create;
begin
  inherited;
  fAmdmntInfDtls26 := TAmendmentInformationDetails26.Create;  
  fAmdmntInfDtls30 := TAmendmentInformationDetails30.Create;
end;

destructor TMandateRelatedInformation.Destroy;
begin
  FreeAndNil(fAmdmntInfDtls26);
  FreeAndNil(fAmdmntInfDtls30);
  inherited;
end;

function TMandateRelatedInformation.Validate(const schema: String; const PmtTpInfSeqTp: String; const appendTo: TStringList = nil): TStringList;
begin
  if appendTo <> nil then
    Result := appendTo
  else
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
  begin
    if (schema = SCHEMA_PAIN_008_002_02) or (schema = SCHEMA_PAIN_008_003_02) then
      AmdmntInfDtls26.Validate(schema, PmtTpInfSeqTp, Result)
    else
      AmdmntInfDtls30.Validate(schema, PmtTpInfSeqTp, Result);
  end;
end;

procedure TMandateRelatedInformation.SaveToStream(const stream: TStream; const schema: String);
begin
  SEPAWriteLine(stream, '<MndtRltdInf>');
  SEPAWriteLine(stream, '<MndtId>'+SEPACleanString(MndtId, MNDT_ID_MAX_LEN)+'</MndtId>');
  SEPAWriteLine(stream, '<DtOfSgntr>'+SEPAFormatDate(DtOfSgntr)+'</DtOfSgntr>');
  SEPAWriteLine(stream, '<AmdmntInd>'+SEPAFormatBoolean(AmdmntInd)+'</AmdmntInd>');
  if AmdmntInd then
  begin
    if (schema = SCHEMA_PAIN_008_002_02) or (schema = SCHEMA_PAIN_008_003_02) then
      AmdmntInfDtls26.SaveToStream(stream, schema)
    else
      AmdmntInfDtls30.SaveToStream(stream, schema);
  end;
  SEPAWriteLine(stream, '</MndtRltdInf>');
end;

// TDirectDebitTransactionInformation

constructor TDirectDebitTransactionInformation.Create;
begin
  inherited;
  fPmtIdEndToEndId      := END_TO_END_ID_NOTPROVIDED;
  fInstdAmtCcy          := CCY_EUR;
  fDrctDbtTxMndtRltdInf := TMandateRelatedInformation.Create;
  fDbtrAgt              := TFinancialInstitution.Create;
  fDbtrAcct             := TAccountIdentification.Create;
end;

destructor TDirectDebitTransactionInformation.Destroy;
begin
  FreeAndNil(fDrctDbtTxMndtRltdInf);
  FreeAndNil(fDbtrAgt);
  FreeAndNil(fDbtrAcct);
  inherited;
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

function TDirectDebitTransactionInformation.Validate(const schema: String; const PmtTpInfSeqTp: String; const appendTo: TStringList = nil): TStringList;
begin
  if appendTo <> nil then
    Result := appendTo
  else
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

  if (InstdAmt <= 0.0) or not SEPACheckRounded(InstdAmt) then
    Result.Append(Format(INVALID_INSTD_AMT, [SEPAFormatAmount(InstdAmt, 4)]));

  if not SEPACheckString(DbtrNm, DBTR_NM_MAX_LEN) then
    Result.Append(Format(INVALID_DBTR_NM, [DbtrNm]));

  if not SEPACheckString(UltmtDbtrNm, DBTR_NM_MAX_LEN) then
    Result.Append(Format(INVALID_ULTMT_DBTR_NM, [UltmtDbtrNm]));

  if not SEPACheckString(RmtInfUstrd, RMT_INF_USTRD_MAX_LEN) then
    Result.Append(Format(INVALID_RMT_INF_USTRD, [RmtInfUstrd]));

  // delegate validations where possible

  DbtrAgt.Validate(schema, Result);
  DbtrAcct.Validate(schema, Result);
  DrctDbtTxMndtRltdInf.Validate(schema, PmtTpInfSeqTp, Result);

  // plausibility checks

  if (DbtrAgt.OthrID = FIN_INSTN_NOTPROVIDED) and not SEPAIsGermanIBAN(DbtrAcct.IBAN) then
      Result.Append(INVALID_IBAN_NOT_DE);
end;

procedure TDirectDebitTransactionInformation.SaveToStream(const stream: TStream; const schema: String);
begin
  SEPAWriteLine(stream, '<DrctDbtTxInf>');

  SEPAWriteLine(stream, '<PmtId><EndToEndId>'+SEPACleanString(PmtIdEndToEndId)+'</EndToEndId></PmtId>');
  SEPAWriteLine(stream, '<InstdAmt Ccy="'+SEPACleanString(InstdAmtCcy)+'">'+SEPAFormatAmount(InstdAmt)+'</InstdAmt>');

  SEPAWriteLine(stream, '<DrctDbtTx>');
  DrctDbtTxMndtRltdInf.SaveToStream(stream, schema);
  SEPAWriteLine(stream, '</DrctDbtTx>');

  SEPAWriteLine(stream, '<DbtrAgt>');
  DbtrAgt.SaveToStream(stream, schema);
  SEPAWriteLine(stream, '</DbtrAgt>');

  SEPAWriteLine(stream, '<Dbtr><Nm>'+SEPACleanString(DbtrNm, DBTR_NM_MAX_LEN)+'</Nm></Dbtr>');

  SEPAWriteLine(stream, '<DbtrAcct>');
  DbtrAcct.SaveToStream(stream, schema);
  SEPAWriteLine(stream, '</DbtrAcct>');

  if UltmtDbtrNm <> '' then
    SEPAWriteLine(stream, '<UltmtDbtr><Nm>'+SEPACleanString(UltmtDbtrNm, DBTR_NM_MAX_LEN)+'</Nm></UltmtDbtr>');
  
  SEPAWriteLine(stream, '<RmtInf><Ustrd>'+SEPACleanString(RmtInfUstrd, RMT_INF_USTRD_MAX_LEN)+'</Ustrd></RmtInf>');

  SEPAWriteLine(stream, '</DrctDbtTxInf>');
end;

// TDirectDebitPaymentInformation

constructor TDirectDebitPaymentInformation.Create;
begin
  inherited;
  fPmtInfId                            := SEPAGenerateUUID;
  fPmtMtd                              := PMT_MTD_DIRECT_DEBIT;
  fPmtTpInfSvcLvlCd                    := SEPA;
  fChrgBr                              := CHRG_BR_SLEV;
  fCdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry := SEPA;
  fCdtrAcct                            := TAccountIdentification.Create;
  fCdtrAgt                             := TFinancialInstitution.Create;
end;

destructor TDirectDebitPaymentInformation.Destroy;
var
  i: Integer;
begin
  FreeAndNil(fCdtrAcct);
  FreeAndNil(fCdtrAgt);
  for i := Low(fDrctDbtTxInf) to High(fDrctDbtTxInf) do
    FreeAndNil(fDrctDbtTxInf[i]);
  inherited;
end;

procedure TDirectDebitPaymentInformation.SetCdtrNm(const str: String);
begin
  fCdtrNm := SEPACleanString(str);
end;

procedure TDirectDebitPaymentInformation.SetCdtrSchmeIdIdPrvtIdOthrId(const str: String);
begin
  fCdtrSchmeIdIdPrvtIdOthrId := SEPACleanIBANorBICorCI(str);
end;

function TDirectDebitPaymentInformation.GetCtrlSum: Currency;
var
  i: Integer;
begin
  Result := 0.0;
  for i := 0 to DrctDbtTxInfCount-1 do
    Result := Result + DrctDbtTxInfEntry[i].InstdAmt;
end;

procedure TDirectDebitPaymentInformation.AppendDrctDbtTxInfEntry(const transaction: TDirectDebitTransactionInformation);
var
  i: Integer;
begin
  i := Length(fDrctDbtTxInf);
  SetLength(fDrctDbtTxInf, i+1);
  fDrctDbtTxInf[i] := transaction;
end;

function TDirectDebitPaymentInformation.GetDrctDbtTxInfEntry(const i: Integer): TDirectDebitTransactionInformation;
begin
  Result := fDrctDbtTxInf[i];
end;

function TDirectDebitPaymentInformation.GetDrctDbtTxInfCount: Integer;
begin
  Result := Length(fDrctDbtTxInf);
end;

function TDirectDebitPaymentInformation.Validate(const schema: String; const appendTo: TStringList = nil): TStringList;
var
  i: Integer;
begin
  if appendTo <> nil then
    Result := appendTo
  else
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
  begin
    if schema = SCHEMA_PAIN_008_002_02 then
      Result.Append(INVALID_LCL_INSTRM_CD_COR1)
    else
      Result.Append(INVALID_LCL_INSTRM_CD_COR1_TO_CORE);
  end;

  if (PmtTpInfSeqTp <> SEQ_TP_FRST) and
     (PmtTpInfSeqTp <> SEQ_TP_RCUR) and
     (PmtTpInfSeqTp <> SEQ_TP_OOFF) and
     (PmtTpInfSeqTp <> SEQ_TP_FNAL) then
    Result.Append(Format(INVALID_SEQ_TP, [PmtTpInfSeqTp]));

  if Trunc(ReqdColltnDt) < SEPAEarliestCollectionDate(PmtTpInfLclInstrmCd, PmtTpInfSeqTp, schema) then
    Result.Append(Format(INVALID_REQD_COLLTN_DT, [DateToStr(ReqdColltnDt)]));

  if PmtTpInfSvcLvlCd <> SEPA then
    Result.Append(Format(INVALID_SVC_LVL_CD, [PmtTpInfSvcLvlCd]));

  if ChrgBr <> CHRG_BR_SLEV then
    Result.Append(Format(INVALID_CHRG_BR, [ChrgBr]));

  if not SEPACheckString(CdtrNm, CDTR_NM_MAX_LEN) then
    Result.Append(Format(INVALID_CDTR_NM, [CdtrNm]));

  if not SEPACheckCI(CdtrSchmeIdIdPrvtIdOthrId) then
    Result.Append(Format(INVALID_CDTR_ID, [CdtrSchmeIdIdPrvtIdOthrId]));

  if CdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry <> SEPA then
    Result.Append(Format(INVALID_CDTR_PRTRY, [CdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry]));

  // delegate validations where possible

  CdtrAcct.Validate(schema, Result);
  CdtrAgt.Validate(schema, Result);

  for i := 0 to DrctDbtTxInfCount-1 do
    DrctDbtTxInfEntry[i].Validate(schema, PmtTpInfSeqTp, Result);

  // plausibility checks

  if not SEPAIsGermanIBAN(CdtrAcct.IBAN) then
    Result.Append(INVALID_CDTR_ACCT_NOT_DE);

  // note: number of objects in DrctDbtTxInf is not checked - if empty, then this
  // object will be ignored by TDirectDebitInitiation; and TDirectDebitInitiation
  // ensures in its validation that it has some transactions
end;

procedure TDirectDebitPaymentInformation.SaveToStream(const stream: TStream; const schema: String);
var
  i: Integer;
begin
  SEPAWriteLine(stream, '<PmtInf>');

  SEPAWriteLine(stream, '<PmtInfId>'+SEPACleanString(PmtInfId)+'</PmtInfId>');
  SEPAWriteLine(stream, '<PmtMtd>'+SEPACleanString(PmtMtd)+'</PmtMtd>');
  SEPAWriteLine(stream, '<NbOfTxs>'+IntToStr(NbOfTxs)+'</NbOfTxs>');
  SEPAWriteLine(stream, '<CtrlSum>'+SEPAFormatAmount(CtrlSum)+'</CtrlSum>');

  SEPAWriteLine(stream, '<PmtTpInf>');
  SEPAWriteLine(stream, '<SvcLvl><Cd>'+SEPACleanString(PmtTpInfSvcLvlCd)+'</Cd></SvcLvl>');
  SEPAWriteLine(stream, '<LclInstrm><Cd>'+SEPACleanString(PmtTpInfLclInstrmCd)+'</Cd></LclInstrm>');
  SEPAWriteLine(stream, '<SeqTp>'+SEPACleanString(fPmtTpInfSeqTp)+'</SeqTp>');
  SEPAWriteLine(stream, '</PmtTpInf>');
  
  SEPAWriteLine(stream, '<ReqdColltnDt>'+SEPAFormatDate(ReqdColltnDt)+'</ReqdColltnDt>');
  SEPAWriteLine(stream, '<Cdtr><Nm>'+SEPACleanString(CdtrNm, CDTR_NM_MAX_LEN)+'</Nm></Cdtr>');

  SEPAWriteLine(stream, '<CdtrAcct>');
  CdtrAcct.SaveToStream(stream, schema);
  SEPAWriteLine(stream, '</CdtrAcct>');

  SEPAWriteLine(stream, '<CdtrAgt>');
  CdtrAgt.SaveToStream(stream, schema);
  SEPAWriteLine(stream, '</CdtrAgt>');

  SEPAWriteLine(stream, '<ChrgBr>'+SEPACleanString(ChrgBr)+'</ChrgBr>');

  SEPAWriteLine(stream, '<CdtrSchmeId><Id><PrvtId><Othr>');
  SEPAWriteLine(stream, '<Id>'+SEPACleanString(CdtrSchmeIdIdPrvtIdOthrId)+'</Id>');
  SEPAWriteLine(stream, '<SchmeNm><Prtry>'+SEPACleanString(CdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry)+'</Prtry></SchmeNm>');
  SEPAWriteLine(stream, '</Othr></PrvtId></Id></CdtrSchmeId>');

  for i := 0 to DrctDbtTxInfCount-1 do
    DrctDbtTxInfEntry[i].SaveToStream(stream, schema);

  SEPAWriteLine(stream, '</PmtInf>');
end;

// TDirectDebitInitiation

constructor TDirectDebitInitiation.Create;
begin
  inherited;
  fSchema        := ''; // empty = auto-select
  fGrpHdrMsgId   := SEPAGenerateUUID;
  fGrpHdrCreDtTm := Now;
end;

destructor TDirectDebitInitiation.Destroy;
var
  i: Integer;
begin
  for i := Low(fPmtInf) to High(fPmtInf) do
    FreeAndNil(fPmtInf[i]);
  inherited;
end;

function TDirectDebitInitiation.GetSchema: String;
begin
  Result := fSchema;
  if Result = '' then
    Result := SCHEMA_PAIN_008_003_02;
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

function TDirectDebitInitiation.GetGrpHdrCtrlSum: Currency;
var
  i: Integer;
begin
  Result := 0.0;
  for i := 0 to PmtInfCount-1 do
    Result := Result + PmtInfEntry[i].CtrlSum;
end;

procedure TDirectDebitInitiation.AppendPmtInfEntry(const instruction: TDirectDebitPaymentInformation);
var
  i: Integer;
begin
  i := Length(fPmtInf);
  SetLength(fPmtInf, i+1);
  fPmtInf[i] := instruction;
end;

function TDirectDebitInitiation.GetPmtInfEntry(const i: Integer): TDirectDebitPaymentInformation;
begin
  Result := fPmtInf[i];
end;

function TDirectDebitInitiation.GetPmtInfCount: Integer;
begin
  Result := Length(fPmtInf);   
end;

function TDirectDebitInitiation.Validate(const appendTo: TStringList = nil): TStringList;
var
  FirstPmtTpInfLclInstrmCd: String;
  i: Integer;
begin
  if appendTo <> nil then
    Result := appendTo
  else
    Result := TStringList.Create;

  // check schema

  if (Schema <> SCHEMA_PAIN_008_002_02) and (Schema <> SCHEMA_PAIN_008_003_02) and (Schema <> SCHEMA_PAIN_008_001_02) then
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
    PmtInfEntry[i].Validate(Schema, Result);

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
  SEPAWriteLine(stream, '<?xml version="1.0" encoding="UTF-8"?>');
  SEPAWriteLine(stream, '<Document xmlns="urn:iso:std:iso:20022:tech:xsd:'+Schema+'"'+
                        ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'+
                        ' xsi:schemaLocation="urn:iso:std:iso:20022:tech:xsd:'+Schema+' '+Schema+'.xsd">');
  SEPAWriteLine(stream, '<CstmrDrctDbtInitn>');

  SEPAWriteLine(stream, '<GrpHdr>');
  SEPAWriteLine(stream, '<MsgId>'+SEPACleanString(GrpHdrMsgId)+'</MsgId>');
  SEPAWriteLine(stream, '<CreDtTm>'+SEPAFormatDateTime(GrpHdrCreDtTm)+'</CreDtTm>');
  SEPAWriteLine(stream, '<NbOfTxs>'+IntToStr(GrpHdrNbOfTxs)+'</NbOfTxs>');
  SEPAWriteLine(stream, '<CtrlSum>'+SEPAFormatAmount(GrpHdrCtrlSum)+'</CtrlSum>');
  SEPAWriteLine(stream, '<InitgPty><Nm>'+SEPACleanString(GrpHdrInitgPtyName, INITG_PTY_NAME_MAX_LEN)+'</Nm></InitgPty>');
  SEPAWriteLine(stream, '</GrpHdr>');

  for i := 0 to PmtInfCount-1 do
    if PmtInfEntry[i].NbOfTxs > 0 then
      PmtInfEntry[i].SaveToStream(stream, Schema);

  SEPAWriteLine(stream, '</CstmrDrctDbtInitn>');
  SEPAWriteLine(stream, '</Document>');
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
