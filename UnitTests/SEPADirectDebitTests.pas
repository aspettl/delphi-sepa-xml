//
//   Unit tests for "SEPADirectDebit.pas"
//   (beta version 0.2.4, 2016-10-01)
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
//   E-mail:  aaron@spettl.de
//
unit SEPADirectDebitTests;

{$IFDEF FPC}                // Lazarus: set compiler mode and file encoding
{%encoding CP1252}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SEPADirectDebit, SEPACommon, SEPATestCase,
  {$IFDEF FPC}
  fpcunit, testutils, testregistry
  {$ELSE}
  TestFrameWork
  {$ENDIF},
  SysUtils, Classes;

type
  TAmendmentInformationDetails26Tests = class(TSEPATestCase)
  private
    fAmendmentInfo: TAmendmentInformationDetails26;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestValidate;
    procedure TestSaveToStream;
  end;

  TAmendmentInformationDetails30Tests = class(TSEPATestCase)
  private
    fAmendmentInfo: TAmendmentInformationDetails30;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestValidate;
    procedure TestSaveToStream;
  end;

  TMandateRelatedInformationTests = class(TSEPATestCase)
  private
    fMandateInfo: TMandateRelatedInformation;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestValidate;
    procedure TestSaveToStream;
  end;

  TDirectDebitTransactionInformationTests = class(TSEPATestCase)
  private
    fTransaction: TDirectDebitTransactionInformation;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestValidate;
    procedure TestSaveToStream;
  end;

  TDirectDebitPaymentInformationTests = class(TSEPATestCase)
  private
    fPaymentInfo: TDirectDebitPaymentInformation;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestGetCtrlSum;
    procedure TestAppendGetAndCountDrctDbtTxEntries;
    procedure TestValidate;
    procedure TestSaveToStream;
  end;

  TDirectDebitInitiationTests = class(TSEPATestCase)
  private
    fSetUpTime: TDateTime;
    fDirectDebit: TDirectDebitInitiation;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestGetAndSetSchema;
    procedure TestGetGrpHdrNbOfTxs;
    procedure TestAppendGetAndCountPmtInfEntries;
    procedure TestValidate;
    procedure TestSaveToStream;
    procedure TestSaveToDisk;
  end;

implementation

// TAmendmentInformationDetails26Tests

procedure TAmendmentInformationDetails26Tests.SetUp;
begin
  inherited;
  fAmendmentInfo := TAmendmentInformationDetails26.Create;
end;

procedure TAmendmentInformationDetails26Tests.TearDown;
begin
  FreeAndNil(fAmendmentInfo);
  inherited;
end;

procedure TAmendmentInformationDetails26Tests.TestCreate;
begin
  CheckNotNull(fAmendmentInfo.OrgnlDbtrAcct);

  CheckEquals('', fAmendmentInfo.OrgnlMndtId);
  CheckEquals('', fAmendmentInfo.OrgnlCdtrSchmeIdNm);
  CheckEquals('', fAmendmentInfo.OrgnlCdtrSchmeIdIdPrvtIdOthrId);
  CheckEquals('', fAmendmentInfo.OrgnlDbtrAgtFinInstIdOthrId);
end;

procedure TAmendmentInformationDetails26Tests.TestValidate;
begin
  // empty object
  CheckValidation([EMPTY_AMDMNT_INF_DTLS], fAmendmentInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));

  // wrong schema
  CheckValidation([EMPTY_AMDMNT_INF_DTLS, Format(SCHEMA_AMDMNT_INF_DTLS_26, [SCHEMA_PAIN_008_001_02])], fAmendmentInfo.Validate(SCHEMA_PAIN_008_001_02, SEQ_TP_FRST));

  // now generate object with all required fields
  fAmendmentInfo.OrgnlMndtId                    := 'MNDTID';
  fAmendmentInfo.OrgnlCdtrSchmeIdNm             := 'Creditor name';
  fAmendmentInfo.OrgnlCdtrSchmeIdIdPrvtIdOthrId := 'DE98ZZZ09999999999';
  fAmendmentInfo.OrgnlDbtrAgtFinInstIdOthrId    := ORGNL_DBTR_AGT_SMNDA;
  CheckValidation([], fAmendmentInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));

  // checks for clean SEPA strings: original mandate identifier is not automatically cleaned
  fAmendmentInfo.OrgnlMndtId := 'test!';
  CheckValidationContains([Format(INVALID_ORGNL_MNDT_ID, ['test!'])], fAmendmentInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  fAmendmentInfo.OrgnlMndtId := 'MNDTID';

  // checks for clean SEPA strings: original mandate identifier is not automatically cleaned
  fAmendmentInfo.OrgnlCdtrSchmeIdNm := 'test!';
  CheckValidationContains([Format(INVALID_ORGNL_CRDTR_NM, ['test!'])], fAmendmentInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  fAmendmentInfo.OrgnlCdtrSchmeIdNm := 'Creditor name';

  // checks for valid creditor identifier
  fAmendmentInfo.OrgnlCdtrSchmeIdIdPrvtIdOthrId := 'DE98ZZZ09999999998';
  CheckValidationContains([Format(INVALID_ORGNL_CRDTR_ID, ['DE98ZZZ09999999998'])], fAmendmentInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  fAmendmentInfo.OrgnlCdtrSchmeIdIdPrvtIdOthrId := 'DE98ZZZ09999999999';

  // checks for "SMNDA" as the only allowed value for new debtor agent "other id"
  fAmendmentInfo.OrgnlDbtrAgtFinInstIdOthrId := 'TEST';
  CheckValidationContains([Format(INVALID_ORGNL_FIN_INST_ID, ['TEST'])], fAmendmentInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  fAmendmentInfo.OrgnlDbtrAgtFinInstIdOthrId := ORGNL_DBTR_AGT_SMNDA;

  // test that original debtor account is validated (only if present)
  // - first check that everything works with a valid IBAN
  fAmendmentInfo.OrgnlDbtrAcct.IBAN := 'DE58123456780123456789';
  CheckValidation([], fAmendmentInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  // - and make sure it doesn't with an invalid IBAN
  fAmendmentInfo.OrgnlDbtrAcct.IBAN := 'DE58123456780123456788';
  CheckValidation([Format(INVALID_IBAN, ['DE58123456780123456788'])], fAmendmentInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  fAmendmentInfo.OrgnlDbtrAcct.IBAN := 'DE58123456780123456789';

  // check usage of SMNDA
  // - FRST with SMNDA (ok)
  fAmendmentInfo.OrgnlDbtrAgtFinInstIdOthrId := ORGNL_DBTR_AGT_SMNDA;
  CheckValidation([], fAmendmentInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  // - RCUR with SMNDA (not ok)
  CheckValidation([INVALID_SEQ_TP_FRST_SMNDA2], fAmendmentInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_RCUR));
  // - RCUR without SMNDA (ok)
  fAmendmentInfo.OrgnlMndtId := 'OLDMANDATEID';
  fAmendmentInfo.OrgnlDbtrAgtFinInstIdOthrId := '';
  CheckValidation([], fAmendmentInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_RCUR));
  // - FRST without SMNDA (not ok)
  CheckValidation([INVALID_SEQ_TP_FRST_SMNDA1], fAmendmentInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
end;

procedure TAmendmentInformationDetails26Tests.TestSaveToStream;
begin
  // empty object: first make sure that all fields are only saved when they are present
  
  fAmendmentInfo.SaveToStream(SaveStream, SCHEMA_PAIN_008_003_02);

  CheckSaveStream('<AmdmntInfDtls>'+
                  '</AmdmntInfDtls>');

  // now test the appearance of all fields

  fAmendmentInfo.OrgnlMndtId                    := 'MNDTID';
  fAmendmentInfo.OrgnlCdtrSchmeIdNm             := 'Creditor name';
  fAmendmentInfo.OrgnlDbtrAcct.IBAN             := 'DE58123456780123456789';
  fAmendmentInfo.OrgnlCdtrSchmeIdIdPrvtIdOthrId := 'DE98ZZZ09999999999';
  fAmendmentInfo.OrgnlDbtrAgtFinInstIdOthrId    := 'SMNDA';

  fAmendmentInfo.SaveToStream(SaveStream, SCHEMA_PAIN_008_003_02);

  CheckSaveStream('<AmdmntInfDtls>'+
                  '<OrgnlMndtId>MNDTID</OrgnlMndtId>'+
                  '<OrgnlCdtrSchmeId>'+
                  '<Nm>Creditor name</Nm>'+
                  '<Id><PrvtId><Othr><Id>DE98ZZZ09999999999</Id><SchmeNm><Prtry>SEPA</Prtry></SchmeNm></Othr></PrvtId></Id>'+
                  '</OrgnlCdtrSchmeId>'+
                  '<OrgnlDbtrAcct>'+
                  '<Id><IBAN>DE58123456780123456789</IBAN></Id>'+
                  '</OrgnlDbtrAcct>'+
                  '<OrgnlDbtrAgt><FinInstnId><Othr><Id>SMNDA</Id></Othr></FinInstnId></OrgnlDbtrAgt>'+
                  '</AmdmntInfDtls>');
end;

// TAmendmentInformationDetails30Tests

procedure TAmendmentInformationDetails30Tests.SetUp;
begin
  inherited;
  fAmendmentInfo := TAmendmentInformationDetails30.Create;
end;

procedure TAmendmentInformationDetails30Tests.TearDown;
begin
  FreeAndNil(fAmendmentInfo);
  inherited;
end;

procedure TAmendmentInformationDetails30Tests.TestCreate;
begin
  CheckEquals('', fAmendmentInfo.OrgnlMndtId);
  CheckEquals('', fAmendmentInfo.OrgnlCdtrSchmeIdNm);
  CheckEquals('', fAmendmentInfo.OrgnlCdtrSchmeIdIdPrvtIdOthrId);
  CheckEquals('', fAmendmentInfo.OrgnlDbtrAcct);
end;

procedure TAmendmentInformationDetails30Tests.TestValidate;
begin
  // empty object
  CheckValidation([EMPTY_AMDMNT_INF_DTLS], fAmendmentInfo.Validate(SCHEMA_PAIN_008_001_02, SEQ_TP_FRST));

  // wrong schema
  CheckValidation([EMPTY_AMDMNT_INF_DTLS, Format(SCHEMA_AMDMNT_INF_DTLS_30, [SCHEMA_PAIN_008_003_02])], fAmendmentInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));

  // now generate object with all required fields
  fAmendmentInfo.OrgnlMndtId                    := 'MNDTID';
  fAmendmentInfo.OrgnlCdtrSchmeIdNm             := 'Creditor name';
  fAmendmentInfo.OrgnlCdtrSchmeIdIdPrvtIdOthrId := 'DE98ZZZ09999999999';
  fAmendmentInfo.OrgnlDbtrAcct                  := ORGNL_DBTR_ACCT_SMNDA;
  CheckValidation([], fAmendmentInfo.Validate(SCHEMA_PAIN_008_001_02, SEQ_TP_FRST));

  // checks for clean SEPA strings: original mandate identifier is not automatically cleaned
  fAmendmentInfo.OrgnlMndtId := 'test!';
  CheckValidationContains([Format(INVALID_ORGNL_MNDT_ID, ['test!'])], fAmendmentInfo.Validate(SCHEMA_PAIN_008_001_02, SEQ_TP_FRST));
  fAmendmentInfo.OrgnlMndtId := 'MNDTID';

  // checks for clean SEPA strings: original mandate identifier is not automatically cleaned
  fAmendmentInfo.OrgnlCdtrSchmeIdNm := 'test!';
  CheckValidationContains([Format(INVALID_ORGNL_CRDTR_NM, ['test!'])], fAmendmentInfo.Validate(SCHEMA_PAIN_008_001_02, SEQ_TP_FRST));
  fAmendmentInfo.OrgnlCdtrSchmeIdNm := 'Creditor name';

  // checks for valid creditor identifier
  fAmendmentInfo.OrgnlCdtrSchmeIdIdPrvtIdOthrId := 'DE98ZZZ09999999998';
  CheckValidationContains([Format(INVALID_ORGNL_CRDTR_ID, ['DE98ZZZ09999999998'])], fAmendmentInfo.Validate(SCHEMA_PAIN_008_001_02, SEQ_TP_FRST));
  fAmendmentInfo.OrgnlCdtrSchmeIdIdPrvtIdOthrId := 'DE98ZZZ09999999999';

  // checks for "SMNDA" as the only allowed value for new debtor agent "other id"
  fAmendmentInfo.OrgnlDbtrAcct := 'TEST';
  CheckValidationContains([Format(INVALID_ORGNL_DBTR_ACCT, ['TEST'])], fAmendmentInfo.Validate(SCHEMA_PAIN_008_001_02, SEQ_TP_FRST));
  fAmendmentInfo.OrgnlDbtrAcct := ORGNL_DBTR_ACCT_SMNDA;
end;

procedure TAmendmentInformationDetails30Tests.TestSaveToStream;
begin
  // empty object: first make sure that all fields are only saved when they are present
  
  fAmendmentInfo.SaveToStream(SaveStream, SCHEMA_PAIN_008_001_02);

  CheckSaveStream('<AmdmntInfDtls>'+
                  '</AmdmntInfDtls>');

  // now test the appearance of all fields

  fAmendmentInfo.OrgnlMndtId                    := 'MNDTID';
  fAmendmentInfo.OrgnlCdtrSchmeIdNm             := 'Creditor name';
  fAmendmentInfo.OrgnlCdtrSchmeIdIdPrvtIdOthrId := 'DE98ZZZ09999999999';
  fAmendmentInfo.OrgnlDbtrAcct                  := ORGNL_DBTR_ACCT_SMNDA;

  fAmendmentInfo.SaveToStream(SaveStream, SCHEMA_PAIN_008_001_02);

  CheckSaveStream('<AmdmntInfDtls>'+
                  '<OrgnlMndtId>MNDTID</OrgnlMndtId>'+
                  '<OrgnlCdtrSchmeId>'+
                  '<Nm>Creditor name</Nm>'+
                  '<Id><PrvtId><Othr><Id>DE98ZZZ09999999999</Id><SchmeNm><Prtry>SEPA</Prtry></SchmeNm></Othr></PrvtId></Id>'+
                  '</OrgnlCdtrSchmeId>'+
                  '<OrgnlDbtrAcct>'+
                  '<Id><Othr><Id>SMNDA</Id></Othr></Id>'+
                  '</OrgnlDbtrAcct>'+
                  '</AmdmntInfDtls>');
end;

// TMandateRelatedInformationTests

procedure TMandateRelatedInformationTests.SetUp;
begin
  inherited;
  fMandateInfo := TMandateRelatedInformation.Create;
end;

procedure TMandateRelatedInformationTests.TearDown;
begin
  FreeAndNil(fMandateInfo);
  inherited;
end;

procedure TMandateRelatedInformationTests.TestCreate;
begin
  CheckNotNull(fMandateInfo.AmdmntInfDtls26);

  CheckEquals('', fMandateInfo.MndtId);
  CheckEquals(0.0, fMandateInfo.DtOfSgntr);
  CheckEquals(false, fMandateInfo.AmdmntInd);
end;

procedure TMandateRelatedInformationTests.TestValidate;
begin
  // empty object
  CheckValidation([EMPTY_MNDT_ID, EMPTY_DT_OF_SGNTR], fMandateInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));

  // now generate object with all required fields
  fMandateInfo.MndtId    := 'MNDTID';
  fMandateInfo.DtOfSgntr := Now;
  CheckValidation([], fMandateInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));

  // checks for clean SEPA strings: mandate identifier is not automatically cleaned
  fMandateInfo.MndtId := 'test!';
  CheckValidationContains([Format(INVALID_MNDT_ID, ['test!'])], fMandateInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  fMandateInfo.MndtId := 'MNDTID';

  // checks for invalid mandate date: a date in the future is not allowed
  fMandateInfo.DtOfSgntr := Now+1;
  CheckValidationContains([Format(INVALID_DT_OF_SGNTR, [DateToStr(Now+1)])], fMandateInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  fMandateInfo.DtOfSgntr := Now;

  // checks that amendment information details are validated in case of specified amendment indicator
  fMandateInfo.AmdmntInd := true;
  fMandateInfo.AmdmntInfDtls26.OrgnlDbtrAcct.IBAN := 'INVALID';
  fMandateInfo.AmdmntInfDtls30.OrgnlDbtrAcct := 'INVALID';
  // - for 2.6-2.9 with pain.008.002.02/pain.008.003.02
  CheckValidation([Format(INVALID_IBAN, ['INVALID'])], fMandateInfo.Validate(SCHEMA_PAIN_008_002_02, SEQ_TP_RCUR));
  CheckValidation([Format(INVALID_IBAN, ['INVALID'])], fMandateInfo.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_RCUR));
  // - for 3.0 with pain.008.001.02
  CheckValidation([Format(INVALID_ORGNL_DBTR_ACCT, ['INVALID'])], fMandateInfo.Validate(SCHEMA_PAIN_008_001_02, SEQ_TP_RCUR));
end;

procedure TMandateRelatedInformationTests.TestSaveToStream;
begin
  fMandateInfo.MndtId    := 'MNDTID';
  fMandateInfo.DtOfSgntr := EncodeDate(2016, 9, 1);

  fMandateInfo.SaveToStream(SaveStream, SCHEMA_PAIN_008_003_02);

  CheckSaveStream('<MndtRltdInf>'+
                  '<MndtId>MNDTID</MndtId>'+
                  '<DtOfSgntr>2016-09-01</DtOfSgntr>'+
                  '<AmdmntInd>false</AmdmntInd>'+
                  '</MndtRltdInf>');

  // now with mandate amendment information details
  fMandateInfo.AmdmntInd := true;
  fMandateInfo.AmdmntInfDtls26.OrgnlMndtId := 'OLD';
  fMandateInfo.AmdmntInfDtls30.OrgnlDbtrAcct := 'INVALID';
  // - for 2.6-2.9 with pain.008.002.02/pain.008.003.02
  fMandateInfo.SaveToStream(SaveStream, SCHEMA_PAIN_008_002_02);
  CheckSaveStream('<MndtRltdInf>'+
                  '<MndtId>MNDTID</MndtId>'+
                  '<DtOfSgntr>2016-09-01</DtOfSgntr>'+
                  '<AmdmntInd>true</AmdmntInd>'+
                  '<AmdmntInfDtls><OrgnlMndtId>OLD</OrgnlMndtId></AmdmntInfDtls>'+
                  '</MndtRltdInf>');
  fMandateInfo.SaveToStream(SaveStream, SCHEMA_PAIN_008_003_02);
  CheckSaveStream('<MndtRltdInf>'+
                  '<MndtId>MNDTID</MndtId>'+
                  '<DtOfSgntr>2016-09-01</DtOfSgntr>'+
                  '<AmdmntInd>true</AmdmntInd>'+
                  '<AmdmntInfDtls><OrgnlMndtId>OLD</OrgnlMndtId></AmdmntInfDtls>'+
                  '</MndtRltdInf>');
  // - for 3.0 with pain.008.001.02    
  fMandateInfo.SaveToStream(SaveStream, SCHEMA_PAIN_008_001_02);
  CheckSaveStream('<MndtRltdInf>'+
                  '<MndtId>MNDTID</MndtId>'+
                  '<DtOfSgntr>2016-09-01</DtOfSgntr>'+
                  '<AmdmntInd>true</AmdmntInd>'+
                  '<AmdmntInfDtls><OrgnlDbtrAcct><Id><Othr><Id>INVALID</Id></Othr></Id></OrgnlDbtrAcct></AmdmntInfDtls>'+
                  '</MndtRltdInf>');
end;

// TDirectDebitTransactionInformationTests

procedure TDirectDebitTransactionInformationTests.SetUp;
begin
  inherited;
  fTransaction := TDirectDebitTransactionInformation.Create;
end;

procedure TDirectDebitTransactionInformationTests.TearDown;
begin
  FreeAndNil(fTransaction);
  inherited;
end;

procedure TDirectDebitTransactionInformationTests.TestCreate;
begin
  CheckNotNull(fTransaction.DrctDbtTxMndtRltdInf);
  CheckNotNull(fTransaction.DbtrAgt);
  CheckNotNull(fTransaction.DbtrAcct);

  CheckEquals(END_TO_END_ID_NOTPROVIDED, fTransaction.PmtIdEndToEndId);
  CheckEquals(CCY_EUR, fTransaction.InstdAmtCcy);
  CheckEquals(0.0, fTransaction.InstdAmt);
  CheckEquals('', fTransaction.DbtrNm);
  CheckEquals('', fTransaction.UltmtDbtrNm);
  CheckEquals('', fTransaction.RmtInfUstrd);
end;

procedure TDirectDebitTransactionInformationTests.TestValidate;
begin
  // fill out debtor IBAN and BIC, their validation is tested in the respective classes
  fTransaction.DbtrAgt.BIC   := 'SOMEFININST';
  fTransaction.DbtrAcct.IBAN := 'DE58123456780123456789';
  // fill out mandate information
  fTransaction.DrctDbtTxMndtRltdInf.MndtId    := 'MNDTID';
  fTransaction.DrctDbtTxMndtRltdInf.DtOfSgntr := EncodeDate(2016, 9, 1);

  // empty object (make sure that no unnecessary "invalid" messages appear)
  CheckValidation([EMPTY_DBTR_NM, EMPTY_RMT_INF_USTRD, Format(INVALID_INSTD_AMT, ['0.0000'])], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));

  // check remaining fields which should not be empty
  fTransaction.PmtIdEndToEndId := '';
  CheckValidationContains([EMPTY_END_TO_END_ID], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  fTransaction.InstdAmtCcy     := '';
  CheckValidationContains([EMPTY_INSTD_AMT_CCY], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));

  // now generate object with all required fields
  fTransaction.PmtIdEndToEndId := 'END-TO-END';
  fTransaction.InstdAmtCcy     := CCY_EUR;
  fTransaction.InstdAmt        := 0.01;
  fTransaction.DbtrNm          := 'Debtor name';
  fTransaction.RmtInfUstrd     := 'Remittance information';
  CheckValidation([], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));

  // checks for clean SEPA strings: End-to-End ID is not automatically cleaned
  fTransaction.PmtIdEndToEndId := 'END-TO-END!';
  CheckValidationContains([Format(INVALID_END_TO_END_ID, ['END-TO-END!'])], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  fTransaction.PmtIdEndToEndId := 'END-TO-END';

  // checks for clean SEPA strings: name and remittance information are automatically cleaned
  fTransaction.DbtrNm          := 'Debtor name! Test';
  CheckEquals('Debtor name  Test', fTransaction.DbtrNm);
  CheckValidation([], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));

  fTransaction.UltmtDbtrNm     := 'Ultimate debtor name! Test';
  CheckEquals('Ultimate debtor name  Test', fTransaction.UltmtDbtrNm);
  CheckValidation([], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));

  fTransaction.RmtInfUstrd     := 'Remittance information! Test';
  CheckEquals('Remittance information  Test', fTransaction.RmtInfUstrd);
  CheckValidation([], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));

  // check for negative instructed amount
  fTransaction.InstdAmt        := -0.01;
  CheckValidationContains([Format(INVALID_INSTD_AMT, ['-0.0100'])], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  fTransaction.InstdAmt        := -0.00;
  CheckValidationContains([Format(INVALID_INSTD_AMT, ['0.0000'])], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));

  // check for instructed amount with more three or four decimal places
  fTransaction.InstdAmt        := 0.011;
  CheckValidationContains([Format(INVALID_INSTD_AMT, ['0.0110'])], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  fTransaction.InstdAmt        := 0.0101;
  CheckValidationContains([Format(INVALID_INSTD_AMT, ['0.0101'])], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));

  // check for instructed amount: decimal places at position 5 and higher are irrelevant
  fTransaction.InstdAmt        := 0.01001;
  CheckValidation([], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));

  // check that debtor agent is validated
  fTransaction.DbtrAgt.BIC     := '';
  CheckValidationContains([EMPTY_BIC_OTHR_ID], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  fTransaction.DbtrAgt.BIC     := 'SOMEFININST';

  // check that debtor account is validated
  fTransaction.DbtrAcct.IBAN   := '';
  CheckValidationContains([EMPTY_IBAN], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  fTransaction.DbtrAcct.IBAN   := 'DE58123456780123456789';

  // check that mandate information object is validated
  fTransaction.DrctDbtTxMndtRltdInf.MndtId := '';
  CheckValidationContains([EMPTY_MNDT_ID], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  fTransaction.DrctDbtTxMndtRltdInf.MndtId := 'MNDTID';

  // check that IBAN-only only allowed for German accounts
  fTransaction.DbtrAgt.BIC     := '';
  fTransaction.DbtrAgt.OthrID  := FIN_INSTN_NOTPROVIDED;
  CheckValidation([], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
  fTransaction.DbtrAcct.IBAN   := 'CH';
  CheckValidationContains([INVALID_IBAN_NOT_DE], fTransaction.Validate(SCHEMA_PAIN_008_003_02, SEQ_TP_FRST));
end;

procedure TDirectDebitTransactionInformationTests.TestSaveToStream;
begin
  fTransaction.PmtIdEndToEndId := 'END-TO-END';
  fTransaction.InstdAmtCcy     := CCY_EUR;
  fTransaction.InstdAmt        := 0.01;
  fTransaction.DbtrNm          := 'Debtor name';
  fTransaction.DbtrAgt.BIC     := 'SOMEFININST';
  fTransaction.DbtrAcct.IBAN   := 'DE58123456780123456789';
  fTransaction.RmtInfUstrd     := 'Remittance information';
  fTransaction.DrctDbtTxMndtRltdInf.MndtId    := 'MNDTID';
  fTransaction.DrctDbtTxMndtRltdInf.DtOfSgntr := EncodeDate(2016, 9, 1);

  fTransaction.SaveToStream(SaveStream, SCHEMA_PAIN_008_003_02);

  CheckSaveStream('<DrctDbtTxInf>'+
                  '<PmtId><EndToEndId>END-TO-END</EndToEndId></PmtId>'+
                  '<InstdAmt Ccy="EUR">0.01</InstdAmt>'+
                  '<DrctDbtTx>'+
                  '<MndtRltdInf>'+
                  '<MndtId>MNDTID</MndtId>'+
                  '<DtOfSgntr>2016-09-01</DtOfSgntr>'+
                  '<AmdmntInd>false</AmdmntInd>'+
                  '</MndtRltdInf>'+
                  '</DrctDbtTx>'+
                  '<DbtrAgt>'+
                  '<FinInstnId><BIC>SOMEFININST</BIC></FinInstnId>'+
                  '</DbtrAgt>'+
                  '<Dbtr><Nm>Debtor name</Nm></Dbtr>'+
                  '<DbtrAcct>'+
                  '<Id><IBAN>DE58123456780123456789</IBAN></Id>'+
                  '</DbtrAcct>'+
                  '<RmtInf><Ustrd>Remittance information</Ustrd></RmtInf>'+
                  '</DrctDbtTxInf>');

  // now IBAN-only
  fTransaction.DbtrAgt.BIC := '';
  fTransaction.DbtrAgt.OthrID := FIN_INSTN_NOTPROVIDED;

  fTransaction.SaveToStream(SaveStream, SCHEMA_PAIN_008_003_02);

  CheckSaveStream('<DrctDbtTxInf>'+
                  '<PmtId><EndToEndId>END-TO-END</EndToEndId></PmtId>'+
                  '<InstdAmt Ccy="EUR">0.01</InstdAmt>'+
                  '<DrctDbtTx>'+
                  '<MndtRltdInf>'+
                  '<MndtId>MNDTID</MndtId>'+
                  '<DtOfSgntr>2016-09-01</DtOfSgntr>'+
                  '<AmdmntInd>false</AmdmntInd>'+
                  '</MndtRltdInf>'+
                  '</DrctDbtTx>'+
                  '<DbtrAgt>'+
                  '<FinInstnId><Othr><Id>NOTPROVIDED</Id></Othr></FinInstnId>'+
                  '</DbtrAgt>'+
                  '<Dbtr><Nm>Debtor name</Nm></Dbtr>'+
                  '<DbtrAcct>'+
                  '<Id><IBAN>DE58123456780123456789</IBAN></Id>'+
                  '</DbtrAcct>'+
                  '<RmtInf><Ustrd>Remittance information</Ustrd></RmtInf>'+
                  '</DrctDbtTxInf>');

  // now add ultimate debtor name
  fTransaction.UltmtDbtrNm := 'Ultimate debtor name';

  fTransaction.SaveToStream(SaveStream, SCHEMA_PAIN_008_003_02);

  CheckSaveStream('<DrctDbtTxInf>'+
                  '<PmtId><EndToEndId>END-TO-END</EndToEndId></PmtId>'+
                  '<InstdAmt Ccy="EUR">0.01</InstdAmt>'+
                  '<DrctDbtTx>'+
                  '<MndtRltdInf>'+
                  '<MndtId>MNDTID</MndtId>'+
                  '<DtOfSgntr>2016-09-01</DtOfSgntr>'+
                  '<AmdmntInd>false</AmdmntInd>'+
                  '</MndtRltdInf>'+
                  '</DrctDbtTx>'+
                  '<DbtrAgt>'+
                  '<FinInstnId><Othr><Id>NOTPROVIDED</Id></Othr></FinInstnId>'+
                  '</DbtrAgt>'+
                  '<Dbtr><Nm>Debtor name</Nm></Dbtr>'+
                  '<DbtrAcct>'+
                  '<Id><IBAN>DE58123456780123456789</IBAN></Id>'+
                  '</DbtrAcct>'+
                  '<UltmtDbtr><Nm>Ultimate debtor name</Nm></UltmtDbtr>'+
                  '<RmtInf><Ustrd>Remittance information</Ustrd></RmtInf>'+
                  '</DrctDbtTxInf>');
end;

// TDirectDebitPaymentInformationTests

procedure TDirectDebitPaymentInformationTests.SetUp;
begin
  inherited;
  fPaymentInfo := TDirectDebitPaymentInformation.Create;
end;

procedure TDirectDebitPaymentInformationTests.TearDown;
begin
  FreeAndNil(fPaymentInfo);
  inherited;
end;

procedure TDirectDebitPaymentInformationTests.TestCreate;
begin
  CheckNotNull(fPaymentInfo.CdtrAcct);
  CheckNotNull(fPaymentInfo.CdtrAgt);

  CheckNotEquals(0, Length(fPaymentInfo.PmtInfId));
  CheckEquals(PMT_MTD_DIRECT_DEBIT, fPaymentInfo.PmtMtd);
  CheckEquals(SEPA, fPaymentInfo.PmtTpInfSvcLvlCd);
  CheckEquals('', fPaymentInfo.PmtTpInfLclInstrmCd);
  CheckEquals('', fPaymentInfo.PmtTpInfSeqTp);
  CheckEquals(0, fPaymentInfo.ReqdColltnDt);
  CheckEquals('', fPaymentInfo.CdtrNm);
  CheckEquals(CHRG_BR_SLEV, fPaymentInfo.ChrgBr);
  CheckEquals('', fPaymentInfo.CdtrSchmeIdIdPrvtIdOthrId);
  CheckEquals(SEPA, fPaymentInfo.CdtrSchmeIdIdPrvtIdOthrSchmeNmPrtry);

  CheckEquals(0, fPaymentInfo.DrctDbtTxInfCount);
end;

procedure TDirectDebitPaymentInformationTests.TestGetCtrlSum;
var
  ti: TDirectDebitTransactionInformation;
  c: Currency;
  f: Double;
begin
  CheckEquals(0.0, fPaymentInfo.CtrlSum);

  // choose very large value such that we have less significant decimal places
  // for double floating-point values
  c := 100000000000000.0;
  f := 100000000000000.0;

  ti := TDirectDebitTransactionInformation.Create;
  ti.InstdAmt := 100000000000000.0;
  fPaymentInfo.AppendDrctDbtTxInfEntry(ti);

  Check(fPaymentInfo.CtrlSum = c,
        Format('expected value was %s, but actual value is %s',
               [SEPAFormatAmount(c), SEPAFormatAmount(fPaymentInfo.CtrlSum)]));

  c := c+0.01; // now contains: 100000000000000.01
  f := f+0.01; // now contains: 100000000000000.02

  ti := TDirectDebitTransactionInformation.Create;
  ti.InstdAmt := 0.01;
  fPaymentInfo.AppendDrctDbtTxInfEntry(ti);

  Check(fPaymentInfo.CtrlSum = c,
        Format('expected value was %s, but actual value is %s (note: wrong '+
               'double addition gives %s)',
               [SEPAFormatAmount(c), SEPAFormatAmount(fPaymentInfo.CtrlSum),
                SEPAFormatAmount(f)]));
end;

procedure TDirectDebitPaymentInformationTests.TestAppendGetAndCountDrctDbtTxEntries;
var
  ti1, ti2, ti3: TDirectDebitTransactionInformation;
begin
  ti1 := TDirectDebitTransactionInformation.Create;
  ti2 := TDirectDebitTransactionInformation.Create;
  ti3 := TDirectDebitTransactionInformation.Create;

  CheckEquals(0, fPaymentInfo.DrctDbtTxInfCount);

  fPaymentInfo.AppendDrctDbtTxInfEntry(ti1);
  CheckEquals(1, fPaymentInfo.DrctDbtTxInfCount);
  Check(ti1 = fPaymentInfo.DrctDbtTxInfEntry[0], 'count 1, entry 0 check');

  fPaymentInfo.AppendDrctDbtTxInfEntry(ti2);
  CheckEquals(2, fPaymentInfo.DrctDbtTxInfCount);
  Check(ti1 = fPaymentInfo.DrctDbtTxInfEntry[0], 'count 2, entry 0 check');
  Check(ti2 = fPaymentInfo.DrctDbtTxInfEntry[1], 'count 2, entry 1 check');

  fPaymentInfo.AppendDrctDbtTxInfEntry(ti3);
  CheckEquals(3, fPaymentInfo.DrctDbtTxInfCount);
  Check(ti1 = fPaymentInfo.DrctDbtTxInfEntry[0], 'count 3, entry 0 check');
  Check(ti2 = fPaymentInfo.DrctDbtTxInfEntry[1], 'count 3, entry 1 check');
  Check(ti3 = fPaymentInfo.DrctDbtTxInfEntry[2], 'count 3, entry 2 check');
end;

procedure TDirectDebitPaymentInformationTests.TestValidate;
begin
  // fill out creditor IBAN and BIC, their validation is tested in the respective classes
  fPaymentInfo.CdtrAgt.BIC   := 'SOMEFININST';
  fPaymentInfo.CdtrAcct.IBAN := 'DE58123456780123456789';

  // empty object (make sure that no unnecessary "invalid" messages appear)
  CheckValidation([EMPTY_CDTR_NM, EMPTY_CDTR_ID, Format(INVALID_LCL_INSTRM_CD, ['']), Format(INVALID_SEQ_TP, ['']), Format(INVALID_REQD_COLLTN_DT, [DateToStr(0)]), Format(INVALID_CDTR_ID, [''])], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));

  // check remaining fields which should not be empty
  fPaymentInfo.PmtInfId := '';
  CheckValidationContains([EMPTY_PMT_INF_ID], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));
  fPaymentInfo.PmtInfId := SEPAGenerateUUID;

  // now generate object with all required fields
  fPaymentInfo.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_CORE;
  fPaymentInfo.PmtTpInfSeqTp := SEQ_TP_FRST;
  fPaymentInfo.ReqdColltnDt := SEPAEarliestCollectionDate(LCL_INSTRM_CD_CORE, SEQ_TP_FRST, SCHEMA_PAIN_008_003_02);
  fPaymentInfo.CdtrNm       := 'Creditor name';
  fPaymentInfo.CdtrSchmeIdIdPrvtIdOthrId := 'DE98ZZZ09999999999';
  CheckValidation([], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));

  // checks for clean SEPA strings: ID is not automatically cleaned
  fPaymentInfo.PmtInfId := 'ID!';
  CheckValidationContains([Format(INVALID_PMT_INF_ID, ['ID!'])], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));
  fPaymentInfo.PmtInfId := SEPAGenerateUUID;

  // check payment method (only "DD" allowed)
  fPaymentInfo.PmtMtd := 'TEST';
  CheckValidationContains([Format(INVALID_PMT_MTD, ['TEST'])], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));
  fPaymentInfo.PmtMtd := PMT_MTD_DIRECT_DEBIT;

  // check local instrument code (only whitelisted values allowed)
  fPaymentInfo.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_CORE;
  CheckValidation([], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));
  fPaymentInfo.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_COR1;
  CheckValidation([], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));
  fPaymentInfo.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_B2B;
  CheckValidation([], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));
  fPaymentInfo.PmtTpInfLclInstrmCd := 'TEST';
  CheckValidation([Format(INVALID_LCL_INSTRM_CD, ['TEST'])], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));
  fPaymentInfo.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_CORE;

  // check local instrument code COR1 is only valid with schema "pain.008.003.02"
  fPaymentInfo.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_COR1;
  CheckValidation([INVALID_LCL_INSTRM_CD_COR1], fPaymentInfo.Validate(SCHEMA_PAIN_008_002_02));
  CheckValidation([INVALID_LCL_INSTRM_CD_COR1_TO_CORE], fPaymentInfo.Validate(SCHEMA_PAIN_008_001_02));
  fPaymentInfo.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_CORE;

  // check sequence type (only whitelisted values allowed)
  fPaymentInfo.PmtTpInfSeqTp := SEQ_TP_FRST;
  CheckValidation([], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));
  fPaymentInfo.PmtTpInfSeqTp := SEQ_TP_RCUR;
  CheckValidation([], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));
  fPaymentInfo.PmtTpInfSeqTp := SEQ_TP_OOFF;
  CheckValidation([], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));
  fPaymentInfo.PmtTpInfSeqTp := SEQ_TP_FNAL;
  CheckValidation([], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));
  fPaymentInfo.PmtTpInfSeqTp := 'TEST';
  CheckValidation([Format(INVALID_SEQ_TP, ['TEST'])], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));
  fPaymentInfo.PmtTpInfSeqTp := SEQ_TP_FRST;

  // check requested collection date
  fPaymentInfo.ReqdColltnDt := SEPAEarliestCollectionDate(LCL_INSTRM_CD_CORE, SEQ_TP_FRST, SCHEMA_PAIN_008_003_02)-1;
  CheckValidationContains([Format(INVALID_REQD_COLLTN_DT, [DateToStr(fPaymentInfo.ReqdColltnDt)])], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));
  fPaymentInfo.ReqdColltnDt := SEPAEarliestCollectionDate(LCL_INSTRM_CD_CORE, SEQ_TP_FRST, SCHEMA_PAIN_008_003_02);

  // checks for clean SEPA strings: name is automatically cleaned
  fPaymentInfo.CdtrNm := 'Creditor name! Test';
  CheckEquals('Creditor name  Test', fPaymentInfo.CdtrNm);
  CheckValidation([], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));

  // checks for invalid creditor identifier
  fPaymentInfo.CdtrSchmeIdIdPrvtIdOthrId := 'DE98ZZZ09999999998';
  CheckValidation([Format(INVALID_CDTR_ID, ['DE98ZZZ09999999998'])], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));
  fPaymentInfo.CdtrSchmeIdIdPrvtIdOthrId := 'DE98ZZZ09999999999';

  // check that creditor account and agent are validated
  fPaymentInfo.CdtrAgt.BIC   := '';
  fPaymentInfo.CdtrAcct.IBAN := 'DE58123456780123456788';
  CheckValidation([Format(INVALID_IBAN, ['DE58123456780123456788']), EMPTY_BIC_OTHR_ID], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));
  fPaymentInfo.CdtrAgt.BIC   := 'SOMEFININST';
  fPaymentInfo.CdtrAcct.IBAN := 'DE58123456780123456789';

  // check that transactions are validated
  CheckEquals('', fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02).Text);
  fPaymentInfo.AppendDrctDbtTxInfEntry(TDirectDebitTransactionInformation.Create);
  CheckNotEquals('', fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02).Text);

  // check that creditor has a German account
  fPaymentInfo.CdtrAcct.IBAN := 'CH';
  CheckValidationContains([INVALID_CDTR_ACCT_NOT_DE], fPaymentInfo.Validate(SCHEMA_PAIN_008_003_02));
  fPaymentInfo.CdtrAcct.IBAN := 'DE58123456780123456789';
end;

procedure TDirectDebitPaymentInformationTests.TestSaveToStream;
var
  xmlTxInfEntry0: RawByteString;
begin
  fPaymentInfo.PmtInfId      := 'PMTINFID';
  fPaymentInfo.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_CORE;
  fPaymentInfo.PmtTpInfSeqTp := SEQ_TP_FRST;
  fPaymentInfo.ReqdColltnDt  := EncodeDate(2016, 9, 1);
  fPaymentInfo.CdtrNm        := 'Creditor name';
  fPaymentInfo.CdtrAgt.BIC   := 'SOMEFININST';
  fPaymentInfo.CdtrAcct.IBAN := 'DE58123456780123456789';
  fPaymentInfo.CdtrSchmeIdIdPrvtIdOthrId := 'DE98ZZZ09999999999';

  fPaymentInfo.SaveToStream(SaveStream, SCHEMA_PAIN_008_003_02);

  CheckSaveStream('<PmtInf>'+
                  '<PmtInfId>PMTINFID</PmtInfId>'+
                  '<PmtMtd>DD</PmtMtd>'+
                  '<NbOfTxs>0</NbOfTxs>'+
                  '<CtrlSum>0.00</CtrlSum>'+
                  '<PmtTpInf>'+
                  '<SvcLvl><Cd>SEPA</Cd></SvcLvl>'+
                  '<LclInstrm><Cd>CORE</Cd></LclInstrm>'+
                  '<SeqTp>FRST</SeqTp>'+
                  '</PmtTpInf>'+
                  '<ReqdColltnDt>2016-09-01</ReqdColltnDt>'+
                  '<Cdtr><Nm>Creditor name</Nm></Cdtr>'+
                  '<CdtrAcct>'+
                  '<Id><IBAN>DE58123456780123456789</IBAN></Id>'+
                  '</CdtrAcct>'+
                  '<CdtrAgt>'+
                  '<FinInstnId><BIC>SOMEFININST</BIC></FinInstnId>'+
                  '</CdtrAgt>'+
                  '<ChrgBr>SLEV</ChrgBr>'+
                  '<CdtrSchmeId><Id><PrvtId><Othr>'+
                  '<Id>DE98ZZZ09999999999</Id>'+
                  '<SchmeNm><Prtry>SEPA</Prtry></SchmeNm>'+
                  '</Othr></PrvtId></Id></CdtrSchmeId>'+
                  '</PmtInf>');

  // test integration of transactions
  fPaymentInfo.AppendDrctDbtTxInfEntry(TDirectDebitTransactionInformation.Create);
  fPaymentInfo.DrctDbtTxInfEntry[0].PmtIdEndToEndId := 'END-TO-END';
  fPaymentInfo.DrctDbtTxInfEntry[0].InstdAmt := 0.01;
  fPaymentInfo.DrctDbtTxInfEntry[0].DbtrAgt.BIC := 'SOMEFININST';
  fPaymentInfo.DrctDbtTxInfEntry[0].DbtrNm := 'Debtor name';
  fPaymentInfo.DrctDbtTxInfEntry[0].DbtrAcct.IBAN := 'DE58123456780123456789';
  fPaymentInfo.DrctDbtTxInfEntry[0].RmtInfUstrd := 'Remittance information';
  fPaymentInfo.DrctDbtTxInfEntry[0].DrctDbtTxMndtRltdInf.MndtId := 'MNDTID';
  fPaymentInfo.DrctDbtTxInfEntry[0].DrctDbtTxMndtRltdInf.DtOfSgntr := EncodeDate(2016, 9, 1);

  fPaymentInfo.DrctDbtTxInfEntry[0].SaveToStream(SaveStream, SCHEMA_PAIN_008_003_02);
  xmlTxInfEntry0 := FetchAndResetSaveStream;
  fPaymentInfo.SaveToStream(SaveStream, SCHEMA_PAIN_008_003_02);

  CheckSaveStream('<PmtInf>'+
                  '<PmtInfId>PMTINFID</PmtInfId>'+
                  '<PmtMtd>DD</PmtMtd>'+
                  '<NbOfTxs>1</NbOfTxs>'+
                  '<CtrlSum>0.01</CtrlSum>'+
                  '<PmtTpInf>'+
                  '<SvcLvl><Cd>SEPA</Cd></SvcLvl>'+
                  '<LclInstrm><Cd>CORE</Cd></LclInstrm>'+
                  '<SeqTp>FRST</SeqTp>'+
                  '</PmtTpInf>'+
                  '<ReqdColltnDt>2016-09-01</ReqdColltnDt>'+
                  '<Cdtr><Nm>Creditor name</Nm></Cdtr>'+
                  '<CdtrAcct>'+
                  '<Id><IBAN>DE58123456780123456789</IBAN></Id>'+
                  '</CdtrAcct>'+
                  '<CdtrAgt>'+
                  '<FinInstnId><BIC>SOMEFININST</BIC></FinInstnId>'+
                  '</CdtrAgt>'+
                  '<ChrgBr>SLEV</ChrgBr>'+
                  '<CdtrSchmeId><Id><PrvtId><Othr>'+
                  '<Id>DE98ZZZ09999999999</Id>'+
                  '<SchmeNm><Prtry>SEPA</Prtry></SchmeNm>'+
                  '</Othr></PrvtId></Id></CdtrSchmeId>'+
                  xmlTxInfEntry0+
                  '</PmtInf>');
end;

// TDirectDebitInitiationTests

procedure TDirectDebitInitiationTests.SetUp;
begin
  fSetUpTime := Now;
  inherited;
  fDirectDebit := TDirectDebitInitiation.Create;
end;

procedure TDirectDebitInitiationTests.TearDown;
begin
  FreeAndNil(fDirectDebit);
  inherited;
end;

procedure TDirectDebitInitiationTests.TestCreate;
begin
  CheckEquals(SCHEMA_PAIN_008_003_02, fDirectDebit.Schema);  // internal fSchema should be empty, but we cannot see that here

  CheckNotEquals(0, Length(fDirectDebit.GrpHdrMsgId));
  Check((fDirectDebit.GrpHdrCreDtTm >= fSetUpTime) and (fDirectDebit.GrpHdrCreDtTm <= Now), 'Timestamp of created file must be between test setup and now');
  CheckEquals('', fDirectDebit.GrpHdrInitgPtyName);

  CheckEquals(0, fDirectDebit.PmtInfCount);
end;

procedure TDirectDebitInitiationTests.TestGetAndSetSchema;
begin
  // empty schema and no contents => should use standard version 2.7
  CheckEquals(SCHEMA_PAIN_008_003_02, fDirectDebit.Schema);

  // set schema manually => should return that one
  fDirectDebit.Schema := SCHEMA_PAIN_008_002_02;
  CheckEquals(SCHEMA_PAIN_008_002_02, fDirectDebit.Schema);
  fDirectDebit.Schema := SCHEMA_PAIN_008_003_02;
  CheckEquals(SCHEMA_PAIN_008_003_02, fDirectDebit.Schema);
end;

procedure TDirectDebitInitiationTests.TestGetGrpHdrNbOfTxs;
var
  pi: TDirectDebitPaymentInformation;
begin
  // empty: no transactions
  CheckEquals(0, fDirectDebit.GrpHdrNbOfTxs);

  // add a payment information object, but without transactions
  pi := TDirectDebitPaymentInformation.Create;
  fDirectDebit.AppendPmtInfEntry(pi);
  CheckEquals(0, fDirectDebit.GrpHdrNbOfTxs);

  // add a transaction to the existing payment information object
  pi.AppendDrctDbtTxInfEntry(TDirectDebitTransactionInformation.Create);
  CheckEquals(1, fDirectDebit.GrpHdrNbOfTxs);

  // add another transaction to the existing payment information object
  pi.AppendDrctDbtTxInfEntry(TDirectDebitTransactionInformation.Create);
  CheckEquals(2, fDirectDebit.GrpHdrNbOfTxs);

  // add another payment information object, but without transactions
  pi := TDirectDebitPaymentInformation.Create;
  fDirectDebit.AppendPmtInfEntry(pi);
  CheckEquals(2, fDirectDebit.GrpHdrNbOfTxs);

  // add a transaction to the previously generated payment information object
  pi.AppendDrctDbtTxInfEntry(TDirectDebitTransactionInformation.Create);
  CheckEquals(3, fDirectDebit.GrpHdrNbOfTxs);
end;

procedure TDirectDebitInitiationTests.TestAppendGetAndCountPmtInfEntries;
var
  pi1, pi2, pi3: TDirectDebitPaymentInformation;
begin
  pi1 := TDirectDebitPaymentInformation.Create;
  pi2 := TDirectDebitPaymentInformation.Create;
  pi3 := TDirectDebitPaymentInformation.Create;

  CheckEquals(0, fDirectDebit.PmtInfCount);

  fDirectDebit.AppendPmtInfEntry(pi1);
  CheckEquals(1, fDirectDebit.PmtInfCount);
  Check(pi1 = fDirectDebit.PmtInfEntry[0], 'count 1, entry 0 check');

  fDirectDebit.AppendPmtInfEntry(pi2);
  CheckEquals(2, fDirectDebit.PmtInfCount);
  Check(pi1 = fDirectDebit.PmtInfEntry[0], 'count 2, entry 0 check');
  Check(pi2 = fDirectDebit.PmtInfEntry[1], 'count 2, entry 1 check');

  fDirectDebit.AppendPmtInfEntry(pi3);
  CheckEquals(3, fDirectDebit.PmtInfCount);
  Check(pi1 = fDirectDebit.PmtInfEntry[0], 'count 3, entry 0 check');
  Check(pi2 = fDirectDebit.PmtInfEntry[1], 'count 3, entry 1 check');
  Check(pi3 = fDirectDebit.PmtInfEntry[2], 'count 3, entry 2 check');
end;

procedure TDirectDebitInitiationTests.TestValidate;
var
  fPaymentInfo, fPaymentInfo2: TDirectDebitPaymentInformation;
  fTransaction, fTransaction2: TDirectDebitTransactionInformation;
begin
  // empty object (make sure that no unnecessary "invalid" messages appear)
  CheckValidation([EMPTY_INITG_PTY_NAME, INVALID_NB_OF_TXS], fDirectDebit.Validate);

  // check remaining fields which should not be empty
  fDirectDebit.GrpHdrMsgId := '';
  CheckValidationContains([EMPTY_GRP_HDR_MSG_ID], fDirectDebit.Validate);
  fDirectDebit.GrpHdrMsgId := SEPAGenerateUUID;

  // now generate object with all required fields, but without transactions
  fDirectDebit.GrpHdrInitgPtyName := 'Initiating party name';
  CheckValidation([INVALID_NB_OF_TXS], fDirectDebit.Validate);

  // checks for clean SEPA strings: ID is not automatically cleaned
  fDirectDebit.GrpHdrMsgId := 'ID!';
  CheckValidationContains([Format(INVALID_GRP_HDR_MSG_ID, ['ID!'])], fDirectDebit.Validate);
  fDirectDebit.GrpHdrMsgId := SEPAGenerateUUID;

  // checks for clean SEPA strings: name and remittance information are automatically cleaned
  fDirectDebit.GrpHdrInitgPtyName := 'Initiating party name! Test';
  CheckEquals('Initiating party name  Test', fDirectDebit.GrpHdrInitgPtyName);
  CheckValidation([INVALID_NB_OF_TXS], fDirectDebit.Validate);

  // dummy contents
  fPaymentInfo := TDirectDebitPaymentInformation.Create;
  fPaymentInfo.PmtInfId      := 'PMTINFID';
  fPaymentInfo.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_CORE;
  fPaymentInfo.PmtTpInfSeqTp := SEQ_TP_FRST;
  fPaymentInfo.ReqdColltnDt  := SEPAEarliestCollectionDate(LCL_INSTRM_CD_CORE, SEQ_TP_FRST, SCHEMA_PAIN_008_003_02);
  fPaymentInfo.CdtrNm        := 'Creditor name';
  fPaymentInfo.CdtrAgt.BIC   := 'SOMEFININST';
  fPaymentInfo.CdtrAcct.IBAN := 'DE58123456780123456789';
  fPaymentInfo.CdtrSchmeIdIdPrvtIdOthrId := 'DE98ZZZ09999999999';
  fDirectDebit.AppendPmtInfEntry(fPaymentInfo);

  fTransaction := TDirectDebitTransactionInformation.Create;
  fTransaction.PmtIdEndToEndId := 'END-TO-END';
  fTransaction.InstdAmtCcy     := CCY_EUR;
  fTransaction.InstdAmt        := 0.01;
  fTransaction.DbtrNm          := 'Creditor name';
  fTransaction.DbtrAgt.BIC     := 'SOMEFININST';
  fTransaction.DbtrAcct.IBAN   := 'DE58123456780123456789';
  fTransaction.DrctDbtTxMndtRltdInf.MndtId    := 'MNDTID';
  fTransaction.DrctDbtTxMndtRltdInf.DtOfSgntr := EncodeDate(2016, 9, 1);
  fTransaction.RmtInfUstrd     := 'Remittance information';
  fPaymentInfo.AppendDrctDbtTxInfEntry(fTransaction);

  // check that now valid (with dummy contents)
  CheckValidation([], fDirectDebit.Validate);

  // check detection of mixed payment information contents
  // - create second payment information entry
  fPaymentInfo2 := TDirectDebitPaymentInformation.Create;
  fPaymentInfo2.PmtInfId      := 'PMTINFID';
  fPaymentInfo2.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_CORE;
  fPaymentInfo2.PmtTpInfSeqTp := SEQ_TP_RCUR;
  fPaymentInfo2.ReqdColltnDt  := SEPAEarliestCollectionDate(LCL_INSTRM_CD_CORE, SEQ_TP_FRST, SCHEMA_PAIN_008_003_02);
  fPaymentInfo2.CdtrNm        := 'Creditor name';
  fPaymentInfo2.CdtrAgt.BIC   := 'SOMEFININST';
  fPaymentInfo2.CdtrAcct.IBAN := 'DE58123456780123456789';
  fPaymentInfo2.CdtrSchmeIdIdPrvtIdOthrId := 'DE98ZZZ09999999999';
  fDirectDebit.AppendPmtInfEntry(fPaymentInfo2);
  fTransaction2 := TDirectDebitTransactionInformation.Create;
  fTransaction2.PmtIdEndToEndId := 'END-TO-END';
  fTransaction2.InstdAmtCcy     := CCY_EUR;
  fTransaction2.InstdAmt        := 0.01;
  fTransaction2.DbtrNm          := 'Creditor name';
  fTransaction2.DbtrAgt.BIC     := 'SOMEFININST';
  fTransaction2.DbtrAcct.IBAN   := 'DE58123456780123456789';
  fTransaction2.DrctDbtTxMndtRltdInf.MndtId    := 'MNDTID';
  fTransaction2.DrctDbtTxMndtRltdInf.DtOfSgntr := EncodeDate(2016, 9, 1);
  fTransaction2.RmtInfUstrd     := 'Remittance information';
  fPaymentInfo2.AppendDrctDbtTxInfEntry(fTransaction2);
  // - make sure now it is ok
  CheckValidation([], fDirectDebit.Validate);
  // - now check mixed configurations that are not ok
  fPaymentInfo.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_CORE;
  fPaymentInfo2.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_COR1;
  CheckValidation([INVALID_PMT_INF_MIXING], fDirectDebit.Validate);
  fPaymentInfo.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_CORE;
  fPaymentInfo2.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_B2B;
  CheckValidation([INVALID_PMT_INF_MIXING], fDirectDebit.Validate);
  fPaymentInfo.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_COR1;
  fPaymentInfo2.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_CORE;
  CheckValidation([INVALID_PMT_INF_MIXING], fDirectDebit.Validate);
  fPaymentInfo.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_COR1;
  fPaymentInfo2.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_B2B;
  CheckValidation([INVALID_PMT_INF_MIXING], fDirectDebit.Validate);
  fPaymentInfo.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_B2B;
  fPaymentInfo2.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_CORE;
  CheckValidation([INVALID_PMT_INF_MIXING], fDirectDebit.Validate);
  fPaymentInfo.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_B2B;
  fPaymentInfo2.PmtTpInfLclInstrmCd := LCL_INSTRM_CD_COR1;
  CheckValidation([INVALID_PMT_INF_MIXING], fDirectDebit.Validate);
end;

procedure TDirectDebitInitiationTests.TestSaveToStream;
var
  xmlPmtInfEntry0: RawByteString;
begin
  fDirectDebit.GrpHdrMsgId        := 'MSGID';
  fDirectDebit.GrpHdrCreDtTm      := EncodeDate(2016, 9, 1) + EncodeTime(12, 0, 1, 100);
  fDirectDebit.GrpHdrInitgPtyName := 'Initiating party name';

  fDirectDebit.SaveToStream(SaveStream);

  CheckSaveStream('<?xml version="1.0" encoding="UTF-8"?>'+
                  '<Document xmlns="urn:iso:std:iso:20022:tech:xsd:pain.008.003.02"'+
                  ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'+
                  ' xsi:schemaLocation="urn:iso:std:iso:20022:tech:xsd:pain.008.003.02 pain.008.003.02.xsd">'+
                  '<CstmrDrctDbtInitn>'+
                  '<GrpHdr>'+
                  '<MsgId>MSGID</MsgId>'+
                  '<CreDtTm>2016-09-01T12:00:01.100Z</CreDtTm>'+
                  '<NbOfTxs>0</NbOfTxs>'+
                  '<InitgPty><Nm>Initiating party name</Nm></InitgPty>'+
                  '</GrpHdr>'+
                  '</CstmrDrctDbtInitn>'+
                  '</Document>');

  // test integration of payment information blocks and transactions
  fDirectDebit.AppendPmtInfEntry(TDirectDebitPaymentInformation.Create);
  fDirectDebit.PmtInfEntry[0].AppendDrctDbtTxInfEntry(TDirectDebitTransactionInformation.Create);
  fDirectDebit.PmtInfEntry[0].DrctDbtTxInfEntry[0].PmtIdEndToEndId := 'END-TO-END';
  fDirectDebit.PmtInfEntry[0].DrctDbtTxInfEntry[0].InstdAmt := 0.01;
  fDirectDebit.PmtInfEntry[0].DrctDbtTxInfEntry[0].DbtrAgt.BIC := 'SOMEFININST';
  fDirectDebit.PmtInfEntry[0].DrctDbtTxInfEntry[0].DbtrNm := 'Debtor name';
  fDirectDebit.PmtInfEntry[0].DrctDbtTxInfEntry[0].DbtrAcct.IBAN := 'DE58123456780123456789';
  fDirectDebit.PmtInfEntry[0].DrctDbtTxInfEntry[0].RmtInfUstrd := 'Remittance information';
  fDirectDebit.PmtInfEntry[0].DrctDbtTxInfEntry[0].DrctDbtTxMndtRltdInf.MndtId := 'MNDTID';
  fDirectDebit.PmtInfEntry[0].DrctDbtTxInfEntry[0].DrctDbtTxMndtRltdInf.DtOfSgntr := EncodeDate(2016, 9, 1);

  fDirectDebit.PmtInfEntry[0].SaveToStream(SaveStream, fDirectDebit.Schema);
  xmlPmtInfEntry0 := FetchAndResetSaveStream;
  fDirectDebit.SaveToStream(SaveStream);

  CheckSaveStream('<?xml version="1.0" encoding="UTF-8"?>'+
                  '<Document xmlns="urn:iso:std:iso:20022:tech:xsd:pain.008.003.02"'+
                  ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'+
                  ' xsi:schemaLocation="urn:iso:std:iso:20022:tech:xsd:pain.008.003.02 pain.008.003.02.xsd">'+
                  '<CstmrDrctDbtInitn>'+
                  '<GrpHdr>'+
                  '<MsgId>MSGID</MsgId>'+
                  '<CreDtTm>2016-09-01T12:00:01.100Z</CreDtTm>'+
                  '<NbOfTxs>1</NbOfTxs>'+
                  '<InitgPty><Nm>Initiating party name</Nm></InitgPty>'+
                  '</GrpHdr>'+
                  xmlPmtInfEntry0+
                  '</CstmrDrctDbtInitn>'+
                  '</Document>');
end;

procedure TDirectDebitInitiationTests.TestSaveToDisk;
var
  writtenFile: TStringList;
  fileName: String;
begin
  fDirectDebit.GrpHdrInitgPtyName := 'Initiating party name';
  fDirectDebit.SaveToStream(SaveStream);

  fileName := 'tmp_'+fDirectDebit.GrpHdrMsgId+'.xml';
  try
    fDirectDebit.SaveToDisk(fileName);

    writtenFile := TStringList.Create;
    writtenFile.LoadFromFile(fileName);

    CheckSaveStream(writtenFile.Text);
  finally
    DeleteFile(fileName);
  end;
end;

initialization
  RegisterTest('SEPADirectDebitTests Suite', TAmendmentInformationDetails26Tests.Suite);
  RegisterTest('SEPADirectDebitTests Suite', TAmendmentInformationDetails30Tests.Suite);
  RegisterTest('SEPADirectDebitTests Suite', TMandateRelatedInformationTests.Suite);
  RegisterTest('SEPADirectDebitTests Suite', TDirectDebitTransactionInformationTests.Suite);
  RegisterTest('SEPADirectDebitTests Suite', TDirectDebitPaymentInformationTests.Suite);
  RegisterTest('SEPADirectDebitTests Suite', TDirectDebitInitiationTests.Suite);

end.
 