//
//   Unit tests for "SEPACreditTransfer.pas"
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
unit SEPACreditTransferTests;

{$IFDEF FPC}                // Lazarus: set compiler mode and file encoding
{%encoding CP1252}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SEPACreditTransfer, SEPACommon, SEPATestCase,
  {$IFDEF FPC}
  fpcunit, testutils, testregistry
  {$ELSE}
  TestFrameWork
  {$ENDIF},
  SysUtils, Classes;

type
  TCreditTransferTransactionInformationTests = class(TSEPATestCase)
  private
    fTransaction: TCreditTransferTransactionInformation;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestValidate;
    procedure TestSaveToStream;
  end;

  TCreditTransferPaymentInformationTests = class(TSEPATestCase)
  private
    fPaymentInfo: TCreditTransferPaymentInformation;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestGetCtrlSum;
    procedure TestAppendGetAndCountCdtTrfTxEntries;
    procedure TestValidate;
    procedure TestSaveToStream;
  end;

  TCreditTransferInitiationTests = class(TSEPATestCase)
  private
    fSetUpTime: TDateTime;
    fCreditTransfer: TCreditTransferInitiation;
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

// TCreditTransferTransactionInformationTests

procedure TCreditTransferTransactionInformationTests.SetUp;
begin
  inherited;
  fTransaction := TCreditTransferTransactionInformation.Create;
end;

procedure TCreditTransferTransactionInformationTests.TearDown;
begin
  FreeAndNil(fTransaction);
  inherited;
end;

procedure TCreditTransferTransactionInformationTests.TestCreate;
begin
  CheckNotNull(fTransaction.CdtrAgt);
  CheckNotNull(fTransaction.CdtrAcct);

  CheckEquals(END_TO_END_ID_NOTPROVIDED, fTransaction.PmtIdEndToEndId);
  CheckEquals(CCY_EUR, fTransaction.InstdAmtCcy);
  CheckEquals(0.0, fTransaction.InstdAmt);
  CheckEquals('', fTransaction.CdtrNm);
  CheckEquals('', fTransaction.RmtInfUstrd);
end;

procedure TCreditTransferTransactionInformationTests.TestValidate;
begin
  // fill out creditor IBAN and BIC, their validation is tested in the respective classes
  fTransaction.CdtrAgt.BIC   := 'SOMEFININST';
  fTransaction.CdtrAcct.IBAN := 'DE58123456780123456789';

  // empty object (make sure that no unnecessary "invalid" messages appear)
  CheckValidation([EMPTY_CDTR_NM, EMPTY_RMT_INF_USTRD, Format(INVALID_INSTD_AMT, ['0.0000'])], fTransaction.Validate(SCHEMA_PAIN_001_003_03));

  // check remaining fields which should not be empty
  fTransaction.PmtIdEndToEndId := '';
  CheckValidationContains([EMPTY_END_TO_END_ID], fTransaction.Validate(SCHEMA_PAIN_001_003_03));
  fTransaction.InstdAmtCcy     := '';
  CheckValidationContains([EMPTY_INSTD_AMT_CCY], fTransaction.Validate(SCHEMA_PAIN_001_003_03));

  // now generate object with all required fields
  fTransaction.PmtIdEndToEndId := 'END-TO-END';
  fTransaction.InstdAmtCcy     := CCY_EUR;
  fTransaction.InstdAmt        := 0.01;
  fTransaction.CdtrNm          := 'Creditor name';
  fTransaction.RmtInfUstrd     := 'Remittance information';
  CheckValidation([], fTransaction.Validate(SCHEMA_PAIN_001_003_03));

  // checks for clean SEPA strings: End-to-End ID is not automatically cleaned
  fTransaction.PmtIdEndToEndId := 'END-TO-END!';
  CheckValidationContains([Format(INVALID_END_TO_END_ID, ['END-TO-END!'])], fTransaction.Validate(SCHEMA_PAIN_001_003_03));
  fTransaction.PmtIdEndToEndId := 'END-TO-END';

  // checks for clean SEPA strings: name and remittance information are automatically cleaned
  fTransaction.CdtrNm          := 'Creditor name! Test';
  CheckEquals('Creditor name  Test', fTransaction.CdtrNm);
  CheckValidation([], fTransaction.Validate(SCHEMA_PAIN_001_003_03));

  fTransaction.RmtInfUstrd     := 'Remittance information! Test';
  CheckEquals('Remittance information  Test', fTransaction.RmtInfUstrd);
  CheckValidation([], fTransaction.Validate(SCHEMA_PAIN_001_003_03));

  // check for negative instructed amount
  fTransaction.InstdAmt        := -0.01;
  CheckValidationContains([Format(INVALID_INSTD_AMT, ['-0.0100'])], fTransaction.Validate(SCHEMA_PAIN_001_003_03));
  fTransaction.InstdAmt        := -0.00;
  CheckValidationContains([Format(INVALID_INSTD_AMT, ['0.0000'])], fTransaction.Validate(SCHEMA_PAIN_001_003_03));

  // check for instructed amount with more three or four decimal places
  fTransaction.InstdAmt        := 0.011;
  CheckValidationContains([Format(INVALID_INSTD_AMT, ['0.0110'])], fTransaction.Validate(SCHEMA_PAIN_001_003_03));
  fTransaction.InstdAmt        := 0.0101;
  CheckValidationContains([Format(INVALID_INSTD_AMT, ['0.0101'])], fTransaction.Validate(SCHEMA_PAIN_001_003_03));

  // check for instructed amount: decimal places at position 5 and higher are irrelevant
  fTransaction.InstdAmt        := 0.01001;
  CheckValidation([], fTransaction.Validate(SCHEMA_PAIN_001_003_03));

  // test that creditor agent is validated
  fTransaction.CdtrAgt.BIC     := '';
  CheckValidationContains([EMPTY_BIC_OTHR_ID], fTransaction.Validate(SCHEMA_PAIN_001_003_03));
  fTransaction.CdtrAgt.BIC     := 'SOMEFININST';

  // test that creditor account is validated
  fTransaction.CdtrAcct.IBAN   := '';
  CheckValidationContains([EMPTY_IBAN], fTransaction.Validate(SCHEMA_PAIN_001_003_03));
  fTransaction.CdtrAcct.IBAN   := 'DE58123456780123456789';

  // check that IBAN-only only allowed for German accounts
  fTransaction.CdtrAgt.BIC     := '';
  fTransaction.CdtrAgt.OthrID  := FIN_INSTN_NOTPROVIDED;
  CheckValidation([], fTransaction.Validate(SCHEMA_PAIN_001_003_03));
  fTransaction.CdtrAcct.IBAN   := 'CH';
  CheckValidationContains([INVALID_IBAN_NOT_DE], fTransaction.Validate(SCHEMA_PAIN_001_003_03));
end;

procedure TCreditTransferTransactionInformationTests.TestSaveToStream;
begin
  fTransaction.PmtIdEndToEndId := 'END-TO-END';
  fTransaction.InstdAmtCcy     := CCY_EUR;
  fTransaction.InstdAmt        := 0.01;
  fTransaction.CdtrNm          := 'Creditor name';
  fTransaction.CdtrAgt.BIC     := 'SOMEFININST';
  fTransaction.CdtrAcct.IBAN   := 'DE58123456780123456789';
  fTransaction.RmtInfUstrd     := 'Remittance information';

  fTransaction.SaveToStream(SaveStream, SCHEMA_PAIN_001_003_03);

  CheckSaveStream('<CdtTrfTxInf>'+
                  '<PmtId><EndToEndId>END-TO-END</EndToEndId></PmtId>'+
                  '<Amt><InstdAmt Ccy="EUR">0.01</InstdAmt></Amt>'+
                  '<CdtrAgt>'+
                  '<FinInstnId><BIC>SOMEFININST</BIC></FinInstnId>'+
                  '</CdtrAgt>'+
                  '<Cdtr><Nm>Creditor name</Nm></Cdtr>'+
                  '<CdtrAcct>'+
                  '<Id><IBAN>DE58123456780123456789</IBAN></Id>'+
                  '</CdtrAcct>'+
                  '<RmtInf><Ustrd>Remittance information</Ustrd></RmtInf>'+
                  '</CdtTrfTxInf>');

  // now IBAN-only
  fTransaction.CdtrAgt.BIC := '';
  fTransaction.CdtrAgt.OthrID := FIN_INSTN_NOTPROVIDED;

  fTransaction.SaveToStream(SaveStream, SCHEMA_PAIN_001_003_03);

  CheckSaveStream('<CdtTrfTxInf>'+
                  '<PmtId><EndToEndId>END-TO-END</EndToEndId></PmtId>'+
                  '<Amt><InstdAmt Ccy="EUR">0.01</InstdAmt></Amt>'+
                  '<Cdtr><Nm>Creditor name</Nm></Cdtr>'+
                  '<CdtrAcct>'+
                  '<Id><IBAN>DE58123456780123456789</IBAN></Id>'+
                  '</CdtrAcct>'+
                  '<RmtInf><Ustrd>Remittance information</Ustrd></RmtInf>'+
                  '</CdtTrfTxInf>');
end;

// TCreditTransferPaymentInformationTests

procedure TCreditTransferPaymentInformationTests.SetUp;
begin
  inherited;
  fPaymentInfo := TCreditTransferPaymentInformation.Create;
end;

procedure TCreditTransferPaymentInformationTests.TearDown;
begin
  FreeAndNil(fPaymentInfo);
  inherited;
end;

procedure TCreditTransferPaymentInformationTests.TestCreate;
begin
  CheckNotNull(fPaymentInfo.DbtrAcct);
  CheckNotNull(fPaymentInfo.DbtrAgt);

  CheckNotEquals(0, Length(fPaymentInfo.PmtInfId));
  CheckEquals(PMT_MTD_CREDIT_TRANSFER, fPaymentInfo.PmtMtd);
  CheckEquals(SEPA, fPaymentInfo.PmtTpInfSvcLvlCd);
  CheckEquals('', fPaymentInfo.PmtTpInfInstrPrty);
  CheckEquals(0, fPaymentInfo.ReqdExctnDt);
  CheckEquals('', fPaymentInfo.DbtrNm);
  CheckEquals(CHRG_BR_SLEV, fPaymentInfo.ChrgBr);

  CheckEquals(0, fPaymentInfo.CdtTrfTxInfCount);
end;

procedure TCreditTransferPaymentInformationTests.TestGetCtrlSum;
var
  ti: TCreditTransferTransactionInformation;
  c: Currency;
  f: Double;
begin
  CheckEquals(0.0, fPaymentInfo.CtrlSum);

  // choose very large value such that we have less significant decimal places
  // for double floating-point values
  c := 100000000000000.0;
  f := 100000000000000.0;

  ti := TCreditTransferTransactionInformation.Create;
  ti.InstdAmt := 100000000000000.0;
  fPaymentInfo.AppendCdtTrfTxInfEntry(ti);

  Check(fPaymentInfo.CtrlSum = c,
        Format('expected value was %s, but actual value is %s',
               [SEPAFormatAmount(c), SEPAFormatAmount(fPaymentInfo.CtrlSum)]));

  c := c+0.01; // now contains: 100000000000000.01
  f := f+0.01; // now contains: 100000000000000.02

  ti := TCreditTransferTransactionInformation.Create;
  ti.InstdAmt := 0.01;
  fPaymentInfo.AppendCdtTrfTxInfEntry(ti);

  Check(fPaymentInfo.CtrlSum = c,
        Format('expected value was %s, but actual value is %s (note: wrong '+
               'double addition gives %s)',
               [SEPAFormatAmount(c), SEPAFormatAmount(fPaymentInfo.CtrlSum),
                SEPAFormatAmount(f)]));
end;

procedure TCreditTransferPaymentInformationTests.TestAppendGetAndCountCdtTrfTxEntries;
var
  ti1, ti2, ti3: TCreditTransferTransactionInformation;
begin
  ti1 := TCreditTransferTransactionInformation.Create;
  ti2 := TCreditTransferTransactionInformation.Create;
  ti3 := TCreditTransferTransactionInformation.Create;

  CheckEquals(0, fPaymentInfo.CdtTrfTxInfCount);

  fPaymentInfo.AppendCdtTrfTxInfEntry(ti1);
  CheckEquals(1, fPaymentInfo.CdtTrfTxInfCount);
  Check(ti1 = fPaymentInfo.CdtTrfTxInfEntry[0], 'count 1, entry 0 check');

  fPaymentInfo.AppendCdtTrfTxInfEntry(ti2);
  CheckEquals(2, fPaymentInfo.CdtTrfTxInfCount);
  Check(ti1 = fPaymentInfo.CdtTrfTxInfEntry[0], 'count 2, entry 0 check');
  Check(ti2 = fPaymentInfo.CdtTrfTxInfEntry[1], 'count 2, entry 1 check');

  fPaymentInfo.AppendCdtTrfTxInfEntry(ti3);
  CheckEquals(3, fPaymentInfo.CdtTrfTxInfCount);
  Check(ti1 = fPaymentInfo.CdtTrfTxInfEntry[0], 'count 3, entry 0 check');
  Check(ti2 = fPaymentInfo.CdtTrfTxInfEntry[1], 'count 3, entry 1 check');
  Check(ti3 = fPaymentInfo.CdtTrfTxInfEntry[2], 'count 3, entry 2 check');
end;

procedure TCreditTransferPaymentInformationTests.TestValidate;
begin
  // fill out debtor IBAN and BIC, their validation is tested in the respective classes
  fPaymentInfo.DbtrAgt.BIC   := 'SOMEFININST';
  fPaymentInfo.DbtrAcct.IBAN := 'DE58123456780123456789';

  // empty object (make sure that no unnecessary "invalid" messages appear)
  CheckValidation([EMPTY_DBTR_NM, Format(INVALID_REQD_EXCTN_DT, [DateToStr(0)])], fPaymentInfo.Validate(SCHEMA_PAIN_001_003_03));

  // check remaining fields which should not be empty
  fPaymentInfo.PmtInfId := '';
  CheckValidationContains([EMPTY_PMT_INF_ID], fPaymentInfo.Validate(SCHEMA_PAIN_001_003_03));
  fPaymentInfo.PmtInfId := SEPAGenerateUUID;

  // now generate object with all required fields
  fPaymentInfo.ReqdExctnDt := Now;
  fPaymentInfo.DbtrNm      := 'Debtor name';
  CheckValidation([], fPaymentInfo.Validate(SCHEMA_PAIN_001_003_03));

  // checks for clean SEPA strings: ID is not automatically cleaned
  fPaymentInfo.PmtInfId := 'ID!';
  CheckValidationContains([Format(INVALID_PMT_INF_ID, ['ID!'])], fPaymentInfo.Validate(SCHEMA_PAIN_001_003_03));
  fPaymentInfo.PmtInfId := SEPAGenerateUUID;

  // checks for clean SEPA strings: name is automatically cleaned
  fPaymentInfo.DbtrNm := 'Debtor name! Test';
  CheckEquals('Debtor name  Test', fPaymentInfo.DbtrNm);
  CheckValidation([], fPaymentInfo.Validate(SCHEMA_PAIN_001_003_03));

  // check requested execution date
  fPaymentInfo.ReqdExctnDt := Now-1;
  CheckValidationContains([Format(INVALID_REQD_EXCTN_DT, [DateToStr(Now-1)])], fPaymentInfo.Validate(SCHEMA_PAIN_001_003_03));
  fPaymentInfo.ReqdExctnDt := Now;

  // check that debtor account and agent are validated
  fPaymentInfo.DbtrAgt.BIC   := '';
  fPaymentInfo.DbtrAcct.IBAN := 'DE58123456780123456788';
  CheckValidation([Format(INVALID_IBAN, ['DE58123456780123456788']), EMPTY_BIC_OTHR_ID], fPaymentInfo.Validate(SCHEMA_PAIN_001_003_03));
  fPaymentInfo.DbtrAgt.BIC   := 'SOMEFININST';
  fPaymentInfo.DbtrAcct.IBAN := 'DE58123456780123456789';

  // check that transactions are validated
  CheckEquals('', fPaymentInfo.Validate(SCHEMA_PAIN_001_003_03).Text);
  fPaymentInfo.AppendCdtTrfTxInfEntry(TCreditTransferTransactionInformation.Create);
  CheckNotEquals('', fPaymentInfo.Validate(SCHEMA_PAIN_001_003_03).Text);

  // check that debtor has a German account
  fPaymentInfo.DbtrAcct.IBAN := 'CH';
  CheckValidationContains([INVALID_DBTR_ACCT_NOT_DE], fPaymentInfo.Validate(SCHEMA_PAIN_001_003_03));
end;

procedure TCreditTransferPaymentInformationTests.TestSaveToStream;
var
  xmlTxInfEntry0: RawByteString;
begin
  fPaymentInfo.PmtInfId      := 'PMTINFID';
  fPaymentInfo.ReqdExctnDt   := EncodeDate(2014, 2, 1);
  fPaymentInfo.DbtrNm        := 'Debtor name';
  fPaymentInfo.DbtrAgt.BIC   := 'SOMEFININST';
  fPaymentInfo.DbtrAcct.IBAN := 'DE58123456780123456789';

  fPaymentInfo.SaveToStream(SaveStream, SCHEMA_PAIN_001_003_03);

  CheckSaveStream('<PmtInf>'+
                  '<PmtInfId>PMTINFID</PmtInfId>'+
                  '<PmtMtd>TRF</PmtMtd>'+
                  '<NbOfTxs>0</NbOfTxs>'+
                  '<CtrlSum>0.00</CtrlSum>'+
                  '<PmtTpInf>'+
                  '<SvcLvl><Cd>SEPA</Cd></SvcLvl>'+
                  '</PmtTpInf>'+
                  '<ReqdExctnDt>2014-02-01</ReqdExctnDt>'+
                  '<Dbtr><Nm>Debtor name</Nm></Dbtr>'+
                  '<DbtrAcct>'+
                  '<Id><IBAN>DE58123456780123456789</IBAN></Id>'+
                  '</DbtrAcct>'+
                  '<DbtrAgt>'+
                  '<FinInstnId><BIC>SOMEFININST</BIC></FinInstnId>'+
                  '</DbtrAgt>'+
                  '<ChrgBr>SLEV</ChrgBr>'+
                  '</PmtInf>');

  fPaymentInfo.PmtTpInfInstrPrty := 'NORM';
  fPaymentInfo.SaveToStream(SaveStream, SCHEMA_PAIN_001_003_03);
  fPaymentInfo.PmtTpInfInstrPrty := '';

  CheckSaveStream('<PmtInf>'+
                  '<PmtInfId>PMTINFID</PmtInfId>'+
                  '<PmtMtd>TRF</PmtMtd>'+
                  '<NbOfTxs>0</NbOfTxs>'+
                  '<CtrlSum>0.00</CtrlSum>'+
                  '<PmtTpInf>'+
                  '<InstrPrty>NORM</InstrPrty>'+
                  '<SvcLvl><Cd>SEPA</Cd></SvcLvl>'+
                  '</PmtTpInf>'+
                  '<ReqdExctnDt>2014-02-01</ReqdExctnDt>'+
                  '<Dbtr><Nm>Debtor name</Nm></Dbtr>'+
                  '<DbtrAcct>'+
                  '<Id><IBAN>DE58123456780123456789</IBAN></Id>'+
                  '</DbtrAcct>'+
                  '<DbtrAgt>'+
                  '<FinInstnId><BIC>SOMEFININST</BIC></FinInstnId>'+
                  '</DbtrAgt>'+
                  '<ChrgBr>SLEV</ChrgBr>'+
                  '</PmtInf>');

  // test integration of transactions
  fPaymentInfo.AppendCdtTrfTxInfEntry(TCreditTransferTransactionInformation.Create);
  fPaymentInfo.CdtTrfTxInfEntry[0].PmtIdEndToEndId := 'END-TO-END';
  fPaymentInfo.CdtTrfTxInfEntry[0].InstdAmt := 0.01;
  fPaymentInfo.CdtTrfTxInfEntry[0].CdtrAgt.BIC := 'SOMEFININST';
  fPaymentInfo.CdtTrfTxInfEntry[0].CdtrNm := 'Creditor name';
  fPaymentInfo.CdtTrfTxInfEntry[0].CdtrAcct.IBAN := 'DE58123456780123456789';
  fPaymentInfo.CdtTrfTxInfEntry[0].RmtInfUstrd := 'Remittance information';

  fPaymentInfo.CdtTrfTxInfEntry[0].SaveToStream(SaveStream, SCHEMA_PAIN_001_003_03);
  xmlTxInfEntry0 := FetchAndResetSaveStream;
  fPaymentInfo.SaveToStream(SaveStream, SCHEMA_PAIN_001_003_03);

  CheckSaveStream('<PmtInf>'+
                  '<PmtInfId>PMTINFID</PmtInfId>'+
                  '<PmtMtd>TRF</PmtMtd>'+
                  '<NbOfTxs>1</NbOfTxs>'+
                  '<CtrlSum>0.01</CtrlSum>'+
                  '<PmtTpInf>'+
                  '<SvcLvl><Cd>SEPA</Cd></SvcLvl>'+
                  '</PmtTpInf>'+
                  '<ReqdExctnDt>2014-02-01</ReqdExctnDt>'+
                  '<Dbtr><Nm>Debtor name</Nm></Dbtr>'+
                  '<DbtrAcct>'+
                  '<Id><IBAN>DE58123456780123456789</IBAN></Id>'+
                  '</DbtrAcct>'+
                  '<DbtrAgt>'+
                  '<FinInstnId><BIC>SOMEFININST</BIC></FinInstnId>'+
                  '</DbtrAgt>'+
                  '<ChrgBr>SLEV</ChrgBr>'+
                  xmlTxInfEntry0+
                  '</PmtInf>');
end;

// TCreditTransferInitiationTests

procedure TCreditTransferInitiationTests.SetUp;
begin
  fSetUpTime := Now;
  inherited;
  fCreditTransfer := TCreditTransferInitiation.Create;
end;

procedure TCreditTransferInitiationTests.TearDown;
begin
  FreeAndNil(fCreditTransfer);
  inherited;
end;

procedure TCreditTransferInitiationTests.TestCreate;
begin
  CheckEquals(SCHEMA_PAIN_001_001_03, fCreditTransfer.Schema);  // internal fSchema should be empty, but we cannot see that here

  CheckNotEquals(0, Length(fCreditTransfer.GrpHdrMsgId));
  Check((fCreditTransfer.GrpHdrCreDtTm >= fSetUpTime) and (fCreditTransfer.GrpHdrCreDtTm <= Now), 'Timestamp of created file must be between test setup and now');
  CheckEquals('', fCreditTransfer.GrpHdrInitgPtyName);

  CheckEquals(0, fCreditTransfer.PmtInfCount);
end;

procedure TCreditTransferInitiationTests.TestGetAndSetSchema;
begin
  // empty schema and no contents => should use standard version 3.0
  CheckEquals(SCHEMA_PAIN_001_001_03, fCreditTransfer.Schema);

  // set schema manually => should return that one
  fCreditTransfer.Schema := SCHEMA_PAIN_001_002_03;
  CheckEquals(SCHEMA_PAIN_001_002_03, fCreditTransfer.Schema);
  fCreditTransfer.Schema := SCHEMA_PAIN_001_003_03;
  CheckEquals(SCHEMA_PAIN_001_003_03, fCreditTransfer.Schema);
end;

procedure TCreditTransferInitiationTests.TestGetGrpHdrNbOfTxs;
var
  pi: TCreditTransferPaymentInformation;
begin
  // empty: no transactions
  CheckEquals(0, fCreditTransfer.GrpHdrNbOfTxs);

  // add a payment information object, but without transactions
  pi := TCreditTransferPaymentInformation.Create;
  fCreditTransfer.AppendPmtInfEntry(pi);
  CheckEquals(0, fCreditTransfer.GrpHdrNbOfTxs);

  // add a transaction to the existing payment information object
  pi.AppendCdtTrfTxInfEntry(TCreditTransferTransactionInformation.Create);
  CheckEquals(1, fCreditTransfer.GrpHdrNbOfTxs);

  // add another transaction to the existing payment information object
  pi.AppendCdtTrfTxInfEntry(TCreditTransferTransactionInformation.Create);
  CheckEquals(2, fCreditTransfer.GrpHdrNbOfTxs);

  // add another payment information object, but without transactions
  pi := TCreditTransferPaymentInformation.Create;
  fCreditTransfer.AppendPmtInfEntry(pi);
  CheckEquals(2, fCreditTransfer.GrpHdrNbOfTxs);

  // add a transaction to the previously generated payment information object
  pi.AppendCdtTrfTxInfEntry(TCreditTransferTransactionInformation.Create);
  CheckEquals(3, fCreditTransfer.GrpHdrNbOfTxs);
end;

procedure TCreditTransferInitiationTests.TestAppendGetAndCountPmtInfEntries;
var
  pi1, pi2, pi3: TCreditTransferPaymentInformation;
begin
  pi1 := TCreditTransferPaymentInformation.Create;
  pi2 := TCreditTransferPaymentInformation.Create;
  pi3 := TCreditTransferPaymentInformation.Create;

  CheckEquals(0, fCreditTransfer.PmtInfCount);

  fCreditTransfer.AppendPmtInfEntry(pi1);
  CheckEquals(1, fCreditTransfer.PmtInfCount);
  Check(pi1 = fCreditTransfer.PmtInfEntry[0], 'count 1, entry 0 check');

  fCreditTransfer.AppendPmtInfEntry(pi2);
  CheckEquals(2, fCreditTransfer.PmtInfCount);
  Check(pi1 = fCreditTransfer.PmtInfEntry[0], 'count 2, entry 0 check');
  Check(pi2 = fCreditTransfer.PmtInfEntry[1], 'count 2, entry 1 check');

  fCreditTransfer.AppendPmtInfEntry(pi3);
  CheckEquals(3, fCreditTransfer.PmtInfCount);
  Check(pi1 = fCreditTransfer.PmtInfEntry[0], 'count 3, entry 0 check');
  Check(pi2 = fCreditTransfer.PmtInfEntry[1], 'count 3, entry 1 check');
  Check(pi3 = fCreditTransfer.PmtInfEntry[2], 'count 3, entry 2 check');
end;

procedure TCreditTransferInitiationTests.TestValidate;
var
  fPaymentInfo: TCreditTransferPaymentInformation;
  fTransaction: TCreditTransferTransactionInformation;
begin
  // empty object (make sure that no unnecessary "invalid" messages appear)
  CheckValidation([EMPTY_INITG_PTY_NAME, INVALID_NB_OF_TXS], fCreditTransfer.Validate);

  // check remaining fields which should not be empty
  fCreditTransfer.GrpHdrMsgId := '';
  CheckValidationContains([EMPTY_GRP_HDR_MSG_ID], fCreditTransfer.Validate);
  fCreditTransfer.GrpHdrMsgId := SEPAGenerateUUID;

  // now generate object with all required fields, but without transactions
  fCreditTransfer.GrpHdrInitgPtyName := 'Initiating party name';
  CheckValidation([INVALID_NB_OF_TXS], fCreditTransfer.Validate);

  // checks for clean SEPA strings: ID is not automatically cleaned
  fCreditTransfer.GrpHdrMsgId := 'ID!';
  CheckValidationContains([Format(INVALID_GRP_HDR_MSG_ID, ['ID!'])], fCreditTransfer.Validate);
  fCreditTransfer.GrpHdrMsgId := SEPAGenerateUUID;

  // checks for clean SEPA strings: name and remittance information are automatically cleaned
  fCreditTransfer.GrpHdrInitgPtyName := 'Initiating party name! Test';
  CheckEquals('Initiating party name  Test', fCreditTransfer.GrpHdrInitgPtyName);
  CheckValidation([INVALID_NB_OF_TXS], fCreditTransfer.Validate);

  // dummy contents
  fPaymentInfo := TCreditTransferPaymentInformation.Create;
  fPaymentInfo.ReqdExctnDt   := Now;
  fPaymentInfo.DbtrNm        := 'Debtor name';
  fPaymentInfo.DbtrAgt.BIC   := 'SOMEFININST';
  fPaymentInfo.DbtrAcct.IBAN := 'DE58123456780123456789';
  fCreditTransfer.AppendPmtInfEntry(fPaymentInfo);

  fTransaction := TCreditTransferTransactionInformation.Create;
  fTransaction.PmtIdEndToEndId := 'END-TO-END';
  fTransaction.InstdAmtCcy     := CCY_EUR;
  fTransaction.InstdAmt        := 0.01;
  fTransaction.CdtrNm          := 'Creditor name';
  fTransaction.CdtrAgt.BIC     := 'SOMEFININST';
  fTransaction.CdtrAcct.IBAN   := 'DE58123456780123456789';
  fTransaction.RmtInfUstrd     := 'Remittance information';
  fPaymentInfo.AppendCdtTrfTxInfEntry(fTransaction);

  // check that now valid (with dummy contents)
  CheckValidation([], fCreditTransfer.Validate);
end;

procedure TCreditTransferInitiationTests.TestSaveToStream;
var
  xmlPmtInfEntry0: RawByteString;
begin
  fCreditTransfer.GrpHdrMsgId        := 'MSGID';
  fCreditTransfer.GrpHdrCreDtTm      := EncodeDate(2014, 2, 1) + EncodeTime(12, 0, 1, 100);
  fCreditTransfer.GrpHdrInitgPtyName := 'Initiating party name';

  fCreditTransfer.SaveToStream(SaveStream);

  CheckSaveStream('<?xml version="1.0" encoding="UTF-8"?>'+
                  '<Document xmlns="urn:iso:std:iso:20022:tech:xsd:pain.001.001.03"'+
                  ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'+
                  ' xsi:schemaLocation="urn:iso:std:iso:20022:tech:xsd:pain.001.001.03 pain.001.001.03.xsd">'+
                  '<CstmrCdtTrfInitn>'+
                  '<GrpHdr>'+
                  '<MsgId>MSGID</MsgId>'+
                  '<CreDtTm>2014-02-01T12:00:01.100Z</CreDtTm>'+
                  '<NbOfTxs>0</NbOfTxs>'+
                  '<CtrlSum>0.00</CtrlSum>'+
                  '<InitgPty><Nm>Initiating party name</Nm></InitgPty>'+
                  '</GrpHdr>'+
                  '</CstmrCdtTrfInitn>'+
                  '</Document>');

  // test integration of payment information blocks and transactions
  fCreditTransfer.AppendPmtInfEntry(TCreditTransferPaymentInformation.Create);
  fCreditTransfer.PmtInfEntry[0].PmtInfId      := 'PMTINFID';
  fCreditTransfer.PmtInfEntry[0].ReqdExctnDt   := EncodeDate(2014, 2, 1);
  fCreditTransfer.PmtInfEntry[0].DbtrNm        := 'Debtor name';
  fCreditTransfer.PmtInfEntry[0].DbtrAgt.BIC   := 'SOMEFININST';
  fCreditTransfer.PmtInfEntry[0].DbtrAcct.IBAN := 'DE58123456780123456789';
  fCreditTransfer.PmtInfEntry[0].AppendCdtTrfTxInfEntry(TCreditTransferTransactionInformation.Create);
  fCreditTransfer.PmtInfEntry[0].CdtTrfTxInfEntry[0].PmtIdEndToEndId := 'END-TO-END';
  fCreditTransfer.PmtInfEntry[0].CdtTrfTxInfEntry[0].InstdAmt := 0.01;
  fCreditTransfer.PmtInfEntry[0].CdtTrfTxInfEntry[0].CdtrAgt.BIC := 'SOMEFININST';
  fCreditTransfer.PmtInfEntry[0].CdtTrfTxInfEntry[0].CdtrNm := 'Creditor name';
  fCreditTransfer.PmtInfEntry[0].CdtTrfTxInfEntry[0].CdtrAcct.IBAN := 'DE58123456780123456789';
  fCreditTransfer.PmtInfEntry[0].CdtTrfTxInfEntry[0].RmtInfUstrd := 'Remittance information';

  fCreditTransfer.PmtInfEntry[0].SaveToStream(SaveStream, fCreditTransfer.Schema);
  xmlPmtInfEntry0 := FetchAndResetSaveStream;
  fCreditTransfer.SaveToStream(SaveStream);

  CheckSaveStream('<?xml version="1.0" encoding="UTF-8"?>'+
                  '<Document xmlns="urn:iso:std:iso:20022:tech:xsd:pain.001.001.03"'+
                  ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'+
                  ' xsi:schemaLocation="urn:iso:std:iso:20022:tech:xsd:pain.001.001.03 pain.001.001.03.xsd">'+
                  '<CstmrCdtTrfInitn>'+
                  '<GrpHdr>'+
                  '<MsgId>MSGID</MsgId>'+
                  '<CreDtTm>2014-02-01T12:00:01.100Z</CreDtTm>'+
                  '<NbOfTxs>1</NbOfTxs>'+
                  '<CtrlSum>0.01</CtrlSum>'+
                  '<InitgPty><Nm>Initiating party name</Nm></InitgPty>'+
                  '</GrpHdr>'+
                  xmlPmtInfEntry0+
                  '</CstmrCdtTrfInitn>'+
                  '</Document>');
end;

procedure TCreditTransferInitiationTests.TestSaveToDisk;
var
  writtenFile: TStringList;
  fileName: String;
begin
  fCreditTransfer.GrpHdrInitgPtyName := 'Initiating party name';
  fCreditTransfer.SaveToStream(SaveStream);

  fileName := 'tmp_'+fCreditTransfer.GrpHdrMsgId+'.xml';
  try
    fCreditTransfer.SaveToDisk(fileName);

    writtenFile := TStringList.Create;
    writtenFile.LoadFromFile(fileName);

    CheckSaveStream(writtenFile.Text);
  finally
    DeleteFile(fileName);
  end;
end;

initialization
  RegisterTest('SEPACreditTransferTests Suite', TCreditTransferTransactionInformationTests.Suite);
  RegisterTest('SEPACreditTransferTests Suite', TCreditTransferPaymentInformationTests.Suite);
  RegisterTest('SEPACreditTransferTests Suite', TCreditTransferInitiationTests.Suite);

end.
 
