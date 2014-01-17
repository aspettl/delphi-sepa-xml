//
//   Delphi unit for SEPA direct debit XML file creation - Lazarus example
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
unit ExampleUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, StrUtils, SEPADirectDebit;

type

  { TExampleForm }

  TExampleForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    CreditorAccountBICEdit: TEdit;
    CreditorAccountIBANEdit: TEdit;
    CreditorAccountNOTPROVIDEDCheckBox: TCheckBox;
    CreditorIdentifierEdit: TEdit;
    CreditorNameEdit: TEdit;
    DebtorBICEdit: TEdit;
    DebtorIBANEdit: TEdit;
    DebtorNameEdit: TEdit;
    DebtorNOTPROVIDEDCheckBox: TCheckBox;
    EndToEndIdComboBox: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    InitiatingPartyNameEdit: TEdit;
    InstructedAmountEdit: TEdit;
    ISOSchemaComboBox: TComboBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LocalInstrumentCodeComboBox: TComboBox;
    MandateAmendmentInformationDetailsCheckBox: TCheckBox;
    MandateDateOfSignatureEdit: TDateEdit;
    MandateIdEdit: TEdit;
    OriginalCreditorIdentifierEdit: TEdit;
    OriginalCreditorNameEdit: TEdit;
    OriginalDebtorAccountIBANEdit: TEdit;
    OriginalDebtorFinInstSMNDACheckBox: TCheckBox;
    OriginalMandateIdEdit: TEdit;
    RemittanceInformationMemo: TMemo;
    RequestedCollectionDateEdit: TDateEdit;
    SaveButton: TButton;
    SaveDialog: TSaveDialog;
    SequenceTypeComboBox: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ExampleForm: TExampleForm;

implementation

{$R *.lfm}

{ TExampleForm }

procedure TExampleForm.FormCreate(Sender: TObject);
begin
  ISOSchemaComboBox.Text           := '';
  RequestedCollectionDateEdit.Date := Date;
  InstructedAmountEdit.Text        := FloatToStr(123.45);
  MandateDateOfSignatureEdit.Date  := Date-1;
  RemittanceInformationMemo.Text   := Trim(RemittanceInformationMemo.Text);
end;

procedure TExampleForm.SaveButtonClick(Sender: TObject);
var
  ddi: TDirectDebitInitiation;
  pii: TPaymentInstructionInformation;
  ddti: TDirectDebitTransactionInformation;
  messages: TStringList;
begin
  // note: the GUI elements return strings as UTF8, so we use UTF8Decode below
  //       (the SEPA direct debit unit expects ANSI strings)

  // XML file
  ddi := TDirectDebitInitiation.Create;
  ddi.Schema             := UTF8Decode(ISOSchemaComboBox.Text);
  ddi.GrpHdrInitgPtyName := UTF8Decode(InitiatingPartyNameEdit.Text);

  // payment instruction
  pii := TPaymentInstructionInformation.Create;
  pii.PmtTpInfLclInstrmCd       := UTF8Decode(LocalInstrumentCodeComboBox.Text);
  pii.PmtTpInfSeqTp             := UTF8Decode(SequenceTypeComboBox.Text);
  pii.ReqdColltnDt              := RequestedCollectionDateEdit.Date;
  pii.CdtrNm                    := UTF8Decode(CreditorNameEdit.Text);
  pii.CdtrAcct.IBAN             := UTF8Decode(CreditorAccountIBANEdit.Text);
  pii.CdtrAgt.BIC               := UTF8Decode(CreditorAccountBICEdit.Text);
  pii.CdtrAgt.OthrID            := IfThen(CreditorAccountNOTPROVIDEDCheckBox.Checked, FIN_INSTN_NOTPROVIDED, '');
  pii.CdtrSchmeIdIdPrvtIdOthrId := UTF8Decode(CreditorIdentifierEdit.Text);
  ddi.AppendPmtInfEntry(pii);

  // direct debit transaction (including mandate details)
  ddti := TDirectDebitTransactionInformation.Create;
  ddti.PmtIdEndToEndId := UTF8Decode(EndToEndIdComboBox.Text);
  ddti.InstdAmt        := StrToFloat(InstructedAmountEdit.Text);
  ddti.DbtrNm          := UTF8Decode(DebtorNameEdit.Text);
  ddti.DbtrAcct.IBAN   := UTF8Decode(DebtorIBANEdit.Text);
  ddti.DbtrAgt.BIC     := UTF8Decode(DebtorBICEdit.Text);
  ddti.DbtrAgt.OthrID  := IfThen(DebtorNOTPROVIDEDCheckBox.Checked, FIN_INSTN_NOTPROVIDED, '');
  ddti.RmtInfUstrd     := Trim(UTF8Decode(RemittanceInformationMemo.Text));
  ddti.DrctDbtTxMndtRltdInf.MndtId    := UTF8Decode(MandateIdEdit.Text);
  ddti.DrctDbtTxMndtRltdInf.DtOfSgntr := MandateDateOfSignatureEdit.Date;
  ddti.DrctDbtTxMndtRltdInf.AmdmntInd := MandateAmendmentInformationDetailsCheckBox.Checked;
  ddti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlMndtId                    := UTF8Decode(OriginalMandateIdEdit.Text);
  ddti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlCdtrSchmeIdNm             := UTF8Decode(OriginalCreditorNameEdit.Text);
  ddti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlCdtrSchmeIdIdPrvtIdOthrId := UTF8Decode(OriginalCreditorIdentifierEdit.Text);
  ddti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlDbtrAcct.IBAN             := UTF8Decode(OriginalDebtorAccountIBANEdit.Text);
  ddti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlDbtrAgtFinInstIdOthrId    := IfThen(OriginalDebtorFinInstSMNDACheckBox.Checked, ORGNL_DBTR_AGT_SMNDA, '');
  pii.AppendDrctDbtTxInfEntry(ddti);

  // validate and save
  messages := ddi.Validate;
  if ((messages.Count = 0) or (MessageDlg(UTF8Encode(messages.Text), mtError, [mbOk, mbIgnore], 0) = mrIgnore)) and
      SaveDialog.Execute then
    ddi.SaveToDisk(UTF8Decode(SaveDialog.FileName));
end;

end.

