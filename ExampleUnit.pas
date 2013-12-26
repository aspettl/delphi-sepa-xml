//
//   Delphi unit for SEPA direct debit XML file creation - example
//   (beta version 0.1.0, 2013-12-26)
//
//   Copyright (C) 2013 by Aaron Spettl
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

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SEPADirectDebit, Mask, ComCtrls, ExtCtrls, StrUtils, Math,
  DateUtils;

type
  TExampleForm = class(TForm)
    GroupBox1: TGroupBox;
    SaveButton: TButton;
    SaveDialog: TSaveDialog;
    Label1: TLabel;
    InitiatingPartyNameEdit: TEdit;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    CreditorNameEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    CreditorAccountIBANEdit: TEdit;
    Label7: TLabel;
    CreditorAccountBICEdit: TEdit;
    Label8: TLabel;
    CreditorIdentifierEdit: TEdit;
    RequestedCollectionDateEdit: TDateTimePicker;
    LocalInstrumentCodeComboBox: TComboBox;
    SequenceTypeComboBox: TComboBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    GroupBox3: TGroupBox;
    Label9: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    InstructedAmountEdit: TEdit;
    DebtorIBANEdit: TEdit;
    DebtorBICEdit: TEdit;
    EndToEndIdComboBox: TComboBox;
    Label10: TLabel;
    Label11: TLabel;
    DebtorNameEdit: TEdit;
    GroupBox4: TGroupBox;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label22: TLabel;
    MandateIdEdit: TEdit;
    OriginalDebtorAccountIBANEdit: TEdit;
    OriginalMandateIdEdit: TEdit;
    RemittanceInformationMemo: TMemo;
    Bevel5: TBevel;
    MandateDateOfSignatureEdit: TDateTimePicker;
    MandateAmendmentInformationDetailsCheckBox: TCheckBox;
    OriginalDebtorFinInstSMNDACheckBox: TCheckBox;
    Label16: TLabel;
    OriginalCreditorNameEdit: TEdit;
    Label21: TLabel;
    OriginalCreditorIdentifierEdit: TEdit;
    CreditorAccountNOTPROVIDEDCheckBox: TCheckBox;
    DebtorNOTPROVIDEDCheckBox: TCheckBox;
    Label23: TLabel;
    ISOSchemaComboBox: TComboBox;
    Label24: TLabel;
    GroupBox5: TGroupBox;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  ExampleForm: TExampleForm;

implementation

{$R *.dfm}

procedure TExampleForm.FormCreate(Sender: TObject);
begin
  DecimalSeparator := '.';

  ISOSchemaComboBox.Text           := '';
  RequestedCollectionDateEdit.Date := Today;
  MandateDateOfSignatureEdit.Date  := Today-1;
  RemittanceInformationMemo.Text   := Trim(RemittanceInformationMemo.Text);
end;

procedure TExampleForm.SaveButtonClick(Sender: TObject);
var
  ddi: TDirectDebitInitiation;
  pii: TPaymentInstructionInformation;
  ddti: TDirectDebitTransactionInformation;
  messages: TStringList;
begin
  // XML file
  ddi := TDirectDebitInitiation.Create;
  ddi.Schema             := ISOSchemaComboBox.Text;
  ddi.GrpHdrInitgPtyName := InitiatingPartyNameEdit.Text;

  // payment instruction
  pii := TPaymentInstructionInformation.Create;
  pii.PmtTpInfLclInstrmCd       := LocalInstrumentCodeComboBox.Text;
  pii.PmtTpInfSeqTp             := SequenceTypeComboBox.Text;
  pii.ReqdColltnDt              := RequestedCollectionDateEdit.Date;
  pii.CdtrNm                    := CreditorNameEdit.Text;
  pii.CdtrAcct.IBAN             := CreditorAccountIBANEdit.Text;
  pii.CdtrAgt.BIC               := CreditorAccountBICEdit.Text;
  pii.CdtrAgt.OthrID            := IfThen(CreditorAccountNOTPROVIDEDCheckBox.Checked, FIN_INSTN_NOTPROVIDED, '');
  pii.CdtrSchmeIdIdPrvtIdOthrId := CreditorIdentifierEdit.Text;
  ddi.AppendPmtInfEntry(pii);

  // direct debit transaction (including mandate details)
  ddti := TDirectDebitTransactionInformation.Create;
  ddti.PmtIdEndToEndId := EndToEndIdComboBox.Text;
  ddti.InstdAmt        := StrToFloat(InstructedAmountEdit.Text);
  ddti.DbtrNm          := DebtorNameEdit.Text;
  ddti.DbtrAcct.IBAN   := DebtorIBANEdit.Text;
  ddti.DbtrAgt.BIC     := DebtorBICEdit.Text;
  ddti.DbtrAgt.OthrID  := IfThen(DebtorNOTPROVIDEDCheckBox.Checked, FIN_INSTN_NOTPROVIDED, '');
  ddti.RmtInfUstrd     := Trim(RemittanceInformationMemo.Text);
  ddti.DrctDbtTxMndtRltdInf.MndtId    := MandateIdEdit.Text;
  ddti.DrctDbtTxMndtRltdInf.DtOfSgntr := MandateDateOfSignatureEdit.Date;
  ddti.DrctDbtTxMndtRltdInf.AmdmntInd := MandateAmendmentInformationDetailsCheckBox.Checked;
  ddti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlMndtId                    := OriginalMandateIdEdit.Text;
  ddti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlCdtrSchmeIdNm             := OriginalCreditorNameEdit.Text;
  ddti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlCdtrSchmeIdIdPrvtIdOthrId := OriginalCreditorIdentifierEdit.Text;
  ddti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlDbtrAcct.IBAN             := OriginalDebtorAccountIBANEdit.Text;
  ddti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlDbtrAgtFinInstIdOthrId    := IfThen(OriginalDebtorFinInstSMNDACheckBox.Checked, ORGNL_DBTR_AGT_SMNDA, '');
  pii.AppendDrctDbtTxInfEntry(ddti);

  // validate and save
  messages := ddi.Validate;
  if ((messages.Count = 0) or (MessageDlg(messages.Text, mtError, [mbOk, mbIgnore], 0) = mrIgnore)) and
      SaveDialog.Execute then
    ddi.SaveToDisk(SaveDialog.FileName);
end;

end.
