//
//   Lazarus example for SEPA XML file creation
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
unit ExampleUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, ComCtrls, StrUtils, SEPACommon, SEPACreditTransfer,
  SEPADirectDebit;

type

  { TExampleForm }

  TExampleForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    CT_CreditorBIC_Edit: TEdit;
    CT_CreditorIBAN_Edit: TEdit;
    CT_CreditorBICNotProvided_CheckBox: TCheckBox;
    CT_CreditorName_Edit: TEdit;
    CT_DebtorBIC_Edit: TEdit;
    CT_DebtorIBAN_Edit: TEdit;
    CT_DebtorName_Edit: TEdit;
    CT_DebtorBICNotProvided_CheckBox: TCheckBox;
    CT_EndToEndId_ComboBox: TComboBox;
    CT_InitiatingPartyName_Edit: TEdit;
    CT_InstructedAmount_Edit: TEdit;
    CT_ISOSchema_ComboBox: TComboBox;
    CT_PaymentInformation_GroupBox: TGroupBox;
    CT_RemittanceInformation_Memo: TMemo;
    CT_Save_Button: TButton;
    CT_SupportGermanSpecialChars_CheckBox: TCheckBox;
    CT_Transaction_GroupBox: TGroupBox;
    CT_XMLFile_GroupBox: TGroupBox;
    DD_CreditorBIC_Edit: TEdit;
    DD_CreditorIBAN_Edit: TEdit;
    DD_CreditorBICNotProvided_CheckBox: TCheckBox;
    DD_CreditorIdentifier_Edit: TEdit;
    DD_CreditorName_Edit: TEdit;
    DD_DebtorBIC_Edit: TEdit;
    DD_DebtorIBAN_Edit: TEdit;
    DD_DebtorName_Edit: TEdit;
    DD_DebtorBICNotProvided_CheckBox: TCheckBox;
    DD_EndToEndId_ComboBox: TComboBox;
    CT_RequestedExecutionDate_Edit: TDateEdit;
    DD_Tips_GroupBox1: TGroupBox;
    DD_XMLFile_GroupBox: TGroupBox;
    DD_PaymentInformation_GroupBox: TGroupBox;
    DD_Transaction_GroupBox: TGroupBox;
    DD_Mandate_GroupBox: TGroupBox;
    DD_Tips_GroupBox: TGroupBox;
    DD_InitiatingPartyName_Edit: TEdit;
    DD_InstructedAmount_Edit: TEdit;
    DD_ISOSchema_ComboBox: TComboBox;
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
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label50: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    DD_LocalInstrumentCode_ComboBox: TComboBox;
    DD_MandateAmendmentInformationDetails_CheckBox: TCheckBox;
    DD_MandateDateOfSignature_Edit: TDateEdit;
    DD_MandateId_Edit: TEdit;
    DD_OriginalCreditorIdentifier_Edit: TEdit;
    DD_OriginalCreditorName_Edit: TEdit;
    DD_OriginalDebtorAccountIBAN_Edit: TEdit;
    DD_OriginalDebtorFinInstSMNDA_CheckBox: TCheckBox;
    DD_OriginalMandateId_Edit: TEdit;
    PageControl: TPageControl;
    DD_RemittanceInformation_Memo: TMemo;
    DD_RequestedCollectionDate_Edit: TDateEdit;
    DD_Save_Button: TButton;
    SaveDialog: TSaveDialog;
    DD_SequenceType_ComboBox: TComboBox;
    DD_SupportGermanSpecialChars_CheckBox: TCheckBox;
    CT_TabSheet: TTabSheet;
    DD_TabSheet: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure CT_DebtorBICNotProvided_CheckBoxChange(Sender: TObject);
    procedure CT_CreditorBICNotProvided_CheckBoxChange(Sender: TObject);
    procedure DD_CreditorBICNotProvided_CheckBoxChange(Sender: TObject);
    procedure DD_DebtorBICNotProvided_CheckBoxChange(Sender: TObject);
    procedure CT_Save_ButtonClick(Sender: TObject);
    procedure DD_Save_ButtonClick(Sender: TObject);
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
  CT_RequestedExecutionDate_Edit.Date  := Now+1;
  CT_InstructedAmount_Edit.Text        := FloatToStr(123.45);
  CT_RemittanceInformation_Memo.Text   := Trim(DD_RemittanceInformation_Memo.Text);

  DD_RequestedCollectionDate_Edit.Date := SEPAEarliestCollectionDate(DD_LocalInstrumentCode_ComboBox.Text, DD_SequenceType_ComboBox.Text);
  DD_InstructedAmount_Edit.Text        := FloatToStr(123.45);
  DD_MandateDateOfSignature_Edit.Date  := Now-1;
  DD_RemittanceInformation_Memo.Text   := Trim(DD_RemittanceInformation_Memo.Text);
end;

procedure TExampleForm.CT_DebtorBICNotProvided_CheckBoxChange(
  Sender: TObject);
begin
  CT_DebtorBIC_Edit.Enabled := not CT_DebtorBICNotProvided_CheckBox.Checked;
  if not CT_DebtorBIC_Edit.Enabled then
    CT_DebtorBIC_Edit.Text := '';
end;

procedure TExampleForm.CT_CreditorBICNotProvided_CheckBoxChange(
  Sender: TObject);
begin
  CT_CreditorBIC_Edit.Enabled := not CT_CreditorBICNotProvided_CheckBox.Checked;
  if not CT_CreditorBIC_Edit.Enabled then
    CT_CreditorBIC_Edit.Text := '';
end;

procedure TExampleForm.DD_CreditorBICNotProvided_CheckBoxChange(
  Sender: TObject);
begin
  DD_CreditorBIC_Edit.Enabled := not DD_CreditorBICNotProvided_CheckBox.Checked;
  if not DD_CreditorBIC_Edit.Enabled then
    DD_CreditorBIC_Edit.Text := '';
end;

procedure TExampleForm.DD_DebtorBICNotProvided_CheckBoxChange(Sender: TObject);
begin
  DD_DebtorBIC_Edit.Enabled := not DD_DebtorBICNotProvided_CheckBox.Checked;
  if not DD_DebtorBIC_Edit.Enabled then
    DD_DebtorBIC_Edit.Text := '';
end;

procedure TExampleForm.CT_Save_ButtonClick(Sender: TObject);
var
  cti: TCreditTransferInitiation;
  pi: TCreditTransferPaymentInformation;
  ti: TCreditTransferTransactionInformation;
  messages: TStringList;
begin
  // note: the GUI elements return strings as UTF8, so we use UTF8Decode below
  //       (the SEPA direct debit unit expects ANSI strings)

  // support for special characters has to be set before creating the objects,
  // otherwise they will be converted in the set-methods
  SEPASupportSpecialChars := CT_SupportGermanSpecialChars_CheckBox.Checked;

  // XML file
  cti := TCreditTransferInitiation.Create;
  cti.Schema             := UTF8Decode(CT_ISOSchema_ComboBox.Text);
  cti.GrpHdrInitgPtyName := UTF8Decode(CT_InitiatingPartyName_Edit.Text);

  // payment instruction
  pi := TCreditTransferPaymentInformation.Create;
  pi.ReqdExctnDt    := CT_RequestedExecutionDate_Edit.Date;
  pi.DbtrNm         := UTF8Decode(CT_DebtorName_Edit.Text);
  pi.DbtrAcct.IBAN  := UTF8Decode(CT_DebtorIBAN_Edit.Text);
  pi.DbtrAgt.BIC    := UTF8Decode(CT_DebtorBIC_Edit.Text);
  pi.DbtrAgt.OthrID := IfThen(CT_DebtorBICNotProvided_CheckBox.Checked, FIN_INSTN_NOTPROVIDED, '');
  cti.AppendPmtInfEntry(pi);

  // credit transfer transaction
  ti := TCreditTransferTransactionInformation.Create;
  ti.PmtIdEndToEndId := UTF8Decode(CT_EndToEndId_ComboBox.Text);
  ti.InstdAmt        := StrToFloat(CT_InstructedAmount_Edit.Text);
  ti.CdtrNm          := UTF8Decode(CT_CreditorName_Edit.Text);
  ti.CdtrAcct.IBAN   := UTF8Decode(CT_CreditorIBAN_Edit.Text);
  ti.CdtrAgt.BIC     := UTF8Decode(CT_CreditorBIC_Edit.Text);
  ti.CdtrAgt.OthrID  := IfThen(CT_CreditorBICNotProvided_CheckBox.Checked, FIN_INSTN_NOTPROVIDED, '');
  ti.RmtInfUstrd     := Trim(UTF8Decode(CT_RemittanceInformation_Memo.Text));
  pi.AppendCdtTrfTxInfEntry(ti);

  // validate and save
  messages := cti.Validate;
  if ((messages.Count = 0) or (MessageDlg(UTF8Encode(messages.Text), mtError, [mbOk, mbIgnore], 0) = mrIgnore)) and
      SaveDialog.Execute then
    cti.SaveToDisk(UTF8Decode(SaveDialog.FileName));

  // free objects
  FreeAndNil(messages);
  FreeAndNil(cti);
  // note that the objects referenced in "cti" are automatically destroyed
  pi := nil;
  ti := nil;
end;

procedure TExampleForm.DD_Save_ButtonClick(Sender: TObject);
var
  ddi: TDirectDebitInitiation;
  pi: TDirectDebitPaymentInformation;
  ti: TDirectDebitTransactionInformation;
  messages: TStringList;
begin
  // note: the GUI elements return strings as UTF8, so we use UTF8Decode below
  //       (the SEPA direct debit unit expects ANSI strings)

  // support for special characters has to be set before creating the objects,
  // otherwise they will be converted in the set-methods
  SEPASupportSpecialChars := DD_SupportGermanSpecialChars_CheckBox.Checked;

  // XML file
  ddi := TDirectDebitInitiation.Create;
  ddi.Schema             := UTF8Decode(DD_ISOSchema_ComboBox.Text);
  ddi.GrpHdrInitgPtyName := UTF8Decode(DD_InitiatingPartyName_Edit.Text);

  // payment instruction
  pi := TDirectDebitPaymentInformation.Create;
  pi.PmtTpInfLclInstrmCd       := UTF8Decode(DD_LocalInstrumentCode_ComboBox.Text);
  pi.PmtTpInfSeqTp             := UTF8Decode(DD_SequenceType_ComboBox.Text);
  pi.ReqdColltnDt              := DD_RequestedCollectionDate_Edit.Date;
  pi.CdtrNm                    := UTF8Decode(DD_CreditorName_Edit.Text);
  pi.CdtrAcct.IBAN             := UTF8Decode(DD_CreditorIBAN_Edit.Text);
  pi.CdtrAgt.BIC               := UTF8Decode(DD_CreditorBIC_Edit.Text);
  pi.CdtrAgt.OthrID            := IfThen(DD_CreditorBICNotProvided_CheckBox.Checked, FIN_INSTN_NOTPROVIDED, '');
  pi.CdtrSchmeIdIdPrvtIdOthrId := UTF8Decode(DD_CreditorIdentifier_Edit.Text);
  ddi.AppendPmtInfEntry(pi);

  // direct debit transaction (including mandate details)
  ti := TDirectDebitTransactionInformation.Create;
  ti.PmtIdEndToEndId := UTF8Decode(DD_EndToEndId_ComboBox.Text);
  ti.InstdAmt        := StrToFloat(DD_InstructedAmount_Edit.Text);
  ti.DbtrNm          := UTF8Decode(DD_DebtorName_Edit.Text);
  ti.DbtrAcct.IBAN   := UTF8Decode(DD_DebtorIBAN_Edit.Text);
  ti.DbtrAgt.BIC     := UTF8Decode(DD_DebtorBIC_Edit.Text);
  ti.DbtrAgt.OthrID  := IfThen(DD_DebtorBICNotProvided_CheckBox.Checked, FIN_INSTN_NOTPROVIDED, '');
  ti.RmtInfUstrd     := Trim(UTF8Decode(DD_RemittanceInformation_Memo.Text));
  ti.DrctDbtTxMndtRltdInf.MndtId    := UTF8Decode(DD_MandateId_Edit.Text);
  ti.DrctDbtTxMndtRltdInf.DtOfSgntr := DD_MandateDateOfSignature_Edit.Date;
  ti.DrctDbtTxMndtRltdInf.AmdmntInd := DD_MandateAmendmentInformationDetails_CheckBox.Checked;
  ti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlMndtId                    := UTF8Decode(DD_OriginalMandateId_Edit.Text);
  ti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlCdtrSchmeIdNm             := UTF8Decode(DD_OriginalCreditorName_Edit.Text);
  ti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlCdtrSchmeIdIdPrvtIdOthrId := UTF8Decode(DD_OriginalCreditorIdentifier_Edit.Text);
  ti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlDbtrAcct.IBAN             := UTF8Decode(DD_OriginalDebtorAccountIBAN_Edit.Text);
  ti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlDbtrAgtFinInstIdOthrId    := IfThen(DD_OriginalDebtorFinInstSMNDA_CheckBox.Checked, ORGNL_DBTR_AGT_SMNDA, '');
  pi.AppendDrctDbtTxInfEntry(ti);

  // validate and save
  messages := ddi.Validate;
  if ((messages.Count = 0) or (MessageDlg(UTF8Encode(messages.Text), mtError, [mbOk, mbIgnore], 0) = mrIgnore)) and
      SaveDialog.Execute then
    ddi.SaveToDisk(UTF8Decode(SaveDialog.FileName));

  // free objects
  FreeAndNil(messages);
  FreeAndNil(ddi);
  // note that the objects referenced in "ddi" are automatically destroyed
  pi := nil;
  ti := nil;
end;

end.

