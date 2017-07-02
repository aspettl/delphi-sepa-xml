//
//   Delphi example for SEPA XML file creation
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
unit ExampleUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ComCtrls, ExtCtrls, StrUtils, Math, DateUtils,
  SEPACommon, SEPACreditTransfer, SEPADirectDebit;

type
  TExampleForm = class(TForm)
    PageControl: TPageControl;
    CT_TabSheet: TTabSheet;
    DD_TabSheet: TTabSheet;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    DD_XMLFile_GroupBox: TGroupBox;
    Label1: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label30: TLabel;
    DD_InitiatingPartyName_Edit: TEdit;
    DD_ISOSchema_ComboBox: TComboBox;
    DD_SupportGermanSpecialChars_CheckBox: TCheckBox;
    DD_Save_Button: TButton;
    DD_PaymentInformation_GroupBox: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    DD_CreditorName_Edit: TEdit;
    DD_CreditorIBAN_Edit: TEdit;
    DD_CreditorBIC_Edit: TEdit;
    DD_CreditorIdentifier_Edit: TEdit;
    DD_RequestedCollectionDate_Edit: TDateTimePicker;
    DD_LocalInstrumentCode_ComboBox: TComboBox;
    DD_SequenceType_ComboBox: TComboBox;
    DD_CreditorBICNotProvided_CheckBox: TCheckBox;
    DD_Transaction_GroupBox: TGroupBox;
    Label9: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    DD_InstructedAmount_Edit: TEdit;
    DD_DebtorIBAN_Edit: TEdit;
    DD_DebtorBIC_Edit: TEdit;
    DD_EndToEndId_ComboBox: TComboBox;
    DD_DebtorName_Edit: TEdit;
    DD_RemittanceInformation_Memo: TMemo;
    DD_DebtorBICNotProvided_CheckBox: TCheckBox;
    DD_Mandate_GroupBox: TGroupBox;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label22: TLabel;
    Label16: TLabel;
    Label21: TLabel;
    DD_MandateId_Edit: TEdit;
    DD_OriginalDebtorAccountIBAN_Edit: TEdit;
    DD_OriginalMandateId_Edit: TEdit;
    DD_MandateDateOfSignature_Edit: TDateTimePicker;
    DD_MandateAmendmentInformationDetails_CheckBox: TCheckBox;
    DD_OriginalDebtorFinInstSMNDA_CheckBox: TCheckBox;
    DD_OriginalCreditorName_Edit: TEdit;
    DD_OriginalCreditorIdentifier_Edit: TEdit;
    DD_Tips_GroupBox: TGroupBox;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    SaveDialog: TSaveDialog;
    CT_XMLFile_GroupBox: TGroupBox;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    CT_InitiatingPartyName_Edit: TEdit;
    CT_ISOSchema_ComboBox: TComboBox;
    CT_SupportGermanSpecialChars_CheckBox: TCheckBox;
    Bevel6: TBevel;
    Bevel7: TBevel;
    CT_PaymentInformation_GroupBox: TGroupBox;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    CT_DebtorName_Edit: TEdit;
    CT_DebtorIBAN_Edit: TEdit;
    CT_DebtorBIC_Edit: TEdit;
    CT_DebtorBICNotProvided_CheckBox: TCheckBox;
    Bevel8: TBevel;
    Bevel9: TBevel;
    CT_Transaction_GroupBox: TGroupBox;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    CT_InstructedAmount_Edit: TEdit;
    CT_CreditorIBAN_Edit: TEdit;
    CT_CreditorBIC_Edit: TEdit;
    CT_EndToEndId_ComboBox: TComboBox;
    CT_CreditorName_Edit: TEdit;
    CT_RemittanceInformation_Memo: TMemo;
    CT_CreditorBICNotProvided_CheckBox: TCheckBox;
    CT_Save_Button: TButton;
    CT_RequestedExecutionDate_Edit: TDateTimePicker;
    GroupBox1: TGroupBox;
    Label35: TLabel;
    Label36: TLabel;
    Label41: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CT_DebtorBICNotProvided_CheckBoxClick(Sender: TObject);
    procedure CT_CreditorBICNotProvided_CheckBoxClick(Sender: TObject);
    procedure DD_CreditorBICNotProvided_CheckBoxClick(Sender: TObject);
    procedure DD_DebtorBICNotProvided_CheckBoxClick(Sender: TObject);
    procedure CT_Save_ButtonClick(Sender: TObject);
    procedure DD_Save_ButtonClick(Sender: TObject);
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
  CT_RequestedExecutionDate_Edit.Date  := Today+1;
  CT_InstructedAmount_Edit.Text        := FloatToStr(123.45);
  CT_RemittanceInformation_Memo.Text   := Trim(DD_RemittanceInformation_Memo.Text);

  DD_RequestedCollectionDate_Edit.Date := SEPAEarliestCollectionDate(DD_LocalInstrumentCode_ComboBox.Text, DD_SequenceType_ComboBox.Text);
  DD_InstructedAmount_Edit.Text        := FloatToStr(123.45);
  DD_MandateDateOfSignature_Edit.Date  := Today-1;
  DD_RemittanceInformation_Memo.Text   := Trim(DD_RemittanceInformation_Memo.Text);
end;

procedure TExampleForm.CT_DebtorBICNotProvided_CheckBoxClick(
  Sender: TObject);
begin
  CT_DebtorBIC_Edit.Enabled := not CT_DebtorBICNotProvided_CheckBox.Checked;
  if not CT_DebtorBIC_Edit.Enabled then
    CT_DebtorBIC_Edit.Text := '';
end;

procedure TExampleForm.CT_CreditorBICNotProvided_CheckBoxClick(
  Sender: TObject);
begin
  CT_CreditorBIC_Edit.Enabled := not CT_CreditorBICNotProvided_CheckBox.Checked;
  if not CT_CreditorBIC_Edit.Enabled then
    CT_CreditorBIC_Edit.Text := '';
end;

procedure TExampleForm.DD_CreditorBICNotProvided_CheckBoxClick(
  Sender: TObject);
begin
  DD_CreditorBIC_Edit.Enabled := not DD_CreditorBICNotProvided_CheckBox.Checked;
  if not DD_CreditorBIC_Edit.Enabled then
    DD_CreditorBIC_Edit.Text := '';
end;

procedure TExampleForm.DD_DebtorBICNotProvided_CheckBoxClick(Sender: TObject);
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
  // support for special characters has to be set before creating the objects,
  // otherwise they will be converted in the set-methods
  SEPASupportSpecialChars := CT_SupportGermanSpecialChars_CheckBox.Checked;

  // XML file
  cti := TCreditTransferInitiation.Create;
  cti.Schema             := CT_ISOSchema_ComboBox.Text;
  cti.GrpHdrInitgPtyName := CT_InitiatingPartyName_Edit.Text;

  // payment instruction
  pi := TCreditTransferPaymentInformation.Create;
  pi.ReqdExctnDt    := CT_RequestedExecutionDate_Edit.Date;
  pi.DbtrNm         := CT_DebtorName_Edit.Text;
  pi.DbtrAcct.IBAN  := CT_DebtorIBAN_Edit.Text;
  pi.DbtrAgt.BIC    := CT_DebtorBIC_Edit.Text;
  pi.DbtrAgt.OthrID := IfThen(CT_DebtorBICNotProvided_CheckBox.Checked, FIN_INSTN_NOTPROVIDED, '');
  cti.AppendPmtInfEntry(pi);

  // credit transfer transaction
  ti := TCreditTransferTransactionInformation.Create;
  ti.PmtIdEndToEndId := CT_EndToEndId_ComboBox.Text;
  ti.InstdAmt        := StrToFloat(CT_InstructedAmount_Edit.Text);
  ti.CdtrNm          := CT_CreditorName_Edit.Text;
  ti.CdtrAcct.IBAN   := CT_CreditorIBAN_Edit.Text;
  ti.CdtrAgt.BIC     := CT_CreditorBIC_Edit.Text;
  ti.CdtrAgt.OthrID  := IfThen(CT_CreditorBICNotProvided_CheckBox.Checked, FIN_INSTN_NOTPROVIDED, '');
  ti.RmtInfUstrd     := Trim(CT_RemittanceInformation_Memo.Text);
  pi.AppendCdtTrfTxInfEntry(ti);

  // validate and save
  messages := cti.Validate;
  if ((messages.Count = 0) or (MessageDlg(messages.Text, mtError, [mbOk, mbIgnore], 0) = mrIgnore)) and
      SaveDialog.Execute then
    cti.SaveToDisk(SaveDialog.FileName);

  // free objects
  FreeAndNil(messages);
  FreeAndNil(cti);
  // note that the objects referenced in "cti" are automatically destroyed
end;

procedure TExampleForm.DD_Save_ButtonClick(Sender: TObject);
var
  ddi: TDirectDebitInitiation;
  pi: TDirectDebitPaymentInformation;
  ti: TDirectDebitTransactionInformation;
  messages: TStringList;
begin
  // support for special characters has to be set before creating the objects,
  // otherwise they will be converted in the set-methods
  SEPASupportSpecialChars := DD_SupportGermanSpecialChars_CheckBox.Checked;

  // XML file
  ddi := TDirectDebitInitiation.Create;
  ddi.Schema             := DD_ISOSchema_ComboBox.Text;
  ddi.GrpHdrInitgPtyName := DD_InitiatingPartyName_Edit.Text;

  // payment instruction
  pi := TDirectDebitPaymentInformation.Create;
  pi.PmtTpInfLclInstrmCd       := DD_LocalInstrumentCode_ComboBox.Text;
  pi.PmtTpInfSeqTp             := DD_SequenceType_ComboBox.Text;
  pi.ReqdColltnDt              := DD_RequestedCollectionDate_Edit.Date;
  pi.CdtrNm                    := DD_CreditorName_Edit.Text;
  pi.CdtrAcct.IBAN             := DD_CreditorIBAN_Edit.Text;
  pi.CdtrAgt.BIC               := DD_CreditorBIC_Edit.Text;
  pi.CdtrAgt.OthrID            := IfThen(DD_CreditorBICNotProvided_CheckBox.Checked, FIN_INSTN_NOTPROVIDED, '');
  pi.CdtrSchmeIdIdPrvtIdOthrId := DD_CreditorIdentifier_Edit.Text;
  ddi.AppendPmtInfEntry(pi);

  // direct debit transaction (including mandate details)
  ti := TDirectDebitTransactionInformation.Create;
  ti.PmtIdEndToEndId := DD_EndToEndId_ComboBox.Text;
  ti.InstdAmt        := StrToFloat(DD_InstructedAmount_Edit.Text);
  ti.DbtrNm          := DD_DebtorName_Edit.Text;
  ti.DbtrAcct.IBAN   := DD_DebtorIBAN_Edit.Text;
  ti.DbtrAgt.BIC     := DD_DebtorBIC_Edit.Text;
  ti.DbtrAgt.OthrID  := IfThen(DD_DebtorBICNotProvided_CheckBox.Checked, FIN_INSTN_NOTPROVIDED, '');
  ti.RmtInfUstrd     := Trim(DD_RemittanceInformation_Memo.Text);
  ti.DrctDbtTxMndtRltdInf.MndtId    := DD_MandateId_Edit.Text;
  ti.DrctDbtTxMndtRltdInf.DtOfSgntr := DD_MandateDateOfSignature_Edit.Date;
  ti.DrctDbtTxMndtRltdInf.AmdmntInd := DD_MandateAmendmentInformationDetails_CheckBox.Checked;
  ti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlMndtId                    := DD_OriginalMandateId_Edit.Text;
  ti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlCdtrSchmeIdNm             := DD_OriginalCreditorName_Edit.Text;
  ti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlCdtrSchmeIdIdPrvtIdOthrId := DD_OriginalCreditorIdentifier_Edit.Text;
  ti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlDbtrAcct.IBAN             := DD_OriginalDebtorAccountIBAN_Edit.Text;
  ti.DrctDbtTxMndtRltdInf.AmdmntInfDtls.OrgnlDbtrAgtFinInstIdOthrId    := IfThen(DD_OriginalDebtorFinInstSMNDA_CheckBox.Checked, ORGNL_DBTR_AGT_SMNDA, '');
  pi.AppendDrctDbtTxInfEntry(ti);

  // validate and save
  messages := ddi.Validate;
  if ((messages.Count = 0) or (MessageDlg(messages.Text, mtError, [mbOk, mbIgnore], 0) = mrIgnore)) and
      SaveDialog.Execute then
    ddi.SaveToDisk(SaveDialog.FileName);

  // free objects
  FreeAndNil(messages);
  FreeAndNil(ddi);
  // note that the objects referenced in "ddi" are automatically destroyed
end;

end.
