object ExampleForm: TExampleForm
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'SEPA XML file example (with only one transaction)'
  ClientHeight = 631
  ClientWidth = 915
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 32
    Top = 104
    Width = 2
    Height = 25
  end
  object Bevel2: TBevel
    Left = 32
    Top = 129
    Width = 17
    Height = 2
  end
  object Bevel3: TBevel
    Left = 64
    Top = 336
    Width = 2
    Height = 25
  end
  object Bevel4: TBevel
    Left = 64
    Top = 361
    Width = 17
    Height = 2
  end
  object Bevel5: TBevel
    Left = 472
    Top = 361
    Width = 17
    Height = 2
  end
  object GroupBox1: TGroupBox
    Left = 24
    Top = 16
    Width = 377
    Height = 81
    Caption = 'XML file / group header'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 48
      Width = 103
      Height = 13
      Caption = 'Initiating party (name):'
    end
    object Label23: TLabel
      Left = 8
      Top = 24
      Width = 61
      Height = 13
      Caption = 'ISO schema:'
    end
    object Label24: TLabel
      Left = 256
      Top = 24
      Width = 98
      Height = 13
      Caption = '(empty = auto-select)'
    end
    object InitiatingPartyNameEdit: TEdit
      Left = 128
      Top = 48
      Width = 225
      Height = 21
      MaxLength = 70
      TabOrder = 1
      Text = 'My name'
    end
    object ISOSchemaComboBox: TComboBox
      Left = 128
      Top = 24
      Width = 121
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'pain.008.002.02'
        'pain.008.003.02')
    end
  end
  object SaveButton: TButton
    Left = 376
    Top = 592
    Width = 91
    Height = 25
    Caption = 'Save XML file...'
    TabOrder = 4
    OnClick = SaveButtonClick
  end
  object GroupBox2: TGroupBox
    Left = 56
    Top = 112
    Width = 377
    Height = 217
    Caption = 'Payment instruction'
    TabOrder = 1
    object Label2: TLabel
      Left = 8
      Top = 24
      Width = 107
      Height = 13
      Caption = 'Local instrument code:'
    end
    object Label3: TLabel
      Left = 8
      Top = 48
      Width = 75
      Height = 13
      Caption = 'Sequence type:'
    end
    object Label4: TLabel
      Left = 8
      Top = 72
      Width = 98
      Height = 13
      Caption = 'Req. collection date:'
    end
    object Label5: TLabel
      Left = 8
      Top = 96
      Width = 68
      Height = 13
      Caption = 'Creditor name:'
    end
    object Label6: TLabel
      Left = 8
      Top = 120
      Width = 115
      Height = 13
      Caption = 'Creditor account (IBAN):'
    end
    object Label7: TLabel
      Left = 8
      Top = 144
      Width = 104
      Height = 13
      Caption = 'Creditor fin. inst. (BIC):'
    end
    object Label8: TLabel
      Left = 8
      Top = 188
      Width = 81
      Height = 13
      Caption = 'Creditor identifier:'
    end
    object CreditorNameEdit: TEdit
      Left = 128
      Top = 96
      Width = 225
      Height = 21
      MaxLength = 70
      TabOrder = 3
      Text = 'My name'
    end
    object CreditorAccountIBANEdit: TEdit
      Left = 128
      Top = 120
      Width = 225
      Height = 21
      TabOrder = 4
      Text = 'DE58 1234 5678 0123 4567 89'
    end
    object CreditorAccountBICEdit: TEdit
      Left = 128
      Top = 144
      Width = 225
      Height = 21
      TabOrder = 5
      Text = 'SOMEFININST'
    end
    object CreditorIdentifierEdit: TEdit
      Left = 128
      Top = 188
      Width = 225
      Height = 21
      MaxLength = 35
      TabOrder = 7
      Text = 'DE98ZZZ09999999999'
    end
    object RequestedCollectionDateEdit: TDateTimePicker
      Left = 128
      Top = 72
      Width = 186
      Height = 21
      CalAlignment = dtaLeft
      Date = 41526.3194741551
      Time = 41526.3194741551
      DateFormat = dfShort
      DateMode = dmComboBox
      Kind = dtkDate
      ParseInput = False
      TabOrder = 2
    end
    object LocalInstrumentCodeComboBox: TComboBox
      Left = 128
      Top = 24
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'CORE'
      Items.Strings = (
        'CORE'
        'B2B'
        'COR1')
    end
    object SequenceTypeComboBox: TComboBox
      Left = 128
      Top = 48
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = 'FRST'
      Items.Strings = (
        'FRST'
        'RCUR'
        'OOFF'
        'FNAL')
    end
    object CreditorAccountNOTPROVIDEDCheckBox: TCheckBox
      Left = 128
      Top = 168
      Width = 225
      Height = 17
      Caption = 'NOTPROVIDED (IBAN-only)'
      TabOrder = 6
    end
  end
  object GroupBox3: TGroupBox
    Left = 88
    Top = 344
    Width = 377
    Height = 233
    Caption = 'Direct debit transaction'
    TabOrder = 2
    object Label9: TLabel
      Left = 8
      Top = 24
      Width = 69
      Height = 13
      Caption = 'End-to-end ID:'
    end
    object Label12: TLabel
      Left = 8
      Top = 48
      Width = 88
      Height = 13
      Caption = 'Instructed amount:'
    end
    object Label13: TLabel
      Left = 8
      Top = 96
      Width = 111
      Height = 13
      Caption = 'Debtor account (IBAN):'
    end
    object Label14: TLabel
      Left = 8
      Top = 120
      Width = 100
      Height = 13
      Caption = 'Debtor fin. inst. (BIC):'
    end
    object Label15: TLabel
      Left = 8
      Top = 164
      Width = 111
      Height = 26
      Caption = 'Remittance information (unstructured):'
      WordWrap = True
    end
    object Label10: TLabel
      Left = 240
      Top = 48
      Width = 23
      Height = 13
      Caption = 'EUR'
    end
    object Label11: TLabel
      Left = 8
      Top = 72
      Width = 64
      Height = 13
      Caption = 'Debtor name:'
    end
    object InstructedAmountEdit: TEdit
      Left = 128
      Top = 48
      Width = 105
      Height = 21
      TabOrder = 1
    end
    object DebtorIBANEdit: TEdit
      Left = 128
      Top = 96
      Width = 225
      Height = 21
      TabOrder = 3
      Text = 'DE58 1234 5678 0123 4567 89'
    end
    object DebtorBICEdit: TEdit
      Left = 128
      Top = 120
      Width = 225
      Height = 21
      TabOrder = 4
      Text = 'SOMEFININST'
    end
    object EndToEndIdComboBox: TComboBox
      Left = 128
      Top = 24
      Width = 225
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      MaxLength = 35
      TabOrder = 0
      Text = 'NOTPROVIDED'
      Items.Strings = (
        'NOTPROVIDED')
    end
    object DebtorNameEdit: TEdit
      Left = 128
      Top = 72
      Width = 225
      Height = 21
      MaxLength = 70
      TabOrder = 2
      Text = 'Customer name'
    end
    object RemittanceInformationMemo: TMemo
      Left = 128
      Top = 164
      Width = 225
      Height = 57
      Lines.Strings = (
        'Customer number 123, invoice number 456')
      MaxLength = 140
      TabOrder = 6
      WantReturns = False
    end
    object DebtorNOTPROVIDEDCheckBox: TCheckBox
      Left = 128
      Top = 144
      Width = 225
      Height = 17
      Caption = 'NOTPROVIDED (IBAN-only)'
      TabOrder = 5
    end
  end
  object GroupBox4: TGroupBox
    Left = 496
    Top = 344
    Width = 377
    Height = 233
    Caption = 'Mandate related information'
    TabOrder = 3
    object Label17: TLabel
      Left = 8
      Top = 24
      Width = 59
      Height = 13
      Caption = 'Mandate ID:'
    end
    object Label18: TLabel
      Left = 8
      Top = 176
      Width = 111
      Height = 13
      Caption = 'Debtor account (IBAN):'
    end
    object Label19: TLabel
      Left = 8
      Top = 200
      Width = 74
      Height = 13
      Caption = 'Debtor fin. inst.:'
    end
    object Label20: TLabel
      Left = 8
      Top = 104
      Width = 59
      Height = 13
      Caption = 'Mandate ID:'
    end
    object Label22: TLabel
      Left = 8
      Top = 48
      Width = 84
      Height = 13
      Caption = 'Date of signature:'
    end
    object Label16: TLabel
      Left = 8
      Top = 128
      Width = 68
      Height = 13
      Caption = 'Creditor name:'
    end
    object Label21: TLabel
      Left = 8
      Top = 152
      Width = 81
      Height = 13
      Caption = 'Creditor identifier:'
    end
    object MandateIdEdit: TEdit
      Left = 128
      Top = 24
      Width = 225
      Height = 21
      MaxLength = 35
      TabOrder = 0
      Text = 'MNDTID'
    end
    object OriginalDebtorAccountIBANEdit: TEdit
      Left = 128
      Top = 176
      Width = 225
      Height = 21
      MaxLength = 34
      TabOrder = 6
    end
    object OriginalMandateIdEdit: TEdit
      Left = 128
      Top = 104
      Width = 225
      Height = 21
      MaxLength = 35
      TabOrder = 3
    end
    object MandateDateOfSignatureEdit: TDateTimePicker
      Left = 128
      Top = 48
      Width = 186
      Height = 21
      CalAlignment = dtaLeft
      Date = 41526.3194741551
      Time = 41526.3194741551
      DateFormat = dfShort
      DateMode = dmComboBox
      Kind = dtkDate
      ParseInput = False
      TabOrder = 1
    end
    object MandateAmendmentInformationDetailsCheckBox: TCheckBox
      Left = 8
      Top = 80
      Width = 249
      Height = 17
      Caption = 'Amendment information details (only changes):'
      TabOrder = 2
    end
    object OriginalDebtorFinInstSMNDACheckBox: TCheckBox
      Left = 128
      Top = 200
      Width = 233
      Height = 17
      Caption = 'SMNDA (same mandate, new debtor agent)'
      TabOrder = 7
    end
    object OriginalCreditorNameEdit: TEdit
      Left = 128
      Top = 128
      Width = 225
      Height = 21
      MaxLength = 70
      TabOrder = 4
    end
    object OriginalCreditorIdentifierEdit: TEdit
      Left = 128
      Top = 152
      Width = 225
      Height = 21
      MaxLength = 35
      TabOrder = 5
    end
  end
  object GroupBox5: TGroupBox
    Left = 512
    Top = 24
    Width = 361
    Height = 137
    Caption = 'Some practical tips'
    TabOrder = 5
    object Label25: TLabel
      Left = 8
      Top = 48
      Width = 343
      Height = 13
      Caption = 
        '- Use only one <PmtInf> ("payment instruction information") bloc' +
        'k per file.'
    end
    object Label26: TLabel
      Left = 8
      Top = 64
      Width = 306
      Height = 13
      Caption = 
        '- Limit the number of transactions per file to e.g. 500 transact' +
        'ions.'
    end
    object Label27: TLabel
      Left = 8
      Top = 80
      Width = 277
      Height = 13
      Caption = '- ISO schema pain.008.003.02 *may* not be supported yet.'
    end
    object Label28: TLabel
      Left = 8
      Top = 104
      Width = 323
      Height = 13
      Caption = 
        'Ask your bank for details - and please let us know further restr' +
        'ictions!'
    end
    object Label29: TLabel
      Left = 8
      Top = 24
      Width = 269
      Height = 13
      Caption = 'For maximum interoperability with different German banks:'
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML files (*.xml)|*.xml|All files|*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save XML file'
    Left = 472
    Top = 592
  end
end
