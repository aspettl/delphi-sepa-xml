object ExampleForm: TExampleForm
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'SEPA XML file example (with only one transaction)'
  ClientHeight = 666
  ClientWidth = 876
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
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 876
    Height = 666
    ActivePage = DD_TabSheet
    Align = alClient
    TabIndex = 1
    TabOrder = 0
    object CT_TabSheet: TTabSheet
      Caption = 'Credit transfer'
      object Bevel6: TBevel
        Left = 16
        Top = 112
        Width = 2
        Height = 25
      end
      object Bevel7: TBevel
        Left = 16
        Top = 137
        Width = 17
        Height = 2
      end
      object Bevel8: TBevel
        Left = 48
        Top = 344
        Width = 2
        Height = 25
      end
      object Bevel9: TBevel
        Left = 48
        Top = 369
        Width = 17
        Height = 2
      end
      object CT_XMLFile_GroupBox: TGroupBox
        Left = 8
        Top = 8
        Width = 377
        Height = 97
        Caption = 'XML file / group header'
        TabOrder = 0
        object Label31: TLabel
          Left = 8
          Top = 48
          Width = 103
          Height = 13
          Caption = 'Initiating party (name):'
        end
        object Label32: TLabel
          Left = 8
          Top = 24
          Width = 61
          Height = 13
          Caption = 'ISO schema:'
        end
        object Label33: TLabel
          Left = 256
          Top = 24
          Width = 98
          Height = 13
          Caption = '(empty = auto-select)'
        end
        object Label34: TLabel
          Left = 8
          Top = 72
          Width = 91
          Height = 13
          Caption = 'Special characters:'
        end
        object CT_InitiatingPartyName_Edit: TEdit
          Left = 128
          Top = 48
          Width = 225
          Height = 21
          MaxLength = 70
          TabOrder = 1
          Text = 'My name'
        end
        object CT_ISOSchema_ComboBox: TComboBox
          Left = 128
          Top = 24
          Width = 121
          Height = 21
          ItemHeight = 13
          TabOrder = 0
          Items.Strings = (
            'pain.001.002.03'
            'pain.001.003.03'
            'pain.001.001.03')
        end
        object CT_SupportGermanSpecialChars_CheckBox: TCheckBox
          Left = 128
          Top = 72
          Width = 225
          Height = 17
          Caption = 'Allow (German) characters: '#196#214#220#228#246#252#223'&&*$%'
          TabOrder = 2
        end
      end
      object CT_PaymentInformation_GroupBox: TGroupBox
        Left = 40
        Top = 120
        Width = 377
        Height = 217
        Caption = 'Payment information'
        TabOrder = 1
        object Label37: TLabel
          Left = 8
          Top = 24
          Width = 99
          Height = 13
          Caption = 'Req. execution date:'
        end
        object Label38: TLabel
          Left = 8
          Top = 48
          Width = 64
          Height = 13
          Caption = 'Debtor name:'
        end
        object Label39: TLabel
          Left = 8
          Top = 72
          Width = 111
          Height = 13
          Caption = 'Debtor account (IBAN):'
        end
        object Label40: TLabel
          Left = 8
          Top = 96
          Width = 100
          Height = 13
          Caption = 'Debtor fin. inst. (BIC):'
        end
        object CT_DebtorName_Edit: TEdit
          Left = 128
          Top = 48
          Width = 225
          Height = 21
          MaxLength = 70
          TabOrder = 1
          Text = 'My name'
        end
        object CT_DebtorIBAN_Edit: TEdit
          Left = 128
          Top = 72
          Width = 225
          Height = 21
          TabOrder = 2
          Text = 'DE58 1234 5678 0123 4567 89'
        end
        object CT_DebtorBIC_Edit: TEdit
          Left = 128
          Top = 96
          Width = 225
          Height = 21
          TabOrder = 3
          Text = 'SOMEFININST'
        end
        object CT_DebtorBICNotProvided_CheckBox: TCheckBox
          Left = 128
          Top = 120
          Width = 225
          Height = 17
          Caption = 'NOTPROVIDED (IBAN-only)'
          TabOrder = 4
          OnClick = CT_DebtorBICNotProvided_CheckBoxClick
        end
        object CT_RequestedExecutionDate_Edit: TDateTimePicker
          Left = 128
          Top = 24
          Width = 186
          Height = 21
          CalAlignment = dtaLeft
          Date = 41526.3194741551
          Time = 41526.3194741551
          DateFormat = dfShort
          DateMode = dmComboBox
          Kind = dtkDate
          ParseInput = False
          TabOrder = 0
        end
      end
      object CT_Transaction_GroupBox: TGroupBox
        Left = 72
        Top = 352
        Width = 377
        Height = 233
        Caption = 'Credit transfer transaction'
        TabOrder = 2
        object Label42: TLabel
          Left = 8
          Top = 24
          Width = 69
          Height = 13
          Caption = 'End-to-end ID:'
        end
        object Label43: TLabel
          Left = 8
          Top = 48
          Width = 88
          Height = 13
          Caption = 'Instructed amount:'
        end
        object Label44: TLabel
          Left = 8
          Top = 96
          Width = 115
          Height = 13
          Caption = 'Creditor account (IBAN):'
        end
        object Label45: TLabel
          Left = 8
          Top = 120
          Width = 104
          Height = 13
          Caption = 'Creditor fin. inst. (BIC):'
        end
        object Label46: TLabel
          Left = 8
          Top = 164
          Width = 111
          Height = 26
          Caption = 'Remittance information (unstructured):'
          WordWrap = True
        end
        object Label47: TLabel
          Left = 240
          Top = 48
          Width = 23
          Height = 13
          Caption = 'EUR'
        end
        object Label48: TLabel
          Left = 8
          Top = 72
          Width = 68
          Height = 13
          Caption = 'Creditor name:'
        end
        object CT_InstructedAmount_Edit: TEdit
          Left = 128
          Top = 48
          Width = 105
          Height = 21
          TabOrder = 1
        end
        object CT_CreditorIBAN_Edit: TEdit
          Left = 128
          Top = 96
          Width = 225
          Height = 21
          TabOrder = 3
          Text = 'DE58 1234 5678 0123 4567 89'
        end
        object CT_CreditorBIC_Edit: TEdit
          Left = 128
          Top = 120
          Width = 225
          Height = 21
          TabOrder = 4
          Text = 'SOMEFININST'
        end
        object CT_EndToEndId_ComboBox: TComboBox
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
        object CT_CreditorName_Edit: TEdit
          Left = 128
          Top = 72
          Width = 225
          Height = 21
          MaxLength = 70
          TabOrder = 2
          Text = 'Customer name'
        end
        object CT_RemittanceInformation_Memo: TMemo
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
        object CT_CreditorBICNotProvided_CheckBox: TCheckBox
          Left = 128
          Top = 144
          Width = 225
          Height = 17
          Caption = 'NOTPROVIDED (IBAN-only)'
          TabOrder = 5
          OnClick = CT_CreditorBICNotProvided_CheckBoxClick
        end
      end
      object CT_Save_Button: TButton
        Left = 357
        Top = 601
        Width = 91
        Height = 25
        Caption = 'Save XML file...'
        TabOrder = 3
        OnClick = CT_Save_ButtonClick
      end
      object GroupBox1: TGroupBox
        Left = 496
        Top = 16
        Width = 361
        Height = 137
        Caption = 'Some practical tips'
        TabOrder = 4
        object Label35: TLabel
          Left = 8
          Top = 48
          Width = 292
          Height = 13
          Caption = '- Use only one <PmtInf> ("payment information") block per file.'
        end
        object Label36: TLabel
          Left = 8
          Top = 64
          Width = 306
          Height = 13
          Caption = 
            '- Limit the number of transactions per file to e.g. 500 transact' +
            'ions.'
        end
        object Label41: TLabel
          Left = 8
          Top = 80
          Width = 277
          Height = 13
          Caption = '- ISO schema pain.001.001.03 *should* be supported now.'
        end
        object Label49: TLabel
          Left = 8
          Top = 104
          Width = 323
          Height = 13
          Caption = 
            'Ask your bank for details - and please let us know further restr' +
            'ictions!'
        end
        object Label50: TLabel
          Left = 8
          Top = 24
          Width = 269
          Height = 13
          Caption = 'For maximum interoperability with different German banks:'
        end
      end
    end
    object DD_TabSheet: TTabSheet
      Caption = 'Direct debit'
      ImageIndex = 1
      object Bevel1: TBevel
        Left = 16
        Top = 112
        Width = 2
        Height = 25
      end
      object Bevel2: TBevel
        Left = 16
        Top = 137
        Width = 17
        Height = 2
      end
      object Bevel3: TBevel
        Left = 48
        Top = 344
        Width = 2
        Height = 25
      end
      object Bevel4: TBevel
        Left = 48
        Top = 369
        Width = 17
        Height = 2
      end
      object Bevel5: TBevel
        Left = 456
        Top = 369
        Width = 17
        Height = 2
      end
      object DD_XMLFile_GroupBox: TGroupBox
        Left = 8
        Top = 8
        Width = 377
        Height = 97
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
        object Label30: TLabel
          Left = 8
          Top = 72
          Width = 91
          Height = 13
          Caption = 'Special characters:'
        end
        object DD_InitiatingPartyName_Edit: TEdit
          Left = 128
          Top = 48
          Width = 225
          Height = 21
          MaxLength = 70
          TabOrder = 1
          Text = 'My name'
        end
        object DD_ISOSchema_ComboBox: TComboBox
          Left = 128
          Top = 24
          Width = 121
          Height = 21
          ItemHeight = 13
          TabOrder = 0
          Items.Strings = (
            'pain.008.002.02'
            'pain.008.003.02'
            'pain.008.001.02')
        end
        object DD_SupportGermanSpecialChars_CheckBox: TCheckBox
          Left = 128
          Top = 72
          Width = 225
          Height = 17
          Caption = 'Allow (German) characters: '#196#214#220#228#246#252#223'&&*$%'
          TabOrder = 2
        end
      end
      object DD_Save_Button: TButton
        Left = 357
        Top = 601
        Width = 91
        Height = 25
        Caption = 'Save XML file...'
        TabOrder = 5
        OnClick = DD_Save_ButtonClick
      end
      object DD_PaymentInformation_GroupBox: TGroupBox
        Left = 40
        Top = 120
        Width = 377
        Height = 217
        Caption = 'Payment information'
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
        object DD_CreditorName_Edit: TEdit
          Left = 128
          Top = 96
          Width = 225
          Height = 21
          MaxLength = 70
          TabOrder = 3
          Text = 'My name'
        end
        object DD_CreditorIBAN_Edit: TEdit
          Left = 128
          Top = 120
          Width = 225
          Height = 21
          TabOrder = 4
          Text = 'DE58 1234 5678 0123 4567 89'
        end
        object DD_CreditorBIC_Edit: TEdit
          Left = 128
          Top = 144
          Width = 225
          Height = 21
          TabOrder = 5
          Text = 'SOMEFININST'
        end
        object DD_CreditorIdentifier_Edit: TEdit
          Left = 128
          Top = 188
          Width = 225
          Height = 21
          MaxLength = 35
          TabOrder = 7
          Text = 'DE98ZZZ09999999999'
        end
        object DD_RequestedCollectionDate_Edit: TDateTimePicker
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
        object DD_LocalInstrumentCode_ComboBox: TComboBox
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
        object DD_SequenceType_ComboBox: TComboBox
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
        object DD_CreditorBICNotProvided_CheckBox: TCheckBox
          Left = 128
          Top = 168
          Width = 225
          Height = 17
          Caption = 'NOTPROVIDED (IBAN-only)'
          TabOrder = 6
          OnClick = DD_CreditorBICNotProvided_CheckBoxClick
        end
      end
      object DD_Transaction_GroupBox: TGroupBox
        Left = 72
        Top = 352
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
        object DD_InstructedAmount_Edit: TEdit
          Left = 128
          Top = 48
          Width = 105
          Height = 21
          TabOrder = 1
        end
        object DD_DebtorIBAN_Edit: TEdit
          Left = 128
          Top = 96
          Width = 225
          Height = 21
          TabOrder = 3
          Text = 'DE58 1234 5678 0123 4567 89'
        end
        object DD_DebtorBIC_Edit: TEdit
          Left = 128
          Top = 120
          Width = 225
          Height = 21
          TabOrder = 4
          Text = 'SOMEFININST'
        end
        object DD_EndToEndId_ComboBox: TComboBox
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
        object DD_DebtorName_Edit: TEdit
          Left = 128
          Top = 72
          Width = 225
          Height = 21
          MaxLength = 70
          TabOrder = 2
          Text = 'Customer name'
        end
        object DD_RemittanceInformation_Memo: TMemo
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
        object DD_DebtorBICNotProvided_CheckBox: TCheckBox
          Left = 128
          Top = 144
          Width = 225
          Height = 17
          Caption = 'NOTPROVIDED (IBAN-only)'
          TabOrder = 5
          OnClick = DD_DebtorBICNotProvided_CheckBoxClick
        end
      end
      object DD_Mandate_GroupBox: TGroupBox
        Left = 480
        Top = 352
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
        object DD_MandateId_Edit: TEdit
          Left = 128
          Top = 24
          Width = 225
          Height = 21
          MaxLength = 35
          TabOrder = 0
          Text = 'MNDTID'
        end
        object DD_OriginalDebtorAccountIBAN_Edit: TEdit
          Left = 128
          Top = 176
          Width = 225
          Height = 21
          MaxLength = 34
          TabOrder = 6
        end
        object DD_OriginalMandateId_Edit: TEdit
          Left = 128
          Top = 104
          Width = 225
          Height = 21
          MaxLength = 35
          TabOrder = 3
        end
        object DD_MandateDateOfSignature_Edit: TDateTimePicker
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
        object DD_MandateAmendmentInformationDetails_CheckBox: TCheckBox
          Left = 8
          Top = 80
          Width = 249
          Height = 17
          Caption = 'Amendment information details (only changes):'
          TabOrder = 2
        end
        object DD_OriginalDebtorFinInstSMNDA_CheckBox: TCheckBox
          Left = 128
          Top = 200
          Width = 233
          Height = 17
          Caption = 'SMNDA (same mandate, new debtor agent)'
          TabOrder = 7
        end
        object DD_OriginalCreditorName_Edit: TEdit
          Left = 128
          Top = 128
          Width = 225
          Height = 21
          MaxLength = 70
          TabOrder = 4
        end
        object DD_OriginalCreditorIdentifier_Edit: TEdit
          Left = 128
          Top = 152
          Width = 225
          Height = 21
          MaxLength = 35
          TabOrder = 5
        end
      end
      object DD_Tips_GroupBox: TGroupBox
        Left = 496
        Top = 16
        Width = 361
        Height = 137
        Caption = 'Some practical tips'
        TabOrder = 4
        object Label25: TLabel
          Left = 8
          Top = 48
          Width = 292
          Height = 13
          Caption = '- Use only one <PmtInf> ("payment information") block per file.'
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
          Caption = '- ISO schema pain.008.001.02 *should* be supported now.'
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
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML files (*.xml)|*.xml|All files|*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save XML file'
    Left = 464
    Top = 624
  end
end
