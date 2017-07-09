## 0.3.0 Beta (WIP, unveröffentlicht)

  - Unterstützung für pain.001.001.03 und pain.008.001.02 nach Version 3.0 der Spezifikation.
  - Ohne Angabe eines Schema ist der Standard weiterhin pain.001.003.03 und pain.008.003.02.
  - Nicht abwärtskompatible Änderung: In TMandateRelatedInformation wurde AmdmntInfDtls zu AmdmntInfDtls26 umbenannt, zusätzlich existiert AmdmntInfDtls30. Je nach Schema wird dann gewählt, welche Daten für die XML-Datei verwendet werden.

## 0.2.4 Beta (2016-10-01)

  - Unterstützung für Lazarus 1.6 mit FPC 3.0 hinzugefügt.

## 0.2.3 Beta (2016-09-03)

  - Unit-Tests vervollständigt.

## 0.2.2 Beta (2014-02-27)

  - Memory-Leaks entfernt.

## 0.2.1 Beta (2014-02-25)

  - Datumsabhängige Entscheidungen bzgl. Datei-Schema und -Validierung im Code entfernt (Stichdatum 01.02.2014 für Wahl vom Standard 2.7 und Zulässigkeit von IBAN-only).
  - Bugfix für Kompatibilität zu Delphi 7 bis 2010.

## 0.2.0 Beta (2014-02-20)

  - Unterstützung für SEPA-Lastschriften. Hierzu wurde die Funktionalität in drei Dateien aufgesplittet: Allgemeines (SEPACommon.pas), Lastschriften (SEPADirectDebit.pas), Überweisungen (SEPACreditTransfer.pas). Um Namenskonflikte zu vermeiden, musste die bisherige Klasse TPaymentInstructionInformation in TDirectDebitPaymentInformation umbenannt werden.
  - Verwende immer Currency statt Double für Geldbeträge, um Gleitkomma-/Rundungsprobleme zu vermeiden.
  - Über eine globale Variable (SEPASupportSpecialChars) kann die Möglichkeit aktiviert werden, deutsche Sonderzeichen in die XML-Dateien zu schreiben. Den Banken ist es allerdings freigestellt, wie sie mit diesen umgehen, also ob sie konvertiert oder tatsächlich benutzt werden.
  - Teilweise Implementierung von Unit-Tests: vorerst nur für die Units SEPACommon.pas und die neu hinzugekommene SEPACreditTransfer.pas. Die Unit-Tests sind lauffähig unter Delphi (auf Basis von [DUnit](http://dunit.sourceforge.net/)) und Lazarus (fpcunit).
  - Danksagung: Unterstützung der Weiterentwicklung durch A to C DataSolutions, www.a-to-c.de, vielen Dank!

## 0.1.3 Beta (2014-01-17)

  - Bugfix in Mandatsänderungen-Validierung: originale Gläubiger-Id.-Nr. darf leer sein.

## 0.1.2 Beta (2014-01-12)

  - Kompatibilität zu Lazarus inkl. Beispielanwendung.

## 0.1.1 Beta (2014-01-04)

  - Überarbeitung für bessere Kompatibilität zu verschiedenen Delphi-Versionen (Delphi 6 und höher).

## 0.1.0 Beta (2013-12-26)

  - Re-Release der eigentlichen Unit als Beta-Version.
  - Erweiterung des Beispielprogramms um einige praktische Hinweise ("Tipps zur Verwendung").

## 0.0.5 Alpha (2013-11-08)

  - Wähle Schema pain.008.003.02 vorerst nur wenn COR1 benötigt wird, da es offenbar nicht von allen Banken unterstützt wird. Erst ab dem 01.02.2014 wird immer pain.008.003.02 gewählt.
  - Validierung: Erlaube IBAN-only nur für deutsche Konten.

## 0.0.4 Alpha (2013-09-24)

  - Verbesserte Validierung von IBAN, BIC und Gläubiger-Identifikationsnummer.
  - Erweiterung des Zeichensatzes, automatische Konvertierung einiger Sonderzeichen (z.B. Umlaute).

## 0.0.3 Alpha (2013-09-22)

  - Unterstützung für COR1 und IBAN-only nach Version 2.7 der Spezifikation (pain.008.003.02).
  - Einige Korrekturen bzw. Verbesserungen.

## 0.0.2 Alpha (2013-09-09)

  - Einige Korrekturen bzw. Verbesserungen.
  - Beispielanwendung hinzugefügt.

## 0.0.1 Alpha (2013-08-23)

  - Erstes Release.
