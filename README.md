# Delphi-Unit zur Erstellung von SEPA-XML-Dateien

Ziel dieses Projekts ist eine einzelne Delphi-Unit, mit deren Hilfe man relativ einfach SEPA-XML-Dateien erstellen kann, speziell für Überweisungen und Lastschriftaufträge in Deutschland. Die Pascal-Datei hat (außer einigen wenigen Standardbibliotheken von Delphi) keinerlei Abhängigkeiten, auch die XML-Datei wird mit einfachsten Mitteln erzeugt. Einfach gehaltene Validierungsmöglichkeiten sind enthalten. Derzeit unterstützt sind Version 2.6 (pain.001.002.03, pain.008.002.02), 2.7 bis 2.9 (pain.001.003.03, pain.008.003.02) und 3.0 bis 3.5 (pain.001.001.03, pain.008.001.02) nach [EBICS: Spezifikation der Datenformate](https://www.ebics.de/de/datenformate) veröffentlicht von der Deutschen Kreditwirtschaft.

*Tipps zur Verwendung:*
Für eine bessere Kompatibilität mit verschiedenen deutschen Banken sollte in einer XML-Datei nur ein "\<PmtInf\>"-Block enthalten sein, d.h. z.B., nur ein Lastschrift-Datum und alle Buchungen mit dem gleichen Typ (FRST/RCUR etc.). Das Dateiformat unterstützt zwar mehrere dieser Blöcke, aber nicht alle Banken akzeptieren dies. Zudem sollte die Anzahl der Datensätze limitiert werden, die maximale Anzahl wie z.B. 500 ist bei der eigenen Bank anzufragen.

*Achtung:*
Die Software war beim Autor einige Jahre im eingeschränkten Produktiveinsatz und hat Beta-Qualität. Getestet wurde die Unit unter Delphi 6 Professional und Lazarus (mit FPC 2.6 und 3.0), sie sollte auch mit neueren Versionen funktionieren.
Soweit gesetzlich zulässig ist jegliche Haftung ausgeschlossen! Für mehr Details siehe Lizenztext.

## Autor / Lizenz:

  - Autor: Aaron Spettl (aaron@spettl.de)
  - Lizenz: wahlweise [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) *oder* [GPL (General Public License), Version 2](http://www.gnu.de/documents/gpl-2.0.de.html) oder höher

## Homepage:

  - https://www.spettl.com/delphi-sepa-xml/
  - https://github.com/aspettl/delphi-sepa-xml
