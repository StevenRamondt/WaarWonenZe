# Waar wonen ze?

Choropleth kaarten voor visualisatie van studentenherkomst per gemeente of wijk. Gebouwd voor Nederlandse onderwijsinstellingen (mbo, hbo, wo).

## Quickstart

Je hoeft zelf niets te installeren; R en alle packages worden automatisch binnengehaald.

1. Download deze repository: **Code → Download ZIP**, en pak de zip uit.
2. Dubbelklik op **`Start app.command`** (macOS) of **`Start app.bat`** (Windows).
3. De app opent in je browser. Klik op **Gebruik demodata** en daarna op **Genereer kaart & tabel**.

Werkt het? Upload dan je eigen export (`.xlsx`) in plaats van de demodata. Sluit het terminalvenster om de app te stoppen.

De eerste start duurt enkele minuten: [pixi](https://pixi.sh) installeert R en de packages (ca. 1,5 GB) in een eigen map binnen het project. Je bestaande software wordt niet aangeraakt. Daarna start de app binnen enkele seconden. De app draait volledig lokaal; geüploade data verlaat je computer niet.

**macOS: "kan niet worden geopend"?** macOS blokkeert scripts uit een download. Ga naar **Systeeminstellingen → Privacy en beveiliging** en klik bij de melding over `Start app.command` op **Open toch**. Dit is eenmalig.

**Windows: "Windows heeft uw pc beschermd"?** Klik op **Meer informatie → Toch uitvoeren**.

Al pixi geïnstalleerd, of op Linux? Run dan `pixi run app` in de projectmap.

## Studentendata

Upload via de app als `.xlsx`. Minimale kolommen (download een voorbeeld via **Download template** in de app):

| Kolom | Gebruikt voor | Voorbeeld |
|-------|---------------|-----------|
| `Gemeente` | Gemeenteniveau | `'s-Gravenhage`, `Den Haag`, `Rijswijk (ZH.)` |
| `Postcode` | Wijkniveau (via CBS koppeltabel) | `2612PD` of `2612 pd` |

Exports uit de inschrijvingscube (kolomnamen als `[Gemeente]`) werken direct. Overige kolommen zoals `IsBekostigd`, `Leerweg` of `Niveau` kun je in de app als **filter** gebruiken.

De app draait alleen lokaal: geüploade data blijft op je eigen computer. Studentdata hoort nooit in deze repository; `.gitignore` sluit `data/01_raw/` en Excel-bestanden uit.

Na het genereren laat de app zien hoeveel studenten op de kaart staan, buiten het gebied wonen of niet herkend zijn. Onder **Info** staat welke waarden niet herkend werden.

## Gebruik zonder app (main.R)

Zet je export in `data/01_raw/studenten.xlsx` (zonder dat bestand gebruikt `main.R` de demodata), kies het profiel bovenin `main.R` en run:

```sh
pixi run kaart
```

Output verschijnt in `output/plot/` en `output/tabel/`.

## Ontwikkelen

Met pixi:

```sh
pixi run test           # tests
pixi run prepare-data   # referentiedata opnieuw maken
```

Liever in je eigen RStudio? Run eenmalig `setup.R` (installeert de packages in je eigen R) en open daarna `app.R` → **Run App**.

## Profielen

| Profiel | Gebied | Niveau | Bijzonderheden |
|---------|--------|--------|----------------|
| `den_haag` | Den Haag | wijk | namen op kaart |
| `haaglanden` | Haaglanden (9 gemeenten) | gemeente | klassen 1-200 … 8001-10000 |
| `zuid_holland` | Zuid-Holland | gemeente | |
| `nederland` | Nederland | gemeente | eigen klassen 1-20 … 9001-10000 |

Alle profielen filteren standaard op `IsBekostigd = TRUE` (als die kolom bestaat). Eigen profielen, filters en klassegrenzen voeg je toe in `config.yml`.

## Referentiedata

De app gebruikt compacte, voorbewerkte CBS-bestanden in `data/reference/` (meegeleverd, ~2,5 MB):

| Bestand | Inhoud |
|---------|--------|
| `gemeenten_2025.rds` | Gemeentegrenzen + provincie |
| `wijken_2025.rds` | Wijkgrenzen |
| `pc6_2025.rds` | Postcode (PC6) → wijk en gemeente |

Bijwerken naar een nieuwe jaargang: pas jaar en URL's aan in `scripts/prepare_data.R` en run `Rscript scripts/prepare_data.R`.

Bron: [CBS Wijk- en buurtkaart 2025](https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/wijk-en-buurtkaart-2025), [CBS Buurt, wijk en gemeente 2025 voor postcode huisnummer](https://www.cbs.nl/nl-nl/maatwerk/2025/38/buurt-wijk-en-gemeente-2025-voor-postcode-huisnummer), [CBS Gemeentelijke indeling 2025](https://www.cbs.nl/nl-nl/onze-diensten/methoden/classificaties/overig/gemeentelijke-indelingen-per-jaar/indeling-per-jaar/gemeentelijke-indeling-op-1-januari-2025). Licentie CC BY 4.0.

## Demodata

`data/voorbeeld/studenten_demo.xlsx` bevat 15.000 **fictieve** studenten met dezelfde kolommen als de inschrijvingscube. De data is met `scripts/maak_demo_data.R` gegenereerd uit geaggregeerde verdelingen (alleen groepen van ≥ 5 per postcodegebied en ≥ 10 per opleidingscombinatie). Er is geen enkele echte rij overgenomen.

## Tests

`pixi run test`, of in R: `source("tests/testthat.R")`. De tests draaien ook automatisch op GitHub bij elke push, in dezelfde pixi-omgeving.

## Licentie

MIT
