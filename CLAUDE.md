# CLAUDE.md

## Project Context
Choropleth kaarten applicatie voor visualisatie van studentenherkomst per gemeente of wijk.
Gebouwd voor Nederlandse onderwijsinstellingen (mbo, hbo, wo).

## Projectstructuur

```
├── app.R                   # Shiny app
├── main.R                  # Zelfde pipeline als script (output naar output/)
├── Start app.command/.bat  # Dubbelklik-starters (installeren pixi + R automatisch)
├── pixi.toml / pixi.lock   # R-omgeving (conda-forge) en taken: app, kaart, test
├── setup.R                 # Alternatief: packages in eigen R/RStudio installeren
├── config.yml              # Alle configuratie (profielen, filters, klassen, kleuren)
├── R/
│   └── functions.R         # Alle herbruikbare functies (pipeline: genereer_kaart())
├── scripts/
│   ├── prepare_data.R      # Download + verklein CBS-referentiedata
│   └── maak_demo_data.R    # Synthetische demodata uit een echte export
├── data/
│   ├── reference/          # Voorbewerkte CBS-data (.rds, in git)
│   ├── voorbeeld/          # Fictieve demodata + template (in git)
│   └── 01_raw/             # Echte studentdata (NIET in git)
├── tests/                  # testthat tests (Rscript tests/testthat.R)
└── output/                 # Gegenereerde PNG/CSV (niet in git)
```

## Gebruik

1. Dubbelklik `Start app.command` (macOS) / `Start app.bat` (Windows), of `pixi run app`
2. In de app: "Gebruik demodata" of upload eigen `.xlsx`
3. Of: zet data in `data/01_raw/studenten.xlsx`, kies `PROFIEL` in main.R en `pixi run kaart`
4. Tests: `pixi run test`

De app is **alleen voor lokaal gebruik** (geen hosting). Nieuwe R-packages toevoegen via
`pixi add r-<naam>` zodat `pixi.lock` bijgewerkt wordt; anders werkt de starter niet.

## Config profielen

- `den_haag`: Gemeente Den Haag, wijkniveau
- `haaglanden`: 9 gemeenten regio Haaglanden, gemeenteniveau (referentiescenario)
- `zuid_holland`: Alle gemeenten in Zuid-Holland
- `nederland`: Heel Nederland, eigen klassegrenzen (referentiescenario)

## Nieuw gebied toevoegen

Voeg toe aan `config.yml`:
```yaml
mijn_regio:
  inherits: default
  gebied:
    naam: "Mijn Regio"
    type: "custom"
    filter:
      gm_codes: ["GM0001", "GM0002"]   # of: provincie: "Zuid-Holland"
  niveau: "gemeente"  # of "wijk"
  filters:
    Leerweg: ["BBL"]
  breaks_preset: "eigen"
  breaks: [1, 20, 150, 500]
```

## Belangrijke conventies

- Functies staan in `R/functions.R`, niet in app.R of main.R
- app.R en main.R gebruiken beide `genereer_kaart()`; geen pipeline dupliceren
- Alle paden via `config.yml` en `here::here()`, nooit hardcoded
- Geo data niveaus: `gemeente` of `wijk`
- Koppelen op **code** (GM_CODE/WK_CODE), niet op naam
- Klassen zijn rechts-gesloten: breaks `c(1, 200, 500)` → `1-200`, `201-500`
- Nooit echte studentdata committen; de repo is publiek
- Starters: `.bat` met CRLF, `.command` met LF en executable bit (zie .gitattributes)

## Studentendata vereisten

Minimale kolommen:
- `Gemeente` (voor gemeente niveau; naam wordt via alias/normalisatie naar GM_CODE vertaald)
- `Postcode` (voor wijk niveau — wordt via CBS PC6 koppeltabel omgezet naar wijkcode)

Kolomnamen tussen blokhaken (`[Gemeente]`, cube-export) worden automatisch opgeschoond.

## Bij fouten

Als iets niet werkt: beschrijf het probleem en voeg oplossing toe aan dit bestand.

- **`show.legend = "polygon"` crasht in ggplot2 ≥ 4.0** (`x must be a vector, not NULL`). Gebruik `show.legend = TRUE`; dat is ook nodig om lege klassen in de legenda te tonen.
- **dplyr-verbs op een sf-object uit `readRDS()` falen** als sf niet geladen is. `laad_geo_data()` laadt sf daarom expliciet.
- **Zee/strand bij kustgemeenten**: de CBS GPKG heeft een kolom `water` (`JA`/`NEE`/`B`); `prepare_data.R` houdt alleen `NEE`.
