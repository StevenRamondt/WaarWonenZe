# CLAUDE.md

## Project Context
Choropleth kaarten applicatie voor visualisatie van studentenherkomst per gemeente of wijk.
Gebouwd voor Nederlandse onderwijsinstellingen (mbo, hbo, wo).

## Projectstructuur

```
choropleth_app/
├── config.yml              # Alle configuratie (gebieden, kleuren, etc.)
├── main.R                  # Hoofdscript - run dit bestand
├── R/
│   └── functions.R         # Alle herbruikbare functies
├── data/
│   ├── 01_raw/            # Ruwe data (studenten.xlsx, GPKG)
│   └── reference/         # Referentiebestanden (gemeenten)
└── output/
    ├── plot/              # PNG kaarten
    └── tabel/             # PNG tabellen
```

## Gebruik

1. Pas `config.yml` aan voor je instelling en gewenste gebied
2. Zet studentendata in `data/01_raw/studenten.xlsx`
3. Zet GPKG in `data/01_raw/`
4. Open `main.R` en wijzig `PROFIEL` naar gewenste configuratie
5. Run `main.R`

## Config profielen

- `den_haag`: Gemeente Den Haag, wijkniveau
- `haaglanden`: 9 gemeenten regio Haaglanden, gemeenteniveau
- `zuid_holland`: Alle gemeenten in Zuid-Holland
- `nederland`: Heel Nederland

## Nieuw gebied toevoegen

Voeg toe aan `config.yml`:
```yaml
mijn_regio:
  inherits: default
  gebied:
    naam: "Mijn Regio"
    type: "custom"
    filter:
      gm_codes: ["GM0001", "GM0002"]
  niveau: "gemeente"  # of "wijk"
```

## Belangrijke conventies

- Functies staan in `R/functions.R`, niet in main.R
- Alle paden via `config.yml`, nooit hardcoded
- Data pipeline: 01_raw → verwerking → output
- Geo data niveaus: `gemeente` of `wijk`
- GPKG lagen: `gemeenten_v0` en `wijken_v0`

## Studentendata vereisten

Minimale kolommen:
- `Gemeente` (voor gemeente niveau)
- `Postcode` (voor wijk niveau — wordt via CBS PC6 koppeltabel omgezet naar wijkcode)

## Bij fouten

Als iets niet werkt: beschrijf het probleem en voeg oplossing toe aan dit bestand.
