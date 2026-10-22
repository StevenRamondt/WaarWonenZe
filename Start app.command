#!/bin/bash
# Dubbelklik om de app te starten (macOS).
# De eerste keer worden pixi, R en alle packages automatisch geïnstalleerd.

cd "$(dirname "$0")" || exit 1

PIXI="$HOME/.pixi/bin/pixi"
command -v pixi >/dev/null 2>&1 && PIXI="$(command -v pixi)"

if [ ! -x "$PIXI" ]; then
  echo "pixi wordt eenmalig geïnstalleerd..."
  curl -fsSL https://pixi.sh/install.sh | PIXI_NO_PATH_UPDATE=1 bash || {
    echo "Installatie van pixi mislukt. Controleer je internetverbinding."
    read -r -p "Druk op Enter om te sluiten."
    exit 1
  }
fi

echo
echo "App starten. De eerste keer duurt dit enkele minuten (R wordt gedownload)."
echo "De app opent vanzelf in je browser. Sluit dit venster om de app te stoppen."
echo

"$PIXI" run app
