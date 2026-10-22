@echo off
rem Dubbelklik om de app te starten (Windows).
rem De eerste keer worden pixi, R en alle packages automatisch geinstalleerd.

cd /d "%~dp0"

set "PIXI=%USERPROFILE%\.pixi\bin\pixi.exe"
where pixi >nul 2>nul && set "PIXI=pixi"

if not "%PIXI%"=="pixi" if not exist "%PIXI%" (
  echo pixi wordt eenmalig geinstalleerd...
  powershell -NoProfile -ExecutionPolicy Bypass -Command "& ([scriptblock]::Create((Invoke-RestMethod https://pixi.sh/install.ps1))) -NoPathUpdate"
  if not exist "%PIXI%" (
    echo Installatie van pixi mislukt. Controleer je internetverbinding.
    pause
    exit /b 1
  )
)

echo.
echo App starten. De eerste keer duurt dit enkele minuten (R wordt gedownload).
echo De app opent vanzelf in je browser. Sluit dit venster om de app te stoppen.
echo.

"%PIXI%" run app
pause
