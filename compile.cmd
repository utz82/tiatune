xm2tiatune.exe
if %ERRORLEVEL% EQU 0 (
  acme main.asm
  if %ERRORLEVEL% EQU 0 (
    stella test.bin
  )
)
