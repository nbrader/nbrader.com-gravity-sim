REM If the uglifyjs command is not available in your terminal, you can run the command npm install uglify-js --global to download it. If you do not have npm either, you can get it with nodejs (https://nodejs.org/en).

@echo off
setlocal

set js=GravitySim.js
set min=GravitySim.min.js

call elm make src/Main.elm --optimize --output=%js%

call uglifyjs %js% --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output %min%

REM for %%I in (%js%) do set size=%%~zI
REM echo Initial size: %size% bytes  (%js%)

REM for %%I in (%min%) do set size=%%~zI
REM echo Minified size: %size% bytes  (%min%)

REM powershell.exe -nologo -noprofile -command "& {Compress-Archive -Path '%min%' -DestinationPath '%min%.zip' -CompressionLevel Optimal -Force}"
REM for %%I in (%min%.zip) do set size=%%~zI
REM echo Zipped size: %size% bytes

pause
endlocal
