@echo off
REM run.bat - Run analysis R scripts (Windows cmd.exe)
REM Usage: double-click or run from project root in cmd.exe

setlocal



































exit /b 0endlocalpause
necho All scripts completed successfully.)  exit /b %errorlevel%  pause  echo ERROR: "03_table6_menu_usage.R" failed with exit code %errorlevel%.if errorlevel 1 (Rscript analysis/03_table6_menu_usage.R
n:: Script 3)  exit /b %errorlevel%  pause  echo ERROR: "02_table5_conversion.R" failed with exit code %errorlevel%.if errorlevel 1 (Rscript analysis/02_table5_conversion.R
n:: Script 2)  exit /b %errorlevel%  pause  echo ERROR: "01_table3_4_search_behavior.R" failed with exit code %errorlevel%.if errorlevel 1 (Rscript analysis/01_table3_4_search_behavior.R
n:: Script 1
necho Running analysis scripts...)  exit /b 1  pause  echo Please install R and ensure Rscript is on your PATH.  echo ERROR: Rscript not found in PATH.if errorlevel 1 (where Rscript >nul 2>&1:: Check that Rscript is available on PATH