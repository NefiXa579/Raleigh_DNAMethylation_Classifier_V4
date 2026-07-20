@echo off
title Meningioma Methylation Classifier
cd /d "%~dp0"
echo ============================================================
echo    MENINGIOMA METHYLATION CLASSIFIER
echo ============================================================
echo.
echo    Starting the website... please WAIT about a minute.
echo    Your web browser will open AUTOMATICALLY when ready.
echo.
echo    If it does not open, go to:  http://localhost:7771
echo.
echo    KEEP THIS WINDOW OPEN while using the website.
echo    To stop the website, close this window.
echo ============================================================
echo.
REM --- Adjust this path if R is installed somewhere else on your machine ---
"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "%~dp0launch.R"
echo.
echo ============================================================
echo    The website has stopped, or an error occurred above.
echo    Read any red text above, then close this window.
echo ============================================================
pause
