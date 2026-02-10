@echo off
REM Run fast test suites: unit/lint + subset of CLI shell tests + literate docs.
REM Uses cli-fast (no gdrive, no device prompt). Expect runtime over 3 minutes.
cd /d "%~dp0\.."
cabal test lint-tests pipeline device-prompt cli-fast generate-literate-docs
