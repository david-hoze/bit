@echo off
REM Install git hooks for the bit repository
REM Run this once after cloning the repository

setlocal

REM Check if we're in a git repository
if not exist ".git" (
    echo Error: Not in a git repository root. Run this from the repository root.
    exit /b 1
)

REM Create .git/hooks directory if it doesn't exist
if not exist ".git\hooks" mkdir ".git\hooks"

REM Copy pre-commit hook
echo Installing pre-commit hook...
copy /Y "scripts\pre-commit" ".git\hooks\pre-commit" >nul
if errorlevel 1 (
    echo Error: Failed to copy pre-commit hook
    exit /b 1
)

echo.
echo ========================================
echo Git hooks installed successfully!
echo ========================================
echo.
echo The pre-commit hook will scan test files for dangerous patterns
echo (like %%CD%%, %%USERPROFILE%%, etc.) and block commits that contain them.
echo.
echo These patterns are dangerous because Windows expands them before
echo command chains execute, which can cause commands to escape the test
echo sandbox and modify the main repository.
echo.

exit /b 0
