# Parallel test runner using cabal build + shelltest.
# Usage: powershell -ExecutionPolicy Bypass -File test\cli\run-cabal-parallel.ps1 [-Exclude <pattern>]
#
# Builds bit via cabal, then runs each .test file in parallel using shelltest
# with the cabal-built binary on PATH.
# Tests listed in $serial are run sequentially after the parallel batch.

param(
    [string]$Exclude = ""
)

$ErrorActionPreference = "Continue"
$projectRoot = (Get-Location).Path
$testDir = "test\cli"

# Prevent findBitRoot from walking past test output dirs into the parent repo
$env:BIT_CEILING_DIRECTORIES = "$projectRoot\test\cli\output"

# Tests that must run sequentially (contend on remote-init resources)
$serial = @("remote-flag.test", "remote-targeted.test")

# Step 1: Build bit via cabal
Write-Host "Building bit..." -ForegroundColor Cyan
cabal build bit 2>&1 | Out-Null
if ($LASTEXITCODE -ne 0) {
    Write-Host "cabal build failed" -ForegroundColor Red
    exit 1
}

# Step 2: Get the cabal-built binary directory and prepend to PATH
$bitBin = (cabal list-bin bit 2>$null).Trim()
$bitDir = Split-Path $bitBin -Parent
$env:PATH = "$bitDir;$env:PATH"
Write-Host "Using bit from: $bitDir" -ForegroundColor Gray

function Run-Shelltest($file) {
    $output = shelltest --debug $file 2>&1 | Out-String
    $exitCode = $LASTEXITCODE
    $passed = 0; $failed = 0
    if ($output -match "Passed\s+(\d+)") { $passed = [int]$Matches[1] }
    if ($output -match "Failed\s+(\d+)") { $failed = [int]$Matches[1] }
    [PSCustomObject]@{
        Name     = (Split-Path $file -Leaf)
        Passed   = $passed
        Failed   = $failed
        ExitCode = $exitCode
        Output   = $output
    }
}

# Run global cleanup first
Write-Host "Running global cleanup..." -ForegroundColor Cyan
$cleanupResult = shelltest --debug "$testDir\000-cleanup.test" 2>&1
if ($LASTEXITCODE -ne 0) {
    Write-Host "Cleanup failed:" -ForegroundColor Red
    Write-Host $cleanupResult
    exit 1
}

# Collect test files (exclude 000-cleanup and any user-specified pattern)
$allTests = Get-ChildItem "$testDir\*.test" |
    Where-Object { $_.Name -ne "000-cleanup.test" } |
    Where-Object { $Exclude -eq "" -or $_.Name -notmatch $Exclude } |
    Sort-Object Name

$parallelTests = $allTests | Where-Object { $_.Name -notin $serial }
$serialTests   = $allTests | Where-Object { $_.Name -in $serial }

$stopwatch = [System.Diagnostics.Stopwatch]::StartNew()

# Phase 1: parallel
Write-Host "Running $($parallelTests.Count) test files in parallel..." -ForegroundColor Cyan
$jobs = $parallelTests | ForEach-Object {
    $file = $_.FullName
    $pathForJob = $env:PATH
    Start-Job -ScriptBlock {
        param($file, $pathEnv)
        $env:PATH = $pathEnv
        $output = shelltest --debug $file 2>&1 | Out-String
        $exitCode = $LASTEXITCODE
        $passed = 0; $failed = 0
        if ($output -match "Passed\s+(\d+)") { $passed = [int]$Matches[1] }
        if ($output -match "Failed\s+(\d+)") { $failed = [int]$Matches[1] }
        [PSCustomObject]@{
            Name     = (Split-Path $file -Leaf)
            Passed   = $passed
            Failed   = $failed
            ExitCode = $exitCode
            Output   = $output
        }
    } -ArgumentList $file, $pathForJob
}

$results = @($jobs | Wait-Job | Receive-Job)
$jobs | Remove-Job

# Phase 2: serial
if ($serialTests.Count -gt 0) {
    Write-Host "Running $($serialTests.Count) test files sequentially..." -ForegroundColor Cyan
    foreach ($t in $serialTests) {
        $results += Run-Shelltest $t.FullName
    }
}

$stopwatch.Stop()

# Summary
$totalPassed = ($results | Measure-Object -Property Passed -Sum).Sum
$totalFailed = ($results | Measure-Object -Property Failed -Sum).Sum
$failedFiles = $results | Where-Object { $_.Failed -gt 0 -or $_.ExitCode -ne 0 }

foreach ($r in $results | Sort-Object Name) {
    if ($r.Failed -gt 0 -or $r.ExitCode -ne 0) {
        Write-Host ("  FAIL  {0}  ({1} passed, {2} failed)" -f $r.Name, $r.Passed, $r.Failed) -ForegroundColor Red
    } else {
        Write-Host ("  OK    {0}  ({1} passed)" -f $r.Name, $r.Passed) -ForegroundColor Green
    }
}

Write-Host ""
Write-Host ("Total: {0} passed, {1} failed  ({2:N1}s)" -f $totalPassed, $totalFailed, $stopwatch.Elapsed.TotalSeconds)

if ($failedFiles) {
    Write-Host ""
    Write-Host "=== FAILURE DETAILS ===" -ForegroundColor Red
    foreach ($r in $failedFiles | Sort-Object Name) {
        Write-Host ""
        Write-Host "--- $($r.Name) ---" -ForegroundColor Red
        Write-Host $r.Output
    }
    exit 1
}

exit 0
