$processes = Get-Process

foreach ($process in $processes)
{
    $cpuUsage = ($process.CPU * 100) / (Get-WmiObject -Class Win32_Processor).MaxClockSpeed

    if ($cpuUsage -gt 50)
    {
        Write-Host ("Process: " + $process.Name + ", CPU Usage: " + $cpuUsage + "%")
    }
}
