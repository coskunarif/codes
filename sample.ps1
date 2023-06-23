$processes = Get-Process

foreach ($process in $processes)
{
    $cpuUsage = ($process.CPU * 100) / (Get-WmiObject -Class Win32_Processor).MaxClockSpeed

    if ($cpuUsage -gt 50)
    {
        # If so, print the process name and its CPU usage
        Write-Host ("Process: " + $process.Name + ", CPU Usage: " + $cpuUsage + "%")
    }
}
