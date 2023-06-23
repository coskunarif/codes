function Get-Instance([string]$version) {
    $hostname = [System.NET.DNS]::GetHostByName($null).HostName
    $instance = (Get-WmiObject -namespace root\Microsoft\SqlServer\ReportServer  -class __Namespace -ComputerName $hostname | select Name).Name
    $namespaceAdminPath = "root\Microsoft\SqlServer\ReportServer\$instance\v$version"
    return Get-WmiObject -namespace $namespaceAdminPath -class MSReportServer_Instance -ComputerName $hostname 
}
function Get-ConfigSet([string]$version) {
    $hostname = [System.NET.DNS]::GetHostByName($null).HostName
    $instance = (Get-WmiObject -namespace root\Microsoft\SqlServer\ReportServer  -class __Namespace -ComputerName $hostname | select Name).Name
    $namespaceAdminPath = "root\Microsoft\SqlServer\ReportServer\$instance\v$version\Admin"
    return Get-WmiObject -namespace $namespaceAdminPath -class MSReportServer_ConfigurationSetting -ComputerName $hostname 
}
