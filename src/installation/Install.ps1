param($installPath, $toolsPath, $package, $project)

$configItem = $project.ProjectItems.Item("OpenCL.Net.dll.config")

$copyToOutput = $configItem.Properties.Item("CopyToOutputDirectory")
$copyToOutput.Value = 1

$buildAction = $configItem.Properties.Item("BuildAction")
$buildAction.Value = 2

