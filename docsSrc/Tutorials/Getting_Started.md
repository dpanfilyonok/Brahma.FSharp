# Installation

    [lang=bash]
    paket install Brahma.FSharp

# Troubleshooting

## Timeout

Sometimes calculations could be interrupted buy GPU driver (OS) timeout (TDR). 
For hot fix you can set TdrLevel registry key (KeyPath : HKEY\_LOCAL\_MACHINE\System\CurrentControlSet\Control\GraphicsDrivers) value to 0. 
If this key is not exists, then you should crete it. For more details look at ["TDR Registry Keys (Windows Drivers)"][tdr].

## OpenCL.dll

If you want to use Brahma.FSharp on Linux/macOS, check OpenCL.Net.dll.config after installation and fix path to opencl.dll if necessary.

