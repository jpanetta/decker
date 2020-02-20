<#
.SYNOPSIS
    Builds and copies vendor libraries to the support folder
.DESCRIPTION
    Counterpart to symlinks.mk for Windows. As symlinks are not properly 
    supported, all required files are copied.
#>

$third = Split-Path -parent $PSCommandPath
$decker = Split-Path $third -parent
$support = $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath("$decker\resource\support\vendor")

Write-Host "Building third party dependencies" -ForegroundColor Green

# Build jquery
Set-Location "$third\jquery"
& npm run build

# Build thebelab
Set-Location "$third\thebelab"
& npm install
& npm run build

Write-Host ("Copy third party dependencies to " + $support) -ForegroundColor Green
New-Item -Path $support -Force -ItemType "directory"
Set-Location $third

# Copy jquery
Copy-Item "$third\jquery\dist\jquery.min.js" "$support\jquery.js" -Force

# Copy thebelab
New-Item -Path "$support\thebelab" -Force -ItemType "directory"
Copy-Item $third\thebelab\lib\*.js "$support\thebelab" -Force
Copy-Item $third\thebelab\lib\*.map "$support\thebelab" -Force

# Copy mathjax
New-Item -Path "$support\mathjax\input" -Force -ItemType "directory"
New-Item -Path "$support\mathjax\output" -Force -ItemType "directory"
Foreach ($i in ("tex-svg.js", "input\tex", "input\tex.js". "output\svg", "output\svg.js")) {
  Copy-Item -Recurse "$third\MathJax\es5\$i" "$support\mathjax\$i" -Force
}

# Copy reveal.js
New-Item "$support\reveal\plugin" -Force -ItemType "directory"
New-Item "$support\reveal\plugin\markdown" -Force -ItemType "directory"
New-Item "$support\reveal\plugin\markdown" -Force -ItemType "directory"
Copy-Item "$third\reveal.js\plugin\markdown\marked.js" "$support\reveal\plugin\markdown\marked.js" -Force
Foreach ($i in ("js", "css", "lib", "plugin\math", "plugin\zoom-js", "plugin\notes")) {
  Copy-Item -r "$third\reveal.js\$i" "$support\reveal\$i" -Force
}

# Copy bootstrap
New-Item "$support\bootstrap" -Force -ItemType "directory"
Copy-Item -Recurse "$third\bootstrap\dist\css" "$support\bootstrap\css" -Force

# Copy piklor.js
Copy-Item -Recurse "$third\piklor.js\src\piklor.min.js" "$support\piklor.js" -Force

# Copy fontawesome
New-Item "$support\fontawesome" -Force -ItemType "directory"
Foreach ($i in ( "js", "css", "webfonts", "svgs", "sprites")) {
  Copy-Item -Recurse "$third\Font-Awesome\js-packages\@fortawesome\fontawesome-free\$i" "$support\fontawesome\$i" -Force
}