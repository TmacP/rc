@echo off
setlocal

REM Put Git’s tools first so Emacs finds ssh.exe/git.exe from Git for Windows
set "PATH=C:\Program Files\Git\usr\bin;C:\Program Files\Git\bin;%PATH%"

REM Launch Emacs (regular Windows GUI) with the PATH above
start "" "C:\Users\trm003\Downloads\bin\runemacs.exe"

endlocal
