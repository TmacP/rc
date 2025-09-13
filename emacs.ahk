#Requires AutoHotkey v2.0
#SingleInstance Force

; Activate in GUI Emacs:
#HotIf WinActive("ahk_exe emacs.exe") || WinActive("ahk_exe runemacs.exe")
LAlt::LCtrl      ; Alt -> Ctrl
RAlt::RCtrl
LWin::LAlt       ; Win -> Alt  (Emacs will treat Alt as Meta)
RWin::RAlt
#HotIf

; Activate in common terminals for emacs -nw:
#HotIf WinActive("ahk_exe mintty.exe") || WinActive("ahk_exe WindowsTerminal.exe") || WinActive("ahk_exe conhost.exe") || WinActive("ahk_exe cmd.exe") || WinActive("ahk_exe powershell.exe")
LAlt::LCtrl
RAlt::RCtrl
LWin::LAlt
RWin::RAlt
#HotIf

; Optional: a way to still open Start when you need it
^Esc::Send("{LWin down}{LWin up}")   ; Ctrl+Esc opens Start
