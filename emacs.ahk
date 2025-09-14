#Requires AutoHotkey v2.0
#SingleInstance Force

; -----------------------------
; In GUI Emacs
; -----------------------------
#HotIf WinActive("ahk_exe emacs.exe") || WinActive("ahk_exe runemacs.exe")

; Remaps:
LAlt::LCtrl      ; Alt → Ctrl
RAlt::RCtrl
LWin::LAlt       ; Win → Alt  (Emacs treats Alt as Meta if w32-alt-is-meta is t)
RWin::RAlt
LCtrl::LWin      ; Ctrl → Win
RCtrl::RWin
#HotIf

; -----------------------------
; In common terminals (emacs -nw)
; -----------------------------
#HotIf WinActive("ahk_exe mintty.exe") || WinActive("ahk_exe WindowsTerminal.exe") || WinActive("ahk_exe conhost.exe") || WinActive("ahk_exe cmd.exe") || WinActive("ahk_exe powershell.exe")

; Same remaps
LAlt::LCtrl
RAlt::RCtrl
LWin::LAlt
RWin::RAlt
LCtrl::LWin
RCtrl::RWin
#HotIf

; -----------------------------
; Optional: open Start with Ctrl+Esc (since bare Ctrl now = Win)
; -----------------------------
^Esc::Send("{LWin down}{LWin up}")
