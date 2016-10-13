;-*- coding: utf-8 -*-
;; Ergohotkey
;; A AutopairHotkey script for system-wide ErgoEmacs keybinding
;;
;;   Copyright © 2009 Milan Santosi
;;   Copyright © 2012 Benjamin Hansen
;;   Copyright © 2013, 2014 Matthew Fidler
;;   This program is free software: you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, either version 3 of the License, or
;;   (at your option) any later version.
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with this program.  If not, see http://www.gnu.org/licenses/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global ergonomic editing command shortcuts for 
;; use with autohotkey http://www.autohotkey.com/
;; hotkey layout taken from http://xahlee.org/emacs/ergonomic_emacs_keybinding.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changelog:
;; Changlog moved to github.
;; Version 0.9:
;; - Added beginning and end of buffer commands.
;; Version 0.8:
;; - Added BigCtl, key translation and SetMark
;; Version 0.7:
;; - Added Caps lock to Menu in emacs.
;; Version 0.6:
;; - Unified Script, fixed kill-line-backwards
;; Version 0.5:
;; - Made this generated inside of ergoemacs.  Malfunctioning kill-line-backwards re-included.
;; Version 0.4: 
;; - Fixed a missing colon, that prevents Alt+i to work. Xah Lee
;; Version 0.3:
;; - added a #SingleInstance directive. Xah Lee
;; Version 0.2: 
;; - 'Fixed' malfunctioning kill-line-backwards by remapping it to
;;   something without a shift modifier. Not very happy about it.
;; - Replaced Send with SendInput
;; - Replaced occurences of DEL with C-x to 'kill' to the clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#SingleInstance force
#MaxHotkeysPerInterval 9999
#NoEnv
#InstallKeybdHook 
SendMode Input
Process, priority, , High
IniRead ToggleCtrl, ergoemacs-settings.ini,BigCtl, App
IniRead CurrCaps, ergoemacs-settings.ini, Caps, App
IniRead CurrRAlt, ergoemacs-settings.ini, RAlt, App
IniRead CurrLAlt, ergoemacs-settings.ini, LAlt, App
IniRead CurrRAltLAlt, ergoemacs-settings.ini, RAltLAlt, App
IniRead OutlookSave, ergoemacs-settings.ini, Outlook, Save
IniRead EmacsClient, ergoemacs-settings.ini, Emacs, EmacsClient
IniRead OutlookTemplate, ergoemacs-settings.ini, Outlook, Template
LayLst=
VarLst=
CareL = 0
CareV = 0
CareLV = 0
g_MarkSet=
g_LastBol=
g_LastEol=
g_LastBobp=
modifiers=
skipUpDown=

IniRead CurrLayout, ergoemacs-settings.ini, Curr, Layout
If (CurrLayout == "ERROR"){
  CurrLayout=us
}

IniRead CurrTheme, ergoemacs-settings.ini, Curr, Theme
If (CurrTheme == "ERROR"){
  CurrTheme=standard
} 

IniRead CurrTrans, ergoemacs-settings.ini, Curr, Trans
If (CurrTrans == "ERROR"){
  CurrTrans=No Translation
}

IniRead CurrTrans2, ergoemacs-settings.ini, Curr, Trans2
If (CurrTrans2 == "ERROR"){
  CurrTrans2=No Translation
}

IniRead ExternalClass, ergoemacs.ini,Class,External
If (ExternalClass == "Error"){
  ExternalClass=TscShellContainerClass
}

StringSplit, ExternalClassArray, ExternalClass, *

;; Add Translation
Loop, 120 {
  ; CurLayout -> TransKey
  IniRead CurrKey, ergoemacs.ini, %CurrLayout%, %A_Index%
  If (CurrTrans == "No Translation"){
     objTrans%CurrKey% := CurrKey
  } else {
      If (CurrKey != ""){
       IniRead TransKey, ergoemacs.ini, %CurrTrans%, %A_Index%
       objTrans%CurrKey% := TransKey
      }
  }
  ; HostLayout -> CurLayout
  IniRead CurrKey, ergoemacs.ini, %CurrTrans2%, %A_Index%
  If (CurrTrans2 == "No Translation"){
     objTrans_%CurrKey% := CurrKey
  } else {
      If (CurrKey != ""){
       IniRead TransKey, ergoemacs.ini, %CurrLayout%, %A_Index%
       objTrans_%CurrKey% := TransKey
      }
  }
}

CurrLayVar= % "[" . CurrLayout . "-" . CurrTheme . "]"
Loop, Read, ergoemacs.ini
{
  If (A_LoopReadLine == "[Layouts]"){
     CareL = 1
     CareV = 0
  } Else If (A_LoopReadLine == "[Themes]"){
     CareV = 1
     CareL = 0
  } Else If (CareL == 1 || CareV == 1){
    tmp = %A_LoopReadLine%
    NextSec := InStr(tmp,"[")
    If (NextSec == 1){
      CareL = 0
      CareV = 0
    } else {
      NextSec := InStr(tmp,"=")
      If (NextSec != 0){
          NextSec := SubStr(tmp,1,NextSec-1)
          If (CareL == 1){
            LayLst  = %LayLst%%NextSec%`n
          } else {
            VarLst  = %VarLst%%NextSec%`n
          }          
      }
    }
  } Else If (A_LoopReadLine == CurrLayVar){
    CareLV = 1
  } Else If (CareLV == 1){
    tmp = %A_LoopReadLine%
    NextSec := InStr(tmp,"[")
    If (NextSec == 1){
      CareLV = 0
    } else {
      NextSec := InStr(tmp,"=")
      If (NextSec != 0){
        fn := SubStr(tmp,1,NextSec - 1)
        NextSec := SubStr(tmp,NextSec + 1)
        objTrans%NextSec% := fn
	objTrans_%NextSec% := fn
        ;;HotKey, %NextSec%, %fn%
      }
    }
  }
}

;; HotKey,(,autopair-paren


; Create Menu
Loop, parse, LayLst, `n 
{
        Menu, TranslateKey, add, No Translation, TranslateKeyHandler
	Menu, TranslateKey2, add, No Translation, TranslateKey2Handler
        If (CurrTrans == "No Translation"){
          Menu, TranslateKey, Check,No Translation
        } else {
          Menu, TranslateKey, UnCheck,No Translation 
        }
	If (CurrTrans2 == "No Translation"){
          Menu, TranslateKey2, Check,No Translation
        } else {
          Menu, TranslateKey2, UnCheck,No Translation 
        }
        If (A_LoopField != ""){
           Menu, MenuKey, add, %A_LoopField%, MenuKeyHandler
           Menu, TranslateKey, add, %A_LoopField%, TranslateKeyHandler
	   Menu, TranslateKey2, add, %A_LoopField%, TranslateKey2Handler
           If (A_LoopField == CurrLayout){
              Menu, MenuKey, Check, %A_LoopField%
           } else {
             Menu, MenuKey, UnCheck, %A_LoopField%
           }

           If (A_LoopField == CurrTrans){
              Menu, TranslateKey, Check, %A_LoopField%
           } else {
              Menu, TranslateKey, UnCheck, %A_LoopField%
           }

	   If (A_LoopField == CurrTrans2){
              Menu, TranslateKey2, Check, %A_LoopField%
           } else {
              Menu, TranslateKey2, UnCheck, %A_LoopField%
           }
        }
}

Loop, parse, VarLst, `n
{
        If (A_LoopField != ""){
           Menu, ThemeKey, add, %A_LoopField%, ThemeKeyHandler
           
           If (A_LoopField == CurrTheme){
              Menu, ThemeKey, Check, %A_LoopField%
           } else {
             Menu, ThemeKey, UnCheck, %A_LoopField%
           }
           
        }
}

Menu, Tray, DeleteAll
Menu, Tray, NoStandard
Menu, tray, add, Keyboard Layouts, :MenuKey
Menu, tray, add, Translated Layout, :TranslateKey
Menu, tray, add, Translated Layout (External), :TranslateKey2
Menu, tray, add, Themes, :ThemeKey
Menu, Tray, add
Menu, Caps, add, Caps Lock, ToggleCaps
Menu, Caps, add, Control, ToggleCaps
Menu, Caps, add, Apps Key, ToggleCaps
Menu, Caps, add, F6, ToggleCaps
If (CurrCaps == "Control"){
  Menu, Caps, Check, Control
  Hotkey, CapsLock, send-ctl
  Hotkey, CapsLock Up, send-ctl-up
  ;Hotkey Up, previous-line
  ;Capslock::Ctrl
  ;+Capslock::Capslock
} Else If (CurrCaps == "Apps Key"){
  Menu, Caps, Check, Apps Key
  Hotkey, CapsLock, send-apps 
  ;Capslock::AppsKey
  ;+Capslock::Capslock
} Else if (CurrCaps == "F6"){
  Menu, Caps, Check, F6
  Hotkey CapsLock, send-f6
  ;Capslock::F6
  ;+Capslock::Capslock
} Else {
  Menu, Caps, Check, Caps Lock
}
Menu, Tray, add, Caps Lock To, :Caps

Menu, RAlt, add, Alt, ToggleRAlt
Menu, RAlt, add, Control, ToggleRAlt
Menu, RAlt, add, Apps Key, ToggleRAlt
Menu, RAlt, add, F6, ToggleRAlt
If (CurrRAlt == "Control"){
  Menu, RAlt, Check, Control
  Hotkey, RAlt, send-ctl
  Hotkey, RAlt Up, send-ctl-up
  } Else If (CurrRAlt == "Apps Key"){
  Menu, RAlt, Check, Apps Key
  Hotkey, RAlt, send-apps 
} Else if (CurrRAlt == "F6"){
  Menu, RAlt, Check, F6
  Hotkey RAlt, send-f6
} Else {
  Menu, RAlt, Check, Alt
}
Menu, Tray, add, Right Alt to, :RAlt

; Left Alt
Menu, LAlt, add, Alt, ToggleLAlt
Menu, LAlt, add, Control, ToggleLAlt
Menu, LAlt, add, Apps Key, ToggleLAlt
Menu, LAlt, add, F6, ToggleLAlt
If (CurrLAlt == "Control"){
  Menu, LAlt, Check, Control
  Hotkey, LAlt, send-ctl
  Hotkey, LAlt Up, send-ctl-up
} Else If (CurrLAlt == "Apps Key"){
  Menu, LAlt, Check, Apps Key
  Hotkey, LAlt, send-apps 
} Else if (CurrLAlt == "F6"){
  Menu, LAlt, Check, F6
  Hotkey LAlt, send-f6
} Else {
  Menu, LAlt, Check, Alt
}

Menu, Tray, add, Left Alt to, :LAlt

Menu, RAltLAlt, add, Alt, ToggleRLA
Menu, RAltLAlt, add, Apps Key, ToggleRLA
Menu, RAltLAlt, add, F6, ToggleRLA
If (CurrRAltLAlt == "Apps Key"){
  Menu, RAltLAlt, Check, Apps Key
  Hotkey, RAlt & LAlt, send-apps 
} Else if (CurrRAltLAlt == "F6"){
  Menu, RAltLAlt, Check, F6
  Hotkey RAlt & LAlt, send-f6
} Else {
  Menu, RAltLAlt, Check, Alt
}
Menu, Tray, add, Left & Right Alt to, :RAltLAlt


Menu, Tray, add, Space->Control, ToggleCtrl
If (ToggleCtrl == "1"){
  Menu, Tray, Check, Space->Control
}
Menu, tray, add, Exit, Exit

; The amount of milliseconds of holding the spacebar after which a
; space key is no longer returned.
g_TimeOut := 300

; The amount of milliseconds to delay returning a Ctrl key sequence
; that are potentially accidentally hit with the space bar. If the
; space bar comes up during this delay the regular keys will be
; returned instead. Probably rounds to the nearest 10 milliseconds by
; the OS.
g_Delay := 70

g_SpacePressDownTime := false
g_OtherKeyPressed := false
g_MovementKeyPressed := false
g_SkipNextSpace := false

Hotkey Up, previous-line
Hotkey Down, next-line
Hotkey Left, backward-char
Hotkey Right, forward-char
Hotkey Home, move-beginning-of-line
Hotkey End, move-end-of-line
Hotkey PgUp, scroll-down
Hotkey PgDn, scroll-up
Hotkey ^Left, backward-word
Hotkey ^Right, forward-word

allKeysStr := "LButton*RButton*MButton*WheelDown*WheelUp*WheelLeft*WheelRight*XButton1*XButton2*Tab*Enter*Escape*Backspace*Delete*Insert*ScrollLock*CapsLock*NumLock*Numpad0*Numpad1*Numpad2*Numpad3*Numpad4*Numpad5*Numpad6*Numpad7*Numpad8*Numpad9*NumpadDot*NumpadDiv*NumpadMult*NumpadAdd*NumpadSub*NumpadEnter*F1*F2*F3*F4*F5*F6*F7*F8*F9*F10*F11*F12*F13*F14*F15*F16*F17*F18*F19*F20*F21*F22*F23*F24*AppsKey*Browser_Back*Browser_Forward*Browser_Refresh*Browser_Stop*Browser_Search*Browser_Favorites*Browser_Home*Volume_Mute*Volume_Down*Volume_Up*Media_Next*Media_Prev*Media_Stop*Media_Play_Pause*Launch_Mail*Launch_Media*Launch_App1*Launch_App2*Help*Sleep*PrintScreen*CtrlBreak*Pause*Break"
StringSplit, allKeysArray, allKeysStr, *
Loop %allKeysArray0%
{
   key := allKeysArray%A_Index%
   Hotkey, % "~*"key, ListenForKey
}

; Keys that are possible to accidentally press with the space key
; while typing fast.
keysToDelayStr := "1*2*3*4*5*6*7*8*9*0*q*w*e*r*t*y*u*i*o*p*[*]*\*a*s*d*f*g*h*j*k*l*;*'*z*x*c*v*b*n*m*,*.*/"
StringSplit, keysToDelayArray, keysToDelayStr, *
Loop %keysToDelayArray0%
{
  key := keysToDelayArray%A_Index% 
  Hotkey, % "*"key, DelayKeyOutput
}


ListenForKey:
  g_MarkSet=
  g_OtherKeyPressed := true
  Return

  
DelayKeyOutput:
  Critical
  isExternal := IsExternalProgram()
  origKey := SubStr(A_ThisHotkey,0)
  ;; Get Modifiers
  modifiers := GetModifiers()
  modifiers2 := GetModifiers2()
  if (modifiers2 < 9){
     timesf := 10
  } else {
    timesf := 100
  }
  pressedKey := origKey
  ;; Translate to the correct layout
  transKey := Asc(pressedKey)
  if (isExternal == 1){
     transKey := Chr(objTrans_%transKey%)
  } else {
    transKey := Chr(objTrans%transKey%)
  }
  if (transKey != ""){
    pressedKey := transKey
  }
  ;; get goto subroutine.
  transKey := Asc(origKey)*timesf+modifiers2
  if (isExternal == 1){
      transKey := objTrans_%transKey%
  } else {
    transKey := objTrans%transKey%
  }
  ; Only wait to see if the space comes up if 1) the space bar key is
  ; down in the first place and 2) it has been held down for less than
  ; the timeout and 3) another Ctrl key combo hasn't already been
  ; pressed.
  if((g_SpacePressDownTime != false) 
    && (GetSpaceBarHoldTime() < g_TimeOut) && !g_OtherKeyPressed)
  {
    ; Do the sleeping of timeout in small increments, that way if the
    ; the space key is released in the middle we can quit early.
    wait_start_time := A_TickCount
    while A_TickCount - wait_start_time + 10 < g_Delay
    {
      Sleep, 10
      if(!getKeyState("Space", "P"))
      {
	; Since space bar was released, remove the Ctrl modifier.
	StringReplace, modifiers, modifiers, ^,
        ; Force space to fire, because its being released could not
        ; fire during this routine because this thread is critical.
	Gosub *Space up
        ; Stop the space in the event queue from firing since we
        ; have already fired it manually.
	g_SkipNextSpace := True 
        Break
      }
    }
  }
  if (IsLabel(transKey) & !WinActive("ahk_class Emacs") & !IsExternalProgram()){
    Goto %transKey%
  } Else {
    SendInput % modifiers pressedKey
    g_MarkSet=
    g_LastBol=
    g_LastEol=
    g_LastBobp=
  }
  g_OtherKeyPressed := true
  Return
  
*Space::
  Critical
  ; Don't update on OS simulated repeats but only when the user
  ; actually pressed the key down for the first time
  if(g_SpacePressDownTime == false)
  {
    g_SpacePressDownTime := A_TickCount
    g_OtherKeyPressed := false
  }
  if (ToggleCtrl == "1"){
    SendInput {RCtrl down}  
  }
  Return
  
*Space up::
  Critical
  if(g_SkipNextSpace)
  {
    g_SkipNextSpace := false
  }
  if (ToggleCtrl == "1"){
    SendInput {RCtrl up}
  }
  if(g_OtherKeyPressed == true)
  {
    g_SpacePressDownTime := false
    Return
  }
  if (GetSpaceBarHoldTime() <= g_TimeOut)
  {
    modifiers := GetModifiers()
    if (WinActive("ahk_class Emacs") | IsExternalProgram()) {
       SendInput % modifiers "{Space}"
    } else {
       If (modifiers == "!"){
          If (g_MarkSet == ""){
            g_MarkSet=1
          } Else {
            g_MarkSet=
            g_LastBol=
            g_LastEol=
            g_LastBobp= 
          }
       } else {
          SendInput % modifiers "{Space}"
       }
    } 
  }
  g_SpacePressDownTime := false
  Return

ToggleCtrl:
If (ToggleCtrl == "1"){
  IniWrite,0,ergoemacs-settings.ini,BigCtl,App
} Else {
  IniWrite,1,ergoemacs-settings.ini,BigCtl,App
}
Reload
return

ToggleRAlt:
IniWrite, %A_ThisMenuItem%,ergoemacs-settings.ini,RAlt,App
Reload
return

ToggleLAlt:
IniWrite, %A_ThisMenuItem%,ergoemacs-settings.ini,LAlt,App
Reload
return

ToggleRLA:
IniWrite, %A_ThisMenuItem%,ergoemacs-settings.ini,RAltLAlt,App
Reload
return


ToggleCaps:
IniWrite, %A_ThisMenuItem%,ergoemacs-settings.ini,Caps,App
Reload
return

ThemeKeyHandler:
IniWrite,%A_ThisMenuItem%,ergoemacs-settings.ini,Curr,Theme
Reload
return

TranslateKeyHandler:
IniWrite, %A_ThisMenuItem%,ergoemacs-settings.ini,Curr,Trans
Reload
return

TranslateKey2Handler:
IniWrite, %A_ThisMenuItem%,ergoemacs-settings.ini,Curr,Trans2
Reload
return

MenuKeyHandler:
IniWrite,%A_ThisMenuItem%,ergoemacs-settings.ini,Curr,Layout
Reload
return

Exit:
ExitApp
return


previous-line:
  SendKey("{Up}",1)
  return


next-line:
  SendKey("{Down}",1)
  return


backward-char:
 SendKey("{Left}",1)
 return


forward-char:
 SendKey("{Right}",1)
 return


backward-word:
 SendKey("{Ctrl down}{Left}{Ctrl up}",1)
 return


forward-word:
  SendKey("{Ctrl down}{Right}{Ctrl up}",1)
  return

move-beginning-of-line:
  SendKey("{Home}",1)
  return

ergoemacs-end-of-line-or-what:
  if (g_LastEol <> ""){
     ;; Last Key was end of line
     ;; Send PgDown...
     SendKey("{PgDown}",1)    
  } else {
     ;; Last key was not bol send home
     SendKey("{End}",1)
  }
  g_LastEol=eol
  return

ergoemacs-beginning-of-line-or-what:
  if (g_LastBol <> ""){
     ;; Last Key was beginning of line
     ;; Send PgUp...
     SendKey("{PgUp}",1)    
  } else {
     ;; Last key was not bol send home
     SendKey("{Home}",1)
  }
  g_LastBol=bol
  return

ergoemacs-end-of-line-or-block:
move-end-of-line:
 SendKey("{End}",1)
 return

beginning-of-buffer:
 SendKey("{Ctrl down}{Home}{Ctrl up}",1)
 return

end-of-buffer:
 SendKey("{Ctrl down}{End}{Ctrl up}",1)
 return


delete-backward-char:
  SendKey("{Backspace}",0)
  return


delete-char:
 SendKey("{Delete}",0)
 return


scroll-down:
 SendKey("{PgUp}",1)
  return


scroll-up:
 SendKey("{PgDn}",1)
 return


isearch-forward:
 SendKey("{Ctrl down}{f}{Ctrl Up}",0)
 return


query-replace:
 SendKey("{Ctrl down}{h}{Ctrl Up}",0)
 return


backward-kill-word:
 SendKey("{Shift down}{Ctrl down}{Left}{Ctrl up}{Shift up}{Ctrl down}{x}{Ctrl up}",0)
 return


kill-word:
  SendKey("{Ctrl down}{Shift down}{Right}{Ctrl up}{Shift up}{Ctrl down}{x}{Ctrl up}",0)
  return


kill-line:
  SendKey("{Shift down}{End}{Shift up}{Ctrl down}{x}{Ctrl up}",0)
  return


ergoemacs-kill-line-backward:
  SendKey("{Shift down}{Home}{Shift up}{Ctrl down}{x}{Ctrl up}",0)
  return

ergoemacs-beginning-or-end-of-buffer:
  if (g_LastBobp <> ""){
     ;; Last Key was not beginning of buffer
     SendKey("{Ctrl down}{Home}{Ctrl up}",1)
     g_LastBobp=yes
  } else {
     ;; Last Key was end of buffer
     SendKey("{End}",1)
     g_LastBobp= 
  }
  return


ergoemacs-cut-line-or-region:
 lastClip = %clipboard%
 SendKey("{Ctrl down}{x}{Ctrl up}",0)
 thisClip = %clipboard%
 if (thisClip == lastClip){
    SendKey("{Home}{Shift down}{End}{Shift up}{Ctrl down}{x}{Ctrl up}",0)
    clipboard = %lastClip%%clipboard%
 }
 return


ergoemacs-copy-line-or-region:
 lastClip = %clipboard%
 SendKey("{Ctrl down}{c}{Ctrl up}",0)
 thisClip = %clipboard%
 if (thisClip == lastClip){
    SendKey("{Home}{Shift down}{End}{Shift up}{Ctrl down}{c}{Ctrl up}",0)
 }
 return

ergoemacs-paste:
yank:
 SendKey("{Ctrl down}{v}{Ctrl up}",0)
 return


undo:
  SendKey("{Ctrl down}{z}{Ctrl up}",0)
  return


redo:
 SendKey("{Ctrl down}{y}{Ctrl up}",0)
 return

execute-extended-command:
  ;; Send to org-outlook if using outlook
  If (!WinActive("ahk_class Emacs") & !IsExternalProgram()){
       If WinActive("ahk_class rctrl_renwnd32"){
          If !FileExist(OutlookSave){
             FileSelectFolder, OutlookSave, ,3, Select Folder to Save Outlook Emails
             IniWrite, %OutlookSave%, ergoemacs-settings.ini, Outlook, Save
          }
          If !FileExist(EmacsClient){
             FileSelectFile, EmacsClient, 1, , Emacs Client, Emacs Client (emacs*.exe)
             IniWrite, %EmacsClient%, ergoemacs-settings.ini, Emacs, EmacsClient
          }
          If (OutlookTemplate == "ERROR") {
                InputBox OutlookTemplate, Org-mode capture template for emails (can't be blank)
                IniWrite, %OutlookTemplate%, ergoemacs-settings.ini, Outlook, Template

          }
          Clipboard=
          SendKey("{Ctrl down}{c}{Ctrl up}")
          ClipWait
          EmailBody=%clipboard%
          EmailBody:=uri_encode(EmailBody)
          SendKey("{F12}",0)
          Clipboard=
          While !WinActive("Save As"){
                Sleep 100
          }
          SendKey("{Ctrl down}{c}{Ctrl up}")
          ClipWait 
          Counter = 1
          Title=%clipboard%
          Title := uri_encode(Title)
          fileName = %OutlookSave%\%clipboard%-%Counter%.msg
          while FileExist(fileName)
          {
             Counter := Counter + 1
             fileName = %OutlookSave%\%clipboard%-%Counter%.msg
          }
          Clipboard =
          Clipboard := fileName
          ClipWait
          While !WinActive("Save As"){
                Sleep 100
          }
          SendKey("{Backspace}")
          SendInput, %Clipboard%
          SendKey("{Enter}")
          While WinActive("Save As"){
                Sleep 100
          }
	  SendKey("{Del}")
	  ocalName = %OutlookSave%\ocal.ics
          If !FileExist(ocalName){
	    Clipboard =
            Clipboard := ocalName 
            ClipWait
	    Send, {CTRLDOWN}2{CTRLUP}
	    Send, {ALTDOWN}{ALTUP}fc
            While !WinActive("Save As"){
             Sleep 100
            }
	    Send, {CTRLDOWN}v{CTRLUP}{TAB  2}{SPACE}{DOWN  3}{TAB}{DOWN  2}{TAB}{SPACE}{DOWN}{SPACE}{TAB  2}{SPACE}{TAB  3}{ENTER}
            While WinActive("Save As"){
             Sleep 100
            }
	    Send, {CTRLDOWN}1{CTRLUP}
	  }

          fileName := uri_encode(fileName)
          fileName = "%EmacsClient%" org-protocol:/capture:/%OutlookTemplate%/%fileName%/%Title%/%EmailBody%
	  Run, %fileName%
	  
       }
  }
  return

comment-dwim:
 ;; Word Alt+Ctrl+M is insert comment
 If WinActive("ahk_class OpusApp"){
   SendKey("{Alt down}{Ctrl down}{M}{Alt up}{Ctrl up}",0)
 }
 return

ergoemacs-toggle-letter-case:
 ;; Word Shift+F3 is toggle letter case.
 ;; Maybe do somthing different in other apps.
 If WinActive("ahk_class OpusApp"){
   SendKey("{Shift down}{F3}{Shift up}",0)
 }
 return

split-window-below:
 ;; Word is Alt+Ctrl+s
 If WinActive("ahk_class OpusApp"){
   SendKey("{Alt down}{Ctrl down}{s}{Ctrl up}{Alt up}{Enter}",0)
 }
 return

delete-other-windows:
  ;; Word is
  If WinActive("ahk_class OpusApp"){
    SendKey("{Alt down}{Ctrl down}{s}{Alt up}{Ctrl up}",0)
  }
  return

ergoemacs-move-cursor-next-pane:
  ;; Word is
  If WinActive("ahk_class OpusApp"){
    SendKey("{F6}")
  }
  return

send-ctl:
  SendKey("{Ctrl down}")
  return

send-ctl-up:
  SendKey("{Ctrl up}")
  return

send-apps:
  SendKey("{AppsKey}")
  return

send-f6:
  SendKey("{F6}")
  return

IsExternalProgram(){
  External = 0
  if (WinActive("ahk_class TscShellContainerClass")){
     External = 1
  } else if (WinActive("ahk_class cygwin/x X rl")){
     External = 1
  } else if (WinActive("ahk_class cygwin/xfree86 rl")){
     External = 1
  } else if (WinActive("ahk_class Transparent Windows Client")){
     External = 1
  }
  Return External
}

GetSpaceBarHoldTime()
{
  global g_SpacePressDownTime
  time_elapsed := A_TickCount - g_SpacePressDownTime
  Return time_elapsed
}

GetModifiers2(){
  ;; Return the hotkey modifiers that are defined in the ergoemacs.ini
  Modifiers = 0
  GetKeyState, state1, LWin
  GetKeyState, state2, RWin
  state = %state1%%state2%
  if state <> UU  ; At least one Windows key is down.
    Modifiers := Modifiers + 1
  GetKeyState, state1, Alt
  if state1 = D
    Modifiers := Modifiers + 2
  GetKeyState, state1, Control
  if state1 = D
    Modifiers := Modifiers + 4
  ;;GetKeyState, state1, Alt
  GetKeyState, state1, LShift
  GetKeyState, state2, RShift
  state=%state1%%state2%
  if state <> UU
    Modifiers := Modifiers + 8
  Return Modifiers
}
  
; Return the hotkey symbols (ie !, #, ^ and +) for the modifiers that
; are currently activated
GetModifiers()
{
  Modifiers =
  GetKeyState, state1, LWin
  GetKeyState, state2, RWin
  state = %state1%%state2%
  if state <> UU  ; At least one Windows key is down.
    Modifiers = %Modifiers%# 
  GetKeyState, state1, Alt
  if state1 = D
    Modifiers = %Modifiers%!
  GetKeyState, state1, Control
  if state1 = D
    Modifiers = %Modifiers%^
  ;;GetKeyState, state1, Alt
  GetKeyState, state1, LShift
  GetKeyState, state2, RShift 
  state = %state1%%state2%
  if state <> UU ; At least one shift key is down
    Modifiers = %Modifiers%+
  Return Modifiers
}

SendKey(key,Movement = 0){
  global g_MarkSet
  global g_OtherKeyPressed
  global g_LastBol
  global g_LastEol
  g_LastEol=
  g_LastBol=
  g_OtherKeyPressed := true
  If (Movement == 0){
    g_MarkSet=
    SendInput % key
  } Else {
    If (g_MarkSet == ""){
      SendInput % key
    } Else {
      SendInput % "{Shift down}" key "{Shift up}"
    }
  }
} 


;functions starts
uri_encode(Unicode_string)
{
;converts unicode_string to uri enocoded string for autohotkey_l unicode version	
;http://www.autohotkey.com/forum/viewtopic.php?t=71619

UTF16 := Unicode_string

n := StrPutVar(UTF16, UTF8, "UTF-8")
raw_hex := MCode_Bin2Hex(&UTF8, n-1)
i := strlen(raw_hex)/2

loop, %i%
	{
	frag := "%" . substr(raw_hex, a_index*2-1,2)
	r_s .= frag
	}
return r_s

}



MCode_Bin2Hex(addr, len) {
    Static fun
    If (fun = "") {
        If Not A_IsUnicode
        h=
        ( LTrim Join
            8B54240C85D2568B7424087E3A53578B7C24148A07478AC8C0E90480F9090F97C3F6
            DB80E30702D980C330240F881E463C090F97C1F6D980E10702C880C130880E464A75
            CE5F5BC606005EC3
        )
        Else
        h=
        ( LTrim Join
            8B44240C8B4C240485C07E53568B74240C578BF88A168AC2C0E804463C090FB6C076
            066683C037EB046683C03066890180E20F83C10280FA09760C0FB6D26683C2376689
            11EB0A0FB6C26683C03066890183C1024F75BD33D25F6689115EC333C0668901C3
        )
        VarSetCapacity(fun, n := StrLen(h)//2)
        Loop % n
            NumPut("0x" . SubStr(h, 2 * A_Index - 1, 2), fun, A_Index - 1, "Char")
    }
    VarSetCapacity(hex, A_IsUnicode ? 4 * len + 2 : 2 * len + 1)
    DllCall(&fun, "uint", &hex, "uint", addr, "uint", len, "cdecl")
    VarSetCapacity(hex, -1) ;update StrLen
    Return hex
}

StrPutVar(string, ByRef var, encoding)
{
    VarSetCapacity( var, StrPut(string, encoding)
        * ((encoding="utf-16"||encoding="cp1200") ? 2 : 1) )
    return StrPut(string, &var, encoding)
}
