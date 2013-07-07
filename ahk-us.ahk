;-*- coding: utf-8 -*-
;; Ergohotkey
;; A AutopairHotkey script for system-wide ErgoEmacs keybinding
;;
;;   Copyright © 2009 Milan Santosi
;;   Copyright © 2012 Benjamin Hansen
;;   Copyright © 2013 Matthew Fidler
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

; Copyright (c) 2012 Benjamin Hansen
;
; Permission is hereby granted, free of charge, to any person
; obtaining a copy of this software and associated documentation files
; (the "Software"), to deal in the Software without restriction,
; including without limitation the rights to use,i copy, modify, merge,
; publish, distribute, sublicense, and/or sell copies of the Software,
; and to permit persons to whom the Software is furnished to do so,
; subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.
;
; Allows the spacebar key to mimic the Ctrl key while retaining most
; of its normal functionality. Holding down the spacebar key down acts
; like holding down the ctrl key. This allows for easier use of
; keyboard shortcuts (such as Ctrl+C for copy). If the spacebar key is
; pressed and released quickly (less than the specified timeout) and
; no other key was pressed then a normal space is sent.
;
; Author:         Ben Hansen <benhansenslc@gmail.com> 

#SingleInstance force
#MaxHotkeysPerInterval 9999
#NoEnv
SendMode Input
SetStoreCapslockMode, Off
Process, priority, , High
IniRead ToggleCtrl, ergoemacs-settings.ini,BigCtl, App
IniRead CurrCaps, ergoemacs-settings.ini, Caps, App
LayLst=
VarLst=
CareL = 0
CareV = 0
CareLV = 0
g_MarkSet=
modifiers=
skipUpDown=

IniRead CurrLayout, ergoemacs-settings.ini, Curr, Layout
If (CurrLayout == "ERROR"){
  CurrLayout=us
}

IniRead CurrTheme, ergoemacs-settings.ini, Curr, Theme
If (CurrTheme == "ERROR"){
  CurrTheme=Standard
} 

IniRead CurrTrans, ergoemacs-settings.ini, Curr, Trans
If (CurrTrans == "ERROR"){
  CurrTrans=No Translation
}
;; Add Translation
Loop, 120 {
  IniRead CurrKey, ergoemacs.ini, %CurrLayout%, %A_Index%
  If (CurrTrans == "No Translation"){
     objTrans%CurrKey% := CurrKey
  } else {
      If (CurrKey != ""){
       IniRead TransKey, ergoemacs.ini, %CurrTrans%, %A_Index%
       objTrans%CurrKey% := TransKey
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
        ;;HotKey, %NextSec%, %fn%
      }
    }
  }
}


HotKey,Capslock,capslock-handle
;; HotKey,(,autopair-paren


; Create Menu

Loop, parse, LayLst, `n 
{
        Menu, TranslateKey, add, No Translation, TranslateKeyHandler
        If (CurrTrans == "No Translation"){
          Menu, TranslateKey, Check,No Translation
        } else {
          Menu, TranslateKey, UnCheck,No Translation 
        }
        If (A_LoopField != ""){
           Menu, MenuKey, add, %A_LoopField%, MenuKeyHandler
           Menu, TranslateKey, add, %A_LoopField%, TranslateKeyHandler
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
Menu, tray, add, Themes, :ThemeKey
Menu, Tray, add, Caps to Menu in Emacs, ToggleCaps
If (CurrCaps == "1"){
  Menu, Tray, Check, Caps to Menu in Emacs
}
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

capslock-handle:
  If ((WinActive("ahk_class Emacs") || WinActive("ahk_class ConsoleWindowClass")) && CurrCaps == "1") {
    SendInput {AppsKey}
  } else {
    SendInput {Capslock}
  }
  return


ListenForKey:
  g_MarkSet=
  g_OtherKeyPressed := true
  Return

  
DelayKeyOutput:
  Critical
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
  transKey := Chr(objTrans%transKey%)
  if (transKey != ""){
    pressedKey := transKey
  }
  ;; get goto subroutine.
  transKey := Asc(origKey)*timesf+modifiers2
  transKey := objTrans%transKey%
  
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
  if (IsLabel(transKey) & !WinActive("ahk_class Emacs")){
    Goto %transKey%
  } Else {
    SendInput % modifiers pressedKey
    g_MarkSet=
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
    if (WinActive("ahk_class Emacs")) {
       SendInput % modifiers "{Space}"
    } else {
       If (modifiers == "!"){
          If (g_MarkSet == ""){
            g_MarkSet=1
          } Else {
            g_MarkSet=
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

ToggleCaps:
If (CurrCaps == "1"){
   IniWrite,0,ergoemacs-settings.ini,Caps,App
} Else {
   IniWrite,1,ergoemacs-settings.ini,Caps,App
}
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
 SendKey("{PgUp}",0)
 return


scroll-up:
 SendKey("{PgDn}",0)
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


ergoemacs-cut-line-or-region:
 SendKey("{Ctrl down}{x}{Ctrl up}",0)
 return


ergoemacs-copy-line-or-region:
 SendKey("{Ctrl down}{c}{Ctrl up}",0)
 return


yank:
 SendKey("{Ctrl down}{v}{Ctrl up}",0)
 return


undo:
  SendKey("{Ctrl down}{z}{Ctrl up}",0)
  return


redo:
 SendKey("{Ctrl down}{y}{Ctrl up}",0)
 return


  
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
