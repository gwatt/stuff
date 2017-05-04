#!/usr/bin/tclsh

package require Tk
package require Tclx

set file ""

option add *tearOff 0

proc savefile { filename {saveas false} } {
	set savefile [subst $$filename]
	if { [string length $savefile] == 0 || $saveas} {
		set savefile [tk_getSaveFile]
	}
	if { [string length $savefile] > 0} {
		set fp [open $savefile w]
		puts -nonewline $fp [.t get 1.0 {end - 1c}]
		close $fp
		set $filename $savefile
	}

	settitle [subst $$filename]
}

proc openfile { filename {openfile ""} } {
	if { [string length $openfile] == 0 } {
		set openfile [tk_getOpenFile]
	}
	if { [string length $openfile] > 0 } {
		set fp [open $openfile r]
		set txt [read $fp]
		.t delete 1.0 end
		.t insert 1.0 $txt
		close $fp
		set $filename $openfile
	}

	settitle [subst $$filename]
}

proc settitle { filename } {
	wm title . "Edit: $filename"
}

proc newline { } {
	set line [.t get [.t index {insert linestart}] [.t index insert]]
	set len [string length $line]
	set i 0
	while { $len > $i && [string is space [string index $line $i]] } {
		incr i
	}
	.t insert [.t index insert] "\n[string range $line 0 $i-1]"
}

wm title . "Edit"
. configure -menu [menu .m]
 set mf [menu .m.f]
 .m add cascade -menu $mf -label File -underline 0
  $mf add command -label "Open" -command { openfile ::file } -underline 0
  $mf add command -label "Save" -command { savefile ::file } -underline 0
  $mf add command -label "Save As" -command { savefile ::file true }
  $mf add separator
  $mf add command -label "Close" -command "exit" -underline 0
 .m add separator

grid columnconfigure . 0 -weight 1; grid rowconfigure . 0 -weight 1

grid [text .t -yscrollcommand {.sy set} -xscrollcommand {.sx set} -wrap none -background {#f9f9f9}] -column 0 -row 0 -sticky news
grid [scrollbar .sy -command {.t yview}] -column 1 -row 0 -sticky nes
grid [scrollbar .sx -orient horizontal -command {.t xview}] -column 0 -row 1 -sticky wes
bind .t <Return> {newline; break}
focus .t

if { $argc > 0 } { openfile ::file [lvarpop argv] }
