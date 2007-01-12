"==============================================================================
"
"     Filename:	vpars.vim
"
" GVIM Version: 7.0
"
"       Author: Bernd Pol
"        Email: bernd.pol AT online DOT de
"
"      Version: 2.1
"      Created: 2006-09-19
"Last Revision: 2007-01-06
"
"      License: Copyright (c) 2006, Bernd Pol
"               This program is free software; you can redistribute it and/or
"               modify it under the terms of the GNU General Public License as
"               published by the Free Software Foundation, version 2 of the
"               License.
"               This program is distributed in the hope that it will be
"               useful, but WITHOUT ANY WARRANTY; without even the implied
"               warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
"               PURPOSE.
"               See the GNU General Public License version 2 for more details.
"
"       Thanks: Felix Ingram for developing the snippetsEmu.vim script.
"       	from which the basic ideas for a powerful, yet basically simple
"       	variable substitution mechanism came.
"
"       	Fritz Mehner for the csupport.vim script which served as a
"       	powerful source of concepts and testbed as well to further
"       	develop those variable substitution mechanisms up to the
"       	current state.
"
"       	Igor Prischepoff for his thorough testing efforts.
"
"  Description: 							   {{{1
"  		Parameter substitution functions to be used in text fragments.
"               Repeatedly finds and replaces variables within given boundaries
"               upon a simple keypress (<F3> by default).
"               Each variable is assumed to have one of these forms:
"
"               Cursor:
"
"               <|>
"               is a cursor mark which will simply be removed and the cursor
"               be positioned there.
"               
"               Simple Variables:
"
"               <|value|>
"               where 'value' denotes both the variable label and the default
"               value.
"
"               <|value|:label:|>
"               where 'value' denotes the (possible empty) default value, and
"               'label' the explicit label of this variable.
"
"               Replacing Default Values:
"
"               Pressing <F3> with the cursor inside the 'value' field of a
"               simple variable will cause this and all equally named variables
"               be replaced by the following text.
"               - The current variable will be replaced by the text extending
"                 from right after the left '<|' delimiter up to the current
"                 cursor position.
"               - If the cursor is positioned right after the left delimiter,
"                 the variable will be replaced by the given 'default' value.
"               - Any text to the right of the cursor does not count, with one
"                 exception:
"               - If the variable bears an explicit ':label:' and was initially
"                 empty (of the form <||:label:|>), any text between the bars
"                 will be taken as replacement, independen of where the cursor
"                 was positioned.
"               - This text will replace any equally labeled variable within
"                 the currently marked text block, except delayed ones (see
"                 below).
"               
"               Alternatives:
"
"               <|alt_1|alt_2|..|alt_n|>
"               where 'alt_1'..'alt_n' denote given alternative substitution
"               values of which the one bearing the cursor will replace this
"               one variable.
"
"               <|alt_1|alt_2|..|alt_n|:label:|>
"               same, but explicitely labeled. The selected substitution will
"               replace any variable with this name.
"
"               Escaped Bars:
"
"               If ever you need to have a bar in an alternative or default
"               substitution text, escape it: '\|'. This will produce a single
"               bar, '|', in the variable replacement.
"
"               Restrictions:
"
"               The variable definitions cannot exceed a line, i.e. both the
"               '<|' and its corresponding '|>' delimiter must exist in the
"               same line.
"               (The user entered replacement strings usually may exceed the
"               line, however.)
"
"               Delayed Variables:
"
"               There is a primitive means of defining another order to visit
"               the variables and/or cursor marks. If the variable mark found
"               is surrounded by another level of '< .. >' delimiters, this
"               extra level will be removed and the variable skipped in this
"               turn.
"               This allows delaying the variable substitution until others are
"               available, or to position a cursor to a mark located near the
"               beginning of the most recently inserted text after all variable
"               substitutions were done.
"
"               Escaping Variable Delay:
"
"               If the value esulting from the substitution itself should be
"               enclosed in < ... > brackets, the delay meaning of outer
"               brackets can be escaped by following it immediately by a dot:
"
"               <.<value>.>
"
"               will be eventually replaced by '<value>'.
"
"
"               Explicit Labeled Variable Substitutions:
"
"               If there is an explicit ':label:' given, all variables bearing
"               this label will be replaced with the substitution of the first
"               occurrence found. This occurs independent of the form the other
"               variables. Especially will the replacement text of simple
"               variables replace alternative variables bearing this name and
"               vice versa.
"               This explicit substitution does not involve delayed variables,
"               however.
"
"               Text Boundaries:
"
"               Text replacement will occur upon pressing <F3>. (This key can
"               be redefined before loading this script, see 'Global Variables'
"               below.) As long as there are variables in this block, <F3>
"               will wrap around from the its end to the marked beginning line.
"               This mechanism allows the handling of delayed variables.
"               - The text boundaries are marked linewise.
"               - Usually, the text bundaries will be marked by some calling
"                 function (see interface description below).
"               - The user can expressely select a visual block, which will
"                 then repeatedly be parsed for variables upon pressing <F3>.
"               - When there is no text block marked or there are no more
"                 variables found, the script switches to 'global mode', where
"                 the next variable from cursor to the end of the buffer will
"                 be found, if any. This global mode is only linear, no
"                 wrapping around will happen.
"
"=== TODO Stuff ==========================================================={{{1
"
" TODO: Allow VPars variables in substitution texts.
"
" TODO: Do not use hard coded '<|..|..|:..:|>' delimiters.
" 	(Might never be done as it probably will considerably complicate the
" 	script.)
"
" TODO: Map/Unmap shortcuts like the menu updates.
"
"=== Known Bugs ==========================================================={{{1
"
" FIXME: When multiple lines were inserted and the cursor left the position
" 	 prematurely (i.e. before the next <F3> keypress) to another line
" 	 inside or outside the block, the b:EndInsert block end boundary will
" 	 be computed (sometimes extremely) false at the next <F3>.
"
" FIXME: A variable starting at the left boundary of the first block line is
" 	 not recognized.
" 
" FIXME: When a visual block was selected fromout insert mode before pressing
" 	 <F3>, the cursor is left inside the '<|' leading delimiter of the
" 	 first variable.
" 	 (A mere nuisance only, the user can go on using the default value for
" 	 replacement by simply pressing <F3> as usual.)
"
"==============================================================================

"==============================================================================
"                                   Interface				   {{{1
"==============================================================================
"
"------------------------------------------------------------------------------
" Global Variables:							   {{{2
"------------------------------------------------------------------------------
"
" g:vpars_vim
" 	Prevents duplicate loading.
" 	Holds the version number if existent: major version
" 	number times 100 plus minor version number.
" 	Note:
" 	- Can be used to block the loading of VPars. In this case it should
" 	  be set to zero in order to signal this circumstance.
"
"------------------------------------------------------------------------------
"
" g:VPars_AltBegin
" 	Alternate shortcut to trigger the VPars_JumpBegin() function.
" 	Defaults to 'jb'.
"
" g:VPars_AltEnd
" 	Alternate shortcut to trigger the VPars_JumpEnd() function.
" 	Defaults to 'je'.
"
" g:VPars_AltInv
" 	Shortcut to trigger the VPars_InvalidateBlock() function.
" 	Defaults to 'ji'.
"
" g:VPars_AltNext
" 	Alternate shortcut to trigger the VPars_RepJump() function.
" 	Defaults to 'jj'.
"
" g:VPars_AltPause
" 	Alternate shortcut to trigger the VPars_Pause() function.
" 	Defaults to 'jp'.
"
" g:VPars_AltResume
" 	Alternate shortcut to trigger the VPars_Resume() function.
" 	Defaults to 'jr'.
"
" g:VPars_AltShow
" 	Shortcut to trigger the VPars_ShowBlock() function.
" 	Defaults to 'jo'
"
" g:VPars_HookPrefix
" 	Holds the prefix the externally provided hook functions will use.
" 	Defaults to 'VPars'.
"
" g:VPars_NextKey
" 	Key to trigger the VPars_RepJump() function. Defaults to <F3>.
"
" g:VPars_SkipBlockend
" 	If set to 1 causes <F3> to automatically advance beyond the text block
" 	end if no more variables are in there.
" 	Defaults to 0, which causes the cursor to stop there. To advance in
" 	this case, the cursor must explicitely be positioned outside the most
" 	recently inserted block.
"
" g:VPars_UseHooks
" 	Determines how hook functions are to be considered. 
" 	String composed of 'parse', 'pre' and/or 'post' in any order.
" 	If it contains 'parse', VPars_PreParseHook() will be called whenever
" 	a variable is about to be parsed.
" 	If it contains 'pre', VPars_PreVarHook() will be called whenever a
" 	variable is about to be substituted.
" 	If it contains 'post', VPars_PostVarHook will be called after any
" 	variable substitution.
" 	A missing g:VParse_UseVarHooks or any other except these values will
" 	cause no hook functions be called at all.
" 	May be set at any time before a VPars_NextVar(), VPars_RepJump(), or
" 	VPars_RepJumpBlock() call.
" 
" g:VPars_Verbose
" 	If nonzero, issue warning messages in some cases of error.
"
"------------------------------------------------------------------------------
" Functions:								   {{{2
"------------------------------------------------------------------------------
"
" ----- Initialization Functions ------------------------------------------{{{3
"
" VPars_Init()
" 	(Re-)initialize some VPars environment and interface settings such as
" 	the trigger key(default <F3>), etc.
"
" VPars_InvalidateBlock()
"	Resets block local variables as well as the b:StartInsert, b:EndInsert
"	boundaries and b:AtEndInsert, thus invalidating the whole dedicated
"	text block.
"
" VPars_ResetVar()
" 	Initialize some buffer local variables.
"
" ----- Menu Functions ----------------------------------------------------{{{3
"
" VPars_DisableMenu( menuLeader )
" 	Disables all VPars menu entries.
"
" VPars_EnableMenu( menuLeader )
" 	Enables all VPars menu entries.
"
" VPars_Menu()
" 	Toggles an own VPars menu display in the GUI.
" 	Note:
" 	- This menu can be forced on start-up by setting g:VPars_ShowMenu = 1.
" 	- The name of the menu can be preset in g:VPars_MenuName.
" 	- The menu position (priority) can be predefined in g:VPars_MenuPos.
"
" VPars_RemoveMenu( menuLeader )
" 	Removes the whole VPars menu.
"
" VPars_SetupMenu( menuLeader, menuLevel )
" 	Makes some VPars functions via a menu under the caller submitted
" 	<menuLeader> at the <menuLevel> position.
" 
" ----- Processing Functions ----------------------------------------------{{{3
"
" VPars_NextVar( glob )
" 	Find next variable, delimited by <|..|>
" 	If <glob> is zero, works between b:StartInsert and b:EndInsert only
" 	otherwise finds the next variable available in the current buffer.
" 	Returns:
"	 1 if another variable or a cursor mark was found
"	 0 if no more variables were found
"	-1 if a delayed variable had been skipped
"	-2 if an invalid variable was detected
"
" VPars_Pause()
"	Disables the processing of the current dedicated text block by setting
"	the b:AtEndInsert end flag to -1, thus indicating a wait state.
"	Note:
"	- No action if there is no valid text block.
"	- No action if the b:AtEndInsert block end condition has already been
"   	  set to 1.
"
" VPars_RepJump( insMode )
"	Replace current variable and jump to next one.
"	<insMode> usually should be 0. It is meant to be used from a menu
"	or shortcut call and will cause the character under the cursor be
"	deleted. This is only a crude hack to get around a column 1 positioning
"	problem in Vim insert mode.
"
" VPars_RepJumpBlock()
" 	Replace current variable and jump to next one in the currently selected
" 	visual block.
" 	Note:
" 	- Sets b:StartInsert and b:EndInsert to the visual block boundaries and
" 	  resets b:AtEndInsert.
"
" VPars_RepList( list )
" 	Accepts an array of label-replacement string pairs of the form:
" 	    [['label_1' 'replacement_1'] ... ['label_n' 'replacement_n']]
" 	Scans the text between b:StartInsert and b:EndInsert for every given
" 	variable with implicit or explicit 'label_i' and replaces it with the
" 	corresponding 'replacement_i'.
" 	Note:
" 	- Explicit labels must be surrounded with ':...:' delimiters.
"
" VPars_Resume()
"	If there is a valid text block, tries to resume processing at the
"	location where the last variable had been replaced.
"	Note:
"	- No action if there is no valid text block.
" 	- If no valid variable processing could be detected (using the
" 	  b:VStLine and b:VarStart variables), processing will start at the
" 	  beginning of the block.
"
" VPars_Run( firstLine, lastLine )
" 	Main VPars entry point. Prepares a dedicated text block starting at
" 	<firstLine> and extending up to and including <lastLine>.
" 	Both <firstLine> or <lastLine> can be zero. In this case the existing
" 	b:StartInsert or b:EndInsert settings will be used as boundaries.
" 	This way an existing block can be extended and started as a new one.
" 	Enters the replacement loop at the beginning of <firstLine>.
" 	Note:
" 	- This function will always reset the buffer local variables, thus
" 	  causing VPars to start processing on a completely fresh block.
" 	  If this is not desired, use VPars_RepJump().
"
" ----- Block Jump Functions ----------------------------------------------{{{3
"  
" VPars_JumpBegin()
" 	Puts the cursor at the beginning of the dedicated text block and resets
" 	AtEndInsert.
"
" VPars_JumpEnd()
" 	Puts the cursor at the end of the dedicated text block and flags
" 	AtEndInsert.
"
" VPars_ShowBlock()
" 	Visually selects the lines between b:StartInsert and b:EndInsert.
" 
"------------------------------------------------------------------------------
" External Functions:							   {{{2
"------------------------------------------------------------------------------
"
" Pre And Post Processing Variable Hook Functions:
" There may be functions (e.g. in templates processing) which will allow some
" processing of a variable before or after it is handled by VParse.
" These must be externally defined and will be called from here if they exist.
"
" Their use may be further controlled by the global g:VPars_UseHooks variable
" (see above).
"
" Note: <prefix> is the hook prefix provided in g:VPars_HookPrefix or 'VPars'
"       by default.
"
" <prefix>_PreParseHook( bufnr, line, varstart, varend )
" 	Pre Parse Variable Hook: Will be called immediately before a variable
" 	is about to be parsed. The variable definition found in this position
" 	me be altered in any way, even made invalid, deleted, or delayed, as
" 	VPars still has no idea of it yet.
" 	The variable to be parsed is located in buffer number <bufnr>, at line
" 	number <line>, there extending between columns <varstart> and <varend>
" 	(these are the outer <| ... |> margins).
" 	Returns:
" 	Zero to skip this parsing, else non-zero.
" 	Note:
" 	- Any change to the text buffer must be reflected in the variable
" 	  properties (see below).
"
" <prefix>_PreVarHook( bufnr, line, varstart, varend, label, subst, alt )
"	Preprocess Variable Hook: Will be called before an actual variable
"	substitution is about to be done.
" 	The variable to be substituted is located in buffer number <bufnr>, at
" 	line number <line>, there extending between columns <varstart> and
" 	<varend> (these are the outer <| ... |> margins).
"	The <label> argument will hold the variable label, and <subst> the
"	string to be put in its place, where <alt> will be non-zero if it was
"	a selection from the alternatives list.
" 	Returns:
" 	Zero to skip this substitution, else non-zero.
"
" <prefix>_PostVarHook( bufnr, line, col, label, subst, alt )
" 	Postprocess Variable Hook: Will be called after every substitution,
" 	but before the substituted text has been processed (e.g. actually been
" 	split into multiple lines).
" 	The variable substituted recently is located in buffer number <bufnr>,
" 	at line number <line>.  The substituted text starting at column <col>.
" 	The <label> argument holds the label of the variable just substituted,
" 	and <subst> the actual substitution string put in its place, with
" 	<alt> being non-zero if it was a selection from an alternatives list.
" 	Note:
" 	- If the number of lines to be substituted ('<NL>' marks in <subst>)
" 	  changes due to postprocessing, this must be reflected in the
" 	  b:VEndLine buffer local variable prior to return.
"
"------------------------------------------------------------------------------
" Buffer Local Variables:						   {{{2
"------------------------------------------------------------------------------
"
" --- Current Variable Properties -----------------------------------------{{{3
"
" b:AltCol	array of column boundaries, relative to the start of the value
"               field, empty if there are no alternatives given
" b:AltSubst	array of alternative substitution values,
"               b:VarSubst is empty in this case
" b:ExpLabel	1 if there is an explicit label, otherwise 0
" b:SubstEmpty	1 if the initial substitution value in an explicitely
"		labeled variable was empty, otherwise 0;
" b:VarEnd	the end column of this variable (after '|>')
" 		(these column numbers are string indexes, starting at 0)
" b:VarLabel	label of this variable
" b:VarStart	the start column of this variable (before '<|')
" b:VarSubst	substitution value of this variable if no alternatives given
" b:VEndLine	the ending line number of this variable (differs only after
" 		multiline replacements)
" b:VPars_curMenu Holds the currently used menu heading.
" b:VPars_Msg	message describing most recently encountered error, if any
" b:VPars_Pos	temporarily left cursor position (used by VPars_ShowBlock() )
" b:VStLine	the starting line number of this variable
" b:VSubstLines	number of lines actually substituted by this replacement
"
" --- Boundaries of the text block to work in ------------------------------{{{3
"
" These have to be externally defined, except when calling VPars_RepJumpBlock()
" (see above), but may be altered elsewhere in this script.
"
" b:AtEndInsert	 1 if the end of the text block has been reached
" 		-1 if the block was left for some unknown cause
" 		 0 if still processing
" b:EndInsert	end line of the most recently inserted text block
" 		0 if no valid block
" b:StartInsert	start line of the most recently inserted text block
" 		0 if no valid block
"
"==============================================================================
"				Startup Sequence			   {{{1
"==============================================================================
"
if exists("g:vpars_vim") || &cp
    finish
endif
"
" Check VIM version number.
"
if v:version < 700
    :echoerr "Sorry! The vpars.vim script needs VIM version 7.0 or higher."
    finish
endif
"
" We keep the version number here for external reference.
" This is major_version * 100 + minor_version:
"
let g:vpars_vim = 201
"
" Note:
" The user can define g:vpars_vim (e.g. in his .vimrc startup script) to
" prohibit loading the script. In this case it should be given the value
" 	g:vpars_vim = 0
" to signal that VPars is currently unavailable.

" Load user definitions along the runtimepath:
"
runtime vpars.rc

" Define a local shortcut leader (defaults to ",").
let s:LL = ","
if exists("g:VPars_LocalLeader")
    let s:LL = g:VPars_LocalLeader
endif
:let maplocalleader = s:LL
"
" Make sure a backslash will show in the menus.
"
if s:LL == "\\"
    let s:LL = "\\\\"
endif

"==============================================================================
"                               Initialization				   {{{2
"==============================================================================

" Save current Vim options and use default ones.
" (These will be reset at the end of the script.)
"
let savecpo = &cpoptions
set cpoptions&vim

"------------------------------------------------------------------------------
" Main Initialization:							   {{{3
"------------------------------------------------------------------------------

function VPars_Init()

    " --- Global variables ------------------------------------------------{{{4
    
    " Show a VPars menu in the GUI
    
    let s:MenuName = "&Variables"
    if exists("g:VPars_MenuName")
	let s:MenuName = g:VPars_MenuName
    endif

    let s:MenuPos = 90
    if exists("g:VPars_MenuPos")
	let s:MenuPos = g:VPars_MenuPos
    endif

    let s:ShowMenu = 0
    
    " --- Key to call the 'Next Variable' function ------------------------{{{4
    "
    " Define the key to trigger to replace and find the next variable actions.

    let s:OldNextKey = ""
    if exists( "g:VPars_NextKey" )
	" Redefine only if necessary.
	if ! exists( "s:NextKey" )
	    let s:NextKey = g:VPars_NextKey
	elseif s:NextKey != g:VPars_NextKey
	    let s:OldNextKey = s:NextKey
	    let s:NextKey = g:VPars_NextKey
	endif
    else
	let s:NextKey = "<F3>"
    endif

    " --- Mappings --------------------------------------------------------{{{4
    "
    " Map the 'Next Variable' key behaviour.
    " But first remove an old mapping if necessary.

    if s:OldNextKey != ""
	exe "nunmap ".s:OldNextKey
	exe "vunmap ".s:OldNextKey
	exe "iunmap ".s:OldNextKey
	let s:OldNextKey = ""
    endif

    :exe "nnoremap <silent> ".s:NextKey."       :call VPars_RepJump(0)<Esc>a"
    " FIXME: In visual mode, we currently need an extra trailing escape in order
    " to switch insert mode off in case it was on when selecting the block.
    " Otherwise the 'a' would get inserted inside the first leading variable
    " delimiter in this case.
    :exe "vnoremap <silent> ".s:NextKey."  <Esc>:call VPars_RepJumpBlock()<Esc><Esc><Esc>a"
    "
    " Insert mode is more elaborate because we must get the cursor column right
    " (especially in utf-8 multibyte environments).
    " The inserted auxiliary character (here: 'x') will immediately be removed
    " again by the VPars_RepJump() function (running in normal mode, then).
    "
    " FIXME: This does not work in replace mode!
    "
    :exe "inoremap <silent> ".s:NextKey." x<Esc>:call VPars_RepJump(1)<Esc>a"

    " --- Alternate Shortcut Mappings -------------------------------------{{{4

    let s:AltNext = "jj"
    if exists( "g:VPars_AltNext" )
	let s:AltNext = g:VPars_AltNext
    endif

    let s:AltBegin = "jb"
    if exists( "g:VPars_AltBegin" )
	let s:AltBegin = g:VPars_AltBegin
    endif

    let s:AltEnd = "je"
    if exists( "g:VPars_AltEnd" )
	let s:AltEnd = g:VPars_AltEnd
    endif

    let s:AltPause = "jp"
    if exists( "g:VPars_AltPause" )
	let s:AltPause = g:VPars_AltPause
    endif

    let s:AltResume = "jr"
    if exists( "g:VPars_AltResume" )
	let s:AltResume = g:VPars_AltResume
    endif

    let s:AltInv = "ji"
    if exists( "g:VPars_AltInv" )
	let s:AltInv = g:VPars_AltInv
    endif

    let s:AltShow = "jo"
    if exists( "g:VPars_AltShow" )
	let s:AltShow = g:VPars_AltShow
    endif

    if s:AltNext != ""
	:exe "nnoremap <silent> <LocalLeader>".s:AltNext."       :call VPars_RepJump(0)<Esc>a"
	:exe "vnoremap <silent> <LocalLeader>".s:AltNext."  <Esc>:call VPars_RepJumpBlock()<Esc><Esc><Esc>a"
	:exe "inoremap <silent> <LocalLeader>".s:AltNext." x<Esc>:call VPars_RepJump(1)<Esc>a"
	let s:OldAltNext = s:AltNext
    else
	if exists( "s:OldAltNext" ) && s:OldAltNext != ""
	    exe "nunmap <LocalLeader>".s:OldAltNext
	    exe "vunmap <LocalLeader>".s:OldAltNext
	    exe "iunmap <LocalLeader>".s:OldAltNext
	endif
    endif

    if s:AltBegin != ""
	:exe "nnoremap <silent> <LocalLeader>".s:AltBegin."      :call VPars_JumpBegin()<Esc>"
	:exe "vnoremap <silent> <LocalLeader>".s:AltBegin."      :call VPars_JumpBegin()<Esc><Esc>i"
	:exe "inoremap <silent> <LocalLeader>".s:AltBegin." <Esc>:call VPars_JumpBegin()<Esc>i"
	let s:OldAltBegin = s:AltBegin
    else
	if exists( "s:OldAltBegin" ) && s:OldAltBegin != ""
	    exe "nunmap <LocalLeader>".s:OldAltBegin
	    exe "vunmap <LocalLeader>".s:OldAltBegin
	    exe "iunmap <LocalLeader>".s:OldAltBegin
	endif
    endif

    if s:AltEnd != ""
	:exe "nnoremap <silent> <LocalLeader>".s:AltEnd."      :call VPars_JumpEnd()<Esc>"
	:exe "vnoremap <silent> <LocalLeader>".s:AltEnd."      :call VPars_JumpEnd()<Esc><Esc>A"
	:exe "inoremap <silent> <LocalLeader>".s:AltEnd." <Esc>:call VPars_JumpEnd()<Esc>A"
	let s:OldAltEnd = s:AltEnd
    else
	if exists( "s:OldAltEnd" ) && s:OldAltEnd != ""
	    exe "nunmap <LocalLeader>".s:OldAltEnd
	    exe "vunmap <LocalLeader>".s:OldAltEnd
	    exe "iunmap <LocalLeader>".s:OldAltEnd
	endif
    endif

    if s:AltPause != ""
	:exe "nnoremap <silent> <LocalLeader>".s:AltPause."      :call VPars_Pause()<Esc>"
	:exe "inoremap <silent> <LocalLeader>".s:AltPause." <Esc>:call VPars_Pause()<Esc>"
	let s:OldAltPause = s:AltPause
    else
	if exists( "s:OldAltPause" ) && s:OldAltPause != ""
	    exe "nunmap <LocalLeader>".s:OldAltPause
	    exe "iunmap <LocalLeader>".s:OldAltPause
	endif
    endif

    if s:AltResume != ""
	:exe "nnoremap <silent> <LocalLeader>".s:AltResume."      :call VPars_Resume()<Esc>a"
	:exe "inoremap <silent> <LocalLeader>".s:AltResume." <Esc>:call VPars_Resume()<Esc>a"
	let s:OldAltResume = s:AltResume
    else
	if exists( "s:OldAltResume" ) && s:OldAltResume != ""
	    exe "nunmap <LocalLeader>".s:OldAltResume
	    exe "iunmap <LocalLeader>".s:OldAltResume
	endif
    endif

    if s:AltInv != ""
	:exe "nnoremap <silent> <LocalLeader>".s:AltInv."      :call VPars_InvalidateBlock()<Esc>"
	:exe "vnoremap <silent> <LocalLeader>".s:AltInv."      :call VPars_InvalidateBlock()<Esc><Esc>"
	:exe "inoremap <silent> <LocalLeader>".s:AltInv." <Esc>:call VPars_InvalidateBlock()<Esc>"
	let s:OldAltInv = s:AltInv
    else
	if exists( "s:OldAltInv" ) && s:OldAltInv != ""
	    exe "nunmap <LocalLeader>".s:OldAltInv
	    exe "vunmap <LocalLeader>".s:OldAltInv
	    exe "iunmap <LocalLeader>".s:OldAltInv
	endif
    endif

    if s:AltShow != ""
	:exe "nnoremap <silent> <LocalLeader>".s:AltShow."      :call VPars_ShowBlock()<Esc>"
	:exe "inoremap <silent> <LocalLeader>".s:AltShow." <Esc>:call VPars_ShowBlock()<Esc>"
	let s:OldAltShow = s:AltShow
    else
	if exists( "s:OldAltShow" ) && s:OldAltShow != ""
	    exe "nunmap <LocalLeader>".s:OldAltShow
	    exe "iunmap <LocalLeader>".s:OldAltShow
	endif
    endif

endfunction

"==============================================================================
"				   Menu Setup				   {{{1
"==============================================================================
"
" These are meant to be called by external scripts to make the VPars navigation
" functions available on another menu.
"
" Note:
" - There is no check whether the menu really exists. The calling function is
"   responsible to make this sure.
"
"------------------------------------------------------------------------------
" Set Up A VPars Menu:							   {{{2
"
" <menuLeader> is a string denoting the menu selection the VPars menu shall be
" put under.
" <menuLevel> is a string denoting the position (menu priority).
"
" Note:
" - The <menuLeader> is kept in the b:VPars_curMenu buffer local flag.
" - There can be at most one VPars menu be displayed per buffer. If another
"   menus already exists, it will be removed prior to setting up the new one.
"------------------------------------------------------------------------------
"
function VPars_SetupMenu( menuLeader, menuLevel )
    " If there is a global menu already remove it before displaying an
    " apllication owned one.
    if a:menuLeader != s:MenuName
	if s:ShowMenu
	    let s:ShowMenu = 0
	    call VPars_RemoveMenu( s:MenuName )
	endif
    endif

    " Remove another existing menu.
    if exists("b:VPars_curMenu") && b:VPars_curMenu != a:menuLeader
	if b:VPars_curMenu != ""
	    call VPars_RemoveMenu( b:VPars_curMenu )
	endif
    endif
    let b:VPars_curMenu = a:menuLeader

    " Now create the new menu.
    "
    exe "nmenu <silent> ".a:menuLevel.".10 ".a:menuLeader.".&next\\ variable<Tab>".s:NextKey."\\ ".s:LL.s:AltNext." :call VPars_RepJump(0)<Esc>a"
    exe "imenu <silent> ".a:menuLevel.".10 ".a:menuLeader.".&next\\ variable<Tab>".s:NextKey."\\ ".s:LL.s:AltNext." x<Esc>:call VPars_RepJump(1)<Esc>a"
    exe "vmenu <silent> ".a:menuLevel.".10 ".a:menuLeader.".&next\\ variable<Tab>".s:NextKey."\\ ".s:LL.s:AltNext." <Esc>:call VPars_RepJumpBlock()<Esc><Esc><Esc>a"
    
    exe "amenu <silent> ".a:menuLevel.".15 ".a:menuLeader.".&pause\\ processing<Tab>".s:LL.s:AltPause." :call VPars_Pause()<CR>"
    "
    exe "nmenu <silent> ".a:menuLevel.".20 ".a:menuLeader.".&resume\\ processing<Tab>".s:LL.s:AltResume." :call VPars_Resume()<Esc>a"
    exe "imenu <silent> ".a:menuLevel.".20 ".a:menuLeader.".&resume\\ processing<Tab>".s:LL.s:AltResume." <Esc>:call VPars_Resume()<Esc>a"
    
    exe "amenu <silent> ".a:menuLevel.".25 ".a:menuLeader.".-VparsJump- :"
    
    exe "nmenu <silent> ".a:menuLevel.".30 ".a:menuLeader.".go\\ block\\ &begin<Tab>".s:LL.s:AltBegin." :call VPars_JumpBegin()<Esc>"
    exe "imenu <silent> ".a:menuLevel.".30 ".a:menuLeader.".go\\ block\\ &begin<Tab>".s:LL.s:AltBegin." <Esc>:call VPars_JumpBegin()<Esc>i"
    exe "vmenu <silent> ".a:menuLevel.".30 ".a:menuLeader.".go\\ block\\ &begin<Tab>".s:LL.s:AltBegin." :call VPars_JumpBegin()<Esc><Esc>i"
    "
    exe "nmenu <silent> ".a:menuLevel.".35 ".a:menuLeader.".go\\ block\\ &end<Tab>".s:LL.s:AltEnd." :call VPars_JumpEnd()<Esc>"
    exe "imenu <silent> ".a:menuLevel.".35 ".a:menuLeader.".go\\ block\\ &end<Tab>".s:LL.s:AltEnd." <Esc>:call VPars_JumpEnd()<Esc>A"
    exe "vmenu <silent> ".a:menuLevel.".35 ".a:menuLeader.".go\\ block\\ &end<Tab>".s:LL.s:AltEnd." :call VPars_JumpEnd()<Esc><Esc>A"
    
    exe "amenu <silent> ".a:menuLevel.".40 ".a:menuLeader.".-VparsBlock- :"
    
    exe "nmenu <silent> ".a:menuLevel.".45 ".a:menuLeader.".sh&ow\\ block<Tab>".s:LL.s:AltShow." :call VPars_ShowBlock()<CR>"
    exe "imenu <silent> ".a:menuLevel.".45 ".a:menuLeader.".sh&ow\\ block<Tab>".s:LL.s:AltShow." <Esc>:call VPars_ShowBlock()<CR>"
    "
    exe "amenu <silent> ".a:menuLevel.".50 ".a:menuLeader.".&invalidate\\ block<Tab>".s:LL.s:AltInv." :call VPars_InvalidateBlock()<Esc>"

    " Finally adjust the menu to the current buffer situation.
    "
    call VPars_UpdateMenu()
endfunction

"------------------------------------------------------------------------------
" Update VPars Menu:							   {{{2
"------------------------------------------------------------------------------
"
function VPars_UpdateMenu()
    " Do only if there is a valid menu.
    if ! exists("b:VPars_curMenu")
	return
    endif
    if b:VPars_curMenu == ""
	return
    endif

    " Handle the Next Key conditions
    "
    let showEntry = 1
    if exists("b:AtEndInsert") && b:AtEndInsert == -1
	let showEntry = 0
    endif
    if showEntry
	exe "nmenu enable ".b:VPars_curMenu.".&next\\ variable"
	exe "imenu enable ".b:VPars_curMenu.".&next\\ variable"
	exe "vmenu enable ".b:VPars_curMenu.".&next\\ variable"
    else
	exe "nmenu disable ".b:VPars_curMenu.".&next\\ variable"
	exe "imenu disable ".b:VPars_curMenu.".&next\\ variable"
	exe "vmenu disable ".b:VPars_curMenu.".&next\\ variable"
    endif

    " Handle the Pause conditions.
    "
    let showEntry = 0
    if exists("b:StartInsert") && exists("b:EndInsert")
	if exists("b:AtEndInsert") && b:AtEndInsert == 0
	    let showEntry = 1
	endif
    endif
    if showEntry
	exe "amenu enable ".b:VPars_curMenu.".&pause\\ processing"
    else
	exe "amenu disable ".b:VPars_curMenu.".&pause\\ processing"
    endif

    " Handle the Resume conditions.
    "
    let showEntry = 0
    if exists("b:StartInsert") && exists("b:EndInsert")
	if exists("b:AtEndInsert") 
	    if b:AtEndInsert == -1
		let showEntry = 1
	    elseif exists("b:VStLine") && b:VStLine != 0
		" There are variables about to be processed.
		" Resume either if at end or immediately at block start.
		if b:AtEndInsert == 1
		    let showEntry = 1
		elseif b:AtEndInsert == 0 && 
			    \ line(".") == b:StartInsert &&
			    \ col(".") == 1
		    let showEntry = 1
		endif
	    endif
	endif
    endif
    if showEntry
	exe "nmenu enable ".b:VPars_curMenu.".&resume\\ processing"
	exe "imenu enable ".b:VPars_curMenu.".&resume\\ processing"
    else
	exe "nmenu disable ".b:VPars_curMenu.".&resume\\ processing"
	exe "imenu disable ".b:VPars_curMenu.".&resume\\ processing"
    endif

    " Handle the block navigation conditions
    "
    let showEntry = 0
    if exists("b:StartInsert") && exists("b:EndInsert")
	if b:StartInsert != 0 && b:EndInsert != 0
	    let showEntry = 1
	endif
    endif
    if showEntry
	exe "nmenu enable ".b:VPars_curMenu.".go\\ block\\ &begin"
	exe "imenu enable ".b:VPars_curMenu.".go\\ block\\ &begin"
	exe "vmenu enable ".b:VPars_curMenu.".go\\ block\\ &begin"
	"
	exe "nmenu enable ".b:VPars_curMenu.".go\\ block\\ &end"
	exe "imenu enable ".b:VPars_curMenu.".go\\ block\\ &end"
	exe "vmenu enable ".b:VPars_curMenu.".go\\ block\\ &end"
	"
	exe "amenu enable ".b:VPars_curMenu.".&invalidate\\ block"
    else
	exe "nmenu disable ".b:VPars_curMenu.".go\\ block\\ &begin"
	exe "imenu disable ".b:VPars_curMenu.".go\\ block\\ &begin"
	exe "vmenu disable ".b:VPars_curMenu.".go\\ block\\ &begin"
	"
	exe "nmenu disable ".b:VPars_curMenu.".go\\ block\\ &end"
	exe "imenu disable ".b:VPars_curMenu.".go\\ block\\ &end"
	exe "vmenu disable ".b:VPars_curMenu.".go\\ block\\ &end"
	"
	exe "amenu disable ".b:VPars_curMenu.".&invalidate\\ block"
    endif

    " Finally handle the Show block conditions.
    " 
    exe "nmenu enable ".b:VPars_curMenu.".sh&ow\\ block"
    exe "imenu enable ".b:VPars_curMenu.".sh&ow\\ block"
endfunction

"------------------------------------------------------------------------------
" Disable VPars Menu:							   {{{2
"
" <menuLeader> is a string denoting the menu selection the VPars menu has been
" put under.
"------------------------------------------------------------------------------
"
function VPars_DisableMenu( menuLeader )
    if b:VPars_curMenu == a:menuLeader
	exe "nmenu disable ".a:menuLeader.".&next\\ variable"
	exe "imenu disable ".a:menuLeader.".&next\\ variable"
	exe "vmenu disable ".a:menuLeader.".&next\\ variable"
	"
	exe "amenu disable ".a:menuLeader.".&pause\\ processing"
	exe "nmenu disable ".a:menuLeader.".&resume\\ processing"
	exe "imenu disable ".a:menuLeader.".&resume\\ processing"
	"
	exe "nmenu disable ".a:menuLeader.".go\\ block\\ &begin"
	exe "imenu disable ".a:menuLeader.".go\\ block\\ &begin"
	exe "vmenu disable ".a:menuLeader.".go\\ block\\ &begin"
	"
	exe "nmenu disable ".a:menuLeader.".go\\ block\\ &end"
	exe "imenu disable ".a:menuLeader.".go\\ block\\ &end"
	exe "vmenu disable ".a:menuLeader.".go\\ block\\ &end"
	"
	exe "nmenu disable ".a:menuLeader.".sh&ow\\ block"
	exe "imenu disable ".a:menuLeader.".sh&ow\\ block"
	"
	exe "amenu disable ".a:menuLeader.".&invalidate\\ block"
    endif
endfunction

"------------------------------------------------------------------------------
" Enable VPars Menu:							   {{{2
"
" <menuLeader> is a string denoting the menu selection the VPars menu has been
" put under.
"------------------------------------------------------------------------------
"
function VPars_EnableMenu( menuLeader )
    if b:VPars_curMenu == a:menuLeader
	call VPars_UpdateMenu()
    endif
endfunction

"------------------------------------------------------------------------------
" Remove VPars Menu:							   {{{2
"
" <menuLeader> is a string denoting the menu selection the VPars menu has been
" put under.
"
" Note:
" - Resets the b:VPars_curMenu flag.
"------------------------------------------------------------------------------
"
function VPars_RemoveMenu( menuLeader )
    exe "aunmenu ".a:menuLeader
    let b:VPars_curMenu = ""
endfunction

"------------------------------------------------------------------------------
" Show VPars Menu:							   {{{2
"
" Toggles an own VPars menu display in the GUI.
"
" Note:
" - If there is another application-owned menu, the global VPars menu will not
"   be displayed. I.e. application VPars menus always take precedence over the
"   global one.
" - This menu can be forced on start-up by setting g:VPars_ShowMenu = 1.
" - The name of the menu can be preset in g:VPars_MenuName.
" - The menu position (priority) can be predefined in g:VPars_MenuPos.
"
"------------------------------------------------------------------------------
"
function VPars_Menu()
    if exists("b:VPars_curMenu") && b:VPars_curMenu != ""
	if b:VPars_curMenu != s:MenuName
	    return
	endif
    endif
    
    if s:ShowMenu
	" There is a menu shown. Switch it off.
	let s:ShowMenu = 0
	call VPars_RemoveMenu( s:MenuName )
    else
	" No menu yet. Show it.
	let s:ShowMenu = 1
	call VPars_SetupMenu( s:MenuName, s:MenuPos )
    endif
endfunction

"==============================================================================
"                                 Functions				   {{{1
"==============================================================================

"===== Auxiliary Functions ================================================{{{2

"------------------------------------------------------------------------------
" Initialize Buffer Variables:						   {{{3
"------------------------------------------------------------------------------
"
function VPars_ResetVar()
    let b:VStLine	= 0
    let b:VEndLine	= 0
    let b:VarStart	= -1
    let b:VarEnd	= -1

    let b:VarLabel	= ""
    let b:ExpLabel	= 0

    let b:VarSubst	= ""
    let b:SubstEmpty	= 0
    let b:AltSubst	= []
    let b:AltCol  	= []
    
    let b:VSubstLines	= 0

    let b:VPars_Msg	= ""
    let b:VPars_Pos	= []
endfunction

"------------------------------------------------------------------------------
" Display Warning Message:						   {{{3
"
" Issues <msg> as warning message if g:VPars_Verbose was set to non-zero.
" If <msg> is empty, issues the b:VPars_Msg script local message.
"------------------------------------------------------------------------------
"
function s:Warn( msg )
    if exists("g:VPars_Verbose") && g:VPars_Verbose
	redraw
	echohl WarningMsg
	if a:msg == ""
	    echomsg "line ".b:VStLine.", col ".b:VarStart.": ".b:VPars_Msg
	else
	    echomsg a:msg
	endif
	echohl None
    endif
endfunction

"===== Variables Handling ================================================={{{2

"------------------------------------------------------------------------------
" Find Next Variable:							   {{{3
" 
" Searches the next variable from the cursor position
" If <glob> is zero the search is limited by the last inserted text block,
" delimited by the b:StartInsert and b:EndInsert lines.
" Else finds the next following variable, if any, up to the end of the buffer.
"
" RETURN:
"   1 if another variable or a cursor mark was found
"   0 if no more variables were found
"  -1 if a delayed variable had been skipped
"  -2 if an invalid variable was detected
"
" Note:
" - The cursor will be positioned immediately after the left '<|' delimiter.
" - If the next variable is a '<|>' cursor position mark, remove it and leave
"   the cursor there.
" - If there is a '<|' not belonging to a valid variable, the cursor will be
"   put after this '<|' mark.
" - If there is no next variable at all, the cursor will be put at:
"   = the end of the last line of this text block if glob is zero,
"   = else the cursor will not move.
"------------------------------------------------------------------------------
"
function s:GoNextVar( glob )
    let curline = line(".")
    let curcol = col(".")
    let found = 0

    let globalsearch = a:glob
    if exists("b:EndInsert")
	let endrange = b:EndInsert
    else
	let endrange = line("$")
	let globalsearch = 1
    endif
    if a:glob
	let endrange = line("$")
    endif

    " Is there another variable on this line?
    let varcol = stridx( getline("."), '<|', curcol )
    if varcol != -1
	let varline = curline
	let found = 1
    else
	" Not on this line. try to find another one in this range.
	let varline = search( '<|', '', endrange )
	if varline != 0
	    let found = 1
	    let varcol = stridx( getline(varline), '<|' )
	endif
    endif

    if found
	let vartype = s:ParseVariable( varline, varcol )
	"
	if vartype == 0		" not a valid variable
	    call cursor( varline, varcol + 3 )
	    return -2
	endif
	"
	let thisline = getline(".")
	if vartype == 2		" delayed variable
	    " Remove one level of brackets.
	    let head = strpart( thisline, 0, b:VarStart - 1 )
	    let mid  = strpart( thisline, b:VarStart, b:VarEnd - b:VarStart )
	    let tail = strpart( thisline, b:VarEnd + 1 )
	    call setline( varline, head.mid.tail )
	    call cursor( b:VStLine, b:VarEnd )
	    return -1
	endif
	"
	if vartype == 1		" cursor mark
	    let head = strpart( thisline, 0, b:VarStart )
	    let tail = strpart( thisline, b:VarStart + 3 )
	    call setline( varline, head.tail )
	    call cursor( b:VStLine, b:VarStart )
	    return 1
	endif
	"
	call cursor( b:VStLine, b:VarStart + 2 )
	return 1
    endif

    " No variable found.
    if globalsearch
	call cursor( curline, curcol )
    else
	call cursor( b:EndInsert, 1 )
	:normal $
    endif
    return 0
endfunction

"------------------------------------------------------------------------------
" Repeatedly Find Next Variable:					   {{{3
"
" Works exactly like s:GoNextVar, but jumps around to b:StartInsert in order
" to find another variable mark which may have been skipped in the previous run.
"
" Note:
" - If the end of the dedicated text block ultimately has been reached, its
"   final column will be kept in b:VarStart with b:VarEnd set to 0.
"------------------------------------------------------------------------------
"
function VPars_NextVar( glob )
    " Nothing to do if at end already.
    if exists("b:AtEndInsert") && b:AtEndInsert
	return 0
    endif

    while 1
	let result = s:GoNextVar( a:glob )
	if result == -2
	    " A seemingly invalid variable has been found.
	    " Warn the user, if wanted, then try again.
	    call s:Warn("")
	    return -2
	endif

	if result == 1
	    " Another variable has been found.
	    return 1
	endif

	if result == 0
	    " At the end of the text block
	    if a:glob
		" Global search: Do nothing
		return 0
	    else
		" Wrap around and try one more time.
		call cursor( b:StartInsert, 1 )
		let result = s:GoNextVar( a:glob )
		" Any variables left unvisited?
		if result == 0
		    " There are none. Now everything is done.
		    let b:AtEndInsert = 1
		    " Forget everything about recently parsed variables.
		    call VPars_ResetVar()
		    " But remember this end position.
		    call cursor( b:EndInsert, 1)
		    :normal $
		    let b:VarStart = col(".")
		    let b:VarEnd = 0
		    return 0
		endif
		if result == 1
		    " We found another valid one. Have it processed.
		    return 1
		endif
		if result == -2
		    call s:Warn("")
		    return -2
		endif
		" When here, we found an invalid or another delayed variable,
		" so go over all this again.
	    endif
	endif
    endwhile
endfunction

"------------------------------------------------------------------------------
" Parse Variable:							   {{{3
"
" Parses the variable on line <linenr>, starting at column <varstart> (which
" must be immediately before a '<|' mark).
"
" RETURN:
"  0 if this is no valid variable and then
"    b:VStLine		the line number where this variable delimiter starts
"    b:VarStart		the start column of the variable delimiter (before '<|' )
"  1 if cursor found and then
"    b:VStLine		the line number of the cursor mark
"    b:VarStart		the cursor column
"    b:VarEnd = b:VarStart
"    b:VarLabel = ''
"  2 if a delayed variable was found and then
"    b:VStLine		the line number where this variable starts
"    b:VarStart		the start column of the variable (before '<|' )
"    b:VarEnd		the end column of the variable (after '|>')
" -1 otherwise, and then
"    b:VStLine		the line number where this variable starts
"    b:VEndLine		the line number where this variable ends
"    			(differs only after later multiline replacements)
"    b:VarStart		the start column of the variable (before '<|' )
"    b:VarEnd		the end column of the variable (after '|>')
"    b:VarLabel		the label of this variable
"    b:ExpLabel = 1 	if this label is explicit, otherwise 0.
"    b:VarSubst = ''	if there is an alternative substition array,
"			otherwise the substitution value
"    b:SubstEmpty = 1	if the initial substitution value in an explicitely
"			labeled variable was empty, otherwise 0;
"			undefined if the variable has no explicit label
"    b:AltSubst		array of alternative substitution values
"    b:AltCol		array of the starting columns of these values,
"			relative to varstart
"
" Note:
" - In case of error there will be a message text in b:VPars_Msg.
"------------------------------------------------------------------------------
"
function s:ParseVariable( linenr, varstart )
    call VPars_ResetVar()
    let b:VStLine = a:linenr
    let b:VarStart = a:varstart
    let thisline = getline( a:linenr )

    " Is it an otherwise valid variable?
    let varend = stridx( thisline, '|>', a:varstart )
    " The end delimiter must be within this line.
    if varend == -1
	let b:VPars_Msg = "No variable end delimiter found in this line."
	return 0
    endif
    " Do the delimiters belong together?
    if strridx( thisline, '<|', varend ) > a:varstart
	let b:VPars_Msg = "Start and end delimiters do not match."
	return 0
    endif
    let b:VarEnd = varend + 2
    let b:VEndLine = b:VStLine

    " Is this variable delayed?
    if strpart( thisline, b:VarStart - 1, 3 ) == '<<|' &&
\      strpart( thisline, b:VarEnd   - 2, 3 ) == '|>>'
	return 2
    endif

    " Is it a cursor mark?
    if strpart( thisline, b:VarStart, 3 ) == "<|>"
	let b:VarEnd = b:VarStart
	let b:VarLabel = ""
	return 1
    endif

    " Need to first call a function?
    if exists("g:VPars_UseVarHooks") && g:VPars_UseVarHooks =~ "parse"
	let hookPrefix = "VPars"
	if exists("g:VPars_HookPrefix") && g:VPars_HookPrefix != ""
	    let hookPrefix = g:VPars_HookPrefix
	endif
	if exists("*".hookPrefix."_PreParseHook")
	    " Let the user prepare this variable and skip parsing if wanted.
	    exe "let hookResult = ".hookPrefix."_PreParseHook( bufnr('%'), b:VStLine, b:VarStart, b:VarEnd )"
	    if hookResult == 0
		" Skip this variable.
		return 0
	    endif
	endif
    endif

    " Is this variable empty?
    if strpart( thisline, b:VarStart, 4 ) == "<||>"
	let b:VarEnd = b:VarStart + 4
	return -1
    endif
    if strpart( thisline, a:varstart, 7 ) == "<||::|>"
	let b:VarEnd = b:VarStart + 7
	let b:ExpLabel = 1
	let b:VarLabel = "::"
	let b:SubstEmpty = 1
	return -1
    endif

    " We found a seemingly valid variable to parse.
    let thisvar = strpart( thisline, b:VarStart + 2, b:VarEnd - b:VarStart - 4 )
    let varend = len(thisvar)

    " Is there an explicit label?
    let pos = match( thisvar, '|:' )
    if pos != -1
	if strpart( thisvar, varend - 1, 1 ) != ":"
	    let b:VPars_Msg = "No end delimiting colon for explicit label found."
	    return 0
	endif
	" We allow, however, empty explicit labels, which will then consist of
	" b:VarLabel = '::' only.
	let b:VarLabel = strpart( thisvar, pos + 1 )
	let b:ExpLabel = 1
	let varend = pos
	if strpart( thisline, a:varstart, 4 ) == "<||:"
	    let b:SubstEmpty = 1
	endif
    endif

    " Is there a list of alternative substitutions?
    let altvar = strpart( thisvar, 0, varend )
    let pos = 0
    let escBars = 0
    while pos < varend
	let nextpos = match( altvar, '|', pos )
	" Take care of escaped bars.
	while nextpos != -1
	    if strpart( altvar, nextpos - 1, 2 ) == '\|'
		let nextpos = match( altvar, '|', nextpos + 1 )
		let escBars = 1
	    else
		break
	    endif
	endwhile
	"
	if nextpos != -1
	    let subst = strpart( altvar, pos, nextpos - pos )
	    if escBars
		let subst = substitute( subst, '\\|', '|', 'g' )
		let escBars = 0
	    endif
	    let b:AltSubst = add( b:AltSubst, subst )
	    let b:AltCol = add( b:AltCol, pos )
	    let pos = nextpos + 1
	    " Handle a possible final empty alternative.
	    if pos == varend
		let b:AltSubst = add( b:AltSubst, '' )
		let b:AltCol = add( b:AltCol, pos )
	    endif
	else
	    " Handle the last alternative, too, if there are already some.
	    if pos < varend && len( b:AltSubst ) != 0
		let subst = strpart( altvar, pos )
		let b:AltSubst = add( b:AltSubst, subst )
		let b:AltCol = add( b:AltCol, pos )
	    endif
	    let pos = varend
	endif
    endwhile
    if len(b:AltCol) > 0
	" Record a final boundary, too, so cursor detection gets easier.
	let b:AltCol = add( b:AltCol, varend + 2 )
    endif

    " Handle simple substitution values.
    if len(b:AltCol) == 0
	let b:VarSubst = strpart( thisvar, 0, varend )
	if escBars
	    let b:VarSubst = substitute( b:VarSubst, '\\|', '|', 'g' )
	endif
	if ! b:ExpLabel
	    let b:VarLabel = b:VarSubst
	endif
    endif

    return -1
endfunction

"------------------------------------------------------------------------------
" Replace Variable:							   {{{3
"
" Assumes the cursor somewhere between '<| .. |>' of the most recently parsed
" variable.
"
" Note:
" The cursor must be BEHIND the last inserted character (as in insert mode)
" in oder to properly recognize the replacement string.
"
" If <firsttime> is nonzero, the replacement field will be parsed depending on
" the variable mode: normal substitution or alternatives selection.
"
" - In normal substitution mode (b:AltCol, b:AltSubst being empty):
"   = If the cursor was positioned immediately behind the '<|' left variable
"     delimiter, b:VarSubst will get the value of b:VarLabel.
"   = If there are any characters between the left delimiter and the cursor (as
"     user did type them in) they will become the new content of b:VarSubst.
" - In alternatives selection mode:
"   = Find the alternative the cursor is positioned in and use this text as
"     replacement in b:VarSubst.
"   = If there is no explicit label given, replace this variable only.
"   = If the cursor is outside any of the alternatives, use an empty replacement
"     text.
"
" If <firsttime> is zero, b:VarLabel and b:VarSubst will remain unchanged.
" No parsing will be done.
" The whole variable, delimiters included, will in any case be replaced by the
" value of b:VarLabel.
" The cursor will be left immediately behind the substituted text.
"-------------------------------------------------------------------------------
"
function s:ReplVar( firsttime )
    let thisline = getline(".")
    let curline = line(".")
    let curcol = col(".")
    let realcol = curcol - 1
    let varline = curline
    let varcol = strridx( thisline, '<|', curcol )

    if a:firsttime
	let varline = b:VStLine
	let varcol = b:VarStart
	"
	if len(b:AltCol) == 0
	    if b:ExpLabel && b:SubstEmpty
		" Use all of the replacement text in this special case.
		if b:VStLine == b:VEndLine
		    let realcol = stridx( getline( b:VEndLine ), "|:", b:VarStart )
		else
		    let realcol = stridx( getline( b:VEndLine ), "|:" )
		endif
	    endif
	    " Find the substitution string.
	    if b:VStLine == b:VEndLine
		if realcol > b:VarStart + 2
		    let b:VarSubst = strpart( thisline, b:VarStart + 2, realcol - b:VarStart - 2 )
		else
		    " Check for an empty replacement.
		    if strpart( thisline, realcol, 2 ) == "|>"
			let b:VarSubst = ""
		    else
			if ! b:ExpLabel
			    let b:VarSubst = b:VarLabel
			endif
		    endif
		endif
	    else
		" First collect a multiline replacement string.
		let b:VarSubst = strpart( getline( b:VStLine ), b:VarStart + 2 )
		if curline != b:VStLine
		    let i = b:VStLine + 1
		    while i < curline
			let b:VarSubst = b:VarSubst."<NL>".getline(i)
			let i = i + 1
		    endwhile
		    let b:VarSubst = b:VarSubst."<NL>".strpart( getline(i), 0, realcol )
		endif
		" Remove the excess lines for now.
		call cursor( b:VStLine, 1 )
		:exe ":".b:VStLine.",".b:VEndLine."join"
	    endif
	else
	    " Find the alternative to be used.
	    let b:VarSubst = ""
	    let realcol = realcol - ( b:VarStart + 2 )
	    let i = len(b:AltCol) - 1
	    if realcol <= b:AltCol[i]
		while i > 0
		    let i = i - 1
		    if b:AltCol[i] <= realcol && realcol < b:AltCol[i+1]
			let b:VarSubst = b:AltSubst[i]
			break
		    endif
		endwhile
	    endif
	endif
    endif

    " Replace this variable, if not delayed.
    let hookPrefix = "VPars"
    if exists("g:VPars_HookPrefix") && g:VPars_HookPrefix != ""
	let hookPrefix = g:VPars_HookPrefix
    endif
    "
    let thisline = getline( varline )
    let endcol = stridx( thisline, '|>', varcol )
    if strpart( thisline, varcol - 1, 3) == '<<|' && strpart( thisline, endcol, 3 ) == '|>>'
	call cursor( 0, endcol + 3 )
    else
	" Preprocess the variable if wanted.
	if exists("g:VPars_UseVarHooks") && g:VPars_UseVarHooks =~ "pre"
	    if exists("*".hookPrefix."_PreVarHook")
		exe "let hookResult = ".hookPrefix."_PreVarHook( bufnr('%'), varline, varcol, endcol, b:VarLabel, b:VarSubst, len( b:AltCol ) )"
		if hookResult == 0
		    " Skip this occurrence.
		    call cursor( b:VStLine, b:VarEnd )
		    return
		endif
	    endif
	endif
	"
	" Adjust for a delay escape dot left or right?
	if strpart( thisline, varcol - 2, 4 ) == "<.<|" || strpart( thisline, varcol - 3, 5 ) == "<..<|"
	    let varcol -= 1
	endif
	if strpart( thisline, endcol, 4 ) == "|>.>" || strpart( thisline, endcol, 5 ) == "|>..>"
	    let endcol += 1
	endif
	"
	" Finally we may replace the variable.
	let head = strpart( thisline, 0, varcol )
	let tail = strpart( thisline, endcol + 2 )
	call setline( ".", head.b:VarSubst.tail )
	"
	" Postprocess the substitution if wanted.
	if exists("g:VPars_UseVarHooks") && g:VPars_UseVarHooks =~ "post"
	    if exists("*".hookPrefix."_PostVarHook")
		exe "call ".hookPrefix."_PostVarHook( bufnr('%'), varline, varcol, b:VarLabel, b:VarSubst, len( b:AltCol ) )"
		call cursor( b:VStLine, b:VarEnd )
		return
	    endif
	endif
	if b:VStLine == b:VEndLine
	    call cursor( 0, varcol + len( b:VarSubst ) + 1 )
	else
	    :silent! substitute/<NL>/\r/g
	    call cursor( b:VEndLine, curcol )
	    if ! a:firsttime
		" Adjust for extra inserted lines.
		let b:EndInsert = b:EndInsert + b:VSubstLines
	    endif
	endif
    endif
endfunction

"===== Text Block Replacement Actions ====================================={{{2

"------------------------------------------------------------------------------
" Substitute Variables In The Most Recently Inserted Text Block:	   {{{3
"
" Finds all variables labeled b:VarLabel in the most recently inserted text
" block in this buffer and replaces them by the b:VarSubst text.
"
" Note:
" - The cursor remains unchanged.
"------------------------------------------------------------------------------
"
function s:SubstVar()
    let curline = line(".")
    let curcol = col(".")

    let thisvar = "|".b:VarLabel."|>"
    if ! b:ExpLabel
	let thisvar = "<".thisvar
    endif
    let curln = b:StartInsert

    while curln <= b:EndInsert
	let curpos = 0
	while curpos != -1
	    let thisline = getline( curln )
	    let curpos = stridx( thisline, thisvar, curpos )
	    if curpos != -1
		if b:ExpLabel
		    " So far we only found the explicit label part. We need to
		    " check the formal correctness of this variable, too.
		    let varpos = strridx( thisline, '<|', curpos )
		    if varpos == -1
			" No variable at all here, try again.
			let curpos = curpos + 1
		    else
			if stridx( thisline, '|>', varpos ) != stridx( thisline, '|>', curpos )
			    " No correct one, try again.
			    let curpos = curpos + 1
			else
			    " This variable bears the proper explicit label.
			    call cursor( curln, varpos + 3 )
			    call s:ReplVar(0)
			    let curpos = col(".") - 1
			endif
		    endif
		else
		    " Not an explicitely labeled variable, replace it anyway.
		    call cursor( curln, curpos + 3 )
		    call s:ReplVar(0)
		    let curpos = col(".") - 1
		endif
	    else
		let curln = curln + 1
	    endif
	endwhile
    endwhile

    call cursor( curline, curcol )
endfunction

"------------------------------------------------------------------------------
" Replace Variables From A List:					   {{{3
"
" Accepts a <list> of labal-replacement string pairs of the form:
"     [['label_1' 'replacement_1'] ... ['label_n' 'replacement_n']]
" Scans the text between b:StartInsert and b:EndInsert for every given
" variable with implicit or explicit 'label_i' and replaces it with the
" corresponding 'replacement_i'.
"
" Note:
" - Explicit labels in the <list> must be surrounded with ':...:' delimiters.
" - No action if the block delimiter marks b:StartInsert and b:EndInsert do not
"   exist or were not properly initialized yet.
" - The cursor will be put at the block start if there was a marked text block.
"   Otherwise its position will not change.
"------------------------------------------------------------------------------
function VPars_RepList( list )
    " Empty list: nothing to do.
    if len( a:list ) == 0
	return
    endif
    " Do nothing if no text block has been marked.
    if ! exists("b:StartInsert") || ! exists("b:EndInsert")
	return
    endif
    if b:StartInsert == 0 || b:EndInsert == 0
	return
    endif
    if b:StartInsert > b:EndInsert
	return
    endif

    " Make sure variable descriptors exist at all.
    if ! exists("b:VarStart")
	call VPars_ResetVar()
    endif

    for thisentry in a:list
	" We allow empty list entries.
	if len( thisentry ) == 0
	    continue
	endif
	" Get label and, possibly empty, replacement string.
	let b:VarLabel = thisentry[0]
	if len( thisentry ) > 1
	    let b:VarSubst = thisentry[1]
	else
	    " Treat missing replacement string as empty.
	    let b:VarSubst = ""
	endif
	" Deduce the label type.
	let b:ExpLabel = 0
	if strpart( b:VarLabel, 0, 1) == ":" && strpart( b:VarLabel, strlen(b:VarLabel) - 1 ) == ":"
	    let b:ExpLabel = 1
	endif
	" Now replace this one everywhere in the text block.
	call s:SubstVar()
    endfor

    call cursor( b:StartInsert, 1 )
endfunction

"------------------------------------------------------------------------------
" Adjust Line Numbers After Long Replacements:				   {{{3
"
" Tries to figure out whether the user typed in a multiline replacement string
" and adjusts the b:VEndLine and b:EndInsert line nubers accordingly.
"
" Note:
" - Also checks whether the cursor left the current text block prematurely.
"------------------------------------------------------------------------------
"
function s:AdjustLines()
    " Is there anything to adjust at all?
    if ! exists("b:VarStart")
	return
    endif

    if b:VarStart == -1
	" There has no variable been parsed yet.
	return
    endif

    if len(b:AltCol) != 0
	" There is an alternative substitution list: Do nothing.
	return
    endif

    let curline = line(".")
    if curline < b:VStLine
	" Cursor outside variable start.
	return
    endif

    let curcol = col(".")
    if curline == b:VStLine
	if curcol < b:VarStart + 2
	    " Cursor outside variable start.
	    return
	endif
	if b:VarEnd != b:VarStart
	    " If there was no cursor mark substitution recently, we still
	    " have to check if the variable end delimiter had been shifted
	    " down by the replacement.
	    if stridx( getline("."), '|>', curcol ) != -1
		" Assume cursor inside variable. No lines inserted.
		return
	    endif
	endif
    endif

    " The user will probably have entered a multiline replacement.
    " Try to deduce the amount of new lines from what we see here.
    if b:VarEnd == b:VarStart
	" Do only if there is a valid text block:
	if exists("b:EndInsert") && b:EndInsert != 0
	    let b:EndInsert = b:EndInsert + (curline - b:VStLine )
	endif
    else
	" Try to find the variable end somewhere down.
	call cursor( b:VStLine, b:VarStart )
	let varendline = search( '|>', 'W' )
	call cursor( curline, curcol )
	if varendline == 0
	    " There is no end delimiter at all.
	    return
	endif
	if varendline < curline
	    " This is probably not a user replacement.
	    return
	else
	    " This is the best we can do.
	    " There is still a slight source of error: We may have found an
	    " end delimiter beyond the most recently inserted text.
	    let b:VEndLine = varendline
	    let b:VSubstLines = curline - b:VStLine
	    if exists("b:EndInsert") && b:EndInsert != 0
		let b:EndInsert = b:EndInsert + b:VSubstLines
	    endif
	endif
    endif
endfunction

"===== Checking Stuff ====================================================={{{2

"------------------------------------------------------------------------------
" Check Whether The Block Prematurely Has Been Left:			   {{{3
"
" Resets the buffer local variables if it appears there is has a new text block
" been dedicated for variables processing.
"
" Note:
" - Assumes valid existence of the buffer local variables.
"------------------------------------------------------------------------------
"
function s:CheckBlockLeave()
    if exists("b:VStLine") && b:VStLine != 0
	if b:VStLine < b:StartInsert || b:VStLine > b:EndInsert
	    " If so reset the buffer local variable processing
	    " states,
	    call VPars_ResetVar()
	    " and reset the end condition (as this most likely
	    " is what the user wanted)
	    let b:AtEndInsert = 0
	endif
    endif
    return
endfunction

"------------------------------------------------------------------------------
" Handle A Possible Premature Block Leave:				   {{{3
"
" If the cursor left the old block for another one before the end condition had
" been reached, prepare to process this new block.
" If the cursor was put outside without setting up another block, only inhibit
" processing of this block so the user can resume later.
"
" Note:
" - Uses a -1 value for b:AtEndInsert if the user left the block while keeping
"   it intact.
"------------------------------------------------------------------------------
"
function s:HandleBlockLeave()
    if exists("b:EndInsert")
	if exists("b:AtEndInsert")
	    if b:AtEndInsert == 1
		" Text block end had been reached. Check where we currently are.
		" (We check the line only, let the user decidedly go off.)
		if line(".") == b:EndInsert
		    " Still within the end line. But past the end column?
		    " (Even in virtualedit=all mode:)
		    :normal $
		    if b:VarEnd == 0 && ( col(".") > b:VarStart )
			" It is: the user moved the cursor away.
			call VPars_InvalidateBlock()
		    endif
		elseif line(".") >= b:StartInsert && line(".") <= b:EndInsert
		    " Inside, but did we leave prematurely to another text
		    " block?
		    call s:CheckBlockLeave()
		else
		    " The user moved the cursor away.
		    call VPars_InvalidateBlock()
		endif
	    else
		" Not at end, but we need still check whether we prematurely
		" left to another text block.
		if line(".") >= b:StartInsert && line(".") <= b:EndInsert
		    call s:CheckBlockLeave()
		else
		    " The user moved the cursor away for another cause,
		    " just seize processing this block for now.
		    let b:AtEndInsert = -1
		endif
	    endif
	endif
    endif
endfunction

"------------------------------------------------------------------------------
" Check Whether Cursor Is In A Variable:				   {{{3
"
" RETURN:
"  0 if the cursor is not positioned inside a '<| .. |>' variable
"  1 if the cursor is inside the substitution field of the most recently parsed
"    variable.
"  2 if the cursor is elsewhere in this variable.
" -1 if the cursor is in a yet unparsed variable.
"------------------------------------------------------------------------------
"
function s:CursorInVar()
    let curline  = line(".")
    let curcol   = col(".")
    let realcol = curcol - 1
    let thisline = getline(".")

    " Is the cursor in the most recently parsed variable at all?
    if ! ( exists("b:VarStart") && exists("b:VarEnd") )
	" There has nothing been parsed yet.
	return 0
    else
	if b:VarStart == -1
	    " No variable parsed yet.
	    return 0
	endif

	if b:VStLine == b:VEndLine
	    "------------------------------------------------------------------
	    " No extra lines were inserted
	    "
	    if b:VarStart == b:VarEnd
		" It was a cursor. Do nothing.
		return 0
	    endif

	    " Handle some common cursor misplacements.
	    "
	    if realcol == b:VarStart
		" Immediately to the left of the delimiting bar: reparse
		:normal 2l
		return -1
	    endif

	    if curline != b:VStLine
		" The user left the line of the recently parsed variable.
		let stDelim = strridx( thisline, '<|', realcol )
		if stDelim != -1
		    if stridx( thisline, '|>', stDelim ) > realcol
			" Inside a new variable, parse this one.
			return -1
		    endif
		endif
		" Not inside a new variable, just reparse from here.
		return 0
	    endif

	    if realcol == b:VarStart + 1
		" Account for normal mode shift-out to the left.
		:normal l
		let curcol += 1
		let realcol += 1
	    endif

	    " Handle the case where the user wants to substitute a single colon.
	    if realcol == b:VarStart + 3
		if strpart( thisline, b:VarStart, 3 ) == "<|:"
		    return 1
		endif
	    endif

	    " -----------------------------------------------------------------
	    " Now for the real work:
	    "
	    if b:VarStart + 2 <= realcol
		if b:ExpLabel
		    let endsubst = stridx( thisline, '|:', b:VarStart )
		else
		    let endsubst = stridx( thisline, '|>', b:VarStart )
		endif

		if (b:VarStart + 2) <= realcol && realcol <= endsubst
		    " Somewhere inside a variable, check for an alternatives list.
		    if len(b:AltCol) == 0
			return 1
		    else
			" Is the alternatives list still intact?
			let newAlt = join( b:AltSubst, '|' )
			let oldAlt = strpart( thisline, b:VarStart + 2, endsubst - b:VarStart - 2 )
			let oldAlt = substitute( oldAlt, '\\|', '|', 'g' )
			if oldAlt == newAlt
			    return 1
			else
			    return 2
			endif
		    endif
		else
		    if b:ExpLabel && 
		    \ realcol > endsubst && 
		    \ realcol <= stridx( thisline, '|>', b:VarStart )
			" Reparse if in explicit label.
			return -1
		    endif
		    " Not in a variable.
		    return 0
		endif
	    endif
	else
	    "------------------------------------------------------------------
	    " There were new lines inserted with the replacement string,
	    " hence we must explicitely check the boundaries.
	    if b:VStLine <= curline && curline <= b:VEndLine
		if curline == b:VStLine && realcol < b:VarStart
		    return 0
		endif
		if curline == b:VEndLine
		    if b:ExpLabel
			let endDelim = '|:'
		    else
			let endDelim = '|>'
		    endif
		    if realcol <= stridx( thisline, endDelim, realcol )
			" Cursor in replacement.
			return 1
		    elseif realcol <= stridx( thisline, '|>', realcol )
			" Cursor elsewhere in variable.
			return 2
		    else
			" Cursor not in this variable.
			return 0
		    endif
		else
		    " Cursor in one of the new lines of this replacement.
		    return 1
		endif
	    else
		" Cursor not in any variable line.
		return 0
	    endif
	endif
    endif

    " The cursor is in between some '<| .. |>' pair, but do they
    " belong to the same variable?
    " FIXME: This rather simple check inhibits the use of variables in
    "        substitution texts.
    if b:VStLine == b:VEndLine
	if stridx( thisline, "|>", b:VarStart ) != b:VarEnd
	    return 0
	endif
	" We still need to check whether the cursor sits inmidst a delimiter.
	let colchars = strpart( thisline, realcol, 2 )
	if colchars == "<|" || colchars == "|>"
	    return 0
	endif
    endif

    return -1
endfunction

"------------------------------------------------------------------------------
" Check Whether The Cursor Is In The Most Recently Inserted Block:	   {{{3
"
" RETURN:
"  0 if the cursor is not positioned inside the  block, otherwise 1.
" 
" Note:
" - The end of the most recently inserted text block counts as not inside if it
"   definitely has been reached.
"------------------------------------------------------------------------------
"
function s:CursorInBlock()
    " Is there such a text block at all?
    if ! ( exists("b:StartInsert") && exists("b:EndInsert") )
	return 0
    endif

    " Has the end been reached?
    if exists("b:AtEndInsert") && b:AtEndInsert
	if exists("g:VPars_SkipBlockend") && g:VPars_SkipBlockend
	    return 0
	endif
	if line(".") == b:EndInsert
	    " Cope with virtualedit=all:
	    :normal $
	    if b:VarEnd == 0 && ( col(".") > b:VarStart )
		" Cursor is past the final column: block has been left.
		return 0
	    endif
	endif
    endif

    if b:StartInsert <= line(".") && line(".") <= b:EndInsert
	" Still inside the text block.
	return 1
    endif

    return 0
endfunction

"===== Outside Control ===================================================={{{2

"------------------------------------------------------------------------------
" Replace And Jump To Next Variable:					   {{{3
"
" If the cursor is inside a variable replaces this accordingly and every equally
" labeled variable inside this most recently inserted text block. Then tries to
" position the cursor to immediately left of the label of the next found variable
" or at the end of the most recently inserted text block.
"
" If <insmode> is non-zero, the function was called from an insert mode mapping.
" In this case it is assumend tht a dummy character has been input at the cursor
" location which will now be deleted by the function. This is a crude workaround
" to get column 1 positioning right.
"
" Note:
" - If used on a new dedicated text block, the b:StartInsert and b:EndInsert
"   boundaries must have been properly set up. Also, the b:AtEndInsert end 
"   condition flag should be reset to zero.
" - Although not necessarily required, unwnated side effects can be minimized
"   when a previously processed text block was explicitely invalidated by a
"   call to VPars_InvalidateBlock() before starting a new text block.
"   Such invalidating in implicitely done by a call VPars_Run() which
"   routinely should be used instead of VPars_RepJump().
" - If the cursor is inside the most recently inserted text block, but not in
"   a variable, only reposition it accordingly.
" - If the cursor is not inside such a text block, but inside a variable, only
"   replace this one and try to find another variable or cursor mark following
"   this one. If none found, leave the cursor immediately after the
"   substituted text.
" - If the end of the most recently inserted text block has been reached, the
"   variable properties and the block boundaries will be reset.
"   All following search variable operations will then be done globally.
"------------------------------------------------------------------------------
"
function VPars_RepJump( insmode )
    " Make sure the cursor is positioned correctly.
    if a:insmode
	normal x
    endif

    if ! exists("b:VPars_Pos")
	let b:VPars_Pos = []
    endif

    " -------------------------------------------------------------------------
    " If the cursor had been temporarily moved, reposition.
    if b:VPars_Pos != []
	call setpos( ".", b:VPars_Pos )
	let b:VPars_Pos = []

	" If the cursor is inside a variable, force to reparse this one.
	let checkLine = getline(".")
	let checkVar = stridx( checkLine, "|>", col(".") - 1 )

	if checkVar != -1
	    let checkVar = strridx( checkLine, "<|", checkVar )
	    if checkVar != -1
		" Inside <|...|>, reposition before the <|.
		if col(".") > checkVar
		    " We are past the <| left delimiter.
		    if checkVar > 0
			" This variable starts somewhere beyond the left boundary.
			call cursor( 0, checkVar )
		    elseif line(".") > 1
			" Variable starts at the left boundary. Make sure the <|
			" will be found: Go to the end of the previous line if
			" possible.
			:normal k$
		    else
			" If at top of the buffer, we have no choice but col 0.
			:normal 0
		    endif
		    " Finally, force the reparse from this position.
		    call VPars_ResetVar()
		else
		    " We are before the next variable found in this line.
		    " Probably there was a cursor mark. Just remain here.
		    return
		endif
	    endif
	else
	    " No variable on this line. Leave cursor where it is.
	    return
	endif
    endif

     " Make sure there are valid block local variables.
    if ! exists("b:VarStart")
	call VPars_ResetVar()
    endif

    " -------------------------------------------------------------------------
    " Switch modes if there is no valid dedicated text block.
    let insideBlock = 1
    if ! exists("b:StartInsert") || ! exists("b:EndInsert")
	" Nothing useful there yet, just provide a dummy variables set.
	call VPars_InvalidateBlock()
    endif

    if b:StartInsert == 0 || b:EndInsert == 0
	let insideBlock = 0
    endif

    if insideBlock
	if b:AtEndInsert == -1
	    " Processing paused, nothing else to check.
	    let insideBlock = 0
	else 
	    " Find out where the cursor really is.
	    if b:AtEndInsert == 0
		" We must however first account for multiline inserts in order
		" to handle the position properly.
		call s:AdjustLines()
	    endif
	    let insideBlock = s:CursorInBlock()
	endif
    endif

    " -------------------------------------------------------------------------
    " Did we leave prematurely?
    if insideBlock
	call s:HandleBlockLeave()
    else
	" We left the current block for some cause.
	if b:StartInsert != 0 && b:EndInsert != 0
	    " If the block waits for later processing, leave it alone.
	    " Otherwise check the circumstances.
	    if b:AtEndInsert == 0
		" The user moved away for some unknown cause,
		" just inhibit processing for now.
		let b:AtEndInsert = -1
	    elseif b:AtEndInsert == 1
		" There is nothing left to be processed.
		call VPars_InvalidateBlock()
	    endif
	endif
    endif

    " -------------------------------------------------------------------------
    " Now to the real work:
    if insideBlock
	" Do nothing if at end.
	if exists( "b:AtEndInsert" ) && b:AtEndInsert
	    " But explicitely reposition the cursor at the last line end.
	    " (This suppresses unwanted cursor shifts in virtualedit mode.)
	    call cursor( b:EndInsert, 1)
	    :normal $
	    return
	endif

	" Check whether we are in a variable.
	let inVar = s:CursorInVar()
	if inVar == 1		" in the substitution field
	    call s:ReplVar(1)
	    call s:SubstVar()

	elseif inVar == 2	" elsewhere in the parsed variable
	    " Let the user handle this variable again.
	    call cursor( b:VStLine, b:VarStart - 1 )

	elseif inVar == -1	" in a yet unparsed variable
	    " Go parse this one first.
	    call cursor( 0, strridx( getline("."), '<|', col(".") ) - 1 )
	endif
	call VPars_NextVar(0)

    else " Outside the most recently inserted block
	"
	" Process this only if there is no text block waiting.
	" (Otherwise buffer local status will not apply properly.)
	" Also do nothing if a wait was flagged in b:AtEndInsert != 0
	"
	if b:StartInsert == 0 && b:EndInsert == 0 && b:AtEndInsert == 0
	    if s:CursorInVar() == 1
		" Substitute this variable.
		call s:ReplVar(1)
	    endif
	    " Otherwise try to find another one someplace ahead.
	    call VPars_NextVar(1)
	    " Take care not to move the cursor beyond the end of the line
	    " in virtualedit mode.
	    if col(".") + 1 >= col("$")
		:normal $
	    endif
	else
	    " There is a block waiting.
	    " Do not move the cursor, taking for virtualedit into account.
	    " FIXME: These are all weird hacks. There must be a better way to
	    "        keep the cursor position.
	    if col(".") + 1 >= col("$")
		:normal $
	    elseif col(".") > 1
		:normal h
	    else
		:normal 0
	    endif
	endif
    endif

    " -------------------------------------------------------------------------
    " Finally update the VPars menu, if any.
    call VPars_UpdateMenu()
endfunction

"------------------------------------------------------------------------------
" Replace And Jump Inside Selected Block:				   {{{3
"
" Like VPars_RepJump but marks the selected block boundaries with b:StartInsert
" and b:EndInsert respectively and seyrches the first variable from the
" beginning of this block.
"
" Note:
" - If called after a show block command, no redefinition will take place,
"   provided the boundaries are still intact.
" - Subsequent variable substitutions should use VPars_RepJump (not necessary
"   to select the block again).
" - The level count for nested if .. else if statments will be reset to -1
"   (i.e. 'no pending if'). This accounts for the most common use, substituting
"   variables in chained if .. if else .. else statements.
" - Always starts a fresh replacement loop. All buffer local VPars state
"   variables will be reset.
"------------------------------------------------------------------------------
"
function VPars_RepJumpBlock()
    " First check whether there was a show block command recently.
    let newBlock = 1
    if exists("b:VPars_Pos") && b:VPars_Pos != []
	if b:StartInsert == line("'<") && b:EndInsert == line("'>")
	    " The boundaries did not change since, so leave the block intact.
	    let newBlock = 0
	endif
    endif

    if newBlock
	" Forget everything about any recent dedicated text block.
	call VPars_ResetVar()
	" Then initialize the new boundaries,
	let b:StartInsert = line("'<")
	let b:EndInsert = line("'>")
	let b:AtEndInsert = 0
	" and process this text block.
	call cursor( b:StartInsert, 1 )
    endif
    call VPars_RepJump( 0 )
endfunction

"------------------------------------------------------------------------------
" Start A Replacement Loop:						   {{{3
"
" Initializes VPars to work with a text block extending from <firstLine> to
" <lastLine> and starts the replacement loop at <firstLine>.
"
" If either <firstLine> or <lastLine> is set to 0, the existing b:StartInsert
" or b:EndInsert settings will be used in its place. Thus VPars_Run can be
" used to redefine the boundaries of an existing block.
"
" If <firstLine> is set to -1, VPars_RepJump() will be called immediately.
"
" Note:
" - If not simply continuing (<firstLine> != -1), this function will always
"   reset the buffer local variables, thus causing VPars to start processing on
"   a completely fresh block.
"   If this is not desired, use VPars_RepJump() instead.
" - No action if the block boundaries are invalid.
"   The user will get an error prompt if g:VPars_Verbose was set to non-zero.
" - The level count for nested if .. else if statments will be reset to -1
"   (i.e. 'no pending if'). This accounts for the most common use, substituting
"   variables in chained if .. if else .. else statements.
"------------------------------------------------------------------------------
"
function VPars_Run( firstLine, lastLine )
    " Only continue processing?
    if a:firstLine == -1
	" Do some validity checks.
	if ! exists("b:StartInsert") || ! exists("b:EndInsert")
	    call s:Warn( "There is no valid text block defined yet." )
	    return
	endif
	if b:StartInsert == 0 || b:EndInsert == 0
	    call s:Warn( "There is no valid text block defined yet." )
	    return
	endif
	" There are valid text boundaries. Initialize if necessary.
	if ! exists("b:AtEndInsert")
	    let b:AtEndInsert = 0
	    call VPars_ResetVar()
	endif
	" Now do the job.
	call VPars_RepJump( 0 )
	return
    endif

    " Just starting: First set the boundaries.
    if a:firstLine != 0
	let b:StartInsert = a:firstLine
    endif
    if a:lastLine != 0
	let b:EndInsert = a:lastLine
    endif

    " Warn the user on g:VPars_Verbose demand if the boundary setting is invalid.
    if ! exists("b:StartInsert") || ! exists("b:EndInsert")
	call s:Warn( "There is no valid text block defined yet." )
	return
    endif
    if exists("b:StartInsert") && exists("b:EndInsert")
	if b:StartInsert == 0 && b:EndInsert == 0
	    call s:Warn( "Text block invalid: Boundaries both reset to 0.")
	    return
	endif
    endif
    if ! exists("b:StartInsert") || b:StartInsert == 0
	call s:Warn( "Text block invalid: No start boundary." )
	return
    endif
    if ! exists("b:EndInsert") || b:EndInsert == 0
	call s:Warn( "Text block invalid: No end boundary." )
	return
    endif
    if b:StartInsert > b:EndInsert
	call s:Warn( "Text block invalid: Start line ".
		    \ b:StartInsert." below end line ".
		    \ b:EndInsert )
	return
    endif

    " When here, all is properly set up. go on.
    call VPars_ResetVar()
    let b:AtEndInsert = 0

    call cursor( b:StartInsert, 1 )
    call VPars_RepJump(0)
endfunction

"------------------------------------------------------------------------------
" Pause Processing Loop:						   {{{3
"
" Disables the processing of the current dedicated text block by setting the
" b:AtEndInsert end flag to -1, thus indicating a wait state.
" The same if there is currently no text block defined, thus inhibiting global
" mode.
" The current cursor position will be remembered in b:VPars_Pos if this buffer
" local variable is currently unused.
"
" If a valid dedicated text block exists, it will be surrounded by <|:0|> and
" <|0:|> markers.
"
" Note:
" - If there is no valid text block initializes b:StartInsert and b:EndInsert
"   to 0.
" - No action if the b:AtEndInsert block end condition has already been
"   set to 1.
" - Udates the VPars menu, if any.
"------------------------------------------------------------------------------
"
function VPars_Pause()
    if ! exists("b:StartInsert") || ! exists("b:EndInsert")
	call VPars_InvalidateBlock()
    endif

    if exists("b:AtEndInsert") && b:AtEndInsert == 1
	return
    endif

    " Remember the correct position before the markers were set.
    if b:VPars_Pos == []
	let b:VPars_Pos = getpos(".")
    endif

    if b:StartInsert != 0 && b:EndInsert != 0 && b:AtEndInsert == 0
	" Set markers only if there is a valid text block.
	let curPos = getpos(".")

	call cursor( b:StartInsert, 1 )
	:exe "normal i<|:0|>"
	call cursor( b:EndInsert, 1 )
	:exe "normal $a<|0:|>"

	call setpos( ".", curPos )
	if line(".") == b:StartInsert
	    :normal 6l
	endif
    endif

    let b:AtEndInsert = -1
    call VPars_UpdateMenu()

    echohl WarningMsg
    echo "Pause - processing of the dedicated text block is now inhibited."
    echohl None
endfunction

"------------------------------------------------------------------------------
" Resume Processing:							   {{{3
"
" If there is a valid text block, tries to resume processing at the location
" where the last variable had been replaced.
"
" Note:
" - No action if there is no valid text block at all.
" - If no valid variable processing could be detected (using the b:VStLine and
"   b:VarStart variables), processing will start at the beginning of the block.
"------------------------------------------------------------------------------
"
function VPars_Resume()
    if ! exists("b:StartInsert") || ! exists("b:EndInsert")
	return
    endif
    if b:StartInsert > b:EndInsert
	return
    endif

    let curPos = getpos(".")
    call cursor( 1, 1 )

    " Remove leading marker and adjust boundaries.
    if search( '<|:0|>' ) != 0
	:normal 6x
	if b:StartInsert != 0 && b:EndInsert != 0
	    let offset = line(".") - b:StartInsert
	    let b:StartInsert += offset
	    let b:EndInsert   += offset
	    let b:VStLine     += offset
	    let b:VEndLine    += offset
	endif
    endif

    " Remove trailing marker and adjust boundaries.
    if search( '<|0:|>', 'W' ) != 0
	:normal 6x
	if b:StartInsert != 0 && b:EndInsert != 0
	    let offset = line(".") - b:EndInsert
	    let b:EndInsert += offset
	    if b:VStLine >= line(".")
		let b:VStLine += offset
	    endif
	    if b:VEndLine >= line(".")
		let b:VEndLine += offset
	    endif
	endif
    endif

    if b:StartInsert != 0
	" Prepare to resume processing in the current dedicated text block.
	call cursor( b:StartInsert, 1 )

	" If a variable had been parsed before, force to reparse it.
	" Otherwise leave the cursor alone.
	if exists("b:VStLine") 
	    if exists("b:VarStart") && b:VarStart != -1
		call cursor( b:VStLine, b:VarStart )
	    endif
	endif
    else
	" There was no dedicated text block, only put the cursor back
	call setpos( ".", curPos )
    endif

    " And now resume processing.
    let b:AtEndInsert = 0
    call VPars_UpdateMenu()
    call VPars_RepJump(0)
endfunction

"===== Navigation Inside A Block =========================================={{{2

"------------------------------------------------------------------------------
" Jump To End:								   {{{3
"
" Puts the cursor at the end of the dedicated text block and flags AtEndInsert.
"------------------------------------------------------------------------------
"
function VPars_JumpEnd()
    if ! exists("b:StartInsert") || ! exists("b:EndInsert")
	return
    endif
    if b:StartInsert == 0 || b:EndInsert == 0
	return
    endif
    call cursor( b:EndInsert, 1 )
    :normal $
    let b:AtEndInsert = 1
    call VPars_UpdateMenu()
endfunction

"------------------------------------------------------------------------------
" Jump To Beginning:							   {{{3
"
" Puts the cursor at the beginning of the dedicated text block and resets
" AtEndInsert.
"------------------------------------------------------------------------------
"
function VPars_JumpBegin()
    if ! exists("b:StartInsert") || ! exists("b:EndInsert")
	return
    endif
    if b:StartInsert == 0 || b:EndInsert == 0
	return
    endif
    call cursor( b:StartInsert, 1 )
    let b:AtEndInsert = 0
    call VPars_UpdateMenu()
endfunction

"===== Text Block Status =================================================={{{2

"------------------------------------------------------------------------------
" Show Text Block:							   {{{3
"------------------------------------------------------------------------------
"
function VPars_ShowBlock()
    if ! exists("b:StartInsert") || ! exists("b:EndInsert")
	echohl WarningMsg
	echo "There is no dedicated text block defined yet."
	echohl None
	return
    endif
    if b:StartInsert == 0 || b:EndInsert == 0
	echohl WarningMsg
	echo "There is no dedicated text block defined yet."
	echohl None
	return
    endif
    " Remember the cursor position we temporarily leave now if this is
    " the first time.
    if b:VPars_Pos == []
	let b:VPars_Pos = getpos(".")
    endif
    " Now make the text block visible.
    call cursor( b:StartInsert, 0 )
    :exe "normal V".b:EndInsert."gg$"
endfunction

"------------------------------------------------------------------------------
" Invalidate Text Block:						   {{{3
"------------------------------------------------------------------------------
"
function VPars_InvalidateBlock()
    let b:StartInsert = 0
    let b:EndInsert = 0
    let b:AtEndInsert = 0
    call VPars_ResetVar()
    call VPars_UpdateMenu()
endfunction

"==============================================================================
"				  Body Actions				   {{{1
"==============================================================================

call VPars_Init()

" Show an initial VPars menu.
" This must be done after initialization, otherwise the menu would be empty.
if exists("g:VPars_ShowMenu") && g:VPars_ShowMenu
    call VPars_Menu()
endif

"=============================================================================
"				 Final Clean Up
"=============================================================================

" ----- Restore cpoptions ----------------------------------------------------
"
let &cpoptions = savecpo

"=== End of vpars.vim =====================================================}}}1
"
" vim:ft=vim:ts=8:sw=4:tw=0:fo=croql:enc=utf8:fdm=marker:fdc=7:fdl=0:fen
