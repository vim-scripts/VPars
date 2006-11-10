"##############################################################################
"
"     Filename:	vpars.vim
"
" GVIM Version: 7.0
"
"       Author: Bernd Pol
"        Email: bernd-pol@online.de
"
"      Version: 1.1
"      Created: 2006-09-19
"Last Revision: 2006-11-10
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
"       	Fritz Mehner for the csupport.vim script which served as a
"       	powerful source of concepts and testbed as well to further
"       	develop those variable substitution mechanisms up to the
"       	current state.
"
"  Description: Parameter substitution functions to be used in text fragments.
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
"                 empty (of the form <||:label:>), any text between the bars
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
"##############################################################################
"
" TODO: Allow VPars variables in substitution texts.
"
" TODO: Do not use hard coded '<|..|..|:..:|>' delimiters.
" 	(Might never be done as it probably will considerably complicate the
" 	script.)
"
" FIXME: Inconsistent behaviour after the cursor has been put inside the
" 	 delimiting brackets or immediately before the left one.
"	 (will sometimes substitute whole lines at the wrong place.)
"	 Same behaviour if b:AtEndInsert was externally forced to 1.
"
"##############################################################################

"==============================================================================
"                                   Interface
"==============================================================================
"
" === Global Variables ========================================================
"
" g:vpars_vim
" 	Prevents duplicate loading.
" 	Holds the version number if existent: major version
" 	number times 100 plus minor version number.
"
" g:VPars_NextKey
" 	Key to trigger the VPars_RepJump() function. Defaults to <F3>.
"
" g:VPars_AltNext
" 	Alternate shortcut to trigger the VPars_RepJump() function.
" 	Defaults to ',jj'.
"
" g:VPars_AltBegin
" 	Alternate shortcut to trigger the VPars_JumpBegin() function.
" 	Defaults to ',jb'.
"
" g:VPars_AltEnd
" 	Alternate shortcut to trigger the VPars_JumpEnd() function.
" 	Defaults to ',je'.
"
" g:VPars_Verbose
" 	If nonzero, issue warning messages in some cases of error.
"
" g:VPars_SkipBlockend
" 	If non-zero causes <F3> to automatically advance beyond the text block
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
" g:VPars_HookPrefix
" 	Holds the prefix the externally provided hook functions will use.
" 	Defaults to 'VPars'.
"
" === Functions ===============================================================
"
" VPars_Init()
" 	(Re-)initialize some VPars environment and interface settings such as
" 	the trigger key(default <F3>), etc.
"
" VPars_ResetVar()
" 	Initialize some buffer local variables.
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
"
" VPars_RepJump()
"	Replace current variable and jump to next one.
"
" VPars_RepJumpBlock()
" 	Replace current variable and jump to next one in the currently selected
" 	visual block.
" 	Note:
" 	Sets b:StartInsert and b:EndInsert to the visual block boundaries and
" 	resets b:AtEndInsert.
"
" VPars_RepList( list )
" 	Accepts an array of label-replacement string pairs of the form:
" 	    [['label_1' 'replacement_1'] ... ['label_n' 'replacement_n']]
" 	Scans the text between b:StartInsert and b:EndInsert for every given
" 	variable with implicit or explicit 'label_i' and replaces it with the
" 	corresponding 'replacement_i'.
" 	Note: Explicit labels must be surrounded with ':...:' delimiters.
"
" VPars_JumpBegin()
" 	Puts the cursor at the beginning of the dedicated text block and resets
" 	AtEndInsert.
"
" VPars_JumpEnd()
" 	Puts the cursor at the end of the dedicated text block and flags
" 	AtEndInsert.
"
" === External Functions ======================================================
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
" === Block Local Variables ===================================================
"
" --- Current Variable Properties ---------------------------------------------
"
" b:VStLine	the starting line number of this variable
" b:VEndLine	the ending line number of this variable (differs only after
" 		multiline replacements)
" b:VSubstLines	number of lines actually substituted by this replacement
" b:VarStart	the start column of this variable (before '<|')
" b:VarEnd	the end column of this variable (after '|>')
" 		(these column numbers are string indexes, starting at 0)
" b:ExpLabel	1 if there is an explicit label, otherwise 0
" b:VarLabel	label of this variable
" b:VarSubst	substitution value of this variable if no alternatives given
" b:SubstEmpty	1 if the initial substitution value in an explicitely
"		labeled variable was empty, otherwise 0;
" b:AltSubst	array of alternative substitution values,
"               b:VarSubst is empty in this case
" b:AltCol	array of column boundaries, relative to the start of the value
"               field, empty if there are no alternatives given
" b:VPars_Msg	message describing most recently encountered error, if any
"
" --- Boundaries of the text block to work in ----------------------------------
"
" These have to be externally defined, except when calling VPars_RepJumpBlock()
" (see above), but may be altered elsewhere in this script.
"
" b:StartInsert	start line of the most recently inserted text block
" b:EndInsert	end line of the most recently inserted text block
" b:AtEndInsert	1 if the end of the text block has been reached.
"
"==============================================================================

" Prevent duplicate loading and loading in compatible mode.
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
let g:vpars_vim = 101

" Load initialization file along the runtimepath:
runtime vparsrc.vim

"==============================================================================
"                               Initialization
"==============================================================================

function VPars_Init()

" --- Key to call the 'Next Variable' Function --------------------------------
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

" --- Mappings ----------------------------------------------------------------
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
:exe "vnoremap <silent> ".s:NextKey."  <Esc>:call VPars_RepJumpBlock()<Esc><Esc>a"
"
" Insert mode is more elaborate because we must get the cursor column right
" (especially in utf-8 multibyte environments).
" The inserted auxiliary character (here: 'x') will immediately be removed
" again by the VPars_RepJump() function (running in normal mode, then).
"
" FIXME: This does not work in replace mode!
"
:exe "inoremap <silent> ".s:NextKey." x<Esc>:call VPars_RepJump(1)<Esc>a"

" --- Alternate Shortcut Mappings ---------------------------------------------

let s:AltNext = ",jj"
if exists( "g:VPars_AltNext" )
    let s:AltNext = g:VPars_AltNext
endif

let s:AltBegin = ",jb"
if exists( "g:VPars_AltBegin" )
    let s:AltBegin = g:VPars_AltBegin
endif

let s:AltEnd = ",je"
if exists( "g:VPars_AltEnd" )
    let s:AltEnd = g:VPars_AltEnd
endif

if s:AltNext != ""
    :exe "nnoremap <silent> ".s:AltNext."       :call VPars_RepJump(0)<Esc>a"
    :exe "vnoremap <silent> ".s:AltNext."  <Esc>:call VPars_RepJumpBlock()<Esc><Esc>a"
    :exe "inoremap <silent> ".s:AltNext." x<Esc>:call VPars_RepJump(1)<Esc>a"
    let s:OldAltNext = s:AltNext
else
    if exists( "s:OldAltNext" ) && s:OldAltNext != ""
	exe "nunmap ".s:OldAltNext
	exe "vunmap ".s:OldAltNext
	exe "iunmap ".s:OldAltNext
    endif
endif

if s:AltBegin != ""
    :exe "nnoremap <silent> ".s:AltBegin."      :call VPars_JumpBegin()<Esc>a"
    :exe "inoremap <silent> ".s:AltBegin." <Esc>:call VPars_JumpBegin()<Esc>a"
    let s:OldAltBegin = s:AltBegin
else
    if exists( "s:OldAltBegin" ) && s:OldAltBegin != ""
	exe "nunmap ".s:OldAltBegin
	exe "iunmap ".s:OldAltBegin
    endif
endif

if s:AltEnd != ""
    :exe "nnoremap <silent> ".s:AltEnd."      :call VPars_JumpEnd()<Esc>a"
    :exe "inoremap <silent> ".s:AltEnd." <Esc>:call VPars_JumpEnd()<Esc>a"
    let s:OldAltEnd = s:AltEnd
else
    if exists( "s:OldAltEnd" ) && s:OldAltEnd != ""
	exe "nunmap ".s:OldAltEnd
	exe "iunmap ".s:OldAltEnd
    endif
endif

endfunction

"==============================================================================
"                                 Functions
"==============================================================================

"------------------------------------------------------------------------------
" Initialize Buffer Variables:
"------------------------------------------------------------------------------
"
function VPars_ResetVar()
    let b:VStLine	= 0
    let b:VEndLine	= 0
    let b:VSubstLines	= 0
    let b:VarStart	= -1
    let b:VarEnd	= -1
    let b:VarLabel	= ""
    let b:ExpLabel	= 0
    let b:VarSubst	= ""
    let b:SubstEmpty	= 0
    let b:AltSubst	= []
    let b:AltCol  	= []
    let b:VPars_Msg	= ""
endfunction

"------------------------------------------------------------------------------
" Find Next Variable:
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
" NOTE:
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
	call cursor( b:EndInsert, 999 )
    endif
    return 0
endfunction

"------------------------------------------------------------------------------
" Repeatedly Find Next Variable:
"
" Works exactly like s:GoNextVar, but jumps around to b:StartInsert in order
" to find another variable mark which may have been skipped in the previous run.
"------------------------------------------------------------------------------
"
function VPars_NextVar( glob )
    while 1
	let result = s:GoNextVar( a:glob )
	if result == -2
	    " A seemingly invalid variable has been found.
	    " Warn the user, if wanted, then try again.
	    if exists("g:VPars_Verbose")
		if g:VPars_Verbose
		    if b:VPars_Msg != ""
			echoerr "line ".b:VStLine.", col ".b:VarStart.": ".b:VPars_Msg
		    endif
		endif
	    endif
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
		    return 0
		endif
		if result == 1
		    " We found another valid one. Have it processed.
		    return 1
		endif
		if result == -2
		    if exists("g:VPars_Verbose")
			if g:VPars_Verbose
			    if b:VPars_Msg != ""
				echoerr "line ".b:VStLine.", col ".b:VarStart.": ".b:VPars_Msg
			    endif
			endif
		    endif
		    return -2
		endif
		" When here, we found an invalid or another delayed variable,
		" so go over all this again.
	    endif
	endif
    endwhile
endfunction

"------------------------------------------------------------------------------
" Parse Variable:
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
" NOTE:
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
" Replace Variable:
"
" Assumes the cursor somewhere between '<| .. |>' of the most recently parsed
" variable.
"
" NOTE:
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

"------------------------------------------------------------------------------
" Substitute Variables In The Most Recently Inserted Text Block:
"
" Finds all variables labeled b:VarLabel in the most recently inserted text
" block in this buffer and replaces them by the b:VarSubst text.
"
" NOTE:
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
" Replace Variables From A List:
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
" Adjust Line Numbers After Long Replacements:

" Tries to figure out whether the user typed in a multiline replacement string
" and adjusts the b:VEndLine and b:EndInsert line nubers accordingly.
"------------------------------------------------------------------------------
"
function s:AdjustLines()
    if exists("b:EndInsert")
	if exists("b:AtEndInsert") && b:AtEndInsert
	    " Text block end had been reached. Check where we currently are.
	    " (We check the line only, let the user decidedly go off.)
	    if line(".") >= b:StartInsert && line(".") <= b:EndInsert
		" Still inside, do nothing.
		return
	    else
		" The user moved the cursor away. This invalidates the text block.
		let b:AtEndInsert = 0
		let b:EndInsert = 0
		let b:StartInsert = 0
	    endif
	endif
    endif

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

"------------------------------------------------------------------------------
" Check Whether Cursor Is In A Variable:
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
	" There has been nothing parsed yet.
	return 0
    else
	if b:VarStart == -1
	    " No variable parsed yet.
	    return 0
	endif

	if b:VStLine == b:VEndLine
	    if b:VarStart + 2 <= realcol
		if b:ExpLabel
		    let endsubst = stridx( thisline, '|:', b:VarStart )
		else
		    let endsubst = stridx( thisline, '|>', b:VarStart )
		endif
		if (b:VarStart + 2) <= realcol && realcol <= endsubst
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
		    return 2
		endif
	    endif
	else
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
" Check Whether The Cursor Is In The Most Recently Inserted Block:
"
" RETURN:
"  0 if the cursor is not positioned inside the  block, otherwise 1.
" 
" NOTE:
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
    endif

    let curline = line(".")
    if b:StartInsert <= curline && curline <= b:EndInsert
	return 1
    endif

    return 0
endfunction

"------------------------------------------------------------------------------
" Replace And Jump To Next Variable:
"
" If the cursor is inside a variable replaces this accordingly and every equally
" labeled variable inside this most recently inserted text block. Then tries to
" position the cursor to immediately left of the label of the next found variable
" or at the end of the most recently inserted text block.
" If <insmode> is non-zero, the function was called from an insert mode mapping.
"
" NOTE:
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

    if ! exists("b:VarStart")
	call VPars_ResetVar()
    endif

    call s:AdjustLines()
    if s:CursorInBlock()
	" Do nothing if at end.
	if exists( "b:AtEndInsert" ) && b:AtEndInsert
	    return
	endif

	" Else check whether we are in a variable.
	let inVar = s:CursorInVar()
	if inVar == 1		" in the substitution field
	    call s:ReplVar(1)
	    call s:SubstVar()

	elseif inVar == 2	" elsewhere in the parsed variable
	    " Let the user try again.
	    call cursor( b:VStLine, b:VarStart - 1 )

	elseif inVar == -1	" in a yet unparsed variable
	    " Go parse this one first.
	    call cursor( 0, strridx( getline("."), '<|', col(".") ) - 1 )
	endif
	call VPars_NextVar(0)

    else " outside the most recently inserted block
	if s:CursorInVar() == 1
	    " Substitute this variable.
	    call s:ReplVar(1)
	endif
	" Otherwise try to find another one someplace ahead.
	call VPars_NextVar(1)
    endif
endfunction

"------------------------------------------------------------------------------
" Replace And Jump Inside Selected Block:
"
" Like VPars_RepJump but marks the selected block boundaries with b:StartInsert
" and b:EndInsert respectively and seyrches the first variable from the
" beginning of this block.
" NOTE:
" - Subsequent variable substitutions should use VPars_RepJump (not necessary
"   to select the block again).
" - The level count for nested if .. else if statments will be reset to -1
"   (i.e. 'no pending if'). This accounts for the most common use, substituting
"   variables in chained if .. if else .. else statements.
"------------------------------------------------------------------------------
"
function VPars_RepJumpBlock()
    let b:CondLevel = -1
    let b:StartInsert = line("'<")
    let b:EndInsert = line("'>")
    let b:AtEndInsert = 0
    call cursor( b:StartInsert, 1 )
    call VPars_RepJump( 0 )
endfunction

"------------------------------------------------------------------------------
" Jump To End:
"
" Puts the cursor at the end of the dedicated text block and flags AtEndInsert.
"------------------------------------------------------------------------------
"
function VPars_JumpEnd()
    if b:StartInsert == 0 || b:EndInsert == 0
	return
    endif
    call cursor( b:EndInsert, 999 )
    let b:AtEndInsert = 1
endfunction

"------------------------------------------------------------------------------
" Jump To Beginning:
"
" Puts the cursor at the beginning of the dedicated text block and resets
" AtEndInsert.
"------------------------------------------------------------------------------
"
function VPars_JumpBegin()
    if b:StartInsert == 0 || b:EndInsert == 0
	return
    endif
    call cursor( b:StartInsert, 1 )
    let b:AtEndInsert = 0
endfunction

"=== Body Actions =============================================================

call VPars_Init()

"=== End of vpars.vim =========================================================
"
" vim:ft=vim:ts=8:sw=4:tw=0:fo=croql
