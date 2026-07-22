// Bindings to isocline by Daan Leijen, generated via karl-zylinski/odin-c-bindgen
package isocline

import "core:c"

when ODIN_OS == .Windows {
	foreign import isocline "../../isocline/isocline.lib"
} else {
	foreign import isocline "../../isocline/libisocline.a"
}

/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

/*! \mainpage
Isocline C API reference.

Isocline is a pure C library that can be used as an alternative to the GNU
readline library.

See the [Github repository](https://github.com/daanx/isocline#readme)
for general information and building the library.

Contents:
- \ref readline
- \ref bbcode
- \ref history
- \ref completion
- \ref highlight
- \ref options
- \ref helper
- \ref completex
- \ref term
- \ref async
- \ref alloc
*/

/// \defgroup readline Readline
/// The basic readline interface.
/// \{

/// Isocline version: 110 = 1.1.0.
IC_VERSION :: (110)

@(default_calling_convention = "c")
foreign isocline {
	/// Read input from the user using rich editing abilities.
	/// @param prompt_text   The prompt text, can be NULL for the default ("").
	///   The displayed prompt becomes `prompt_text` followed by the `prompt_marker`
	///   ("> ").
	/// @returns the heap allocated input on succes, which should be `free`d by the
	/// caller.
	///   Returns NULL on error, or if the user typed ctrl+d or ctrl+c.
	///
	/// If the standard input (`stdin`) has no editing capability
	/// (like a dumb terminal (e.g. `TERM`=`dumb`), running in a debuggen, a pipe or
	/// redirected file, etc.) the input is read directly from the input stream up
	/// to the next line without editing capability. See also \a
	/// ic_set_prompt_marker(), \a ic_style_def()
	///
	/// @see ic_set_prompt_marker(), ic_style_def()
	ic_readline :: proc(prompt_text: cstring) -> cstring ---

	/// Print to the terminal while respection bbcode markup.
	/// Any unclosed tags are closed automatically at the end of the print.
	/// For example:
	/// ```
	/// ic_print("[b]bold, [i]bold and italic[/i], [red]red and bold[/][/b]
	/// default."); ic_print("[b]bold[/], [i b]bold and italic[/], [yellow on
	/// blue]yellow on blue background"); ic_style_add("em","i color=#888800");
	/// ic_print("[em]emphasis");
	/// ```
	/// Properties that can be assigned are:
	/// * `color=` _clr_, `bgcolor=` _clr_: where _clr_ is either a hex value
	/// `#`RRGGBB or `#`RGB, a
	///    standard HTML color name, or an ANSI palette name, like `ansi-maroon`,
	///    `ansi-default`, etc.
	/// * `bold`,`italic`,`reverse`,`underline`: can be `on` or `off`.
	/// * everything else is a style; all HTML and ANSI color names are also a style
	/// (so we can just use `red`
	///   instead of `color=red`, or `on red` instead of `bgcolor=red`), and there
	///   are the `b`, `i`, `u`, and `r` styles for bold, italic, underline, and
	///   reverse.
	///
	/// See [here](https://github.com/daanx/isocline#bbcode-format) for a
	/// description of the full bbcode format.
	ic_print :: proc(s: cstring) ---

	/// Print with bbcode markup ending with a newline.
	/// @see ic_print()
	ic_println :: proc(s: cstring) ---

	/// Print formatted with bbcode markup.
	/// @see ic_print()
	ic_printf :: proc(fmt: cstring, #c_vararg _: ..any) ---

	/// Print formatted with bbcode markup.
	/// @see ic_print
	ic_vprintf :: proc(fmt: cstring, args: i32) ---

	/// Define or redefine a style.
	/// @param style_name The name of the style.
	/// @param fmt        The `fmt` string is the content of a tag and can contain
	///   other styles. This is very useful to theme the output of a program
	///   by assigning standard styles like `em` or `warning` etc.
	ic_style_def :: proc(style_name: cstring, fmt: cstring) ---

	/// Start a global style that is only reset when calling a matching
	/// ic_style_close().
	ic_style_open :: proc(fmt: cstring) ---

	/// End a global style.
	ic_style_close :: proc() ---

	/// Enable history.
	/// Use a \a NULL filename to not persist the history. Use -1 for max_entries to
	/// get the default (200).
	ic_set_history :: proc(fname: cstring, max_entries: c.long) ---

	/// Remove the last entry in the history.
	/// The last returned input from ic_readline() is automatically added to the
	/// history; this function removes it.
	ic_history_remove_last :: proc() ---

	/// Clear the history.
	ic_history_clear :: proc() ---

	/// Add an entry to the history
	ic_history_add :: proc(entry: cstring) ---
}

/// A completion environment
ic_completion_env_s :: struct {}

/// A completion environment
ic_completion_env_t :: ic_completion_env_s

/// A completion callback that is called by isocline when tab is pressed.
/// It is passed a completion environment (containing the current input and the
/// current cursor position), the current input up-to the cursor (`prefix`) and
/// the user given argument when the callback was set. When using completion
/// transformers, like `ic_complete_quoted_word` the `prefix` contains the the
/// word to be completed without escape characters or quotes.
ic_completer_fun_t :: proc "c" (cenv: ^ic_completion_env_t, prefix: cstring)

@(default_calling_convention = "c")
foreign isocline {
	/// Set the default completion handler.
	/// @param completer  The completion function
	/// @param arg        Argument passed to the \a completer.
	/// There can only be one default completion function, setting it again disables
	/// the previous one. The initial completer use `ic_complete_filename`.
	ic_set_default_completer :: proc(completer: ^ic_completer_fun_t, arg: rawptr) ---

	/// In a completion callback (usually from ic_complete_word()), use this
	/// function to add a completion. (the completion string is copied by isocline
	/// and do not need to be preserved or allocated).
	///
	/// Returns `true` if the callback should continue trying to find more possible
	/// completions. If `false` is returned, the callback should try to return and
	/// not add more completions (for improved latency).
	ic_add_completion :: proc(cenv: ^ic_completion_env_t, completion: cstring) -> i32 ---

	/// In a completion callback (usually from ic_complete_word()), use this
	/// function to add a completion. The `display` is used to display the
	/// completion in the completion menu, and `help` is displayed for hints for
	/// example. Both can be `NULL` for the default. (all are copied by isocline and
	/// do not need to be preserved or allocated).
	///
	/// Returns `true` if the callback should continue trying to find more possible
	/// completions. If `false` is returned, the callback should try to return and
	/// not add more completions (for improved latency).
	ic_add_completion_ex :: proc(cenv: ^ic_completion_env_t, completion: cstring, display: cstring, help: cstring) -> i32 ---

	/// In a completion callback (usually from ic_complete_word()), use this
	/// function to add completions. The `completions` array should be terminated
	/// with a NULL element, and all elements are added as completions if they start
	/// with `prefix`.
	///
	/// Returns `true` if the callback should continue trying to find more possible
	/// completions. If `false` is returned, the callback should try to return and
	/// not add more completions (for improved latency).
	ic_add_completions :: proc(cenv: ^ic_completion_env_t, prefix: cstring, completions: ^cstring) -> i32 ---

	/// Complete a filename.
	/// Complete a filename given a semi-colon separated list of root directories
	/// `roots` and semi-colon separated list of possible extensions (excluding
	/// directories). If `roots` is NULL, the current directory is the root (".").
	/// If `extensions` is NULL, any extension will match.
	/// Each root directory should _not_ end with a directory separator.
	/// If a directory is completed, the `dir_separator` is added at the end if it
	/// is not `0`. Usually the `dir_separator` is `/` but it can be set to `\\` on
	/// Windows systems. For example:
	/// ```
	/// /ho         --> /home/
	/// /home/.ba   --> /home/.bashrc
	/// ```
	/// (This already uses ic_complete_quoted_word() so do not call it from inside a
	/// word handler).
	ic_complete_filename :: proc(cenv: ^ic_completion_env_t, prefix: cstring, dir_separator: i8, roots: cstring, extensions: cstring) ---
}

/// Function that returns whether a (utf8) character (of length `len`) is in a
/// certain character class
/// @see ic_char_is_separator() etc.
bool :: proc "c" (
) -> i32 /* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

@(default_calling_convention = "c")
foreign isocline {
	/// Complete a _word_ (i.e. _token_).
	/// Calls the user provided function `fun` to complete on the
	/// current _word_. Almost all user provided completers should use this
	/// function. If `is_word_char` is NULL, the default `&ic_char_is_nonseparator`
	/// is used. The `prefix` passed to `fun` is modified to only contain the
	/// current word, and any results from `ic_add_completion` are automatically
	/// adjusted to replace that part. For example, on the input "hello w", a the
	/// user `fun` only gets `w` and can just complete with "world" resulting in
	/// "hello world" without needing to consider `delete_before` etc.
	/// @see ic_complete_qword() for completing quoted and escaped tokens.
	ic_complete_word :: proc(cenv: ^ic_completion_env_t, prefix: cstring, fun: ^ic_completer_fun_t, is_word_char: ^i32) ---

	/// Complete a quoted _word_.
	/// Calls the user provided function `fun` to complete while taking
	/// care of quotes and escape characters. Almost all user provided completers
	/// should use this function. The `prefix` passed to `fun` is modified to be
	/// unquoted and unescaped, and any results from `ic_add_completion` are
	/// automatically quoted and escaped again. For example, completing `hello
	/// world`, the `fun` always just completes `hel` or `hello w` to `hello world`,
	/// but depending on user input, it will complete as:
	/// ```
	/// hel        -->  hello\ world
	/// hello\ w   -->  hello\ world
	/// hello w    -->                   # no completion, the word is just 'w'>
	/// "hel       -->  "hello world"
	/// "hello w   -->  "hello world"
	/// ```
	/// with proper quotes and escapes.
	/// If `is_word_char` is NULL, the default `&ic_char_is_nonseparator` is used.
	/// @see ic_complete_quoted_word() to customize the word boundary, quotes etc.
	ic_complete_qword :: proc(cenv: ^ic_completion_env_t, prefix: cstring, fun: ^ic_completer_fun_t, is_word_char: ^i32) ---

	/// Complete a _word_.
	/// Calls the user provided function `fun` to complete while taking
	/// care of quotes and escape characters. Almost all user provided completers
	/// should use this function. The `is_word_char` is a set of characters that are
	/// part of a "word". Use NULL for the default (`&ic_char_is_nonseparator`). The
	/// `escape_char` is the escaping character, usually `\` but use 0 to not have
	/// escape characters. The `quote_chars` define the quotes, use NULL for the
	/// default `"\'\""` quotes.
	/// @see ic_complete_word() which uses the default values for `non_word_chars`,
	/// `quote_chars` and `\` for escape characters.
	ic_complete_qword_ex :: proc(cenv: ^ic_completion_env_t, prefix: cstring, fun: ic_completer_fun_t, is_word_char: ^i32, escape_char: i8, quote_chars: cstring) ---
}

/// A syntax highlight environment
ic_highlight_env_s :: struct {}
ic_highlight_env_t :: ic_highlight_env_s

/// A syntax highlighter callback that is called by readline to syntax highlight
/// user input.
ic_highlight_fun_t :: proc "c" (henv: ^ic_highlight_env_t, input: cstring, arg: rawptr)

@(default_calling_convention = "c")
foreign isocline {
	/// Set a syntax highlighter.
	/// There can only be one highlight function, setting it again disables the
	/// previous one.
	ic_set_default_highlighter :: proc(highlighter: ^ic_highlight_fun_t, arg: rawptr) ---

	/// Set the style of characters starting at position `pos`.
	ic_highlight :: proc(henv: ^ic_highlight_env_t, pos: c.long, count: c.long, style: cstring) ---
}

/// Experimental: Convenience callback for a function that highlights `s` using
/// bbcode's. The returned string should be allocated and is free'd by the
/// caller.
ic_highlight_format_fun_t :: proc "c" (s: cstring, arg: rawptr) -> cstring

@(default_calling_convention = "c")
foreign isocline {
	/// Experimental: Convenience function for highlighting with bbcodes.
	/// Can be called in a `ic_highlight_fun_t` callback to colorize the `input`
	/// using the the provided `formatted` input that is the styled `input` with
	/// bbcodes. The content of `formatted` without bbcode tags should match `input`
	/// exactly.
	ic_highlight_formatted :: proc(henv: ^ic_highlight_env_t, input: cstring, formatted: cstring) ---

	/// Read input from the user using rich editing abilities,
	/// using a particular completion function and highlighter for this call only.
	/// both can be NULL in which case the defaults are used.
	/// @see ic_readline(), ic_set_prompt_marker(), ic_set_default_completer(),
	/// ic_set_default_highlighter().
	ic_readline_ex :: proc(prompt_text: cstring, completer: ^ic_completer_fun_t, completer_arg: rawptr, highlighter: ^ic_highlight_fun_t, highlighter_arg: rawptr) -> cstring ---

	/// Set a prompt marker and a potential marker for extra lines with multiline
	/// input. Pass \a NULL for the `prompt_marker` for the default marker (`"> "`).
	/// Pass \a NULL for continuation prompt marker to make it equal to the
	/// `prompt_marker`.
	ic_set_prompt_marker :: proc(prompt_marker: cstring, continuation_prompt_marker: cstring) ---

	/// Get the current prompt marker.
	ic_get_prompt_marker :: proc() -> cstring ---

	/// Get the current continuation prompt marker.
	ic_get_continuation_prompt_marker :: proc() -> cstring ---

	/// Disable or enable multi-line input (enabled by default).
	/// Returns the previous setting.
	ic_enable_multiline :: proc(_: bool) -> i32 ---

	/// Disable or enable sound (enabled by default).
	/// A beep is used when tab cannot find any completion for example.
	/// Returns the previous setting.
	ic_enable_beep :: proc(_: bool) -> i32 ---

	/// Disable or enable color output (enabled by default).
	/// Returns the previous setting.
	ic_enable_color :: proc(_: bool) -> i32 ---

	/// Disable or enable duplicate entries in the history (disabled by default).
	/// Returns the previous setting.
	ic_enable_history_duplicates :: proc(_: bool) -> i32 ---

	/// Disable or enable automatic tab completion after a completion
	/// to expand as far as possible if the completions are unique. (disabled by
	/// default). Returns the previous setting.
	ic_enable_auto_tab :: proc(_: bool) -> i32 ---

	/// Disable or enable preview of a completion selection (enabled by default)
	/// Returns the previous setting.
	ic_enable_completion_preview :: proc(_: bool) -> i32 ---

	/// Disable or enable automatic identation of continuation lines in multiline
	/// input so it aligns with the initial prompt.
	/// Returns the previous setting.
	ic_enable_multiline_indent :: proc(_: bool) -> i32 ---

	/// Disable or enable display of short help messages for history search etc.
	/// (full help is always dispayed when pressing F1 regardless of this setting)
	/// @returns the previous setting.
	ic_enable_inline_help :: proc(_: bool) -> i32 ---

	/// Disable or enable hinting (enabled by default)
	/// Shows a hint inline when there is a single possible completion.
	/// @returns the previous setting.
	ic_enable_hint :: proc(_: bool) -> i32 ---

	/// Set millisecond delay before a hint is displayed. Can be zero. (500ms by
	/// default).
	ic_set_hint_delay :: proc(delay_ms: c.long) -> c.long ---

	/// Disable or enable syntax highlighting (enabled by default).
	/// This applies regardless whether a syntax highlighter callback was set
	/// (`ic_set_highlighter`) Returns the previous setting.
	ic_enable_highlight :: proc(_: bool) -> i32 ---

	/// Set millisecond delay for reading escape sequences in order to distinguish
	/// a lone ESC from the start of a escape sequence. The defaults are 100ms and
	/// 10ms, but it may be increased if working with very slow terminals.
	ic_set_tty_esc_delay :: proc(initial_delay_ms: c.long, followup_delay_ms: c.long) ---

	/// Enable highlighting of matching braces (and error highlight unmatched
	/// braces).`
	ic_enable_brace_matching :: proc(_: bool) -> i32 ---

	/// Set matching brace pairs.
	/// Pass \a NULL for the default `"()[]{}"`.
	ic_set_matching_braces :: proc(brace_pairs: cstring) ---

	/// Enable automatic brace insertion (enabled by default).
	ic_enable_brace_insertion :: proc(_: bool) -> i32 ---

	/// Set matching brace pairs for automatic insertion.
	/// Pass \a NULL for the default `()[]{}\"\"''`
	ic_set_insertion_braces :: proc(brace_pairs: cstring) ---

	/// Get the raw current input (and cursor position if `cursor` != NULL) for the
	/// completion. Usually completer functions should look at their `prefix` though
	/// as transformers like `ic_complete_word` may modify the prefix (for example,
	/// unescape it).
	ic_completion_input :: proc(cenv: ^ic_completion_env_t, cursor: ^c.long) -> cstring ---

	/// Get the completion argument passed to `ic_set_completer`.
	ic_completion_arg :: proc(cenv: ^ic_completion_env_t) -> rawptr ---

	/// Do we have already some completions?
	ic_has_completions :: proc(_: ^ic_completion_env_t) -> i32 ---

	/// Do we already have enough completions and should we return if possible? (for
	/// improved latency)
	ic_stop_completing :: proc(_: ^ic_completion_env_t) -> i32 ---

	/// Primitive completion, cannot be used with most transformers (like
	/// `ic_complete_word` and `ic_complete_qword`). When completed, `delete_before`
	/// _bytes_ are deleted before the cursor position, `delete_after` _bytes_ are
	/// deleted after the cursor, and finally `completion` is inserted. The
	/// `display` is used to display the completion in the completion menu, and
	/// `help` is displayed with hinting. Both `display` and `help` can be NULL.
	/// (all are copied by isocline and do not need to be preserved or allocated).
	///
	/// Returns `true` if the callback should continue trying to find more possible
	/// completions. If `false` is returned, the callback should try to return and
	/// not add more completions (for improved latency).
	ic_add_completion_prim :: proc(_: ^ic_completion_env_t, _: cstring, _: cstring, _: cstring, _: c.long, _: c.long) -> i32 ---

	/// Convenience: return the position of a previous code point in a UTF-8 string
	/// `s` from postion `pos`. Returns `-1` if `pos <= 0` or `pos > strlen(s)` (or
	/// other errors).
	ic_prev_char :: proc(s: cstring, pos: c.long) -> c.long ---

	/// Convenience: return the position of the next code point in a UTF-8 string
	/// `s` from postion `pos`. Returns `-1` if `pos < 0` or `pos >= strlen(s)` (or
	/// other errors).
	ic_next_char :: proc(s: cstring, pos: c.long) -> c.long ---

	/// Convenience: does a string `s` starts with a given `prefix` ?
	ic_starts_with :: proc(_: cstring, _: cstring) -> i32 ---

	/// Convenience: does a string `s` starts with a given `prefix` ignoring (ascii)
	/// case?
	ic_istarts_with :: proc(_: cstring, _: cstring) -> i32 ---

	/// Convenience: character class for whitespace `[ \t\r\n]`.
	ic_char_is_white :: proc(_: cstring, _: c.long) -> i32 ---

	/// Convenience: character class for non-whitespace `[^ \t\r\n]`.
	ic_char_is_nonwhite :: proc(_: cstring, _: c.long) -> i32 ---

	/// Convenience: character class for separators.
	/// (``[ \t\r\n,.;:/\\(){}\[\]]``.)
	/// This is used for word boundaries in isocline.
	ic_char_is_separator :: proc(_: cstring, _: c.long) -> i32 ---

	/// Convenience: character class for non-separators.
	ic_char_is_nonseparator :: proc(_: cstring, _: c.long) -> i32 ---

	/// Convenience: character class for letters (`[A-Za-z]` and any unicode >
	/// 0x80).
	ic_char_is_letter :: proc(_: cstring, _: c.long) -> i32 ---

	/// Convenience: character class for digits (`[0-9]`).
	ic_char_is_digit :: proc(_: cstring, _: c.long) -> i32 ---

	/// Convenience: character class for hexadecimal digits (`[A-Fa-f0-9]`).
	ic_char_is_hexdigit :: proc(_: cstring, _: c.long) -> i32 ---

	/// Convenience: character class for identifier letters (`[A-Za-z0-9_-]` and any
	/// unicode > 0x80).
	ic_char_is_idletter :: proc(_: cstring, _: c.long) -> i32 ---

	/// Convenience: character class for filename letters (_not in_ "
	/// \t\r\n`@$><=;|&\{\}\(\)\[\]]").
	ic_char_is_filename_letter :: proc(_: cstring, _: c.long) -> i32 ---

	/// Convenience: If this is a token start, return the length. Otherwise return
	/// 0.
	ic_is_token :: proc(s: cstring, pos: c.long, is_token_char: ^i32) -> c.long ---

	/// Convenience: Does this match the specified token?
	/// Ensures not to match prefixes or suffixes, and returns the length of the
	/// match (in bytes). E.g.
	/// `ic_match_token("function",0,&ic_char_is_letter,"fun")` returns 0. while
	/// `ic_match_token("fun x",0,&ic_char_is_letter,"fun"})` returns 3.
	ic_match_token :: proc(s: cstring, pos: c.long, is_token_char: ^i32, token: cstring) -> c.long ---

	/// Convenience: Do any of the specified tokens match?
	/// Ensures not to match prefixes or suffixes, and returns the length of the
	/// match (in bytes). E.g.
	/// `ic_match_any_token("function",0,&ic_char_is_letter,{"fun","func",NULL})`
	/// returns 0. while `ic_match_any_token("func
	/// x",0,&ic_char_is_letter,{"fun","func",NULL})` returns 4.
	ic_match_any_token :: proc(s: cstring, pos: c.long, is_token_char: ^i32, tokens: ^cstring) -> c.long ---

	/// Initialize for terminal output.
	/// Call this before using the terminal write functions (`ic_term_write`)
	/// Does nothing on most platforms but on Windows it sets the console to UTF8
	/// output and possible enables virtual terminal processing.
	ic_term_init :: proc() ---

	/// Call this when done with the terminal functions.
	ic_term_done :: proc() ---

	/// Flush the terminal output.
	/// (happens automatically on newline characters ('\n') as well).
	ic_term_flush :: proc() ---

	/// Write a string to the console (and process CSI escape sequences).
	ic_term_write :: proc(s: cstring) ---

	/// Write a string to the console and end with a newline
	/// (and process CSI escape sequences).
	ic_term_writeln :: proc(s: cstring) ---

	/// Write a formatted string to the console.
	/// (and process CSI escape sequences)
	ic_term_writef :: proc(fmt: cstring, #c_vararg _: ..any) ---

	/// Write a formatted string to the console.
	ic_term_vwritef :: proc(fmt: cstring, args: i32) ---

	/// Set text attributes from a style.
	ic_term_style :: proc(style: cstring) ---

	/// Set text attribute to bold.
	ic_term_bold :: proc(enable: bool) ---

	/// Set text attribute to underline.
	ic_term_underline :: proc(enable: bool) ---

	/// Set text attribute to italic.
	ic_term_italic :: proc(enable: bool) ---

	/// Set text attribute to reverse video.
	ic_term_reverse :: proc(enable: bool) ---

	/// Set text attribute to ansi color palette index between 0 and 255 (or 256 for
	/// the ANSI "default" color). (auto matched to smaller palette if not
	/// supported)
	ic_term_color_ansi :: proc(foreground: bool, color: i32) ---

	/// Set text attribute to 24-bit RGB color (between `0x000000` and `0xFFFFFF`).
	/// (auto matched to smaller palette if not supported)
	ic_term_color_rgb :: proc(foreground: bool, color: u32) ---

	/// Reset the text attributes.
	ic_term_reset :: proc() ---

	/// Get the palette used by the terminal:
	/// This is usually initialized from the COLORTERM environment variable. The
	/// possible values of COLORTERM for each palette are given in parenthesis.
	///
	/// - 1: monochrome (`monochrome`)
	/// - 3: old ANSI terminal with 8 colors, using bold for bright
	/// (`8color`/`3bit`)
	/// - 4: regular ANSI terminal with 16 colors.     (`16color`/`4bit`)
	/// - 8: terminal with ANSI 256 color palette.     (`256color`/`8bit`)
	/// - 24: true-color terminal with full RGB colors.
	/// (`truecolor`/`24bit`/`direct`)
	ic_term_get_color_bits :: proc() -> i32 ---

	/// Thread-safe way to asynchronously unblock a readline.
	/// Behaves as if the user pressed the `ctrl-C` character
	/// (resulting in returning NULL from `ic_readline`).
	/// Returns `true` if the event was successfully delivered.
	/// (This may not be supported on all platforms, but it is
	/// functional on Linux, macOS and Windows).
	ic_async_stop :: proc() -> i32 ---
}

//--------------------------------------------------------------
/// \defgroup alloc Initialization and Custom Allocation
/// Initialized and register allocation functions for custom allocators
/// \{
ic_malloc_fun_t :: proc "c" (size: i32) -> rawptr
ic_realloc_fun_t :: proc "c" (p: rawptr, newsize: i32) -> rawptr
ic_free_fun_t :: proc "c" (p: rawptr)

@(default_calling_convention = "c")
foreign isocline {
	/// Initialize with using stderr as output.
	/// This must be called as early as possible in a program!
	ic_init :: proc(use_std_err: bool) ---

	/// Initialize with custom allocation functions.
	/// This must be called as early as possible in a program!
	ic_init_custom_alloc :: proc(_malloc: ^ic_malloc_fun_t, _realloc: ^ic_realloc_fun_t, _free: ^ic_free_fun_t) ---

	/// Initialize with custom allocation functions and potentially use stderr as
	/// output. This must be called as early as possible in a program!
	ic_init_custom_alloc_ex :: proc(_malloc: ^ic_malloc_fun_t, _realloc: ^ic_realloc_fun_t, _free: ^ic_free_fun_t, use_std_err: bool) ---

	/// Free a potentially custom alloc'd pointer (in particular, the result
	/// returned from `ic_readline`)
	ic_free :: proc(p: rawptr) ---

	/// Allocate using the current memory allocator.
	ic_malloc :: proc(sz: i32) -> rawptr ---

	/// Duplicate a string using the current memory allocator.
	ic_strdup :: proc(s: cstring) -> cstring ---
}
