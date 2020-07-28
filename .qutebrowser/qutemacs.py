# qutemacs - a simple, preconfigured Emacs binding set for qutebrowser
#
# The aim of this binding set is not to provide bindings for absolutely
# everything, but to provide a stable launching point for people to make their
# own bindings.
#
# Installation:
#
# 1. Copy this file or add this repo as a submodule to your dotfiles.
# 2. Add this line to your config.py, and point the path to this file:
# config.source('qutemacs/qutemacs.py')


config = config  # type: ConfigAPI # noqa: F821 pylint: disable=E0602,C0103
c = c  # type: ConfigContainer # noqa: F821 pylint: disable=E0602,C0103

# disable insert mode completely
c.input.insert_mode.auto_enter = False
c.input.insert_mode.auto_leave = False
c.input.insert_mode.plugins = False

# Get rid of the status bar 
c.statusbar.hide = True

## Format to use for the tab title. The following placeholders are
## defined:  * `{perc}`: Percentage as a string like `[10%]`. *
## `{perc_raw}`: Raw percentage, e.g. `10`. * `{current_title}`: Title of
## the current web page. * `{title_sep}`: The string ` - ` if a title is
## set, empty otherwise. * `{index}`: Index of this tab. * `{id}`:
## Internal tab ID of this tab. * `{scroll_pos}`: Page scroll position. *
## `{host}`: Host of the current web page. * `{backend}`: Either
## ''webkit'' or ''webengine'' * `{private}`: Indicates when private mode
## is enabled. * `{current_url}`: URL of the current web page. *
## `{protocol}`: Protocol (http/https/...) of the current web page. *
## `{audio}`: Indicator for audio/mute status.
## Type: FormatString
c.tabs.title.format = '{index} | {scroll_pos} | {audio}: {current_title}'
c.window.title_format = '{perc} {scroll_pos}{title_sep}{current_title}{private}'

## When to show the tab bar.
## Type: String
## Valid values:
##   - always: Always show the tab bar.
##   - never: Always hide the tab bar.
##   - multiple: Hide the tab bar if only one tab is open.
##   - switching: Show the tab bar when switching tabs.
c.tabs.show = 'multiple'

## Position of new tabs which are not opened from another tab. See
## `tabs.new_position.stacking` for controlling stacking behavior.
## Type: NewTabPosition
## Valid values:
##   - prev: Before the current tab.
##   - next: After the current tab.
##   - first: At the beginning.
##   - last: At the end.
c.tabs.new_position.unrelated = 'next'

## Which categories to show (in which order) in the :open completion.
## Type: FlagList
## Valid values:
##   - searchengines
##   - quickmarks
##   - bookmarks
##   - history
c.completion.open_categories = ['bookmarks', 'searchengines', 'quickmarks', 'history']

## Number of URLs to show in the web history. 0: no history / -1:
## unlimited
## Type: Int
c.completion.web_history.max_items = -1

## Allow pdf.js to view PDF files in the browser. Note that the files can
## still be downloaded by clicking the download button in the pdf.js
## viewer.
## Type: Bool
c.content.pdfjs = True

# Spellchecker
# c.spellcheck.languages = ["en-US", "nl-NL"]

# Set sites to function as search engines from console 
c.url.searchengines = {
    'DEFAULT': 'https://duckduckgo.com/?q={}',
    're': 'https://www.reddit.com/r/{}',
    'yt': 'https://www.youtube.com/results?search_query={}',
    'ma': 'https://www.google.com/maps/place/{}',
    'lib': 'http://gen.lib.rus.ec/search.php?req={}',
    'mp': 'https://www.marktplaats.nl/q/{}',
    'th': 'https://www.thesaurus.com/browse/{}',
    'tr': 'https://translate.google.com/#view=home&op=translate&sl=auto&tl=en&text={}',
    'sc': 'https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q={}'
}

# Forward unbound keys
c.input.forward_unbound_keys = "all"

ESC_BIND = 'clear-keychain ;; search ;; fullscreen --leave'

c.bindings.default['normal'] = {}
# Bindings
c.bindings.commands['normal'] = {
        # Development
        '<ctrl-x>d': 'view-source',

        # Bookmarks
        '<ctrl-c>a': 'bookmark-add',
        '<ctrl-c>d': 'bookmark-del',
    
	# Navigation
	'<ctrl-v>': 'scroll-page 0 0.5',
	'<alt-v>': 'scroll-page 0 -0.5',
	'<ctrl-shift-v>': 'scroll-page 0 1',
	'<alt-shift-v>': 'scroll-page 0 -1',
	'<ctrl-x>o': 'set-cmd-text -s :tab-focus',
	'<ctrl-x>%': 'set-cmd-text -s :scroll-to-perc',

	# Commands
	'<alt-x>': 'set-cmd-text :',
	'<ctrl-x><ctrl-c>': 'close',
	# '<ctrl-/>': 'undo',

	# searching
	'<ctrl-s>': 'set-cmd-text /',
	'<ctrl-r>': 'set-cmd-text ?',

        # qute-pass
        '<shift-alt-p>': 'set-cmd-text -s :spawn --userscript qute-lastpass --password-only',
        '<ctrl-alt-p>': 'set-cmd-text -s :spawn --userscript qute-lastpass --username-only',
    
	# quickmarks
	'<ctrl-q>l': 'set-cmd-text -s :quickmark-load ',
	'<ctrl-q>a': 'set-cmd-text -s :quickmark-add ',
	'<ctrl-q>d': 'set-cmd-text -s :quickmark-del ',

        # Copy url
        '<ctrl-c>u': 'yank pretty-url -s',
        '<ctrl-c>t': 'yank title -s',
        '<ctrl-c>d': 'yank domain -s',
        '<ctrl-c>o': 'yank inline [[{url}][{title}]] -s',
    
	# hinting (shows all links)
	'<alt-s>': 'hint all',
        '<alt-l>f': 'fullscreen',
	'<alt-l>i': 'hint images',
	'<alt-l><alt-i>': 'hint images download',
	'<alt-l><ctrl-i>': 'hint images tab',
	'<alt-l>l': 'hint links',
	'<alt-l><alt-l>': 'hint links yank',
	'<alt-l><ctrl-l>': 'hint links tab',
	'<alt-l>h': 'hint all hover',
	'<alt-l>p': 'hint inputs',
	'<alt-l><alt-p>': 'hint inputs --first',
	'<alt-l>w': 'hint --rapid links window',
	'<alt-l>o': 'hint links fill :open -t -r {hint-url}',
    
	# history
	# FIXME maybe this should be <C-b> <C-n>? Or would that be too confusing?
	'<ctrl-]>': 'forward',
	'<ctrl-[>': 'back',
	'<ctrl-z>': 'repeat-command',

	# tabs
        '<ctrl-x>b': 'set-cmd-text -s :buffer ',
	'<ctrl-alt-f>': 'tab-next',
	'<ctrl-alt-b>': 'tab-prev',
	'<ctrl-x>1': 'tab-only',
	'<ctrl-x>k': 'tab-close',
	'<ctrl-x>0': 'tab-close',
        '<ctrl-x>-': 'tab-move -',
        '<ctrl-x>+': 'tab-move +',
	'<ctrl-x>3': 'tab-clone',
        '<ctrl-x>r': 'reload -f',
        '<ctrl-x>m': 'tab-mute',
        '<ctrl-x>s': 'save',
        '<ctrl-x><ctrl-n>': 'tab-give',
        '<ctrl-x><ctrl-t>': 'set-cmd-text -s :tab-take',
        '<ctrl-x><ctrl-k>': 'set-cmd-text -s :tab-take --keep',
        '<ctrl-x><ctrl-f>': 'set-cmd-text -s :open -t ',
        '<ctrl-x><ctrl-shift-f>': 'set-cmd-text -s :open ',
	'<alt-1>': 'tab-focus 1',
	'<alt-2>': 'tab-focus 2',
	'<alt-3>': 'tab-focus 3',
	'<alt-4>': 'tab-focus 4',
	'<alt-5>': 'tab-focus 5',
	'<alt-6>': 'tab-focus 6',
	'<alt-7>': 'tab-focus 7',
	'<alt-8>': 'tab-focus 8',
	'<alt-9>': 'tab-focus 9',
	'<alt-0>': 'tab-focus -1',

        # Zoom
        '<ctrl-=>': 'zoom-in',
        '<ctrl-->': 'zoom-out',
        '<ctrl-alt-=>': 'zoom',
      
        # Reload config
        '<ctrl-c>r': 'config-source',

	# editing
	'<ctrl-f>': 'fake-key <Right>',
	'<ctrl-b>': 'fake-key <Left>',
	'<ctrl-a>': 'fake-key <Home>',
	'<ctrl-e>': 'fake-key <End>',
	'<ctrl-n>': 'fake-key <Down>',
	'<ctrl-p>': 'fake-key <Up>',
	'<ctrl-k>': 'fake-key <Ctrl-Delete>',
	'<alt-f>': 'fake-key <Ctrl-Right>',
	'<alt-b>': 'fake-key <Ctrl-Left>',
	'<ctrl-d>': 'fake-key <Delete>',
	'<alt-d>': 'fake-key <Ctrl-Delete>',
	'<alt-backspace>': 'fake-key <Ctrl-Backspace>',
	'<ctrl-w>': 'fake-key <Ctrl-Backspace>',
	'<ctrl-y>': 'insert-text {primary}',

	# Numbers
	# https://github.com/qutebrowser/qutebrowser/issues/4213
	'1': 'fake-key 1',
	'2': 'fake-key 2',
	'3': 'fake-key 3',
	'4': 'fake-key 4',
	'5': 'fake-key 5',
	'6': 'fake-key 6',
	'7': 'fake-key 7',
	'8': 'fake-key 8',
	'9': 'fake-key 9',
	'0': 'fake-key 0',

	# escape hatch
	'<ctrl-h>': 'set-cmd-text -s :help',
	'<ctrl-g>': ESC_BIND,
}

c.bindings.commands['command'] = {
	'<ctrl-s>': 'search-next',
	'<ctrl-r>': 'search-prev',

	'<ctrl-p>': 'completion-item-focus prev',
	'<ctrl-n>': 'completion-item-focus next',

	'<alt-p>': 'command-history-prev',
	'<alt-n>': 'command-history-next',

	# escape hatch
	'<ctrl-g>': 'leave-mode',
}

c.bindings.commands['hint'] = {
	# escape hatch
	'<ctrl-g>': 'leave-mode',
}


c.bindings.commands['caret'] = {
	# escape hatch
	'<ctrl-g>': 'leave-mode',
}

