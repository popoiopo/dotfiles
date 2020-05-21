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

# Forward unbound keys
c.input.forward_unbound_keys = "all"

ESC_BIND = 'clear-keychain ;; search ;; fullscreen --leave'

c.bindings.default['normal'] = {}
# Bindings
c.bindings.commands['normal'] = {
	# Navigation
	'<ctrl-v>': 'scroll-page 0 0.5',
	'<alt-v>': 'scroll-page 0 -0.5',
	'<ctrl-shift-v>': 'scroll-page 0 1',
	'<alt-shift-v>': 'scroll-page 0 -1',
	# FIXME come up with logical bindings for scrolling left/right

	# Commands
	'<alt-x>': 'set-cmd-text :',
	'<ctrl-x>b': 'set-cmd-text -s :buffer',
	'<ctrl-x><ctrl-c>': 'quit',

	# searching
	'<ctrl-s>': 'set-cmd-text /',
	'<ctrl-r>': 'set-cmd-text ?',
    
	# quickmarks
	'<ctrl-q>l': 'set-cmd-text -s :quickmark-load ',
	'<ctrl-q>a': 'set-cmd-text -s :quickmark-add ',

	# hinting (shows all links)
	'<alt-s>': 'hint all',

	# history
	# FIXME maybe this should be <C-b> <C-n>? Or would that be too confusing?
	'<ctrl-]>': 'forward',
	'<ctrl-[>': 'back',

	# tabs
	'<ctrl-j>': 'tab-next',
	'<ctrl-k>': 'tab-prev',
	'<ctrl-x>1': 'tab-only',
	'<ctrl-x>0': 'tab-close',
	'<ctrl-x>3': 'tab-clone',
	'<ctrl-x>o': 'tab-focus',

        # Zoom
        '<ctrl-=>': 'zoom-in',
        '<ctrl-->': 'zoom-out',
        '<ctrl-x>=': 'zoom',
      
        # Zoom
        '<ctrl-c>r': 'config-source',

	# open links
	'<ctrl-l>': 'set-cmd-text -s :open',
	'<alt-l>': 'set-cmd-text -s :open -t',

	# editing
	'<ctrl-f>': 'fake-key <Right>',
	'<ctrl-b>': 'fake-key <Left>',
	'<ctrl-a>': 'fake-key <Home>',
	'<ctrl-e>': 'fake-key <End>',
	'<ctrl-n>': 'fake-key <Down>',
	'<ctrl-p>': 'fake-key <Up>',
	'<alt-f>': 'fake-key <Ctrl-Right>',
	'<alt-b>': 'fake-key <Ctrl-Left>',
	'<ctrl-d>': 'fake-key <Delete>',
	'<alt-d>': 'fake-key <Ctrl-Delete>',
	'<alt-backspace>': 'fake-key <Ctrl-Backspace>',
	'<ctrl-w>': 'fake-key <Ctrl-backspace>',
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

