
Faradays-Book is a great way to store your random notes and have them organized by tags implicitly.
This is an emacs client for the wonderful faradays-book.

# INSTALL

Download
http://github.com/icylisper/faradays-book.el/blob/master/frb.el and
add it to path

    (require 'frb)
    (setq frb-username <your-email-address>)
    (setq frb-password <your-password>)

# COMPONENTS
Faradays-book emacs client has 4 components: 

1. POST COMMANDS
2. QUERY COMMANDS
3. NOTES MODE
4. TAGS MODE

# FEATURES
## 1.0 [current]
### POST COMMANDS
* Post a note `frb-note` and `frb-open-note`
* Post a region in a buffer `frb-note-region` and `frb-open-note-region`
* Post an entire buffer `frb-note-buffer` and `frb-open-note-buffer`

### QUERY COMMANDS
* Get all notes  `frb-notes-all`
* Get all open notes `frb-open-notes`
* Get all notes for given tag `frb-notes : tag` (autocomplete tags in minibuffer)
* Get all tags   `frb-tags`
* Get all open tags  `frb-open-tags` 
* Command to login as a different user and query/post `frb-login`

### NOTES MODE
* frb-notes-mode - a readonly buffer with syntax highlighting
* Show help options
* Edit a note (TODO)
* Delete a note (TODO)
* Share a note (TODO)

### TAGS MODE
* frb-tags-mode - a readonly buffer with syntax highlighting
* Show help options
* Get all notes for given tag

## 1.1
### POST COMMANDS
* `frb-post-mode` - an alternative interactive mode to post notes.
* Highlight existing tags in a new note
* Spell-checker minor mode
* Post a given file (support from dired mode) `frb-note-file` and `frb-open-note-file`
* Post multiple notes in the buffer by specifying separator (defaults to empty line) `frb-note-multi`

### QUERY COMMANDS:
* Get all notes by regex
* Get all notes by time/duration
* Caching of tags for fast lookup in minibuffer autocomplete

### VIEW MODE
* Support offline viewing

### TAGS MODE
* Caching of tags
