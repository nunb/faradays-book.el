
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

### POST COMMANDS
* Post a note `frb-note` / `frb-open-note`
* Post a region in a buffer `frb-note-region` / `frb-open-note-region`
* Post an entire buffer `frb-note-buffer` / `frb-open-note-buffer`
* `frb-post-mode` - an alternate interactive mode to post notes (*TODO*)
* Highlight existing tags in a new note (*TODO*)
* Spell-checker minor mode (*TODO*)
* Post a given file (support from dired mode) `frb-note-file` and `frb-open-note-file` (*TODO*)
* Post multiple notes in the buffer by specifying separator (defaults to empty line) `frb-note-multi` (*TODO*)

### QUERY COMMANDS
* Get all notes  `frb-notes-all`
* Get all open notes `frb-open-notes`
* Get all notes for given tag `frb-notes : tag` (autocomplete tags in minibuffer)
* Get all tags   `frb-tags`
* Get all open tags  `frb-open-tags` 
* Command to login as a different user `frb-login`
* Get all notes by regex. `frb-notes` takes a regex or a tag (*TODO*)
* Get all notes by time/duration. `frb-notes-time` pops-up an emacs calendar to chose date/duration. (*TODO*)
* Caching of tags for fast lookup in minibuffer autocomplete (*TODO*)

### NOTES MODE
* `frb-notes-mode` - a minor mode for viewing notes with highlighting
and interactive view 
* `?` shows help options
* Edit a note (*TODO*) 
* Delete a note
* Share/Unshare a note
* Offline viewing  (*TODO*)

### TAGS MODE
* `frb-tags-mode` - a major mode for viewing notes with highlighting and interactive view
* `?` shows help options
* [return] key on a tag gets all notes for the tag and shifts to notes-mode 
* Caching of tags (*TODO*)
