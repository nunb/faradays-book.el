
Faradays-Book is a great way to store your random notes and have them organized by tags implicitly.
This is an emacs client for the wonderful faradays-book.

# INSTALL

1. Download http://github.com/icylisper/faradays-book.el/blob/master/frb.el
2. Add frb.el to path

    (require 'frb)
    (setq frb-username <your-email-address>)
    (setq frb-password <your-password>)

# COMPONENTS
The fraradays book emacs client has 4 components: 

1. QUERY
2. VIEW NOTES
3. VIEW TAGS
4. POST

# FEATURES
## 1.0 [current]

### QUERY
* Get all notes  `frb-notes-all`
* Get all open notes `frb-open-notes`
* Get all notes for given tag `frb-notes : tag`
   Autocomplete tags in minibuffer
* Get all tags   `frb-tags`
* Get all open tags  `frb-open-tags` 
* Option to login as a different user and query/post
* Faradaysbook server can be any instance of the faradaysbook application. Defaults to http://faradaysbook.com

### VIEW NOTES
* frb-notes-mode - a readonly buffer with syntax highlighting
* Show help options
* Edit a note (TODO)
* Delete a note (TODO)
* Share a note (TODO)

### VIEW TAGS
* frb-tags-mode - a readonly buffer with syntax highlighting
* Show help options
* Get all notes for given tag

### POST
* Post a note `frb-note`
* Post a region in a buffer `frb-note-region` 
* Post an entire buffer `frb-note-buffer`
* Openbook clone of all post functionality

## 1.1
### QUERY:
* Get all notes by regex
* Get all notes by time/duration
* Caching of tags for fast lookup in minibuffer autocomplete

### VIEW NOTES
* Support offline viewing

### VIEW TAGS
* Caching of  tags

### POST
* frb-post-mode - an alternative interactive mode to post notes.
* Highlight existing tags in a new note
* Spell-checker 
* Post a given file (support from dired mode)
* Post multiple notes by specifying separator (defaults to empty line)

