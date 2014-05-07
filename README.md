Bog is a system for taking research notes in [Org mode](http://orgmode.org/). It adds a few
research-specific features, nearly all of which are focused on managing
and taking notes with Org, not on writing research articles with Org.

# Installation

Bog is available on [MELPA](http://melpa.milkbox.net/).

To enable Bog in all Org buffers, add it to `org-mode-hook`.

    (add-hook 'org-mode-hook 'bog-mode)

# Workflow

Many people use Org for taking research notes, and there are some really
nice descriptions of systems people have come up with (for a few
examples, see [these](http://thread.gmane.org/gmane.emacs.orgmode/78983) [threads](http://thread.gmane.org/gmane.emacs.orgmode/14756) on the Org mode mailing list).

The Bog workflow is focused around the citekey, which is the only study
information that must be included in the notes. This unique identifier
is used as a link to the BibTeX file and other associated files.

In the example below, the citekey "name2000word" is a study heading. Bog
expects the citekey to be the title or property of a heading. The
citekey "another1999study" is a reference to another study (which may or
may not have a subtree in this or another Org file).

    * Topic heading
    
    ** TODO name2000word                               :atag:
    
    <URL for study>
    
    Article notes ... a reference to another1999study ...

The default format for the citekey is the first author's last name, the
year, and then the first non-trivial word. To have BibTeX mode
automatically generate a key of this format, the `bibtex-autokey-*`
settings can be modified.

    (setq bibtex-autokey-year-length 4
          bibtex-autokey-titleword-length nil
          bibtex-autokey-titlewords-stretch 0
          bibtex-autokey-titlewords 1
          bibtex-autokey-year-title-separator "")

# Main features

Many Bog functions take the citekey from the notes. If the point is on a
citekey (like "another1999study" above), then that citekey will be used.
Otherwise, the citekey will be taken from the first parent heading that
is a study (if that makes sense for the given function).
-   `bog-find-citekey-file`
    
    Open an associated file for a citekey.

-   `bog-find-citekey-bib`
    
    Open a BibTeX file for a citekey.
    
    BibTeX entries can be stored in one of two ways:
    
    -   As a single file with many entries
    -   As single-entry files named citekey.bib within a common directory

-   `bog-search-citekey-on-web`
    
    Search Google Scholar for a citekey. The default citekey format (first
    author's last name, year, and first non-trivial word) usually contains
    enough information to make this search successful.

-   `bog-rename-staged-file-to-citekey`
    
    Rename a new file.

-   `bog-clean-and-rename-staged-bibs`
    
    Rename new BibTeX files. If a separate BibTeX file is used for each
    citekey, this function can be used to rename all new BibTeX files.
    `bibtex-clean-entry` is used to clean the entry and autogenerate the
    key.

-   `bog-create-combined-bib`
    
    Generate a combined BibTeX file for all citekeys in buffer. This is
    useful if single-entry BibTeX files are used.

Other useful functions include

-   `bog-goto-citekey-heading-in-buffer`
-   `bog-goto-citekey-heading-in-notes`
-   `bog-citekey-tree-to-indirect-buffer`
-   `bog-refile`
-   `bog-search-notes`
-   `bog-search-notes-for-citekey`
-   `bog-sort-topic-headings-in-buffer`
-   `bog-sort-topic-headings-in-notes`

# Variables

Several variables determine where Bog looks for things.
-   `bog-notes-directory`
-   `bog-file-directory`
-   `bog-bib-directory` or `bog-bib-file`
-   `bog-stage-directory`

The variables below are important for specifying how Bog behaves.

-   `bog-citekey-format`
    
    A regular expression that defines the format used for citekeys

-   `bog-citekey-function`
    
    A function to extract a citekey from the current subtree. Use this to
    indicate whether the citekey should be taken from the heading or
    property.

-   `bog-find-citekey-bib-function`
    
    A function to find a citekey in a BibTeX file. This determines whether
    a directory of single-entry BibTeX files or a single BibTeX file is
    used.

# Keybindings

A keymap is defined for Bog under the prefix `C-c "​`. If you prefer
something else (like `C-c b`), set `bog-keymap-prefix`.

Some Bog functions are useful outside of an Org buffer (e.g.,
`bog-search-notes`). These functions are available through the
`bog-commander` interface (based of off the `projectile-commander`).
This can be bound to a global key for quick access.