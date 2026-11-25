When running Emacs UI commands via `emacsclient` (opening files, dired, etc.), always target the non-agent window using:

```elisp
(with-selected-window
  (seq-find (lambda (w)
              (not (string-match-p "eca" (buffer-name (window-buffer w)))))
            (window-list-1 nil 'nomini 'visible))
  ...)
```

(Agent buffers are named like `<eca-chat[Fredi]:2:0>`, with no `*`. Search visible frames, because under `emacsclient -e` the selected frame can be the daemon's hidden `F1`.)

## Always show the file you're working on

Whenever you read, investigate, cite or edit a specific file (including one a subagent changes in a worktree, or the key file behind a finding you're presenting), show it in the non-agent window with the exact lines you're looking at or changing selected:

```sh
emacsclient -s "${ECA_EMACS_SERVER:-server}" -e \
  "(eca-show-region \"FILE\" START END \"NOTE\" \"${ECA_SHOW_ID:-main}\")"
```

Replace `FILE` with the absolute path, `START`/`END` with 1-based line numbers, and `NOTE` with 1–3 plain sentences on exactly what you're doing with that code and why (e.g. `"Reading the import catch block: this is where PurchaseOrderImportFailed would fire."`). Always pass a NOTE. `eca-show-region` is autoloaded from `eca-show.el`; `M-x eca-show-clear` dismisses all agents at once.

**Quoting:** the outer double-quotes delimit the elisp string passed to `emacsclient -e`. Escape any literal `"` inside FILE or NOTE as `\"`. Line numbers are integers — no quoting needed.

### Subagents

Each agent gets its own window, colour and note, keyed by `ECA_SHOW_ID` (default `main` = you). When you spawn a subagent that reads or edits files, put this in its task brief, with a unique ID per agent (e.g. `dev-1`, `review`):

> Whenever you read or edit a file, first run:
> ```sh
> emacsclient -s "${ECA_EMACS_SERVER:-server}" -e \
>   "(eca-show-region \"FILE\" START END \"NOTE\" \"<id>\")"
> ```
> (every time your focus moves). When you finish, run:
> ```sh
> emacsclient -s "${ECA_EMACS_SERVER:-server}" -e "(eca-show-release \"<id>\")"
> ```

If a subagent ends without releasing, run the release call yourself with its ID. Up to 4 agent windows open at once; beyond that the least recently used is reused. Re-run it whenever focus moves, to a new file or to a different part of the same file, so the region always matches the code under discussion. After an edit, re-run it with the edited lines. Skip it only for scratch files under `/tmp` and bulk greps.
