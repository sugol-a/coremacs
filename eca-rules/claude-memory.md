# Claude Code Memory

At the start of each session, check whether a Claude Code memory index exists for the current project.

The memory directory path is derived by encoding the workspace root's absolute path: replace every `/` with `-` and prepend `-`, then look at:

```
~/.claude/projects/<encoded-path>/memory/MEMORY.md
```

For example, the workspace `/home/al/Documents/source/blongo` → `~/.claude/projects/-home-al-Documents-source-blongo/memory/MEMORY.md`.

If `MEMORY.md` exists, read it silently. It is an index of memory files, one line per entry in the format `- [Title](file.md) — hook`. Do not read the individual files yet.

During the session, when a task or question looks relevant to an index entry (based on its title and hook), read that memory file from the same directory before proceeding.

## Writing and updating memories

You are allowed to save and update memories for the current project. Use your judgment — save things that would be useful to know in a future session, such as project conventions, user preferences, recurring patterns, or important decisions.

**To save a new memory:**
1. Write a new `.md` file in the memory directory with this frontmatter:
```
---
name: <short_snake_case_name>
description: one-line summary used to judge relevance
metadata:
  type: feedback | user | project | reference
---
The fact.
**Why:** …
**How to apply:** …
```
2. Append a line to `MEMORY.md`: `- [Title](file.md) — hook`

**To update an existing memory:** edit the file in place. Update the `MEMORY.md` index line if the hook changes.

Create the memory directory if it doesn't exist yet.
