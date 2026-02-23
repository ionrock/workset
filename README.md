# workset

`workset` is a workflow tool inspired by `superset.sh` for running AI-assisted development in parallel.

It coordinates:

- Git worktrees (one task/branch per workspace)
- `vterm` sessions (one terminal per workspace)
- Agent-friendly layouts (fast context switching across tasks)

## Why workset

AI coding agents work best when each task has isolated state:

- Separate branch and filesystem (`git worktree`)
- Dedicated terminal process (`vterm`)
- Repeatable task lifecycle (create, jump in, clean up)

`workset` standardizes that loop so you can spin up multiple agent tasks without shell clutter or branch collisions.

## Core model

Each workset maps to:

- A named worktree directory
- A matching branch
- A matching `vterm` session

Think of it as: **task = branch + worktree + terminal**.

## Typical workflow

1. Create a workset for a task.
2. `workset` creates (or links) a git worktree and branch.
3. `workset` opens or focuses a `vterm` for that task.
4. Run your AI agent inside that terminal.
5. Commit/push when done.
6. Remove/archive the workset when merged.

## Example usage

```bash
# create a new task workspace
workset create fix-login-bug

# jump to an existing workspace terminal/worktree
workset open fix-login-bug

# list active worksets
workset list

# remove a completed workspace
workset remove fix-login-bug
```

## Recommended setup

- Emacs with `vterm` installed and working
- Git 2.5+ (worktree support)
- A repository where you run `workset`

## Use cases

- Running multiple AI agents on different tickets in parallel
- Keeping experiments isolated without stashing
- Fast branch switching without losing terminal context

## Status

Initial project scaffolding. This repository currently documents the workflow and intent for `workset`.

