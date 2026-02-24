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

## Installation

### straight.el

```elisp
(straight-use-package
 '(workset :type git :host github :repo "ionrock/workset"))
```

### use-package (with straight)

```elisp
(use-package workset
  :straight (workset :type git :host github :repo "ionrock/workset")
  :commands (workset workset-create workset-open workset-vterm workset-list workset-remove))
```

### Manual

```elisp
(add-to-list 'load-path "/path/to/workset")
(require 'workset)
```

## Configuration

```elisp
(setq workset-base-directory (expand-file-name "~/.workset"))
(setq workset-project-backend 'auto)
(setq workset-copy-patterns
      '(".env" ".envrc" ".env.local"
        "docker-compose.yml" "docker-compose.yaml"
        ".tool-versions" ".node-version" ".python-version" ".ruby-version"))
(setq workset-vterm-buffer-name-format "*workset: %r/%t<%n>*")
(setq workset-branch-prefix "eric/")
(setq workset-start-point "HEAD")
```

## Usage

- `M-x workset` opens the transient menu
- `M-x workset-create` creates a new workset
- `M-x workset-open` switches to an existing workset
- `M-x workset-vterm` opens an additional terminal
- `M-x workset-list` lists active worksets
- `M-x workset-remove` removes a workset

### Keybindings

Workset installs a global prefix map at `C-c w` by default.

- `C-c w w` → `workset` (transient menu)
- `C-c w c` → `workset-create`
- `C-c w o` → `workset-open`
- `C-c w t` → `workset-vterm`
- `C-c w l` → `workset-list`
- `C-c w r` → `workset-remove`

To change the prefix:

```elisp
(setq workset-keymap-prefix "C-c w")
```

## Development

### Prerequisites

- Emacs 29.1+
- Eask

Install Eask:

```bash
curl -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/install.sh | sh
```

### Running tests

```bash
make test
```

Or directly with Emacs:

```bash
emacs --batch -L . -l test/workset-test.el -f ert-run-tests-batch-and-exit
```

### Other targets

```bash
make compile
make lint
make checkdoc
```
