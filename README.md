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
(setq workset-notify-enabled t)
(setq workset-notify-method 'modeline-and-message)
(setq workset-notify-input-patterns
      '("\\bawaiting your input\\b"
        "\\bneed your input\\b"
        "\\bplease respond\\b"))
(setq workset-notify-idle-seconds 10)
(setq workset-notify-debounce-seconds 0.5)
```

## Sound Notifications

workset can play macOS system sounds when an agent finishes or needs
input.  Sound notifications are only supported on macOS (they are
silently skipped on other platforms).

### Enabling sound

Set `workset-notify-method` to `modeline-and-sound` to play a sound
alongside the modeline indicator whenever the buffer is not currently
visible:

```elisp
(setq workset-notify-method 'modeline-and-sound)
```

Other sound-enabled methods:

- `sound` — sound only, no modeline update.
- `modeline-message-and-sound` — modeline, echo-area message, and sound.

### Available sounds

Sounds are played from `/System/Library/Sounds/`.  The default values
are `"Glass"` (agent done) and `"Sosumi"` (agent needs input).
Common alternatives include `"Ping"`, `"Tink"`, `"Pop"`, `"Bottle"`,
`"Blow"`, `"Frog"`, and `"Hero"`.

Customize via:

```elisp
(setq workset-notify-sound-done "Glass")
(setq workset-notify-sound-needs-input "Sosumi")
```

### Throttle

Rapid repeated state changes will not trigger a new sound until the
throttle interval has elapsed:

```elisp
(setq workset-notify-sound-throttle-seconds 5)
```

Set to `0` to disable throttling.

### Agent presets

Use `M-x workset-notify-use-preset` to load detection patterns tuned
for a specific AI agent.  Available presets: `claude-code`, `cursor`,
`aider`.

```elisp
;; Load the Claude Code preset on startup
(workset-notify-use-preset 'claude-code)
```

Preset patterns are merged with (not replace) any existing patterns,
so custom patterns are preserved.

### Full sound configuration example

```elisp
(setq workset-notify-enabled t)
(setq workset-notify-method 'modeline-and-sound)
(setq workset-notify-sound-enabled t)
(setq workset-notify-sound-done "Glass")
(setq workset-notify-sound-needs-input "Sosumi")
(setq workset-notify-sound-throttle-seconds 5)
(setq workset-notify-idle-seconds 10)
(setq workset-notify-debounce-seconds 0.5)
;; Optional: load agent-specific patterns
(workset-notify-use-preset 'claude-code)
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
