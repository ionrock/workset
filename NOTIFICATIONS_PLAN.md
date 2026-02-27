# Notifications Plan

Goal: detect when an agent running inside a workset vterm needs user input or is done, and notify via Emacs-native UI (modeline/message) before OS-native alerts.

1. Define UX and configuration
- Add defcustoms for regex patterns indicating "needs input" and "done" (agent-specific defaults).
- Add defcustoms for notification methods: modeline-only vs modeline + `message`/`display-warning`.
- Store buffer-local state per vterm: `working`, `needs-input`, `done`, `idle`.

2. Detect agent state from vterm output
- Hook detection into `workset-vterm-create` when a vterm buffer is created.
- Use `vterm-output-filter-functions` (or a process filter) to scan output.
- Maintain a small rolling output window to handle split lines/patterns.
- Update buffer-local state on matches; debounce noisy transitions.

3. Notify via modeline and Emacs-native cues
- Add a modeline segment for vterm buffers showing current state with faces.
- Trigger a one-shot `message`/`display-warning` on transition to `needs-input` or `done`.
- Clear `needs-input` when user focuses the buffer or sends input.

4. Tests and documentation
- Unit-test matching and state transitions with pure helper functions.
- Document configuration and usage in `README.md`.

Notes
- Keep OS-native notifications out of scope for now; add later if needed.
- Ensure notifications are opt-in or easy to disable.
