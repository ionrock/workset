# Notifications Plan

Goal: detect when an agent running inside a workset vterm needs user input or is done, and notify via Emacs-native UI (modeline/message) before OS-native alerts.

1. Define UX and configuration
- Decide the notification surface for vterm buffers: modeline indicator always on, optional transient `message` or `display-warning`.
- Define explicit state machine: `idle` (no recent output), `working` (streaming output), `needs-input` (agent prompt), `done` (explicit completion).
- Add defcustoms:
  - Regex lists for `needs-input` and `done` patterns (seed with common agent prompts).
  - Optional regex for “working” activity (or infer from any output).
  - Methods for how/when to notify (modeline only vs modeline + messages).
  - Max rolling output length, debounce interval, and commands that clear “needs-input”.
- Decide per-buffer vs global enablement: buffer-local minor mode enabled on workset vterms by default, disable globally if needed.

2. Detect agent state from vterm output
- Hook detection into `workset-vterm-create` right after `vterm-mode` is activated.
- Use `vterm-output-filter-functions` to receive text chunks; avoid altering output (return original).
- Maintain a buffer-local rolling window of recent output:
  - Append new output.
  - Trim to `workset-notify-max-output`.
  - Use this window to match prompts that may be split across chunks.
- Run matching in priority order: `needs-input` first, then `done`, else `working` when output arrives.
- Debounce state transitions to avoid rapid flapping (e.g., only notify on a new state or after N seconds).
- Provide a small helper to force state transitions for manual testing.

3. Notify via modeline and Emacs-native cues
- Add a modeline segment (`[Input]`, `[Done]`, `[Working]`) with faces, buffer-local only.
- Make modeline segment dynamic (`:eval`) and lightweight; update via `force-mode-line-update`.
- Emit a one-shot Emacs notification on transition into `needs-input` or `done`:
  - Choose `message` or `display-warning` based on config.
  - Include buffer name in the message for quick identification.
- Clear `needs-input` when:
  - User focuses the buffer (window selection hook).
  - User types in the vterm (post-command hook, configurable command list).
- Ensure the minor mode shows a lighter so users can confirm it’s enabled.

4. Tests and documentation
- Unit-test:
  - Regex matching helpers.
  - State transition ordering (`needs-input` overrides `done`).
  - Rolling window trimming.
- Integration sanity check in vterm if possible (manual/interactive guidance).
- Document:
  - How to enable/disable notifications.
  - How to customize patterns for specific agents.
  - Example setup for minimal vs verbose notifications.

Notes
- Keep OS-native notifications out of scope for now; add later if needed.
- Ensure notifications are opt-in or easy to disable.
