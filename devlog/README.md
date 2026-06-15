# Devlog

A running log of LLM-assisted and human coding sessions on the WX compiler. Intended to give future contributors and LLMs fast context on where the project has been and where it's going.

## File naming

```
devlog/{YYYY-MM-DD}-{short-name}.md
```

Example: `devlog/2026-06-15-tir-ast-refactor.md`

## Index

All sessions are listed in [`index.md`](index.md), grouped by date in reverse chronological order.

## Writing a session log

Create a new file using the naming convention above and add a link to `index.md`. There is no required structure — write whatever is actually useful. Suggested sections:

- **Summary** — what was worked on and why
- **Changes** — files and components touched
- **Key findings** — non-obvious things learned about the codebase
- **Decisions** — choices made and their rationale
- **Context for future sessions** — facts that would help an LLM or contributor hit the ground running
- **Open questions** — unresolved things worth revisiting

Only include sections that have something worth saying. If there were no key findings, skip that section. Keep it dense — these logs exist to transfer knowledge, not to document every keystroke.
