# external-tags

Tooling for reviewing cross-reference tags (`@[stacks ...]`, `@[kerodon ...]`,
`@[wikidata ...]`) that PR authors add to [mathlib4](https://github.com/leanprover-community/mathlib4).

This repository lives outside Mathlib so it can iterate freely without
adding review burden. Mathlib's contribution to the pipeline is just
`scripts/dump_crossref_tags.lean` (which walks `Mathlib.CrossRef.tagExt`
and emits a TSV) plus the build_template.yml emit step and the
`crossref_review.yml` workflow_run shim — ~240 LOC total.

## What this provides

Three Lake executables:

| Exe | Purpose |
|---|---|
| `crossref-snippet <db> <tag>…` | Fetch a one-line `(title, description)` for each tag from the upstream database. TSV out, network in. |
| `crossref-render --tsv <path> [--diff <range>] [--out <path>]` | Consume the dump TSV, optionally filter by `git diff --name-only`, fetch snippets, emit the Markdown PR comment. Used by mathlib-ci's PR-comment orchestrator. |
| `crossref-review --pr <N>` | Local convenience wrapper: fetch the mathlib4 CI artifact for PR `N` (5-day retention), filter by the PR's diff, render to Markdown, open the result. |

## Quick start

```sh
lake build
lake exe crossref-snippet wikidata Q42 Q43 Q9999999999
lake exe crossref-render --tsv example-tags.tsv --out /tmp/review.md
lake exe crossref-review --pr 12345
```

## Pipeline

```
[mathlib4 CI build]
       │
       ▼ scripts/dump_crossref_tags.lean
   crossref-tags.tsv  (≈55 KB, 491 rows today)
       │
       ▼ leanprover-community/privilege-escalation-bridge/emit
   build artifact
       │
       ▼ workflow_run triggers crossref_review.yml in mathlib4
       ▼ privilege-escalation-bridge/consume
   mathlib-ci orchestrator (post-comment.sh)
       │
       ▼ uses external-tags@<PINNED_SHA> from the workflow's actions/cache
       ▼ gh pr diff --name-only → filter TSV
       ▼ lake exe crossref-render --tsv … --changed-files … --out comment.md
       ▼ update_PR_comment.sh
   PR comment posted ✓
```

The privileged workflow_run job runs *only* code from `mathlib-ci@<pinned SHA>`,
which in turn runs *only* code from this repo at a pinned SHA. The TSV from
the build is treated as untrusted data: never executed, parsed as text only,
all user-controllable strings (tag comments, snippet titles, snippet
descriptions) are escaped before being interpolated into the PR comment.

## Cache

Set `CROSSREF_CACHE_DIR` to a directory to memoise upstream responses per
`(database, tag)`. Useful for CI (precomputed cache shipped between runs)
and for local repeated invocations.

```sh
export CROSSREF_CACHE_DIR=~/.cache/crossref
lake exe crossref-snippet wikidata Q42  # first call hits the network
lake exe crossref-snippet wikidata Q42  # second call is free
```

## Adding a database

`Database` is an `inductive` in `Crossrefs/Fetch.lean`. To add (say) `nlab`:

1. Add a constructor in `Database`.
2. Update `Database.name`, `Database.ofName?`, `databaseURL`, `databaseLabel`,
   and `Database.gerbyBase?` in `Crossrefs/Fetch.lean`.
3. Implement the per-database branch in `Crossrefs.Snippet.fetchMany`
   (single-API or batched, with `Snippet`'s on-disk cache).
4. In mathlib4: add the parser, attribute registration, and `#nlab_tags`
   trace command in `Mathlib/Tactic/CrossRefAttribute.lean`.

The `Database.ofName?_name` roundtrip theorem in `Fetch.lean` will fail to
compile until step 2 is consistent with itself.

## Layout

```
Crossrefs/
  Fetch.lean       Database enum, SnippetOutcome, HTTP / HTML / JSON helpers
  Snippet.lean     Batched + cached fetcher (fetchMany)
  Record.lean      TSV row type + parser
  Diff.lean        git diff --name-only wrapper (for crossref-render --diff)
  Render.lean      Markdown PR comment renderer with table escaping
  PRArtifact.lean  gh run download wrapper (for crossref-review --pr)
Cli/
  Snippet.lean     crossref-snippet entry point
  Render.lean      crossref-render entry point
  Review.lean      crossref-review entry point (local convenience tool)
```

## History

Most of `Fetch.lean` and `Snippet.lean` is lifted verbatim from three stacked
PRs against mathlib4 that originally added this tooling in-tree:

- https://github.com/leanprover-community/mathlib4/pull/39662 — standalone script
- https://github.com/leanprover-community/mathlib4/pull/39664 — info-view widget (dropped)
- https://github.com/leanprover-community/mathlib4/pull/39666 — CI workflow

Following maintainer discussion, the tooling was extracted here to keep
~1,300 LOC of review surface out of Mathlib. The mathlib4 surface is now
the dump script (~80 LOC including the README entry), the `post_steps`
emit in `build_template.yml` (~30 LOC), and the `crossref_review.yml`
workflow_run shim (~120 LOC including the caching scaffolding).
