/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
import Crossrefs.Record
import Crossrefs.Snippet

/-!
# Render a PR bot comment from dump TSV + fetched snippets

`renderComment` consumes `Record`s and produces a Markdown table grouped by
database. The caller decides which records to feed it (typically: filtered
by `git diff --name-only` of the PR range).

User-provided strings (tag comments, snippet titles, snippet descriptions)
are escaped for Markdown tables so that an `|` or backtick in the upstream
data can't break out of the cell.

The privileged workflow that posts this comment must pass the Markdown to
the GitHub API as comment body, not as HTML. GitHub's Markdown rendering
already sandboxes inline HTML; we additionally refuse to interpolate raw
HTML from snippets here.
-/

namespace Crossrefs

/-! ## Escaping -/

/-- Escape a string for safe verbatim rendering inside a Markdown table
cell on GitHub. Both upstream snippet text and PR-author tag comments end
up here; we treat both as untrusted.

What's covered (and why):
- `\` doubled first, so subsequent backslash escapes aren't ambiguous;
- `|` to prevent breaking out of the table cell;
- backticks swapped for `ˋ` to prevent code-span injection;
- `&`, `<`, `>` HTML-entity-encoded so an upstream `<table>` or `<img>`
  can't restructure the bot comment (GitHub renders inline HTML, even
  though it strips `<script>` etc.);
- `*`, `_`, `[`, `]`, `(`, `)`, `#` backslash-escaped so emphasis,
  link-spoofing, and heading syntax don't fire;
- `\r\n`, `\n`, `\r` collapsed to spaces (TSV is one row per line and
  Markdown tables don't survive embedded newlines anyway).

A PR author who puts `**missing**` in a tag comment will see it as
literal `\*\*missing\*\*` in the rendered cell, and not trigger the
orchestrator's fail-the-check signal (which uses crossref-render's exit
code, not a grep of the rendered Markdown). -/
def mdTableEscape (s : String) : String :=
  s.replace "\\" "\\\\"
   |>.replace "&" "&amp;"
   |>.replace "<" "&lt;"
   |>.replace ">" "&gt;"
   |>.replace "|" "\\|"
   |>.replace "`" "ˋ"
   |>.replace "*" "\\*"
   |>.replace "_" "\\_"
   |>.replace "[" "\\["
   |>.replace "]" "\\]"
   |>.replace "(" "\\("
   |>.replace ")" "\\)"
   |>.replace "#" "\\#"
   |>.replace "\r\n" " "
   |>.replace "\n" " "
   |>.replace "\r" " "

/-! ## Comment header / footer -/

/-- A magic marker we include in every comment so the workflow that updates
the comment can find its own previous post. Must be exact-match unique. -/
def commentMarker : String := "<!-- external-tags:crossref-review -->"

/-- Top of the PR comment: marker + the H2 we want GitHub to render. -/
def commentHeader : String :=
  s!"{commentMarker}\n## Cross-reference review\n"

/-- Small grey footer line that goes at the bottom of every PR comment.
Note: the underlying check is *advisory* — the TSV is produced by a script
in mathlib4's `scripts/`, which a PR can edit, so a determined PR author
can hide tags from this comment. The orchestrator caps render time and
row counts to bound abuse, but the check shouldn't be treated as a
guarantee that every tag in the PR is upstream-resolved. -/
def commentFooterNote : String :=
  "<sub>Posted by [external-tags](https://github.com/leanprover-community/external-tags). \
  Snippets are fetched from upstream live. \
  This check is **advisory**: the dump script runs from the PR checkout, so a \
  PR can edit the producer to hide tags from this comment.</sub>"

/-! ## Per-database rendering -/

/-- Render one `(record, outcome)` pair as a Markdown table row. The tag is
percent-encoded before going into the URL so a `)` in an adversarial tag
can't close the Markdown link target early. -/
def renderRow (r : Record) (outcome : SnippetOutcome) : String :=
  let url := s!"{databaseURL r.database}{percentEncode r.tag}"
  let tagCell := s!"[`{mdTableEscape r.tag}`]({url})"
  let declCell := s!"`{mdTableEscape r.declName}`"
  let (titleCell, descCell) := match outcome with
    | .ok title desc => (mdTableEscape title, mdTableEscape desc)
    | .missing        => ("**missing**", "tag not found upstream")
    | .network reason => ("_network error_", mdTableEscape reason)
  let commentCell := if r.comment.isEmpty then "" else mdTableEscape r.comment
  s!"| {tagCell} | {titleCell} | {descCell} | {declCell} | {commentCell} |"

/-- Render one database's rows as a Markdown subsection (H3 + table).
Returns the empty string if `rows` is empty, so the caller can splice it
unconditionally. -/
def renderDatabaseSection (db : Database) (rows : Array (Record × SnippetOutcome)) :
    String := Id.run do
  if rows.isEmpty then return ""
  let mut out := s!"### {databaseLabel db}\n\n"
  out := out ++ "| Tag | Title | Description | Declaration | Comment |\n"
  out := out ++ "|---|---|---|---|---|\n"
  for (r, o) in rows do
    out := out ++ renderRow r o ++ "\n"
  out ++ "\n"

/-! ## Top-level -/

/-- Group records by database in the canonical order (Wikidata, Stacks, Kerodon). -/
def groupByDatabase (rows : Array (Record × SnippetOutcome)) :
    Array (Database × Array (Record × SnippetOutcome)) :=
  #[.wikidata, .stacks, .kerodon].map fun db =>
    (db, rows.filter fun (r, _) => r.database == db)

/-- True if at least one outcome is `missing`. The CLI uses this to decide
whether to set a non-zero exit code (which the CI workflow turns into a red
check). -/
def anyMissing (rows : Array (Record × SnippetOutcome)) : Bool :=
  rows.any fun (_, o) => match o with | .missing => true | _ => false

/-- Render the full PR comment body, or `none` if there's nothing to say
(no records — typical "no cross-references touched" case). -/
def renderComment (rows : Array (Record × SnippetOutcome)) : Option String := Id.run do
  if rows.isEmpty then return none
  let mut body := commentHeader ++ "\n"
  body := body ++ s!"This PR touches {rows.size} cross-reference \
    tag{if rows.size == 1 then "" else "s"}:\n\n"
  for (db, rs) in groupByDatabase rows do
    body := body ++ renderDatabaseSection db rs
  if anyMissing rows then
    body := body ++ "> ⚠ Some tags above are reported **missing** upstream. \
      Double-check the identifiers.\n\n"
  body := body ++ commentFooterNote ++ "\n"
  return some body

end Crossrefs
