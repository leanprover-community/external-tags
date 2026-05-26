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

/-- Escape a string for safe inclusion inside a Markdown table cell. We
replace `|` with `\|`, newlines with `<br>`, and backticks with a fancy
unicode tick. The output is meant to render verbatim — no upstream content
should be interpreted as Markdown syntax. -/
def mdTableEscape (s : String) : String :=
  s.replace "\\" "\\\\"
   |>.replace "|" "\\|"
   |>.replace "`" "ˋ"
   |>.replace "\r\n" " "
   |>.replace "\n" " "
   |>.replace "\r" " "

/-! ## Comment header / footer -/

/-- A magic marker we include in every comment so the workflow that updates
the comment can find its own previous post. Must be exact-match unique. -/
def commentMarker : String := "<!-- external-tags:crossref-review -->"

def commentHeader : String :=
  s!"{commentMarker}\n## Cross-reference review\n"

def commentFooterNote : String :=
  "<sub>Posted by [external-tags](https://github.com/leanprover-community/external-tags). \
  Snippets are fetched from upstream live; if a tag is reported missing, check that the \
  identifier exists on the source site.</sub>"

/-! ## Per-database rendering -/

/-- Render one `(record, outcome)` pair as a Markdown table row. -/
def renderRow (r : Record) (outcome : SnippetOutcome) : String :=
  let url := s!"{databaseURL r.database}{r.tag}"
  let tagCell := s!"[`{mdTableEscape r.tag}`]({url})"
  let declCell := s!"`{mdTableEscape r.declName}`"
  let (titleCell, descCell) := match outcome with
    | .ok title desc => (mdTableEscape title, mdTableEscape desc)
    | .missing        => ("**missing**", "tag not found upstream")
    | .network reason => ("_network error_", mdTableEscape reason)
  let commentCell := if r.comment.isEmpty then "" else mdTableEscape r.comment
  s!"| {tagCell} | {titleCell} | {descCell} | {declCell} | {commentCell} |"

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
