/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
import Crossrefs.Record
import Crossrefs.Render
import Crossrefs.Diff

/-!
# `crossref-render` CLI

```sh
crossref-render --tsv <path> [--diff <range>] [--out <path>]
```

Consumes the dump TSV produced by mathlib4's `scripts/dump_crossref_tags.lean`,
optionally filters to records whose source module is in the changed-file set
of a git diff range, fetches snippets, and writes the Markdown bot comment.

If `--out` is omitted, writes to stdout.

If `--diff` is omitted, renders all records (useful for offline inspection of
the full set, but normally CI passes a diff range).

Exit codes: 0 = nothing to report, 1 = comment written and at least one tag
is `missing`, 2 = comment written, all tags resolve. Other non-zero = error.
-/

open Crossrefs

structure Args where
  tsv           : Option System.FilePath := none
  diff          : Option String := none
  changedFiles  : Option System.FilePath := none
  out           : Option System.FilePath := none

def parseArgs (argv : List String) : IO (Option Args) := do
  let mut out : Args := {}
  let mut i := 0
  let argv := argv.toArray
  while i < argv.size do
    let a := argv[i]!
    if a == "--tsv" then
      if i + 1 ≥ argv.size then IO.eprintln "--tsv expects a value"; return none
      out := { out with tsv := some argv[i + 1]! }
      i := i + 2
    else if a == "--diff" then
      if i + 1 ≥ argv.size then IO.eprintln "--diff expects a value"; return none
      out := { out with diff := some argv[i + 1]! }
      i := i + 2
    else if a == "--changed-files" then
      if i + 1 ≥ argv.size then IO.eprintln "--changed-files expects a value"; return none
      out := { out with changedFiles := some argv[i + 1]! }
      i := i + 2
    else if a == "--out" then
      if i + 1 ≥ argv.size then IO.eprintln "--out expects a value"; return none
      out := { out with out := some argv[i + 1]! }
      i := i + 2
    else
      IO.eprintln s!"unknown argument: {a}"; return none
  return some out

def usage : IO Unit := do
  IO.eprintln "Usage: crossref-render --tsv <path> [--diff <range>] \
    [--changed-files <path>] [--out <path>]"
  IO.eprintln "  --diff requires being inside a git checkout of the repo."
  IO.eprintln "  --changed-files reads one path per line; no git required."

def loadChangedFiles (path : System.FilePath) : IO (Std.HashSet String) := do
  let text ← IO.FS.readFile path
  let mut s : Std.HashSet String := ∅
  for line in text.splitOn "\n" do
    let trimmed := line.trimAscii.toString
    if !trimmed.isEmpty then s := s.insert trimmed
  return s

def filterByDiff? (records : Array Record) (args : Args) :
    IO (Array Record) := do
  match args.changedFiles, args.diff with
  | some path, _ =>
    let changed ← loadChangedFiles path
    return records.filter fun r => changed.contains r.module
  | none, some range =>
    let changed ← gitChangedFiles range
    return records.filter fun r => changed.contains r.module
  | none, none => return records

/-- Run `fetchMany` for each database, then zip the outcomes back onto the
records in the original order. -/
def gatherOutcomes (records : Array Record) :
    IO (Array (Record × SnippetOutcome)) := do
  -- Group tags by database, deduplicating.
  let mut byDb : Std.HashMap (String) (Array String) := ∅
  for r in records do
    let key := r.database.name
    byDb := byDb.insert key ((byDb.getD key #[]).push r.tag)
  -- Fetch each database's tags.
  let mut results : Std.HashMap (String × String) SnippetOutcome := ∅
  for (dbName, tags) in byDb.toList do
    let some db := Database.ofName? dbName | continue
    let uniq := tags.toList.eraseDups
    for (tag, outcome) in (← fetchMany db uniq) do
      results := results.insert (dbName, tag) outcome
  return records.map fun r =>
    let key := (r.database.name, r.tag)
    let outcome := results.getD key (.network "no result")
    (r, outcome)

def main (argv : List String) : IO UInt32 := do
  let some args ← parseArgs argv
    | usage; return 64
  let some tsv := args.tsv
    | usage; return 64
  let text ← IO.FS.readFile tsv
  let parsed := Record.parseTsv text
  let mut malformed : Array String := #[]
  let mut records : Array Record := #[]
  for entry in parsed do
    match entry with
    | .inl line => malformed := malformed.push line
    | .inr r    => records := records.push r
  if !malformed.isEmpty then
    IO.eprintln s!"warning: ignored {malformed.size} malformed TSV row(s)"
  let filtered ← filterByDiff? records args
  if filtered.isEmpty then
    IO.eprintln "no cross-reference tags in scope; nothing to comment"
    return 0
  let withOutcomes ← gatherOutcomes filtered
  let some body := renderComment withOutcomes
    | return 0
  match args.out with
  | none => IO.println body
  | some path => IO.FS.writeFile path body
  if anyMissing withOutcomes then return 1 else return 2
