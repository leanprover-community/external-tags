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
crossref-render --tsv <path>
                [--baseline-tsv <path>]
                [--diff <range> | --changed-files <path>]
                [--strict]
                [--out <path>]
```

Consumes the dump TSV produced by mathlib4's `scripts/dump_crossref_tags.lean`,
optionally filters by which files the PR touched, optionally subtracts a
baseline TSV (so we don't re-render tags that already existed on master at
the PR's branch point), fetches snippets, and writes the Markdown bot
comment.

If `--out` is omitted, writes to stdout.

If neither `--diff` nor `--changed-files` is supplied, all records are
considered (useful for offline inspection of the full set).

`--baseline-tsv` takes the TSV produced by the dump script for the PR's
merge-base commit. Rows in the current TSV that match a row in the
baseline verbatim (`db\ttag\tdeclName\tmodule\tcomment`) are dropped:
they're not changes introduced by this PR. This keeps a maintenance PR
that touches a thousand files but doesn't change any tag attribute from
flooding the comment with snippets it didn't author.

`--strict` makes malformed TSV rows fatal (default: warn and skip).
The CI orchestrator passes `--strict` so producer bugs or malicious
artifacts can't silently hide tags from the bot comment.

Exit codes: 0 = nothing to report, 1 = comment written and at least one
tag is `missing`, 2 = comment written, all tags resolve. Other = error.
-/

open Crossrefs

structure Args where
  tsv          : Option System.FilePath := none
  baselineTsv  : Option System.FilePath := none
  diff         : Option String := none
  changedFiles : Option System.FilePath := none
  out          : Option System.FilePath := none
  strict       : Bool := false

def parseArgs (argv : List String) : IO (Option Args) := do
  let mut out : Args := {}
  let mut i := 0
  let argv := argv.toArray
  while i < argv.size do
    let a := argv[i]!
    let needsArg : IO (Option String) := do
      if i + 1 ≥ argv.size then
        IO.eprintln s!"{a} expects a value"
        return none
      return some argv[i + 1]!
    if a == "--tsv" then
      let some v ← needsArg | return none
      out := { out with tsv := some v }; i := i + 2
    else if a == "--baseline-tsv" then
      let some v ← needsArg | return none
      out := { out with baselineTsv := some v }; i := i + 2
    else if a == "--diff" then
      let some v ← needsArg | return none
      out := { out with diff := some v }; i := i + 2
    else if a == "--changed-files" then
      let some v ← needsArg | return none
      out := { out with changedFiles := some v }; i := i + 2
    else if a == "--out" then
      let some v ← needsArg | return none
      out := { out with out := some v }; i := i + 2
    else if a == "--strict" then
      out := { out with strict := true }; i := i + 1
    else
      IO.eprintln s!"unknown argument: {a}"; return none
  return some out

def usage : IO Unit := do
  IO.eprintln "Usage: crossref-render --tsv <path> [--baseline-tsv <path>] \
    [--diff <range> | --changed-files <path>] [--strict] [--out <path>]"

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

/-- Build a `(db, tag, declName, module, comment)` set from a TSV file.
Used to subtract baseline rows from the current PR's rows. -/
def loadRowKeys (path : System.FilePath) : IO (Std.HashSet String) := do
  let text ← IO.FS.readFile path
  let mut s : Std.HashSet String := ∅
  for line in text.splitOn "\n" do
    let trimmed := line.trimAscii.toString
    if !trimmed.isEmpty then s := s.insert trimmed
  return s

def filterByBaseline? (records : Array Record) (args : Args) :
    IO (Array Record) := do
  match args.baselineTsv with
  | none => return records
  | some path =>
    let baseline ← loadRowKeys path
    return records.filter fun r => !baseline.contains r.toTsvKey

/-- Run `fetchMany` for each database, then zip the outcomes back onto the
records in the original order. -/
def gatherOutcomes (records : Array Record) :
    IO (Array (Record × SnippetOutcome)) := do
  -- Group tags by database, deduplicating.
  let mut byDb : Std.HashMap String (Array String) := ∅
  for r in records do
    byDb := byDb.insert r.database.name ((byDb.getD r.database.name #[]).push r.tag)
  let mut results : Std.HashMap (String × String) SnippetOutcome := ∅
  for (dbName, tags) in byDb.toList do
    let some db := Database.ofName? dbName | continue
    let uniq := tags.toList.eraseDups
    for (tag, outcome) in (← fetchMany db uniq) do
      results := results.insert (dbName, tag) outcome
  return records.map fun r =>
    let key := (r.database.name, r.tag)
    (r, results.getD key (.network "no result"))

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
    if args.strict then
      IO.eprintln s!"error (--strict): {malformed.size} malformed TSV row(s)"
      for l in malformed.take 5 do IO.eprintln s!"  {l}"
      return 65
    else
      IO.eprintln s!"warning: ignored {malformed.size} malformed TSV row(s)"
  let filtered ← filterByDiff? records args
  let filtered ← filterByBaseline? filtered args
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
