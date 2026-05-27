/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
import Crossrefs.PRArtifact

/-!
# `crossref-review` CLI

Local convenience wrapper: "grab the diff of a PR and spit out a nicely
formatted page locally."

```sh
crossref-review --pr <N> [--repo <owner/repo>] [--out <path>]
```

Flow:
1. Find the most recent successful mathlib4 CI run for the PR.
2. `gh run download` the bridge artifact (containing the dump TSV).
3. `gh pr diff --name-only` for the changed-files list.
4. Run `crossref-render` on the TSV, filtered by that list. Markdown out.
5. Open the result with `xdg-open` / `open`.

CI artifacts expire after 5 days. There's no fallback to building Mathlib
locally yet — if you need that, run the dump script in your own checkout
and pass the TSV to `crossref-render` directly.

Without `--out`, writes to a temp file and opens it. With `--out`, writes
to the given path and does not open anything.
-/

open Crossrefs

structure Args where
  pr : Option Nat := none
  repo : String := defaultRepo
  out : Option System.FilePath := none

def parseArgs (argv : List String) : IO (Option Args) := do
  let mut out : Args := {}
  let mut i := 0
  let argv := argv.toArray
  while i < argv.size do
    let a := argv[i]!
    if a == "--pr" then
      if i + 1 ≥ argv.size then IO.eprintln "--pr expects a value"; return none
      let some n := argv[i + 1]!.toNat?
        | IO.eprintln s!"--pr expects a number, got {argv[i + 1]!}"; return none
      out := { out with pr := some n }
      i := i + 2
    else if a == "--repo" then
      if i + 1 ≥ argv.size then IO.eprintln "--repo expects a value"; return none
      out := { out with repo := argv[i + 1]! }
      i := i + 2
    else if a == "--out" then
      if i + 1 ≥ argv.size then IO.eprintln "--out expects a value"; return none
      out := { out with out := some argv[i + 1]! }
      i := i + 2
    else
      IO.eprintln s!"unknown argument: {a}"; return none
  return some out

def usage : IO Unit := do
  IO.eprintln "Usage: crossref-review --pr <N> [--repo <owner/repo>] [--out <path>]"

def openInBrowser (path : System.FilePath) : IO Unit := do
  let opener := if System.Platform.isOSX then "open" else "xdg-open"
  let _ ← IO.Process.spawn { cmd := opener, args := #[path.toString] }

def main (argv : List String) : IO UInt32 := do
  let some args ← parseArgs argv
    | usage; return 64
  let some pr := args.pr
    | usage; return 64
  IO.eprintln s!"Looking for the most recent successful CI run for PR #{pr} on {args.repo}…"
  match ← downloadTsvForPR pr args.repo with
  | .error e =>
    IO.eprintln s!"{e}"
    IO.eprintln "(CI artifacts expire after 5 days; if the build is older, run \
      scripts/dump_crossref_tags.lean in a local Mathlib checkout and pass the \
      TSV to `crossref-render --tsv …` directly.)"
    return 1
  | .ok result =>
    IO.eprintln s!"Got bridge artifact from run {result.runId}; TSV at {result.tsvPath}"
    -- Filter the (whole-Mathlib) TSV down to what the PR actually touched,
    -- using `gh pr diff --name-only` so we don't need a local mathlib checkout.
    let diffProc ← IO.Process.output {
      cmd := "gh"
      args := #["pr", "diff", toString pr, "--repo", args.repo, "--name-only"]
    }
    if diffProc.exitCode != 0 then
      IO.eprintln s!"gh pr diff failed:\n{diffProc.stderr}"
      return 1
    let changedFiles := result.extractDir / "changed.txt"
    let lean := diffProc.stdout.splitOn "\n" |>.filter fun l => l.endsWith ".lean"
    IO.FS.writeFile changedFiles (String.intercalate "\n" lean)
    let outPath := args.out.getD (result.extractDir / "crossref-review.md")
    let renderExe := (← IO.appPath).parent.getD "." / "crossref-render"
    let renderArgs : Array String :=
      #["--tsv", result.tsvPath.toString,
        "--changed-files", changedFiles.toString,
        "--out", outPath.toString]
    let proc ← IO.Process.spawn { cmd := renderExe.toString, args := renderArgs }
    let exit ← proc.wait
    if exit != 0 && exit != 1 && exit != 2 then
      IO.eprintln s!"crossref-render exited {exit}"
      return 1
    IO.eprintln s!"Wrote {outPath}"
    if args.out.isNone then openInBrowser outPath
    return 0
