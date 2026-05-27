/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
import Std.Data.HashSet

/-!
# Git diff helpers

Backs `crossref-render --diff <range>` when invoked inside a git checkout
of the target repo. The CI orchestrator and `crossref-review --pr N` use
`gh pr diff --name-only` + `--changed-files` instead (no checkout
needed), so this is only reached from interactive local use.
-/

namespace Crossrefs

/-- Run `git diff --name-only <range>` in `cwd` and return the set of changed
paths. A failed `git diff` is a hard error: turning it into an empty set
would silently pass CI on PRs whose base hasn't been fetched. -/
def gitChangedFiles (range : String) (cwd : Option System.FilePath := none) :
    IO (Std.HashSet String) := do
  let output ← IO.Process.output {
    cmd := "git"
    args := #["diff", "--name-only", range]
    cwd  := cwd
  }
  if output.exitCode != 0 then
    throw <| .userError s!"`git diff --name-only {range}` failed (exit \
      {output.exitCode}). Make sure the base ref is fetched.\n\
      Stderr:\n{output.stderr}"
  let mut s : Std.HashSet String := ∅
  for line in output.stdout.splitOn "\n" do
    let trimmed := line.trimAscii.toString
    if !trimmed.isEmpty then s := s.insert trimmed
  return s

end Crossrefs
