/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/

/-!
# `gh run download` wrapper

`crossref-review --pr <N>` downloads the bridge artifact that mathlib4's
build emitted for the PR's most recent successful CI run, and locates the
embedded TSV inside it. Falls back (after explicit prompt) to building
Mathlib locally and re-running the dump script.
-/

namespace Crossrefs

/-- The repo we look in for build artifacts. -/
def defaultRepo : String := "leanprover-community/mathlib4"

/-- The artifact name emitted by mathlib4's build pipeline. Must match the
`artifact:` value in `build_template.yml`'s `privilege-escalation-bridge/emit`
step (`crossref-tags-bridge`). -/
def bridgeArtifactName : String := "crossref-tags-bridge"

/-- The expected TSV filename inside the bridge artifact. -/
def tsvName : String := "crossref-tags.tsv"

structure DownloadResult where
  /-- Filesystem path to the extracted TSV. -/
  tsvPath : System.FilePath
  /-- The directory we extracted into (caller cleans up). -/
  extractDir : System.FilePath
  /-- The CI run ID we sourced this artifact from. -/
  runId : String
  deriving Repr

/-- Find the most recent successful CI run for the PR via `gh pr checks`. -/
def findLatestRunId (repo : String) (pr : Nat) : IO (Option String) := do
  let output ← IO.Process.output {
    cmd := "gh"
    args := #["pr", "view", toString pr, "--repo", repo,
              "--json", "statusCheckRollup", "--jq",
              "[.statusCheckRollup[] | select(.status == \"COMPLETED\" and \
               .conclusion == \"SUCCESS\" and .workflowName == \"continuous integration\") \
               | .detailsUrl] | last"]
  }
  if output.exitCode != 0 then return none
  let trimmed := output.stdout.trimAscii.toString
  if trimmed.isEmpty then return none
  -- detailsUrl looks like ".../actions/runs/<RUN_ID>/job/<JOB_ID>"
  let parts := trimmed.splitOn "/runs/"
  match parts.tail? with
  | some (after :: _) =>
    let runId := after.splitOn "/" |>.headD ""
    if runId.isEmpty then return none else return some runId
  | _ => return none

/-- Download the bridge artifact for `runId` into `dir`. -/
def ghRunDownload (repo : String) (runId : String) (dir : System.FilePath) :
    IO (Except String Unit) := do
  let output ← IO.Process.output {
    cmd := "gh"
    args := #["run", "download", runId, "--repo", repo,
              "--name", bridgeArtifactName, "--dir", dir.toString]
  }
  if output.exitCode != 0 then
    return .error s!"gh run download exited {output.exitCode}: {output.stderr}"
  return .ok ()

/-- Fetch the TSV for PR `pr`. The caller is responsible for cleaning up
`result.extractDir`. -/
def downloadTsvForPR (pr : Nat) (repo : String := defaultRepo) :
    IO (Except String DownloadResult) := do
  let some runId ← findLatestRunId repo pr
    | return .error s!"no completed successful CI run found for PR #{pr}"
  let baseDir ← IO.appPath
  let parent := match baseDir.parent with
    | some p => p
    | none => "."
  let extractDir := parent / s!"crossref-review-{pr}-{runId}"
  IO.FS.createDirAll extractDir
  match ← ghRunDownload repo runId extractDir with
  | .error e => return .error e
  | .ok () =>
    let tsvPath := extractDir / tsvName
    if !(← tsvPath.pathExists) then
      return .error s!"artifact downloaded but {tsvName} not found in {extractDir}"
    return .ok { tsvPath, extractDir, runId }

end Crossrefs
