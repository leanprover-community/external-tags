/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/

/-!
# `gh run download` wrappers

`crossref-review --pr <N>` uses these to download two artifacts:

* the PR's most recent build (`crossref-tags-bridge` artifact) for the
  current TSV;
* the PR's merge-base build (`crossref-tags-baseline` artifact) so the
  renderer can subtract tags that already existed at the branch point
  and only highlight changes the PR actually authored.

Both artifacts have a 5-day retention. If either is missing, the caller
falls back to a simpler mode (whole-Mathlib render for a missing bridge;
no baseline filter for a missing baseline).
-/

namespace Crossrefs

/-- The repo we look in for build artifacts. -/
def defaultRepo : String := "leanprover-community/mathlib4"

/-- Artifact name emitted by `build_template.yml`'s
`privilege-escalation-bridge/emit` step. PR runs only. -/
def bridgeArtifactName : String := "crossref-tags-bridge"

/-- Artifact name uploaded plain on every build (PR + master push) so the
orchestrator can locate a baseline TSV for the PR's merge-base. -/
def baselineArtifactName : String := "crossref-tags-baseline"

/-- The TSV filename inside both artifacts. -/
def tsvName : String := "crossref-tags.tsv"

/-- The set of workflow names that emit our artifacts. Filtering by name
avoids matching unrelated successful checks (e.g. `Lint and suggest`)
whose runs don't carry the artifact. Same-repo PRs run as `continuous
integration`; fork PRs run as `continuous integration (mathlib forks)`. -/
def candidateWorkflowNames : List String :=
  ["continuous integration", "continuous integration (mathlib forks)"]

/-- Result of `downloadTsvForPR`. -/
structure DownloadResult where
  /-- Filesystem path to the extracted TSV. -/
  tsvPath : System.FilePath
  /-- The directory we extracted into. Caller is responsible for cleanup. -/
  extractDir : System.FilePath
  /-- The CI run ID we sourced this artifact from. -/
  runId : String
  deriving Repr

private def extractRunIdFromUrl (url : String) : Option String :=
  -- detailsUrl looks like "…/actions/runs/<RUN_ID>/job/<JOB_ID>"
  let parts := url.splitOn "/runs/"
  match parts.tail? with
  | some (after :: _) =>
    let runId := after.splitOn "/" |>.headD ""
    if runId.isEmpty then none else some runId
  | _ => none

/-- Find the most recent successful CI run for `pr` whose `workflowName`
is one of `candidateWorkflowNames`. -/
def findLatestRunId (repo : String) (pr : Nat) : IO (Option String) := do
  let workflowsOr := "(" ++ String.intercalate " or " (candidateWorkflowNames.map fun w =>
    s!".workflowName == \"{w}\"") ++ ")"
  let jq := s!"[.statusCheckRollup[] | select(.status == \"COMPLETED\" and \
    .conclusion == \"SUCCESS\" and {workflowsOr}) | .detailsUrl] | last"
  let output ← IO.Process.output {
    cmd := "gh"
    args := #["pr", "view", toString pr, "--repo", repo,
              "--json", "statusCheckRollup", "--jq", jq]
  }
  if output.exitCode != 0 then return none
  let trimmed := output.stdout.trimAscii.toString
  if trimmed.isEmpty then return none
  return extractRunIdFromUrl trimmed

/-- Find the most recent successful master CI run whose head SHA matches
`mergeBaseSha`, i.e. the run for the commit the PR branched off. -/
def findMergeBaseRunId (repo : String) (mergeBaseSha : String) : IO (Option String) := do
  for workflow in candidateWorkflowNames do
    let output ← IO.Process.output {
      cmd := "gh"
      args := #["run", "list", "--repo", repo, "--branch", "master",
                "--workflow", workflow, "--status", "success",
                "--commit", mergeBaseSha,
                "--limit", "1",
                "--json", "databaseId", "--jq", ".[0].databaseId // empty"]
    }
    if output.exitCode == 0 then
      let trimmed := output.stdout.trimAscii.toString
      if !trimmed.isEmpty then return some trimmed
  return none

/-- Look up the PR's merge-base SHA via the GitHub `compare` API. No git
checkout required. -/
def fetchMergeBaseSha (repo : String) (baseRef : String) (headSha : String) :
    IO (Option String) := do
  let output ← IO.Process.output {
    cmd := "gh"
    args := #["api", s!"repos/{repo}/compare/{baseRef}...{headSha}",
              "--jq", ".merge_base_commit.sha // empty"]
  }
  if output.exitCode != 0 then return none
  let trimmed := output.stdout.trimAscii.toString
  return if trimmed.isEmpty then none else some trimmed

/-- Download a named artifact for `runId` into `dir`. -/
def ghRunDownload (repo : String) (runId : String) (artifact : String)
    (dir : System.FilePath) : IO (Except String Unit) := do
  let output ← IO.Process.output {
    cmd := "gh"
    args := #["run", "download", runId, "--repo", repo,
              "--name", artifact, "--dir", dir.toString]
  }
  if output.exitCode != 0 then
    return .error s!"gh run download exited {output.exitCode}: {output.stderr}"
  return .ok ()

/-- Fetch the PR's most recent bridge artifact. Caller cleans up
`result.extractDir`. -/
def downloadTsvForPR (pr : Nat) (repo : String := defaultRepo) :
    IO (Except String DownloadResult) := do
  let some runId ← findLatestRunId repo pr
    | return .error s!"no completed successful CI run found for PR #{pr}"
  let tmpRoot : System.FilePath := (← IO.getEnv "TMPDIR").getD "/tmp"
  let extractDir := tmpRoot / s!"crossref-review-{pr}-{runId}"
  IO.FS.createDirAll extractDir
  match ← ghRunDownload repo runId bridgeArtifactName extractDir with
  | .error e => return .error e
  | .ok () =>
    let tsvPath := extractDir / tsvName
    if !(← tsvPath.pathExists) then
      return .error s!"artifact downloaded but {tsvName} not found in {extractDir}"
    return .ok { tsvPath, extractDir, runId }

/-- Try to download the baseline TSV for the PR's merge-base commit.
Returns `none` (and logs to stderr) if any step fails — the caller should
proceed without a baseline filter rather than aborting. -/
def downloadBaselineForPR (pr : Nat) (extractDir : System.FilePath)
    (repo : String := defaultRepo) : IO (Option System.FilePath) := do
  -- Get the PR's base ref and head SHA.
  let info ← IO.Process.output {
    cmd := "gh"
    args := #["pr", "view", toString pr, "--repo", repo,
              "--json", "baseRefName,headRefOid", "--jq",
              "[.baseRefName, .headRefOid] | @tsv"]
  }
  if info.exitCode != 0 then
    IO.eprintln s!"baseline: gh pr view failed: {info.stderr}"
    return none
  let parts := info.stdout.trimAscii.toString.splitOn "\t"
  let (baseRef, headSha) := match parts with
    | [b, h] => (b, h)
    | _      => ("", "")
  if baseRef.isEmpty || headSha.isEmpty then
    IO.eprintln "baseline: could not parse PR base/head"
    return none
  let some mb ← fetchMergeBaseSha repo baseRef headSha
    | IO.eprintln "baseline: merge-base lookup failed"; return none
  let some runId ← findMergeBaseRunId repo mb
    | IO.eprintln s!"baseline: no successful CI run found for merge-base {mb} \
        (likely expired; artifacts have 5-day retention)"; return none
  let baselineDir := extractDir / "baseline"
  IO.FS.createDirAll baselineDir
  match ← ghRunDownload repo runId baselineArtifactName baselineDir with
  | .error e =>
    IO.eprintln s!"baseline: download failed: {e}"; return none
  | .ok () =>
    let path := baselineDir / tsvName
    if !(← path.pathExists) then
      IO.eprintln s!"baseline: artifact downloaded but {tsvName} missing"; return none
    return some path

end Crossrefs
