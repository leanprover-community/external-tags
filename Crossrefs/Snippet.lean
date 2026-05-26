/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
import Crossrefs.Fetch
import Std.Data.HashMap

/-!
# Batched + cached snippet fetcher

`fetchMany` for one database at a time, with on-disk caching when
`CROSSREF_CACHE_DIR` is set. The CI workflow that posts the PR comment
runs this against hundreds of tags per build, so amortising HTTP cost
matters.

Wikidata's `wbgetentities` endpoint takes up to 50 IDs per call; the other
two are queried one at a time. All three share a `(database, tag) → body`
on-disk cache so re-runs (and local development) are instant once warm.
-/

namespace Crossrefs

open Lean

/-- Take elements of a list in fixed-size chunks. -/
partial def chunkList (n : Nat) (xs : List α) : List (List α) :=
  if xs.isEmpty then []
  else
    let k := n.max 1
    (xs.take k) :: chunkList n (xs.drop k)

/-! ## On-disk cache -/

/-- Locate the per-tag cache file, if `CROSSREF_CACHE_DIR` is set. -/
def cacheFile? (db : Database) (tag : String) : IO (Option System.FilePath) := do
  match ← IO.getEnv "CROSSREF_CACHE_DIR" with
  | none => return none
  | some dir =>
    let path : System.FilePath := dir
    IO.FS.createDirAll path
    return some (path / s!"{db.name}-{tag}.json")

/-- Read a cached raw response body, if one exists. -/
def cacheLoad (db : Database) (tag : String) : IO (Option String) := do
  match ← cacheFile? db tag with
  | none => return none
  | some f =>
    if ← f.pathExists then
      return some (← IO.FS.readFile f)
    else
      return none

/-- Save a raw response body to the cache. Empty body marks "known missing". -/
def cacheStore (db : Database) (tag : String) (body : String) : IO Unit := do
  match ← cacheFile? db tag with
  | none => return ()
  | some f => IO.FS.writeFile f body

/-! ## Wikidata (batched) -/

/-- Wikidata's `wbgetentities` endpoint supports up to 50 IDs per request. -/
def wikidataBatchSize : Nat := 50

/-- Pull the English label and description out of one entity payload. -/
def wikidataOutcomeOf (ent : Json) : SnippetOutcome :=
  match ent.getObjVal? "missing" with
  | .ok _ => .missing
  | _ =>
    let label := jsonStrPath? ent ["labels", "en", "value"] |>.getD ""
    let desc  := jsonStrPath? ent ["descriptions", "en", "value"] |>.getD ""
    .ok (flattenWhitespace label) (flattenWhitespace desc)

/-- Outcome of inspecting one parsed Wikidata response. -/
inductive WikidataParse where
  /-- Per-QID results for every input id. -/
  | results (rs : List (String × SnippetOutcome))
  /-- Wikidata refused the whole batch because one specific id was malformed
  (`no-such-entity`). We pull that id out, mark it missing, and retry the rest. -/
  | retryWithout (badId : String)
  /-- Transient failure (`maxlag`, `ratelimit`, …) — every id maps to `network`. -/
  | transient (reason : String)

/-- Inspect a parsed `wbgetentities` response. Wikidata returns a per-batch
top-level `error` rather than a per-id flag when any single id is malformed,
so we have to read `error.id` and retry without it. -/
def parseWikidataResponse (qids : List String) (json : Json) : WikidataParse :=
  match json.getObjVal? "error" with
  | .ok err =>
    let code := jsonStrPath? err ["code"] |>.getD ""
    let info := jsonStrPath? err ["info"] |>.getD code
    if code == "no-such-entity" then
      match jsonStrPath? err ["id"] with
      | some bad => .retryWithout bad
      | none => .results (qids.map fun q => (q, .missing))
    else
      .transient s!"wikidata {code}: {info}"
  | _ =>
    match json.getObjVal? "entities" with
    | .error _ => .transient "wikidata: no `entities` field"
    | .ok entities =>
      .results <| qids.map fun q =>
        match entities.getObjVal? q with
        | .error _ => (q, .missing)
        | .ok ent => (q, wikidataOutcomeOf ent)

/-- Fetch one batch of QIDs from the live Wikidata API. Retries with the
offending id removed whenever Wikidata refuses the whole batch over a single
malformed identifier. -/
partial def fetchWikidataBatch (qids : List String) :
    IO (List (String × SnippetOutcome)) := do
  if qids.isEmpty then return []
  let ids := "|".intercalate qids
  let url := s!"https://www.wikidata.org/w/api.php?action=wbgetentities&ids={ids}\
              &languages=en&props=labels%7Cdescriptions&format=json"
  let (status, body) ← fetchUrl url
  if status != 200 then
    return qids.map fun q => (q, .network s!"wikidata HTTP {status}")
  match Json.parse body with
  | .error e => return qids.map fun q => (q, .network s!"wikidata json: {e}")
  | .ok json =>
    match parseWikidataResponse qids json with
    | .transient r => return qids.map fun q => (q, .network r)
    | .retryWithout bad =>
      let rest := qids.filter (· != bad)
      let restResults ← fetchWikidataBatch rest
      cacheStore .wikidata bad ""
      let table : Std.HashMap String SnippetOutcome :=
        restResults.foldl (fun m (k, v) => m.insert k v) ∅
      return qids.map fun q =>
        if q == bad then (q, .missing)
        else (q, table.getD q (.network "wikidata: lost from response"))
    | .results results =>
      if let .ok entities := json.getObjVal? "entities" then
        for (q, _) in results do
          if let .ok ent := entities.getObjVal? q then
            cacheStore .wikidata q ent.compress
      for (q, r) in results do
        match r with
        | .missing => cacheStore .wikidata q ""
        | _ => pure ()
      return results

/-- Batched Wikidata fetch with on-disk cache. -/
def fetchWikidataMany (qids : List String) :
    IO (List (String × SnippetOutcome)) := do
  let mut cached : Std.HashMap String SnippetOutcome := ∅
  let mut todo : Array String := #[]
  for q in qids do
    match ← cacheLoad .wikidata q with
    | some body =>
      if body.isEmpty then
        cached := cached.insert q .missing
      else
        match Json.parse body with
        | .ok ent => cached := cached.insert q (wikidataOutcomeOf ent)
        | .error _ => todo := todo.push q
    | none => todo := todo.push q
  let mut fresh : Std.HashMap String SnippetOutcome := ∅
  for batch in chunkList wikidataBatchSize todo.toList do
    for (q, r) in (← fetchWikidataBatch batch) do
      fresh := fresh.insert q r
  return qids.map fun q =>
    if let some r := cached[q]? then (q, r)
    else if let some r := fresh[q]? then (q, r)
    else (q, .network "missing from response")

/-! ## Stacks / Kerodon (Gerby), with cache -/

/-- Fetch one Gerby tag, hitting the on-disk cache when available. -/
def fetchGerbyCached (db : Database) (tag : String) : IO SnippetOutcome := do
  if let some cached ← cacheLoad db tag then
    if cached.isEmpty then return .missing
    let title := parseGerbyTitle cached
    let snippet := stripHtml cached
    return .ok title snippet
  let some base := db.gerbyBase? | return .network s!"{db.name}: no Gerby base"
  let url := s!"{base}/data/tag/{tag}/content/statement"
  let (status, body) ← fetchUrl url
  if status != 200 then
    return .network s!"{db.name} HTTP {status}"
  if body.trimAscii.toString == "This tag does not exist." then
    cacheStore db tag ""
    return .missing
  cacheStore db tag body
  let title := parseGerbyTitle body
  let snippet := stripHtml body
  if title.isEmpty && snippet.isEmpty then
    return .network s!"{db.name}: could not parse statement"
  return .ok title snippet

def fetchGerbyAll (db : Database) (tags : List String) :
    IO (List (String × SnippetOutcome)) :=
  tags.mapM fun t => return (t, ← fetchGerbyCached db t)

/-! ## Top-level dispatch -/

/-- Batched fetch for one database. -/
def fetchMany (db : Database) (tags : List String) :
    IO (List (String × SnippetOutcome)) :=
  match db with
  | .wikidata => fetchWikidataMany tags
  | .stacks   => fetchGerbyAll db tags
  | .kerodon  => fetchGerbyAll db tags

end Crossrefs
