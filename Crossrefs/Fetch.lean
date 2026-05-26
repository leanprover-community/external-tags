/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
import Lean.Data.Json

/-!
# Cross-reference snippet types and single-tag fetch

`Database`, the outcome type, and HTTP / parsing primitives. `Crossrefs.Snippet`
builds the batched + cached fetcher on top.
-/

namespace Crossrefs

open Lean

/-- The three supported cross-reference databases.

When adding a new case, also update:
- `databaseURL`, `databaseLabel`, `Database.name`, `Database.ofName?`,
  the per-database branch in `fetchOne`, and `Database.gerbyBase?` below;
- the parser, `syntax (name := ...)`, `registerBuiltinAttribute`, and
  `#X_tags` trace command in mathlib4's `Mathlib/Tactic/CrossRefAttribute.lean`;
- the per-database branch in `Crossrefs.Snippet.fetchMany`.

The `ofName?_name` roundtrip theorem below catches drift between `name` and
`ofName?` at compile time. -/
inductive Database where
  | kerodon
  | stacks
  | wikidata
  deriving BEq, Hashable, Repr, Inhabited

/-- Short machine-readable name (`"kerodon"`, `"stacks"`, `"wikidata"`).
Stable to round-trip through JSON, CLI args, and cache filenames. -/
def Database.name : Database → String
  | .kerodon  => "kerodon"
  | .stacks   => "stacks"
  | .wikidata => "wikidata"

/-- Parse the short name back into a `Database`. -/
def Database.ofName? : String → Option Database
  | "kerodon"  => some .kerodon
  | "stacks"   => some .stacks
  | "wikidata" => some .wikidata
  | _          => none

/-- `Database.name` and `Database.ofName?` roundtrip. -/
theorem Database.ofName?_name (d : Database) : Database.ofName? d.name = some d := by
  cases d <;> rfl

/-- The base URL for an external database's tag pages. Always ends with `/`. -/
def databaseURL : Database → String
  | .kerodon  => "https://kerodon.net/tag/"
  | .stacks   => "https://stacks.math.columbia.edu/tag/"
  | .wikidata => "https://www.wikidata.org/wiki/"

/-- The display label used in PR comments and trace output. -/
def databaseLabel : Database → String
  | .kerodon  => "Kerodon Tag"
  | .stacks   => "Stacks Tag"
  | .wikidata => "Wikidata"

/-- The outcome of trying to fetch a snippet. -/
inductive SnippetOutcome where
  /-- Upstream returned a `(title, description)`. Either may be empty. -/
  | ok (title : String) (description : String)
  /-- The tag was authoritatively missing upstream. -/
  | missing
  /-- A transient problem (network, parse, …). -/
  | network (reason : String)
  deriving Repr, Inhabited

/-- The `User-Agent` curl sends. Wikidata's API will throttle anonymous clients
without one, so we identify ourselves. -/
def userAgent : String :=
  "external-tags-bot/1 (https://github.com/leanprover-community/external-tags)"

/-- Make a GET request and return `(http-status, body)`. Status is `0` if curl
itself failed. We append the HTTP status to the body via `-w '\n%{http_code}'`
and recover it from the final line — that avoids juggling a temp file. -/
def fetchUrl (url : String) : IO (Nat × String) := do
  let output ← IO.Process.output {
    cmd := "curl"
    args := #["-sSL", "--max-time", "10", "-A", userAgent,
              "-w", "\n%{http_code}", url]
  }
  if output.exitCode != 0 then return (0, "")
  let parts := output.stdout.splitOn "\n"
  match parts.reverse with
  | last :: rest =>
    let body := "\n".intercalate rest.reverse
    let status := last.trimAscii.toString.toNat?.getD 0
    return (status, body)
  | [] => return (0, "")

/-- Replace runs of whitespace by a single space and strip leading/trailing. -/
def flattenWhitespace (s : String) : String :=
  let go : Char → (String × Bool) → (String × Bool) := fun c (acc, prevSpace) =>
    let isWs := c == ' ' || c == '\t' || c == '\n' || c == '\r'
    if isWs then
      if prevSpace || acc.isEmpty then (acc, true)
      else (acc.push ' ', true)
    else
      (acc.push c, false)
  let (out, _) := s.toList.foldl (fun st c => go c st) ("", false)
  out.trimAscii.toString

/-- Best-effort HTML→text. We treat `<x…>` as a tag only when `x` is a letter,
`/`, or `!`, so a literal `<` inside LaTeX (`0 < 1`) is preserved. -/
def stripHtml (html : String) : String :=
  let chars := html.toList
  let rec go : List Char → Bool → String → String
    | [], _, acc => acc
    | '<' :: rest, false, acc =>
      match rest with
      | c :: _ =>
        if c.isAlpha || c == '/' || c == '!' then go rest true acc
        else go rest false (acc.push '<')
      | [] => acc.push '<'
    | '>' :: rest, true, acc => go rest false acc
    | _ :: rest, true,  acc => go rest true  acc
    | c :: rest, false, acc => go rest false (acc.push c)
  let raw := go chars false ""
  let decoded := raw
    |>.replace "&nbsp;" " "
    |>.replace "&amp;" "&"
    |>.replace "&lt;" "<"
    |>.replace "&gt;" ">"
    |>.replace "&quot;" "\""
    |>.replace "&#39;" "'"
  flattenWhitespace decoded

/-- Walk a path of object keys in a `Json` value, returning the leaf as a
string if every step succeeds and the leaf is a string. -/
def jsonStrPath? (j : Json) (path : List String) : Option String :=
  let rec go (cur : Json) : List String → Option String
    | [] => cur.getStr?.toOption
    | k :: rest =>
      match cur.getObjVal? k with
      | .ok next => go next rest
      | .error _ => none
  go j path

/-! ## Wikidata -/

/-- Fetch a single QID from Wikidata. -/
def fetchWikidataOne (qid : String) : IO SnippetOutcome := do
  let url := s!"https://www.wikidata.org/w/api.php?action=wbgetentities&ids={qid}\
              &languages=en&props=labels%7Cdescriptions&format=json"
  let (status, body) ← fetchUrl url
  if status != 200 then return .network s!"wikidata HTTP {status}"
  match Json.parse body with
  | .error e => return .network s!"wikidata json: {e}"
  | .ok json =>
    match json.getObjVal? "error" with
    | .ok err =>
      let code := jsonStrPath? err ["code"] |>.getD ""
      let info := jsonStrPath? err ["info"] |>.getD code
      if code == "no-such-entity" then return .missing
      else return .network s!"wikidata {code}: {info}"
    | _ =>
      match json.getObjVal? "entities" with
      | .error _ => return .network "wikidata: no `entities` field"
      | .ok entities =>
        match entities.getObjVal? qid with
        | .error _ => return .missing
        | .ok ent =>
          match ent.getObjVal? "missing" with
          | .ok _ => return .missing
          | _ =>
            let label := jsonStrPath? ent ["labels", "en", "value"] |>.getD ""
            let desc  := jsonStrPath? ent ["descriptions", "en", "value"] |>.getD ""
            return .ok (flattenWhitespace label) (flattenWhitespace desc)

/-! ## Stacks / Kerodon (Gerby) -/

/-- The base URL for a Gerby-style database, or `none` for Wikidata. -/
def Database.gerbyBase? : Database → Option String
  | .stacks   => some "https://stacks.math.columbia.edu"
  | .kerodon  => some "https://kerodon.net"
  | .wikidata => none

/-- Return the substring of `s` after the first occurrence of `needle`,
or `none` if `needle` is absent. -/
def afterFirst? (s needle : String) : Option String :=
  let parts := s.splitOn needle
  match parts with
  | _ :: rest@(_ :: _) => some (needle.intercalate rest)
  | _ => none

/-- Take everything up to (but not including) the first occurrence of `c`. -/
def takeUntilChar (s : String) (c : Char) : String :=
  String.ofList (s.toList.takeWhile (· != c))

/-- Pull the environment type (`Lemma`, `Proposition`, …) and reference number
from the `/content/statement` HTML. Both Stacks and Kerodon wrap each tag in
`<article class="env-{TYPE}" id="{TAG}">` and lead with
`<a ...>Lemma <span data-tag="...">14.32.3</span>.</a>`. -/
def parseGerbyTitle (html : String) : String :=
  let envType :=
    match afterFirst? html "class=\"env-" with
    | none => ""
    | some rest => takeUntilChar rest '"'
  let reference :=
    match afterFirst? html "data-tag=\"" with
    | none => ""
    | some afterAttr =>
      match afterFirst? afterAttr ">" with
      | none => ""
      | some inside => takeUntilChar inside '<'
  let cap := envType.capitalize
  flattenWhitespace (if reference.isEmpty then cap else s!"{cap} {reference}")

/-- Fetch one Stacks/Kerodon tag. Gerby returns HTTP 200 even for missing
tags; we detect via the body text. -/
def fetchGerbyOne (db : Database) (tag : String) : IO SnippetOutcome := do
  let some base := db.gerbyBase? | return .network s!"{db.name}: no Gerby base"
  let url := s!"{base}/data/tag/{tag}/content/statement"
  let (status, body) ← fetchUrl url
  if status != 200 then return .network s!"{db.name} HTTP {status}"
  if body.trimAscii.toString == "This tag does not exist." then return .missing
  let title := parseGerbyTitle body
  let snippet := stripHtml body
  if title.isEmpty && snippet.isEmpty then
    return .network s!"{db.name}: could not parse statement"
  return .ok title snippet

/-- Fetch one snippet from the appropriate upstream database. -/
def fetchOne (db : Database) (tag : String) : IO SnippetOutcome :=
  match db with
  | .wikidata => fetchWikidataOne tag
  | _ => fetchGerbyOne db tag

end Crossrefs
