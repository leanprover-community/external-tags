/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
import Crossrefs.Snippet

/-!
# `crossref-snippet` CLI

```sh
crossref-snippet <database> <tag> [<tag>...]
```

Fetches a one-line `(title, description)` for each tag from the upstream
database. One TSV record per tag to stdout:

```
<tag>\t<title>\t<description>
<tag>\tERROR\tmissing
<tag>\tERROR\tnetwork: <reason>
```

Exit codes: 0 = all resolved, 2 = at least one missing, 3 = at least one
network error, 64 = bad usage.
-/

open Crossrefs

def emit (tag : String) : SnippetOutcome → IO Unit
  | .ok title desc  => IO.println s!"{tag}\t{title}\t{desc}"
  | .missing        => IO.println s!"{tag}\tERROR\tmissing"
  | .network reason => IO.println s!"{tag}\tERROR\tnetwork: {reason}"

def usage : IO Unit := do
  IO.eprintln "Usage: crossref-snippet <database> <tag> [<tag>...]"
  IO.eprintln "  <database>: wikidata | stacks | kerodon"

def main (args : List String) : IO UInt32 := do
  match args with
  | dbStr :: tag :: rest =>
    let some db := Database.ofName? dbStr
      | usage; return 64
    let results ← fetchMany db (tag :: rest)
    let mut sawMissing := false
    let mut sawNetwork := false
    for (t, r) in results do
      emit t r
      match r with
      | .missing    => sawMissing := true
      | .network _  => sawNetwork := true
      | _           => pure ()
    if sawMissing then return 2
    if sawNetwork then return 3
    return 0
  | _ => usage; return 64
