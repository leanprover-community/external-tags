/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
import Crossrefs.Fetch

/-!
# Cross-reference record (one row of the dump TSV)

Mathlib's `scripts/dump_crossref_tags.lean` writes one record per tagged
declaration to a TSV with columns:

```
<database>\t<tag>\t<declName>\t<module>\t<comment>
```

This module parses that TSV back into structured records.
-/

namespace Crossrefs

/-- One tagged declaration as recorded in the dump TSV. -/
structure Record where
  database : Database
  tag      : String
  declName : String
  /-- Source module, e.g. `Mathlib/Algebra/Foo.lean`. -/
  module   : String
  /-- Optional comment supplied with the `@[…]` attribute, possibly empty. -/
  comment  : String
  deriving Repr, Inhabited

/-- Parse one TSV row. Returns `none` for malformed rows (wrong column count
or unknown database name). The caller decides whether to skip or hard-fail. -/
def Record.parseRow? (line : String) : Option Record := do
  let parts := line.splitOn "\t"
  match parts with
  | [dbStr, tag, declName, module, comment] => do
    let db ← Database.ofName? dbStr
    return { database := db, tag, declName, module, comment }
  | _ => none

/-- Parse a whole dump TSV. Blank lines are skipped; malformed rows are
returned as `Sum.inl line` so the caller can decide what to do with them. -/
def Record.parseTsv (text : String) :
    Array (Sum String Record) := Id.run do
  let mut out : Array (Sum String Record) := #[]
  for line in text.splitOn "\n" do
    if line.trimAscii.toString.isEmpty then continue
    match parseRow? line with
    | some r => out := out.push (.inr r)
    | none   => out := out.push (.inl line)
  return out

end Crossrefs
