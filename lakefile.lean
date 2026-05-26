import Lake
open Lake DSL

package «external-tags» where
  -- No package-level options yet.

lean_lib Crossrefs

@[default_target]
lean_exe «crossref-snippet» where
  root := `Cli.Snippet
  supportInterpreter := true

lean_exe «crossref-render» where
  root := `Cli.Render
  supportInterpreter := true

lean_exe «crossref-review» where
  root := `Cli.Review
  supportInterpreter := true
