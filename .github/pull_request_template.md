## What and why

<!-- What changes, and the reason for it. Link an issue if there is one. -->

## Verification

<!-- How you checked it: tests added or run, pages looked at. -->

- [ ] `dune build @install @check` and `dune test` pass
- [ ] Gated suite (`scripts/test-gated.sh`) passes, if SQL, stores or handlers changed
- [ ] `dune build @fmt` is clean (ocamlformat 0.28.1)
- [ ] New migration (if any) is additive, and `db/schema.sql` is regenerated
