# Sprint C Task Board

Move items from TODO -> IN PROGRESS -> DONE during standups.

## TODO

- [ ] Return explicit pass/fail output with clear exit status.
- [ ] Decide `train_linear_adam` scope: implement or de-scope for MS3.
- [ ] Decide `load_csv_samples` scope: implement or de-scope for MS3.
- [ ] Improve `export-dot` output label readability and add tests.
- [ ] Add README examples for `check-grad` and `export-dot` render flow.
- [ ] Generate Bisect coverage report and record baseline metrics.
- [ ] Add targeted tests for lowest-coverage modules.

## IN PROGRESS

- [ ]

## DONE

- [x] Parse optional `eps` and `abs_tol` args in `check-grad` command.
- [x] Wire `check-grad` CLI to `Grad_check.check_expr_gradient`.
- [x] Forward mode (`Forward_ad.gradient_expr`) implemented.
- [x] Numerical gradient checker (`Grad_check.check_expr_gradient`) implemented.
- [x] Graph export core (`Graphviz_export.expr_to_dot`) implemented.
- [x] Baseline test suites for forward-mode and grad-check are present.

## Risks

- Risk: command behavior diverges from docs.
  - Mitigation: treat README commands as acceptance tests.
- Risk: unstable numerical checks for extreme values.
  - Mitigation: expose defaults and allow CLI override for `eps/abs_tol`.
- Risk: last-minute optimizer scope creep.
  - Mitigation: make Adam/CSV go/no-go decision by mid-sprint.

## Daily Gate

- `dune fmt`
- `dune test`
- `dune runtest --instrument-with bisect_ppx` (or equivalent coverage command)
