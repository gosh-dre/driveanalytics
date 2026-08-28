# Security notes — LLM app-builder pipeline

Known, currently-unmitigated risks in the generated-YAML path. These are **documented, not
yet fixed** (deferred by decision). Read before enabling this feature for untrusted users.

## 1. Prompt → YAML → `eval()` / arbitrary file read (RCE-class)

The analysis agent can emit module params that are executed or used as file paths when the
generated app runs. `build_module_catalog` (`R/gen_yaml_maker.R`) advertises every module
formal (minus `id/input/output/session`) to the LLM, so these params are offered to the model
and can appear in the YAML.

Sinks:

| Module | Param | Sink |
|---|---|---|
| `gen_distribution_plots` | `filter_condition` | `eval(rlang::parse_expr(filter_condition))` — `gen_distribution_plots-shiny.R:57`, eval in `R/gen_distribution_plots.R:37-39`. **Live arbitrary R code execution.** |
| `ppl_filter_col` | `filter_expr` | `eval(rlang::parse_expr(...))` — `R/ppl_filter_col.R:13,59`. Currently broken (evals an undefined `filter_condition` var) but still advertised. |
| `gen_location_analysis` | `shp_path` | `sf::st_read(shp_path)` — `gen_location_analysis-shiny.R:111`, `R/gen_location_analysis.R:116-117`. **Arbitrary file read.** |

Exploit path: user prompt → analysis agent → YAML param → `.method2fncall_server` builds
`param_list` from `mthd$params`, filters to `formalArgs(f_server)`, then
`do.call(f_server, valid_params)` (`picture.platform/R/app_picture_server.R:272,284`) → the
sink above. A prompt crafted to land `system(...)`/`file.remove(...)` in `filter_condition`
is code execution when the app is opened.

Recommended remediation (when approved):
- Stop advertising these params: replace the 4-name denylist in `build_module_catalog`
  (`R/gen_yaml_maker.R`, the `setdiff(params, c("id","input","output","session"))` line) with
  one that also drops `filter_condition`, `filter_expr`, `shp_path` — or switch to a per-module
  allowlist of safe params.
- Add a judge reject for these keys as defence-in-depth (`llm_yaml/judge.py`).
- Remove/replace the module `eval(rlang::parse_expr(...))` with a structured filter (column +
  operator + value); this also protects hand-authored YAML.

## 2. Free-text rendered into the app (stored XSS / template injection)

`title`, `description`, `tab_lbl` pass through the pipeline unescaped. They are rendered as
Shiny tag text (auto-escaped) at `picture.platform/R/app_picture_ui.R` / `app_picture_server.R`,
but `img` is placed into an HTML `src` attribute (`app_picture_ui.R:71`) — an attribute-context
sink that tag-text escaping does not fully cover. A payload such as
`title: <img src=x onerror=...>` reaches the YAML verbatim (confirmed in red-teaming).

Recommended remediation: sanitise/escape free-text fields in `llm_yaml/assemble.py`, and
confirm the PICTURE renderer escapes both text and attribute contexts.
