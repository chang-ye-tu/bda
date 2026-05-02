#!/usr/bin/env python3
"""Build BDA lecture units to PDF (cross-platform).

Each unit goes through knit -> xelatex -> (bibtex if needed) -> xelatex (x2).

Usage:
    python build.py                       # build all 7 units
    python build.py 01 03 06              # build only unit01, unit03, unit06
    python build.py unit01 unit03         # full names also accepted
    python build.py --quiet               # suppress per-step progress
    python build.py --skip-pkg-check      # skip the R-package availability probe
    python build.py --pkg-check           # only run the probe, don't build

Required CLI tools (must be on PATH):
    Rscript, xelatex, bibtex
Optional:
    pdfinfo                                (used only to report page counts)

Required R packages (probed at launch unless --skip-pkg-check is given):
    brulee, cluster, clustMixType, dplyr, ggplot2, glmnet, ISLR2, kernlab,
    knitr, modeldata, parsnip, patchwork, probably, ranger, recipes,
    rpart, rpart.plot, rsample, Rtsne, themis, tibble, tidymodels, tidyr,
    tidyverse, torch, tune, uwot, xgboost, yardstick

Optional R packages (mentioned only in prose / eval=FALSE chunks; the build
succeeds without them, but the student cannot run those interactive lines):
    ClusterR, dbscan, finetune, mclust, paran, randomForest, rrcov, skimr

The probe aborts on missing required packages and warns on missing optional
packages. The 'torch' package additionally needs its C++ backend; after
installing the R package run once:
    Rscript -e 'torch::install_torch()'
"""

from __future__ import annotations

import argparse
import re
import shutil
import subprocess
import sys
from pathlib import Path

UNITS = ["unit01", "unit02", "unit03", "unit04", "unit05", "unit06", "unit07"]
UNITS_WITH_BIB = {"unit06", "unit07"}
ROOT = Path(__file__).parent.resolve()

# R packages loaded by chunks that actually execute during the build.
# Missing any of these aborts the build.
R_PACKAGES_REQUIRED = [
    "brulee", "cluster", "clustMixType", "dplyr", "ggplot2",
    "glmnet", "ISLR2", "kernlab", "knitr", "modeldata",
    "parsnip", "patchwork", "probably", "ranger", "recipes",
    "rpart", "rpart.plot", "rsample", "Rtsne", "themis",
    "tibble", "tidymodels", "tidyr", "tidyverse", "torch",
    "tune", "uwot", "xgboost", "yardstick",
]

# R packages mentioned only in prose or in eval=FALSE chunks (alternatives
# the student may want to explore interactively). Their absence does NOT
# break the build; --pkg-check warns but does not abort.
R_PACKAGES_OPTIONAL = [
    "ClusterR", "dbscan", "finetune", "mclust", "paran",
    "randomForest", "rrcov", "skimr",
]


def normalize_unit(arg: str) -> str:
    """Accept '1', '01', 'unit1', 'unit01'; return 'unit01'."""
    s = arg.lower()
    if s.startswith("unit"):
        s = s[4:]
    try:
        return f"unit{int(s):02d}"
    except ValueError:
        raise SystemExit(f"ERROR: cannot parse unit identifier: {arg!r}")


def check_tool(name: str) -> None:
    if shutil.which(name) is None:
        sys.exit(f"ERROR: {name!r} not found on PATH.")


def _probe_packages(pkgs: list) -> list:
    """Return the subset of pkgs not installed (per requireNamespace)."""
    pkg_vec = ", ".join(f'"{p}"' for p in pkgs)
    rscript = (
        f"pkgs <- c({pkg_vec}); "
        "missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), "
        "quietly = TRUE)]; "
        "if (length(missing)) cat(missing, sep='\\n')"
    )
    rc, out = run(["Rscript", "-e", rscript], cwd=ROOT)
    if rc != 0:
        sys.exit(f"ERROR: R package probe failed (rc={rc}):\n{out}")
    return [line.strip() for line in out.splitlines() if line.strip()]


def check_r_packages() -> None:
    """Probe REQUIRED (abort if missing) then OPTIONAL (warn only)."""
    missing_req = _probe_packages(R_PACKAGES_REQUIRED)
    missing_opt = _probe_packages(R_PACKAGES_OPTIONAL)

    if missing_opt:
        print("WARNING: optional R package(s) not installed (prose mentions "
              "alternatives that won't run interactively, but the build "
              "still succeeds):", file=sys.stderr)
        for p in missing_opt:
            print(f"  - {p}", file=sys.stderr)
        quoted = ", ".join(f'"{p}"' for p in missing_opt)
        print(f"  Install (optional):\n    Rscript -e "
              f"'install.packages(c({quoted}))'\n", file=sys.stderr)

    if not missing_req:
        return
    print("ERROR: missing required R package(s):", file=sys.stderr)
    for p in missing_req:
        print(f"  - {p}", file=sys.stderr)
    quoted = ", ".join(f'"{p}"' for p in missing_req)
    print(
        f"\nInstall with:\n  Rscript -e 'install.packages(c({quoted}))'",
        file=sys.stderr,
    )
    if "torch" in missing_req:
        print(
            "\nNote: 'torch' additionally needs the LibTorch C++ backend.\n"
            "After installing the R package, run once:\n"
            "  Rscript -e 'torch::install_torch()'",
            file=sys.stderr,
        )
    sys.exit(1)


def run(cmd: list, cwd: Path) -> tuple:
    """Run cmd in cwd. Return (returncode, captured_stdout_stderr_text)."""
    try:
        proc = subprocess.run(
            cmd, cwd=cwd, capture_output=True, text=True, check=False
        )
    except FileNotFoundError as e:
        return 127, str(e)
    return proc.returncode, (proc.stdout or "") + (proc.stderr or "")


def get_pages(pdf: Path):
    """Return page count via pdfinfo if available, else None."""
    if shutil.which("pdfinfo") is None or not pdf.exists():
        return None
    rc, out = run(["pdfinfo", str(pdf)], cwd=pdf.parent)
    if rc != 0:
        return None
    for line in out.splitlines():
        if line.startswith("Pages:"):
            try:
                return int(line.split()[1])
            except (IndexError, ValueError):
                return None
    return None


def count_pattern(log: Path, pattern: str) -> int:
    if not log.exists():
        return 0
    pat = re.compile(pattern)
    n = 0
    with log.open(encoding="utf-8", errors="replace") as f:
        for line in f:
            if pat.search(line):
                n += 1
    return n


def show_log_tail(log: Path, n: int = 30) -> None:
    if not log.exists():
        return
    with log.open(encoding="utf-8", errors="replace") as f:
        lines = f.readlines()
    print(f"--- last {min(n, len(lines))} lines of {log.name} ---", file=sys.stderr)
    for line in lines[-n:]:
        print(line.rstrip(), file=sys.stderr)


def build_unit(unit: str, quiet: bool) -> dict:
    rnw = ROOT / f"{unit}.Rnw"
    tex = ROOT / f"{unit}.tex"
    pdf = ROOT / f"{unit}.pdf"
    log = ROOT / f"{unit}.log"
    has_bib = unit in UNITS_WITH_BIB
    n_steps = 4 if has_bib else 3

    if not rnw.exists():
        return {"unit": unit, "ok": False, "step": "missing"}

    if not quiet:
        print(f"=== {unit} ===")

    def step(idx: int, label: str, cmd: list, fatal: bool = True):
        if not quiet:
            print(f"  [{idx}/{n_steps}] {label} ... ", end="", flush=True)
        rc, out = run(cmd, cwd=ROOT)
        if rc != 0 and fatal:
            if not quiet:
                print("FAILED")
            else:
                print(f"{unit}: {label} FAILED", file=sys.stderr)
            if out.strip():
                print(out[-2000:], file=sys.stderr)
            return rc
        if not quiet:
            print("ok")
        return rc

    # 1. knit
    rc = step(1, "knit", ["Rscript", "-e", f"knitr::knit('{rnw.name}', quiet=TRUE)"])
    if rc != 0:
        return {"unit": unit, "ok": False, "step": "knit"}

    # 2. xelatex pass 1
    step(2, "xelatex (1)",
         ["xelatex", "-shell-escape", "-interaction=nonstopmode", tex.name],
         fatal=False)

    # 3. bibtex (+ extra xelatex)
    if has_bib:
        step(3, "bibtex",
             ["bibtex", unit], fatal=False)
        # bibtex writes .bbl; one more xelatex now folds it in
        run(["xelatex", "-shell-escape", "-interaction=nonstopmode", tex.name],
            cwd=ROOT)

    # final xelatex
    step(n_steps, "xelatex (final)",
         ["xelatex", "-shell-escape", "-interaction=nonstopmode", tex.name],
         fatal=False)

    errors = count_pattern(log, r"^!|^Error:")
    undef  = count_pattern(log, r"Reference .* undefined|Citation .* undefined")
    pages  = get_pages(pdf)
    ok     = pdf.exists() and errors == 0 and undef == 0

    if not ok and not quiet:
        show_log_tail(log)

    return {"unit": unit, "ok": ok, "errors": errors, "undef": undef, "pages": pages}


def main(argv: list) -> int:
    parser = argparse.ArgumentParser(
        description=__doc__.split("\n", 1)[0],
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__.split("\n", 1)[1],
    )
    parser.add_argument(
        "units", nargs="*",
        help="Units to build (e.g., 01 03 unit06). Default: all 7.",
    )
    parser.add_argument(
        "-q", "--quiet", action="store_true",
        help="Suppress per-step progress; only print the summary table.",
    )
    parser.add_argument(
        "--skip-pkg-check", action="store_true",
        help="Skip the R-package availability probe.",
    )
    parser.add_argument(
        "--pkg-check", action="store_true",
        help="Run only the R-package probe and exit (no build).",
    )
    args = parser.parse_args(argv)

    check_tool("Rscript")
    if not args.skip_pkg_check or args.pkg_check:
        check_r_packages()
    if args.pkg_check:
        print("All required R packages are available.")
        return 0
    for tool in ("xelatex", "bibtex"):
        check_tool(tool)

    targets = [normalize_unit(u) for u in args.units] if args.units else list(UNITS)
    bad = [u for u in targets if u not in UNITS]
    if bad:
        sys.exit(f"ERROR: unknown unit(s): {bad}. Valid: {UNITS}")

    results = [build_unit(u, args.quiet) for u in targets]

    print("\n" + "=" * 56)
    print(f"{'Unit':<8} {'Status':<8} {'Pages':<7} {'Errors':<8} {'Undef':<6}")
    print("-" * 56)
    for r in results:
        status = "[OK]" if r["ok"] else "[FAIL]"
        pages  = "?" if r.get("pages") is None else str(r["pages"])
        errs   = "?" if r.get("errors") is None else str(r["errors"])
        undef  = "?" if r.get("undef")  is None else str(r["undef"])
        print(f"{r['unit']:<8} {status:<8} {pages:<7} {errs:<8} {undef:<6}")

    failed = [r for r in results if not r["ok"]]
    if failed:
        print(f"\n{len(failed)} of {len(results)} unit(s) failed.")
        return 1
    print(f"\nAll {len(results)} unit(s) built successfully.")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
