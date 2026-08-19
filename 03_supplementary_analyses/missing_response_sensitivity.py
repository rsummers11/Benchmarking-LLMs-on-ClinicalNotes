"""Sensitivity analysis for how missing/unparseable judge responses are handled. Recomputes
Table 2 (per-model, micro-averaged over all comorbidities) three ways:
  - "drop"       : current published method -- rows with an unparseable response excluded.
  - "impute"     : missing responses treated as an explicit negative prediction.
  - "worst_case" : missing responses treated as wrong regardless of the true answer.

95% CIs: exact Clopper-Pearson for precision/recall, percentile bootstrap (R=2000) for F1.
"""
import numpy as np
import pandas as pd
import pyarrow.parquet as pq
from scipy.stats import beta

FINAL_DATA = "your_path_here/final_data"
RESULTS_DIR = "your_path_here/results"
SEED = 42
N_BOOT = 2000

COLS = ["model", "model_family", "model_name", "diagnosis", "gt", "GPT-o3-mini_binary"]


def clopper_pearson(successes, n, alpha=0.05):
    if n == 0:
        return np.nan, np.nan
    lower = beta.ppf(alpha / 2, successes, n - successes + 1) if successes > 0 else 0.0
    upper = beta.ppf(1 - alpha / 2, successes + 1, n - successes) if successes < n else 1.0
    return lower, upper


def f1_from_pr(precision, recall):
    if precision + recall == 0:
        return 0.0
    return 2 * precision * recall / (precision + recall)


def compute_metrics(gt_true, pred_true, pred_missing):
    """gt_true / pred_true / pred_missing are boolean arrays over the SAME set of rows."""
    n = len(gt_true)

    # drop: exclude missing rows entirely before computing anything
    keep = ~pred_missing
    gt_d, pred_d = gt_true[keep], pred_true[keep]
    recall_denom_d = gt_d.sum()
    recall_num_d = (gt_d & pred_d).sum()
    prec_denom_d = pred_d.sum()
    prec_num_d = (pred_d & gt_d).sum()

    # impute: missing treated as an explicit negative prediction, nothing dropped
    recall_denom_i = gt_true.sum()
    recall_num_i = (gt_true & pred_true).sum()  # identical to recall_num_d by construction
    prec_denom_i = pred_true.sum()  # identical to prec_denom_d: missing never in pred_true
    prec_num_i = (pred_true & gt_true).sum()

    # worst_case: missing treated as wrong regardless of truth -- FN when gt=True (same as
    # impute), FP when gt=False (added to precision's denominator, not its numerator)
    added_fp = (pred_missing & ~gt_true).sum()
    recall_denom_w = recall_denom_i  # same as impute -- worst_case doesn't touch the gt=True side differently
    recall_num_w = recall_num_i
    prec_denom_w = prec_denom_i + added_fp
    prec_num_w = prec_num_i  # added rows are FPs, never TPs

    recall_d = recall_num_d / recall_denom_d if recall_denom_d else np.nan
    precision_d = prec_num_d / prec_denom_d if prec_denom_d else np.nan
    recall_i = recall_num_i / recall_denom_i if recall_denom_i else np.nan
    precision_i = prec_num_i / prec_denom_i if prec_denom_i else np.nan
    recall_w = recall_num_w / recall_denom_w if recall_denom_w else np.nan
    precision_w = prec_num_w / prec_denom_w if prec_denom_w else np.nan

    return dict(
        n=n, n_missing=int(pred_missing.sum()),
        recall_denom_drop=int(recall_denom_d), recall_num_drop=int(recall_num_d),
        precision_denom_drop=int(prec_denom_d), precision_num_drop=int(prec_num_d),
        recall_drop=recall_d, precision_drop=precision_d, f1_drop=f1_from_pr(precision_d, recall_d),
        recall_denom_impute=int(recall_denom_i), recall_num_impute=int(recall_num_i),
        precision_denom_impute=int(prec_denom_i), precision_num_impute=int(prec_num_i),
        recall_impute=recall_i, precision_impute=precision_i, f1_impute=f1_from_pr(precision_i, recall_i),
        recall_denom_worst=int(recall_denom_w), recall_num_worst=int(recall_num_w),
        precision_denom_worst=int(prec_denom_w), precision_num_worst=int(prec_num_w),
        recall_worst=recall_w, precision_worst=precision_w, f1_worst=f1_from_pr(precision_w, recall_w),
    )


def bootstrap_f1_ci(gt_true, pred_true, pred_missing, rng, n_boot=N_BOOT):
    n = len(gt_true)
    f1_drop_samples = np.empty(n_boot)
    f1_impute_samples = np.empty(n_boot)
    f1_worst_samples = np.empty(n_boot)
    for b in range(n_boot):
        idx = rng.integers(0, n, size=n)
        gt_b, pred_b, miss_b = gt_true[idx], pred_true[idx], pred_missing[idx]

        keep = ~miss_b
        gt_d, pred_d = gt_b[keep], pred_b[keep]
        rd = gt_d.sum()
        rn = (gt_d & pred_d).sum()
        pd_ = pred_d.sum()
        pn = (pred_d & gt_d).sum()
        r_drop = rn / rd if rd else np.nan
        p_drop = pn / pd_ if pd_ else np.nan
        f1_drop_samples[b] = f1_from_pr(p_drop, r_drop)

        rd_i = gt_b.sum()
        rn_i = (gt_b & pred_b).sum()
        pd_i = pred_b.sum()
        pn_i = (pred_b & gt_b).sum()
        r_imp = rn_i / rd_i if rd_i else np.nan
        p_imp = pn_i / pd_i if pd_i else np.nan
        f1_impute_samples[b] = f1_from_pr(p_imp, r_imp)

        added_fp = (miss_b & ~gt_b).sum()
        pd_w = pd_i + added_fp
        p_worst = pn_i / pd_w if pd_w else np.nan
        f1_worst_samples[b] = f1_from_pr(p_worst, r_imp)

    def pct_ci(samples):
        s = samples[~np.isnan(samples)]
        if len(s) == 0:
            return np.nan, np.nan
        return np.percentile(s, 2.5), np.percentile(s, 97.5)

    return pct_ci(f1_drop_samples), pct_ci(f1_impute_samples), pct_ci(f1_worst_samples)


def main():
    print("Reading all_data.parquet (relevant columns only)...")
    df = pq.read_table(FINAL_DATA + "/all_data.parquet", columns=COLS).to_pandas()
    print("rows:", len(df))

    # gt is an actual bool column; GPT-o3-mini_binary is object dtype holding True/False/None
    # (bools, not strings, in the parquet -- unlike the CSV, which uses "True"/"False"/"").
    gt_true_all = df["gt"].to_numpy(dtype=bool)
    pred_missing_all = df["GPT-o3-mini_binary"].isna().to_numpy()
    pred_true_all = (df["GPT-o3-mini_binary"] == True).to_numpy()  # noqa: E712 (None == True -> False, correctly excluded)
    models = df["model"].to_numpy()

    rng = np.random.default_rng(SEED)
    rows = []
    model_meta = df[["model", "model_family", "model_name"]].drop_duplicates().set_index("model")

    for model in sorted(df["model"].unique()):
        mask = models == model
        gt_true = gt_true_all[mask]
        pred_true = pred_true_all[mask]
        pred_missing = pred_missing_all[mask]

        m = compute_metrics(gt_true, pred_true, pred_missing)
        rl_d = clopper_pearson(m["recall_num_drop"], m["recall_denom_drop"])
        rl_i = clopper_pearson(m["recall_num_impute"], m["recall_denom_impute"])
        rl_w = clopper_pearson(m["recall_num_worst"], m["recall_denom_worst"])
        pr_d = clopper_pearson(m["precision_num_drop"], m["precision_denom_drop"])
        pr_i = clopper_pearson(m["precision_num_impute"], m["precision_denom_impute"])
        pr_w = clopper_pearson(m["precision_num_worst"], m["precision_denom_worst"])
        (f1_d_lo, f1_d_hi), (f1_i_lo, f1_i_hi), (f1_w_lo, f1_w_hi) = bootstrap_f1_ci(gt_true, pred_true, pred_missing, rng)

        meta = model_meta.loc[model]
        rows.append({
            "model": model,
            "model_family": meta["model_family"],
            "model_name": meta["model_name"],
            "n_total": m["n"],
            "n_missing": m["n_missing"],
            "pct_missing": 100 * m["n_missing"] / m["n"],
            "recall_drop": m["recall_drop"], "recall_drop_LCL": rl_d[0], "recall_drop_UCL": rl_d[1],
            "recall_impute": m["recall_impute"], "recall_impute_LCL": rl_i[0], "recall_impute_UCL": rl_i[1],
            "recall_delta_pp": 100 * (m["recall_impute"] - m["recall_drop"]),
            "recall_worst": m["recall_worst"], "recall_worst_LCL": rl_w[0], "recall_worst_UCL": rl_w[1],
            "recall_worst_delta_pp": 100 * (m["recall_worst"] - m["recall_drop"]),
            "precision_drop": m["precision_drop"], "precision_drop_LCL": pr_d[0], "precision_drop_UCL": pr_d[1],
            "precision_impute": m["precision_impute"], "precision_impute_LCL": pr_i[0], "precision_impute_UCL": pr_i[1],
            "precision_delta_pp": 100 * (m["precision_impute"] - m["precision_drop"]),
            "precision_worst": m["precision_worst"], "precision_worst_LCL": pr_w[0], "precision_worst_UCL": pr_w[1],
            "precision_worst_delta_pp": 100 * (m["precision_worst"] - m["precision_drop"]),
            "f1_drop": m["f1_drop"], "f1_drop_LCL": f1_d_lo, "f1_drop_UCL": f1_d_hi,
            "f1_impute": m["f1_impute"], "f1_impute_LCL": f1_i_lo, "f1_impute_UCL": f1_i_hi,
            "f1_delta_pp": 100 * (m["f1_impute"] - m["f1_drop"]),
            "f1_worst": m["f1_worst"], "f1_worst_LCL": f1_w_lo, "f1_worst_UCL": f1_w_hi,
            "f1_worst_delta_pp": 100 * (m["f1_worst"] - m["f1_drop"]),
        })
        print(f"{model}: missing={m['n_missing']}/{m['n']} ({100*m['n_missing']/m['n']:.1f}%) "
              f"recall {m['recall_drop']:.3f}->{m['recall_impute']:.3f}->{m['recall_worst']:.3f} "
              f"precision {m['precision_drop']:.3f}->{m['precision_impute']:.3f}->{m['precision_worst']:.3f} "
              f"F1 {m['f1_drop']:.3f}->{m['f1_impute']:.3f}->{m['f1_worst']:.3f}")

    result = pd.DataFrame(rows).sort_values("pct_missing", ascending=False)
    out_path = f"{RESULTS_DIR}/missing_response_sensitivity_by_model.csv"
    result.to_csv(out_path, index=False)
    print("\nWrote", out_path)

    # sanity check: precision must be numerically identical between drop/impute, but should
    # drop under worst_case (added false positives from missing rows where gt==False)
    max_prec_diff = (result["precision_drop"] - result["precision_impute"]).abs().max()
    print(f"\nSanity check -- max |precision_drop - precision_impute| across all models: {max_prec_diff:.2e} (expected ~0)")
    n_worse = (result["precision_worst"] < result["precision_drop"] - 1e-12).sum()
    print(f"Sanity check -- models where precision_worst < precision_drop: {n_worse}/{len(result)} (expected: all models with any missing rows where gt==False)")


if __name__ == "__main__":
    main()
