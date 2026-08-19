"""Compares delta-method vs. bootstrap F1 confidence intervals for the 5 rarest diagnoses
(9-19 positive notes out of 380), for each open-weights model family: delta-method CIs (from
Clopper-Pearson precision/recall CIs via the manuscript's variance-propagation formula) vs.
percentile bootstrap CIs (R=2000, resampling notes with replacement).
"""
import numpy as np
import pandas as pd
import pyarrow.parquet as pq
from scipy.stats import beta

FINAL_DATA = "your_path_here/final_data"
RESULTS_DIR = "your_path_here/results"
SEED = 42
N_BOOT = 2000

RARE_DIAGNOSES = ["Drug abuse", "AIDS/HIV infection", "Rheumatoid arthritis/collagen vascular diseases",
                  "Lymphoma", "Psychoses"]


def clopper_pearson(successes, n, alpha=0.05):
    if n == 0:
        return np.nan, np.nan, np.nan
    mean = successes / n
    lower = beta.ppf(alpha / 2, successes, n - successes + 1) if successes > 0 else 0.0
    upper = beta.ppf(1 - alpha / 2, successes + 1, n - successes) if successes < n else 1.0
    return mean, lower, upper


def delta_f1_ci(prec_mean, prec_lo, prec_hi, rec_mean, rec_lo, rec_hi):
    var_p = ((prec_hi - prec_lo) / (2 * 1.96)) ** 2
    var_r = ((rec_hi - rec_lo) / (2 * 1.96)) ** 2
    denom = (prec_mean + rec_mean)
    if denom == 0:
        return 0.0, 0.0, 0.0, 0.0
    f1 = 2 * prec_mean * rec_mean / denom
    dF1_dP = 2 * rec_mean ** 2 / denom ** 2
    dF1_dR = 2 * prec_mean ** 2 / denom ** 2
    var_f1 = dF1_dP ** 2 * var_p + dF1_dR ** 2 * var_r
    se_f1 = np.sqrt(var_f1)
    return f1, se_f1, f1 - 1.96 * se_f1, f1 + 1.96 * se_f1


def f1_from_counts(gt, pred):
    tp = (gt & pred).sum()
    fp = ((~gt) & pred).sum()
    fn = (gt & (~pred)).sum()
    precision = tp / (tp + fp) if (tp + fp) else 0.0
    recall = tp / (tp + fn) if (tp + fn) else 0.0
    f1 = 2 * precision * recall / (precision + recall) if (precision + recall) else 0.0
    return precision, recall, f1


def bootstrap_f1_ci(gt, pred, note_ids, rng, n_boot=N_BOOT):
    unique_notes = np.unique(note_ids)
    n = len(unique_notes)
    note_to_idx = {}
    for i, nid in enumerate(note_ids):
        note_to_idx.setdefault(nid, []).append(i)
    samples = np.empty(n_boot)
    for b in range(n_boot):
        chosen_notes = rng.choice(unique_notes, size=n, replace=True)
        idx = np.concatenate([note_to_idx[nid] for nid in chosen_notes])
        _, _, f1 = f1_from_counts(gt[idx], pred[idx])
        samples[b] = f1
    return np.percentile(samples, 2.5), np.percentile(samples, 97.5)


def main():
    print("Reading data...")
    df = pq.read_table(f"{FINAL_DATA}/all_data.parquet",
                        columns=["model", "model_family", "open_source", "note_id", "diagnosis", "gt", "GPT-o3-mini_binary"]).to_pandas()
    df = df[df["open_source"] == "yes"]
    df = df[df["diagnosis"].isin(RARE_DIAGNOSES)]
    df = df[df["GPT-o3-mini_binary"].notna()]
    gt_all = df["gt"].to_numpy(dtype=bool)
    pred_all = (df["GPT-o3-mini_binary"] == True).to_numpy()  # noqa: E712

    rng = np.random.default_rng(SEED)
    rows = []
    for diagnosis in RARE_DIAGNOSES:
        for family in ["Llama", "Qwen"]:
            mask = (df["diagnosis"] == diagnosis).to_numpy() & (df["model_family"] == family).to_numpy()
            gt_sub, pred_sub = gt_all[mask], pred_all[mask]
            note_sub = df["note_id"].to_numpy()[mask]
            n_positive_notes = df.loc[mask & df["gt"], "note_id"].nunique()

            prec_mean, prec_lo, prec_hi = clopper_pearson((pred_sub & gt_sub).sum(), pred_sub.sum())
            rec_mean, rec_lo, rec_hi = clopper_pearson((gt_sub & pred_sub).sum(), gt_sub.sum())
            f1_delta, se_f1, delta_lo, delta_hi = delta_f1_ci(prec_mean, prec_lo, prec_hi, rec_mean, rec_lo, rec_hi)

            boot_lo, boot_hi = bootstrap_f1_ci(gt_sub, pred_sub, note_sub, rng)

            rows.append({
                "diagnosis": diagnosis, "model_family": family,
                "n_rows": mask.sum(), "n_positive_notes": n_positive_notes,
                "precision": round(prec_mean, 3), "recall": round(rec_mean, 3),
                "f1": round(f1_delta, 3),
                "delta_lo": round(delta_lo, 3), "delta_hi": round(delta_hi, 3),
                "delta_width": round(delta_hi - delta_lo, 3),
                "boot_lo": round(boot_lo, 3), "boot_hi": round(boot_hi, 3),
                "boot_width": round(boot_hi - boot_lo, 3),
                "delta_out_of_range": bool(delta_lo < 0 or delta_hi > 1),
            })

    out = pd.DataFrame(rows)
    out.to_csv(f"{RESULTS_DIR}/delta_vs_bootstrap_rare_diagnoses.csv", index=False)
    print(out.to_string(index=False))


if __name__ == "__main__":
    main()
