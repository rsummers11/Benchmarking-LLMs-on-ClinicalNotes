"""Checks whether models' quoted evidence is actually grounded in the source note. Extracts every
quoted span from each model's free-text response and checks whether it appears (verbatim,
allowing minor whitespace/markdown noise and "..." elisions) in that note's text; quotes that
can't be found are flagged as unverified/fabricated.

Matching rules:
  1. normalize whitespace and strip markdown emphasis markers (**, __, *, _)
  2. strip leading/trailing punctuation before matching
  3. case-insensitive substring check against the note's full text
  4. quotes containing an ellipsis are split on it, requiring each segment (>=8 chars) to match
  5. quotes that just restate the diagnosis/condition being asked about are dropped (not evidence)
  6. quotes containing "?" are dropped (restated questions, not note text)

Quotes shorter than 8 characters after normalization are skipped.
"""
import re
import pyarrow.parquet as pq
import pandas as pd

FINAL_DATA = "your_path_here/final_data"
RESULTS_DIR = "your_path_here/results"

QUOTE_RE = re.compile(r'"([^"]{4,400})"|“([^”]{4,400})”')
MARKDOWN_RE = re.compile(r'\*\*|\*|__|_')
WS_RE = re.compile(r'\s+')
ELLIPSIS_RE = re.compile(r'\.{3,}|…')
EDGE_PUNCT_RE = re.compile(r'^[\s.,;:!?\'"-]+|[\s.,;:!?\'"-]+$')


def normalize(text):
    text = MARKDOWN_RE.sub('', text)
    text = WS_RE.sub(' ', text).strip()
    return text.lower()


def strip_edges(s):
    return EDGE_PUNCT_RE.sub('', s)


def extract_quotes(response):
    if not isinstance(response, str):
        return []
    quotes = []
    for m in QUOTE_RE.finditer(response):
        q = m.group(1) or m.group(2)
        q = normalize(q)
        if len(q) >= 8:
            quotes.append(q)
    return quotes


def is_diagnosis_echo(quote_stripped, diagnosis_norm):
    """True if the quote is just the model restating the diagnosis/condition name (the
    question), not citing text from the note -- shouldn't be scored as evidence either way."""
    if not diagnosis_norm or not quote_stripped:
        return False
    if quote_stripped == diagnosis_norm:
        return True
    if quote_stripped in diagnosis_norm:
        return True
    if diagnosis_norm in quote_stripped and len(quote_stripped) <= len(diagnosis_norm) + 15:
        return True
    return False


def is_grounded(quote_stripped, note_norm):
    if quote_stripped in note_norm:
        return True
    segments = [strip_edges(s) for s in ELLIPSIS_RE.split(quote_stripped)]
    segments = [s for s in segments if len(s) >= 8]
    if not segments:
        return False
    return all(seg in note_norm for seg in segments)


def main():
    print("Reading note_id -> note_text (deduplicated)...")
    notes_df = pq.read_table(f"{FINAL_DATA}/all_data.parquet", columns=["note_id", "note_text"]).to_pandas()
    notes_df = notes_df.drop_duplicates("note_id")
    note_norm = {nid: normalize(txt) for nid, txt in zip(notes_df["note_id"], notes_df["note_text"])}
    print("distinct notes:", len(note_norm))
    del notes_df

    print("Reading model/diagnosis/note_id/response/gt/prediction...")
    df = pq.read_table(f"{FINAL_DATA}/all_data.parquet",
                        columns=["model", "model_family", "note_id", "diagnosis", "response", "gt", "GPT-o3-mini_binary"]).to_pandas()
    print("rows:", len(df))

    diagnosis_norm_cache = {}

    def diag_norm(d):
        if d not in diagnosis_norm_cache:
            diagnosis_norm_cache[d] = normalize(d) if isinstance(d, str) else ""
        return diagnosis_norm_cache[d]

    quote_rows = []
    resp_rows = []
    n_diagnosis_echo = 0
    n_question_echo = 0
    cols = zip(df["model"], df["note_id"], df["diagnosis"], df["response"], df["gt"], df["GPT-o3-mini_binary"])
    for model, note_id, diagnosis, response, gt, pred in cols:
        quotes = extract_quotes(response)
        if not quotes:
            continue
        note_txt = note_norm.get(note_id)
        if note_txt is None:
            continue
        d_norm = diag_norm(diagnosis)

        grounded_flags = []
        for q in quotes:
            q_stripped = strip_edges(q)
            if is_diagnosis_echo(q_stripped, d_norm):
                n_diagnosis_echo += 1
                continue
            if '?' in q_stripped:
                n_question_echo += 1
                continue
            grounded_flags.append(is_grounded(q_stripped, note_txt))

        if not grounded_flags:
            continue
        for g in grounded_flags:
            quote_rows.append((model, g))
        # correct is None when the judge couldn't parse a prediction for this response (pred is
        # None) -- can't say whether it "got the right answer for the wrong reason" without one
        correct = (pred == gt) if pred is not None else None
        resp_rows.append((model, all(grounded_flags), any(not g for g in grounded_flags), correct))

    quote_df = pd.DataFrame(quote_rows, columns=["model", "grounded"])
    print("total quotes extracted (excluding diagnosis-echo and question-echo quotes):", len(quote_df))
    print("diagnosis-echo quotes dropped:", n_diagnosis_echo)
    print("question-echo quotes dropped:", n_question_echo)

    meta = pd.read_csv(f"{FINAL_DATA}/models_run.csv")
    summary = quote_df.groupby("model").agg(
        n_quotes=("grounded", "size"),
        n_grounded=("grounded", "sum"),
    ).reset_index()
    summary["pct_grounded"] = 100 * summary["n_grounded"] / summary["n_quotes"]
    summary["n_unverified"] = summary["n_quotes"] - summary["n_grounded"]
    summary["pct_unverified"] = 100 - summary["pct_grounded"]

    # response-level rate too: what fraction of RESPONSES that contained >=1 quote had >=1
    # quote that couldn't be verified (a coarser, easier-to-report number)
    resp_df = pd.DataFrame(resp_rows, columns=["model", "all_grounded", "any_unverified", "correct"])
    resp_summary = resp_df.groupby("model").agg(
        n_responses_with_quotes=("all_grounded", "size"),
        n_responses_all_grounded=("all_grounded", "sum"),
        n_responses_any_unverified=("any_unverified", "sum"),
    ).reset_index()
    resp_summary["pct_responses_any_unverified"] = 100 * resp_summary["n_responses_any_unverified"] / resp_summary["n_responses_with_quotes"]

    out = summary.merge(resp_summary, on="model").merge(meta, on="model", how="left")
    out = out.sort_values("pct_unverified", ascending=False)
    out.to_csv(f"{RESULTS_DIR}/quote_grounding_by_model.csv", index=False)

    print("\n=== Quote grounding by model (worst 10 by % unverified) ===")
    display_cols = ["model_name", "n_quotes", "pct_grounded", "n_responses_with_quotes", "pct_responses_any_unverified"]
    print(out[display_cols].head(10).round(1).to_string(index=False))
    print("\n=== best 10 ===")
    print(out[display_cols].tail(10).round(1).to_string(index=False))

    overall_pct = 100 * quote_df["grounded"].mean()
    print(f"\nOverall: {len(quote_df)} quotes extracted across {df['response'].notna().sum()} responses; {overall_pct:.1f}% verified as present in source note.")

    # --- correctness cross-tab: is fabricated evidence more common among CORRECT answers? ---
    scored = resp_df.dropna(subset=["correct"]).copy()
    scored["correct"] = scored["correct"].astype(bool)

    print("\n=== Overall: % of responses with >=1 unverified quote, by correctness ===")
    overall_xtab = scored.groupby("correct")["any_unverified"].agg(["size", "mean"]).rename(columns={"mean": "pct_any_unverified"})
    overall_xtab["pct_any_unverified"] *= 100
    print(overall_xtab.round(1).to_string())

    xtab = scored.groupby(["model", "correct"]).agg(
        n=("any_unverified", "size"),
        pct_any_unverified=("any_unverified", "mean"),
    ).reset_index()
    xtab["pct_any_unverified"] *= 100
    xtab_wide = xtab.pivot(index="model", columns="correct", values=["n", "pct_any_unverified"])
    xtab_wide.columns = [f"{a}_{'correct' if b else 'incorrect'}" for a, b in xtab_wide.columns]
    xtab_wide = xtab_wide.reset_index().merge(meta[["model", "model_name"]], on="model", how="left")
    xtab_wide["gap_pp"] = xtab_wide["pct_any_unverified_correct"] - xtab_wide["pct_any_unverified_incorrect"]
    xtab_wide = xtab_wide.sort_values("gap_pp", ascending=False)
    xtab_wide.to_csv(f"{RESULTS_DIR}/quote_grounding_by_correctness.csv", index=False)

    print("\n=== Per-model: % responses with unverified quotes, correct vs incorrect answers ===")
    print("(sorted by gap: correct-answer unverified rate minus incorrect-answer rate)")
    show_cols = ["model_name", "n_correct", "pct_any_unverified_correct", "n_incorrect", "pct_any_unverified_incorrect", "gap_pp"]
    print(xtab_wide[show_cols].round(1).to_string(index=False))


if __name__ == "__main__":
    main()
