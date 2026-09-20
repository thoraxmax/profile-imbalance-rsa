"""Response Surface Analysis (RSA) used in the manuscript.

Python 3.10.20

Expected input file: merged_data9.csv
Outputs are written to: rsa_output/

The script fits polynomial OLS models of the form
    RCADS = b0 + b1*X + b2*Y + b3*X^2 + b4*X*Y + b5*Y^2 + covariates + error
and derives the standard RSA surface parameters:
    a1 = b1 + b2
    a2 = b3 + b4 + b5
    a3 = b1 - b2
    a4 = b3 - b4 + b5

It also compares each polynomial model with its corresponding linear model
using the nested-model F test and produces the contour figure used to inspect
cross-domain response surfaces.
"""

from pathlib import Path
import numpy as np
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf
from scipy import stats as sps
import matplotlib.pyplot as plt

# -----------------------------------------------------------------------------
# Settings
# -----------------------------------------------------------------------------
DATA_FILE = Path("merged_data9.csv")
OUTPUT_DIR = Path("rsa_output")
OUTPUT_DIR.mkdir(exist_ok=True)

OUTCOME = "rcads"
COVARIATES = ["age", "male"]
TO_REVERSE = ["p_neuro"]

BESSI = ["b_SELFM", "b_SOCENG", "b_COOPER", "b_EMORES"]
HIPIC = ["p_neuro", "p_extrav", "p_consc", "p_open"]
ESWAN = ["e_EMOTION", "e_SOCCAP", "e_REGUL"]
INDICATORS = BESSI + ESWAN + HIPIC

DOMAINS = {
    "self_management": ["b_SELFM", "p_consc", "e_REGUL"],
    "social_engagement": ["b_SOCENG", "p_extrav", "e_SOCCAP"],
    "cooperation": ["b_COOPER", "e_SOCCAP"],
    "emotional_resilience": ["b_EMORES", "p_neuro", "e_EMOTION"],
    "openness_only": ["p_open"],
}

DOMAIN_LABELS = {
    "self_management": "Self-management/regulation",
    "social_engagement": "Social engagement/drive",
    "cooperation": "Cooperation",
    "emotional_resilience": "Emotional resilience/emotions",
    "openness_only": "Openness",
}


def framework(var):
    if var.startswith("b_"):
        return "BESSI"
    if var.startswith("e_"):
        return "E-SWAN"
    if var.startswith("p_"):
        return "HiPiC"
    return "Other"


def zscore(series):
    """Sample-SD z standardization (ddof=1), matching the analysis notebook."""
    return (series - series.mean()) / series.std(ddof=1)


def load_and_prepare(path):
    df = pd.read_csv(path)
    required = set(INDICATORS + [OUTCOME] + COVARIATES)
    missing = sorted(required.difference(df.columns))
    if missing:
        raise ValueError(f"Missing required columns: {missing}")

    df = df.copy()
    for var in TO_REVERSE:
        df[var] = -df[var]

    # Standardize continuous RSA variables and age. Male is retained as coded.
    for var in INDICATORS + [OUTCOME, "age"]:
        df[var] = zscore(df[var].astype(float))

    return df


# -----------------------------------------------------------------------------
# Pairwise RSA
# -----------------------------------------------------------------------------
def fit_rsa(df, x, y, outcome=OUTCOME, covariates=COVARIATES):
    """Fit one polynomial RSA model and return a1-a4 and model comparison."""
    cols = [x, y, outcome] + list(covariates)
    data = df[cols].dropna().copy()
    if len(data) < 10:
        return None

    X = pd.DataFrame({
        x: data[x].to_numpy(),
        y: data[y].to_numpy(),
        f"{x}2": data[x].to_numpy() ** 2,
        f"{x}_{y}": data[x].to_numpy() * data[y].to_numpy(),
        f"{y}2": data[y].to_numpy() ** 2,
    }, index=data.index)
    for cov in covariates:
        X[cov] = data[cov].to_numpy()

    X = sm.add_constant(X, has_constant="add")
    outcome_values = data[outcome].to_numpy()
    model = sm.OLS(outcome_values, X).fit()
    b = model.params
    V = model.cov_params()

    # Contrast vectors for a1-a4 in terms of the polynomial coefficients.
    terms = list(model.params.index)
    def contrast(weights):
        c = np.zeros(len(terms))
        for term, weight in weights.items():
            c[terms.index(term)] = weight
        estimate = float(c @ b.to_numpy())
        variance = float(c @ V.to_numpy() @ c)
        se = np.sqrt(max(variance, 0.0))
        t_value = estimate / se if se > 0 else np.nan
        p_value = 2 * sps.t.sf(abs(t_value), df=model.df_resid) if np.isfinite(t_value) else np.nan
        return estimate, se, float(p_value)

    a1, se_a1, p_a1 = contrast({x: 1, y: 1})
    a2, se_a2, p_a2 = contrast({f"{x}2": 1, f"{x}_{y}": 1, f"{y}2": 1})
    a3, se_a3, p_a3 = contrast({x: 1, y: -1})
    a4, se_a4, p_a4 = contrast({f"{x}2": 1, f"{x}_{y}": -1, f"{y}2": 1})

    X_linear = sm.add_constant(data[[x, y] + list(covariates)], has_constant="add")
    linear_model = sm.OLS(outcome_values, X_linear).fit()
    f_value, p_f, df_diff = model.compare_f_test(linear_model)

    return {
        "N": int(model.nobs),
        "a1": a1, "SE_a1": se_a1, "p_a1": p_a1,
        "a2": a2, "SE_a2": se_a2, "p_a2": p_a2,
        "a3": a3, "SE_a3": se_a3, "p_a3": p_a3,
        "a4": a4, "SE_a4": se_a4, "p_a4": p_a4,
        "F_RSA_vs_linear": float(f_value),
        "p_F": float(p_f),
        "df_diff": float(df_diff),
        "R2_RSA": float(model.rsquared),
        "R2_linear": float(linear_model.rsquared),
    }


def run_pairwise_rsa(df):
    """Run same-framework, cross-domain RSA comparisons used in the notebook."""
    indicator_domain = {}
    for domain, variables in DOMAINS.items():
        for var in variables:
            indicator_domain.setdefault(var, domain)

    rows = []
    seen = set()
    for domain, domain_vars in DOMAINS.items():
        for x in domain_vars:
            if x not in INDICATORS:
                continue
            for y in INDICATORS:
                if y == x or y in domain_vars:
                    continue
                if framework(x) != framework(y):
                    continue

                # Avoid duplicate unordered pairs generated from different loops.
                pair_key = tuple(sorted((x, y)))
                if pair_key in seen:
                    continue
                seen.add(pair_key)

                result = fit_rsa(df, x, y)
                if result is None:
                    continue
                rows.append({
                    "domain": domain,
                    "X": x,
                    "Y": y,
                    "X_domain": indicator_domain.get(x, "no_domain"),
                    "Y_domain": indicator_domain.get(y, "no_domain"),
                    "framework": framework(x),
                    **result,
                })

    results = pd.DataFrame(rows)
    results.to_csv(OUTPUT_DIR / "rsa_pairwise_results.csv", index=False)
    return results


# -----------------------------------------------------------------------------
# Contour surfaces
# -----------------------------------------------------------------------------
def predict_surface(df, x, y, grid_n=40, grid_limit=(-2.5, 2.5)):
    """Fit an RSA polynomial model and return a prediction grid at mean covariates."""
    cols = [x, y, OUTCOME] + COVARIATES
    data = df[cols].dropna().copy()
    if len(data) < 10:
        return None

    data["X"] = data[x]
    data["Y"] = data[y]
    data["X2"] = data["X"] ** 2
    data["Y2"] = data["Y"] ** 2
    data["XY"] = data["X"] * data["Y"]

    formula = f"{OUTCOME} ~ X + Y + X2 + XY + Y2 + " + " + ".join(COVARIATES)
    model = smf.ols(formula, data=data).fit()

    values = np.linspace(grid_limit[0], grid_limit[1], grid_n)
    GX, GY = np.meshgrid(values, values)
    pred = pd.DataFrame({
        "X": GX.ravel(),
        "Y": GY.ravel(),
        "X2": (GX ** 2).ravel(),
        "Y2": (GY ** 2).ravel(),
        "XY": (GX * GY).ravel(),
        # Age is standardized, so 0 is its sample mean. Male=0 gives a
        # consistent reference category for visualization.
        "age": 0.0,
        "male": 0.0,
    })
    Z = model.predict(pred).to_numpy().reshape(GX.shape)
    return GX, GY, Z


def make_contour_figure(df):
    """Reproduce the notebook's mean cross-domain RSA contour visualization."""
    plt.rcParams.update({
        "font.family": "serif",
        "font.serif": ["Times New Roman"],
        "font.size": 9,
        "axes.labelsize": 9,
        "axes.titlesize": 10,
    })

    # The original figure averaged BESSI and E-SWAN surfaces; HiPiC was not
    # included in this visualization.
    allowed_frameworks = {"BESSI", "E-SWAN"}
    domain_names = [d for d in DOMAINS if d != "openness_only"]
    pairs = [(domain_names[i], domain_names[j])
             for i in range(len(domain_names))
             for j in range(i + 1, len(domain_names))]

    surfaces = []
    grids = []
    for domain_a, domain_b in pairs:
        vars_a = [v for v in DOMAINS[domain_a] if framework(v) in allowed_frameworks]
        vars_b = [v for v in DOMAINS[domain_b] if framework(v) in allowed_frameworks]
        z_list = []
        grid = None
        for x in vars_a:
            for y in vars_b:
                out = predict_surface(df, x, y)
                if out is not None:
                    GX, GY, Z = out
                    grid = (GX, GY)
                    z_list.append(Z)
        surfaces.append(np.mean(z_list, axis=0) if z_list else None)
        grids.append(grid)

    valid = [z.ravel() for z in surfaces if z is not None]
    if not valid:
        return
    all_z = np.concatenate(valid)
    vmin, vmax = np.nanpercentile(all_z, [2, 98])

    cm = 1 / 2.54
    fig, axes = plt.subplots(2, 3, figsize=(17 * cm, 10 * cm))
    axes = np.asarray(axes).ravel()
    contour_for_colorbar = None

    for ax, (domain_a, domain_b), Z, grid in zip(axes, pairs, surfaces, grids):
        ax.set_xlabel(f"{DOMAIN_LABELS[domain_a]} (z)", fontsize=8)
        ax.set_ylabel(f"{DOMAIN_LABELS[domain_b]} (z)", fontsize=8)
        if Z is not None and grid is not None:
            GX, GY = grid
            contour_for_colorbar = ax.contourf(
                GX, GY, Z, levels=18, cmap="RdYlBu_r", vmin=vmin, vmax=vmax
            )
        else:
            ax.text(0.5, 0.5, "Insufficient data", ha="center", va="center", transform=ax.transAxes)
        ax.axhline(0, color="gray", lw=0.7, linestyle=":")
        ax.axvline(0, color="gray", lw=0.7, linestyle=":")
        ax.tick_params(axis="both", which="both", direction="in", labelsize=7)
        ax.set_aspect("equal")

    if contour_for_colorbar is not None:
        fig.colorbar(contour_for_colorbar, ax=axes.tolist(), shrink=0.80, pad=0.03,
                     label="Predicted RCADS (z)")
    fig.suptitle("Mean RSA surfaces: cross-domain pairs (BESSI & E-SWAN)",
                 fontsize=11, fontweight="bold")
    fig.subplots_adjust(left=0.10, right=0.88, bottom=0.12, top=0.88, wspace=0.65, hspace=0.35)
    fig.savefig(OUTPUT_DIR / "rsa_contours.png", dpi=600, bbox_inches="tight")
    fig.savefig(OUTPUT_DIR / "rsa_contours.svg", bbox_inches="tight")
    plt.close(fig)


def main():
    df = load_and_prepare(DATA_FILE)
    results = run_pairwise_rsa(df)
    make_contour_figure(df)
    print(f"Completed {len(results)} RSA pairwise models.")
    print(f"Outputs saved to: {OUTPUT_DIR.resolve()}")


if __name__ == "__main__":
    main()
