# Selective-exercise abuse: simulation specification

**Target section:** a *new* subsection §7.9, inserted immediately after §7.8 Commuter memberships (`\label{sec:commuter}`) and before §8 Discussion. Suggested title "Selective exercise", label `\label{sec:selective-exercise}`.

**Relationship to §7.7.** §7.7 (membership abuse) is unchanged and complementary. §7.7 stresses *which trips* a flat-membership rider substitutes into — longer routes, peak hours. This section stresses *which days* a commuter exercises when route and departure hour are already fixed by the contract.

**Scope for the implementing agent:** this is a *wrapper* around the existing §7.8 commuter pipeline, which is itself not modified — neither its code nor its text, tables, or figures. Do **not** re-implement pricing, route sampling (5.2), travel-time sampling (5.3), or the premium (4.6)/(7.5). The only new logic is a selection rule applied to payouts the existing pipeline already produces, plus aggregation and plotting.

---

## 1. Mechanism

A commuter holds `M = 30` capped rides on a fixed origin–destination pair at a fixed departure hour. The existing analysis assumes rides not taken are dropped **at random**, which makes unused rides pure profit (Figure 10).

A rational holder does not drop at random. On the morning of each occasion the rider observes some information about conditions — weather, a traffic app, prior experience of that day of week — and exercises the membership when the ride looks expensive, using an outside option (drive, transit) when it looks cheap. This is optimal option exercise, and it is the one abuse channel available to a commuter whose route and departure hour are already fixed by the contract.

The rider's private benefit on occasion `i` is the amount the cap saves them, `(P_i − K)⁺`, which is exactly the provider's payout. **Rider objective and provider loss are the same object**, so no separate utility model is needed.

Parameter `λ ∈ [0,1]` is the rider's foresight: the rank correlation between the rider's signal and the realized fare.

- `λ = 0` — no foresight; reproduces the current random-breakage result.
- `λ = 1` — perfect foresight; the rider always exercises on the worst-traffic days.

The design **nests** the existing Figure 10 rather than replacing it.

---

## 2. Notation and quantities

Per member `m`, per configuration `(model, strike, h)`, the existing pipeline already yields:

| Symbol | Meaning | Source |
|---|---|---|
| `R̄_m` | per-ride premium | Eq. (7.5) |
| `Π_m = M · R̄_m` | monthly premium collected up front | Eq. (7.5) |
| `P_{m,i}`, `i = 1..M` | realized fare on occasion `i` | route sampling (5.2) + travel-time sampling (5.3) |
| `K_m` | the month's strike (`P` or `0.9P`) | §7.8 |

Derived:

```
L_{m,i} = max(P_{m,i} − K_m, 0)        payout on occasion i
```

### Profit identity (state this in the paper)

The rider pays `Π_m` up front, then `min(P_i, K)` per ride taken; the provider's cost of supplying ride `i` is `P_i`. For a taken set `A`:

```
profit = Π_m + Σ_{i∈A} min(P_i, K) − Σ_{i∈A} P_i
       = Π_m − Σ_{i∈A} (P_i − K)⁺
       = Π_m − Σ_{i∈A} L_{m,i}
```

Utilization enters **only** through which `L_i` are summed. That is the whole mechanism, and it is why the simulation is cheap.

---

## 3. Selection rule

Within each member's own month (not pooled across members):

```
r_{m,i}  = rank of P_{m,i} among {P_{m,1..M}}, ascending, ties broken at random
z_{m,i}  = Φ⁻¹((r_{m,i} − 0.5) / M)              within-month normal scores
ε_{m,i}  ~ N(0,1) iid                             drawn ONCE per member, reused for all λ and k
S_{m,i}(λ) = λ · z_{m,i} + sqrt(1 − λ²) · ε_{m,i}
```

Selection order `π_m(λ)` = indices sorted by `S_{m,i}(λ)` **descending**. Taken set at `k` rides:

```
A_m(k, λ) = first k entries of π_m(λ)
Λ_m(k, λ) = Σ_{i ∈ A_m(k,λ)} L_{m,i}            payout
Y_m(k, λ) = Π_m − Λ_m(k, λ)                     dollar profit
LR_m(k, λ) = Λ_m(k, λ) / Π_m                    loss ratio
```

Notes for the implementer:

- Ranking is on `P`, not on `L`. `L` has an atom at zero (out-of-the-money occasions) which would create massive ties; `P` is continuous and `L` is monotone in `P`, so ranking on `P` induces the correct ranking on `L`.
- Normal scores rather than raw `P` make `λ` scale-free and give `S` the same marginal distribution at every `λ`.
- **Common random numbers are mandatory.** The same `ε_{m,·}` and the same realized `(P_{m,·})` must be reused across every `λ` and every `k` for a given member. Consequences: the `k`-curve is automatically nested (`A(k,λ) ⊂ A(k+1,λ)`), and comparisons across `λ` are paired, so SEs on *differences* between λ curves must be computed paired (see §6).
- `λ = 1` gives `sqrt(1−λ²) = 0` exactly; guard the ranking code against an all-deterministic `S`.

---

## 4. Grids and configuration

| Dimension | Values | Note |
|---|---|---|
| `λ` | `{0, 0.25, 0.50, 0.75, 1.0}` | add `0.9` if the curve turns sharply near 1 |
| `k` | `0, 1, …, 30` | integer; `u = k/30`, no interpolation needed for curves |
| `model` | `population`, `trip-specific` | as Table 5 |
| `strike` | `K = P`, `K = 0.9P` | as Table 5 |
| `h` | `8` for the headline; `{2,4,6,8}` optional | see caveat below |
| `N` members | **500** | 100 (Table 5) is too noisy for break-even estimation |

**`h` caveat.** Table 5's max-loss column grows with `h` (−86 → −199 → −157 → −239) because the eligible OD-pair pool shrinks from 1,001 pairs at `h=2` to 63 at `h=8`, not because pricing degrades. If the `h` axis is swept here, **hold the pair set fixed at the `h=8`-eligible pairs across all `h`**, so the axis is a genuine comparison. Otherwise just fix `h=8` and drop the axis from this section.

---

## 5. Algorithm

```
for each config (model, strike, h):
    for m in 1..N:
        (Π_m, P_{m,1..M}, K_m) ← EXISTING §7.8 PIPELINE      # unchanged
        L_{m,i} ← max(P_{m,i} − K_m, 0)
        z_{m,·} ← within-month normal scores of P_{m,·}
        ε_{m,·} ~ N(0,1)                                      # once
        for λ in Λgrid:
            S ← λ·z_{m,·} + sqrt(1−λ²)·ε_{m,·}
            ord ← order(S, decreasing = TRUE)
            cum ← cumsum(L_{m, ord})                          # length M
            for k in 0..M:
                Λ ← (k == 0 ? 0 : cum[k])
                emit row(m, model, strike, h, λ, k,
                         premium = Π_m, payout = Λ,
                         profit  = Π_m − Λ,
                         fares_taken = sum(P_{m, ord[1..k]}),
                         fares_total = sum(P_{m,·}),
                         loss_ratio  = Λ / Π_m)
```

Cost is `N × |Λgrid| × (M log M)` on top of the existing pipeline — negligible. The expensive part (pricing and travel-time simulation) runs once per member and is shared across all `λ` and `k`.

### Output schema

One long-format CSV, `selective_exercise_raw.csv`:

```
member_id, model, strike, h, lambda, k, u, premium, payout, profit,
fares_taken, fares_total, loss_ratio
```

Do not aggregate in the simulation loop. All tables and figures are produced downstream from this file.

---

## 6. Aggregation and headline outputs

**Normalize by loss ratio, not by "% of fares."** `Π_m` is fixed within a member while `fares_taken` moves with `k`, so a "% of fares taken" denominator contaminates the utilization curve with its own trend. Report `LR` as the headline and dollar profit as the secondary column. (This also resolves the two-denominator reconciliation paragraph currently on p. 21.)

### Output 1 — Figure (a new figure in §7.9, extending Figure 10)

Figure 10 (`fig:commuter-breakage`) stays where it is in §7.8. This is a separate figure that extends it.

Mean dollar profit per member against utilization `u = k/30`, one line per `λ`, faceted by strike (`K=P`, `K=0.9P`), line type by model. The `λ = 0` line **must** overlay the existing Figure 10 curve.

Second panel, same axes, plotting mean `LR` with a horizontal reference at `LR = 1`.

The qualitative claim to look for: `LR(u, 0)` is **linear** in `u`, while `LR(u, λ)` for `λ > 0` is **concave** — the provider's exposure is front-loaded onto the occasions the rider chooses to exercise. The gap between the linear and concave curves is the cost of selective exercise.

### Output 2 — Table: break-even utilization

```
u*(λ) = min{ u : mean_m LR_m(u, λ) ≥ 1 }
```

linearly interpolated between adjacent `k`. Report `u*` by `λ × model × strike`, with member-level bootstrap CIs (see below). Report `u* = NA (>1)` when the product never breaks even within the month.

This is the headline number. It converts "unused rides are pure profit" into "unused rides are pure profit **only if** the rider cannot predict traffic."

### Output 3 — Inverted calibration (recommended over calibrating λ)

Rather than estimating the rider's true foresight, invert the question. At the observed/assumed utilization `u₀` (e.g. `u₀ = 0.8`), report

```
λ*(u₀) = min{ λ : mean_m LR_m(u₀, λ) ≥ 1 }
```

i.e. **how good a forecaster the rider must be before the product loses money.** This is a defensible claim without any behavioural calibration, and it is a stronger rhetorical position than a point estimate of `λ`: "the commuter membership is robust unless riders can rank their own travel times with rank correlation above `λ*`."

Sweep `u₀ ∈ {0.6, 0.7, 0.8, 0.9, 1.0}` and report `λ*` as a small table or a single contour on the `(u, λ)` plane.

### Standard errors

Member-level nonparametric bootstrap, `B = 1000`, resampling members with replacement within each config. For SEs on *differences* between `λ` values, resample members (not `(m, λ)` cells) so the pairing induced by common random numbers is preserved — this will make the λ contrasts far tighter than their marginal SEs suggest, and that should be stated.

---

## 7. Acceptance tests

The agent should implement these as assertions before producing any figures.

| # | Test | Tolerance |
|---|---|---|
| 1 | `LR_m(0, λ) == 0` for all `m, λ` | exact |
| 2 | `LR_m(M, λ)` is **identical across all λ** for each `m` | exact (floating point) |
| 3 | `LR_m(k, λ)` is non-decreasing in `k` for every `m, λ` | exact (`L ≥ 0`) |
| 4 | At `λ = 1`, the selection order equals `order(P_{m,·}, decreasing=TRUE)` | exact |
| 5 | `mean_m LR_m(k, 0)` is linear in `k`, slope `= M·E[L]/Π` | R² > 0.999 on the fitted line |
| 6 | `mean_m LR_m(k, λ)` is non-decreasing in `λ` at each fixed `k` | within 2 bootstrap SE |
| 7 | The `λ = 0` profit curve reproduces the existing Figure 10 | within Monte-Carlo error of the original run |

Test 2 is the strongest structural check: taking all 30 rides gives the same payout regardless of the order chosen. If it fails, the selection indexing is wrong.

Test 7 is the regression test that protects the existing published result. Run it against the current `h`-and-strike configuration before changing anything.

---

## 8. Optional extension (lower priority)

**Endogenous utilization.** Instead of exercising exactly `k` rides, the rider exercises whenever the perceived saving clears an outside-option threshold: take ride `i` iff `S_{m,i}(λ) ≥ τ`. Calibrate `τ` per `λ` so that mean utilization matches a target `ū`, then report the distribution of realized `k` alongside profit. This is more realistic but adds a parameter and makes `k` random; it is not needed for the headline claim and can go in an appendix or be dropped.

Do not build this until §§1–7 are complete and the acceptance tests pass.

---

## 9. What the section should conclude

State the result as a correction to the paper's own current claim, not as an add-on. The target is the closing paragraph of §7.8, which presently says exposure is worst against the fully utilizing member and that any realistic attrition moves the product into profit. That holds under `λ = 0` only. Under selective exercise the provider pays the expected sum of the top-`k` payouts, which strictly exceeds `k · E[L]`, so there exists a utilization strictly below 100% at which the provider still loses. With a right-skewed payout distribution — which travel time supplies — `u*` may sit well below one half.

The revised claim: **exposure is worst against the selectively utilizing member, and breakage is a benefit only to the extent that non-exercise is uninformed.**

Since §7.8 keeps its own wording, append one forward-pointing sentence to the end of that closing paragraph directing the reader to §7.9, so the two claims are not read in isolation. That is the only edit this section asks for outside itself.

This also slots into the conditioning argument. The commuter contract conditions on origin, destination, and hour, leaving only *which day* free — and `λ` measures how much the holder can select on that residual dimension. `λ = 0` is the fully-conditioned limit; `λ = 1` is the within-cell dispersion of the fair premium realized in full.
