# Speaker Notes — Portable Polarization

*Partisan Differences in COVID-19 Health Behaviors*

Christopher Soria

---

## The Partisan Mortality Divide

Open with the puzzle. Early in the pandemic, mortality was higher in Democratic (urban) counties because that's where the virus arrived first. By fall 2020, Republican-leaning counties overtook them and never looked back — especially after vaccines became available. This is motivational, not causal: we're not making ecological inferences. But it raises the individual-level question we actually test.

---

## From Counties to Individuals

This slide bridges the aggregate puzzle to our contribution. Stress that we're not committing the ecological fallacy — county patterns motivate the question, individual data test it. The BICS study gives us exactly what we need: actual contact counts and mask-wearing during specific interactions, not abstract attitudes.

---

## A. Partisanship and Health: Not a COVID Invention

Baum (2011) "Red State, Blue State, Flu State" is the key citation here — partisan gaps in swine flu vaccination in 2009, driven by media self-selection. Republicans who consumed conservative media were less likely to get vaccinated, not because of any policy difference but because the information environment shaped their perception of risk. This is the clearest pre-COVID precedent.

---

## B. The Partisan Gap in Public Health Perception Has Grown

Gadarian, Goodman & Pepinsky (2021) "Partisanship, health behavior, and policy attitudes in the early stages of the COVID-19 pandemic" is the key panel study here. They show that partisan gaps in concern, behavior, and policy support were large from the very start of the pandemic and grew over time. Also worth noting: elite cue divergence was early — Green et al. (2020) show that congressional Democrats and Republicans were already sending divergent messages about COVID by mid-February 2020, weeks before the first lockdowns.

---

## C. Documented Behavioral Differences During COVID

Key citations: Allcott et al. (2020) use smartphone mobility data to show Republicans reduced movement less. Gollwitzer et al. (2020) link partisan physical distancing to county-level health outcomes. Grossman et al. (2020) use a natural experiment around governor recommendations. Wallace et al. (2023) is the most striking: excess death rates were 43% higher among registered Republicans than Democrats after vaccines became available. Gadarian et al. (2024) show it's specifically Trump support — not generic party ID — that drives vaccine hesitancy.

---

## D. Partisanship as Social Identity

SCRIPT FOR SLIDE D:

"So where does that leave us theoretically? The prior slides established that partisan health gaps are real, they predate COVID, and they've been documented across many outcomes. The question is: why?

The framework I want to propose is that partisanship has become what sociologists call a *fundamental social identity* — something like race or religion — that organizes how people see the world and how they behave in it. This comes from Henri Tajfel's social identity theory: when people categorize themselves as group members, they internalize group norms and act in ways that signal belonging.

What that predicts for health behavior is pretty specific. First, *in-group conformity* — you do what your group does, not just what's rational from an individual risk standpoint. Second, *symbolic differentiation* — behaviors become markers of who you are and who you're not. Wearing or not wearing a mask stopped being purely a health decision and became a statement. Third, *durable dispositions* — Cockerham's health lifestyles framework tells us these aren't one-off choices, they're patterned regularities that persist across situations.

The COVID context turbocharged this process. Elite partisan cues diverged almost immediately — by March 2020, Republican and Democratic politicians were already sending different signals about the severity of the threat. Mask mandates became party-coded. Vaccines became a litmus test. So health behaviors got pulled into an existing partisan identity structure that was already very strong.

Now, the key theoretical move in our paper is this: *if* partisanship is a fundamental identity that drives behavior, then behavioral gaps should be *portable*. They shouldn't disappear when you move to a different state, or a different policy environment, or control for demographics. Identity travels with you. That's what we test — and that's what the next section shows."

---

## Competing Explanations & Testable Predictions

This is the analytical framework for the rest of the talk. Each subsequent findings section is a test of one or more rows. Walk the audience through it quickly — the payoff is that we test them all. The "portable polarization" label is the outcome when explanation 7 is what we find.

---

## Data: Berkeley Interpersonal Contact Study (BICS)

Emphasize the measurement quality. BICS captures actual behaviors in a concrete recall task rather than general behavioral dispositions. This is important for the partisan bias critique — counting face-to-face contacts yesterday is harder to distort expressively than reporting general attitudes. Also note the raked weights — we adjust for party ID to match Gallup targets, so our sample isn't over-representing engaged partisans.

**On raked weights (if asked):** Raked weights are an algorithm that adjusts the sample to match the national distribution on multiple characteristics simultaneously. Our standard weights already match Census targets on age, gender, race, and education. Raking adds party ID: it further adjusts weights so the weighted sample also matches Gallup 2020 benchmarks (~30% Democrat, 29% Republican, 39% Independent). Online surveys tend to oversample politically engaged, educated respondents who skew Democratic — without this correction, we'd be comparing a non-representative Democrat sample to a non-representative Republican sample. Raking ensures each party group is weighted to reflect its true share of the U.S. adult population.

---

## Partisan Gaps Across All Three Behaviors

Show the basic finding. Republicans: more contacts (`r round(contacts_rep, 1)` vs `r round(contacts_dem, 1)` for Dems), less masking (`r round(mask_rep)`% vs `r round(mask_dem)`%). Independents fall in between. Both behaviors, same directional pattern. This is the baseline for everything that follows.

---

## Sort each panel independently by its own absolute gap (largest first)

This is the magnitude point to hammer home. The partisan mask gap is not just statistically significant — it's substantively larger than most other standard demographic predictors. This puts partisan identity in conversation with race, gender, and class as a structuring force for health behavior.

---

## But These Groups Really Do Differ Demographically

This slide motivates the first several tests. The groups genuinely differ: Republicans in BICS skew older, whiter, more male, and less metro than Democrats. These are precisely the demographic variables that independently predict risk-taking and mask skepticism. So the hypothesis that behavioral gaps are just demographics in disguise is scientifically reasonable — not a straw man. That's what makes Test 1 important: we need to actually check.

---

## Three Competing Hypotheses

Frame these as genuine scientific alternatives, not straw men. H1, H2a, and H2b are all reasonable — the groups DO differ demographically, DO live in different contexts, and DO report different levels of concern. The tests that follow are designed to adjudicate: H1 predicts gaps disappear after demographic controls; H2a predicts gaps shrink within the same policy/geographic context; H2b predicts gaps shrink among partisans with matched levels of concern. If none of these work, that's evidence for H3.

---

## Adjusted gaps (+ wave + demographics)

Test 1. The unadjusted vs. adjusted comparison makes the argument immediately visible: the two points barely move. Controlling for all major demographic predictors doesn't close the gap. This rules out explanation #2 — partisan gaps aren't just demographic sorting in disguise.

---

## Test 2 — Urban/Rural Doesn't Explain It

Test 2. Often the urban/rural divide is assumed to drive partisan health differences because Republicans are more rural and rural areas have lower density. But within metro areas, gaps remain. This rules out density-based explanations.

---

## Test 3 — Policy Doesn't Explain It

Test 3. Within the same mask mandate regime, partisan gaps persist. Both contacts and masks show the same pattern: policy shifts the absolute level but the Republican-Democrat gap remains. This rules out the policy explanation.

---

## Test 4 — Severity Asymmetry: Contacts Compress, Masks Widen

Test 4 — the most theoretically nuanced finding. Look at the left panel: the contact gap (R−D) is largest in spring 2020 (+2.2), then compresses during the winter 2020–21 surge (+0.8 in Dec, +0.5 in Feb), before widening again in May 2021. The contact gap tracks mortality — when conditions were objectively worst, partisans converged on cautious behavior. Now look at the right panel: the mask gap grows steadily from spring 2020 through spring 2021, from about 5pp to 23pp, with no compression during the mortality surge. The same pandemic, the same partisans, two completely different behavioral logics. Private behavior (contacts) responds to objective risk. Public behavior (masks) tracks group identity regardless of conditions. This asymmetry sets up the visibility mechanism.

---

## Test 5 — Stated Concern Doesn't Account for It

Test 5. Severity asymmetry showed masks don't track objective conditions — now the question is whether subjective perception explains it. Even when we compare Democrats and Republicans who express identical levels of concern, behavioral gaps remain at every concern level. Crucially, Democrats who say they are "not at all concerned" still mask more than Republicans who say the same. Stated concern itself is partially performative — expressive of group membership rather than genuine cognitive risk assessment. Masks signal identity, not risk response. Rules out explanation #5.

---

## Test 6 — Geography Doesn't Explain It

Test 6. The gap map uses weighted descriptive means (weight_party_raked). The level bar plot uses emmeans from OLS models: num_cc ~ party * region + wave + log(mortality), weighted by weight_party_raked. Marginal means are evaluated at average wave and mortality conditions, so the bars show what each party's behavior would look like in an average wave/mortality context. SEs are emmeans model-based SEs. The adjustment for wave and local mortality rules out that regional gaps simply reflect when surveys were conducted or how severe COVID was locally — the partisan gap within each region survives those controls.

---

## Level plot (animated in)

Test 7. Lead with gaps: contacts gap is nearly identical in same-party (1.19) and opposing-party (1.18) districts — context has no effect on contacts. The mask gap collapses from 20.7pp in same-party districts to near zero in opposing-party districts, but this is because BOTH parties shift: Democrats in Republican CDs wear fewer masks (pulled down by local norms) while Republicans in Democratic CDs wear more masks (pushed up). Click reveals the levels: the lines converge in opposing-party districts because context moves both baselines toward each other, not because partisan identity disappears.

---

## "Portable Polarization"

This is the conceptual heart of the paper. The portable polarization label captures the finding that gaps don't shrink when you control for the contexts where each alternative theory predicted they should. Behavioral differences are properties of individuals, not environments. This is what we'd expect if partisan identity has become a fundamental social identity in Tajfel's sense.

---

## Private vs. Public Behavior

This is the theoretical core of the visibility mechanism argument. If the same underlying factor (risk perception, health dispositions) drove both behaviors, we'd expect similar divergence patterns. Instead they went in opposite directions — contacts compressed, masks widened. The difference is social observability: masks are publicly legible, contacts are private. This points to identity-driven processes operating selectively on visible behaviors.

---

## Key Takeaways

This is the summary slide before moving to implications. Walk through each point briefly. The fourth point is the most nuanced — it distinguishes between objective severity (which moves contacts) and stated concern (which doesn't explain masks). The key insight is that "not concerned" Republicans still reduce contacts when mortality is high, meaning the lever is concrete local conditions, not stated cognition. Masks follow a different logic entirely.

---

## Theoretical Contributions

Summarize theoretical contributions. This is what we want the audience to take away as "the paper's argument." Hit each point briefly.

---

## Limitations

Be direct about these. The causation point is the most important methodologically — we're doing principled pattern-matching, not causal identification. The Trump support point is actually a theoretical refinement: if the mechanism is identity-consistent behavior cued by elite signals, Trump supporters received a much stronger and more consistent anti-mask signal than generic Republicans. The religiosity gap is a real omitted variable concern, especially for contacts — evangelical communities maintained in-person gatherings longer, and skew Republican.

---

## Implications for Public Health Communication

This is the public health punchline from Test 5. The standard health behavior model assumes: increase perceived risk → increase protective behavior. Our data suggest that for mask use specifically, this pathway is blocked or bypassed by partisan identity. Democrats who aren't worried still mask; Republicans who are worried still don't. The implication is not that risk communication is useless, but that it needs to work through different mechanisms for highly identity-governed behaviors — social norm messaging, trusted community voices, or reframing behaviors so they don't carry partisan valence.

---

## Implications for Disease Modeling

This is the applied contribution. The disease modeling audience cares about behavioral heterogeneity in transmission models — who contacts whom, and how protected those contacts are. We can offer two precise contributions: (1) party ID is a meaningful stratification variable, above and beyond age and geography; (2) the behavioral parameters aren't fixed — they interact with local mortality and policy context. A model that treats Republicans and Democrats as exchangeable will underestimate between-group transmission differences, and a model that ignores contextual modulation will get the temporal dynamics wrong.

---

## Conclusion: Partisanship as a Structuring Force in Health

Close on the theoretical payoff. The two-behavior asymmetry is the most theoretically interesting finding: the same partisans, facing the same pandemic, respond differently depending on whether the behavior is privately consequential (contacts) or publicly visible (masks). This maps onto a distinction between behavior driven by genuine risk assessment versus behavior serving an expressive, identity-signaling function. The broader claim: partisan identity now operates like race or religion in structuring health behavior — not as a proxy for something else, but as its own independent force.

---

## Thank You

Leave key stats visible for Q&A reference. Be ready to discuss: (1) the expressive responding critique, (2) the causal identification problem, (3) implications for current health politicization (vaccines, chronic disease, reproductive health).

---

## Mask Gap Among the "Very Concerned" Over Time

This is the supporting evidence for Test 5. Even among respondents who all said they were "very concerned" about COVID-19 — identical stated concern — the partisan mask gap grew substantially over time. Early in the pandemic (Jun '20) Republicans who were very concerned actually masked at similar or higher rates than Democrats. By Feb '21, a large gap had opened. Concern held constant; behavior diverged. This is the clearest illustration that stated concern is not the mechanism driving differential masking.

---

