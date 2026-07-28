# Analysis TODO

## General
- fix the model being run in the control. I think its running free slopes for across diff conditions but i think free intercepts is more reasonable.
- add the image diff analysis
- [ ] Convert all results reporting to the response scale (currently mixing link and response scale) — note: slopes may need to stay on the link scale, or use a multiplicative (odds ratio) explanation instead
- [ ] Check whether the way results sections are introduced is correct
- [ ] Establish logic for when/where to reference tables and figures inline
- [ ] Add figure captions to all figures
- [ ] Add stimuli images to figure facets (as inset or panel label illustration)
- [ ] Add correct x and y axis labels to all figures
- [ ] Format table headers and rownames for publication
- [ ] Use pub-ready experiment names (e.g. "perpendicular/parallel", "oblique +45/-45", rename natcan experiments to "canopyA", "canopyB", "canopyF1" etc.)
- [ ] Consider running single function to generate mTc and mTest — apply mTc model where control exists, mTest where not (possibly via a function that selects model type based on data)
- [ ] Check how papers in behavioural ecology report mixed model coefficient tables — specifically whether/how the random effect SD row is included (e.g. with "—" for p-value, or separated visually)
- [ ] Add in-depth explanation of model results somewhere (methods or supplementary) — should cover:
  - Side-biases (reward_side) are often present/significant in the models
  - Other factors included in the model (sideAlt_trial) and their interpretation
  - Factors that were considered but dropped or found unimportant (e.g. duration of reward on one side, day, etc.) and why they were excluded
  - Note: this section might fit well at the beginning of the results

## Q1
- [ ] Consider integrating Q1 and Q2 narrative and figures
- [ ] Consider moving slope results elsewhere — feels abrupt in current position
- [ ] Write comparison among artificial stimuli section — outstanding questions:
  - Comparison of prop correct at the same trial number?
  - Comparison of slopes?
  - What would be the appropriate way to compare among these experiments?

## Q2
- [ ] Slope results are intertwined with the end-of-test prediction narrative — decide whether to separate them or keep together, but either way be consistent with how slopes are reported across all questions in the results

## Q3
- [ ] Make panel labels in Figure 6 same as those in other figures using Affinity
- [x] Replace figure 3 with a single plot with empty panels where controls were not run
- [x] Free up space on x-axis in figure 4
- [ ] Run an overall test comparing naturalistic and artificial canopies (including all data)
- [ ] Consider pooling all naturalistic trials and running a binomial test against 50% as a simple overall test of above-chance performance
- [x] Merge testPts table and TC table for Q3
- [x] Add end-trial estimates and CIs to all figures
- [ ] Complement the raincloud plot with lineplots of artificial and naturalistic side-by-side

## Issues
- Day is multicollinear in the thin oblique experiment — makes the slope appear less steep, driving the slope difference detected between perpPara and thin oblique. Is this difference real? Not confident experiments were run in sufficiently the same way to be comparable. Would a significant slope difference remain if day were removed from the model?
- Could the slope difference be attributable to slower learning due to the side bias evident in the thin oblique experiment (possibly caused by different pre-testing training regimes)?
- Why is there a side bias in thin oblique when those wasps had just come off a thick oblique experiment — was the side bias present then too?
- Conclusions regarding slope differences depend on which model is run
- Massive confidence intervals around the brightness experiment

## Supplementary
- [ ] Clean up all supplementary tables (see sub-items below)
- [ ] Add diffuser experiment data to supplementary, along with repeated natcan, thick oblique, and artificial-following-natcan experiments
- [ ] Table S1: round to 2 decimal places, combine middle and end trial number columns
- [ ] Table S2: break up visually per experiment, order by nat/art
- [ ] Arrange all experiments in tables by nat vs art column
- [ ] Change CI separator to use `,` or `;`
- [ ] Merge contrast and pts predictions tables for TC data
- [ ] Add control coefficient to TC model results table
- [ ] Abbreviate `testEndTrial` to `TrEnd`
- [ ] Break up model results by experiment, stop repetition within experiment column
- [ ] Add proper table captions
- [ ] Numerate tables with S prefix

## Good to check before publication
- [ ] Check how predictions at trial 1 were obtained — was the intercept of the model adjusted, or were these just predicted from emmeans?
- [ ] Add predicted $P_{\mathrm{corr}}$ at a common reference trial (e.g. trial 10 or 20) for all experiments, to allow fair cross-stimulus comparisons on the response scale
