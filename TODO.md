# Analysis TODO

## General
- [ ] Convert all results reporting to the response scale (currently mixing link and response scale) — note: slopes may need to stay on the link scale, or use a multiplicative (odds ratio) explanation instead
- [ ] Check whether the way results sections are introduced is correct
- [ ] Establish logic for when/where to reference tables and figures inline
- [ ] Add figure captions to all figures
- [ ] Add correct x and y axis labels to all figures
- [ ] Format table headers and rownames for publication
- [ ] Use pub-ready experiment names (e.g. "perpendicular/parallel", "oblique +45/-45", rename natcan experiments to "canopyA", "canopyB", "canopyF1" etc.)
- [ ] Consider running single function to generate mTc and mTest — apply mTc model where control exists, mTest where not (possibly via a function that selects model type based on data)
- 

## Q1
- [ ] Consider integrating Q1 and Q2 narrative and figures
- [ ] Consider moving slope results elsewhere — feels abrupt in current position

## Q2
- [ ] Slope results are intertwined with the end-of-test prediction narrative — decide whether to separate them or keep together, but either way be consistent with how slopes are reported across all questions in the results
- [ ] Write comparison among artificial stimuli section — outstanding questions:
  - Comparison of prop correct at the same trial number?
  - Comparison of slopes?
  - What would be the appropriate way to compare among these experiments?

## Q3
- [ ] Replace figure 3 with a single plot with empty panels where controls were not run
- [ ] Free up space on x-axis in figure 4
- [ ] Run an overall test comparing naturalistic and artificial canopies (including all data)
- [ ] Merge testPts table and TC table for Q3
- [ ] Add end-trial estimates and CIs to all figures
- [ ] Complement the raincloud plot with lineplots of artificial and naturalistic side-by-side

## Issues
- Day is multicollinear in the thin oblique experiment — makes the slope appear less steep, driving the slope difference detected between perpPara and thin oblique. Is this difference real? Not confident experiments were run in sufficiently the same way to be comparable. Would a significant slope difference remain if day were removed from the model?
- Could the slope difference be attributable to slower learning due to the side bias evident in the thin oblique experiment (possibly caused by different pre-testing training regimes)?
- Why is there a side bias in thin oblique when those wasps had just come off a thick oblique experiment — was the side bias present then too?
- Conclusions regarding slope differences depend on which model is run
- Massive confidence intervals around the brightness experiment

## Supplementary
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
