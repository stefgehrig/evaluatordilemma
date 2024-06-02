# evaluatordilemma

This repository contains data and analysis code to reproduce results from the article **"Towards a Methodological Toolkit to Mitigate the Evaluator’s Dilemma: a Case Study of Pastoralist Communal Rangelands Management"**.

<body> 
 
<div style="display: flex;"> 
  <img src="results/plot_radar.png" style="height: 150px;"> 
  <img src="results/plot_dags.png" style="height: 150px;"> 
  <img src="results/plot_mainresults.png" style="height: 150px;"> 
</div> 
 
</body> 

<br>

All data is stored in `/data`. The script `R/compute_results.R` loads these data and computes all results (fitting models, creating tables and figures, ...), also sourcing the custom funtions in `R/functions.R`. In `R/draw_dags.R`, the causal graphs shown in the article are drawn.

<br>

**R session info:** (from `R/compute_results.R`): <a href="sessionInfo.txt">Here</a>
