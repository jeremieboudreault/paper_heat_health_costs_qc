Historical and projected heat-related health costs
================================================================================

This is a code example for the implementation presented in the paper "*Assessing the historical and projected heat-related health costs, applied to a Canadian province*" to compute the historical and future heat-related health costs based on climate and demographic changes.

- By [Jérémie Boudreault](https://jeremieboudreault.github.io/), Céline Campagna, Fateh Chebana and Éric Lavigne.

---

### Scripts

Scripts are located in the `scripts/` folder : 

- `1_climate_data.R` : Climate models ensemble data (EPSO-G6-R2) under SSP2-4.5 and SSP5-8.5. 
- `2_pop_data.R` : Population projection data under SSP2 and SSP5.
- `3_projected_an.R` : Projected AN to heat based on climate and population data.
- `4_projected_hw.R` : Computation of projected heatwaves and affected population.
- `5_simulated_costs.R` : Monte Carlo simulation of heat-realted health costs.
- `6_projected_costs.R` : Computation of projected heat-related health costs.


Helper functions are located in the `R/` folder.

---

***Enjoy !***