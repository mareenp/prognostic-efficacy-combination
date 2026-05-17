# prognostic-efficacy-combination
R Code for the corresponding article: Pigorsch, M., Möckel, M., Gehrig, S., Wiemer, J. C., Koehler, F., & Rauch, G. (2022). Performance evaluation of a new prognostic‐efficacy‐combination design in the context of telemedical interventions. ESC Heart Failure. https://onlinelibrary.wiley.com/doi/full/10.1002/ehf2.14122

* **What the project does:**
This is the R code used for the simulations in the manuscript "Performance evaluation of a new prognostic-efficacy-combination design in the context of telemedical interventions".
It additionally includes code for a resampling version of the prognostic-efficacy-combination design using a 10-times 10-fold cross-validation. 

* **Why the project is useful:**
The investigated design allows to define a biomarker cut-off and to perform a randomized controlled trial (RCT) in a biomarker-selected population within a single study. This methodological research work evaluates if a double use of the control group for biomarker cut-off definition and efficacy assessment within the RCT leads to biased treatment effect estimates. The update shows that equipping the prognostic-efficacy-combination design with cross-validation reduces bias. An overview of the results is shown in Results/Plot_bias_CV.png.

* **How users can get started with the project:**
Clone the repository and run "sim1_define_function.R" and "sim2_call_function_scenarios.R" for the simulations. The script "sim3_print_results.R" creates an overview and tables of the results, while "sim4_plot_bias.R" creates a figure.
The scripts "sim1b_define_function.R", "sim2b_call_function_scenarios.R" and "sim4b_plot_bias_CV.R" are updates including a resampling version of the prognostic-efficacy-combination design (New-CV).

* **Where users can get help with your project:**
See the above mentioned article. For any questions or comments, please reach out to mareen.pigorsch@charite.de.

* **Who maintains and contributes to the project:**
The code was written by Mareen Pigorsch, Stefan Gehrig and Geraldine Rauch.
