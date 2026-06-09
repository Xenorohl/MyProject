The file WorldEcosystem.tiff isn't in the "/data" folder because it was too heavy. It was not pushed by adding it to a .gitignore folder to the project 

Important notes : 

1. to run properly the project, the file "inst/packages.r" must be run entirely before anything 
2. run the file "inst/launcher.r" after the packages, one line one by one, even for the analysis (PS : the script analysis_2_1_grid_pred.r has its final matrix saved in "/data" folder as grid_final.csv)
3. there will be an error for "src/ecosystem.r" because the file WorldEcosystem.tif isn't on this github (as explained upper)
4. the same error will show "src/final_project/analysis_2_1_grid_pred.r" for WorldEcosystem.tif
5. the final panel that I submit is called **true_final_panel.png** in the "/plots" folder (pre_final_panel.png is the one I did modifications on, because I wasn't entirely satisfied of it but found it still kinda cool so I kept it)
6. the analysis of each plot on the final panel is at the end of the script "final_project/src/analysis_4_final_panel.r"
7. Some plots were made for the intermediate project as well, but were not placed in the final panel. However, they are relevant to look at (those are verification plots) 


Steps to run this script : 

1. Run /inst/launcher.r line by line
2. Wait to see the arrow > popping up again in the console between each script (can take some time)
3. For plot visualization, go directly to the dedicated script, at the end of the scripts
