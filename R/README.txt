General guide to run code:
Each folder has a Master_...R file. 
You have to run the Master_...R file in each folder - all wrangled data and results will be saved 
in the corresponding folder in the path: DATA/wrangled_data/ and Results/ .

Finally, you should run the Master_...R file from gather_res folder. 
This script will gather results from different datasources used here, and run the final Bayesian analysis 
with rarefied data (100 replicates of randomly drawn 105 communities).