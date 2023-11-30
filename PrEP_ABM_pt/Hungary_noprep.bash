#!/bin/bash
#SBATCH --job-name=fit_prep      # Job name as it will appear in the queue manager
## #SBATCH --mail-type=END,FAIL         # Mail events (NONE, BEGIN, END, FAIL, ALL)
## #SBATCH --mail-user= wangboxuan990617@gmail.com   # Where to send mail. Maybe if you put your address you'll get notified when the job ends or fails, but I'm not sure    
#SBATCH --ntasks=1                   # Run a single task. I think we should keep this =1          
#SBATCH --cpus-per-task=12            # Number of CPU cores per task: this is important for parallelizing. How many CPU in parallel you want to run. I think some nodes allow for up to 24, some up to 12. You can keep 12 to be safe. 
#SBATCH --mem=30gb                    # Job memory request: how much RAM you think you'll need
#SBATCH --output=myjob.log     # Standard output and error log: you can customize the name of the log file
#SBATCH --nodes=1                     # Number of nodes to use
#SBATCH --time=12:00:00
# this line below is the command you would normally execute. MAKE SURE YOU'RE USING THE RIGHT PYTHON (or whatever you're using) by using the full path to the python dist that you installed for your purposes
#cd /home/boxuan/cost_effectivness_prep_abm/HPC_ABM/
#source activate abm_wbx_2

Rscript Hungary_noprep.R