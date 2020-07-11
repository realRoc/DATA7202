Data released: 19 May 2017
Data created by: Dr. Jiwon Kim, School of Civil Engineering (jiwon.kim@uq.edu.au)
Data sent to: Dr. Ian Wood, School of Mathematics & Physics (i.wood1@uq.edu.au)

The data described below are intended to be used for course DATA7202.
Please contact Jiwon Kim (jiwon.kim@uq.edu.au) to use the data in other courses or research.

This folder contains the following data files:
- cell_info_2km.csv
- cell_info_2km_1.PNG
- cell_info_2km_2.PNG
- crossBoundaryFlow_20130225-20130303.csv
- crossBoundaryFlow_20130304-20130310.csv
- crossBoundaryFlow_20130311-20130317.csv
- crossBoundaryFlow_20130318-20130324.csv

"cell_info_2km.csv" provides the coordinates of cell centroids.

"cell_info_2km_~.PNG" shows locations and boundaries of cells on the map.

"flow_A_B.csv" files contain data for cross-boundary flows from origin A to destination B for bus passengers, where the magnitudes of flow represent approximately 50% of go card bus passengers.

Each file contains the following data columns:
- date : observation date in the format of yyyy-MM-dd
- time_id : integer index for observation time window
- start_time : start time of the observation time window, in total minutes since the start of the day.
- end_time : end time of the observation time window, in total minutes since the start of the day.
- source_cell : start cell of the cross-boundary flow
- target_cell : end cell of the cross boundary flow
- v0_num_traj : flow (from origin cell to destination cell) during observation time window