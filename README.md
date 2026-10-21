# Indoor Positioning System (IPS)

**STAT 410 Consulting Project** · Qwyn Petersen & Truong Lam · December 2024

GPS doesn't work well indoors because walls, ceilings and furniture block satellite signals. This project builds an **Indoor Positioning System** that predicts where a device is on a single floor of a building using only the **Wi-Fi signal strength** it receives from nearby access points.

The approach has two steps:

1. **Signal → Distance:** use **K-Nearest Neighbors (KNN) regression** to predict how far the device is from each router, based on signal strength.
2. **Distance → Location:** use **trilateration with least squares** to turn the distances to three routers into an (x, y) position.

## Data

The data comes from a ~15 m × 36 m single-floor building with six Wi-Fi access points.

| Dataset | File | Description |
|---|---|---|
| Offline (training) | `offline.final.trace.txt` | 166 locations × 8 orientations (every 45°) × 110 samples each |
| Online (testing) | `online.final.trace.txt` | 60 random location/orientation combinations × 110 samples (6,600 measurements) |
| Access points | `accessPointLocations.txt` | MAC address and (x, y) coordinates of each router |

Each record contains a timestamp, the device's position and orientation, and the MAC address, signal strength, channel and type for every access point it detected.

## What the code does

All of the analysis is in **`IPS_R_code.R`**, which runs in three parts:

### 1. Load and clean the data
- Parses the raw trace files (splitting each line on `;`, `=` and `,`) into a data frame and drops comment lines and records with no signal readings
- Converts columns to numeric types and timestamps from milliseconds to date-times
- Removes redundant columns (`scanMac`, `posZ`)
- Keeps only the six relevant access points and joins in their coordinates
- Rounds each device orientation to the nearest 45° bin (0°, 45°, …, 315°)
- Computes the Euclidean distance between the device and each router

### 2. Reduce noise
Signal readings are noisy, so the script builds four versions of the training data and compares them:

| Version | Description | KNN RMSE | Least Squares RMSE |
|---|---|---|---|
| All data | Every raw signal reading | 5.56 m | 12.71 m |
| Median signal | Median of the 110 readings per location/orientation/router | 5.79 m | 12.51 m |
| **3 closest routers, device facing router** | Only the nearest three routers, using the reading where the device pointed most directly at each one | **3.18 m** | **4.81 m** |
| 3 closest routers, strongest signal | Only the nearest three routers, using the orientation with the strongest signal | 3.78 m | 6.27 m |

### 3. Predict and evaluate
- **KNN regression** (`FNN::knn.reg`) predicts device-to-router distance from median signal strength
- **Trilateration**: for each test location, the predicted distances to three routers are linearized into a system `Ax = b` and solved with the pseudoinverse (`pracma::pinv`) to get the predicted (x, y)
- **Evaluation**: RMSE for both models, plus residual plots, Q-Q plots, residual histograms, a predicted-vs-actual position plot with error arrows, and an error heatmap across the floor

## Results

- The best version (three closest routers, device facing the router) predicted **router distance within 3.18 m** (KNN) and **device location within 4.81 m (~16 ft)** (least squares).
- Signal strength and distance have a clear, strong relationship, which makes signal strength usable for positioning.
- Device orientation affects signal strength, and filtering by orientation noticeably improved accuracy.
- Noise filtering (fewer, closer routers) mattered more than anything else for accuracy.
- The model is still sensitive to outliers and to obstacles in the building, so ~5 m accuracy leaves room to improve.

## Future work

- Combine the methods: use trilateration to find a rough area, then KNN to refine it
- Map physical obstacles (walls, furniture) to explain where errors are largest
- Try other models such as random forests or neural networks
- Look into other variables (channel, timestamp) and test model assumptions like equal variance

## How to run

1. Install R and the required packages:
   ```r
   install.packages(c("dplyr", "tidyr", "tidyverse", "ggplot2", "magrittr", "FNN", "caret", "pracma"))
   ```
2. Put `offline.final.trace.txt`, `online.final.trace.txt` and `accessPointLocations.txt` in one folder.
3. In `IPS_R_code.R`, change the `setwd(...)` line near the top to point to that folder.
4. Run the script. It prints the RMSE for each model and draws the diagnostic plots.

To compare noise-reduction methods, change `train_x` / `train_y` in the prediction section to `train_final1`, `train_final2`, `train_final3` or `train_final4` (use `signal` for versions 1–2 and `median_signal` for 3–4). The number of neighbors is set with `k`.

## Repository contents

| File | Description |
|---|---|
| `IPS_R_code.R` | Full data cleaning, modeling and visualization pipeline |
| `IPS_Report.pdf` | Final written report |
| `IPS_Presentation.pptx` | Project presentation slides |

## References

- Teixeira-Pinto, A. (2022). *Machine Learning for Biostatistics: K-nearest Neighbours Regression.* [bookdown.org](https://bookdown.org/tpinto_home/Regression-and-Classification/k-nearest-neighbours-regression.html)
- *Uniwide WiFi Based Positioning System.* IEEE. [ieeexplore.ieee.org/document/5514639](https://ieeexplore.ieee.org/abstract/document/5514639/)
- *An Adaptive K-Nearest Neighbor Algorithm.* IEEE. [ieeexplore.ieee.org/document/5569740](https://ieeexplore.ieee.org/abstract/document/5569740/)
- Holtz, Y. *The R Graph Gallery.* [r-graph-gallery.com](https://r-graph-gallery.com/)
