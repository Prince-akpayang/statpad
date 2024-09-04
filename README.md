# statpad
A comprehensive tool designed to streamline the process of summarizing and analyzing yearly academic results for learners of Teach For Nigeria fellows. StatPad efficiently aggregates data, generates insights, and presents results in a user-friendly format, empowering fellows to make data-driven decisions to improve educational outcomes.


Sure, I can help you craft a more accurate README file for your Shiny app project. Here's a draft based on the code and context you've provided:

---

# Learner Performance Analytics Dashboard

## Overview

This project is a Shiny app built using R and the Shiny library, designed to provide detailed analytics on learner performance. The app allows users to upload data, filter information by specific fellows (students), and generate various summary tables and plots to analyze learner performance across different terms and arms. It is particularly useful for educational administrators and teachers who need to track and assess student progress in a structured manner.

## Features

- **Data Upload:** Users can upload a CSV file containing learner performance data. The expected format includes columns such as `S/N`, `Name`, `Gender`, `Test`, `Exam Score`, `Total`, `Arm`, and `Term`.
  
- **Fellow Selection:** The app allows the user to select a specific fellow (student) from the dataset to view detailed information and performance metrics.
  
- **Summary Tables:** Generate a variety of summary tables, including:
  - Average Score per Term and Arm
  - Performance Summary Across Terms
  - Test vs. Exam Performance by Term and Arm
  - Gender Performance Analysis by Term and Arm
  - Pass/Fail Rate by Term and Arm
  - And more

- **Visual Analytics:** Create plots to visually analyze data, such as:
  - Average Score per Term and Arm
  - Class Average Scores by Term and Arm
  - Test vs. Exam Performance by Term and Arm
  - Gender Performance Analysis by Term and Arm

## Installation

To run this Shiny app, you need to have R installed on your system. Additionally, you need to install the required libraries:

```R
install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "DT", "tidyverse", "shinyBS"))
```

## Usage

1. Clone this repository:

```bash
git clone https://github.com/Prince-akpayang/your-repo-name.git
```

2. Set up your working directory to the cloned repository and ensure the `data/` directory contains your CSV data file.

3. Run the app in R:

```R
shiny::runApp()
```

4. Upload your dataset using the `Upload Data` tab.

5. Navigate through the other tabs to select fellows, view summary tables, and generate plots.

## Example Dataset

The expected structure for the CSV file is as follows:

```plaintext
S/N, Name, Gender, Test, Exam Score, Total, Arm, Term
1, John Doe, Male, 60, 70, 130, A, Term 1
2, Jane Smith, Female, 75, 80, 155, B, Term 2
...
```

## Contact

If you encounter any issues or have questions, feel free to contact me at 
- **Email:**: [princeakpayang@gmail.com](mailto:princeakpayang@gmail.com).
- **LinkedIn:** [Etini Akpayang](https://www.linkedin.com/in/etini-akpayang/)
- **Website:** [TFN Fellows StatPad](https://sloneo-etini-akpayang.shinyapps.io/TFN_FELLOWS_STATPAD/)


Here’s the updated contact information section based on your preference:

---
