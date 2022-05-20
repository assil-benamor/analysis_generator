
<div id="top"></div>




<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/lilos404/analysis_generator.git">
    <img src="https://data.humdata.org/image/2015-11-05-231341.581365REACHlogo_300x125_grey.png" alt="Logo">
  </a>

<h3 align="center">Analysis Generator</h3>

  <p align="center">
    A quick and easy way to generate summary statistics from survey data collected using Kobo / ODK
    <br />
    <br />
    <a href="https://github.com/lilos404/analysis_generator.git/issues">Report Bug</a>
    ·
    <a href="https://github.com/lilos404/analysis_generator.git/issues">Request Feature</a>
  </p>
</div>



<!-- ABOUT THE PROJECT -->
### About The Project

This repository was created for REACH YEMEN mission to speed up the process of generating summary statistics for the different RCs. The input files needed to run the script are the kobo tool (questions and choices sheets) and the cleaned dataset.




### Built With

* [R](https://www.r-project.org/)




<!-- GETTING STARTED -->

### Steps to use/run the script

**0. Prerequisite: git**

Git should be installed on your system, if that's not the case, follow this [link](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) for download and installation guide.

**1. Clone the repo**

   
   From Windows CMD or Linux terminal (for geeks):
   
   ```sh
   git clone https://github.com/lilos404/analysis_generator.git
   ```
   
   Using RStudio:
   
  
  Click on File menu ==> New Project ==> Version Control ==> Git == > Paste the repo   [link](https://github.com/lilos404/analysis_generator.git) in the Repository URL field and specify where to download the project
   
**2. Install the required packages** 

This is a one time setup, run the following time after you download the project for the first time.

```R
source("./src/0_install_dependencies.R")
```

**3. Prepare your dataset** 

Modify "1_data_preparation.R" script according to your needs. The script has multiple useful code chunks that you can use to prepare the data set before running the analysis. To use one of the sections, uncomment it using ctrl+shift+c or remove the leading "#" in each line.

**4. Run the analysis and enjoy** 

In "2_run_analysis.R" fill in the required parameters (dataset path, language of the export ... ) and run the rest of the code.
If you face any issues while running the script send me a message on Skype/Teams (For urgent requests bring me chocolate or nuts). 



<!-- CONTRIBUTING -->
### Contributing

To contribute to this project, please fork the repo and create a pull request. 

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/NewFeature`)
3. Commit your Changes (`git commit -m 'Add some NewFeature'`)
4. Push to the Branch (`git push origin feature/NewFeature`)
5. Open a Pull Request



<!-- CONTACT -->
### Contact

Assil BEN AMOR - assil.benamor@reach-initiative.org


<p align="right">(<a href="#top">back to top</a>)</p>

