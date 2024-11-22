# care4cmodel 1.0.3

* Fixed CRAN check note missing package anchor for package deSolve in the 
  documentation of setup_statevars().
* In the output of `fuel_and_co2_evaluation()`, the variable name 
  `co2_road_maint_total_l` has been corrected to `co2_road_maint_total_kg`. I.e.
  the variable name now contains the correct unit (kg).


# care4cmodel 1.0.2

* The authors have published an article that explains the science behind the 
  package in detail. We have inserted links to this article (open access) in the 
  README file and in the vignette. The article is available online here:
  https://www.sciencedirect.com/science/article/pii/S0168169924004824.


# care4cmodel 1.0.1

* Bugfix: When simulating concepts with a stand development phase having
  only one subphase, R's default behaviour of making a one-column matrix subset
  a vector caused an error.


# care4cmodel 1.0.0

* Initial CRAN submission.
