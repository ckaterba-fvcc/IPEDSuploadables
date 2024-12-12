## FVCC Changes 

1. 2-year college graduation rate survey (gr2)

  - Functions:
  
    - make_gr2_part_j.R where j is in B, C D 
    
    - produce_gr2_report.R
    
  - Use: 
  
    - Same df as specified for produce_gr_report, but add a Completed100 column (binary); I believe we can ignore the 4 year and 5 year columns.
    
2. 200 Grad Rate (g22)

  - Functions:
  
    - make_gr2002.R
    
    - produce_gr2002_report.R
    
  - Use:
    
    - Same df as specified for produce_gr200_report; `IsComp` field, at least according to IPEDS, seems to indicate only if a student has completed in year 4 (ie between 151 and 200 percent of program length), not <= 4 years. Maybe use same 

[Below is TO DO (functions copied from 4-year college version)]

3. Fall Enrollment
  
  - Functions:
  
    - make_ef2_part_j.R where j is in A, B, C, D, E, F, G, H
    
    - produce_ef2_report.R
    
  - Use

4. 12-month enrollment

  (this seems to basically just work without changing much but should be double checked)

  - Functions:
  
    - make_e12_part_j.R where j is in A, B, C, D, E, F
    
    - produce_e12_report.R
    
5. Check outcome measures for 2-year schools.

## Still need to test 2-4! 
