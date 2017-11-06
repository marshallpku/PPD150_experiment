# Master file for simulation of a single plan

# Outline
 # Inputs: standardized model inputs. 
 # Modeling process
   # 1. Data preparation
   #    - Description: 
   #       - modifiy decrement tables
   #       - create a complete salary table
   #       - create matrices for intial members. 
   #       - estimate age distribution of new entrants
   #    - inputs: standardized model inputs
   #    - outputs: modified inputs that can be used by the model
   # 2. Demographics
   #    - inputs: initial demographic
   #    - outputs: 
   #       - demo projection for current employees (including current-employee-turned retirees)
   #       - demo projection for current retirees 
   #       - demo projection for new employees (including new-employee-turned retirees)
   # 3. Individual liabilities and costs 
   #    - inputs: single-value inputs; benefits for initial retirees
   #    - outputs:
   #       - actives:  AL, NC, salary, PVFB, by start.year, ea, age
   #       - retirees: AL, B, by start.year, retirement year, ea, age
   #       - disability? 
   # 4. Aggregate liabilities and costs
   #    - inputs:  demographic projections, individual liabilities
   #    - outputs: uncalibrated AL, NC, B, PVFB, salary for current employees, new employees and current retirees. 
   # 5. Calibration
   #    - inputs: single-value inputs, aggregate liablties and costs
   #    - outputs: 
   #       - calibrated AL, NC, B, PVFB, salary for current employees 
   #       - calibrated AL, B for current beneficiaries. 
   #       - ? Does new employees need calibration?
   # 6. Combine results:
   #    - inputs: calibrated results
   #    - outputs: Total AL, PVFB, NC, B, salary, saved in ./Outputs_liab 
   # 7. Investment scenarios 
   # 8. Simulation of plan funding
   # 9. Risk analysis
 












