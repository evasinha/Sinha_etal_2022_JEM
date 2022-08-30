for config in 'reference_SSP2-4p5' 'fert_global_const_15_SSP2-4p5' 'carbon_tax' 'carbon_tax_global_const_15'
do
     echo $config
     sbatch -J $config run-gcam.zsh
done
