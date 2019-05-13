#!/bin/bash
/usr/bin/Rscript main.R -f cfg.yaml -o T -i SIM3591822 -n 3 -s 3591822 -a 25 -d 0.5 -b 0.1 -p 0.3 -m 30 -t 40
/usr/bin/Rscript main.R -f cfg.yaml -o T -i SIM3591821 -n 3 -s 3591821 -a 25 -d 0.5 -b 0.2 -p 0.3 -m 20 -t 40
/usr/bin/Rscript main.R -f cfg.yaml -o T -i SIM3591823 -n 3 -s 3591823 -a 25 -d 0.5 -b 0.1 -p 0.3 -m 30 -t 50