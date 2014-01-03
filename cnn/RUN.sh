
cd paleodeepdive.sampler

sbt "run-main org.hazy.gibbs.GibbsSampler -v ../variables.txt -f ../factors.txt -w ../weights.txt -l 10 -s 10 -i 10 -t 4 -o /tmp/out.out --alpha 0.01"

