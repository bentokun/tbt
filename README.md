tbt is an multi-thread expression generator using genetic algorithm. tbt tends to
be easy to use and generate correct expression that return exact input value.

- options:
	+ workers: number of workers (threads)
	+ genlen: population size
	+ maxgen: maximum generations
	+ crossrate: crossover rate
	+ mutrate: mutation rate
	+ onedigit: each number in expression has only onedigit

- eg:
	tbt -onedigit=0 -workers=10 -genlen=100 -maxgen=50 10
	
