.PHONY: driver prove prove_verbose

driver:
	gnatmake -f driver

# -f means force analysing everything each time
prove:
	gnatprove -P default.gpr -f

prove_verbose:
	gnatprove -P default.gpr -f --report=all

clean:
	gnatprove -P default.gpr --clean
	gnatclean driver
	rm -f gnatinspect*
