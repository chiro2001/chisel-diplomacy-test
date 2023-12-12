init:
	git submodule update --init
	cd rocket-chip && git submodule update --init cde hardfloat

idea:
	mill -i mill.idea.GenIdea/idea